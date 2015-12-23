	.file	"emit-rtl.c.bc"
	.text
	.globl	gen_raw_REG
	.align	16, 0x90
	.type	gen_raw_REG,@function
gen_raw_REG:                            # @gen_raw_REG
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp2:
	.cfi_def_cfa_offset 16
.Ltmp3:
	.cfi_offset %rbx, -16
	movq	%rsi, %rbx
	movq	%rdi, %rax
	movq	$61, %rdi
	movq	%rax, %rsi
	movq	%rbx, %rdx
	callq	gen_rtx_fmt_i0
	movl	%ebx, 16(%rax)
	popq	%rbx
	retq
.Ltmp4:
	.size	gen_raw_REG, .Ltmp4-gen_raw_REG
	.cfi_endproc

	.globl	gen_rtx_CONST_INT
	.align	16, 0x90
	.type	gen_rtx_CONST_INT,@function
gen_rtx_CONST_INT:                      # @gen_rtx_CONST_INT
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp7:
	.cfi_def_cfa_offset 16
	subq	$16, %rsp
.Ltmp8:
	.cfi_def_cfa_offset 32
.Ltmp9:
	.cfi_offset %rbx, -16
	movq	%rsi, %rax
	movq	%rax, 8(%rsp)
	leaq	64(%rax), %rcx
	cmpq	$128, %rcx
	ja	.LBB1_2
# BB#1:                                 # %if.then
	movq	const_int_rtx+512(,%rax,8), %rax
	jmp	.LBB1_4
.LBB1_2:                                # %if.end
	movq	const_int_htab(%rip), %rdi
	leaq	8(%rsp), %rsi
	movq	$1, %rcx
	movq	%rax, %rdx
	callq	htab_find_slot_with_hash
	movq	%rax, %rbx
	movq	(%rbx), %rax
	testq	%rax, %rax
	jne	.LBB1_4
# BB#3:                                 # %if.then4
	movq	8(%rsp), %rdx
	movq	$54, %rdi
	xorq	%rsi, %rsi
	callq	gen_rtx_fmt_w
	movq	%rax, (%rbx)
.LBB1_4:                                # %return
	addq	$16, %rsp
	popq	%rbx
	retq
.Ltmp10:
	.size	gen_rtx_CONST_INT, .Ltmp10-gen_rtx_CONST_INT
	.cfi_endproc

	.globl	gen_int_mode
	.align	16, 0x90
	.type	gen_int_mode,@function
gen_int_mode:                           # @gen_int_mode
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp13:
	.cfi_def_cfa_offset 16
	subq	$16, %rsp
.Ltmp14:
	.cfi_def_cfa_offset 32
.Ltmp15:
	.cfi_offset %rbx, -16
	callq	trunc_int_for_mode
	movq	%rax, 8(%rsp)
	movq	%rax, %rcx
	addq	$64, %rcx
	cmpq	$128, %rcx
	ja	.LBB2_2
# BB#1:                                 # %if.then.i
	movq	const_int_rtx+512(,%rax,8), %rax
	jmp	.LBB2_4
.LBB2_2:                                # %if.end.i
	movq	const_int_htab(%rip), %rdi
	leaq	8(%rsp), %rsi
	movq	$1, %rcx
	movq	%rax, %rdx
	callq	htab_find_slot_with_hash
	movq	%rax, %rbx
	movq	(%rbx), %rax
	testq	%rax, %rax
	jne	.LBB2_4
# BB#3:                                 # %if.then4.i
	movq	8(%rsp), %rdx
	movq	$54, %rdi
	xorq	%rsi, %rsi
	callq	gen_rtx_fmt_w
	movq	%rax, (%rbx)
.LBB2_4:                                # %gen_rtx_CONST_INT.exit
	addq	$16, %rsp
	popq	%rbx
	retq
.Ltmp16:
	.size	gen_int_mode, .Ltmp16-gen_int_mode
	.cfi_endproc

	.globl	gen_rtx_CONST_DOUBLE
	.align	16, 0x90
	.type	gen_rtx_CONST_DOUBLE,@function
gen_rtx_CONST_DOUBLE:                   # @gen_rtx_CONST_DOUBLE
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp21:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp22:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp23:
	.cfi_def_cfa_offset 32
.Ltmp24:
	.cfi_offset %rbx, -32
.Ltmp25:
	.cfi_offset %r14, -24
.Ltmp26:
	.cfi_offset %r15, -16
	movq	%rdx, %r14
	movq	%rsi, %r15
	movq	%rdi, %rbx
	movq	$55, %rdi
	callq	rtx_alloc
	movb	%bl, 2(%rax)
	movzbl	rtx_length+55(%rip), %ecx
	movq	$0, 8(%rax)
	movq	%r15, 16(%rax)
	movq	%r14, 24(%rax)
	leaq	-1(%rcx), %rdx
	cmpq	$3, %rdx
	jl	.LBB3_3
# BB#1:                                 # %for.body.lr.ph
	leaq	(%rax,%rcx,8), %rsi
	.align	16, 0x90
.LBB3_2:                                # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	$0, (%rsi)
	decq	%rdx
	movq	$32, %rcx
	movq	%rdx, %rdi
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	addq	$-8, %rsi
	cmpq	$2, %rdi
	jg	.LBB3_2
.LBB3_3:                                # %for.end
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp27:
	.size	gen_rtx_CONST_DOUBLE, .Ltmp27-gen_rtx_CONST_DOUBLE
	.cfi_endproc

	.globl	gen_rtx_REG
	.align	16, 0x90
	.type	gen_rtx_REG,@function
gen_rtx_REG:                            # @gen_rtx_REG
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp31:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp32:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp33:
	.cfi_def_cfa_offset 32
.Ltmp34:
	.cfi_offset %rbx, -24
.Ltmp35:
	.cfi_offset %r14, -16
	movl	target_flags(%rip), %ebx
	movq	%rsi, %r14
	movq	%rdi, %rax
	movabsq	$4294967295, %rsi       # imm = 0xFFFFFFFF
                                        # kill: RDI<def> RAX<kill>
	andq	%rsi, %rdi
	andq	$33554432, %rbx         # imm = 0x2000000
	movq	$25, %rcx
	movq	%rbx, %rdx
	shrq	%cl, %rdx
	orq	$4, %rdx
	cmpq	%rdi, %rdx
	jne	.LBB4_21
# BB#1:                                 # %entry
	movl	reload_in_progress(%rip), %ecx
	testq	%rsi, %rcx
	jne	.LBB4_21
# BB#2:                                 # %if.then
	movq	%r14, %rcx
	andq	%rsi, %rcx
	cmpq	$6, %rcx
	je	.LBB4_6
# BB#3:                                 # %if.then
	cmpq	$16, %rcx
	jne	.LBB4_4
# BB#7:                                 # %if.then8
	movq	global_rtl+40(%rip), %rax
	jmp	.LBB4_22
.LBB4_6:                                # %if.then5
	movq	global_rtl+32(%rip), %rax
	jmp	.LBB4_22
.LBB4_4:                                # %if.then
	cmpq	$20, %rcx
	jne	.LBB4_8
# BB#5:                                 # %if.then3
	movq	global_rtl+24(%rip), %rax
	jmp	.LBB4_22
.LBB4_8:                                # %if.end9
	testq	%rsi, %rbx
	jne	.LBB4_12
# BB#9:                                 # %lor.rhs
	movl	flag_pic(%rip), %edx
	testq	%rdx, %rdx
	je	.LBB4_10
# BB#11:                                # %select.mid
	movq	$3, %rsi
	jmp	.LBB4_12
.LBB4_10:
	movabsq	$4294967295, %rsi       # imm = 0xFFFFFFFF
.LBB4_12:                               # %lor.end
	cmpq	%rcx, %rsi
	jne	.LBB4_19
# BB#13:                                # %land.lhs.true15
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rbx
	jne	.LBB4_17
# BB#14:                                # %lor.rhs18
	movl	flag_pic(%rip), %ecx
	testq	%rcx, %rcx
	je	.LBB4_15
# BB#16:                                # %select.mid2
	movq	$3, %rcx
	jmp	.LBB4_17
.LBB4_15:
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
.LBB4_17:                               # %lor.end21
	movzbl	fixed_regs(%rcx), %ecx
	testq	%rcx, %rcx
	je	.LBB4_19
# BB#18:                                # %if.then24
	movq	pic_offset_table_rtx(%rip), %rax
	jmp	.LBB4_22
.LBB4_19:                               # %if.end25
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%r14, %rcx
	cmpq	$7, %rcx
	jne	.LBB4_21
# BB#20:                                # %if.then28
	movq	global_rtl+16(%rip), %rax
	jmp	.LBB4_22
.LBB4_21:                               # %if.end30
	movq	$61, %rdi
	movq	%rax, %rsi
	movq	%r14, %rdx
	callq	gen_rtx_fmt_i0
	movl	%r14d, 16(%rax)
.LBB4_22:                               # %return
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp36:
	.size	gen_rtx_REG, .Ltmp36-gen_rtx_REG
	.cfi_endproc

	.globl	gen_rtx_MEM
	.align	16, 0x90
	.type	gen_rtx_MEM,@function
gen_rtx_MEM:                            # @gen_rtx_MEM
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rax
.Ltmp38:
	.cfi_def_cfa_offset 16
	movq	%rsi, %rax
	movq	%rdi, %rcx
	movq	$66, %rdi
	movq	%rcx, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e0
	movq	$0, 16(%rax)
	popq	%rdx
	retq
.Ltmp39:
	.size	gen_rtx_MEM, .Ltmp39-gen_rtx_MEM
	.cfi_endproc

	.globl	gen_rtx_SUBREG
	.align	16, 0x90
	.type	gen_rtx_SUBREG,@function
gen_rtx_SUBREG:                         # @gen_rtx_SUBREG
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rax
.Ltmp41:
	.cfi_def_cfa_offset 16
	movq	%rdx, %r8
	movq	%rdi, %r9
	movq	$32, %rcx
	movq	%r8, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$32, %rcx
	movq	%r9, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movzbl	mode_size(%rdx), %edi
	movq	%rsi, %rcx
	cqto
	idivq	%rdi
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %rdx
	jne	.LBB6_1
# BB#2:                                 # %if.end
	movq	$63, %rdi
	movq	%r9, %rsi
	movq	%rcx, %rdx
	movq	%r8, %rcx
	popq	%rax
	jmp	gen_rtx_fmt_ei  # TAILCALL
.LBB6_1:                                # %if.then
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.gen_rtx_SUBREG, %rcx
	movq	$456, %rsi              # imm = 0x1C8
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp42:
	.size	gen_rtx_SUBREG, .Ltmp42-gen_rtx_SUBREG
	.cfi_endproc

	.globl	gen_lowpart_SUBREG
	.align	16, 0x90
	.type	gen_lowpart_SUBREG,@function
gen_lowpart_SUBREG:                     # @gen_lowpart_SUBREG
	.cfi_startproc
# BB#0:                                 # %entry
	movq	%rdi, %rax
	movq	$63, %rdi
	xorq	%rcx, %rcx
	movq	%rsi, %rdx
	movq	%rax, %rsi
	jmp	gen_rtx_fmt_ei  # TAILCALL
.Ltmp43:
	.size	gen_lowpart_SUBREG, .Ltmp43-gen_lowpart_SUBREG
	.cfi_endproc

	.globl	subreg_lowpart_offset
	.align	16, 0x90
	.type	subreg_lowpart_offset,@function
subreg_lowpart_offset:                  # @subreg_lowpart_offset
	.cfi_startproc
# BB#0:                                 # %entry
	xorq	%rax, %rax
	retq
.Ltmp44:
	.size	subreg_lowpart_offset, .Ltmp44-subreg_lowpart_offset
	.cfi_endproc

	.globl	gen_rtx
	.align	16, 0x90
	.type	gen_rtx,@function
gen_rtx:                                # @gen_rtx
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp49:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp50:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp51:
	.cfi_def_cfa_offset 32
	subq	$80, %rsp
.Ltmp52:
	.cfi_def_cfa_offset 112
.Ltmp53:
	.cfi_offset %rbx, -32
.Ltmp54:
	.cfi_offset %r14, -24
.Ltmp55:
	.cfi_offset %r15, -16
	movq	%rsi, %r15
	movq	%rdi, %rbx
	movq	%r9, 40(%rsp)
	movq	%r8, 32(%rsp)
	movq	%rcx, 24(%rsp)
	movq	%rdx, 16(%rsp)
	movq	$32, %rcx
	movq	%rbx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$65, %rax
	leaq	(%rsp), %rax
	movq	%rax, 64(%rsp)
	leaq	112(%rsp), %rax
	movq	%rax, 56(%rsp)
	movq	$48, 52(%rsp)
	movq	$16, 48(%rsp)
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	jg	.LBB9_44
# BB#1:                                 # %entry
	movq	%rbx, %rcx
	andq	%rax, %rcx
	cmpq	$54, %rcx
	je	.LBB9_6
# BB#2:                                 # %entry
	cmpq	$55, %rcx
	jne	.LBB9_3
# BB#13:                                # %sw.bb3
	movl	48(%rsp), %eax
	movq	$32, %rcx
	movq	%rax, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	cmpq	$41, %rax
	jae	.LBB9_14
# BB#16:                                # %vaarg.end14
	movq	64(%rsp), %rcx
	leaq	8(%rax), %rdx
	movl	%edx, 48(%rsp)
	movq	(%rcx,%rsi), %r14
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rdx, %rcx
	cmpq	$40, %rcx
	ja	.LBB9_15
# BB#17:                                # %vaarg.in_reg20
	movq	64(%rsp), %rsi
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	addq	%rsi, %rdx
	addq	$16, %rax
	movl	%eax, 48(%rsp)
	jmp	.LBB9_18
.LBB9_44:                               # %entry
	andq	%rbx, %rax
	cmpq	$66, %rax
	jne	.LBB9_50
# BB#45:                                # %sw.bb43
	movl	48(%rsp), %eax
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	$40, %rax
	ja	.LBB9_47
# BB#46:                                # %vaarg.in_reg48
	movq	64(%rsp), %rcx
	addq	%rdx, %rcx
	addq	$8, %rax
	movl	%eax, 48(%rsp)
	jmp	.LBB9_48
.LBB9_6:                                # %sw.bb
	movl	48(%rsp), %eax
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	$40, %rax
	ja	.LBB9_8
# BB#7:                                 # %vaarg.in_reg
	movq	64(%rsp), %rcx
	addq	%rdx, %rcx
	addq	$8, %rax
	movl	%eax, 48(%rsp)
	jmp	.LBB9_9
.LBB9_3:                                # %entry
	cmpq	$61, %rcx
	jne	.LBB9_50
# BB#4:                                 # %sw.bb29
	movl	48(%rsp), %edx
	movq	$32, %rcx
	movq	%rdx, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	cmpq	$40, %rdx
	ja	.LBB9_21
# BB#5:                                 # %vaarg.in_reg34
	movq	64(%rsp), %rcx
	addq	%rsi, %rcx
	addq	$8, %rdx
	movl	%edx, 48(%rsp)
	jmp	.LBB9_22
.LBB9_50:                               # %sw.default
	movq	%rbx, %rdi
	callq	rtx_alloc
	movq	$32, %rcx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	movzbl	rtx_length(%rbx), %edx
	movb	%r15b, 2(%rax)
	testq	%rdx, %rdx
	je	.LBB9_49
# BB#51:                                # %for.body.lr.ph
	movq	rtx_format(,%rbx,8), %r9
	movq	$1, %rdi
	movabsq	$4294967295, %r8        # imm = 0xFFFFFFFF
	jmp	.LBB9_52
.LBB9_47:                               # %vaarg.in_mem50
	movq	56(%rsp), %rcx
	leaq	8(%rcx), %rax
	movq	%rax, 56(%rsp)
.LBB9_48:                               # %vaarg.end54
	movq	(%rcx), %rdx
	movq	$66, %rdi
	movq	%r15, %rsi
	callq	gen_rtx_fmt_e0
	movq	$0, 16(%rax)
	jmp	.LBB9_49
.LBB9_67:                               # %vaarg.in_mem69
                                        #   in Loop: Header=BB9_52 Depth=1
	movq	56(%rsp), %rcx
	leaq	8(%rcx), %rsi
	movq	%rsi, 56(%rsp)
.LBB9_68:                               # %vaarg.end73
                                        #   in Loop: Header=BB9_52 Depth=1
	movl	(%rcx), %ecx
	movl	%ecx, (%rax,%rdi,8)
	jmp	.LBB9_71
	.align	16, 0x90
.LBB9_52:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movsbq	-1(%r9,%rdi), %rcx
	cmpq	$68, %rcx
	jg	.LBB9_55
# BB#53:                                # %for.body
                                        #   in Loop: Header=BB9_52 Depth=1
	andq	%r8, %rcx
	cmpq	$48, %rcx
	je	.LBB9_71
	jmp	.LBB9_54
	.align	16, 0x90
.LBB9_55:                               # %for.body
                                        #   in Loop: Header=BB9_52 Depth=1
	cmpq	$97, %rcx
	jg	.LBB9_59
# BB#56:                                # %for.body
                                        #   in Loop: Header=BB9_52 Depth=1
	andq	%r8, %rcx
	cmpq	$69, %rcx
	je	.LBB9_57
	jmp	.LBB9_54
	.align	16, 0x90
.LBB9_59:                               # %for.body
                                        #   in Loop: Header=BB9_52 Depth=1
	cmpq	$114, %rcx
	jg	.LBB9_65
# BB#60:                                # %for.body
                                        #   in Loop: Header=BB9_52 Depth=1
	andq	%r8, %rcx
	cmpq	$98, %rcx
	je	.LBB9_57
# BB#61:                                # %for.body
                                        #   in Loop: Header=BB9_52 Depth=1
	cmpq	$101, %rcx
	je	.LBB9_57
# BB#62:                                # %for.body
                                        #   in Loop: Header=BB9_52 Depth=1
	cmpq	$105, %rcx
	jne	.LBB9_54
# BB#63:                                # %sw.bb62
                                        #   in Loop: Header=BB9_52 Depth=1
	movl	48(%rsp), %esi
	movq	$32, %rcx
	movq	%rsi, %rbx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	cmpq	$40, %rsi
	ja	.LBB9_67
# BB#64:                                # %vaarg.in_reg67
                                        #   in Loop: Header=BB9_52 Depth=1
	movq	64(%rsp), %rcx
	addq	%rbx, %rcx
	addq	$8, %rsi
	movl	%esi, 48(%rsp)
	jmp	.LBB9_68
.LBB9_65:                               # %for.body
                                        #   in Loop: Header=BB9_52 Depth=1
	addq	$-115, %rcx
	andq	%r8, %rcx
	cmpq	$4, %rcx
	ja	.LBB9_54
# BB#66:                                # %for.body
                                        #   in Loop: Header=BB9_52 Depth=1
	jmpq	*.LJTI9_0(,%rcx,8)
.LBB9_57:                               # %sw.bb77
                                        #   in Loop: Header=BB9_52 Depth=1
	movl	48(%rsp), %ebx
	movq	$32, %rcx
	movq	%rbx, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	cmpq	$40, %rbx
	ja	.LBB9_69
# BB#58:                                # %vaarg.in_reg82
                                        #   in Loop: Header=BB9_52 Depth=1
	movq	64(%rsp), %rcx
	addq	%rsi, %rcx
	addq	$8, %rbx
	movl	%ebx, 48(%rsp)
	jmp	.LBB9_70
	.align	16, 0x90
.LBB9_69:                               # %vaarg.in_mem84
                                        #   in Loop: Header=BB9_52 Depth=1
	movq	56(%rsp), %rcx
	leaq	8(%rcx), %rsi
	movq	%rsi, 56(%rsp)
.LBB9_70:                               # %vaarg.end88
                                        #   in Loop: Header=BB9_52 Depth=1
	movq	(%rcx), %rcx
	movq	%rcx, (%rax,%rdi,8)
.LBB9_71:                               # %for.inc
                                        #   in Loop: Header=BB9_52 Depth=1
	movq	$32, %rcx
	movq	%rdi, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	incq	%rdi
	cmpq	%rdx, %rsi
	jl	.LBB9_52
	jmp	.LBB9_49
.LBB9_14:                               # %vaarg.end14.thread
	movq	56(%rsp), %rax
	leaq	8(%rax), %rcx
	movq	%rcx, 56(%rsp)
	movq	(%rax), %r14
.LBB9_15:                               # %vaarg.in_mem22
	movq	56(%rsp), %rdx
	leaq	8(%rdx), %rax
	movq	%rax, 56(%rsp)
.LBB9_18:                               # %vaarg.end26
	movq	(%rdx), %rbx
	movq	$55, %rdi
	callq	rtx_alloc
	movb	%r15b, 2(%rax)
	movzbl	rtx_length+55(%rip), %ecx
	movq	$0, 8(%rax)
	movq	%r14, 16(%rax)
	movq	%rbx, 24(%rax)
	leaq	-1(%rcx), %rdx
	cmpq	$3, %rdx
	jl	.LBB9_49
# BB#19:                                # %for.body.lr.ph.i
	leaq	(%rax,%rcx,8), %rsi
	.align	16, 0x90
.LBB9_20:                               # %for.body.i
                                        # =>This Inner Loop Header: Depth=1
	movq	$0, (%rsi)
	decq	%rdx
	movq	$32, %rcx
	movq	%rdx, %rdi
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	addq	$-8, %rsi
	cmpq	$2, %rdi
	jg	.LBB9_20
	jmp	.LBB9_49
.LBB9_8:                                # %vaarg.in_mem
	movq	56(%rsp), %rcx
	leaq	8(%rcx), %rax
	movq	%rax, 56(%rsp)
.LBB9_9:                                # %vaarg.end
	movq	(%rcx), %rdx
	movq	%rdx, 72(%rsp)
	leaq	64(%rdx), %rax
	cmpq	$128, %rax
	ja	.LBB9_11
# BB#10:                                # %if.then.i
	movq	const_int_rtx+512(,%rdx,8), %rax
	jmp	.LBB9_49
.LBB9_11:                               # %if.end.i
	movq	const_int_htab(%rip), %rdi
	leaq	72(%rsp), %rsi
	movq	$1, %rcx
	callq	htab_find_slot_with_hash
	movq	%rax, %rbx
	movq	(%rbx), %rax
	testq	%rax, %rax
	jne	.LBB9_49
# BB#12:                                # %if.then4.i
	movq	72(%rsp), %rdx
	movq	$54, %rdi
	xorq	%rsi, %rsi
	callq	gen_rtx_fmt_w
	movq	%rax, (%rbx)
	jmp	.LBB9_49
.LBB9_21:                               # %vaarg.in_mem36
	movq	56(%rsp), %rcx
	leaq	8(%rcx), %rdx
	movq	%rdx, 56(%rsp)
.LBB9_22:                               # %vaarg.end40
	movl	target_flags(%rip), %edx
	movl	(%rcx), %r14d
	andq	$33554432, %rdx         # imm = 0x2000000
	movq	$25, %rcx
	movq	%rdx, %rsi
	shrq	%cl, %rsi
	orq	$4, %rsi
	movq	%r15, %rcx
	andq	%rax, %rcx
	cmpq	%rcx, %rsi
	jne	.LBB9_43
# BB#23:                                # %vaarg.end40
	movl	reload_in_progress(%rip), %ecx
	testq	%rax, %rcx
	jne	.LBB9_43
# BB#24:                                # %if.then.i207
	movq	%r14, %rcx
	andq	%rax, %rcx
	cmpq	$6, %rcx
	je	.LBB9_28
# BB#25:                                # %if.then.i207
	cmpq	$16, %rcx
	jne	.LBB9_26
# BB#29:                                # %if.then8.i
	movq	global_rtl+40(%rip), %rax
	jmp	.LBB9_49
.LBB9_54:                               # %sw.default173
	movabsq	$.L.str, %rdi
	movabsq	$.L__FUNCTION__.gen_rtx, %rdx
	movq	$589, %rsi              # imm = 0x24D
	callq	fancy_abort
.LBB9_28:                               # %if.then5.i
	movq	global_rtl+32(%rip), %rax
	jmp	.LBB9_49
.LBB9_26:                               # %if.then.i207
	cmpq	$20, %rcx
	jne	.LBB9_30
# BB#27:                                # %if.then3.i
	movq	global_rtl+24(%rip), %rax
	jmp	.LBB9_49
.LBB9_30:                               # %if.end9.i
	testq	%rax, %rdx
	jne	.LBB9_34
# BB#31:                                # %lor.rhs.i
	movl	flag_pic(%rip), %eax
	testq	%rax, %rax
	je	.LBB9_32
# BB#33:                                # %select.mid
	movq	$3, %rax
	jmp	.LBB9_34
.LBB9_32:
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
.LBB9_34:                               # %lor.end.i
	cmpq	%rcx, %rax
	jne	.LBB9_41
# BB#35:                                # %land.lhs.true15.i
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %rdx
	jne	.LBB9_39
# BB#36:                                # %lor.rhs18.i
	movl	flag_pic(%rip), %eax
	testq	%rax, %rax
	je	.LBB9_37
# BB#38:                                # %select.mid25
	movq	$3, %rax
	jmp	.LBB9_39
.LBB9_37:
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
.LBB9_39:                               # %lor.end21.i
	movzbl	fixed_regs(%rax), %eax
	testq	%rax, %rax
	je	.LBB9_41
# BB#40:                                # %if.then24.i
	movq	pic_offset_table_rtx(%rip), %rax
	jmp	.LBB9_49
.LBB9_41:                               # %if.end25.i
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%r14, %rax
	cmpq	$7, %rax
	jne	.LBB9_43
# BB#42:                                # %if.then28.i
	movq	global_rtl+16(%rip), %rax
	jmp	.LBB9_49
.LBB9_43:                               # %if.end30.i
	movq	$61, %rdi
	movq	%r15, %rsi
	movq	%r14, %rdx
	callq	gen_rtx_fmt_i0
	movl	%r14d, 16(%rax)
.LBB9_49:                               # %sw.epilog174
	addq	$80, %rsp
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp56:
	.size	gen_rtx, .Ltmp56-gen_rtx
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI9_0:
	.quad	.LBB9_57
	.quad	.LBB9_57
	.quad	.LBB9_57
	.quad	.LBB9_54
	.quad	.LBB9_57

	.text
	.globl	gen_rtvec
	.align	16, 0x90
	.type	gen_rtvec,@function
gen_rtvec:                              # @gen_rtvec
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbp
.Ltmp60:
	.cfi_def_cfa_offset 16
.Ltmp61:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
.Ltmp62:
	.cfi_def_cfa_register %rbp
	pushq	%r15
	pushq	%r14
	pushq	%r12
	pushq	%rbx
	subq	$80, %rsp
.Ltmp63:
	.cfi_offset %rbx, -48
.Ltmp64:
	.cfi_offset %r12, -40
.Ltmp65:
	.cfi_offset %r14, -32
.Ltmp66:
	.cfi_offset %r15, -24
	movq	%r9, -72(%rbp)
	movq	%r8, -80(%rbp)
	movq	%rcx, -88(%rbp)
	movq	%rdx, -96(%rbp)
	movq	%rsi, -104(%rbp)
	xorq	%rax, %rax
	movabsq	$4294967295, %r14       # imm = 0xFFFFFFFF
	testq	%r14, %rdi
	leaq	-112(%rbp), %rcx
	movq	%rcx, -48(%rbp)
	leaq	16(%rbp), %rcx
	movq	%rcx, -56(%rbp)
	movq	$48, -60(%rbp)
	movq	$8, -64(%rbp)
	je	.LBB10_11
# BB#1:                                 # %if.end
	movq	$32, %rcx
	movq	%rdi, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	leaq	15(,%rax,8), %rcx
	andq	$-16, %rcx
	movq	%rsp, %r15
	subq	%rcx, %r15
	movq	%r15, %rsp
	testq	%rax, %rax
	jle	.LBB10_2
# BB#3:                                 # %for.body.lr.ph
	movl	-64(%rbp), %eax
	movq	$1, %rdx
	movq	%rdi, %r12
	andq	%r14, %r12
	.align	16, 0x90
.LBB10_4:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	%rax, %rcx
	andq	%r14, %rcx
	cmpq	$40, %rcx
	ja	.LBB10_6
# BB#5:                                 # %vaarg.in_reg
                                        #   in Loop: Header=BB10_4 Depth=1
	movq	-48(%rbp), %rbx
	movq	$32, %rcx
	movq	%rax, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	addq	%rbx, %rsi
	addq	$8, %rax
	movl	%eax, -64(%rbp)
	jmp	.LBB10_7
	.align	16, 0x90
.LBB10_6:                               # %vaarg.in_mem
                                        #   in Loop: Header=BB10_4 Depth=1
	movq	-56(%rbp), %rsi
	leaq	8(%rsi), %rcx
	movq	%rcx, -56(%rbp)
.LBB10_7:                               # %vaarg.end
                                        #   in Loop: Header=BB10_4 Depth=1
	movq	(%rsi), %rcx
	movq	%rcx, -8(%r15,%rdx,8)
	movq	%rdx, %rcx
	andq	%r14, %rcx
	incq	%rdx
	cmpq	%r12, %rcx
	jne	.LBB10_4
# BB#8:                                 # %if.end.i
	movq	$32, %rcx
	movq	%rdi, %rbx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	callq	rtvec_alloc
	testq	%rbx, %rbx
	jle	.LBB10_11
# BB#9:
	movq	$1, %rcx
	.align	16, 0x90
.LBB10_10:                              # %for.body.i
                                        # =>This Inner Loop Header: Depth=1
	movq	-8(%r15,%rcx,8), %rdx
	movq	%rdx, (%rax,%rcx,8)
	movq	%rcx, %rdx
	andq	%r14, %rdx
	incq	%rcx
	cmpq	%r12, %rdx
	jne	.LBB10_10
	jmp	.LBB10_11
.LBB10_2:                               # %if.end.i.thread
	callq	rtvec_alloc
.LBB10_11:                              # %return
	leaq	-32(%rbp), %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	popq	%rbp
	retq
.Ltmp67:
	.size	gen_rtvec, .Ltmp67-gen_rtvec
	.cfi_endproc

	.globl	gen_rtvec_v
	.align	16, 0x90
	.type	gen_rtvec_v,@function
gen_rtvec_v:                            # @gen_rtvec_v
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp73:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp74:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp75:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp76:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp77:
	.cfi_def_cfa_offset 48
.Ltmp78:
	.cfi_offset %rbx, -40
.Ltmp79:
	.cfi_offset %r12, -32
.Ltmp80:
	.cfi_offset %r14, -24
.Ltmp81:
	.cfi_offset %r15, -16
	movq	%rsi, %r14
	movq	%rdi, %rbx
	xorq	%rax, %rax
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	testq	%r15, %rbx
	je	.LBB11_4
# BB#1:                                 # %if.end
	movq	$32, %rcx
	movq	%rbx, %r12
	shlq	%cl, %r12
	movq	$32, %rcx
	sarq	%cl, %r12
	movq	%rbx, %rdi
	callq	rtvec_alloc
	testq	%r12, %r12
	jle	.LBB11_4
# BB#2:
	movq	$1, %rcx
	andq	%r15, %rbx
	.align	16, 0x90
.LBB11_3:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	-8(%r14,%rcx,8), %rdx
	movq	%rdx, (%rax,%rcx,8)
	movq	%rcx, %rdx
	andq	%r15, %rdx
	incq	%rcx
	cmpq	%rbx, %rdx
	jne	.LBB11_3
.LBB11_4:                               # %return
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.Ltmp82:
	.size	gen_rtvec_v, .Ltmp82-gen_rtvec_v
	.cfi_endproc

	.globl	gen_reg_rtx
	.align	16, 0x90
	.type	gen_reg_rtx,@function
gen_reg_rtx:                            # @gen_reg_rtx
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
	movl	no_new_pseudos(%rip), %eax
	movq	%rdi, %r12
	testq	%rax, %rax
	jne	.LBB12_10
# BB#1:                                 # %if.end
	movl	generating_concat_p(%rip), %eax
	movq	cfun(%rip), %r14
	testq	%rax, %rax
	je	.LBB12_7
# BB#2:                                 # %land.lhs.true
	movq	$32, %rcx
	movq	%r12, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movl	mode_class(,%rdx,4), %eax
	leaq	-5(%rax), %rcx
	movabsq	$4294967295, %rsi       # imm = 0xFFFFFFFF
	andq	%rsi, %rcx
	cmpq	$1, %rcx
	ja	.LBB12_7
# BB#3:                                 # %if.then5
	movzbl	mode_unit_size(%rdx), %edi
	movq	$3, %rcx
	shlq	%cl, %rdi
	andq	%rsi, %rax
	cmpq	$6, %rax
	jne	.LBB12_4
# BB#5:                                 # %if.then5
	movabsq	$1, %rsi
	jmp	.LBB12_6
.LBB12_7:                               # %if.end15
	movq	%r12, 8(%rsp)           # 8-byte Spill
	movq	24(%r14), %rax
	movl	(%rax), %ebx
	movq	$32, %rcx
	movq	%rbx, %r12
	movl	56(%rax), %edx
	shlq	%cl, %r12
	movq	$32, %rcx
	sarq	%cl, %r12
	cmpq	%rdx, %rbx
	jne	.LBB12_9
# BB#8:                                 # %if.then19
	movq	64(%rax), %rdi
	movq	$33, %rcx
	movq	%rbx, %r15
	shlq	%cl, %r15
	leaq	(%rbx,%rbx), %rax
	movq	%rax, (%rsp)            # 8-byte Spill
	movq	$32, %rcx
	sarq	%cl, %r15
	movq	%r15, %rsi
	callq	xrealloc
	movq	%rax, %rbx
	leaq	(%rbx,%r12), %rdi
	xorq	%rsi, %rsi
	movq	%r12, %rdx
	callq	memset
	movq	24(%r14), %rax
	movq	%rbx, 64(%rax)
	movq	80(%rax), %rdi
	movq	$3, %rcx
	shlq	%cl, %r15
	movq	%r15, %rsi
	callq	xrealloc
	movq	%rax, %r13
	movq	$3, %rcx
	movq	%r12, %rbx
	shlq	%cl, %rbx
	leaq	(%r13,%r12,8), %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	callq	memset
	movq	cfun(%rip), %rax
	movq	24(%rax), %rax
	movq	%r13, 80(%rax)
	movq	24(%r14), %rax
	movq	72(%rax), %rdi
	movq	%r15, %rsi
	callq	xrealloc
	movq	%rax, %r15
	leaq	(%r15,%r12,8), %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	callq	memset
	movq	24(%r14), %rax
	movq	%r15, 72(%rax)
	movq	(%rsp), %rcx            # 8-byte Reload
	movl	%ecx, 56(%rax)
	movq	cfun(%rip), %rax
	movq	24(%rax), %rax
	movl	(%rax), %ebx
.LBB12_9:                               # %if.end54
	movq	8(%rsp), %rsi           # 8-byte Reload
	movq	$61, %rdi
	movq	%rbx, %rdx
	callq	gen_rtx_fmt_i0
	movl	%ebx, 16(%rax)
	movq	cfun(%rip), %rcx
	movq	24(%rcx), %rcx
	movslq	(%rcx), %rdx
	leaq	1(%rdx), %rsi
	movl	%esi, (%rcx)
	movq	80(%rcx), %rcx
	movq	%rax, (%rcx,%rdx,8)
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB12_4:
	movabsq	$0, %rsi
.LBB12_6:                               # %if.then5
	incq	%rsi
	xorq	%rdx, %rdx
	callq	mode_for_size
	movq	%rax, %rbx
	movq	%rbx, %rdi
	callq	gen_reg_rtx
	movq	%rax, %r15
	movq	%rbx, %rdi
	callq	gen_reg_rtx
	movq	$65, %rdi
	movq	%r12, %rsi
	movq	%r15, %rdx
	movq	%rax, %rcx
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	gen_rtx_fmt_ee  # TAILCALL
.LBB12_10:                              # %if.then
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.gen_reg_rtx, %rcx
	movq	$662, %rsi              # imm = 0x296
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp100:
	.size	gen_reg_rtx, .Ltmp100-gen_reg_rtx
	.cfi_endproc

	.globl	mark_user_reg
	.align	16, 0x90
	.type	mark_user_reg,@function
mark_user_reg:                          # @mark_user_reg
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rax
.Ltmp102:
	.cfi_def_cfa_offset 16
	movl	(%rdi), %eax
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$61, %rcx
	jne	.LBB13_1
# BB#4:                                 # %if.then12
	orq	$134217728, %rax        # imm = 0x8000000
	movl	%eax, (%rdi)
	popq	%rax
	retq
.LBB13_1:                               # %entry
	cmpq	$65, %rcx
	jne	.LBB13_3
# BB#2:                                 # %if.then
	movq	8(%rdi), %rax
	movl	(%rax), %ecx
	orq	$134217728, %rcx        # imm = 0x8000000
	movl	%ecx, (%rax)
	movq	16(%rdi), %rax
	movl	(%rax), %ecx
	orq	$134217728, %rcx        # imm = 0x8000000
	movl	%ecx, (%rax)
	popq	%rax
	retq
.LBB13_3:                               # %if.else16
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.mark_user_reg, %rcx
	movq	$732, %rsi              # imm = 0x2DC
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp103:
	.size	mark_user_reg, .Ltmp103-mark_user_reg
	.cfi_endproc

	.globl	mark_reg_pointer
	.align	16, 0x90
	.type	mark_reg_pointer,@function
mark_reg_pointer:                       # @mark_reg_pointer
	.cfi_startproc
# BB#0:                                 # %entry
	movslq	(%rdi), %rax
	testq	%rax, %rax
	js	.LBB14_3
# BB#1:                                 # %if.then
	movabsq	$2147483648, %rcx       # imm = 0x80000000
	orq	%rcx, %rax
	movl	%eax, (%rdi)
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %rsi
	je	.LBB14_6
# BB#2:                                 # %if.then3
	movq	cfun(%rip), %rax
	movq	24(%rax), %rax
	movl	8(%rdi), %ecx
	movq	64(%rax), %rax
	movb	%sil, (%rax,%rcx)
	retq
.LBB14_3:                               # %if.else
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %rsi
	je	.LBB14_6
# BB#4:                                 # %land.lhs.true
	movq	cfun(%rip), %rax
	movq	24(%rax), %rcx
	movl	8(%rdi), %r8d
	movq	64(%rcx), %rdx
	movzbl	(%rdx,%r8), %edi
	movq	$32, %rcx
	movq	%rsi, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	%rax, %rdi
	jle	.LBB14_6
# BB#5:                                 # %if.then15
	movb	%sil, (%rdx,%r8)
.LBB14_6:                               # %if.end25
	retq
.Ltmp104:
	.size	mark_reg_pointer, .Ltmp104-mark_reg_pointer
	.cfi_endproc

	.globl	max_reg_num
	.align	16, 0x90
	.type	max_reg_num,@function
max_reg_num:                            # @max_reg_num
	.cfi_startproc
# BB#0:                                 # %entry
	movq	cfun(%rip), %rax
	movq	24(%rax), %rax
	movl	(%rax), %eax
	retq
.Ltmp105:
	.size	max_reg_num, .Ltmp105-max_reg_num
	.cfi_endproc

	.globl	max_label_num
	.align	16, 0x90
	.type	max_label_num,@function
max_label_num:                          # @max_label_num
	.cfi_startproc
# BB#0:                                 # %entry
	movl	last_label_num(%rip), %eax
	testq	%rax, %rax
	movabsq	$0, %rcx
	movabsq	$1, %r8
	movq	%rcx, %rdx
	je	.LBB16_2
# BB#1:                                 # %entry
	movq	%r8, %rdx
.LBB16_2:                               # %entry
	movl	label_num(%rip), %edi
	movl	base_label_num(%rip), %esi
	cmpq	%rsi, %rdi
	jne	.LBB16_4
# BB#3:                                 # %entry
	movq	%r8, %rcx
.LBB16_4:                               # %entry
	testq	%rcx, %rdx
	jne	.LBB16_6
# BB#5:                                 # %entry
	movq	%rdi, %rax
.LBB16_6:                               # %entry
	retq
.Ltmp106:
	.size	max_label_num, .Ltmp106-max_label_num
	.cfi_endproc

	.globl	get_first_label_num
	.align	16, 0x90
	.type	get_first_label_num,@function
get_first_label_num:                    # @get_first_label_num
	.cfi_startproc
# BB#0:                                 # %entry
	movq	cfun(%rip), %rax
	movq	24(%rax), %rax
	movl	4(%rax), %eax
	retq
.Ltmp107:
	.size	get_first_label_num, .Ltmp107-get_first_label_num
	.cfi_endproc

	.globl	subreg_hard_regno
	.align	16, 0x90
	.type	subreg_hard_regno,@function
subreg_hard_regno:                      # @subreg_hard_regno
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp112:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp113:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp114:
	.cfi_def_cfa_offset 32
.Ltmp115:
	.cfi_offset %rbx, -32
.Ltmp116:
	.cfi_offset %r14, -24
.Ltmp117:
	.cfi_offset %r15, -16
	movq	%rdi, %r14
	movl	(%r14), %edx
	movq	%rsi, %rax
	movq	$16, %rcx
	movq	%rdx, %rbx
	shrq	%cl, %rbx
	andq	$65535, %rdx            # imm = 0xFFFF
	cmpq	$63, %rdx
	jne	.LBB18_2
# BB#1:                                 # %lor.lhs.false
	movq	8(%r14), %rcx
	movl	(%rcx), %esi
	movq	%rsi, %rdx
	andq	$65535, %rdx            # imm = 0xFFFF
	cmpq	$61, %rdx
	jne	.LBB18_2
# BB#4:                                 # %if.end
	movl	8(%rcx), %edi
	cmpq	$53, %rdi
	jae	.LBB18_5
# BB#6:                                 # %if.end10
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	testq	%r15, %rax
	je	.LBB18_9
# BB#7:                                 # %land.lhs.true
	movq	$16, %rcx
	shrq	%cl, %rsi
	andq	$255, %rsi
	callq	ix86_hard_regno_mode_ok
	testq	%r15, %rax
	je	.LBB18_8
.LBB18_9:                               # %if.end16
	andq	$255, %rbx
	movl	16(%r14), %eax
	movzbl	mode_size(%rbx), %ecx
	xorq	%rdx, %rdx
	divq	%rcx
	testq	%rdx, %rdx
	jne	.LBB18_10
# BB#11:                                # %if.end24
	movq	%r14, %rdi
	popq	%rbx
	popq	%r14
	popq	%r15
	jmp	subreg_regno  # TAILCALL
.LBB18_2:                               # %if.then
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.subreg_hard_regno, %rcx
	movq	$796, %rsi              # imm = 0x31C
	jmp	.LBB18_3
.LBB18_5:                               # %if.then9
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.subreg_hard_regno, %rcx
	movq	$799, %rsi              # imm = 0x31F
	jmp	.LBB18_3
.LBB18_8:                               # %if.then15
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.subreg_hard_regno, %rcx
	movq	$801, %rsi              # imm = 0x321
	jmp	.LBB18_3
.LBB18_10:                              # %if.then23
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.subreg_hard_regno, %rcx
	movq	$806, %rsi              # imm = 0x326
.LBB18_3:                               # %if.then
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp118:
	.size	subreg_hard_regno, .Ltmp118-subreg_hard_regno
	.cfi_endproc

	.globl	gen_lowpart_common
	.align	16, 0x90
	.type	gen_lowpart_common,@function
gen_lowpart_common:                     # @gen_lowpart_common
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
	subq	$176, %rsp
.Ltmp130:
	.cfi_def_cfa_offset 224
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
	movq	%rsi, %r15
	movq	%rdi, %r14
	movabsq	$4294967295, %r8        # imm = 0xFFFFFFFF
	movq	%r14, %rax
	andq	%r8, %rax
	movq	$32, %rcx
	movq	%r14, %r12
	movl	(%r15), %edi
	shlq	%cl, %r12
	movq	$32, %rcx
	sarq	%cl, %r12
	movq	$16, %rcx
	movq	%rdi, %r9
	shrq	%cl, %r9
	andq	$255, %r9
	cmpq	%rax, %r9
	jne	.LBB19_2
# BB#1:
	movq	%r15, %rax
	jmp	.LBB19_69
.LBB19_2:                               # %if.end
	movzbl	mode_size(%r12), %r10d
	movzbl	mode_size(%r9), %r11d
	testq	%r9, %r9
	je	.LBB19_4
# BB#3:                                 # %land.lhs.true
	movl	target_flags(%rip), %ebx
	movq	$23, %rcx
	shrq	%cl, %rbx
	andq	$4, %rbx
	leaq	4(%rbx), %r13
	leaq	3(%rbx,%r10), %rax
	cqto
	idivq	%r13
	movq	%rax, %rsi
	movq	$32, %rcx
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	leaq	3(%rbx,%r11), %rax
	cqto
	idivq	%r13
	movq	%rax, %rdx
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	xorq	%rax, %rax
	cmpq	%rdx, %rsi
	jg	.LBB19_69
.LBB19_4:                               # %if.end28
	movl	mode_class(,%r12,4), %esi
	cmpq	$2, %rsi
	jne	.LBB19_7
# BB#5:                                 # %land.lhs.true33
	testq	$16711680, %rdi         # imm = 0xFF0000
	je	.LBB19_7
# BB#6:                                 # %land.lhs.true33
	xorq	%rax, %rax
	cmpq	%r11, %r10
	ja	.LBB19_69
.LBB19_7:                               # %if.end43
	movq	%rdi, %rax
	andq	$65534, %rax            # imm = 0xFFFE
	cmpq	$120, %rax
	jne	.LBB19_15
# BB#8:                                 # %land.lhs.true55
	movq	%rsi, %rax
	orq	$2, %rax
	cmpq	$3, %rax
	jne	.LBB19_15
# BB#9:                                 # %if.then65
	movq	8(%r15), %rcx
	movzbl	2(%rcx), %eax
	andq	%r14, %r8
	cmpq	%r8, %rax
	jne	.LBB19_11
# BB#10:
	movq	%rcx, %rax
	jmp	.LBB19_69
.LBB19_15:                              # %if.else117
	movq	%rdi, %rdx
	andq	$65535, %rdx            # imm = 0xFFFF
	leaq	-61(%rdx), %rcx
	andq	%r8, %rcx
	cmpq	$4, %rcx
	ja	.LBB19_18
# BB#16:                                # %if.else117
	movq	$1, %rax
	shlq	%cl, %rax
	testq	$21, %rax
	je	.LBB19_18
# BB#17:                                # %if.then132
	xorq	%rcx, %rcx
	movq	%r14, %rdi
	movq	%r15, %rsi
	movq	%r9, %rdx
	callq	simplify_gen_subreg
	jmp	.LBB19_69
.LBB19_18:                              # %if.else137
	movq	%rsi, %rcx
	orq	$2, %rcx
	cmpq	$3, %rcx
	jne	.LBB19_39
# BB#19:                                # %land.lhs.true147
	movq	%rdi, %rax
	andq	$16777214, %rax         # imm = 0xFFFFFE
	cmpq	$54, %rax
	jne	.LBB19_39
# BB#20:                                # %if.then163
	movzbl	mode_bitsize(%r12,%r12), %ecx
	cmpq	$127, %rcx
	jbe	.LBB19_22
# BB#21:
	movq	%r15, %rax
	jmp	.LBB19_69
.LBB19_39:                              # %if.else223
	cmpq	$2, %rsi
	jne	.LBB19_51
# BB#40:                                # %land.lhs.true228
	movzbl	mode_bitsize(%r12,%r12), %eax
	cmpq	$32, %rax
	jne	.LBB19_45
# BB#41:                                # %land.lhs.true234
	cmpq	$54, %rdx
	jne	.LBB19_44
# BB#42:                                # %if.then239
	movq	8(%r15), %rsi
	leaq	136(%rsp), %rdi
	callq	ereal_from_float
	movq	152(%rsp), %rax
	movq	%rax, 16(%rsp)
	movq	136(%rsp), %rax
	movq	144(%rsp), %rcx
	jmp	.LBB19_43
.LBB19_11:                              # %if.else
	movzbl	mode_size(%rax), %eax
	cmpq	%rax, %r10
	jae	.LBB19_13
# BB#12:                                # %if.then90
	movq	%r14, %rdi
	movq	%rcx, %rsi
	callq	gen_lowpart_common
	jmp	.LBB19_69
.LBB19_13:                              # %if.else95
	xorq	%rax, %rax
	cmpq	%r11, %r10
	jae	.LBB19_69
# BB#14:                                # %if.then107
	andq	$65535, %rdi            # imm = 0xFFFF
	movq	%r14, %rsi
	movq	%rcx, %rdx
	callq	gen_rtx_fmt_e
	jmp	.LBB19_69
.LBB19_22:                              # %if.else170
	xorq	%rax, %rax
	cmpq	$64, %rcx
	ja	.LBB19_69
# BB#23:                                # %if.else177
	jne	.LBB19_31
# BB#24:                                # %if.then183
	cmpq	$54, %rdx
	jne	.LBB19_26
# BB#25:
	movq	%r15, %rax
	jmp	.LBB19_69
.LBB19_44:                              # %if.else244
	cmpq	$2, %rsi
	jne	.LBB19_51
.LBB19_45:                              # %land.lhs.true249
	cmpq	$64, %rax
	jne	.LBB19_51
# BB#46:                                # %land.lhs.true249
	andq	$16777214, %rdi         # imm = 0xFFFFFE
	andq	%r8, %rdi
	cmpq	$54, %rdi
	jne	.LBB19_51
# BB#47:                                # %if.then271
	cmpq	$54, %rdx
	je	.LBB19_48
# BB#49:                                # %if.then271
	addq	$16, %r15
	jmp	.LBB19_50
.LBB19_51:                              # %if.else297
	xorq	%rax, %rax
	cmpq	$3, %rcx
	jne	.LBB19_69
# BB#52:                                # %land.lhs.true307
	cmpq	$55, %rdx
	jne	.LBB19_69
# BB#53:                                # %land.lhs.true312
	movl	mode_class(,%r9,4), %ecx
	cmpq	$2, %rcx
	jne	.LBB19_69
# BB#54:                                # %if.then320
	movq	32(%r15), %rax
	movq	%rax, 88(%rsp)
	movq	16(%r15), %rcx
	movq	24(%r15), %rdx
	movzbl	mode_bitsize(%r9,%r9), %eax
	movq	%rdx, 80(%rsp)
	movq	%rcx, 72(%rsp)
	cmpq	$63, %rax
	jg	.LBB19_57
# BB#55:                                # %if.then320
	cmpq	$32, %rax
	jne	.LBB19_66
# BB#56:                                # %sw.bb
	movq	88(%rsp), %rax
	movq	%rax, 16(%rsp)
	movq	72(%rsp), %rax
	movq	80(%rsp), %rcx
	movq	%rcx, 8(%rsp)
	movq	%rax, (%rsp)
	callq	etarsingle
	movq	%rax, 32(%rsp)
	movq	$0, 56(%rsp)
	movq	$0, 48(%rsp)
	movq	$0, 40(%rsp)
	jmp	.LBB19_68
.LBB19_48:
	addq	$8, %r15
.LBB19_50:                              # %if.then271
	movq	(%r15), %rax
	movq	%rax, 96(%rsp)
	leaq	112(%rsp), %rdi
	leaq	96(%rsp), %rsi
	callq	ereal_from_double
	movq	128(%rsp), %rax
	movq	%rax, 16(%rsp)
	movq	112(%rsp), %rax
	movq	120(%rsp), %rcx
.LBB19_43:                              # %return
	movq	%rcx, 8(%rsp)
	movq	%rax, (%rsp)
	movq	%r14, %rdi
	callq	immed_real_const_1
	jmp	.LBB19_69
.LBB19_31:                              # %if.else192
	leaq	8(%r15), %rbx
	cmpq	$54, %rdx
	movq	%rbx, %rax
	je	.LBB19_33
# BB#32:                                # %if.else192
	leaq	16(%r15), %rax
.LBB19_33:                              # %if.else192
	movq	(%rax), %rdi
	movq	%r14, %rsi
	callq	trunc_int_for_mode
	movzbl	(%r15), %ecx
	movq	%rax, %rdx
	cmpq	$54, %rcx
	jne	.LBB19_35
# BB#34:                                # %land.lhs.true212
	movq	(%rbx), %rax
	cmpq	%rdx, %rax
	movq	%r15, %rax
	je	.LBB19_69
.LBB19_35:                              # %cond.false219
	movq	%rdx, 160(%rsp)
	leaq	64(%rdx), %rax
	cmpq	$128, %rax
	ja	.LBB19_37
# BB#36:                                # %if.then.i
	movq	const_int_rtx+512(,%rdx,8), %rax
	jmp	.LBB19_69
.LBB19_57:                              # %if.then320
	cmpq	$64, %rax
	je	.LBB19_67
# BB#58:                                # %if.then320
	cmpq	$96, %rax
	jne	.LBB19_59
# BB#62:                                # %sw.bb356
	movzbl	target_flags+3(%rip), %eax
	testq	$1, %rax
	je	.LBB19_63
# BB#64:                                # %cond.true382
	movq	88(%rsp), %rax
	movq	%rax, 16(%rsp)
	movq	72(%rsp), %rax
	movq	80(%rsp), %rcx
	movq	%rcx, 8(%rsp)
	leaq	32(%rsp), %rdi
	movq	%rax, (%rsp)
	callq	etartdouble
	movq	$0, 56(%rsp)
	jmp	.LBB19_68
.LBB19_26:                              # %cond.false
	movq	16(%r15), %rdx
	movq	%rdx, 168(%rsp)
	leaq	64(%rdx), %rax
	cmpq	$128, %rax
	ja	.LBB19_28
# BB#27:                                # %if.then.i539
	movq	const_int_rtx+512(,%rdx,8), %rax
	jmp	.LBB19_69
.LBB19_37:                              # %if.end.i
	movq	const_int_htab(%rip), %rdi
	leaq	160(%rsp), %rsi
	movq	$1, %rcx
	callq	htab_find_slot_with_hash
	movq	%rax, %rbx
	movq	(%rbx), %rax
	testq	%rax, %rax
	jne	.LBB19_69
# BB#38:                                # %if.then4.i
	movq	160(%rsp), %rdx
	jmp	.LBB19_30
.LBB19_67:                              # %sw.bb345
	movq	88(%rsp), %rax
	movq	%rax, 16(%rsp)
	movq	72(%rsp), %rax
	movq	80(%rsp), %rcx
	movq	%rcx, 8(%rsp)
	leaq	32(%rsp), %rdi
	movq	%rax, (%rsp)
	callq	etardouble
	movq	$0, 56(%rsp)
	movq	$0, 48(%rsp)
	jmp	.LBB19_68
.LBB19_59:                              # %if.then320
	cmpq	$128, %rax
	jne	.LBB19_66
# BB#60:                                # %sw.bb394
	movzbl	target_flags+3(%rip), %eax
	testq	$1, %rax
	je	.LBB19_61
# BB#65:                                # %cond.true416
	movq	88(%rsp), %rax
	movq	%rax, 16(%rsp)
	movq	72(%rsp), %rax
	movq	80(%rsp), %rcx
	movq	%rcx, 8(%rsp)
	leaq	32(%rsp), %rdi
	movq	%rax, (%rsp)
	callq	etartdouble
	jmp	.LBB19_68
.LBB19_28:                              # %if.end.i543
	movq	const_int_htab(%rip), %rdi
	leaq	168(%rsp), %rsi
	movq	$1, %rcx
	callq	htab_find_slot_with_hash
	movq	%rax, %rbx
	movq	(%rbx), %rax
	testq	%rax, %rax
	jne	.LBB19_69
# BB#29:                                # %if.then4.i545
	movq	168(%rsp), %rdx
.LBB19_30:                              # %return
	movq	$54, %rdi
	xorq	%rsi, %rsi
	callq	gen_rtx_fmt_w
	movq	%rax, (%rbx)
	jmp	.LBB19_69
.LBB19_63:                              # %cond.true372
	movq	88(%rsp), %rax
	movq	%rax, 16(%rsp)
	movq	72(%rsp), %rax
	movq	80(%rsp), %rcx
	movq	%rcx, 8(%rsp)
	leaq	32(%rsp), %rdi
	movq	%rax, (%rsp)
	callq	etarldouble
	movq	$0, 56(%rsp)
	jmp	.LBB19_68
.LBB19_61:                              # %cond.true408
	movq	88(%rsp), %rax
	movq	%rax, 16(%rsp)
	movq	72(%rsp), %rax
	movq	80(%rsp), %rcx
	movq	%rcx, 8(%rsp)
	leaq	32(%rsp), %rdi
	movq	%rax, (%rsp)
	callq	etarldouble
.LBB19_68:                              # %sw.epilog
	movq	32(%rsp), %rax
	movq	40(%rsp), %rdi
	movq	$32, %rcx
	shlq	%cl, %rdi
	movq	56(%rsp), %rsi
	movq	48(%rsp), %rdx
	orq	%rax, %rdi
	movq	$32, %rcx
	shlq	%cl, %rsi
	orq	%rdx, %rsi
	movq	%r14, %rdx
	callq	immed_double_const
.LBB19_69:                              # %return
	addq	$176, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB19_66:                              # %sw.default
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.gen_lowpart_common, %rcx
	movq	$1075, %rsi             # imm = 0x433
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp136:
	.size	gen_lowpart_common, .Ltmp136-gen_lowpart_common
	.cfi_endproc

	.globl	gen_realpart
	.align	16, 0x90
	.type	gen_realpart,@function
gen_realpart:                           # @gen_realpart
	.cfi_startproc
# BB#0:                                 # %entry
	jmp	gen_lowpart  # TAILCALL
.Ltmp137:
	.size	gen_realpart, .Ltmp137-gen_realpart
	.cfi_endproc

	.globl	gen_lowpart
	.align	16, 0x90
	.type	gen_lowpart,@function
gen_lowpart:                            # @gen_lowpart
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp141:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp142:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp143:
	.cfi_def_cfa_offset 32
.Ltmp144:
	.cfi_offset %rbx, -24
.Ltmp145:
	.cfi_offset %r14, -16
	movq	%rsi, %rbx
	movq	%rdi, %r14
	jmp	.LBB21_1
.LBB21_10:                              # %if.then17
                                        #   in Loop: Header=BB21_1 Depth=1
	movq	$16, %rcx
	shrq	%cl, %rdi
	andq	$255, %rdi
	movq	%rbx, %rsi
	callq	force_reg
	movq	%rax, %rbx
	.align	16, 0x90
.LBB21_1:                               # %entry
                                        # =>This Inner Loop Header: Depth=1
	movq	%r14, %rdi
	movq	%rbx, %rsi
	callq	gen_lowpart_common
	testq	%rax, %rax
	jne	.LBB21_11
# BB#2:                                 # %if.else
                                        #   in Loop: Header=BB21_1 Depth=1
	movl	(%rbx), %edi
	movq	%rdi, %rax
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$70, %rax
	je	.LBB21_10
# BB#3:                                 # %if.else
	cmpq	$66, %rax
	jne	.LBB21_4
# BB#8:                                 # %if.then10
	xorq	%rdx, %rdx
	movq	$1, %rcx
	movq	$1, %r8
	movq	%rbx, %rdi
	movq	%r14, %rsi
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	jmp	adjust_address_1  # TAILCALL
.LBB21_4:                               # %if.else
	cmpq	$61, %rax
	jne	.LBB21_9
# BB#5:                                 # %if.then1
	movq	%rbx, %rdi
	callq	copy_to_reg
	movq	%r14, %rdi
	movq	%rax, %rsi
	callq	gen_lowpart_common
	testq	%rax, %rax
	je	.LBB21_6
.LBB21_11:                              # %return
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.LBB21_9:                               # %if.else22
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.gen_lowpart, %rcx
	movq	$1197, %rsi             # imm = 0x4AD
	jmp	.LBB21_7
.LBB21_6:                               # %if.then5
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.gen_lowpart, %rcx
	movq	$1175, %rsi             # imm = 0x497
.LBB21_7:                               # %if.then5
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp146:
	.size	gen_lowpart, .Ltmp146-gen_lowpart
	.cfi_endproc

	.globl	gen_imagpart
	.align	16, 0x90
	.type	gen_imagpart,@function
gen_imagpart:                           # @gen_imagpart
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rax
.Ltmp148:
	.cfi_def_cfa_offset 16
	movq	$32, %rcx
	movq	%rdi, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movl	target_flags(%rip), %edx
	movzbl	mode_bitsize(%rax,%rax), %eax
	movq	$20, %rcx
	shrq	%cl, %rdx
	andq	$32, %rdx
	addq	$32, %rdx
	movabsq	$4294967264, %rcx       # imm = 0xFFFFFFE0
	andq	%rdx, %rcx
	cmpq	%rcx, %rax
	jae	.LBB22_4
# BB#1:                                 # %land.lhs.true
	movzbl	(%rsi), %eax
	cmpq	$61, %rax
	jne	.LBB22_4
# BB#2:                                 # %land.lhs.true4
	movl	8(%rsi), %eax
	cmpq	$52, %rax
	jbe	.LBB22_3
.LBB22_4:                               # %if.else
	popq	%rax
	jmp	gen_highpart  # TAILCALL
.LBB22_3:                               # %if.then
	movabsq	$.L.str1, %rcx
	xorq	%rax, %rax
	movq	%rcx, %rdi
	callq	internal_error
.Ltmp149:
	.size	gen_imagpart, .Ltmp149-gen_imagpart
	.cfi_endproc

	.globl	gen_highpart
	.align	16, 0x90
	.type	gen_highpart,@function
gen_highpart:                           # @gen_highpart
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rax
.Ltmp151:
	.cfi_def_cfa_offset 16
	movq	$32, %rcx
	movq	%rdi, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movzbl	mode_size(%rax), %r8d
	movl	target_flags(%rip), %eax
	movq	$23, %rcx
	shrq	%cl, %rax
	andq	$4, %rax
	addq	$4, %rax
	movl	(%rsi), %edx
	movabsq	$4294967292, %rcx       # imm = 0xFFFFFFFC
	andq	%rax, %rcx
	cmpq	%rcx, %r8
	jbe	.LBB23_4
# BB#1:                                 # %land.lhs.true
	movq	$16, %rcx
	movq	%rdx, %rax
	shrq	%cl, %rax
	andq	$255, %rax
	movzbl	mode_unit_size(%rax), %eax
	cmpq	%rax, %r8
	jne	.LBB23_2
.LBB23_4:                               # %if.end
	movq	$16, %rcx
	shrq	%cl, %rdx
	andq	$255, %rdx
	movzbl	mode_size(%rdx), %ecx
	cmpq	%r8, %rcx
	jb	.LBB23_5
# BB#6:                                 # %subreg_highpart_offset.exit
	subq	%r8, %rcx
	jns	.LBB23_8
# BB#7:                                 # %subreg_highpart_offset.exit
	xorq	%rcx, %rcx
.LBB23_8:                               # %subreg_highpart_offset.exit
	callq	simplify_gen_subreg
	testq	%rax, %rax
	je	.LBB23_11
# BB#9:                                 # %land.lhs.true16
	movzbl	(%rax), %ecx
	cmpq	$66, %rcx
	jne	.LBB23_12
# BB#10:                                # %if.end23
	movq	%rax, %rdi
	callq	validize_mem
	testq	%rax, %rax
	je	.LBB23_11
.LBB23_12:                              # %if.end26
	popq	%rdx
	retq
.LBB23_2:                               # %if.then
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.gen_highpart, %rcx
	movq	$1215, %rsi             # imm = 0x4BF
	jmp	.LBB23_3
.LBB23_5:                               # %if.then.i
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.subreg_highpart_offset, %rcx
	movq	$1278, %rsi             # imm = 0x4FE
	jmp	.LBB23_3
.LBB23_11:                              # %if.then25
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.gen_highpart, %rcx
	movq	$1227, %rsi             # imm = 0x4CB
.LBB23_3:                               # %if.then
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp152:
	.size	gen_highpart, .Ltmp152-gen_highpart
	.cfi_endproc

	.globl	subreg_realpart_p
	.align	16, 0x90
	.type	subreg_realpart_p,@function
subreg_realpart_p:                      # @subreg_realpart_p
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rax
.Ltmp154:
	.cfi_def_cfa_offset 16
	movzbl	(%rdi), %eax
	cmpq	$63, %rax
	jne	.LBB24_2
# BB#1:                                 # %if.end
	movq	8(%rdi), %rax
	movzbl	2(%rax), %eax
	movl	16(%rdi), %ecx
	movzbl	mode_unit_size(%rax), %eax
	cmpq	%rax, %rcx
	sbbq	%rax, %rax
	andq	$1, %rax
	popq	%rdx
	retq
.LBB24_2:                               # %if.then
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.subreg_realpart_p, %rcx
	movq	$1148, %rsi             # imm = 0x47C
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp155:
	.size	subreg_realpart_p, .Ltmp155-subreg_realpart_p
	.cfi_endproc

	.globl	adjust_address_1
	.align	16, 0x90
	.type	adjust_address_1,@function
adjust_address_1:                       # @adjust_address_1
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp162:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp163:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp164:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp165:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp166:
	.cfi_def_cfa_offset 48
	subq	$112, %rsp
.Ltmp167:
	.cfi_def_cfa_offset 160
.Ltmp168:
	.cfi_offset %rbx, -48
.Ltmp169:
	.cfi_offset %r12, -40
.Ltmp170:
	.cfi_offset %r13, -32
.Ltmp171:
	.cfi_offset %r14, -24
.Ltmp172:
	.cfi_offset %r15, -16
	movq	%rdi, %rbx
	movq	16(%rbx), %rax
	movq	8(%rbx), %rdi
	movq	%rbx, %r12
	movq	%r8, %r15
	movq	%rcx, 24(%rsp)          # 8-byte Spill
	movq	%rdx, %r13
	movq	%rsi, 16(%rsp)          # 8-byte Spill
	testq	%rax, %rax
	je	.LBB25_1
# BB#2:                                 # %cond.true11
	movq	16(%rax), %r14
	movl	32(%rax), %eax
	movq	%rax, 32(%rsp)          # 8-byte Spill
	jmp	.LBB25_3
.LBB25_1:
	movq	$8, 32(%rsp)            # 8-byte Folded Spill
	xorq	%r14, %r14
.LBB25_3:                               # %cond.end16
	callq	copy_rtx
	movq	%rax, %rbx
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %r15
	movq	%rax, %r15
	je	.LBB25_11
# BB#4:                                 # %if.then
	movzbl	2(%r12), %edi
	cmpq	$51, %rdi
	je	.LBB25_9
# BB#5:                                 # %land.lhs.true
	movzbl	(%rbx), %eax
	cmpq	$135, %rax
	jne	.LBB25_9
# BB#6:                                 # %land.lhs.true
	testq	%r13, %r13
	js	.LBB25_9
# BB#7:                                 # %land.lhs.true24
	callq	get_mode_alignment
	movabsq	$4294967288, %rdx       # imm = 0xFFFFFFF8
	andq	%rax, %rdx
	movq	$3, %rcx
	shrq	%cl, %rdx
	cmpq	%r13, %rdx
	jbe	.LBB25_9
# BB#8:                                 # %if.then31
	movl	target_flags(%rip), %r15d
	movq	$25, %rcx
	movq	8(%rbx), %rax
	movq	%rax, 8(%rsp)           # 8-byte Spill
	movq	16(%rbx), %rdi
	shrq	%cl, %r15
	andq	$1, %r15
	orq	$4, %r15
	movq	%r13, %rsi
	callq	plus_constant_wide
	movq	$135, %rdi
	movq	%r15, %rsi
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	movq	8(%rsp), %rdx           # 8-byte Reload
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	jmp	.LBB25_10
.LBB25_9:                               # %if.else
	movq	%rbx, %rdi
	movq	%r13, %rsi
	callq	plus_constant_wide
.LBB25_10:                              # %if.end43
	movq	%rax, %rbx
.LBB25_11:                              # %if.end43
	movq	%r12, %rdi
	movq	%rdi, 40(%rsp)          # 8-byte Spill
	movq	16(%rsp), %rsi          # 8-byte Reload
	movq	%rbx, %rdx
	movq	24(%rsp), %rcx          # 8-byte Reload
	callq	change_address_1
	xorq	%r12, %r12
	testq	%r14, %r14
	je	.LBB25_17
# BB#12:                                # %if.then46
	movq	8(%r14), %rcx
	leaq	(%rcx,%r13), %rdx
	movq	%rdx, 48(%rsp)
	leaq	64(%rcx,%r13), %rcx
	cmpq	$128, %rcx
	ja	.LBB25_14
# BB#13:                                # %if.then.i
	movq	const_int_rtx+512(,%rdx,8), %r12
	jmp	.LBB25_17
.LBB25_14:                              # %if.end.i
	movq	%rax, %r14
	movq	const_int_htab(%rip), %rdi
	leaq	48(%rsp), %rsi
	movq	$1, %rcx
	callq	htab_find_slot_with_hash
	movq	%rax, %rbx
	movq	(%rbx), %r12
	testq	%r12, %r12
	jne	.LBB25_16
# BB#15:                                # %if.then4.i
	movq	48(%rsp), %rdx
	movq	$54, %rdi
	xorq	%rsi, %rsi
	callq	gen_rtx_fmt_w
	movq	%rax, %r12
	movq	%r12, (%rbx)
.LBB25_16:                              # %if.end6.i
	movq	%r14, %rax
.LBB25_17:                              # %if.end50
	testq	%r13, %r13
	je	.LBB25_18
# BB#19:                                # %if.then53
	movq	32(%rsp), %r14          # 8-byte Reload
	andq	%r15, %r14
	movq	%r13, %rdx
	negq	%rdx
	andq	%r13, %rdx
	movq	$3, %rcx
	shlq	%cl, %rdx
	cmpq	%rdx, %r14
	jb	.LBB25_21
# BB#20:                                # %if.then53
	movq	%rdx, %r14
	jmp	.LBB25_21
.LBB25_18:
	movq	32(%rsp), %r14          # 8-byte Reload
.LBB25_21:                              # %if.end67
	movzbl	2(%rax), %ecx
	cmpq	$51, %rcx
	jne	.LBB25_22
# BB#27:                                # %if.else80
	movq	%rax, 32(%rsp)          # 8-byte Spill
	movq	40(%rsp), %rcx          # 8-byte Reload
	movq	16(%rcx), %rax
	testq	%rax, %rax
	je	.LBB25_29
# BB#28:                                # %cond.true86
	xorq	%rbx, %rbx
	cmpq	$0, 24(%rax)
	jne	.LBB25_37
	jmp	.LBB25_45
.LBB25_22:                              # %if.then73
	movq	%rax, 32(%rsp)          # 8-byte Spill
	movzbl	mode_size(%rcx), %edx
	movq	%rdx, 56(%rsp)
	leaq	64(%rdx), %rax
	cmpq	$128, %rax
	ja	.LBB25_24
# BB#23:                                # %if.then.i219
	movq	const_int_rtx+512(,%rdx,8), %rbx
	jmp	.LBB25_44
.LBB25_24:                              # %if.end.i223
	movq	%r15, %r13
	movq	const_int_htab(%rip), %rdi
	leaq	56(%rsp), %rsi
	movq	$1, %rcx
	callq	htab_find_slot_with_hash
	movq	%rax, %r15
	movq	(%r15), %rbx
	testq	%rbx, %rbx
	jne	.LBB25_26
# BB#25:                                # %if.then4.i225
	movq	56(%rsp), %rdx
	movq	$54, %rdi
	xorq	%rsi, %rsi
	callq	gen_rtx_fmt_w
	movq	%rax, %rbx
	movq	%rbx, (%r15)
.LBB25_26:                              # %if.end6.i226
	movq	%r13, %r15
	jmp	.LBB25_44
.LBB25_29:                              # %cond.false92
	movzbl	2(%rcx), %eax
	xorq	%rbx, %rbx
	cmpq	$51, %rax
	je	.LBB25_45
# BB#30:                                # %cond.true98
	movzbl	mode_size(%rax), %edx
	movq	%rdx, 72(%rsp)
	leaq	64(%rdx), %rax
	cmpq	$128, %rax
	ja	.LBB25_32
# BB#31:                                # %if.then.i232
	movq	const_int_rtx+512(,%rdx,8), %rax
	jmp	.LBB25_35
.LBB25_32:                              # %if.end.i236
	movq	const_int_htab(%rip), %rdi
	leaq	72(%rsp), %rsi
	movq	$1, %rcx
	callq	htab_find_slot_with_hash
	movq	%rax, %r15
	movq	(%r15), %rax
	testq	%rax, %rax
	jne	.LBB25_34
# BB#33:                                # %if.then4.i238
	movq	72(%rsp), %rdx
	movq	$54, %rdi
	xorq	%rsi, %rsi
	callq	gen_rtx_fmt_w
	movq	%rax, (%r15)
.LBB25_34:                              # %if.end6.i239
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
.LBB25_35:                              # %gen_rtx_CONST_INT.exit241
	movq	40(%rsp), %rcx          # 8-byte Reload
	testq	%rax, %rax
	je	.LBB25_45
# BB#36:                                # %if.then108
	movq	16(%rcx), %rax
	testq	%rax, %rax
	je	.LBB25_38
.LBB25_37:                              # %cond.true114
	movq	24(%rax), %rdi
.LBB25_43:                              # %cond.end136
	negq	%r13
	movq	%r13, %rsi
	callq	plus_constant_wide
	movq	%rax, %rbx
.LBB25_44:                              # %if.end141
	movq	40(%rsp), %rcx          # 8-byte Reload
.LBB25_45:                              # %if.end141
	movq	16(%rcx), %rdx
	xorq	%rax, %rax
	testq	%rdx, %rdx
	movabsq	$0, %rcx
	je	.LBB25_47
# BB#46:                                # %cond.false160
	movq	(%rdx), %rax
	movq	8(%rdx), %rcx
.LBB25_47:                              # %cond.end164
	testq	%rax, %rax
	jne	.LBB25_56
# BB#48:                                # %cond.end164
	testq	%rcx, %rcx
	jne	.LBB25_56
# BB#49:                                # %cond.end164
	testq	%r12, %r12
	jne	.LBB25_56
# BB#50:                                # %land.lhs.true4.i
	testq	%rbx, %rbx
	je	.LBB25_54
# BB#51:                                # %lor.lhs.false.i
	movq	32(%rsp), %rdx          # 8-byte Reload
	movzbl	2(%rdx), %edx
	cmpq	$51, %rdx
	je	.LBB25_56
# BB#52:                                # %land.lhs.true7.i
	movzbl	mode_size(%rdx), %edx
	movq	8(%rbx), %rsi
	cmpq	%rsi, %rdx
	jne	.LBB25_56
# BB#53:                                # %land.lhs.true7.i
	xorq	%rdx, %rdx
	andq	%r14, %r15
	jmp	.LBB25_55
.LBB25_54:                              # %land.lhs.true11.i
	andq	%r14, %r15
	xorq	%rdx, %rdx
.LBB25_55:                              # %land.lhs.true11.i
	cmpq	$8, %r15
	je	.LBB25_58
.LBB25_56:                              # %if.end.i258
	movq	%rax, 72(%rsp)
	movq	%rcx, 80(%rsp)
	movq	mem_attrs_htab(%rip), %rdi
	movq	%r12, 88(%rsp)
	movq	%rbx, 96(%rsp)
	movl	%r14d, 104(%rsp)
	leaq	72(%rsp), %rsi
	movq	$1, %rdx
	callq	htab_find_slot
	movq	%rax, %rbx
	movq	(%rbx), %rdx
	testq	%rdx, %rdx
	jne	.LBB25_58
# BB#57:                                # %if.then21.i
	movq	$40, %rdi
	callq	ggc_alloc
	movq	%rax, (%rbx)
	movq	104(%rsp), %rcx
	movq	%rcx, 32(%rax)
	movq	96(%rsp), %rcx
	movq	%rcx, 24(%rax)
	movq	88(%rsp), %rcx
	movq	%rcx, 16(%rax)
	movq	72(%rsp), %rcx
	movq	80(%rsp), %rdx
	movq	%rdx, 8(%rax)
	movq	%rcx, (%rax)
	movq	(%rbx), %rdx
.LBB25_58:                              # %get_mem_attrs.exit
	movq	32(%rsp), %rax          # 8-byte Reload
	movq	%rdx, 16(%rax)
	addq	$112, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB25_38:                              # %cond.false119
	movzbl	2(%rcx), %eax
	xorq	%rdi, %rdi
	cmpq	$51, %rax
	je	.LBB25_43
# BB#39:                                # %cond.true125
	movzbl	mode_size(%rax), %edx
	movq	%rdx, 64(%rsp)
	leaq	64(%rdx), %rax
	cmpq	$128, %rax
	ja	.LBB25_41
# BB#40:                                # %if.then.i245
	movq	const_int_rtx+512(,%rdx,8), %rdi
	jmp	.LBB25_43
.LBB25_41:                              # %if.end.i249
	movq	const_int_htab(%rip), %rdi
	leaq	64(%rsp), %rsi
	movq	$1, %rcx
	callq	htab_find_slot_with_hash
	movq	%rax, %rbx
	movq	(%rbx), %rdi
	testq	%rdi, %rdi
	jne	.LBB25_43
# BB#42:                                # %if.then4.i251
	movq	64(%rsp), %rdx
	movq	$54, %rdi
	xorq	%rsi, %rsi
	callq	gen_rtx_fmt_w
	movq	%rax, %rdi
	movq	%rdi, (%rbx)
	jmp	.LBB25_43
.Ltmp173:
	.size	adjust_address_1, .Ltmp173-adjust_address_1
	.cfi_endproc

	.globl	subreg_highpart_offset
	.align	16, 0x90
	.type	subreg_highpart_offset,@function
subreg_highpart_offset:                 # @subreg_highpart_offset
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rax
.Ltmp175:
	.cfi_def_cfa_offset 16
	movq	$32, %rcx
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	movzbl	mode_size(%rsi), %eax
	movq	$32, %rcx
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	movzbl	mode_size(%rdi), %ecx
	subq	%rcx, %rax
	jb	.LBB26_4
# BB#1:                                 # %if.end
	testq	%rax, %rax
	jns	.LBB26_3
# BB#2:                                 # %if.end
	xorq	%rax, %rax
.LBB26_3:                               # %if.end
	popq	%rdx
	retq
.LBB26_4:                               # %if.then
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.subreg_highpart_offset, %rcx
	movq	$1278, %rsi             # imm = 0x4FE
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp176:
	.size	subreg_highpart_offset, .Ltmp176-subreg_highpart_offset
	.cfi_endproc

	.globl	gen_highpart_mode
	.align	16, 0x90
	.type	gen_highpart_mode,@function
gen_highpart_mode:                      # @gen_highpart_mode
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rax
.Ltmp178:
	.cfi_def_cfa_offset 16
	movzbl	2(%rdx), %ecx
	movq	%rsi, %rax
	testq	%rcx, %rcx
	je	.LBB27_5
# BB#1:                                 # %if.then
	movabsq	$4294967295, %rsi       # imm = 0xFFFFFFFF
	andq	%rsi, %rax
	cmpq	%rax, %rcx
	jne	.LBB27_2
# BB#4:                                 # %if.end
	movq	%rdx, %rsi
	popq	%rax
	jmp	gen_highpart  # TAILCALL
.LBB27_5:                               # %if.end6
	movq	$32, %rcx
	movq	%rax, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	movzbl	mode_size(%rsi), %r8d
	movq	$32, %rcx
	movq	%rdi, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	movzbl	mode_size(%rsi), %ecx
	subq	%rcx, %r8
	jb	.LBB27_6
# BB#7:                                 # %subreg_highpart_offset.exit
	testq	%r8, %r8
	jns	.LBB27_9
# BB#8:                                 # %subreg_highpart_offset.exit
	xorq	%r8, %r8
.LBB27_9:                               # %subreg_highpart_offset.exit
	movq	%rdx, %rsi
	movq	%rax, %rdx
	movq	%r8, %rcx
	popq	%rax
	jmp	simplify_gen_subreg  # TAILCALL
.LBB27_2:                               # %if.then5
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.gen_highpart_mode, %rcx
	movq	$1241, %rsi             # imm = 0x4D9
	jmp	.LBB27_3
.LBB27_6:                               # %if.then.i
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.subreg_highpart_offset, %rcx
	movq	$1278, %rsi             # imm = 0x4FE
.LBB27_3:                               # %if.then5
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp179:
	.size	gen_highpart_mode, .Ltmp179-gen_highpart_mode
	.cfi_endproc

	.globl	subreg_lowpart_p
	.align	16, 0x90
	.type	subreg_lowpart_p,@function
subreg_lowpart_p:                       # @subreg_lowpart_p
	.cfi_startproc
# BB#0:                                 # %entry
	movzbl	(%rdi), %eax
	cmpq	$63, %rax
	movq	$1, %rax
	jne	.LBB28_5
# BB#1:                                 # %if.else
	movq	8(%rdi), %rax
	movzbl	2(%rax), %ecx
	xorq	%rax, %rax
	testq	%rcx, %rcx
	je	.LBB28_5
# BB#2:                                 # %if.end5
	movl	16(%rdi), %eax
	testq	%rax, %rax
	jne	.LBB28_3
# BB#4:                                 # %if.end5
	movabsq	$1, %rax
.LBB28_5:                               # %return
	retq
.LBB28_3:
	movabsq	$0, %rax
	retq
.Ltmp180:
	.size	subreg_lowpart_p, .Ltmp180-subreg_lowpart_p
	.cfi_endproc

	.globl	constant_subword
	.align	16, 0x90
	.type	constant_subword,@function
constant_subword:                       # @constant_subword
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp184:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp185:
	.cfi_def_cfa_offset 24
	subq	$152, %rsp
.Ltmp186:
	.cfi_def_cfa_offset 176
.Ltmp187:
	.cfi_offset %rbx, -24
.Ltmp188:
	.cfi_offset %r14, -16
	movl	target_flags(%rip), %r8d
	movq	%rdx, %r11
	movq	%rsi, %r14
	movq	%r8, %rbx
	andq	$33554432, %rbx         # imm = 0x2000000
	movq	$20, %rcx
	movq	%rbx, %rsi
	shrq	%cl, %rsi
	addq	$32, %rsi
	movq	$32, %rcx
	shlq	%cl, %r11
	movq	$32, %rcx
	sarq	%cl, %r11
	movl	mode_class(,%r11,4), %r10d
	movq	$64, %rax
	xorq	%rdx, %rdx
	idivq	%rsi
	movq	%rax, %r9
	cmpq	$1, %r10
	jne	.LBB29_10
# BB#1:                                 # %land.lhs.true
	movzbl	mode_size(%r11), %eax
	movabsq	$4286578688, %rcx       # imm = 0xFF800000
	andq	%rcx, %rbx
	movq	$23, %rcx
	shrq	%cl, %rbx
	addq	$4, %rbx
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rbx, %rcx
	cmpq	%rcx, %rax
	movabsq	$0, %rcx
	movabsq	$1, %rdx
	movq	%rcx, %rax
	jne	.LBB29_3
# BB#2:                                 # %land.lhs.true
	movq	%rdx, %rax
.LBB29_3:                               # %land.lhs.true
	movq	const_int_rtx+512(%rip), %rsi
	cmpq	%rdi, %rsi
	movq	%rcx, %rsi
	jne	.LBB29_5
# BB#4:                                 # %land.lhs.true
	movq	%rdx, %rsi
.LBB29_5:                               # %land.lhs.true
	cmpq	$1, %r10
	je	.LBB29_7
# BB#6:                                 # %land.lhs.true
	movq	%rdx, %rcx
.LBB29_7:                               # %land.lhs.true
	orq	%rsi, %rax
	movq	%rdi, %rax
	jne	.LBB29_9
# BB#8:                                 # %land.lhs.true
	movabsq	$0, %rax
.LBB29_9:                               # %land.lhs.true
	je	.LBB29_53
	jmp	.LBB29_69
.LBB29_10:                              # %land.lhs.true13
	cmpq	$2, %r10
	jne	.LBB29_47
# BB#11:                                # %land.lhs.true18
	movzbl	mode_bitsize(%r11,%r11), %eax
	cmpq	$64, %rax
	jne	.LBB29_25
# BB#12:                                # %land.lhs.true24
	movzbl	(%rdi), %ecx
	cmpq	$55, %rcx
	jne	.LBB29_25
# BB#13:                                # %do.body
	movq	32(%rdi), %rax
	movq	%rax, 16(%rsp)
	movq	16(%rdi), %rax
	movq	24(%rdi), %rcx
	movq	%rcx, 8(%rsp)
	leaq	80(%rsp), %rdi
	movq	%rax, (%rsp)
	callq	etardouble
	movzbl	target_flags+3(%rip), %eax
	testq	$2, %rax
	je	.LBB29_14
# BB#18:                                # %if.else
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %r14
	jne	.LBB29_23
# BB#19:                                # %if.then46
	movq	80(%rsp), %rdx
	movq	88(%rsp), %rsi
	movq	$32, %rcx
	shlq	%cl, %rsi
	andq	%rdx, %rax
	orq	%rsi, %rax
	movq	%rax, 112(%rsp)
	leaq	64(%rax), %rcx
	cmpq	$128, %rcx
	ja	.LBB29_21
# BB#20:                                # %if.then.i360
	movq	const_int_rtx+512(,%rax,8), %rax
	jmp	.LBB29_69
.LBB29_25:                              # %land.lhs.true85
	cmpq	$65, %rax
	jb	.LBB29_41
# BB#26:                                # %land.lhs.true91
	movzbl	(%rdi), %eax
	cmpq	$55, %rax
	jne	.LBB29_47
# BB#27:                                # %do.body99
	movq	32(%rdi), %rax
	movq	%rax, 40(%rsp)
	movq	16(%rdi), %rax
	movq	24(%rdi), %rcx
	movq	%rcx, 32(%rsp)
	movq	%rax, 24(%rsp)
	testq	$16777216, %r8          # imm = 0x1000000
	je	.LBB29_28
# BB#29:                                # %cond.true125
	movq	40(%rsp), %rax
	movq	%rax, 16(%rsp)
	movq	24(%rsp), %rax
	movq	32(%rsp), %rcx
	movq	%rcx, 8(%rsp)
	leaq	48(%rsp), %rdi
	movq	%rax, (%rsp)
	callq	etartdouble
	jmp	.LBB29_30
.LBB29_41:                              # %land.lhs.true172
	cmpq	$32, %rax
	jne	.LBB29_47
# BB#42:                                # %land.lhs.true178
	movzbl	(%rdi), %eax
	cmpq	$55, %rax
	jne	.LBB29_47
# BB#43:                                # %do.body185
	movq	32(%rdi), %rax
	movq	%rax, 16(%rsp)
	movq	16(%rdi), %rax
	movq	24(%rdi), %rcx
	movq	%rcx, 8(%rsp)
	movq	%rax, (%rsp)
	callq	etarsingle
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	%rax, 136(%rsp)
	leaq	64(%rax), %rcx
	cmpq	$128, %rcx
	ja	.LBB29_45
# BB#44:                                # %if.then.i399
	movq	const_int_rtx+512(,%rax,8), %rax
	jmp	.LBB29_69
.LBB29_47:                              # %if.end213
	movq	const_int_rtx+512(%rip), %rax
	cmpq	$1, %r10
	je	.LBB29_48
# BB#49:                                # %if.end213
	movabsq	$1, %rcx
	jmp	.LBB29_50
.LBB29_48:
	movabsq	$0, %rcx
.LBB29_50:                              # %if.end213
	cmpq	%rdi, %rax
	movq	%rdi, %rax
	je	.LBB29_52
# BB#51:                                # %if.end213
	movabsq	$0, %rax
.LBB29_52:                              # %if.end213
	je	.LBB29_69
.LBB29_53:                              # %if.end213
	testq	%rcx, %rcx
	jne	.LBB29_69
# BB#54:                                # %lor.lhs.false
	movl	(%rdi), %ebx
	movq	%rbx, %rcx
	andq	$65534, %rcx            # imm = 0xFFFE
	xorq	%rax, %rax
	cmpq	$54, %rcx
	jne	.LBB29_69
# BB#55:                                # %if.end238
	andq	$65535, %rbx            # imm = 0xFFFF
	movq	$32, %rcx
	movq	%r14, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$32, %rcx
	movq	%r9, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	cqto
	idivq	%rsi
	movabsq	$4294967295, %rsi       # imm = 0xFFFFFFFF
	testq	%rsi, %rax
	je	.LBB29_56
# BB#59:                                # %cond.false257
	cmpq	$54, %rbx
	jne	.LBB29_61
# BB#60:                                # %cond.true262
	movq	8(%rdi), %rdi
	movq	$63, %rcx
	sarq	%cl, %rdi
	jmp	.LBB29_62
.LBB29_56:                              # %cond.true242
	cmpq	$54, %rbx
	jne	.LBB29_58
# BB#57:                                # %cond.true247
	movq	8(%rdi), %rdi
	jmp	.LBB29_62
.LBB29_61:                              # %cond.false270
	movq	24(%rdi), %rdi
	jmp	.LBB29_62
.LBB29_58:                              # %cond.false251
	movq	16(%rdi), %rdi
.LBB29_62:                              # %cond.end276
	movq	$20, %rcx
	shrq	%cl, %r8
	andq	$32, %r8
	addq	$32, %r8
	movabsq	$4294967264, %rax       # imm = 0xFFFFFFE0
	andq	%r8, %rax
	cmpq	$63, %rax
	ja	.LBB29_64
# BB#63:                                # %if.then283
	movq	$32, %rcx
	shlq	%cl, %r14
	movq	$32, %rcx
	sarq	%cl, %r14
	movq	$32, %rcx
	shlq	%cl, %r9
	movq	$32, %rcx
	sarq	%cl, %r9
	movq	%r14, %rax
	cqto
	idivq	%r9
	imulq	%rdx, %r8
	andq	%rsi, %r8
	movq	%r8, %rcx
	sarq	%cl, %rdi
.LBB29_64:                              # %if.end289
	movl	word_mode(%rip), %esi
	callq	trunc_int_for_mode
	movq	%rax, 144(%rsp)
	movq	%rax, %rcx
	addq	$64, %rcx
	cmpq	$128, %rcx
	ja	.LBB29_66
# BB#65:                                # %if.then.i412
	movq	const_int_rtx+512(,%rax,8), %rax
	jmp	.LBB29_69
.LBB29_66:                              # %if.end.i416
	movq	const_int_htab(%rip), %rdi
	leaq	144(%rsp), %rsi
	movq	$1, %rcx
	movq	%rax, %rdx
	callq	htab_find_slot_with_hash
	movq	%rax, %rbx
	movq	(%rbx), %rax
	testq	%rax, %rax
	jne	.LBB29_69
# BB#67:                                # %if.then4.i418
	movq	144(%rsp), %rdx
.LBB29_68:                              # %return
	movq	$54, %rdi
	xorq	%rsi, %rsi
	callq	gen_rtx_fmt_w
	movq	%rax, (%rbx)
	jmp	.LBB29_69
.LBB29_14:                              # %if.then34
	movq	$32, %rcx
	shlq	%cl, %r14
	movq	$32, %rcx
	sarq	%cl, %r14
	movslq	80(%rsp,%r14,8), %rdx
	movq	%rdx, 104(%rsp)
	leaq	64(%rdx), %rax
	cmpq	$128, %rax
	ja	.LBB29_16
# BB#15:                                # %if.then.i
	movq	const_int_rtx+512(,%rdx,8), %rax
	jmp	.LBB29_69
.LBB29_21:                              # %if.end.i364
	movq	const_int_htab(%rip), %rdi
	leaq	112(%rsp), %rsi
	movq	$1, %rcx
	callq	htab_find_slot_with_hash
	movq	%rax, %rbx
	movq	(%rbx), %rax
	testq	%rax, %rax
	jne	.LBB29_69
# BB#22:                                # %if.then4.i366
	movq	112(%rsp), %rdx
	jmp	.LBB29_68
.LBB29_28:                              # %cond.true117
	movq	40(%rsp), %rax
	movq	%rax, 16(%rsp)
	movq	24(%rsp), %rax
	movq	32(%rsp), %rcx
	movq	%rcx, 8(%rsp)
	leaq	48(%rsp), %rdi
	movq	%rax, (%rsp)
	callq	etarldouble
.LBB29_30:                              # %cond.end129
	movzbl	target_flags+3(%rip), %eax
	movq	$32, %rcx
	testq	$2, %rax
	je	.LBB29_31
# BB#35:                                # %if.else142
	movq	%r14, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$1, %rax
	jg	.LBB29_40
# BB#36:                                # %if.then151
	movq	$33, %rcx
	shlq	%cl, %r14
	movq	$32, %rcx
	sarq	%cl, %r14
	movq	56(%rsp,%r14,8), %rsi
	movq	$32, %rcx
	movq	48(%rsp,%r14,8), %rdx
	shlq	%cl, %rsi
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rdx, %rax
	orq	%rsi, %rax
	movq	%rax, 128(%rsp)
	leaq	64(%rax), %rcx
	cmpq	$128, %rcx
	ja	.LBB29_38
# BB#37:                                # %if.then.i386
	movq	const_int_rtx+512(,%rax,8), %rax
	jmp	.LBB29_69
.LBB29_31:                              # %if.then135
	shlq	%cl, %r14
	movq	$32, %rcx
	sarq	%cl, %r14
	movslq	48(%rsp,%r14,8), %rdx
	movq	%rdx, 120(%rsp)
	leaq	64(%rdx), %rax
	cmpq	$128, %rax
	ja	.LBB29_33
# BB#32:                                # %if.then.i373
	movq	const_int_rtx+512(,%rdx,8), %rax
	jmp	.LBB29_69
.LBB29_38:                              # %if.end.i390
	movq	const_int_htab(%rip), %rdi
	leaq	128(%rsp), %rsi
	movq	$1, %rcx
	callq	htab_find_slot_with_hash
	movq	%rax, %rbx
	movq	(%rbx), %rax
	testq	%rax, %rax
	jne	.LBB29_69
# BB#39:                                # %if.then4.i392
	movq	128(%rsp), %rdx
	jmp	.LBB29_68
.LBB29_45:                              # %if.end.i403
	movq	const_int_htab(%rip), %rdi
	leaq	136(%rsp), %rsi
	movq	$1, %rcx
	movq	%rax, %rdx
	callq	htab_find_slot_with_hash
	movq	%rax, %rbx
	movq	(%rbx), %rax
	testq	%rax, %rax
	jne	.LBB29_69
# BB#46:                                # %if.then4.i405
	movq	136(%rsp), %rdx
	jmp	.LBB29_68
.LBB29_16:                              # %if.end.i
	movq	const_int_htab(%rip), %rdi
	leaq	104(%rsp), %rsi
	movq	$1, %rcx
	callq	htab_find_slot_with_hash
	movq	%rax, %rbx
	movq	(%rbx), %rax
	testq	%rax, %rax
	jne	.LBB29_69
# BB#17:                                # %if.then4.i
	movq	104(%rsp), %rdx
	jmp	.LBB29_68
.LBB29_33:                              # %if.end.i377
	movq	const_int_htab(%rip), %rdi
	leaq	120(%rsp), %rsi
	movq	$1, %rcx
	callq	htab_find_slot_with_hash
	movq	%rax, %rbx
	movq	(%rbx), %rax
	testq	%rax, %rax
	je	.LBB29_34
.LBB29_69:                              # %return
	addq	$152, %rsp
	popq	%rbx
	popq	%r14
	retq
.LBB29_34:                              # %if.then4.i379
	movq	120(%rsp), %rdx
	jmp	.LBB29_68
.LBB29_23:                              # %if.else73
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.constant_subword, %rcx
	movq	$1374, %rsi             # imm = 0x55E
	jmp	.LBB29_24
.LBB29_40:                              # %if.else165
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.constant_subword, %rcx
	movq	$1403, %rsi             # imm = 0x57B
.LBB29_24:                              # %if.else73
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp189:
	.size	constant_subword, .Ltmp189-constant_subword
	.cfi_endproc

	.globl	operand_subword
	.align	16, 0x90
	.type	operand_subword,@function
operand_subword:                        # @operand_subword
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp196:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp197:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp198:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp199:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp200:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp201:
	.cfi_def_cfa_offset 64
.Ltmp202:
	.cfi_offset %rbx, -48
.Ltmp203:
	.cfi_offset %r12, -40
.Ltmp204:
	.cfi_offset %r13, -32
.Ltmp205:
	.cfi_offset %r14, -24
.Ltmp206:
	.cfi_offset %r15, -16
	movq	%rcx, %rbx
	movq	%rsi, %r8
	movq	%rdi, %r14
	movabsq	$4294967295, %r12       # imm = 0xFFFFFFFF
	testq	%r12, %rbx
	jne	.LBB30_2
# BB#1:                                 # %if.end
	movzbl	2(%r14), %ebx
	testq	%rbx, %rbx
	je	.LBB30_11
.LBB30_2:                               # %if.end3
	movq	%rdx, %r15
	movq	%rbx, %rax
	andq	%r12, %rax
	cmpq	$51, %rax
	je	.LBB30_6
# BB#3:                                 # %land.lhs.true
	movq	$32, %rcx
	movq	%rbx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movzbl	mode_size(%rax), %eax
	movl	target_flags(%rip), %edx
	movq	$23, %rcx
	shrq	%cl, %rdx
	andq	$4, %rdx
	addq	$4, %rdx
	movabsq	$4294967292, %rcx       # imm = 0xFFFFFFFC
	andq	%rdx, %rcx
	xorq	%r13, %r13
	cmpq	%rcx, %rax
	jb	.LBB30_10
# BB#4:                                 # %land.lhs.true11
	leaq	1(%r8), %rcx
	imulq	%rcx, %rdx
	andq	%r12, %rdx
	cmpq	%rax, %rdx
	jbe	.LBB30_6
# BB#5:                                 # %if.then20
	movq	const_int_rtx+512(%rip), %r13
	jmp	.LBB30_10
.LBB30_6:                               # %if.end21
	movzbl	(%r14), %eax
	cmpq	$66, %rax
	jne	.LBB30_13
# BB#7:                                 # %if.then26
	movl	target_flags(%rip), %eax
	movq	$23, %rcx
	shrq	%cl, %rax
	andq	$4, %rax
	addq	$4, %rax
	movl	word_mode(%rip), %esi
	imulq	%r8, %rax
	movq	%r8, 8(%rsp)            # 8-byte Spill
	movabsq	$4294967292, %rdx       # imm = 0xFFFFFFFC
	andq	%rax, %rdx
	xorq	%rcx, %rcx
	movq	$1, %r8
	movq	%r14, %rdi
	callq	adjust_address_1
	movq	%rax, %r13
	testq	%r12, %r15
	je	.LBB30_10
# BB#8:                                 # %if.else
	movl	reload_completed(%rip), %eax
	testq	%rax, %rax
	je	.LBB30_12
# BB#9:                                 # %if.then35
	movq	8(%r13), %rsi
	movl	word_mode(%rip), %edi
	callq	strict_memory_address_p
	xorq	%r13, %r13
	testq	%r12, %rax
	movq	8(%rsp), %r8            # 8-byte Reload
	je	.LBB30_10
.LBB30_13:                              # %if.end48
	movl	target_flags(%rip), %eax
	movq	$23, %rcx
	shrq	%cl, %rax
	movl	word_mode(%rip), %edi
	andq	$4, %rax
	addq	$4, %rax
	imulq	%r8, %rax
	movq	%r14, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	simplify_gen_subreg  # TAILCALL
.LBB30_10:                              # %return
	movq	%r13, %rax
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB30_12:                              # %if.else41
	movq	8(%r13), %rbx
	movq	%rbx, %rdi
	movq	%rbx, %rsi
	callq	update_temp_slot_address
	xorq	%rsi, %rsi
	movq	$1, %rcx
	movq	%r13, %rdi
	movq	%rbx, %rdx
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	change_address_1  # TAILCALL
.LBB30_11:                              # %if.then2
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.operand_subword, %rcx
	movq	$1560, %rsi             # imm = 0x618
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp207:
	.size	operand_subword, .Ltmp207-operand_subword
	.cfi_endproc

	.globl	replace_equiv_address
	.align	16, 0x90
	.type	replace_equiv_address,@function
replace_equiv_address:                  # @replace_equiv_address
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp211:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp212:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp213:
	.cfi_def_cfa_offset 32
.Ltmp214:
	.cfi_offset %rbx, -24
.Ltmp215:
	.cfi_offset %r14, -16
	movq	%rdi, %r14
	movq	8(%r14), %rdi
	movq	%rsi, %rbx
                                        # kill: RSI<def> RBX<kill>
	callq	update_temp_slot_address
	xorq	%rsi, %rsi
	movq	$1, %rcx
	movq	%r14, %rdi
	movq	%rbx, %rdx
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	jmp	change_address_1  # TAILCALL
.Ltmp216:
	.size	replace_equiv_address, .Ltmp216-replace_equiv_address
	.cfi_endproc

	.globl	operand_subword_force
	.align	16, 0x90
	.type	operand_subword_force,@function
operand_subword_force:                  # @operand_subword_force
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp221:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp222:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp223:
	.cfi_def_cfa_offset 32
.Ltmp224:
	.cfi_offset %rbx, -32
.Ltmp225:
	.cfi_offset %r14, -24
.Ltmp226:
	.cfi_offset %r15, -16
	movq	%rdx, %rbx
	movq	%rsi, %r14
	movq	%rdi, %r15
	movq	$1, %rdx
                                        # kill: RDI<def> R15<kill>
                                        # kill: RSI<def> R14<kill>
	movq	%rbx, %rcx
	callq	operand_subword
	testq	%rax, %rax
	jne	.LBB32_8
# BB#1:                                 # %if.end
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %rbx
	je	.LBB32_7
# BB#2:                                 # %if.end
	andq	%rbx, %rax
	cmpq	$51, %rax
	je	.LBB32_7
# BB#3:                                 # %if.then2
	movzbl	(%r15), %eax
	cmpq	$61, %rax
	jne	.LBB32_5
# BB#4:                                 # %if.then4
	movq	%r15, %rdi
	callq	copy_to_reg
	jmp	.LBB32_6
.LBB32_5:                               # %if.else
	movq	%rbx, %rdi
	movq	%r15, %rsi
	callq	force_reg
.LBB32_6:                               # %if.end8
	movq	%rax, %r15
.LBB32_7:                               # %if.end8
	movq	$1, %rdx
	movq	%r15, %rdi
	movq	%r14, %rsi
	movq	%rbx, %rcx
	callq	operand_subword
	testq	%rax, %rax
	je	.LBB32_9
.LBB32_8:                               # %return
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.LBB32_9:                               # %if.then11
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.operand_subword_force, %rcx
	movq	$1622, %rsi             # imm = 0x656
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp227:
	.size	operand_subword_force, .Ltmp227-operand_subword_force
	.cfi_endproc

	.globl	reverse_comparison
	.align	16, 0x90
	.type	reverse_comparison,@function
reverse_comparison:                     # @reverse_comparison
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp230:
	.cfi_def_cfa_offset 16
.Ltmp231:
	.cfi_offset %rbx, -16
	movq	32(%rdi), %rbx
	movzbl	(%rbx), %eax
	cmpq	$47, %rax
	movq	%rbx, %rax
	je	.LBB33_2
# BB#1:                                 # %if.else
	movq	8(%rbx), %rax
	movq	8(%rax), %rax
.LBB33_2:                               # %if.end
	movq	16(%rax), %rax
	movl	(%rax), %edx
	movq	%rdx, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$74, %rcx
	jne	.LBB33_4
# BB#3:                                 # %if.then13
	movq	8(%rax), %rcx
	movq	16(%rax), %rdx
	movq	%rdx, 8(%rax)
	movq	%rcx, 16(%rax)
	popq	%rbx
	retq
.LBB33_4:                               # %if.else26
	movq	$13, %rcx
	shrq	%cl, %rdx
	andq	$2040, %rdx             # imm = 0x7F8
	movq	const_tiny_rtx(%rdx), %rdx
	movq	$74, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movzbl	(%rbx), %ecx
	cmpq	$47, %rcx
	jne	.LBB33_6
# BB#5:                                 # %if.then33
	movq	%rax, 16(%rbx)
	popq	%rbx
	retq
.LBB33_6:                               # %if.else37
	movq	8(%rbx), %rcx
	movq	8(%rcx), %rcx
	movq	%rax, 16(%rcx)
	popq	%rbx
	retq
.Ltmp232:
	.size	reverse_comparison, .Ltmp232-reverse_comparison
	.cfi_endproc

	.globl	set_mem_attributes
	.align	16, 0x90
	.type	set_mem_attributes,@function
set_mem_attributes:                     # @set_mem_attributes
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp239:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp240:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp241:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp242:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp243:
	.cfi_def_cfa_offset 48
	subq	$128, %rsp
.Ltmp244:
	.cfi_def_cfa_offset 176
.Ltmp245:
	.cfi_offset %rbx, -48
.Ltmp246:
	.cfi_offset %r12, -40
.Ltmp247:
	.cfi_offset %r13, -32
.Ltmp248:
	.cfi_offset %r14, -24
.Ltmp249:
	.cfi_offset %r15, -16
	movq	%rdi, %r14
	movq	16(%r14), %rdi
	movq	%rdx, 40(%rsp)          # 8-byte Spill
	movq	%rsi, %r12
	testq	%rdi, %rdi
	je	.LBB34_2
# BB#1:                                 # %cond.end48.thread679
	movq	8(%rdi), %rsi
	movq	16(%rdi), %rcx
	movq	24(%rdi), %rax
.LBB34_9:                               # %cond.true55
	movl	32(%rdi), %ebx
	movq	%rcx, %rdx
	jmp	.LBB34_10
.LBB34_2:                               # %cond.false38
	movzbl	2(%r14), %ecx
	xorq	%rax, %rax
	cmpq	$51, %rcx
	movq	$8, %rbx
	je	.LBB34_3
# BB#4:                                 # %cond.true40
	movzbl	mode_size(%rcx), %edx
	movq	%rdx, 56(%rsp)
	leaq	64(%rdx), %rax
	cmpq	$128, %rax
	ja	.LBB34_6
# BB#5:                                 # %if.then.i
	movq	const_int_rtx+512(,%rdx,8), %rax
.LBB34_3:
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
.LBB34_10:                              # %cond.end61
	testq	%r12, %r12
	je	.LBB34_88
# BB#11:                                # %if.end
	movq	%rsi, 8(%rsp)           # 8-byte Spill
	movq	%rdx, 24(%rsp)          # 8-byte Spill
	movq	%rax, 32(%rsp)          # 8-byte Spill
	movq	%rbx, 48(%rsp)          # 8-byte Spill
	movzbl	16(%r12), %eax
	movzbl	tree_code_type(%rax), %eax
	cmpq	$116, %rax
	movq	%r12, %rbx
	je	.LBB34_14
# BB#12:                                # %cond.end76
	movq	8(%r12), %rbx
	cmpq	$100, %rax
	jne	.LBB34_14
# BB#13:                                # %land.lhs.true
	movq	144(%r12), %rax
	cmpq	%r14, %rax
	je	.LBB34_89
.LBB34_14:                              # %if.end106
	leaq	16(%r12), %r13
	movq	%r12, %rdi
	callq	get_alias_set
	movl	16(%rbx), %esi
	movl	(%r14), %edx
	movq	$16, %rcx
	shlq	%cl, %rsi
	andq	$134217728, %rsi        # imm = 0x8000000
	andq	$-134217729, %rdx       # imm = 0xFFFFFFFFF7FFFFFF
	orq	%rsi, %rdx
	movl	%edx, (%r14)
	movzbl	16(%rbx), %ecx
	addq	$-18, %rcx
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	andq	%r15, %rcx
	cmpq	$5, %rcx
	sbbq	%rsi, %rsi
	andq	$268435456, %rsi        # imm = 0x10000000
	movzbl	lang_hooks+96(%rip), %ecx
	andq	$-268435457, %rdx       # imm = 0xFFFFFFFFEFFFFFFF
	orq	%rsi, %rdx
	movl	%edx, (%r14)
	testq	%rcx, %rcx
	je	.LBB34_15
# BB#16:                                # %land.lhs.true153
	movzbl	17(%rbx), %ecx
	movq	$1, %rdi
	testq	$16, %rcx
	jne	.LBB34_20
# BB#17:                                # %lor.lhs.false159
	movl	(%r13), %esi
	testq	$4096, %rsi             # imm = 0x1000
	jne	.LBB34_20
	jmp	.LBB34_18
.LBB34_15:                              # %if.end106.lor.rhs166_crit_edge
	movl	(%r13), %esi
.LBB34_18:                              # %lor.rhs166
	movq	%rsi, %rcx
	andq	$255, %rcx
	movzbl	tree_code_type(%rcx), %ecx
	xorq	%rdi, %rdi
	cmpq	$116, %rcx
	je	.LBB34_20
# BB#19:                                # %land.rhs
	andq	$512, %rsi              # imm = 0x200
	movq	$9, %rcx
	shrq	%cl, %rsi
	movq	%rsi, %rdi
.LBB34_20:                              # %lor.end181
	movq	$26, %rcx
	shlq	%cl, %rdi
	orq	%rdx, %rdi
	movl	%edi, (%r14)
	movq	40(%rsp), %rsi          # 8-byte Reload
	testq	%r15, %rsi
	jne	.LBB34_22
# BB#21:                                # %lor.lhs.false192
	movl	(%r13), %ecx
	movq	%rcx, %rdx
	andq	$255, %rdx
	movzbl	tree_code_type(%rdx), %edx
	cmpq	$100, %rdx
	jne	.LBB34_26
.LBB34_22:                              # %land.lhs.true202
	movzbl	16(%rbx), %ecx
	addq	$-18, %rcx
	andq	%r15, %rcx
	cmpq	$5, %rcx
	jb	.LBB34_24
# BB#23:                                # %if.then237
	movabsq	$2147483648, %rcx       # imm = 0x80000000
	orq	%rcx, %rdi
	movl	%edi, (%r14)
.LBB34_24:                              # %if.end241
	testq	%r15, %rsi
	jne	.LBB34_28
# BB#25:                                # %if.end241.lor.lhs.false243_crit_edge
	movl	(%r13), %ecx
.LBB34_26:                              # %lor.lhs.false243
	andq	$255, %rcx
	cmpq	$41, %rcx
	je	.LBB34_28
# BB#27:                                # %lor.lhs.false250
	movzbl	18(%rbx), %ecx
	testq	$2, %rcx
	je	.LBB34_30
.LBB34_28:                              # %if.then256
	movl	64(%rbx), %edx
	movq	48(%rsp), %rcx          # 8-byte Reload
	andq	%r15, %rcx
	cmpq	%rdx, %rcx
	ja	.LBB34_30
# BB#29:                                # %if.then256
	movq	%rdx, 48(%rsp)          # 8-byte Spill
.LBB34_30:                              # %if.then256
	movq	%rax, 16(%rsp)          # 8-byte Spill
	movq	40(%rbx), %rdi
	testq	%rdi, %rdi
	je	.LBB34_36
# BB#31:                                # %land.lhs.true270
	movq	$1, %rsi
	callq	host_integerp
	testq	%r15, %rax
	je	.LBB34_36
# BB#32:                                # %if.then275
	movq	40(%rbx), %rdi
	movq	$1, %rsi
	callq	tree_low_cst
	movq	%rax, 64(%rsp)
	movq	%rax, %rcx
	addq	$64, %rcx
	cmpq	$128, %rcx
	ja	.LBB34_34
# BB#33:                                # %if.then.i607
	movq	const_int_rtx+512(,%rax,8), %rax
	movq	%rax, 32(%rsp)          # 8-byte Spill
	jmp	.LBB34_36
.LBB34_34:                              # %if.end.i611
	movq	const_int_htab(%rip), %rdi
	leaq	64(%rsp), %rsi
	movq	$1, %rcx
	movq	%rax, %rdx
	callq	htab_find_slot_with_hash
	movq	(%rax), %rcx
	movq	%rcx, 32(%rsp)          # 8-byte Spill
	testq	%rcx, %rcx
	jne	.LBB34_36
# BB#35:                                # %if.then4.i613
	movq	64(%rsp), %rdx
	movq	$54, %rdi
	xorq	%rsi, %rsi
	movq	%rax, 40(%rsp)          # 8-byte Spill
	callq	gen_rtx_fmt_w
	movq	%rax, 32(%rsp)          # 8-byte Spill
	movq	40(%rsp), %rcx          # 8-byte Reload
	movq	%rax, (%rcx)
.LBB34_36:                              # %if.end280
	movzbl	(%r13), %eax
	movzbl	tree_code_type(%rax), %eax
	cmpq	$116, %rax
	je	.LBB34_37
# BB#39:                                # %if.then290
	movq	%r14, %rdi
	movq	%r12, %rsi
	callq	maybe_set_unchanging
	movl	16(%r12), %eax
	testq	$2048, %rax             # imm = 0x800
	je	.LBB34_41
# BB#40:                                # %if.then297
	movl	(%r14), %eax
	orq	$134217728, %rax        # imm = 0x8000000
	movl	%eax, (%r14)
	movl	(%r13), %eax
.LBB34_41:                              # %while.cond.preheader
	andq	$255, %rax
	addq	$-114, %rax
	andq	%r15, %rax
	cmpq	$5, %rax
	jae	.LBB34_42
	.align	16, 0x90
.LBB34_50:                              # %while.body
                                        # =>This Inner Loop Header: Depth=1
	movq	32(%r12), %r12
	movzbl	16(%r12), %eax
	addq	$-114, %rax
	andq	%r15, %rax
	cmpq	$5, %rax
	jb	.LBB34_50
# BB#51:
	leaq	16(%r12), %r13
.LBB34_42:                              # %lor.rhs329
	movq	%r12, %rdi
	callq	can_address_p
	testq	%r15, %rax
	jne	.LBB34_44
# BB#43:                                # %if.then341
	movl	(%r14), %eax
	orq	$16777216, %rax         # imm = 0x1000000
	movl	%eax, (%r14)
.LBB34_44:                              # %if.end345
	movzbl	(%r13), %eax
	movzbl	tree_code_type(%rax), %ecx
	cmpq	$99, %rcx
	jne	.LBB34_45
# BB#56:                                # %if.then384
	movl	64(%rbx), %esi
	movq	%r12, %rdi
	callq	ix86_constant_alignment
	movq	%rax, %r8
	jmp	.LBB34_38
.LBB34_45:                              # %if.end345
	cmpq	$100, %rcx
	movq	24(%rsp), %rsi          # 8-byte Reload
	jne	.LBB34_57
# BB#46:                                # %if.then355
	movq	64(%r12), %rdi
	movq	const_int_rtx+512(%rip), %rsi
	xorq	%r13, %r13
	testq	%rdi, %rdi
	je	.LBB34_55
# BB#47:                                # %land.lhs.true359
	movq	%rsi, %rbx
	movq	$1, %rsi
	callq	host_integerp
	movq	%rbx, %rsi
	testq	%r15, %rax
	je	.LBB34_55
# BB#48:                                # %cond.true364
	movq	%rsi, 24(%rsp)          # 8-byte Spill
	movq	64(%r12), %rdi
	movq	$1, %rsi
	callq	tree_low_cst
	movq	%rax, 72(%rsp)
	movq	%rax, %rcx
	addq	$64, %rcx
	cmpq	$128, %rcx
	ja	.LBB34_52
# BB#49:                                # %if.then.i620
	movq	const_int_rtx+512(,%rax,8), %r13
	jmp	.LBB34_54
.LBB34_6:                               # %if.end.i
	movq	const_int_htab(%rip), %rdi
	leaq	56(%rsp), %rsi
	movq	$1, %rcx
	callq	htab_find_slot_with_hash
	movq	%rax, %r15
	movq	(%r15), %rax
	testq	%rax, %rax
	jne	.LBB34_8
# BB#7:                                 # %if.then4.i
	movq	56(%rsp), %rdx
	movq	$54, %rdi
	xorq	%rsi, %rsi
	callq	gen_rtx_fmt_w
	movq	%rax, (%r15)
.LBB34_8:                               # %cond.end48
	movq	16(%r14), %rdi
	xorq	%rsi, %rsi
	testq	%rdi, %rdi
	movabsq	$0, %rcx
	movabsq	$0, %rdx
	jne	.LBB34_9
	jmp	.LBB34_10
.LBB34_57:                              # %if.else388
	cmpq	$43, %rax
	jne	.LBB34_58
# BB#62:                                # %if.then413
	movq	global_trees+120(%rip), %r13
	.align	16, 0x90
.LBB34_63:                              # %do.body
                                        # =>This Inner Loop Header: Depth=1
	movq	8(%r12), %rax
	movq	sizetype_tab(%rip), %rbx
	movq	40(%r12), %rdx
	movq	40(%rax), %rcx
	movq	$61, %rdi
	xorq	%rax, %rax
	movq	%rbx, %rsi
	callq	build
	movq	%rax, %rdi
	callq	fold
	movq	%rax, %rcx
	movq	$59, %rdi
	xorq	%rax, %rax
	movq	%rbx, %rsi
	movq	%rcx, %rdx
	movq	%r13, %rcx
	callq	build
	movq	%rax, %rdi
	callq	fold
	movq	32(%r12), %r12
	movzbl	16(%r12), %ecx
	movq	%rax, %r13
	cmpq	$43, %rcx
	je	.LBB34_63
# BB#64:                                # %do.body
	cmpq	$39, %rcx
	jne	.LBB34_37
# BB#65:                                # %if.then440
	movq	%r12, %rdi
	callq	component_ref_for_mem_expr
	movq	%rax, 8(%rsp)           # 8-byte Spill
	movq	$1, %rsi
	movq	%r13, %rdi
	callq	host_integerp
	testq	%r15, %rax
	je	.LBB34_37
# BB#66:                                # %if.then444
	movq	$1, %rsi
	movq	%r13, %rdi
	callq	tree_low_cst
	movq	%rax, 80(%rsp)
	movq	%rax, %rcx
	addq	$64, %rcx
	cmpq	$128, %rcx
	ja	.LBB34_68
# BB#67:                                # %if.then.i633
	movq	const_int_rtx+512(,%rax,8), %rsi
	movq	48(%rsp), %r8           # 8-byte Reload
	jmp	.LBB34_70
.LBB34_37:
	movq	48(%rsp), %r8           # 8-byte Reload
.LBB34_38:                              # %if.end453
	movq	32(%rsp), %r13          # 8-byte Reload
	movq	24(%rsp), %rsi          # 8-byte Reload
.LBB34_71:                              # %if.end453
	movq	16(%rsp), %rdi          # 8-byte Reload
	movq	8(%rsp), %rax           # 8-byte Reload
.LBB34_72:                              # %if.end453
	movl	(%r14), %ebx
	movq	$16, %rcx
	movq	%rbx, %rdx
	shrq	%cl, %rdx
	testq	%rdi, %rdi
	jne	.LBB34_81
# BB#73:                                # %if.end453
	testq	%rax, %rax
	jne	.LBB34_81
# BB#74:                                # %if.end453
	testq	%rsi, %rsi
	jne	.LBB34_81
# BB#75:                                # %land.lhs.true4.i
	testq	%r13, %r13
	je	.LBB34_79
# BB#76:                                # %lor.lhs.false.i
	andq	$255, %rdx
	cmpq	$51, %rdx
	je	.LBB34_81
# BB#77:                                # %land.lhs.true7.i
	movzbl	mode_size(%rdx), %ecx
	movq	8(%r13), %rdx
	cmpq	%rdx, %rcx
	jne	.LBB34_81
# BB#78:                                # %land.lhs.true7.i
	xorq	%rcx, %rcx
	andq	%r8, %r15
	jmp	.LBB34_80
.LBB34_79:                              # %land.lhs.true11.i
	andq	%r8, %r15
	xorq	%rcx, %rcx
.LBB34_80:                              # %land.lhs.true11.i
	cmpq	$8, %r15
	je	.LBB34_84
.LBB34_81:                              # %if.end.i646
	movq	%rdi, 88(%rsp)
	movq	%rax, 96(%rsp)
	movq	mem_attrs_htab(%rip), %rdi
	movq	%rsi, 104(%rsp)
	movq	%r13, 112(%rsp)
	movl	%r8d, 120(%rsp)
	leaq	88(%rsp), %rsi
	movq	$1, %rdx
	callq	htab_find_slot
	movq	%rax, %rbx
	movq	(%rbx), %rcx
	testq	%rcx, %rcx
	jne	.LBB34_83
# BB#82:                                # %if.then21.i
	movq	$40, %rdi
	callq	ggc_alloc
	movq	%rax, (%rbx)
	movq	120(%rsp), %rcx
	movq	%rcx, 32(%rax)
	movq	112(%rsp), %rcx
	movq	%rcx, 24(%rax)
	movq	104(%rsp), %rcx
	movq	%rcx, 16(%rax)
	movq	88(%rsp), %rcx
	movq	96(%rsp), %rdx
	movq	%rdx, 8(%rax)
	movq	%rcx, (%rax)
	movq	(%rbx), %rcx
.LBB34_83:                              # %if.end23.i
	movl	(%r14), %ebx
.LBB34_84:                              # %get_mem_attrs.exit
	movq	%rcx, 16(%r14)
	movabsq	$2415919104, %rcx       # imm = 0x90000000
	testq	%rcx, %rbx
	jne	.LBB34_88
# BB#85:                                # %if.else470
	movzbl	16(%r12), %ecx
	cmpq	$44, %rcx
	ja	.LBB34_88
# BB#86:                                # %if.else470
	movq	$1, %rdx
	shlq	%cl, %rdx
	movabsq	$28037546508288, %rcx   # imm = 0x198000000000
	testq	%rcx, %rdx
	je	.LBB34_88
# BB#87:                                # %if.then498
	orq	$268435456, %rbx        # imm = 0x10000000
	movl	%ebx, (%r14)
.LBB34_88:                              # %if.end503
	addq	$128, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB34_58:                              # %if.else388
	cmpq	$39, %rax
	movq	48(%rsp), %r8           # 8-byte Reload
	movq	8(%rsp), %rax           # 8-byte Reload
	jne	.LBB34_59
# BB#60:                                # %land.lhs.true395
	movq	40(%r12), %rcx
	movzbl	49(%rcx), %ecx
	testq	$16, %rcx
	movq	32(%rsp), %r13          # 8-byte Reload
	movq	16(%rsp), %rdi          # 8-byte Reload
	jne	.LBB34_72
# BB#61:                                # %if.then404
	movq	%r12, %rdi
	movq	%r8, %rbx
	callq	component_ref_for_mem_expr
	movq	%rbx, %r8
	movq	16(%rsp), %rdi          # 8-byte Reload
	movq	const_int_rtx+512(%rip), %rsi
	jmp	.LBB34_72
.LBB34_59:
	movq	32(%rsp), %r13          # 8-byte Reload
	movq	16(%rsp), %rdi          # 8-byte Reload
	jmp	.LBB34_72
.LBB34_52:                              # %if.end.i624
	movq	const_int_htab(%rip), %rdi
	leaq	72(%rsp), %rsi
	movq	$1, %rcx
	movq	%rax, %rdx
	callq	htab_find_slot_with_hash
	movq	%rax, %rbx
	movq	(%rbx), %r13
	testq	%r13, %r13
	jne	.LBB34_54
# BB#53:                                # %if.then4.i626
	movq	72(%rsp), %rdx
	movq	$54, %rdi
	xorq	%rsi, %rsi
	callq	gen_rtx_fmt_w
	movq	%rax, %r13
	movq	%r13, (%rbx)
.LBB34_54:                              # %if.end6.i627
	movq	24(%rsp), %rsi          # 8-byte Reload
.LBB34_55:                              # %cond.end370
	movl	56(%r12), %r8d
	andq	$16777215, %r8          # imm = 0xFFFFFF
	movq	%r12, %rax
	movq	16(%rsp), %rdi          # 8-byte Reload
	jmp	.LBB34_72
.LBB34_68:                              # %if.end.i637
	movq	const_int_htab(%rip), %rdi
	leaq	80(%rsp), %rsi
	movq	$1, %rcx
	movq	%rax, %rdx
	callq	htab_find_slot_with_hash
	movq	%rax, %rbx
	movq	(%rbx), %rsi
	testq	%rsi, %rsi
	movq	48(%rsp), %r8           # 8-byte Reload
	jne	.LBB34_70
# BB#69:                                # %if.then4.i639
	movq	80(%rsp), %rdx
	movq	$54, %rdi
	xorq	%rsi, %rsi
	movq	%r8, %r13
	callq	gen_rtx_fmt_w
	movq	%r13, %r8
	movq	%rax, %rsi
	movq	%rsi, (%rbx)
.LBB34_70:                              # %if.end6.i640
	movq	32(%rsp), %r13          # 8-byte Reload
	jmp	.LBB34_71
.LBB34_89:                              # %if.then105
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.set_mem_attributes, %rcx
	movq	$1729, %rsi             # imm = 0x6C1
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp250:
	.size	set_mem_attributes, .Ltmp250-set_mem_attributes
	.cfi_endproc

	.align	16, 0x90
	.type	component_ref_for_mem_expr,@function
component_ref_for_mem_expr:             # @component_ref_for_mem_expr
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp256:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp257:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp258:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp259:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp260:
	.cfi_def_cfa_offset 48
.Ltmp261:
	.cfi_offset %rbx, -40
.Ltmp262:
	.cfi_offset %r12, -32
.Ltmp263:
	.cfi_offset %r14, -24
.Ltmp264:
	.cfi_offset %r15, -16
	movq	%rdi, %r15
	movq	32(%r15), %rdx
	movzbl	16(%rdx), %eax
	cmpq	$39, %rax
	jne	.LBB35_2
# BB#1:                                 # %if.then
	movq	%rdx, %rdi
	callq	component_ref_for_mem_expr
	movq	%rax, %rdx
	jmp	.LBB35_12
.LBB35_2:                               # %if.else
	movq	$0, (%rsp)
	movl	16(%rdx), %eax
	movq	%rax, %rcx
	andq	$255, %rcx
	addq	$-57, %rcx
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	movq	%rcx, %rsi
	andq	%rbx, %rsi
	cmpq	$62, %rsi
	jae	.LBB35_9
# BB#3:
	movabsq	$4467570830351532033, %r12 # imm = 0x3E00000000000001
	leaq	(%rsp), %r14
	.align	16, 0x90
.LBB35_4:                               # %lor.end
                                        # =>This Inner Loop Header: Depth=1
	andq	%rbx, %rcx
	movq	%r12, %rsi
	shrq	%cl, %rsi
	andq	$255, %rax
	testq	$1, %rsi
	je	.LBB35_10
# BB#5:                                 # %while.body
                                        #   in Loop: Header=BB35_4 Depth=1
	cmpq	$57, %rax
	jne	.LBB35_7
# BB#6:                                 # %if.then39
                                        #   in Loop: Header=BB35_4 Depth=1
	movq	%rdx, %rdi
	movq	%r14, %rsi
	callq	find_placeholder
	movq	%rax, %rdx
	jmp	.LBB35_8
	.align	16, 0x90
.LBB35_7:                               # %if.else41
                                        #   in Loop: Header=BB35_4 Depth=1
	movq	32(%rdx), %rdx
.LBB35_8:                               # %while.cond.backedge
                                        #   in Loop: Header=BB35_4 Depth=1
	movl	16(%rdx), %eax
	movq	%rax, %rcx
	andq	$255, %rcx
	addq	$-57, %rcx
	movq	%rcx, %rsi
	andq	%rbx, %rsi
	cmpq	$62, %rsi
	jb	.LBB35_4
.LBB35_9:                               # %lor.end.thread
	andq	$255, %rax
.LBB35_10:                              # %while.end
	movzbl	tree_code_type(%rax), %eax
	cmpq	$100, %rax
	je	.LBB35_12
# BB#11:                                # %select.mid
	xorq	%rdx, %rdx
.LBB35_12:                              # %if.end54
	movq	32(%r15), %rax
	cmpq	%rax, %rdx
	je	.LBB35_14
# BB#13:                                # %if.else61
	movq	8(%r15), %rsi
	movq	40(%r15), %rcx
	movq	$39, %rdi
	xorq	%rax, %rax
	callq	build
	movq	%rax, %r15
.LBB35_14:                              # %return
	movq	%r15, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.Ltmp265:
	.size	component_ref_for_mem_expr, .Ltmp265-component_ref_for_mem_expr
	.cfi_endproc

	.globl	set_mem_alias_set
	.align	16, 0x90
	.type	set_mem_alias_set,@function
set_mem_alias_set:                      # @set_mem_alias_set
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp271:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp272:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp273:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp274:
	.cfi_def_cfa_offset 40
	subq	$56, %rsp
.Ltmp275:
	.cfi_def_cfa_offset 96
.Ltmp276:
	.cfi_offset %rbx, -40
.Ltmp277:
	.cfi_offset %r12, -32
.Ltmp278:
	.cfi_offset %r14, -24
.Ltmp279:
	.cfi_offset %r15, -16
	movq	%rdi, %r15
	movq	16(%r15), %rdi
	movq	%rsi, %r14
	testq	%rdi, %rdi
	je	.LBB36_2
# BB#1:                                 # %cond.end33.thread84
	movq	8(%rdi), %rcx
	movq	16(%rdi), %rsi
	movq	24(%rdi), %rax
.LBB36_9:                               # %cond.true40
	movl	32(%rdi), %ebx
	movq	%rsi, %rdx
	jmp	.LBB36_10
.LBB36_2:                               # %cond.false23
	movzbl	2(%r15), %ecx
	xorq	%rax, %rax
	cmpq	$51, %rcx
	movq	$8, %rbx
	je	.LBB36_3
# BB#4:                                 # %cond.true25
	movzbl	mode_size(%rcx), %edx
	movq	%rdx, 8(%rsp)
	leaq	64(%rdx), %rax
	cmpq	$128, %rax
	ja	.LBB36_6
# BB#5:                                 # %if.then.i
	movq	const_int_rtx+512(,%rdx,8), %rax
.LBB36_3:
	xorq	%rcx, %rcx
	xorq	%rdx, %rdx
.LBB36_10:                              # %cond.end45
	testq	%r14, %r14
	jne	.LBB36_19
# BB#11:                                # %cond.end45
	testq	%rcx, %rcx
	jne	.LBB36_19
# BB#12:                                # %cond.end45
	testq	%rdx, %rdx
	jne	.LBB36_19
# BB#13:                                # %land.lhs.true4.i
	testq	%rax, %rax
	je	.LBB36_17
# BB#14:                                # %lor.lhs.false.i
	movzbl	2(%r15), %esi
	cmpq	$51, %rsi
	je	.LBB36_19
# BB#15:                                # %land.lhs.true7.i
	movzbl	mode_size(%rsi), %esi
	movq	8(%rax), %rdi
	cmpq	%rdi, %rsi
	jne	.LBB36_19
# BB#16:                                # %land.lhs.true7.i
	xorq	%rsi, %rsi
	movabsq	$4294967295, %rdi       # imm = 0xFFFFFFFF
	andq	%rbx, %rdi
	jmp	.LBB36_18
.LBB36_17:                              # %land.lhs.true11.i
	movabsq	$4294967295, %rdi       # imm = 0xFFFFFFFF
	andq	%rbx, %rdi
	xorq	%rsi, %rsi
.LBB36_18:                              # %land.lhs.true11.i
	cmpq	$8, %rdi
	je	.LBB36_21
.LBB36_19:                              # %if.end.i68
	movq	%r14, 16(%rsp)
	movq	%rcx, 24(%rsp)
	movq	mem_attrs_htab(%rip), %rdi
	movq	%rdx, 32(%rsp)
	movq	%rax, 40(%rsp)
	movl	%ebx, 48(%rsp)
	leaq	16(%rsp), %rsi
	movq	$1, %rdx
	callq	htab_find_slot
	movq	%rax, %rbx
	movq	(%rbx), %rsi
	testq	%rsi, %rsi
	jne	.LBB36_21
# BB#20:                                # %if.then21.i
	movq	$40, %rdi
	callq	ggc_alloc
	movq	%rax, (%rbx)
	movq	48(%rsp), %rcx
	movq	%rcx, 32(%rax)
	movq	40(%rsp), %rcx
	movq	%rcx, 24(%rax)
	movq	32(%rsp), %rcx
	movq	%rcx, 16(%rax)
	movq	16(%rsp), %rcx
	movq	24(%rsp), %rdx
	movq	%rdx, 8(%rax)
	movq	%rcx, (%rax)
	movq	(%rbx), %rsi
.LBB36_21:                              # %get_mem_attrs.exit
	movq	%rsi, 16(%r15)
	addq	$56, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.LBB36_6:                               # %if.end.i
	movq	const_int_htab(%rip), %rdi
	leaq	8(%rsp), %rsi
	movq	$1, %rcx
	callq	htab_find_slot_with_hash
	movq	%rax, %r12
	movq	(%r12), %rax
	testq	%rax, %rax
	jne	.LBB36_8
# BB#7:                                 # %if.then4.i
	movq	8(%rsp), %rdx
	movq	$54, %rdi
	xorq	%rsi, %rsi
	callq	gen_rtx_fmt_w
	movq	%rax, (%r12)
.LBB36_8:                               # %cond.end33
	movq	16(%r15), %rdi
	xorq	%rcx, %rcx
	testq	%rdi, %rdi
	movabsq	$0, %rsi
	movabsq	$0, %rdx
	je	.LBB36_10
	jmp	.LBB36_9
.Ltmp280:
	.size	set_mem_alias_set, .Ltmp280-set_mem_alias_set
	.cfi_endproc

	.globl	set_mem_align
	.align	16, 0x90
	.type	set_mem_align,@function
set_mem_align:                          # @set_mem_align
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp285:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp286:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp287:
	.cfi_def_cfa_offset 32
	subq	$48, %rsp
.Ltmp288:
	.cfi_def_cfa_offset 80
.Ltmp289:
	.cfi_offset %rbx, -32
.Ltmp290:
	.cfi_offset %r14, -24
.Ltmp291:
	.cfi_offset %r15, -16
	movq	%rdi, %r15
	movq	16(%r15), %rax
	movq	%rsi, %r14
	testq	%rax, %rax
	je	.LBB37_2
# BB#1:                                 # %cond.true30
	movq	(%rax), %rdx
	movq	8(%rax), %rsi
	movq	16(%rax), %rcx
	movq	24(%rax), %rax
	jmp	.LBB37_9
.LBB37_2:                               # %cond.false34
	movzbl	2(%r15), %eax
	cmpq	$51, %rax
	jne	.LBB37_4
# BB#3:
	xorq	%rcx, %rcx
	xorq	%rdx, %rdx
	xorq	%rsi, %rsi
	xorq	%rax, %rax
	jmp	.LBB37_9
.LBB37_4:                               # %cond.true36
	movzbl	mode_size(%rax), %edx
	movq	%rdx, (%rsp)
	leaq	64(%rdx), %rax
	cmpq	$128, %rax
	ja	.LBB37_6
# BB#5:                                 # %if.then.i
	movq	const_int_rtx+512(,%rdx,8), %rax
	jmp	.LBB37_8
.LBB37_6:                               # %if.end.i
	movq	const_int_htab(%rip), %rdi
	leaq	(%rsp), %rsi
	movq	$1, %rcx
	callq	htab_find_slot_with_hash
	movq	%rax, %rbx
	movq	(%rbx), %rax
	testq	%rax, %rax
	jne	.LBB37_8
# BB#7:                                 # %if.then4.i
	movq	(%rsp), %rdx
	movq	$54, %rdi
	xorq	%rsi, %rsi
	callq	gen_rtx_fmt_w
	movq	%rax, (%rbx)
.LBB37_8:                               # %if.end6.i
	xorq	%rcx, %rcx
	xorq	%rdx, %rdx
	xorq	%rsi, %rsi
.LBB37_9:                               # %cond.end44
	testq	%rdx, %rdx
	jne	.LBB37_18
# BB#10:                                # %cond.end44
	testq	%rsi, %rsi
	jne	.LBB37_18
# BB#11:                                # %cond.end44
	testq	%rcx, %rcx
	jne	.LBB37_18
# BB#12:                                # %land.lhs.true4.i
	testq	%rax, %rax
	je	.LBB37_16
# BB#13:                                # %lor.lhs.false.i
	movzbl	2(%r15), %edi
	cmpq	$51, %rdi
	je	.LBB37_18
# BB#14:                                # %land.lhs.true7.i
	movzbl	mode_size(%rdi), %edi
	movq	8(%rax), %rbx
	cmpq	%rbx, %rdi
	jne	.LBB37_18
# BB#15:                                # %land.lhs.true7.i
	xorq	%rdi, %rdi
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	andq	%r14, %rbx
	jmp	.LBB37_17
.LBB37_16:                              # %land.lhs.true11.i
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	andq	%r14, %rbx
	xorq	%rdi, %rdi
.LBB37_17:                              # %land.lhs.true11.i
	cmpq	$8, %rbx
	je	.LBB37_20
.LBB37_18:                              # %if.end.i67
	movq	%rdx, 8(%rsp)
	movq	%rsi, 16(%rsp)
	movq	mem_attrs_htab(%rip), %rdi
	movq	%rcx, 24(%rsp)
	movq	%rax, 32(%rsp)
	movl	%r14d, 40(%rsp)
	leaq	8(%rsp), %rsi
	movq	$1, %rdx
	callq	htab_find_slot
	movq	%rax, %rbx
	movq	(%rbx), %rdi
	testq	%rdi, %rdi
	jne	.LBB37_20
# BB#19:                                # %if.then21.i
	movq	$40, %rdi
	callq	ggc_alloc
	movq	%rax, (%rbx)
	movq	40(%rsp), %rcx
	movq	%rcx, 32(%rax)
	movq	32(%rsp), %rcx
	movq	%rcx, 24(%rax)
	movq	24(%rsp), %rcx
	movq	%rcx, 16(%rax)
	movq	8(%rsp), %rcx
	movq	16(%rsp), %rdx
	movq	%rdx, 8(%rax)
	movq	%rcx, (%rax)
	movq	(%rbx), %rdi
.LBB37_20:                              # %get_mem_attrs.exit
	movq	%rdi, 16(%r15)
	addq	$48, %rsp
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp292:
	.size	set_mem_align, .Ltmp292-set_mem_align
	.cfi_endproc

	.globl	set_mem_expr
	.align	16, 0x90
	.type	set_mem_expr,@function
set_mem_expr:                           # @set_mem_expr
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp298:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp299:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp300:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp301:
	.cfi_def_cfa_offset 40
	subq	$56, %rsp
.Ltmp302:
	.cfi_def_cfa_offset 96
.Ltmp303:
	.cfi_offset %rbx, -40
.Ltmp304:
	.cfi_offset %r12, -32
.Ltmp305:
	.cfi_offset %r14, -24
.Ltmp306:
	.cfi_offset %r15, -16
	movq	%rdi, %r15
	movq	16(%r15), %rdi
	movq	%rsi, %r14
	testq	%rdi, %rdi
	je	.LBB38_2
# BB#1:                                 # %cond.end33.thread84
	movq	(%rdi), %rcx
	movq	16(%rdi), %rsi
	movq	24(%rdi), %rax
.LBB38_9:                               # %cond.true40
	movl	32(%rdi), %ebx
	movq	%rsi, %rdx
	jmp	.LBB38_10
.LBB38_2:                               # %cond.false23
	movzbl	2(%r15), %ecx
	xorq	%rax, %rax
	cmpq	$51, %rcx
	movq	$8, %rbx
	je	.LBB38_3
# BB#4:                                 # %cond.true25
	movzbl	mode_size(%rcx), %edx
	movq	%rdx, 8(%rsp)
	leaq	64(%rdx), %rax
	cmpq	$128, %rax
	ja	.LBB38_6
# BB#5:                                 # %if.then.i
	movq	const_int_rtx+512(,%rdx,8), %rax
.LBB38_3:
	xorq	%rcx, %rcx
	xorq	%rdx, %rdx
.LBB38_10:                              # %cond.end45
	testq	%rcx, %rcx
	jne	.LBB38_19
# BB#11:                                # %cond.end45
	testq	%r14, %r14
	jne	.LBB38_19
# BB#12:                                # %cond.end45
	testq	%rdx, %rdx
	jne	.LBB38_19
# BB#13:                                # %land.lhs.true4.i
	testq	%rax, %rax
	je	.LBB38_17
# BB#14:                                # %lor.lhs.false.i
	movzbl	2(%r15), %esi
	cmpq	$51, %rsi
	je	.LBB38_19
# BB#15:                                # %land.lhs.true7.i
	movzbl	mode_size(%rsi), %esi
	movq	8(%rax), %rdi
	cmpq	%rdi, %rsi
	jne	.LBB38_19
# BB#16:                                # %land.lhs.true7.i
	xorq	%rsi, %rsi
	movabsq	$4294967295, %rdi       # imm = 0xFFFFFFFF
	andq	%rbx, %rdi
	jmp	.LBB38_18
.LBB38_17:                              # %land.lhs.true11.i
	movabsq	$4294967295, %rdi       # imm = 0xFFFFFFFF
	andq	%rbx, %rdi
	xorq	%rsi, %rsi
.LBB38_18:                              # %land.lhs.true11.i
	cmpq	$8, %rdi
	je	.LBB38_21
.LBB38_19:                              # %if.end.i68
	movq	%rcx, 16(%rsp)
	movq	%r14, 24(%rsp)
	movq	mem_attrs_htab(%rip), %rdi
	movq	%rdx, 32(%rsp)
	movq	%rax, 40(%rsp)
	movl	%ebx, 48(%rsp)
	leaq	16(%rsp), %rsi
	movq	$1, %rdx
	callq	htab_find_slot
	movq	%rax, %rbx
	movq	(%rbx), %rsi
	testq	%rsi, %rsi
	jne	.LBB38_21
# BB#20:                                # %if.then21.i
	movq	$40, %rdi
	callq	ggc_alloc
	movq	%rax, (%rbx)
	movq	48(%rsp), %rcx
	movq	%rcx, 32(%rax)
	movq	40(%rsp), %rcx
	movq	%rcx, 24(%rax)
	movq	32(%rsp), %rcx
	movq	%rcx, 16(%rax)
	movq	16(%rsp), %rcx
	movq	24(%rsp), %rdx
	movq	%rdx, 8(%rax)
	movq	%rcx, (%rax)
	movq	(%rbx), %rsi
.LBB38_21:                              # %get_mem_attrs.exit
	movq	%rsi, 16(%r15)
	addq	$56, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.LBB38_6:                               # %if.end.i
	movq	const_int_htab(%rip), %rdi
	leaq	8(%rsp), %rsi
	movq	$1, %rcx
	callq	htab_find_slot_with_hash
	movq	%rax, %r12
	movq	(%r12), %rax
	testq	%rax, %rax
	jne	.LBB38_8
# BB#7:                                 # %if.then4.i
	movq	8(%rsp), %rdx
	movq	$54, %rdi
	xorq	%rsi, %rsi
	callq	gen_rtx_fmt_w
	movq	%rax, (%r12)
.LBB38_8:                               # %cond.end33
	movq	16(%r15), %rdi
	xorq	%rcx, %rcx
	testq	%rdi, %rdi
	movabsq	$0, %rsi
	movabsq	$0, %rdx
	je	.LBB38_10
	jmp	.LBB38_9
.Ltmp307:
	.size	set_mem_expr, .Ltmp307-set_mem_expr
	.cfi_endproc

	.globl	set_mem_offset
	.align	16, 0x90
	.type	set_mem_offset,@function
set_mem_offset:                         # @set_mem_offset
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp313:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp314:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp315:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp316:
	.cfi_def_cfa_offset 40
	subq	$56, %rsp
.Ltmp317:
	.cfi_def_cfa_offset 96
.Ltmp318:
	.cfi_offset %rbx, -40
.Ltmp319:
	.cfi_offset %r12, -32
.Ltmp320:
	.cfi_offset %r14, -24
.Ltmp321:
	.cfi_offset %r15, -16
	movq	%rdi, %r15
	movq	16(%r15), %rdi
	movq	%rsi, %r14
	testq	%rdi, %rdi
	je	.LBB39_2
# BB#1:                                 # %cond.end33.thread84
	movq	(%rdi), %rcx
	movq	8(%rdi), %rsi
	movq	24(%rdi), %rax
.LBB39_10:                              # %cond.true40
	movl	32(%rdi), %ebx
	movq	%rsi, %rdx
	jmp	.LBB39_11
.LBB39_2:                               # %cond.false23
	movzbl	2(%r15), %eax
	cmpq	$51, %rax
	movq	$8, %rbx
	jne	.LBB39_5
# BB#3:
	xorq	%rax, %rax
	jmp	.LBB39_4
.LBB39_5:                               # %cond.true25
	movzbl	mode_size(%rax), %edx
	movq	%rdx, 8(%rsp)
	leaq	64(%rdx), %rax
	cmpq	$128, %rax
	ja	.LBB39_7
# BB#6:                                 # %if.then.i
	movq	const_int_rtx+512(,%rdx,8), %rax
.LBB39_4:                               # %cond.end45
	xorq	%rcx, %rcx
	xorq	%rdx, %rdx
.LBB39_11:                              # %cond.end45
	testq	%rcx, %rcx
	jne	.LBB39_20
# BB#12:                                # %cond.end45
	testq	%rdx, %rdx
	jne	.LBB39_20
# BB#13:                                # %cond.end45
	testq	%r14, %r14
	jne	.LBB39_20
# BB#14:                                # %land.lhs.true4.i
	testq	%rax, %rax
	je	.LBB39_18
# BB#15:                                # %lor.lhs.false.i
	movzbl	2(%r15), %esi
	cmpq	$51, %rsi
	je	.LBB39_20
# BB#16:                                # %land.lhs.true7.i
	movzbl	mode_size(%rsi), %esi
	movq	8(%rax), %rdi
	cmpq	%rdi, %rsi
	jne	.LBB39_20
# BB#17:                                # %land.lhs.true7.i
	xorq	%rsi, %rsi
	movabsq	$4294967295, %rdi       # imm = 0xFFFFFFFF
	andq	%rbx, %rdi
	jmp	.LBB39_19
.LBB39_18:                              # %land.lhs.true11.i
	movabsq	$4294967295, %rdi       # imm = 0xFFFFFFFF
	andq	%rbx, %rdi
	xorq	%rsi, %rsi
.LBB39_19:                              # %land.lhs.true11.i
	cmpq	$8, %rdi
	je	.LBB39_22
.LBB39_20:                              # %if.end.i68
	movq	%rcx, 16(%rsp)
	movq	%rdx, 24(%rsp)
	movq	mem_attrs_htab(%rip), %rdi
	movq	%r14, 32(%rsp)
	movq	%rax, 40(%rsp)
	movl	%ebx, 48(%rsp)
	leaq	16(%rsp), %rsi
	movq	$1, %rdx
	callq	htab_find_slot
	movq	%rax, %rbx
	movq	(%rbx), %rsi
	testq	%rsi, %rsi
	jne	.LBB39_22
# BB#21:                                # %if.then21.i
	movq	$40, %rdi
	callq	ggc_alloc
	movq	%rax, (%rbx)
	movq	48(%rsp), %rcx
	movq	%rcx, 32(%rax)
	movq	40(%rsp), %rcx
	movq	%rcx, 24(%rax)
	movq	32(%rsp), %rcx
	movq	%rcx, 16(%rax)
	movq	16(%rsp), %rcx
	movq	24(%rsp), %rdx
	movq	%rdx, 8(%rax)
	movq	%rcx, (%rax)
	movq	(%rbx), %rsi
.LBB39_22:                              # %get_mem_attrs.exit
	movq	%rsi, 16(%r15)
	addq	$56, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.LBB39_7:                               # %if.end.i
	movq	const_int_htab(%rip), %rdi
	leaq	8(%rsp), %rsi
	movq	$1, %rcx
	callq	htab_find_slot_with_hash
	movq	%rax, %r12
	movq	(%r12), %rax
	testq	%rax, %rax
	jne	.LBB39_9
# BB#8:                                 # %if.then4.i
	movq	8(%rsp), %rdx
	movq	$54, %rdi
	xorq	%rsi, %rsi
	callq	gen_rtx_fmt_w
	movq	%rax, (%r12)
.LBB39_9:                               # %cond.end33
	movq	16(%r15), %rdi
	xorq	%rcx, %rcx
	testq	%rdi, %rdi
	movabsq	$0, %rsi
	movabsq	$0, %rdx
	je	.LBB39_11
	jmp	.LBB39_10
.Ltmp322:
	.size	set_mem_offset, .Ltmp322-set_mem_offset
	.cfi_endproc

	.globl	change_address
	.align	16, 0x90
	.type	change_address,@function
change_address:                         # @change_address
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp329:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp330:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp331:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp332:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp333:
	.cfi_def_cfa_offset 48
	subq	$48, %rsp
.Ltmp334:
	.cfi_def_cfa_offset 96
.Ltmp335:
	.cfi_offset %rbx, -48
.Ltmp336:
	.cfi_offset %r12, -40
.Ltmp337:
	.cfi_offset %r13, -32
.Ltmp338:
	.cfi_offset %r14, -24
.Ltmp339:
	.cfi_offset %r15, -16
	movq	%rdi, %rbx
	movq	$1, %rcx
                                        # kill: RDI<def> RBX<kill>
	callq	change_address_1
	movq	%rax, %r14
	movq	16(%rbx), %rax
	movzbl	2(%r14), %r15d
	xorq	%rbx, %rbx
	testq	%rax, %rax
	movabsq	$0, %r13
	je	.LBB40_2
# BB#1:                                 # %cond.false
	movq	(%rax), %r13
.LBB40_2:                               # %cond.end
	movq	$8, %rax
	cmpq	$51, %r15
	je	.LBB40_8
# BB#3:                                 # %cond.false6
	movzbl	mode_size(%r15), %edx
	movq	%rdx, 8(%rsp)
	leaq	64(%rdx), %rax
	cmpq	$128, %rax
	ja	.LBB40_5
# BB#4:                                 # %cond.end9.thread37
	movq	const_int_rtx+512(,%rdx,8), %rbx
	jmp	.LBB40_7
.LBB40_5:                               # %if.end.i
	movq	const_int_htab(%rip), %rdi
	leaq	8(%rsp), %rsi
	movq	$1, %rcx
	callq	htab_find_slot_with_hash
	movq	%rax, %r12
	movq	(%r12), %rbx
	testq	%rbx, %rbx
	jne	.LBB40_7
# BB#6:                                 # %if.then4.i
	movq	8(%rsp), %rdx
	movq	$54, %rdi
	xorq	%rsi, %rsi
	callq	gen_rtx_fmt_w
	movq	%rax, %rbx
	movq	%rbx, (%r12)
.LBB40_7:                               # %cond.false14
	movq	%r15, %rdi
	callq	get_mode_alignment
.LBB40_8:                               # %cond.end16
	testq	%r13, %r13
	jne	.LBB40_15
# BB#9:                                 # %land.lhs.true4.i
	testq	%rbx, %rbx
	je	.LBB40_13
# BB#10:                                # %lor.lhs.false.i
	cmpq	$51, %r15
	je	.LBB40_15
# BB#11:                                # %land.lhs.true7.i
	movzbl	mode_size(%r15), %ecx
	movq	8(%rbx), %rdx
	cmpq	%rdx, %rcx
	jne	.LBB40_15
# BB#12:                                # %land.lhs.true7.i
	xorq	%rcx, %rcx
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	andq	%rax, %rdx
	jmp	.LBB40_14
.LBB40_13:                              # %land.lhs.true11.i
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	andq	%rax, %rdx
	xorq	%rcx, %rcx
.LBB40_14:                              # %land.lhs.true11.i
	cmpq	$8, %rdx
	je	.LBB40_17
.LBB40_15:                              # %if.end.i34
	movq	%r13, 8(%rsp)
	movq	$0, 24(%rsp)
	movq	mem_attrs_htab(%rip), %rdi
	movq	$0, 16(%rsp)
	movq	%rbx, 32(%rsp)
	movl	%eax, 40(%rsp)
	leaq	8(%rsp), %rsi
	movq	$1, %rdx
	callq	htab_find_slot
	movq	%rax, %rbx
	movq	(%rbx), %rcx
	testq	%rcx, %rcx
	jne	.LBB40_17
# BB#16:                                # %if.then21.i
	movq	$40, %rdi
	callq	ggc_alloc
	movq	%rax, (%rbx)
	movq	40(%rsp), %rcx
	movq	%rcx, 32(%rax)
	movq	32(%rsp), %rcx
	movq	%rcx, 24(%rax)
	movq	24(%rsp), %rcx
	movq	%rcx, 16(%rax)
	movq	8(%rsp), %rcx
	movq	16(%rsp), %rdx
	movq	%rdx, 8(%rax)
	movq	%rcx, (%rax)
	movq	(%rbx), %rcx
.LBB40_17:                              # %get_mem_attrs.exit
	movq	%rcx, 16(%r14)
	movq	%r14, %rax
	addq	$48, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp340:
	.size	change_address, .Ltmp340-change_address
	.cfi_endproc

	.align	16, 0x90
	.type	change_address_1,@function
change_address_1:                       # @change_address_1
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp346:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp347:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp348:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp349:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp350:
	.cfi_def_cfa_offset 48
.Ltmp351:
	.cfi_offset %rbx, -40
.Ltmp352:
	.cfi_offset %r12, -32
.Ltmp353:
	.cfi_offset %r14, -24
.Ltmp354:
	.cfi_offset %r15, -16
	movq	%rdi, %r12
	movl	(%r12), %edi
	movq	%rcx, %rax
	movq	%rdx, %r14
	movq	%rsi, %r15
	movq	%rdi, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$66, %rcx
	jne	.LBB41_1
# BB#3:                                 # %if.end
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	testq	%rbx, %r15
	jne	.LBB41_5
# BB#4:                                 # %if.then2
	movq	$16, %rcx
	shrq	%cl, %rdi
	andq	$255, %rdi
	movq	%rdi, %r15
.LBB41_5:                               # %if.end5
	testq	%r14, %r14
	jne	.LBB41_7
# BB#6:                                 # %if.then7
	movq	8(%r12), %r14
.LBB41_7:                               # %if.end8
	testq	%rbx, %rax
	je	.LBB41_12
# BB#8:                                 # %if.then9
	movl	reload_in_progress(%rip), %eax
	movl	reload_completed(%rip), %ecx
	orq	%rax, %rcx
	movq	%r15, %rdi
	testq	%rbx, %rcx
	je	.LBB41_11
# BB#9:                                 # %if.then12
	movq	%r14, %rsi
	callq	memory_address_p
	testq	%rbx, %rax
	jne	.LBB41_12
# BB#10:                                # %if.then14
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.change_address_1, %rcx
	movq	$1934, %rsi             # imm = 0x78E
	jmp	.LBB41_2
.LBB41_11:                              # %if.else
	movq	%r14, %rsi
	callq	memory_address
	movq	%rax, %r14
.LBB41_12:                              # %if.end18
	movq	8(%r12), %rsi
	movq	%r14, %rdi
	callq	rtx_equal_p
	testq	%rbx, %rax
	je	.LBB41_14
# BB#13:                                # %land.lhs.true
	movzbl	2(%r12), %eax
	andq	%r15, %rbx
	cmpq	%rax, %rbx
	je	.LBB41_15
.LBB41_14:                              # %if.end29
	movq	$66, %rdi
	movq	%r15, %rsi
	movq	%r14, %rdx
	callq	gen_rtx_fmt_e0
	movq	$0, 16(%rax)
	movl	(%r12), %edx
	movl	(%rax), %ecx
	andq	$134217728, %rdx        # imm = 0x8000000
	andq	$-134217729, %rcx       # imm = 0xFFFFFFFFF7FFFFFF
	orq	%rdx, %rcx
	movl	%ecx, (%rax)
	movl	(%r12), %edx
	andq	$268435456, %rdx        # imm = 0x10000000
	andq	$-268435457, %rcx       # imm = 0xFFFFFFFFEFFFFFFF
	orq	%rdx, %rcx
	movl	%ecx, (%rax)
	movl	(%r12), %edx
	andq	$-2147483648, %rdx      # imm = 0xFFFFFFFF80000000
	andq	$2147483647, %rcx       # imm = 0x7FFFFFFF
	orq	%rdx, %rcx
	movl	%ecx, (%rax)
	movl	(%r12), %edx
	andq	$67108864, %rdx         # imm = 0x4000000
	andq	$-67108865, %rcx        # imm = 0xFFFFFFFFFBFFFFFF
	orq	%rdx, %rcx
	movl	%ecx, (%rax)
	movl	(%r12), %edx
	andq	$16777216, %rdx         # imm = 0x1000000
	movabsq	$4278190079, %rsi       # imm = 0xFEFFFFFF
	andq	%rcx, %rsi
	orq	%rdx, %rsi
	movl	%esi, (%rax)
	movq	16(%r12), %rcx
	movq	%rcx, 16(%rax)
	movq	%rax, %r12
.LBB41_15:                              # %return
	movq	%r12, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.LBB41_1:                               # %if.then
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.change_address_1, %rcx
	movq	$1923, %rsi             # imm = 0x783
.LBB41_2:                               # %if.then
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp355:
	.size	change_address_1, .Ltmp355-change_address_1
	.cfi_endproc

	.globl	adjust_automodify_address_1
	.align	16, 0x90
	.type	adjust_automodify_address_1,@function
adjust_automodify_address_1:            # @adjust_automodify_address_1
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp360:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp361:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp362:
	.cfi_def_cfa_offset 32
.Ltmp363:
	.cfi_offset %rbx, -32
.Ltmp364:
	.cfi_offset %r14, -24
.Ltmp365:
	.cfi_offset %r15, -16
	movq	%r8, %rbx
	movq	%rcx, %r14
	movq	%rsi, %r15
	xorq	%rsi, %rsi
	movq	%rbx, %rcx
	callq	change_address_1
	xorq	%r8, %r8
	movq	%rax, %rdi
	movq	%r15, %rsi
	movq	%r14, %rdx
	movq	%rbx, %rcx
	popq	%rbx
	popq	%r14
	popq	%r15
	jmp	adjust_address_1  # TAILCALL
.Ltmp366:
	.size	adjust_automodify_address_1, .Ltmp366-adjust_automodify_address_1
	.cfi_endproc

	.globl	offset_address
	.align	16, 0x90
	.type	offset_address,@function
offset_address:                         # @offset_address
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp373:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp374:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp375:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp376:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp377:
	.cfi_def_cfa_offset 48
	subq	$48, %rsp
.Ltmp378:
	.cfi_def_cfa_offset 96
.Ltmp379:
	.cfi_offset %rbx, -48
.Ltmp380:
	.cfi_offset %r12, -40
.Ltmp381:
	.cfi_offset %r13, -32
.Ltmp382:
	.cfi_offset %r14, -24
.Ltmp383:
	.cfi_offset %r15, -16
	movl	target_flags(%rip), %eax
	movq	%rdx, %r15
	movq	%rsi, %r14
	movq	%rdi, %r13
	movq	8(%r13), %r12
	movq	$25, %rcx
	shrq	%cl, %rax
	andq	$1, %rax
	orq	$4, %rax
	movq	$75, %rdi
	movq	%rax, %rsi
	movq	%r12, %rdx
	movq	%r14, %rcx
	callq	simplify_gen_binary
	movzbl	2(%r13), %edi
	movq	%rax, %rbx
	movq	%rbx, %rsi
	callq	memory_address_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB43_4
# BB#1:                                 # %land.lhs.true
	movq	%r14, (%rsp)            # 8-byte Spill
	movl	(%r12), %edi
	movq	%rdi, %rax
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$75, %rax
	jne	.LBB43_4
# BB#2:                                 # %land.lhs.true5
	movq	8(%r12), %rax
	movq	pic_offset_table_rtx(%rip), %rcx
	cmpq	%rcx, %rax
	jne	.LBB43_4
# BB#3:                                 # %if.then
	movq	$16, %rcx
	shrq	%cl, %rdi
	andq	$255, %rdi
	movq	%r12, %rsi
	callq	force_reg
	movl	target_flags(%rip), %esi
	movq	$25, %rcx
	shrq	%cl, %rsi
	andq	$1, %rsi
	orq	$4, %rsi
	movq	$75, %rdi
	movq	%rax, %rdx
	movq	(%rsp), %rcx            # 8-byte Reload
	callq	simplify_gen_binary
	movq	%rax, %rbx
.LBB43_4:                               # %if.end
	movq	8(%r13), %rdi
	movq	%rbx, %rsi
	callq	update_temp_slot_address
	xorq	%rsi, %rsi
	movq	$1, %rcx
	xorq	%r12, %r12
	movq	%r13, %rdi
	movq	%rbx, %rdx
	callq	change_address_1
	movq	16(%r13), %rdx
	movq	%rax, %r13
	testq	%rdx, %rdx
	je	.LBB43_5
# BB#6:                                 # %cond.true44
	movq	(%rdx), %r12
	movq	8(%rdx), %rax
	movl	32(%rdx), %esi
	jmp	.LBB43_7
.LBB43_5:
	movq	$8, %rsi
	xorq	%rdx, %rdx
	xorq	%rax, %rax
.LBB43_7:                               # %cond.end49
	movq	$3, %rcx
	shlq	%cl, %r15
	cmpq	%r15, %rsi
	jae	.LBB43_10
# BB#8:                                 # %cond.true53
	movq	$8, %r15
	testq	%rdx, %rdx
	je	.LBB43_10
# BB#9:                                 # %cond.true59
	movl	32(%rdx), %r15d
.LBB43_10:                              # %cond.end70
	testq	%r12, %r12
	jne	.LBB43_13
# BB#11:                                # %cond.end70
	testq	%rax, %rax
	jne	.LBB43_13
# BB#12:                                # %cond.end70
	xorq	%rcx, %rcx
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	andq	%r15, %rdx
	cmpq	$8, %rdx
	je	.LBB43_15
.LBB43_13:                              # %if.end.i
	movq	%r12, 8(%rsp)
	movq	%rax, 16(%rsp)
	movq	mem_attrs_htab(%rip), %rdi
	movq	$0, 32(%rsp)
	movq	$0, 24(%rsp)
	movl	%r15d, 40(%rsp)
	leaq	8(%rsp), %rsi
	movq	$1, %rdx
	callq	htab_find_slot
	movq	%rax, %rbx
	movq	(%rbx), %rcx
	testq	%rcx, %rcx
	jne	.LBB43_15
# BB#14:                                # %if.then21.i
	movq	$40, %rdi
	callq	ggc_alloc
	movq	%rax, (%rbx)
	movq	40(%rsp), %rcx
	movq	%rcx, 32(%rax)
	movq	32(%rsp), %rcx
	movq	%rcx, 24(%rax)
	movq	24(%rsp), %rcx
	movq	%rcx, 16(%rax)
	movq	8(%rsp), %rcx
	movq	16(%rsp), %rdx
	movq	%rdx, 8(%rax)
	movq	%rcx, (%rax)
	movq	(%rbx), %rcx
.LBB43_15:                              # %get_mem_attrs.exit
	movq	%rcx, 16(%r13)
	movq	%r13, %rax
	addq	$48, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp384:
	.size	offset_address, .Ltmp384-offset_address
	.cfi_endproc

	.globl	replace_equiv_address_nv
	.align	16, 0x90
	.type	replace_equiv_address_nv,@function
replace_equiv_address_nv:               # @replace_equiv_address_nv
	.cfi_startproc
# BB#0:                                 # %entry
	movq	%rsi, %rax
	xorq	%rsi, %rsi
	xorq	%rcx, %rcx
	movq	%rax, %rdx
	jmp	change_address_1  # TAILCALL
.Ltmp385:
	.size	replace_equiv_address_nv, .Ltmp385-replace_equiv_address_nv
	.cfi_endproc

	.globl	widen_memory_access
	.align	16, 0x90
	.type	widen_memory_access,@function
widen_memory_access:                    # @widen_memory_access
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp392:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp393:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp394:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp395:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp396:
	.cfi_def_cfa_offset 48
	subq	$80, %rsp
.Ltmp397:
	.cfi_def_cfa_offset 128
.Ltmp398:
	.cfi_offset %rbx, -48
.Ltmp399:
	.cfi_offset %r12, -40
.Ltmp400:
	.cfi_offset %r13, -32
.Ltmp401:
	.cfi_offset %r14, -24
.Ltmp402:
	.cfi_offset %r15, -16
	movq	%rsi, %rbx
	movq	$1, %rcx
	movq	$1, %r8
                                        # kill: RSI<def> RBX<kill>
	callq	adjust_address_1
	movq	%rax, 24(%rsp)          # 8-byte Spill
	movq	16(%rax), %rax
	testq	%rax, %rax
	je	.LBB45_1
# BB#2:                                 # %cond.end15
	movq	16(%rax), %r12
	movq	$32, %rcx
	movq	%rbx, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movzbl	mode_size(%rdx), %r13d
	testq	%r12, %r12
	leaq	mode_size(%rdx), %rcx
	movq	%rcx, (%rsp)            # 8-byte Spill
	movq	%r13, %rcx
	je	.LBB45_3
# BB#4:                                 # %while.cond.preheader
	movq	%rbx, 8(%rsp)           # 8-byte Spill
	movq	8(%rax), %r15
	testq	%r15, %r15
	je	.LBB45_5
# BB#6:                                 # %while.body.lr.ph
	movq	%rcx, 16(%rsp)          # 8-byte Spill
	.align	16, 0x90
.LBB45_7:                               # %while.body
                                        # =>This Inner Loop Header: Depth=1
	movzbl	16(%r15), %eax
	cmpq	$39, %rax
	jne	.LBB45_18
# BB#8:                                 # %if.then21
                                        #   in Loop: Header=BB45_7 Depth=1
	movq	40(%r15), %rbx
	movq	64(%rbx), %rdi
	testq	%rdi, %rdi
	je	.LBB45_25
# BB#9:                                 # %if.end25
                                        #   in Loop: Header=BB45_7 Depth=1
	movzbl	16(%rdi), %eax
	cmpq	$25, %rax
	jne	.LBB45_12
# BB#10:                                # %land.lhs.true
                                        #   in Loop: Header=BB45_7 Depth=1
	movq	%r13, %rsi
	callq	compare_tree_int
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	js	.LBB45_12
# BB#11:                                # %land.lhs.true40
                                        #   in Loop: Header=BB45_7 Depth=1
	cmpq	$0, 8(%r12)
	jns	.LBB45_26
.LBB45_12:                              # %if.end46
                                        #   in Loop: Header=BB45_7 Depth=1
	movq	88(%rbx), %rdi
	movq	$1, %rsi
	callq	host_integerp
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB45_25
# BB#13:                                # %if.end51
                                        #   in Loop: Header=BB45_7 Depth=1
	movq	32(%r15), %r15
	movq	8(%r12), %r14
	movq	88(%rbx), %rdi
	movq	$1, %rsi
	callq	tree_low_cst
	movq	160(%rbx), %rdi
	movq	%rax, %rbx
	addq	%r14, %rbx
	movq	$1, %rsi
	callq	tree_low_cst
	movq	$63, %rcx
	movq	%rax, %rsi
	sarq	%cl, %rsi
	movq	$61, %rcx
	shrq	%cl, %rsi
	addq	%rax, %rsi
	movq	$3, %rcx
	sarq	%cl, %rsi
	leaq	(%rbx,%rsi), %rdx
	movq	%rdx, 32(%rsp)
	leaq	64(%rsi,%rbx), %rax
	cmpq	$128, %rax
	ja	.LBB45_16
# BB#14:                                # %if.then.i
                                        #   in Loop: Header=BB45_7 Depth=1
	movq	const_int_rtx+512(,%rdx,8), %r12
	jmp	.LBB45_15
	.align	16, 0x90
.LBB45_16:                              # %if.end.i
                                        #   in Loop: Header=BB45_7 Depth=1
	movq	const_int_htab(%rip), %rdi
	movq	$1, %rcx
	leaq	32(%rsp), %rsi
	callq	htab_find_slot_with_hash
	movq	%rax, %r14
	movq	(%r14), %r12
	testq	%r12, %r12
	jne	.LBB45_15
# BB#17:                                # %if.then4.i
                                        #   in Loop: Header=BB45_7 Depth=1
	movq	32(%rsp), %rdx
	movq	$54, %rdi
	xorq	%rsi, %rsi
	callq	gen_rtx_fmt_w
	movq	%rax, %r12
	movq	%r12, (%r14)
.LBB45_15:                              # %while.cond.backedge
                                        #   in Loop: Header=BB45_7 Depth=1
	testq	%r15, %r15
	jne	.LBB45_7
	jmp	.LBB45_25
.LBB45_1:                               # %cond.end15.thread
	movq	$32, %rcx
	movq	%rbx, %rax
	movq	%rbx, 8(%rsp)           # 8-byte Spill
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movzbl	mode_size(%rax), %ecx
	leaq	mode_size(%rax), %rax
	movq	%rax, (%rsp)            # 8-byte Spill
	xorq	%r12, %r12
	jmp	.LBB45_5
.LBB45_3:
	movq	%rbx, 8(%rsp)           # 8-byte Spill
	xorq	%r12, %r12
.LBB45_5:                               # %while.cond.preheader.while.end.loopexit_crit_edge
	movq	%rcx, 16(%rsp)          # 8-byte Spill
	movq	%rcx, %r13
	andq	$255, %r13
.LBB45_25:                              # %if.else102
	xorq	%r15, %r15
.LBB45_26:                              # %while.end
	testq	%r15, %r15
	movq	%r15, %r14
	je	.LBB45_28
# BB#27:                                # %while.end
	movq	%r12, %r14
.LBB45_28:                              # %while.end
	movq	%r13, 40(%rsp)
	leaq	64(%r13), %rax
	cmpq	$128, %rax
	movq	24(%rsp), %r12          # 8-byte Reload
	ja	.LBB45_30
# BB#29:                                # %if.then.i158
	movq	const_int_rtx+512(,%r13,8), %rax
	jmp	.LBB45_32
.LBB45_30:                              # %if.end.i162
	movq	const_int_htab(%rip), %rdi
	movq	16(%rsp), %rdx          # 8-byte Reload
	andq	$255, %rdx
	leaq	40(%rsp), %rsi
	movq	$1, %rcx
	callq	htab_find_slot_with_hash
	movq	%rax, %rbx
	movq	(%rbx), %rax
	testq	%rax, %rax
	jne	.LBB45_32
# BB#31:                                # %if.then4.i164
	movq	40(%rsp), %rdx
	movq	$54, %rdi
	xorq	%rsi, %rsi
	callq	gen_rtx_fmt_w
	movq	%rax, (%rbx)
.LBB45_32:                              # %gen_rtx_CONST_INT.exit167
	movq	16(%r12), %rdx
	testq	%rdx, %rdx
	movq	$8, %rcx
	je	.LBB45_34
# BB#33:                                # %cond.true114
	movl	32(%rdx), %ecx
.LBB45_34:                              # %cond.end119
	testq	%r15, %r15
	jne	.LBB45_42
# BB#35:                                # %cond.end119
	testq	%r14, %r14
	jne	.LBB45_42
# BB#36:                                # %land.lhs.true4.i
	movabsq	$4294967295, %rsi       # imm = 0xFFFFFFFF
	testq	%rax, %rax
	je	.LBB45_40
# BB#37:                                # %lor.lhs.false.i
	movq	8(%rsp), %rdx           # 8-byte Reload
	andq	%rsi, %rdx
	cmpq	$51, %rdx
	je	.LBB45_42
# BB#38:                                # %land.lhs.true7.i
	movq	(%rsp), %rdx            # 8-byte Reload
	movzbl	(%rdx), %edx
	movq	8(%rax), %rdi
	cmpq	%rdi, %rdx
	jne	.LBB45_42
# BB#39:                                # %land.lhs.true7.i
	xorq	%rdx, %rdx
	andq	%rcx, %rsi
	jmp	.LBB45_41
.LBB45_40:                              # %land.lhs.true11.i
	andq	%rcx, %rsi
	xorq	%rdx, %rdx
.LBB45_41:                              # %land.lhs.true11.i
	cmpq	$8, %rsi
	je	.LBB45_44
.LBB45_42:                              # %if.end.i171
	movq	$0, 40(%rsp)
	movq	%r15, 48(%rsp)
	movq	mem_attrs_htab(%rip), %rdi
	movq	%r14, 56(%rsp)
	movq	%rax, 64(%rsp)
	movl	%ecx, 72(%rsp)
	leaq	40(%rsp), %rsi
	movq	$1, %rdx
	callq	htab_find_slot
	movq	%rax, %rbx
	movq	(%rbx), %rdx
	testq	%rdx, %rdx
	jne	.LBB45_44
# BB#43:                                # %if.then21.i
	movq	$40, %rdi
	callq	ggc_alloc
	movq	%rax, (%rbx)
	movq	72(%rsp), %rcx
	movq	%rcx, 32(%rax)
	movq	64(%rsp), %rcx
	movq	%rcx, 24(%rax)
	movq	56(%rsp), %rcx
	movq	%rcx, 16(%rax)
	movq	40(%rsp), %rcx
	movq	48(%rsp), %rdx
	movq	%rdx, 8(%rax)
	movq	%rcx, (%rax)
	movq	(%rbx), %rdx
.LBB45_44:                              # %get_mem_attrs.exit
	movq	%rdx, 16(%r12)
	movq	%r12, %rax
	addq	$80, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB45_18:                              # %if.else
	movzbl	tree_code_type(%rax), %eax
	cmpq	$100, %rax
	jne	.LBB45_25
# BB#19:                                # %land.lhs.true74
	movq	64(%r15), %rdi
	testq	%rdi, %rdi
	je	.LBB45_25
# BB#20:                                # %land.lhs.true78
	movzbl	16(%rdi), %eax
	cmpq	$25, %rax
	jne	.LBB45_25
# BB#21:                                # %land.lhs.true87
	movq	%r13, %rsi
	callq	compare_tree_int
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	js	.LBB45_25
# BB#22:                                # %land.lhs.true94
	testq	%r12, %r12
	je	.LBB45_23
# BB#24:                                # %lor.lhs.false
	cmpq	$0, 8(%r12)
	jns	.LBB45_26
	jmp	.LBB45_25
.LBB45_23:
	xorq	%r12, %r12
	jmp	.LBB45_26
.Ltmp403:
	.size	widen_memory_access, .Ltmp403-widen_memory_access
	.cfi_endproc

	.globl	gen_label_rtx
	.align	16, 0x90
	.type	gen_label_rtx,@function
gen_label_rtx:                          # @gen_label_rtx
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp406:
	.cfi_def_cfa_offset 16
	subq	$16, %rsp
.Ltmp407:
	.cfi_def_cfa_offset 32
.Ltmp408:
	.cfi_offset %rbx, -16
	movl	label_num(%rip), %r9d
	leaq	1(%r9), %rax
	movl	%eax, label_num(%rip)
	movq	$0, 8(%rsp)
	movq	$0, (%rsp)
	movq	$36, %rdi
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	xorq	%r8, %r8
	xorq	%rbx, %rbx
	callq	gen_rtx_fmt_iuu00iss
	movl	%ebx, 32(%rax)
	movq	$0, 64(%rax)
	addq	$16, %rsp
	popq	%rbx
	retq
.Ltmp409:
	.size	gen_label_rtx, .Ltmp409-gen_label_rtx
	.cfi_endproc

	.globl	set_new_first_and_last_insn
	.align	16, 0x90
	.type	set_new_first_and_last_insn,@function
set_new_first_and_last_insn:            # @set_new_first_and_last_insn
	.cfi_startproc
# BB#0:                                 # %entry
	movq	cfun(%rip), %rax
	movq	24(%rax), %rax
	movq	%rdi, 8(%rax)
	movq	cfun(%rip), %rax
	movq	24(%rax), %rax
	movq	%rsi, 16(%rax)
	movq	cfun(%rip), %rax
	movq	24(%rax), %r8
	xorq	%rax, %rax
	testq	%rdi, %rdi
	movl	%eax, 40(%r8)
	movq	$1, %rcx
	je	.LBB47_5
	.align	16, 0x90
.LBB47_1:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movslq	8(%rdi), %rsi
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rsi, %rdx
	jge	.LBB47_3
# BB#2:                                 # %for.body
                                        #   in Loop: Header=BB47_1 Depth=1
	movq	%rsi, %rax
.LBB47_3:                               # %for.body
                                        #   in Loop: Header=BB47_1 Depth=1
	movl	%eax, 40(%r8)
	movq	24(%rdi), %rdi
	testq	%rdi, %rdi
	jne	.LBB47_1
# BB#4:                                 # %for.cond.for.end_crit_edge
	incq	%rax
	movq	%rax, %rcx
.LBB47_5:                               # %for.end
	movl	%ecx, 40(%r8)
	retq
.Ltmp410:
	.size	set_new_first_and_last_insn, .Ltmp410-set_new_first_and_last_insn
	.cfi_endproc

	.globl	set_new_first_and_last_label_num
	.align	16, 0x90
	.type	set_new_first_and_last_label_num,@function
set_new_first_and_last_label_num:       # @set_new_first_and_last_label_num
	.cfi_startproc
# BB#0:                                 # %entry
	movl	label_num(%rip), %eax
	movq	cfun(%rip), %rcx
	movl	%eax, base_label_num(%rip)
	movq	24(%rcx), %rax
	movl	%edi, 4(%rax)
	movl	%esi, last_label_num(%rip)
	retq
.Ltmp411:
	.size	set_new_first_and_last_label_num, .Ltmp411-set_new_first_and_last_label_num
	.cfi_endproc

	.globl	set_new_last_label_num
	.align	16, 0x90
	.type	set_new_last_label_num,@function
set_new_last_label_num:                 # @set_new_last_label_num
	.cfi_startproc
# BB#0:                                 # %entry
	movl	label_num(%rip), %eax
	movl	%eax, base_label_num(%rip)
	movl	%edi, last_label_num(%rip)
	retq
.Ltmp412:
	.size	set_new_last_label_num, .Ltmp412-set_new_last_label_num
	.cfi_endproc

	.globl	restore_emit_status
	.align	16, 0x90
	.type	restore_emit_status,@function
restore_emit_status:                    # @restore_emit_status
	.cfi_startproc
# BB#0:                                 # %entry
	xorq	%rax, %rax
	movl	%eax, last_label_num(%rip)
	movq	$0, sequence_result+32(%rip)
	movq	$0, sequence_result+24(%rip)
	movq	$0, sequence_result+16(%rip)
	movq	$0, sequence_result+8(%rip)
	movq	$0, sequence_result(%rip)
	retq
.Ltmp413:
	.size	restore_emit_status, .Ltmp413-restore_emit_status
	.cfi_endproc

	.globl	clear_emit_caches
	.align	16, 0x90
	.type	clear_emit_caches,@function
clear_emit_caches:                      # @clear_emit_caches
	.cfi_startproc
# BB#0:                                 # %entry
	movq	$0, sequence_result+32(%rip)
	movq	$0, sequence_result+24(%rip)
	movq	$0, sequence_result+16(%rip)
	movq	$0, sequence_result+8(%rip)
	movq	$0, sequence_result(%rip)
	retq
.Ltmp414:
	.size	clear_emit_caches, .Ltmp414-clear_emit_caches
	.cfi_endproc

	.globl	free_emit_status
	.align	16, 0x90
	.type	free_emit_status,@function
free_emit_status:                       # @free_emit_status
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp417:
	.cfi_def_cfa_offset 16
.Ltmp418:
	.cfi_offset %rbx, -16
	movq	%rdi, %rbx
	movq	24(%rbx), %rax
	movq	80(%rax), %rdi
	callq	free
	movq	24(%rbx), %rax
	movq	64(%rax), %rdi
	callq	free
	movq	24(%rbx), %rax
	movq	72(%rax), %rdi
	callq	free
	movq	24(%rbx), %rdi
	callq	free
	movq	$0, 24(%rbx)
	popq	%rbx
	retq
.Ltmp419:
	.size	free_emit_status, .Ltmp419-free_emit_status
	.cfi_endproc

	.globl	unshare_all_rtl
	.align	16, 0x90
	.type	unshare_all_rtl,@function
unshare_all_rtl:                        # @unshare_all_rtl
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp424:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp425:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp426:
	.cfi_def_cfa_offset 32
.Ltmp427:
	.cfi_offset %rbx, -32
.Ltmp428:
	.cfi_offset %r14, -24
.Ltmp429:
	.cfi_offset %r15, -16
	movq	%rdi, %r14
	movq	88(%r14), %rbx
	movq	%rsi, %r15
	jmp	.LBB53_1
	.align	16, 0x90
.LBB53_4:                               # %cond.end
                                        #   in Loop: Header=BB53_1 Depth=1
	callq	copy_rtx_if_shared
	movq	%rax, 144(%rbx)
	movq	(%rbx), %rbx
.LBB53_1:                               # %entry
                                        # =>This Inner Loop Header: Depth=1
	testq	%rbx, %rbx
	je	.LBB53_5
# BB#2:                                 # %for.body
                                        #   in Loop: Header=BB53_1 Depth=1
	movq	144(%rbx), %rdi
	testq	%rdi, %rdi
	jne	.LBB53_4
# BB#3:                                 # %cond.false
                                        #   in Loop: Header=BB53_1 Depth=1
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	make_decl_rtl
	movq	144(%rbx), %rdi
	jmp	.LBB53_4
.LBB53_5:                               # %for.end
	movq	104(%r14), %rdi
	callq	unshare_all_decls
	jmp	.LBB53_6
	.align	16, 0x90
.LBB53_9:                               # %for.inc.i
                                        #   in Loop: Header=BB53_6 Depth=1
	movq	24(%r15), %r15
.LBB53_6:                               # %for.end
                                        # =>This Inner Loop Header: Depth=1
	testq	%r15, %r15
	je	.LBB53_10
# BB#7:                                 # %for.body.i
                                        #   in Loop: Header=BB53_6 Depth=1
	movzbl	(%r15), %eax
	movzbl	rtx_class(%rax), %eax
	cmpq	$105, %rax
	jne	.LBB53_9
# BB#8:                                 # %if.then.i
                                        #   in Loop: Header=BB53_6 Depth=1
	movq	32(%r15), %rdi
	callq	copy_rtx_if_shared
	movq	%rax, 32(%r15)
	movq	56(%r15), %rdi
	callq	copy_rtx_if_shared
	movq	%rax, 56(%r15)
	movq	48(%r15), %rdi
	callq	copy_rtx_if_shared
	movq	%rax, 48(%r15)
	jmp	.LBB53_9
.LBB53_10:                              # %unshare_all_rtl_1.exit
	movq	cfun(%rip), %rax
	movq	216(%rax), %rdi
	callq	copy_rtx_if_shared
	movq	cfun(%rip), %rcx
	movq	%rax, 216(%rcx)
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp430:
	.size	unshare_all_rtl, .Ltmp430-unshare_all_rtl
	.cfi_endproc

	.globl	copy_rtx_if_shared
	.align	16, 0x90
	.type	copy_rtx_if_shared,@function
copy_rtx_if_shared:                     # @copy_rtx_if_shared
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp437:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp438:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp439:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp440:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp441:
	.cfi_def_cfa_offset 48
	subq	$32, %rsp
.Ltmp442:
	.cfi_def_cfa_offset 80
.Ltmp443:
	.cfi_offset %rbx, -48
.Ltmp444:
	.cfi_offset %r12, -40
.Ltmp445:
	.cfi_offset %r13, -32
.Ltmp446:
	.cfi_offset %r14, -24
.Ltmp447:
	.cfi_offset %r15, -16
	xorq	%rax, %rax
	testq	%rdi, %rdi
	je	.LBB54_4
# BB#1:                                 # %if.end
	movl	(%rdi), %eax
	movq	%rax, %r14
	andq	$65535, %r14            # imm = 0xFFFF
	leaq	-32(%r14), %rcx
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	andq	%r15, %rcx
	cmpq	$39, %rcx
	ja	.LBB54_12
# BB#2:                                 # %if.end
	movq	$1, %rdx
	shlq	%cl, %rdx
	movabsq	$757688434751, %rsi     # imm = 0xB069C0003F
	testq	%rsi, %rdx
	je	.LBB54_5
.LBB54_3:
	movq	%rdi, %rax
	jmp	.LBB54_4
.LBB54_5:                               # %if.end
	cmpq	$26, %rcx
	jne	.LBB54_6
# BB#9:                                 # %sw.bb1
	movq	8(%rdi), %rcx
	movzbl	(%rcx), %edx
	cmpq	$75, %rdx
	jne	.LBB54_12
# BB#10:                                # %land.lhs.true
	movq	8(%rcx), %rdx
	movzbl	(%rdx), %edx
	cmpq	$68, %rdx
	jne	.LBB54_12
# BB#11:                                # %land.lhs.true14
	movq	16(%rcx), %rcx
	movzbl	(%rcx), %ecx
	cmpq	$54, %rcx
	je	.LBB54_3
	jmp	.LBB54_12
.LBB54_6:                               # %if.end
	cmpq	$34, %rcx
	jne	.LBB54_12
# BB#7:                                 # %sw.bb27
	movq	8(%rdi), %rcx
	movzbl	(%rcx), %ecx
	addq	$-54, %rcx
	andq	%r15, %rcx
	cmpq	$14, %rcx
	ja	.LBB54_12
# BB#8:                                 # %sw.bb27
	movq	$1, %rdx
	shlq	%cl, %rdx
	testq	$24595, %rdx            # imm = 0x6013
	jne	.LBB54_3
.LBB54_12:                              # %sw.epilog
	movq	%rdi, %r12
	testq	$536870912, %rax        # imm = 0x20000000
	je	.LBB54_13
# BB#14:                                # %if.then65
	movq	%r14, %rdi
	callq	rtx_alloc
	movzbl	rtx_length(%r14), %r13d
	movq	%rax, %rbx
	leaq	8(,%r13,8), %rdx
	movq	%rbx, %rdi
	movq	%r12, %rsi
	callq	memcpy
	movl	(%rbx), %eax
	movq	$1, %rcx
	jmp	.LBB54_15
.LBB54_13:                              # %sw.epilog.if.end67_crit_edge
	movzbl	rtx_length(%r14), %r13d
	xorq	%rcx, %rcx
	movq	%r12, %rbx
.LBB54_15:                              # %if.end67
	orq	$536870912, %rax        # imm = 0x20000000
	movl	%eax, (%rbx)
	testq	$255, %r13
	movq	%r15, %r12
	je	.LBB54_16
# BB#17:                                # %for.body.lr.ph
	movq	%rbx, %r15
	movq	rtx_format(,%r14,8), %rdx
	andq	$255, %r13
	movq	%r13, 16(%rsp)          # 8-byte Spill
	xorq	%r13, %r13
	testq	%rcx, %rcx
	je	.LBB54_29
	.align	16, 0x90
.LBB54_18:                              # %for.body.us
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB54_23 Depth 2
                                        #     Child Loop BB54_25 Depth 2
	movsbq	(%rdx), %rax
	incq	%rdx
	andq	%r12, %rax
	cmpq	$101, %rax
	jne	.LBB54_19
# BB#27:                                # %sw.bb78.us
                                        #   in Loop: Header=BB54_18 Depth=1
	movq	%rdx, 24(%rsp)          # 8-byte Spill
	movq	8(%r15,%r13,8), %rdi
	callq	copy_rtx_if_shared
	movq	%rax, 8(%r15,%r13,8)
	jmp	.LBB54_28
	.align	16, 0x90
.LBB54_19:                              # %for.body.us
                                        #   in Loop: Header=BB54_18 Depth=1
	movq	%rdx, 24(%rsp)          # 8-byte Spill
	cmpq	$69, %rax
	jne	.LBB54_28
# BB#20:                                # %sw.bb88.us
                                        #   in Loop: Header=BB54_18 Depth=1
	movq	8(%r15,%r13,8), %r14
	testq	%r14, %r14
	je	.LBB54_28
# BB#21:                                # %if.then94.us
                                        #   in Loop: Header=BB54_18 Depth=1
	movslq	(%r14), %r12
	testq	%r12, %r12
	jle	.LBB54_28
# BB#22:                                # %for.body.i.preheader.us
                                        #   in Loop: Header=BB54_18 Depth=1
	movq	%r12, %rdi
	callq	rtvec_alloc
	movq	$1, %rcx
	movabsq	$4294967295, %rsi       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB54_23:                              # %for.body.i.us
                                        #   Parent Loop BB54_18 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	(%r14,%rcx,8), %rdx
	movq	%rdx, (%rax,%rcx,8)
	movq	%rcx, %rdx
	andq	%rsi, %rdx
	movq	%r12, %rbx
	andq	%rsi, %rbx
	incq	%rcx
	cmpq	%rbx, %rdx
	jne	.LBB54_23
# BB#24:                                # %for.cond114.preheader.us
                                        #   in Loop: Header=BB54_18 Depth=1
	testq	%r12, %r12
	movq	%rax, 8(%r15,%r13,8)
	movq	$1, %r14
	jle	.LBB54_28
	jmp	.LBB54_25
	.align	16, 0x90
.LBB54_26:                              # %for.body117.us.for.body117.us_crit_edge
                                        #   in Loop: Header=BB54_25 Depth=2
	movq	8(%r15,%r13,8), %rax
	incq	%r14
.LBB54_25:                              # %for.body117.us
                                        #   Parent Loop BB54_18 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	(%rax,%r14,8), %rdi
	callq	copy_rtx_if_shared
	movq	8(%r15,%r13,8), %rcx
	movq	%rax, (%rcx,%r14,8)
	movq	%r14, %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	cmpq	%rbx, %rax
	jne	.LBB54_26
	.align	16, 0x90
.LBB54_28:                              # %for.inc135.us
                                        #   in Loop: Header=BB54_18 Depth=1
	incq	%r13
	movq	$32, %rcx
	movq	%r13, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	16(%rsp), %rcx          # 8-byte Reload
	cmpq	%rcx, %rax
	movabsq	$4294967295, %r12       # imm = 0xFFFFFFFF
	movq	24(%rsp), %rdx          # 8-byte Reload
	jl	.LBB54_18
	jmp	.LBB54_33
	.align	16, 0x90
.LBB54_29:                              # %for.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB54_36 Depth 2
	movsbq	(%rdx), %rax
	incq	%rdx
	andq	%r12, %rax
	cmpq	$69, %rax
	jne	.LBB54_30
# BB#34:                                # %sw.bb88
                                        #   in Loop: Header=BB54_29 Depth=1
	movq	%rdx, 24(%rsp)          # 8-byte Spill
	movq	8(%r15,%r13,8), %rax
	testq	%rax, %rax
	je	.LBB54_32
# BB#35:                                # %for.cond114.preheader
                                        #   in Loop: Header=BB54_29 Depth=1
	movslq	(%rax), %r14
	testq	%r14, %r14
	movq	$1, %rbx
	jle	.LBB54_32
	jmp	.LBB54_36
	.align	16, 0x90
.LBB54_37:                              # %for.body117.for.body117_crit_edge
                                        #   in Loop: Header=BB54_36 Depth=2
	movq	8(%r15,%r13,8), %rax
	incq	%rbx
.LBB54_36:                              # %for.body117
                                        #   Parent Loop BB54_29 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	(%rax,%rbx,8), %rdi
	callq	copy_rtx_if_shared
	movq	8(%r15,%r13,8), %rcx
	movq	%rax, (%rcx,%rbx,8)
	movq	%rbx, %rax
	andq	%r12, %rax
	movq	%r14, %rcx
	andq	%r12, %rcx
	cmpq	%rcx, %rax
	jne	.LBB54_37
	jmp	.LBB54_32
	.align	16, 0x90
.LBB54_30:                              # %for.body
                                        #   in Loop: Header=BB54_29 Depth=1
	movq	%rdx, 24(%rsp)          # 8-byte Spill
	cmpq	$101, %rax
	jne	.LBB54_32
# BB#31:                                # %sw.bb78
                                        #   in Loop: Header=BB54_29 Depth=1
	movq	8(%r15,%r13,8), %rdi
	callq	copy_rtx_if_shared
	movq	%rax, 8(%r15,%r13,8)
.LBB54_32:                              # %for.inc135
                                        #   in Loop: Header=BB54_29 Depth=1
	incq	%r13
	movq	$32, %rcx
	movq	%r13, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	16(%rsp), %rcx          # 8-byte Reload
	cmpq	%rcx, %rax
	movq	24(%rsp), %rdx          # 8-byte Reload
	jl	.LBB54_29
.LBB54_33:
	movq	%r15, %rax
	jmp	.LBB54_4
.LBB54_16:
	movq	%rbx, %rax
.LBB54_4:                               # %return
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp448:
	.size	copy_rtx_if_shared, .Ltmp448-copy_rtx_if_shared
	.cfi_endproc

	.align	16, 0x90
	.type	unshare_all_decls,@function
unshare_all_decls:                      # @unshare_all_decls
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp452:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp453:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp454:
	.cfi_def_cfa_offset 32
.Ltmp455:
	.cfi_offset %rbx, -24
.Ltmp456:
	.cfi_offset %r14, -16
	movq	%rdi, %r14
	movq	32(%r14), %rbx
	jmp	.LBB55_1
	.align	16, 0x90
.LBB55_4:                               # %for.inc
                                        #   in Loop: Header=BB55_1 Depth=1
	movq	(%rbx), %rbx
.LBB55_1:                               # %entry
                                        # =>This Inner Loop Header: Depth=1
	testq	%rbx, %rbx
	je	.LBB55_5
# BB#2:                                 # %for.body
                                        #   in Loop: Header=BB55_1 Depth=1
	movq	144(%rbx), %rdi
	testq	%rdi, %rdi
	je	.LBB55_4
# BB#3:                                 # %cond.end
                                        #   in Loop: Header=BB55_1 Depth=1
	callq	copy_rtx_if_shared
	movq	%rax, 144(%rbx)
	jmp	.LBB55_4
.LBB55_5:                               # %for.end
	movq	40(%r14), %rbx
	jmp	.LBB55_7
	.align	16, 0x90
.LBB55_6:                               # %for.body13
                                        #   in Loop: Header=BB55_7 Depth=1
	movq	%rbx, %rdi
	callq	unshare_all_decls
	movq	(%rbx), %rbx
.LBB55_7:                               # %for.body13
                                        # =>This Inner Loop Header: Depth=1
	testq	%rbx, %rbx
	jne	.LBB55_6
# BB#8:                                 # %for.end17
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp457:
	.size	unshare_all_decls, .Ltmp457-unshare_all_decls
	.cfi_endproc

	.globl	unshare_all_rtl_again
	.align	16, 0x90
	.type	unshare_all_rtl_again,@function
unshare_all_rtl_again:                  # @unshare_all_rtl_again
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp461:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp462:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp463:
	.cfi_def_cfa_offset 32
.Ltmp464:
	.cfi_offset %rbx, -24
.Ltmp465:
	.cfi_offset %r14, -16
	movq	%rdi, %r14
	testq	%r14, %r14
	je	.LBB56_5
# BB#1:
	movq	%r14, %rbx
	.align	16, 0x90
.LBB56_2:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movzbl	(%rbx), %eax
	movzbl	rtx_class(%rax), %eax
	cmpq	$105, %rax
	jne	.LBB56_4
# BB#3:                                 # %if.then
                                        #   in Loop: Header=BB56_2 Depth=1
	movq	32(%rbx), %rdi
	callq	reset_used_flags
	movq	56(%rbx), %rdi
	callq	reset_used_flags
	movq	48(%rbx), %rdi
	callq	reset_used_flags
.LBB56_4:                               # %for.inc
                                        #   in Loop: Header=BB56_2 Depth=1
	movq	24(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB56_2
.LBB56_5:                               # %for.end
	movq	cfun(%rip), %rax
	movq	48(%rax), %rax
	movq	104(%rax), %rdi
	callq	reset_used_decls
	movq	cfun(%rip), %rax
	movq	48(%rax), %rcx
	movq	88(%rcx), %rbx
	testq	%rbx, %rbx
	je	.LBB56_10
	.align	16, 0x90
.LBB56_6:                               # %for.body18
                                        # =>This Inner Loop Header: Depth=1
	movq	144(%rbx), %rdi
	testq	%rdi, %rdi
	jne	.LBB56_8
# BB#7:                                 # %cond.false
                                        #   in Loop: Header=BB56_6 Depth=1
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	make_decl_rtl
	movq	144(%rbx), %rdi
.LBB56_8:                               # %cond.end
                                        #   in Loop: Header=BB56_6 Depth=1
	callq	reset_used_flags
	movq	(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB56_6
# BB#9:                                 # %for.cond16.for.end26_crit_edge
	movq	cfun(%rip), %rax
.LBB56_10:                              # %for.end26
	movq	216(%rax), %rdi
	callq	reset_used_flags
	movq	cfun(%rip), %rax
	movq	48(%rax), %rdi
	movq	%r14, %rsi
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	jmp	unshare_all_rtl  # TAILCALL
.Ltmp466:
	.size	unshare_all_rtl_again, .Ltmp466-unshare_all_rtl_again
	.cfi_endproc

	.globl	reset_used_flags
	.align	16, 0x90
	.type	reset_used_flags,@function
reset_used_flags:                       # @reset_used_flags
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp473:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp474:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp475:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp476:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp477:
	.cfi_def_cfa_offset 48
.Ltmp478:
	.cfi_offset %rbx, -48
.Ltmp479:
	.cfi_offset %r12, -40
.Ltmp480:
	.cfi_offset %r13, -32
.Ltmp481:
	.cfi_offset %r14, -24
.Ltmp482:
	.cfi_offset %r15, -16
	movq	%rdi, %r13
	testq	%r13, %r13
	je	.LBB57_10
# BB#1:                                 # %if.end
	movl	(%r13), %edx
	movq	%rdx, %rax
	andq	$65535, %rax            # imm = 0xFFFF
	leaq	-32(%rax), %rcx
	movabsq	$4294967295, %rsi       # imm = 0xFFFFFFFF
	andq	%rsi, %rcx
	cmpq	$39, %rcx
	ja	.LBB57_3
# BB#2:                                 # %if.end
	movq	$1, %rsi
	shlq	%cl, %rsi
	movabsq	$790974431295, %rcx     # imm = 0xB829C0003F
	testq	%rcx, %rsi
	jne	.LBB57_10
.LBB57_3:                               # %sw.epilog
	movzbl	rtx_length(%rax), %r15d
	movabsq	$3758096383, %rcx       # imm = 0xDFFFFFFF
	andq	%rcx, %rdx
	movl	%edx, (%r13)
	testq	%r15, %r15
	je	.LBB57_10
# BB#4:                                 # %for.body.lr.ph
	movq	rtx_format(,%rax,8), %r12
	xorq	%rbx, %rbx
	.align	16, 0x90
.LBB57_5:                               # %for.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB57_8 Depth 2
	movsbq	(%r12), %rax
	incq	%r12
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	cmpq	$101, %rax
	jne	.LBB57_6
# BB#11:                                # %sw.bb9
                                        #   in Loop: Header=BB57_5 Depth=1
	movq	8(%r13,%rbx,8), %rdi
	callq	reset_used_flags
	jmp	.LBB57_9
	.align	16, 0x90
.LBB57_6:                               # %for.body
                                        #   in Loop: Header=BB57_5 Depth=1
	cmpq	$69, %rax
	jne	.LBB57_9
# BB#7:                                 # %for.cond13.preheader
                                        #   in Loop: Header=BB57_5 Depth=1
	movq	8(%r13,%rbx,8), %rax
	movslq	(%rax), %rcx
	testq	%rcx, %rcx
	movq	$1, %r14
	jle	.LBB57_9
	.align	16, 0x90
.LBB57_8:                               # %for.body19
                                        #   Parent Loop BB57_5 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	(%rax,%r14,8), %rdi
	callq	reset_used_flags
	movq	8(%r13,%rbx,8), %rax
	movslq	(%rax), %rdx
	movq	$32, %rcx
	movq	%r14, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	incq	%r14
	cmpq	%rdx, %rsi
	jl	.LBB57_8
	.align	16, 0x90
.LBB57_9:                               # %for.inc27
                                        #   in Loop: Header=BB57_5 Depth=1
	incq	%rbx
	movq	$32, %rcx
	movq	%rbx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	%r15, %rax
	jl	.LBB57_5
.LBB57_10:                              # %for.end29
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp483:
	.size	reset_used_flags, .Ltmp483-reset_used_flags
	.cfi_endproc

	.align	16, 0x90
	.type	reset_used_decls,@function
reset_used_decls:                       # @reset_used_decls
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp487:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp488:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp489:
	.cfi_def_cfa_offset 32
.Ltmp490:
	.cfi_offset %rbx, -24
.Ltmp491:
	.cfi_offset %r14, -16
	movq	%rdi, %r14
	movq	32(%r14), %rbx
	jmp	.LBB58_1
	.align	16, 0x90
.LBB58_4:                               # %for.inc
                                        #   in Loop: Header=BB58_1 Depth=1
	movq	(%rbx), %rbx
.LBB58_1:                               # %entry
                                        # =>This Inner Loop Header: Depth=1
	testq	%rbx, %rbx
	je	.LBB58_5
# BB#2:                                 # %for.body
                                        #   in Loop: Header=BB58_1 Depth=1
	movq	144(%rbx), %rdi
	testq	%rdi, %rdi
	je	.LBB58_4
# BB#3:                                 # %cond.end
                                        #   in Loop: Header=BB58_1 Depth=1
	callq	reset_used_flags
	jmp	.LBB58_4
.LBB58_5:                               # %for.end
	movq	40(%r14), %rbx
	jmp	.LBB58_7
	.align	16, 0x90
.LBB58_6:                               # %for.body11
                                        #   in Loop: Header=BB58_7 Depth=1
	movq	%rbx, %rdi
	callq	reset_used_decls
	movq	(%rbx), %rbx
.LBB58_7:                               # %for.body11
                                        # =>This Inner Loop Header: Depth=1
	testq	%rbx, %rbx
	jne	.LBB58_6
# BB#8:                                 # %for.end15
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp492:
	.size	reset_used_decls, .Ltmp492-reset_used_decls
	.cfi_endproc

	.globl	copy_most_rtx
	.align	16, 0x90
	.type	copy_most_rtx,@function
copy_most_rtx:                          # @copy_most_rtx
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp499:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp500:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp501:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp502:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp503:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp504:
	.cfi_def_cfa_offset 64
.Ltmp505:
	.cfi_offset %rbx, -48
.Ltmp506:
	.cfi_offset %r12, -40
.Ltmp507:
	.cfi_offset %r13, -32
.Ltmp508:
	.cfi_offset %r14, -24
.Ltmp509:
	.cfi_offset %r15, -16
	movq	%rsi, %rbx
	movq	%rdi, %rdx
	cmpq	%rbx, %rdx
	je	.LBB59_30
# BB#1:                                 # %lor.lhs.false
	movzbl	(%rbx), %eax
	cmpq	$3, %rax
	jne	.LBB59_3
# BB#2:                                 # %land.lhs.true
	movq	%rbx, %rdi
	movq	%rdx, %rsi
	movq	%rdx, %r14
	callq	in_expr_list_p
	movq	%r14, %rdx
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB59_30
.LBB59_3:                               # %if.end
	movq	%rbx, %r13
	movzbl	(%rdx), %edi
	leaq	-36(%rdi), %rcx
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %rcx
	cmpq	$35, %rcx
	ja	.LBB59_5
# BB#4:                                 # %if.end
	movq	$1, %rax
	shlq	%cl, %rax
	movabsq	$47288418305, %rcx      # imm = 0xB029C0001
	testq	%rcx, %rax
	jne	.LBB59_30
.LBB59_5:                               # %sw.epilog
	movq	%rdx, %rbx
	callq	rtx_alloc
	movq	%rax, %r8
	movl	(%rbx), %ecx
	movl	(%r8), %eax
	andq	$16711680, %rcx         # imm = 0xFF0000
	movabsq	$4278255615, %rdx       # imm = 0xFF00FFFF
	andq	%rax, %rdx
	orq	%rcx, %rdx
	movl	%edx, (%r8)
	movl	(%rbx), %ecx
	andq	$268435456, %rcx        # imm = 0x10000000
	movabsq	$4026531839, %rsi       # imm = 0xEFFFFFFF
	andq	%rdx, %rsi
	orq	%rcx, %rsi
	movl	%esi, (%r8)
	movl	(%rbx), %ecx
	andq	$134217728, %rcx        # imm = 0x8000000
	movabsq	$4160749567, %rdx       # imm = 0xF7FFFFFF
	andq	%rsi, %rdx
	orq	%rcx, %rdx
	movl	%edx, (%r8)
	movl	(%rbx), %ecx
	andq	$67108864, %rcx         # imm = 0x4000000
	movabsq	$4227858431, %rsi       # imm = 0xFBFFFFFF
	andq	%rdx, %rsi
	orq	%rcx, %rsi
	movl	%esi, (%r8)
	movl	(%rbx), %ecx
	andq	$1073741824, %rcx       # imm = 0x40000000
	movabsq	$3221225471, %rdx       # imm = 0xBFFFFFFF
	andq	%rsi, %rdx
	orq	%rcx, %rdx
	movl	%edx, (%r8)
	movl	(%rbx), %ecx
	movabsq	$2147483648, %rsi       # imm = 0x80000000
	andq	%rcx, %rsi
	andq	$65535, %rax            # imm = 0xFFFF
	movzbl	rtx_length(%rax), %ecx
	andq	$2147483647, %rdx       # imm = 0x7FFFFFFF
	orq	%rsi, %rdx
	movl	%edx, (%r8)
	testq	%rcx, %rcx
	je	.LBB59_29
# BB#6:                                 # %for.body.lr.ph
	movq	rtx_format(,%rax,8), %r15
	xorq	%r12, %r12
	jmp	.LBB59_7
.LBB59_24:                              # %for.body
                                        #   in Loop: Header=BB59_7 Depth=1
	addq	$-105, %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	cmpq	$14, %rax
	ja	.LBB59_12
# BB#25:                                # %for.body
                                        #   in Loop: Header=BB59_7 Depth=1
	jmpq	*.LJTI59_0(,%rax,8)
.LBB59_26:                              # %sw.bb153
                                        #   in Loop: Header=BB59_7 Depth=1
	movl	8(%rbx,%r12,8), %eax
	movl	%eax, 8(%r8,%r12,8)
	jmp	.LBB59_27
	.align	16, 0x90
.LBB59_7:                               # %for.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB59_18 Depth 2
	movsbq	(%r15), %rax
	incq	%r15
	cmpq	$68, %rax
	jg	.LBB59_10
# BB#8:                                 # %for.body
                                        #   in Loop: Header=BB59_7 Depth=1
	movq	%r15, 8(%rsp)           # 8-byte Spill
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	cmpq	$48, %rax
	jne	.LBB59_12
.LBB59_9:                               # %sw.bb88
                                        #   in Loop: Header=BB59_7 Depth=1
	movq	8(%rbx,%r12,8), %rax
	movq	%rax, 8(%r8,%r12,8)
.LBB59_27:                              # %for.inc188
                                        #   in Loop: Header=BB59_7 Depth=1
	movq	%r13, %r15
	jmp	.LBB59_28
	.align	16, 0x90
.LBB59_10:                              # %for.body
                                        #   in Loop: Header=BB59_7 Depth=1
	movq	%r15, 8(%rsp)           # 8-byte Spill
	cmpq	$82, %rax
	jg	.LBB59_13
# BB#11:                                # %for.body
                                        #   in Loop: Header=BB59_7 Depth=1
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	cmpq	$69, %rax
	je	.LBB59_16
	jmp	.LBB59_12
	.align	16, 0x90
.LBB59_13:                              # %for.body
                                        #   in Loop: Header=BB59_7 Depth=1
	cmpq	$100, %rax
	jg	.LBB59_19
# BB#14:                                # %for.body
                                        #   in Loop: Header=BB59_7 Depth=1
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	cmpq	$83, %rax
	je	.LBB59_9
# BB#15:                                # %for.body
                                        #   in Loop: Header=BB59_7 Depth=1
	cmpq	$86, %rax
	jne	.LBB59_12
	.align	16, 0x90
.LBB59_16:                              # %sw.bb97
                                        #   in Loop: Header=BB59_7 Depth=1
	movq	8(%rbx,%r12,8), %rax
	movq	%rax, 8(%r8,%r12,8)
	movq	8(%rbx,%r12,8), %rax
	testq	%rax, %rax
	movq	%r13, %r15
	je	.LBB59_28
# BB#17:                                # %if.then111
                                        #   in Loop: Header=BB59_7 Depth=1
	movl	(%rax), %edi
	movq	%r8, %r14
	callq	rtvec_alloc
	movq	%r14, %r8
	movq	%rax, 8(%r8,%r12,8)
	movslq	(%rax), %rax
	testq	%rax, %rax
	movq	$1, %r14
	jle	.LBB59_28
	.align	16, 0x90
.LBB59_18:                              # %for.body129
                                        #   Parent Loop BB59_7 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	8(%rbx,%r12,8), %rax
	movq	(%rax,%r14,8), %rdi
	movq	%r15, %rsi
	movq	%r8, %r13
	callq	copy_most_rtx
	movq	%r13, %r8
	movq	8(%r8,%r12,8), %rcx
	movq	%rax, (%rcx,%r14,8)
	movq	8(%r8,%r12,8), %rax
	movslq	(%rax), %rax
	movq	$32, %rcx
	movq	%r14, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	incq	%r14
	cmpq	%rax, %rdx
	jl	.LBB59_18
	jmp	.LBB59_28
.LBB59_19:                              # %for.body
                                        #   in Loop: Header=BB59_7 Depth=1
	cmpq	$104, %rax
	jg	.LBB59_24
# BB#20:                                # %for.body
                                        #   in Loop: Header=BB59_7 Depth=1
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	cmpq	$101, %rax
	jne	.LBB59_12
# BB#21:                                # %sw.bb57
                                        #   in Loop: Header=BB59_7 Depth=1
	movq	8(%rbx,%r12,8), %rax
	movq	%rax, 8(%r8,%r12,8)
	movq	8(%rbx,%r12,8), %rdi
	testq	%rdi, %rdi
	movq	%r13, %r15
	je	.LBB59_28
# BB#22:                                # %sw.bb57
                                        #   in Loop: Header=BB59_7 Depth=1
	cmpq	%r15, %rdi
	je	.LBB59_28
# BB#23:                                # %if.then77
                                        #   in Loop: Header=BB59_7 Depth=1
	movq	%r15, %rsi
	movq	%r8, %r14
	callq	copy_most_rtx
	movq	%r14, %r8
	movq	%rax, 8(%r8,%r12,8)
	.align	16, 0x90
.LBB59_28:                              # %for.inc188
                                        #   in Loop: Header=BB59_7 Depth=1
	movq	%r15, %r13
	movzbl	(%r8), %eax
	incq	%r12
	movzbl	rtx_length(%rax), %eax
	movq	$32, %rcx
	movq	%r12, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rax, %rdx
	movq	8(%rsp), %r15           # 8-byte Reload
	jl	.LBB59_7
.LBB59_29:
	movq	%r8, %rdx
.LBB59_30:                              # %return
	movq	%rdx, %rax
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB59_12:                              # %sw.default186
	movabsq	$.L.str, %rdi
	movabsq	$.L__FUNCTION__.copy_most_rtx, %rdx
	movq	$2498, %rsi             # imm = 0x9C2
	callq	fancy_abort
.Ltmp510:
	.size	copy_most_rtx, .Ltmp510-copy_most_rtx
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI59_0:
	.quad	.LBB59_26
	.quad	.LBB59_12
	.quad	.LBB59_12
	.quad	.LBB59_12
	.quad	.LBB59_12
	.quad	.LBB59_26
	.quad	.LBB59_12
	.quad	.LBB59_12
	.quad	.LBB59_12
	.quad	.LBB59_12
	.quad	.LBB59_9
	.quad	.LBB59_9
	.quad	.LBB59_9
	.quad	.LBB59_12
	.quad	.LBB59_9

	.text
	.globl	make_safe_from
	.align	16, 0x90
	.type	make_safe_from,@function
make_safe_from:                         # @make_safe_from
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp515:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp516:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp517:
	.cfi_def_cfa_offset 32
.Ltmp518:
	.cfi_offset %rbx, -32
.Ltmp519:
	.cfi_offset %r14, -24
.Ltmp520:
	.cfi_offset %r15, -16
	movq	%rdi, %r15
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	movabsq	$1729382256910270476, %rax # imm = 0x180000000000000C
	jmp	.LBB60_1
	.align	16, 0x90
.LBB60_14:                              # %while.body.backedge
                                        #   in Loop: Header=BB60_1 Depth=1
	movq	8(%rsi), %rsi
.LBB60_1:                               # %while.body
                                        # =>This Inner Loop Header: Depth=1
	movzbl	(%rsi), %ecx
	addq	$-61, %rcx
	andq	%rbx, %rcx
	cmpq	$60, %rcx
	ja	.LBB60_13
# BB#2:                                 # %while.body
                                        #   in Loop: Header=BB60_1 Depth=1
	movq	$1, %rdx
	shlq	%cl, %rdx
	testq	%rax, %rdx
	jne	.LBB60_14
# BB#3:                                 # %while.body
	testq	%rcx, %rcx
	jne	.LBB60_4
# BB#10:                                # %land.lhs.true49
	movl	8(%rsi), %eax
	cmpq	$53, %rax
	jb	.LBB60_12
# BB#11:                                # %lor.lhs.false53
	movq	%rsi, %rdi
	movq	%r15, %rsi
	callq	reg_mentioned_p
	testq	%rbx, %rax
	jne	.LBB60_12
	jmp	.LBB60_13
.LBB60_4:                               # %while.body
	cmpq	$5, %rcx
	jne	.LBB60_13
# BB#5:                                 # %land.lhs.true
	movzbl	(%r15), %ecx
	cmpq	$133, %rcx
	jg	.LBB60_8
# BB#6:                                 # %land.lhs.true
	addq	$-54, %rcx
	andq	%rbx, %rcx
	cmpq	$14, %rcx
	ja	.LBB60_12
# BB#7:                                 # %land.lhs.true
	movq	$1, %rax
	shlq	%cl, %rax
	testq	$25239, %rax            # imm = 0x6297
	jne	.LBB60_13
	jmp	.LBB60_12
.LBB60_8:                               # %land.lhs.true
	cmpq	$134, %rcx
	je	.LBB60_13
# BB#9:                                 # %land.lhs.true
	cmpq	$140, %rcx
	je	.LBB60_13
.LBB60_12:                              # %if.then
	movzbl	2(%r15), %edi
	callq	gen_reg_rtx
	movq	%rax, %r14
	movq	%r14, %rdi
	movq	%r15, %rsi
	callq	emit_move_insn
	movq	%r14, %r15
.LBB60_13:                              # %return
	movq	%r15, %rax
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp521:
	.size	make_safe_from, .Ltmp521-make_safe_from
	.cfi_endproc

	.globl	get_insns
	.align	16, 0x90
	.type	get_insns,@function
get_insns:                              # @get_insns
	.cfi_startproc
# BB#0:                                 # %entry
	movq	cfun(%rip), %rax
	movq	24(%rax), %rax
	movq	8(%rax), %rax
	retq
.Ltmp522:
	.size	get_insns, .Ltmp522-get_insns
	.cfi_endproc

	.globl	get_last_insn
	.align	16, 0x90
	.type	get_last_insn,@function
get_last_insn:                          # @get_last_insn
	.cfi_startproc
# BB#0:                                 # %entry
	movq	cfun(%rip), %rax
	movq	24(%rax), %rax
	movq	16(%rax), %rax
	retq
.Ltmp523:
	.size	get_last_insn, .Ltmp523-get_last_insn
	.cfi_endproc

	.globl	set_last_insn
	.align	16, 0x90
	.type	set_last_insn,@function
set_last_insn:                          # @set_last_insn
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rax
.Ltmp525:
	.cfi_def_cfa_offset 16
	cmpq	$0, 24(%rdi)
	jne	.LBB63_2
# BB#1:                                 # %if.end
	movq	cfun(%rip), %rax
	movq	24(%rax), %rax
	movq	%rdi, 16(%rax)
	popq	%rax
	retq
.LBB63_2:                               # %if.then
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.set_last_insn, %rcx
	movq	$2749, %rsi             # imm = 0xABD
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp526:
	.size	set_last_insn, .Ltmp526-set_last_insn
	.cfi_endproc

	.globl	get_last_insn_anywhere
	.align	16, 0x90
	.type	get_last_insn_anywhere,@function
get_last_insn_anywhere:                 # @get_last_insn_anywhere
	.cfi_startproc
# BB#0:                                 # %entry
	movq	cfun(%rip), %rax
	movq	24(%rax), %rcx
	movq	16(%rcx), %rax
	testq	%rax, %rax
	jne	.LBB64_6
# BB#1:                                 # %if.end
	movq	32(%rcx), %rcx
	testq	%rcx, %rcx
	je	.LBB64_2
	.align	16, 0x90
.LBB64_5:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	8(%rcx), %rax
	testq	%rax, %rax
	jne	.LBB64_6
# BB#3:                                 # %for.cond
                                        #   in Loop: Header=BB64_5 Depth=1
	movq	24(%rcx), %rcx
	testq	%rcx, %rcx
	jne	.LBB64_5
# BB#4:
	xorq	%rax, %rax
	retq
.LBB64_6:                               # %return
	retq
.LBB64_2:
	xorq	%rax, %rax
	retq
.Ltmp527:
	.size	get_last_insn_anywhere, .Ltmp527-get_last_insn_anywhere
	.cfi_endproc

	.globl	get_max_uid
	.align	16, 0x90
	.type	get_max_uid,@function
get_max_uid:                            # @get_max_uid
	.cfi_startproc
# BB#0:                                 # %entry
	movq	cfun(%rip), %rax
	movq	24(%rax), %rax
	movl	40(%rax), %eax
	retq
.Ltmp528:
	.size	get_max_uid, .Ltmp528-get_max_uid
	.cfi_endproc

	.globl	renumber_insns
	.align	16, 0x90
	.type	renumber_insns,@function
renumber_insns:                         # @renumber_insns
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp533:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp534:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp535:
	.cfi_def_cfa_offset 32
.Ltmp536:
	.cfi_offset %rbx, -32
.Ltmp537:
	.cfi_offset %r14, -24
.Ltmp538:
	.cfi_offset %r15, -16
	movl	flag_renumber_insns(%rip), %eax
	movq	%rdi, %r15
	testq	%rax, %rax
	je	.LBB66_8
# BB#1:                                 # %entry
	cmpq	$1, %rax
	jne	.LBB66_2
# BB#3:                                 # %land.lhs.true
	movq	cfun(%rip), %rax
	movq	24(%rax), %rax
	movslq	40(%rax), %rcx
	cmpq	$25000, %rcx            # imm = 0x61A8
	jl	.LBB66_8
	jmp	.LBB66_4
.LBB66_2:                               # %entry.if.end3_crit_edge
	movq	cfun(%rip), %rax
	movq	24(%rax), %rax
.LBB66_4:                               # %if.end3
	movq	$1, %rcx
	movl	%ecx, 40(%rax)
	movq	8(%rax), %rbx
	testq	%rbx, %rbx
	je	.LBB66_8
# BB#5:                                 # %for.body.lr.ph
	testq	%r15, %r15
	je	.LBB66_9
# BB#6:
	movabsq	$.L.str2, %r14
	.align	16, 0x90
.LBB66_7:                               # %if.end11
                                        # =>This Inner Loop Header: Depth=1
	movl	40(%rax), %ecx
	movl	8(%rbx), %edx
	xorq	%rax, %rax
	movq	%r15, %rdi
	movq	%r14, %rsi
	callq	fprintf
	movq	cfun(%rip), %rax
	movq	24(%rax), %rax
	movl	40(%rax), %ecx
	leaq	1(%rcx), %rdx
	movl	%edx, 40(%rax)
	movl	%ecx, 8(%rbx)
	movq	24(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB66_7
	jmp	.LBB66_8
	.align	16, 0x90
.LBB66_9:                               # %if.end11.us
                                        # =>This Inner Loop Header: Depth=1
	movl	40(%rax), %ecx
	leaq	1(%rcx), %rdx
	movl	%edx, 40(%rax)
	movl	%ecx, 8(%rbx)
	movq	24(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB66_9
.LBB66_8:                               # %for.end
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp539:
	.size	renumber_insns, .Ltmp539-renumber_insns
	.cfi_endproc

	.globl	next_insn
	.align	16, 0x90
	.type	next_insn,@function
next_insn:                              # @next_insn
	.cfi_startproc
# BB#0:                                 # %entry
	xorq	%rax, %rax
	testq	%rdi, %rdi
	je	.LBB67_6
# BB#1:                                 # %if.then
	movq	24(%rdi), %rcx
	testq	%rcx, %rcx
	je	.LBB67_6
# BB#2:                                 # %land.lhs.true
	movzbl	(%rcx), %eax
	cmpq	$32, %rax
	jne	.LBB67_3
# BB#4:                                 # %land.lhs.true2
	movq	32(%rcx), %rdx
	movzbl	(%rdx), %eax
	cmpq	$24, %rax
	movq	%rcx, %rax
	jne	.LBB67_6
# BB#5:                                 # %if.then9
	movq	8(%rdx), %rax
	movq	8(%rax), %rax
.LBB67_6:                               # %if.end16
	retq
.LBB67_3:
	movq	%rcx, %rax
	retq
.Ltmp540:
	.size	next_insn, .Ltmp540-next_insn
	.cfi_endproc

	.globl	previous_insn
	.align	16, 0x90
	.type	previous_insn,@function
previous_insn:                          # @previous_insn
	.cfi_startproc
# BB#0:                                 # %entry
	xorq	%rax, %rax
	testq	%rdi, %rdi
	je	.LBB68_6
# BB#1:                                 # %if.then
	movq	16(%rdi), %rcx
	testq	%rcx, %rcx
	je	.LBB68_6
# BB#2:                                 # %land.lhs.true
	movzbl	(%rcx), %eax
	cmpq	$32, %rax
	jne	.LBB68_3
# BB#4:                                 # %land.lhs.true2
	movq	32(%rcx), %rdx
	movzbl	(%rdx), %eax
	cmpq	$24, %rax
	movq	%rcx, %rax
	jne	.LBB68_6
# BB#5:                                 # %if.then9
	movq	8(%rdx), %rax
	movl	(%rax), %edx
	decq	%rdx
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	8(%rax,%rdx,8), %rax
.LBB68_6:                               # %if.end22
	retq
.LBB68_3:
	movq	%rcx, %rax
	retq
.Ltmp541:
	.size	previous_insn, .Ltmp541-previous_insn
	.cfi_endproc

	.globl	next_nonnote_insn
	.align	16, 0x90
	.type	next_nonnote_insn,@function
next_nonnote_insn:                      # @next_nonnote_insn
	.cfi_startproc
# BB#0:                                 # %entry
	testq	%rdi, %rdi
	je	.LBB69_1
	.align	16, 0x90
.LBB69_2:                               # %while.body
                                        # =>This Inner Loop Header: Depth=1
	movq	24(%rdi), %rdi
	testq	%rdi, %rdi
	je	.LBB69_3
# BB#4:                                 # %lor.lhs.false
                                        #   in Loop: Header=BB69_2 Depth=1
	movzbl	(%rdi), %eax
	cmpq	$37, %rax
	je	.LBB69_2
# BB#5:                                 # %while.end
	movq	%rdi, %rax
	retq
.LBB69_1:
	xorq	%rdi, %rdi
	movq	%rdi, %rax
	retq
.LBB69_3:
	xorq	%rdi, %rdi
	movq	%rdi, %rax
	retq
.Ltmp542:
	.size	next_nonnote_insn, .Ltmp542-next_nonnote_insn
	.cfi_endproc

	.globl	prev_nonnote_insn
	.align	16, 0x90
	.type	prev_nonnote_insn,@function
prev_nonnote_insn:                      # @prev_nonnote_insn
	.cfi_startproc
# BB#0:                                 # %entry
	testq	%rdi, %rdi
	je	.LBB70_1
	.align	16, 0x90
.LBB70_2:                               # %while.body
                                        # =>This Inner Loop Header: Depth=1
	movq	16(%rdi), %rdi
	testq	%rdi, %rdi
	je	.LBB70_3
# BB#4:                                 # %lor.lhs.false
                                        #   in Loop: Header=BB70_2 Depth=1
	movzbl	(%rdi), %eax
	cmpq	$37, %rax
	je	.LBB70_2
# BB#5:                                 # %while.end
	movq	%rdi, %rax
	retq
.LBB70_1:
	xorq	%rdi, %rdi
	movq	%rdi, %rax
	retq
.LBB70_3:
	xorq	%rdi, %rdi
	movq	%rdi, %rax
	retq
.Ltmp543:
	.size	prev_nonnote_insn, .Ltmp543-prev_nonnote_insn
	.cfi_endproc

	.globl	next_real_insn
	.align	16, 0x90
	.type	next_real_insn,@function
next_real_insn:                         # @next_real_insn
	.cfi_startproc
# BB#0:                                 # %entry
	testq	%rdi, %rdi
	je	.LBB71_1
# BB#2:
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB71_3:                               # %while.body
                                        # =>This Inner Loop Header: Depth=1
	movq	24(%rdi), %rdi
	testq	%rdi, %rdi
	je	.LBB71_4
# BB#5:                                 # %lor.lhs.false
                                        #   in Loop: Header=BB71_3 Depth=1
	movzbl	(%rdi), %ecx
	addq	$-32, %rcx
	andq	%rax, %rcx
	cmpq	$2, %rcx
	ja	.LBB71_3
# BB#6:                                 # %while.end
	movq	%rdi, %rax
	retq
.LBB71_1:
	xorq	%rdi, %rdi
	movq	%rdi, %rax
	retq
.LBB71_4:
	xorq	%rdi, %rdi
	movq	%rdi, %rax
	retq
.Ltmp544:
	.size	next_real_insn, .Ltmp544-next_real_insn
	.cfi_endproc

	.globl	prev_real_insn
	.align	16, 0x90
	.type	prev_real_insn,@function
prev_real_insn:                         # @prev_real_insn
	.cfi_startproc
# BB#0:                                 # %entry
	testq	%rdi, %rdi
	je	.LBB72_1
# BB#2:
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB72_3:                               # %while.body
                                        # =>This Inner Loop Header: Depth=1
	movq	16(%rdi), %rdi
	testq	%rdi, %rdi
	je	.LBB72_4
# BB#5:                                 # %lor.lhs.false
                                        #   in Loop: Header=BB72_3 Depth=1
	movzbl	(%rdi), %ecx
	addq	$-32, %rcx
	andq	%rax, %rcx
	cmpq	$2, %rcx
	ja	.LBB72_3
# BB#6:                                 # %while.end
	movq	%rdi, %rax
	retq
.LBB72_1:
	xorq	%rdi, %rdi
	movq	%rdi, %rax
	retq
.LBB72_4:
	xorq	%rdi, %rdi
	movq	%rdi, %rax
	retq
.Ltmp545:
	.size	prev_real_insn, .Ltmp545-prev_real_insn
	.cfi_endproc

	.globl	active_insn_p
	.align	16, 0x90
	.type	active_insn_p,@function
active_insn_p:                          # @active_insn_p
	.cfi_startproc
# BB#0:                                 # %entry
	movzbl	(%rdi), %ecx
	leaq	-33(%rcx), %rax
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	andq	%rax, %rdx
	cmpq	$2, %rdx
	movq	$1, %rax
	jb	.LBB73_6
# BB#1:                                 # %lor.rhs
	xorq	%rax, %rax
	cmpq	$32, %rcx
	jne	.LBB73_6
# BB#2:                                 # %land.rhs
	movl	reload_completed(%rip), %eax
	testq	%rax, %rax
	movq	$1, %rax
	je	.LBB73_6
# BB#3:                                 # %lor.rhs7
	movq	32(%rdi), %rax
	movl	(%rax), %eax
	andq	$65534, %rax            # imm = 0xFFFE
	cmpq	$48, %rax
	je	.LBB73_4
# BB#5:                                 # %lor.rhs7
	movabsq	$1, %rax
.LBB73_6:                               # %lor.end19
	retq
.LBB73_4:
	movabsq	$0, %rax
	retq
.Ltmp546:
	.size	active_insn_p, .Ltmp546-active_insn_p
	.cfi_endproc

	.globl	next_active_insn
	.align	16, 0x90
	.type	next_active_insn,@function
next_active_insn:                       # @next_active_insn
	.cfi_startproc
# BB#0:                                 # %entry
	xorq	%rax, %rax
	testq	%rdi, %rdi
	je	.LBB74_14
# BB#1:                                 # %while.body.preheader
	movq	24(%rdi), %rcx
	testq	%rcx, %rcx
	je	.LBB74_14
# BB#2:                                 # %lor.lhs.false.lr.ph
	movl	reload_completed(%rip), %edx
	testq	%rdx, %rdx
	je	.LBB74_3
# BB#7:
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB74_8:                               # %lor.lhs.false
                                        # =>This Inner Loop Header: Depth=1
	movzbl	(%rcx), %esi
	leaq	-33(%rsi), %rdi
	andq	%rdx, %rdi
	cmpq	$2, %rdi
	jb	.LBB74_9
# BB#10:                                # %lor.rhs.i
                                        #   in Loop: Header=BB74_8 Depth=1
	cmpq	$32, %rsi
	jne	.LBB74_13
# BB#11:                                # %lor.rhs7.i
                                        #   in Loop: Header=BB74_8 Depth=1
	movq	32(%rcx), %rsi
	movl	(%rsi), %esi
	andq	$65534, %rsi            # imm = 0xFFFE
	cmpq	$48, %rsi
	jne	.LBB74_12
.LBB74_13:                              # %while.body.backedge
                                        #   in Loop: Header=BB74_8 Depth=1
	movq	24(%rcx), %rcx
	testq	%rcx, %rcx
	jne	.LBB74_8
	jmp	.LBB74_14
.LBB74_3:
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB74_4:                               # %lor.lhs.false.us
                                        # =>This Inner Loop Header: Depth=1
	movzbl	(%rcx), %esi
	addq	$-32, %rsi
	andq	%rdx, %rsi
	cmpq	$3, %rsi
	jb	.LBB74_5
# BB#6:                                 # %while.body.backedge.us
                                        #   in Loop: Header=BB74_4 Depth=1
	movq	24(%rcx), %rcx
	testq	%rcx, %rcx
	jne	.LBB74_4
.LBB74_14:                              # %while.end
	retq
.LBB74_9:
	movq	%rcx, %rax
	retq
.LBB74_5:
	movq	%rcx, %rax
	retq
.LBB74_12:
	movq	%rcx, %rax
	retq
.Ltmp547:
	.size	next_active_insn, .Ltmp547-next_active_insn
	.cfi_endproc

	.globl	prev_active_insn
	.align	16, 0x90
	.type	prev_active_insn,@function
prev_active_insn:                       # @prev_active_insn
	.cfi_startproc
# BB#0:                                 # %entry
	xorq	%rax, %rax
	testq	%rdi, %rdi
	je	.LBB75_11
# BB#1:                                 # %while.body.preheader
	movq	16(%rdi), %rcx
	testq	%rcx, %rcx
	je	.LBB75_11
# BB#2:                                 # %lor.lhs.false.lr.ph
	movl	reload_completed(%rip), %r8d
	movabsq	$4294967295, %rsi       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB75_3:                               # %lor.lhs.false
                                        # =>This Inner Loop Header: Depth=1
	movzbl	(%rcx), %edi
	leaq	-33(%rdi), %rdx
	andq	%rsi, %rdx
	cmpq	$2, %rdx
	jb	.LBB75_4
# BB#5:                                 # %lor.rhs.i
                                        #   in Loop: Header=BB75_3 Depth=1
	cmpq	$32, %rdi
	jne	.LBB75_10
# BB#6:                                 # %land.rhs.i
                                        #   in Loop: Header=BB75_3 Depth=1
	testq	%rsi, %r8
	je	.LBB75_7
# BB#8:                                 # %lor.rhs7.i
                                        #   in Loop: Header=BB75_3 Depth=1
	movq	32(%rcx), %rdx
	movl	(%rdx), %edx
	andq	$65534, %rdx            # imm = 0xFFFE
	cmpq	$48, %rdx
	jne	.LBB75_9
.LBB75_10:                              # %while.body.backedge
                                        #   in Loop: Header=BB75_3 Depth=1
	movq	16(%rcx), %rcx
	testq	%rcx, %rcx
	jne	.LBB75_3
.LBB75_11:                              # %while.end
	retq
.LBB75_4:
	movq	%rcx, %rax
	retq
.LBB75_7:
	movq	%rcx, %rax
	retq
.LBB75_9:
	movq	%rcx, %rax
	retq
.Ltmp548:
	.size	prev_active_insn, .Ltmp548-prev_active_insn
	.cfi_endproc

	.globl	next_label
	.align	16, 0x90
	.type	next_label,@function
next_label:                             # @next_label
	.cfi_startproc
# BB#0:                                 # %entry
	testq	%rdi, %rdi
	je	.LBB76_1
	.align	16, 0x90
.LBB76_2:                               # %while.body
                                        # =>This Inner Loop Header: Depth=1
	movq	24(%rdi), %rdi
	testq	%rdi, %rdi
	je	.LBB76_3
# BB#4:                                 # %lor.lhs.false
                                        #   in Loop: Header=BB76_2 Depth=1
	movzbl	(%rdi), %eax
	cmpq	$36, %rax
	jne	.LBB76_2
# BB#5:                                 # %while.end
	movq	%rdi, %rax
	retq
.LBB76_1:
	xorq	%rdi, %rdi
	movq	%rdi, %rax
	retq
.LBB76_3:
	xorq	%rdi, %rdi
	movq	%rdi, %rax
	retq
.Ltmp549:
	.size	next_label, .Ltmp549-next_label
	.cfi_endproc

	.globl	prev_label
	.align	16, 0x90
	.type	prev_label,@function
prev_label:                             # @prev_label
	.cfi_startproc
# BB#0:                                 # %entry
	testq	%rdi, %rdi
	je	.LBB77_1
	.align	16, 0x90
.LBB77_2:                               # %while.body
                                        # =>This Inner Loop Header: Depth=1
	movq	16(%rdi), %rdi
	testq	%rdi, %rdi
	je	.LBB77_3
# BB#4:                                 # %lor.lhs.false
                                        #   in Loop: Header=BB77_2 Depth=1
	movzbl	(%rdi), %eax
	cmpq	$36, %rax
	jne	.LBB77_2
# BB#5:                                 # %while.end
	movq	%rdi, %rax
	retq
.LBB77_1:
	xorq	%rdi, %rdi
	movq	%rdi, %rax
	retq
.LBB77_3:
	xorq	%rdi, %rdi
	movq	%rdi, %rax
	retq
.Ltmp550:
	.size	prev_label, .Ltmp550-prev_label
	.cfi_endproc

	.globl	try_split
	.align	16, 0x90
	.type	try_split,@function
try_split:                              # @try_split
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp557:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp558:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp559:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp560:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp561:
	.cfi_def_cfa_offset 48
	subq	$80, %rsp
.Ltmp562:
	.cfi_def_cfa_offset 128
.Ltmp563:
	.cfi_offset %rbx, -48
.Ltmp564:
	.cfi_offset %r12, -40
.Ltmp565:
	.cfi_offset %r13, -32
.Ltmp566:
	.cfi_offset %r14, -24
.Ltmp567:
	.cfi_offset %r15, -16
	movq	%rsi, %rbx
	movq	16(%rbx), %rax
	movq	%rax, 40(%rsp)          # 8-byte Spill
	movq	24(%rbx), %r12
	movq	%rdx, %r14
	movq	%rdi, %r13
	movq	%rbx, %rdi
	callq	any_condjump_p
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	testq	%r15, %rax
	je	.LBB78_3
# BB#1:                                 # %land.lhs.true
	movq	$16, %rsi
	xorq	%rdx, %rdx
	movq	%rbx, %rdi
	callq	find_reg_note
	testq	%rax, %rax
	je	.LBB78_3
# BB#2:                                 # %if.then
	movq	%r14, 48(%rsp)          # 8-byte Spill
	movq	8(%rax), %rax
	movq	8(%rax), %rax
	movq	%rax, 56(%rsp)          # 8-byte Spill
	movl	%eax, split_branch_probability(%rip)
	jmp	.LBB78_4
.LBB78_3:                               # %if.endthread-pre-split
	movq	%r14, 48(%rsp)          # 8-byte Spill
	movl	split_branch_probability(%rip), %eax
	movq	%rax, 56(%rsp)          # 8-byte Spill
.LBB78_4:                               # %if.end
	movq	%r13, %rdi
	movq	%rbx, %rsi
	movq	%rbx, 64(%rsp)          # 8-byte Spill
	callq	split_insns
	movq	%rax, %r14
	movq	%r15, %rbx
	movl	%ebx, split_branch_probability(%rip)
	xorq	%rcx, %rcx
	testq	%r12, %r12
	movabsq	$0, %r15
	je	.LBB78_8
# BB#5:                                 # %land.lhs.true13
	movzbl	(%r12), %eax
	xorq	%rcx, %rcx
	cmpq	$35, %rax
	jne	.LBB78_6
# BB#7:                                 # %if.then15
	movq	24(%r12), %r15
	movq	$1, %rcx
	jmp	.LBB78_8
.LBB78_6:
	movq	%r12, %r15
.LBB78_8:                               # %if.end19
	testq	%r14, %r14
	je	.LBB78_9
# BB#10:                                # %if.then21
	movq	%rcx, 32(%rsp)          # 8-byte Spill
	movzbl	(%r14), %eax
	cmpq	$24, %rax
	jne	.LBB78_78
# BB#11:                                # %for.cond.preheader
	movq	%r15, 16(%rsp)          # 8-byte Spill
	movq	%rbx, %r15
	movq	8(%r14), %rax
	movslq	(%rax), %r12
	testq	%r12, %r12
	jle	.LBB78_18
# BB#12:
	movq	$1, %rbx
	.align	16, 0x90
.LBB78_13:                              # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	(%rax,%rbx,8), %rcx
	movzbl	(%rcx), %edx
	cmpq	$32, %rdx
	jne	.LBB78_17
# BB#14:                                # %land.lhs.true39
                                        #   in Loop: Header=BB78_13 Depth=1
	movq	32(%rcx), %rdi
	movq	%r13, %rsi
	callq	rtx_equal_p
	testq	%r15, %rax
	jne	.LBB78_15
# BB#16:                                # %land.lhs.true39.for.inc_crit_edge
                                        #   in Loop: Header=BB78_13 Depth=1
	movq	8(%r14), %rax
.LBB78_17:                              # %for.inc
                                        #   in Loop: Header=BB78_13 Depth=1
	movslq	(%rax), %r12
	movq	$32, %rcx
	movq	%rbx, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	incq	%rbx
	cmpq	%r12, %rdx
	jl	.LBB78_13
.LBB78_18:                              # %for.cond57.preheader
	movq	$32, %rcx
	movq	56(%rsp), %rax          # 8-byte Reload
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	%rax, 8(%rsp)           # 8-byte Spill
	leaq	64(%rax), %rax
	movq	%rax, (%rsp)            # 8-byte Spill
	xorq	%rax, %rax
	jmp	.LBB78_24
.LBB78_9:
	movq	64(%rsp), %rax          # 8-byte Reload
	jmp	.LBB78_87
.LBB78_78:                              # %if.else
	movq	%r14, %rdi
	movq	%r13, %rsi
	callq	rtx_equal_p
	testq	%rbx, %rax
	je	.LBB78_80
# BB#79:
	movq	64(%rsp), %rax          # 8-byte Reload
	jmp	.LBB78_87
.LBB78_80:                              # %if.else367
	movq	64(%rsp), %rsi          # 8-byte Reload
	movq	%r14, 32(%rsi)
	movl	%ebx, 40(%rsi)
	movq	%rbx, %r13
	movq	%r14, %rdi
	movq	48(%rsp), %r14          # 8-byte Reload
	movq	%r14, %rdx
	callq	try_split
	movq	40(%rsp), %rbx          # 8-byte Reload
	jmp	.LBB78_81
.LBB78_15:
	movq	64(%rsp), %rax          # 8-byte Reload
	jmp	.LBB78_87
.LBB78_30:                              # %gen_rtx_CONST_INT.exit
                                        #   in Loop: Header=BB78_24 Depth=1
	movq	56(%r13), %rcx
	movq	$3, %rdi
	movq	$16, %rsi
	callq	gen_rtx_fmt_ee
	movq	%rax, 56(%r13)
	movq	%rbx, %rax
	jmp	.LBB78_24
	.align	16, 0x90
.LBB78_19:                              # %for.cond57
                                        #   Parent Loop BB78_24 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	$32, %rcx
	movq	%r12, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jle	.LBB78_31
# BB#20:                                # %for.body60
                                        #   in Loop: Header=BB78_19 Depth=2
	movq	8(%r14), %rax
	movq	(%rax,%r12,8), %r13
	movzbl	(%r13), %eax
	decq	%r12
	cmpq	$33, %rax
	jne	.LBB78_19
# BB#21:                                # %if.then71
                                        #   in Loop: Header=BB78_24 Depth=1
	movq	32(%r13), %rdi
	movq	56(%rsp), %rbx          # 8-byte Reload
	andq	%r15, %rbx
	xorq	%rdx, %rdx
	movq	%r13, %rsi
	callq	mark_jump_label
	movq	24(%rsp), %rax          # 8-byte Reload
	leaq	1(%rax), %rax
	cmpq	%r15, %rbx
	je	.LBB78_24
# BB#22:                                # %land.lhs.true90
                                        #   in Loop: Header=BB78_24 Depth=1
	movq	%r13, %rdi
	movq	%rax, %rbx
	callq	any_condjump_p
	testq	%r15, %rax
	movq	%rbx, %rax
	je	.LBB78_24
# BB#23:                                # %land.lhs.true93
                                        #   in Loop: Header=BB78_24 Depth=1
	movq	$16, %rsi
	xorq	%rdx, %rdx
	movq	%r13, %rdi
	movq	%rax, %rbx
	callq	find_reg_note
	testq	%rax, %rax
	movq	%rbx, %rax
	jne	.LBB78_24
# BB#25:                                # %if.then96
                                        #   in Loop: Header=BB78_24 Depth=1
	movq	%rax, %rbx
	movq	24(%rsp), %rax          # 8-byte Reload
	testq	%r15, %rax
	movq	8(%rsp), %rax           # 8-byte Reload
	jne	.LBB78_88
# BB#26:                                # %if.end100
                                        #   in Loop: Header=BB78_24 Depth=1
	cmpq	$128, (%rsp)            # 8-byte Folded Reload
	movq	%rax, 72(%rsp)
	ja	.LBB78_28
# BB#27:                                # %if.then.i
                                        #   in Loop: Header=BB78_24 Depth=1
	movq	const_int_rtx+512(,%rax,8), %rdx
	jmp	.LBB78_30
	.align	16, 0x90
.LBB78_24:                              # %for.cond57.outer
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB78_19 Depth 2
	movq	%rax, 24(%rsp)          # 8-byte Spill
	movq	$32, %rcx
	shlq	%cl, %r12
	movq	$32, %rcx
	sarq	%cl, %r12
	jmp	.LBB78_19
.LBB78_28:                              # %if.end.i
                                        #   in Loop: Header=BB78_24 Depth=1
	movq	const_int_htab(%rip), %rdi
	movq	$1, %rcx
	leaq	72(%rsp), %rsi
	movq	56(%rsp), %rdx          # 8-byte Reload
	callq	htab_find_slot_with_hash
	movq	(%rax), %rdx
	testq	%rdx, %rdx
	jne	.LBB78_30
# BB#29:                                # %if.then4.i
                                        #   in Loop: Header=BB78_24 Depth=1
	movq	72(%rsp), %rdx
	movq	$54, %rdi
	xorq	%rsi, %rsi
	movq	%rax, 24(%rsp)          # 8-byte Spill
	callq	gen_rtx_fmt_w
	movq	%rax, %rdx
	movq	24(%rsp), %rax          # 8-byte Reload
	movq	%rdx, (%rax)
	jmp	.LBB78_30
.LBB78_31:                              # %for.end113
	movq	64(%rsp), %rax          # 8-byte Reload
	movzbl	(%rax), %eax
	cmpq	$34, %rax
	jne	.LBB78_37
# BB#32:                                # %if.then118
	movq	8(%r14), %rcx
	movslq	(%rcx), %rax
	testq	%rax, %rax
	jg	.LBB78_33
	jmp	.LBB78_37
	.align	16, 0x90
.LBB78_35:                              # %for.cond124.backedge.for.body127_crit_edge
                                        #   in Loop: Header=BB78_33 Depth=1
	movq	8(%r14), %rcx
.LBB78_33:                              # %for.body127.lr.ph
                                        # =>This Inner Loop Header: Depth=1
	decq	%rax
	movq	8(%rcx,%rax,8), %rcx
	movzbl	(%rcx), %edx
	cmpq	$34, %rdx
	jne	.LBB78_34
# BB#36:                                # %if.then138
                                        #   in Loop: Header=BB78_33 Depth=1
	movq	64(%rsp), %rdx          # 8-byte Reload
	movq	64(%rdx), %rdx
	movq	%rdx, 64(%rcx)
.LBB78_34:                              # %for.cond124.backedge
                                        #   in Loop: Header=BB78_33 Depth=1
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	testq	%rdx, %rdx
	jg	.LBB78_35
.LBB78_37:                              # %if.end155
	movq	64(%rsp), %rax          # 8-byte Reload
	movq	56(%rax), %r12
	jmp	.LBB78_38
	.align	16, 0x90
.LBB78_58:                              # %for.inc284
                                        #   in Loop: Header=BB78_38 Depth=1
	movq	16(%r12), %r12
.LBB78_38:                              # %if.end155
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB78_54 Depth 2
                                        #     Child Loop BB78_49 Depth 2
                                        #     Child Loop BB78_42 Depth 2
	testq	%r12, %r12
	je	.LBB78_59
# BB#39:                                # %for.body161
                                        #   in Loop: Header=BB78_38 Depth=1
	movzbl	2(%r12), %eax
	addq	$-23, %rax
	andq	%r15, %rax
	cmpq	$6, %rax
	ja	.LBB78_58
# BB#40:                                # %for.body161
                                        #   in Loop: Header=BB78_38 Depth=1
	jmpq	*.LJTI78_0(,%rax,8)
.LBB78_48:                              # %sw.bb206
                                        #   in Loop: Header=BB78_38 Depth=1
	movq	8(%r14), %rax
	movslq	(%rax), %r13
	testq	%r13, %r13
	jle	.LBB78_58
	jmp	.LBB78_49
	.align	16, 0x90
.LBB78_51:                              # %for.cond212.backedge.for.body215_crit_edge
                                        #   in Loop: Header=BB78_49 Depth=2
	movq	8(%r14), %rax
.LBB78_49:                              # %for.body215.lr.ph
                                        #   Parent Loop BB78_38 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	decq	%r13
	movq	8(%rax,%r13,8), %rbx
	movzbl	(%rbx), %eax
	cmpq	$34, %rax
	jne	.LBB78_50
# BB#52:                                # %if.then227
                                        #   in Loop: Header=BB78_49 Depth=2
	movq	8(%r12), %rdx
	movq	56(%rbx), %rcx
	movzbl	2(%r12), %esi
	movq	$3, %rdi
	callq	gen_rtx_fmt_ee
	movq	%rax, 56(%rbx)
.LBB78_50:                              # %for.cond212.backedge
                                        #   in Loop: Header=BB78_49 Depth=2
	movq	$32, %rcx
	movq	%r13, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jg	.LBB78_51
	jmp	.LBB78_58
.LBB78_41:                              # %sw.bb
                                        #   in Loop: Header=BB78_38 Depth=1
	movq	8(%r14), %rax
	movslq	(%rax), %r13
	testq	%r13, %r13
	jle	.LBB78_58
	jmp	.LBB78_42
	.align	16, 0x90
.LBB78_46:                              # %for.cond169.backedge.for.body172_crit_edge
                                        #   in Loop: Header=BB78_42 Depth=2
	movq	8(%r14), %rax
.LBB78_42:                              # %for.body172.lr.ph
                                        #   Parent Loop BB78_38 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	decq	%r13
	movq	8(%rax,%r13,8), %rbx
	movzbl	(%rbx), %eax
	cmpq	$34, %rax
	je	.LBB78_47
# BB#43:                                # %lor.lhs.false
                                        #   in Loop: Header=BB78_42 Depth=2
	movl	flag_non_call_exceptions(%rip), %eax
	testq	%rax, %rax
	je	.LBB78_45
# BB#44:                                # %land.lhs.true185
                                        #   in Loop: Header=BB78_42 Depth=2
	movq	32(%rbx), %rdi
	callq	may_trap_p
	testq	%r15, %rax
	je	.LBB78_45
	.align	16, 0x90
.LBB78_47:                              # %if.then191
                                        #   in Loop: Header=BB78_42 Depth=2
	movq	8(%r12), %rdx
	movq	56(%rbx), %rcx
	movq	$3, %rdi
	movq	$23, %rsi
	callq	gen_rtx_fmt_ee
	movq	%rax, 56(%rbx)
.LBB78_45:                              # %for.cond169.backedge
                                        #   in Loop: Header=BB78_42 Depth=2
	movq	$32, %rcx
	movq	%r13, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jg	.LBB78_46
	jmp	.LBB78_58
.LBB78_53:                              # %sw.bb245
                                        #   in Loop: Header=BB78_38 Depth=1
	movq	8(%r14), %rax
	movslq	(%rax), %r13
	testq	%r13, %r13
	jle	.LBB78_58
	jmp	.LBB78_54
	.align	16, 0x90
.LBB78_56:                              # %for.cond251.backedge.for.body254_crit_edge
                                        #   in Loop: Header=BB78_54 Depth=2
	movq	8(%r14), %rax
.LBB78_54:                              # %for.body254.lr.ph
                                        #   Parent Loop BB78_38 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	decq	%r13
	movq	8(%rax,%r13,8), %rbx
	movzbl	(%rbx), %eax
	cmpq	$33, %rax
	jne	.LBB78_55
# BB#57:                                # %if.then266
                                        #   in Loop: Header=BB78_54 Depth=2
	movq	8(%r12), %rdx
	movq	56(%rbx), %rcx
	movzbl	2(%r12), %esi
	movq	$3, %rdi
	callq	gen_rtx_fmt_ee
	movq	%rax, 56(%rbx)
.LBB78_55:                              # %for.cond251.backedge
                                        #   in Loop: Header=BB78_54 Depth=2
	movq	$32, %rcx
	movq	%r13, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jg	.LBB78_56
	jmp	.LBB78_58
.LBB78_59:                              # %for.end288
	movq	%r15, %r13
	movq	64(%rsp), %rax          # 8-byte Reload
	movzbl	(%rax), %eax
	cmpq	$32, %rax
	jne	.LBB78_65
# BB#60:                                # %if.then293
	movq	8(%r14), %rax
	movslq	(%rax), %rbx
	testq	%rbx, %rbx
	jg	.LBB78_61
	jmp	.LBB78_65
	.align	16, 0x90
.LBB78_63:                              # %for.cond299.backedge.for.body302_crit_edge
                                        #   in Loop: Header=BB78_61 Depth=1
	movq	8(%r14), %rax
.LBB78_61:                              # %for.body302.lr.ph
                                        # =>This Inner Loop Header: Depth=1
	decq	%rbx
	movq	8(%rax,%rbx,8), %rax
	movzbl	(%rax), %ecx
	cmpq	$32, %rcx
	jne	.LBB78_62
# BB#64:                                # %if.then313
                                        #   in Loop: Header=BB78_61 Depth=1
	movq	32(%rax), %rdi
	callq	mark_label_nuses
.LBB78_62:                              # %for.cond299.backedge
                                        #   in Loop: Header=BB78_61 Depth=1
	movq	$32, %rcx
	movq	%rbx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jg	.LBB78_63
.LBB78_65:                              # %if.end327
	movzbl	(%r14), %eax
	cmpq	$24, %rax
	jne	.LBB78_69
# BB#66:                                # %for.cond.preheader.i
	movq	8(%r14), %rax
	movslq	(%rax), %rcx
	testq	%rcx, %rcx
	movq	64(%rsp), %r12          # 8-byte Reload
	movq	16(%rsp), %r15          # 8-byte Reload
	jle	.LBB78_70
# BB#67:
	movq	$1, %rbx
	movq	64(%rsp), %rsi          # 8-byte Reload
	.align	16, 0x90
.LBB78_68:                              # %for.body.i
                                        # =>This Inner Loop Header: Depth=1
	movq	(%rax,%rbx,8), %r12
	movq	%r12, %rdi
	callq	add_insn_after
	movq	8(%r14), %rax
	movslq	(%rax), %rdx
	movq	$32, %rcx
	movq	%rbx, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	incq	%rbx
	cmpq	%rdx, %rsi
	movq	%r12, %rsi
	jl	.LBB78_68
	jmp	.LBB78_70
.LBB78_69:                              # %if.else.i
	movq	$32, %rdi
	callq	rtx_alloc
	movq	cfun(%rip), %rcx
	movq	24(%rcx), %rcx
	movl	40(%rcx), %edx
	movq	%rax, %r12
	leaq	1(%rdx), %rax
	movl	%eax, 40(%rcx)
	movl	%edx, 8(%r12)
	movq	%r14, 32(%r12)
	movl	%r13d, 40(%r12)
	movq	$0, 56(%r12)
	movq	$0, 48(%r12)
	movq	%r12, %rdi
	movq	64(%rsp), %rsi          # 8-byte Reload
	callq	add_insn_after
	movq	16(%rsp), %r15          # 8-byte Reload
.LBB78_70:                              # %emit_insn_after.exit
	movq	64(%rsp), %rdi          # 8-byte Reload
	callq	delete_related_insns
	cmpq	$0, 32(%rsp)            # 8-byte Folded Reload
	movq	48(%rsp), %r14          # 8-byte Reload
	je	.LBB78_72
# BB#71:                                # %if.then331
	movq	$35, %rdi
	callq	rtx_alloc
	movq	cfun(%rip), %rcx
	movq	24(%rcx), %rcx
	movl	40(%rcx), %edx
	leaq	1(%rdx), %rsi
	movl	%esi, 40(%rcx)
	movl	%edx, 8(%rax)
	movq	%rax, %rdi
	movq	%r12, %rsi
	callq	add_insn_after
.LBB78_72:                              # %for.cond337.preheader
	movq	40(%rsp), %rbx          # 8-byte Reload
	movq	24(%rbx), %rax
	jmp	.LBB78_73
	.align	16, 0x90
.LBB78_75:                              # %for.cond337.backedge
                                        #   in Loop: Header=BB78_73 Depth=1
	movq	24(%rax), %rax
.LBB78_73:                              # %for.cond337.preheader
                                        # =>This Inner Loop Header: Depth=1
	cmpq	%r15, %rax
	je	.LBB78_81
# BB#74:                                # %for.body340
                                        #   in Loop: Header=BB78_73 Depth=1
	movl	(%rax), %ecx
	testq	$134217728, %rcx        # imm = 0x8000000
	jne	.LBB78_75
# BB#76:                                # %land.lhs.true345
                                        #   in Loop: Header=BB78_73 Depth=1
	andq	$65535, %rcx            # imm = 0xFFFF
	movzbl	rtx_class(%rcx), %ecx
	cmpq	$105, %rcx
	jne	.LBB78_75
# BB#77:                                # %if.then353
                                        #   in Loop: Header=BB78_73 Depth=1
	movq	32(%rax), %rdi
	movq	$1, %rdx
	movq	%rax, %rsi
	callq	try_split
	movq	24(%rax), %rax
	jmp	.LBB78_73
.LBB78_81:                              # %if.end375
	testq	%r13, %r14
	je	.LBB78_85
# BB#82:                                # %cond.true
	testq	%r15, %r15
	je	.LBB78_84
# BB#83:                                # %cond.true378
	addq	$16, %r15
	jmp	.LBB78_86
.LBB78_85:                              # %cond.false382
	addq	$24, %rbx
	movq	%rbx, %r15
	jmp	.LBB78_86
.LBB78_84:                              # %cond.false
	movq	cfun(%rip), %rax
	movq	24(%rax), %r15
	addq	$16, %r15
.LBB78_86:                              # %cond.end386
	movq	(%r15), %rax
.LBB78_87:                              # %return
	addq	$80, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB78_88:                              # %if.then99
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.try_split, %rcx
	movq	$3153, %rsi             # imm = 0xC51
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp568:
	.size	try_split, .Ltmp568-try_split
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI78_0:
	.quad	.LBB78_41
	.quad	.LBB78_58
	.quad	.LBB78_58
	.quad	.LBB78_48
	.quad	.LBB78_53
	.quad	.LBB78_48
	.quad	.LBB78_48

	.text
	.align	16, 0x90
	.type	mark_label_nuses,@function
mark_label_nuses:                       # @mark_label_nuses
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp575:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp576:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp577:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp578:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp579:
	.cfi_def_cfa_offset 48
.Ltmp580:
	.cfi_offset %rbx, -48
.Ltmp581:
	.cfi_offset %r12, -40
.Ltmp582:
	.cfi_offset %r13, -32
.Ltmp583:
	.cfi_offset %r14, -24
.Ltmp584:
	.cfi_offset %r15, -16
	movq	%rdi, %r12
	movzbl	(%r12), %eax
	cmpq	$67, %rax
	jne	.LBB79_2
# BB#1:                                 # %if.then
	movq	8(%r12), %rcx
	movl	32(%rcx), %edx
	incq	%rdx
	movl	%edx, 32(%rcx)
.LBB79_2:                               # %if.end
	movzbl	rtx_length(%rax), %r14d
	testq	%r14, %r14
	je	.LBB79_7
# BB#3:                                 # %for.body.lr.ph
	movq	rtx_format(,%rax,8), %r15
	.align	16, 0x90
.LBB79_4:                               # %for.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB79_10 Depth 2
	movq	%r14, %r13
	movzbl	-1(%r15,%r13), %eax
	leaq	-1(%r13), %r14
	cmpq	$69, %rax
	jne	.LBB79_5
# BB#9:                                 # %if.then23
                                        #   in Loop: Header=BB79_4 Depth=1
	movq	(%r12,%r13,8), %rax
	movslq	(%rax), %rbx
	testq	%rbx, %rbx
	jle	.LBB79_6
	jmp	.LBB79_10
	.align	16, 0x90
.LBB79_11:                              # %for.body31.for.body31_crit_edge
                                        #   in Loop: Header=BB79_10 Depth=2
	movq	(%r12,%r13,8), %rax
.LBB79_10:                              # %for.body31.lr.ph
                                        #   Parent Loop BB79_4 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	decq	%rbx
	movq	8(%rax,%rbx,8), %rdi
	callq	mark_label_nuses
	movq	$32, %rcx
	movq	%rbx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jg	.LBB79_11
	jmp	.LBB79_6
	.align	16, 0x90
.LBB79_5:                               # %for.body
                                        #   in Loop: Header=BB79_4 Depth=1
	cmpq	$101, %rax
	jne	.LBB79_6
# BB#8:                                 # %if.then13
                                        #   in Loop: Header=BB79_4 Depth=1
	movq	(%r12,%r13,8), %rdi
	callq	mark_label_nuses
.LBB79_6:                               # %for.cond.backedge
                                        #   in Loop: Header=BB79_4 Depth=1
	movq	$32, %rcx
	movq	%r14, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jg	.LBB79_4
.LBB79_7:                               # %for.end42
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp585:
	.size	mark_label_nuses, .Ltmp585-mark_label_nuses
	.cfi_endproc

	.globl	emit_insn_after
	.align	16, 0x90
	.type	emit_insn_after,@function
emit_insn_after:                        # @emit_insn_after
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp591:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp592:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp593:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp594:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp595:
	.cfi_def_cfa_offset 48
.Ltmp596:
	.cfi_offset %rbx, -40
.Ltmp597:
	.cfi_offset %r12, -32
.Ltmp598:
	.cfi_offset %r14, -24
.Ltmp599:
	.cfi_offset %r15, -16
	movq	%rdi, %r14
	movzbl	(%r14), %eax
	movq	%rsi, %r15
	cmpq	$24, %rax
	jne	.LBB80_5
# BB#1:                                 # %for.cond.preheader
	movq	8(%r14), %rax
	movslq	(%rax), %rcx
	testq	%rcx, %rcx
	jle	.LBB80_2
# BB#3:
	movq	$1, %rbx
	.align	16, 0x90
.LBB80_4:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	(%rax,%rbx,8), %r12
	movq	%r12, %rdi
	movq	%r15, %rsi
	callq	add_insn_after
	movq	8(%r14), %rax
	movslq	(%rax), %rdx
	movq	$32, %rcx
	movq	%rbx, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	incq	%rbx
	cmpq	%rdx, %rsi
	movq	%r12, %r15
	jl	.LBB80_4
	jmp	.LBB80_6
.LBB80_5:                               # %if.else
	movq	$32, %rdi
	callq	rtx_alloc
	movq	cfun(%rip), %rcx
	movq	24(%rcx), %rcx
	movl	40(%rcx), %edx
	movq	%rax, %r12
	leaq	1(%rdx), %rax
	movl	%eax, 40(%rcx)
	movl	%edx, 8(%r12)
	movq	%r14, 32(%r12)
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	movl	%eax, 40(%r12)
	movq	$0, 56(%r12)
	movq	$0, 48(%r12)
	movq	%r12, %rdi
	movq	%r15, %rsi
	callq	add_insn_after
	jmp	.LBB80_6
.LBB80_2:
	movq	%r15, %r12
.LBB80_6:                               # %if.end
	movq	%r12, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.Ltmp600:
	.size	emit_insn_after, .Ltmp600-emit_insn_after
	.cfi_endproc

	.globl	emit_barrier_after
	.align	16, 0x90
	.type	emit_barrier_after,@function
emit_barrier_after:                     # @emit_barrier_after
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp604:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp605:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp606:
	.cfi_def_cfa_offset 32
.Ltmp607:
	.cfi_offset %rbx, -24
.Ltmp608:
	.cfi_offset %r14, -16
	movq	%rdi, %r14
	movq	$35, %rdi
	callq	rtx_alloc
	movq	cfun(%rip), %rcx
	movq	24(%rcx), %rcx
	movl	40(%rcx), %edx
	movq	%rax, %rbx
	leaq	1(%rdx), %rax
	movl	%eax, 40(%rcx)
	movl	%edx, 8(%rbx)
	movq	%rbx, %rdi
	movq	%r14, %rsi
	callq	add_insn_after
	movq	%rbx, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp609:
	.size	emit_barrier_after, .Ltmp609-emit_barrier_after
	.cfi_endproc

	.globl	make_insn_raw
	.align	16, 0x90
	.type	make_insn_raw,@function
make_insn_raw:                          # @make_insn_raw
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp612:
	.cfi_def_cfa_offset 16
.Ltmp613:
	.cfi_offset %rbx, -16
	movq	%rdi, %rbx
	movq	$32, %rdi
	callq	rtx_alloc
	movq	cfun(%rip), %rcx
	movq	24(%rcx), %rcx
	movl	40(%rcx), %edx
	leaq	1(%rdx), %rsi
	movl	%esi, 40(%rcx)
	movl	%edx, 8(%rax)
	movq	%rbx, 32(%rax)
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	movl	%ecx, 40(%rax)
	movq	$0, 56(%rax)
	movq	$0, 48(%rax)
	popq	%rbx
	retq
.Ltmp614:
	.size	make_insn_raw, .Ltmp614-make_insn_raw
	.cfi_endproc

	.globl	add_insn
	.align	16, 0x90
	.type	add_insn,@function
add_insn:                               # @add_insn
	.cfi_startproc
# BB#0:                                 # %entry
	movq	cfun(%rip), %rax
	movq	24(%rax), %rax
	movq	16(%rax), %rax
	movq	%rax, 16(%rdi)
	movq	$0, 24(%rdi)
	movq	cfun(%rip), %rax
	movq	24(%rax), %rax
	movq	16(%rax), %rcx
	testq	%rcx, %rcx
	je	.LBB83_2
# BB#1:                                 # %if.then
	movq	%rdi, 24(%rcx)
	movq	cfun(%rip), %rax
	movq	24(%rax), %rax
.LBB83_2:                               # %if.end
	cmpq	$0, 8(%rax)
	jne	.LBB83_4
# BB#3:                                 # %if.then13
	movq	%rdi, 8(%rax)
	movq	cfun(%rip), %rax
	movq	24(%rax), %rax
.LBB83_4:                               # %if.end16
	movq	%rdi, 16(%rax)
	retq
.Ltmp615:
	.size	add_insn, .Ltmp615-add_insn
	.cfi_endproc

	.globl	add_insn_after
	.align	16, 0x90
	.type	add_insn_after,@function
add_insn_after:                         # @add_insn_after
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp620:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp621:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp622:
	.cfi_def_cfa_offset 32
.Ltmp623:
	.cfi_offset %rbx, -32
.Ltmp624:
	.cfi_offset %r14, -24
.Ltmp625:
	.cfi_offset %r15, -16
	movq	%rsi, %rbx
	movl	optimize(%rip), %ecx
	movq	24(%rbx), %rax
	movq	%rdi, %r14
	testq	%rcx, %rcx
	je	.LBB84_4
# BB#1:                                 # %land.lhs.true
	movzbl	3(%rbx), %ecx
	testq	$8, %rcx
	jne	.LBB84_2
.LBB84_4:                               # %if.end
	testq	%rax, %rax
	movq	%rax, 24(%r14)
	movq	%rbx, 16(%r14)
	je	.LBB84_8
# BB#5:                                 # %if.then9
	movq	%r14, 16(%rax)
	movzbl	(%rax), %ecx
	cmpq	$32, %rcx
	jne	.LBB84_16
# BB#6:                                 # %land.lhs.true15
	movq	32(%rax), %rax
	movzbl	(%rax), %ecx
	cmpq	$24, %rcx
	jne	.LBB84_16
# BB#7:                                 # %if.then22
	movq	8(%rax), %rax
	movq	8(%rax), %rax
	movq	%r14, 16(%rax)
	jmp	.LBB84_16
.LBB84_8:                               # %if.else
	movq	cfun(%rip), %rax
	movq	24(%rax), %rax
	movq	16(%rax), %rcx
	cmpq	%rbx, %rcx
	je	.LBB84_9
# BB#10:                                # %if.else37
	movq	32(%rax), %rax
	jmp	.LBB84_11
	.align	16, 0x90
.LBB84_13:                              # %for.cond
                                        #   in Loop: Header=BB84_11 Depth=1
	movq	24(%rax), %rax
.LBB84_11:                              # %if.else37
                                        # =>This Inner Loop Header: Depth=1
	testq	%rax, %rax
	je	.LBB84_12
# BB#14:                                # %for.body
                                        #   in Loop: Header=BB84_11 Depth=1
	movq	8(%rax), %rcx
	cmpq	%rbx, %rcx
	jne	.LBB84_13
# BB#15:                                # %if.then41
	movq	%r14, 8(%rax)
	jmp	.LBB84_16
.LBB84_9:                               # %if.then34
	movq	%r14, 16(%rax)
.LBB84_16:                              # %if.end49
	movq	basic_block_for_insn(%rip), %rax
	testq	%rax, %rax
	je	.LBB84_24
# BB#17:                                # %land.lhs.true51
	movl	8(%rbx), %esi
	movq	$32, %rcx
	movq	%rsi, %rdx
	movq	(%rax), %rdi
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rdi, %rsi
	jae	.LBB84_24
# BB#18:                                # %land.lhs.true56
	movq	32(%rax,%rdx,8), %r15
	testq	%r15, %r15
	je	.LBB84_24
# BB#19:                                # %if.then63
	movq	%r14, %rdi
	movq	%r15, %rsi
	callq	set_block_for_insn
	movq	8(%r15), %rax
	cmpq	%rbx, %rax
	jne	.LBB84_24
# BB#20:                                # %land.lhs.true66
	movzbl	(%r14), %eax
	cmpq	$35, %rax
	je	.LBB84_24
# BB#21:                                # %land.lhs.true66
	cmpq	$37, %rax
	jne	.LBB84_23
# BB#22:                                # %lor.lhs.false
	movl	40(%r14), %eax
	movabsq	$4294967216, %rcx       # imm = 0xFFFFFFB0
	cmpq	%rcx, %rax
	je	.LBB84_24
.LBB84_23:                              # %if.then81
	movq	%r14, 8(%r15)
.LBB84_24:                              # %if.end84
	movq	%r14, 24(%rbx)
	movzbl	(%rbx), %eax
	cmpq	$32, %rax
	jne	.LBB84_27
# BB#25:                                # %land.lhs.true92
	movq	32(%rbx), %rax
	movzbl	(%rax), %ecx
	cmpq	$24, %rcx
	jne	.LBB84_27
# BB#26:                                # %if.then100
	movq	8(%rax), %rax
	movl	(%rax), %edx
	decq	%rdx
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	8(%rax,%rdx,8), %rax
	movq	%r14, 24(%rax)
.LBB84_27:                              # %if.end116
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.LBB84_2:                               # %if.then
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.add_insn_after, %rcx
	movq	$3365, %rsi             # imm = 0xD25
	jmp	.LBB84_3
.LBB84_12:                              # %if.then46
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.add_insn_after, %rcx
	movq	$3390, %rsi             # imm = 0xD3E
.LBB84_3:                               # %if.then
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp626:
	.size	add_insn_after, .Ltmp626-add_insn_after
	.cfi_endproc

	.globl	add_insn_before
	.align	16, 0x90
	.type	add_insn_before,@function
add_insn_before:                        # @add_insn_before
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp631:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp632:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp633:
	.cfi_def_cfa_offset 32
.Ltmp634:
	.cfi_offset %rbx, -32
.Ltmp635:
	.cfi_offset %r14, -24
.Ltmp636:
	.cfi_offset %r15, -16
	movq	%rsi, %rbx
	movl	optimize(%rip), %ecx
	movq	16(%rbx), %rax
	movq	%rdi, %r14
	testq	%rcx, %rcx
	je	.LBB85_4
# BB#1:                                 # %land.lhs.true
	movzbl	3(%rbx), %ecx
	testq	$8, %rcx
	jne	.LBB85_2
.LBB85_4:                               # %if.end
	testq	%rax, %rax
	movq	%rax, 16(%r14)
	movq	%rbx, 24(%r14)
	je	.LBB85_8
# BB#5:                                 # %if.then9
	movq	%r14, 24(%rax)
	movzbl	(%rax), %ecx
	cmpq	$32, %rcx
	jne	.LBB85_16
# BB#6:                                 # %land.lhs.true15
	movq	32(%rax), %rax
	movzbl	(%rax), %ecx
	cmpq	$24, %rcx
	jne	.LBB85_16
# BB#7:                                 # %if.then22
	movq	8(%rax), %rax
	movl	(%rax), %edx
	decq	%rdx
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	8(%rax,%rdx,8), %rax
	movq	%r14, 24(%rax)
	jmp	.LBB85_16
.LBB85_8:                               # %if.else
	movq	cfun(%rip), %rax
	movq	24(%rax), %rax
	movq	8(%rax), %rcx
	cmpq	%rbx, %rcx
	je	.LBB85_9
# BB#10:                                # %if.else40
	movq	32(%rax), %rax
	jmp	.LBB85_11
	.align	16, 0x90
.LBB85_13:                              # %for.cond
                                        #   in Loop: Header=BB85_11 Depth=1
	movq	24(%rax), %rax
.LBB85_11:                              # %if.else40
                                        # =>This Inner Loop Header: Depth=1
	testq	%rax, %rax
	je	.LBB85_12
# BB#14:                                # %for.body
                                        #   in Loop: Header=BB85_11 Depth=1
	movq	(%rax), %rcx
	cmpq	%rbx, %rcx
	jne	.LBB85_13
# BB#15:                                # %if.then44
	movq	%r14, (%rax)
	jmp	.LBB85_16
.LBB85_9:                               # %if.then37
	movq	%r14, 8(%rax)
.LBB85_16:                              # %if.end51
	movq	basic_block_for_insn(%rip), %rax
	testq	%rax, %rax
	je	.LBB85_24
# BB#17:                                # %land.lhs.true53
	movl	8(%rbx), %esi
	movq	$32, %rcx
	movq	%rsi, %rdx
	movq	(%rax), %rdi
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rdi, %rsi
	jae	.LBB85_24
# BB#18:                                # %land.lhs.true58
	movq	32(%rax,%rdx,8), %r15
	testq	%r15, %r15
	je	.LBB85_24
# BB#19:                                # %if.then66
	movq	%r14, %rdi
	movq	%r15, %rsi
	callq	set_block_for_insn
	movq	(%r15), %rax
	cmpq	%r14, %rax
	jne	.LBB85_24
# BB#20:                                # %land.lhs.true69
	movzbl	(%r14), %eax
	cmpq	$35, %rax
	je	.LBB85_24
# BB#21:                                # %land.lhs.true69
	cmpq	$37, %rax
	jne	.LBB85_23
# BB#22:                                # %lor.lhs.false
	movl	40(%r14), %eax
	movabsq	$4294967216, %rcx       # imm = 0xFFFFFFB0
	cmpq	%rcx, %rax
	jne	.LBB85_23
.LBB85_24:                              # %if.end86
	movq	%r14, 16(%rbx)
	movzbl	(%rbx), %eax
	cmpq	$32, %rax
	jne	.LBB85_27
# BB#25:                                # %land.lhs.true94
	movq	32(%rbx), %rax
	movzbl	(%rax), %ecx
	cmpq	$24, %rcx
	jne	.LBB85_27
# BB#26:                                # %if.then102
	movq	8(%rax), %rax
	movq	8(%rax), %rax
	movq	%r14, 16(%rax)
.LBB85_27:                              # %if.end114
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.LBB85_2:                               # %if.then
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.add_insn_before, %rcx
	movq	$3429, %rsi             # imm = 0xD65
	jmp	.LBB85_3
.LBB85_12:                              # %if.then48
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.add_insn_before, %rcx
	movq	$3457, %rsi             # imm = 0xD81
	jmp	.LBB85_3
.LBB85_23:                              # %if.then84
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.add_insn_before, %rcx
	movq	$3472, %rsi             # imm = 0xD90
.LBB85_3:                               # %if.then
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp637:
	.size	add_insn_before, .Ltmp637-add_insn_before
	.cfi_endproc

	.globl	remove_insn
	.align	16, 0x90
	.type	remove_insn,@function
remove_insn:                            # @remove_insn
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rax
.Ltmp639:
	.cfi_def_cfa_offset 16
	movq	16(%rdi), %r8
	movq	24(%rdi), %r10
	testq	%r8, %r8
	je	.LBB86_4
# BB#1:                                 # %if.then
	movq	%r10, 24(%r8)
	movzbl	(%r8), %eax
	cmpq	$32, %rax
	jne	.LBB86_13
# BB#2:                                 # %land.lhs.true
	movq	32(%r8), %rax
	movzbl	(%rax), %ecx
	cmpq	$24, %rcx
	jne	.LBB86_13
# BB#3:                                 # %if.then13
	movq	8(%rax), %rax
	movl	(%rax), %esi
	decq	%rsi
	movq	$32, %rcx
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	movq	8(%rax,%rsi,8), %rax
	movq	%r10, 24(%rax)
	jmp	.LBB86_13
.LBB86_4:                               # %if.else
	movq	cfun(%rip), %rax
	movq	24(%rax), %rax
	movq	8(%rax), %rcx
	cmpq	%rdi, %rcx
	je	.LBB86_5
# BB#6:                                 # %if.else30
	movq	32(%rax), %rcx
	jmp	.LBB86_7
	.align	16, 0x90
.LBB86_10:                              # %for.cond
                                        #   in Loop: Header=BB86_7 Depth=1
	movq	24(%rcx), %rcx
.LBB86_7:                               # %if.else30
                                        # =>This Inner Loop Header: Depth=1
	testq	%rcx, %rcx
	je	.LBB86_8
# BB#11:                                # %for.body
                                        #   in Loop: Header=BB86_7 Depth=1
	movq	(%rcx), %rax
	cmpq	%rdi, %rax
	jne	.LBB86_10
# BB#12:                                # %if.then34
	movq	%r10, (%rcx)
	jmp	.LBB86_13
.LBB86_5:                               # %if.then27
	movq	%r10, 8(%rax)
.LBB86_13:                              # %if.end42
	testq	%r10, %r10
	je	.LBB86_17
# BB#14:                                # %if.then44
	movq	%r8, 16(%r10)
	movzbl	(%r10), %eax
	cmpq	$32, %rax
	jne	.LBB86_25
# BB#15:                                # %land.lhs.true51
	movq	32(%r10), %rax
	movzbl	(%rax), %ecx
	cmpq	$24, %rcx
	jne	.LBB86_25
# BB#16:                                # %if.then58
	movq	8(%rax), %rax
	movq	8(%rax), %rax
	movq	%r8, 16(%rax)
	jmp	.LBB86_25
.LBB86_17:                              # %if.else71
	movq	cfun(%rip), %rax
	movq	24(%rax), %rax
	movq	16(%rax), %rcx
	cmpq	%rdi, %rcx
	je	.LBB86_18
# BB#19:                                # %if.else77
	movq	32(%rax), %rcx
	jmp	.LBB86_20
	.align	16, 0x90
.LBB86_22:                              # %for.cond81
                                        #   in Loop: Header=BB86_20 Depth=1
	movq	24(%rcx), %rcx
.LBB86_20:                              # %if.else77
                                        # =>This Inner Loop Header: Depth=1
	testq	%rcx, %rcx
	je	.LBB86_21
# BB#23:                                # %for.body83
                                        #   in Loop: Header=BB86_20 Depth=1
	movq	8(%rcx), %rax
	cmpq	%rdi, %rax
	jne	.LBB86_22
# BB#24:                                # %if.then85
	movq	%r8, 8(%rcx)
	jmp	.LBB86_25
.LBB86_18:                              # %if.then74
	movq	%r8, 16(%rax)
.LBB86_25:                              # %if.end95
	movq	basic_block_for_insn(%rip), %rsi
	testq	%rsi, %rsi
	je	.LBB86_34
# BB#26:                                # %land.lhs.true97
	movl	8(%rdi), %r9d
	movq	$32, %rcx
	movq	%r9, %rax
	movq	(%rsi), %rdx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	%rdx, %r9
	jae	.LBB86_34
# BB#27:                                # %land.lhs.true102
	movq	32(%rsi,%rax,8), %rcx
	testq	%rcx, %rcx
	je	.LBB86_34
# BB#28:                                # %if.then110
	movq	(%rcx), %rax
	cmpq	%rdi, %rax
	jne	.LBB86_32
# BB#29:                                # %if.then113
	movzbl	(%rdi), %eax
	cmpq	$37, %rax
	je	.LBB86_30
# BB#31:                                # %if.end119
	movq	%r10, (%rcx)
.LBB86_32:                              # %if.end121
	movq	8(%rcx), %rax
	cmpq	%rdi, %rax
	jne	.LBB86_34
# BB#33:                                # %if.then124
	movq	%r8, 8(%rcx)
.LBB86_34:                              # %if.end127
	popq	%rax
	retq
.LBB86_8:                               # %if.then39
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.remove_insn, %rcx
	movq	$3513, %rsi             # imm = 0xDB9
	jmp	.LBB86_9
.LBB86_21:                              # %if.then92
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.remove_insn, %rcx
	movq	$3536, %rsi             # imm = 0xDD0
	jmp	.LBB86_9
.LBB86_30:                              # %if.then118
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.remove_insn, %rcx
	movq	$3547, %rsi             # imm = 0xDDB
.LBB86_9:                               # %if.then39
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp640:
	.size	remove_insn, .Ltmp640-remove_insn
	.cfi_endproc

	.globl	delete_insns_since
	.align	16, 0x90
	.type	delete_insns_since,@function
delete_insns_since:                     # @delete_insns_since
	.cfi_startproc
# BB#0:                                 # %entry
	testq	%rdi, %rdi
	je	.LBB87_1
# BB#2:                                 # %if.else
	movq	$0, 24(%rdi)
	jmp	.LBB87_3
.LBB87_1:                               # %if.then
	movq	cfun(%rip), %rax
	movq	24(%rax), %rax
	movq	$0, 8(%rax)
.LBB87_3:                               # %if.end
	movq	cfun(%rip), %rax
	movq	24(%rax), %rax
	movq	%rdi, 16(%rax)
	retq
.Ltmp641:
	.size	delete_insns_since, .Ltmp641-delete_insns_since
	.cfi_endproc

	.globl	reorder_insns_nobb
	.align	16, 0x90
	.type	reorder_insns_nobb,@function
reorder_insns_nobb:                     # @reorder_insns_nobb
	.cfi_startproc
# BB#0:                                 # %entry
	movq	16(%rdi), %rax
	testq	%rax, %rax
	je	.LBB88_2
# BB#1:                                 # %if.then
	movq	24(%rsi), %rcx
	movq	%rcx, 24(%rax)
.LBB88_2:                               # %if.end
	movq	24(%rsi), %rax
	testq	%rax, %rax
	je	.LBB88_4
# BB#3:                                 # %if.then14
	movq	16(%rdi), %rcx
	movq	%rcx, 16(%rax)
.LBB88_4:                               # %if.end24
	movq	cfun(%rip), %rax
	movq	24(%rax), %rax
	movq	16(%rax), %rcx
	cmpq	%rsi, %rcx
	jne	.LBB88_6
# BB#5:                                 # %if.then25
	movq	16(%rdi), %rcx
	movq	%rcx, 16(%rax)
	movq	cfun(%rip), %rax
	movq	24(%rax), %rax
.LBB88_6:                               # %if.end31
	movq	8(%rax), %rcx
	cmpq	%rdi, %rcx
	jne	.LBB88_8
# BB#7:                                 # %if.then34
	movq	24(%rsi), %rcx
	movq	%rcx, 8(%rax)
.LBB88_8:                               # %if.end40
	movq	24(%rdx), %rcx
	xorq	%rax, %rax
	testq	%rcx, %rcx
	je	.LBB88_10
# BB#9:                                 # %if.then45
	movq	%rsi, 16(%rcx)
	movq	24(%rdx), %rax
.LBB88_10:                              # %if.end52
	movq	%rax, 24(%rsi)
	movq	%rdx, 16(%rdi)
	movq	%rdi, 24(%rdx)
	movq	cfun(%rip), %rax
	movq	24(%rax), %rax
	movq	16(%rax), %rcx
	cmpq	%rdx, %rcx
	jne	.LBB88_12
# BB#11:                                # %if.then68
	movq	%rsi, 16(%rax)
.LBB88_12:                              # %if.end71
	retq
.Ltmp642:
	.size	reorder_insns_nobb, .Ltmp642-reorder_insns_nobb
	.cfi_endproc

	.globl	reorder_insns
	.align	16, 0x90
	.type	reorder_insns,@function
reorder_insns:                          # @reorder_insns
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp647:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp648:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp649:
	.cfi_def_cfa_offset 32
.Ltmp650:
	.cfi_offset %rbx, -32
.Ltmp651:
	.cfi_offset %r14, -24
.Ltmp652:
	.cfi_offset %r15, -16
	movq	%rdi, %rbx
	movq	16(%rbx), %r8
	movq	%rsi, %r14
	testq	%r8, %r8
	je	.LBB89_2
# BB#1:                                 # %if.then.i
	movq	24(%r14), %rax
	movq	%rax, 24(%r8)
.LBB89_2:                               # %if.end.i
	movq	24(%r14), %rax
	testq	%rax, %rax
	je	.LBB89_4
# BB#3:                                 # %if.then14.i
	movq	16(%rbx), %rcx
	movq	%rcx, 16(%rax)
.LBB89_4:                               # %if.end24.i
	movq	cfun(%rip), %rax
	movq	24(%rax), %rcx
	movq	16(%rcx), %rax
	cmpq	%r14, %rax
	jne	.LBB89_6
# BB#5:                                 # %if.then25.i
	movq	16(%rbx), %rax
	movq	%rax, 16(%rcx)
	movq	cfun(%rip), %rax
	movq	24(%rax), %rcx
.LBB89_6:                               # %if.end31.i
	movq	8(%rcx), %rax
	cmpq	%rbx, %rax
	jne	.LBB89_8
# BB#7:                                 # %if.then34.i
	movq	24(%r14), %rax
	movq	%rax, 8(%rcx)
.LBB89_8:                               # %if.end40.i
	movq	24(%rdx), %rcx
	xorq	%rax, %rax
	testq	%rcx, %rcx
	je	.LBB89_10
# BB#9:                                 # %if.then45.i
	movq	%r14, 16(%rcx)
	movq	24(%rdx), %rax
.LBB89_10:                              # %if.end52.i
	movq	%rax, 24(%r14)
	movq	%rdx, 16(%rbx)
	movq	%rbx, 24(%rdx)
	movq	cfun(%rip), %rax
	movq	24(%rax), %rax
	movq	16(%rax), %rcx
	cmpq	%rdx, %rcx
	jne	.LBB89_12
# BB#11:                                # %if.then68.i
	movq	%r14, 16(%rax)
.LBB89_12:                              # %reorder_insns_nobb.exit
	movq	basic_block_for_insn(%rip), %rsi
	testq	%rsi, %rsi
	je	.LBB89_23
# BB#13:                                # %land.lhs.true
	movl	8(%rdx), %r9d
	movq	$32, %rcx
	movq	%r9, %rax
	movq	(%rsi), %rdi
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	%rdi, %r9
	jae	.LBB89_23
# BB#14:                                # %land.lhs.true4
	movq	32(%rsi,%rax,8), %r15
	testq	%r15, %r15
	je	.LBB89_23
# BB#15:                                # %land.lhs.true12
	movl	8(%rbx), %r9d
	movq	$32, %rcx
	movq	%r9, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	%rdi, %r9
	jae	.LBB89_19
# BB#16:                                # %land.lhs.true20
	movq	32(%rsi,%rax,8), %rax
	testq	%rax, %rax
	je	.LBB89_19
# BB#17:                                # %if.then29
	movq	8(%rax), %rcx
	cmpq	%r14, %rcx
	jne	.LBB89_19
# BB#18:                                # %if.then32
	movq	%r8, 8(%rax)
.LBB89_19:                              # %if.end34
	movq	8(%r15), %rax
	cmpq	%rdx, %rax
	jne	.LBB89_21
# BB#20:                                # %if.then38
	movq	%r14, 8(%r15)
.LBB89_21:                              # %for.cond.preheader
	movq	24(%r14), %rax
	cmpq	%rbx, %rax
	je	.LBB89_23
	.align	16, 0x90
.LBB89_22:                              # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	%rbx, %rdi
	movq	%r15, %rsi
	callq	set_block_for_insn
	movq	24(%rbx), %rbx
	movq	24(%r14), %rax
	cmpq	%rax, %rbx
	jne	.LBB89_22
.LBB89_23:                              # %if.end49
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp653:
	.size	reorder_insns, .Ltmp653-reorder_insns
	.cfi_endproc

	.globl	reorder_insns_with_line_notes
	.align	16, 0x90
	.type	reorder_insns_with_line_notes,@function
reorder_insns_with_line_notes:          # @reorder_insns_with_line_notes
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp660:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp661:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp662:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp663:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp664:
	.cfi_def_cfa_offset 48
.Ltmp665:
	.cfi_offset %rbx, -48
.Ltmp666:
	.cfi_offset %r12, -40
.Ltmp667:
	.cfi_offset %r13, -32
.Ltmp668:
	.cfi_offset %r14, -24
.Ltmp669:
	.cfi_offset %r15, -16
	movl	no_line_numbers(%rip), %eax
	movq	%rdx, %r12
	movq	%rsi, %r14
	xorq	%r15, %r15
	testq	%rax, %rax
	movabsq	$0, %rbx
	jne	.LBB90_7
# BB#1:                                 # %entry
	testq	%rdi, %rdi
	movabsq	$0, %rbx
	je	.LBB90_7
# BB#2:
	movq	%rdi, %rbx
	.align	16, 0x90
.LBB90_3:                               # %for.body.i
                                        # =>This Inner Loop Header: Depth=1
	movzbl	(%rbx), %ecx
	cmpq	$37, %rcx
	jne	.LBB90_5
# BB#4:                                 # %land.lhs.true.i
                                        #   in Loop: Header=BB90_3 Depth=1
	movslq	40(%rbx), %rcx
	testq	%rcx, %rcx
	jns	.LBB90_7
.LBB90_5:                               # %for.inc.i
                                        #   in Loop: Header=BB90_3 Depth=1
	movq	16(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB90_3
# BB#6:
	xorq	%rbx, %rbx
.LBB90_7:                               # %find_line_note.exit
	testq	%rax, %rax
	jne	.LBB90_14
# BB#8:                                 # %find_line_note.exit
	testq	%r12, %r12
	je	.LBB90_14
# BB#9:
	movq	%r12, %rax
	.align	16, 0x90
.LBB90_10:                              # %for.body.i39
                                        # =>This Inner Loop Header: Depth=1
	movzbl	(%rax), %ecx
	cmpq	$37, %rcx
	jne	.LBB90_13
# BB#11:                                # %land.lhs.true.i43
                                        #   in Loop: Header=BB90_10 Depth=1
	movslq	40(%rax), %rcx
	testq	%rcx, %rcx
	jns	.LBB90_12
.LBB90_13:                              # %for.inc.i47
                                        #   in Loop: Header=BB90_10 Depth=1
	movq	16(%rax), %rax
	testq	%rax, %rax
	jne	.LBB90_10
	jmp	.LBB90_14
.LBB90_12:
	movq	%rax, %r15
.LBB90_14:                              # %find_line_note.exit49
	movq	%r14, %rsi
	movq	%r12, %rdx
	callq	reorder_insns
	cmpq	%r15, %rbx
	je	.LBB90_24
# BB#15:                                # %if.end
	testq	%rbx, %rbx
	je	.LBB90_20
# BB#16:                                # %if.then2
	movl	no_line_numbers(%rip), %eax
	movl	40(%rbx), %r13d
	testq	%rax, %rax
	je	.LBB90_19
# BB#17:                                # %if.then2
	movq	$32, %rcx
	movq	%r13, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jle	.LBB90_19
# BB#18:                                # %if.then.i56
	movq	cfun(%rip), %rax
	movq	24(%rax), %rax
	movl	40(%rax), %ecx
	incq	%rcx
	movl	%ecx, 40(%rax)
	jmp	.LBB90_20
.LBB90_19:                              # %if.end.i66
	movq	32(%rbx), %rbx
	movq	$37, %rdi
	callq	rtx_alloc
	movq	cfun(%rip), %rcx
	movq	24(%rcx), %rcx
	movl	40(%rcx), %edx
	leaq	1(%rdx), %rsi
	movl	%esi, 40(%rcx)
	movl	%edx, 8(%rax)
	movq	%rbx, 32(%rax)
	movl	%r13d, 40(%rax)
	movq	%rax, %rdi
	movq	%r12, %rsi
	callq	add_insn_after
.LBB90_20:                              # %if.end6
	testq	%r15, %r15
	je	.LBB90_24
# BB#21:                                # %if.then8
	movl	no_line_numbers(%rip), %eax
	movl	40(%r15), %r12d
	testq	%rax, %rax
	je	.LBB90_25
# BB#22:                                # %if.then8
	movq	$32, %rcx
	movq	%r12, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jle	.LBB90_25
# BB#23:                                # %if.then.i
	movq	cfun(%rip), %rax
	movq	24(%rax), %rax
	movl	40(%rax), %ecx
	incq	%rcx
	movl	%ecx, 40(%rax)
.LBB90_24:                              # %if.end16
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB90_25:                              # %if.end.i
	movq	32(%r15), %rbx
	movq	$37, %rdi
	callq	rtx_alloc
	movq	cfun(%rip), %rcx
	movq	24(%rcx), %rcx
	movl	40(%rcx), %edx
	leaq	1(%rdx), %rsi
	movl	%esi, 40(%rcx)
	movl	%edx, 8(%rax)
	movq	%rbx, 32(%rax)
	movl	%r12d, 40(%rax)
	movq	%rax, %rdi
	movq	%r14, %rsi
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	add_insn_after  # TAILCALL
.Ltmp670:
	.size	reorder_insns_with_line_notes, .Ltmp670-reorder_insns_with_line_notes
	.cfi_endproc

	.globl	emit_line_note_after
	.align	16, 0x90
	.type	emit_line_note_after,@function
emit_line_note_after:                   # @emit_line_note_after
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp676:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp677:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp678:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp679:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp680:
	.cfi_def_cfa_offset 48
.Ltmp681:
	.cfi_offset %rbx, -40
.Ltmp682:
	.cfi_offset %r12, -32
.Ltmp683:
	.cfi_offset %r14, -24
.Ltmp684:
	.cfi_offset %r15, -16
	movl	no_line_numbers(%rip), %eax
	movq	%rdx, %r14
	movq	%rsi, %r12
	movq	%rdi, %r15
	testq	%rax, %rax
	je	.LBB91_3
# BB#1:                                 # %entry
	movq	$32, %rcx
	movq	%r12, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jle	.LBB91_3
# BB#2:                                 # %if.then
	movq	cfun(%rip), %rax
	movq	24(%rax), %rax
	movl	40(%rax), %ecx
	incq	%rcx
	movl	%ecx, 40(%rax)
	xorq	%rbx, %rbx
	jmp	.LBB91_4
.LBB91_3:                               # %if.end
	movq	$37, %rdi
	callq	rtx_alloc
	movq	cfun(%rip), %rcx
	movq	24(%rcx), %rcx
	movl	40(%rcx), %edx
	movq	%rax, %rbx
	leaq	1(%rdx), %rax
	movl	%eax, 40(%rcx)
	movl	%edx, 8(%rbx)
	movq	%r15, 32(%rbx)
	movl	%r12d, 40(%rbx)
	movq	%rbx, %rdi
	movq	%r14, %rsi
	callq	add_insn_after
.LBB91_4:                               # %return
	movq	%rbx, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.Ltmp685:
	.size	emit_line_note_after, .Ltmp685-emit_line_note_after
	.cfi_endproc

	.globl	remove_unnecessary_notes
	.align	16, 0x90
	.type	remove_unnecessary_notes,@function
remove_unnecessary_notes:               # @remove_unnecessary_notes
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp692:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp693:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp694:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp695:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp696:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp697:
	.cfi_def_cfa_offset 64
.Ltmp698:
	.cfi_offset %rbx, -48
.Ltmp699:
	.cfi_offset %r12, -40
.Ltmp700:
	.cfi_offset %r13, -32
.Ltmp701:
	.cfi_offset %r14, -24
.Ltmp702:
	.cfi_offset %r15, -16
	movq	cfun(%rip), %rax
	movq	24(%rax), %rax
	movq	8(%rax), %rax
	movq	24(%rax), %r12
	xorq	%r15, %r15
	testq	%r12, %r12
	je	.LBB92_1
# BB#2:
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	xorq	%r14, %r14
	.align	16, 0x90
.LBB92_3:                               # %for.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB92_22 Depth 2
	movq	%r12, %rbx
	movzbl	(%rbx), %eax
	movq	24(%rbx), %r12
	cmpq	$37, %rax
	jne	.LBB92_30
# BB#4:                                 # %if.end
                                        #   in Loop: Header=BB92_3 Depth=1
	movl	40(%rbx), %eax
	addq	$99, %rax
	andq	%r13, %rax
	cmpq	$14, %rax
	ja	.LBB92_30
# BB#5:                                 # %if.end
                                        #   in Loop: Header=BB92_3 Depth=1
	jmpq	*.LJTI92_0(,%rax,8)
.LBB92_6:                               # %sw.bb
                                        #   in Loop: Header=BB92_3 Depth=1
	movq	%rbx, %rdi
	callq	remove_insn
	jmp	.LBB92_30
.LBB92_14:                              # %sw.bb27
                                        #   in Loop: Header=BB92_3 Depth=1
	cmpq	$0, 32(%rbx)
	je	.LBB92_15
# BB#16:                                # %if.end32
                                        #   in Loop: Header=BB92_3 Depth=1
	movq	%rbx, %rdi
	movq	%r15, %rsi
	callq	alloc_INSN_LIST
	movq	%rax, %r15
	jmp	.LBB92_30
.LBB92_17:                              # %sw.bb34
                                        #   in Loop: Header=BB92_3 Depth=1
	testq	%r15, %r15
	je	.LBB92_18
# BB#19:                                # %if.end37
                                        #   in Loop: Header=BB92_3 Depth=1
	movq	8(%r15), %rax
	movq	32(%rax), %rax
	movq	32(%rbx), %rcx
	cmpq	%rcx, %rax
	jne	.LBB92_20
# BB#21:                                # %if.end49
                                        #   in Loop: Header=BB92_3 Depth=1
	movq	16(%r15), %rax
	movq	%rax, 8(%rsp)           # 8-byte Spill
	movq	%r15, %rdi
	callq	free_INSN_LIST_node
	movq	16(%rbx), %r15
	jmp	.LBB92_22
.LBB92_7:                               # %sw.bb6
                                        #   in Loop: Header=BB92_3 Depth=1
	movq	%rbx, %rdi
	movq	%r14, %rsi
	callq	alloc_INSN_LIST
	movq	%rax, %r14
	jmp	.LBB92_30
.LBB92_8:                               # %sw.bb8
                                        #   in Loop: Header=BB92_3 Depth=1
	testq	%r14, %r14
	je	.LBB92_9
# BB#11:                                # %if.end11
                                        #   in Loop: Header=BB92_3 Depth=1
	movq	8(%r14), %rax
	movl	32(%rax), %eax
	movl	32(%rbx), %ecx
	cmpq	%rcx, %rax
	jne	.LBB92_12
# BB#13:                                # %if.end23
                                        #   in Loop: Header=BB92_3 Depth=1
	movq	16(%r14), %rbx
	movq	%r14, %rdi
	callq	free_INSN_LIST_node
	movq	%rbx, %r14
	jmp	.LBB92_30
	.align	16, 0x90
.LBB92_27:                              # %for.cond56.backedge
                                        #   in Loop: Header=BB92_22 Depth=2
	movq	16(%r15), %r15
.LBB92_22:                              # %if.end49
                                        #   Parent Loop BB92_3 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	testq	%r15, %r15
	je	.LBB92_29
# BB#23:                                # %for.body58
                                        #   in Loop: Header=BB92_22 Depth=2
	movzbl	(%r15), %eax
	movzbl	rtx_class(%rax), %ecx
	cmpq	$105, %rcx
	je	.LBB92_29
# BB#24:                                # %if.end65
                                        #   in Loop: Header=BB92_22 Depth=2
	cmpq	$37, %rax
	jne	.LBB92_27
# BB#25:                                # %if.end71
                                        #   in Loop: Header=BB92_22 Depth=2
	movl	40(%r15), %eax
	movabsq	$4294967198, %rcx       # imm = 0xFFFFFF9E
	cmpq	%rcx, %rax
	je	.LBB92_28
# BB#26:                                # %if.end71
                                        #   in Loop: Header=BB92_22 Depth=2
	movabsq	$4294967199, %rcx       # imm = 0xFFFFFF9F
	cmpq	%rcx, %rax
	jne	.LBB92_27
	jmp	.LBB92_29
.LBB92_28:                              # %if.then77
                                        #   in Loop: Header=BB92_3 Depth=1
	movq	32(%rbx), %rdi
	callq	is_body_block
	testq	%r13, %rax
	jne	.LBB92_29
# BB#33:                                # %land.lhs.true
                                        #   in Loop: Header=BB92_3 Depth=1
	movq	debug_hooks(%rip), %rax
	movq	32(%rbx), %rdi
	callq	*64(%rax)
	testq	%rax, %rax
	je	.LBB92_29
# BB#34:                                # %if.then88
                                        #   in Loop: Header=BB92_3 Depth=1
	movq	%r15, %rdi
	callq	remove_insn
	movq	%rbx, %rdi
	callq	remove_insn
.LBB92_29:                              #   in Loop: Header=BB92_3 Depth=1
	movq	8(%rsp), %r15           # 8-byte Reload
	.align	16, 0x90
.LBB92_30:                              # %for.cond.backedge
                                        #   in Loop: Header=BB92_3 Depth=1
	testq	%r12, %r12
	jne	.LBB92_3
	jmp	.LBB92_31
.LBB92_1:
	xorq	%r14, %r14
.LBB92_31:                              # %for.end102
	orq	%r14, %r15
	jne	.LBB92_32
# BB#35:                                # %if.end106
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB92_15:                              # %if.then31
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.remove_unnecessary_notes, %rcx
	movq	$3729, %rsi             # imm = 0xE91
	jmp	.LBB92_10
.LBB92_18:                              # %if.then36
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.remove_unnecessary_notes, %rcx
	movq	$3736, %rsi             # imm = 0xE98
	jmp	.LBB92_10
.LBB92_20:                              # %if.then48
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.remove_unnecessary_notes, %rcx
	movq	$3739, %rsi             # imm = 0xE9B
	jmp	.LBB92_10
.LBB92_9:                               # %if.then10
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.remove_unnecessary_notes, %rcx
	movq	$3716, %rsi             # imm = 0xE84
	jmp	.LBB92_10
.LBB92_12:                              # %if.then22
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.remove_unnecessary_notes, %rcx
	movq	$3719, %rsi             # imm = 0xE87
	jmp	.LBB92_10
.LBB92_32:                              # %if.then105
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.remove_unnecessary_notes, %rcx
	movq	$3789, %rsi             # imm = 0xECD
.LBB92_10:                              # %if.then10
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp703:
	.size	remove_unnecessary_notes, .Ltmp703-remove_unnecessary_notes
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI92_0:
	.quad	.LBB92_6
	.quad	.LBB92_14
	.quad	.LBB92_17
	.quad	.LBB92_30
	.quad	.LBB92_30
	.quad	.LBB92_30
	.quad	.LBB92_30
	.quad	.LBB92_6
	.quad	.LBB92_30
	.quad	.LBB92_30
	.quad	.LBB92_30
	.quad	.LBB92_30
	.quad	.LBB92_30
	.quad	.LBB92_7
	.quad	.LBB92_8

	.text
	.globl	emit_insn_before
	.align	16, 0x90
	.type	emit_insn_before,@function
emit_insn_before:                       # @emit_insn_before
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp709:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp710:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp711:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp712:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp713:
	.cfi_def_cfa_offset 48
.Ltmp714:
	.cfi_offset %rbx, -40
.Ltmp715:
	.cfi_offset %r12, -32
.Ltmp716:
	.cfi_offset %r14, -24
.Ltmp717:
	.cfi_offset %r15, -16
	movq	%rdi, %r15
	movzbl	(%r15), %eax
	movq	%rsi, %r14
	cmpq	$24, %rax
	jne	.LBB93_5
# BB#1:                                 # %for.cond.preheader
	movq	8(%r15), %rax
	movslq	(%rax), %rcx
	testq	%rcx, %rcx
	jle	.LBB93_2
# BB#3:
	movq	$1, %rbx
	.align	16, 0x90
.LBB93_4:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	(%rax,%rbx,8), %r12
	movq	%r12, %rdi
	movq	%r14, %rsi
	callq	add_insn_before
	movq	8(%r15), %rax
	movslq	(%rax), %rdx
	movq	$32, %rcx
	movq	%rbx, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	incq	%rbx
	cmpq	%rdx, %rsi
	jl	.LBB93_4
	jmp	.LBB93_6
.LBB93_5:                               # %if.else
	movq	$32, %rdi
	callq	rtx_alloc
	movq	cfun(%rip), %rcx
	movq	24(%rcx), %rcx
	movl	40(%rcx), %edx
	movq	%rax, %r12
	leaq	1(%rdx), %rax
	movl	%eax, 40(%rcx)
	movl	%edx, 8(%r12)
	movq	%r15, 32(%r12)
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	movl	%eax, 40(%r12)
	movq	$0, 56(%r12)
	movq	$0, 48(%r12)
	movq	%r12, %rdi
	movq	%r14, %rsi
	callq	add_insn_before
	jmp	.LBB93_6
.LBB93_2:
	movq	%r14, %r12
.LBB93_6:                               # %if.end
	movq	%r12, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.Ltmp718:
	.size	emit_insn_before, .Ltmp718-emit_insn_before
	.cfi_endproc

	.globl	emit_jump_insn_before
	.align	16, 0x90
	.type	emit_jump_insn_before,@function
emit_jump_insn_before:                  # @emit_jump_insn_before
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp724:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp725:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp726:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp727:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp728:
	.cfi_def_cfa_offset 48
.Ltmp729:
	.cfi_offset %rbx, -40
.Ltmp730:
	.cfi_offset %r12, -32
.Ltmp731:
	.cfi_offset %r14, -24
.Ltmp732:
	.cfi_offset %r15, -16
	movq	%rdi, %r15
	movzbl	(%r15), %eax
	movq	%rsi, %r14
	cmpq	$24, %rax
	jne	.LBB94_5
# BB#1:                                 # %for.cond.preheader.i
	movq	8(%r15), %rax
	movslq	(%rax), %rcx
	testq	%rcx, %rcx
	jle	.LBB94_2
# BB#3:
	movq	$1, %rbx
	.align	16, 0x90
.LBB94_4:                               # %for.body.i
                                        # =>This Inner Loop Header: Depth=1
	movq	(%rax,%rbx,8), %r12
	movq	%r12, %rdi
	movq	%r14, %rsi
	callq	add_insn_before
	movq	8(%r15), %rax
	movslq	(%rax), %rdx
	movq	$32, %rcx
	movq	%rbx, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	incq	%rbx
	cmpq	%rdx, %rsi
	jl	.LBB94_4
	jmp	.LBB94_6
.LBB94_5:                               # %if.else
	movq	$33, %rdi
	callq	rtx_alloc
	movq	cfun(%rip), %rcx
	movq	24(%rcx), %rcx
	movl	40(%rcx), %edx
	movq	%rax, %r12
	leaq	1(%rdx), %rax
	movl	%eax, 40(%rcx)
	movl	%edx, 8(%r12)
	movq	%r15, 32(%r12)
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	movl	%eax, 40(%r12)
	movq	$0, 64(%r12)
	movq	$0, 56(%r12)
	movq	$0, 48(%r12)
	movq	%r12, %rdi
	movq	%r14, %rsi
	callq	add_insn_before
	jmp	.LBB94_6
.LBB94_2:
	movq	%r14, %r12
.LBB94_6:                               # %if.end
	movq	%r12, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.Ltmp733:
	.size	emit_jump_insn_before, .Ltmp733-emit_jump_insn_before
	.cfi_endproc

	.globl	emit_call_insn_before
	.align	16, 0x90
	.type	emit_call_insn_before,@function
emit_call_insn_before:                  # @emit_call_insn_before
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp739:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp740:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp741:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp742:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp743:
	.cfi_def_cfa_offset 48
.Ltmp744:
	.cfi_offset %rbx, -40
.Ltmp745:
	.cfi_offset %r12, -32
.Ltmp746:
	.cfi_offset %r14, -24
.Ltmp747:
	.cfi_offset %r15, -16
	movq	%rdi, %r15
	movzbl	(%r15), %eax
	movq	%rsi, %r14
	cmpq	$24, %rax
	jne	.LBB95_5
# BB#1:                                 # %for.cond.preheader.i
	movq	8(%r15), %rax
	movslq	(%rax), %rcx
	testq	%rcx, %rcx
	jle	.LBB95_2
# BB#3:
	movq	$1, %rbx
	.align	16, 0x90
.LBB95_4:                               # %for.body.i
                                        # =>This Inner Loop Header: Depth=1
	movq	(%rax,%rbx,8), %r12
	movq	%r12, %rdi
	movq	%r14, %rsi
	callq	add_insn_before
	movq	8(%r15), %rax
	movslq	(%rax), %rdx
	movq	$32, %rcx
	movq	%rbx, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	incq	%rbx
	cmpq	%rdx, %rsi
	jl	.LBB95_4
	jmp	.LBB95_6
.LBB95_5:                               # %if.else
	movq	$34, %rdi
	movq	$34, %rbx
	callq	rtx_alloc
	movq	cfun(%rip), %rcx
	movq	24(%rcx), %rcx
	movl	40(%rcx), %edx
	movq	%rax, %r12
	leaq	1(%rdx), %rax
	movl	%eax, 40(%rcx)
	movl	%edx, 8(%r12)
	movq	%r15, 32(%r12)
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	movl	%eax, 40(%r12)
	movq	$0, 64(%r12)
	movq	$0, 56(%r12)
	movq	$0, 48(%r12)
	movq	%r12, %rdi
	movq	%r14, %rsi
	callq	add_insn_before
	movw	%bx, (%r12)
	jmp	.LBB95_6
.LBB95_2:
	movq	%r14, %r12
.LBB95_6:                               # %if.end
	movq	%r12, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.Ltmp748:
	.size	emit_call_insn_before, .Ltmp748-emit_call_insn_before
	.cfi_endproc

	.globl	emit_barrier_before
	.align	16, 0x90
	.type	emit_barrier_before,@function
emit_barrier_before:                    # @emit_barrier_before
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp752:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp753:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp754:
	.cfi_def_cfa_offset 32
.Ltmp755:
	.cfi_offset %rbx, -24
.Ltmp756:
	.cfi_offset %r14, -16
	movq	%rdi, %r14
	movq	$35, %rdi
	callq	rtx_alloc
	movq	cfun(%rip), %rcx
	movq	24(%rcx), %rcx
	movl	40(%rcx), %edx
	movq	%rax, %rbx
	leaq	1(%rdx), %rax
	movl	%eax, 40(%rcx)
	movl	%edx, 8(%rbx)
	movq	%rbx, %rdi
	movq	%r14, %rsi
	callq	add_insn_before
	movq	%rbx, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp757:
	.size	emit_barrier_before, .Ltmp757-emit_barrier_before
	.cfi_endproc

	.globl	emit_label_before
	.align	16, 0x90
	.type	emit_label_before,@function
emit_label_before:                      # @emit_label_before
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp760:
	.cfi_def_cfa_offset 16
.Ltmp761:
	.cfi_offset %rbx, -16
	movq	%rdi, %rbx
	movl	8(%rbx), %eax
	testq	%rax, %rax
	jne	.LBB97_2
# BB#1:                                 # %if.then
	movq	cfun(%rip), %rax
	movq	24(%rax), %rax
	movl	40(%rax), %ecx
	leaq	1(%rcx), %rdx
	movl	%edx, 40(%rax)
	movl	%ecx, 8(%rbx)
	movq	%rbx, %rdi
	callq	add_insn_before
.LBB97_2:                               # %if.end
	movq	%rbx, %rax
	popq	%rbx
	retq
.Ltmp762:
	.size	emit_label_before, .Ltmp762-emit_label_before
	.cfi_endproc

	.globl	emit_note_before
	.align	16, 0x90
	.type	emit_note_before,@function
emit_note_before:                       # @emit_note_before
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp767:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp768:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp769:
	.cfi_def_cfa_offset 32
.Ltmp770:
	.cfi_offset %rbx, -32
.Ltmp771:
	.cfi_offset %r14, -24
.Ltmp772:
	.cfi_offset %r15, -16
	movq	%rsi, %r14
	movq	%rdi, %r15
	movq	$37, %rdi
	callq	rtx_alloc
	movq	cfun(%rip), %rcx
	movq	24(%rcx), %rcx
	movl	40(%rcx), %edx
	movq	%rax, %rbx
	leaq	1(%rdx), %rax
	movl	%eax, 40(%rcx)
	movl	%edx, 8(%rbx)
	movq	$0, 32(%rbx)
	movl	%r15d, 40(%rbx)
	movq	%rbx, %rdi
	movq	%r14, %rsi
	callq	add_insn_before
	movq	%rbx, %rax
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp773:
	.size	emit_note_before, .Ltmp773-emit_note_before
	.cfi_endproc

	.globl	emit_insn_after_with_line_notes
	.align	16, 0x90
	.type	emit_insn_after_with_line_notes,@function
emit_insn_after_with_line_notes:        # @emit_insn_after_with_line_notes
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp780:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp781:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp782:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp783:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp784:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp785:
	.cfi_def_cfa_offset 64
.Ltmp786:
	.cfi_offset %rbx, -48
.Ltmp787:
	.cfi_offset %r12, -40
.Ltmp788:
	.cfi_offset %r13, -32
.Ltmp789:
	.cfi_offset %r14, -24
.Ltmp790:
	.cfi_offset %r15, -16
	movl	no_line_numbers(%rip), %eax
	movq	%rsi, %r14
	movq	%rdi, %r15
	xorq	%rsi, %rsi
	testq	%rax, %rax
	movabsq	$0, %r13
	jne	.LBB99_7
# BB#1:                                 # %entry
	testq	%rdx, %rdx
	movabsq	$0, %r13
	je	.LBB99_7
	.align	16, 0x90
.LBB99_2:                               # %for.body.i
                                        # =>This Inner Loop Header: Depth=1
	movzbl	(%rdx), %ecx
	cmpq	$37, %rcx
	jne	.LBB99_5
# BB#3:                                 # %land.lhs.true.i
                                        #   in Loop: Header=BB99_2 Depth=1
	movslq	40(%rdx), %rcx
	testq	%rcx, %rcx
	jns	.LBB99_4
.LBB99_5:                               # %for.inc.i
                                        #   in Loop: Header=BB99_2 Depth=1
	movq	16(%rdx), %rdx
	testq	%rdx, %rdx
	jne	.LBB99_2
# BB#6:
	xorq	%r13, %r13
	jmp	.LBB99_7
.LBB99_4:
	movq	%rdx, %r13
.LBB99_7:                               # %find_line_note.exit
	testq	%rax, %rax
	jne	.LBB99_14
# BB#8:                                 # %find_line_note.exit
	testq	%r14, %r14
	je	.LBB99_14
# BB#9:
	movq	%r14, %rax
	.align	16, 0x90
.LBB99_10:                              # %for.body.i34
                                        # =>This Inner Loop Header: Depth=1
	movzbl	(%rax), %ecx
	cmpq	$37, %rcx
	jne	.LBB99_13
# BB#11:                                # %land.lhs.true.i38
                                        #   in Loop: Header=BB99_10 Depth=1
	movslq	40(%rax), %rcx
	testq	%rcx, %rcx
	jns	.LBB99_12
.LBB99_13:                              # %for.inc.i42
                                        #   in Loop: Header=BB99_10 Depth=1
	movq	16(%rax), %rax
	testq	%rax, %rax
	jne	.LBB99_10
	jmp	.LBB99_14
.LBB99_12:
	movq	%rax, %rsi
.LBB99_14:                              # %find_line_note.exit44
	movq	%rsi, 8(%rsp)           # 8-byte Spill
	movzbl	(%r15), %eax
	cmpq	$24, %rax
	jne	.LBB99_18
# BB#15:                                # %for.cond.preheader.i
	movq	8(%r15), %rax
	movslq	(%rax), %rcx
	testq	%rcx, %rcx
	movq	%r14, %rbx
	jle	.LBB99_19
# BB#16:
	movq	$1, %r12
	movq	%r14, %rsi
	.align	16, 0x90
.LBB99_17:                              # %for.body.i49
                                        # =>This Inner Loop Header: Depth=1
	movq	(%rax,%r12,8), %rbx
	movq	%rbx, %rdi
	callq	add_insn_after
	movq	8(%r15), %rax
	movslq	(%rax), %rdx
	movq	$32, %rcx
	movq	%r12, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	incq	%r12
	cmpq	%rdx, %rsi
	movq	%rbx, %rsi
	jl	.LBB99_17
	jmp	.LBB99_19
.LBB99_18:                              # %if.else.i
	movq	$32, %rdi
	callq	rtx_alloc
	movq	cfun(%rip), %rcx
	movq	24(%rcx), %rcx
	movl	40(%rcx), %edx
	movq	%rax, %rbx
	leaq	1(%rdx), %rax
	movl	%eax, 40(%rcx)
	movl	%edx, 8(%rbx)
	movq	%r15, 32(%rbx)
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	movl	%eax, 40(%rbx)
	movq	$0, 56(%rbx)
	movq	$0, 48(%rbx)
	movq	%rbx, %rdi
	movq	%r14, %rsi
	callq	add_insn_after
.LBB99_19:                              # %emit_insn_after.exit
	testq	%r13, %r13
	je	.LBB99_24
# BB#20:                                # %if.then
	movl	no_line_numbers(%rip), %eax
	movl	40(%r13), %r15d
	testq	%rax, %rax
	je	.LBB99_23
# BB#21:                                # %if.then
	movq	$32, %rcx
	movq	%r15, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jle	.LBB99_23
# BB#22:                                # %if.then.i57
	movq	cfun(%rip), %rax
	movq	24(%rax), %rax
	movl	40(%rax), %ecx
	incq	%rcx
	movl	%ecx, 40(%rax)
	jmp	.LBB99_24
.LBB99_23:                              # %if.end.i67
	movq	32(%r13), %r12
	movq	$37, %rdi
	callq	rtx_alloc
	movq	cfun(%rip), %rcx
	movq	24(%rcx), %rcx
	movl	40(%rcx), %edx
	leaq	1(%rdx), %rsi
	movl	%esi, 40(%rcx)
	movl	%edx, 8(%rax)
	movq	%r12, 32(%rax)
	movl	%r15d, 40(%rax)
	movq	%rax, %rdi
	movq	%r14, %rsi
	callq	add_insn_after
.LBB99_24:                              # %if.end
	movq	8(%rsp), %rdx           # 8-byte Reload
	testq	%rdx, %rdx
	je	.LBB99_28
# BB#25:                                # %if.then7
	movl	no_line_numbers(%rip), %eax
	movl	40(%rdx), %r14d
	testq	%rax, %rax
	je	.LBB99_29
# BB#26:                                # %if.then7
	movq	$32, %rcx
	movq	%r14, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jle	.LBB99_29
# BB#27:                                # %if.then.i
	movq	cfun(%rip), %rax
	movq	24(%rax), %rax
	movl	40(%rax), %ecx
	incq	%rcx
	movl	%ecx, 40(%rax)
.LBB99_28:                              # %if.end15
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB99_29:                              # %if.end.i
	movq	32(%rdx), %r15
	movq	$37, %rdi
	callq	rtx_alloc
	movq	cfun(%rip), %rcx
	movq	24(%rcx), %rcx
	movl	40(%rcx), %edx
	leaq	1(%rdx), %rsi
	movl	%esi, 40(%rcx)
	movl	%edx, 8(%rax)
	movq	%r15, 32(%rax)
	movl	%r14d, 40(%rax)
	movq	%rax, %rdi
	movq	%rbx, %rsi
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	add_insn_after  # TAILCALL
.Ltmp791:
	.size	emit_insn_after_with_line_notes, .Ltmp791-emit_insn_after_with_line_notes
	.cfi_endproc

	.globl	emit_jump_insn_after
	.align	16, 0x90
	.type	emit_jump_insn_after,@function
emit_jump_insn_after:                   # @emit_jump_insn_after
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp797:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp798:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp799:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp800:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp801:
	.cfi_def_cfa_offset 48
.Ltmp802:
	.cfi_offset %rbx, -40
.Ltmp803:
	.cfi_offset %r12, -32
.Ltmp804:
	.cfi_offset %r14, -24
.Ltmp805:
	.cfi_offset %r15, -16
	movq	%rdi, %r14
	movzbl	(%r14), %eax
	movq	%rsi, %r15
	cmpq	$24, %rax
	jne	.LBB100_5
# BB#1:                                 # %for.cond.preheader.i
	movq	8(%r14), %rax
	movslq	(%rax), %rcx
	testq	%rcx, %rcx
	jle	.LBB100_2
# BB#3:
	movq	$1, %rbx
	.align	16, 0x90
.LBB100_4:                              # %for.body.i
                                        # =>This Inner Loop Header: Depth=1
	movq	(%rax,%rbx,8), %r12
	movq	%r12, %rdi
	movq	%r15, %rsi
	callq	add_insn_after
	movq	8(%r14), %rax
	movslq	(%rax), %rdx
	movq	$32, %rcx
	movq	%rbx, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	incq	%rbx
	cmpq	%rdx, %rsi
	movq	%r12, %r15
	jl	.LBB100_4
	jmp	.LBB100_6
.LBB100_5:                              # %if.else
	movq	$33, %rdi
	callq	rtx_alloc
	movq	cfun(%rip), %rcx
	movq	24(%rcx), %rcx
	movl	40(%rcx), %edx
	movq	%rax, %r12
	leaq	1(%rdx), %rax
	movl	%eax, 40(%rcx)
	movl	%edx, 8(%r12)
	movq	%r14, 32(%r12)
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	movl	%eax, 40(%r12)
	movq	$0, 64(%r12)
	movq	$0, 56(%r12)
	movq	$0, 48(%r12)
	movq	%r12, %rdi
	movq	%r15, %rsi
	callq	add_insn_after
	jmp	.LBB100_6
.LBB100_2:
	movq	%r15, %r12
.LBB100_6:                              # %if.end
	movq	%r12, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.Ltmp806:
	.size	emit_jump_insn_after, .Ltmp806-emit_jump_insn_after
	.cfi_endproc

	.globl	emit_label_after
	.align	16, 0x90
	.type	emit_label_after,@function
emit_label_after:                       # @emit_label_after
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp809:
	.cfi_def_cfa_offset 16
.Ltmp810:
	.cfi_offset %rbx, -16
	movq	%rdi, %rbx
	movl	8(%rbx), %eax
	testq	%rax, %rax
	jne	.LBB101_2
# BB#1:                                 # %if.then
	movq	cfun(%rip), %rax
	movq	24(%rax), %rax
	movl	40(%rax), %ecx
	leaq	1(%rcx), %rdx
	movl	%edx, 40(%rax)
	movl	%ecx, 8(%rbx)
	movq	%rbx, %rdi
	callq	add_insn_after
.LBB101_2:                              # %if.end
	movq	%rbx, %rax
	popq	%rbx
	retq
.Ltmp811:
	.size	emit_label_after, .Ltmp811-emit_label_after
	.cfi_endproc

	.globl	emit_note_after
	.align	16, 0x90
	.type	emit_note_after,@function
emit_note_after:                        # @emit_note_after
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp816:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp817:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp818:
	.cfi_def_cfa_offset 32
.Ltmp819:
	.cfi_offset %rbx, -32
.Ltmp820:
	.cfi_offset %r14, -24
.Ltmp821:
	.cfi_offset %r15, -16
	movq	%rsi, %r14
	movq	%rdi, %r15
	movq	$37, %rdi
	callq	rtx_alloc
	movq	cfun(%rip), %rcx
	movq	24(%rcx), %rcx
	movl	40(%rcx), %edx
	movq	%rax, %rbx
	leaq	1(%rdx), %rax
	movl	%eax, 40(%rcx)
	movl	%edx, 8(%rbx)
	movq	$0, 32(%rbx)
	movl	%r15d, 40(%rbx)
	movq	%rbx, %rdi
	movq	%r14, %rsi
	callq	add_insn_after
	movq	%rbx, %rax
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp822:
	.size	emit_note_after, .Ltmp822-emit_note_after
	.cfi_endproc

	.globl	emit_insn
	.align	16, 0x90
	.type	emit_insn,@function
emit_insn:                              # @emit_insn
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp826:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp827:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp828:
	.cfi_def_cfa_offset 32
.Ltmp829:
	.cfi_offset %rbx, -24
.Ltmp830:
	.cfi_offset %r14, -16
	movq	%rdi, %r14
	movzbl	(%r14), %eax
	cmpq	$24, %rax
	jne	.LBB103_10
# BB#1:                                 # %if.then
	movq	cfun(%rip), %rax
	movq	8(%r14), %rsi
	movq	24(%rax), %rax
	movslq	(%rsi), %rdx
	movq	16(%rax), %rcx
	testq	%rdx, %rdx
	jle	.LBB103_2
# BB#3:
	movq	$1, %rbx
	jmp	.LBB103_4
	.align	16, 0x90
.LBB103_9:                              # %add_insn.exit.for.body_crit_edge
                                        #   in Loop: Header=BB103_4 Depth=1
	movq	cfun(%rip), %rax
	movq	24(%rax), %rax
	movq	16(%rax), %rcx
	incq	%rbx
.LBB103_4:                              # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	(%rsi,%rbx,8), %rax
	movq	%rcx, 16(%rax)
	movq	$0, 24(%rax)
	movq	cfun(%rip), %rcx
	movq	24(%rcx), %rcx
	movq	16(%rcx), %rsi
	testq	%rsi, %rsi
	je	.LBB103_6
# BB#5:                                 # %if.then.i
                                        #   in Loop: Header=BB103_4 Depth=1
	movq	%rax, 24(%rsi)
	movq	cfun(%rip), %rcx
	movq	24(%rcx), %rcx
.LBB103_6:                              # %if.end.i
                                        #   in Loop: Header=BB103_4 Depth=1
	cmpq	$0, 8(%rcx)
	jne	.LBB103_8
# BB#7:                                 # %if.then13.i
                                        #   in Loop: Header=BB103_4 Depth=1
	movq	%rax, 8(%rcx)
	movq	cfun(%rip), %rcx
	movq	24(%rcx), %rcx
.LBB103_8:                              # %add_insn.exit
                                        #   in Loop: Header=BB103_4 Depth=1
	movq	%rax, 16(%rcx)
	movq	8(%r14), %rsi
	movslq	(%rsi), %rdi
	movq	$32, %rcx
	movq	%rbx, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rdi, %rdx
	jl	.LBB103_9
	jmp	.LBB103_15
.LBB103_10:                             # %if.else
	movq	$32, %rdi
	callq	rtx_alloc
	movq	cfun(%rip), %rcx
	movq	24(%rcx), %rcx
	movl	40(%rcx), %edx
	leaq	1(%rdx), %rsi
	movl	%esi, 40(%rcx)
	movl	%edx, 8(%rax)
	movq	%r14, 32(%rax)
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	movl	%ecx, 40(%rax)
	movq	$0, 56(%rax)
	movq	$0, 48(%rax)
	movq	cfun(%rip), %rcx
	movq	24(%rcx), %rcx
	movq	16(%rcx), %rcx
	movq	%rcx, 16(%rax)
	movq	$0, 24(%rax)
	movq	cfun(%rip), %rcx
	movq	24(%rcx), %rcx
	movq	16(%rcx), %rdx
	testq	%rdx, %rdx
	je	.LBB103_12
# BB#11:                                # %if.then.i23
	movq	%rax, 24(%rdx)
	movq	cfun(%rip), %rcx
	movq	24(%rcx), %rcx
.LBB103_12:                             # %if.end.i26
	cmpq	$0, 8(%rcx)
	jne	.LBB103_14
# BB#13:                                # %if.then13.i30
	movq	%rax, 8(%rcx)
	movq	cfun(%rip), %rcx
	movq	24(%rcx), %rcx
.LBB103_14:                             # %add_insn.exit32
	movq	%rax, 16(%rcx)
	jmp	.LBB103_15
.LBB103_2:
	movq	%rcx, %rax
.LBB103_15:                             # %if.end
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp831:
	.size	emit_insn, .Ltmp831-emit_insn
	.cfi_endproc

	.globl	emit_insns
	.align	16, 0x90
	.type	emit_insns,@function
emit_insns:                             # @emit_insns
	.cfi_startproc
# BB#0:                                 # %entry
	xorq	%rax, %rax
	jmp	.LBB104_1
	.align	16, 0x90
.LBB104_6:                              # %add_insn.exit
                                        #   in Loop: Header=BB104_1 Depth=1
	movq	%rax, 16(%rcx)
.LBB104_1:                              # %entry
                                        # =>This Inner Loop Header: Depth=1
	testq	%rdi, %rdi
	je	.LBB104_7
# BB#2:                                 # %while.body
                                        #   in Loop: Header=BB104_1 Depth=1
	movq	cfun(%rip), %rax
	movq	24(%rax), %rax
	movq	16(%rax), %rcx
	movq	%rdi, %rax
	movq	24(%rax), %rdi
	movq	%rcx, 16(%rax)
	movq	$0, 24(%rax)
	movq	cfun(%rip), %rcx
	movq	24(%rcx), %rcx
	movq	16(%rcx), %rdx
	testq	%rdx, %rdx
	je	.LBB104_4
# BB#3:                                 # %if.then.i
                                        #   in Loop: Header=BB104_1 Depth=1
	movq	%rax, 24(%rdx)
	movq	cfun(%rip), %rcx
	movq	24(%rcx), %rcx
.LBB104_4:                              # %if.end.i
                                        #   in Loop: Header=BB104_1 Depth=1
	cmpq	$0, 8(%rcx)
	jne	.LBB104_6
# BB#5:                                 # %if.then13.i
                                        #   in Loop: Header=BB104_1 Depth=1
	movq	%rax, 8(%rcx)
	movq	cfun(%rip), %rcx
	movq	24(%rcx), %rcx
	jmp	.LBB104_6
.LBB104_7:                              # %while.end
	retq
.Ltmp832:
	.size	emit_insns, .Ltmp832-emit_insns
	.cfi_endproc

	.globl	emit_insns_before
	.align	16, 0x90
	.type	emit_insns_before,@function
emit_insns_before:                      # @emit_insns_before
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp837:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp838:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp839:
	.cfi_def_cfa_offset 32
.Ltmp840:
	.cfi_offset %rbx, -32
.Ltmp841:
	.cfi_offset %r14, -24
.Ltmp842:
	.cfi_offset %r15, -16
	movq	%rsi, %r14
	movq	%rdi, %r15
	xorq	%rbx, %rbx
	jmp	.LBB105_2
	.align	16, 0x90
.LBB105_1:                              # %while.body
                                        #   in Loop: Header=BB105_2 Depth=1
	movq	%r15, %rbx
	movq	24(%rbx), %r15
	movq	%rbx, %rdi
	movq	%r14, %rsi
	callq	add_insn_before
.LBB105_2:                              # %while.body
                                        # =>This Inner Loop Header: Depth=1
	testq	%r15, %r15
	jne	.LBB105_1
# BB#3:                                 # %while.end
	movq	%rbx, %rax
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp843:
	.size	emit_insns_before, .Ltmp843-emit_insns_before
	.cfi_endproc

	.globl	emit_insns_after
	.align	16, 0x90
	.type	emit_insns_after,@function
emit_insns_after:                       # @emit_insns_after
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp850:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp851:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp852:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp853:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp854:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp855:
	.cfi_def_cfa_offset 64
.Ltmp856:
	.cfi_offset %rbx, -48
.Ltmp857:
	.cfi_offset %r12, -40
.Ltmp858:
	.cfi_offset %r13, -32
.Ltmp859:
	.cfi_offset %r14, -24
.Ltmp860:
	.cfi_offset %r15, -16
	movq	%rdi, %r15
	testq	%rsi, %rsi
	je	.LBB106_17
# BB#1:                                 # %if.end
	testq	%r15, %r15
	je	.LBB106_2
# BB#3:                                 # %if.end3
	movq	basic_block_for_insn(%rip), %rax
	testq	%rax, %rax
	movq	%r15, %rcx
	je	.LBB106_11
# BB#4:                                 # %land.lhs.true
	movl	8(%rsi), %ebx
	movq	$32, %rcx
	movq	%rbx, %rdx
	movq	(%rax), %rdi
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rdi, %rbx
	movq	%r15, %rcx
	jae	.LBB106_11
# BB#5:                                 # %land.lhs.true6
	movq	32(%rax,%rdx,8), %r12
	testq	%r12, %r12
	movq	%r15, %rcx
	je	.LBB106_11
# BB#6:                                 # %for.cond.preheader
	movq	%rsi, 8(%rsp)           # 8-byte Spill
	movq	24(%r15), %rbx
	movq	%r15, %rdi
	movq	%r12, %rsi
	callq	set_block_for_insn
	cmpq	$0, %rbx
	movq	%r15, %rbx
	je	.LBB106_9
# BB#7:
	leaq	24(%r15), %r13
	.align	16, 0x90
.LBB106_8:                              # %for.inc
                                        # =>This Inner Loop Header: Depth=1
	movq	(%r13), %rbx
	movq	24(%rbx), %r14
	leaq	24(%rbx), %r13
	movq	%rbx, %rdi
	movq	%r12, %rsi
	callq	set_block_for_insn
	cmpq	$0, %r14
	jne	.LBB106_8
.LBB106_9:                              # %for.end
	movq	8(%r12), %rax
	movq	8(%rsp), %rsi           # 8-byte Reload
	cmpq	%rsi, %rax
	jne	.LBB106_12
# BB#10:                                # %if.then22
	movq	%rbx, 8(%r12)
	jmp	.LBB106_12
	.align	16, 0x90
.LBB106_11:                             # %for.cond25
                                        # =>This Inner Loop Header: Depth=1
	movq	%rcx, %rbx
	movq	24(%rbx), %rcx
	testq	%rcx, %rcx
	jne	.LBB106_11
.LBB106_12:                             # %if.end36
	movq	24(%rsi), %rax
	testq	%rax, %rax
	movq	%r15, 24(%rsi)
	movq	%rsi, 16(%r15)
	movq	%rax, 24(%rbx)
	je	.LBB106_14
# BB#13:                                # %if.then50
	movq	%rbx, 16(%rax)
.LBB106_14:                             # %if.end54
	movq	cfun(%rip), %rax
	movq	24(%rax), %rax
	movq	16(%rax), %rcx
	cmpq	%rsi, %rcx
	jne	.LBB106_16
# BB#15:                                # %if.then57
	movq	%rbx, 16(%rax)
	jmp	.LBB106_16
.LBB106_2:
	movq	%rsi, %rbx
.LBB106_16:                             # %return
	movq	%rbx, %rax
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB106_17:                             # %if.then
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.emit_insns_after, %rcx
	movq	$4142, %rsi             # imm = 0x102E
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp861:
	.size	emit_insns_after, .Ltmp861-emit_insns_after
	.cfi_endproc

	.globl	emit_jump_insn
	.align	16, 0x90
	.type	emit_jump_insn,@function
emit_jump_insn:                         # @emit_jump_insn
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp864:
	.cfi_def_cfa_offset 16
.Ltmp865:
	.cfi_offset %rbx, -16
	movq	%rdi, %rbx
	movzbl	(%rbx), %eax
	cmpq	$24, %rax
	jne	.LBB107_1
# BB#6:                                 # %if.then
	movq	%rbx, %rdi
	popq	%rbx
	jmp	emit_insn  # TAILCALL
.LBB107_1:                              # %if.else
	movq	$33, %rdi
	callq	rtx_alloc
	movq	cfun(%rip), %rcx
	movq	24(%rcx), %rcx
	movl	40(%rcx), %edx
	leaq	1(%rdx), %rsi
	movl	%esi, 40(%rcx)
	movl	%edx, 8(%rax)
	movq	%rbx, 32(%rax)
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	movl	%ecx, 40(%rax)
	movq	$0, 64(%rax)
	movq	$0, 56(%rax)
	movq	$0, 48(%rax)
	movq	cfun(%rip), %rcx
	movq	24(%rcx), %rcx
	movq	16(%rcx), %rcx
	movq	%rcx, 16(%rax)
	movq	$0, 24(%rax)
	movq	cfun(%rip), %rcx
	movq	24(%rcx), %rcx
	movq	16(%rcx), %rdx
	testq	%rdx, %rdx
	je	.LBB107_3
# BB#2:                                 # %if.then.i
	movq	%rax, 24(%rdx)
	movq	cfun(%rip), %rcx
	movq	24(%rcx), %rcx
.LBB107_3:                              # %if.end.i
	cmpq	$0, 8(%rcx)
	jne	.LBB107_5
# BB#4:                                 # %if.then13.i
	movq	%rax, 8(%rcx)
	movq	cfun(%rip), %rcx
	movq	24(%rcx), %rcx
.LBB107_5:                              # %return
	movq	%rax, 16(%rcx)
	popq	%rbx
	retq
.Ltmp866:
	.size	emit_jump_insn, .Ltmp866-emit_jump_insn
	.cfi_endproc

	.globl	emit_call_insn
	.align	16, 0x90
	.type	emit_call_insn,@function
emit_call_insn:                         # @emit_call_insn
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp869:
	.cfi_def_cfa_offset 16
.Ltmp870:
	.cfi_offset %rbx, -16
	movq	%rdi, %rbx
	movzbl	(%rbx), %eax
	cmpq	$24, %rax
	jne	.LBB108_1
# BB#6:                                 # %if.then
	movq	%rbx, %rdi
	popq	%rbx
	jmp	emit_insn  # TAILCALL
.LBB108_1:                              # %if.else
	movq	$34, %rdi
	callq	rtx_alloc
	movq	cfun(%rip), %rcx
	movq	24(%rcx), %rcx
	movl	40(%rcx), %edx
	leaq	1(%rdx), %rsi
	movl	%esi, 40(%rcx)
	movl	%edx, 8(%rax)
	movq	%rbx, 32(%rax)
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	movl	%ecx, 40(%rax)
	movq	$0, 64(%rax)
	movq	$0, 56(%rax)
	movq	$0, 48(%rax)
	movq	cfun(%rip), %rcx
	movq	24(%rcx), %rcx
	movq	16(%rcx), %rcx
	movq	%rcx, 16(%rax)
	movq	$0, 24(%rax)
	movq	cfun(%rip), %rcx
	movq	24(%rcx), %rcx
	movq	16(%rcx), %rdx
	testq	%rdx, %rdx
	je	.LBB108_3
# BB#2:                                 # %if.then.i
	movq	%rax, 24(%rdx)
	movq	cfun(%rip), %rcx
	movq	24(%rcx), %rcx
.LBB108_3:                              # %if.end.i
	cmpq	$0, 8(%rcx)
	jne	.LBB108_5
# BB#4:                                 # %if.then13.i
	movq	%rax, 8(%rcx)
	movq	cfun(%rip), %rcx
	movq	24(%rcx), %rcx
.LBB108_5:                              # %return
	movq	%rax, 16(%rcx)
	movq	$34, %rcx
	movw	%cx, (%rax)
	popq	%rbx
	retq
.Ltmp871:
	.size	emit_call_insn, .Ltmp871-emit_call_insn
	.cfi_endproc

	.globl	emit_label
	.align	16, 0x90
	.type	emit_label,@function
emit_label:                             # @emit_label
	.cfi_startproc
# BB#0:                                 # %entry
	movl	8(%rdi), %eax
	testq	%rax, %rax
	jne	.LBB109_6
# BB#1:                                 # %if.then
	movq	cfun(%rip), %rax
	movq	24(%rax), %rax
	movl	40(%rax), %ecx
	leaq	1(%rcx), %rdx
	movl	%edx, 40(%rax)
	movl	%ecx, 8(%rdi)
	movq	16(%rax), %rax
	movq	%rax, 16(%rdi)
	movq	$0, 24(%rdi)
	movq	cfun(%rip), %rax
	movq	24(%rax), %rax
	movq	16(%rax), %rcx
	testq	%rcx, %rcx
	je	.LBB109_3
# BB#2:                                 # %if.then.i
	movq	%rdi, 24(%rcx)
	movq	cfun(%rip), %rax
	movq	24(%rax), %rax
.LBB109_3:                              # %if.end.i
	cmpq	$0, 8(%rax)
	jne	.LBB109_5
# BB#4:                                 # %if.then13.i
	movq	%rdi, 8(%rax)
	movq	cfun(%rip), %rax
	movq	24(%rax), %rax
.LBB109_5:                              # %add_insn.exit
	movq	%rdi, 16(%rax)
.LBB109_6:                              # %if.end
	movq	%rdi, %rax
	retq
.Ltmp872:
	.size	emit_label, .Ltmp872-emit_label
	.cfi_endproc

	.globl	emit_barrier
	.align	16, 0x90
	.type	emit_barrier,@function
emit_barrier:                           # @emit_barrier
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rax
.Ltmp874:
	.cfi_def_cfa_offset 16
	movq	$35, %rdi
	callq	rtx_alloc
	movq	cfun(%rip), %rcx
	movq	24(%rcx), %rcx
	movl	40(%rcx), %edx
	leaq	1(%rdx), %rsi
	movl	%esi, 40(%rcx)
	movl	%edx, 8(%rax)
	movq	16(%rcx), %rcx
	movq	%rcx, 16(%rax)
	movq	$0, 24(%rax)
	movq	cfun(%rip), %rcx
	movq	24(%rcx), %rcx
	movq	16(%rcx), %rdx
	testq	%rdx, %rdx
	je	.LBB110_2
# BB#1:                                 # %if.then.i
	movq	%rax, 24(%rdx)
	movq	cfun(%rip), %rcx
	movq	24(%rcx), %rcx
.LBB110_2:                              # %if.end.i
	cmpq	$0, 8(%rcx)
	jne	.LBB110_4
# BB#3:                                 # %if.then13.i
	movq	%rax, 8(%rcx)
	movq	cfun(%rip), %rcx
	movq	24(%rcx), %rcx
.LBB110_4:                              # %add_insn.exit
	movq	%rax, 16(%rcx)
	popq	%rdx
	retq
.Ltmp875:
	.size	emit_barrier, .Ltmp875-emit_barrier
	.cfi_endproc

	.globl	emit_line_note
	.align	16, 0x90
	.type	emit_line_note,@function
emit_line_note:                         # @emit_line_note
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp879:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp880:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp881:
	.cfi_def_cfa_offset 32
.Ltmp882:
	.cfi_offset %rbx, -24
.Ltmp883:
	.cfi_offset %r14, -16
	movq	%rsi, %r14
	movq	%rdi, %rbx
                                        # kill: RDI<def> RBX<kill>
                                        # kill: RSI<def> R14<kill>
	callq	set_file_and_line_for_stmt
	movq	%rbx, %rdi
	movq	%r14, %rsi
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	jmp	emit_note  # TAILCALL
.Ltmp884:
	.size	emit_line_note, .Ltmp884-emit_line_note
	.cfi_endproc

	.globl	emit_note
	.align	16, 0x90
	.type	emit_note,@function
emit_note:                              # @emit_note
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp889:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp890:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp891:
	.cfi_def_cfa_offset 32
.Ltmp892:
	.cfi_offset %rbx, -32
.Ltmp893:
	.cfi_offset %r14, -24
.Ltmp894:
	.cfi_offset %r15, -16
	movq	%rsi, %rbx
	movq	%rdi, %r14
	movq	$32, %rcx
	movq	%rbx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jle	.LBB112_7
# BB#1:                                 # %if.then
	movq	cfun(%rip), %rax
	movq	24(%rax), %r15
	testq	%r14, %r14
	je	.LBB112_5
# BB#2:                                 # %land.lhs.true
	movq	48(%r15), %rsi
	testq	%rsi, %rsi
	je	.LBB112_5
# BB#3:                                 # %land.lhs.true2
	movq	%r14, %rdi
	callq	strcmp
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB112_5
# BB#4:                                 # %land.lhs.true6
	movl	44(%r15), %edx
	andq	%rbx, %rcx
	xorq	%rax, %rax
	cmpq	%rcx, %rdx
	je	.LBB112_12
.LBB112_5:                              # %if.end14
	movq	%r14, 48(%r15)
	movq	cfun(%rip), %rax
	movq	24(%rax), %rax
	movl	%ebx, 44(%rax)
	movl	no_line_numbers(%rip), %ecx
	testq	%rcx, %rcx
	je	.LBB112_7
# BB#6:                                 # %if.then18
	movl	40(%rax), %ecx
	incq	%rcx
	movl	%ecx, 40(%rax)
	xorq	%rax, %rax
	jmp	.LBB112_12
.LBB112_7:                              # %if.end20
	movq	$37, %rdi
	callq	rtx_alloc
	movq	cfun(%rip), %rcx
	movq	24(%rcx), %rcx
	movl	40(%rcx), %edx
	leaq	1(%rdx), %rsi
	movl	%esi, 40(%rcx)
	movl	%edx, 8(%rax)
	movq	%r14, 32(%rax)
	movl	%ebx, 40(%rax)
	movq	cfun(%rip), %rcx
	movq	24(%rcx), %rcx
	movq	16(%rcx), %rcx
	movq	%rcx, 16(%rax)
	movq	$0, 24(%rax)
	movq	cfun(%rip), %rcx
	movq	24(%rcx), %rcx
	movq	16(%rcx), %rdx
	testq	%rdx, %rdx
	je	.LBB112_9
# BB#8:                                 # %if.then.i
	movq	%rax, 24(%rdx)
	movq	cfun(%rip), %rcx
	movq	24(%rcx), %rcx
.LBB112_9:                              # %if.end.i
	cmpq	$0, 8(%rcx)
	jne	.LBB112_11
# BB#10:                                # %if.then13.i
	movq	%rax, 8(%rcx)
	movq	cfun(%rip), %rcx
	movq	24(%rcx), %rcx
.LBB112_11:                             # %add_insn.exit
	movq	%rax, 16(%rcx)
.LBB112_12:                             # %return
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp895:
	.size	emit_note, .Ltmp895-emit_note
	.cfi_endproc

	.globl	emit_line_note_force
	.align	16, 0x90
	.type	emit_line_note_force,@function
emit_line_note_force:                   # @emit_line_note_force
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp899:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp900:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp901:
	.cfi_def_cfa_offset 32
.Ltmp902:
	.cfi_offset %rbx, -24
.Ltmp903:
	.cfi_offset %r14, -16
	movq	cfun(%rip), %rax
	movq	24(%rax), %rax
	movq	%rsi, %r14
	movq	%rdi, %rbx
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	movl	%ecx, 44(%rax)
                                        # kill: RDI<def> RBX<kill>
                                        # kill: RSI<def> R14<kill>
	callq	set_file_and_line_for_stmt
	movq	%rbx, %rdi
	movq	%r14, %rsi
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	jmp	emit_note  # TAILCALL
.Ltmp904:
	.size	emit_line_note_force, .Ltmp904-emit_line_note_force
	.cfi_endproc

	.globl	force_next_line_note
	.align	16, 0x90
	.type	force_next_line_note,@function
force_next_line_note:                   # @force_next_line_note
	.cfi_startproc
# BB#0:                                 # %entry
	movq	cfun(%rip), %rax
	movq	24(%rax), %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	movl	%ecx, 44(%rax)
	retq
.Ltmp905:
	.size	force_next_line_note, .Ltmp905-force_next_line_note
	.cfi_endproc

	.globl	set_unique_reg_note
	.align	16, 0x90
	.type	set_unique_reg_note,@function
set_unique_reg_note:                    # @set_unique_reg_note
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp912:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp913:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp914:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp915:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp916:
	.cfi_def_cfa_offset 48
.Ltmp917:
	.cfi_offset %rbx, -48
.Ltmp918:
	.cfi_offset %r12, -40
.Ltmp919:
	.cfi_offset %r13, -32
.Ltmp920:
	.cfi_offset %r14, -24
.Ltmp921:
	.cfi_offset %r15, -16
	movq	%rdx, %r14
	movq	%rsi, %rbx
	movq	%rdi, %r15
	xorq	%rdx, %rdx
                                        # kill: RDI<def> R15<kill>
                                        # kill: RSI<def> RBX<kill>
	callq	find_reg_note
	movq	%rax, %r12
	leaq	-3(%rbx), %rax
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	andq	%r13, %rax
	cmpq	$1, %rax
	ja	.LBB115_6
# BB#1:                                 # %sw.bb
	movq	32(%r15), %rax
	movzbl	(%rax), %eax
	cmpq	$39, %rax
	jne	.LBB115_5
# BB#2:                                 # %land.lhs.true
	movq	%r15, %rdi
	callq	multiple_sets
	testq	%r13, %rax
	je	.LBB115_5
# BB#3:                                 # %if.then
	xorq	%rax, %rax
	testq	%r12, %r12
	je	.LBB115_9
# BB#4:                                 # %if.then3
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.set_unique_reg_note, %rcx
	movq	$4335, %rsi             # imm = 0x10EF
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.LBB115_5:                              # %if.end4
	movzbl	(%r14), %ecx
	xorq	%rax, %rax
	cmpq	$41, %rcx
	je	.LBB115_9
.LBB115_6:                              # %sw.epilog
	testq	%r12, %r12
	je	.LBB115_8
# BB#7:                                 # %if.then11
	movq	%r14, 8(%r12)
	movq	%r12, %rax
	jmp	.LBB115_9
.LBB115_8:                              # %if.end15
	movq	56(%r15), %rcx
	movq	$3, %rdi
	movq	%rbx, %rsi
	movq	%r14, %rdx
	callq	gen_rtx_fmt_ee
	movq	%rax, 56(%r15)
.LBB115_9:                              # %return
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp922:
	.size	set_unique_reg_note, .Ltmp922-set_unique_reg_note
	.cfi_endproc

	.globl	classify_insn
	.align	16, 0x90
	.type	classify_insn,@function
classify_insn:                          # @classify_insn
	.cfi_startproc
# BB#0:                                 # %entry
	movzbl	(%rdi), %ecx
	cmpq	$46, %rcx
	jg	.LBB116_11
# BB#1:                                 # %entry
	movq	$36, %rax
	cmpq	$36, %rcx
	je	.LBB116_21
# BB#2:                                 # %entry
	cmpq	$39, %rcx
	jne	.LBB116_10
# BB#3:                                 # %if.then29
	movq	8(%rdi), %rdx
	movslq	(%rdx), %rsi
	testq	%rsi, %rsi
	jle	.LBB116_10
# BB#4:                                 # %for.body.lr.ph
	movq	global_rtl(%rip), %r8
	decq	%rsi
	.align	16, 0x90
.LBB116_5:                              # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	8(%rdx,%rsi,8), %rcx
	movzbl	(%rcx), %edi
	cmpq	$50, %rdi
	movq	$34, %rax
	je	.LBB116_21
# BB#6:                                 # %for.body
                                        #   in Loop: Header=BB116_5 Depth=1
	cmpq	$47, %rdi
	jne	.LBB116_9
# BB#7:                                 # %land.lhs.true
                                        #   in Loop: Header=BB116_5 Depth=1
	movq	8(%rcx), %rax
	cmpq	%r8, %rax
	movq	$33, %rax
	je	.LBB116_21
# BB#8:                                 # %land.lhs.true72
                                        #   in Loop: Header=BB116_5 Depth=1
	movq	16(%rcx), %rax
	movzbl	(%rax), %eax
	cmpq	$50, %rax
	movq	$34, %rax
	je	.LBB116_21
.LBB116_9:                              # %for.cond.backedge
                                        #   in Loop: Header=BB116_5 Depth=1
	movq	$32, %rcx
	movq	%rsi, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	decq	%rsi
	testq	%rax, %rax
	jg	.LBB116_5
	jmp	.LBB116_10
.LBB116_11:                             # %entry
	cmpq	$47, %rcx
	je	.LBB116_16
# BB#12:                                # %entry
	cmpq	$51, %rcx
	jne	.LBB116_13
# BB#15:                                # %if.then9
	movq	$33, %rax
	retq
.LBB116_16:                             # %if.then14
	movq	8(%rdi), %rax
	movq	global_rtl(%rip), %rcx
	cmpq	%rcx, %rax
	movq	$33, %rax
	je	.LBB116_21
# BB#17:                                # %if.else
	movq	16(%rdi), %rax
	movzbl	(%rax), %eax
	cmpq	$50, %rax
	jne	.LBB116_18
# BB#19:                                # %if.else
	movabsq	$1, %rax
	jmp	.LBB116_20
.LBB116_13:                             # %entry
	cmpq	$50, %rcx
	jne	.LBB116_10
# BB#14:                                # %if.then4
	movq	$34, %rax
	retq
.LBB116_10:                             # %if.end89
	movq	$32, %rax
	retq
.LBB116_18:
	movabsq	$0, %rax
.LBB116_20:                             # %if.else
	leaq	32(%rax,%rax), %rax
.LBB116_21:                             # %return
	retq
.Ltmp923:
	.size	classify_insn, .Ltmp923-classify_insn
	.cfi_endproc

	.globl	emit
	.align	16, 0x90
	.type	emit,@function
emit:                                   # @emit
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp927:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp928:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp929:
	.cfi_def_cfa_offset 32
.Ltmp930:
	.cfi_offset %rbx, -24
.Ltmp931:
	.cfi_offset %r14, -16
	movq	%rdi, %r14
	movzbl	(%r14), %eax
	cmpq	$46, %rax
	jg	.LBB117_11
# BB#1:                                 # %entry
	cmpq	$36, %rax
	jne	.LBB117_2
# BB#16:                                # %if.then
	movl	8(%r14), %eax
	testq	%rax, %rax
	jne	.LBB117_42
# BB#17:                                # %if.then.i37
	movq	cfun(%rip), %rax
	movq	24(%rax), %rax
	movl	40(%rax), %ecx
	leaq	1(%rcx), %rdx
	movl	%edx, 40(%rax)
	movl	%ecx, 8(%r14)
	movq	16(%rax), %rax
	movq	%rax, 16(%r14)
	movq	$0, 24(%r14)
	movq	cfun(%rip), %rax
	movq	24(%rax), %rax
	movq	16(%rax), %rcx
	testq	%rcx, %rcx
	je	.LBB117_19
# BB#18:                                # %if.then.i.i42
	movq	%r14, 24(%rcx)
	movq	cfun(%rip), %rax
	movq	24(%rax), %rax
.LBB117_19:                             # %if.end.i.i45
	cmpq	$0, 8(%rax)
	jne	.LBB117_21
# BB#20:                                # %if.then13.i.i49
	movq	%r14, 8(%rax)
	movq	cfun(%rip), %rax
	movq	24(%rax), %rax
.LBB117_21:                             # %add_insn.exit.i51
	movq	%r14, 16(%rax)
	jmp	.LBB117_42
.LBB117_11:                             # %entry
	cmpq	$51, %rax
	je	.LBB117_24
# BB#12:                                # %entry
	cmpq	$50, %rax
	je	.LBB117_37
# BB#13:                                # %entry
	cmpq	$47, %rax
	jne	.LBB117_10
# BB#14:                                # %if.then14.i
	movq	8(%r14), %rax
	movq	global_rtl(%rip), %rcx
	cmpq	%rcx, %rax
	je	.LBB117_24
# BB#15:                                # %classify_insn.exit
	movq	16(%r14), %rax
	movzbl	(%rax), %eax
	cmpq	$50, %rax
	jne	.LBB117_10
	jmp	.LBB117_37
.LBB117_2:                              # %entry
	cmpq	$39, %rax
	jne	.LBB117_10
# BB#3:                                 # %if.then29.i
	movq	8(%r14), %rdx
	movslq	(%rdx), %rdi
	testq	%rdi, %rdi
	jle	.LBB117_10
# BB#4:                                 # %for.body.lr.ph.i
	movq	global_rtl(%rip), %r8
	decq	%rdi
	.align	16, 0x90
.LBB117_5:                              # %for.body.i
                                        # =>This Inner Loop Header: Depth=1
	movq	8(%rdx,%rdi,8), %rcx
	movzbl	(%rcx), %ebx
	cmpq	$50, %rbx
	je	.LBB117_36
# BB#6:                                 # %for.body.i
                                        #   in Loop: Header=BB117_5 Depth=1
	cmpq	$47, %rbx
	jne	.LBB117_9
# BB#7:                                 # %land.lhs.true.i
                                        #   in Loop: Header=BB117_5 Depth=1
	movq	8(%rcx), %rbx
	cmpq	%r8, %rbx
	je	.LBB117_22
# BB#8:                                 # %land.lhs.true72.i
                                        #   in Loop: Header=BB117_5 Depth=1
	movq	16(%rcx), %rcx
	movzbl	(%rcx), %ecx
	cmpq	$50, %rcx
	je	.LBB117_36
.LBB117_9:                              # %for.cond.backedge.i
                                        #   in Loop: Header=BB117_5 Depth=1
	movq	$32, %rcx
	movq	%rdi, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	decq	%rdi
	testq	%rsi, %rsi
	jg	.LBB117_5
	jmp	.LBB117_10
.LBB117_36:                             # %if.then15
	cmpq	$24, %rax
	jne	.LBB117_37
.LBB117_10:                             # %if.then3
	movq	%r14, %rdi
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	jmp	emit_insn  # TAILCALL
.LBB117_37:                             # %if.else.i29
	movq	$34, %rdi
	callq	rtx_alloc
	movq	cfun(%rip), %rcx
	movq	24(%rcx), %rcx
	movl	40(%rcx), %edx
	leaq	1(%rdx), %rsi
	movl	%esi, 40(%rcx)
	movl	%edx, 8(%rax)
	movq	%r14, 32(%rax)
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	movl	%ecx, 40(%rax)
	movq	$0, 64(%rax)
	movq	$0, 56(%rax)
	movq	$0, 48(%rax)
	movq	cfun(%rip), %rcx
	movq	24(%rcx), %rcx
	movq	16(%rcx), %rcx
	movq	%rcx, 16(%rax)
	movq	$0, 24(%rax)
	movq	cfun(%rip), %rcx
	movq	24(%rcx), %rcx
	movq	16(%rcx), %rdx
	testq	%rdx, %rdx
	je	.LBB117_39
# BB#38:                                # %if.then.i.i
	movq	%rax, 24(%rdx)
	movq	cfun(%rip), %rcx
	movq	24(%rcx), %rcx
.LBB117_39:                             # %if.end.i.i
	cmpq	$0, 8(%rcx)
	jne	.LBB117_41
# BB#40:                                # %if.then13.i.i
	movq	%rax, 8(%rcx)
	movq	cfun(%rip), %rcx
	movq	24(%rcx), %rcx
.LBB117_41:                             # %add_insn.exit.i
	movq	%rax, 16(%rcx)
	movq	$34, %rcx
	movw	%cx, (%rax)
	movq	%rax, %r14
	jmp	.LBB117_42
.LBB117_22:                             # %if.then7
	cmpq	$24, %rax
	jne	.LBB117_24
# BB#23:                                # %if.then.i56
	movq	%r14, %rdi
	callq	emit_insn
	movq	%rax, %rbx
	jmp	.LBB117_29
.LBB117_24:                             # %if.else.i71
	movq	$33, %rdi
	callq	rtx_alloc
	movq	cfun(%rip), %rcx
	movq	24(%rcx), %rcx
	movl	40(%rcx), %edx
	movq	%rax, %rbx
	leaq	1(%rdx), %rax
	movl	%eax, 40(%rcx)
	movl	%edx, 8(%rbx)
	movq	%r14, 32(%rbx)
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	movl	%eax, 40(%rbx)
	movq	$0, 64(%rbx)
	movq	$0, 56(%rbx)
	movq	$0, 48(%rbx)
	movq	cfun(%rip), %rax
	movq	24(%rax), %rax
	movq	16(%rax), %rax
	movq	%rax, 16(%rbx)
	movq	$0, 24(%rbx)
	movq	cfun(%rip), %rax
	movq	24(%rax), %rax
	movq	16(%rax), %rcx
	testq	%rcx, %rcx
	je	.LBB117_26
# BB#25:                                # %if.then.i.i76
	movq	%rbx, 24(%rcx)
	movq	cfun(%rip), %rax
	movq	24(%rax), %rax
.LBB117_26:                             # %if.end.i.i79
	cmpq	$0, 8(%rax)
	jne	.LBB117_28
# BB#27:                                # %if.then13.i.i83
	movq	%rbx, 8(%rax)
	movq	cfun(%rip), %rax
	movq	24(%rax), %rax
.LBB117_28:                             # %add_insn.exit.i85
	movq	%rbx, 16(%rax)
.LBB117_29:                             # %emit_jump_insn.exit
	movq	%rbx, %rdi
	callq	any_uncondjump_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB117_31
# BB#30:                                # %lor.lhs.false
	movzbl	(%r14), %eax
	cmpq	$51, %rax
	movq	%rbx, %r14
	jne	.LBB117_42
.LBB117_31:                             # %if.then11
	movq	$35, %rdi
	callq	rtx_alloc
	movq	cfun(%rip), %rcx
	movq	24(%rcx), %rcx
	movl	40(%rcx), %edx
	movq	%rax, %r14
	leaq	1(%rdx), %rax
	movl	%eax, 40(%rcx)
	movl	%edx, 8(%r14)
	movq	16(%rcx), %rax
	movq	%rax, 16(%r14)
	movq	$0, 24(%r14)
	movq	cfun(%rip), %rax
	movq	24(%rax), %rax
	movq	16(%rax), %rcx
	testq	%rcx, %rcx
	je	.LBB117_33
# BB#32:                                # %if.then.i.i102
	movq	%r14, 24(%rcx)
	movq	cfun(%rip), %rax
	movq	24(%rax), %rax
.LBB117_33:                             # %if.end.i.i105
	cmpq	$0, 8(%rax)
	jne	.LBB117_35
# BB#34:                                # %if.then13.i.i109
	movq	%r14, 8(%rax)
	movq	cfun(%rip), %rax
	movq	24(%rax), %rax
.LBB117_35:                             # %emit_barrier.exit
	movq	%r14, 16(%rax)
.LBB117_42:                             # %return
	movq	%r14, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp932:
	.size	emit, .Ltmp932-emit
	.cfi_endproc

	.globl	start_sequence
	.align	16, 0x90
	.type	start_sequence,@function
start_sequence:                         # @start_sequence
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rax
.Ltmp934:
	.cfi_def_cfa_offset 16
	movq	$32, %rdi
	callq	xmalloc
	movq	cfun(%rip), %rcx
	movq	24(%rcx), %rcx
	movq	32(%rcx), %rdx
	movq	%rdx, 24(%rax)
	movq	8(%rcx), %rdx
	movq	16(%rcx), %rsi
	movq	%rsi, 8(%rax)
	movq	%rdx, (%rax)
	movq	24(%rcx), %rdx
	movq	%rdx, 16(%rax)
	movq	%rax, 32(%rcx)
	movq	cfun(%rip), %rax
	movq	24(%rax), %rax
	movq	$0, 8(%rax)
	movq	cfun(%rip), %rax
	movq	24(%rax), %rax
	movq	$0, 16(%rax)
	popq	%rax
	retq
.Ltmp935:
	.size	start_sequence, .Ltmp935-start_sequence
	.cfi_endproc

	.globl	start_sequence_for_rtl_expr
	.align	16, 0x90
	.type	start_sequence_for_rtl_expr,@function
start_sequence_for_rtl_expr:            # @start_sequence_for_rtl_expr
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp938:
	.cfi_def_cfa_offset 16
.Ltmp939:
	.cfi_offset %rbx, -16
	movq	%rdi, %rbx
	movq	$32, %rdi
	callq	xmalloc
	movq	cfun(%rip), %rcx
	movq	24(%rcx), %rcx
	movq	32(%rcx), %rdx
	movq	%rdx, 24(%rax)
	movq	8(%rcx), %rdx
	movq	16(%rcx), %rsi
	movq	%rsi, 8(%rax)
	movq	%rdx, (%rax)
	movq	24(%rcx), %rdx
	movq	%rdx, 16(%rax)
	movq	%rax, 32(%rcx)
	movq	cfun(%rip), %rax
	movq	24(%rax), %rax
	movq	$0, 8(%rax)
	movq	cfun(%rip), %rax
	movq	24(%rax), %rax
	movq	$0, 16(%rax)
	movq	cfun(%rip), %rax
	movq	24(%rax), %rax
	movq	%rbx, 24(%rax)
	popq	%rbx
	retq
.Ltmp940:
	.size	start_sequence_for_rtl_expr, .Ltmp940-start_sequence_for_rtl_expr
	.cfi_endproc

	.globl	push_to_sequence
	.align	16, 0x90
	.type	push_to_sequence,@function
push_to_sequence:                       # @push_to_sequence
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp943:
	.cfi_def_cfa_offset 16
.Ltmp944:
	.cfi_offset %rbx, -16
	movq	%rdi, %rbx
	movq	$32, %rdi
	callq	xmalloc
	movq	cfun(%rip), %rcx
	movq	24(%rcx), %rcx
	movq	32(%rcx), %rdx
	movq	%rdx, 24(%rax)
	movq	8(%rcx), %rdx
	movq	16(%rcx), %rsi
	movq	%rsi, 8(%rax)
	movq	%rdx, (%rax)
	movq	24(%rcx), %rdx
	movq	%rdx, 16(%rax)
	movq	%rax, 32(%rcx)
	movq	cfun(%rip), %rax
	movq	24(%rax), %rax
	movq	$0, 8(%rax)
	movq	cfun(%rip), %rax
	movq	24(%rax), %rax
	movq	$0, 16(%rax)
	movq	%rbx, %rdx
	.align	16, 0x90
.LBB120_1:                              # %for.cond
                                        # =>This Inner Loop Header: Depth=1
	movq	%rdx, %rcx
	xorq	%rax, %rax
	testq	%rcx, %rcx
	je	.LBB120_3
# BB#2:                                 # %land.rhs
                                        #   in Loop: Header=BB120_1 Depth=1
	movq	24(%rcx), %rdx
	testq	%rdx, %rdx
	movq	%rcx, %rax
	jne	.LBB120_1
.LBB120_3:                              # %for.end
	movq	cfun(%rip), %rcx
	movq	24(%rcx), %rcx
	movq	%rbx, 8(%rcx)
	movq	cfun(%rip), %rcx
	movq	24(%rcx), %rcx
	movq	%rax, 16(%rcx)
	popq	%rbx
	retq
.Ltmp945:
	.size	push_to_sequence, .Ltmp945-push_to_sequence
	.cfi_endproc

	.globl	push_to_full_sequence
	.align	16, 0x90
	.type	push_to_full_sequence,@function
push_to_full_sequence:                  # @push_to_full_sequence
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp949:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp950:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp951:
	.cfi_def_cfa_offset 32
.Ltmp952:
	.cfi_offset %rbx, -24
.Ltmp953:
	.cfi_offset %r14, -16
	movq	%rsi, %rbx
	movq	%rdi, %r14
	movq	$32, %rdi
	callq	xmalloc
	movq	cfun(%rip), %rcx
	movq	24(%rcx), %rcx
	movq	32(%rcx), %rdx
	movq	%rdx, 24(%rax)
	movq	8(%rcx), %rdx
	movq	16(%rcx), %rsi
	movq	%rsi, 8(%rax)
	movq	%rdx, (%rax)
	movq	24(%rcx), %rdx
	movq	%rdx, 16(%rax)
	movq	%rax, 32(%rcx)
	movq	cfun(%rip), %rax
	movq	24(%rax), %rax
	movq	$0, 8(%rax)
	movq	cfun(%rip), %rax
	movq	24(%rax), %rax
	movq	$0, 16(%rax)
	movq	cfun(%rip), %rax
	movq	24(%rax), %rax
	movq	%r14, 8(%rax)
	movq	cfun(%rip), %rax
	movq	24(%rax), %rax
	testq	%rbx, %rbx
	movq	%rbx, 16(%rax)
	je	.LBB121_2
# BB#1:                                 # %land.lhs.true
	cmpq	$0, 24(%rbx)
	jne	.LBB121_3
.LBB121_2:                              # %if.end
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.LBB121_3:                              # %if.then
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.push_to_full_sequence, %rcx
	movq	$4491, %rsi             # imm = 0x118B
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp954:
	.size	push_to_full_sequence, .Ltmp954-push_to_full_sequence
	.cfi_endproc

	.globl	push_topmost_sequence
	.align	16, 0x90
	.type	push_topmost_sequence,@function
push_topmost_sequence:                  # @push_topmost_sequence
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rax
.Ltmp956:
	.cfi_def_cfa_offset 16
	movq	$32, %rdi
	callq	xmalloc
	movq	cfun(%rip), %rcx
	movq	24(%rcx), %rcx
	movq	32(%rcx), %rdx
	movq	%rdx, 24(%rax)
	movq	8(%rcx), %rdx
	movq	16(%rcx), %rsi
	movq	%rsi, 8(%rax)
	movq	%rdx, (%rax)
	movq	24(%rcx), %rdx
	movq	%rdx, 16(%rax)
	movq	%rax, 32(%rcx)
	movq	cfun(%rip), %rax
	movq	24(%rax), %rax
	movq	$0, 8(%rax)
	movq	cfun(%rip), %rax
	movq	24(%rax), %rax
	movq	$0, 16(%rax)
	movq	cfun(%rip), %rax
	movq	24(%rax), %rcx
	leaq	32(%rcx), %rdx
	xorq	%rsi, %rsi
	.align	16, 0x90
.LBB122_1:                              # %for.cond
                                        # =>This Inner Loop Header: Depth=1
	movq	%rsi, %rax
	movq	(%rdx), %rsi
	leaq	24(%rsi), %rdx
	testq	%rsi, %rsi
	jne	.LBB122_1
# BB#2:                                 # %for.end
	movq	(%rax), %rdx
	movq	%rdx, 8(%rcx)
	movq	cfun(%rip), %rcx
	movq	8(%rax), %rdx
	movq	24(%rcx), %rcx
	movq	%rdx, 16(%rcx)
	movq	cfun(%rip), %rcx
	movq	16(%rax), %rax
	movq	24(%rcx), %rcx
	movq	%rax, 24(%rcx)
	popq	%rax
	retq
.Ltmp957:
	.size	push_topmost_sequence, .Ltmp957-push_topmost_sequence
	.cfi_endproc

	.globl	pop_topmost_sequence
	.align	16, 0x90
	.type	pop_topmost_sequence,@function
pop_topmost_sequence:                   # @pop_topmost_sequence
	.cfi_startproc
# BB#0:                                 # %entry
	movq	cfun(%rip), %rax
	movq	24(%rax), %rax
	leaq	32(%rax), %rdx
	xorq	%rsi, %rsi
	.align	16, 0x90
.LBB123_1:                              # %for.cond
                                        # =>This Inner Loop Header: Depth=1
	movq	%rsi, %rcx
	movq	(%rdx), %rsi
	leaq	24(%rsi), %rdx
	testq	%rsi, %rsi
	jne	.LBB123_1
# BB#2:                                 # %for.end
	movq	8(%rax), %rax
	movq	%rax, (%rcx)
	movq	cfun(%rip), %rax
	movq	24(%rax), %rax
	movq	16(%rax), %rax
	movq	%rax, 8(%rcx)
	movq	cfun(%rip), %rax
	movq	24(%rax), %rax
	movq	32(%rax), %rdi
	movq	(%rdi), %rcx
	movq	%rcx, 8(%rax)
	movq	cfun(%rip), %rax
	movq	8(%rdi), %rcx
	movq	24(%rax), %rax
	movq	%rcx, 16(%rax)
	movq	cfun(%rip), %rax
	movq	16(%rdi), %rcx
	movq	24(%rax), %rax
	movq	%rcx, 24(%rax)
	movq	cfun(%rip), %rax
	movq	24(%rdi), %rcx
	movq	24(%rax), %rax
	movq	%rcx, 32(%rax)
	jmp	free  # TAILCALL
.Ltmp958:
	.size	pop_topmost_sequence, .Ltmp958-pop_topmost_sequence
	.cfi_endproc

	.globl	end_sequence
	.align	16, 0x90
	.type	end_sequence,@function
end_sequence:                           # @end_sequence
	.cfi_startproc
# BB#0:                                 # %entry
	movq	cfun(%rip), %rax
	movq	24(%rax), %rax
	movq	32(%rax), %rdi
	movq	(%rdi), %rcx
	movq	%rcx, 8(%rax)
	movq	cfun(%rip), %rax
	movq	8(%rdi), %rcx
	movq	24(%rax), %rax
	movq	%rcx, 16(%rax)
	movq	cfun(%rip), %rax
	movq	16(%rdi), %rcx
	movq	24(%rax), %rax
	movq	%rcx, 24(%rax)
	movq	cfun(%rip), %rax
	movq	24(%rdi), %rcx
	movq	24(%rax), %rax
	movq	%rcx, 32(%rax)
	jmp	free  # TAILCALL
.Ltmp959:
	.size	end_sequence, .Ltmp959-end_sequence
	.cfi_endproc

	.globl	end_full_sequence
	.align	16, 0x90
	.type	end_full_sequence,@function
end_full_sequence:                      # @end_full_sequence
	.cfi_startproc
# BB#0:                                 # %entry
	movq	cfun(%rip), %rax
	movq	24(%rax), %rax
	movq	8(%rax), %rax
	movq	%rax, (%rdi)
	movq	cfun(%rip), %rax
	movq	24(%rax), %rax
	movq	16(%rax), %rax
	movq	%rax, (%rsi)
	movq	cfun(%rip), %rax
	movq	24(%rax), %rax
	movq	32(%rax), %rdi
	movq	(%rdi), %rcx
	movq	%rcx, 8(%rax)
	movq	cfun(%rip), %rax
	movq	8(%rdi), %rcx
	movq	24(%rax), %rax
	movq	%rcx, 16(%rax)
	movq	cfun(%rip), %rax
	movq	16(%rdi), %rcx
	movq	24(%rax), %rax
	movq	%rcx, 24(%rax)
	movq	cfun(%rip), %rax
	movq	24(%rdi), %rcx
	movq	24(%rax), %rax
	movq	%rcx, 32(%rax)
	jmp	free  # TAILCALL
.Ltmp960:
	.size	end_full_sequence, .Ltmp960-end_full_sequence
	.cfi_endproc

	.globl	in_sequence_p
	.align	16, 0x90
	.type	in_sequence_p,@function
in_sequence_p:                          # @in_sequence_p
	.cfi_startproc
# BB#0:                                 # %entry
	movq	cfun(%rip), %rax
	movq	24(%rax), %rax
	cmpq	$0, 32(%rax)
	je	.LBB126_1
# BB#2:                                 # %entry
	movabsq	$1, %rax
	retq
.LBB126_1:
	movabsq	$0, %rax
	retq
.Ltmp961:
	.size	in_sequence_p, .Ltmp961-in_sequence_p
	.cfi_endproc

	.globl	gen_sequence
	.align	16, 0x90
	.type	gen_sequence,@function
gen_sequence:                           # @gen_sequence
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rax
.Ltmp963:
	.cfi_def_cfa_offset 16
	movq	cfun(%rip), %rax
	movq	24(%rax), %rax
	movq	8(%rax), %rax
	xorq	%rdi, %rdi
	testq	%rax, %rax
	je	.LBB127_7
# BB#1:
	movq	%rax, %rcx
	.align	16, 0x90
.LBB127_2:                              # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	24(%rcx), %rcx
	movq	%rdi, %rdx
	leaq	1(%rdx), %rdi
	testq	%rcx, %rcx
	jne	.LBB127_2
# BB#3:                                 # %for.end
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rdx
	jne	.LBB127_7
# BB#4:                                 # %land.lhs.true
	movl	(%rax), %ecx
	movabsq	$2147549183, %rdx       # imm = 0x8000FFFF
	andq	%rcx, %rdx
	cmpq	$32, %rdx
	movq	$1, %rdi
	jne	.LBB127_7
# BB#5:                                 # %land.lhs.true9
	cmpq	$0, 56(%rax)
	je	.LBB127_6
.LBB127_7:                              # %if.end
	callq	rtvec_alloc
	movq	$24, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_E
	movq	cfun(%rip), %rcx
	movq	24(%rcx), %rcx
	movq	8(%rcx), %rcx
	testq	%rcx, %rcx
	je	.LBB127_10
# BB#8:                                 # %for.body26.lr.ph
	movq	$8, %rdx
	.align	16, 0x90
.LBB127_9:                              # %for.body26
                                        # =>This Inner Loop Header: Depth=1
	movq	8(%rax), %rsi
	movq	%rcx, (%rsi,%rdx)
	movq	24(%rcx), %rcx
	addq	$8, %rdx
	testq	%rcx, %rcx
	jne	.LBB127_9
.LBB127_10:                             # %return
	popq	%rdx
	retq
.LBB127_6:                              # %if.then
	movq	32(%rax), %rax
	popq	%rdx
	retq
.Ltmp964:
	.size	gen_sequence, .Ltmp964-gen_sequence
	.cfi_endproc

	.globl	init_virtual_regs
	.align	16, 0x90
	.type	init_virtual_regs,@function
init_virtual_regs:                      # @init_virtual_regs
	.cfi_startproc
# BB#0:                                 # %entry
	movq	80(%rdi), %rax
	movq	global_rtl+48(%rip), %rcx
	movq	%rcx, 424(%rax)
	movq	global_rtl+56(%rip), %rcx
	movq	%rcx, 432(%rax)
	movq	global_rtl+64(%rip), %rcx
	movq	%rcx, 440(%rax)
	movq	global_rtl+72(%rip), %rcx
	movq	%rcx, 448(%rax)
	movq	global_rtl+80(%rip), %rcx
	movq	%rcx, 456(%rax)
	retq
.Ltmp965:
	.size	init_virtual_regs, .Ltmp965-init_virtual_regs
	.cfi_endproc

	.globl	copy_insn_1
	.align	16, 0x90
	.type	copy_insn_1,@function
copy_insn_1:                            # @copy_insn_1
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp972:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp973:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp974:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp975:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp976:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp977:
	.cfi_def_cfa_offset 64
.Ltmp978:
	.cfi_offset %rbx, -48
.Ltmp979:
	.cfi_offset %r12, -40
.Ltmp980:
	.cfi_offset %r13, -32
.Ltmp981:
	.cfi_offset %r14, -24
.Ltmp982:
	.cfi_offset %r15, -16
	movq	%rdi, %r15
	movzbl	(%r15), %ebx
	leaq	-36(%rbx), %rcx
	movabsq	$4294967295, %rdi       # imm = 0xFFFFFFFF
	andq	%rdi, %rcx
	cmpq	$35, %rcx
	ja	.LBB129_14
# BB#1:                                 # %entry
	movq	$1, %rax
	shlq	%cl, %rax
	movabsq	$64468287489, %rdx      # imm = 0xF029C0001
	testq	%rdx, %rax
	je	.LBB129_3
# BB#2:
	movq	%r15, %r13
	jmp	.LBB129_40
.LBB129_3:                              # %entry
	cmpq	$22, %rcx
	jne	.LBB129_4
# BB#10:                                # %sw.bb5
	movq	8(%r15), %rax
	movzbl	(%rax), %ecx
	cmpq	$75, %rcx
	jne	.LBB129_14
# BB#11:                                # %land.lhs.true
	movq	8(%rax), %rcx
	movzbl	(%rcx), %ecx
	cmpq	$68, %rcx
	jne	.LBB129_14
# BB#12:                                # %land.lhs.true19
	movq	16(%rax), %rax
	movzbl	(%rax), %eax
	cmpq	$54, %rax
	jne	.LBB129_14
# BB#13:
	movq	%r15, %r13
	jmp	.LBB129_40
.LBB129_4:                              # %entry
	cmpq	$26, %rcx
	jne	.LBB129_14
# BB#5:                                 # %for.cond.preheader
	movslq	copy_insn_n_scratches(%rip), %rax
	testq	%rax, %rax
	jle	.LBB129_14
# BB#6:
	movq	$1, %rdx
	.align	16, 0x90
.LBB129_8:                              # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	copy_insn_scratch_in-8(,%rdx,8), %rcx
	cmpq	%r15, %rcx
	je	.LBB129_9
# BB#7:                                 # %for.cond
                                        #   in Loop: Header=BB129_8 Depth=1
	movq	$32, %rcx
	movq	%rdx, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	incq	%rdx
	cmpq	%rax, %rsi
	jl	.LBB129_8
.LBB129_14:                             # %sw.epilog
	movq	%rdi, %r14
	movq	%rbx, %rdi
	callq	rtx_alloc
	movq	%rax, %r13
	movq	%r13, 8(%rsp)           # 8-byte Spill
	movq	(%r15), %rax
	movq	%rax, (%r13)
	movzbl	rtx_class(%rbx), %edx
	movq	%rbx, (%rsp)            # 8-byte Spill
	movabsq	$3758096383, %rcx       # imm = 0xDFFFFFFF
	andq	%rax, %rcx
	movl	%ecx, (%r13)
	cmpq	$105, %rdx
	jne	.LBB129_16
# BB#15:                                # %if.then37
	andq	$1560281087, %rax       # imm = 0x5CFFFFFF
	movl	%eax, (%r13)
	movq	%rax, %rcx
.LBB129_16:                             # %if.end44
	andq	$65535, %rcx            # imm = 0xFFFF
	movzbl	rtx_length(%rcx), %eax
	testq	%rax, %rax
	movq	%r14, %rsi
	je	.LBB129_35
# BB#17:                                # %for.body57.lr.ph
	movq	rtx_format(,%rcx,8), %rbx
	xorq	%r14, %r14
	.align	16, 0x90
.LBB129_18:                             # %for.body57
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB129_33 Depth 2
	movq	8(%r15,%r14,8), %rax
	movq	%rax, 8(%r13,%r14,8)
	movsbq	(%rbx), %rcx
	incq	%rbx
	cmpq	$68, %rcx
	jg	.LBB129_21
# BB#19:                                # %for.body57
                                        #   in Loop: Header=BB129_18 Depth=1
	andq	%rsi, %rcx
	cmpq	$48, %rcx
	je	.LBB129_34
	jmp	.LBB129_20
	.align	16, 0x90
.LBB129_21:                             # %for.body57
                                        #   in Loop: Header=BB129_18 Depth=1
	addq	$-69, %rcx
	andq	%rsi, %rcx
	cmpq	$50, %rcx
	ja	.LBB129_20
# BB#22:                                # %for.body57
                                        #   in Loop: Header=BB129_18 Depth=1
	movq	$1, %rax
	shlq	%cl, %rax
	movabsq	$1618549835579392, %rdx # imm = 0x5C01000004000
	testq	%rdx, %rax
	jne	.LBB129_34
# BB#23:                                # %for.body57
                                        #   in Loop: Header=BB129_18 Depth=1
	movq	$1, %rax
	shlq	%cl, %rax
	testq	$131073, %rax           # imm = 0x20001
	je	.LBB129_24
# BB#27:                                # %sw.bb83
                                        #   in Loop: Header=BB129_18 Depth=1
	movq	8(%r15,%r14,8), %rax
	movq	orig_asm_constraints_vector(%rip), %rcx
	cmpq	%rcx, %rax
	jne	.LBB129_29
# BB#28:                                # %if.then89
                                        #   in Loop: Header=BB129_18 Depth=1
	movq	copy_asm_constraints_vector(%rip), %rax
	movq	%rax, 8(%r13,%r14,8)
	jmp	.LBB129_34
.LBB129_24:                             # %for.body57
                                        #   in Loop: Header=BB129_18 Depth=1
	cmpq	$32, %rcx
	jne	.LBB129_20
# BB#25:                                # %sw.bb65
                                        #   in Loop: Header=BB129_18 Depth=1
	movq	8(%r15,%r14,8), %rdi
	testq	%rdi, %rdi
	je	.LBB129_34
# BB#26:                                # %if.then72
                                        #   in Loop: Header=BB129_18 Depth=1
	movq	%rsi, %r12
	callq	copy_insn_1
	movq	%r12, %rsi
	movq	%rax, 8(%r13,%r14,8)
	jmp	.LBB129_34
.LBB129_29:                             # %if.else
                                        #   in Loop: Header=BB129_18 Depth=1
	movq	orig_asm_operands_vector(%rip), %rcx
	cmpq	%rcx, %rax
	jne	.LBB129_31
# BB#30:                                # %if.then100
                                        #   in Loop: Header=BB129_18 Depth=1
	movq	copy_asm_operands_vector(%rip), %rax
	movq	%rax, 8(%r13,%r14,8)
	jmp	.LBB129_34
.LBB129_31:                             # %if.else105
                                        #   in Loop: Header=BB129_18 Depth=1
	testq	%rax, %rax
	je	.LBB129_34
# BB#32:                                # %if.then112
                                        #   in Loop: Header=BB129_18 Depth=1
	movl	(%rax), %edi
	movq	%rsi, %r12
	callq	rtvec_alloc
	movq	%r12, %rsi
	movq	%rax, 8(%r13,%r14,8)
	movslq	(%rax), %rax
	testq	%rax, %rax
	movq	$1, %r12
	jle	.LBB129_34
	.align	16, 0x90
.LBB129_33:                             # %for.body130
                                        #   Parent Loop BB129_18 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	8(%r15,%r14,8), %rax
	movq	(%rax,%r12,8), %rdi
	movq	%r15, %r13
	movq	%rsi, %r15
	callq	copy_insn_1
	movq	%r15, %rsi
	movq	%r13, %r15
	movq	8(%rsp), %r13           # 8-byte Reload
	movq	8(%r13,%r14,8), %rcx
	movq	%rax, (%rcx,%r12,8)
	movq	8(%r13,%r14,8), %rax
	movslq	(%rax), %rax
	movq	$32, %rcx
	movq	%r12, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	incq	%r12
	cmpq	%rax, %rdx
	jl	.LBB129_33
	.align	16, 0x90
.LBB129_34:                             # %for.inc154
                                        #   in Loop: Header=BB129_18 Depth=1
	movzbl	(%r13), %eax
	incq	%r14
	movzbl	rtx_length(%rax), %eax
	movq	$32, %rcx
	movq	%r14, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rax, %rdx
	jl	.LBB129_18
.LBB129_35:                             # %for.end156
	movq	(%rsp), %rax            # 8-byte Reload
	cmpq	$41, %rax
	jne	.LBB129_36
# BB#39:                                # %if.then172
	movq	32(%r15), %rax
	movq	%rax, orig_asm_operands_vector(%rip)
	movq	32(%r13), %rax
	movq	%rax, copy_asm_operands_vector(%rip)
	movq	40(%r15), %rax
	movq	%rax, orig_asm_constraints_vector(%rip)
	movq	40(%r13), %rax
	movq	%rax, copy_asm_constraints_vector(%rip)
	jmp	.LBB129_40
.LBB129_36:                             # %for.end156
	cmpq	$62, %rax
	jne	.LBB129_40
# BB#37:                                # %if.then159
	movslq	copy_insn_n_scratches(%rip), %rax
	leaq	1(%rax), %rcx
	cmpq	$30, %rax
	movl	%ecx, copy_insn_n_scratches(%rip)
	jge	.LBB129_41
# BB#38:                                # %if.end164
	movq	%r15, copy_insn_scratch_in(,%rax,8)
	movq	%r13, copy_insn_scratch_out(,%rax,8)
.LBB129_40:                             # %return
	movq	%r13, %rax
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB129_20:                             # %sw.default152
	movabsq	$.L.str, %rdi
	movabsq	$.L__FUNCTION__.copy_insn_1, %rdx
	movq	$4773, %rsi             # imm = 0x12A5
	callq	fancy_abort
.LBB129_9:                              # %if.then
	movq	copy_insn_scratch_out-8(,%rdx,8), %r13
	jmp	.LBB129_40
.LBB129_41:                             # %if.then163
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.copy_insn_1, %rcx
	movq	$4781, %rsi             # imm = 0x12AD
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp983:
	.size	copy_insn_1, .Ltmp983-copy_insn_1
	.cfi_endproc

	.globl	copy_insn
	.align	16, 0x90
	.type	copy_insn,@function
copy_insn:                              # @copy_insn
	.cfi_startproc
# BB#0:                                 # %entry
	xorq	%rax, %rax
	movl	%eax, copy_insn_n_scratches(%rip)
	movq	$0, orig_asm_operands_vector(%rip)
	movq	$0, orig_asm_constraints_vector(%rip)
	movq	$0, copy_asm_operands_vector(%rip)
	movq	$0, copy_asm_constraints_vector(%rip)
	jmp	copy_insn_1  # TAILCALL
.Ltmp984:
	.size	copy_insn, .Ltmp984-copy_insn
	.cfi_endproc

	.globl	init_emit
	.align	16, 0x90
	.type	init_emit,@function
init_emit:                              # @init_emit
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp987:
	.cfi_def_cfa_offset 16
.Ltmp988:
	.cfi_offset %rbx, -16
	movq	cfun(%rip), %rbx
	movq	$88, %rdi
	callq	xmalloc
	movq	%rax, 24(%rbx)
	movq	cfun(%rip), %rax
	movq	24(%rax), %rax
	movq	$0, 8(%rax)
	movq	cfun(%rip), %rax
	movq	24(%rax), %rax
	movq	$0, 16(%rax)
	movq	cfun(%rip), %rax
	movq	24(%rax), %rax
	movq	$0, 24(%rax)
	movq	cfun(%rip), %rax
	movq	24(%rax), %rax
	movq	$1, %rcx
	movl	%ecx, 40(%rax)
	movq	$58, %rcx
	movl	%ecx, (%rax)
	xorq	%rcx, %rcx
	movl	%ecx, 44(%rax)
	movq	$0, 48(%rax)
	movq	cfun(%rip), %rax
	movl	label_num(%rip), %edx
	movq	24(%rax), %rax
	movl	%edx, 4(%rax)
	movl	%ecx, last_label_num(%rip)
	movq	$0, 32(%rax)
	movq	$0, sequence_result+32(%rip)
	movq	$0, sequence_result+24(%rip)
	movq	$0, sequence_result+16(%rip)
	movq	$0, sequence_result+8(%rip)
	movq	$0, sequence_result(%rip)
	movq	24(%rbx), %rax
	movq	$158, %rcx
	movl	%ecx, 56(%rax)
	movq	$158, %rdi
	movq	$1, %rsi
	callq	xcalloc
	movq	24(%rbx), %rcx
	movq	%rax, 64(%rcx)
	movslq	56(%rcx), %rdi
	movq	$8, %rsi
	callq	xcalloc
	movq	cfun(%rip), %rcx
	movq	24(%rcx), %rcx
	movq	%rax, 80(%rcx)
	movq	24(%rbx), %rax
	movslq	56(%rax), %rdi
	movq	$8, %rsi
	callq	xcalloc
	movq	24(%rbx), %rcx
	movq	%rax, 72(%rcx)
	movq	80(%rcx), %rax
	movq	global_rtl+48(%rip), %rcx
	movq	%rcx, 424(%rax)
	movq	global_rtl+56(%rip), %rcx
	movq	%rcx, 432(%rax)
	movq	global_rtl+64(%rip), %rcx
	movq	%rcx, 440(%rax)
	movq	global_rtl+72(%rip), %rcx
	movq	%rcx, 448(%rax)
	movq	global_rtl+80(%rip), %rcx
	movq	%rcx, 456(%rax)
	movq	global_rtl+16(%rip), %rcx
	movl	(%rcx), %edx
	movabsq	$2147483648, %rax       # imm = 0x80000000
	orq	%rax, %rdx
	movl	%edx, (%rcx)
	movq	global_rtl+24(%rip), %rcx
	movl	(%rcx), %edx
	orq	%rax, %rdx
	movl	%edx, (%rcx)
	movq	global_rtl+32(%rip), %rcx
	movl	(%rcx), %edx
	orq	%rax, %rdx
	movl	%edx, (%rcx)
	movq	global_rtl+40(%rip), %rcx
	movl	(%rcx), %edx
	orq	%rax, %rdx
	movl	%edx, (%rcx)
	movq	global_rtl+48(%rip), %rcx
	movl	(%rcx), %edx
	orq	%rax, %rdx
	movl	%edx, (%rcx)
	movq	global_rtl+56(%rip), %rcx
	movl	(%rcx), %edx
	orq	%rax, %rdx
	movl	%edx, (%rcx)
	movq	global_rtl+64(%rip), %rcx
	movl	(%rcx), %edx
	orq	%rax, %rdx
	movl	%edx, (%rcx)
	movq	global_rtl+72(%rip), %rcx
	movl	(%rcx), %edx
	orq	%rax, %rdx
	movl	%edx, (%rcx)
	movq	global_rtl+80(%rip), %rcx
	movl	(%rcx), %edx
	orq	%rax, %rdx
	movl	%edx, (%rcx)
	movq	cfun(%rip), %rax
	movq	24(%rax), %rax
	movl	target_flags(%rip), %edx
	movq	$20, %rcx
	movq	64(%rax), %rax
	shrq	%cl, %rdx
	andq	$32, %rdx
	addq	$32, %rdx
	movb	%dl, 7(%rax)
	movq	cfun(%rip), %rax
	movq	24(%rax), %rax
	movl	target_flags(%rip), %edx
	movq	$20, %rcx
	movq	64(%rax), %rax
	shrq	%cl, %rdx
	andq	$32, %rdx
	addq	$32, %rdx
	movb	%dl, 20(%rax)
	movq	cfun(%rip), %rax
	movq	24(%rax), %rax
	movl	target_flags(%rip), %edx
	movq	$20, %rcx
	movq	64(%rax), %rax
	shrq	%cl, %rdx
	andq	$32, %rdx
	addq	$32, %rdx
	movb	%dl, 6(%rax)
	movq	cfun(%rip), %rax
	movq	24(%rax), %rax
	movl	target_flags(%rip), %edx
	movq	$20, %rcx
	movq	64(%rax), %rax
	shrq	%cl, %rdx
	andq	$32, %rdx
	addq	$32, %rdx
	movb	%dl, 16(%rax)
	movq	cfun(%rip), %rax
	movq	24(%rax), %rax
	movl	target_flags(%rip), %edx
	movq	$20, %rcx
	movq	64(%rax), %rax
	shrq	%cl, %rdx
	andq	$32, %rdx
	addq	$32, %rdx
	movb	%dl, 53(%rax)
	movq	cfun(%rip), %rax
	movq	24(%rax), %rax
	movl	target_flags(%rip), %edx
	movq	$20, %rcx
	movq	64(%rax), %rax
	shrq	%cl, %rdx
	andq	$32, %rdx
	addq	$32, %rdx
	movb	%dl, 54(%rax)
	movq	cfun(%rip), %rax
	movq	24(%rax), %rax
	movl	target_flags(%rip), %edx
	movq	$20, %rcx
	movq	64(%rax), %rax
	shrq	%cl, %rdx
	andq	$32, %rdx
	addq	$32, %rdx
	movb	%dl, 55(%rax)
	movq	cfun(%rip), %rax
	movq	24(%rax), %rax
	movl	target_flags(%rip), %edx
	movq	$20, %rcx
	movq	64(%rax), %rax
	shrq	%cl, %rdx
	andq	$32, %rdx
	addq	$32, %rdx
	movb	%dl, 56(%rax)
	movq	cfun(%rip), %rax
	movq	24(%rax), %rax
	movl	target_flags(%rip), %edx
	movq	$20, %rcx
	movq	64(%rax), %rax
	shrq	%cl, %rdx
	andq	$32, %rdx
	addq	$32, %rdx
	movb	%dl, 57(%rax)
	popq	%rbx
	retq
.Ltmp989:
	.size	init_emit, .Ltmp989-init_emit
	.cfi_endproc

	.globl	mark_emit_status
	.align	16, 0x90
	.type	mark_emit_status,@function
mark_emit_status:                       # @mark_emit_status
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp996:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp997:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp998:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp999:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp1000:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp1001:
	.cfi_def_cfa_offset 64
.Ltmp1002:
	.cfi_offset %rbx, -48
.Ltmp1003:
	.cfi_offset %r12, -40
.Ltmp1004:
	.cfi_offset %r13, -32
.Ltmp1005:
	.cfi_offset %r14, -24
.Ltmp1006:
	.cfi_offset %r15, -16
	testq	%rdi, %rdi
	je	.LBB132_30
# BB#1:                                 # %if.end
	movslq	56(%rdi), %r15
	movq	%rdi, 8(%rsp)           # 8-byte Spill
	testq	%r15, %r15
	jle	.LBB132_12
# BB#2:                                 # %do.body.lr.ph
	movq	8(%rsp), %rax           # 8-byte Reload
	movq	72(%rax), %r12
	movq	80(%rax), %r13
	movabsq	$4294967295, %r14       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB132_3:                              # %do.body
                                        # =>This Inner Loop Header: Depth=1
	movq	(%r13), %rbx
	testq	%rbx, %rbx
	je	.LBB132_6
# BB#4:                                 # %land.lhs.true
                                        #   in Loop: Header=BB132_3 Depth=1
	movq	%rbx, %rdi
	callq	ggc_set_mark
	testq	%r14, %rax
	jne	.LBB132_6
# BB#5:                                 # %if.then3
                                        #   in Loop: Header=BB132_3 Depth=1
	movq	%rbx, %rdi
	callq	ggc_mark_rtx_children
.LBB132_6:                              # %do.body5
                                        #   in Loop: Header=BB132_3 Depth=1
	movq	(%r12), %rbx
	testq	%rbx, %rbx
	je	.LBB132_11
# BB#7:                                 # %land.lhs.true7
                                        #   in Loop: Header=BB132_3 Depth=1
	movq	%rbx, %rdi
	callq	ggc_set_mark
	testq	%r14, %rax
	jne	.LBB132_11
# BB#8:                                 # %do.body11
                                        #   in Loop: Header=BB132_3 Depth=1
	movq	ggc_pending_trees(%rip), %rax
	movq	(%rax), %rsi
	movq	8(%rax), %rcx
	cmpq	%rsi, %rcx
	jb	.LBB132_10
# BB#9:                                 # %if.then13
                                        #   in Loop: Header=BB132_3 Depth=1
	addq	%rsi, %rsi
	movq	%rax, %rdi
	callq	varray_grow
	movq	%rax, ggc_pending_trees(%rip)
	movq	8(%rax), %rcx
.LBB132_10:                             # %if.end16
                                        #   in Loop: Header=BB132_3 Depth=1
	leaq	1(%rcx), %rdx
	movq	%rdx, 8(%rax)
	movq	%rbx, 32(%rax,%rcx,8)
.LBB132_11:                             # %for.inc
                                        #   in Loop: Header=BB132_3 Depth=1
	decq	%r15
	movq	$32, %rcx
	movq	%r15, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	addq	$8, %r13
	addq	$8, %r12
	testq	%rax, %rax
	jg	.LBB132_3
.LBB132_12:                             # %for.end
	movq	8(%rsp), %rax           # 8-byte Reload
	movq	32(%rax), %rbx
	testq	%rbx, %rbx
	je	.LBB132_23
# BB#13:
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB132_14:                             # %do.body.i
                                        # =>This Inner Loop Header: Depth=1
	movq	(%rbx), %r14
	testq	%r14, %r14
	je	.LBB132_17
# BB#15:                                # %land.lhs.true.i
                                        #   in Loop: Header=BB132_14 Depth=1
	movq	%r14, %rdi
	callq	ggc_set_mark
	testq	%r15, %rax
	jne	.LBB132_17
# BB#16:                                # %if.then.i
                                        #   in Loop: Header=BB132_14 Depth=1
	movq	%r14, %rdi
	callq	ggc_mark_rtx_children
.LBB132_17:                             # %do.body2.i
                                        #   in Loop: Header=BB132_14 Depth=1
	movq	16(%rbx), %r14
	testq	%r14, %r14
	je	.LBB132_22
# BB#18:                                # %land.lhs.true4.i
                                        #   in Loop: Header=BB132_14 Depth=1
	movq	%r14, %rdi
	callq	ggc_set_mark
	testq	%r15, %rax
	jne	.LBB132_22
# BB#19:                                # %do.body8.i
                                        #   in Loop: Header=BB132_14 Depth=1
	movq	ggc_pending_trees(%rip), %rax
	movq	(%rax), %rsi
	movq	8(%rax), %rcx
	cmpq	%rsi, %rcx
	jb	.LBB132_21
# BB#20:                                # %if.then10.i
                                        #   in Loop: Header=BB132_14 Depth=1
	addq	%rsi, %rsi
	movq	%rax, %rdi
	callq	varray_grow
	movq	%rax, ggc_pending_trees(%rip)
	movq	8(%rax), %rcx
.LBB132_21:                             # %if.end13.i
                                        #   in Loop: Header=BB132_14 Depth=1
	leaq	1(%rcx), %rdx
	movq	%rdx, 8(%rax)
	movq	%r14, 32(%rax,%rcx,8)
.LBB132_22:                             # %do.end17.i
                                        #   in Loop: Header=BB132_14 Depth=1
	movq	24(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB132_14
.LBB132_23:                             # %mark_sequence_stack.exit
	movq	8(%rsp), %rbx           # 8-byte Reload
	movq	24(%rbx), %r14
	testq	%r14, %r14
	je	.LBB132_28
# BB#24:                                # %land.lhs.true25
	movq	%r14, %rdi
	callq	ggc_set_mark
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB132_28
# BB#25:                                # %do.body29
	movq	ggc_pending_trees(%rip), %rax
	movq	(%rax), %rsi
	movq	8(%rax), %rcx
	cmpq	%rsi, %rcx
	jb	.LBB132_27
# BB#26:                                # %if.then33
	addq	%rsi, %rsi
	movq	%rax, %rdi
	callq	varray_grow
	movq	%rax, ggc_pending_trees(%rip)
	movq	8(%rax), %rcx
.LBB132_27:                             # %if.end37
	leaq	1(%rcx), %rdx
	movq	%rdx, 8(%rax)
	movq	%r14, 32(%rax,%rcx,8)
.LBB132_28:                             # %do.body46
	movq	8(%rbx), %rbx
	testq	%rbx, %rbx
	je	.LBB132_30
# BB#29:                                # %land.lhs.true49
	movq	%rbx, %rdi
	callq	ggc_set_mark
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB132_31
.LBB132_30:                             # %do.end54
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB132_31:                             # %if.then52
	movq	%rbx, %rdi
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	ggc_mark_rtx_children  # TAILCALL
.Ltmp1007:
	.size	mark_emit_status, .Ltmp1007-mark_emit_status
	.cfi_endproc

	.globl	init_emit_once
	.align	16, 0x90
	.type	init_emit_once,@function
init_emit_once:                         # @init_emit_once
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp1014:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp1015:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp1016:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp1017:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp1018:
	.cfi_def_cfa_offset 48
	subq	$128, %rsp
.Ltmp1019:
	.cfi_def_cfa_offset 176
.Ltmp1020:
	.cfi_offset %rbx, -48
.Ltmp1021:
	.cfi_offset %r12, -40
.Ltmp1022:
	.cfi_offset %r13, -32
.Ltmp1023:
	.cfi_offset %r14, -24
.Ltmp1024:
	.cfi_offset %r15, -16
	movq	%rdi, %rbx
	movabsq	$const_int_htab_hash, %rsi
	movabsq	$const_int_htab_eq, %rdx
	movq	$37, %rdi
	xorq	%rcx, %rcx
	xorq	%r14, %r14
	callq	htab_create
	movq	%rax, const_int_htab(%rip)
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	movq	%rax, %rdi
	callq	ggc_add_deletable_htab
	movabsq	$mem_attrs_htab_hash, %rsi
	movabsq	$mem_attrs_htab_eq, %rdx
	movq	$37, %rdi
	xorq	%rcx, %rcx
	callq	htab_create
	movq	%rax, mem_attrs_htab(%rip)
	movabsq	$mem_attrs_mark, %rdx
	xorq	%rsi, %rsi
	movq	%rax, %rdi
	callq	ggc_add_deletable_htab
	movabsq	$4294967295, %r12       # imm = 0xFFFFFFFF
	testq	%r12, %rbx
	jne	.LBB133_1
# BB#2:                                 # %entry
	movabsq	$1, %rax
	jmp	.LBB133_3
.LBB133_1:
	movabsq	$0, %rax
.LBB133_3:                              # %entry
	movl	class_narrowest_mode+4(%rip), %r13d
	movl	%eax, no_line_numbers(%rip)
	movl	%r14d, byte_mode(%rip)
	movl	%r14d, word_mode(%rip)
	testq	%r13, %r13
	je	.LBB133_12
# BB#4:                                 # %for.body.lr.ph
	movl	target_flags(%rip), %eax
	movq	$20, %rcx
	shrq	%cl, %rax
	andq	$32, %rax
	addq	$32, %rax
	xorq	%rdx, %rdx
	andq	%r12, %rax
	xorq	%rsi, %rsi
	movq	%r13, %rbx
	.align	16, 0x90
.LBB133_5:                              # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	$32, %rcx
	movq	%rbx, %rdi
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	movzbl	mode_bitsize(%rdi,%rdi), %ecx
	cmpq	$8, %rcx
	jne	.LBB133_8
# BB#6:                                 # %for.body
                                        #   in Loop: Header=BB133_5 Depth=1
	testq	%r12, %rsi
	jne	.LBB133_8
# BB#7:                                 # %if.then
                                        #   in Loop: Header=BB133_5 Depth=1
	movl	%ebx, byte_mode(%rip)
	movq	%rbx, %rsi
.LBB133_8:                              # %if.end
                                        #   in Loop: Header=BB133_5 Depth=1
	cmpq	%rax, %rcx
	jne	.LBB133_11
# BB#9:                                 # %if.end
                                        #   in Loop: Header=BB133_5 Depth=1
	testq	%r12, %rdx
	jne	.LBB133_11
# BB#10:                                # %if.then15
                                        #   in Loop: Header=BB133_5 Depth=1
	movl	%ebx, word_mode(%rip)
	movq	%rbx, %rdx
.LBB133_11:                             # %for.inc
                                        #   in Loop: Header=BB133_5 Depth=1
	movzbl	mode_wider_mode(%rdi), %ebx
	testq	%rbx, %rbx
	jne	.LBB133_5
.LBB133_12:                             # %for.end
	movl	class_narrowest_mode+8(%rip), %eax
	xorq	%r15, %r15
	testq	%rax, %rax
	je	.LBB133_21
# BB#13:
	movabsq	$0, %rbx
	movabsq	$1, %rdx
	movq	%rax, %rsi
	.align	16, 0x90
.LBB133_14:                             # %for.body23
                                        # =>This Inner Loop Header: Depth=1
	movq	$32, %rcx
	movq	%rsi, %rdi
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	movzbl	mode_bitsize(%rdi,%rdi), %eax
	cmpq	$64, %rax
	movq	%rbx, %rcx
	jne	.LBB133_16
# BB#15:                                # %for.body23
                                        #   in Loop: Header=BB133_14 Depth=1
	movq	%rdx, %rcx
.LBB133_16:                             # %for.body23
                                        #   in Loop: Header=BB133_14 Depth=1
	testq	%r12, %r15
	movq	%rbx, %rax
	jne	.LBB133_18
# BB#17:                                # %for.body23
                                        #   in Loop: Header=BB133_14 Depth=1
	movq	%rdx, %rax
.LBB133_18:                             # %for.body23
                                        #   in Loop: Header=BB133_14 Depth=1
	testq	%rax, %rcx
	jne	.LBB133_20
# BB#19:                                # %for.body23
                                        #   in Loop: Header=BB133_14 Depth=1
	movq	%r15, %rsi
.LBB133_20:                             # %for.body23
                                        #   in Loop: Header=BB133_14 Depth=1
	movq	%rsi, %r15
	movzbl	mode_wider_mode(%rdi), %esi
	testq	%rsi, %rsi
	jne	.LBB133_14
.LBB133_21:                             # %for.end38
	movl	target_flags(%rip), %eax
	andq	$33554432, %rax         # imm = 0x2000000
	movq	$20, %rcx
	movq	%rax, %rdi
	shrq	%cl, %rdi
	movq	$23, %rcx
	shrq	%cl, %rax
	movl	mode_class+16(%rax), %esi
	movq	$20, %r14
	addq	$32, %rdi
	xorq	%rdx, %rdx
	callq	mode_for_size
	movl	%eax, ptr_mode(%rip)
	movq	$59, %rdi
	xorq	%rsi, %rsi
	xorq	%rax, %rax
	callq	gen_rtx
	movq	%rax, global_rtl(%rip)
	movq	$69, %rdi
	xorq	%rsi, %rsi
	xorq	%rax, %rax
	callq	gen_rtx
	movl	target_flags(%rip), %esi
	movq	%rax, global_rtl+8(%rip)
	movq	$25, %rcx
	shrq	%cl, %rsi
	andq	$1, %rsi
	orq	$4, %rsi
	movq	$61, %rdi
	movq	$7, %rdx
	movq	$7, %rbx
	callq	gen_rtx_fmt_i0
	movl	%ebx, 16(%rax)
	movl	target_flags(%rip), %esi
	movq	%rax, global_rtl+16(%rip)
	movq	$25, %rcx
	shrq	%cl, %rsi
	andq	$1, %rsi
	orq	$4, %rsi
	movq	$61, %rdi
	movq	$20, %rdx
	callq	gen_rtx_fmt_i0
	movl	%r14d, 16(%rax)
	movq	%rax, global_rtl+24(%rip)
	cmpq	$0, global_rtl+32(%rip)
	jne	.LBB133_23
# BB#22:                                # %if.then60
	movl	target_flags(%rip), %esi
	movq	$25, %rcx
	shrq	%cl, %rsi
	andq	$1, %rsi
	orq	$4, %rsi
	movq	$61, %rdi
	movq	$6, %rdx
	movq	$6, %rbx
	callq	gen_rtx_fmt_i0
	movl	%ebx, 16(%rax)
	movq	%rax, global_rtl+32(%rip)
.LBB133_23:                             # %if.end65
	cmpq	$0, global_rtl+40(%rip)
	jne	.LBB133_25
# BB#24:                                # %if.then68
	movl	target_flags(%rip), %esi
	movq	$25, %rcx
	shrq	%cl, %rsi
	andq	$1, %rsi
	orq	$4, %rsi
	movq	$61, %rdi
	movq	$16, %rdx
	movq	$16, %rbx
	callq	gen_rtx_fmt_i0
	movl	%ebx, 16(%rax)
	movq	%rax, global_rtl+40(%rip)
.LBB133_25:                             # %if.end73
	movl	target_flags(%rip), %esi
	movq	$25, %rcx
	shrq	%cl, %rsi
	andq	$1, %rsi
	orq	$4, %rsi
	movq	$61, %rdi
	movq	$53, %rdx
	movq	$53, %rbx
	callq	gen_rtx_fmt_i0
	movl	%ebx, 16(%rax)
	movl	target_flags(%rip), %esi
	movq	%rax, global_rtl+48(%rip)
	movabsq	$global_rtl, %r14
	movq	$25, %rcx
	shrq	%cl, %rsi
	andq	$1, %rsi
	orq	$4, %rsi
	movq	$61, %rdi
	movq	$54, %rdx
	movq	$54, %rbx
	callq	gen_rtx_fmt_i0
	movl	%ebx, 16(%rax)
	movl	target_flags(%rip), %esi
	movq	%rax, global_rtl+56(%rip)
	movq	$25, %rcx
	shrq	%cl, %rsi
	andq	$1, %rsi
	orq	$4, %rsi
	movq	$61, %rdi
	movq	$55, %rdx
	movq	$55, %rbx
	callq	gen_rtx_fmt_i0
	movl	%ebx, 16(%rax)
	movl	target_flags(%rip), %esi
	movq	%rax, global_rtl+64(%rip)
	movq	$25, %rcx
	shrq	%cl, %rsi
	andq	$1, %rsi
	orq	$4, %rsi
	movq	$61, %rdi
	movq	$56, %rdx
	movq	$56, %rbx
	callq	gen_rtx_fmt_i0
	movl	%ebx, 16(%rax)
	movl	target_flags(%rip), %esi
	movq	%rax, global_rtl+72(%rip)
	movq	$25, %rcx
	shrq	%cl, %rsi
	andq	$1, %rsi
	orq	$4, %rsi
	movq	$61, %rdi
	movq	$57, %rdx
	movq	$57, %rbx
	callq	gen_rtx_fmt_i0
	movl	%ebx, 16(%rax)
	movq	%rax, global_rtl+80(%rip)
	movq	$11, %rsi
	movq	%r14, %rdi
	callq	ggc_add_rtx_root
	movq	$-64, %rbx
	.align	16, 0x90
.LBB133_26:                             # %for.body97
                                        # =>This Inner Loop Header: Depth=1
	movq	$54, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	callq	gen_rtx_fmt_w
	movq	%rax, const_int_rtx+512(,%rbx,8)
	incq	%rbx
	cmpq	$65, %rbx
	jne	.LBB133_26
# BB#27:                                # %for.end103
	movabsq	$const_int_rtx, %rdi
	movq	$129, %rsi
	callq	ggc_add_rtx_root
	movq	const_int_rtx+520(%rip), %rax
	movq	%rax, const_true_rtx(%rip)
	movabsq	$.L.str3, %rsi
	leaq	104(%rsp), %rdi
	movq	%r15, %rdx
	callq	ereal_atof
	movq	120(%rsp), %rax
	movq	104(%rsp), %rcx
	movq	112(%rsp), %rdx
	movq	%rax, dconst0+16(%rip)
	movq	%rdx, dconst0+8(%rip)
	movq	%rcx, dconst0(%rip)
	movabsq	$.L.str4, %rsi
	leaq	80(%rsp), %rdi
	movq	%r15, %rdx
	callq	ereal_atof
	movq	96(%rsp), %rax
	movq	80(%rsp), %rcx
	movq	88(%rsp), %rdx
	movq	%rax, dconst1+16(%rip)
	movq	%rdx, dconst1+8(%rip)
	movq	%rcx, dconst1(%rip)
	movabsq	$.L.str5, %rsi
	leaq	56(%rsp), %rdi
	movq	%r15, %rdx
	callq	ereal_atof
	movq	72(%rsp), %rax
	movq	56(%rsp), %rcx
	movq	64(%rsp), %rdx
	movq	%rax, dconst2+16(%rip)
	movq	%rdx, dconst2+8(%rip)
	movq	%rcx, dconst2(%rip)
	movabsq	$.L.str6, %rsi
	leaq	32(%rsp), %rdi
	movq	%r15, %rdx
	callq	ereal_atof
	movq	48(%rsp), %rax
	movq	%rax, dconstm1+16(%rip)
	movq	32(%rsp), %rax
	movq	40(%rsp), %rcx
	movl	class_narrowest_mode+12(%rip), %r15d
	movq	%rcx, dconstm1+8(%rip)
	movq	%rax, dconstm1(%rip)
	xorq	%rbx, %rbx
	.align	16, 0x90
.LBB133_28:                             # %for.body110
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB133_29 Depth 2
                                        #     Child Loop BB133_36 Depth 2
                                        #     Child Loop BB133_38 Depth 2
	movl	class_narrowest_mode+8(%rip), %eax
	testq	%rax, %rax
	movq	%rax, %r14
	je	.LBB133_35
	.align	16, 0x90
.LBB133_29:                             # %for.body114
                                        #   Parent Loop BB133_28 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	$55, %rdi
	callq	rtx_alloc
	movq	$0, 24(%rsp)
	movq	$0, 16(%rsp)
	movq	$0, 8(%rsp)
	movq	%rbx, %rcx
	andq	%r12, %rcx
	cmpq	$1, %rcx
	jne	.LBB133_30
# BB#32:                                # %cond.true122
                                        #   in Loop: Header=BB133_29 Depth=2
	movq	dconst1+16(%rip), %rcx
	movq	dconst1+8(%rip), %rdx
	movq	dconst1(%rip), %rsi
	jmp	.LBB133_34
	.align	16, 0x90
.LBB133_30:                             # %for.body114
                                        #   in Loop: Header=BB133_29 Depth=2
	testq	%r12, %rbx
	jne	.LBB133_33
# BB#31:                                # %cond.true
                                        #   in Loop: Header=BB133_29 Depth=2
	movq	dconst0+16(%rip), %rcx
	movq	dconst0+8(%rip), %rdx
	movq	dconst0(%rip), %rsi
	jmp	.LBB133_34
	.align	16, 0x90
.LBB133_33:                             # %cond.false123
                                        #   in Loop: Header=BB133_29 Depth=2
	movq	dconst2+16(%rip), %rcx
	movq	dconst2+8(%rip), %rdx
	movq	dconst2(%rip), %rsi
.LBB133_34:                             # %cond.end124
                                        #   in Loop: Header=BB133_29 Depth=2
	movq	%rcx, 24(%rsp)
	movq	%rdx, 16(%rsp)
	movq	%rsi, 8(%rsp)
	movq	24(%rsp), %rcx
	movq	%rcx, 32(%rax)
	movq	8(%rsp), %rcx
	movq	16(%rsp), %rdx
	movq	%rdx, 24(%rax)
	movq	%rcx, 16(%rax)
	movq	$0, 8(%rax)
	movb	%r14b, 2(%rax)
	movq	$32, %rcx
	shlq	%cl, %r14
	movq	$32, %rcx
	sarq	%cl, %r14
	imulq	$472, %rbx, %rcx        # imm = 0x1D8
	movq	%rax, const_tiny_rtx(%rcx,%r14,8)
	movzbl	mode_wider_mode(%r14), %r14d
	testq	%r14, %r14
	jne	.LBB133_29
.LBB133_35:                             # %gen_rtx_CONST_INT.exit
                                        #   in Loop: Header=BB133_28 Depth=1
	movq	const_int_rtx+512(,%rbx,8), %rax
	imulq	$472, %rbx, %rdx        # imm = 0x1D8
	testq	%r13, %r13
	movq	%rax, const_tiny_rtx(%rdx)
	movq	%r13, %rsi
	je	.LBB133_37
	.align	16, 0x90
.LBB133_36:                             # %gen_rtx_CONST_INT.exit343
                                        #   Parent Loop BB133_28 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	$32, %rcx
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	movq	%rax, const_tiny_rtx(%rdx,%rsi,8)
	movzbl	mode_wider_mode(%rsi), %esi
	testq	%rsi, %rsi
	jne	.LBB133_36
.LBB133_37:                             # %for.end156
                                        #   in Loop: Header=BB133_28 Depth=1
	testq	%r12, %r15
	movq	%r15, %rsi
	je	.LBB133_39
	.align	16, 0x90
.LBB133_38:                             # %gen_rtx_CONST_INT.exit356
                                        #   Parent Loop BB133_28 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	$32, %rcx
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	movq	%rax, const_tiny_rtx(%rdx,%rsi,8)
	movzbl	mode_wider_mode(%rsi), %esi
	testq	%rsi, %rsi
	jne	.LBB133_38
.LBB133_39:                             # %for.inc172
                                        #   in Loop: Header=BB133_28 Depth=1
	incq	%rbx
	cmpq	$3, %rbx
	jne	.LBB133_28
# BB#40:                                # %for.end174
	movl	class_narrowest_mode+28(%rip), %r14d
	jmp	.LBB133_41
	.align	16, 0x90
.LBB133_48:                             # %gen_const_vector_0.exit
                                        #   in Loop: Header=BB133_41 Depth=1
	movq	$56, %rdi
	movq	%r14, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_E
	movzbl	mode_wider_mode(%r15), %r14d
	movq	%rax, const_tiny_rtx(,%r15,8)
.LBB133_41:                             # %for.end174
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB133_47 Depth 2
	testq	%r14, %r14
	je	.LBB133_49
# BB#42:                                # %for.body178
                                        #   in Loop: Header=BB133_41 Depth=1
	movq	$32, %rcx
	movq	%r14, %r15
	shlq	%cl, %r15
	movq	$32, %rcx
	sarq	%cl, %r15
	movzbl	mode_unit_size(%r15), %ecx
	testq	%rcx, %rcx
	movabsq	$0, %r13
	je	.LBB133_44
# BB#43:                                # %cond.false.i
                                        #   in Loop: Header=BB133_41 Depth=1
	movzbl	mode_size(%r15), %eax
	xorq	%rdx, %rdx
	divq	%rcx
	movq	%rax, %r13
.LBB133_44:                             # %cond.end.i
                                        #   in Loop: Header=BB133_41 Depth=1
	movq	%r13, %rdi
	callq	rtvec_alloc
	movslq	inner_mode_array(,%r15,4), %rdx
	movq	const_tiny_rtx(,%rdx,8), %rcx
	testq	%rcx, %rcx
	je	.LBB133_78
# BB#45:                                # %for.cond.preheader.i
                                        #   in Loop: Header=BB133_41 Depth=1
	testq	%r12, %r13
	je	.LBB133_48
# BB#46:                                # %for.body.i.preheader
                                        #   in Loop: Header=BB133_41 Depth=1
	movq	%rcx, 8(%rax)
	movq	%r13, %rcx
	andq	%r12, %rcx
	movq	$2, %rsi
	cmpq	$1, %rcx
	jbe	.LBB133_48
	.align	16, 0x90
.LBB133_47:                             # %for.body.for.body_crit_edge.i
                                        #   Parent Loop BB133_41 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	const_tiny_rtx(,%rdx,8), %rcx
	movq	%rcx, (%rax,%rsi,8)
	movq	$32, %rcx
	movq	%rsi, %rdi
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	movq	$32, %rcx
	movq	%r13, %rbx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	incq	%rsi
	cmpq	%rbx, %rdi
	jl	.LBB133_47
	jmp	.LBB133_48
.LBB133_49:                             # %for.end186
	movl	class_narrowest_mode+32(%rip), %r14d
	jmp	.LBB133_50
	.align	16, 0x90
.LBB133_77:                             # %gen_const_vector_0.exit385
                                        #   in Loop: Header=BB133_50 Depth=1
	movq	$56, %rdi
	movq	%r14, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_E
	movzbl	mode_wider_mode(%r15), %r14d
	movq	%rax, const_tiny_rtx(,%r15,8)
.LBB133_50:                             # %for.end186
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB133_76 Depth 2
	testq	%r14, %r14
	je	.LBB133_51
# BB#71:                                # %for.body190
                                        #   in Loop: Header=BB133_50 Depth=1
	movq	$32, %rcx
	movq	%r14, %r15
	shlq	%cl, %r15
	movq	$32, %rcx
	sarq	%cl, %r15
	movzbl	mode_unit_size(%r15), %ecx
	testq	%rcx, %rcx
	movabsq	$0, %r13
	je	.LBB133_73
# BB#72:                                # %cond.false.i366
                                        #   in Loop: Header=BB133_50 Depth=1
	movzbl	mode_size(%r15), %eax
	xorq	%rdx, %rdx
	divq	%rcx
	movq	%rax, %r13
.LBB133_73:                             # %cond.end.i373
                                        #   in Loop: Header=BB133_50 Depth=1
	movq	%r13, %rdi
	callq	rtvec_alloc
	movslq	inner_mode_array(,%r15,4), %rdx
	movq	const_tiny_rtx(,%rdx,8), %rcx
	testq	%rcx, %rcx
	je	.LBB133_78
# BB#74:                                # %for.cond.preheader.i375
                                        #   in Loop: Header=BB133_50 Depth=1
	testq	%r12, %r13
	je	.LBB133_77
# BB#75:                                # %for.body.i381.preheader
                                        #   in Loop: Header=BB133_50 Depth=1
	movq	%rcx, 8(%rax)
	movq	%r13, %rcx
	andq	%r12, %rcx
	movq	$2, %rsi
	cmpq	$1, %rcx
	jbe	.LBB133_77
	.align	16, 0x90
.LBB133_76:                             # %for.body.for.body_crit_edge.i383
                                        #   Parent Loop BB133_50 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	const_tiny_rtx(,%rdx,8), %rcx
	movq	%rcx, (%rax,%rsi,8)
	movq	$32, %rcx
	movq	%rsi, %rdi
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	movq	$32, %rcx
	movq	%r13, %rbx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	incq	%rsi
	cmpq	%rbx, %rdi
	jl	.LBB133_76
	jmp	.LBB133_77
.LBB133_51:                             # %for.cond199.preheader
	movl	mode_class+208(%rip), %ecx
	movq	const_int_rtx+512(%rip), %rax
	cmpq	$4, %rcx
	jne	.LBB133_53
# BB#52:                                # %if.then207
	movq	%rax, const_tiny_rtx+416(%rip)
.LBB133_53:                             # %for.inc211
	movl	mode_class+212(%rip), %ecx
	cmpq	$4, %rcx
	jne	.LBB133_55
# BB#54:                                # %if.then207.1
	movq	%rax, const_tiny_rtx+424(%rip)
.LBB133_55:                             # %for.inc211.1
	movl	mode_class+216(%rip), %ecx
	cmpq	$4, %rcx
	jne	.LBB133_57
# BB#56:                                # %if.then207.2
	movq	%rax, const_tiny_rtx+432(%rip)
.LBB133_57:                             # %for.inc211.2
	movl	mode_class+220(%rip), %ecx
	cmpq	$4, %rcx
	jne	.LBB133_59
# BB#58:                                # %if.then207.3
	movq	%rax, const_tiny_rtx+440(%rip)
.LBB133_59:                             # %for.inc211.3
	movl	mode_class+224(%rip), %ecx
	cmpq	$4, %rcx
	jne	.LBB133_61
# BB#60:                                # %if.then207.4
	movq	%rax, const_tiny_rtx+448(%rip)
.LBB133_61:                             # %for.inc211.4
	movl	mode_class+228(%rip), %ecx
	cmpq	$4, %rcx
	jne	.LBB133_63
# BB#62:                                # %if.then207.5
	movq	%rax, const_tiny_rtx+456(%rip)
.LBB133_63:                             # %for.inc211.5
	movl	mode_class+232(%rip), %ecx
	cmpq	$4, %rcx
	jne	.LBB133_65
# BB#64:                                # %if.then207.6
	movq	%rax, const_tiny_rtx+464(%rip)
.LBB133_65:                             # %for.inc211.6
	movq	const_int_rtx+520(%rip), %rcx
	movq	%rax, const_tiny_rtx+8(%rip)
	movabsq	$const_tiny_rtx, %rdi
	movq	%rcx, const_tiny_rtx+480(%rip)
	movq	$177, %rsi
	callq	ggc_add_rtx_root
	movabsq	$const_true_rtx, %rdi
	movq	$1, %rsi
	callq	ggc_add_rtx_root
	movl	target_flags(%rip), %eax
	movq	$0, struct_value_rtx(%rip)
	movq	$0, struct_value_incoming_rtx(%rip)
	andq	$33554432, %rax         # imm = 0x2000000
	movq	$25, %rcx
	shrq	%cl, %rax
	movq	%rax, %rsi
	orq	$4, %rsi
	testq	%rax, %rax
	movq	$39, %rbx
	jne	.LBB133_67
# BB#66:                                # %for.inc211.6
	movq	$2, %rbx
.LBB133_67:                             # %for.inc211.6
	movq	$61, %rdi
	movq	%rbx, %rdx
	callq	gen_rtx_fmt_i0
	movl	%ebx, 16(%rax)
	movl	target_flags(%rip), %ecx
	movq	%rax, static_chain_rtx(%rip)
	movq	%rax, static_chain_incoming_rtx(%rip)
	testq	$33554432, %rcx         # imm = 0x2000000
	jne	.LBB133_70
# BB#68:                                # %for.inc211.6
	movl	flag_pic(%rip), %eax
	testq	%r12, %rax
	je	.LBB133_70
# BB#69:                                # %lor.end239
	movq	$61, %rdi
	movq	$4, %rsi
	movq	$3, %rdx
	movq	$3, %rbx
	callq	gen_rtx_fmt_i0
	movl	%ebx, 16(%rax)
	movq	%rax, pic_offset_table_rtx(%rip)
.LBB133_70:                             # %if.end242
	movabsq	$pic_offset_table_rtx, %rdi
	movq	$1, %rsi
	callq	ggc_add_rtx_root
	movabsq	$struct_value_rtx, %rdi
	movq	$1, %rsi
	callq	ggc_add_rtx_root
	movabsq	$struct_value_incoming_rtx, %rdi
	movq	$1, %rsi
	callq	ggc_add_rtx_root
	movabsq	$static_chain_rtx, %rdi
	movq	$1, %rsi
	callq	ggc_add_rtx_root
	movabsq	$static_chain_incoming_rtx, %rdi
	movq	$1, %rsi
	callq	ggc_add_rtx_root
	movabsq	$return_address_pointer_rtx, %rdi
	movq	$1, %rsi
	callq	ggc_add_rtx_root
	addq	$128, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB133_78:                             # %if.then.i359
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.gen_const_vector_0, %rcx
	movq	$4941, %rsi             # imm = 0x134D
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp1025:
	.size	init_emit_once, .Ltmp1025-init_emit_once
	.cfi_endproc

	.align	16, 0x90
	.type	const_int_htab_hash,@function
const_int_htab_hash:                    # @const_int_htab_hash
	.cfi_startproc
# BB#0:                                 # %entry
	movq	8(%rdi), %rax
	retq
.Ltmp1026:
	.size	const_int_htab_hash, .Ltmp1026-const_int_htab_hash
	.cfi_endproc

	.align	16, 0x90
	.type	const_int_htab_eq,@function
const_int_htab_eq:                      # @const_int_htab_eq
	.cfi_startproc
# BB#0:                                 # %entry
	movq	8(%rdi), %rax
	movq	(%rsi), %rcx
	cmpq	%rcx, %rax
	jne	.LBB135_1
# BB#2:                                 # %entry
	movabsq	$1, %rax
	retq
.LBB135_1:
	movabsq	$0, %rax
	retq
.Ltmp1027:
	.size	const_int_htab_eq, .Ltmp1027-const_int_htab_eq
	.cfi_endproc

	.align	16, 0x90
	.type	mem_attrs_htab_hash,@function
mem_attrs_htab_hash:                    # @mem_attrs_htab_hash
	.cfi_startproc
# BB#0:                                 # %entry
	movl	32(%rdi), %eax
	imulq	$1000, %rax, %rax       # imm = 0x3E8
	movabsq	$4294967288, %rdx       # imm = 0xFFFFFFF8
	andq	%rax, %rdx
	movq	(%rdi), %rcx
	movq	16(%rdi), %rsi
	xorq	%rdx, %rcx
	xorq	%rdx, %rdx
	testq	%rsi, %rsi
	movabsq	$0, %rax
	je	.LBB136_2
# BB#1:                                 # %cond.true
	movq	8(%rsi), %rax
	imulq	$50000, %rax, %rax      # imm = 0xC350
.LBB136_2:                              # %cond.end
	movq	24(%rdi), %rsi
	xorq	%rax, %rcx
	testq	%rsi, %rsi
	je	.LBB136_4
# BB#3:                                 # %cond.true5
	movq	8(%rsi), %rax
	imulq	$2500000, %rax, %rdx    # imm = 0x2625A0
.LBB136_4:                              # %cond.end11
	movq	8(%rdi), %rax
	xorq	%rdx, %rcx
	xorq	%rcx, %rax
	retq
.Ltmp1028:
	.size	mem_attrs_htab_hash, .Ltmp1028-mem_attrs_htab_hash
	.cfi_endproc

	.align	16, 0x90
	.type	mem_attrs_htab_eq,@function
mem_attrs_htab_eq:                      # @mem_attrs_htab_eq
	.cfi_startproc
# BB#0:                                 # %entry
	movq	(%rdi), %rcx
	movq	(%rsi), %rdx
	xorq	%rax, %rax
	cmpq	%rdx, %rcx
	jne	.LBB137_7
# BB#1:                                 # %land.lhs.true
	movq	8(%rdi), %rcx
	movq	8(%rsi), %rdx
	cmpq	%rdx, %rcx
	jne	.LBB137_7
# BB#2:                                 # %land.lhs.true4
	movq	16(%rdi), %rcx
	movq	16(%rsi), %rdx
	cmpq	%rdx, %rcx
	jne	.LBB137_7
# BB#3:                                 # %land.lhs.true7
	movq	24(%rdi), %rcx
	movq	24(%rsi), %rdx
	cmpq	%rdx, %rcx
	jne	.LBB137_7
# BB#4:                                 # %land.rhs
	movl	32(%rdi), %eax
	movl	32(%rsi), %ecx
	cmpq	%rcx, %rax
	jne	.LBB137_5
# BB#6:                                 # %land.rhs
	movabsq	$1, %rax
.LBB137_7:                              # %land.end
	retq
.LBB137_5:
	movabsq	$0, %rax
	retq
.Ltmp1029:
	.size	mem_attrs_htab_eq, .Ltmp1029-mem_attrs_htab_eq
	.cfi_endproc

	.align	16, 0x90
	.type	mem_attrs_mark,@function
mem_attrs_mark:                         # @mem_attrs_mark
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp1034:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp1035:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp1036:
	.cfi_def_cfa_offset 32
.Ltmp1037:
	.cfi_offset %rbx, -32
.Ltmp1038:
	.cfi_offset %r14, -24
.Ltmp1039:
	.cfi_offset %r15, -16
	movq	%rdi, %r15
	movq	8(%r15), %r14
	testq	%r14, %r14
	je	.LBB138_5
# BB#1:                                 # %land.lhs.true
	movq	%r14, %rdi
	callq	ggc_set_mark
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB138_5
# BB#2:                                 # %do.body4
	movq	ggc_pending_trees(%rip), %rax
	movq	(%rax), %rsi
	movq	8(%rax), %rcx
	cmpq	%rsi, %rcx
	jb	.LBB138_4
# BB#3:                                 # %if.then6
	addq	%rsi, %rsi
	movq	%rax, %rdi
	callq	varray_grow
	movq	%rax, ggc_pending_trees(%rip)
	movq	8(%rax), %rcx
.LBB138_4:                              # %if.end
	leaq	1(%rcx), %rdx
	movq	%rdx, 8(%rax)
	movq	%r14, 32(%rax,%rcx,8)
.LBB138_5:                              # %if.end12
	movq	16(%r15), %rbx
	testq	%rbx, %rbx
	je	.LBB138_8
# BB#6:                                 # %land.lhs.true18
	movq	%rbx, %rdi
	callq	ggc_set_mark
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB138_8
# BB#7:                                 # %if.then21
	movq	%rbx, %rdi
	callq	ggc_mark_rtx_children
.LBB138_8:                              # %if.end24
	movq	24(%r15), %rbx
	testq	%rbx, %rbx
	je	.LBB138_10
# BB#9:                                 # %land.lhs.true31
	movq	%rbx, %rdi
	callq	ggc_set_mark
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB138_11
.LBB138_10:                             # %if.end37
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.LBB138_11:                             # %if.then34
	movq	%rbx, %rdi
	popq	%rbx
	popq	%r14
	popq	%r15
	jmp	ggc_mark_rtx_children  # TAILCALL
.Ltmp1040:
	.size	mem_attrs_mark, .Ltmp1040-mem_attrs_mark
	.cfi_endproc

	.globl	force_line_numbers
	.align	16, 0x90
	.type	force_line_numbers,@function
force_line_numbers:                     # @force_line_numbers
	.cfi_startproc
# BB#0:                                 # %entry
	movl	no_line_numbers(%rip), %eax
	xorq	%rcx, %rcx
	testq	%rax, %rax
	movl	%ecx, no_line_numbers(%rip)
	je	.LBB139_2
# BB#1:                                 # %if.then
	movq	cfun(%rip), %rcx
	movq	24(%rcx), %rcx
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	movl	%edx, 44(%rcx)
.LBB139_2:                              # %if.end
	retq
.Ltmp1041:
	.size	force_line_numbers, .Ltmp1041-force_line_numbers
	.cfi_endproc

	.globl	restore_line_number_status
	.align	16, 0x90
	.type	restore_line_number_status,@function
restore_line_number_status:             # @restore_line_number_status
	.cfi_startproc
# BB#0:                                 # %entry
	movl	%edi, no_line_numbers(%rip)
	retq
.Ltmp1042:
	.size	restore_line_number_status, .Ltmp1042-restore_line_number_status
	.cfi_endproc

	.type	split_branch_probability,@object # @split_branch_probability
	.data
	.globl	split_branch_probability
	.align	4
split_branch_probability:
	.long	4294967295              # 0xffffffff
	.size	split_branch_probability, 4

	.type	const_int_rtx,@object   # @const_int_rtx
	.comm	const_int_rtx,1032,16
	.type	const_int_htab,@object  # @const_int_htab
	.local	const_int_htab
	.comm	const_int_htab,8,8
	.type	global_rtl,@object      # @global_rtl
	.comm	global_rtl,88,16
	.type	pic_offset_table_rtx,@object # @pic_offset_table_rtx
	.comm	pic_offset_table_rtx,8,8
	.type	.L.str,@object          # @.str
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.asciz	"emit-rtl.c"
	.size	.L.str, 11

	.type	.L__FUNCTION__.gen_rtx_SUBREG,@object # @__FUNCTION__.gen_rtx_SUBREG
.L__FUNCTION__.gen_rtx_SUBREG:
	.asciz	"gen_rtx_SUBREG"
	.size	.L__FUNCTION__.gen_rtx_SUBREG, 15

	.type	.L__FUNCTION__.gen_rtx,@object # @__FUNCTION__.gen_rtx
.L__FUNCTION__.gen_rtx:
	.asciz	"gen_rtx"
	.size	.L__FUNCTION__.gen_rtx, 8

	.type	.L__FUNCTION__.gen_reg_rtx,@object # @__FUNCTION__.gen_reg_rtx
.L__FUNCTION__.gen_reg_rtx:
	.asciz	"gen_reg_rtx"
	.size	.L__FUNCTION__.gen_reg_rtx, 12

	.type	.L__FUNCTION__.mark_user_reg,@object # @__FUNCTION__.mark_user_reg
.L__FUNCTION__.mark_user_reg:
	.asciz	"mark_user_reg"
	.size	.L__FUNCTION__.mark_user_reg, 14

	.type	last_label_num,@object  # @last_label_num
	.local	last_label_num
	.comm	last_label_num,4,4
	.type	label_num,@object       # @label_num
	.data
	.align	4
label_num:
	.long	1                       # 0x1
	.size	label_num, 4

	.type	base_label_num,@object  # @base_label_num
	.local	base_label_num
	.comm	base_label_num,4,4
	.type	.L__FUNCTION__.subreg_hard_regno,@object # @__FUNCTION__.subreg_hard_regno
	.section	.rodata.str1.1,"aMS",@progbits,1
.L__FUNCTION__.subreg_hard_regno:
	.asciz	"subreg_hard_regno"
	.size	.L__FUNCTION__.subreg_hard_regno, 18

	.type	.L__FUNCTION__.gen_lowpart_common,@object # @__FUNCTION__.gen_lowpart_common
.L__FUNCTION__.gen_lowpart_common:
	.asciz	"gen_lowpart_common"
	.size	.L__FUNCTION__.gen_lowpart_common, 19

	.type	.L.str1,@object         # @.str1
.L.str1:
	.asciz	"can't access imaginary part of complex value in hard register"
	.size	.L.str1, 62

	.type	.L__FUNCTION__.subreg_realpart_p,@object # @__FUNCTION__.subreg_realpart_p
.L__FUNCTION__.subreg_realpart_p:
	.asciz	"subreg_realpart_p"
	.size	.L__FUNCTION__.subreg_realpart_p, 18

	.type	.L__FUNCTION__.gen_lowpart,@object # @__FUNCTION__.gen_lowpart
.L__FUNCTION__.gen_lowpart:
	.asciz	"gen_lowpart"
	.size	.L__FUNCTION__.gen_lowpart, 12

	.type	.L__FUNCTION__.gen_highpart,@object # @__FUNCTION__.gen_highpart
.L__FUNCTION__.gen_highpart:
	.asciz	"gen_highpart"
	.size	.L__FUNCTION__.gen_highpart, 13

	.type	.L__FUNCTION__.gen_highpart_mode,@object # @__FUNCTION__.gen_highpart_mode
.L__FUNCTION__.gen_highpart_mode:
	.asciz	"gen_highpart_mode"
	.size	.L__FUNCTION__.gen_highpart_mode, 18

	.type	.L__FUNCTION__.subreg_highpart_offset,@object # @__FUNCTION__.subreg_highpart_offset
.L__FUNCTION__.subreg_highpart_offset:
	.asciz	"subreg_highpart_offset"
	.size	.L__FUNCTION__.subreg_highpart_offset, 23

	.type	.L__FUNCTION__.constant_subword,@object # @__FUNCTION__.constant_subword
.L__FUNCTION__.constant_subword:
	.asciz	"constant_subword"
	.size	.L__FUNCTION__.constant_subword, 17

	.type	word_mode,@object       # @word_mode
	.comm	word_mode,4,4
	.type	.L__FUNCTION__.operand_subword,@object # @__FUNCTION__.operand_subword
.L__FUNCTION__.operand_subword:
	.asciz	"operand_subword"
	.size	.L__FUNCTION__.operand_subword, 16

	.type	.L__FUNCTION__.operand_subword_force,@object # @__FUNCTION__.operand_subword_force
.L__FUNCTION__.operand_subword_force:
	.asciz	"operand_subword_force"
	.size	.L__FUNCTION__.operand_subword_force, 22

	.type	const_tiny_rtx,@object  # @const_tiny_rtx
	.comm	const_tiny_rtx,1416,16
	.type	.L__FUNCTION__.set_mem_attributes,@object # @__FUNCTION__.set_mem_attributes
.L__FUNCTION__.set_mem_attributes:
	.asciz	"set_mem_attributes"
	.size	.L__FUNCTION__.set_mem_attributes, 19

	.type	.L__FUNCTION__.copy_most_rtx,@object # @__FUNCTION__.copy_most_rtx
.L__FUNCTION__.copy_most_rtx:
	.asciz	"copy_most_rtx"
	.size	.L__FUNCTION__.copy_most_rtx, 14

	.type	.L__FUNCTION__.set_last_insn,@object # @__FUNCTION__.set_last_insn
.L__FUNCTION__.set_last_insn:
	.asciz	"set_last_insn"
	.size	.L__FUNCTION__.set_last_insn, 14

	.type	.L.str2,@object         # @.str2
.L.str2:
	.asciz	"Renumbering insn %d to %d\n"
	.size	.L.str2, 27

	.type	.L__FUNCTION__.try_split,@object # @__FUNCTION__.try_split
.L__FUNCTION__.try_split:
	.asciz	"try_split"
	.size	.L__FUNCTION__.try_split, 10

	.type	.L__FUNCTION__.add_insn_after,@object # @__FUNCTION__.add_insn_after
.L__FUNCTION__.add_insn_after:
	.asciz	"add_insn_after"
	.size	.L__FUNCTION__.add_insn_after, 15

	.type	.L__FUNCTION__.add_insn_before,@object # @__FUNCTION__.add_insn_before
.L__FUNCTION__.add_insn_before:
	.asciz	"add_insn_before"
	.size	.L__FUNCTION__.add_insn_before, 16

	.type	.L__FUNCTION__.remove_insn,@object # @__FUNCTION__.remove_insn
.L__FUNCTION__.remove_insn:
	.asciz	"remove_insn"
	.size	.L__FUNCTION__.remove_insn, 12

	.type	.L__FUNCTION__.remove_unnecessary_notes,@object # @__FUNCTION__.remove_unnecessary_notes
.L__FUNCTION__.remove_unnecessary_notes:
	.asciz	"remove_unnecessary_notes"
	.size	.L__FUNCTION__.remove_unnecessary_notes, 25

	.type	no_line_numbers,@object # @no_line_numbers
	.local	no_line_numbers
	.comm	no_line_numbers,4,4
	.type	.L__FUNCTION__.emit_insns_after,@object # @__FUNCTION__.emit_insns_after
.L__FUNCTION__.emit_insns_after:
	.asciz	"emit_insns_after"
	.size	.L__FUNCTION__.emit_insns_after, 17

	.type	.L__FUNCTION__.set_unique_reg_note,@object # @__FUNCTION__.set_unique_reg_note
.L__FUNCTION__.set_unique_reg_note:
	.asciz	"set_unique_reg_note"
	.size	.L__FUNCTION__.set_unique_reg_note, 20

	.type	.L__FUNCTION__.push_to_full_sequence,@object # @__FUNCTION__.push_to_full_sequence
.L__FUNCTION__.push_to_full_sequence:
	.asciz	"push_to_full_sequence"
	.size	.L__FUNCTION__.push_to_full_sequence, 22

	.type	sequence_result,@object # @sequence_result
	.local	sequence_result
	.comm	sequence_result,40,16
	.type	copy_insn_n_scratches,@object # @copy_insn_n_scratches
	.local	copy_insn_n_scratches
	.comm	copy_insn_n_scratches,4,4
	.type	copy_insn_scratch_in,@object # @copy_insn_scratch_in
	.local	copy_insn_scratch_in
	.comm	copy_insn_scratch_in,240,16
	.type	copy_insn_scratch_out,@object # @copy_insn_scratch_out
	.local	copy_insn_scratch_out
	.comm	copy_insn_scratch_out,240,16
	.type	orig_asm_constraints_vector,@object # @orig_asm_constraints_vector
	.local	orig_asm_constraints_vector
	.comm	orig_asm_constraints_vector,8,8
	.type	copy_asm_constraints_vector,@object # @copy_asm_constraints_vector
	.local	copy_asm_constraints_vector
	.comm	copy_asm_constraints_vector,8,8
	.type	orig_asm_operands_vector,@object # @orig_asm_operands_vector
	.local	orig_asm_operands_vector
	.comm	orig_asm_operands_vector,8,8
	.type	copy_asm_operands_vector,@object # @copy_asm_operands_vector
	.local	copy_asm_operands_vector
	.comm	copy_asm_operands_vector,8,8
	.type	.L__FUNCTION__.copy_insn_1,@object # @__FUNCTION__.copy_insn_1
.L__FUNCTION__.copy_insn_1:
	.asciz	"copy_insn_1"
	.size	.L__FUNCTION__.copy_insn_1, 12

	.type	mem_attrs_htab,@object  # @mem_attrs_htab
	.local	mem_attrs_htab
	.comm	mem_attrs_htab,8,8
	.type	byte_mode,@object       # @byte_mode
	.comm	byte_mode,4,4
	.type	ptr_mode,@object        # @ptr_mode
	.comm	ptr_mode,4,4
	.type	const_true_rtx,@object  # @const_true_rtx
	.comm	const_true_rtx,8,8
	.type	dconst0,@object         # @dconst0
	.comm	dconst0,24,8
	.type	.L.str3,@object         # @.str3
.L.str3:
	.asciz	"0"
	.size	.L.str3, 2

	.type	dconst1,@object         # @dconst1
	.comm	dconst1,24,8
	.type	.L.str4,@object         # @.str4
.L.str4:
	.asciz	"1"
	.size	.L.str4, 2

	.type	dconst2,@object         # @dconst2
	.comm	dconst2,24,8
	.type	.L.str5,@object         # @.str5
.L.str5:
	.asciz	"2"
	.size	.L.str5, 2

	.type	dconstm1,@object        # @dconstm1
	.comm	dconstm1,24,8
	.type	.L.str6,@object         # @.str6
.L.str6:
	.asciz	"-1"
	.size	.L.str6, 3

	.type	struct_value_rtx,@object # @struct_value_rtx
	.comm	struct_value_rtx,8,8
	.type	struct_value_incoming_rtx,@object # @struct_value_incoming_rtx
	.comm	struct_value_incoming_rtx,8,8
	.type	static_chain_rtx,@object # @static_chain_rtx
	.comm	static_chain_rtx,8,8
	.type	static_chain_incoming_rtx,@object # @static_chain_incoming_rtx
	.comm	static_chain_incoming_rtx,8,8
	.type	return_address_pointer_rtx,@object # @return_address_pointer_rtx
	.comm	return_address_pointer_rtx,8,8
	.type	double_mode,@object     # @double_mode
	.comm	double_mode,4,4
	.type	.L__FUNCTION__.gen_const_vector_0,@object # @__FUNCTION__.gen_const_vector_0
.L__FUNCTION__.gen_const_vector_0:
	.asciz	"gen_const_vector_0"
	.size	.L__FUNCTION__.gen_const_vector_0, 19

	.type	.L__FUNCTION__.change_address_1,@object # @__FUNCTION__.change_address_1
.L__FUNCTION__.change_address_1:
	.asciz	"change_address_1"
	.size	.L__FUNCTION__.change_address_1, 17


	.ident	"clang version 3.5 (trunk)"
	.section	".note.GNU-stack","",@progbits
