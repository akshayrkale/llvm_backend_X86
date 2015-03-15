	.file	"numeric.c.bc"
	.text
	.globl	Perl_cast_ulong
	.align	16, 0x90
	.type	Perl_cast_ulong,@function
Perl_cast_ulong:                        # @Perl_cast_ulong
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
.Ltmp11:
	.cfi_offset %rbx, -48
.Ltmp12:
	.cfi_offset %r12, -40
.Ltmp13:
	.cfi_offset %r13, -32
.Ltmp14:
	.cfi_offset %r14, -24
.Ltmp15:
	.cfi_offset %r15, -16
	movq	%rdi, %r14
	xorq	%rsi, %rsi
                                        # kill: RDI<def> R14<kill>
	callq	__unorddf2
	testq	%rax, %rax
	movabsq	$0, %r15
	movabsq	$1, %r12
	movq	%r15, %rbx
	je	.LBB0_2
# BB#1:                                 # %entry
	movq	%r12, %rbx
.LBB0_2:                                # %entry
	xorq	%rsi, %rsi
	movq	%r14, %rdi
	callq	__gedf2
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	js	.LBB0_4
# BB#3:                                 # %entry
	movq	%r12, %r15
.LBB0_4:                                # %entry
	orq	%r15, %rbx
	jne	.LBB0_7
# BB#5:                                 # %if.then
	movabsq	$-4476578029606273024, %rsi # imm = 0xC1E0000000000000
	movq	%r14, %rdi
	callq	__ltdf2
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$-2147483648, %rbx      # imm = 0xFFFFFFFF80000000
	testq	%rax, %rax
	js	.LBB0_14
	jmp	.LBB0_6
.LBB0_7:                                # %if.end
	movabsq	$4751297606875873280, %r15 # imm = 0x41F0000000000000
	movq	%r14, %rdi
	movq	%r15, %rsi
	callq	__unorddf2
	testq	%rax, %rax
	movabsq	$0, %r12
	movabsq	$1, %r13
	movq	%r12, %rbx
	je	.LBB0_9
# BB#8:                                 # %if.end
	movq	%r13, %rbx
.LBB0_9:                                # %if.end
	movq	%r14, %rdi
	movq	%r15, %rsi
	callq	__gedf2
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	js	.LBB0_11
# BB#10:                                # %if.end
	movq	%r13, %r12
.LBB0_11:                               # %if.end
	orq	%r12, %rbx
	jne	.LBB0_12
.LBB0_6:                                # %cond.false
	movq	%r14, %rdi
	callq	float64_to_int64
	movq	%rax, %rbx
	jmp	.LBB0_14
.LBB0_12:                               # %if.end6
	xorq	%rsi, %rsi
	xorq	%rbx, %rbx
	movq	%r14, %rdi
	callq	__gtdf2
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jle	.LBB0_14
# BB#13:
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
.LBB0_14:                               # %return
	movq	%rbx, %rax
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp16:
	.size	Perl_cast_ulong, .Ltmp16-Perl_cast_ulong
	.cfi_endproc

	.globl	Perl_cast_i32
	.align	16, 0x90
	.type	Perl_cast_i32,@function
Perl_cast_i32:                          # @Perl_cast_i32
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp23:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp24:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp25:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp26:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp27:
	.cfi_def_cfa_offset 48
.Ltmp28:
	.cfi_offset %rbx, -48
.Ltmp29:
	.cfi_offset %r12, -40
.Ltmp30:
	.cfi_offset %r13, -32
.Ltmp31:
	.cfi_offset %r14, -24
.Ltmp32:
	.cfi_offset %r15, -16
	movq	%rdi, %r14
	movabsq	$4746794007248502784, %r15 # imm = 0x41E0000000000000
                                        # kill: RDI<def> R14<kill>
	movq	%r15, %rsi
	callq	__unorddf2
	testq	%rax, %rax
	movabsq	$0, %r12
	movabsq	$1, %r13
	movq	%r12, %rbx
	je	.LBB1_2
# BB#1:                                 # %entry
	movq	%r13, %rbx
.LBB1_2:                                # %entry
	movq	%r14, %rdi
	movq	%r15, %rsi
	callq	__gedf2
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	js	.LBB1_4
# BB#3:                                 # %entry
	movq	%r13, %r12
.LBB1_4:                                # %entry
	orq	%r12, %rbx
	jne	.LBB1_7
# BB#5:                                 # %if.then
	movabsq	$-4476578029606273024, %rsi # imm = 0xC1E0000000000000
	movq	%r14, %rdi
	callq	__ltdf2
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$-2147483648, %rbx      # imm = 0xFFFFFFFF80000000
	testq	%rax, %rax
	js	.LBB1_14
	jmp	.LBB1_6
.LBB1_7:                                # %if.end
	movabsq	$4751297606875873280, %r15 # imm = 0x41F0000000000000
	movq	%r14, %rdi
	movq	%r15, %rsi
	callq	__unorddf2
	testq	%rax, %rax
	movabsq	$0, %r12
	movabsq	$1, %r13
	movq	%r12, %rbx
	je	.LBB1_9
# BB#8:                                 # %if.end
	movq	%r13, %rbx
.LBB1_9:                                # %if.end
	movq	%r14, %rdi
	movq	%r15, %rsi
	callq	__gedf2
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	js	.LBB1_11
# BB#10:                                # %if.end
	movq	%r13, %r12
.LBB1_11:                               # %if.end
	orq	%r12, %rbx
	jne	.LBB1_12
.LBB1_6:                                # %cond.false
	movq	%r14, %rdi
	callq	float64_to_int64
	movq	%rax, %rbx
	jmp	.LBB1_14
.LBB1_12:                               # %if.end6
	xorq	%rsi, %rsi
	xorq	%rbx, %rbx
	movq	%r14, %rdi
	callq	__gtdf2
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jle	.LBB1_14
# BB#13:
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
.LBB1_14:                               # %return
	movq	%rbx, %rax
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp33:
	.size	Perl_cast_i32, .Ltmp33-Perl_cast_i32
	.cfi_endproc

	.globl	Perl_cast_iv
	.align	16, 0x90
	.type	Perl_cast_iv,@function
Perl_cast_iv:                           # @Perl_cast_iv
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
.Ltmp45:
	.cfi_offset %rbx, -48
.Ltmp46:
	.cfi_offset %r12, -40
.Ltmp47:
	.cfi_offset %r13, -32
.Ltmp48:
	.cfi_offset %r14, -24
.Ltmp49:
	.cfi_offset %r15, -16
	movq	%rdi, %r14
	movabsq	$4746794007248502784, %r15 # imm = 0x41E0000000000000
                                        # kill: RDI<def> R14<kill>
	movq	%r15, %rsi
	callq	__unorddf2
	testq	%rax, %rax
	movabsq	$0, %r12
	movabsq	$1, %r13
	movq	%r12, %rbx
	je	.LBB2_2
# BB#1:                                 # %entry
	movq	%r13, %rbx
.LBB2_2:                                # %entry
	movq	%r14, %rdi
	movq	%r15, %rsi
	callq	__gedf2
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	js	.LBB2_4
# BB#3:                                 # %entry
	movq	%r13, %r12
.LBB2_4:                                # %entry
	orq	%r12, %rbx
	jne	.LBB2_7
# BB#5:                                 # %if.then
	movabsq	$-4476578029606273024, %rsi # imm = 0xC1E0000000000000
	movq	%r14, %rdi
	callq	__ltdf2
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$-2147483648, %rbx      # imm = 0xFFFFFFFF80000000
	testq	%rax, %rax
	js	.LBB2_14
	jmp	.LBB2_6
.LBB2_7:                                # %if.end
	movabsq	$4751297606875873280, %r15 # imm = 0x41F0000000000000
	movq	%r14, %rdi
	movq	%r15, %rsi
	callq	__unorddf2
	testq	%rax, %rax
	movabsq	$0, %r12
	movabsq	$1, %r13
	movq	%r12, %rbx
	je	.LBB2_9
# BB#8:                                 # %if.end
	movq	%r13, %rbx
.LBB2_9:                                # %if.end
	movq	%r14, %rdi
	movq	%r15, %rsi
	callq	__gedf2
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	js	.LBB2_11
# BB#10:                                # %if.end
	movq	%r13, %r12
.LBB2_11:                               # %if.end
	orq	%r12, %rbx
	jne	.LBB2_12
.LBB2_6:                                # %cond.false
	movq	%r14, %rdi
	callq	float64_to_int64
	movq	%rax, %rbx
	jmp	.LBB2_14
.LBB2_12:                               # %if.end6
	xorq	%rsi, %rsi
	xorq	%rbx, %rbx
	movq	%r14, %rdi
	callq	__gtdf2
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jle	.LBB2_14
# BB#13:
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
.LBB2_14:                               # %return
	movq	%rbx, %rax
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp50:
	.size	Perl_cast_iv, .Ltmp50-Perl_cast_iv
	.cfi_endproc

	.globl	Perl_cast_uv
	.align	16, 0x90
	.type	Perl_cast_uv,@function
Perl_cast_uv:                           # @Perl_cast_uv
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp57:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp58:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp59:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp60:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp61:
	.cfi_def_cfa_offset 48
.Ltmp62:
	.cfi_offset %rbx, -48
.Ltmp63:
	.cfi_offset %r12, -40
.Ltmp64:
	.cfi_offset %r13, -32
.Ltmp65:
	.cfi_offset %r14, -24
.Ltmp66:
	.cfi_offset %r15, -16
	movq	%rdi, %r14
	xorq	%rsi, %rsi
                                        # kill: RDI<def> R14<kill>
	callq	__unorddf2
	testq	%rax, %rax
	movabsq	$0, %r15
	movabsq	$1, %r12
	movq	%r15, %rbx
	je	.LBB3_2
# BB#1:                                 # %entry
	movq	%r12, %rbx
.LBB3_2:                                # %entry
	xorq	%rsi, %rsi
	movq	%r14, %rdi
	callq	__gedf2
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	js	.LBB3_4
# BB#3:                                 # %entry
	movq	%r12, %r15
.LBB3_4:                                # %entry
	orq	%r15, %rbx
	jne	.LBB3_7
# BB#5:                                 # %if.then
	movabsq	$-4476578029606273024, %rsi # imm = 0xC1E0000000000000
	movq	%r14, %rdi
	callq	__ltdf2
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$-2147483648, %rbx      # imm = 0xFFFFFFFF80000000
	testq	%rax, %rax
	js	.LBB3_14
	jmp	.LBB3_6
.LBB3_7:                                # %if.end
	movabsq	$4751297606875873280, %r15 # imm = 0x41F0000000000000
	movq	%r14, %rdi
	movq	%r15, %rsi
	callq	__unorddf2
	testq	%rax, %rax
	movabsq	$0, %r12
	movabsq	$1, %r13
	movq	%r12, %rbx
	je	.LBB3_9
# BB#8:                                 # %if.end
	movq	%r13, %rbx
.LBB3_9:                                # %if.end
	movq	%r14, %rdi
	movq	%r15, %rsi
	callq	__gedf2
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	js	.LBB3_11
# BB#10:                                # %if.end
	movq	%r13, %r12
.LBB3_11:                               # %if.end
	orq	%r12, %rbx
	jne	.LBB3_12
.LBB3_6:                                # %cond.false
	movq	%r14, %rdi
	callq	float64_to_int64
	movq	%rax, %rbx
	jmp	.LBB3_14
.LBB3_12:                               # %if.end6
	xorq	%rsi, %rsi
	xorq	%rbx, %rbx
	movq	%r14, %rdi
	callq	__gtdf2
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jle	.LBB3_14
# BB#13:
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
.LBB3_14:                               # %return
	movq	%rbx, %rax
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp67:
	.size	Perl_cast_uv, .Ltmp67-Perl_cast_uv
	.cfi_endproc

	.globl	Perl_huge
	.align	16, 0x90
	.type	Perl_huge,@function
Perl_huge:                              # @Perl_huge
	.cfi_startproc
# BB#0:                                 # %entry
	movabsq	$9218868437227405312, %rax # imm = 0x7FF0000000000000
	retq
.Ltmp68:
	.size	Perl_huge, .Ltmp68-Perl_huge
	.cfi_endproc

	.globl	Perl_grok_bin
	.align	16, 0x90
	.type	Perl_grok_bin,@function
Perl_grok_bin:                          # @Perl_grok_bin
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp75:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp76:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp77:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp78:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp79:
	.cfi_def_cfa_offset 48
	subq	$48, %rsp
.Ltmp80:
	.cfi_def_cfa_offset 96
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
	movq	(%rdx), %rax
	movq	(%rsi), %r12
	testq	$2, %rax
	jne	.LBB5_1
# BB#2:                                 # %entry
	testq	%r12, %r12
	je	.LBB5_3
# BB#4:                                 # %if.then3
	movzbl	(%rdi), %ebx
	cmpq	$98, %rbx
	jne	.LBB5_6
# BB#5:                                 # %if.then7
	leaq	1(%rdi), %r13
	decq	%r12
	jmp	.LBB5_13
.LBB5_1:
	movq	%rdi, %r13
	jmp	.LBB5_13
.LBB5_3:
	movq	%rdi, %r13
	jmp	.LBB5_13
.LBB5_6:                                # %if.else
	cmpq	$2, %r12
	jb	.LBB5_7
# BB#8:                                 # %if.else
	movq	%rsi, %r8
	cmpq	$48, %rbx
	jne	.LBB5_9
# BB#10:                                # %land.lhs.true14
	movzbl	1(%rdi), %esi
	cmpq	$98, %rsi
	movq	%r8, %rsi
	jne	.LBB5_11
# BB#12:                                # %if.then19
	leaq	2(%rdi), %r13
	addq	$-2, %r12
	jmp	.LBB5_13
.LBB5_7:
	movq	%rdi, %r13
	jmp	.LBB5_13
.LBB5_9:
	movq	%rdi, %r13
	movq	%r8, %rsi
	jmp	.LBB5_13
.LBB5_11:
	movq	%rdi, %r13
.LBB5_13:                               # %for.cond.preheader
	testq	%r12, %r12
	je	.LBB5_14
# BB#15:                                # %land.rhs.lr.ph
	movq	%rdi, 8(%rsp)           # 8-byte Spill
	movq	%rsi, 16(%rsp)          # 8-byte Spill
	movq	%rcx, (%rsp)            # 8-byte Spill
	movq	%rdx, 24(%rsp)          # 8-byte Spill
	movq	$0, 40(%rsp)            # 8-byte Folded Spill
	testq	$1, %rax
	je	.LBB5_16
# BB#30:
	leaq	-1(%r12), %rax
	xorq	%r14, %r14
	movq	$0, 32(%rsp)            # 8-byte Folded Spill
	.align	16, 0x90
.LBB5_31:                               # %land.rhs
                                        # =>This Inner Loop Header: Depth=1
	movzbl	(%r13), %r15d
	testq	%r15, %r15
	je	.LBB5_57
# BB#32:                                # %land.rhs
                                        #   in Loop: Header=BB5_31 Depth=1
	leaq	-48(%r15), %rcx
	andq	$255, %rcx
	cmpq	$1, %rcx
	ja	.LBB5_45
# BB#33:                                #   in Loop: Header=BB5_31 Depth=1
	movq	%rax, %r12
	jmp	.LBB5_34
	.align	16, 0x90
.LBB5_45:                               # %if.end66
                                        #   in Loop: Header=BB5_31 Depth=1
	testq	%rax, %rax
	je	.LBB5_49
# BB#46:                                # %if.end66
                                        #   in Loop: Header=BB5_31 Depth=1
	cmpq	$95, %r15
	jne	.LBB5_49
# BB#47:                                # %land.lhs.true75
                                        #   in Loop: Header=BB5_31 Depth=1
	movzbl	1(%r13), %r15d
	movq	%r15, %rax
	andq	$254, %rax
	cmpq	$48, %rax
	jne	.LBB5_49
# BB#48:                                # %if.then87
                                        #   in Loop: Header=BB5_31 Depth=1
	incq	%r13
	addq	$-2, %r12
.LBB5_34:                               # %redo
                                        #   in Loop: Header=BB5_31 Depth=1
	testq	$255, 40(%rsp)          # 8-byte Folded Reload
	jne	.LBB5_43
# BB#35:                                # %if.then35
                                        #   in Loop: Header=BB5_31 Depth=1
	movq	32(%rsp), %rax          # 8-byte Reload
	cmpq	$2147483647, %rax       # imm = 0x7FFFFFFF
	ja	.LBB5_37
# BB#36:                                # %if.then38
                                        #   in Loop: Header=BB5_31 Depth=1
	addq	%rax, %rax
	movq	$56, %rcx
	shlq	%cl, %r15
	movq	$56, %rcx
	sarq	%cl, %r15
	addq	$-48, %r15
	orq	%r15, %rax
	movq	%rax, 32(%rsp)          # 8-byte Spill
	movq	$0, 40(%rsp)            # 8-byte Folded Spill
	jmp	.LBB5_44
	.align	16, 0x90
.LBB5_37:                               # %if.end42
                                        #   in Loop: Header=BB5_31 Depth=1
	movq	PL_curcop(%rip), %rax
	cmpq	$0, 88(%rax)
	je	.LBB5_41
# BB#38:                                # %lor.lhs.false45
                                        #   in Loop: Header=BB5_31 Depth=1
	movq	PL_curcop(%rip), %rax
	movq	88(%rax), %rax
	cmpq	$24, %rax
	je	.LBB5_41
# BB#39:                                # %lor.lhs.false49
                                        #   in Loop: Header=BB5_31 Depth=1
	movq	PL_curcop(%rip), %rax
	movq	88(%rax), %rax
	cmpq	$48, %rax
	je	.LBB5_42
# BB#40:                                # %land.lhs.true53
                                        #   in Loop: Header=BB5_31 Depth=1
	movq	PL_curcop(%rip), %rax
	movq	88(%rax), %rax
	movq	(%rax), %rax
	movq	(%rax), %rax
	movzbl	3(%rax), %eax
	testq	$64, %rax
	je	.LBB5_42
.LBB5_41:                               # %if.then59
                                        #   in Loop: Header=BB5_31 Depth=1
	movq	$15, %rdi
	xorq	%rax, %rax
	movabsq	$.L.str, %rsi
	callq	Perl_warner
.LBB5_42:                               # %if.end60
                                        #   in Loop: Header=BB5_31 Depth=1
	movq	32(%rsp), %rdi          # 8-byte Reload
	callq	int64_to_float64
	movq	%rax, %r14
	movq	$1, 40(%rsp)            # 8-byte Folded Spill
.LBB5_43:                               # %if.end62
                                        #   in Loop: Header=BB5_31 Depth=1
	movq	%r14, %rdi
	movq	%r14, %rsi
	callq	float64_add
	movq	%rax, %rbx
	movq	$56, %rcx
	shlq	%cl, %r15
	movq	$56, %rcx
	sarq	%cl, %r15
	addq	$-48, %r15
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %r15
	movq	%r15, %rdi
	callq	int32_to_float64
	movq	%rax, %rdi
	movq	%rbx, %rsi
	callq	float64_add
	movq	%rax, %r14
.LBB5_44:                               # %for.inc
                                        #   in Loop: Header=BB5_31 Depth=1
	incq	%r13
	leaq	-1(%r12), %rax
	testq	%r12, %r12
	jne	.LBB5_31
	jmp	.LBB5_57
.LBB5_14:                               # %if.end160.thread
	subq	%rdi, %r13
	movq	%r13, (%rsi)
	xorq	%rax, %rax
	movq	$0, (%rdx)
	jmp	.LBB5_75
.LBB5_16:
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	xorq	%r14, %r14
	movq	$0, 32(%rsp)            # 8-byte Folded Spill
	.align	16, 0x90
.LBB5_17:                               # %land.rhs.us
                                        # =>This Inner Loop Header: Depth=1
	movzbl	(%r13), %ebx
	movq	$56, %rcx
	movq	%rbx, %rax
	shlq	%cl, %rax
	movq	$56, %rcx
	sarq	%cl, %rax
	testq	%rbx, %rbx
	je	.LBB5_57
# BB#18:                                # %land.rhs.us
                                        #   in Loop: Header=BB5_17 Depth=1
	leaq	-48(%rbx), %rcx
	andq	$255, %rcx
	cmpq	$1, %rcx
	ja	.LBB5_49
# BB#19:                                # %redo.us
                                        #   in Loop: Header=BB5_17 Depth=1
	testq	$255, 40(%rsp)          # 8-byte Folded Reload
	jne	.LBB5_27
# BB#20:                                # %if.then35.us
                                        #   in Loop: Header=BB5_17 Depth=1
	movq	$31, %rcx
	movq	32(%rsp), %rsi          # 8-byte Reload
	movq	%rsi, %rdx
	shrq	%cl, %rdx
	testq	%rdx, %rdx
	jne	.LBB5_21
# BB#28:                                # %if.then38.us
                                        #   in Loop: Header=BB5_17 Depth=1
	addq	%rsi, %rsi
	addq	$-48, %rax
	orq	%rax, %rsi
	movq	%rsi, 32(%rsp)          # 8-byte Spill
	movq	$0, 40(%rsp)            # 8-byte Folded Spill
	jmp	.LBB5_29
	.align	16, 0x90
.LBB5_21:                               # %if.end42.us
                                        #   in Loop: Header=BB5_17 Depth=1
	movq	PL_curcop(%rip), %rax
	cmpq	$0, 88(%rax)
	je	.LBB5_25
# BB#22:                                # %lor.lhs.false45.us
                                        #   in Loop: Header=BB5_17 Depth=1
	movq	PL_curcop(%rip), %rax
	movq	88(%rax), %rax
	cmpq	$24, %rax
	je	.LBB5_25
# BB#23:                                # %lor.lhs.false49.us
                                        #   in Loop: Header=BB5_17 Depth=1
	movq	PL_curcop(%rip), %rax
	movq	88(%rax), %rax
	cmpq	$48, %rax
	je	.LBB5_26
# BB#24:                                # %land.lhs.true53.us
                                        #   in Loop: Header=BB5_17 Depth=1
	movq	PL_curcop(%rip), %rax
	movq	88(%rax), %rax
	movq	(%rax), %rax
	movq	(%rax), %rax
	movzbl	3(%rax), %eax
	testq	$64, %rax
	je	.LBB5_26
.LBB5_25:                               # %if.then59.us
                                        #   in Loop: Header=BB5_17 Depth=1
	movq	$15, %rdi
	xorq	%rax, %rax
	movabsq	$.L.str, %rsi
	callq	Perl_warner
.LBB5_26:                               # %if.end60.us
                                        #   in Loop: Header=BB5_17 Depth=1
	movq	32(%rsp), %rdi          # 8-byte Reload
	callq	int64_to_float64
	movq	%rax, %r14
	movq	$1, 40(%rsp)            # 8-byte Folded Spill
.LBB5_27:                               # %if.end62.us
                                        #   in Loop: Header=BB5_17 Depth=1
	movq	%r14, %rdi
	movq	%r14, %rsi
	callq	float64_add
	movq	%rax, %r14
	movq	$56, %rcx
	shlq	%cl, %rbx
	movq	$56, %rcx
	sarq	%cl, %rbx
	addq	$-48, %rbx
	andq	%r15, %rbx
	movq	%rbx, %rdi
	callq	int32_to_float64
	movq	%rax, %rdi
	movq	%r14, %rsi
	callq	float64_add
	movq	%rax, %r14
.LBB5_29:                               # %for.inc.us
                                        #   in Loop: Header=BB5_17 Depth=1
	incq	%r13
	decq	%r12
	jne	.LBB5_17
	jmp	.LBB5_57
.LBB5_49:                               # %if.end90
	movq	24(%rsp), %rax          # 8-byte Reload
	movzbl	(%rax), %eax
	testq	$4, %rax
	jne	.LBB5_57
# BB#50:                                # %land.lhs.true93
	movq	PL_curcop(%rip), %rax
	cmpq	$0, 88(%rax)
	je	.LBB5_54
# BB#51:                                # %land.lhs.true97
	movq	PL_curcop(%rip), %rax
	movq	88(%rax), %rax
	cmpq	$48, %rax
	je	.LBB5_54
# BB#52:                                # %land.lhs.true101
	movq	PL_curcop(%rip), %rax
	movq	88(%rax), %rax
	cmpq	$24, %rax
	je	.LBB5_56
# BB#53:                                # %lor.lhs.false105
	movq	PL_curcop(%rip), %rax
	movq	88(%rax), %rax
	movq	(%rax), %rax
	movq	(%rax), %rax
	movzbl	7(%rax), %eax
	testq	$64, %rax
	jne	.LBB5_56
.LBB5_54:                               # %lor.lhs.false113
	movq	PL_curcop(%rip), %rax
	cmpq	$0, 88(%rax)
	jne	.LBB5_57
# BB#55:                                # %land.lhs.true117
	movzbl	PL_dowarn(%rip), %eax
	testq	$1, %rax
	je	.LBB5_57
.LBB5_56:                               # %if.then121
	movsbq	(%r13), %rdx
	movabsq	$.L.str1, %rsi
	movq	$31, %rdi
	xorq	%rax, %rax
	callq	Perl_warner
.LBB5_57:                               # %for.end
	testq	$255, 40(%rsp)          # 8-byte Folded Reload
	je	.LBB5_70
# BB#58:                                # %for.end
	movabsq	$4751297606873776128, %r15 # imm = 0x41EFFFFFFFE00000
	movq	%r14, %rdi
	movq	%r15, %rsi
	callq	__unorddf2
	testq	%rax, %rax
	movabsq	$0, %r12
	movabsq	$1, %rax
	movq	%r12, %rbx
	je	.LBB5_60
# BB#59:                                # %for.end
	movq	%rax, %rbx
.LBB5_60:                               # %for.end
	movq	%r14, %rdi
	movq	%r15, %rsi
	callq	__ledf2
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jg	.LBB5_62
# BB#61:                                # %for.end
	movabsq	$1, %r12
.LBB5_62:                               # %for.end
	orq	%r12, %rbx
	jne	.LBB5_70
# BB#63:                                # %if.then130
	movq	PL_curcop(%rip), %rax
	cmpq	$0, 88(%rax)
	je	.LBB5_67
# BB#64:                                # %land.lhs.true134
	movq	PL_curcop(%rip), %rax
	movq	88(%rax), %rax
	cmpq	$48, %rax
	je	.LBB5_67
# BB#65:                                # %land.lhs.true138
	movq	PL_curcop(%rip), %rax
	movq	88(%rax), %rax
	cmpq	$24, %rax
	je	.LBB5_69
# BB#66:                                # %lor.lhs.false142
	movq	PL_curcop(%rip), %rax
	movq	88(%rax), %rax
	movq	(%rax), %rax
	movq	(%rax), %rax
	movzbl	4(%rax), %eax
	testq	$4, %rax
	jne	.LBB5_69
.LBB5_67:                               # %lor.lhs.false150
	movq	PL_curcop(%rip), %rax
	cmpq	$0, 88(%rax)
	jne	.LBB5_70
# BB#68:                                # %land.lhs.true154
	movzbl	PL_dowarn(%rip), %eax
	testq	$1, %rax
	je	.LBB5_70
.LBB5_69:                               # %if.then158
	movabsq	$.L.str2, %rsi
	movq	$17, %rdi
	xorq	%rax, %rax
	callq	Perl_warner
.LBB5_70:                               # %if.end160
	movq	8(%rsp), %rax           # 8-byte Reload
	subq	%rax, %r13
	movq	16(%rsp), %rax          # 8-byte Reload
	movq	%r13, (%rax)
	testq	$255, 40(%rsp)          # 8-byte Folded Reload
	movq	24(%rsp), %rdx          # 8-byte Reload
	movq	32(%rsp), %rax          # 8-byte Reload
	je	.LBB5_71
# BB#72:                                # %if.end163
	movq	$2, (%rdx)
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	movq	(%rsp), %rax            # 8-byte Reload
	testq	%rax, %rax
	je	.LBB5_74
# BB#73:                                # %if.then165
	movq	%r14, (%rax)
.LBB5_74:                               # %return
	movq	%rcx, %rax
	jmp	.LBB5_75
.LBB5_71:                               # %if.then162
	movq	$0, (%rdx)
.LBB5_75:                               # %return
	addq	$48, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp86:
	.size	Perl_grok_bin, .Ltmp86-Perl_grok_bin
	.cfi_endproc

	.globl	Perl_grok_hex
	.align	16, 0x90
	.type	Perl_grok_hex,@function
Perl_grok_hex:                          # @Perl_grok_hex
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
	subq	$80, %rsp
.Ltmp98:
	.cfi_def_cfa_offset 128
.Ltmp99:
	.cfi_offset %rbx, -48
.Ltmp100:
	.cfi_offset %r12, -40
.Ltmp101:
	.cfi_offset %r13, -32
.Ltmp102:
	.cfi_offset %r14, -24
.Ltmp103:
	.cfi_offset %r15, -16
	movq	(%rdx), %rax
	movq	%rax, 40(%rsp)          # 8-byte Spill
	movq	(%rsi), %r14
	testq	$2, %rax
	jne	.LBB6_1
# BB#2:                                 # %entry
	testq	%r14, %r14
	je	.LBB6_3
# BB#4:                                 # %if.then3
	movzbl	(%rdi), %eax
	cmpq	$120, %rax
	jne	.LBB6_6
# BB#5:                                 # %if.then7
	leaq	1(%rdi), %r15
	decq	%r14
	jmp	.LBB6_13
.LBB6_1:
	movq	%rdi, %r15
	jmp	.LBB6_13
.LBB6_3:
	movq	%rdi, %r15
	jmp	.LBB6_13
.LBB6_6:                                # %if.else
	cmpq	$2, %r14
	jb	.LBB6_7
# BB#8:                                 # %if.else
	cmpq	$48, %rax
	jne	.LBB6_9
# BB#10:                                # %land.lhs.true14
	movzbl	1(%rdi), %eax
	cmpq	$120, %rax
	jne	.LBB6_11
# BB#12:                                # %if.then19
	leaq	2(%rdi), %r15
	addq	$-2, %r14
	jmp	.LBB6_13
.LBB6_7:
	movq	%rdi, %r15
	jmp	.LBB6_13
.LBB6_9:
	movq	%rdi, %r15
	jmp	.LBB6_13
.LBB6_11:
	movq	%rdi, %r15
.LBB6_13:                               # %for.cond.preheader
	testq	%r14, %r14
	je	.LBB6_14
# BB#15:                                # %land.rhs.lr.ph
	movq	%rdi, 16(%rsp)          # 8-byte Spill
	movq	%rsi, 24(%rsp)          # 8-byte Spill
	movq	%rcx, 8(%rsp)           # 8-byte Spill
	movq	%rdx, 32(%rsp)          # 8-byte Spill
	andq	$1, 40(%rsp)            # 8-byte Folded Spill
	movq	$0, 64(%rsp)            # 8-byte Folded Spill
	movq	$0, 72(%rsp)            # 8-byte Folded Spill
	movq	$0, 48(%rsp)            # 8-byte Folded Spill
	.align	16, 0x90
.LBB6_16:                               # %land.rhs
                                        # =>This Inner Loop Header: Depth=1
	movzbl	(%r15), %ebx
	testq	%rbx, %rbx
	je	.LBB6_43
# BB#17:                                # %for.body
                                        #   in Loop: Header=BB6_16 Depth=1
	movq	%r14, 56(%rsp)          # 8-byte Spill
	leaq	-1(%r14), %r14
	movq	PL_hexdigit(%rip), %r13
	movq	$56, %rcx
	movq	%rbx, %rsi
	shlq	%cl, %rsi
	movq	$56, %rcx
	sarq	%cl, %rsi
	movq	%r13, %rdi
	callq	strchr
	movq	%rax, %r12
	testq	%r12, %r12
	jne	.LBB6_18
# BB#29:                                # %if.end61
                                        #   in Loop: Header=BB6_16 Depth=1
	testq	%r14, %r14
	je	.LBB6_35
# BB#30:                                # %if.end61
                                        #   in Loop: Header=BB6_16 Depth=1
	cmpq	$95, %rbx
	jne	.LBB6_35
# BB#31:                                # %if.end61
                                        #   in Loop: Header=BB6_16 Depth=1
	cmpq	$0, 40(%rsp)            # 8-byte Folded Reload
	je	.LBB6_35
# BB#32:                                # %land.lhs.true70
                                        #   in Loop: Header=BB6_16 Depth=1
	movzbl	1(%r15), %esi
	testq	%rsi, %rsi
	je	.LBB6_35
# BB#33:                                # %land.lhs.true74
                                        #   in Loop: Header=BB6_16 Depth=1
	movq	$56, %rcx
	shlq	%cl, %rsi
	movq	$56, %rcx
	sarq	%cl, %rsi
	movq	%r13, %rdi
	callq	strchr
	movq	%rax, %r12
	testq	%r12, %r12
	je	.LBB6_35
# BB#34:                                # %if.then79
                                        #   in Loop: Header=BB6_16 Depth=1
	incq	%r15
	movq	56(%rsp), %r14          # 8-byte Reload
	addq	$-2, %r14
.LBB6_18:                               # %redo
                                        #   in Loop: Header=BB6_16 Depth=1
	testq	$255, 64(%rsp)          # 8-byte Folded Reload
	jne	.LBB6_27
# BB#19:                                # %if.then31
                                        #   in Loop: Header=BB6_16 Depth=1
	movq	48(%rsp), %rax          # 8-byte Reload
	cmpq	$268435455, %rax        # imm = 0xFFFFFFF
	ja	.LBB6_21
# BB#20:                                # %if.then34
                                        #   in Loop: Header=BB6_16 Depth=1
	movq	$4, %rcx
	shlq	%cl, %rax
	subq	%r13, %r12
	andq	$15, %r12
	orq	%r12, %rax
	movq	%rax, 48(%rsp)          # 8-byte Spill
	movq	$0, 64(%rsp)            # 8-byte Folded Spill
	jmp	.LBB6_28
	.align	16, 0x90
.LBB6_21:                               # %if.end36
                                        #   in Loop: Header=BB6_16 Depth=1
	movq	PL_curcop(%rip), %rax
	cmpq	$0, 88(%rax)
	je	.LBB6_25
# BB#22:                                # %lor.lhs.false
                                        #   in Loop: Header=BB6_16 Depth=1
	movq	PL_curcop(%rip), %rax
	movq	88(%rax), %rax
	cmpq	$24, %rax
	je	.LBB6_25
# BB#23:                                # %lor.lhs.false42
                                        #   in Loop: Header=BB6_16 Depth=1
	movq	PL_curcop(%rip), %rax
	movq	88(%rax), %rax
	cmpq	$48, %rax
	je	.LBB6_26
# BB#24:                                # %land.lhs.true46
                                        #   in Loop: Header=BB6_16 Depth=1
	movq	PL_curcop(%rip), %rax
	movq	88(%rax), %rax
	movq	(%rax), %rax
	movq	(%rax), %rax
	movzbl	3(%rax), %eax
	testq	$64, %rax
	je	.LBB6_26
.LBB6_25:                               # %if.then52
                                        #   in Loop: Header=BB6_16 Depth=1
	movq	$15, %rdi
	xorq	%rax, %rax
	movabsq	$.L.str3, %rsi
	callq	Perl_warner
.LBB6_26:                               # %if.end53
                                        #   in Loop: Header=BB6_16 Depth=1
	movq	48(%rsp), %rdi          # 8-byte Reload
	callq	int64_to_float64
	movq	PL_hexdigit(%rip), %r13
	movq	%rax, 72(%rsp)          # 8-byte Spill
	movq	$1, 64(%rsp)            # 8-byte Folded Spill
.LBB6_27:                               # %if.end55
                                        #   in Loop: Header=BB6_16 Depth=1
	movq	72(%rsp), %rdi          # 8-byte Reload
	movabsq	$4625196817309499392, %rsi # imm = 0x4030000000000000
	callq	float64_mul
	movq	%rax, %rbx
	subq	%r13, %r12
	andq	$15, %r12
	movq	%r12, %rdi
	callq	int64_to_float64
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	float64_add
	movq	%rax, 72(%rsp)          # 8-byte Spill
.LBB6_28:                               # %for.inc
                                        #   in Loop: Header=BB6_16 Depth=1
	incq	%r15
	testq	%r14, %r14
	jne	.LBB6_16
	jmp	.LBB6_43
.LBB6_14:                               # %if.end152.thread
	subq	%rdi, %r15
	movq	%r15, (%rsi)
	xorq	%rax, %rax
	movq	$0, (%rdx)
	jmp	.LBB6_60
.LBB6_35:                               # %if.end82
	movq	32(%rsp), %rax          # 8-byte Reload
	movzbl	(%rax), %eax
	testq	$4, %rax
	jne	.LBB6_43
# BB#36:                                # %land.lhs.true85
	movq	PL_curcop(%rip), %rax
	cmpq	$0, 88(%rax)
	je	.LBB6_40
# BB#37:                                # %land.lhs.true89
	movq	PL_curcop(%rip), %rax
	movq	88(%rax), %rax
	cmpq	$48, %rax
	je	.LBB6_40
# BB#38:                                # %land.lhs.true93
	movq	PL_curcop(%rip), %rax
	movq	88(%rax), %rax
	cmpq	$24, %rax
	je	.LBB6_42
# BB#39:                                # %lor.lhs.false97
	movq	PL_curcop(%rip), %rax
	movq	88(%rax), %rax
	movq	(%rax), %rax
	movq	(%rax), %rax
	movzbl	7(%rax), %eax
	testq	$64, %rax
	jne	.LBB6_42
.LBB6_40:                               # %lor.lhs.false105
	movq	PL_curcop(%rip), %rax
	cmpq	$0, 88(%rax)
	jne	.LBB6_43
# BB#41:                                # %land.lhs.true109
	movzbl	PL_dowarn(%rip), %eax
	testq	$1, %rax
	je	.LBB6_43
.LBB6_42:                               # %if.then113
	movsbq	(%r15), %rdx
	movabsq	$.L.str4, %rsi
	movq	$31, %rdi
	xorq	%rax, %rax
	callq	Perl_warner
.LBB6_43:                               # %for.end
	testq	$255, 64(%rsp)          # 8-byte Folded Reload
	je	.LBB6_56
# BB#44:                                # %for.end
	movabsq	$4751297606873776128, %r14 # imm = 0x41EFFFFFFFE00000
	movq	72(%rsp), %rdi          # 8-byte Reload
	movq	%r14, %rsi
	callq	__unorddf2
	testq	%rax, %rax
	movabsq	$0, %r12
	movabsq	$1, %rbx
	movq	%r12, %r13
	je	.LBB6_46
# BB#45:                                # %for.end
	movq	%rbx, %r13
.LBB6_46:                               # %for.end
	movq	72(%rsp), %rdi          # 8-byte Reload
	movq	%r14, %rsi
	callq	__ledf2
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jg	.LBB6_48
# BB#47:                                # %for.end
	movq	%rbx, %r12
.LBB6_48:                               # %for.end
	orq	%r12, %r13
	jne	.LBB6_56
# BB#49:                                # %if.then122
	movq	PL_curcop(%rip), %rax
	cmpq	$0, 88(%rax)
	je	.LBB6_53
# BB#50:                                # %land.lhs.true126
	movq	PL_curcop(%rip), %rax
	movq	88(%rax), %rax
	cmpq	$48, %rax
	je	.LBB6_53
# BB#51:                                # %land.lhs.true130
	movq	PL_curcop(%rip), %rax
	movq	88(%rax), %rax
	cmpq	$24, %rax
	je	.LBB6_55
# BB#52:                                # %lor.lhs.false134
	movq	PL_curcop(%rip), %rax
	movq	88(%rax), %rax
	movq	(%rax), %rax
	movq	(%rax), %rax
	movzbl	4(%rax), %eax
	testq	$4, %rax
	jne	.LBB6_55
.LBB6_53:                               # %lor.lhs.false142
	movq	PL_curcop(%rip), %rax
	cmpq	$0, 88(%rax)
	jne	.LBB6_56
# BB#54:                                # %land.lhs.true146
	movzbl	PL_dowarn(%rip), %eax
	testq	$1, %rax
	je	.LBB6_56
.LBB6_55:                               # %if.then150
	movabsq	$.L.str5, %rsi
	movq	$17, %rdi
	xorq	%rax, %rax
	callq	Perl_warner
.LBB6_56:                               # %if.end152
	movq	16(%rsp), %rax          # 8-byte Reload
	subq	%rax, %r15
	movq	24(%rsp), %rax          # 8-byte Reload
	movq	%r15, (%rax)
	testq	$255, 64(%rsp)          # 8-byte Folded Reload
	movq	32(%rsp), %rdx          # 8-byte Reload
	movq	48(%rsp), %rax          # 8-byte Reload
	je	.LBB6_57
# BB#58:                                # %if.end158
	movq	$2, (%rdx)
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	movq	8(%rsp), %rcx           # 8-byte Reload
	testq	%rcx, %rcx
	je	.LBB6_60
# BB#59:                                # %if.then160
	movq	72(%rsp), %rdx          # 8-byte Reload
	movq	%rdx, (%rcx)
	jmp	.LBB6_60
.LBB6_57:                               # %if.then157
	movq	$0, (%rdx)
.LBB6_60:                               # %return
	addq	$80, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp104:
	.size	Perl_grok_hex, .Ltmp104-Perl_grok_hex
	.cfi_endproc

	.globl	Perl_grok_oct
	.align	16, 0x90
	.type	Perl_grok_oct,@function
Perl_grok_oct:                          # @Perl_grok_oct
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp111:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp112:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp113:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp114:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp115:
	.cfi_def_cfa_offset 48
	subq	$64, %rsp
.Ltmp116:
	.cfi_def_cfa_offset 112
.Ltmp117:
	.cfi_offset %rbx, -48
.Ltmp118:
	.cfi_offset %r12, -40
.Ltmp119:
	.cfi_offset %r13, -32
.Ltmp120:
	.cfi_offset %r14, -24
.Ltmp121:
	.cfi_offset %r15, -16
	movq	(%rsi), %r13
	testq	%r13, %r13
	je	.LBB7_1
# BB#2:                                 # %land.rhs.lr.ph
	movq	%rsi, 24(%rsp)          # 8-byte Spill
	movq	%rcx, 8(%rsp)           # 8-byte Spill
	movq	(%rdx), %rax
	movq	%rdx, 32(%rsp)          # 8-byte Spill
	andq	$1, %rax
	movq	%rax, 40(%rsp)          # 8-byte Spill
	movq	$0, 56(%rsp)            # 8-byte Folded Spill
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	movq	%rdi, %r14
	movq	%rdi, 16(%rsp)          # 8-byte Spill
	xorq	%rbx, %rbx
	movq	$0, 48(%rsp)            # 8-byte Folded Spill
	.align	16, 0x90
.LBB7_3:                                # %land.rhs
                                        # =>This Inner Loop Header: Depth=1
	movzbl	(%r14), %r12d
	testq	%r12, %r12
	je	.LBB7_31
# BB#4:                                 # %for.body
                                        #   in Loop: Header=BB7_3 Depth=1
	movq	$56, %rcx
	shlq	%cl, %r12
	movq	$56, %rcx
	sarq	%cl, %r12
	addq	$-48, %r12
	leaq	-1(%r13), %rcx
	movq	%r12, %rax
	andq	%r15, %rax
	cmpq	$7, %rax
	ja	.LBB7_17
# BB#5:                                 #   in Loop: Header=BB7_3 Depth=1
	movq	%rcx, %r13
	jmp	.LBB7_6
	.align	16, 0x90
.LBB7_17:                               # %if.end32
                                        #   in Loop: Header=BB7_3 Depth=1
	testq	%rcx, %rcx
	je	.LBB7_22
# BB#18:                                # %if.end32
                                        #   in Loop: Header=BB7_3 Depth=1
	cmpq	$47, %rax
	jne	.LBB7_22
# BB#19:                                # %if.end32
                                        #   in Loop: Header=BB7_3 Depth=1
	cmpq	$0, 40(%rsp)            # 8-byte Folded Reload
	je	.LBB7_22
# BB#20:                                # %land.lhs.true40
                                        #   in Loop: Header=BB7_3 Depth=1
	movsbq	1(%r14), %rax
	leaq	-48(%rax), %r12
	addq	$-49, %rax
	andq	%r15, %rax
	cmpq	$6, %rax
	ja	.LBB7_22
# BB#21:                                # %if.then51
                                        #   in Loop: Header=BB7_3 Depth=1
	incq	%r14
	addq	$-2, %r13
.LBB7_6:                                # %redo
                                        #   in Loop: Header=BB7_3 Depth=1
	testq	$255, 56(%rsp)          # 8-byte Folded Reload
	jne	.LBB7_15
# BB#7:                                 # %if.then8
                                        #   in Loop: Header=BB7_3 Depth=1
	movq	48(%rsp), %rax          # 8-byte Reload
	cmpq	$536870911, %rax        # imm = 0x1FFFFFFF
	ja	.LBB7_9
# BB#8:                                 # %if.then11
                                        #   in Loop: Header=BB7_3 Depth=1
	movq	$3, %rcx
	shlq	%cl, %rax
	orq	%r12, %rax
	movq	%rax, 48(%rsp)          # 8-byte Spill
	movq	$0, 56(%rsp)            # 8-byte Folded Spill
	jmp	.LBB7_16
	.align	16, 0x90
.LBB7_9:                                # %if.end
                                        #   in Loop: Header=BB7_3 Depth=1
	movq	PL_curcop(%rip), %rax
	cmpq	$0, 88(%rax)
	je	.LBB7_13
# BB#10:                                # %lor.lhs.false
                                        #   in Loop: Header=BB7_3 Depth=1
	movq	PL_curcop(%rip), %rax
	movq	88(%rax), %rax
	cmpq	$24, %rax
	je	.LBB7_13
# BB#11:                                # %lor.lhs.false18
                                        #   in Loop: Header=BB7_3 Depth=1
	movq	PL_curcop(%rip), %rax
	movq	88(%rax), %rax
	cmpq	$48, %rax
	je	.LBB7_14
# BB#12:                                # %land.lhs.true22
                                        #   in Loop: Header=BB7_3 Depth=1
	movq	PL_curcop(%rip), %rax
	movq	88(%rax), %rax
	movq	(%rax), %rax
	movq	(%rax), %rax
	movzbl	3(%rax), %eax
	testq	$64, %rax
	je	.LBB7_14
.LBB7_13:                               # %if.then27
                                        #   in Loop: Header=BB7_3 Depth=1
	movq	$15, %rdi
	xorq	%rax, %rax
	movabsq	$.L.str6, %rsi
	callq	Perl_warner
.LBB7_14:                               # %if.end28
                                        #   in Loop: Header=BB7_3 Depth=1
	movq	48(%rsp), %rdi          # 8-byte Reload
	callq	int64_to_float64
	movq	%rax, %rbx
	movq	$1, 56(%rsp)            # 8-byte Folded Spill
.LBB7_15:                               # %if.end30
                                        #   in Loop: Header=BB7_3 Depth=1
	movq	%rbx, %rdi
	movabsq	$4620693217682128896, %rsi # imm = 0x4020000000000000
	callq	float64_mul
	movq	%rax, %rbx
	andq	%r15, %r12
	movq	%r12, %rdi
	callq	int32_to_float64
	movq	%rax, %rdi
	movq	%rbx, %rsi
	callq	float64_add
	movq	%rax, %rbx
.LBB7_16:                               # %for.inc
                                        #   in Loop: Header=BB7_3 Depth=1
	incq	%r14
	testq	%r13, %r13
	jne	.LBB7_3
	jmp	.LBB7_31
.LBB7_1:                                # %if.end130.thread
	movq	$0, (%rsi)
	xorq	%rax, %rax
	jmp	.LBB7_45
.LBB7_22:                               # %if.end53
	movabsq	$4294967294, %rax       # imm = 0xFFFFFFFE
	andq	%r12, %rax
	cmpq	$8, %rax
	jne	.LBB7_31
# BB#23:                                # %if.then59
	movq	32(%rsp), %rax          # 8-byte Reload
	movzbl	(%rax), %eax
	testq	$4, %rax
	jne	.LBB7_31
# BB#24:                                # %land.lhs.true62
	movq	PL_curcop(%rip), %rax
	cmpq	$0, 88(%rax)
	je	.LBB7_28
# BB#25:                                # %land.lhs.true66
	movq	PL_curcop(%rip), %rax
	movq	88(%rax), %rax
	cmpq	$48, %rax
	je	.LBB7_28
# BB#26:                                # %land.lhs.true70
	movq	PL_curcop(%rip), %rax
	movq	88(%rax), %rax
	cmpq	$24, %rax
	je	.LBB7_30
# BB#27:                                # %lor.lhs.false74
	movq	PL_curcop(%rip), %rax
	movq	88(%rax), %rax
	movq	(%rax), %rax
	movq	(%rax), %rax
	movzbl	7(%rax), %eax
	testq	$64, %rax
	jne	.LBB7_30
.LBB7_28:                               # %lor.lhs.false82
	movq	PL_curcop(%rip), %rax
	cmpq	$0, 88(%rax)
	jne	.LBB7_31
# BB#29:                                # %land.lhs.true86
	movzbl	PL_dowarn(%rip), %eax
	testq	$1, %rax
	je	.LBB7_31
.LBB7_30:                               # %if.then90
	movsbq	(%r14), %rdx
	movabsq	$.L.str7, %rsi
	movq	$31, %rdi
	xorq	%rax, %rax
	callq	Perl_warner
.LBB7_31:                               # %for.end
	testq	$255, 56(%rsp)          # 8-byte Folded Reload
	je	.LBB7_44
# BB#32:                                # %for.end
	movabsq	$4751297606873776128, %rsi # imm = 0x41EFFFFFFFE00000
	movq	%rbx, %rdi
	callq	__unorddf2
	testq	%rax, %rax
	movabsq	$0, %r12
	movabsq	$1, %rax
	movq	%r12, %r13
	je	.LBB7_34
# BB#33:                                # %for.end
	movq	%rax, %r13
.LBB7_34:                               # %for.end
	movq	%rbx, %rdi
	movabsq	$4751297606873776128, %rsi # imm = 0x41EFFFFFFFE00000
	callq	__ledf2
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jg	.LBB7_36
# BB#35:                                # %for.end
	movabsq	$1, %r12
.LBB7_36:                               # %for.end
	orq	%r12, %r13
	jne	.LBB7_44
# BB#37:                                # %if.then100
	movq	PL_curcop(%rip), %rax
	cmpq	$0, 88(%rax)
	je	.LBB7_41
# BB#38:                                # %land.lhs.true104
	movq	PL_curcop(%rip), %rax
	movq	88(%rax), %rax
	cmpq	$48, %rax
	je	.LBB7_41
# BB#39:                                # %land.lhs.true108
	movq	PL_curcop(%rip), %rax
	movq	88(%rax), %rax
	cmpq	$24, %rax
	je	.LBB7_43
# BB#40:                                # %lor.lhs.false112
	movq	PL_curcop(%rip), %rax
	movq	88(%rax), %rax
	movq	(%rax), %rax
	movq	(%rax), %rax
	movzbl	4(%rax), %eax
	testq	$4, %rax
	jne	.LBB7_43
.LBB7_41:                               # %lor.lhs.false120
	movq	PL_curcop(%rip), %rax
	cmpq	$0, 88(%rax)
	jne	.LBB7_44
# BB#42:                                # %land.lhs.true124
	movzbl	PL_dowarn(%rip), %eax
	testq	$1, %rax
	je	.LBB7_44
.LBB7_43:                               # %if.then128
	movabsq	$.L.str8, %rsi
	movq	$17, %rdi
	xorq	%rax, %rax
	callq	Perl_warner
.LBB7_44:                               # %if.end130
	movq	16(%rsp), %rax          # 8-byte Reload
	subq	%rax, %r14
	movq	24(%rsp), %rax          # 8-byte Reload
	movq	%r14, (%rax)
	testq	$255, 56(%rsp)          # 8-byte Folded Reload
	movq	32(%rsp), %rdx          # 8-byte Reload
	movq	48(%rsp), %rax          # 8-byte Reload
	je	.LBB7_45
# BB#46:                                # %if.end133
	movq	$2, (%rdx)
	movq	8(%rsp), %rax           # 8-byte Reload
	testq	%rax, %rax
	je	.LBB7_48
# BB#47:                                # %if.then135
	movq	%rbx, (%rax)
	jmp	.LBB7_48
.LBB7_45:                               # %if.then132
	movq	$0, (%rdx)
	movq	%rax, %r15
.LBB7_48:                               # %return
	movq	%r15, %rax
	addq	$64, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp122:
	.size	Perl_grok_oct, .Ltmp122-Perl_grok_oct
	.cfi_endproc

	.globl	Perl_scan_bin
	.align	16, 0x90
	.type	Perl_scan_bin,@function
Perl_scan_bin:                          # @Perl_scan_bin
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp125:
	.cfi_def_cfa_offset 16
	subq	$32, %rsp
.Ltmp126:
	.cfi_def_cfa_offset 48
.Ltmp127:
	.cfi_offset %rbx, -16
	movq	%rdx, %rbx
	movq	%rsi, 24(%rsp)
	cmpq	$0, (%rbx)
	je	.LBB8_1
# BB#2:                                 # %entry
	movabsq	$1, %rax
	jmp	.LBB8_3
.LBB8_1:
	movabsq	$0, %rax
.LBB8_3:                                # %entry
	movq	%rax, 8(%rsp)
	leaq	24(%rsp), %rsi
	leaq	8(%rsp), %rdx
	leaq	16(%rsp), %rcx
	callq	Perl_grok_bin
	movq	24(%rsp), %rcx
	movq	%rcx, (%rbx)
	movzbl	8(%rsp), %ecx
	testq	$2, %rcx
	je	.LBB8_5
# BB#4:                                 # %cond.true
	movq	16(%rsp), %rax
	jmp	.LBB8_6
.LBB8_5:                                # %cond.false
	movq	%rax, %rdi
	callq	int64_to_float64
.LBB8_6:                                # %cond.end
	addq	$32, %rsp
	popq	%rbx
	retq
.Ltmp128:
	.size	Perl_scan_bin, .Ltmp128-Perl_scan_bin
	.cfi_endproc

	.globl	Perl_scan_oct
	.align	16, 0x90
	.type	Perl_scan_oct,@function
Perl_scan_oct:                          # @Perl_scan_oct
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp131:
	.cfi_def_cfa_offset 16
	subq	$32, %rsp
.Ltmp132:
	.cfi_def_cfa_offset 48
.Ltmp133:
	.cfi_offset %rbx, -16
	movq	%rdx, %rbx
	movq	%rsi, 24(%rsp)
	cmpq	$0, (%rbx)
	je	.LBB9_1
# BB#2:                                 # %entry
	movabsq	$1, %rax
	jmp	.LBB9_3
.LBB9_1:
	movabsq	$0, %rax
.LBB9_3:                                # %entry
	movq	%rax, 8(%rsp)
	leaq	24(%rsp), %rsi
	leaq	8(%rsp), %rdx
	leaq	16(%rsp), %rcx
	callq	Perl_grok_oct
	movq	24(%rsp), %rcx
	movq	%rcx, (%rbx)
	movzbl	8(%rsp), %ecx
	testq	$2, %rcx
	je	.LBB9_5
# BB#4:                                 # %cond.true
	movq	16(%rsp), %rax
	jmp	.LBB9_6
.LBB9_5:                                # %cond.false
	movq	%rax, %rdi
	callq	int64_to_float64
.LBB9_6:                                # %cond.end
	addq	$32, %rsp
	popq	%rbx
	retq
.Ltmp134:
	.size	Perl_scan_oct, .Ltmp134-Perl_scan_oct
	.cfi_endproc

	.globl	Perl_scan_hex
	.align	16, 0x90
	.type	Perl_scan_hex,@function
Perl_scan_hex:                          # @Perl_scan_hex
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp137:
	.cfi_def_cfa_offset 16
	subq	$32, %rsp
.Ltmp138:
	.cfi_def_cfa_offset 48
.Ltmp139:
	.cfi_offset %rbx, -16
	movq	%rdx, %rbx
	movq	%rsi, 24(%rsp)
	cmpq	$0, (%rbx)
	je	.LBB10_1
# BB#2:                                 # %entry
	movabsq	$1, %rax
	jmp	.LBB10_3
.LBB10_1:
	movabsq	$0, %rax
.LBB10_3:                               # %entry
	movq	%rax, 8(%rsp)
	leaq	24(%rsp), %rsi
	leaq	8(%rsp), %rdx
	leaq	16(%rsp), %rcx
	callq	Perl_grok_hex
	movq	24(%rsp), %rcx
	movq	%rcx, (%rbx)
	movzbl	8(%rsp), %ecx
	testq	$2, %rcx
	je	.LBB10_5
# BB#4:                                 # %cond.true
	movq	16(%rsp), %rax
	jmp	.LBB10_6
.LBB10_5:                               # %cond.false
	movq	%rax, %rdi
	callq	int64_to_float64
.LBB10_6:                               # %cond.end
	addq	$32, %rsp
	popq	%rbx
	retq
.Ltmp140:
	.size	Perl_scan_hex, .Ltmp140-Perl_scan_hex
	.cfi_endproc

	.globl	Perl_grok_numeric_radix
	.align	16, 0x90
	.type	Perl_grok_numeric_radix,@function
Perl_grok_numeric_radix:                # @Perl_grok_numeric_radix
	.cfi_startproc
# BB#0:                                 # %entry
	movq	(%rdi), %rcx
	xorq	%rax, %rax
	cmpq	%rsi, %rcx
	jae	.LBB11_3
# BB#1:                                 # %land.lhs.true
	movzbl	(%rcx), %edx
	cmpq	$46, %rdx
	jne	.LBB11_3
# BB#2:                                 # %if.then
	incq	%rcx
	movq	%rcx, (%rdi)
	movq	$1, %rax
.LBB11_3:                               # %return
	retq
.Ltmp141:
	.size	Perl_grok_numeric_radix, .Ltmp141-Perl_grok_numeric_radix
	.cfi_endproc

	.globl	Perl_grok_number
	.align	16, 0x90
	.type	Perl_grok_number,@function
Perl_grok_number:                       # @Perl_grok_number
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp146:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp147:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp148:
	.cfi_def_cfa_offset 32
.Ltmp149:
	.cfi_offset %rbx, -32
.Ltmp150:
	.cfi_offset %r14, -24
.Ltmp151:
	.cfi_offset %r15, -16
	movq	%rdx, %r14
	leaq	(%rdi,%rsi), %rax
	testq	%rsi, %rsi
	movq	%rdi, %rdx
	jle	.LBB12_5
# BB#1:
	movq	%rdi, %rdx
	.align	16, 0x90
.LBB12_3:                               # %land.rhs
                                        # =>This Inner Loop Header: Depth=1
	movzbl	(%rdx), %ecx
	addq	$-9, %rcx
	andq	$255, %rcx
	cmpq	$23, %rcx
	ja	.LBB12_5
# BB#4:                                 # %land.end
                                        #   in Loop: Header=BB12_3 Depth=1
	movq	$8388635, %rbx          # imm = 0x80001B
	shrq	%cl, %rbx
	testq	$1, %rbx
	je	.LBB12_5
# BB#2:                                 # %while.cond
                                        #   in Loop: Header=BB12_3 Depth=1
	incq	%rdx
	cmpq	%rax, %rdx
	jb	.LBB12_3
.LBB12_5:                               # %while.end
	xorq	%r15, %r15
	cmpq	%rax, %rdx
	je	.LBB12_103
# BB#6:                                 # %if.else
	movzbl	(%rdx), %ecx
	xorq	%r8, %r8
	cmpq	$43, %rcx
	jne	.LBB12_7
# BB#9:                                 # %if.then28
	incq	%rdx
	jmp	.LBB12_10
.LBB12_7:                               # %if.else
	cmpq	$45, %rcx
	jne	.LBB12_10
# BB#8:                                 # %if.then22
	incq	%rdx
	movq	$8, %r8
.LBB12_10:                              # %if.end31
	xorq	%r15, %r15
	cmpq	%rax, %rdx
	je	.LBB12_103
# BB#11:                                # %if.end35
	movsbq	(%rdx), %rbx
	leaq	-48(%rbx), %r10
	movq	%r10, %rcx
	andq	$255, %rcx
	cmpq	$9, %rcx
	ja	.LBB12_47
# BB#12:                                # %if.then42
	leaq	1(%rdx), %rbx
	cmpq	%rax, %rbx
	jae	.LBB12_30
# BB#13:                                # %if.then48
	movsbq	(%rbx), %r11
	addq	$-48, %r11
	movabsq	$4294967295, %r9        # imm = 0xFFFFFFFF
	movq	%r11, %rcx
	andq	%r9, %rcx
	cmpq	$9, %rcx
	ja	.LBB12_30
# BB#14:                                # %if.then56
	leaq	(%r10,%r10,4), %rcx
	leaq	(%r11,%rcx,2), %r10
	leaq	2(%rdx), %rbx
	cmpq	%rax, %rbx
	jae	.LBB12_30
# BB#15:                                # %if.then61
	movsbq	(%rbx), %r11
	addq	$-48, %r11
	movq	%r11, %rcx
	andq	%r9, %rcx
	cmpq	$9, %rcx
	ja	.LBB12_30
# BB#16:                                # %if.then69
	leaq	(%r10,%r10,4), %rcx
	leaq	(%r11,%rcx,2), %r10
	leaq	3(%rdx), %rbx
	cmpq	%rax, %rbx
	jae	.LBB12_30
# BB#17:                                # %if.then76
	movsbq	(%rbx), %r11
	addq	$-48, %r11
	movq	%r11, %rcx
	andq	%r9, %rcx
	cmpq	$9, %rcx
	ja	.LBB12_30
# BB#18:                                # %if.then84
	leaq	(%r10,%r10,4), %rcx
	leaq	(%r11,%rcx,2), %r10
	leaq	4(%rdx), %rbx
	cmpq	%rax, %rbx
	jae	.LBB12_30
# BB#19:                                # %if.then91
	movsbq	(%rbx), %r11
	addq	$-48, %r11
	movq	%r11, %rcx
	andq	%r9, %rcx
	cmpq	$9, %rcx
	ja	.LBB12_30
# BB#20:                                # %if.then99
	leaq	(%r10,%r10,4), %rcx
	leaq	(%r11,%rcx,2), %r10
	leaq	5(%rdx), %rbx
	cmpq	%rax, %rbx
	jae	.LBB12_30
# BB#21:                                # %if.then106
	movsbq	(%rbx), %r11
	addq	$-48, %r11
	movq	%r11, %rcx
	andq	%r9, %rcx
	cmpq	$9, %rcx
	ja	.LBB12_30
# BB#22:                                # %if.then114
	leaq	(%r10,%r10,4), %rcx
	leaq	(%r11,%rcx,2), %r10
	leaq	6(%rdx), %rbx
	cmpq	%rax, %rbx
	jae	.LBB12_30
# BB#23:                                # %if.then121
	movsbq	(%rbx), %r11
	addq	$-48, %r11
	movq	%r11, %rcx
	andq	%r9, %rcx
	cmpq	$9, %rcx
	ja	.LBB12_30
# BB#24:                                # %if.then129
	leaq	(%r10,%r10,4), %rcx
	leaq	(%r11,%rcx,2), %r10
	leaq	7(%rdx), %rbx
	cmpq	%rax, %rbx
	jae	.LBB12_30
# BB#25:                                # %if.then136
	movsbq	(%rbx), %r11
	addq	$-48, %r11
	movq	%r11, %rcx
	andq	%r9, %rcx
	cmpq	$9, %rcx
	ja	.LBB12_30
# BB#26:                                # %if.then144
	leaq	(%r10,%r10,4), %rcx
	leaq	(%r11,%rcx,2), %r10
	leaq	8(%rdx), %rbx
	cmpq	%rax, %rbx
	jae	.LBB12_30
# BB#27:                                # %if.then151
	movsbq	(%rbx), %r11
	addq	$-48, %r11
	movq	%r11, %rcx
	andq	%r9, %rcx
	cmpq	$9, %rcx
	ja	.LBB12_30
# BB#28:                                # %if.then159
	leaq	(%r10,%r10,4), %rcx
	leaq	(%r11,%rcx,2), %r10
	addq	$9, %rdx
	cmpq	%rax, %rdx
	jae	.LBB12_29
.LBB12_32:                              # %while.cond169
                                        # =>This Inner Loop Header: Depth=1
	movsbq	(%rdx), %rbx
	addq	$-48, %rbx
	movq	%rbx, %rcx
	andq	%r9, %rcx
	cmpq	$9, %rcx
	ja	.LBB12_29
# BB#33:                                # %land.rhs175
                                        #   in Loop: Header=BB12_32 Depth=1
	cmpq	$429496729, %r10        # imm = 0x19999999
	jb	.LBB12_36
# BB#34:                                # %lor.rhs178
                                        #   in Loop: Header=BB12_32 Depth=1
	jne	.LBB12_37
# BB#35:                                # %lor.rhs178
                                        #   in Loop: Header=BB12_32 Depth=1
	cmpq	$5, %rbx
	jg	.LBB12_37
.LBB12_36:                              # %while.body187
                                        #   in Loop: Header=BB12_32 Depth=1
	leaq	(%r10,%r10,4), %rcx
	leaq	(%rbx,%rcx,2), %r10
	incq	%rdx
	cmpq	%rax, %rdx
	jb	.LBB12_32
.LBB12_37:                              # %while.end199
	cmpq	%rax, %rdx
	jae	.LBB12_29
# BB#38:                                # %do.body.preheader
	incq	%rdx
.LBB12_39:                              # %do.body
                                        # =>This Inner Loop Header: Depth=1
	movq	%rdx, %rbx
	cmpq	%rax, %rbx
	jae	.LBB12_41
# BB#40:                                # %land.rhs212
                                        #   in Loop: Header=BB12_39 Depth=1
	movzbl	(%rbx), %ecx
	addq	$-48, %rcx
	andq	$255, %rcx
	leaq	1(%rbx), %rdx
	cmpq	$10, %rcx
	jb	.LBB12_39
.LBB12_41:                              # %do.end
	orq	$2, %r8
	jmp	.LBB12_42
.LBB12_47:                              # %if.else263
	cmpq	%rax, %rdx
	jae	.LBB12_67
# BB#48:                                # %if.else263
	movq	%rbx, %rcx
	andq	$255, %rcx
	cmpq	$46, %rcx
	jne	.LBB12_67
# BB#49:                                # %if.then266
	leaq	1(%rdx), %rcx
	cmpq	%rax, %rcx
	jae	.LBB12_103
# BB#50:                                # %land.lhs.true270
	movzbl	(%rcx), %ecx
	addq	$-48, %rcx
	andq	$255, %rcx
	cmpq	$9, %rcx
	ja	.LBB12_103
# BB#51:                                # %do.body279.preheader
	orq	$5, %r8
	addq	$2, %rdx
	.align	16, 0x90
.LBB12_52:                              # %do.body279
                                        # =>This Inner Loop Header: Depth=1
	movq	%rdx, %rbx
	cmpq	%rax, %rbx
	jae	.LBB12_54
# BB#53:                                # %land.rhs284
                                        #   in Loop: Header=BB12_52 Depth=1
	movzbl	(%rbx), %ecx
	addq	$-48, %rcx
	andq	$255, %rcx
	leaq	1(%rbx), %rdx
	cmpq	$10, %rcx
	jb	.LBB12_52
.LBB12_54:                              # %do.end294
	testq	%r14, %r14
	je	.LBB12_56
# BB#55:                                # %if.then296
	movq	$0, (%r14)
	jmp	.LBB12_56
.LBB12_67:                              # %if.else300
	cmpq	$104, %rbx
	jg	.LBB12_75
# BB#68:                                # %if.else300
	andq	$255, %rbx
	cmpq	$73, %rbx
	je	.LBB12_77
# BB#69:                                # %if.else300
	cmpq	$78, %rbx
	jne	.LBB12_103
	jmp	.LBB12_70
.LBB12_75:                              # %if.else300
	andq	$255, %rbx
	cmpq	$110, %rbx
	jne	.LBB12_76
.LBB12_70:                              # %if.then409
	leaq	1(%rdx), %rcx
	cmpq	%rax, %rcx
	je	.LBB12_103
# BB#71:                                # %lor.lhs.false413
	movzbl	(%rcx), %ecx
	orq	$32, %rcx
	andq	$255, %rcx
	cmpq	$97, %rcx
	jne	.LBB12_103
# BB#72:                                # %if.end422
	leaq	2(%rdx), %rcx
	cmpq	%rax, %rcx
	je	.LBB12_103
# BB#73:                                # %lor.lhs.false426
	movzbl	(%rcx), %ecx
	orq	$32, %rcx
	andq	$255, %rcx
	cmpq	$110, %rcx
	jne	.LBB12_103
# BB#74:                                # %if.then447
	addq	$3, %rdx
	orq	$36, %r8
	movq	%rdx, %rbx
	jmp	.LBB12_93
.LBB12_76:                              # %if.else300
	cmpq	$105, %rbx
	jne	.LBB12_103
.LBB12_77:                              # %if.then308
	leaq	1(%rdx), %rcx
	cmpq	%rax, %rcx
	je	.LBB12_103
# BB#78:                                # %lor.lhs.false312
	movzbl	(%rcx), %ecx
	orq	$32, %rcx
	andq	$255, %rcx
	cmpq	$110, %rcx
	jne	.LBB12_103
# BB#79:                                # %if.end321
	leaq	2(%rdx), %rcx
	cmpq	%rax, %rcx
	je	.LBB12_103
# BB#80:                                # %lor.lhs.false325
	movzbl	(%rcx), %ecx
	orq	$32, %rcx
	andq	$255, %rcx
	cmpq	$102, %rcx
	jne	.LBB12_103
# BB#81:                                # %if.end334
	leaq	3(%rdx), %rbx
	cmpq	%rax, %rbx
	jae	.LBB12_92
# BB#82:                                # %land.lhs.true338
	movzbl	(%rbx), %ecx
	orq	$32, %rcx
	andq	$255, %rcx
	cmpq	$105, %rcx
	jne	.LBB12_92
# BB#83:                                # %if.then346
	leaq	4(%rdx), %rcx
	cmpq	%rax, %rcx
	je	.LBB12_103
# BB#84:                                # %lor.lhs.false350
	movzbl	(%rcx), %ecx
	orq	$32, %rcx
	andq	$255, %rcx
	cmpq	$110, %rcx
	jne	.LBB12_103
# BB#85:                                # %if.end359
	leaq	5(%rdx), %rcx
	cmpq	%rax, %rcx
	je	.LBB12_103
# BB#86:                                # %lor.lhs.false363
	movzbl	(%rcx), %ecx
	orq	$32, %rcx
	andq	$255, %rcx
	cmpq	$105, %rcx
	jne	.LBB12_103
# BB#87:                                # %if.end372
	leaq	6(%rdx), %rcx
	cmpq	%rax, %rcx
	je	.LBB12_103
# BB#88:                                # %lor.lhs.false376
	movzbl	(%rcx), %ecx
	orq	$32, %rcx
	andq	$255, %rcx
	cmpq	$116, %rcx
	jne	.LBB12_103
# BB#89:                                # %if.end385
	leaq	7(%rdx), %rcx
	cmpq	%rax, %rcx
	je	.LBB12_103
# BB#90:                                # %lor.lhs.false389
	movzbl	(%rcx), %ecx
	orq	$32, %rcx
	andq	$255, %rcx
	cmpq	$121, %rcx
	jne	.LBB12_103
# BB#91:                                # %if.end398
	addq	$8, %rdx
	movq	%rdx, %rbx
.LBB12_92:                              # %if.then443
	orq	$20, %r8
	jmp	.LBB12_93
.LBB12_29:
	movq	%rdx, %rbx
.LBB12_30:                              # %if.end239
	orq	$1, %r8
	testq	%r14, %r14
	je	.LBB12_42
# BB#31:                                # %if.then241
	movq	%r10, (%r14)
.LBB12_42:                              # %skip_value
	cmpq	%rax, %rbx
	jae	.LBB12_56
# BB#43:                                # %land.lhs.true.i
	movzbl	(%rbx), %ecx
	cmpq	$46, %rcx
	jne	.LBB12_56
# BB#44:                                # %if.then244
	incq	%rbx
	orq	$4, %r8
	movq	%rbx, %rcx
	.align	16, 0x90
.LBB12_45:                              # %while.cond246
                                        # =>This Inner Loop Header: Depth=1
	movq	%rcx, %rbx
	cmpq	%rax, %rbx
	jae	.LBB12_56
# BB#46:                                # %land.rhs249
                                        #   in Loop: Header=BB12_45 Depth=1
	movzbl	(%rbx), %edx
	addq	$-48, %rdx
	andq	$255, %rdx
	leaq	1(%rbx), %rcx
	cmpq	$10, %rdx
	jb	.LBB12_45
.LBB12_56:                              # %if.else450
	cmpq	%rax, %rbx
	jae	.LBB12_93
# BB#57:                                # %if.then453
	movzbl	(%rbx), %ecx
	orq	$32, %rcx
	andq	$255, %rcx
	cmpq	$101, %rcx
	jne	.LBB12_93
# BB#58:                                # %if.then461
	leaq	1(%rbx), %rcx
	cmpq	%rax, %rcx
	jae	.LBB12_62
# BB#59:                                # %land.lhs.true467
	movzbl	(%rcx), %edx
	cmpq	$43, %rdx
	je	.LBB12_61
# BB#60:                                # %land.lhs.true467
	cmpq	$45, %rdx
	jne	.LBB12_62
.LBB12_61:                              # %if.then475
	addq	$2, %rbx
	movq	%rbx, %rcx
.LBB12_62:                              # %if.end477
	cmpq	%rax, %rcx
	jae	.LBB12_103
# BB#63:                                # %land.lhs.true480
	movzbl	(%rcx), %edx
	addq	$-48, %rdx
	andq	$255, %rdx
	cmpq	$9, %rdx
	ja	.LBB12_103
# BB#64:                                # %do.body489.preheader
	andq	$8, %r8
	orq	$4, %r8
	incq	%rcx
	.align	16, 0x90
.LBB12_65:                              # %do.body489
                                        # =>This Inner Loop Header: Depth=1
	movq	%rcx, %rbx
	cmpq	%rax, %rbx
	jae	.LBB12_93
# BB#66:                                # %land.rhs494
                                        #   in Loop: Header=BB12_65 Depth=1
	movzbl	(%rbx), %edx
	addq	$-48, %rdx
	andq	$255, %rdx
	leaq	1(%rbx), %rcx
	cmpq	$10, %rdx
	jb	.LBB12_65
.LBB12_93:                              # %while.cond511.preheader
	cmpq	%rax, %rbx
	jae	.LBB12_94
	.align	16, 0x90
.LBB12_97:                              # %land.rhs514
                                        # =>This Inner Loop Header: Depth=1
	movzbl	(%rbx), %ecx
	addq	$-9, %rcx
	andq	$255, %rcx
	cmpq	$23, %rcx
	ja	.LBB12_99
# BB#98:                                # %land.end535
                                        #   in Loop: Header=BB12_97 Depth=1
	movq	$8388635, %rdx          # imm = 0x80001B
	shrq	%cl, %rdx
	testq	$1, %rdx
	je	.LBB12_99
# BB#95:                                # %while.cond511
                                        #   in Loop: Header=BB12_97 Depth=1
	incq	%rbx
	cmpq	%rax, %rbx
	jb	.LBB12_97
# BB#96:
	movq	%r8, %r15
	jmp	.LBB12_103
.LBB12_94:
	movq	%r8, %r15
	jmp	.LBB12_103
.LBB12_99:                              # %if.end542
	cmpq	$10, %rsi
	jne	.LBB12_103
# BB#100:                               # %land.lhs.true545
	movabsq	$.L.str9, %rsi
	movq	$10, %rdx
	callq	memcmp
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB12_103
# BB#101:                               # %if.then548
	movq	$1, %r15
	testq	%r14, %r14
	je	.LBB12_103
# BB#102:                               # %if.then550
	movq	$0, (%r14)
.LBB12_103:                             # %return
	movq	%r15, %rax
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp152:
	.size	Perl_grok_number, .Ltmp152-Perl_grok_number
	.cfi_endproc

	.globl	Perl_my_atof
	.align	16, 0x90
	.type	Perl_my_atof,@function
Perl_my_atof:                           # @Perl_my_atof
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rax
.Ltmp154:
	.cfi_def_cfa_offset 16
	movq	$0, (%rsp)
	leaq	(%rsp), %rsi
	callq	Perl_my_atof2
	movq	(%rsp), %rax
	popq	%rdx
	retq
.Ltmp155:
	.size	Perl_my_atof, .Ltmp155-Perl_my_atof
	.cfi_endproc

	.globl	Perl_my_atof2
	.align	16, 0x90
	.type	Perl_my_atof2,@function
Perl_my_atof2:                          # @Perl_my_atof2
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
	subq	$160, %rsp
.Ltmp167:
	.cfi_def_cfa_offset 208
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
	movq	%rsi, %r12
	movq	%rdi, %r15
	movq	$0, 144(%rsp)
	movq	$0, 136(%rsp)
	movq	$0, 128(%rsp)
	movq	$0, 120(%rsp)
	movq	$0, 112(%rsp)
                                        # kill: RDI<def> R15<kill>
	callq	strlen
	movq	$-1, 104(%rsp)
	movq	$-1, 96(%rsp)
	movzbl	(%r15), %edx
	leaq	-9(%rdx), %rcx
	movq	%rcx, %rsi
	andq	$255, %rsi
	movq	%r15, %r14
	cmpq	$24, %rsi
	jae	.LBB14_3
	.align	16, 0x90
.LBB14_2:                               # %lor.end
                                        # =>This Inner Loop Header: Depth=1
	andq	$255, %rcx
	movq	$8388635, %rsi          # imm = 0x80001B
	shrq	%cl, %rsi
	testq	$1, %rsi
	je	.LBB14_3
# BB#1:                                 # %while.cond
                                        #   in Loop: Header=BB14_2 Depth=1
	movzbl	1(%r14), %edx
	leaq	-9(%rdx), %rcx
	incq	%r14
	movq	%rcx, %rsi
	andq	$255, %rsi
	cmpq	$23, %rsi
	jbe	.LBB14_2
.LBB14_3:                               # %while.end
	leaq	-1(%rax,%r15), %rbx
	xorq	%rsi, %rsi
	movq	$56, %rcx
	movq	%rdx, %rax
	shlq	%cl, %rax
	movq	$56, %rcx
	sarq	%cl, %rax
	andq	$255, %rdx
	cmpq	$43, %rdx
	je	.LBB14_7
# BB#4:                                 # %while.end
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	cmpq	$45, %rax
	jne	.LBB14_5
# BB#6:                                 # %sw.bb
	movq	$1, %rsi
.LBB14_7:                               # %sw.bb18
	movq	%rsi, 64(%rsp)          # 8-byte Spill
	movq	%r12, 72(%rsp)          # 8-byte Spill
	incq	%r14
	jmp	.LBB14_8
.LBB14_5:
	movq	$0, 64(%rsp)            # 8-byte Folded Spill
	movq	%r12, 72(%rsp)          # 8-byte Spill
.LBB14_8:                               # %while.body21.preheader
	xorq	%rdi, %rdi
	movabsq	$1, %rdx
	xorq	%rsi, %rsi
	movq	$0, 80(%rsp)            # 8-byte Folded Spill
	xorq	%r15, %r15
	xorq	%r12, %r12
	xorq	%r13, %r13
	jmp	.LBB14_9
.LBB14_47:                              # %if.end92
                                        #   in Loop: Header=BB14_9 Depth=1
	addq	%r12, %rcx
	movq	%rcx, 112(%rsp,%r15,8)
	movq	%rax, 96(%rsp,%r15,8)
	movq	$1, %rdi
	.align	16, 0x90
.LBB14_9:                               # %while.body21.outer
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB14_18 Depth 2
                                        #     Child Loop BB14_37 Depth 2
                                        #     Child Loop BB14_29 Depth 2
	movq	%r12, %rax
	cmpq	$18, %r13
	jl	.LBB14_18
# BB#10:                                # %while.body21.us
                                        #   in Loop: Header=BB14_9 Depth=1
	movzbl	(%r14), %r12d
	leaq	-48(%r12), %rcx
	andq	$255, %rcx
	cmpq	$10, %rcx
	jb	.LBB14_19
	jmp	.LBB14_11
	.align	16, 0x90
.LBB14_51:                              # %if.then105
                                        #   in Loop: Header=BB14_18 Depth=2
	incq	%r14
	movq	$1, %r15
.LBB14_18:                              # %while.body21
                                        #   Parent Loop BB14_9 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movzbl	(%r14), %r12d
	leaq	-48(%r12), %rcx
	andq	$255, %rcx
	cmpq	$9, %rcx
	jbe	.LBB14_19
# BB#48:                                # %if.else99
                                        #   in Loop: Header=BB14_18 Depth=2
	testq	%r15, %r15
	jne	.LBB14_16
# BB#49:                                # %land.lhs.true101
                                        #   in Loop: Header=BB14_18 Depth=2
	xorq	%r15, %r15
	cmpq	%rbx, %r14
	jae	.LBB14_16
# BB#50:                                # %land.lhs.true101
                                        #   in Loop: Header=BB14_18 Depth=2
	andq	$255, %r12
	cmpq	$46, %r12
	je	.LBB14_51
	jmp	.LBB14_16
	.align	16, 0x90
.LBB14_19:                              # %if.then
                                        #   in Loop: Header=BB14_9 Depth=1
	incq	%r14
	movq	$56, %rcx
	shlq	%cl, %r12
	movq	$56, %rcx
	sarq	%cl, %r12
	addq	$-48, %r12
	addq	%r15, %rsi
	movq	%r12, %rcx
	orq	%r13, %rcx
	movq	$1, %rdi
	je	.LBB14_9
# BB#20:                                # %if.end37
                                        #   in Loop: Header=BB14_9 Depth=1
	cmpq	$17, %r13
	leaq	1(%r13), %r13
	jge	.LBB14_21
# BB#31:                                # %if.else79
                                        #   in Loop: Header=BB14_9 Depth=1
	movq	112(%rsp,%r15,8), %rcx
	cmpq	$429496728, %rcx        # imm = 0x19999998
	jbe	.LBB14_32
# BB#33:                                # %if.then83
                                        #   in Loop: Header=BB14_9 Depth=1
	movq	%rcx, 24(%rsp)          # 8-byte Spill
	movq	96(%rsp,%r15,8), %rcx
	movq	128(%rsp,%r15,8), %rax
	movq	%r15, 56(%rsp)          # 8-byte Spill
	testq	%rcx, %rcx
	jne	.LBB14_35
# BB#34:                                #   in Loop: Header=BB14_9 Depth=1
	movq	%rsi, 40(%rsp)          # 8-byte Spill
	jmp	.LBB14_45
.LBB14_32:                              # %if.else79.if.end92_crit_edge
                                        #   in Loop: Header=BB14_9 Depth=1
	movq	96(%rsp,%r15,8), %rax
	addq	%rcx, %rcx
	leaq	(%rcx,%rcx,4), %rcx
	incq	%rax
	jmp	.LBB14_47
.LBB14_35:                              # %if.end.i
                                        #   in Loop: Header=BB14_9 Depth=1
	movq	%rcx, %r15
	movq	%rsi, 40(%rsp)          # 8-byte Spill
	xorq	%rsi, %rsi
	movq	$0, 88(%rsp)            # 8-byte Folded Spill
	movq	%rax, %rdi
	movq	%rdi, 16(%rsp)          # 8-byte Spill
	callq	__eqdf2
	testq	%rax, %rax
	je	.LBB14_46
# BB#36:                                # %if.end3.i
                                        #   in Loop: Header=BB14_9 Depth=1
	movq	$63, %rcx
	movq	%r15, 8(%rsp)           # 8-byte Spill
	movq	%r15, %rax
	sarq	%cl, %rax
	leaq	(%r15,%rax), %r15
	xorq	%rax, %r15
	movq	$1, %rcx
	movabsq	$4621819117588971520, %rax # imm = 0x4024000000000000
	movq	%rax, %rdx
	movabsq	$4607182418800017408, %rax # imm = 0x3FF0000000000000
	movq	%rax, 48(%rsp)          # 8-byte Spill
	je	.LBB14_42
.LBB14_37:                              # %for.body.i
                                        #   Parent Loop BB14_9 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	%rcx, 88(%rsp)          # 8-byte Spill
	testq	%rcx, %r15
	je	.LBB14_41
# BB#38:                                # %if.then8.i
                                        #   in Loop: Header=BB14_37 Depth=2
	movq	%rdx, %rdi
	movq	%rdx, 32(%rsp)          # 8-byte Spill
	movq	48(%rsp), %rsi          # 8-byte Reload
	callq	float64_mul
	movq	88(%rsp), %rcx          # 8-byte Reload
	cmpq	%rcx, %r15
	je	.LBB14_39
# BB#40:                                #   in Loop: Header=BB14_37 Depth=2
	movq	%rax, 48(%rsp)          # 8-byte Spill
	xorq	%rcx, %r15
	movq	32(%rsp), %rdx          # 8-byte Reload
.LBB14_41:                              # %if.end12.i
                                        #   in Loop: Header=BB14_37 Depth=2
	movq	%rdx, %rdi
	movq	%rdx, %rsi
	callq	float64_mul
	movq	%rax, %rdx
	movq	88(%rsp), %rax          # 8-byte Reload
	addq	%rax, %rax
	testq	%r15, %r15
	movq	%rax, %rcx
	jne	.LBB14_37
	jmp	.LBB14_42
.LBB14_21:                              # %if.then41
                                        #   in Loop: Header=BB14_9 Depth=1
	cmpq	$6, %r12
	jge	.LBB14_24
# BB#22:                                # %if.else
                                        #   in Loop: Header=BB14_9 Depth=1
	cmpq	$5, %r12
	jne	.LBB14_25
# BB#23:                                # %if.else
                                        #   in Loop: Header=BB14_9 Depth=1
	andq	$1, %rax
	je	.LBB14_25
.LBB14_24:                              # %if.then51
                                        #   in Loop: Header=BB14_9 Depth=1
	incq	112(%rsp,%r15,8)
.LBB14_25:                              # %if.end56
                                        #   in Loop: Header=BB14_9 Depth=1
	testq	%r15, %r15
	movabsq	$0, %rax
	jne	.LBB14_27
# BB#26:                                # %if.end56
                                        #   in Loop: Header=BB14_9 Depth=1
	movq	%rdx, %rax
.LBB14_27:                              # %if.end56
                                        #   in Loop: Header=BB14_9 Depth=1
	movq	%r15, %rcx
	negq	%rcx
	sbbq	%rcx, %rcx
	movq	%rdx, %rdi
	movzbl	(%r14), %edx
	addq	%rcx, %rsi
	addq	%rax, 80(%rsp)          # 8-byte Folded Spill
	addq	$-48, %rdx
	andq	$255, %rdx
	cmpq	$9, %rdx
	movq	%rdi, %rdx
	movq	$1, %rdi
	ja	.LBB14_9
# BB#28:                                # %while.body71.lr.ph
                                        #   in Loop: Header=BB14_9 Depth=1
	movq	%rdx, %r8
	movq	%r15, %rax
	xorq	$1, %rax
	movq	80(%rsp), %rdx          # 8-byte Reload
.LBB14_29:                              # %while.body71
                                        #   Parent Loop BB14_9 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movzbl	1(%r14), %ecx
	addq	%rax, %rdx
	incq	%r14
	addq	$-48, %rcx
	andq	$255, %rcx
	cmpq	$10, %rcx
	jb	.LBB14_29
# BB#30:                                #   in Loop: Header=BB14_9 Depth=1
	movq	%rdx, 80(%rsp)          # 8-byte Spill
	movq	%r8, %rdx
	jmp	.LBB14_9
.LBB14_39:                              #   in Loop: Header=BB14_9 Depth=1
	movq	%rax, 48(%rsp)          # 8-byte Spill
.LBB14_42:                              # %for.end.i
                                        #   in Loop: Header=BB14_9 Depth=1
	cmpq	$0, 8(%rsp)             # 8-byte Folded Reload
	js	.LBB14_43
# BB#44:                                # %cond.false.i
                                        #   in Loop: Header=BB14_9 Depth=1
	movq	16(%rsp), %rdi          # 8-byte Reload
	movq	48(%rsp), %rsi          # 8-byte Reload
	callq	float64_mul
	jmp	.LBB14_45
.LBB14_43:                              # %cond.true.i
                                        #   in Loop: Header=BB14_9 Depth=1
	movq	16(%rsp), %rdi          # 8-byte Reload
	movq	48(%rsp), %rsi          # 8-byte Reload
	callq	float64_div
.LBB14_45:                              # %S_mulexp10.exit
                                        #   in Loop: Header=BB14_9 Depth=1
	movq	%rax, 88(%rsp)          # 8-byte Spill
.LBB14_46:                              # %S_mulexp10.exit
                                        #   in Loop: Header=BB14_9 Depth=1
	movq	24(%rsp), %rdi          # 8-byte Reload
	callq	int64_to_float64
	movq	88(%rsp), %rdi          # 8-byte Reload
	movq	%rax, %rsi
	callq	float64_add
	movq	56(%rsp), %r15          # 8-byte Reload
	movq	%rax, 128(%rsp,%r15,8)
	movq	$0, 112(%rsp,%r15,8)
	movq	$0, 96(%rsp,%r15,8)
	xorq	%rcx, %rcx
	movq	$1, %rax
	movq	40(%rsp), %rsi          # 8-byte Reload
	movabsq	$1, %rdx
	jmp	.LBB14_47
.LBB14_11:                              # %if.else99.us
	testq	%r15, %r15
	jne	.LBB14_16
# BB#12:                                # %land.lhs.true101.us
	xorq	%r15, %r15
	cmpq	%rbx, %r14
	jae	.LBB14_16
# BB#13:                                # %land.lhs.true101.us
	andq	$255, %r12
	cmpq	$46, %r12
	jne	.LBB14_16
# BB#14:                                # %while.cond110.preheader
	incq	%r14
	movq	$1, %r15
	.align	16, 0x90
.LBB14_15:                              # %while.cond110
                                        # =>This Inner Loop Header: Depth=1
	movzbl	1(%r14), %eax
	incq	%r14
	addq	$-48, %rax
	andq	$255, %rax
	cmpq	$10, %rax
	jb	.LBB14_15
.LBB14_16:                              # %while.end126
	movq	%rdi, 56(%rsp)          # 8-byte Spill
	movq	%rsi, 40(%rsp)          # 8-byte Spill
	movq	96(%rsp), %r12
	movq	128(%rsp), %rbx
	testq	%r12, %r12
	je	.LBB14_17
# BB#52:                                # %if.end.i350
	xorq	%rsi, %rsi
	xorq	%r13, %r13
	movq	%rbx, %rdi
	callq	__eqdf2
	testq	%rax, %rax
	je	.LBB14_64
# BB#53:                                # %if.end3.i355
	movq	%rbx, 32(%rsp)          # 8-byte Spill
	movq	$63, %rcx
	movq	%r12, 48(%rsp)          # 8-byte Spill
	movq	%r12, %rax
	sarq	%cl, %rax
	leaq	(%r12,%rax), %r13
	xorq	%rax, %r13
	movabsq	$4607182418800017408, %rax # imm = 0x3FF0000000000000
	movq	%rax, 88(%rsp)          # 8-byte Spill
	je	.LBB14_60
# BB#54:
	movabsq	$4621819117588971520, %rbx # imm = 0x4024000000000000
	movq	$1, %r12
	.align	16, 0x90
.LBB14_55:                              # %for.body.i362
                                        # =>This Inner Loop Header: Depth=1
	testq	%r12, %r13
	je	.LBB14_59
# BB#56:                                # %if.then8.i366
                                        #   in Loop: Header=BB14_55 Depth=1
	movq	%rbx, %rdi
	movq	88(%rsp), %rsi          # 8-byte Reload
	callq	float64_mul
	cmpq	%r12, %r13
	je	.LBB14_57
# BB#58:                                #   in Loop: Header=BB14_55 Depth=1
	movq	%rax, 88(%rsp)          # 8-byte Spill
	xorq	%r12, %r13
.LBB14_59:                              # %if.end12.i372
                                        #   in Loop: Header=BB14_55 Depth=1
	movq	%rbx, %rdi
	movq	%rbx, %rsi
	callq	float64_mul
	movq	%rax, %rbx
	addq	%r12, %r12
	testq	%r13, %r13
	jne	.LBB14_55
	jmp	.LBB14_60
.LBB14_17:
	movq	%rbx, %r13
	jmp	.LBB14_64
.LBB14_57:
	movq	%rax, 88(%rsp)          # 8-byte Spill
.LBB14_60:                              # %for.end.i375
	cmpq	$0, 48(%rsp)            # 8-byte Folded Reload
	js	.LBB14_61
# BB#62:                                # %cond.false.i379
	movq	32(%rsp), %rdi          # 8-byte Reload
	movq	88(%rsp), %rsi          # 8-byte Reload
	callq	float64_mul
	jmp	.LBB14_63
.LBB14_61:                              # %cond.true.i377
	movq	32(%rsp), %rdi          # 8-byte Reload
	movq	88(%rsp), %rsi          # 8-byte Reload
	callq	float64_div
.LBB14_63:                              # %S_mulexp10.exit381
	movq	%rax, %r13
.LBB14_64:                              # %S_mulexp10.exit381
	movq	112(%rsp), %rdi
	callq	int64_to_float64
	movq	%r13, %rdi
	movq	%rax, %rsi
	callq	float64_add
	movq	%rax, %rbx
	movq	%rbx, 128(%rsp)
	testq	%r15, %r15
	movq	80(%rsp), %r12          # 8-byte Reload
	je	.LBB14_80
# BB#65:                                # %if.then135
	movq	%rbx, 48(%rsp)          # 8-byte Spill
	movq	104(%rsp), %rax
	movq	136(%rsp), %rbx
	testq	%rax, %rax
	je	.LBB14_66
# BB#67:                                # %if.end.i316
	movq	%rax, 32(%rsp)          # 8-byte Spill
	xorq	%rsi, %rsi
	xorq	%r13, %r13
	movq	%rbx, %rdi
	callq	__eqdf2
	testq	%rax, %rax
	je	.LBB14_79
# BB#68:                                # %if.end3.i321
	movq	%rbx, 24(%rsp)          # 8-byte Spill
	movq	$63, %rcx
	movq	32(%rsp), %rdx          # 8-byte Reload
	movq	%rdx, %rax
	sarq	%cl, %rax
	leaq	(%rdx,%rax), %r12
	xorq	%rax, %r12
	movabsq	$4607182418800017408, %rax # imm = 0x3FF0000000000000
	movq	%rax, 88(%rsp)          # 8-byte Spill
	je	.LBB14_75
# BB#69:
	movabsq	$4621819117588971520, %rbx # imm = 0x4024000000000000
	movq	$1, %r13
	.align	16, 0x90
.LBB14_70:                              # %for.body.i328
                                        # =>This Inner Loop Header: Depth=1
	testq	%r13, %r12
	je	.LBB14_74
# BB#71:                                # %if.then8.i332
                                        #   in Loop: Header=BB14_70 Depth=1
	movq	%rbx, %rdi
	movq	88(%rsp), %rsi          # 8-byte Reload
	callq	float64_mul
	cmpq	%r13, %r12
	je	.LBB14_72
# BB#73:                                #   in Loop: Header=BB14_70 Depth=1
	movq	%rax, 88(%rsp)          # 8-byte Spill
	xorq	%r13, %r12
.LBB14_74:                              # %if.end12.i338
                                        #   in Loop: Header=BB14_70 Depth=1
	movq	%rbx, %rdi
	movq	%rbx, %rsi
	callq	float64_mul
	movq	%rax, %rbx
	addq	%r13, %r13
	testq	%r12, %r12
	jne	.LBB14_70
	jmp	.LBB14_75
.LBB14_66:
	movq	%rbx, %r13
	jmp	.LBB14_79
.LBB14_72:
	movq	%rax, 88(%rsp)          # 8-byte Spill
.LBB14_75:                              # %for.end.i341
	cmpq	$0, 32(%rsp)            # 8-byte Folded Reload
	js	.LBB14_76
# BB#77:                                # %cond.false.i345
	movq	24(%rsp), %rdi          # 8-byte Reload
	movq	88(%rsp), %rsi          # 8-byte Reload
	callq	float64_mul
	jmp	.LBB14_78
.LBB14_76:                              # %cond.true.i343
	movq	24(%rsp), %rdi          # 8-byte Reload
	movq	88(%rsp), %rsi          # 8-byte Reload
	callq	float64_div
.LBB14_78:                              # %S_mulexp10.exit347
	movq	%rax, %r13
	movq	80(%rsp), %r12          # 8-byte Reload
.LBB14_79:                              # %S_mulexp10.exit347
	movq	120(%rsp), %rdi
	callq	int64_to_float64
	movq	%r13, %rdi
	movq	%rax, %rsi
	callq	float64_add
	movq	%rax, 136(%rsp)
	movq	48(%rsp), %rbx          # 8-byte Reload
.LBB14_80:                              # %if.end143
	xorq	%r13, %r13
	testq	$255, 56(%rsp)          # 8-byte Folded Reload
	je	.LBB14_91
# BB#81:                                # %land.lhs.true146
	movzbl	(%r14), %eax
	orq	$32, %rax
	andq	$255, %rax
	cmpq	$101, %rax
	jne	.LBB14_91
# BB#82:                                # %if.then154
	movsbq	1(%r14), %rdx
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rdx, %rcx
	xorq	%rax, %rax
	cmpq	$43, %rcx
	je	.LBB14_86
# BB#83:                                # %if.then154
	cmpq	$45, %rcx
	jne	.LBB14_84
# BB#85:                                # %sw.bb157
	movq	$1, %rax
.LBB14_86:                              # %sw.bb158
	movzbl	2(%r14), %edx
	addq	$2, %r14
	jmp	.LBB14_87
.LBB14_84:
	incq	%r14
.LBB14_87:                              # %while.cond161.preheader
	leaq	-48(%rdx), %rcx
	andq	$255, %rcx
	xorq	%r13, %r13
	cmpq	$9, %rcx
	ja	.LBB14_89
	.align	16, 0x90
.LBB14_88:                              # %while.body170
                                        # =>This Inner Loop Header: Depth=1
	leaq	(%r13,%r13,4), %rsi
	movq	$56, %rcx
	shlq	%cl, %rdx
	movq	$56, %rcx
	sarq	%cl, %rdx
	leaq	-48(%rdx,%rsi,2), %r13
	movzbl	1(%r14), %edx
	incq	%r14
	leaq	-48(%rdx), %rcx
	andq	$255, %rcx
	cmpq	$10, %rcx
	jb	.LBB14_88
.LBB14_89:                              # %while.end177
	testq	%rax, %rax
	je	.LBB14_91
# BB#90:                                # %while.end177
	negq	%r13
.LBB14_91:                              # %if.end182
	addq	%r13, %r12
	je	.LBB14_92
# BB#93:                                # %if.end.i282
	xorq	%rsi, %rsi
	movq	$0, 88(%rsp)            # 8-byte Folded Spill
	movq	%rbx, %rdi
	callq	__eqdf2
	testq	%rax, %rax
	je	.LBB14_105
# BB#94:                                # %if.end3.i287
	movq	%rbx, 48(%rsp)          # 8-byte Spill
	movq	%r15, 56(%rsp)          # 8-byte Spill
	movq	$63, %rcx
	movq	%r12, %rax
	sarq	%cl, %rax
	movq	%r12, 80(%rsp)          # 8-byte Spill
	leaq	(%r12,%rax), %r12
	xorq	%rax, %r12
	movabsq	$4607182418800017408, %rax # imm = 0x3FF0000000000000
	movq	%rax, 88(%rsp)          # 8-byte Spill
	je	.LBB14_101
# BB#95:
	movabsq	$4621819117588971520, %rbx # imm = 0x4024000000000000
	movq	$1, %r15
	.align	16, 0x90
.LBB14_96:                              # %for.body.i294
                                        # =>This Inner Loop Header: Depth=1
	testq	%r15, %r12
	je	.LBB14_100
# BB#97:                                # %if.then8.i298
                                        #   in Loop: Header=BB14_96 Depth=1
	movq	%rbx, %rdi
	movq	88(%rsp), %rsi          # 8-byte Reload
	callq	float64_mul
	cmpq	%r15, %r12
	je	.LBB14_98
# BB#99:                                #   in Loop: Header=BB14_96 Depth=1
	movq	%rax, 88(%rsp)          # 8-byte Spill
	xorq	%r15, %r12
.LBB14_100:                             # %if.end12.i304
                                        #   in Loop: Header=BB14_96 Depth=1
	movq	%rbx, %rdi
	movq	%rbx, %rsi
	callq	float64_mul
	movq	%rax, %rbx
	addq	%r15, %r15
	testq	%r12, %r12
	jne	.LBB14_96
	jmp	.LBB14_101
.LBB14_92:
	movq	%rbx, 88(%rsp)          # 8-byte Spill
	jmp	.LBB14_105
.LBB14_98:
	movq	%rax, 88(%rsp)          # 8-byte Spill
.LBB14_101:                             # %for.end.i307
	cmpq	$0, 80(%rsp)            # 8-byte Folded Reload
	js	.LBB14_102
# BB#103:                               # %cond.false.i311
	movq	48(%rsp), %rdi          # 8-byte Reload
	movq	88(%rsp), %rsi          # 8-byte Reload
	callq	float64_mul
	jmp	.LBB14_104
.LBB14_102:                             # %cond.true.i309
	movq	48(%rsp), %rdi          # 8-byte Reload
	movq	88(%rsp), %rsi          # 8-byte Reload
	callq	float64_div
.LBB14_104:                             # %S_mulexp10.exit313
	movq	%rax, 88(%rsp)          # 8-byte Spill
	movq	56(%rsp), %r15          # 8-byte Reload
.LBB14_105:                             # %S_mulexp10.exit313
	testq	%r15, %r15
	je	.LBB14_121
# BB#106:                               # %if.then184
	movq	136(%rsp), %rbx
	movq	40(%rsp), %rax          # 8-byte Reload
	subq	%rax, %r13
	jne	.LBB14_108
# BB#107:
	movq	%rbx, %r15
	jmp	.LBB14_120
.LBB14_121:                             # %if.else195
	movq	88(%rsp), %rax          # 8-byte Reload
	jmp	.LBB14_122
.LBB14_108:                             # %if.end.i248
	xorq	%rsi, %rsi
	xorq	%r15, %r15
	movq	%rbx, %rdi
	callq	__eqdf2
	testq	%rax, %rax
	je	.LBB14_120
# BB#109:                               # %if.end3.i253
	movq	%rbx, 56(%rsp)          # 8-byte Spill
	movq	$63, %rcx
	movq	%r13, %rax
	sarq	%cl, %rax
	leaq	(%r13,%rax), %r12
	xorq	%rax, %r12
	movabsq	$4607182418800017408, %rax # imm = 0x3FF0000000000000
	movq	%rax, 80(%rsp)          # 8-byte Spill
	je	.LBB14_116
# BB#110:
	movabsq	$4621819117588971520, %rbx # imm = 0x4024000000000000
	movq	$1, %r15
	.align	16, 0x90
.LBB14_111:                             # %for.body.i260
                                        # =>This Inner Loop Header: Depth=1
	testq	%r15, %r12
	je	.LBB14_115
# BB#112:                               # %if.then8.i264
                                        #   in Loop: Header=BB14_111 Depth=1
	movq	%rbx, %rdi
	movq	80(%rsp), %rsi          # 8-byte Reload
	callq	float64_mul
	cmpq	%r15, %r12
	je	.LBB14_113
# BB#114:                               #   in Loop: Header=BB14_111 Depth=1
	movq	%rax, 80(%rsp)          # 8-byte Spill
	xorq	%r15, %r12
.LBB14_115:                             # %if.end12.i270
                                        #   in Loop: Header=BB14_111 Depth=1
	movq	%rbx, %rdi
	movq	%rbx, %rsi
	callq	float64_mul
	movq	%rax, %rbx
	addq	%r15, %r15
	testq	%r12, %r12
	jne	.LBB14_111
	jmp	.LBB14_116
.LBB14_113:
	movq	%rax, 80(%rsp)          # 8-byte Spill
.LBB14_116:                             # %for.end.i273
	testq	%r13, %r13
	js	.LBB14_117
# BB#118:                               # %cond.false.i277
	movq	56(%rsp), %rdi          # 8-byte Reload
	movq	80(%rsp), %rsi          # 8-byte Reload
	callq	float64_mul
	jmp	.LBB14_119
.LBB14_117:                             # %cond.true.i275
	movq	56(%rsp), %rdi          # 8-byte Reload
	movq	80(%rsp), %rsi          # 8-byte Reload
	callq	float64_div
.LBB14_119:                             # %S_mulexp10.exit279
	movq	%rax, %r15
.LBB14_120:                             # %S_mulexp10.exit279
	movq	88(%rsp), %rdi          # 8-byte Reload
	movq	%r15, %rsi
	callq	float64_add
.LBB14_122:                             # %if.end201
	movq	%rax, 144(%rsp)
	cmpq	$0, 64(%rsp)            # 8-byte Folded Reload
	movq	72(%rsp), %rcx          # 8-byte Reload
	je	.LBB14_124
# BB#123:                               # %if.then203
	movabsq	$-9223372036854775808, %rdx # imm = 0x8000000000000000
	xorq	%rdx, %rax
	movq	%rax, 144(%rsp)
.LBB14_124:                             # %if.end207
	movq	%rax, (%rcx)
	movq	%r14, %rax
	addq	$160, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp173:
	.size	Perl_my_atof2, .Ltmp173-Perl_my_atof2
	.cfi_endproc

	.type	.L.str,@object          # @.str
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.asciz	"Integer overflow in binary number"
	.size	.L.str, 34

	.type	.L.str1,@object         # @.str1
.L.str1:
	.asciz	"Illegal binary digit '%c' ignored"
	.size	.L.str1, 34

	.type	.L.str2,@object         # @.str2
.L.str2:
	.asciz	"Binary number > 0b11111111111111111111111111111111 non-portable"
	.size	.L.str2, 64

	.type	.L.str3,@object         # @.str3
.L.str3:
	.asciz	"Integer overflow in hexadecimal number"
	.size	.L.str3, 39

	.type	.L.str4,@object         # @.str4
.L.str4:
	.asciz	"Illegal hexadecimal digit '%c' ignored"
	.size	.L.str4, 39

	.type	.L.str5,@object         # @.str5
.L.str5:
	.asciz	"Hexadecimal number > 0xffffffff non-portable"
	.size	.L.str5, 45

	.type	.L.str6,@object         # @.str6
.L.str6:
	.asciz	"Integer overflow in octal number"
	.size	.L.str6, 33

	.type	.L.str7,@object         # @.str7
.L.str7:
	.asciz	"Illegal octal digit '%c' ignored"
	.size	.L.str7, 33

	.type	.L.str8,@object         # @.str8
.L.str8:
	.asciz	"Octal number > 037777777777 non-portable"
	.size	.L.str8, 41

	.type	.L.str9,@object         # @.str9
.L.str9:
	.asciz	"0 but true"
	.size	.L.str9, 11


	.ident	"clang version 3.5 (trunk)"
	.section	".note.GNU-stack","",@progbits
