	.file	"mg.c.bc"
	.text
	.globl	Perl_mg_magical
	.align	16, 0x90
	.type	Perl_mg_magical,@function
Perl_mg_magical:                        # @Perl_mg_magical
	.cfi_startproc
# BB#0:                                 # %entry
	movq	(%rdi), %rax
	movq	40(%rax), %rax
	jmp	.LBB0_1
	.align	16, 0x90
.LBB0_11:                               # %for.inc
                                        #   in Loop: Header=BB0_1 Depth=1
	movq	(%rax), %rax
.LBB0_1:                                # %entry
                                        # =>This Inner Loop Header: Depth=1
	testq	%rax, %rax
	je	.LBB0_12
# BB#2:                                 # %for.body
                                        #   in Loop: Header=BB0_1 Depth=1
	movq	8(%rax), %rcx
	testq	%rcx, %rcx
	je	.LBB0_11
# BB#3:                                 # %if.then
                                        #   in Loop: Header=BB0_1 Depth=1
	cmpq	$0, (%rcx)
	je	.LBB0_6
# BB#4:                                 # %land.lhs.true
                                        #   in Loop: Header=BB0_1 Depth=1
	movzbl	19(%rax), %edx
	testq	$4, %rdx
	jne	.LBB0_6
# BB#5:                                 # %if.then4
                                        #   in Loop: Header=BB0_1 Depth=1
	orq	$8192, 16(%rdi)         # imm = 0x2000
.LBB0_6:                                # %if.end
                                        #   in Loop: Header=BB0_1 Depth=1
	cmpq	$0, 8(%rcx)
	movq	16(%rdi), %rdx
	je	.LBB0_8
# BB#7:                                 # %if.then6
                                        #   in Loop: Header=BB0_1 Depth=1
	orq	$16384, %rdx            # imm = 0x4000
	movq	%rdx, 16(%rdi)
.LBB0_8:                                # %if.end9
                                        #   in Loop: Header=BB0_1 Depth=1
	testq	$24576, %rdx            # imm = 0x6000
	je	.LBB0_10
# BB#9:                                 # %lor.lhs.false
                                        #   in Loop: Header=BB0_1 Depth=1
	cmpq	$0, 24(%rcx)
	je	.LBB0_11
.LBB0_10:                               # %if.then14
                                        #   in Loop: Header=BB0_1 Depth=1
	orq	$32768, %rdx            # imm = 0x8000
	movq	%rdx, 16(%rdi)
	jmp	.LBB0_11
.LBB0_12:                               # %for.end
	retq
.Ltmp0:
	.size	Perl_mg_magical, .Ltmp0-Perl_mg_magical
	.cfi_endproc

	.globl	Perl_mg_get
	.align	16, 0x90
	.type	Perl_mg_get,@function
Perl_mg_get:                            # @Perl_mg_get
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp7:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp8:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp9:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp10:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp11:
	.cfi_def_cfa_offset 48
	subq	$32, %rsp
.Ltmp12:
	.cfi_def_cfa_offset 80
.Ltmp13:
	.cfi_offset %rbx, -48
.Ltmp14:
	.cfi_offset %r12, -40
.Ltmp15:
	.cfi_offset %r13, -32
.Ltmp16:
	.cfi_offset %r14, -24
.Ltmp17:
	.cfi_offset %r15, -16
	movq	%rdi, %rbx
	movq	$24, %rdi
	xorq	%rsi, %rsi
	callq	Perl_save_alloc
	movq	16(%rbx), %r14
	movq	%rax, 8(%rsp)           # 8-byte Spill
	testq	%rbx, %rbx
	movq	%rbx, PL_Sv(%rip)
	je	.LBB1_2
# BB#1:                                 # %land.rhs
	incq	8(%rbx)
.LBB1_2:                                # %land.end
	movq	%rbx, %rdi
	callq	Perl_sv_2mortal
	testq	$2048, %r14             # imm = 0x800
	jne	.LBB1_4
# BB#3:                                 # %if.then
	andq	$-2049, 16(%rbx)        # imm = 0xFFFFFFFFFFFFF7FF
.LBB1_4:                                # %if.end
	xorq	%rax, %rax
	xorq	%r12, %r12
	movq	8(%rsp), %rdi           # 8-byte Reload
	movq	%rbx, %rsi
	callq	save_magic
	movq	(%rbx), %r8
	movq	%rbx, %r15
	movq	40(%r8), %rdx
	movq	%rdx, 16(%rsp)          # 8-byte Spill
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	movabsq	$1, %r9
	movq	%rdx, %rbx
	movq	%rdx, %rcx
	movq	%rdx, 24(%rsp)          # 8-byte Spill
	jmp	.LBB1_5
	.align	16, 0x90
.LBB1_29:                               # %land.lhs.true36
                                        #   in Loop: Header=BB1_5 Depth=1
	movq	%r9, %r12
.LBB1_5:                                # %while.cond.outer
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB1_6 Depth 2
	movq	%rcx, %r14
	.align	16, 0x90
.LBB1_6:                                # %while.cond
                                        #   Parent Loop BB1_5 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	testq	%rbx, %rbx
	je	.LBB1_30
# BB#7:                                 # %while.body
                                        #   in Loop: Header=BB1_6 Depth=2
	movzbl	19(%rbx), %ecx
	testq	$4, %rcx
	jne	.LBB1_13
# BB#8:                                 # %while.body
                                        #   in Loop: Header=BB1_6 Depth=2
	movq	8(%rbx), %rcx
	testq	%rcx, %rcx
	je	.LBB1_13
# BB#9:                                 # %land.lhs.true12
                                        #   in Loop: Header=BB1_6 Depth=2
	movq	(%rcx), %rcx
	testq	%rcx, %rcx
	je	.LBB1_13
# BB#10:                                # %if.then14
                                        #   in Loop: Header=BB1_6 Depth=2
	movq	%r15, %rdi
	movq	%rbx, %rsi
	movq	%r9, %r13
	callq	*%rcx
	movq	%r13, %r9
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	movq	(%r15), %r8
	cmpq	$0, 40(%r8)
	je	.LBB1_30
# BB#11:                                # %if.end21
                                        #   in Loop: Header=BB1_6 Depth=2
	movzbl	19(%rbx), %ecx
	testq	$4, %rcx
	je	.LBB1_13
# BB#12:                                # %if.then26
                                        #   in Loop: Header=BB1_6 Depth=2
	movq	PL_savestack(%rip), %rcx
	movq	8(%rsp), %rdx           # 8-byte Reload
	movq	$0, 8(%rcx,%rdx)
	.align	16, 0x90
.LBB1_13:                               # %if.end28
                                        #   in Loop: Header=BB1_6 Depth=2
	movq	(%rbx), %rsi
	testq	%rax, %r12
	movabsq	$0, %rcx
	je	.LBB1_15
# BB#14:                                # %if.end28
                                        #   in Loop: Header=BB1_6 Depth=2
	movq	%r9, %rcx
.LBB1_15:                               # %if.end28
                                        #   in Loop: Header=BB1_6 Depth=2
	cmpq	%r14, %rsi
	movabsq	$0, %rdi
	jne	.LBB1_17
# BB#16:                                # %if.end28
                                        #   in Loop: Header=BB1_6 Depth=2
	movq	%r9, %rdi
.LBB1_17:                               # %if.end28
                                        #   in Loop: Header=BB1_6 Depth=2
	xorq	%rdx, %rdx
	andq	%rdi, %rcx
	movq	16(%rsp), %rbx          # 8-byte Reload
	jne	.LBB1_19
# BB#18:                                # %if.end28
                                        #   in Loop: Header=BB1_6 Depth=2
	movq	%rsi, %rbx
.LBB1_19:                               # %if.end28
                                        #   in Loop: Header=BB1_6 Depth=2
	jne	.LBB1_21
# BB#20:                                # %if.end28
                                        #   in Loop: Header=BB1_6 Depth=2
	movq	%r12, %rdx
.LBB1_21:                               # %if.end28
                                        #   in Loop: Header=BB1_6 Depth=2
	testq	%rcx, %rcx
	movq	24(%rsp), %rcx          # 8-byte Reload
	jne	.LBB1_23
# BB#22:                                # %if.end28
                                        #   in Loop: Header=BB1_6 Depth=2
	movq	%r14, %rcx
.LBB1_23:                               # %if.end28
                                        #   in Loop: Header=BB1_6 Depth=2
	testq	%rax, %rdx
	movq	%rcx, %r14
	movq	%rdx, %r12
	jne	.LBB1_6
# BB#24:                                # %land.lhs.true36
                                        #   in Loop: Header=BB1_5 Depth=1
	movq	40(%r8), %rdx
	movq	%rdx, 24(%rsp)          # 8-byte Spill
	cmpq	%rcx, %rdx
	je	.LBB1_26
# BB#25:                                # %land.lhs.true36
                                        #   in Loop: Header=BB1_5 Depth=1
	movq	%rbx, 16(%rsp)          # 8-byte Spill
.LBB1_26:                               # %land.lhs.true36
                                        #   in Loop: Header=BB1_5 Depth=1
	je	.LBB1_28
# BB#27:                                # %land.lhs.true36
                                        #   in Loop: Header=BB1_5 Depth=1
	movq	24(%rsp), %rbx          # 8-byte Reload
.LBB1_28:                               # %land.lhs.true36
                                        #   in Loop: Header=BB1_5 Depth=1
	movabsq	$0, %r12
	je	.LBB1_5
	jmp	.LBB1_29
.LBB1_30:                               # %while.end
	movq	PL_savestack(%rip), %rax
	movq	8(%rsp), %rcx           # 8-byte Reload
	movq	(%rax,%rcx), %rcx
	testq	%rcx, %rcx
	je	.LBB1_49
# BB#31:                                # %if.end.i
	movq	16(%rcx), %rdx
	movq	%rdx, %rsi
	andq	$255, %rsi
	cmpq	$7, %rsi
	jb	.LBB1_47
# BB#32:                                # %land.lhs.true.i
	movq	(%rcx), %rsi
	movq	40(%rsi), %rsi
	testq	%rsi, %rsi
	je	.LBB1_47
# BB#33:                                # %if.then2.i
	movq	8(%rsp), %rdi           # 8-byte Reload
	movq	8(%rax,%rdi), %rdi
	testq	%rdi, %rdi
	je	.LBB1_35
# BB#34:                                # %if.then4.i
	orq	%rdi, %rdx
	movq	%rdx, 16(%rcx)
	jmp	.LBB1_45
	.align	16, 0x90
.LBB1_35:                               # %for.body.i
                                        # =>This Inner Loop Header: Depth=1
	movq	8(%rsi), %rdi
	testq	%rdi, %rdi
	je	.LBB1_44
# BB#36:                                # %if.then.i
                                        #   in Loop: Header=BB1_35 Depth=1
	cmpq	$0, (%rdi)
	je	.LBB1_39
# BB#37:                                # %land.lhs.true.i89
                                        #   in Loop: Header=BB1_35 Depth=1
	movzbl	19(%rsi), %ebx
	testq	$4, %rbx
	jne	.LBB1_39
# BB#38:                                # %if.then4.i91
                                        #   in Loop: Header=BB1_35 Depth=1
	orq	$8192, %rdx             # imm = 0x2000
	movq	%rdx, 16(%rcx)
.LBB1_39:                               # %if.end.i93
                                        #   in Loop: Header=BB1_35 Depth=1
	cmpq	$0, 8(%rdi)
	je	.LBB1_41
# BB#40:                                # %if.then6.i
                                        #   in Loop: Header=BB1_35 Depth=1
	orq	$16384, %rdx            # imm = 0x4000
	movq	%rdx, 16(%rcx)
.LBB1_41:                               # %if.end9.i
                                        #   in Loop: Header=BB1_35 Depth=1
	testq	$24576, %rdx            # imm = 0x6000
	je	.LBB1_43
# BB#42:                                # %lor.lhs.false.i
                                        #   in Loop: Header=BB1_35 Depth=1
	cmpq	$0, 24(%rdi)
	je	.LBB1_44
.LBB1_43:                               # %if.then14.i
                                        #   in Loop: Header=BB1_35 Depth=1
	orq	$32768, %rdx            # imm = 0x8000
	movq	%rdx, 16(%rcx)
.LBB1_44:                               # %for.inc.i
                                        #   in Loop: Header=BB1_35 Depth=1
	movq	(%rsi), %rsi
	testq	%rsi, %rsi
	jne	.LBB1_35
.LBB1_45:                               # %if.end7.i
	testq	$8192, %rdx             # imm = 0x2000
	je	.LBB1_47
# BB#46:                                # %if.then11.i
	andq	$-458753, %rdx          # imm = 0xFFFFFFFFFFF8FFFF
	movq	%rdx, 16(%rcx)
.LBB1_47:                               # %if.end15.i
	movq	8(%rsp), %rdx           # 8-byte Reload
	movq	$0, (%rax,%rdx)
	movq	PL_savestack_ix(%rip), %rcx
	movq	16(%rax,%rdx), %rax
	cmpq	%rax, %rcx
	jne	.LBB1_49
# BB#48:                                # %if.then18.i
	movq	PL_savestack(%rip), %rax
	leaq	-5(%rcx), %rdx
	movq	%rdx, PL_savestack_ix(%rip)
	movq	-40(%rax,%rcx,8), %rax
	subq	%rax, %rdx
	movq	%rdx, PL_savestack_ix(%rip)
.LBB1_49:                               # %restore_magic.exit
	movq	%r15, %rdi
	movq	8(%rdi), %rax
	cmpq	$1, %rax
	jne	.LBB1_52
# BB#50:                                # %if.then46
	movq	16(%rdi), %rax
	movq	%rax, %rcx
	andq	$1223753727, %rcx       # imm = 0x48F0FFFF
	movq	%rcx, 16(%rdi)
	testq	$2097152, %rax          # imm = 0x200000
	je	.LBB1_52
# BB#51:                                # %land.rhs52
	callq	Perl_sv_backoff
.LBB1_52:                               # %if.end57
	xorq	%rax, %rax
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp18:
	.size	Perl_mg_get, .Ltmp18-Perl_mg_get
	.cfi_endproc

	.globl	Perl_mg_set
	.align	16, 0x90
	.type	Perl_mg_set,@function
Perl_mg_set:                            # @Perl_mg_set
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp23:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp24:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp25:
	.cfi_def_cfa_offset 32
.Ltmp26:
	.cfi_offset %rbx, -32
.Ltmp27:
	.cfi_offset %r14, -24
.Ltmp28:
	.cfi_offset %r15, -16
	movq	%rdi, %r15
	movq	$24, %rdi
	xorq	%rsi, %rsi
	callq	Perl_save_alloc
	movq	%rax, %r14
	xorq	%rax, %rax
	movq	%r14, %rdi
	movq	%r15, %rsi
	callq	save_magic
	movq	(%r15), %rax
	movq	40(%rax), %rbx
	jmp	.LBB2_1
.LBB2_6:                                # %if.then9
                                        #   in Loop: Header=BB2_1 Depth=1
	movq	%r15, %rdi
	callq	*%rcx
	.align	16, 0x90
.LBB2_1:                                # %entry
                                        # =>This Inner Loop Header: Depth=1
	testq	%rbx, %rbx
	je	.LBB2_7
# BB#2:                                 # %for.body
                                        #   in Loop: Header=BB2_1 Depth=1
	movq	%rbx, %rsi
	movq	(%rsi), %rbx
	movzbl	19(%rsi), %ecx
	movq	8(%rsi), %rax
	testq	$4, %rcx
	je	.LBB2_4
# BB#3:                                 # %if.then
                                        #   in Loop: Header=BB2_1 Depth=1
	andq	$251, %rcx
	movb	%cl, 19(%rsi)
	movq	PL_savestack(%rip), %rcx
	movq	$0, 8(%rcx,%r14)
.LBB2_4:                                # %if.end
                                        #   in Loop: Header=BB2_1 Depth=1
	testq	%rax, %rax
	je	.LBB2_1
# BB#5:                                 # %land.lhs.true
                                        #   in Loop: Header=BB2_1 Depth=1
	movq	8(%rax), %rcx
	testq	%rcx, %rcx
	je	.LBB2_1
	jmp	.LBB2_6
.LBB2_7:                                # %for.end
	movq	PL_savestack(%rip), %rax
	movq	(%rax,%r14), %rcx
	testq	%rcx, %rcx
	je	.LBB2_26
# BB#8:                                 # %if.end.i
	movq	16(%rcx), %rdx
	movq	%rdx, %rsi
	andq	$255, %rsi
	cmpq	$7, %rsi
	jb	.LBB2_24
# BB#9:                                 # %land.lhs.true.i
	movq	(%rcx), %rsi
	movq	40(%rsi), %rsi
	testq	%rsi, %rsi
	je	.LBB2_24
# BB#10:                                # %if.then2.i
	movq	8(%r14,%rax), %rdi
	testq	%rdi, %rdi
	je	.LBB2_12
# BB#11:                                # %if.then4.i
	orq	%rdi, %rdx
	movq	%rdx, 16(%rcx)
	jmp	.LBB2_22
	.align	16, 0x90
.LBB2_12:                               # %for.body.i
                                        # =>This Inner Loop Header: Depth=1
	movq	8(%rsi), %rdi
	testq	%rdi, %rdi
	je	.LBB2_21
# BB#13:                                # %if.then.i
                                        #   in Loop: Header=BB2_12 Depth=1
	cmpq	$0, (%rdi)
	je	.LBB2_16
# BB#14:                                # %land.lhs.true.i29
                                        #   in Loop: Header=BB2_12 Depth=1
	movzbl	19(%rsi), %ebx
	testq	$4, %rbx
	jne	.LBB2_16
# BB#15:                                # %if.then4.i31
                                        #   in Loop: Header=BB2_12 Depth=1
	orq	$8192, %rdx             # imm = 0x2000
	movq	%rdx, 16(%rcx)
.LBB2_16:                               # %if.end.i33
                                        #   in Loop: Header=BB2_12 Depth=1
	cmpq	$0, 8(%rdi)
	je	.LBB2_18
# BB#17:                                # %if.then6.i
                                        #   in Loop: Header=BB2_12 Depth=1
	orq	$16384, %rdx            # imm = 0x4000
	movq	%rdx, 16(%rcx)
.LBB2_18:                               # %if.end9.i
                                        #   in Loop: Header=BB2_12 Depth=1
	testq	$24576, %rdx            # imm = 0x6000
	je	.LBB2_20
# BB#19:                                # %lor.lhs.false.i
                                        #   in Loop: Header=BB2_12 Depth=1
	cmpq	$0, 24(%rdi)
	je	.LBB2_21
.LBB2_20:                               # %if.then14.i
                                        #   in Loop: Header=BB2_12 Depth=1
	orq	$32768, %rdx            # imm = 0x8000
	movq	%rdx, 16(%rcx)
.LBB2_21:                               # %for.inc.i
                                        #   in Loop: Header=BB2_12 Depth=1
	movq	(%rsi), %rsi
	testq	%rsi, %rsi
	jne	.LBB2_12
.LBB2_22:                               # %if.end7.i
	testq	$8192, %rdx             # imm = 0x2000
	je	.LBB2_24
# BB#23:                                # %if.then11.i
	andq	$-458753, %rdx          # imm = 0xFFFFFFFFFFF8FFFF
	movq	%rdx, 16(%rcx)
.LBB2_24:                               # %if.end15.i
	movq	$0, (%rax,%r14)
	movq	PL_savestack_ix(%rip), %rcx
	movq	16(%rax,%r14), %rax
	cmpq	%rax, %rcx
	jne	.LBB2_26
# BB#25:                                # %if.then18.i
	movq	PL_savestack(%rip), %rax
	leaq	-5(%rcx), %rdx
	movq	%rdx, PL_savestack_ix(%rip)
	movq	-40(%rax,%rcx,8), %rax
	subq	%rax, %rdx
	movq	%rdx, PL_savestack_ix(%rip)
.LBB2_26:                               # %restore_magic.exit
	xorq	%rax, %rax
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp29:
	.size	Perl_mg_set, .Ltmp29-Perl_mg_set
	.cfi_endproc

	.globl	Perl_mg_length
	.align	16, 0x90
	.type	Perl_mg_length,@function
Perl_mg_length:                         # @Perl_mg_length
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp35:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp36:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp37:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp38:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp39:
	.cfi_def_cfa_offset 48
.Ltmp40:
	.cfi_offset %rbx, -40
.Ltmp41:
	.cfi_offset %r12, -32
.Ltmp42:
	.cfi_offset %r14, -24
.Ltmp43:
	.cfi_offset %r15, -16
	movq	%rdi, %r15
	movq	(%r15), %rax
	movq	40(%rax), %rbx
	jmp	.LBB3_1
	.align	16, 0x90
.LBB3_23:                               # %for.inc
                                        #   in Loop: Header=BB3_1 Depth=1
	movq	(%rbx), %rbx
.LBB3_1:                                # %entry
                                        # =>This Inner Loop Header: Depth=1
	testq	%rbx, %rbx
	je	.LBB3_24
# BB#2:                                 # %for.body
                                        #   in Loop: Header=BB3_1 Depth=1
	movq	8(%rbx), %r12
	testq	%r12, %r12
	je	.LBB3_23
# BB#3:                                 # %land.lhs.true
                                        #   in Loop: Header=BB3_1 Depth=1
	cmpq	$0, 16(%r12)
	je	.LBB3_23
# BB#4:                                 # %if.then
	movq	$24, %rdi
	xorq	%rsi, %rsi
	callq	Perl_save_alloc
	movq	%rax, %r14
	xorq	%rax, %rax
	movq	%r14, %rdi
	movq	%r15, %rsi
	callq	save_magic
	movq	%r15, %rdi
	movq	%rbx, %rsi
	callq	*16(%r12)
	movq	PL_savestack(%rip), %rcx
	movq	%rax, (%rsp)
	movq	(%rcx,%r14), %rax
	testq	%rax, %rax
	je	.LBB3_34
# BB#5:                                 # %if.end.i
	movq	16(%rax), %rdx
	movq	%rdx, %rsi
	andq	$255, %rsi
	cmpq	$7, %rsi
	jb	.LBB3_21
# BB#6:                                 # %land.lhs.true.i
	movq	(%rax), %rsi
	movq	40(%rsi), %rsi
	testq	%rsi, %rsi
	je	.LBB3_21
# BB#7:                                 # %if.then2.i
	movq	8(%r14,%rcx), %rdi
	testq	%rdi, %rdi
	je	.LBB3_9
# BB#8:                                 # %if.then4.i
	orq	%rdi, %rdx
	movq	%rdx, 16(%rax)
	jmp	.LBB3_19
.LBB3_24:                               # %for.end
	movq	16(%r15), %rax
	testq	$536870912, %rax        # imm = 0x20000000
	je	.LBB3_27
# BB#25:                                # %land.lhs.true7
	movq	PL_curcop(%rip), %rcx
	movzbl	37(%rcx), %ecx
	testq	$8, %rcx
	je	.LBB3_29
# BB#26:                                # %land.lhs.true7.if.else_crit_edge
	movq	16(%r15), %rax
.LBB3_27:                               # %if.else
	testq	$262144, %rax           # imm = 0x40000
	je	.LBB3_33
# BB#28:                                # %cond.true22
	movq	(%r15), %rax
	movq	8(%rax), %rax
	movq	%rax, (%rsp)
	jmp	.LBB3_34
.LBB3_33:                               # %cond.false27
	leaq	(%rsp), %rsi
	movq	$2, %rdx
	movq	%r15, %rdi
	callq	Perl_sv_2pv_flags
	jmp	.LBB3_34
.LBB3_29:                               # %if.then10
	testq	$262144, %rax           # imm = 0x40000
	je	.LBB3_31
# BB#30:                                # %cond.true
	movq	(%r15), %rax
	movq	8(%rax), %rsi
	movq	%rsi, (%rsp)
	movq	(%rax), %rdi
	jmp	.LBB3_32
.LBB3_31:                               # %cond.false
	leaq	(%rsp), %rsi
	movq	$2, %rdx
	movq	%r15, %rdi
	callq	Perl_sv_2pv_flags
	movq	(%rsp), %rsi
	movq	%rax, %rdi
.LBB3_32:                               # %cond.end
	addq	%rdi, %rsi
	callq	Perl_utf8_length
	movq	%rax, (%rsp)
.LBB3_34:                               # %return
	movq	(%rsp), %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.LBB3_9:                                # %for.body.i
                                        # =>This Inner Loop Header: Depth=1
	movq	8(%rsi), %rdi
	testq	%rdi, %rdi
	je	.LBB3_18
# BB#10:                                # %if.then.i
                                        #   in Loop: Header=BB3_9 Depth=1
	cmpq	$0, (%rdi)
	je	.LBB3_13
# BB#11:                                # %land.lhs.true.i53
                                        #   in Loop: Header=BB3_9 Depth=1
	movzbl	19(%rsi), %ebx
	testq	$4, %rbx
	jne	.LBB3_13
# BB#12:                                # %if.then4.i55
                                        #   in Loop: Header=BB3_9 Depth=1
	orq	$8192, %rdx             # imm = 0x2000
	movq	%rdx, 16(%rax)
.LBB3_13:                               # %if.end.i57
                                        #   in Loop: Header=BB3_9 Depth=1
	cmpq	$0, 8(%rdi)
	je	.LBB3_15
# BB#14:                                # %if.then6.i
                                        #   in Loop: Header=BB3_9 Depth=1
	orq	$16384, %rdx            # imm = 0x4000
	movq	%rdx, 16(%rax)
.LBB3_15:                               # %if.end9.i
                                        #   in Loop: Header=BB3_9 Depth=1
	testq	$24576, %rdx            # imm = 0x6000
	je	.LBB3_17
# BB#16:                                # %lor.lhs.false.i
                                        #   in Loop: Header=BB3_9 Depth=1
	cmpq	$0, 24(%rdi)
	je	.LBB3_18
.LBB3_17:                               # %if.then14.i
                                        #   in Loop: Header=BB3_9 Depth=1
	orq	$32768, %rdx            # imm = 0x8000
	movq	%rdx, 16(%rax)
.LBB3_18:                               # %for.inc.i
                                        #   in Loop: Header=BB3_9 Depth=1
	movq	(%rsi), %rsi
	testq	%rsi, %rsi
	jne	.LBB3_9
.LBB3_19:                               # %if.end7.i
	testq	$8192, %rdx             # imm = 0x2000
	je	.LBB3_21
# BB#20:                                # %if.then11.i
	andq	$-458753, %rdx          # imm = 0xFFFFFFFFFFF8FFFF
	movq	%rdx, 16(%rax)
.LBB3_21:                               # %if.end15.i
	movq	$0, (%rcx,%r14)
	movq	PL_savestack_ix(%rip), %rax
	movq	16(%rcx,%r14), %rcx
	cmpq	%rcx, %rax
	jne	.LBB3_34
# BB#22:                                # %if.then18.i
	movq	PL_savestack(%rip), %rcx
	leaq	-5(%rax), %rdx
	movq	%rdx, PL_savestack_ix(%rip)
	movq	-40(%rcx,%rax,8), %rax
	subq	%rax, %rdx
	movq	%rdx, PL_savestack_ix(%rip)
	jmp	.LBB3_34
.Ltmp44:
	.size	Perl_mg_length, .Ltmp44-Perl_mg_length
	.cfi_endproc

	.globl	Perl_mg_size
	.align	16, 0x90
	.type	Perl_mg_size,@function
Perl_mg_size:                           # @Perl_mg_size
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp50:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp51:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp52:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp53:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp54:
	.cfi_def_cfa_offset 48
.Ltmp55:
	.cfi_offset %rbx, -40
.Ltmp56:
	.cfi_offset %r12, -32
.Ltmp57:
	.cfi_offset %r14, -24
.Ltmp58:
	.cfi_offset %r15, -16
	movq	%rdi, %r15
	movq	(%r15), %rax
	movq	40(%rax), %rbx
	jmp	.LBB4_1
	.align	16, 0x90
.LBB4_23:                               # %for.inc
                                        #   in Loop: Header=BB4_1 Depth=1
	movq	(%rbx), %rbx
.LBB4_1:                                # %entry
                                        # =>This Inner Loop Header: Depth=1
	testq	%rbx, %rbx
	je	.LBB4_24
# BB#2:                                 # %for.body
                                        #   in Loop: Header=BB4_1 Depth=1
	movq	8(%rbx), %r12
	testq	%r12, %r12
	je	.LBB4_23
# BB#3:                                 # %land.lhs.true
                                        #   in Loop: Header=BB4_1 Depth=1
	cmpq	$0, 16(%r12)
	je	.LBB4_23
# BB#4:                                 # %if.then
	movq	$24, %rdi
	xorq	%rsi, %rsi
	callq	Perl_save_alloc
	movq	%rax, %r14
	xorq	%rax, %rax
	movq	%r14, %rdi
	movq	%r15, %rsi
	callq	save_magic
	movq	%r15, %rdi
	movq	%rbx, %rsi
	callq	*16(%r12)
	movq	PL_savestack(%rip), %r8
	movq	(%r8,%r14), %rdx
	movq	%rax, %rbx
	testq	%rdx, %rdx
	je	.LBB4_27
# BB#5:                                 # %if.end.i
	movq	16(%rdx), %rax
	movq	%rax, %rsi
	andq	$255, %rsi
	cmpq	$7, %rsi
	jb	.LBB4_21
# BB#6:                                 # %land.lhs.true.i
	movq	(%rdx), %rsi
	movq	40(%rsi), %rsi
	testq	%rsi, %rsi
	je	.LBB4_21
# BB#7:                                 # %if.then2.i
	movq	8(%r14,%r8), %rdi
	testq	%rdi, %rdi
	je	.LBB4_9
# BB#8:                                 # %if.then4.i
	orq	%rdi, %rax
	movq	%rax, 16(%rdx)
	jmp	.LBB4_19
.LBB4_24:                               # %for.end
	movzbl	16(%r15), %ecx
	cmpq	$10, %rcx
	jne	.LBB4_26
# BB#25:                                # %sw.bb
	movq	8(%rax), %rbx
	jmp	.LBB4_27
.LBB4_26:                               # %sw.default
	movabsq	$.L.str, %rdi
	xorq	%rax, %rax
	xorq	%rbx, %rbx
	callq	Perl_croak
.LBB4_27:                               # %return
	movq	%rbx, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.LBB4_9:                                # %for.body.i
                                        # =>This Inner Loop Header: Depth=1
	movq	8(%rsi), %rdi
	testq	%rdi, %rdi
	je	.LBB4_18
# BB#10:                                # %if.then.i
                                        #   in Loop: Header=BB4_9 Depth=1
	cmpq	$0, (%rdi)
	je	.LBB4_13
# BB#11:                                # %land.lhs.true.i24
                                        #   in Loop: Header=BB4_9 Depth=1
	movzbl	19(%rsi), %ecx
	testq	$4, %rcx
	jne	.LBB4_13
# BB#12:                                # %if.then4.i26
                                        #   in Loop: Header=BB4_9 Depth=1
	orq	$8192, %rax             # imm = 0x2000
	movq	%rax, 16(%rdx)
.LBB4_13:                               # %if.end.i28
                                        #   in Loop: Header=BB4_9 Depth=1
	cmpq	$0, 8(%rdi)
	je	.LBB4_15
# BB#14:                                # %if.then6.i
                                        #   in Loop: Header=BB4_9 Depth=1
	orq	$16384, %rax            # imm = 0x4000
	movq	%rax, 16(%rdx)
.LBB4_15:                               # %if.end9.i
                                        #   in Loop: Header=BB4_9 Depth=1
	testq	$24576, %rax            # imm = 0x6000
	je	.LBB4_17
# BB#16:                                # %lor.lhs.false.i
                                        #   in Loop: Header=BB4_9 Depth=1
	cmpq	$0, 24(%rdi)
	je	.LBB4_18
.LBB4_17:                               # %if.then14.i
                                        #   in Loop: Header=BB4_9 Depth=1
	orq	$32768, %rax            # imm = 0x8000
	movq	%rax, 16(%rdx)
.LBB4_18:                               # %for.inc.i
                                        #   in Loop: Header=BB4_9 Depth=1
	movq	(%rsi), %rsi
	testq	%rsi, %rsi
	jne	.LBB4_9
.LBB4_19:                               # %if.end7.i
	testq	$8192, %rax             # imm = 0x2000
	je	.LBB4_21
# BB#20:                                # %if.then11.i
	andq	$-458753, %rax          # imm = 0xFFFFFFFFFFF8FFFF
	movq	%rax, 16(%rdx)
.LBB4_21:                               # %if.end15.i
	movq	$0, (%r8,%r14)
	movq	PL_savestack_ix(%rip), %rax
	movq	16(%r8,%r14), %rcx
	cmpq	%rcx, %rax
	jne	.LBB4_27
# BB#22:                                # %if.then18.i
	movq	PL_savestack(%rip), %rcx
	leaq	-5(%rax), %rdx
	movq	%rdx, PL_savestack_ix(%rip)
	movq	-40(%rcx,%rax,8), %rax
	subq	%rax, %rdx
	movq	%rdx, PL_savestack_ix(%rip)
	jmp	.LBB4_27
.Ltmp59:
	.size	Perl_mg_size, .Ltmp59-Perl_mg_size
	.cfi_endproc

	.globl	Perl_mg_clear
	.align	16, 0x90
	.type	Perl_mg_clear,@function
Perl_mg_clear:                          # @Perl_mg_clear
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp64:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp65:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp66:
	.cfi_def_cfa_offset 32
.Ltmp67:
	.cfi_offset %rbx, -32
.Ltmp68:
	.cfi_offset %r14, -24
.Ltmp69:
	.cfi_offset %r15, -16
	movq	%rdi, %r15
	movq	$24, %rdi
	xorq	%rsi, %rsi
	callq	Perl_save_alloc
	movq	%rax, %r14
	xorq	%rax, %rax
	movq	%r14, %rdi
	movq	%r15, %rsi
	callq	save_magic
	movq	(%r15), %rax
	movq	40(%rax), %rbx
	jmp	.LBB5_1
	.align	16, 0x90
.LBB5_5:                                # %for.inc
                                        #   in Loop: Header=BB5_1 Depth=1
	movq	(%rbx), %rbx
.LBB5_1:                                # %entry
                                        # =>This Inner Loop Header: Depth=1
	testq	%rbx, %rbx
	je	.LBB5_6
# BB#2:                                 # %for.body
                                        #   in Loop: Header=BB5_1 Depth=1
	movq	8(%rbx), %rax
	testq	%rax, %rax
	je	.LBB5_5
# BB#3:                                 # %land.lhs.true
                                        #   in Loop: Header=BB5_1 Depth=1
	movq	24(%rax), %rax
	testq	%rax, %rax
	je	.LBB5_5
# BB#4:                                 # %if.then
                                        #   in Loop: Header=BB5_1 Depth=1
	movq	%r15, %rdi
	movq	%rbx, %rsi
	callq	*%rax
	jmp	.LBB5_5
.LBB5_6:                                # %for.end
	movq	PL_savestack(%rip), %rax
	movq	(%rax,%r14), %rcx
	testq	%rcx, %rcx
	je	.LBB5_25
# BB#7:                                 # %if.end.i
	movq	16(%rcx), %rdx
	movq	%rdx, %rsi
	andq	$255, %rsi
	cmpq	$7, %rsi
	jb	.LBB5_23
# BB#8:                                 # %land.lhs.true.i
	movq	(%rcx), %rsi
	movq	40(%rsi), %rsi
	testq	%rsi, %rsi
	je	.LBB5_23
# BB#9:                                 # %if.then2.i
	movq	8(%r14,%rax), %rdi
	testq	%rdi, %rdi
	je	.LBB5_11
# BB#10:                                # %if.then4.i
	orq	%rdi, %rdx
	movq	%rdx, 16(%rcx)
	jmp	.LBB5_21
	.align	16, 0x90
.LBB5_11:                               # %for.body.i
                                        # =>This Inner Loop Header: Depth=1
	movq	8(%rsi), %rdi
	testq	%rdi, %rdi
	je	.LBB5_20
# BB#12:                                # %if.then.i
                                        #   in Loop: Header=BB5_11 Depth=1
	cmpq	$0, (%rdi)
	je	.LBB5_15
# BB#13:                                # %land.lhs.true.i19
                                        #   in Loop: Header=BB5_11 Depth=1
	movzbl	19(%rsi), %ebx
	testq	$4, %rbx
	jne	.LBB5_15
# BB#14:                                # %if.then4.i21
                                        #   in Loop: Header=BB5_11 Depth=1
	orq	$8192, %rdx             # imm = 0x2000
	movq	%rdx, 16(%rcx)
.LBB5_15:                               # %if.end.i23
                                        #   in Loop: Header=BB5_11 Depth=1
	cmpq	$0, 8(%rdi)
	je	.LBB5_17
# BB#16:                                # %if.then6.i
                                        #   in Loop: Header=BB5_11 Depth=1
	orq	$16384, %rdx            # imm = 0x4000
	movq	%rdx, 16(%rcx)
.LBB5_17:                               # %if.end9.i
                                        #   in Loop: Header=BB5_11 Depth=1
	testq	$24576, %rdx            # imm = 0x6000
	je	.LBB5_19
# BB#18:                                # %lor.lhs.false.i
                                        #   in Loop: Header=BB5_11 Depth=1
	cmpq	$0, 24(%rdi)
	je	.LBB5_20
.LBB5_19:                               # %if.then14.i
                                        #   in Loop: Header=BB5_11 Depth=1
	orq	$32768, %rdx            # imm = 0x8000
	movq	%rdx, 16(%rcx)
.LBB5_20:                               # %for.inc.i
                                        #   in Loop: Header=BB5_11 Depth=1
	movq	(%rsi), %rsi
	testq	%rsi, %rsi
	jne	.LBB5_11
.LBB5_21:                               # %if.end7.i
	testq	$8192, %rdx             # imm = 0x2000
	je	.LBB5_23
# BB#22:                                # %if.then11.i
	andq	$-458753, %rdx          # imm = 0xFFFFFFFFFFF8FFFF
	movq	%rdx, 16(%rcx)
.LBB5_23:                               # %if.end15.i
	movq	$0, (%rax,%r14)
	movq	PL_savestack_ix(%rip), %rcx
	movq	16(%rax,%r14), %rax
	cmpq	%rax, %rcx
	jne	.LBB5_25
# BB#24:                                # %if.then18.i
	movq	PL_savestack(%rip), %rax
	leaq	-5(%rcx), %rdx
	movq	%rdx, PL_savestack_ix(%rip)
	movq	-40(%rax,%rcx,8), %rax
	subq	%rax, %rdx
	movq	%rdx, PL_savestack_ix(%rip)
.LBB5_25:                               # %restore_magic.exit
	xorq	%rax, %rax
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp70:
	.size	Perl_mg_clear, .Ltmp70-Perl_mg_clear
	.cfi_endproc

	.globl	Perl_mg_find
	.align	16, 0x90
	.type	Perl_mg_find,@function
Perl_mg_find:                           # @Perl_mg_find
	.cfi_startproc
# BB#0:                                 # %entry
	xorq	%rax, %rax
	testq	%rdi, %rdi
	je	.LBB6_6
# BB#1:                                 # %if.end
	movq	(%rdi), %rcx
	movq	40(%rcx), %rcx
	testq	%rcx, %rcx
	je	.LBB6_6
# BB#2:
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	andq	%rdx, %rsi
	.align	16, 0x90
.LBB6_4:                                # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movsbq	18(%rcx), %rdi
	andq	%rdx, %rdi
	cmpq	%rsi, %rdi
	je	.LBB6_5
# BB#3:                                 # %for.cond
                                        #   in Loop: Header=BB6_4 Depth=1
	movq	(%rcx), %rcx
	testq	%rcx, %rcx
	jne	.LBB6_4
	jmp	.LBB6_6
.LBB6_5:
	movq	%rcx, %rax
.LBB6_6:                                # %return
	retq
.Ltmp71:
	.size	Perl_mg_find, .Ltmp71-Perl_mg_find
	.cfi_endproc

	.globl	Perl_mg_copy
	.align	16, 0x90
	.type	Perl_mg_copy,@function
Perl_mg_copy:                           # @Perl_mg_copy
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
	subq	$16, %rsp
.Ltmp83:
	.cfi_def_cfa_offset 64
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
	movq	%rdi, 8(%rsp)           # 8-byte Spill
	movq	(%rdi), %rax
	movq	40(%rax), %rbx
	movq	%rcx, %r15
	movq	%rdx, %r13
	movq	%rsi, %r14
	xorq	%r12, %r12
	testq	%rbx, %rbx
	je	.LBB7_17
# BB#1:                                 # %for.body.lr.ph
	xorq	%r12, %r12
	.align	16, 0x90
.LBB7_2:                                # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movzbl	19(%rbx), %eax
	testq	$8, %rax
	je	.LBB7_5
# BB#3:                                 # %land.lhs.true
                                        #   in Loop: Header=BB7_2 Depth=1
	movq	8(%rbx), %rax
	movq	40(%rax), %rax
	testq	%rax, %rax
	jne	.LBB7_4
.LBB7_5:                                # %if.else
                                        #   in Loop: Header=BB7_2 Depth=1
	movzbl	18(%rbx), %eax
	leaq	-65(%rax), %rcx
	andq	$255, %rcx
	cmpq	$25, %rcx
	ja	.LBB7_16
# BB#6:                                 # %if.then12
                                        #   in Loop: Header=BB7_2 Depth=1
	movq	%rax, %rcx
	andq	$255, %rcx
	cmpq	$68, %rcx
	jne	.LBB7_7
# BB#11:                                # %land.lhs.true27
                                        #   in Loop: Header=BB7_2 Depth=1
	xorq	%rcx, %rcx
	cmpq	$0, 24(%rbx)
	movq	$68, %rdi
	movq	8(%rsp), %rsi           # 8-byte Reload
	jne	.LBB7_13
	jmp	.LBB7_12
	.align	16, 0x90
.LBB7_4:                                # %if.then
                                        #   in Loop: Header=BB7_2 Depth=1
	movq	8(%rsp), %rdi           # 8-byte Reload
	movq	%rbx, %rsi
	movq	%r14, %rdx
	movq	%r13, %rcx
	movq	%r15, %r8
	callq	*%rax
	addq	%rax, %r12
	jmp	.LBB7_16
.LBB7_7:                                # %if.then12
                                        #   in Loop: Header=BB7_2 Depth=1
	cmpq	$80, %rcx
	jne	.LBB7_8
# BB#9:                                 # %cond.true
                                        #   in Loop: Header=BB7_2 Depth=1
	movq	24(%rbx), %rsi
	testq	%rsi, %rsi
	movq	$80, %rdi
	jne	.LBB7_13
# BB#10:                                # %cond.false
                                        #   in Loop: Header=BB7_2 Depth=1
	movq	8(%rsp), %rdi           # 8-byte Reload
	callq	Perl_newRV
	movq	%rax, %rdi
	callq	Perl_sv_2mortal
	movzbl	18(%rbx), %edi
	movq	%rax, %rsi
	jmp	.LBB7_13
.LBB7_8:                                # %if.then12.cond.false31_crit_edge
                                        #   in Loop: Header=BB7_2 Depth=1
	movq	24(%rbx), %rcx
.LBB7_12:                               # %cond.false31
                                        #   in Loop: Header=BB7_2 Depth=1
	movq	%rax, %rdi
	movq	%rcx, %rsi
.LBB7_13:                               # %cond.end35
                                        #   in Loop: Header=BB7_2 Depth=1
	movq	$56, %rcx
	movq	%rdi, %rdx
	shlq	%cl, %rdx
	movq	$56, %rcx
	sarq	%cl, %rdx
	addq	$-65, %rdi
	andq	$255, %rdi
	cmpq	$26, %rdi
	jae	.LBB7_15
# BB#14:                                #   in Loop: Header=BB7_2 Depth=1
	addq	$32, %rdx
.LBB7_15:                               # %cond.end35
                                        #   in Loop: Header=BB7_2 Depth=1
	movq	%r14, %rdi
	movq	%r13, %rcx
	movq	%r15, %r8
	callq	Perl_sv_magic
	incq	%r12
.LBB7_16:                               # %for.inc
                                        #   in Loop: Header=BB7_2 Depth=1
	movq	(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB7_2
.LBB7_17:                               # %for.end
	movq	%r12, %rax
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp89:
	.size	Perl_mg_copy, .Ltmp89-Perl_mg_copy
	.cfi_endproc

	.globl	Perl_mg_free
	.align	16, 0x90
	.type	Perl_mg_free,@function
Perl_mg_free:                           # @Perl_mg_free
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp94:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp95:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp96:
	.cfi_def_cfa_offset 32
.Ltmp97:
	.cfi_offset %rbx, -32
.Ltmp98:
	.cfi_offset %r14, -24
.Ltmp99:
	.cfi_offset %r15, -16
	movq	%rdi, %r14
	movq	(%r14), %rcx
	movq	40(%rcx), %rbx
	testq	%rbx, %rbx
	je	.LBB8_15
	.align	16, 0x90
.LBB8_1:                                # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	8(%rbx), %rax
	movq	(%rbx), %r15
	testq	%rax, %rax
	je	.LBB8_4
# BB#2:                                 # %land.lhs.true
                                        #   in Loop: Header=BB8_1 Depth=1
	movq	32(%rax), %rax
	testq	%rax, %rax
	je	.LBB8_4
# BB#3:                                 # %if.then
                                        #   in Loop: Header=BB8_1 Depth=1
	movq	%r14, %rdi
	movq	%rbx, %rsi
	callq	*%rax
.LBB8_4:                                # %if.end
                                        #   in Loop: Header=BB8_1 Depth=1
	movq	32(%rbx), %rdi
	testq	%rdi, %rdi
	je	.LBB8_11
# BB#5:                                 # %land.lhs.true5
                                        #   in Loop: Header=BB8_1 Depth=1
	movzbl	18(%rbx), %eax
	cmpq	$103, %rax
	je	.LBB8_11
# BB#6:                                 # %if.then7
                                        #   in Loop: Header=BB8_1 Depth=1
	movq	40(%rbx), %rcx
	testq	%rcx, %rcx
	jg	.LBB8_8
# BB#7:                                 # %if.then7
                                        #   in Loop: Header=BB8_1 Depth=1
	cmpq	$119, %rax
	je	.LBB8_8
# BB#9:                                 # %if.else
                                        #   in Loop: Header=BB8_1 Depth=1
	cmpq	$-2, %rcx
	jne	.LBB8_11
# BB#10:                                # %if.then19
                                        #   in Loop: Header=BB8_1 Depth=1
	callq	Perl_sv_free
	jmp	.LBB8_11
.LBB8_8:                                # %if.then14
                                        #   in Loop: Header=BB8_1 Depth=1
	callq	Perl_safesysfree
	.align	16, 0x90
.LBB8_11:                               # %if.end23
                                        #   in Loop: Header=BB8_1 Depth=1
	movzbl	19(%rbx), %eax
	testq	$2, %rax
	je	.LBB8_13
# BB#12:                                # %if.then26
                                        #   in Loop: Header=BB8_1 Depth=1
	movq	24(%rbx), %rdi
	callq	Perl_sv_free
.LBB8_13:                               # %if.end27
                                        #   in Loop: Header=BB8_1 Depth=1
	movq	%rbx, %rdi
	callq	Perl_safesysfree
	testq	%r15, %r15
	movq	%r15, %rbx
	jne	.LBB8_1
# BB#14:                                # %for.cond.for.end_crit_edge
	movq	(%r14), %rcx
.LBB8_15:                               # %for.end
	xorq	%rax, %rax
	movq	$0, 40(%rcx)
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp100:
	.size	Perl_mg_free, .Ltmp100-Perl_mg_free
	.cfi_endproc

	.globl	Perl_magic_regdata_cnt
	.align	16, 0x90
	.type	Perl_magic_regdata_cnt,@function
Perl_magic_regdata_cnt:                 # @Perl_magic_regdata_cnt
	.cfi_startproc
# BB#0:                                 # %entry
	movq	PL_curpm(%rip), %rcx
	testq	%rcx, %rcx
	movq	$-1, %rax
	je	.LBB9_5
# BB#1:                                 # %land.lhs.true
	movq	80(%rcx), %rcx
	testq	%rcx, %rcx
	je	.LBB9_5
# BB#2:                                 # %if.then
	cmpq	$0, 24(%rsi)
	je	.LBB9_4
# BB#3:                                 # %if.then3
	movq	96(%rcx), %rax
	retq
.LBB9_4:                                # %if.else
	movq	104(%rcx), %rax
.LBB9_5:                                # %return
	retq
.Ltmp101:
	.size	Perl_magic_regdata_cnt, .Ltmp101-Perl_magic_regdata_cnt
	.cfi_endproc

	.globl	Perl_magic_regdatum_get
	.align	16, 0x90
	.type	Perl_magic_regdatum_get,@function
Perl_magic_regdatum_get:                # @Perl_magic_regdatum_get
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp104:
	.cfi_def_cfa_offset 16
.Ltmp105:
	.cfi_offset %rbx, -16
	movq	PL_curpm(%rip), %rcx
	movq	%rsi, %rax
	movq	%rdi, %rbx
	testq	%rcx, %rcx
	je	.LBB10_13
# BB#1:                                 # %land.lhs.true
	movq	80(%rcx), %rcx
	testq	%rcx, %rcx
	je	.LBB10_13
# BB#2:                                 # %if.then
	movq	40(%rax), %rdx
	testq	%rdx, %rdx
	js	.LBB10_13
# BB#3:                                 # %if.end
	movq	96(%rcx), %rsi
	cmpq	%rsi, %rdx
	jg	.LBB10_13
# BB#4:                                 # %land.lhs.true4
	movq	(%rcx), %rsi
	movq	(%rsi,%rdx,8), %rsi
	cmpq	$-1, %rsi
	je	.LBB10_13
# BB#5:                                 # %land.lhs.true6
	movq	8(%rcx), %rdi
	movq	(%rdi,%rdx,8), %rdx
	cmpq	$-1, %rdx
	je	.LBB10_13
# BB#6:                                 # %if.then9
	cmpq	$0, 24(%rax)
	je	.LBB10_8
# BB#7:                                 # %select.mid
	movq	%rdx, %rsi
.LBB10_8:                               # %select.end
	testq	%rsi, %rsi
	jle	.LBB10_12
# BB#9:                                 # %land.lhs.true14
	movzbl	123(%rcx), %eax
	testq	$16, %rax
	je	.LBB10_12
# BB#10:                                # %if.then16
	movq	48(%rcx), %rdi
	testq	%rdi, %rdi
	je	.LBB10_12
# BB#11:                                # %if.then18
	addq	%rdi, %rsi
	callq	Perl_utf8_length
	movq	%rax, %rsi
.LBB10_12:                              # %if.end20
	movq	%rbx, %rdi
	callq	Perl_sv_setiv
.LBB10_13:                              # %return
	xorq	%rax, %rax
	popq	%rbx
	retq
.Ltmp106:
	.size	Perl_magic_regdatum_get, .Ltmp106-Perl_magic_regdatum_get
	.cfi_endproc

	.globl	Perl_magic_regdatum_set
	.align	16, 0x90
	.type	Perl_magic_regdatum_set,@function
Perl_magic_regdatum_set:                # @Perl_magic_regdatum_set
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rax
.Ltmp108:
	.cfi_def_cfa_offset 16
	movabsq	$PL_no_modify, %rdi
	xorq	%rax, %rax
	callq	Perl_croak
	xorq	%rax, %rax
	popq	%rdx
	retq
.Ltmp109:
	.size	Perl_magic_regdatum_set, .Ltmp109-Perl_magic_regdatum_set
	.cfi_endproc

	.globl	Perl_magic_len
	.align	16, 0x90
	.type	Perl_magic_len,@function
Perl_magic_len:                         # @Perl_magic_len
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp115:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp116:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp117:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp118:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp119:
	.cfi_def_cfa_offset 48
.Ltmp120:
	.cfi_offset %rbx, -40
.Ltmp121:
	.cfi_offset %r12, -32
.Ltmp122:
	.cfi_offset %r14, -24
.Ltmp123:
	.cfi_offset %r15, -16
	movq	32(%rsi), %rax
	movsbq	(%rax), %rcx
	movq	%rdi, %rbx
	cmpq	$95, %rcx
	jg	.LBB12_31
# BB#1:                                 # %entry
	cmpq	$37, %rcx
	jg	.LBB12_6
# BB#2:                                 # %entry
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %rcx
	cmpq	$14, %rcx
	jne	.LBB12_39
# BB#3:                                 # %sw.bb99
	movq	PL_curpm(%rip), %rax
	xorq	%r14, %r14
	testq	%rax, %rax
	je	.LBB12_44
# BB#4:                                 # %land.lhs.true101
	movq	80(%rax), %rbx
	testq	%rbx, %rbx
	je	.LBB12_44
# BB#5:                                 # %if.then104
	movq	112(%rbx), %rax
.LBB12_30:                              # %if.then94
	testq	%rax, %rax
	jne	.LBB12_11
	jmp	.LBB12_44
.LBB12_31:                              # %entry
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %rcx
	cmpq	$96, %rcx
	jne	.LBB12_39
# BB#32:                                # %sw.bb109
	movq	PL_curpm(%rip), %rax
	xorq	%r14, %r14
	testq	%rax, %rax
	je	.LBB12_44
# BB#33:                                # %land.lhs.true111
	movq	80(%rax), %rbx
	testq	%rbx, %rbx
	je	.LBB12_44
# BB#34:                                # %if.then114
	movq	(%rbx), %rax
	movq	(%rax), %r12
	xorq	%rcx, %rcx
	testq	%r12, %r12
	movq	%r12, %rax
	movabsq	$0, %r14
	jg	.LBB12_15
	jmp	.LBB12_44
.LBB12_6:                               # %entry
	addq	$-38, %rcx
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	andq	%rcx, %rdx
	cmpq	$19, %rdx
	ja	.LBB12_39
# BB#7:                                 # %entry
	jmpq	*.LJTI12_0(,%rdx,8)
.LBB12_8:                               # %sw.bb
	movq	PL_curpm(%rip), %rcx
	testq	%rcx, %rcx
	je	.LBB12_20
# BB#9:                                 # %land.lhs.true
	movq	80(%rcx), %rbx
	testq	%rbx, %rbx
	je	.LBB12_20
# BB#10:                                # %if.then
	movq	%rax, %rdi
	callq	atoi
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
.LBB12_11:                              # %getparen
	movq	96(%rbx), %rcx
	cmpq	%rcx, %rax
	jg	.LBB12_20
# BB#12:                                # %land.lhs.true5
	movq	(%rbx), %rcx
	movq	(%rcx,%rax,8), %rcx
	cmpq	$-1, %rcx
	je	.LBB12_20
# BB#13:                                # %land.lhs.true8
	movq	8(%rbx), %rdx
	movq	(%rdx,%rax,8), %r12
	cmpq	$-1, %r12
	je	.LBB12_20
# BB#14:                                # %getlen
	movq	%r12, %rax
	subq	%rcx, %rax
	testq	%rax, %rax
	movq	%rax, %r14
	jg	.LBB12_15
	jmp	.LBB12_18
.LBB12_39:                              # %sw.epilog
	xorq	%rax, %rax
	xorq	%r14, %r14
	movq	%rbx, %rdi
	callq	magic_get
	movq	16(%rbx), %rax
	testq	$262144, %rax           # imm = 0x40000
	jne	.LBB12_42
# BB#40:                                # %sw.epilog
	movq	%rax, %rcx
	andq	$196608, %rcx           # imm = 0x30000
	je	.LBB12_42
# BB#41:                                # %if.then158
	leaq	(%rsp), %rsi
	movq	$2, %rdx
	movq	%rbx, %rdi
	callq	Perl_sv_2pv_flags
	movq	16(%rbx), %rax
.LBB12_42:                              # %if.end160
	testq	$262144, %rax           # imm = 0x40000
	je	.LBB12_44
# BB#43:                                # %if.then164
	movq	(%rbx), %rax
	movq	8(%rax), %r14
.LBB12_44:                              # %return
	movq	%r14, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.LBB12_20:                              # %if.else
	movq	PL_curcop(%rip), %rax
	cmpq	$0, 88(%rax)
	je	.LBB12_24
# BB#21:                                # %land.lhs.true32
	movq	PL_curcop(%rip), %rax
	movq	88(%rax), %rax
	cmpq	$48, %rax
	je	.LBB12_24
# BB#22:                                # %land.lhs.true36
	movq	PL_curcop(%rip), %rax
	movq	88(%rax), %rax
	cmpq	$24, %rax
	je	.LBB12_26
# BB#23:                                # %lor.lhs.false
	movq	PL_curcop(%rip), %rax
	movq	88(%rax), %rax
	movq	(%rax), %rax
	movq	(%rax), %rax
	movzbl	10(%rax), %eax
	testq	$4, %rax
	jne	.LBB12_26
.LBB12_24:                              # %lor.lhs.false45
	movq	PL_curcop(%rip), %rax
	xorq	%r14, %r14
	cmpq	$0, 88(%rax)
	jne	.LBB12_44
# BB#25:                                # %land.lhs.true49
	movzbl	PL_dowarn(%rip), %eax
	testq	$1, %rax
	je	.LBB12_44
.LBB12_26:                              # %if.then53
	xorq	%rax, %rax
	xorq	%r14, %r14
	callq	report_uninit
	jmp	.LBB12_44
.LBB12_35:                              # %sw.bb128
	movq	PL_curpm(%rip), %rax
	xorq	%r14, %r14
	testq	%rax, %rax
	je	.LBB12_44
# BB#36:                                # %land.lhs.true130
	movq	80(%rax), %rbx
	testq	%rbx, %rbx
	je	.LBB12_44
# BB#37:                                # %if.then133
	movq	8(%rbx), %rax
	movq	(%rax), %rcx
	cmpq	$-1, %rcx
	je	.LBB12_44
# BB#38:                                # %if.then138
	movq	64(%rbx), %r12
	movq	%r12, %rax
	subq	%rcx, %rax
	testq	%rax, %rax
	jle	.LBB12_44
.LBB12_15:                              # %land.lhs.true15
	movzbl	123(%rbx), %edx
	testq	$16, %rdx
	movq	%rax, %r14
	je	.LBB12_18
# BB#16:                                # %if.then17
	movq	48(%rbx), %rbx
	leaq	(%rbx,%rcx), %r15
	movq	%r12, %r14
	subq	%rcx, %r14
	movq	%r15, %rdi
	movq	%r14, %rsi
	callq	Perl_is_utf8_string
	testq	$255, %rax
	je	.LBB12_18
# BB#17:                                # %if.then23
	addq	%r12, %rbx
	movq	%r15, %rdi
	movq	%rbx, %rsi
	callq	Perl_utf8_length
	movq	%rax, %r14
.LBB12_18:                              # %if.end25
	testq	%r14, %r14
	jns	.LBB12_44
# BB#19:                                # %if.then28
	movabsq	$.L.str1, %rdi
	xorq	%rax, %rax
	movq	%r14, %rsi
	callq	Perl_croak
	jmp	.LBB12_44
.LBB12_27:                              # %sw.bb89
	movq	PL_curpm(%rip), %rax
	xorq	%r14, %r14
	testq	%rax, %rax
	je	.LBB12_44
# BB#28:                                # %land.lhs.true91
	movq	80(%rax), %rbx
	testq	%rbx, %rbx
	je	.LBB12_44
# BB#29:                                # %if.then94
	movq	104(%rbx), %rax
	jmp	.LBB12_30
.Ltmp124:
	.size	Perl_magic_len, .Ltmp124-Perl_magic_len
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI12_0:
	.quad	.LBB12_8
	.quad	.LBB12_35
	.quad	.LBB12_39
	.quad	.LBB12_39
	.quad	.LBB12_39
	.quad	.LBB12_27
	.quad	.LBB12_39
	.quad	.LBB12_39
	.quad	.LBB12_39
	.quad	.LBB12_39
	.quad	.LBB12_39
	.quad	.LBB12_8
	.quad	.LBB12_8
	.quad	.LBB12_8
	.quad	.LBB12_8
	.quad	.LBB12_8
	.quad	.LBB12_8
	.quad	.LBB12_8
	.quad	.LBB12_8
	.quad	.LBB12_8

	.text
	.globl	Perl_magic_get
	.align	16, 0x90
	.type	Perl_magic_get,@function
Perl_magic_get:                         # @Perl_magic_get
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp130:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp131:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp132:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp133:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp134:
	.cfi_def_cfa_offset 48
.Ltmp135:
	.cfi_offset %rbx, -40
.Ltmp136:
	.cfi_offset %r12, -32
.Ltmp137:
	.cfi_offset %r14, -24
.Ltmp138:
	.cfi_offset %r15, -16
	movq	32(%rsi), %rbx
	movsbq	(%rbx), %rax
	movq	%rdi, %r14
	cmpq	$123, %rax
	jg	.LBB13_3
# BB#1:                                 # %entry
	decq	%rax
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	andq	%r15, %rax
	cmpq	$95, %rax
	ja	.LBB13_135
# BB#2:                                 # %entry
	jmpq	*.LJTI13_0(,%rax,8)
.LBB13_66:                              # %sw.bb157
	movq	PL_curpm(%rip), %rax
	testq	%rax, %rax
	je	.LBB13_28
# BB#67:                                # %land.lhs.true159
	movq	80(%rax), %r12
	testq	%r12, %r12
	je	.LBB13_28
# BB#68:                                # %if.then161
	movq	%rbx, %rdi
	callq	atoi
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
.LBB13_69:                              # %getparen
	movq	96(%r12), %rcx
	cmpq	%rcx, %rax
	jg	.LBB13_28
# BB#70:                                # %land.lhs.true167
	movq	(%r12), %rcx
	movq	(%rcx,%rax,8), %rcx
	cmpq	$-1, %rcx
	je	.LBB13_28
# BB#71:                                # %land.lhs.true170
	movq	8(%r12), %rdx
	movq	(%rdx,%rax,8), %rbx
	cmpq	$-1, %rbx
	je	.LBB13_28
# BB#72:                                # %if.then174
	movq	48(%r12), %r15
	testq	%r15, %r15
	je	.LBB13_135
# BB#73:
	subq	%rcx, %rbx
	addq	%rcx, %r15
	jmp	.LBB13_74
.LBB13_3:                               # %entry
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	cmpq	$124, %rax
	jne	.LBB13_4
# BB#120:                               # %sw.bb401
	movq	PL_defoutgv(%rip), %rax
	movq	(%rax), %rax
	movq	56(%rax), %rax
	movq	16(%rax), %rax
	testq	%rax, %rax
	je	.LBB13_135
# BB#121:                               # %if.then406
	movq	(%rax), %rax
	movzbl	163(%rax), %esi
	movq	$2, %rcx
	shrq	%cl, %rsi
	andq	$1, %rsi
.LBB13_46:                              # %cond.end96
	movq	%r14, %rdi
	callq	Perl_sv_setiv
	jmp	.LBB13_135
.LBB13_4:                               # %entry
	cmpq	$126, %rax
	jne	.LBB13_135
# BB#5:                                 # %sw.bb340
	movq	PL_defoutgv(%rip), %rax
	movq	(%rax), %rcx
	movq	56(%rcx), %rcx
	movq	16(%rcx), %rdx
	testq	%rdx, %rdx
	je	.LBB13_7
# BB#6:                                 # %if.end350
	movq	(%rdx), %rdx
	movq	128(%rdx), %rsi
	testq	%rsi, %rsi
	jne	.LBB13_22
.LBB13_7:                               # %if.then352
	movq	48(%rcx), %rcx
	testq	%rcx, %rcx
	je	.LBB13_9
# BB#8:                                 # %if.then352
	movq	%rcx, %rax
.LBB13_9:                               # %if.then352
	movq	(%rax), %rax
	movq	64(%rax), %rsi
.LBB13_22:                              # %if.then24
	movq	%r14, %rdi
	callq	Perl_sv_setpv
	jmp	.LBB13_135
.LBB13_29:                              # %sw.bb
	movq	PL_bodytarget(%rip), %rsi
	jmp	.LBB13_30
.LBB13_10:                              # %sw.bb1
	movsbq	PL_minus_c(%rip), %rsi
	jmp	.LBB13_46
.LBB13_11:                              # %sw.bb3
	movq	PL_debug(%rip), %rsi
	andq	$2093055, %rsi          # imm = 0x1FEFFF
	jmp	.LBB13_46
.LBB13_12:                              # %sw.bb4
	movzbl	1(%rbx), %eax
	testq	%rax, %rax
	je	.LBB13_13
# BB#17:                                # %if.else
	incq	%rbx
	movabsq	$.L.str3, %rsi
	movq	%rbx, %rdi
	callq	strcmp
	testq	%r15, %rax
	jne	.LBB13_135
# BB#18:                                # %if.then18
	movq	PL_encoding(%rip), %rsi
	jmp	.LBB13_30
.LBB13_19:                              # %sw.bb20
	movq	PL_maxsysfd(%rip), %rsi
	jmp	.LBB13_46
.LBB13_20:                              # %sw.bb21
	movq	PL_hints(%rip), %rsi
	jmp	.LBB13_46
.LBB13_21:                              # %sw.bb22
	movq	PL_inplace(%rip), %rsi
	testq	%rsi, %rsi
	jne	.LBB13_22
	jmp	.LBB13_28
.LBB13_90:                              # %sw.bb241
	movq	PL_curpm(%rip), %rax
	testq	%rax, %rax
	je	.LBB13_28
# BB#91:                                # %land.lhs.true243
	movq	80(%rax), %r12
	testq	%r12, %r12
	je	.LBB13_28
# BB#92:                                # %if.then246
	movq	112(%r12), %rax
	jmp	.LBB13_89
.LBB13_23:                              # %sw.bb27
	movzbl	1(%rbx), %eax
	testq	%rax, %rax
	je	.LBB13_24
# BB#26:                                # %if.else37
	movabsq	$.L.str4, %rsi
	movq	%rbx, %rdi
	callq	strcmp
	testq	%r15, %rax
	jne	.LBB13_135
# BB#27:                                # %if.then41
	movq	PL_compiling+96(%rip), %rsi
	testq	%rsi, %rsi
	jne	.LBB13_30
	jmp	.LBB13_28
.LBB13_31:                              # %sw.bb48
	movq	PL_perldb(%rip), %rsi
	jmp	.LBB13_46
.LBB13_32:                              # %sw.bb49
	movzbl	1(%rbx), %eax
	testq	%rax, %rax
	jne	.LBB13_135
# BB#33:                                # %if.then55
	movq	PL_lex_state(%rip), %rax
	cmpq	$11, %rax
	jne	.LBB13_34
# BB#36:                                # %if.else66
	movl	PL_in_eval(%rip), %eax
	testq	%rax, %rax
	je	.LBB13_38
# BB#37:                                # %if.then68
	movslq	PL_in_eval(%rip), %rsi
	andq	$-9, %rsi
	jmp	.LBB13_46
.LBB13_39:                              # %sw.bb75
	movzbl	1(%rbx), %eax
	testq	%rax, %rax
	je	.LBB13_40
# BB#41:                                # %if.else82
	movabsq	$.L.str5, %rsi
	movq	%rbx, %rdi
	callq	strcmp
	testq	%r15, %rax
	jne	.LBB13_135
# BB#42:                                # %if.then86
	movzbl	PL_tainting(%rip), %eax
	xorq	%rsi, %rsi
	testq	%rax, %rax
	je	.LBB13_46
# BB#43:                                # %cond.true89
	movzbl	PL_taint_warn(%rip), %eax
	testq	%rax, %rax
	movq	$-1, %rsi
	jne	.LBB13_46
# BB#44:                                # %lor.rhs
	movzbl	PL_unsafe(%rip), %eax
	testq	%rax, %rax
	movq	$-1, %rsi
	jne	.LBB13_46
# BB#45:                                # %select.mid
	movq	$1, %rsi
	jmp	.LBB13_46
.LBB13_47:                              # %sw.bb101
	movabsq	$.L.str6, %rsi
	movq	%rbx, %rdi
	callq	strcmp
	testq	%r15, %rax
	je	.LBB13_48
# BB#50:                                # %if.else106
	movabsq	$.L.str7, %rsi
	movq	%rbx, %rdi
	callq	strcmp
	testq	%r15, %rax
	jne	.LBB13_135
# BB#51:                                # %if.then110
	movsbq	PL_utf8locale(%rip), %rsi
	jmp	.LBB13_49
.LBB13_52:                              # %sw.bb114
	movzbl	1(%rbx), %eax
	testq	%rax, %rax
	je	.LBB13_53
# BB#55:                                # %if.else126
	incq	%rbx
	movabsq	$.L.str8, %rsi
	movq	%rbx, %rdi
	callq	strcmp
	testq	%r15, %rax
	jne	.LBB13_135
# BB#56:                                # %if.then131
	movq	PL_compiling+88(%rip), %rsi
	testq	%rsi, %rsi
	je	.LBB13_59
# BB#57:                                # %if.then131
	cmpq	$24, %rsi
	jne	.LBB13_58
# BB#61:                                # %if.then140
	movabsq	$.L.str10, %rdi
	xorq	%rsi, %rsi
	callq	Perl_get_hv
	testq	%rax, %rax
	je	.LBB13_136
# BB#62:                                # %land.lhs.true
	movabsq	$.L.str11, %rsi
	movq	$3, %rdx
	xorq	%rcx, %rcx
	movq	%rax, %rdi
	callq	Perl_hv_fetch
	testq	%rax, %rax
	je	.LBB13_136
# BB#63:                                # %if.then145
	movq	(%rax), %rsi
	jmp	.LBB13_64
.LBB13_125:                             # %sw.bb423
	callq	__errno_location
	movl	(%rax), %ebx
	callq	__errno_location
	movl	(%rax), %edi
	callq	int32_to_float64
	movq	%r14, %rdi
	movq	%rax, %rsi
	callq	Perl_sv_setnv
	callq	__errno_location
	movl	(%rax), %eax
	testq	%rax, %rax
	jne	.LBB13_15
# BB#126:
	movabsq	$.L.str2, %rsi
	jmp	.LBB13_16
.LBB13_124:                             # %sw.bb422
	movq	PL_ofmt(%rip), %rsi
	jmp	.LBB13_22
.LBB13_117:                             # %sw.bb389
	movq	PL_defoutgv(%rip), %rax
	movq	(%rax), %rax
	movq	56(%rax), %rax
	movq	16(%rax), %rax
	testq	%rax, %rax
	je	.LBB13_135
# BB#118:                               # %if.then394
	movq	(%rax), %rax
	movq	88(%rax), %rsi
	jmp	.LBB13_46
.LBB13_97:                              # %sw.bb269
	movq	PL_curpm(%rip), %rax
	testq	%rax, %rax
	je	.LBB13_28
# BB#98:                                # %land.lhs.true271
	movq	80(%rax), %r12
	testq	%r12, %r12
	je	.LBB13_28
# BB#99:                                # %if.then274
	movq	48(%r12), %r15
	testq	%r15, %r15
	je	.LBB13_28
# BB#100:                               # %land.lhs.true277
	movq	8(%r12), %rax
	movq	(%rax), %rax
	cmpq	$-1, %rax
	je	.LBB13_28
# BB#101:                               # %if.then282
	movq	64(%r12), %rbx
	addq	%rax, %r15
	subq	%rax, %rbx
	jmp	.LBB13_74
.LBB13_129:                             # %sw.bb443
	movl	PL_gid(%rip), %esi
	jmp	.LBB13_131
.LBB13_130:                             # %sw.bb445
	movl	PL_egid(%rip), %esi
.LBB13_131:                             # %add_groups
	movq	%r14, %rdi
	callq	Perl_sv_setiv
	movq	16(%r14), %rax
	testq	$2097152, %rax          # imm = 0x200000
	je	.LBB13_133
# BB#132:                               # %land.rhs450
	movq	%r14, %rdi
	callq	Perl_sv_backoff
	movq	16(%r14), %rax
.LBB13_133:                             # %land.end453
	orq	$16842752, %rax         # imm = 0x1010000
.LBB13_134:                             # %sw.epilog
	movq	%rax, 16(%r14)
	jmp	.LBB13_135
.LBB13_86:                              # %sw.bb231
	movq	PL_curpm(%rip), %rax
	testq	%rax, %rax
	je	.LBB13_28
# BB#87:                                # %land.lhs.true233
	movq	80(%rax), %r12
	testq	%r12, %r12
	je	.LBB13_28
# BB#88:                                # %if.then236
	movq	104(%r12), %rax
.LBB13_89:                              # %if.then236
	testq	%rax, %rax
	jne	.LBB13_69
	jmp	.LBB13_28
.LBB13_115:                             # %sw.bb378
	movq	PL_defoutgv(%rip), %rax
	movq	(%rax), %rax
	movq	56(%rax), %rax
	movq	16(%rax), %rax
	testq	%rax, %rax
	je	.LBB13_135
# BB#116:                               # %if.then383
	movq	(%rax), %rax
	movq	104(%rax), %rsi
	jmp	.LBB13_46
.LBB13_102:                             # %sw.bb292
	movq	PL_last_in_gv(%rip), %rax
	testq	%rax, %rax
	je	.LBB13_135
# BB#103:                               # %land.lhs.true294
	movzbl	16(%rax), %ecx
	cmpq	$13, %rcx
	jne	.LBB13_135
# BB#104:                               # %land.lhs.true299
	movq	(%rax), %rax
	movq	56(%rax), %rax
	testq	%rax, %rax
	je	.LBB13_135
# BB#105:                               # %cond.true302
	movq	16(%rax), %rax
	testq	%rax, %rax
	je	.LBB13_135
# BB#106:                               # %if.then307
	movq	(%rax), %rax
	movq	80(%rax), %rsi
	jmp	.LBB13_46
.LBB13_127:                             # %sw.bb439
	movl	PL_uid(%rip), %esi
	jmp	.LBB13_46
.LBB13_113:                             # %sw.bb367
	movq	PL_defoutgv(%rip), %rax
	movq	(%rax), %rax
	movq	56(%rax), %rax
	movq	16(%rax), %rax
	testq	%rax, %rax
	je	.LBB13_135
# BB#114:                               # %if.then372
	movq	(%rax), %rax
	movq	96(%rax), %rsi
	jmp	.LBB13_46
.LBB13_128:                             # %sw.bb441
	movl	PL_euid(%rip), %esi
	jmp	.LBB13_46
.LBB13_107:                             # %sw.bb313
	movq	PL_statusvalue(%rip), %rsi
	jmp	.LBB13_46
.LBB13_119:                             # %sw.bb400
	movq	PL_curcop(%rip), %rax
	movq	72(%rax), %rsi
	jmp	.LBB13_46
.LBB13_122:                             # %sw.bb418
	movq	PL_ors_sv(%rip), %rsi
	testq	%rsi, %rsi
	je	.LBB13_135
# BB#123:                               # %if.then420
	movq	%r14, %rdi
	callq	Perl_sv_copypv
	jmp	.LBB13_135
.LBB13_108:                             # %sw.bb314
	movq	PL_defoutgv(%rip), %rax
	movq	(%rax), %rcx
	movq	56(%rcx), %rcx
	movq	16(%rcx), %rdx
	testq	%rdx, %rdx
	je	.LBB13_110
# BB#109:                               # %if.end324
	movq	(%rdx), %rdx
	movq	112(%rdx), %rsi
	testq	%rsi, %rsi
	jne	.LBB13_22
.LBB13_110:                             # %if.else327
	movq	48(%rcx), %rcx
	testq	%rcx, %rcx
	je	.LBB13_112
# BB#111:                               # %if.else327
	movq	%rcx, %rax
.LBB13_112:                             # %if.else327
	movq	(%rax), %rax
	movq	64(%rax), %rsi
	movq	%r14, %rdi
	callq	Perl_sv_setpv
	movabsq	$.L.str13, %rsi
	movq	%r14, %rdi
	callq	Perl_sv_catpv
	jmp	.LBB13_135
.LBB13_93:                              # %sw.bb251
	movq	PL_curpm(%rip), %rax
	testq	%rax, %rax
	je	.LBB13_28
# BB#94:                                # %land.lhs.true253
	movq	80(%rax), %r12
	testq	%r12, %r12
	je	.LBB13_28
# BB#95:                                # %if.then256
	movq	48(%r12), %r15
	testq	%r15, %r15
	je	.LBB13_28
# BB#96:                                # %land.lhs.true259
	movq	(%r12), %rax
	movq	(%rax), %rbx
	cmpq	$-1, %rbx
	je	.LBB13_28
.LBB13_74:                              # %getrx
	testq	%rbx, %rbx
	js	.LBB13_28
# BB#75:                                # %if.then182
	movq	%r14, %rdi
	movq	%r15, %rsi
	movq	%rbx, %rdx
	callq	Perl_sv_setpvn
	movzbl	123(%r12), %eax
	testq	$16, %rax
	je	.LBB13_78
# BB#76:                                # %land.lhs.true185
	movq	%r15, %rdi
	movq	%rbx, %rsi
	callq	Perl_is_utf8_string
	testq	$255, %rax
	je	.LBB13_78
# BB#77:                                # %if.then189
	orq	$536870912, 16(%r14)    # imm = 0x20000000
	jmp	.LBB13_79
.LBB13_28:                              # %if.else25
	movabsq	$PL_sv_undef, %rsi
.LBB13_30:                              # %if.else44
	movq	$2, %rdx
	movq	%r14, %rdi
	callq	Perl_sv_setsv_flags
.LBB13_135:                             # %sw.epilog
	xorq	%rax, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.LBB13_13:                              # %if.then
	callq	__errno_location
	movl	(%rax), %ebx
	callq	__errno_location
	movl	(%rax), %edi
	callq	int32_to_float64
	movq	%r14, %rdi
	movq	%rax, %rsi
	callq	Perl_sv_setnv
	callq	__errno_location
	movl	(%rax), %eax
	testq	%rax, %rax
	je	.LBB13_14
.LBB13_15:                              # %cond.true
	callq	__errno_location
	movl	(%rax), %edi
	callq	strerror
	movq	%rax, %rsi
.LBB13_16:                              # %cond.end
	movq	%r14, %rdi
	callq	Perl_sv_setpv
	callq	__errno_location
	movl	%ebx, (%rax)
	orq	$33685504, 16(%r14)     # imm = 0x2020000
	jmp	.LBB13_135
.LBB13_24:                              # %if.then33
	movq	PL_osname(%rip), %rsi
	movq	%r14, %rdi
	callq	Perl_sv_setpv
	movzbl	PL_tainting(%rip), %eax
	testq	%rax, %rax
	jne	.LBB13_25
	jmp	.LBB13_135
.LBB13_40:                              # %if.then81
	movq	PL_basetime(%rip), %rsi
	jmp	.LBB13_46
.LBB13_48:                              # %if.then105
	movq	PL_unicode(%rip), %rsi
.LBB13_49:                              # %sw.epilog
	movq	%r14, %rdi
	callq	Perl_sv_setuv
	jmp	.LBB13_135
.LBB13_53:                              # %if.then120
	movzbl	PL_dowarn(%rip), %esi
	andq	$1, %rsi
	jmp	.LBB13_46
.LBB13_78:                              # %if.else192
	andq	$-536870913, 16(%r14)   # imm = 0xFFFFFFFFDFFFFFFF
.LBB13_79:                              # %if.end195
	movzbl	PL_tainting(%rip), %eax
	testq	%rax, %rax
	je	.LBB13_135
# BB#80:                                # %if.then197
	movzbl	122(%r12), %eax
	testq	$8, %rax
	je	.LBB13_25
# BB#81:                                # %if.then201
	movq	(%r14), %rax
	movq	40(%rax), %rbx
	movq	$1, %rax
	movb	%al, PL_tainted(%rip)
	movq	(%rbx), %rcx
	movq	(%r14), %rax
	movq	%rcx, 40(%rax)
	movzbl	PL_tainting(%rip), %edx
	testq	%rdx, %rdx
	je	.LBB13_84
# BB#82:                                # %if.then201
	movzbl	PL_tainted(%rip), %edx
	testq	$255, %rdx
	je	.LBB13_84
# BB#83:                                # %if.then210
	movq	%r14, %rdi
	callq	Perl_sv_taint
	movq	(%r14), %rax
	movq	40(%rax), %rcx
.LBB13_84:                              # %if.end213
	testq	%rcx, %rcx
	je	.LBB13_135
# BB#85:                                # %if.then217
	movq	%rcx, (%rbx)
	movq	%rbx, 40(%rax)
	jmp	.LBB13_135
.LBB13_25:                              # %if.then35
	movq	%r14, %rdi
	callq	Perl_sv_untaint
	jmp	.LBB13_135
.LBB13_34:                              # %if.then58
	movq	16(%r14), %rax
	movq	%rax, %rcx
	andq	$1223753727, %rcx       # imm = 0x48F0FFFF
	movq	%rcx, 16(%r14)
	testq	$2097152, %rax          # imm = 0x200000
	je	.LBB13_135
# BB#35:                                # %land.rhs
	movq	%r14, %rdi
	callq	Perl_sv_backoff
	jmp	.LBB13_135
.LBB13_14:
	movabsq	$.L.str2, %rsi
	jmp	.LBB13_16
.LBB13_58:                              # %if.then131
	cmpq	$48, %rsi
	jne	.LBB13_64
.LBB13_59:                              # %if.then136
	movabsq	$.L.str9, %rsi
	jmp	.LBB13_60
.LBB13_38:                              # %if.else71
	xorq	%rsi, %rsi
	jmp	.LBB13_46
.LBB13_64:                              # %if.else148
	movq	$2, %rdx
	movq	%r14, %rdi
	callq	Perl_sv_setsv_flags
	jmp	.LBB13_65
.LBB13_136:                             # %if.else146
	movabsq	$.L.str12, %rsi
.LBB13_60:                              # %if.end150
	movq	$12, %rdx
	movq	%r14, %rdi
	callq	Perl_sv_setpvn
.LBB13_65:                              # %if.end150
	movq	16(%r14), %rax
	andq	$1223753727, %rax       # imm = 0x48F0FFFF
	orq	$67371008, %rax         # imm = 0x4040000
	jmp	.LBB13_134
.Ltmp139:
	.size	Perl_magic_get, .Ltmp139-Perl_magic_get
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI13_0:
	.quad	.LBB13_29
	.quad	.LBB13_135
	.quad	.LBB13_10
	.quad	.LBB13_11
	.quad	.LBB13_12
	.quad	.LBB13_19
	.quad	.LBB13_135
	.quad	.LBB13_20
	.quad	.LBB13_21
	.quad	.LBB13_135
	.quad	.LBB13_135
	.quad	.LBB13_135
	.quad	.LBB13_135
	.quad	.LBB13_90
	.quad	.LBB13_23
	.quad	.LBB13_31
	.quad	.LBB13_135
	.quad	.LBB13_135
	.quad	.LBB13_32
	.quad	.LBB13_39
	.quad	.LBB13_47
	.quad	.LBB13_135
	.quad	.LBB13_52
	.quad	.LBB13_135
	.quad	.LBB13_135
	.quad	.LBB13_135
	.quad	.LBB13_135
	.quad	.LBB13_135
	.quad	.LBB13_135
	.quad	.LBB13_135
	.quad	.LBB13_135
	.quad	.LBB13_135
	.quad	.LBB13_125
	.quad	.LBB13_135
	.quad	.LBB13_124
	.quad	.LBB13_135
	.quad	.LBB13_117
	.quad	.LBB13_66
	.quad	.LBB13_97
	.quad	.LBB13_129
	.quad	.LBB13_130
	.quad	.LBB13_135
	.quad	.LBB13_86
	.quad	.LBB13_135
	.quad	.LBB13_115
	.quad	.LBB13_102
	.quad	.LBB13_135
	.quad	.LBB13_135
	.quad	.LBB13_66
	.quad	.LBB13_66
	.quad	.LBB13_66
	.quad	.LBB13_66
	.quad	.LBB13_66
	.quad	.LBB13_66
	.quad	.LBB13_66
	.quad	.LBB13_66
	.quad	.LBB13_66
	.quad	.LBB13_135
	.quad	.LBB13_135
	.quad	.LBB13_127
	.quad	.LBB13_113
	.quad	.LBB13_128
	.quad	.LBB13_107
	.quad	.LBB13_135
	.quad	.LBB13_135
	.quad	.LBB13_135
	.quad	.LBB13_135
	.quad	.LBB13_135
	.quad	.LBB13_135
	.quad	.LBB13_135
	.quad	.LBB13_135
	.quad	.LBB13_135
	.quad	.LBB13_135
	.quad	.LBB13_135
	.quad	.LBB13_135
	.quad	.LBB13_135
	.quad	.LBB13_135
	.quad	.LBB13_135
	.quad	.LBB13_135
	.quad	.LBB13_135
	.quad	.LBB13_135
	.quad	.LBB13_135
	.quad	.LBB13_135
	.quad	.LBB13_135
	.quad	.LBB13_135
	.quad	.LBB13_135
	.quad	.LBB13_135
	.quad	.LBB13_135
	.quad	.LBB13_135
	.quad	.LBB13_135
	.quad	.LBB13_119
	.quad	.LBB13_122
	.quad	.LBB13_135
	.quad	.LBB13_108
	.quad	.LBB13_135
	.quad	.LBB13_93

	.text
	.globl	Perl_magic_getuvar
	.align	16, 0x90
	.type	Perl_magic_getuvar,@function
Perl_magic_getuvar:                     # @Perl_magic_getuvar
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rax
.Ltmp141:
	.cfi_def_cfa_offset 16
	movq	32(%rsi), %rcx
	movq	%rdi, %rax
	testq	%rcx, %rcx
	je	.LBB14_3
# BB#1:                                 # %land.lhs.true
	movq	(%rcx), %rdx
	testq	%rdx, %rdx
	je	.LBB14_3
# BB#2:                                 # %if.then
	movq	16(%rcx), %rdi
	movq	%rax, %rsi
	callq	*%rdx
.LBB14_3:                               # %if.end
	xorq	%rax, %rax
	popq	%rdx
	retq
.Ltmp142:
	.size	Perl_magic_getuvar, .Ltmp142-Perl_magic_getuvar
	.cfi_endproc

	.globl	Perl_magic_setenv
	.align	16, 0x90
	.type	Perl_magic_setenv,@function
Perl_magic_setenv:                      # @Perl_magic_setenv
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp149:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp150:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp151:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp152:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp153:
	.cfi_def_cfa_offset 48
	subq	$432, %rsp              # imm = 0x1B0
.Ltmp154:
	.cfi_def_cfa_offset 480
.Ltmp155:
	.cfi_offset %rbx, -48
.Ltmp156:
	.cfi_offset %r12, -40
.Ltmp157:
	.cfi_offset %r13, -32
.Ltmp158:
	.cfi_offset %r14, -24
.Ltmp159:
	.cfi_offset %r15, -16
	movzbl	18(%rdi), %eax
	movq	%rsi, %r14
	testq	$4, %rax
	je	.LBB15_2
# BB#1:                                 # %cond.true
	movq	(%rdi), %rax
	movq	8(%rax), %rcx
	movq	%rcx, 424(%rsp)
	movq	(%rax), %rbx
	jmp	.LBB15_3
.LBB15_2:                               # %cond.false
	leaq	424(%rsp), %rsi
	movq	$2, %rdx
	callq	Perl_sv_2pv_flags
	movq	%rax, %rbx
.LBB15_3:                               # %cond.end
	movq	40(%r14), %rax
	movq	%rax, 416(%rsp)
	movabsq	$4294967295, %r12       # imm = 0xFFFFFFFF
	movq	32(%r14), %r15
	andq	%r12, %rax
	movabsq	$4294967294, %rcx       # imm = 0xFFFFFFFE
	cmpq	%rcx, %rax
	jne	.LBB15_7
# BB#4:                                 # %cond.true4
	movzbl	18(%r15), %eax
	testq	$4, %rax
	je	.LBB15_6
# BB#5:                                 # %cond.true9
	movq	(%r15), %rax
	movq	8(%rax), %rcx
	movq	%rcx, 416(%rsp)
	movq	(%rax), %r15
	jmp	.LBB15_7
.LBB15_6:                               # %cond.false16
	leaq	416(%rsp), %rsi
	movq	$2, %rdx
	movq	%r15, %rdi
	callq	Perl_sv_2pv_flags
	movq	%rax, %r15
.LBB15_7:                               # %cond.end23
	movq	%r15, %rdi
	movq	%rbx, %rsi
	callq	Perl_my_setenv
	movzbl	PL_tainting(%rip), %eax
	testq	%rax, %rax
	je	.LBB15_19
# BB#8:                                 # %if.then
	movzbl	19(%r14), %eax
	andq	$254, %rax
	movb	%al, 19(%r14)
	testq	%rbx, %rbx
	je	.LBB15_19
# BB#9:                                 # %if.then
	movq	416(%rsp), %rax
	cmpq	$4, %rax
	jne	.LBB15_19
# BB#10:                                # %land.lhs.true31
	movabsq	$.L.str14, %rsi
	movq	%r15, %rdi
	callq	strcmp
	testq	%r12, %rax
	jne	.LBB15_19
# BB#11:                                # %if.then34
	movq	424(%rsp), %r15
	testq	%r15, %r15
	jle	.LBB15_19
# BB#12:                                # %while.body.lr.ph
	addq	%rbx, %r15
	leaq	160(%rsp), %r13
	.align	16, 0x90
.LBB15_13:                              # %while.body
                                        # =>This Inner Loop Header: Depth=1
	movq	$58, %r8
	movq	%r13, %rdi
	leaq	416(%rsp), %rsi
	movq	%rbx, %rdx
	movq	%r15, %rcx
	leaq	8(%rsp), %r9
	callq	Perl_delimcpy
	movq	8(%rsp), %rcx
	movq	%rax, %rbx
	cmpq	$255, %rcx
	ja	.LBB15_18
# BB#14:                                # %while.body
                                        #   in Loop: Header=BB15_13 Depth=1
	movzbl	160(%rsp), %eax
	andq	$255, %rax
	cmpq	$47, %rax
	jne	.LBB15_18
# BB#15:                                # %lor.lhs.false46
                                        #   in Loop: Header=BB15_13 Depth=1
	movq	%r13, %rdi
	leaq	16(%rsp), %rsi
	callq	stat
	testq	%r12, %rax
	jne	.LBB15_16
# BB#17:                                # %land.lhs.true51
                                        #   in Loop: Header=BB15_13 Depth=1
	movzbl	40(%rsp), %eax
	testq	$2, %rax
	jne	.LBB15_18
.LBB15_16:                              # %while.cond.backedge
                                        #   in Loop: Header=BB15_13 Depth=1
	incq	%rbx
	cmpq	%r15, %rbx
	jb	.LBB15_13
	jmp	.LBB15_19
.LBB15_18:                              # %if.then54
	movzbl	19(%r14), %eax
	orq	$1, %rax
	movb	%al, 19(%r14)
.LBB15_19:                              # %return
	xorq	%rax, %rax
	addq	$432, %rsp              # imm = 0x1B0
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp160:
	.size	Perl_magic_setenv, .Ltmp160-Perl_magic_setenv
	.cfi_endproc

	.globl	Perl_magic_clearenv
	.align	16, 0x90
	.type	Perl_magic_clearenv,@function
Perl_magic_clearenv:                    # @Perl_magic_clearenv
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rax
.Ltmp162:
	.cfi_def_cfa_offset 16
	movq	40(%rsi), %rax
	movq	%rax, (%rsp)
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	movq	32(%rsi), %rdi
	andq	%rax, %rcx
	movabsq	$4294967294, %rax       # imm = 0xFFFFFFFE
	cmpq	%rax, %rcx
	jne	.LBB16_4
# BB#1:                                 # %cond.true
	movzbl	18(%rdi), %eax
	testq	$4, %rax
	je	.LBB16_3
# BB#2:                                 # %cond.true4
	movq	(%rdi), %rax
	movq	8(%rax), %rcx
	movq	%rcx, (%rsp)
	movq	(%rax), %rdi
	jmp	.LBB16_4
.LBB16_3:                               # %cond.false
	leaq	(%rsp), %rsi
	movq	$2, %rdx
	callq	Perl_sv_2pv_flags
	movq	%rax, %rdi
.LBB16_4:                               # %cond.end11
	xorq	%rsi, %rsi
	callq	Perl_my_setenv
	xorq	%rax, %rax
	popq	%rdx
	retq
.Ltmp163:
	.size	Perl_magic_clearenv, .Ltmp163-Perl_magic_clearenv
	.cfi_endproc

	.globl	Perl_magic_set_all_env
	.align	16, 0x90
	.type	Perl_magic_set_all_env,@function
Perl_magic_set_all_env:                 # @Perl_magic_set_all_env
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp170:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp171:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp172:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp173:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp174:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp175:
	.cfi_def_cfa_offset 64
.Ltmp176:
	.cfi_offset %rbx, -48
.Ltmp177:
	.cfi_offset %r12, -40
.Ltmp178:
	.cfi_offset %r13, -32
.Ltmp179:
	.cfi_offset %r14, -24
.Ltmp180:
	.cfi_offset %r15, -16
	movl	PL_localizing(%rip), %eax
	movq	%rdi, %r12
	testq	%rax, %rax
	je	.LBB17_7
# BB#1:                                 # %if.then
	xorq	%rax, %rax
	movq	%r12, %rdi
	callq	magic_clear_all_env
	movq	%r12, %rdi
	callq	Perl_hv_iterinit
	movq	%r12, %rdi
	callq	Perl_hv_iternext
	movq	%rax, %rbx
	testq	%rbx, %rbx
	je	.LBB17_7
# BB#2:
	leaq	(%rsp), %r14
	.align	16, 0x90
.LBB17_3:                               # %while.body
                                        # =>This Inner Loop Header: Depth=1
	movq	%rbx, %rdi
	movq	%r14, %rsi
	callq	Perl_hv_iterkey
	movq	%rax, %r13
	movq	%r12, %rdi
	movq	%rbx, %rsi
	callq	Perl_hv_iterval
	movq	16(%rax), %r15
	movq	%r12, %rdi
	movq	%rbx, %rsi
	callq	Perl_hv_iterval
	testq	$262144, %r15           # imm = 0x40000
	jne	.LBB17_4
# BB#5:                                 # %cond.false
                                        #   in Loop: Header=BB17_3 Depth=1
	movq	$2, %rdx
	movq	%rax, %rdi
	leaq	8(%rsp), %rsi
	callq	Perl_sv_2pv_flags
	movq	%rax, %rsi
	jmp	.LBB17_6
	.align	16, 0x90
.LBB17_4:                               # %cond.true
                                        #   in Loop: Header=BB17_3 Depth=1
	movq	(%rax), %rax
	movq	8(%rax), %rax
	movq	%rax, 8(%rsp)
	movq	%r12, %rdi
	movq	%rbx, %rsi
	callq	Perl_hv_iterval
	movq	(%rax), %rax
	movq	(%rax), %rsi
.LBB17_6:                               # %cond.end
                                        #   in Loop: Header=BB17_3 Depth=1
	movq	%r13, %rdi
	callq	Perl_my_setenv
	movq	%r12, %rdi
	callq	Perl_hv_iternext
	movq	%rax, %rbx
	testq	%rbx, %rbx
	jne	.LBB17_3
.LBB17_7:                               # %if.end
	xorq	%rax, %rax
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp181:
	.size	Perl_magic_set_all_env, .Ltmp181-Perl_magic_set_all_env
	.cfi_endproc

	.globl	Perl_magic_clear_all_env
	.align	16, 0x90
	.type	Perl_magic_clear_all_env,@function
Perl_magic_clear_all_env:               # @Perl_magic_clear_all_env
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp184:
	.cfi_def_cfa_offset 16
.Ltmp185:
	.cfi_offset %rbx, -16
	movl	PL_use_safe_putenv(%rip), %eax
	movq	environ(%rip), %rcx
	testq	%rax, %rax
	jne	.LBB18_5
# BB#1:                                 # %if.then
	movq	PL_origenviron(%rip), %rax
	cmpq	%rax, %rcx
	je	.LBB18_6
# BB#2:                                 # %for.cond.preheader
	movq	(%rcx), %rdi
	testq	%rdi, %rdi
	je	.LBB18_5
# BB#3:
	movq	$8, %rbx
	.align	16, 0x90
.LBB18_4:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	callq	Perl_safesysfree
	movq	environ(%rip), %rcx
	movq	(%rcx,%rbx), %rdi
	addq	$8, %rbx
	testq	%rdi, %rdi
	jne	.LBB18_4
	jmp	.LBB18_5
.LBB18_6:                               # %if.then1
	movq	$8, %rdi
	callq	Perl_safesysmalloc
	movq	%rax, %rcx
	movq	%rcx, environ(%rip)
.LBB18_5:                               # %if.end4
	xorq	%rax, %rax
	movq	$0, (%rcx)
	popq	%rbx
	retq
.Ltmp186:
	.size	Perl_magic_clear_all_env, .Ltmp186-Perl_magic_clear_all_env
	.cfi_endproc

	.globl	Perl_magic_getsig
	.align	16, 0x90
	.type	Perl_magic_getsig,@function
Perl_magic_getsig:                      # @Perl_magic_getsig
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp192:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp193:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp194:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp195:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp196:
	.cfi_def_cfa_offset 48
.Ltmp197:
	.cfi_offset %rbx, -40
.Ltmp198:
	.cfi_offset %r12, -32
.Ltmp199:
	.cfi_offset %r14, -24
.Ltmp200:
	.cfi_offset %r15, -16
	movq	40(%rsi), %rax
	movq	%rdi, %r14
	movq	%rax, (%rsp)
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	movq	32(%rsi), %r12
	andq	%r15, %rax
	movabsq	$4294967294, %rcx       # imm = 0xFFFFFFFE
	cmpq	%rcx, %rax
	jne	.LBB19_4
# BB#1:                                 # %cond.true
	movzbl	18(%r12), %eax
	testq	$4, %rax
	je	.LBB19_3
# BB#2:                                 # %cond.true4
	movq	(%r12), %rax
	movq	8(%rax), %rcx
	movq	%rcx, (%rsp)
	movq	(%rax), %r12
	jmp	.LBB19_4
.LBB19_3:                               # %cond.false
	leaq	(%rsp), %rsi
	movq	$2, %rdx
	movq	%r12, %rdi
	callq	Perl_sv_2pv_flags
	movq	%rax, %r12
.LBB19_4:                               # %cond.end11
	movq	PL_sig_name(%rip), %rsi
	testq	%rsi, %rsi
	je	.LBB19_18
# BB#5:
	movabsq	$PL_sig_name, %rbx
	.align	16, 0x90
.LBB19_7:                               # %for.body.i
                                        # =>This Inner Loop Header: Depth=1
	movq	%r12, %rdi
	callq	strcmp
	testq	%r15, %rax
	je	.LBB19_8
# BB#6:                                 # %for.cond.i
                                        #   in Loop: Header=BB19_7 Depth=1
	movq	8(%rbx), %rsi
	addq	$8, %rbx
	testq	%rsi, %rsi
	jne	.LBB19_7
.LBB19_18:                              # %for.end.i
	movabsq	$.L.str39, %rsi
	movq	%r12, %rdi
	callq	strcmp
	testq	%r15, %rax
	movq	$17, %rbx
	jne	.LBB19_17
	jmp	.LBB19_9
.LBB19_8:                               # %Perl_whichsig.exit
	movabsq	$PL_sig_name, %rax
	subq	%rax, %rbx
	movq	$3, %rcx
	sarq	%cl, %rbx
	movslq	PL_sig_num(,%rbx,4), %rbx
	testq	%rbx, %rbx
	jle	.LBB19_17
.LBB19_9:                               # %if.then
	movq	PL_psig_ptr(%rip), %rax
	movq	(%rax,%rbx,8), %rsi
	testq	%rsi, %rsi
	je	.LBB19_11
# BB#10:                                # %if.then16
	movq	$2, %rdx
	movq	%r14, %rdi
	callq	Perl_sv_setsv_flags
	jmp	.LBB19_17
.LBB19_11:                              # %if.else
	movq	%rbx, %rdi
	callq	Perl_rsignal_state
	cmpq	$1, %rax
	je	.LBB19_12
# BB#13:                                # %if.else23
	movabsq	$PL_sv_undef, %rsi
	movq	$2, %rdx
	movq	%r14, %rdi
	callq	Perl_sv_setsv_flags
	jmp	.LBB19_14
.LBB19_12:                              # %if.then22
	movabsq	$.L.str15, %rsi
	movq	%r14, %rdi
	callq	Perl_sv_setpv
.LBB19_14:                              # %if.end
	movq	%r14, PL_Sv(%rip)
	testq	%r14, %r14
	je	.LBB19_16
# BB#15:                                # %land.rhs
	incq	8(%r14)
.LBB19_16:                              # %land.end
	movq	PL_psig_ptr(%rip), %rax
	movq	%r14, (%rax,%rbx,8)
	andq	$-2049, 16(%r14)        # imm = 0xFFFFFFFFFFFFF7FF
.LBB19_17:                              # %if.end30
	xorq	%rax, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.Ltmp201:
	.size	Perl_magic_getsig, .Ltmp201-Perl_magic_getsig
	.cfi_endproc

	.globl	Perl_whichsig
	.align	16, 0x90
	.type	Perl_whichsig,@function
Perl_whichsig:                          # @Perl_whichsig
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp206:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp207:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp208:
	.cfi_def_cfa_offset 32
.Ltmp209:
	.cfi_offset %rbx, -32
.Ltmp210:
	.cfi_offset %r14, -24
.Ltmp211:
	.cfi_offset %r15, -16
	movq	PL_sig_name(%rip), %rsi
	movq	%rdi, %r14
	testq	%rsi, %rsi
	je	.LBB20_5
# BB#1:
	movabsq	$PL_sig_name, %rbx
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB20_3:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	%r14, %rdi
	callq	strcmp
	testq	%r15, %rax
	je	.LBB20_4
# BB#2:                                 # %for.cond
                                        #   in Loop: Header=BB20_3 Depth=1
	movq	8(%rbx), %rsi
	addq	$8, %rbx
	testq	%rsi, %rsi
	jne	.LBB20_3
.LBB20_5:                               # %for.end
	movabsq	$.L.str39, %rsi
	movq	%r14, %rdi
	callq	strcmp
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rax, %rcx
	cmpq	$1, %rcx
	sbbq	%rax, %rax
	notq	%rax
	orq	$17, %rax
	jmp	.LBB20_6
.LBB20_4:                               # %if.then
	movabsq	$PL_sig_name, %rax
	subq	%rax, %rbx
	movq	$3, %rcx
	sarq	%cl, %rbx
	movslq	PL_sig_num(,%rbx,4), %rax
.LBB20_6:                               # %return
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp212:
	.size	Perl_whichsig, .Ltmp212-Perl_whichsig
	.cfi_endproc

	.globl	Perl_magic_clearsig
	.align	16, 0x90
	.type	Perl_magic_clearsig,@function
Perl_magic_clearsig:                    # @Perl_magic_clearsig
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp217:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp218:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp219:
	.cfi_def_cfa_offset 32
	subq	$16, %rsp
.Ltmp220:
	.cfi_def_cfa_offset 48
.Ltmp221:
	.cfi_offset %rbx, -32
.Ltmp222:
	.cfi_offset %r14, -24
.Ltmp223:
	.cfi_offset %r15, -16
	movq	40(%rsi), %rax
	movq	%rax, 8(%rsp)
	movabsq	$4294967295, %r14       # imm = 0xFFFFFFFF
	movq	32(%rsi), %r15
	andq	%r14, %rax
	movabsq	$4294967294, %rcx       # imm = 0xFFFFFFFE
	cmpq	%rcx, %rax
	jne	.LBB21_4
# BB#1:                                 # %cond.true
	movzbl	18(%r15), %eax
	testq	$4, %rax
	je	.LBB21_3
# BB#2:                                 # %cond.true4
	movq	(%r15), %rax
	movq	8(%rax), %rcx
	movq	%rcx, 8(%rsp)
	movq	(%rax), %r15
	jmp	.LBB21_4
.LBB21_3:                               # %cond.false
	leaq	8(%rsp), %rsi
	movq	$2, %rdx
	movq	%r15, %rdi
	callq	Perl_sv_2pv_flags
	movq	%rax, %r15
.LBB21_4:                               # %cond.end11
	movzbl	(%r15), %eax
	cmpq	$95, %rax
	jne	.LBB21_12
# BB#5:                                 # %if.then
	movabsq	$.L.str16, %rsi
	movq	%r15, %rdi
	callq	strcmp
	testq	%r14, %rax
	je	.LBB21_6
# BB#7:                                 # %if.else
	movabsq	$.L.str17, %rsi
	movq	%r15, %rdi
	callq	strcmp
	testq	%r14, %rax
	je	.LBB21_8
# BB#9:                                 # %if.else21
	movabsq	$.L.str18, %rdi
	xorq	%rax, %rax
	movq	%r15, %rsi
	callq	Perl_croak
                                        # implicit-def: RAX
	jmp	.LBB21_10
.LBB21_12:                              # %if.else26
	movq	PL_sig_name(%rip), %rsi
	testq	%rsi, %rsi
	je	.LBB21_28
# BB#13:
	movabsq	$PL_sig_name, %rbx
	.align	16, 0x90
.LBB21_15:                              # %for.body.i
                                        # =>This Inner Loop Header: Depth=1
	movq	%r15, %rdi
	callq	strcmp
	testq	%r14, %rax
	je	.LBB21_16
# BB#14:                                # %for.cond.i
                                        #   in Loop: Header=BB21_15 Depth=1
	movq	8(%rbx), %rsi
	addq	$8, %rbx
	testq	%rsi, %rsi
	jne	.LBB21_15
.LBB21_28:                              # %for.end.i
	movabsq	$.L.str39, %rsi
	movq	%r15, %rdi
	callq	strcmp
	testq	%r14, %rax
	movq	$17, %r14
	jne	.LBB21_27
	jmp	.LBB21_17
.LBB21_6:
	movabsq	$PL_diehook, %rax
	jmp	.LBB21_10
.LBB21_16:                              # %Perl_whichsig.exit
	movabsq	$PL_sig_name, %rax
	subq	%rax, %rbx
	movq	$3, %rcx
	sarq	%cl, %rbx
	movslq	PL_sig_num(,%rbx,4), %r14
	testq	%r14, %r14
	jle	.LBB21_27
.LBB21_17:                              # %if.then30
	movl	PL_sig_pending(%rip), %eax
	testq	%rax, %rax
	je	.LBB21_22
# BB#18:                                # %if.then32
	xorq	%r15, %r15
	movl	%r15d, PL_sig_pending(%rip)
	movq	$1, %rbx
	.align	16, 0x90
.LBB21_19:                              # %for.body.i68
                                        # =>This Inner Loop Header: Depth=1
	movq	PL_psig_pend(%rip), %rax
	movl	(%rax,%rbx,4), %ecx
	testq	%rcx, %rcx
	je	.LBB21_21
# BB#20:                                # %if.then.i69
                                        #   in Loop: Header=BB21_19 Depth=1
	movl	%r15d, (%rax,%rbx,4)
	movq	%rbx, %rdi
	callq	*PL_sighandlerp(%rip)
.LBB21_21:                              # %for.inc.i
                                        #   in Loop: Header=BB21_19 Depth=1
	incq	%rbx
	cmpq	$16, %rbx
	jne	.LBB21_19
.LBB21_22:                              # %if.end33
	xorq	%rsi, %rsi
	movq	%r14, %rdi
	callq	Perl_rsignal
	movq	PL_psig_name(%rip), %rax
	movq	(%rax,%r14,8), %rdi
	testq	%rdi, %rdi
	je	.LBB21_24
# BB#23:                                # %if.then37
	callq	Perl_sv_free
	movq	PL_psig_name(%rip), %rax
	movq	$0, (%rax,%r14,8)
.LBB21_24:                              # %if.end40
	movq	PL_psig_ptr(%rip), %rax
	movq	(%rax,%r14,8), %rbx
	testq	%rbx, %rbx
	je	.LBB21_26
# BB#25:                                # %if.then43
	movq	$0, (%rax,%r14,8)
	callq	Perl_pop_scope
	movq	%rbx, %rdi
	callq	Perl_sv_free
	jmp	.LBB21_27
.LBB21_8:
	movabsq	$PL_warnhook, %rax
.LBB21_10:                              # %if.end22
	movq	(%rax), %rdi
	testq	%rdi, %rdi
	je	.LBB21_27
# BB#11:                                # %if.then24
	movq	$0, (%rax)
	callq	Perl_sv_free
	jmp	.LBB21_27
.LBB21_26:                              # %if.else46
	callq	Perl_pop_scope
.LBB21_27:                              # %if.end49
	xorq	%rax, %rax
	addq	$16, %rsp
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp224:
	.size	Perl_magic_clearsig, .Ltmp224-Perl_magic_clearsig
	.cfi_endproc

	.globl	Perl_despatch_signals
	.align	16, 0x90
	.type	Perl_despatch_signals,@function
Perl_despatch_signals:                  # @Perl_despatch_signals
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp228:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp229:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp230:
	.cfi_def_cfa_offset 32
.Ltmp231:
	.cfi_offset %rbx, -24
.Ltmp232:
	.cfi_offset %r14, -16
	xorq	%r14, %r14
	movl	%r14d, PL_sig_pending(%rip)
	movq	$1, %rbx
	.align	16, 0x90
.LBB22_1:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	PL_psig_pend(%rip), %rax
	movl	(%rax,%rbx,4), %ecx
	testq	%rcx, %rcx
	je	.LBB22_3
# BB#2:                                 # %if.then
                                        #   in Loop: Header=BB22_1 Depth=1
	movl	%r14d, (%rax,%rbx,4)
	movq	%rbx, %rdi
	callq	*PL_sighandlerp(%rip)
.LBB22_3:                               # %for.inc
                                        #   in Loop: Header=BB22_1 Depth=1
	incq	%rbx
	cmpq	$16, %rbx
	jne	.LBB22_1
# BB#4:                                 # %for.end
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp233:
	.size	Perl_despatch_signals, .Ltmp233-Perl_despatch_signals
	.cfi_endproc

	.globl	Perl_raise_signal
	.align	16, 0x90
	.type	Perl_raise_signal,@function
Perl_raise_signal:                      # @Perl_raise_signal
	.cfi_startproc
# BB#0:                                 # %entry
	movq	$32, %rcx
	movq	PL_psig_pend(%rip), %rax
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	movl	(%rax,%rdi,4), %ecx
	incq	%rcx
	movl	%ecx, (%rax,%rdi,4)
	movq	$1, %rax
	movl	%eax, PL_sig_pending(%rip)
	retq
.Ltmp234:
	.size	Perl_raise_signal, .Ltmp234-Perl_raise_signal
	.cfi_endproc

	.globl	Perl_csighandler
	.align	16, 0x90
	.type	Perl_csighandler,@function
Perl_csighandler:                       # @Perl_csighandler
	.cfi_startproc
# BB#0:                                 # %entry
	movzbl	PL_signals(%rip), %eax
	testq	$1, %rax
	je	.LBB24_1
# BB#2:                                 # %if.then
	jmpq	*PL_sighandlerp(%rip)  # TAILCALL
.LBB24_1:                               # %if.end
	movq	$32, %rcx
	movq	PL_psig_pend(%rip), %rax
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	movl	(%rax,%rdi,4), %ecx
	incq	%rcx
	movl	%ecx, (%rax,%rdi,4)
	movq	$1, %rax
	movl	%eax, PL_sig_pending(%rip)
	retq
.Ltmp235:
	.size	Perl_csighandler, .Ltmp235-Perl_csighandler
	.cfi_endproc

	.globl	Perl_magic_setsig
	.align	16, 0x90
	.type	Perl_magic_setsig,@function
Perl_magic_setsig:                      # @Perl_magic_setsig
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp242:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp243:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp244:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp245:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp246:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp247:
	.cfi_def_cfa_offset 64
.Ltmp248:
	.cfi_offset %rbx, -48
.Ltmp249:
	.cfi_offset %r12, -40
.Ltmp250:
	.cfi_offset %r13, -32
.Ltmp251:
	.cfi_offset %r14, -24
.Ltmp252:
	.cfi_offset %r15, -16
	movq	40(%rsi), %rax
	movq	%rdi, %rbx
	movq	%rax, 8(%rsp)
	movabsq	$4294967295, %r14       # imm = 0xFFFFFFFF
	movq	32(%rsi), %r13
	andq	%r14, %rax
	movabsq	$4294967294, %rcx       # imm = 0xFFFFFFFE
	cmpq	%rcx, %rax
	jne	.LBB25_4
# BB#1:                                 # %cond.true
	movzbl	18(%r13), %eax
	testq	$4, %rax
	je	.LBB25_3
# BB#2:                                 # %cond.true4
	movq	(%r13), %rax
	movq	8(%rax), %rcx
	movq	%rcx, 8(%rsp)
	movq	(%rax), %r13
	jmp	.LBB25_4
.LBB25_3:                               # %cond.false
	leaq	8(%rsp), %rsi
	movq	$2, %rdx
	movq	%r13, %rdi
	callq	Perl_sv_2pv_flags
	movq	%rax, %r13
.LBB25_4:                               # %cond.end11
	movzbl	(%r13), %eax
	cmpq	$95, %rax
	jne	.LBB25_13
# BB#5:                                 # %if.then
	movabsq	$.L.str16, %rsi
	movq	%r13, %rdi
	callq	strcmp
	testq	%r14, %rax
	je	.LBB25_6
# BB#7:                                 # %if.else
	movabsq	$.L.str17, %rsi
	movq	%r13, %rdi
	callq	strcmp
	testq	%r14, %rax
	je	.LBB25_8
# BB#9:                                 # %if.else21
	movabsq	$.L.str18, %rdi
	xorq	%rax, %rax
	movq	%r13, %rsi
	callq	Perl_croak
	xorq	%rdx, %rdx
	jmp	.LBB25_10
.LBB25_13:                              # %if.else26
	movq	%rbx, (%rsp)            # 8-byte Spill
	movq	PL_sig_name(%rip), %rsi
	testq	%rsi, %rsi
	je	.LBB25_25
# BB#14:
	movabsq	$PL_sig_name, %rbx
	.align	16, 0x90
.LBB25_16:                              # %for.body.i
                                        # =>This Inner Loop Header: Depth=1
	movq	%r13, %rdi
	callq	strcmp
	testq	%r14, %rax
	je	.LBB25_17
# BB#15:                                # %for.cond.i
                                        #   in Loop: Header=BB25_16 Depth=1
	movq	8(%rbx), %rsi
	addq	$8, %rbx
	testq	%rsi, %rsi
	jne	.LBB25_16
.LBB25_25:                              # %for.end.i
	movabsq	$.L.str39, %rsi
	movq	%r13, %rdi
	callq	strcmp
	testq	%r14, %rax
	movq	$17, %r12
	jne	.LBB25_18
	jmp	.LBB25_26
.LBB25_6:
	movabsq	$PL_diehook, %rdx
	jmp	.LBB25_10
.LBB25_17:                              # %Perl_whichsig.exit
	movabsq	$PL_sig_name, %rax
	subq	%rax, %rbx
	movq	$3, %rcx
	sarq	%cl, %rbx
	movslq	PL_sig_num(,%rbx,4), %r12
	testq	%r12, %r12
	js	.LBB25_18
.LBB25_26:                              # %if.end56
	movl	PL_sig_pending(%rip), %eax
	testq	%rax, %rax
	je	.LBB25_31
# BB#27:                                # %if.then58
	xorq	%r15, %r15
	movl	%r15d, PL_sig_pending(%rip)
	movq	$1, %rbx
	.align	16, 0x90
.LBB25_28:                              # %for.body.i201
                                        # =>This Inner Loop Header: Depth=1
	movq	PL_psig_pend(%rip), %rax
	movl	(%rax,%rbx,4), %ecx
	testq	%rcx, %rcx
	je	.LBB25_30
# BB#29:                                # %if.then.i202
                                        #   in Loop: Header=BB25_28 Depth=1
	movl	%r15d, (%rax,%rbx,4)
	movq	%rbx, %rdi
	callq	*PL_sighandlerp(%rip)
.LBB25_30:                              # %for.inc.i
                                        #   in Loop: Header=BB25_28 Depth=1
	incq	%rbx
	cmpq	$16, %rbx
	jne	.LBB25_28
.LBB25_31:                              # %if.end59
	movq	PL_psig_name(%rip), %rax
	movq	(%rax,%r12,8), %rdi
	callq	Perl_sv_free
	movq	PL_psig_ptr(%rip), %rax
	movq	(%rax,%r12,8), %r15
	movq	(%rsp), %rbx            # 8-byte Reload
	movq	%rbx, PL_Sv(%rip)
	testq	%rbx, %rbx
	je	.LBB25_33
# BB#32:                                # %land.rhs
	incq	8(%rbx)
.LBB25_33:                              # %land.end
	movq	%rbx, (%rax,%r12,8)
	andq	$-2049, 16(%rbx)        # imm = 0xFFFFFFFFFFFFF7FF
	movq	8(%rsp), %rsi
	movq	%r13, %rdi
	callq	Perl_newSVpvn
	movq	PL_psig_name(%rip), %rcx
	movq	%rax, (%rcx,%r12,8)
	movq	PL_psig_name(%rip), %rax
	movq	(%rax,%r12,8), %rax
	orq	$8388608, 16(%rax)      # imm = 0x800000
	xorq	%rdx, %rdx
	jmp	.LBB25_34
.LBB25_18:                              # %if.then30
	movq	PL_curcop(%rip), %rax
	cmpq	$0, 88(%rax)
	je	.LBB25_22
# BB#19:                                # %land.lhs.true
	movq	PL_curcop(%rip), %rax
	movq	88(%rax), %rax
	cmpq	$48, %rax
	je	.LBB25_22
# BB#20:                                # %land.lhs.true36
	movq	PL_curcop(%rip), %rax
	movq	88(%rax), %rax
	cmpq	$24, %rax
	je	.LBB25_24
# BB#21:                                # %lor.lhs.false
	movq	PL_curcop(%rip), %rax
	movq	88(%rax), %rax
	movq	(%rax), %rax
	movq	(%rax), %rax
	movzbl	6(%rax), %eax
	testq	$16, %rax
	jne	.LBB25_24
.LBB25_22:                              # %lor.lhs.false46
	movq	PL_curcop(%rip), %rax
	cmpq	$0, 88(%rax)
	jne	.LBB25_62
# BB#23:                                # %land.lhs.true50
	movzbl	PL_dowarn(%rip), %eax
	testq	$1, %rax
	je	.LBB25_62
.LBB25_24:                              # %if.then54
	movabsq	$.L.str19, %rsi
	movq	$26, %rdi
	xorq	%rax, %rax
	movq	%r13, %rdx
	callq	Perl_warner
	jmp	.LBB25_62
.LBB25_8:
	movabsq	$PL_warnhook, %rdx
.LBB25_10:                              # %if.end22
	movq	(%rdx), %r15
	xorq	%r12, %r12
	testq	%r15, %r15
	je	.LBB25_11
# BB#12:                                # %if.then24
	movq	$0, (%rdx)
	xorq	%r12, %r12
	jmp	.LBB25_34
.LBB25_11:
	xorq	%r15, %r15
.LBB25_34:                              # %if.end71
	movq	16(%rbx), %rax
	movq	%rax, %rcx
	andq	$255, %rcx
	cmpq	$13, %rcx
	je	.LBB25_36
# BB#35:                                # %if.end71
	movq	%rax, %rcx
	andq	$524288, %rcx           # imm = 0x80000
	jne	.LBB25_36
# BB#41:                                # %if.end97
	andq	$10223616, %rax         # imm = 0x9C0000
	cmpq	$262144, %rax           # imm = 0x40000
	jne	.LBB25_43
# BB#42:                                # %cond.true102
	movq	%rdx, (%rsp)            # 8-byte Spill
	movq	(%rbx), %rax
	movq	%rbx, %r13
	movq	8(%rax), %rcx
	movq	%rcx, 8(%rsp)
	movq	(%rax), %rbx
	jmp	.LBB25_44
.LBB25_36:                              # %if.then80
	testq	%r12, %r12
	je	.LBB25_38
# BB#37:                                # %if.then82
	movq	PL_csighandlerp(%rip), %rsi
	jmp	.LBB25_47
.LBB25_43:                              # %cond.false107
	movq	%rdx, (%rsp)            # 8-byte Spill
	leaq	8(%rsp), %rsi
	movq	$2, %rdx
	movq	%rbx, %rdi
	movq	%rbx, %r13
	callq	Perl_sv_pvn_force_flags
	movq	%rax, %rbx
.LBB25_44:                              # %cond.end109
	movabsq	$.L.str15, %rsi
	movq	%rbx, %rdi
	callq	strcmp
	testq	%r14, %rax
	je	.LBB25_45
# BB#48:                                # %if.else119
	movabsq	$.L.str20, %rsi
	movq	%rbx, %rdi
	callq	strcmp
	testq	%r14, %rax
	je	.LBB25_50
# BB#49:                                # %lor.lhs.false122
	movzbl	(%rbx), %eax
	testq	%rax, %rax
	je	.LBB25_50
# BB#52:                                # %if.else130
	movq	$58, %rsi
	movq	%rbx, %rdi
	callq	strchr
	testq	%rax, %rax
	jne	.LBB25_55
# BB#53:                                # %land.lhs.true133
	movq	$39, %rsi
	movq	%rbx, %rdi
	callq	strchr
	testq	%rax, %rax
	jne	.LBB25_55
# BB#54:                                # %if.then136
	movabsq	$.L.str21, %rcx
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	movq	$6, %r8
	movq	%r13, %rdi
	callq	Perl_sv_insert
.LBB25_55:                              # %if.end137
	testq	%r12, %r12
	je	.LBB25_57
# BB#56:                                # %if.then139
	movq	PL_csighandlerp(%rip), %rsi
	jmp	.LBB25_47
.LBB25_50:                              # %if.then124
	testq	%r12, %r12
	je	.LBB25_60
# BB#51:                                # %if.then126
	xorq	%rsi, %rsi
	jmp	.LBB25_47
.LBB25_38:                              # %if.else85
	movq	%rbx, PL_Sv(%rip)
	testq	%rbx, %rbx
	je	.LBB25_40
# BB#39:                                # %land.rhs87
	incq	8(%rbx)
.LBB25_40:                              # %land.end91
	movq	%rbx, (%rdx)
	jmp	.LBB25_60
.LBB25_45:                              # %if.then113
	testq	%r12, %r12
	je	.LBB25_60
# BB#46:                                # %if.then115
	movq	$1, %rsi
.LBB25_47:                              # %if.end152
	movq	%r12, %rdi
	callq	Perl_rsignal
.LBB25_60:                              # %if.end152
	testq	%r15, %r15
	je	.LBB25_62
# BB#61:                                # %if.then154
	movq	%r15, %rdi
	callq	Perl_sv_free
.LBB25_62:                              # %return
	xorq	%rax, %rax
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB25_57:                              # %if.else142
	movq	%r13, PL_Sv(%rip)
	testq	%r13, %r13
	je	.LBB25_59
# BB#58:                                # %land.rhs144
	incq	8(%r13)
.LBB25_59:                              # %land.end148
	movq	(%rsp), %rax            # 8-byte Reload
	movq	%r13, (%rax)
	jmp	.LBB25_60
.Ltmp253:
	.size	Perl_magic_setsig, .Ltmp253-Perl_magic_setsig
	.cfi_endproc

	.globl	Perl_magic_setisa
	.align	16, 0x90
	.type	Perl_magic_setisa,@function
Perl_magic_setisa:                      # @Perl_magic_setisa
	.cfi_startproc
# BB#0:                                 # %entry
	incq	PL_sub_generation(%rip)
	xorq	%rax, %rax
	retq
.Ltmp254:
	.size	Perl_magic_setisa, .Ltmp254-Perl_magic_setisa
	.cfi_endproc

	.globl	Perl_magic_setamagic
	.align	16, 0x90
	.type	Perl_magic_setamagic,@function
Perl_magic_setamagic:                   # @Perl_magic_setamagic
	.cfi_startproc
# BB#0:                                 # %entry
	incq	PL_amagic_generation(%rip)
	xorq	%rax, %rax
	retq
.Ltmp255:
	.size	Perl_magic_setamagic, .Ltmp255-Perl_magic_setamagic
	.cfi_endproc

	.globl	Perl_magic_getnkeys
	.align	16, 0x90
	.type	Perl_magic_getnkeys,@function
Perl_magic_getnkeys:                    # @Perl_magic_getnkeys
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp260:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp261:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp262:
	.cfi_def_cfa_offset 32
.Ltmp263:
	.cfi_offset %rbx, -32
.Ltmp264:
	.cfi_offset %r14, -24
.Ltmp265:
	.cfi_offset %r15, -16
	movq	%rdi, %r14
	movq	(%r14), %rax
	movq	72(%rax), %r15
	xorq	%rbx, %rbx
	testq	%r15, %r15
	je	.LBB28_10
# BB#1:                                 # %if.then
	movq	%r15, %rdi
	callq	Perl_hv_iterinit
	movzbl	17(%r15), %eax
	testq	$128, %rax
	je	.LBB28_2
# BB#4:                                 # %if.end.i
	movq	(%r15), %rax
	movq	40(%rax), %rcx
	jmp	.LBB28_6
	.align	16, 0x90
.LBB28_5:                               # %for.cond.i
                                        #   in Loop: Header=BB28_6 Depth=1
	movq	(%rcx), %rcx
.LBB28_6:                               # %for.cond.i
                                        # =>This Inner Loop Header: Depth=1
	testq	%rcx, %rcx
	je	.LBB28_3
# BB#7:                                 # %for.body.i
                                        #   in Loop: Header=BB28_6 Depth=1
	movzbl	18(%rcx), %edx
	cmpq	$80, %rdx
	jne	.LBB28_5
# BB#8:
	movq	$-1, %rbx
	.align	16, 0x90
.LBB28_9:                               # %while.cond
                                        # =>This Inner Loop Header: Depth=1
	movq	%r15, %rdi
	callq	Perl_hv_iternext
	incq	%rbx
	testq	%rax, %rax
	jne	.LBB28_9
	jmp	.LBB28_10
.LBB28_2:                               # %if.then.if.then4_crit_edge
	movq	(%r15), %rax
.LBB28_3:                               # %if.then4
	movq	24(%rax), %rbx
	movq	32(%rax), %rdi
	callq	float64_to_int64
	subq	%rax, %rbx
.LBB28_10:                              # %if.end9
	movq	%r14, %rdi
	movq	%rbx, %rsi
	callq	Perl_sv_setiv
	xorq	%rax, %rax
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp266:
	.size	Perl_magic_getnkeys, .Ltmp266-Perl_magic_getnkeys
	.cfi_endproc

	.globl	Perl_magic_setnkeys
	.align	16, 0x90
	.type	Perl_magic_setnkeys,@function
Perl_magic_setnkeys:                    # @Perl_magic_setnkeys
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp269:
	.cfi_def_cfa_offset 16
.Ltmp270:
	.cfi_offset %rbx, -16
	movq	(%rdi), %rax
	movq	72(%rax), %rbx
	testq	%rbx, %rbx
	je	.LBB29_5
# BB#1:                                 # %if.then
	movzbl	18(%rdi), %ecx
	testq	$1, %rcx
	je	.LBB29_3
# BB#2:                                 # %cond.true
	movq	24(%rax), %rsi
	jmp	.LBB29_4
.LBB29_3:                               # %cond.false
	callq	Perl_sv_2iv
	movq	%rax, %rsi
.LBB29_4:                               # %cond.end
	movq	%rbx, %rdi
	callq	Perl_hv_ksplit
.LBB29_5:                               # %if.end
	xorq	%rax, %rax
	popq	%rbx
	retq
.Ltmp271:
	.size	Perl_magic_setnkeys, .Ltmp271-Perl_magic_setnkeys
	.cfi_endproc

	.globl	Perl_magic_getpack
	.align	16, 0x90
	.type	Perl_magic_getpack,@function
Perl_magic_getpack:                     # @Perl_magic_getpack
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rax
.Ltmp273:
	.cfi_def_cfa_offset 16
	cmpq	$0, 32(%rsi)
	je	.LBB30_2
# BB#1:                                 # %if.then
	movzbl	19(%rsi), %eax
	orq	$4, %rax
	movb	%al, 19(%rsi)
.LBB30_2:                               # %if.end
	movabsq	$.L.str22, %rdx
	xorq	%rax, %rax
	callq	magic_methpack
	xorq	%rax, %rax
	popq	%rdx
	retq
.Ltmp274:
	.size	Perl_magic_getpack, .Ltmp274-Perl_magic_getpack
	.cfi_endproc

	.globl	Perl_magic_setpack
	.align	16, 0x90
	.type	Perl_magic_setpack,@function
Perl_magic_setpack:                     # @Perl_magic_setpack
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp279:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp280:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp281:
	.cfi_def_cfa_offset 32
.Ltmp282:
	.cfi_offset %rbx, -32
.Ltmp283:
	.cfi_offset %r14, -24
.Ltmp284:
	.cfi_offset %r15, -16
	movq	PL_stack_sp(%rip), %rbx
	movq	%rsi, %r14
	movq	%rdi, %r15
	callq	Perl_push_scope
	movq	PL_curstackinfo(%rip), %rax
	movq	48(%rax), %rax
	testq	%rax, %rax
	jne	.LBB31_2
# BB#1:                                 # %if.then
	movq	$32, %rdi
	movq	$12, %rsi
	callq	Perl_new_stackinfo
	movq	PL_curstackinfo(%rip), %rcx
	movq	%rcx, 40(%rax)
	movq	PL_curstackinfo(%rip), %rcx
	movq	%rax, 48(%rcx)
.LBB31_2:                               # %if.end
	movq	$2, 32(%rax)
	movq	$-1, 16(%rax)
	movq	(%rax), %rcx
	movq	(%rcx), %rdx
	movq	$0, 8(%rdx)
	movq	PL_curstack(%rip), %rcx
	movq	PL_stack_base(%rip), %rsi
	movq	(%rcx), %rdi
	subq	%rsi, %rbx
	movq	$3, %rcx
	sarq	%cl, %rbx
	movq	%rbx, 8(%rdi)
	movq	(%rdx), %rcx
	movq	%rcx, PL_stack_base(%rip)
	movq	(%rax), %rdx
	movq	(%rdx), %rdx
	movq	16(%rdx), %rdx
	leaq	(%rcx,%rdx,8), %rdx
	movq	%rdx, PL_stack_max(%rip)
	movq	(%rax), %rdx
	movq	(%rdx), %rdx
	movq	8(%rdx), %rdx
	leaq	(%rcx,%rdx,8), %rcx
	movq	%rcx, PL_stack_sp(%rip)
	movq	(%rax), %rcx
	movq	%rcx, PL_curstack(%rip)
	movq	%rax, PL_curstackinfo(%rip)
	movabsq	$.L.str23, %rdx
	movq	$2, %rcx
	movq	$3, %r8
	xorq	%rax, %rax
	movq	%r15, %rdi
	movq	%r14, %rsi
	movq	%r15, %r9
	callq	magic_methcall
	movq	PL_curstackinfo(%rip), %rax
	movq	40(%rax), %r14
	movq	PL_stack_sp(%rip), %rbx
	testq	%r14, %r14
	jne	.LBB31_9
# BB#3:                                 # %if.then19
	movq	PL_stderrgv(%rip), %rax
	testq	%rax, %rax
	je	.LBB31_7
# BB#4:                                 # %land.lhs.true
	movzbl	16(%rax), %ecx
	cmpq	$13, %rcx
	jne	.LBB31_7
# BB#5:                                 # %land.lhs.true21
	movq	(%rax), %rax
	movq	56(%rax), %rax
	movq	16(%rax), %rax
	testq	%rax, %rax
	je	.LBB31_7
# BB#6:                                 # %land.lhs.true24
	movq	(%rax), %rax
	movq	64(%rax), %rdi
	testq	%rdi, %rdi
	jne	.LBB31_8
.LBB31_7:                               # %cond.false
	callq	Perl_PerlIO_stderr
	movq	%rax, %rdi
.LBB31_8:                               # %cond.end
	movabsq	$.L.str24, %rsi
	xorq	%rax, %rax
	callq	PerlIO_printf
	movq	$1, %rdi
	callq	Perl_my_exit
.LBB31_9:                               # %if.end37
	movq	PL_curstack(%rip), %rax
	movq	PL_stack_base(%rip), %rcx
	movq	(%rax), %rax
	subq	%rcx, %rbx
	movq	$3, %rcx
	sarq	%cl, %rbx
	movq	%rbx, 8(%rax)
	movq	(%r14), %rax
	movq	(%rax), %rax
	movq	(%rax), %rax
	movq	%rax, PL_stack_base(%rip)
	movq	(%r14), %rcx
	movq	(%rcx), %rcx
	movq	16(%rcx), %rcx
	leaq	(%rax,%rcx,8), %rcx
	movq	%rcx, PL_stack_max(%rip)
	movq	(%r14), %rcx
	movq	(%rcx), %rcx
	movq	8(%rcx), %rcx
	leaq	(%rax,%rcx,8), %rax
	movq	%rax, PL_stack_sp(%rip)
	movq	(%r14), %rax
	movq	%rax, PL_curstack(%rip)
	movq	%r14, PL_curstackinfo(%rip)
	callq	Perl_pop_scope
	xorq	%rax, %rax
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp285:
	.size	Perl_magic_setpack, .Ltmp285-Perl_magic_setpack
	.cfi_endproc

	.globl	Perl_magic_clearpack
	.align	16, 0x90
	.type	Perl_magic_clearpack,@function
Perl_magic_clearpack:                   # @Perl_magic_clearpack
	.cfi_startproc
# BB#0:                                 # %entry
	movabsq	$.L.str25, %rdx
	xorq	%rax, %rax
	jmp	magic_methpack  # TAILCALL
.Ltmp286:
	.size	Perl_magic_clearpack, .Ltmp286-Perl_magic_clearpack
	.cfi_endproc

	.globl	Perl_magic_sizepack
	.align	16, 0x90
	.type	Perl_magic_sizepack,@function
Perl_magic_sizepack:                    # @Perl_magic_sizepack
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp292:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp293:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp294:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp295:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp296:
	.cfi_def_cfa_offset 48
.Ltmp297:
	.cfi_offset %rbx, -40
.Ltmp298:
	.cfi_offset %r12, -32
.Ltmp299:
	.cfi_offset %r14, -24
.Ltmp300:
	.cfi_offset %r15, -16
	movq	PL_stack_sp(%rip), %rbx
	movq	%rsi, %r14
	movq	%rdi, %r15
	callq	Perl_push_scope
	movabsq	$PL_tmps_floor, %rdi
	callq	Perl_save_int
	movq	PL_tmps_ix(%rip), %rax
	movq	PL_curstackinfo(%rip), %rcx
	movq	%rax, PL_tmps_floor(%rip)
	movq	48(%rcx), %rax
	testq	%rax, %rax
	jne	.LBB33_2
# BB#1:                                 # %if.then
	movq	$32, %rdi
	movq	$12, %rsi
	callq	Perl_new_stackinfo
	movq	PL_curstackinfo(%rip), %rcx
	movq	%rcx, 40(%rax)
	movq	PL_curstackinfo(%rip), %rcx
	movq	%rax, 48(%rcx)
.LBB33_2:                               # %if.end
	movq	$2, 32(%rax)
	movq	$-1, 16(%rax)
	movq	(%rax), %rcx
	movq	(%rcx), %rdx
	movq	$0, 8(%rdx)
	movq	PL_curstack(%rip), %rcx
	movq	PL_stack_base(%rip), %rsi
	movq	(%rcx), %rdi
	subq	%rsi, %rbx
	movq	$3, %rcx
	sarq	%cl, %rbx
	movq	%rbx, 8(%rdi)
	movq	(%rdx), %rcx
	movq	%rcx, PL_stack_base(%rip)
	movq	(%rax), %rdx
	movq	(%rdx), %rdx
	movq	16(%rdx), %rdx
	leaq	(%rcx,%rdx,8), %rdx
	movq	%rdx, PL_stack_max(%rip)
	movq	(%rax), %rdx
	movq	(%rdx), %rdx
	movq	8(%rdx), %rdx
	leaq	(%rcx,%rdx,8), %rcx
	movq	%rcx, PL_stack_sp(%rip)
	movq	(%rax), %rcx
	movq	%rcx, PL_curstack(%rip)
	movq	%rax, PL_curstackinfo(%rip)
	movabsq	$.L.str26, %rdx
	xorq	%rcx, %rcx
	movq	$2, %r8
	xorq	%r9, %r9
	xorq	%rax, %rax
	xorq	%r12, %r12
	movq	%r15, %rdi
	movq	%r14, %rsi
	callq	magic_methcall
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB33_7
# BB#3:                                 # %if.then16
	movq	PL_stack_sp(%rip), %rax
	leaq	-8(%rax), %rcx
	movq	%rcx, PL_stack_sp(%rip)
	movq	(%rax), %rdi
	movzbl	18(%rdi), %eax
	testq	$1, %rax
	je	.LBB33_5
# BB#4:                                 # %cond.true
	movq	(%rdi), %rax
	movq	24(%rax), %r12
	jmp	.LBB33_6
.LBB33_5:                               # %cond.false
	callq	Perl_sv_2iv
	movq	%rax, %r12
.LBB33_6:                               # %cond.end
	decq	%r12
.LBB33_7:                               # %if.end20
	movq	PL_curstackinfo(%rip), %rax
	movq	40(%rax), %r14
	movq	PL_stack_sp(%rip), %rbx
	testq	%r14, %r14
	jne	.LBB33_14
# BB#8:                                 # %if.then26
	movq	PL_stderrgv(%rip), %rax
	testq	%rax, %rax
	je	.LBB33_12
# BB#9:                                 # %land.lhs.true
	movzbl	16(%rax), %ecx
	cmpq	$13, %rcx
	jne	.LBB33_12
# BB#10:                                # %land.lhs.true30
	movq	(%rax), %rax
	movq	56(%rax), %rax
	movq	16(%rax), %rax
	testq	%rax, %rax
	je	.LBB33_12
# BB#11:                                # %land.lhs.true33
	movq	(%rax), %rax
	movq	64(%rax), %rdi
	testq	%rdi, %rdi
	jne	.LBB33_13
.LBB33_12:                              # %cond.false45
	callq	Perl_PerlIO_stderr
	movq	%rax, %rdi
.LBB33_13:                              # %cond.end47
	movabsq	$.L.str24, %rsi
	xorq	%rax, %rax
	callq	PerlIO_printf
	movq	$1, %rdi
	callq	Perl_my_exit
.LBB33_14:                              # %if.end50
	movq	PL_curstack(%rip), %rax
	movq	PL_stack_base(%rip), %rcx
	movq	(%rax), %rax
	subq	%rcx, %rbx
	movq	$3, %rcx
	sarq	%cl, %rbx
	movq	%rbx, 8(%rax)
	movq	(%r14), %rax
	movq	(%rax), %rax
	movq	(%rax), %rax
	movq	%rax, PL_stack_base(%rip)
	movq	(%r14), %rcx
	movq	(%rcx), %rcx
	movq	16(%rcx), %rcx
	leaq	(%rax,%rcx,8), %rcx
	movq	%rcx, PL_stack_max(%rip)
	movq	(%r14), %rcx
	movq	(%rcx), %rcx
	movq	8(%rcx), %rcx
	leaq	(%rax,%rcx,8), %rax
	movq	%rax, PL_stack_sp(%rip)
	movq	(%r14), %rax
	movq	PL_tmps_ix(%rip), %rcx
	movq	PL_tmps_floor(%rip), %rdx
	movq	%rax, PL_curstack(%rip)
	movq	%r14, PL_curstackinfo(%rip)
	cmpq	%rdx, %rcx
	jle	.LBB33_16
# BB#15:                                # %if.then72
	callq	Perl_free_tmps
.LBB33_16:                              # %if.end73
	callq	Perl_pop_scope
	movq	%r12, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.Ltmp301:
	.size	Perl_magic_sizepack, .Ltmp301-Perl_magic_sizepack
	.cfi_endproc

	.globl	Perl_magic_wipepack
	.align	16, 0x90
	.type	Perl_magic_wipepack,@function
Perl_magic_wipepack:                    # @Perl_magic_wipepack
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp306:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp307:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp308:
	.cfi_def_cfa_offset 32
.Ltmp309:
	.cfi_offset %rbx, -32
.Ltmp310:
	.cfi_offset %r14, -24
.Ltmp311:
	.cfi_offset %r15, -16
	movq	PL_stack_sp(%rip), %rbx
	movq	%rsi, %r15
	movq	%rdi, %r14
	callq	Perl_push_scope
	movq	PL_curstackinfo(%rip), %rax
	movq	48(%rax), %rax
	testq	%rax, %rax
	jne	.LBB34_2
# BB#1:                                 # %if.then
	movq	$32, %rdi
	movq	$12, %rsi
	callq	Perl_new_stackinfo
	movq	PL_curstackinfo(%rip), %rcx
	movq	%rcx, 40(%rax)
	movq	PL_curstackinfo(%rip), %rcx
	movq	%rax, 48(%rcx)
.LBB34_2:                               # %if.end
	movq	$2, 32(%rax)
	movq	$-1, 16(%rax)
	movq	(%rax), %rcx
	movq	(%rcx), %rdx
	movq	$0, 8(%rdx)
	movq	PL_curstack(%rip), %rcx
	movq	PL_stack_base(%rip), %rsi
	movq	(%rcx), %rdi
	subq	%rsi, %rbx
	movq	$3, %rcx
	sarq	%cl, %rbx
	movq	%rbx, 8(%rdi)
	movq	(%rdx), %rcx
	movq	%rcx, PL_stack_base(%rip)
	movq	(%rax), %rdx
	movq	(%rdx), %rdx
	movq	16(%rdx), %rdx
	leaq	(%rcx,%rdx,8), %rdx
	movq	%rdx, PL_stack_max(%rip)
	movq	(%rax), %rsi
	movq	(%rsi), %rsi
	movq	8(%rsi), %rsi
	leaq	(%rcx,%rsi,8), %rbx
	movq	%rbx, PL_stack_sp(%rip)
	movq	(%rax), %rsi
	movq	%rsi, PL_curstack(%rip)
	movq	PL_markstack_ptr(%rip), %rsi
	movq	PL_markstack_max(%rip), %rdi
	movq	%rax, PL_curstackinfo(%rip)
	addq	$8, %rsi
	movq	%rsi, PL_markstack_ptr(%rip)
	cmpq	%rdi, %rsi
	jne	.LBB34_4
# BB#3:                                 # %if.then13
	callq	Perl_markstack_grow
	movq	PL_stack_base(%rip), %rcx
	movq	PL_markstack_ptr(%rip), %rsi
	movq	PL_stack_max(%rip), %rdx
.LBB34_4:                               # %if.end14
	movq	%rbx, %rax
	subq	%rcx, %rax
	movq	$3, %rcx
	sarq	%cl, %rax
	movq	%rax, (%rsi)
	subq	%rbx, %rdx
	cmpq	$7, %rdx
	jg	.LBB34_6
# BB#5:                                 # %if.then25
	movq	$1, %rdx
	movq	%rbx, %rdi
	movq	%rbx, %rsi
	callq	Perl_stack_grow
	movq	%rax, %rbx
.LBB34_6:                               # %if.end27
	movq	24(%r15), %rax
	testq	%rax, %rax
	jne	.LBB34_8
# BB#7:                                 # %cond.false
	movq	%r14, %rdi
	callq	Perl_newRV
	movq	%rax, %rdi
	callq	Perl_sv_2mortal
.LBB34_8:                               # %cond.end
	movq	%rax, 8(%rbx)
	addq	$8, %rbx
	movq	%rbx, PL_stack_sp(%rip)
	movabsq	$.L.str27, %rdi
	movq	$2, %rsi
	callq	Perl_call_method
	movq	PL_curstackinfo(%rip), %rax
	movq	40(%rax), %r14
	movq	PL_stack_sp(%rip), %rbx
	testq	%r14, %r14
	jne	.LBB34_15
# BB#9:                                 # %if.then40
	movq	PL_stderrgv(%rip), %rax
	testq	%rax, %rax
	je	.LBB34_13
# BB#10:                                # %land.lhs.true
	movzbl	16(%rax), %ecx
	cmpq	$13, %rcx
	jne	.LBB34_13
# BB#11:                                # %land.lhs.true43
	movq	(%rax), %rax
	movq	56(%rax), %rax
	movq	16(%rax), %rax
	testq	%rax, %rax
	je	.LBB34_13
# BB#12:                                # %land.lhs.true46
	movq	(%rax), %rax
	movq	64(%rax), %rdi
	testq	%rdi, %rdi
	jne	.LBB34_14
.LBB34_13:                              # %cond.false58
	callq	Perl_PerlIO_stderr
	movq	%rax, %rdi
.LBB34_14:                              # %cond.end60
	movabsq	$.L.str24, %rsi
	xorq	%rax, %rax
	callq	PerlIO_printf
	movq	$1, %rdi
	callq	Perl_my_exit
.LBB34_15:                              # %if.end63
	movq	PL_curstack(%rip), %rax
	movq	PL_stack_base(%rip), %rcx
	movq	(%rax), %rax
	subq	%rcx, %rbx
	movq	$3, %rcx
	sarq	%cl, %rbx
	movq	%rbx, 8(%rax)
	movq	(%r14), %rax
	movq	(%rax), %rax
	movq	(%rax), %rax
	movq	%rax, PL_stack_base(%rip)
	movq	(%r14), %rcx
	movq	(%rcx), %rcx
	movq	16(%rcx), %rcx
	leaq	(%rax,%rcx,8), %rcx
	movq	%rcx, PL_stack_max(%rip)
	movq	(%r14), %rcx
	movq	(%rcx), %rcx
	movq	8(%rcx), %rcx
	leaq	(%rax,%rcx,8), %rax
	movq	%rax, PL_stack_sp(%rip)
	movq	(%r14), %rax
	movq	%rax, PL_curstack(%rip)
	movq	%r14, PL_curstackinfo(%rip)
	callq	Perl_pop_scope
	xorq	%rax, %rax
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp312:
	.size	Perl_magic_wipepack, .Ltmp312-Perl_magic_wipepack
	.cfi_endproc

	.globl	Perl_magic_nextpack
	.align	16, 0x90
	.type	Perl_magic_nextpack,@function
Perl_magic_nextpack:                    # @Perl_magic_nextpack
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp319:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp320:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp321:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp322:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp323:
	.cfi_def_cfa_offset 48
.Ltmp324:
	.cfi_offset %rbx, -48
.Ltmp325:
	.cfi_offset %r12, -40
.Ltmp326:
	.cfi_offset %r13, -32
.Ltmp327:
	.cfi_offset %r14, -24
.Ltmp328:
	.cfi_offset %r15, -16
	movq	%rdx, %r13
	movq	16(%r13), %rax
	movq	PL_stack_sp(%rip), %rbx
	movq	%rsi, %r12
	movq	%rdi, %r15
	testq	$118423552, %rax        # imm = 0x70F0000
	jne	.LBB35_1
# BB#2:                                 # %entry
	movabsq	$.L.str29, %r14
	jmp	.LBB35_3
.LBB35_1:
	movabsq	$.L.str28, %r14
.LBB35_3:                               # %entry
	callq	Perl_push_scope
	movabsq	$PL_tmps_floor, %rdi
	callq	Perl_save_int
	movq	PL_tmps_ix(%rip), %rax
	movq	PL_curstackinfo(%rip), %rcx
	movq	%rax, PL_tmps_floor(%rip)
	movq	48(%rcx), %rax
	testq	%rax, %rax
	jne	.LBB35_5
# BB#4:                                 # %if.then
	movq	$32, %rdi
	movq	$12, %rsi
	callq	Perl_new_stackinfo
	movq	PL_curstackinfo(%rip), %rcx
	movq	%rcx, 40(%rax)
	movq	PL_curstackinfo(%rip), %rcx
	movq	%rax, 48(%rcx)
.LBB35_5:                               # %if.end
	movq	$2, 32(%rax)
	movq	$-1, 16(%rax)
	movq	(%rax), %rcx
	movq	(%rcx), %rdx
	movq	$0, 8(%rdx)
	movq	PL_curstack(%rip), %rcx
	movq	PL_stack_base(%rip), %rsi
	movq	(%rcx), %rdi
	subq	%rsi, %rbx
	movq	$3, %rcx
	sarq	%cl, %rbx
	movq	%rbx, 8(%rdi)
	movq	(%rdx), %rcx
	movq	%rcx, PL_stack_base(%rip)
	movq	(%rax), %rdx
	movq	(%rdx), %rdx
	movq	16(%rdx), %rdx
	leaq	(%rcx,%rdx,8), %rdx
	movq	%rdx, PL_stack_max(%rip)
	movq	(%rax), %rsi
	movq	(%rsi), %rsi
	movq	8(%rsi), %rsi
	leaq	(%rcx,%rsi,8), %rbx
	movq	%rbx, PL_stack_sp(%rip)
	movq	(%rax), %rsi
	movq	%rsi, PL_curstack(%rip)
	movq	PL_markstack_ptr(%rip), %rsi
	movq	PL_markstack_max(%rip), %rdi
	movq	%rax, PL_curstackinfo(%rip)
	addq	$8, %rsi
	movq	%rsi, PL_markstack_ptr(%rip)
	cmpq	%rdi, %rsi
	jne	.LBB35_7
# BB#6:                                 # %if.then14
	callq	Perl_markstack_grow
	movq	PL_stack_base(%rip), %rcx
	movq	PL_markstack_ptr(%rip), %rsi
	movq	PL_stack_max(%rip), %rdx
.LBB35_7:                               # %if.end15
	movq	%rbx, %rax
	subq	%rcx, %rax
	movq	$3, %rcx
	sarq	%cl, %rax
	movq	%rax, (%rsi)
	subq	%rbx, %rdx
	cmpq	$15, %rdx
	jg	.LBB35_9
# BB#8:                                 # %if.then26
	movq	$2, %rdx
	movq	%rbx, %rdi
	movq	%rbx, %rsi
	callq	Perl_stack_grow
	movq	%rax, %rbx
.LBB35_9:                               # %if.end28
	movq	24(%r12), %rax
	testq	%rax, %rax
	jne	.LBB35_11
# BB#10:                                # %cond.false
	movq	%r15, %rdi
	callq	Perl_newRV
	movq	%rax, %rdi
	callq	Perl_sv_2mortal
.LBB35_11:                              # %cond.end
	movq	%rax, 8(%rbx)
	movzbl	18(%r13), %eax
	testq	$1807, %rax             # imm = 0x70F
	je	.LBB35_12
# BB#13:                                # %if.then38
	movq	%r13, 16(%rbx)
	addq	$16, %rbx
	jmp	.LBB35_14
.LBB35_12:
	addq	$8, %rbx
.LBB35_14:                              # %if.end40
	movq	%rbx, PL_stack_sp(%rip)
	xorq	%rsi, %rsi
	movq	%r14, %rdi
	callq	Perl_call_method
	testq	%rax, %rax
	je	.LBB35_16
# BB#15:                                # %if.then43
	movq	PL_stack_sp(%rip), %rax
	leaq	-8(%rax), %rcx
	movq	%rcx, PL_stack_sp(%rip)
	movq	(%rax), %rsi
	movq	$2, %rdx
	movq	%r13, %rdi
	callq	Perl_sv_setsv_flags
.LBB35_16:                              # %if.end45
	movq	PL_curstackinfo(%rip), %rax
	movq	40(%rax), %r14
	movq	PL_stack_sp(%rip), %rbx
	testq	%r14, %r14
	jne	.LBB35_23
# BB#17:                                # %if.then51
	movq	PL_stderrgv(%rip), %rax
	testq	%rax, %rax
	je	.LBB35_21
# BB#18:                                # %land.lhs.true
	movzbl	16(%rax), %ecx
	cmpq	$13, %rcx
	jne	.LBB35_21
# BB#19:                                # %land.lhs.true56
	movq	(%rax), %rax
	movq	56(%rax), %rax
	movq	16(%rax), %rax
	testq	%rax, %rax
	je	.LBB35_21
# BB#20:                                # %land.lhs.true59
	movq	(%rax), %rax
	movq	64(%rax), %rdi
	testq	%rdi, %rdi
	jne	.LBB35_22
.LBB35_21:                              # %cond.false71
	callq	Perl_PerlIO_stderr
	movq	%rax, %rdi
.LBB35_22:                              # %cond.end73
	movabsq	$.L.str24, %rsi
	xorq	%rax, %rax
	callq	PerlIO_printf
	movq	$1, %rdi
	callq	Perl_my_exit
.LBB35_23:                              # %if.end76
	movq	PL_curstack(%rip), %rax
	movq	PL_stack_base(%rip), %rcx
	movq	(%rax), %rax
	subq	%rcx, %rbx
	movq	$3, %rcx
	sarq	%cl, %rbx
	movq	%rbx, 8(%rax)
	movq	(%r14), %rax
	movq	(%rax), %rax
	movq	(%rax), %rax
	movq	%rax, PL_stack_base(%rip)
	movq	(%r14), %rcx
	movq	(%rcx), %rcx
	movq	16(%rcx), %rcx
	leaq	(%rax,%rcx,8), %rcx
	movq	%rcx, PL_stack_max(%rip)
	movq	(%r14), %rcx
	movq	(%rcx), %rcx
	movq	8(%rcx), %rcx
	leaq	(%rax,%rcx,8), %rax
	movq	%rax, PL_stack_sp(%rip)
	movq	(%r14), %rax
	movq	PL_tmps_ix(%rip), %rcx
	movq	PL_tmps_floor(%rip), %rdx
	movq	%rax, PL_curstack(%rip)
	movq	%r14, PL_curstackinfo(%rip)
	cmpq	%rdx, %rcx
	jle	.LBB35_25
# BB#24:                                # %if.then98
	callq	Perl_free_tmps
.LBB35_25:                              # %if.end99
	callq	Perl_pop_scope
	xorq	%rax, %rax
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp329:
	.size	Perl_magic_nextpack, .Ltmp329-Perl_magic_nextpack
	.cfi_endproc

	.globl	Perl_magic_existspack
	.align	16, 0x90
	.type	Perl_magic_existspack,@function
Perl_magic_existspack:                  # @Perl_magic_existspack
	.cfi_startproc
# BB#0:                                 # %entry
	movabsq	$.L.str30, %rdx
	xorq	%rax, %rax
	jmp	magic_methpack  # TAILCALL
.Ltmp330:
	.size	Perl_magic_existspack, .Ltmp330-Perl_magic_existspack
	.cfi_endproc

	.globl	Perl_magic_scalarpack
	.align	16, 0x90
	.type	Perl_magic_scalarpack,@function
Perl_magic_scalarpack:                  # @Perl_magic_scalarpack
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp336:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp337:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp338:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp339:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp340:
	.cfi_def_cfa_offset 48
.Ltmp341:
	.cfi_offset %rbx, -40
.Ltmp342:
	.cfi_offset %r12, -32
.Ltmp343:
	.cfi_offset %r14, -24
.Ltmp344:
	.cfi_offset %r15, -16
	movq	%rsi, %r12
	movq	24(%r12), %r14
	movq	PL_stack_sp(%rip), %rbx
	movq	%rdi, %r15
	testq	%r14, %r14
	jne	.LBB37_2
# BB#1:                                 # %cond.false
	movq	%r15, %rdi
	callq	Perl_newRV
	movq	%rax, %rdi
	callq	Perl_sv_2mortal
	movq	%rax, %r14
.LBB37_2:                               # %cond.end
	movq	(%r14), %rax
	movq	(%rax), %rax
	movq	(%rax), %rax
	movq	48(%rax), %rdi
	movabsq	$.L.str31, %rsi
	xorq	%rdx, %rdx
	callq	Perl_gv_fetchmethod_autoload
	testq	%rax, %rax
	je	.LBB37_3
# BB#8:                                 # %if.end16
	callq	Perl_push_scope
	movq	PL_curstackinfo(%rip), %rax
	movq	48(%rax), %rax
	testq	%rax, %rax
	jne	.LBB37_10
# BB#9:                                 # %if.then18
	movq	$32, %rdi
	movq	$12, %rsi
	callq	Perl_new_stackinfo
	movq	PL_curstackinfo(%rip), %rcx
	movq	%rcx, 40(%rax)
	movq	PL_curstackinfo(%rip), %rcx
	movq	%rax, 48(%rcx)
.LBB37_10:                              # %if.end21
	movq	$2, 32(%rax)
	movq	$-1, 16(%rax)
	movq	(%rax), %rcx
	movq	(%rcx), %rdx
	movq	$0, 8(%rdx)
	movq	PL_curstack(%rip), %rcx
	movq	PL_stack_base(%rip), %rsi
	movq	(%rcx), %rdi
	subq	%rsi, %rbx
	movq	$3, %rcx
	sarq	%cl, %rbx
	movq	%rbx, 8(%rdi)
	movq	(%rdx), %rcx
	movq	%rcx, PL_stack_base(%rip)
	movq	(%rax), %rdx
	movq	(%rdx), %rdx
	movq	16(%rdx), %rdx
	leaq	(%rcx,%rdx,8), %rdx
	movq	%rdx, PL_stack_max(%rip)
	movq	(%rax), %rsi
	movq	(%rsi), %rsi
	movq	8(%rsi), %rsi
	leaq	(%rcx,%rsi,8), %rbx
	movq	%rbx, PL_stack_sp(%rip)
	movq	(%rax), %rsi
	movq	%rsi, PL_curstack(%rip)
	movq	PL_markstack_ptr(%rip), %rsi
	movq	PL_markstack_max(%rip), %rdi
	movq	%rax, PL_curstackinfo(%rip)
	addq	$8, %rsi
	movq	%rsi, PL_markstack_ptr(%rip)
	cmpq	%rdi, %rsi
	jne	.LBB37_12
# BB#11:                                # %if.then34
	callq	Perl_markstack_grow
	movq	PL_stack_base(%rip), %rcx
	movq	PL_markstack_ptr(%rip), %rsi
	movq	PL_stack_max(%rip), %rdx
.LBB37_12:                              # %if.end35
	movq	%rbx, %rax
	subq	%rcx, %rax
	movq	$3, %rcx
	sarq	%cl, %rax
	movq	%rax, (%rsi)
	subq	%rbx, %rdx
	cmpq	$7, %rdx
	jg	.LBB37_14
# BB#13:                                # %if.then46
	movq	$1, %rdx
	movq	%rbx, %rdi
	movq	%rbx, %rsi
	callq	Perl_stack_grow
	movq	%rax, %rbx
.LBB37_14:                              # %if.end48
	movq	%r14, 8(%rbx)
	addq	$8, %rbx
	movq	%rbx, PL_stack_sp(%rip)
	movabsq	$.L.str31, %rdi
	xorq	%rsi, %rsi
	callq	Perl_call_method
	movq	PL_stack_sp(%rip), %rbx
	testq	%rax, %rax
	je	.LBB37_15
# BB#16:                                # %if.then52
	leaq	-8(%rbx), %rax
	movq	%rax, PL_stack_sp(%rip)
	movq	(%rbx), %r14
	movq	%rax, %rbx
	jmp	.LBB37_17
.LBB37_3:                               # %if.then
	movq	(%r15), %rax
	cmpq	$0, 64(%rax)
	je	.LBB37_5
# BB#4:
	movabsq	$PL_sv_yes, %r14
	jmp	.LBB37_25
.LBB37_15:
	movabsq	$PL_sv_undef, %r14
.LBB37_17:                              # %if.end54
	movq	PL_curstackinfo(%rip), %rax
	movq	40(%rax), %r15
	testq	%r15, %r15
	jne	.LBB37_24
# BB#18:                                # %if.then60
	movq	PL_stderrgv(%rip), %rax
	testq	%rax, %rax
	je	.LBB37_22
# BB#19:                                # %land.lhs.true
	movzbl	16(%rax), %ecx
	cmpq	$13, %rcx
	jne	.LBB37_22
# BB#20:                                # %land.lhs.true65
	movq	(%rax), %rax
	movq	56(%rax), %rax
	movq	16(%rax), %rax
	testq	%rax, %rax
	je	.LBB37_22
# BB#21:                                # %land.lhs.true68
	movq	(%rax), %rax
	movq	64(%rax), %rdi
	testq	%rdi, %rdi
	jne	.LBB37_23
.LBB37_22:                              # %cond.false80
	callq	Perl_PerlIO_stderr
	movq	%rax, %rdi
.LBB37_23:                              # %cond.end82
	movabsq	$.L.str24, %rsi
	xorq	%rax, %rax
	callq	PerlIO_printf
	movq	$1, %rdi
	callq	Perl_my_exit
.LBB37_24:                              # %if.end85
	movq	PL_curstack(%rip), %rax
	movq	PL_stack_base(%rip), %rcx
	movq	(%rax), %rax
	subq	%rcx, %rbx
	movq	$3, %rcx
	sarq	%cl, %rbx
	movq	%rbx, 8(%rax)
	movq	(%r15), %rax
	movq	(%rax), %rax
	movq	(%rax), %rax
	movq	%rax, PL_stack_base(%rip)
	movq	(%r15), %rcx
	movq	(%rcx), %rcx
	movq	16(%rcx), %rcx
	leaq	(%rax,%rcx,8), %rcx
	movq	%rcx, PL_stack_max(%rip)
	movq	(%r15), %rcx
	movq	(%rcx), %rcx
	movq	8(%rcx), %rcx
	leaq	(%rax,%rcx,8), %rax
	movq	%rax, PL_stack_sp(%rip)
	movq	(%r15), %rax
	movq	%rax, PL_curstack(%rip)
	movq	%r15, PL_curstackinfo(%rip)
	callq	Perl_pop_scope
.LBB37_25:                              # %return
	movq	%r14, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.LBB37_5:                               # %if.end
	callq	Perl_sv_newmortal
	movq	%rax, %r14
	xorq	%rax, %rax
	movq	%r15, %rdi
	movq	%r12, %rsi
	movq	%r14, %rdx
	callq	magic_nextpack
	movq	(%r15), %rax
	movq	$0, 64(%rax)
	movzbl	18(%r14), %eax
	testq	$1807, %rax             # imm = 0x70F
	jne	.LBB37_6
# BB#7:                                 # %if.end
	movabsq	$PL_sv_no, %r14
	jmp	.LBB37_25
.LBB37_6:
	movabsq	$PL_sv_yes, %r14
	jmp	.LBB37_25
.Ltmp345:
	.size	Perl_magic_scalarpack, .Ltmp345-Perl_magic_scalarpack
	.cfi_endproc

	.globl	Perl_magic_setdbline
	.align	16, 0x90
	.type	Perl_magic_setdbline,@function
Perl_magic_setdbline:                   # @Perl_magic_setdbline
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp351:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp352:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp353:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp354:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp355:
	.cfi_def_cfa_offset 48
.Ltmp356:
	.cfi_offset %rbx, -40
.Ltmp357:
	.cfi_offset %r12, -32
.Ltmp358:
	.cfi_offset %r14, -24
.Ltmp359:
	.cfi_offset %r15, -16
	movq	PL_DBline(%rip), %rbx
	movq	%rsi, %r15
	testq	%rdi, %rdi
	je	.LBB38_1
# BB#2:                                 # %cond.false
	movq	16(%rdi), %rax
	testq	$262144, %rax           # imm = 0x40000
	je	.LBB38_9
# BB#3:                                 # %cond.true2
	movq	(%rdi), %rax
	xorq	%r14, %r14
	testq	%rax, %rax
	movq	%rax, PL_Xpv(%rip)
	je	.LBB38_18
# BB#4:                                 # %land.rhs
	movq	8(%rax), %rcx
	cmpq	$1, %rcx
	movq	$1, %r14
	ja	.LBB38_18
# BB#5:                                 # %lor.rhs
	testq	%rcx, %rcx
	movabsq	$0, %r14
	je	.LBB38_18
# BB#6:                                 # %land.rhs6
	movq	(%rax), %rax
	movzbl	(%rax), %eax
	cmpq	$48, %rax
	je	.LBB38_7
# BB#8:                                 # %land.rhs6
	movabsq	$1, %r14
	jmp	.LBB38_18
.LBB38_1:
	xorq	%r14, %r14
.LBB38_18:                              # %cond.end33
	movq	(%rbx), %rax
	movq	56(%rax), %rax
	movq	40(%r15), %rcx
	movq	32(%rax), %rbx
	movq	%rcx, (%rsp)
	movabsq	$4294967295, %r12       # imm = 0xFFFFFFFF
	movq	32(%r15), %rdi
	andq	%r12, %rcx
	movabsq	$4294967294, %rax       # imm = 0xFFFFFFFE
	cmpq	%rax, %rcx
	jne	.LBB38_22
# BB#19:                                # %cond.true40
	movzbl	18(%rdi), %eax
	testq	$4, %rax
	je	.LBB38_21
# BB#20:                                # %cond.true45
	movq	(%rdi), %rax
	movq	8(%rax), %rcx
	movq	%rcx, (%rsp)
	movq	(%rax), %rdi
	jmp	.LBB38_22
.LBB38_9:                               # %cond.false10
	testq	$65536, %rax            # imm = 0x10000
	je	.LBB38_13
# BB#10:                                # %cond.true14
	movq	(%rdi), %rax
	cmpq	$0, 24(%rax)
	je	.LBB38_11
# BB#12:                                # %cond.true14
	movabsq	$1, %r14
	jmp	.LBB38_18
.LBB38_21:                              # %cond.false52
	leaq	(%rsp), %rsi
	movq	$2, %rdx
	callq	Perl_sv_2pv_flags
	movq	%rax, %rdi
.LBB38_22:                              # %cond.end59
	callq	atoi
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	xorq	%rdx, %rdx
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	Perl_av_fetch
	testq	%rax, %rax
	je	.LBB38_29
# BB#23:                                # %land.lhs.true
	movq	(%rax), %rax
	movzbl	19(%rax), %ecx
	testq	$1, %rcx
	je	.LBB38_29
# BB#24:                                # %land.lhs.true68
	movq	(%rax), %rax
	movq	24(%rax), %rax
	testq	%rax, %rax
	je	.LBB38_29
# BB#25:                                # %if.then
	movzbl	36(%rax), %ecx
	testq	%r12, %r14
	je	.LBB38_27
# BB#26:                                # %if.then73
	orq	$128, %rcx
	jmp	.LBB38_28
.LBB38_27:                              # %if.else
	andq	$127, %rcx
.LBB38_28:                              # %if.end80
	movb	%cl, 36(%rax)
.LBB38_29:                              # %if.end80
	xorq	%rax, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.LBB38_13:                              # %cond.false18
	testq	$131072, %rax           # imm = 0x20000
	je	.LBB38_17
# BB#14:                                # %cond.true22
	movq	(%rdi), %rax
	movq	32(%rax), %rdi
	xorq	%rsi, %rsi
	callq	__nedf2
	testq	%rax, %rax
	je	.LBB38_15
# BB#16:                                # %cond.true22
	movabsq	$1, %r14
	jmp	.LBB38_18
.LBB38_11:
	movabsq	$0, %r14
	jmp	.LBB38_18
.LBB38_7:
	movabsq	$0, %r14
	jmp	.LBB38_18
.LBB38_17:                              # %cond.false26
	callq	Perl_sv_2bool
	movq	%rax, %r14
	jmp	.LBB38_18
.LBB38_15:
	movabsq	$0, %r14
	jmp	.LBB38_18
.Ltmp360:
	.size	Perl_magic_setdbline, .Ltmp360-Perl_magic_setdbline
	.cfi_endproc

	.globl	Perl_magic_getarylen
	.align	16, 0x90
	.type	Perl_magic_getarylen,@function
Perl_magic_getarylen:                   # @Perl_magic_getarylen
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp363:
	.cfi_def_cfa_offset 16
.Ltmp364:
	.cfi_offset %rbx, -16
	movq	24(%rsi), %rax
	movzbl	17(%rax), %ecx
	movq	%rdi, %rbx
	testq	$128, %rcx
	je	.LBB39_2
# BB#1:                                 # %cond.true
	movq	%rax, %rdi
	callq	Perl_mg_size
	jmp	.LBB39_3
.LBB39_2:                               # %cond.false
	movq	(%rax), %rax
	movq	8(%rax), %rax
.LBB39_3:                               # %cond.end
	movq	PL_curcop(%rip), %rcx
	movq	72(%rcx), %rsi
	addq	%rax, %rsi
	movq	%rbx, %rdi
	callq	Perl_sv_setiv
	xorq	%rax, %rax
	popq	%rbx
	retq
.Ltmp365:
	.size	Perl_magic_getarylen, .Ltmp365-Perl_magic_getarylen
	.cfi_endproc

	.globl	Perl_magic_setarylen
	.align	16, 0x90
	.type	Perl_magic_setarylen,@function
Perl_magic_setarylen:                   # @Perl_magic_setarylen
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp368:
	.cfi_def_cfa_offset 16
.Ltmp369:
	.cfi_offset %rbx, -16
	movzbl	18(%rdi), %eax
	movq	24(%rsi), %rbx
	testq	$1, %rax
	je	.LBB40_2
# BB#1:                                 # %cond.true
	movq	(%rdi), %rax
	movq	24(%rax), %rsi
	jmp	.LBB40_3
.LBB40_2:                               # %cond.false
	callq	Perl_sv_2iv
	movq	%rax, %rsi
.LBB40_3:                               # %cond.end
	movq	PL_curcop(%rip), %rax
	movq	72(%rax), %rax
	subq	%rax, %rsi
	movq	%rbx, %rdi
	callq	Perl_av_fill
	xorq	%rax, %rax
	popq	%rbx
	retq
.Ltmp370:
	.size	Perl_magic_setarylen, .Ltmp370-Perl_magic_setarylen
	.cfi_endproc

	.globl	Perl_magic_getpos
	.align	16, 0x90
	.type	Perl_magic_getpos,@function
Perl_magic_getpos:                      # @Perl_magic_getpos
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp373:
	.cfi_def_cfa_offset 16
	subq	$16, %rsp
.Ltmp374:
	.cfi_def_cfa_offset 32
.Ltmp375:
	.cfi_offset %rbx, -16
	movq	%rdi, %rbx
	movq	(%rbx), %rax
	movq	72(%rax), %rdi
	movzbl	16(%rdi), %eax
	cmpq	$7, %rax
	jb	.LBB41_3
# BB#1:                                 # %land.lhs.true
	movq	(%rdi), %rax
	movq	40(%rax), %rax
	testq	%rax, %rax
	je	.LBB41_3
# BB#2:                                 # %land.lhs.true
	testq	%rdi, %rdi
	je	.LBB41_3
	.align	16, 0x90
.LBB41_6:                               # %for.body.i
                                        # =>This Inner Loop Header: Depth=1
	movzbl	18(%rax), %ecx
	cmpq	$103, %rcx
	je	.LBB41_7
# BB#5:                                 # %for.cond.i
                                        #   in Loop: Header=BB41_6 Depth=1
	movq	(%rax), %rax
	testq	%rax, %rax
	jne	.LBB41_6
	jmp	.LBB41_3
.LBB41_7:                               # %land.lhs.true3
	movq	40(%rax), %rax
	testq	%rax, %rax
	js	.LBB41_3
# BB#8:                                 # %if.then5
	movq	%rax, 8(%rsp)
	movzbl	19(%rdi), %eax
	testq	$32, %rax
	je	.LBB41_11
# BB#9:                                 # %land.lhs.true10
	movq	PL_curcop(%rip), %rax
	movzbl	37(%rax), %eax
	testq	$8, %rax
	jne	.LBB41_11
# BB#10:                                # %if.then13
	leaq	8(%rsp), %rsi
	callq	Perl_sv_pos_b2u
.LBB41_11:                              # %if.end
	movq	8(%rsp), %rax
	movq	PL_curcop(%rip), %rcx
	movq	72(%rcx), %rsi
	addq	%rax, %rsi
	movq	%rbx, %rdi
	callq	Perl_sv_setiv
	jmp	.LBB41_12
.LBB41_3:                               # %if.end15
	movq	16(%rbx), %rax
	movq	%rax, %rcx
	andq	$1223753727, %rcx       # imm = 0x48F0FFFF
	movq	%rcx, 16(%rbx)
	testq	$2097152, %rax          # imm = 0x200000
	je	.LBB41_12
# BB#4:                                 # %land.rhs
	movq	%rbx, %rdi
	callq	Perl_sv_backoff
.LBB41_12:                              # %return
	xorq	%rax, %rax
	addq	$16, %rsp
	popq	%rbx
	retq
.Ltmp376:
	.size	Perl_magic_getpos, .Ltmp376-Perl_magic_getpos
	.cfi_endproc

	.globl	Perl_magic_setpos
	.align	16, 0x90
	.type	Perl_magic_setpos,@function
Perl_magic_setpos:                      # @Perl_magic_setpos
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp383:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp384:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp385:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp386:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp387:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp388:
	.cfi_def_cfa_offset 64
.Ltmp389:
	.cfi_offset %rbx, -48
.Ltmp390:
	.cfi_offset %r12, -40
.Ltmp391:
	.cfi_offset %r13, -32
.Ltmp392:
	.cfi_offset %r14, -24
.Ltmp393:
	.cfi_offset %r15, -16
	movq	%rdi, %r14
	movq	(%r14), %rax
	movq	72(%rax), %r12
	movzbl	16(%r12), %eax
	cmpq	$7, %rax
	jb	.LBB42_3
# BB#1:                                 # %land.lhs.true
	movq	(%r12), %rax
	movq	40(%rax), %rbx
	testq	%rbx, %rbx
	je	.LBB42_3
# BB#2:                                 # %land.lhs.true
	testq	%r12, %r12
	je	.LBB42_3
	.align	16, 0x90
.LBB42_7:                               # %for.body.i
                                        # =>This Inner Loop Header: Depth=1
	movzbl	18(%rbx), %eax
	cmpq	$103, %rax
	je	.LBB42_8
# BB#6:                                 # %for.cond.i
                                        #   in Loop: Header=BB42_7 Depth=1
	movq	(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB42_7
.LBB42_3:                               # %if.then3
	movzbl	18(%r14), %eax
	testq	$1807, %rax             # imm = 0x70F
	je	.LBB42_32
# BB#4:                                 # %if.end8
	leaq	16(%r14), %r13
	xorq	%rsi, %rsi
	movq	$103, %rdx
	xorq	%rcx, %rcx
	xorq	%r8, %r8
	xorq	%rbx, %rbx
	movq	%r12, %rdi
	callq	Perl_sv_magic
	testq	%r12, %r12
	je	.LBB42_14
# BB#5:                                 # %if.end.i91
	movq	(%r12), %rax
	movq	40(%rax), %rax
	jmp	.LBB42_11
	.align	16, 0x90
.LBB42_10:                              # %for.cond.i95
                                        #   in Loop: Header=BB42_11 Depth=1
	movq	(%rax), %rax
.LBB42_11:                              # %for.cond.i95
                                        # =>This Inner Loop Header: Depth=1
	testq	%rax, %rax
	je	.LBB42_14
# BB#12:                                # %for.body.i100
                                        #   in Loop: Header=BB42_11 Depth=1
	movzbl	18(%rax), %ecx
	cmpq	$103, %rcx
	jne	.LBB42_10
# BB#13:
	movq	%rax, %rbx
	jmp	.LBB42_14
.LBB42_8:                               # %if.else
	movzbl	18(%r14), %eax
	testq	$1807, %rax             # imm = 0x70F
	je	.LBB42_33
# BB#9:
	leaq	16(%r14), %r13
.LBB42_14:                              # %if.end15
	movzbl	18(%r12), %eax
	testq	$4, %rax
	je	.LBB42_16
# BB#15:                                # %cond.true
	movq	(%r12), %rax
	movq	8(%rax), %r15
	jmp	.LBB42_17
.LBB42_16:                              # %cond.false
	movq	%r12, %rdi
	callq	Perl_sv_len
	movq	%rax, %r15
.LBB42_17:                              # %cond.end
	movzbl	2(%r13), %eax
	testq	$1, %rax
	je	.LBB42_19
# BB#18:                                # %cond.true24
	movq	(%r14), %rax
	movq	24(%rax), %r14
	jmp	.LBB42_20
.LBB42_19:                              # %cond.false26
	movq	%r14, %rdi
	callq	Perl_sv_2iv
	movq	%rax, %r14
.LBB42_20:                              # %cond.end28
	movq	PL_curcop(%rip), %rax
	movzbl	19(%r12), %ecx
	movq	72(%rax), %r13
	xorq	%rax, %rax
	testq	$32, %rcx
	je	.LBB42_24
# BB#21:                                # %land.lhs.true33
	movq	PL_curcop(%rip), %rcx
	movzbl	37(%rcx), %ecx
	testq	$8, %rcx
	jne	.LBB42_24
# BB#22:                                # %if.then36
	movq	%r12, %rdi
	callq	Perl_sv_len_utf8
	testq	%rax, %rax
	je	.LBB42_24
# BB#23:                                # %if.then36
	movq	%rax, %r15
.LBB42_24:                              # %if.end41
	subq	%r13, %r14
	js	.LBB42_25
# BB#26:                                # %if.else49
	cmpq	%r15, %r14
	jg	.LBB42_28
# BB#27:                                # %if.else49
	movq	%r14, %r15
	jmp	.LBB42_28
.LBB42_25:                              # %if.then44
	xorq	%rcx, %rcx
	addq	%r14, %r15
	js	.LBB42_29
.LBB42_28:                              # %if.else49
	movq	%r15, %rcx
.LBB42_29:                              # %if.end54
	testq	%rax, %rax
	je	.LBB42_31
# BB#30:                                # %if.then56
	movq	%rcx, 8(%rsp)
	leaq	8(%rsp), %rsi
	xorq	%rdx, %rdx
	movq	%r12, %rdi
	callq	Perl_sv_pos_u2b
	movq	8(%rsp), %rcx
.LBB42_31:                              # %if.end57
	movq	%rcx, 40(%rbx)
	movzbl	19(%rbx), %eax
	andq	$254, %rax
	movb	%al, 19(%rbx)
.LBB42_32:                              # %return
	xorq	%rax, %rax
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB42_33:                              # %if.then13
	movq	$-1, 40(%rbx)
	jmp	.LBB42_32
.Ltmp394:
	.size	Perl_magic_setpos, .Ltmp394-Perl_magic_setpos
	.cfi_endproc

	.globl	Perl_magic_getglob
	.align	16, 0x90
	.type	Perl_magic_getglob,@function
Perl_magic_getglob:                     # @Perl_magic_getglob
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp397:
	.cfi_def_cfa_offset 16
.Ltmp398:
	.cfi_offset %rbx, -16
	movq	%rdi, %rbx
	movq	16(%rbx), %rax
	testq	$1048576, %rax          # imm = 0x100000
	je	.LBB43_2
# BB#1:                                 # %if.then
	andq	$-1048577, %rax         # imm = 0xFFFFFFFFFFEFFFFF
	movq	%rax, 16(%rbx)
	movabsq	$.L.str32, %rdx
	movq	$1, %rcx
	movq	%rbx, %rdi
	movq	%rbx, %rsi
	callq	Perl_gv_efullname4
	orq	$1048576, 16(%rbx)      # imm = 0x100000
	jmp	.LBB43_3
.LBB43_2:                               # %if.else
	movabsq	$.L.str32, %rdx
	movq	$1, %rcx
	movq	%rbx, %rdi
	movq	%rbx, %rsi
	callq	Perl_gv_efullname4
.LBB43_3:                               # %if.end
	xorq	%rax, %rax
	popq	%rbx
	retq
.Ltmp399:
	.size	Perl_magic_getglob, .Ltmp399-Perl_magic_getglob
	.cfi_endproc

	.globl	Perl_magic_setglob
	.align	16, 0x90
	.type	Perl_magic_setglob,@function
Perl_magic_setglob:                     # @Perl_magic_setglob
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp403:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp404:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp405:
	.cfi_def_cfa_offset 32
.Ltmp406:
	.cfi_offset %rbx, -24
.Ltmp407:
	.cfi_offset %r14, -16
	movq	%rdi, %r14
	movq	16(%r14), %rax
	testq	$118423552, %rax        # imm = 0x70F0000
	je	.LBB44_11
# BB#1:                                 # %if.end
	testq	$262144, %rax           # imm = 0x40000
	je	.LBB44_3
# BB#2:                                 # %cond.true
	movq	(%r14), %rax
	movq	8(%rax), %rcx
	movq	%rcx, (%rsp)
	movq	(%rax), %rdi
	jmp	.LBB44_4
.LBB44_3:                               # %cond.false
	leaq	(%rsp), %rsi
	movq	$2, %rdx
	movq	%r14, %rdi
	callq	Perl_sv_2pv_flags
	movq	%rax, %rdi
.LBB44_4:                               # %cond.end
	movzbl	(%rdi), %eax
	cmpq	$42, %rax
	jne	.LBB44_7
# BB#5:                                 # %land.lhs.true
	movzbl	1(%rdi), %eax
	testq	%rax, %rax
	je	.LBB44_7
# BB#6:                                 # %select.mid
	incq	%rdi
.LBB44_7:                               # %if.end9
	movq	$1, %rsi
	movq	$13, %rdx
	callq	Perl_gv_fetchpv
	movq	%rax, %rbx
	cmpq	%r14, %rbx
	je	.LBB44_11
# BB#8:                                 # %if.end14
	movq	(%r14), %rax
	cmpq	$0, 56(%rax)
	je	.LBB44_10
# BB#9:                                 # %if.then17
	movq	%r14, %rdi
	callq	Perl_gp_free
.LBB44_10:                              # %if.end18
	movq	(%rbx), %rax
	movq	56(%rax), %rdi
	callq	Perl_gp_ref
	movq	(%r14), %rcx
	movq	%rax, 56(%rcx)
.LBB44_11:                              # %return
	xorq	%rax, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp408:
	.size	Perl_magic_setglob, .Ltmp408-Perl_magic_setglob
	.cfi_endproc

	.globl	Perl_magic_getsubstr
	.align	16, 0x90
	.type	Perl_magic_getsubstr,@function
Perl_magic_getsubstr:                   # @Perl_magic_getsubstr
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp413:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp414:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp415:
	.cfi_def_cfa_offset 32
	subq	$32, %rsp
.Ltmp416:
	.cfi_def_cfa_offset 64
.Ltmp417:
	.cfi_offset %rbx, -32
.Ltmp418:
	.cfi_offset %r14, -24
.Ltmp419:
	.cfi_offset %r15, -16
	movq	%rdi, %r14
	movq	(%r14), %rcx
	movq	72(%rcx), %rbx
	movzbl	18(%rbx), %eax
	testq	$4, %rax
	je	.LBB45_2
# BB#1:                                 # %cond.true
	movq	(%rbx), %rax
	movq	8(%rax), %rdx
	movq	%rdx, 24(%rsp)
	movq	(%rax), %r15
	jmp	.LBB45_3
.LBB45_2:                               # %cond.false
	leaq	24(%rsp), %rsi
	movq	$2, %rdx
	movq	%rbx, %rdi
	callq	Perl_sv_2pv_flags
	movq	(%r14), %rcx
	movq	%rax, %r15
.LBB45_3:                               # %cond.end
	movq	56(%rcx), %rax
	movq	%rax, 16(%rsp)
	movq	64(%rcx), %rcx
	movq	%rcx, 8(%rsp)
	movzbl	19(%rbx), %ecx
	testq	$32, %rcx
	je	.LBB45_5
# BB#4:                                 # %if.then
	leaq	16(%rsp), %rsi
	leaq	8(%rsp), %rdx
	movq	%rbx, %rdi
	callq	Perl_sv_pos_u2b
	movq	16(%rsp), %rax
.LBB45_5:                               # %if.end
	movq	24(%rsp), %rcx
	cmpq	%rcx, %rax
	jle	.LBB45_7
# BB#6:                                 # %if.then8
	movq	%rcx, 16(%rsp)
	movq	%rcx, %rax
.LBB45_7:                               # %if.end9
	movq	8(%rsp), %rdx
	leaq	(%rax,%rdx), %rsi
	cmpq	%rcx, %rsi
	jle	.LBB45_9
# BB#8:                                 # %if.then11
	subq	%rax, %rcx
	movq	%rcx, 8(%rsp)
	movq	%rcx, %rdx
.LBB45_9:                               # %if.end12
	addq	%rax, %r15
	movq	%r14, %rdi
	movq	%r15, %rsi
	callq	Perl_sv_setpvn
	movzbl	19(%rbx), %eax
	testq	$32, %rax
	je	.LBB45_11
# BB#10:                                # %if.then16
	orq	$536870912, 16(%r14)    # imm = 0x20000000
.LBB45_11:                              # %if.end18
	xorq	%rax, %rax
	addq	$32, %rsp
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp420:
	.size	Perl_magic_getsubstr, .Ltmp420-Perl_magic_getsubstr
	.cfi_endproc

	.globl	Perl_magic_setsubstr
	.align	16, 0x90
	.type	Perl_magic_setsubstr,@function
Perl_magic_setsubstr:                   # @Perl_magic_setsubstr
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp424:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp425:
	.cfi_def_cfa_offset 24
	subq	$24, %rsp
.Ltmp426:
	.cfi_def_cfa_offset 48
.Ltmp427:
	.cfi_offset %rbx, -24
.Ltmp428:
	.cfi_offset %r14, -16
	movq	%rdi, %rbx
	movq	16(%rbx), %rcx
	testq	$262144, %rcx           # imm = 0x40000
	je	.LBB46_2
# BB#1:                                 # %cond.true
	movq	(%rbx), %rdx
	movq	8(%rdx), %rax
	movq	%rax, 16(%rsp)
	movq	(%rdx), %r14
	jmp	.LBB46_3
.LBB46_2:                               # %cond.false
	leaq	16(%rsp), %rsi
	movq	$2, %rdx
	movq	%rbx, %rdi
	callq	Perl_sv_2pv_flags
	movq	(%rbx), %rdx
	movq	16(%rbx), %rcx
	movq	%rax, %r14
.LBB46_3:                               # %cond.end
	movq	56(%rdx), %rax
	movq	72(%rdx), %rbx
	movq	%rax, 8(%rsp)
	movq	64(%rdx), %rax
	movq	%rax, (%rsp)
	testq	$536870912, %rcx        # imm = 0x20000000
	je	.LBB46_6
# BB#4:                                 # %land.lhs.true
	movq	PL_curcop(%rip), %rax
	movzbl	37(%rax), %eax
	testq	$8, %rax
	je	.LBB46_5
.LBB46_6:                               # %if.else
	testq	%rbx, %rbx
	je	.LBB46_9
# BB#7:                                 # %land.lhs.true12
	movzbl	19(%rbx), %eax
	testq	$32, %rax
	je	.LBB46_9
# BB#8:                                 # %if.then16
	leaq	8(%rsp), %rsi
	leaq	(%rsp), %rdx
	movq	%rbx, %rdi
	callq	Perl_sv_pos_u2b
	leaq	16(%rsp), %rsi
	movq	%r14, %rdi
	callq	Perl_bytes_to_utf8
	movq	8(%rsp), %rsi
	movq	(%rsp), %rdx
	movq	16(%rsp), %r8
	movq	%rax, %r14
	movq	%rbx, %rdi
	movq	%r14, %rcx
	callq	Perl_sv_insert
	movq	%r14, %rdi
	callq	Perl_safesysfree
	jmp	.LBB46_10
.LBB46_9:                               # %if.else18
	movq	8(%rsp), %rsi
	movq	(%rsp), %rdx
	movq	16(%rsp), %r8
	movq	%rbx, %rdi
	movq	%r14, %rcx
	callq	Perl_sv_insert
	jmp	.LBB46_10
.LBB46_5:                               # %if.then
	movq	$2, %rsi
	movq	%rbx, %rdi
	callq	Perl_sv_utf8_upgrade_flags
	leaq	8(%rsp), %rsi
	leaq	(%rsp), %rdx
	movq	%rbx, %rdi
	callq	Perl_sv_pos_u2b
	movq	8(%rsp), %rsi
	movq	(%rsp), %rdx
	movq	16(%rsp), %r8
	movq	%rbx, %rdi
	movq	%r14, %rcx
	callq	Perl_sv_insert
	orq	$536870912, 16(%rbx)    # imm = 0x20000000
.LBB46_10:                              # %if.end19
	xorq	%rax, %rax
	addq	$24, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp429:
	.size	Perl_magic_setsubstr, .Ltmp429-Perl_magic_setsubstr
	.cfi_endproc

	.globl	Perl_magic_gettaint
	.align	16, 0x90
	.type	Perl_magic_gettaint,@function
Perl_magic_gettaint:                    # @Perl_magic_gettaint
	.cfi_startproc
# BB#0:                                 # %entry
	movq	40(%rsi), %rax
	testq	$1, %rax
	jne	.LBB47_3
# BB#1:                                 # %lor.lhs.false
	testq	$2, %rax
	je	.LBB47_4
# BB#2:                                 # %land.lhs.true
	movq	24(%rsi), %rax
	cmpq	%rdi, %rax
	jne	.LBB47_4
.LBB47_3:                               # %if.then
	movq	$1, %rax
	movb	%al, PL_tainted(%rip)
.LBB47_4:                               # %if.end
	xorq	%rax, %rax
	retq
.Ltmp430:
	.size	Perl_magic_gettaint, .Ltmp430-Perl_magic_gettaint
	.cfi_endproc

	.globl	Perl_magic_settaint
	.align	16, 0x90
	.type	Perl_magic_settaint,@function
Perl_magic_settaint:                    # @Perl_magic_settaint
	.cfi_startproc
# BB#0:                                 # %entry
	movl	PL_localizing(%rip), %ecx
	testq	%rcx, %rcx
	je	.LBB48_4
# BB#1:                                 # %if.then
	movq	40(%rsi), %rax
	cmpq	$1, %rcx
	jne	.LBB48_3
# BB#2:                                 # %if.then1
	addq	%rax, %rax
	jmp	.LBB48_7
.LBB48_4:                               # %if.else3
	movzbl	PL_tainted(%rip), %ecx
	movq	40(%rsi), %rax
	testq	%rcx, %rcx
	je	.LBB48_6
# BB#5:                                 # %if.then5
	orq	$1, %rax
	jmp	.LBB48_7
.LBB48_3:                               # %if.else
	movq	$1, %rcx
	sarq	%cl, %rax
	jmp	.LBB48_7
.LBB48_6:                               # %if.else7
	andq	$-2, %rax
.LBB48_7:                               # %if.end10
	movq	%rax, 40(%rsi)
	xorq	%rax, %rax
	retq
.Ltmp431:
	.size	Perl_magic_settaint, .Ltmp431-Perl_magic_settaint
	.cfi_endproc

	.globl	Perl_magic_getvec
	.align	16, 0x90
	.type	Perl_magic_getvec,@function
Perl_magic_getvec:                      # @Perl_magic_getvec
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp434:
	.cfi_def_cfa_offset 16
.Ltmp435:
	.cfi_offset %rbx, -16
	movq	%rdi, %rbx
	movq	(%rbx), %rax
	movq	72(%rax), %rdi
	testq	%rdi, %rdi
	je	.LBB49_1
# BB#3:                                 # %if.end
	movq	56(%rax), %rsi
	movq	64(%rax), %rdx
	xorq	%rax, %rax
	callq	do_vecget
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	Perl_sv_setuv
	jmp	.LBB49_4
.LBB49_1:                               # %if.then
	movq	16(%rbx), %rax
	movq	%rax, %rcx
	andq	$1223753727, %rcx       # imm = 0x48F0FFFF
	movq	%rcx, 16(%rbx)
	testq	$2097152, %rax          # imm = 0x200000
	je	.LBB49_4
# BB#2:                                 # %land.rhs
	movq	%rbx, %rdi
	callq	Perl_sv_backoff
.LBB49_4:                               # %return
	xorq	%rax, %rax
	popq	%rbx
	retq
.Ltmp436:
	.size	Perl_magic_getvec, .Ltmp436-Perl_magic_getvec
	.cfi_endproc

	.globl	Perl_magic_setvec
	.align	16, 0x90
	.type	Perl_magic_setvec,@function
Perl_magic_setvec:                      # @Perl_magic_setvec
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rax
.Ltmp438:
	.cfi_def_cfa_offset 16
	xorq	%rax, %rax
	callq	do_vecset
	xorq	%rax, %rax
	popq	%rdx
	retq
.Ltmp439:
	.size	Perl_magic_setvec, .Ltmp439-Perl_magic_setvec
	.cfi_endproc

	.globl	Perl_magic_getdefelem
	.align	16, 0x90
	.type	Perl_magic_getdefelem,@function
Perl_magic_getdefelem:                  # @Perl_magic_getdefelem
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp446:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp447:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp448:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp449:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp450:
	.cfi_def_cfa_offset 48
.Ltmp451:
	.cfi_offset %rbx, -48
.Ltmp452:
	.cfi_offset %r12, -40
.Ltmp453:
	.cfi_offset %r13, -32
.Ltmp454:
	.cfi_offset %r14, -24
.Ltmp455:
	.cfi_offset %r15, -16
	movq	%rdi, %r15
	movq	(%r15), %rax
	cmpq	$0, 64(%rax)
	movq	%rsi, %r14
	je	.LBB51_14
# BB#1:                                 # %if.then
	movq	24(%r14), %rsi
	movq	72(%rax), %r12
	testq	%rsi, %rsi
	je	.LBB51_6
# BB#2:                                 # %if.then2
	movzbl	16(%r12), %eax
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	xorq	%rbx, %rbx
	movq	%r12, %rdi
	cmpq	$11, %rax
	jne	.LBB51_5
# BB#3:                                 # %if.then4
	callq	Perl_hv_fetch_ent
	testq	%rax, %rax
	je	.LBB51_15
# BB#4:                                 # %if.then7
	addq	$16, %rax
	jmp	.LBB51_11
.LBB51_14:                              # %if.else46
	movq	72(%rax), %rbx
	jmp	.LBB51_15
.LBB51_6:                               # %if.else14
	movzbl	17(%r12), %ecx
	movq	56(%rax), %r13
	testq	$128, %rcx
	je	.LBB51_8
# BB#7:                                 # %cond.true
	movq	%r12, %rdi
	callq	Perl_mg_size
	jmp	.LBB51_9
.LBB51_5:                               # %if.else
	callq	Perl_avhv_fetch_ent
	testq	%rax, %rax
	jne	.LBB51_11
	jmp	.LBB51_15
.LBB51_8:                               # %cond.false
	movq	(%r12), %rax
	movq	8(%rax), %rax
.LBB51_9:                               # %cond.end
	xorq	%rbx, %rbx
	cmpq	%rax, %r13
	jg	.LBB51_15
# BB#10:                                # %if.then24
	movq	(%r15), %rax
	movq	(%r12), %rcx
	movq	56(%rax), %rax
	movq	(%rcx), %rcx
	leaq	(%rcx,%rax,8), %rax
.LBB51_11:                              # %if.end29
	movq	(%rax), %rbx
	testq	%rbx, %rbx
	je	.LBB51_15
# BB#12:                                # %if.end29
	movabsq	$PL_sv_undef, %rax
	cmpq	%rax, %rbx
	je	.LBB51_15
# BB#13:                                # %land.end
	movq	(%r15), %rax
	movq	72(%rax), %rdi
	callq	Perl_sv_free
	movq	%rbx, PL_Sv(%rip)
	incq	8(%rbx)
	movq	(%r15), %rax
	movq	%rbx, 72(%rax)
	movq	$0, 64(%rax)
	movq	24(%r14), %rdi
	callq	Perl_sv_free
	movq	$0, 24(%r14)
	movzbl	19(%r14), %eax
	andq	$253, %rax
	movb	%al, 19(%r14)
.LBB51_15:                              # %if.end49
	testq	%rbx, %rbx
	jne	.LBB51_17
# BB#16:                                # %if.end49
	movabsq	$PL_sv_undef, %rbx
.LBB51_17:                              # %if.end49
	movq	$2, %rdx
	movq	%r15, %rdi
	movq	%rbx, %rsi
	callq	Perl_sv_setsv_flags
	xorq	%rax, %rax
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp456:
	.size	Perl_magic_getdefelem, .Ltmp456-Perl_magic_getdefelem
	.cfi_endproc

	.globl	Perl_magic_setdefelem
	.align	16, 0x90
	.type	Perl_magic_setdefelem,@function
Perl_magic_setdefelem:                  # @Perl_magic_setdefelem
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp459:
	.cfi_def_cfa_offset 16
.Ltmp460:
	.cfi_offset %rbx, -16
	movq	%rdi, %rbx
	movq	(%rbx), %rax
	cmpq	$0, 64(%rax)
	je	.LBB52_2
# BB#1:                                 # %if.then
	xorq	%rax, %rax
	movq	%rbx, %rdi
	callq	vivify_defelem
	movq	(%rbx), %rax
.LBB52_2:                               # %if.end
	movq	72(%rax), %rdi
	testq	%rdi, %rdi
	je	.LBB52_5
# BB#3:                                 # %if.then3
	movq	$2, %rdx
	movq	%rbx, %rsi
	callq	Perl_sv_setsv_flags
	movq	(%rbx), %rax
	movq	72(%rax), %rdi
	movzbl	17(%rdi), %eax
	testq	$64, %rax
	je	.LBB52_5
# BB#4:                                 # %if.then9
	callq	Perl_mg_set
.LBB52_5:                               # %if.end14
	xorq	%rax, %rax
	popq	%rbx
	retq
.Ltmp461:
	.size	Perl_magic_setdefelem, .Ltmp461-Perl_magic_setdefelem
	.cfi_endproc

	.globl	Perl_vivify_defelem
	.align	16, 0x90
	.type	Perl_vivify_defelem,@function
Perl_vivify_defelem:                    # @Perl_vivify_defelem
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp467:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp468:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp469:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp470:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp471:
	.cfi_def_cfa_offset 48
.Ltmp472:
	.cfi_offset %rbx, -40
.Ltmp473:
	.cfi_offset %r12, -32
.Ltmp474:
	.cfi_offset %r14, -24
.Ltmp475:
	.cfi_offset %r15, -16
	movq	%rdi, %r15
	movq	(%r15), %rcx
	movq	64(%rcx), %rax
	testq	%rax, %rax
	je	.LBB53_28
# BB#1:                                 # %if.end.i
	movq	40(%rcx), %rbx
	jmp	.LBB53_3
	.align	16, 0x90
.LBB53_2:                               # %for.cond.i
                                        #   in Loop: Header=BB53_3 Depth=1
	movq	(%rbx), %rbx
.LBB53_3:                               # %for.cond.i
                                        # =>This Inner Loop Header: Depth=1
	testq	%rbx, %rbx
	je	.LBB53_28
# BB#4:                                 # %for.body.i
                                        #   in Loop: Header=BB53_3 Depth=1
	movzbl	18(%rbx), %edx
	cmpq	$121, %rdx
	jne	.LBB53_2
# BB#5:                                 # %if.end
	movq	24(%rbx), %rsi
	movq	72(%rcx), %r14
	testq	%rsi, %rsi
	je	.LBB53_16
# BB#6:                                 # %if.then3
	movzbl	16(%r14), %eax
	movq	$1, %rdx
	xorq	%rcx, %rcx
	xorq	%r12, %r12
	movq	%r14, %rdi
	cmpq	$11, %rax
	jne	.LBB53_9
# BB#7:                                 # %if.then5
	callq	Perl_hv_fetch_ent
	testq	%rax, %rax
	je	.LBB53_12
# BB#8:                                 # %if.then9
	addq	$16, %rax
	jmp	.LBB53_10
.LBB53_16:                              # %if.else32
	testq	%rax, %rax
	jns	.LBB53_22
# BB#17:                                # %land.lhs.true
	movzbl	17(%r14), %eax
	movq	56(%rcx), %r12
	testq	$128, %rax
	je	.LBB53_19
# BB#18:                                # %cond.true42
	movq	%r14, %rdi
	callq	Perl_mg_size
	jmp	.LBB53_20
.LBB53_9:                               # %if.else
	callq	Perl_avhv_fetch_ent
	testq	%rax, %rax
	je	.LBB53_12
.LBB53_10:                              # %if.end16
	movq	(%rax), %r12
	testq	%r12, %r12
	je	.LBB53_12
# BB#11:                                # %if.end16
	movabsq	$PL_sv_undef, %rax
	cmpq	%rax, %r12
	jne	.LBB53_25
.LBB53_12:                              # %if.then20
	movq	24(%rbx), %rdi
	movzbl	18(%rdi), %eax
	testq	$4, %rax
	je	.LBB53_14
# BB#13:                                # %cond.true
	movq	(%rdi), %rax
	movq	8(%rax), %rcx
	movq	%rcx, (%rsp)
	movq	(%rax), %rsi
	jmp	.LBB53_15
.LBB53_14:                              # %cond.false
	leaq	(%rsp), %rsi
	movq	$2, %rdx
	callq	Perl_sv_2pv_flags
	movq	%rax, %rsi
.LBB53_15:                              # %cond.end
	movabsq	$PL_no_helem, %rdi
	xorq	%rax, %rax
	callq	Perl_croak
	jmp	.LBB53_25
.LBB53_19:                              # %cond.false44
	movq	(%r14), %rax
	movq	8(%rax), %rax
.LBB53_20:                              # %cond.end46
	movq	(%r15), %rcx
	cmpq	%rax, %r12
	jle	.LBB53_22
# BB#21:                                # %if.end65.thread
	movq	$0, 72(%rcx)
	movq	$0, PL_Sv(%rip)
	xorq	%r14, %r14
	jmp	.LBB53_27
.LBB53_22:                              # %if.else52
	movq	56(%rcx), %rsi
	movq	$1, %rdx
	movq	%r14, %rdi
	callq	Perl_av_fetch
	xorq	%r14, %r14
	testq	%rax, %rax
	je	.LBB53_24
# BB#23:                                # %lor.lhs.false58
	movq	(%rax), %r12
	movabsq	$PL_sv_undef, %r14
	cmpq	%r14, %r12
	jne	.LBB53_25
.LBB53_24:                              # %if.then60
	movq	(%r15), %rax
	movq	56(%rax), %rsi
	movabsq	$PL_no_aelem, %rdi
	xorq	%rax, %rax
	callq	Perl_croak
	movq	%r14, %r12
.LBB53_25:                              # %if.end65
	movq	%r12, PL_Sv(%rip)
	xorq	%r14, %r14
	testq	%r12, %r12
	je	.LBB53_27
# BB#26:                                # %land.rhs
	incq	8(%r12)
	movq	%r12, %r14
.LBB53_27:                              # %land.end
	movq	(%r15), %rax
	movq	72(%rax), %rdi
	callq	Perl_sv_free
	movq	(%r15), %rax
	movq	%r14, 72(%rax)
	movq	$0, 64(%rax)
	movq	24(%rbx), %rdi
	callq	Perl_sv_free
	movq	$0, 24(%rbx)
	movzbl	19(%rbx), %eax
	andq	$253, %rax
	movb	%al, 19(%rbx)
.LBB53_28:                              # %return
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.Ltmp476:
	.size	Perl_vivify_defelem, .Ltmp476-Perl_vivify_defelem
	.cfi_endproc

	.globl	Perl_magic_killbackrefs
	.align	16, 0x90
	.type	Perl_magic_killbackrefs,@function
Perl_magic_killbackrefs:                # @Perl_magic_killbackrefs
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp483:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp484:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp485:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp486:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp487:
	.cfi_def_cfa_offset 48
.Ltmp488:
	.cfi_offset %rbx, -48
.Ltmp489:
	.cfi_offset %r12, -40
.Ltmp490:
	.cfi_offset %r13, -32
.Ltmp491:
	.cfi_offset %r14, -24
.Ltmp492:
	.cfi_offset %r15, -16
	movq	24(%rsi), %r14
	movq	(%r14), %rax
	movq	8(%rax), %rbx
	testq	%rbx, %rbx
	js	.LBB54_9
# BB#1:                                 # %while.body.preheader
	movq	(%rax), %r12
	incq	%rbx
	movabsq	$2148007936, %r13       # imm = 0x80080000
	movabsq	$.L.str33, %r15
	.align	16, 0x90
.LBB54_2:                               # %while.body
                                        # =>This Inner Loop Header: Depth=1
	movq	-8(%r12,%rbx,8), %rax
	testq	%rax, %rax
	je	.LBB54_8
# BB#3:                                 # %if.then
                                        #   in Loop: Header=BB54_2 Depth=1
	movq	16(%rax), %rcx
	andq	%r13, %rcx
	cmpq	%r13, %rcx
	je	.LBB54_5
# BB#4:                                 # %if.then4
                                        #   in Loop: Header=BB54_2 Depth=1
	xorq	%rax, %rax
	movq	%r15, %rdi
	callq	Perl_croak
	movq	-8(%r12,%rbx,8), %rax
.LBB54_5:                               # %if.end
                                        #   in Loop: Header=BB54_2 Depth=1
	movq	(%rax), %rax
	movq	$0, (%rax)
	movq	-8(%r12,%rbx,8), %rdi
	movq	16(%rdi), %rcx
	movq	%rcx, %rax
	andq	$1223753727, %rax       # imm = 0x48F0FFFF
	movq	%rax, 16(%rdi)
	testq	$2097152, %rcx          # imm = 0x200000
	je	.LBB54_7
# BB#6:                                 # %land.rhs
                                        #   in Loop: Header=BB54_2 Depth=1
	callq	Perl_sv_backoff
	movq	-8(%r12,%rbx,8), %rdi
	movq	16(%rdi), %rax
.LBB54_7:                               # %land.end
                                        #   in Loop: Header=BB54_2 Depth=1
	andq	$2146959359, %rax       # imm = 0x7FF7FFFF
	movq	%rax, 16(%rdi)
	movq	$0, -8(%r12,%rbx,8)
.LBB54_8:                               # %if.end20
                                        #   in Loop: Header=BB54_2 Depth=1
	decq	%rbx
	testq	%rbx, %rbx
	jg	.LBB54_2
.LBB54_9:                               # %while.end
	movq	%r14, %rdi
	callq	Perl_sv_free
	xorq	%rax, %rax
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp493:
	.size	Perl_magic_killbackrefs, .Ltmp493-Perl_magic_killbackrefs
	.cfi_endproc

	.globl	Perl_magic_setmglob
	.align	16, 0x90
	.type	Perl_magic_setmglob,@function
Perl_magic_setmglob:                    # @Perl_magic_setmglob
	.cfi_startproc
# BB#0:                                 # %entry
	movq	$-1, 40(%rsi)
	andq	$-134217729, 16(%rdi)   # imm = 0xFFFFFFFFF7FFFFFF
	xorq	%rax, %rax
	retq
.Ltmp494:
	.size	Perl_magic_setmglob, .Ltmp494-Perl_magic_setmglob
	.cfi_endproc

	.globl	Perl_magic_setbm
	.align	16, 0x90
	.type	Perl_magic_setbm,@function
Perl_magic_setbm:                       # @Perl_magic_setbm
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp497:
	.cfi_def_cfa_offset 16
.Ltmp498:
	.cfi_offset %rbx, -16
	movq	%rdi, %rbx
	movq	$66, %rsi
                                        # kill: RDI<def> RBX<kill>
	callq	Perl_sv_unmagic
	andq	$2147483647, 16(%rbx)   # imm = 0x7FFFFFFF
	xorq	%rax, %rax
	popq	%rbx
	retq
.Ltmp499:
	.size	Perl_magic_setbm, .Ltmp499-Perl_magic_setbm
	.cfi_endproc

	.globl	Perl_magic_setfm
	.align	16, 0x90
	.type	Perl_magic_setfm,@function
Perl_magic_setfm:                       # @Perl_magic_setfm
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp502:
	.cfi_def_cfa_offset 16
.Ltmp503:
	.cfi_offset %rbx, -16
	movq	%rdi, %rbx
	movq	$102, %rsi
                                        # kill: RDI<def> RBX<kill>
	callq	Perl_sv_unmagic
	andq	$2147483647, 16(%rbx)   # imm = 0x7FFFFFFF
	xorq	%rax, %rax
	popq	%rbx
	retq
.Ltmp504:
	.size	Perl_magic_setfm, .Ltmp504-Perl_magic_setfm
	.cfi_endproc

	.globl	Perl_magic_setuvar
	.align	16, 0x90
	.type	Perl_magic_setuvar,@function
Perl_magic_setuvar:                     # @Perl_magic_setuvar
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rax
.Ltmp506:
	.cfi_def_cfa_offset 16
	movq	32(%rsi), %rcx
	movq	%rdi, %rax
	testq	%rcx, %rcx
	je	.LBB58_3
# BB#1:                                 # %land.lhs.true
	movq	8(%rcx), %rdx
	testq	%rdx, %rdx
	je	.LBB58_3
# BB#2:                                 # %if.then
	movq	16(%rcx), %rdi
	movq	%rax, %rsi
	callq	*%rdx
.LBB58_3:                               # %if.end
	xorq	%rax, %rax
	popq	%rdx
	retq
.Ltmp507:
	.size	Perl_magic_setuvar, .Ltmp507-Perl_magic_setuvar
	.cfi_endproc

	.globl	Perl_magic_setregexp
	.align	16, 0x90
	.type	Perl_magic_setregexp,@function
Perl_magic_setregexp:                   # @Perl_magic_setregexp
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rax
.Ltmp509:
	.cfi_def_cfa_offset 16
	movq	$114, %rsi
	callq	Perl_sv_unmagic
	xorq	%rax, %rax
	popq	%rdx
	retq
.Ltmp510:
	.size	Perl_magic_setregexp, .Ltmp510-Perl_magic_setregexp
	.cfi_endproc

	.globl	Perl_magic_freeregexp
	.align	16, 0x90
	.type	Perl_magic_freeregexp,@function
Perl_magic_freeregexp:                  # @Perl_magic_freeregexp
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rax
.Ltmp512:
	.cfi_def_cfa_offset 16
	movq	24(%rsi), %rdi
	callq	*PL_regfree(%rip)
	xorq	%rax, %rax
	popq	%rdx
	retq
.Ltmp513:
	.size	Perl_magic_freeregexp, .Ltmp513-Perl_magic_freeregexp
	.cfi_endproc

	.globl	Perl_magic_setutf8
	.align	16, 0x90
	.type	Perl_magic_setutf8,@function
Perl_magic_setutf8:                     # @Perl_magic_setutf8
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp516:
	.cfi_def_cfa_offset 16
.Ltmp517:
	.cfi_offset %rbx, -16
	movq	%rsi, %rbx
	movq	32(%rbx), %rdi
	callq	Perl_safesysfree
	movq	$0, 32(%rbx)
	xorq	%rax, %rax
	movq	$-1, 40(%rbx)
	popq	%rbx
	retq
.Ltmp518:
	.size	Perl_magic_setutf8, .Ltmp518-Perl_magic_setutf8
	.cfi_endproc

	.globl	Perl_magic_set
	.align	16, 0x90
	.type	Perl_magic_set,@function
Perl_magic_set:                         # @Perl_magic_set
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp523:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp524:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp525:
	.cfi_def_cfa_offset 32
	subq	$16, %rsp
.Ltmp526:
	.cfi_def_cfa_offset 48
.Ltmp527:
	.cfi_offset %rbx, -32
.Ltmp528:
	.cfi_offset %r14, -24
.Ltmp529:
	.cfi_offset %r15, -16
	movq	32(%rsi), %rax
	movsbq	(%rax), %rcx
	movq	%rdi, %r15
	cmpq	$123, %rcx
	jg	.LBB62_8
# BB#1:                                 # %entry
	cmpq	$90, %rcx
	jg	.LBB62_4
# BB#2:                                 # %entry
	decq	%rcx
	movabsq	$4294967295, %r14       # imm = 0xFFFFFFFF
	andq	%r14, %rcx
	cmpq	$62, %rcx
	ja	.LBB62_224
# BB#3:                                 # %entry
	jmpq	*.LJTI62_0(,%rcx,8)
.LBB62_50:                              # %sw.bb
	movq	PL_bodytarget(%rip), %rdi
.LBB62_51:                              # %if.else109
	movq	$2, %rdx
	movq	%r15, %rsi
	callq	Perl_sv_setsv_flags
	jmp	.LBB62_224
.LBB62_8:                               # %entry
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %rcx
	cmpq	$124, %rcx
	jne	.LBB62_9
# BB#119:                               # %sw.bb384
	movq	PL_defoutgv(%rip), %rax
	movq	(%rax), %rax
	movq	56(%rax), %rax
	movq	16(%rax), %rbx
	testq	%rbx, %rbx
	je	.LBB62_224
# BB#120:                               # %if.end390
	movzbl	18(%r15), %eax
	testq	$1, %rax
	je	.LBB62_122
# BB#121:                               # %cond.true394
	movq	(%r15), %rax
	movq	24(%rax), %rax
	jmp	.LBB62_123
.LBB62_4:                               # %entry
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %rcx
	cmpq	$91, %rcx
	je	.LBB62_150
# BB#5:                                 # %entry
	cmpq	$92, %rcx
	jne	.LBB62_6
# BB#137:                               # %sw.bb442
	movq	PL_ors_sv(%rip), %rdi
	testq	%rdi, %rdi
	je	.LBB62_139
# BB#138:                               # %if.then444
	callq	Perl_sv_free
.LBB62_139:                             # %if.end445
	movl	16(%r15), %eax
	testq	$118431744, %rax        # imm = 0x70F2000
	je	.LBB62_141
# BB#140:                               # %if.then453
	movq	%r15, %rdi
	callq	Perl_newSVsv
	movq	%rax, PL_ors_sv(%rip)
	jmp	.LBB62_224
.LBB62_9:                               # %entry
	cmpq	$126, %rcx
	jne	.LBB62_224
# BB#10:                                # %sw.bb309
	movq	PL_defoutgv(%rip), %rax
	movq	(%rax), %rax
	movq	56(%rax), %rax
	movq	16(%rax), %rax
	movq	(%rax), %rax
	movq	128(%rax), %rdi
	callq	Perl_safesysfree
	movq	%r15, %rdi
	callq	Perl_savesvpv
	movq	PL_defoutgv(%rip), %rcx
	movq	(%rcx), %rcx
	movq	56(%rcx), %rcx
	movq	16(%rcx), %rcx
	movq	(%rcx), %rcx
	movq	%rax, 128(%rcx)
	movq	$1, %rsi
	movq	$15, %rdx
	movq	%rax, %rdi
	callq	Perl_gv_fetchpv
	movq	PL_defoutgv(%rip), %rcx
	movq	(%rcx), %rcx
	movq	56(%rcx), %rcx
	movq	16(%rcx), %rcx
	movq	(%rcx), %rcx
	movq	%rax, 136(%rcx)
	jmp	.LBB62_224
.LBB62_150:                             # %sw.bb477
	movzbl	18(%r15), %eax
	testq	$1, %rax
	je	.LBB62_152
# BB#151:                               # %cond.true481
	movq	(%r15), %rax
	movq	24(%rax), %rax
	movq	%rax, PL_compiling+72(%rip)
	jmp	.LBB62_224
.LBB62_6:                               # %entry
	cmpq	$94, %rcx
	jne	.LBB62_224
# BB#7:                                 # %sw.bb293
	movq	PL_defoutgv(%rip), %rax
	movq	(%rax), %rax
	movq	56(%rax), %rax
	movq	16(%rax), %rax
	movq	(%rax), %rax
	movq	112(%rax), %rdi
	callq	Perl_safesysfree
	movq	%r15, %rdi
	callq	Perl_savesvpv
	movq	PL_defoutgv(%rip), %rcx
	movq	(%rcx), %rcx
	movq	56(%rcx), %rcx
	movq	16(%rcx), %rcx
	movq	(%rcx), %rcx
	movq	%rax, 112(%rcx)
	movq	$1, %rsi
	movq	$15, %rdx
	movq	%rax, %rdi
	callq	Perl_gv_fetchpv
	movq	PL_defoutgv(%rip), %rcx
	movq	(%rcx), %rcx
	movq	56(%rcx), %rcx
	movq	16(%rcx), %rcx
	movq	(%rcx), %rcx
	movq	%rax, 120(%rcx)
	jmp	.LBB62_224
.LBB62_122:                             # %cond.false397
	movq	%r15, %rdi
	callq	Perl_sv_2iv
.LBB62_123:                             # %cond.end399
	movq	(%rbx), %rcx
	movzbl	163(%rcx), %edx
	testq	%rax, %rax
	je	.LBB62_124
# BB#125:                               # %if.else408
	testq	$4, %rdx
	jne	.LBB62_224
# BB#126:                               # %if.then414
	movq	64(%rcx), %rdi
	testq	%rdi, %rdi
	je	.LBB62_128
# BB#127:                               # %if.then417
	callq	Perl_PerlIO_flush
	movq	(%rbx), %rcx
	movzbl	163(%rcx), %edx
.LBB62_128:                             # %if.end419
	orq	$4, %rdx
	movb	%dl, 163(%rcx)
	jmp	.LBB62_224
.LBB62_124:                             # %if.then403
	andq	$251, %rdx
	movb	%dl, 163(%rcx)
	jmp	.LBB62_224
.LBB62_141:                             # %if.else455
	movq	$0, PL_ors_sv(%rip)
	jmp	.LBB62_224
.LBB62_152:                             # %cond.false484
	movq	%r15, %rdi
	callq	Perl_sv_2iv
	movq	%rax, PL_compiling+72(%rip)
.LBB62_224:                             # %sw.epilog
	xorq	%rax, %rax
.LBB62_225:                             # %sw.epilog
	addq	$16, %rsp
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.LBB62_11:                              # %sw.bb1
	movzbl	18(%r15), %eax
	testq	$1, %rax
	je	.LBB62_13
# BB#12:                                # %cond.true
	movq	(%r15), %rax
	movq	24(%rax), %rax
	movb	%al, PL_minus_c(%rip)
	jmp	.LBB62_224
.LBB62_14:                              # %sw.bb3
	movzbl	18(%r15), %eax
	testq	$1, %rax
	je	.LBB62_16
# BB#15:                                # %cond.true7
	movq	(%r15), %rax
	movq	24(%rax), %rax
	jmp	.LBB62_17
.LBB62_18:                              # %sw.bb14
	movzbl	1(%rax), %ecx
	testq	%rcx, %rcx
	je	.LBB62_19
# BB#23:                                # %if.else
	incq	%rax
	movabsq	$.L.str3, %rsi
	movq	%rax, %rdi
	callq	strcmp
	testq	%r14, %rax
	jne	.LBB62_224
# BB#24:                                # %if.then34
	movq	PL_encoding(%rip), %rdi
	testq	%rdi, %rdi
	je	.LBB62_26
# BB#25:                                # %if.then36
	callq	Perl_sv_free
.LBB62_26:                              # %if.end
	movl	16(%r15), %eax
	testq	$118431744, %rax        # imm = 0x70F2000
	je	.LBB62_28
# BB#27:                                # %if.then43
	movq	%r15, %rdi
	callq	Perl_newSVsv
	movq	%rax, PL_encoding(%rip)
	jmp	.LBB62_224
.LBB62_29:                              # %sw.bb49
	movzbl	18(%r15), %eax
	testq	$1, %rax
	je	.LBB62_31
# BB#30:                                # %cond.true53
	movq	(%r15), %rax
	movq	24(%rax), %rax
	movq	%rax, PL_maxsysfd(%rip)
	jmp	.LBB62_224
.LBB62_32:                              # %sw.bb60
	movzbl	18(%r15), %eax
	testq	$1, %rax
	je	.LBB62_34
# BB#33:                                # %cond.true64
	movq	(%r15), %rax
	movq	24(%rax), %rax
	movq	%rax, PL_hints(%rip)
	jmp	.LBB62_224
.LBB62_35:                              # %sw.bb71
	movq	PL_inplace(%rip), %rdi
	testq	%rdi, %rdi
	je	.LBB62_37
# BB#36:                                # %if.then73
	callq	Perl_safesysfree
.LBB62_37:                              # %if.end74
	movzbl	18(%r15), %eax
	testq	$1807, %rax             # imm = 0x70F
	je	.LBB62_39
# BB#38:                                # %if.then78
	movq	%r15, %rdi
	callq	Perl_savesvpv
	movq	%rax, PL_inplace(%rip)
	jmp	.LBB62_224
.LBB62_40:                              # %sw.bb82
	movzbl	1(%rax), %ecx
	testq	%rcx, %rcx
	je	.LBB62_41
# BB#47:                                # %if.else101
	movabsq	$.L.str4, %rsi
	movq	%rax, %rdi
	callq	strcmp
	testq	%r14, %rax
	jne	.LBB62_224
# BB#48:                                # %if.then105
	movq	PL_compiling+96(%rip), %rdi
	testq	%rdi, %rdi
	jne	.LBB62_51
# BB#49:                                # %if.then107
	movq	%r15, %rdi
	callq	Perl_newSVsv
	movq	%rax, PL_compiling+96(%rip)
	jmp	.LBB62_224
.LBB62_52:                              # %sw.bb113
	movzbl	18(%r15), %eax
	testq	$1, %rax
	je	.LBB62_54
# BB#53:                                # %cond.true117
	movq	(%r15), %rax
	movq	24(%rax), %rax
	jmp	.LBB62_55
.LBB62_58:                              # %sw.bb129
	movzbl	18(%r15), %eax
	testq	$1, %rax
	je	.LBB62_60
# BB#59:                                # %cond.true133
	movq	(%r15), %rax
	movq	24(%rax), %rax
	movq	%rax, PL_basetime(%rip)
	jmp	.LBB62_224
.LBB62_61:                              # %sw.bb140
	movzbl	1(%rax), %ecx
	testq	%rcx, %rcx
	je	.LBB62_62
# BB#70:                                # %if.else168
	incq	%rax
	movabsq	$.L.str8, %rsi
	movq	%rax, %rdi
	callq	strcmp
	testq	%r14, %rax
	jne	.LBB62_224
# BB#71:                                # %if.then173
	movzbl	PL_dowarn(%rip), %eax
	testq	$6, %rax
	jne	.LBB62_224
# BB#72:                                # %if.then177
	movq	16(%r15), %rax
	andq	$262144, %rax           # imm = 0x40000
	jne	.LBB62_75
# BB#73:                                # %if.then177
	movl	PL_localizing(%rip), %ecx
	testq	%r14, %rcx
	je	.LBB62_75
# BB#74:                                # %if.then183
	movabsq	$.L.str9, %rsi
	movq	$12, %rdx
	movq	%r15, %rdi
	callq	Perl_sv_setpvn
	movq	$48, PL_compiling+88(%rip)
	jmp	.LBB62_224
.LBB62_158:                             # %sw.bb504
	movq	16(%r15), %rax
	testq	$65536, %rax            # imm = 0x10000
	je	.LBB62_160
# BB#159:                               # %cond.true508
	movq	(%r15), %rax
	movq	24(%rax), %r14
	jmp	.LBB62_162
.LBB62_147:                             # %sw.bb472
	movq	PL_ofmt(%rip), %rdi
	testq	%rdi, %rdi
	je	.LBB62_149
# BB#148:                               # %if.then474
	callq	Perl_safesysfree
.LBB62_149:                             # %if.end475
	movq	%r15, %rdi
	callq	Perl_savesvpv
	movq	%rax, PL_ofmt(%rip)
	jmp	.LBB62_224
.LBB62_115:                             # %sw.bb369
	movzbl	18(%r15), %eax
	testq	$1, %rax
	je	.LBB62_117
# BB#116:                               # %cond.true373
	movq	(%r15), %rax
	movq	24(%rax), %rax
	jmp	.LBB62_118
.LBB62_189:                             # %sw.bb593
	movzbl	18(%r15), %eax
	testq	$1, %rax
	je	.LBB62_191
# BB#190:                               # %cond.true597
	movq	(%r15), %rax
	movq	24(%rax), %rax
	jmp	.LBB62_192
.LBB62_201:                             # %sw.bb630
	movzbl	18(%r15), %eax
	testq	$1, %rax
	je	.LBB62_203
# BB#202:                               # %cond.true634
	movq	(%r15), %rax
	movq	24(%rax), %rax
	jmp	.LBB62_204
.LBB62_129:                             # %sw.bb427
	movzbl	18(%r15), %eax
	testq	$1, %rax
	je	.LBB62_131
# BB#130:                               # %cond.true431
	movq	(%r15), %rax
	movq	24(%rax), %rax
	jmp	.LBB62_132
.LBB62_142:                             # %sw.bb457
	movq	PL_ofs_sv(%rip), %rdi
	testq	%rdi, %rdi
	je	.LBB62_144
# BB#143:                               # %if.then459
	callq	Perl_sv_free
.LBB62_144:                             # %if.end460
	movl	16(%r15), %eax
	testq	$118431744, %rax        # imm = 0x70F2000
	je	.LBB62_146
# BB#145:                               # %if.then468
	movq	%r15, %rdi
	callq	Perl_newSVsv
	movq	%rax, PL_ofs_sv(%rip)
	jmp	.LBB62_224
.LBB62_109:                             # %sw.bb340
	movzbl	18(%r15), %eax
	testq	$1, %rax
	je	.LBB62_111
# BB#110:                               # %cond.true344
	movq	(%r15), %rax
	movq	24(%rax), %rax
	jmp	.LBB62_112
.LBB62_93:                              # %sw.bb250
	movl	PL_localizing(%rip), %eax
	testq	%rax, %rax
	jne	.LBB62_94
# BB#96:                                # %if.else257
	movq	16(%r15), %rax
	testq	$118423552, %rax        # imm = 0x70F0000
	je	.LBB62_224
# BB#97:                                # %if.else257
	movq	PL_last_in_gv(%rip), %rcx
	testq	%rcx, %rcx
	je	.LBB62_224
# BB#98:                                # %land.lhs.true263
	movzbl	16(%rcx), %edx
	cmpq	$13, %rdx
	jne	.LBB62_224
# BB#99:                                # %land.lhs.true268
	movq	(%rcx), %rcx
	movq	56(%rcx), %rcx
	testq	%rcx, %rcx
	je	.LBB62_224
# BB#100:                               # %cond.true271
	movq	16(%rcx), %rcx
	testq	%rcx, %rcx
	je	.LBB62_224
# BB#101:                               # %if.then276
	testq	$65536, %rax            # imm = 0x10000
	je	.LBB62_103
# BB#102:                               # %cond.true280
	movq	(%r15), %rax
	movq	24(%rax), %rax
	jmp	.LBB62_104
.LBB62_136:                             # %sw.bb440
	movq	PL_rs(%rip), %rdi
	callq	Perl_sv_free
	movq	%r15, %rdi
	callq	Perl_newSVsv
	movq	%rax, PL_rs(%rip)
	jmp	.LBB62_224
.LBB62_216:                             # %sw.bb681
	movq	16(%r15), %rax
	andq	$10223616, %rax         # imm = 0x9C0000
	cmpq	$262144, %rax           # imm = 0x40000
	jne	.LBB62_218
# BB#217:                               # %cond.true686
	movq	(%r15), %rax
	movq	8(%rax), %rdx
	movq	%rdx, 8(%rsp)
	movq	(%rax), %rsi
	jmp	.LBB62_219
.LBB62_213:                             # %sw.bb667
	movq	16(%r15), %rax
	andq	$10223616, %rax         # imm = 0x9C0000
	cmpq	$262144, %rax           # imm = 0x40000
	jne	.LBB62_215
# BB#214:                               # %cond.true672
	movq	(%r15), %rax
	movq	8(%rax), %rcx
	movq	%rcx, 8(%rsp)
	movq	(%rax), %rax
	movq	%rax, PL_chopset(%rip)
	jmp	.LBB62_224
.LBB62_163:                             # %sw.bb524
	movzbl	18(%r15), %eax
	testq	$1, %rax
	je	.LBB62_165
# BB#164:                               # %cond.true528
	movq	(%r15), %rax
	movq	24(%rax), %rax
	jmp	.LBB62_166
.LBB62_105:                             # %sw.bb325
	movzbl	18(%r15), %eax
	testq	$1, %rax
	je	.LBB62_107
# BB#106:                               # %cond.true329
	movq	(%r15), %rax
	movq	24(%rax), %rax
	jmp	.LBB62_108
.LBB62_177:                             # %sw.bb556
	movzbl	18(%r15), %eax
	testq	$1, %rax
	je	.LBB62_179
# BB#178:                               # %cond.true560
	movq	(%r15), %rax
	movq	24(%rax), %rax
	jmp	.LBB62_180
.LBB62_153:                             # %sw.bb488
	movzbl	18(%r15), %eax
	testq	$1, %rax
	je	.LBB62_155
# BB#154:                               # %cond.true492
	movq	(%r15), %rax
	movq	24(%rax), %rax
	jmp	.LBB62_156
.LBB62_94:                              # %sw.bb250
	cmpq	$1, %rax
	jne	.LBB62_224
# BB#95:                                # %if.then255
	movabsq	$PL_last_in_gv, %rdi
	callq	Perl_save_sptr
	jmp	.LBB62_224
.LBB62_218:                             # %cond.false691
	leaq	8(%rsp), %rsi
	movq	$2, %rdx
	movq	%r15, %rdi
	callq	Perl_sv_pvn_force_flags
	movq	8(%rsp), %rdx
	movq	%rax, %rsi
.LBB62_219:                             # %cond.end693
	movq	PL_origargv(%rip), %rcx
	movq	PL_origalen(%rip), %rax
	movq	(%rcx), %rdi
	cmpq	%rax, %rdx
	jae	.LBB62_220
# BB#221:                               # %if.else701
	callq	memcpy
	movq	PL_origargv(%rip), %rax
	movq	8(%rsp), %rcx
	movq	(%rax), %rax
	xorq	%rdx, %rdx
	movb	%dl, (%rax,%rcx)
	movq	PL_origargv(%rip), %rax
	movq	(%rax), %rax
	movq	8(%rsp), %rdx
	movq	PL_origalen(%rip), %rcx
	leaq	1(%rdx,%rax), %rdi
	notq	%rdx
	addq	%rcx, %rdx
	movq	$32, %rsi
	callq	memset
	movslq	PL_origargc(%rip), %rax
	cmpq	$2, %rax
	jl	.LBB62_224
# BB#222:                               # %for.body715.lr.ph
	movq	$1, %rcx
.LBB62_223:                             # %for.body715
                                        # =>This Inner Loop Header: Depth=1
	movq	PL_origargv(%rip), %rdx
	movq	$0, (%rdx,%rcx,8)
	incq	%rcx
	cmpq	%rax, %rcx
	jl	.LBB62_223
	jmp	.LBB62_224
.LBB62_215:                             # %cond.false677
	leaq	8(%rsp), %rsi
	movq	$2, %rdx
	movq	%r15, %rdi
	callq	Perl_sv_pvn_force_flags
	movq	%rax, PL_chopset(%rip)
	jmp	.LBB62_224
.LBB62_220:                             # %if.then697
	movq	%rax, %rdx
	callq	memcpy
	movq	PL_origargv(%rip), %rax
	movq	PL_origalen(%rip), %rcx
	movq	(%rax), %rax
	xorq	%rdx, %rdx
	movb	%dl, -1(%rcx,%rax)
	jmp	.LBB62_224
.LBB62_13:                              # %cond.false
	movq	%r15, %rdi
	callq	Perl_sv_2iv
	movb	%al, PL_minus_c(%rip)
	jmp	.LBB62_224
.LBB62_16:                              # %cond.false10
	movq	%r15, %rdi
	callq	Perl_sv_2iv
.LBB62_17:                              # %cond.end12
	movabsq	$2147483648, %rcx       # imm = 0x80000000
	orq	%rax, %rcx
	movq	%rcx, PL_debug(%rip)
	jmp	.LBB62_224
.LBB62_19:                              # %if.then
	movzbl	18(%r15), %eax
	testq	$1, %rax
	je	.LBB62_21
# BB#20:                                # %cond.true21
	movq	(%r15), %rax
	movq	24(%rax), %rbx
	jmp	.LBB62_22
.LBB62_31:                              # %cond.false56
	movq	%r15, %rdi
	callq	Perl_sv_2iv
	movq	%rax, PL_maxsysfd(%rip)
	jmp	.LBB62_224
.LBB62_34:                              # %cond.false67
	movq	%r15, %rdi
	callq	Perl_sv_2iv
	movq	%rax, PL_hints(%rip)
	jmp	.LBB62_224
.LBB62_39:                              # %if.else80
	movq	$0, PL_inplace(%rip)
	jmp	.LBB62_224
.LBB62_41:                              # %if.then88
	movq	PL_osname(%rip), %rdi
	testq	%rdi, %rdi
	je	.LBB62_43
# BB#42:                                # %if.then90
	callq	Perl_safesysfree
	movq	$0, PL_osname(%rip)
.LBB62_43:                              # %if.end91
	movzbl	18(%r15), %eax
	testq	$1807, %rax             # imm = 0x70F
	je	.LBB62_224
# BB#44:                                # %if.then95
	movzbl	PL_tainting(%rip), %eax
	testq	%rax, %rax
	je	.LBB62_46
# BB#45:                                # %if.then97
	movabsq	$.L.str34, %rsi
	xorq	%rdi, %rdi
	callq	Perl_taint_proper
.LBB62_46:                              # %if.end98
	movq	%r15, %rdi
	callq	Perl_savesvpv
	movq	%rax, PL_osname(%rip)
	jmp	.LBB62_224
.LBB62_54:                              # %cond.false120
	movq	%r15, %rdi
	callq	Perl_sv_2iv
.LBB62_55:                              # %cond.end122
	movq	%rax, PL_perldb(%rip)
	testq	%rax, %rax
	je	.LBB62_224
# BB#56:                                # %cond.end122
	movq	PL_DBsingle(%rip), %rax
	testq	%rax, %rax
	jne	.LBB62_224
# BB#57:                                # %if.then126
	xorq	%rax, %rax
	callq	init_debugger
	jmp	.LBB62_224
.LBB62_60:                              # %cond.false136
	movq	%r15, %rdi
	callq	Perl_sv_2iv
	movq	%rax, PL_basetime(%rip)
	jmp	.LBB62_224
.LBB62_62:                              # %if.then146
	movzbl	PL_dowarn(%rip), %ecx
	testq	$6, %rcx
	jne	.LBB62_224
# BB#63:                                # %if.then150
	movzbl	18(%r15), %eax
	testq	$1, %rax
	je	.LBB62_65
# BB#64:                                # %cond.true154
	movq	(%r15), %rax
	movq	24(%rax), %rax
	jmp	.LBB62_66
.LBB62_160:                             # %cond.false511
	xorq	%r14, %r14
	testq	$118423552, %rax        # imm = 0x70F0000
	je	.LBB62_162
# BB#161:                               # %cond.true515
	movq	%r15, %rdi
	callq	Perl_sv_2iv
	movq	%rax, %r14
.LBB62_162:                             # %cond.end520
	callq	__errno_location
	movl	%r14d, (%rax)
	jmp	.LBB62_224
.LBB62_117:                             # %cond.false376
	movq	%r15, %rdi
	callq	Perl_sv_2iv
.LBB62_118:                             # %cond.end378
	movq	PL_defoutgv(%rip), %rcx
	movq	(%rcx), %rcx
	movq	56(%rcx), %rcx
	movq	16(%rcx), %rcx
	movq	(%rcx), %rcx
	movq	%rax, 88(%rcx)
	jmp	.LBB62_224
.LBB62_191:                             # %cond.false600
	movq	%r15, %rdi
	callq	Perl_sv_2iv
.LBB62_192:                             # %cond.end602
	movl	PL_delaymagic(%rip), %ecx
	movl	%eax, PL_gid(%rip)
	testq	%rcx, %rcx
	je	.LBB62_194
# BB#193:                               # %if.then606
	orq	$16, %rcx
	movl	%ecx, PL_delaymagic(%rip)
	jmp	.LBB62_224
.LBB62_203:                             # %cond.false637
	movq	%r15, %rdi
	callq	Perl_sv_2iv
.LBB62_204:                             # %cond.end639
	movl	PL_delaymagic(%rip), %ecx
	movl	%eax, PL_egid(%rip)
	testq	%rcx, %rcx
	je	.LBB62_206
# BB#205:                               # %if.then643
	orq	$32, %rcx
	movl	%ecx, PL_delaymagic(%rip)
	jmp	.LBB62_224
.LBB62_131:                             # %cond.false434
	movq	%r15, %rdi
	callq	Perl_sv_2iv
.LBB62_132:                             # %cond.end436
	testq	%rax, %rax
	je	.LBB62_133
# BB#134:                               # %cond.end436
	movabsq	$1, %rax
	movl	%eax, PL_multiline(%rip)
	jmp	.LBB62_224
.LBB62_133:
	movabsq	$0, %rax
	movl	%eax, PL_multiline(%rip)
	jmp	.LBB62_224
.LBB62_146:                             # %if.else470
	movq	$0, PL_ofs_sv(%rip)
	jmp	.LBB62_224
.LBB62_111:                             # %cond.false347
	movq	%r15, %rdi
	callq	Perl_sv_2iv
.LBB62_112:                             # %cond.end349
	movq	PL_defoutgv(%rip), %rcx
	movq	(%rcx), %rcx
	movq	56(%rcx), %rcx
	movq	16(%rcx), %rcx
	movq	(%rcx), %rcx
	testq	%rax, %rax
	movabsq	$0, %rdx
	js	.LBB62_114
# BB#113:                               # %cond.end349
	movq	%rax, %rdx
.LBB62_114:                             # %cond.end349
	xorq	%rax, %rax
	movq	%rdx, 104(%rcx)
	jmp	.LBB62_225
.LBB62_165:                             # %cond.false531
	movq	%r15, %rdi
	callq	Perl_sv_2iv
.LBB62_166:                             # %cond.end533
	movl	PL_delaymagic(%rip), %ecx
	movl	%eax, PL_uid(%rip)
	testq	%rcx, %rcx
	je	.LBB62_168
# BB#167:                               # %if.then537
	orq	$1, %rcx
	movl	%ecx, PL_delaymagic(%rip)
	jmp	.LBB62_224
.LBB62_107:                             # %cond.false332
	movq	%r15, %rdi
	callq	Perl_sv_2iv
.LBB62_108:                             # %cond.end334
	movq	PL_defoutgv(%rip), %rcx
	movq	(%rcx), %rcx
	movq	56(%rcx), %rcx
	movq	16(%rcx), %rcx
	movq	(%rcx), %rcx
	movq	%rax, 96(%rcx)
	jmp	.LBB62_224
.LBB62_179:                             # %cond.false563
	movq	%r15, %rdi
	callq	Perl_sv_2iv
.LBB62_180:                             # %cond.end565
	movl	PL_delaymagic(%rip), %ecx
	movl	%eax, PL_euid(%rip)
	testq	%rcx, %rcx
	je	.LBB62_182
# BB#181:                               # %if.then569
	orq	$2, %rcx
	movl	%ecx, PL_delaymagic(%rip)
	jmp	.LBB62_224
.LBB62_155:                             # %cond.false495
	movq	%r15, %rdi
	callq	Perl_sv_2iv
.LBB62_156:                             # %cond.end497
	movq	%rax, PL_statusvalue(%rip)
	cmpq	$-1, %rax
	je	.LBB62_224
# BB#157:                               # %if.then501
	andq	$65535, %rax            # imm = 0xFFFF
	movq	%rax, PL_statusvalue(%rip)
	jmp	.LBB62_224
.LBB62_194:                             # %if.end608
	movl	PL_egid(%rip), %ecx
	movq	%rax, %rdx
	andq	%r14, %rdx
	cmpq	%rcx, %rdx
	jne	.LBB62_196
# BB#195:                               # %if.then611
	movq	%rax, %rdi
	callq	setgid
	jmp	.LBB62_197
.LBB62_206:                             # %if.end645
	movl	PL_gid(%rip), %ecx
	movq	%rax, %rdx
	andq	%r14, %rdx
	cmpq	%rcx, %rdx
	jne	.LBB62_208
# BB#207:                               # %if.then648
	movq	%rax, %rdi
	callq	setgid
	jmp	.LBB62_209
.LBB62_168:                             # %if.end539
	movl	PL_euid(%rip), %ecx
	movq	%rax, %rdx
	andq	%r14, %rdx
	cmpq	%rcx, %rdx
	jne	.LBB62_170
# BB#169:                               # %if.then542
	movq	%rax, %rdi
	callq	setuid
	jmp	.LBB62_171
.LBB62_182:                             # %if.end571
	movl	PL_uid(%rip), %ecx
	movq	%rax, %rdx
	andq	%r14, %rdx
	cmpq	%rcx, %rdx
	jne	.LBB62_184
# BB#183:                               # %if.then574
	movq	%rax, %rdi
	callq	setuid
	jmp	.LBB62_185
.LBB62_196:                             # %if.else613
	callq	getgid
	movl	%eax, PL_gid(%rip)
	movabsq	$.L.str37, %rdi
	xorq	%rax, %rax
	callq	Perl_croak
.LBB62_197:                             # %if.end615
	callq	getgid
	movl	PL_uid(%rip), %edx
	movl	%eax, PL_gid(%rip)
	xorq	%rcx, %rcx
	testq	%rdx, %rdx
	je	.LBB62_176
# BB#198:                               # %land.rhs618
	movl	PL_euid(%rip), %ecx
	cmpq	%rdx, %rcx
	movq	$1, %rcx
	jne	.LBB62_176
# BB#199:                               # %lor.rhs621
	movl	PL_egid(%rip), %ecx
	andq	%r14, %rax
	cmpq	%rax, %rcx
	jne	.LBB62_175
# BB#200:
	movabsq	$0, %rcx
	jmp	.LBB62_176
.LBB62_208:                             # %if.else650
	callq	getegid
	movl	%eax, PL_egid(%rip)
	movabsq	$.L.str38, %rdi
	xorq	%rax, %rax
	callq	Perl_croak
.LBB62_209:                             # %if.end652
	callq	getegid
	movl	PL_uid(%rip), %edx
	movl	%eax, PL_egid(%rip)
	xorq	%rcx, %rcx
	testq	%rdx, %rdx
	je	.LBB62_176
# BB#210:                               # %land.rhs655
	movl	PL_euid(%rip), %ecx
	cmpq	%rdx, %rcx
	movq	$1, %rcx
	jne	.LBB62_176
# BB#211:                               # %lor.rhs658
	movl	PL_gid(%rip), %ecx
	andq	%r14, %rax
	cmpq	%rcx, %rax
	jne	.LBB62_175
# BB#212:
	movabsq	$0, %rcx
	jmp	.LBB62_176
.LBB62_170:                             # %if.else544
	callq	getuid
	movl	%eax, PL_uid(%rip)
	movabsq	$.L.str35, %rdi
	xorq	%rax, %rax
	callq	Perl_croak
.LBB62_171:                             # %if.end546
	callq	getuid
	xorq	%rcx, %rcx
	testq	%r14, %rax
	movl	%eax, PL_uid(%rip)
	je	.LBB62_176
# BB#172:                               # %land.rhs
	movl	PL_euid(%rip), %ecx
	andq	%r14, %rax
	cmpq	%rax, %rcx
	movq	$1, %rcx
	jne	.LBB62_176
# BB#173:                               # %lor.rhs
	movl	PL_egid(%rip), %eax
	movl	PL_gid(%rip), %ecx
	cmpq	%rcx, %rax
	jne	.LBB62_175
# BB#174:
	movabsq	$0, %rcx
	jmp	.LBB62_176
.LBB62_184:                             # %if.else576
	callq	geteuid
	movl	%eax, PL_euid(%rip)
	movabsq	$.L.str36, %rdi
	xorq	%rax, %rax
	callq	Perl_croak
.LBB62_185:                             # %if.end578
	callq	geteuid
	movl	PL_uid(%rip), %edx
	movl	%eax, PL_euid(%rip)
	xorq	%rcx, %rcx
	testq	%rdx, %rdx
	je	.LBB62_176
# BB#186:                               # %land.rhs581
	andq	%r14, %rax
	movq	$1, %rcx
	cmpq	%rdx, %rax
	jne	.LBB62_176
# BB#187:                               # %lor.rhs584
	movl	PL_egid(%rip), %eax
	movl	PL_gid(%rip), %ecx
	cmpq	%rcx, %rax
	je	.LBB62_188
.LBB62_175:                             # %lor.rhs
	movabsq	$1, %rcx
	jmp	.LBB62_176
.LBB62_21:                              # %cond.false24
	movq	%r15, %rdi
	callq	Perl_sv_2iv
	movq	%rax, %rbx
.LBB62_22:                              # %cond.end26
	callq	__errno_location
	movl	%ebx, (%rax)
	jmp	.LBB62_224
.LBB62_28:                              # %if.else45
	movq	$0, PL_encoding(%rip)
	jmp	.LBB62_224
.LBB62_188:
	movabsq	$0, %rcx
.LBB62_176:                             # %land.end
	movzbl	PL_tainting(%rip), %eax
	orq	%rcx, %rax
	movb	%al, PL_tainting(%rip)
	jmp	.LBB62_224
.LBB62_65:                              # %cond.false157
	movq	%r15, %rdi
	callq	Perl_sv_2iv
	movzbl	PL_dowarn(%rip), %ecx
.LBB62_66:                              # %cond.end159
	andq	$254, %rcx
	testq	%rax, %rax
	je	.LBB62_67
# BB#68:                                # %cond.end159
	movabsq	$1, %rax
	jmp	.LBB62_69
.LBB62_67:
	movabsq	$0, %rax
.LBB62_69:                              # %cond.end159
	orq	%rax, %rcx
	movb	%cl, PL_dowarn(%rip)
	jmp	.LBB62_224
.LBB62_75:                              # %if.end184
	testq	%rax, %rax
	je	.LBB62_77
# BB#76:                                # %cond.true191
	movq	(%r15), %rax
	movq	8(%rax), %rcx
	movq	%rcx, (%rsp)
	movq	(%rax), %rax
	jmp	.LBB62_78
.LBB62_103:                             # %cond.false283
	movq	%r15, %rdi
	callq	Perl_sv_2iv
	movq	PL_last_in_gv(%rip), %rcx
	movq	(%rcx), %rcx
	movq	56(%rcx), %rcx
	movq	16(%rcx), %rcx
.LBB62_104:                             # %cond.end285
	movq	(%rcx), %rcx
	movq	%rax, 80(%rcx)
	jmp	.LBB62_224
.LBB62_77:                              # %cond.false194
	leaq	(%rsp), %rax
	movq	$2, %rdx
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	Perl_sv_2pv_flags
	movq	(%rsp), %rcx
.LBB62_78:                              # %cond.end196
	testq	%rcx, %rcx
	je	.LBB62_82
# BB#79:
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	xorq	%rdi, %rdi
.LBB62_80:                              # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	%rdx, %rbx
	movsbq	(%rax,%rsi), %rdx
	orq	%rdx, %rdi
	andq	$170, %rdx
	orq	%rbx, %rdx
	incq	%rsi
	cmpq	%rcx, %rsi
	jb	.LBB62_80
# BB#81:                                # %for.end
	testq	%r14, %rdi
	je	.LBB62_82
# BB#83:                                # %if.else208
	movq	(%r15), %rax
	movq	(%rax), %rax
	movzbl	(%rax), %eax
	testq	$1, %rax
	je	.LBB62_86
# BB#84:                                # %if.else208
	testq	%r14, %rdx
	jne	.LBB62_86
# BB#85:                                # %if.then217
	movzbl	PL_dowarn(%rip), %eax
	movq	$24, PL_compiling+88(%rip)
	jmp	.LBB62_92
.LBB62_82:                              # %if.then207
	movq	$48, PL_compiling+88(%rip)
	jmp	.LBB62_224
.LBB62_86:                              # %if.else221
	movq	PL_compiling+88(%rip), %rax
	cmpq	$48, %rax
	ja	.LBB62_89
# BB#87:                                # %if.else221
	movq	$1, %rdx
	movq	%rax, %rcx
	shlq	%cl, %rdx
	movabsq	$281474993487873, %rcx  # imm = 0x1000001000001
	testq	%rcx, %rdx
	je	.LBB62_89
# BB#88:                                # %if.then230
	movq	%r15, %rdi
	callq	Perl_newSVsv
	movq	%rax, PL_compiling+88(%rip)
	jmp	.LBB62_90
.LBB62_89:                              # %if.else232
	movq	$2, %rdx
	movq	%rax, %rdi
	movq	%r15, %rsi
	callq	Perl_sv_setsv_flags
	movq	PL_compiling+88(%rip), %rax
.LBB62_90:                              # %if.end233
	movq	(%rax), %rax
	movq	(%rax), %rax
	movzbl	3(%rax), %eax
	testq	$16, %rax
	je	.LBB62_224
# BB#91:                                # %if.then240
	movzbl	PL_dowarn(%rip), %eax
.LBB62_92:                              # %sw.epilog
	orq	$8, %rax
	movb	%al, PL_dowarn(%rip)
	jmp	.LBB62_224
.Ltmp530:
	.size	Perl_magic_set, .Ltmp530-Perl_magic_set
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI62_0:
	.quad	.LBB62_50
	.quad	.LBB62_224
	.quad	.LBB62_11
	.quad	.LBB62_14
	.quad	.LBB62_18
	.quad	.LBB62_29
	.quad	.LBB62_224
	.quad	.LBB62_32
	.quad	.LBB62_35
	.quad	.LBB62_224
	.quad	.LBB62_224
	.quad	.LBB62_224
	.quad	.LBB62_224
	.quad	.LBB62_224
	.quad	.LBB62_40
	.quad	.LBB62_52
	.quad	.LBB62_224
	.quad	.LBB62_224
	.quad	.LBB62_224
	.quad	.LBB62_58
	.quad	.LBB62_224
	.quad	.LBB62_224
	.quad	.LBB62_61
	.quad	.LBB62_224
	.quad	.LBB62_224
	.quad	.LBB62_224
	.quad	.LBB62_224
	.quad	.LBB62_224
	.quad	.LBB62_224
	.quad	.LBB62_224
	.quad	.LBB62_224
	.quad	.LBB62_224
	.quad	.LBB62_158
	.quad	.LBB62_224
	.quad	.LBB62_147
	.quad	.LBB62_224
	.quad	.LBB62_115
	.quad	.LBB62_224
	.quad	.LBB62_224
	.quad	.LBB62_189
	.quad	.LBB62_201
	.quad	.LBB62_129
	.quad	.LBB62_224
	.quad	.LBB62_142
	.quad	.LBB62_109
	.quad	.LBB62_93
	.quad	.LBB62_136
	.quad	.LBB62_216
	.quad	.LBB62_224
	.quad	.LBB62_224
	.quad	.LBB62_224
	.quad	.LBB62_224
	.quad	.LBB62_224
	.quad	.LBB62_224
	.quad	.LBB62_224
	.quad	.LBB62_224
	.quad	.LBB62_224
	.quad	.LBB62_213
	.quad	.LBB62_224
	.quad	.LBB62_163
	.quad	.LBB62_105
	.quad	.LBB62_177
	.quad	.LBB62_153

	.text
	.globl	Perl_sighandler
	.align	16, 0x90
	.type	Perl_sighandler,@function
Perl_sighandler:                        # @Perl_sighandler
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp537:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp538:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp539:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp540:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp541:
	.cfi_def_cfa_offset 48
	subq	$64, %rsp
.Ltmp542:
	.cfi_def_cfa_offset 112
.Ltmp543:
	.cfi_offset %rbx, -48
.Ltmp544:
	.cfi_offset %r12, -40
.Ltmp545:
	.cfi_offset %r13, -32
.Ltmp546:
	.cfi_offset %r14, -24
.Ltmp547:
	.cfi_offset %r15, -16
	movq	%rdi, %rbx
	movq	PL_stack_sp(%rip), %r13
	movq	PL_Sv(%rip), %rax
	movq	%rax, 32(%rsp)          # 8-byte Spill
	movq	PL_op(%rip), %rax
	movq	%rax, 24(%rsp)          # 8-byte Spill
	movq	PL_savestack_ix(%rip), %rdx
	movq	PL_savestack_max(%rip), %rax
	movq	PL_Xpv(%rip), %rcx
	movq	%rcx, 16(%rsp)          # 8-byte Spill
	movq	$0, 56(%rsp)
	leaq	15(%rdx), %rcx
	cmpq	%rax, %rcx
	jg	.LBB63_1
# BB#2:                                 # %entry
	movabsq	$1, %rax
	jmp	.LBB63_3
.LBB63_1:
	movabsq	$0, %rax
.LBB63_3:                               # %entry
	movq	PL_markstack_max(%rip), %rcx
	movq	PL_markstack_ptr(%rip), %rsi
	movq	%rax, 40(%rsp)
	addq	$-16, %rcx
	cmpq	%rcx, %rsi
	jae	.LBB63_5
# BB#4:                                 # %if.then2
	orq	$4, %rax
	movq	%rax, 40(%rsp)
.LBB63_5:                               # %if.end4
	movq	PL_retstack_max(%rip), %rcx
	movq	PL_retstack_ix(%rip), %rsi
	addq	$-2, %rcx
	cmpq	%rcx, %rsi
	jge	.LBB63_7
# BB#6:                                 # %if.then6
	orq	$8, %rax
	movq	%rax, 40(%rsp)
.LBB63_7:                               # %if.end8
	movq	PL_scopestack_max(%rip), %rcx
	movq	PL_scopestack_ix(%rip), %rsi
	addq	$-3, %rcx
	cmpq	%rcx, %rsi
	jge	.LBB63_9
# BB#8:                                 # %if.then11
	orq	$16, %rax
	movq	%rax, 40(%rsp)
.LBB63_9:                               # %if.end13
	movq	$32, %rcx
	movq	%rbx, %r14
	movq	PL_psig_ptr(%rip), %rsi
	shlq	%cl, %r14
	movq	$32, %rcx
	sarq	%cl, %r14
	cmpq	$0, (%rsi,%r14,8)
	je	.LBB63_10
# BB#16:                                # %if.end33
	testq	$1, %rax
	je	.LBB63_18
# BB#17:                                # %if.then36
	addq	$5, %rdx
	movq	%rdx, PL_savestack_ix(%rip)
	movabsq	$unwind_handler_stack, %rdi
	leaq	40(%rsp), %rsi
	callq	Perl_save_destructor_x
	movq	40(%rsp), %rax
.LBB63_18:                              # %if.end38
	testq	$4, %rax
	je	.LBB63_20
# BB#19:                                # %if.then41
	addq	$8, PL_markstack_ptr(%rip)
.LBB63_20:                              # %if.end42
	movq	%rbx, 8(%rsp)           # 8-byte Spill
	testq	$8, %rax
	je	.LBB63_22
# BB#21:                                # %if.then45
	movq	PL_retstack_ix(%rip), %rcx
	movq	PL_retstack(%rip), %rdx
	leaq	1(%rcx), %rsi
	movq	%rsi, PL_retstack_ix(%rip)
	movq	$0, 8(%rdx,%rcx,8)
.LBB63_22:                              # %if.end47
	testq	$16, %rax
	je	.LBB63_24
# BB#23:                                # %if.then50
	incq	PL_scopestack_ix(%rip)
.LBB63_24:                              # %if.end52
	movq	PL_psig_ptr(%rip), %rax
	movq	(%rax,%r14,8), %rdi
	movzbl	18(%rdi), %eax
	testq	$8, %rax
	je	.LBB63_27
# BB#25:                                # %lor.lhs.false
	movq	(%rdi), %rax
	movq	(%rax), %r12
	testq	%r12, %r12
	je	.LBB63_27
# BB#26:                                # %lor.lhs.false62
	movzbl	16(%r12), %eax
	cmpq	$12, %rax
	je	.LBB63_28
.LBB63_27:                              # %if.then66
	leaq	48(%rsp), %rsi
	leaq	56(%rsp), %rdx
	movq	$1, %rcx
	callq	Perl_sv_2cv
	movq	%rax, %r12
.LBB63_28:                              # %if.end70
	testq	%r12, %r12
	je	.LBB63_30
# BB#29:                                # %lor.lhs.false72
	movq	(%r12), %rax
	cmpq	$0, 72(%rax)
	je	.LBB63_30
# BB#43:                                # %if.end144
	movq	PL_psig_name(%rip), %rax
	movq	(%rax,%r14,8), %r15
	testq	%r15, %r15
	je	.LBB63_45
# BB#44:                                # %if.then148
	movq	%r15, PL_Sv(%rip)
	incq	8(%r15)
	orq	$64, 40(%rsp)
	movq	%r15, sig_sv(%rip)
	jmp	.LBB63_46
.LBB63_30:                              # %if.then75
	movq	PL_curcop(%rip), %rax
	cmpq	$0, 88(%rax)
	je	.LBB63_34
# BB#31:                                # %land.lhs.true77
	movq	PL_curcop(%rip), %rax
	movq	88(%rax), %rax
	cmpq	$48, %rax
	je	.LBB63_34
# BB#32:                                # %land.lhs.true80
	movq	PL_curcop(%rip), %rax
	movq	88(%rax), %rax
	cmpq	$24, %rax
	je	.LBB63_36
# BB#33:                                # %lor.lhs.false83
	movq	PL_curcop(%rip), %rax
	movq	88(%rax), %rax
	movq	(%rax), %rax
	movq	(%rax), %rax
	movzbl	6(%rax), %eax
	testq	$16, %rax
	jne	.LBB63_36
.LBB63_34:                              # %lor.lhs.false89
	movq	PL_curcop(%rip), %rax
	xorq	%r15, %r15
	cmpq	$0, 88(%rax)
	jne	.LBB63_69
# BB#35:                                # %land.lhs.true93
	movzbl	PL_dowarn(%rip), %eax
	testq	$1, %rax
	je	.LBB63_69
.LBB63_36:                              # %if.then97
	movq	56(%rsp), %rax
	movq	PL_sig_name(,%r14,8), %rdx
	testq	%rax, %rax
	jne	.LBB63_39
# BB#37:                                # %cond.false113
	movabsq	$.L.str42, %rcx
	testq	%r12, %r12
	je	.LBB63_42
# BB#38:                                # %land.lhs.true115
	movq	(%r12), %rax
	movq	96(%rax), %rax
	testq	%rax, %rax
	je	.LBB63_42
.LBB63_39:                              # %cond.true118
	movq	(%rax), %rcx
	movq	56(%rcx), %rcx
	movq	48(%rcx), %rcx
	testq	%rcx, %rcx
	je	.LBB63_41
# BB#40:                                # %cond.true118
	movq	%rcx, %rax
.LBB63_41:                              # %cond.true118
	movq	(%rax), %rax
	movq	64(%rax), %rcx
.LBB63_42:                              # %cond.end141
	movabsq	$.L.str41, %rsi
	movq	$26, %rdi
	xorq	%rax, %rax
	xorq	%r15, %r15
	callq	Perl_warner
	jmp	.LBB63_69
.LBB63_45:                              # %if.else
	callq	Perl_sv_newmortal
	movq	PL_sig_name(,%r14,8), %rsi
	movq	%rax, %r15
	movq	%r15, %rdi
	callq	Perl_sv_setpv
.LBB63_46:                              # %if.end158
	movq	PL_curstackinfo(%rip), %rax
	movq	48(%rax), %rax
	testq	%rax, %rax
	jne	.LBB63_48
# BB#47:                                # %if.then160
	movq	$32, %rdi
	movq	$12, %rsi
	callq	Perl_new_stackinfo
	movq	PL_curstackinfo(%rip), %rcx
	movq	%rcx, 40(%rax)
	movq	PL_curstackinfo(%rip), %rcx
	movq	%rax, 48(%rcx)
.LBB63_48:                              # %if.end163
	movq	$4, 32(%rax)
	movq	$-1, 16(%rax)
	movq	(%rax), %rcx
	movq	(%rcx), %rdx
	movq	$0, 8(%rdx)
	movq	PL_curstack(%rip), %rcx
	movq	PL_stack_base(%rip), %rsi
	movq	(%rcx), %rdi
	subq	%rsi, %r13
	movq	$3, %rcx
	sarq	%cl, %r13
	movq	%r13, 8(%rdi)
	movq	(%rdx), %r13
	movq	%r13, PL_stack_base(%rip)
	movq	(%rax), %rcx
	movq	(%rcx), %rcx
	movq	16(%rcx), %rcx
	leaq	(%r13,%rcx,8), %rcx
	movq	%rcx, PL_stack_max(%rip)
	movq	(%rax), %rcx
	movq	(%rcx), %rcx
	movq	8(%rcx), %r14
	leaq	(%r13,%r14,8), %rbx
	movq	%rbx, PL_stack_sp(%rip)
	movq	(%rax), %rcx
	movq	%rcx, PL_curstack(%rip)
	movq	PL_markstack_ptr(%rip), %rdx
	movq	PL_markstack_max(%rip), %rcx
	movq	%rax, PL_curstackinfo(%rip)
	addq	$8, %rdx
	movq	%rdx, PL_markstack_ptr(%rip)
	cmpq	%rcx, %rdx
	movq	%r13, %rax
	jne	.LBB63_50
# BB#49:                                # %if.then180
	callq	Perl_markstack_grow
	movq	PL_stack_base(%rip), %rax
	movq	PL_markstack_ptr(%rip), %rdx
.LBB63_50:                              # %if.end181
	subq	%rax, %rbx
	movq	$3, %rcx
	sarq	%cl, %rbx
	movq	%rbx, (%rdx)
	leaq	8(%r13,%r14,8), %rax
	movq	%r15, 8(%r13,%r14,8)
	movq	%rax, PL_stack_sp(%rip)
	movq	$6, %rsi
	movq	%r12, %rdi
	callq	Perl_call_sv
	movq	PL_curstackinfo(%rip), %rax
	movq	40(%rax), %r14
	movq	PL_stack_sp(%rip), %rbx
	testq	%r14, %r14
	jne	.LBB63_57
# BB#51:                                # %if.then194
	movq	PL_stderrgv(%rip), %rax
	testq	%rax, %rax
	je	.LBB63_55
# BB#52:                                # %land.lhs.true196
	movzbl	16(%rax), %ecx
	cmpq	$13, %rcx
	jne	.LBB63_55
# BB#53:                                # %land.lhs.true201
	movq	(%rax), %rax
	movq	56(%rax), %rax
	movq	16(%rax), %rax
	testq	%rax, %rax
	je	.LBB63_55
# BB#54:                                # %land.lhs.true206
	movq	(%rax), %rax
	movq	64(%rax), %rdi
	testq	%rdi, %rdi
	jne	.LBB63_56
.LBB63_55:                              # %cond.false219
	callq	Perl_PerlIO_stderr
	movq	%rax, %rdi
.LBB63_56:                              # %cond.end221
	movabsq	$.L.str24, %rsi
	xorq	%rax, %rax
	callq	PerlIO_printf
	movq	$1, %rdi
	callq	Perl_my_exit
.LBB63_57:                              # %if.end224
	movq	PL_curstack(%rip), %rax
	movq	PL_stack_base(%rip), %rcx
	movq	(%rax), %rax
	subq	%rcx, %rbx
	movq	$3, %rcx
	sarq	%cl, %rbx
	movq	%rbx, 8(%rax)
	movq	(%r14), %rax
	movq	(%rax), %rax
	movq	(%rax), %rax
	movq	%rax, PL_stack_base(%rip)
	movq	(%r14), %rcx
	movq	(%rcx), %rcx
	movq	16(%rcx), %rcx
	leaq	(%rax,%rcx,8), %rcx
	movq	%rcx, PL_stack_max(%rip)
	movq	(%r14), %rcx
	movq	(%rcx), %rcx
	movq	8(%rcx), %rcx
	leaq	(%rax,%rcx,8), %rax
	movq	%rax, PL_stack_sp(%rip)
	movq	(%r14), %rax
	movq	PL_errgv(%rip), %rcx
	movq	%rax, PL_curstack(%rip)
	movq	%r14, PL_curstackinfo(%rip)
	movq	(%rcx), %rax
	movq	56(%rax), %rax
	movq	(%rax), %rdi
	testq	%rdi, %rdi
	je	.LBB63_69
# BB#58:                                # %cond.false249
	movq	16(%rdi), %rax
	testq	$262144, %rax           # imm = 0x40000
	je	.LBB63_63
# BB#59:                                # %cond.true256
	movq	(%rdi), %rax
	testq	%rax, %rax
	movq	%rax, PL_Xpv(%rip)
	je	.LBB63_69
# BB#60:                                # %land.lhs.true262
	movq	8(%rax), %rcx
	cmpq	$1, %rcx
	ja	.LBB63_68
# BB#61:                                # %lor.lhs.false265
	testq	%rcx, %rcx
	je	.LBB63_69
# BB#62:                                # %land.lhs.true268
	movq	(%rax), %rax
	movzbl	(%rax), %eax
	cmpq	$48, %rax
	jne	.LBB63_68
	jmp	.LBB63_69
.LBB63_63:                              # %cond.false275
	testq	$65536, %rax            # imm = 0x10000
	je	.LBB63_65
# BB#64:                                # %cond.true282
	movq	(%rdi), %rax
	cmpq	$0, 24(%rax)
	jne	.LBB63_68
	jmp	.LBB63_69
.LBB63_65:                              # %cond.false289
	testq	$131072, %rax           # imm = 0x20000
	je	.LBB63_67
# BB#66:                                # %cond.true296
	movq	(%rdi), %rax
	movq	32(%rax), %rdi
	xorq	%rsi, %rsi
	callq	__nedf2
	testq	%rax, %rax
	jne	.LBB63_68
	jmp	.LBB63_69
.LBB63_67:                              # %cond.false303
	callq	Perl_sv_2bool
	testq	$255, %rax
	je	.LBB63_69
.LBB63_68:                              # %if.then310
	movq	$1, %rsi
	movq	8(%rsp), %rbx           # 8-byte Reload
	movq	%rbx, %rdi
	callq	Perl_rsignal
	movq	PL_csighandlerp(%rip), %rsi
	movq	%rbx, %rdi
	callq	Perl_rsignal
	xorq	%rdi, %rdi
	xorq	%rax, %rax
	callq	Perl_die
.LBB63_69:                              # %cleanup
	movq	40(%rsp), %rax
	testq	$1, %rax
	je	.LBB63_71
# BB#70:                                # %if.then317
	addq	$-8, PL_savestack_ix(%rip)
.LBB63_71:                              # %if.end319
	testq	$4, %rax
	je	.LBB63_73
# BB#72:                                # %if.then322
	addq	$-8, PL_markstack_ptr(%rip)
.LBB63_73:                              # %if.end324
	testq	$8, %rax
	je	.LBB63_75
# BB#74:                                # %if.then327
	decq	PL_retstack_ix(%rip)
.LBB63_75:                              # %if.end328
	testq	$16, %rax
	je	.LBB63_77
# BB#76:                                # %if.then331
	decq	PL_scopestack_ix(%rip)
.LBB63_77:                              # %if.end333
	testq	$64, %rax
	je	.LBB63_79
# BB#78:                                # %if.then336
	movq	%r15, %rdi
	callq	Perl_sv_free
.LBB63_79:                              # %if.end337
	movq	24(%rsp), %rax          # 8-byte Reload
	movq	%rax, PL_op(%rip)
	movq	32(%rsp), %rax          # 8-byte Reload
	movq	%rax, PL_Sv(%rip)
	movq	16(%rsp), %rax          # 8-byte Reload
	movq	%rax, PL_Xpv(%rip)
	addq	$64, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB63_10:                              # %if.then14
	movq	PL_stderrgv(%rip), %rax
	testq	%rax, %rax
	je	.LBB63_14
# BB#11:                                # %land.lhs.true
	movzbl	16(%rax), %ecx
	cmpq	$13, %rcx
	jne	.LBB63_14
# BB#12:                                # %land.lhs.true17
	movq	(%rax), %rax
	movq	56(%rax), %rax
	movq	16(%rax), %rax
	testq	%rax, %rax
	je	.LBB63_14
# BB#13:                                # %land.lhs.true19
	movq	(%rax), %rax
	movq	64(%rax), %rcx
	testq	%rcx, %rcx
	jne	.LBB63_15
.LBB63_14:                              # %cond.false
	callq	Perl_PerlIO_stderr
	movq	%rax, %rcx
.LBB63_15:                              # %cond.end
	movq	PL_sig_name(,%r14,8), %rdx
	movabsq	$.L.str40, %rsi
	xorq	%rax, %rax
	movq	%rcx, %rdi
	callq	PerlIO_printf
	movq	%rbx, %rdi
	callq	exit
.Ltmp548:
	.size	Perl_sighandler, .Ltmp548-Perl_sighandler
	.cfi_endproc

	.align	16, 0x90
	.type	unwind_handler_stack,@function
unwind_handler_stack:                   # @unwind_handler_stack
	.cfi_startproc
# BB#0:                                 # %entry
	movq	(%rdi), %rax
	testq	$1, %rax
	je	.LBB64_2
# BB#1:                                 # %if.then
	addq	$-5, PL_savestack_ix(%rip)
.LBB64_2:                               # %if.end
	testq	$64, %rax
	je	.LBB64_3
# BB#4:                                 # %if.then3
	movq	sig_sv(%rip), %rdi
	jmp	Perl_sv_free  # TAILCALL
.LBB64_3:                               # %if.end4
	retq
.Ltmp549:
	.size	unwind_handler_stack, .Ltmp549-unwind_handler_stack
	.cfi_endproc

	.type	.L.str,@object          # @.str
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.asciz	"Size magic not implemented"
	.size	.L.str, 27

	.type	.L.str1,@object         # @.str1
.L.str1:
	.asciz	"panic: magic_len: %ld"
	.size	.L.str1, 22

	.type	.L.str2,@object         # @.str2
.L.str2:
	.zero	1
	.size	.L.str2, 1

	.type	.L.str3,@object         # @.str3
.L.str3:
	.asciz	"NCODING"
	.size	.L.str3, 8

	.type	.L.str4,@object         # @.str4
.L.str4:
	.asciz	"\017PEN"
	.size	.L.str4, 5

	.type	.L.str5,@object         # @.str5
.L.str5:
	.asciz	"\024AINT"
	.size	.L.str5, 6

	.type	.L.str6,@object         # @.str6
.L.str6:
	.asciz	"\025NICODE"
	.size	.L.str6, 8

	.type	.L.str7,@object         # @.str7
.L.str7:
	.asciz	"\025TF8LOCALE"
	.size	.L.str7, 11

	.type	.L.str8,@object         # @.str8
.L.str8:
	.asciz	"ARNING_BITS"
	.size	.L.str8, 12

	.type	.L.str9,@object         # @.str9
	.section	.rodata,"a",@progbits
.L.str9:
	.zero	13
	.size	.L.str9, 13

	.type	.L.str10,@object        # @.str10
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str10:
	.asciz	"warnings::Bits"
	.size	.L.str10, 15

	.type	.L.str11,@object        # @.str11
.L.str11:
	.asciz	"all"
	.size	.L.str11, 4

	.type	.L.str12,@object        # @.str12
.L.str12:
	.asciz	"UUUUUUUUUUUU"
	.size	.L.str12, 13

	.type	.L.str13,@object        # @.str13
.L.str13:
	.asciz	"_TOP"
	.size	.L.str13, 5

	.type	.L.str14,@object        # @.str14
.L.str14:
	.asciz	"PATH"
	.size	.L.str14, 5

	.type	.L.str15,@object        # @.str15
.L.str15:
	.asciz	"IGNORE"
	.size	.L.str15, 7

	.type	.L.str16,@object        # @.str16
.L.str16:
	.asciz	"__DIE__"
	.size	.L.str16, 8

	.type	.L.str17,@object        # @.str17
.L.str17:
	.asciz	"__WARN__"
	.size	.L.str17, 9

	.type	.L.str18,@object        # @.str18
.L.str18:
	.asciz	"No such hook: %s"
	.size	.L.str18, 17

	.type	.L.str19,@object        # @.str19
.L.str19:
	.asciz	"No such signal: SIG%s"
	.size	.L.str19, 22

	.type	.L.str20,@object        # @.str20
.L.str20:
	.asciz	"DEFAULT"
	.size	.L.str20, 8

	.type	.L.str21,@object        # @.str21
.L.str21:
	.asciz	"main::"
	.size	.L.str21, 7

	.type	.L.str22,@object        # @.str22
.L.str22:
	.asciz	"FETCH"
	.size	.L.str22, 6

	.type	.L.str23,@object        # @.str23
.L.str23:
	.asciz	"STORE"
	.size	.L.str23, 6

	.type	.L.str24,@object        # @.str24
.L.str24:
	.asciz	"panic: POPSTACK\n"
	.size	.L.str24, 17

	.type	.L.str25,@object        # @.str25
.L.str25:
	.asciz	"DELETE"
	.size	.L.str25, 7

	.type	.L.str26,@object        # @.str26
.L.str26:
	.asciz	"FETCHSIZE"
	.size	.L.str26, 10

	.type	.L.str27,@object        # @.str27
.L.str27:
	.asciz	"CLEAR"
	.size	.L.str27, 6

	.type	.L.str28,@object        # @.str28
.L.str28:
	.asciz	"NEXTKEY"
	.size	.L.str28, 8

	.type	.L.str29,@object        # @.str29
.L.str29:
	.asciz	"FIRSTKEY"
	.size	.L.str29, 9

	.type	.L.str30,@object        # @.str30
.L.str30:
	.asciz	"EXISTS"
	.size	.L.str30, 7

	.type	.L.str31,@object        # @.str31
.L.str31:
	.asciz	"SCALAR"
	.size	.L.str31, 7

	.type	.L.str32,@object        # @.str32
.L.str32:
	.asciz	"*"
	.size	.L.str32, 2

	.type	.L.str33,@object        # @.str33
.L.str33:
	.asciz	"panic: magic_killbackrefs"
	.size	.L.str33, 26

	.type	.L.str34,@object        # @.str34
.L.str34:
	.asciz	"assigning to $^O"
	.size	.L.str34, 17

	.type	.L.str35,@object        # @.str35
.L.str35:
	.asciz	"setruid() not implemented"
	.size	.L.str35, 26

	.type	.L.str36,@object        # @.str36
.L.str36:
	.asciz	"seteuid() not implemented"
	.size	.L.str36, 26

	.type	.L.str37,@object        # @.str37
.L.str37:
	.asciz	"setrgid() not implemented"
	.size	.L.str37, 26

	.type	.L.str38,@object        # @.str38
.L.str38:
	.asciz	"setegid() not implemented"
	.size	.L.str38, 26

	.type	.L.str39,@object        # @.str39
.L.str39:
	.asciz	"CLD"
	.size	.L.str39, 4

	.type	.L.str40,@object        # @.str40
.L.str40:
	.asciz	"Signal SIG%s received, but no signal handler set.\n"
	.size	.L.str40, 51

	.type	.L.str41,@object        # @.str41
.L.str41:
	.asciz	"SIG%s handler \"%s\" not defined.\n"
	.size	.L.str41, 33

	.type	.L.str42,@object        # @.str42
.L.str42:
	.asciz	"__ANON__"
	.size	.L.str42, 9

	.type	sig_sv,@object          # @sig_sv
	.local	sig_sv
	.comm	sig_sv,8,8

	.ident	"clang version 3.5 (trunk)"
	.section	".note.GNU-stack","",@progbits
