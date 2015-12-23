	.file	"pad.c.bc"
	.text
	.globl	Perl_pad_new
	.align	16, 0x90
	.type	Perl_pad_new,@function
Perl_pad_new:                           # @Perl_pad_new
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
	subq	$16, %rsp
.Ltmp11:
	.cfi_def_cfa_offset 64
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
	movq	%rdi, %rbx
	testq	$2, %rbx
	je	.LBB0_1
# BB#2:                                 # %if.then
	movq	PL_savestack_ix(%rip), %rax
	movq	PL_savestack_max(%rip), %rcx
	leaq	2(%rax), %rdx
	cmpq	%rcx, %rdx
	jle	.LBB0_4
# BB#3:                                 # %if.then1
	callq	Perl_savestack_grow
	movq	PL_savestack_ix(%rip), %rax
.LBB0_4:                                # %if.end
	movq	PL_comppad(%rip), %rcx
	movq	PL_savestack(%rip), %rdx
	movq	%rcx, (%rdx,%rax,8)
	movq	PL_savestack(%rip), %rcx
	leaq	2(%rax), %rdx
	movq	%rdx, PL_savestack_ix(%rip)
	movq	$33, 8(%rcx,%rax,8)
	movabsq	$PL_comppad_name, %rdi
	callq	Perl_save_sptr
	movq	%rbx, %r13
	andq	$1, %r13
	jne	.LBB0_7
# BB#5:                                 # %if.then6
	movabsq	$PL_padix, %rdi
	callq	Perl_save_I32
	movabsq	$PL_comppad_name_fill, %rdi
	callq	Perl_save_I32
	movabsq	$PL_min_intro_pending, %rdi
	callq	Perl_save_I32
	movabsq	$PL_max_intro_pending, %rdi
	callq	Perl_save_I32
	xorq	%r13, %r13
	testq	$4, %rbx
	je	.LBB0_7
# BB#6:                                 # %if.then9
	movabsq	$PL_pad_reset_pending, %rdi
	callq	Perl_save_I32
	jmp	.LBB0_7
.LBB0_1:                                # %entry.if.end12_crit_edge
	andq	$1, %rbx
	movq	%rbx, %r13
.LBB0_7:                                # %if.end12
	callq	Perl_newAV
	movq	%rax, %r12
	callq	Perl_newAV
	movq	%rax, %rbx
	callq	Perl_newAV
	movq	%rax, %r14
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	testq	%r15, %r13
	je	.LBB0_9
# BB#8:                                 # %if.then17
	callq	Perl_newAV
	movq	%rbx, 8(%rsp)           # 8-byte Spill
	movq	%rax, %rbx
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	Perl_av_extend
	xorq	%rsi, %rsi
	movq	%r14, %rdi
	movq	%rbx, %rdx
	callq	Perl_av_store
	movq	(%rbx), %rax
	movq	8(%rsp), %rbx           # 8-byte Reload
	movq	$2, %rcx
	movb	%cl, 72(%rax)
	jmp	.LBB0_10
.LBB0_9:                                # %if.else
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	movq	%r14, %rdi
	callq	Perl_av_store
.LBB0_10:                               # %if.end21
	movq	(%r12), %rax
	movzbl	72(%rax), %ecx
	andq	$254, %rcx
	movb	%cl, 72(%rax)
	xorq	%rsi, %rsi
	movq	%r12, %rdi
	movq	%rbx, %rdx
	callq	Perl_av_store
	movq	$1, %rsi
	movq	%r12, %rdi
	movq	%r14, %rdx
	callq	Perl_av_store
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	movq	%r12, %rdi
	callq	Perl_av_fetch
	movq	(%rax), %rax
	movq	%rax, PL_comppad_name(%rip)
	movq	$1, %rsi
	xorq	%rdx, %rdx
	movq	%r12, %rdi
	callq	Perl_av_fetch
	movq	(%rax), %rax
	movq	%rax, PL_comppad(%rip)
	movq	(%rax), %rax
	movq	(%rax), %rax
	testq	%r15, %r13
	movq	%rax, PL_curpad(%rip)
	jne	.LBB0_12
# BB#11:                                # %if.then33
	movq	$0, PL_comppad_name_fill(%rip)
	movq	$0, PL_min_intro_pending(%rip)
	movq	$0, PL_padix(%rip)
.LBB0_12:                               # %if.end34
	movq	%r12, %rax
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp17:
	.size	Perl_pad_new, .Ltmp17-Perl_pad_new
	.cfi_endproc

	.globl	Perl_pad_undef
	.align	16, 0x90
	.type	Perl_pad_undef,@function
Perl_pad_undef:                         # @Perl_pad_undef
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
	subq	$48, %rsp
.Ltmp29:
	.cfi_def_cfa_offset 96
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
	movq	(%rdi), %rax
	movq	120(%rax), %r12
	testq	%r12, %r12
	je	.LBB1_39
# BB#1:                                 # %if.end
	cmpq	$0, 8(%r12)
	je	.LBB1_39
# BB#2:                                 # %if.end5
	movq	%rdi, 32(%rsp)          # 8-byte Spill
	movzbl	PL_dirty(%rip), %ecx
	testq	%rcx, %rcx
	jne	.LBB1_28
# BB#3:                                 # %if.then7
	movq	(%r12), %rcx
	movq	(%rcx), %rdx
	movq	(%rdx), %rcx
	movq	(%rcx), %rcx
	movq	8(%rcx), %rbx
	testq	%rbx, %rbx
	jle	.LBB1_28
# BB#4:                                 # %for.body.lr.ph
	movq	8(%rdx), %rdx
	movq	%rdx, 24(%rsp)          # 8-byte Spill
	movq	(%rdx), %rdx
	movq	128(%rax), %rsi
	movq	(%rcx), %r13
	movq	(%rdx), %rcx
	movq	%rcx, 40(%rsp)          # 8-byte Spill
	testq	%rsi, %rsi
	je	.LBB1_5
# BB#15:
	movq	%rsi, 16(%rsp)          # 8-byte Spill
	movq	144(%rax), %rax
	movq	%rax, 8(%rsp)           # 8-byte Spill
	movabsq	$PL_sv_undef, %rdx
	.align	16, 0x90
.LBB1_16:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	(%r13,%rbx,8), %rdi
	testq	%rdi, %rdi
	je	.LBB1_27
# BB#17:                                # %for.body
                                        #   in Loop: Header=BB1_16 Depth=1
	cmpq	%rdx, %rdi
	je	.LBB1_27
# BB#18:                                # %land.lhs.true22
                                        #   in Loop: Header=BB1_16 Depth=1
	movq	(%rdi), %rax
	movq	(%rax), %rax
	movzbl	(%rax), %eax
	cmpq	$38, %rax
	jne	.LBB1_27
# BB#19:                                # %if.then26
                                        #   in Loop: Header=BB1_16 Depth=1
	movq	%rdx, %r14
	movq	40(%rsp), %rax          # 8-byte Reload
	movq	(%rax,%rbx,8), %r15
	movq	$0, (%r13,%rbx,8)
	callq	Perl_sv_free
	movq	24(%rsp), %rax          # 8-byte Reload
	movq	8(%rax), %rax
	cmpq	$1, %rax
	ja	.LBB1_21
# BB#20:                                # %if.then32
                                        #   in Loop: Header=BB1_16 Depth=1
	movq	40(%rsp), %rax          # 8-byte Reload
	movq	$0, (%rax,%rbx,8)
	movq	%r15, %rdi
	callq	Perl_sv_free
.LBB1_21:                               # %if.end34
                                        #   in Loop: Header=BB1_16 Depth=1
	cmpq	$0, 8(%r15)
	movq	%r14, %rdx
	je	.LBB1_27
# BB#22:                                # %land.lhs.true37
                                        #   in Loop: Header=BB1_16 Depth=1
	movq	(%r15), %rax
	movq	128(%rax), %rcx
	movq	32(%rsp), %rsi          # 8-byte Reload
	cmpq	%rsi, %rcx
	jne	.LBB1_27
# BB#23:                                # %land.lhs.true44
                                        #   in Loop: Header=BB1_16 Depth=1
	movq	%rdx, %rdi
	movq	16(%rsp), %rsi          # 8-byte Reload
	movq	8(%rsi), %rcx
	testq	%rcx, %rcx
	jne	.LBB1_24
# BB#25:                                # %if.else
                                        #   in Loop: Header=BB1_16 Depth=1
	movq	$0, 128(%rax)
	jmp	.LBB1_26
.LBB1_24:                               # %land.rhs
                                        #   in Loop: Header=BB1_16 Depth=1
	movzbl	136(%rax), %edx
	andq	$64511, %rdx            # imm = 0xFBFF
	movw	%dx, 136(%rax)
	movq	%rsi, 128(%rax)
	movq	8(%rsp), %rdx           # 8-byte Reload
	movq	%rdx, 144(%rax)
	movq	%rsi, PL_Sv(%rip)
	incq	%rcx
	movq	%rcx, 8(%rsi)
.LBB1_26:                               # %for.inc
                                        #   in Loop: Header=BB1_16 Depth=1
	movq	%rdi, %rdx
	.align	16, 0x90
.LBB1_27:                               # %for.inc
                                        #   in Loop: Header=BB1_16 Depth=1
	decq	%rbx
	testq	%rbx, %rbx
	jg	.LBB1_16
	jmp	.LBB1_28
.LBB1_5:
	movabsq	$PL_sv_undef, %r14
	.align	16, 0x90
.LBB1_6:                                # %for.body.us
                                        # =>This Inner Loop Header: Depth=1
	movq	(%r13,%rbx,8), %rdi
	testq	%rdi, %rdi
	je	.LBB1_14
# BB#7:                                 # %for.body.us
                                        #   in Loop: Header=BB1_6 Depth=1
	cmpq	%r14, %rdi
	je	.LBB1_14
# BB#8:                                 # %land.lhs.true22.us
                                        #   in Loop: Header=BB1_6 Depth=1
	movq	(%rdi), %rax
	movq	(%rax), %rax
	movzbl	(%rax), %eax
	cmpq	$38, %rax
	jne	.LBB1_14
# BB#9:                                 # %if.then26.us
                                        #   in Loop: Header=BB1_6 Depth=1
	movq	40(%rsp), %rax          # 8-byte Reload
	movq	(%rax,%rbx,8), %r15
	movq	$0, (%r13,%rbx,8)
	callq	Perl_sv_free
	movq	24(%rsp), %rax          # 8-byte Reload
	movq	8(%rax), %rax
	cmpq	$1, %rax
	ja	.LBB1_11
# BB#10:                                # %if.then32.us
                                        #   in Loop: Header=BB1_6 Depth=1
	movq	40(%rsp), %rax          # 8-byte Reload
	movq	$0, (%rax,%rbx,8)
	movq	%r15, %rdi
	callq	Perl_sv_free
.LBB1_11:                               # %if.end34.us
                                        #   in Loop: Header=BB1_6 Depth=1
	cmpq	$0, 8(%r15)
	je	.LBB1_14
# BB#12:                                # %land.lhs.true37.us
                                        #   in Loop: Header=BB1_6 Depth=1
	movq	(%r15), %rax
	movq	128(%rax), %rcx
	movq	32(%rsp), %rdx          # 8-byte Reload
	cmpq	%rdx, %rcx
	jne	.LBB1_14
# BB#13:                                # %if.else.us
                                        #   in Loop: Header=BB1_6 Depth=1
	movq	$0, 128(%rax)
	.align	16, 0x90
.LBB1_14:                               # %for.inc.us
                                        #   in Loop: Header=BB1_6 Depth=1
	decq	%rbx
	testq	%rbx, %rbx
	jg	.LBB1_6
.LBB1_28:                               # %if.end63
	movq	(%r12), %rax
	movq	8(%rax), %rbx
	testq	%rbx, %rbx
	js	.LBB1_38
# BB#29:                                # %while.body.preheader
	incq	%rbx
	jmp	.LBB1_30
	.align	16, 0x90
.LBB1_37:                               # %while.cond.backedge.while.body_crit_edge
                                        #   in Loop: Header=BB1_30 Depth=1
	movq	(%r12), %rax
.LBB1_30:                               # %while.body
                                        # =>This Inner Loop Header: Depth=1
	movq	(%rax), %rax
	movq	-8(%rax,%rbx,8), %rdi
	testq	%rdi, %rdi
	je	.LBB1_36
# BB#31:                                # %if.end74
                                        #   in Loop: Header=BB1_30 Depth=1
	movq	PL_comppad_name(%rip), %rax
	cmpq	%rax, %rdi
	jne	.LBB1_33
# BB#32:                                # %if.then77
                                        #   in Loop: Header=BB1_30 Depth=1
	movq	$0, PL_comppad_name(%rip)
	jmp	.LBB1_35
	.align	16, 0x90
.LBB1_33:                               # %if.else78
                                        #   in Loop: Header=BB1_30 Depth=1
	movq	PL_comppad(%rip), %rax
	cmpq	%rax, %rdi
	jne	.LBB1_35
# BB#34:                                # %if.then81
                                        #   in Loop: Header=BB1_30 Depth=1
	movq	$0, PL_comppad(%rip)
	movq	$0, PL_curpad(%rip)
.LBB1_35:                               # %if.end83
                                        #   in Loop: Header=BB1_30 Depth=1
	callq	Perl_sv_free
.LBB1_36:                               # %while.cond.backedge
                                        #   in Loop: Header=BB1_30 Depth=1
	decq	%rbx
	testq	%rbx, %rbx
	jg	.LBB1_37
.LBB1_38:                               # %while.end
	movq	32(%rsp), %rbx          # 8-byte Reload
	movq	(%rbx), %rax
	movq	120(%rax), %rdi
	callq	Perl_sv_free
	movq	(%rbx), %rax
	movq	$0, 120(%rax)
.LBB1_39:                               # %return
	addq	$48, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp35:
	.size	Perl_pad_undef, .Ltmp35-Perl_pad_undef
	.cfi_endproc

	.globl	Perl_pad_add_name
	.align	16, 0x90
	.type	Perl_pad_add_name,@function
Perl_pad_add_name:                      # @Perl_pad_add_name
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
	subq	$16, %rsp
.Ltmp47:
	.cfi_def_cfa_offset 64
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
	movq	%rcx, 8(%rsp)           # 8-byte Spill
	movq	%rdx, %r12
	movq	%rsi, %r15
	movq	%rdi, %r14
	movq	$9, %rdi
	movq	$1024, %rsi             # imm = 0x400
	xorq	%rax, %rax
	callq	pad_alloc
	movq	%rax, %r13
	movq	$32, %rcx
	shlq	%cl, %r13
	movq	$32, %rcx
	sarq	%cl, %r13
	xorq	%rdi, %rdi
	callq	Perl_newSV
	movq	%rax, %rbx
	testq	%r12, %r12
	movq	$13, %rsi
	jne	.LBB2_5
# BB#1:                                 # %cond.false
	testq	%r15, %r15
	je	.LBB2_2
# BB#3:                                 # %cond.false
	movabsq	$1, %rsi
	jmp	.LBB2_4
.LBB2_2:
	movabsq	$0, %rsi
.LBB2_4:                                # %cond.false
	orq	$6, %rsi
.LBB2_5:                                # %cond.end
	movq	%rbx, %rdi
	callq	Perl_sv_upgrade
	movq	%rbx, %rdi
	movq	%r14, %rsi
	callq	Perl_sv_setpv
	testq	%r15, %r15
	je	.LBB2_7
# BB#6:                                 # %if.then
	orq	$1073741824, 16(%rbx)   # imm = 0x40000000
	movq	%r15, PL_Sv(%rip)
	incq	8(%r15)
	movq	(%rbx), %rax
	movq	%r15, 48(%rax)
.LBB2_7:                                # %if.end
	testq	%r12, %r12
	je	.LBB2_9
# BB#8:                                 # %land.end18
	movabsq	$2147483648, %rax       # imm = 0x80000000
	orq	%rax, 16(%rbx)
	movq	%r12, PL_Sv(%rip)
	incq	8(%r12)
	movq	(%rbx), %rax
	movq	%r12, 80(%rax)
.LBB2_9:                                # %if.end21
	movq	PL_comppad_name(%rip), %rdi
	movq	%r13, %rsi
	movq	%rbx, %rdx
	callq	Perl_av_store
	testq	$255, 8(%rsp)           # 8-byte Folded Reload
	je	.LBB2_11
# BB#10:                                # %if.then24
	orq	$1048576, 16(%rbx)      # imm = 0x100000
	jmp	.LBB2_19
.LBB2_11:                               # %if.else
	movq	(%rbx), %rax
	movabsq	$4741671816358002688, %rcx # imm = 0x41CDCD64FF800000
	movq	%rcx, 32(%rax)
	movq	$0, 24(%rax)
	cmpq	$0, PL_min_intro_pending(%rip)
	jne	.LBB2_13
# BB#12:                                # %if.then30
	movq	%r13, PL_min_intro_pending(%rip)
.LBB2_13:                               # %if.end31
	movq	%r13, PL_max_intro_pending(%rip)
	movzbl	(%r14), %eax
	cmpq	$37, %rax
	jne	.LBB2_14
# BB#16:                                # %if.then41
	movq	PL_comppad(%rip), %rbx
	callq	Perl_newHV
	jmp	.LBB2_17
.LBB2_14:                               # %if.end31
	cmpq	$64, %rax
	jne	.LBB2_18
# BB#15:                                # %if.then34
	movq	PL_comppad(%rip), %rbx
	callq	Perl_newAV
.LBB2_17:                               # %if.end45
	movq	%rbx, %rdi
	movq	%r13, %rsi
	movq	%rax, %rdx
	callq	Perl_av_store
.LBB2_18:                               # %if.end45
	movq	PL_curpad(%rip), %rax
	movq	(%rax,%r13,8), %rax
	orq	$1280, 16(%rax)         # imm = 0x500
.LBB2_19:                               # %if.end48
	movq	%r13, %rax
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp53:
	.size	Perl_pad_add_name, .Ltmp53-Perl_pad_add_name
	.cfi_endproc

	.globl	Perl_pad_alloc
	.align	16, 0x90
	.type	Perl_pad_alloc,@function
Perl_pad_alloc:                         # @Perl_pad_alloc
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp59:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp60:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp61:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp62:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp63:
	.cfi_def_cfa_offset 48
.Ltmp64:
	.cfi_offset %rbx, -40
.Ltmp65:
	.cfi_offset %r12, -32
.Ltmp66:
	.cfi_offset %r14, -24
.Ltmp67:
	.cfi_offset %r15, -16
	movq	PL_comppad(%rip), %rax
	movq	(%rax), %rax
	movq	(%rax), %rax
	movq	PL_curpad(%rip), %rcx
	movq	%rsi, %r14
	cmpq	%rcx, %rax
	je	.LBB3_2
# BB#1:                                 # %if.then
	movabsq	$.L.str, %rdi
	xorq	%rax, %rax
	callq	Perl_croak
.LBB3_2:                                # %if.end
	cmpq	$0, PL_pad_reset_pending(%rip)
	je	.LBB3_4
# BB#3:                                 # %if.then2
	xorq	%rax, %rax
	callq	pad_reset
.LBB3_4:                                # %if.end3
	testq	$1024, %r14             # imm = 0x400
	je	.LBB3_7
	.align	16, 0x90
.LBB3_5:                                # %do.body
                                        # =>This Inner Loop Header: Depth=1
	movq	PL_comppad(%rip), %rdi
	movq	(%rdi), %rax
	movq	8(%rax), %rsi
	incq	%rsi
	movq	$1, %rdx
	callq	Perl_av_fetch
	movq	(%rax), %rcx
	movq	16(%rcx), %rdx
	testq	$256, %rdx              # imm = 0x100
	jne	.LBB3_5
# BB#6:                                 # %do.end
	movq	PL_comppad(%rip), %rax
	movq	(%rax), %rsi
	leaq	8(%rsi), %rax
	jmp	.LBB3_13
.LBB3_7:                                # %if.else
	movq	PL_comppad_name(%rip), %rax
	movq	(%rax), %rax
	movq	(%rax), %r15
	movq	8(%rax), %rbx
	movabsq	$PL_sv_undef, %r12
	.align	16, 0x90
.LBB3_8:                                # %for.cond
                                        # =>This Inner Loop Header: Depth=1
	movq	PL_padix(%rip), %rax
	leaq	1(%rax), %rsi
	cmpq	%rbx, %rax
	movq	%rsi, PL_padix(%rip)
	jge	.LBB3_11
# BB#9:                                 # %land.lhs.true
                                        #   in Loop: Header=BB3_8 Depth=1
	movq	8(%r15,%rax,8), %rax
	testq	%rax, %rax
	je	.LBB3_11
# BB#10:                                # %land.lhs.true
                                        #   in Loop: Header=BB3_8 Depth=1
	cmpq	%r12, %rax
	jne	.LBB3_8
	.align	16, 0x90
.LBB3_11:                               # %if.end21
                                        #   in Loop: Header=BB3_8 Depth=1
	movq	PL_comppad(%rip), %rdi
	movq	$1, %rdx
	callq	Perl_av_fetch
	movq	(%rax), %rcx
	movq	16(%rcx), %rdx
	testq	$1536, %rdx             # imm = 0x600
	jne	.LBB3_8
# BB#12:                                # %if.end28.loopexit
	movq	PL_comppad(%rip), %rax
	movq	(%rax), %rsi
	movabsq	$PL_padix, %rax
.LBB3_13:                               # %if.end28
	movq	(%rax), %rax
	orq	%r14, %rdx
	movq	%rdx, 16(%rcx)
	movq	(%rsi), %rcx
	movq	%rcx, PL_curpad(%rip)
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.Ltmp68:
	.size	Perl_pad_alloc, .Ltmp68-Perl_pad_alloc
	.cfi_endproc

	.globl	Perl_pad_add_anon
	.align	16, 0x90
	.type	Perl_pad_add_anon,@function
Perl_pad_add_anon:                      # @Perl_pad_add_anon
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp73:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp74:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp75:
	.cfi_def_cfa_offset 32
.Ltmp76:
	.cfi_offset %rbx, -32
.Ltmp77:
	.cfi_offset %r14, -24
.Ltmp78:
	.cfi_offset %r15, -16
	movq	%rsi, %rbx
	movq	%rdi, %r14
	xorq	%rdi, %rdi
	callq	Perl_newSV
	movq	%rax, %r15
	movq	$6, %rsi
	movq	%r15, %rdi
	callq	Perl_sv_upgrade
	movabsq	$.L.str1, %rsi
	movq	$1, %rdx
	movq	%r15, %rdi
	callq	Perl_sv_setpvn
	movq	(%r15), %rax
	movq	$-1, 24(%rax)
	movabsq	$4607182418800017408, %rcx # imm = 0x3FF0000000000000
	movq	%rcx, 32(%rax)
	movq	$1024, %rsi             # imm = 0x400
	xorq	%rax, %rax
	movq	%rbx, %rdi
	callq	pad_alloc
	movq	%rax, %rbx
	movq	$32, %rcx
	movq	PL_comppad_name(%rip), %rdi
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	movq	%rbx, %rsi
	movq	%r15, %rdx
	callq	Perl_av_store
	movq	PL_comppad(%rip), %rdi
	movq	%rbx, %rsi
	movq	%r14, %rdx
	callq	Perl_av_store
	orq	$1280, 16(%r14)         # imm = 0x500
	movq	(%r14), %rax
	movq	128(%rax), %rdi
	testq	%rdi, %rdi
	je	.LBB4_2
# BB#1:                                 # %if.then
	movzbl	136(%rax), %ecx
	orq	$1024, %rcx             # imm = 0x400
	movw	%cx, 136(%rax)
	callq	Perl_sv_free
.LBB4_2:                                # %if.end
	movq	%rbx, %rax
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp79:
	.size	Perl_pad_add_anon, .Ltmp79-Perl_pad_add_anon
	.cfi_endproc

	.globl	Perl_pad_check_dup
	.align	16, 0x90
	.type	Perl_pad_check_dup,@function
Perl_pad_check_dup:                     # @Perl_pad_check_dup
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp86:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp87:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp88:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp89:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp90:
	.cfi_def_cfa_offset 48
	subq	$32, %rsp
.Ltmp91:
	.cfi_def_cfa_offset 80
.Ltmp92:
	.cfi_offset %rbx, -48
.Ltmp93:
	.cfi_offset %r12, -40
.Ltmp94:
	.cfi_offset %r13, -32
.Ltmp95:
	.cfi_offset %r14, -24
.Ltmp96:
	.cfi_offset %r15, -16
	movq	PL_curcop(%rip), %rax
	cmpq	$0, 88(%rax)
	movq	%rdx, 16(%rsp)          # 8-byte Spill
	movq	%rdi, 24(%rsp)          # 8-byte Spill
	je	.LBB5_4
# BB#1:                                 # %land.lhs.true
	movq	PL_curcop(%rip), %rax
	movq	88(%rax), %rax
	cmpq	$48, %rax
	je	.LBB5_4
# BB#2:                                 # %land.lhs.true3
	movq	PL_curcop(%rip), %rax
	movq	88(%rax), %rax
	cmpq	$24, %rax
	je	.LBB5_6
# BB#3:                                 # %lor.lhs.false
	movq	PL_curcop(%rip), %rax
	movq	88(%rax), %rax
	movq	(%rax), %rax
	movq	(%rax), %rax
	movzbl	3(%rax), %eax
	testq	$1, %rax
	jne	.LBB5_6
.LBB5_4:                                # %lor.lhs.false7
	movq	PL_curcop(%rip), %rax
	cmpq	$0, 88(%rax)
	jne	.LBB5_46
# BB#5:                                 # %land.lhs.true11
	movzbl	PL_dowarn(%rip), %eax
	testq	$1, %rax
	je	.LBB5_46
.LBB5_6:                                # %lor.lhs.false15
	movq	PL_comppad_name(%rip), %rax
	movq	(%rax), %rax
	movq	8(%rax), %rbx
	testq	%rbx, %rbx
	js	.LBB5_46
# BB#7:                                 # %if.end
	movq	PL_comppad_name_floor(%rip), %r14
	movq	(%rax), %r13
	cmpq	%r14, %rbx
	jle	.LBB5_8
# BB#9:                                 # %for.body.lr.ph
	testq	$255, %rsi
	movq	%rsi, 8(%rsp)           # 8-byte Spill
	je	.LBB5_10
# BB#18:
	movabsq	$PL_sv_undef, %r12
	movabsq	$2147483648, %rsi       # imm = 0x80000000
	.align	16, 0x90
.LBB5_19:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	(%r13,%rbx,8), %rax
	testq	%rax, %rax
	je	.LBB5_34
# BB#20:                                # %for.body
                                        #   in Loop: Header=BB5_19 Depth=1
	cmpq	%r12, %rax
	je	.LBB5_34
# BB#21:                                # %land.lhs.true29
                                        #   in Loop: Header=BB5_19 Depth=1
	movq	16(%rax), %rcx
	testq	$1048576, %rcx          # imm = 0x100000
	jne	.LBB5_34
# BB#22:                                # %land.lhs.true32
                                        #   in Loop: Header=BB5_19 Depth=1
	movq	(%rax), %rax
	movq	24(%rax), %r15
	testq	%r15, %r15
	je	.LBB5_24
# BB#23:                                # %land.lhs.true32
                                        #   in Loop: Header=BB5_19 Depth=1
	cmpq	$999999999, %r15        # imm = 0x3B9AC9FF
	jne	.LBB5_34
.LBB5_24:                               # %lor.lhs.false43
                                        #   in Loop: Header=BB5_19 Depth=1
	testq	%rsi, %rcx
	je	.LBB5_34
# BB#25:                                # %land.lhs.true47
                                        #   in Loop: Header=BB5_19 Depth=1
	movq	80(%rax), %rcx
	cmpq	%rdx, %rcx
	jne	.LBB5_34
# BB#26:                                # %land.lhs.true51
                                        #   in Loop: Header=BB5_19 Depth=1
	movq	(%rax), %rsi
	movq	24(%rsp), %rdi          # 8-byte Reload
	callq	strcmp
	movabsq	$2147483648, %rsi       # imm = 0x80000000
	movq	16(%rsp), %rdx          # 8-byte Reload
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB5_27
	.align	16, 0x90
.LBB5_34:                               # %for.inc
                                        #   in Loop: Header=BB5_19 Depth=1
	decq	%rbx
	cmpq	%r14, %rbx
	jg	.LBB5_19
	jmp	.LBB5_35
.LBB5_8:
	movq	%rsi, 8(%rsp)           # 8-byte Spill
	jmp	.LBB5_35
.LBB5_10:
	movabsq	$PL_sv_undef, %rsi
	.align	16, 0x90
.LBB5_11:                               # %for.body.us
                                        # =>This Inner Loop Header: Depth=1
	movq	(%r13,%rbx,8), %rax
	testq	%rax, %rax
	je	.LBB5_17
# BB#12:                                # %for.body.us
                                        #   in Loop: Header=BB5_11 Depth=1
	cmpq	%rsi, %rax
	je	.LBB5_17
# BB#13:                                # %land.lhs.true29.us
                                        #   in Loop: Header=BB5_11 Depth=1
	movzbl	18(%rax), %ecx
	testq	$16, %rcx
	jne	.LBB5_17
# BB#14:                                # %land.lhs.true32.us
                                        #   in Loop: Header=BB5_11 Depth=1
	movq	(%rax), %rax
	movq	24(%rax), %r15
	testq	%r15, %r15
	je	.LBB5_16
# BB#15:                                # %land.lhs.true32.us
                                        #   in Loop: Header=BB5_11 Depth=1
	cmpq	$999999999, %r15        # imm = 0x3B9AC9FF
	jne	.LBB5_17
.LBB5_16:                               # %land.lhs.true51.us
                                        #   in Loop: Header=BB5_11 Depth=1
	movq	(%rax), %rsi
	movq	24(%rsp), %rdi          # 8-byte Reload
	movq	%rdx, %r12
	callq	strcmp
	movabsq	$PL_sv_undef, %rsi
	movq	%r12, %rdx
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB5_27
	.align	16, 0x90
.LBB5_17:                               # %for.inc.us
                                        #   in Loop: Header=BB5_11 Depth=1
	decq	%rbx
	cmpq	%r14, %rbx
	jg	.LBB5_11
	jmp	.LBB5_35
.LBB5_27:                               # %if.then55
	movq	%rdx, %r14
	movq	8(%rsp), %rax           # 8-byte Reload
	testq	$255, %rax
	jne	.LBB5_28
# BB#29:                                # %if.then55
	movabsq	$.L.str4, %rdx
	jmp	.LBB5_30
.LBB5_28:
	movabsq	$.L.str3, %rdx
.LBB5_30:                               # %if.then55
	cmpq	$999999999, %r15        # imm = 0x3B9AC9FF
	je	.LBB5_31
# BB#32:                                # %if.then55
	movabsq	$.L.str6, %r8
	jmp	.LBB5_33
.LBB5_31:
	movabsq	$.L.str5, %r8
.LBB5_33:                               # %if.then55
	movabsq	$.L.str2, %rsi
	movq	$12, %rdi
	xorq	%rax, %rax
	movq	24(%rsp), %rcx          # 8-byte Reload
	callq	Perl_warner
	decq	%rbx
	movq	%r14, %rdx
.LBB5_35:                               # %for.end
	testq	$255, 8(%rsp)           # 8-byte Folded Reload
	je	.LBB5_46
# BB#36:
	movabsq	$PL_sv_undef, %r14
	movabsq	$2147483648, %r15       # imm = 0x80000000
	.align	16, 0x90
.LBB5_37:                               # %do.body
                                        # =>This Inner Loop Header: Depth=1
	movq	(%r13,%rbx,8), %rax
	testq	%rax, %rax
	je	.LBB5_45
# BB#38:                                # %do.body
                                        #   in Loop: Header=BB5_37 Depth=1
	cmpq	%r14, %rax
	je	.LBB5_45
# BB#39:                                # %land.lhs.true72
                                        #   in Loop: Header=BB5_37 Depth=1
	movq	16(%rax), %rcx
	testq	$1048576, %rcx          # imm = 0x100000
	jne	.LBB5_45
# BB#40:                                # %land.lhs.true76
                                        #   in Loop: Header=BB5_37 Depth=1
	movq	(%rax), %rax
	movq	24(%rax), %rsi
	testq	%rsi, %rsi
	je	.LBB5_42
# BB#41:                                # %land.lhs.true76
                                        #   in Loop: Header=BB5_37 Depth=1
	cmpq	$999999999, %rsi        # imm = 0x3B9AC9FF
	jne	.LBB5_45
.LBB5_42:                               # %land.lhs.true86
                                        #   in Loop: Header=BB5_37 Depth=1
	testq	%r15, %rcx
	je	.LBB5_45
# BB#43:                                # %land.lhs.true90
                                        #   in Loop: Header=BB5_37 Depth=1
	movq	80(%rax), %rcx
	cmpq	%rdx, %rcx
	jne	.LBB5_45
# BB#44:                                # %land.lhs.true95
                                        #   in Loop: Header=BB5_37 Depth=1
	movq	(%rax), %rsi
	movq	24(%rsp), %rdi          # 8-byte Reload
	movq	%rdx, %r12
	callq	strcmp
	movq	%r12, %rdx
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB5_47
	.align	16, 0x90
.LBB5_45:                               # %do.cond
                                        #   in Loop: Header=BB5_37 Depth=1
	decq	%rbx
	cmpq	$-1, %rbx
	jne	.LBB5_37
.LBB5_46:                               # %if.end105
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB5_47:                               # %if.then100
	movabsq	$.L.str7, %rsi
	movq	$12, %rdi
	xorq	%rax, %rax
	movq	24(%rsp), %rdx          # 8-byte Reload
	callq	Perl_warner
	movabsq	$.L.str8, %rsi
	movq	$12, %rdi
	xorq	%rax, %rax
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	Perl_warner  # TAILCALL
.Ltmp97:
	.size	Perl_pad_check_dup, .Ltmp97-Perl_pad_check_dup
	.cfi_endproc

	.globl	Perl_pad_findmy
	.align	16, 0x90
	.type	Perl_pad_findmy,@function
Perl_pad_findmy:                        # @Perl_pad_findmy
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp104:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp105:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp106:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp107:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp108:
	.cfi_def_cfa_offset 48
	subq	$32, %rsp
.Ltmp109:
	.cfi_def_cfa_offset 80
.Ltmp110:
	.cfi_offset %rbx, -48
.Ltmp111:
	.cfi_offset %r12, -40
.Ltmp112:
	.cfi_offset %r13, -32
.Ltmp113:
	.cfi_offset %r14, -24
.Ltmp114:
	.cfi_offset %r15, -16
	movq	PL_comppad_name(%rip), %rax
	movq	(%rax), %rax
	movq	8(%rax), %rbx
	movq	%rdi, 24(%rsp)          # 8-byte Spill
	testq	%rbx, %rbx
	jle	.LBB6_1
# BB#2:
	movq	$0, 16(%rsp)            # 8-byte Folded Spill
	movq	(%rax), %r13
	movq	PL_cop_seqmax(%rip), %r15
	movq	$0, 8(%rsp)             # 8-byte Folded Spill
	.align	16, 0x90
.LBB6_3:                                # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	(%r13,%rbx,8), %r14
	testq	%r14, %r14
	je	.LBB6_16
# BB#4:                                 # %for.body
                                        #   in Loop: Header=BB6_3 Depth=1
	movabsq	$PL_sv_undef, %rax
	cmpq	%rax, %r14
	je	.LBB6_16
# BB#5:                                 # %lor.lhs.false3
                                        #   in Loop: Header=BB6_3 Depth=1
	movq	%r15, %r12
	movq	(%r14), %r15
	movq	(%r15), %rdi
	movq	24(%rsp), %rsi          # 8-byte Reload
	callq	strcmp
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB6_6
# BB#7:                                 # %if.end
                                        #   in Loop: Header=BB6_3 Depth=1
	movzbl	18(%r14), %eax
	testq	$16, %rax
	jne	.LBB6_8
# BB#9:                                 # %if.else
                                        #   in Loop: Header=BB6_3 Depth=1
	movq	32(%r15), %rdi
	callq	Perl_cast_ulong
	movq	%r12, %r15
	cmpq	%rax, %r15
	jbe	.LBB6_12
# BB#10:                                # %land.lhs.true
                                        #   in Loop: Header=BB6_3 Depth=1
	movq	(%r14), %rax
	movq	24(%rax), %rax
	cmpq	%rax, %r15
	jbe	.LBB6_11
.LBB6_12:                               # %if.else14
                                        #   in Loop: Header=BB6_3 Depth=1
	movzbl	19(%r14), %eax
	testq	$128, %rax
	je	.LBB6_16
# BB#13:                                # %land.lhs.true18
                                        #   in Loop: Header=BB6_3 Depth=1
	movq	(%r14), %rax
	movq	32(%rax), %rdi
	callq	Perl_cast_ulong
	cmpq	$999999999, %rax        # imm = 0x3B9AC9FF
	movq	%rbx, %rax
	je	.LBB6_15
# BB#14:                                # %land.lhs.true18
                                        #   in Loop: Header=BB6_3 Depth=1
	movq	8(%rsp), %rax           # 8-byte Reload
.LBB6_15:                               # %land.lhs.true18
                                        #   in Loop: Header=BB6_3 Depth=1
	movq	%rax, 8(%rsp)           # 8-byte Spill
	jmp	.LBB6_16
	.align	16, 0x90
.LBB6_6:                                #   in Loop: Header=BB6_3 Depth=1
	movq	%r12, %r15
	jmp	.LBB6_16
.LBB6_8:                                #   in Loop: Header=BB6_3 Depth=1
	movq	%rbx, 16(%rsp)          # 8-byte Spill
	movq	%r12, %r15
	.align	16, 0x90
.LBB6_16:                               # %for.inc
                                        #   in Loop: Header=BB6_3 Depth=1
	decq	%rbx
	testq	%rbx, %rbx
	jg	.LBB6_3
# BB#17:                                # %for.end
	movq	16(%rsp), %rax          # 8-byte Reload
	testq	%rax, %rax
	movq	8(%rsp), %rbx           # 8-byte Reload
	jne	.LBB6_21
	jmp	.LBB6_18
.LBB6_1:
	xorq	%rbx, %rbx
.LBB6_18:                               # %if.end29
	movq	PL_compcv(%rip), %rdx
	xorq	%rsi, %rsi
	xorq	%rax, %rax
	movq	24(%rsp), %rdi          # 8-byte Reload
	callq	pad_findlex
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB6_20
# BB#19:                                # %if.then32
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	jmp	.LBB6_21
.LBB6_20:                               # %if.end33
	cmpq	$1, %rbx
	sbbq	%rax, %rax
	orq	%rbx, %rax
.LBB6_21:                               # %return
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB6_11:
	movq	%rbx, %rax
	jmp	.LBB6_21
.Ltmp115:
	.size	Perl_pad_findmy, .Ltmp115-Perl_pad_findmy
	.cfi_endproc

	.globl	Perl_pad_sv
	.align	16, 0x90
	.type	Perl_pad_sv,@function
Perl_pad_sv:                            # @Perl_pad_sv
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp118:
	.cfi_def_cfa_offset 16
.Ltmp119:
	.cfi_offset %rbx, -16
	movq	%rdi, %rbx
	testq	%rbx, %rbx
	jne	.LBB7_2
# BB#1:                                 # %if.then
	movabsq	$.L.str9, %rdi
	xorq	%rax, %rax
	callq	Perl_croak
.LBB7_2:                                # %if.end
	movq	PL_curpad(%rip), %rax
	movq	(%rax,%rbx,8), %rax
	popq	%rbx
	retq
.Ltmp120:
	.size	Perl_pad_sv, .Ltmp120-Perl_pad_sv
	.cfi_endproc

	.globl	Perl_pad_block_start
	.align	16, 0x90
	.type	Perl_pad_block_start,@function
Perl_pad_block_start:                   # @Perl_pad_block_start
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp123:
	.cfi_def_cfa_offset 16
.Ltmp124:
	.cfi_offset %rbx, -16
	movq	%rdi, %rbx
	movabsq	$PL_comppad_name_floor, %rdi
	callq	Perl_save_I32
	movq	PL_comppad_name(%rip), %rax
	movq	(%rax), %rax
	movq	8(%rax), %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rbx
	movq	%rax, PL_comppad_name_floor(%rip)
	je	.LBB8_2
# BB#1:                                 # %if.then
	movq	%rax, PL_comppad_name_fill(%rip)
.LBB8_2:                                # %if.end
	testq	%rax, %rax
	jns	.LBB8_4
# BB#3:                                 # %if.then1
	movq	$0, PL_comppad_name_floor(%rip)
.LBB8_4:                                # %if.end2
	movabsq	$PL_min_intro_pending, %rdi
	callq	Perl_save_I32
	movabsq	$PL_max_intro_pending, %rdi
	callq	Perl_save_I32
	movq	$0, PL_min_intro_pending(%rip)
	movabsq	$PL_comppad_name_fill, %rdi
	callq	Perl_save_I32
	movabsq	$PL_padix_floor, %rdi
	callq	Perl_save_I32
	movq	PL_padix(%rip), %rax
	movq	%rax, PL_padix_floor(%rip)
	movq	$0, PL_pad_reset_pending(%rip)
	popq	%rbx
	retq
.Ltmp125:
	.size	Perl_pad_block_start, .Ltmp125-Perl_pad_block_start
	.cfi_endproc

	.globl	Perl_intro_my
	.align	16, 0x90
	.type	Perl_intro_my,@function
Perl_intro_my:                          # @Perl_intro_my
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp131:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp132:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp133:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp134:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp135:
	.cfi_def_cfa_offset 48
.Ltmp136:
	.cfi_offset %rbx, -40
.Ltmp137:
	.cfi_offset %r12, -32
.Ltmp138:
	.cfi_offset %r14, -24
.Ltmp139:
	.cfi_offset %r15, -16
	movq	PL_min_intro_pending(%rip), %rbx
	testq	%rbx, %rbx
	je	.LBB9_1
# BB#2:                                 # %if.end
	movq	PL_max_intro_pending(%rip), %rcx
	cmpq	%rcx, %rbx
	jg	.LBB9_10
# BB#3:                                 # %for.body.preheader
	movq	PL_comppad_name(%rip), %rax
	movq	(%rax), %rax
	movq	(%rax), %r14
	decq	%rbx
	movabsq	$PL_sv_undef, %r15
	.align	16, 0x90
.LBB9_4:                                # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	8(%r14,%rbx,8), %rax
	testq	%rax, %rax
	je	.LBB9_9
# BB#5:                                 # %for.body
                                        #   in Loop: Header=BB9_4 Depth=1
	cmpq	%r15, %rax
	je	.LBB9_9
# BB#6:                                 # %land.lhs.true3
                                        #   in Loop: Header=BB9_4 Depth=1
	movzbl	18(%rax), %edx
	testq	$16, %rdx
	jne	.LBB9_9
# BB#7:                                 # %land.lhs.true5
                                        #   in Loop: Header=BB9_4 Depth=1
	movq	(%rax), %r12
	cmpq	$0, 24(%r12)
	jne	.LBB9_9
# BB#8:                                 # %if.then8
                                        #   in Loop: Header=BB9_4 Depth=1
	movq	$999999999, 24(%r12)    # imm = 0x3B9AC9FF
	movq	PL_cop_seqmax(%rip), %rdi
	callq	int64_to_float64
	movq	%rax, 32(%r12)
	movq	PL_max_intro_pending(%rip), %rcx
	.align	16, 0x90
.LBB9_9:                                # %for.inc
                                        #   in Loop: Header=BB9_4 Depth=1
	incq	%rbx
	cmpq	%rcx, %rbx
	jl	.LBB9_4
.LBB9_10:                               # %for.end
	movq	PL_cop_seqmax(%rip), %rax
	movq	$0, PL_min_intro_pending(%rip)
	movq	%rcx, PL_comppad_name_fill(%rip)
	leaq	1(%rax), %rcx
	movq	%rcx, PL_cop_seqmax(%rip)
	jmp	.LBB9_11
.LBB9_1:                                # %if.then
	movq	PL_cop_seqmax(%rip), %rax
.LBB9_11:                               # %return
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.Ltmp140:
	.size	Perl_intro_my, .Ltmp140-Perl_intro_my
	.cfi_endproc

	.globl	Perl_pad_leavemy
	.align	16, 0x90
	.type	Perl_pad_leavemy,@function
Perl_pad_leavemy:                       # @Perl_pad_leavemy
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp146:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp147:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp148:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp149:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp150:
	.cfi_def_cfa_offset 48
.Ltmp151:
	.cfi_offset %rbx, -40
.Ltmp152:
	.cfi_offset %r12, -32
.Ltmp153:
	.cfi_offset %r14, -24
.Ltmp154:
	.cfi_offset %r15, -16
	movq	PL_comppad_name(%rip), %rax
	movq	(%rax), %rcx
	movq	(%rcx), %r15
	movq	PL_min_intro_pending(%rip), %rdx
	movq	PL_comppad_name_fill(%rip), %rax
	movq	$0, PL_pad_reset_pending(%rip)
	testq	%rdx, %rdx
	je	.LBB10_14
# BB#1:                                 # %entry
	cmpq	%rdx, %rax
	jge	.LBB10_14
# BB#2:                                 # %if.then
	movq	PL_max_intro_pending(%rip), %rbx
	cmpq	%rdx, %rbx
	jl	.LBB10_14
# BB#3:                                 # %for.body.preheader
	incq	%rbx
	movabsq	$PL_sv_undef, %r12
	movabsq	$.L.str10, %r14
	.align	16, 0x90
.LBB10_4:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	-8(%r15,%rbx,8), %rdx
	testq	%rdx, %rdx
	je	.LBB10_12
# BB#5:                                 # %for.body
                                        #   in Loop: Header=BB10_4 Depth=1
	cmpq	%r12, %rdx
	je	.LBB10_12
# BB#6:                                 # %land.lhs.true5
                                        #   in Loop: Header=BB10_4 Depth=1
	movzbl	18(%rdx), %eax
	testq	$16, %rax
	jne	.LBB10_12
# BB#7:                                 # %land.lhs.true7
                                        #   in Loop: Header=BB10_4 Depth=1
	movq	PL_curcop(%rip), %rax
	cmpq	$0, 88(%rax)
	je	.LBB10_11
# BB#8:                                 # %lor.lhs.false
                                        #   in Loop: Header=BB10_4 Depth=1
	movq	PL_curcop(%rip), %rax
	movq	88(%rax), %rax
	cmpq	$24, %rax
	je	.LBB10_11
# BB#9:                                 # %lor.lhs.false11
                                        #   in Loop: Header=BB10_4 Depth=1
	movq	PL_curcop(%rip), %rax
	movq	88(%rax), %rax
	cmpq	$48, %rax
	je	.LBB10_12
# BB#10:                                # %land.lhs.true14
                                        #   in Loop: Header=BB10_4 Depth=1
	movq	PL_curcop(%rip), %rax
	movq	88(%rax), %rax
	movq	(%rax), %rax
	movq	(%rax), %rax
	movzbl	6(%rax), %eax
	testq	$1, %rax
	je	.LBB10_12
	.align	16, 0x90
.LBB10_11:                              # %if.then20
                                        #   in Loop: Header=BB10_4 Depth=1
	movq	$24, %rdi
	xorq	%rax, %rax
	movq	%r14, %rsi
	callq	Perl_warner
.LBB10_12:                              # %for.inc
                                        #   in Loop: Header=BB10_4 Depth=1
	movq	PL_min_intro_pending(%rip), %rax
	decq	%rbx
	cmpq	%rax, %rbx
	jg	.LBB10_4
# BB#13:                                # %for.cond.if.end21.loopexit_crit_edge
	movq	PL_comppad_name(%rip), %rax
	movq	(%rax), %rcx
	movq	PL_comppad_name_fill(%rip), %rax
.LBB10_14:                              # %if.end21
	movq	8(%rcx), %rcx
	cmpq	%rax, %rcx
	jle	.LBB10_22
# BB#15:
	movabsq	$PL_sv_undef, %rdx
	.align	16, 0x90
.LBB10_16:                              # %for.body26
                                        # =>This Inner Loop Header: Depth=1
	movq	(%r15,%rcx,8), %rsi
	testq	%rsi, %rsi
	je	.LBB10_21
# BB#17:                                # %for.body26
                                        #   in Loop: Header=BB10_16 Depth=1
	cmpq	%rdx, %rsi
	je	.LBB10_21
# BB#18:                                # %land.lhs.true32
                                        #   in Loop: Header=BB10_16 Depth=1
	movzbl	18(%rsi), %edi
	testq	$16, %rdi
	jne	.LBB10_21
# BB#19:                                # %land.lhs.true36
                                        #   in Loop: Header=BB10_16 Depth=1
	movq	(%rsi), %rsi
	movq	24(%rsi), %rdi
	cmpq	$999999999, %rdi        # imm = 0x3B9AC9FF
	jne	.LBB10_21
# BB#20:                                # %if.then40
                                        #   in Loop: Header=BB10_16 Depth=1
	movq	PL_cop_seqmax(%rip), %rax
	movq	%rax, 24(%rsi)
	movq	PL_comppad_name_fill(%rip), %rax
	.align	16, 0x90
.LBB10_21:                              # %for.inc44
                                        #   in Loop: Header=BB10_16 Depth=1
	decq	%rcx
	cmpq	%rax, %rcx
	jg	.LBB10_16
.LBB10_22:                              # %for.end46
	incq	PL_cop_seqmax(%rip)
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.Ltmp155:
	.size	Perl_pad_leavemy, .Ltmp155-Perl_pad_leavemy
	.cfi_endproc

	.globl	Perl_pad_swipe
	.align	16, 0x90
	.type	Perl_pad_swipe,@function
Perl_pad_swipe:                         # @Perl_pad_swipe
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp159:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp160:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp161:
	.cfi_def_cfa_offset 32
.Ltmp162:
	.cfi_offset %rbx, -24
.Ltmp163:
	.cfi_offset %r14, -16
	movq	PL_curpad(%rip), %rax
	movq	%rsi, %r14
	movq	%rdi, %rbx
	testq	%rax, %rax
	je	.LBB11_11
# BB#1:                                 # %if.end
	movq	PL_comppad(%rip), %rcx
	movq	(%rcx), %rcx
	movq	(%rcx), %rcx
	cmpq	%rax, %rcx
	je	.LBB11_3
# BB#2:                                 # %if.then1
	movabsq	$.L.str11, %rdi
	xorq	%rax, %rax
	callq	Perl_croak
.LBB11_3:                               # %if.end2
	testq	%rbx, %rbx
	jne	.LBB11_5
# BB#4:                                 # %if.then4
	movabsq	$.L.str12, %rdi
	xorq	%rax, %rax
	callq	Perl_croak
.LBB11_5:                               # %if.end5
	movq	PL_curpad(%rip), %rax
	movq	(%rax,%rbx,8), %rdi
	testq	%rdi, %rdi
	je	.LBB11_7
# BB#6:                                 # %if.then7
	andq	$-513, 16(%rdi)         # imm = 0xFFFFFFFFFFFFFDFF
.LBB11_7:                               # %if.end9
	testq	$255, %r14
	je	.LBB11_9
# BB#8:                                 # %if.then11
	callq	Perl_sv_free
.LBB11_9:                               # %if.end13
	xorq	%rdi, %rdi
	callq	Perl_newSV
	movq	PL_curpad(%rip), %rcx
	movq	%rax, (%rcx,%rbx,8)
	movq	PL_curpad(%rip), %rax
	movq	(%rax,%rbx,8), %rax
	orq	$768, 16(%rax)          # imm = 0x300
	movq	PL_padix(%rip), %rax
	cmpq	%rbx, %rax
	jle	.LBB11_11
# BB#10:                                # %if.then18
	decq	%rbx
	movq	%rbx, PL_padix(%rip)
.LBB11_11:                              # %if.end19
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp164:
	.size	Perl_pad_swipe, .Ltmp164-Perl_pad_swipe
	.cfi_endproc

	.globl	Perl_pad_reset
	.align	16, 0x90
	.type	Perl_pad_reset,@function
Perl_pad_reset:                         # @Perl_pad_reset
	.cfi_startproc
# BB#0:                                 # %entry
	movq	$0, PL_pad_reset_pending(%rip)
	retq
.Ltmp165:
	.size	Perl_pad_reset, .Ltmp165-Perl_pad_reset
	.cfi_endproc

	.globl	Perl_pad_tidy
	.align	16, 0x90
	.type	Perl_pad_tidy,@function
Perl_pad_tidy:                          # @Perl_pad_tidy
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp172:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp173:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp174:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp175:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp176:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp177:
	.cfi_def_cfa_offset 64
.Ltmp178:
	.cfi_offset %rbx, -48
.Ltmp179:
	.cfi_offset %r12, -40
.Ltmp180:
	.cfi_offset %r13, -32
.Ltmp181:
	.cfi_offset %r14, -24
.Ltmp182:
	.cfi_offset %r15, -16
	movq	PL_comppad_name(%rip), %rax
	movq	PL_comppad(%rip), %rcx
	movq	(%rax), %rdx
	movq	(%rcx), %rcx
	movq	8(%rdx), %rdx
	movq	8(%rcx), %rsi
	movq	%rdi, %rbx
	cmpq	%rsi, %rdx
	jge	.LBB13_2
# BB#1:                                 # %if.then
	xorq	%rdx, %rdx
	movq	%rax, %rdi
	callq	Perl_av_store
.LBB13_2:                               # %if.end
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %rbx
	je	.LBB13_17
# BB#3:                                 # %if.end
	andq	%rax, %rbx
	cmpq	$2, %rbx
	jne	.LBB13_4
# BB#26:                                # %if.then82
	movq	PL_comppad(%rip), %rax
	movq	(%rax), %rax
	movq	8(%rax), %rax
	testq	%rax, %rax
	je	.LBB13_35
# BB#27:                                # %for.body88.lr.ph
	movq	PL_curpad(%rip), %rcx
	movabsq	$PL_sv_undef, %rdx
	movabsq	$PL_sv_yes, %rsi
	movabsq	$PL_sv_no, %r9
	movabsq	$PL_sv_placeholder, %r8
	.align	16, 0x90
.LBB13_28:                              # %for.body88
                                        # =>This Inner Loop Header: Depth=1
	movq	(%rcx,%rax,8), %rbx
	movq	16(%rbx), %rdi
	testq	$1024, %rdi             # imm = 0x400
	jne	.LBB13_34
# BB#29:                                # %for.body88
                                        #   in Loop: Header=BB13_28 Depth=1
	cmpq	%rdx, %rbx
	je	.LBB13_34
# BB#30:                                # %for.body88
                                        #   in Loop: Header=BB13_28 Depth=1
	cmpq	%rsi, %rbx
	je	.LBB13_34
# BB#31:                                # %for.body88
                                        #   in Loop: Header=BB13_28 Depth=1
	cmpq	%r9, %rbx
	je	.LBB13_34
# BB#32:                                # %for.body88
                                        #   in Loop: Header=BB13_28 Depth=1
	cmpq	%r8, %rbx
	je	.LBB13_34
# BB#33:                                # %if.then109
                                        #   in Loop: Header=BB13_28 Depth=1
	orq	$768, %rdi              # imm = 0x300
	movq	%rdi, 16(%rbx)
.LBB13_34:                              # %for.inc114
                                        #   in Loop: Header=BB13_28 Depth=1
	decq	%rax
	jne	.LBB13_28
	jmp	.LBB13_35
.LBB13_17:                              # %if.then44
	callq	Perl_newAV
	movq	%rax, %rbx
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	Perl_av_extend
	movq	PL_comppad(%rip), %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	callq	Perl_av_store
	movq	(%rbx), %rax
	movq	$2, %rcx
	movb	%cl, 72(%rax)
	movq	PL_comppad(%rip), %rax
	movq	(%rax), %rax
	movq	8(%rax), %rax
	testq	%rax, %rax
	je	.LBB13_35
# BB#18:                                # %for.body50.lr.ph
	movq	PL_curpad(%rip), %rcx
	movabsq	$PL_sv_undef, %rdx
	movabsq	$PL_sv_yes, %rsi
	movabsq	$PL_sv_no, %r9
	movabsq	$PL_sv_placeholder, %r8
	.align	16, 0x90
.LBB13_19:                              # %for.body50
                                        # =>This Inner Loop Header: Depth=1
	movq	(%rcx,%rax,8), %rbx
	cmpq	%rdx, %rbx
	je	.LBB13_25
# BB#20:                                # %for.body50
                                        #   in Loop: Header=BB13_19 Depth=1
	cmpq	%rsi, %rbx
	je	.LBB13_25
# BB#21:                                # %for.body50
                                        #   in Loop: Header=BB13_19 Depth=1
	cmpq	%r9, %rbx
	je	.LBB13_25
# BB#22:                                # %for.body50
                                        #   in Loop: Header=BB13_19 Depth=1
	cmpq	%r8, %rbx
	je	.LBB13_25
# BB#23:                                # %if.end67
                                        #   in Loop: Header=BB13_19 Depth=1
	movq	16(%rbx), %rdi
	testq	$1024, %rdi             # imm = 0x400
	jne	.LBB13_25
# BB#24:                                # %if.then72
                                        #   in Loop: Header=BB13_19 Depth=1
	orq	$768, %rdi              # imm = 0x300
	movq	%rdi, 16(%rbx)
.LBB13_25:                              # %for.inc76
                                        #   in Loop: Header=BB13_19 Depth=1
	decq	%rax
	jne	.LBB13_19
	jmp	.LBB13_35
.LBB13_4:                               # %if.end
	cmpq	$1, %rbx
	jne	.LBB13_35
# BB#5:                                 # %if.then6
	movq	PL_comppad(%rip), %rax
	movq	(%rax), %rax
	movq	8(%rax), %rbx
	testq	%rbx, %rbx
	je	.LBB13_35
# BB#6:
	movq	PL_comppad_name(%rip), %rax
	movq	(%rax), %rax
	movq	(%rax), %rax
	movq	%rax, 8(%rsp)           # 8-byte Spill
	movabsq	$PL_sv_undef, %r15
	movabsq	$PL_sv_yes, %rdx
	movabsq	$PL_sv_no, %rsi
	movabsq	$PL_sv_placeholder, %r13
	.align	16, 0x90
.LBB13_7:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	PL_curpad(%rip), %rax
	movq	(%rax,%rbx,8), %rdi
	cmpq	%r15, %rdi
	je	.LBB13_16
# BB#8:                                 # %for.body
                                        #   in Loop: Header=BB13_7 Depth=1
	cmpq	%rdx, %rdi
	je	.LBB13_16
# BB#9:                                 # %for.body
                                        #   in Loop: Header=BB13_7 Depth=1
	cmpq	%rsi, %rdi
	je	.LBB13_16
# BB#10:                                # %for.body
                                        #   in Loop: Header=BB13_7 Depth=1
	cmpq	%r13, %rdi
	je	.LBB13_16
# BB#11:                                # %if.end21
                                        #   in Loop: Header=BB13_7 Depth=1
	movq	8(%rsp), %rax           # 8-byte Reload
	movq	(%rax,%rbx,8), %rax
	testq	%rax, %rax
	je	.LBB13_15
# BB#12:                                # %if.end21
                                        #   in Loop: Header=BB13_7 Depth=1
	cmpq	%r15, %rax
	je	.LBB13_15
# BB#13:                                # %land.lhs.true25
                                        #   in Loop: Header=BB13_7 Depth=1
	movzbl	18(%rax), %ecx
	testq	$16, %rcx
	jne	.LBB13_16
# BB#14:                                # %lor.lhs.false26
                                        #   in Loop: Header=BB13_7 Depth=1
	movq	(%rax), %rax
	movq	(%rax), %rax
	movzbl	(%rax), %eax
	cmpq	$38, %rax
	je	.LBB13_16
	.align	16, 0x90
.LBB13_15:                              # %if.then30
                                        #   in Loop: Header=BB13_7 Depth=1
	movq	%rsi, %r12
	movq	%rdx, %r14
	callq	Perl_sv_free
	movq	%r14, %rdx
	movq	%r12, %rsi
	movq	PL_curpad(%rip), %rax
	movq	$0, (%rax,%rbx,8)
.LBB13_16:                              # %for.inc
                                        #   in Loop: Header=BB13_7 Depth=1
	decq	%rbx
	jne	.LBB13_7
.LBB13_35:                              # %if.end118
	movq	PL_comppad(%rip), %rax
	movq	(%rax), %rax
	movq	(%rax), %rax
	movq	%rax, PL_curpad(%rip)
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp183:
	.size	Perl_pad_tidy, .Ltmp183-Perl_pad_tidy
	.cfi_endproc

	.globl	Perl_pad_free
	.align	16, 0x90
	.type	Perl_pad_free,@function
Perl_pad_free:                          # @Perl_pad_free
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp186:
	.cfi_def_cfa_offset 16
.Ltmp187:
	.cfi_offset %rbx, -16
	movq	PL_curpad(%rip), %rax
	movq	%rdi, %rbx
	testq	%rax, %rax
	je	.LBB14_10
# BB#1:                                 # %if.end
	movq	PL_comppad(%rip), %rcx
	movq	(%rcx), %rcx
	movq	(%rcx), %rcx
	cmpq	%rax, %rcx
	je	.LBB14_3
# BB#2:                                 # %if.then1
	movabsq	$.L.str13, %rdi
	xorq	%rax, %rax
	callq	Perl_croak
.LBB14_3:                               # %if.end2
	testq	%rbx, %rbx
	jne	.LBB14_5
# BB#4:                                 # %if.then4
	movabsq	$.L.str14, %rdi
	xorq	%rax, %rax
	callq	Perl_croak
.LBB14_5:                               # %if.end5
	movq	PL_curpad(%rip), %rax
	movq	(%rax,%rbx,8), %rax
	testq	%rax, %rax
	je	.LBB14_8
# BB#6:                                 # %if.end5
	movabsq	$PL_sv_undef, %rcx
	cmpq	%rcx, %rax
	je	.LBB14_8
# BB#7:                                 # %if.then9
	andq	$-513, 16(%rax)         # imm = 0xFFFFFFFFFFFFFDFF
.LBB14_8:                               # %if.end11
	movq	PL_padix(%rip), %rax
	cmpq	%rbx, %rax
	jle	.LBB14_10
# BB#9:                                 # %if.then13
	decq	%rbx
	movq	%rbx, PL_padix(%rip)
.LBB14_10:                              # %if.end14
	popq	%rbx
	retq
.Ltmp188:
	.size	Perl_pad_free, .Ltmp188-Perl_pad_free
	.cfi_endproc

	.globl	Perl_do_dump_pad
	.align	16, 0x90
	.type	Perl_do_dump_pad,@function
Perl_do_dump_pad:                       # @Perl_do_dump_pad
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp195:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp196:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp197:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp198:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp199:
	.cfi_def_cfa_offset 48
	subq	$80, %rsp
.Ltmp200:
	.cfi_def_cfa_offset 128
.Ltmp201:
	.cfi_offset %rbx, -48
.Ltmp202:
	.cfi_offset %r12, -40
.Ltmp203:
	.cfi_offset %r13, -32
.Ltmp204:
	.cfi_offset %r14, -24
.Ltmp205:
	.cfi_offset %r15, -16
	movq	%rcx, 40(%rsp)          # 8-byte Spill
	movq	%rdx, %r14
	movq	%rsi, 56(%rsp)          # 8-byte Spill
	movq	%rdi, 64(%rsp)          # 8-byte Spill
	testq	%r14, %r14
	je	.LBB15_23
# BB#1:                                 # %if.end
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	movq	%r14, %rdi
	callq	Perl_av_fetch
	movq	(%rax), %r15
	movq	%r15, 72(%rsp)          # 8-byte Spill
	movq	$1, %rsi
	xorq	%rdx, %rdx
	movq	$1, %rbx
	movq	%r14, %rdi
	callq	Perl_av_fetch
	movq	(%rax), %r9
	movq	(%r15), %rax
	movq	(%r9), %rcx
	movq	(%rax), %r12
	movq	(%rcx), %rcx
	movq	%rcx, 48(%rsp)          # 8-byte Spill
	movabsq	$.L.str15, %rdx
	xorq	%rax, %rax
	movq	%rcx, (%rsp)
	movq	64(%rsp), %rdi          # 8-byte Reload
	movq	56(%rsp), %rsi          # 8-byte Reload
	movq	%r15, %rcx
	movq	%r12, %r8
	callq	Perl_dump_indent
	movq	(%r15), %rax
	cmpq	$0, 8(%rax)
	jle	.LBB15_23
# BB#2:                                 # %for.body.lr.ph
	incq	64(%rsp)                # 8-byte Folded Spill
	movabsq	$0, %r14
	.align	16, 0x90
.LBB15_3:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	(%r12,%rbx,8), %rax
	testq	%rax, %rax
	movq	%r14, %rcx
	je	.LBB15_5
# BB#4:                                 # %for.body
                                        #   in Loop: Header=BB15_3 Depth=1
	movabsq	$1, %rcx
.LBB15_5:                               # %for.body
                                        #   in Loop: Header=BB15_3 Depth=1
	movabsq	$PL_sv_undef, %rdx
	cmpq	%rdx, %rax
	movq	%r14, %rdx
	jne	.LBB15_7
# BB#6:                                 # %for.body
                                        #   in Loop: Header=BB15_3 Depth=1
	movabsq	$1, %rdx
.LBB15_7:                               # %for.body
                                        #   in Loop: Header=BB15_3 Depth=1
	xorq	%r13, %r13
	testq	%rdx, %rcx
	jne	.LBB15_9
# BB#8:                                 # %for.body
                                        #   in Loop: Header=BB15_3 Depth=1
	movq	%rax, %r13
.LBB15_9:                               # %for.body
                                        #   in Loop: Header=BB15_3 Depth=1
	testq	%r13, %r13
	jne	.LBB15_10
# BB#17:                                # %if.else35
                                        #   in Loop: Header=BB15_3 Depth=1
	movq	40(%rsp), %rax          # 8-byte Reload
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB15_22
# BB#18:                                # %if.then37
                                        #   in Loop: Header=BB15_3 Depth=1
	movq	48(%rsp), %rax          # 8-byte Reload
	movq	(%rax,%rbx,8), %r8
	xorq	%r9, %r9
	testq	%r8, %r8
	je	.LBB15_20
# BB#19:                                # %cond.true43
                                        #   in Loop: Header=BB15_3 Depth=1
	movq	8(%r8), %r9
.LBB15_20:                              # %cond.end47
                                        #   in Loop: Header=BB15_3 Depth=1
	xorq	%rax, %rax
	movq	64(%rsp), %rdi          # 8-byte Reload
	movq	56(%rsp), %rsi          # 8-byte Reload
	movabsq	$.L.str18, %rdx
	movq	%rbx, %rcx
	jmp	.LBB15_21
	.align	16, 0x90
.LBB15_10:                              # %if.then10
                                        #   in Loop: Header=BB15_3 Depth=1
	movzbl	18(%r13), %eax
	movq	48(%rsp), %rcx          # 8-byte Reload
	movq	(%rcx,%rbx,8), %r15
	testq	$16, %rax
	jne	.LBB15_11
# BB#14:                                # %if.else
                                        #   in Loop: Header=BB15_3 Depth=1
	xorq	%rax, %rax
	testq	%r15, %r15
	je	.LBB15_16
# BB#15:                                # %cond.true23
                                        #   in Loop: Header=BB15_3 Depth=1
	movq	8(%r15), %rax
.LBB15_16:                              # %cond.end27
                                        #   in Loop: Header=BB15_3 Depth=1
	movq	%rax, 32(%rsp)          # 8-byte Spill
	movq	(%r13), %rax
	movq	32(%rax), %rdi
	callq	Perl_cast_ulong
	movq	(%r13), %rcx
	movq	(%rcx), %rdx
	movq	24(%rcx), %rsi
	movq	%rax, %rcx
	movq	%rdx, 16(%rsp)
	movq	%rsi, 8(%rsp)
	xorq	%rax, %rax
	movq	%rcx, (%rsp)
	movq	64(%rsp), %rdi          # 8-byte Reload
	movq	56(%rsp), %rsi          # 8-byte Reload
	movabsq	$.L.str17, %rdx
	movq	%rbx, %rcx
	movq	%r15, %r8
	movq	32(%rsp), %r9           # 8-byte Reload
	jmp	.LBB15_21
	.align	16, 0x90
.LBB15_11:                              # %if.then12
                                        #   in Loop: Header=BB15_3 Depth=1
	xorq	%r9, %r9
	testq	%r15, %r15
	je	.LBB15_13
# BB#12:                                # %cond.true
                                        #   in Loop: Header=BB15_3 Depth=1
	movq	8(%r15), %r9
.LBB15_13:                              # %cond.end
                                        #   in Loop: Header=BB15_3 Depth=1
	movq	(%r13), %rax
	movq	(%rax), %rcx
	xorq	%rax, %rax
	movq	%rcx, (%rsp)
	movq	64(%rsp), %rdi          # 8-byte Reload
	movq	56(%rsp), %rsi          # 8-byte Reload
	movabsq	$.L.str16, %rdx
	movq	%rbx, %rcx
	movq	%r15, %r8
.LBB15_21:                              # %for.inc
                                        #   in Loop: Header=BB15_3 Depth=1
	callq	Perl_dump_indent
.LBB15_22:                              # %for.inc
                                        #   in Loop: Header=BB15_3 Depth=1
	movq	72(%rsp), %rax          # 8-byte Reload
	movq	(%rax), %rax
	movq	8(%rax), %rax
	cmpq	%rax, %rbx
	leaq	1(%rbx), %rbx
	jl	.LBB15_3
.LBB15_23:                              # %for.end
	addq	$80, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp206:
	.size	Perl_do_dump_pad, .Ltmp206-Perl_do_dump_pad
	.cfi_endproc

	.globl	Perl_cv_clone
	.align	16, 0x90
	.type	Perl_cv_clone,@function
Perl_cv_clone:                          # @Perl_cv_clone
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rax
.Ltmp208:
	.cfi_def_cfa_offset 16
	movq	(%rdi), %rax
	movq	128(%rax), %rsi
	xorq	%rax, %rax
	callq	cv_clone2
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	popq	%rdx
	retq
.Ltmp209:
	.size	Perl_cv_clone, .Ltmp209-Perl_cv_clone
	.cfi_endproc

	.globl	Perl_pad_fixup_inner_anons
	.align	16, 0x90
	.type	Perl_pad_fixup_inner_anons,@function
Perl_pad_fixup_inner_anons:             # @Perl_pad_fixup_inner_anons
	.cfi_startproc
# BB#0:                                 # %entry
	movq	(%rdi), %rax
	movq	(%rax), %rsi
	movq	(%rsi), %rax
	movq	(%rax), %rcx
	movq	8(%rcx), %rax
	testq	%rax, %rax
	jle	.LBB17_7
# BB#1:
	movq	8(%rsi), %rsi
	movq	(%rsi), %rsi
	movq	(%rcx), %rcx
	movq	(%rsi), %r8
	movabsq	$PL_sv_undef, %rdi
	.align	16, 0x90
.LBB17_2:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	(%rcx,%rax,8), %rsi
	testq	%rsi, %rsi
	je	.LBB17_6
# BB#3:                                 # %for.body
                                        #   in Loop: Header=BB17_2 Depth=1
	cmpq	%rdi, %rsi
	je	.LBB17_6
# BB#4:                                 # %land.lhs.true11
                                        #   in Loop: Header=BB17_2 Depth=1
	movq	(%rsi), %rsi
	movq	(%rsi), %rsi
	movzbl	(%rsi), %esi
	cmpq	$38, %rsi
	jne	.LBB17_6
# BB#5:                                 # %if.then
                                        #   in Loop: Header=BB17_2 Depth=1
	movq	(%r8,%rax,8), %rsi
	movq	(%rsi), %rsi
	movq	%rdx, 128(%rsi)
.LBB17_6:                               # %for.inc
                                        #   in Loop: Header=BB17_2 Depth=1
	decq	%rax
	testq	%rax, %rax
	jg	.LBB17_2
.LBB17_7:                               # %for.end
	retq
.Ltmp210:
	.size	Perl_pad_fixup_inner_anons, .Ltmp210-Perl_pad_fixup_inner_anons
	.cfi_endproc

	.globl	Perl_pad_push
	.align	16, 0x90
	.type	Perl_pad_push,@function
Perl_pad_push:                          # @Perl_pad_push
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp217:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp218:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp219:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp220:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp221:
	.cfi_def_cfa_offset 48
	subq	$32, %rsp
.Ltmp222:
	.cfi_def_cfa_offset 80
.Ltmp223:
	.cfi_offset %rbx, -48
.Ltmp224:
	.cfi_offset %r12, -40
.Ltmp225:
	.cfi_offset %r13, -32
.Ltmp226:
	.cfi_offset %r14, -24
.Ltmp227:
	.cfi_offset %r15, -16
	movq	(%rdi), %rax
	movq	%rsi, %r12
	movq	$32, %rcx
                                        # kill: RSI<def> R12<kill>
	movq	8(%rax), %rbx
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	cmpq	%rbx, %rsi
	jle	.LBB18_21
# BB#1:                                 # %if.end
	movq	%rdx, 16(%rsp)          # 8-byte Spill
	movq	%rsi, (%rsp)            # 8-byte Spill
	movq	%rdi, 8(%rsp)           # 8-byte Spill
	movq	(%rax), %rbx
	callq	Perl_newAV
	movq	8(%rbx), %rcx
	movq	(%rcx), %rdx
	movq	%rax, %r13
	decq	%r12
	movq	$32, %rcx
	movq	8(%rdx), %r14
	shlq	%cl, %r12
	movq	$32, %rcx
	sarq	%cl, %r12
	testq	%r14, %r14
	jle	.LBB18_18
# BB#2:
	movq	(%rbx,%r12,8), %rax
	movq	(%rbx), %rcx
	movq	(%rax), %rax
	movq	(%rcx), %rcx
	movq	(%rax), %rax
	movq	%rax, 24(%rsp)          # 8-byte Spill
	movq	(%rcx), %r15
	movq	8(%rcx), %r12
	jmp	.LBB18_3
	.align	16, 0x90
.LBB18_5:                               # %if.then21
                                        #   in Loop: Header=BB18_3 Depth=1
	movzbl	18(%rax), %ecx
	testq	$16, %rcx
	jne	.LBB18_9
# BB#6:                                 # %lor.lhs.false
                                        #   in Loop: Header=BB18_3 Depth=1
	movq	(%rax), %rax
	movq	(%rax), %rax
	movzbl	(%rax), %eax
	cmpq	$37, %rax
	je	.LBB18_14
# BB#7:                                 # %lor.lhs.false
                                        #   in Loop: Header=BB18_3 Depth=1
	cmpq	$64, %rax
	jne	.LBB18_8
# BB#12:                                # %if.then36
                                        #   in Loop: Header=BB18_3 Depth=1
	callq	Perl_newAV
	jmp	.LBB18_13
.LBB18_14:                              # %if.then43
                                        #   in Loop: Header=BB18_3 Depth=1
	callq	Perl_newHV
	jmp	.LBB18_13
.LBB18_8:                               # %lor.lhs.false
                                        #   in Loop: Header=BB18_3 Depth=1
	cmpq	$38, %rax
	jne	.LBB18_15
.LBB18_9:                               # %if.then28
                                        #   in Loop: Header=BB18_3 Depth=1
	movq	24(%rsp), %rax          # 8-byte Reload
	movq	(%rax,%r14,8), %rdx
	testq	%rdx, %rdx
	movq	%rdx, PL_Sv(%rip)
	je	.LBB18_11
# BB#10:                                # %land.rhs
                                        #   in Loop: Header=BB18_3 Depth=1
	incq	8(%rdx)
.LBB18_11:                              # %land.end
                                        #   in Loop: Header=BB18_3 Depth=1
	movq	%r13, %rdi
	movq	%r14, %rsi
	callq	Perl_av_store
	jmp	.LBB18_17
.LBB18_15:                              # %if.else46
                                        #   in Loop: Header=BB18_3 Depth=1
	xorq	%rdi, %rdi
	callq	Perl_newSV
.LBB18_13:                              # %for.inc
                                        #   in Loop: Header=BB18_3 Depth=1
	movq	%rax, %rbx
	movq	%r13, %rdi
	movq	%r14, %rsi
	movq	%rbx, %rdx
	callq	Perl_av_store
	orq	$1280, 16(%rbx)         # imm = 0x500
	jmp	.LBB18_17
	.align	16, 0x90
.LBB18_3:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	cmpq	%r14, %r12
	jl	.LBB18_16
# BB#4:                                 # %land.lhs.true
                                        #   in Loop: Header=BB18_3 Depth=1
	movq	(%r15,%r14,8), %rax
	movabsq	$PL_sv_undef, %rcx
	cmpq	%rcx, %rax
	jne	.LBB18_5
.LBB18_16:                              # %if.else53
                                        #   in Loop: Header=BB18_3 Depth=1
	xorq	%rdi, %rdi
	callq	Perl_newSV
	movq	%rax, %rbx
	movq	%r13, %rdi
	movq	%r14, %rsi
	movq	%rbx, %rdx
	callq	Perl_av_store
	orq	$768, 16(%rbx)          # imm = 0x300
.LBB18_17:                              # %for.inc
                                        #   in Loop: Header=BB18_3 Depth=1
	decq	%r14
	testq	%r14, %r14
	jg	.LBB18_3
.LBB18_18:                              # %for.end
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	movq	16(%rsp), %rcx          # 8-byte Reload
	testq	%rax, %rcx
	je	.LBB18_20
# BB#19:                                # %if.then60
	callq	Perl_newAV
	movq	%rax, %rbx
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	Perl_av_extend
	xorq	%rsi, %rsi
	movq	%r13, %rdi
	movq	%rbx, %rdx
	callq	Perl_av_store
	movq	(%rbx), %rax
	movq	$2, %rcx
	movb	%cl, 72(%rax)
.LBB18_20:                              # %if.end64
	movq	8(%rsp), %rbx           # 8-byte Reload
	movq	%rbx, %rdi
	movq	(%rsp), %r14            # 8-byte Reload
	movq	%r14, %rsi
	movq	%r13, %rdx
	callq	Perl_av_store
	movq	(%rbx), %rax
	movq	%r14, 8(%rax)
.LBB18_21:                              # %return
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp228:
	.size	Perl_pad_push, .Ltmp228-Perl_pad_push
	.cfi_endproc

	.type	.L.str,@object          # @.str
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.asciz	"panic: pad_alloc"
	.size	.L.str, 17

	.type	.L.str1,@object         # @.str1
.L.str1:
	.asciz	"&"
	.size	.L.str1, 2

	.type	.L.str2,@object         # @.str2
.L.str2:
	.asciz	"\"%s\" variable %s masks earlier declaration in same %s"
	.size	.L.str2, 54

	.type	.L.str3,@object         # @.str3
.L.str3:
	.asciz	"our"
	.size	.L.str3, 4

	.type	.L.str4,@object         # @.str4
.L.str4:
	.asciz	"my"
	.size	.L.str4, 3

	.type	.L.str5,@object         # @.str5
.L.str5:
	.asciz	"scope"
	.size	.L.str5, 6

	.type	.L.str6,@object         # @.str6
.L.str6:
	.asciz	"statement"
	.size	.L.str6, 10

	.type	.L.str7,@object         # @.str7
.L.str7:
	.asciz	"\"our\" variable %s redeclared"
	.size	.L.str7, 29

	.type	.L.str8,@object         # @.str8
.L.str8:
	.asciz	"\t(Did you mean \"local\" instead of \"our\"?)\n"
	.size	.L.str8, 43

	.type	.L.str9,@object         # @.str9
.L.str9:
	.asciz	"panic: pad_sv po"
	.size	.L.str9, 17

	.type	.L.str10,@object        # @.str10
.L.str10:
	.asciz	"%_ never introduced"
	.size	.L.str10, 20

	.type	.L.str11,@object        # @.str11
.L.str11:
	.asciz	"panic: pad_swipe curpad"
	.size	.L.str11, 24

	.type	.L.str12,@object        # @.str12
.L.str12:
	.asciz	"panic: pad_swipe po"
	.size	.L.str12, 20

	.type	.L.str13,@object        # @.str13
.L.str13:
	.asciz	"panic: pad_free curpad"
	.size	.L.str13, 23

	.type	.L.str14,@object        # @.str14
.L.str14:
	.asciz	"panic: pad_free po"
	.size	.L.str14, 19

	.type	.L.str15,@object        # @.str15
.L.str15:
	.asciz	"PADNAME = 0x%lx(0x%lx) PAD = 0x%lx(0x%lx)\n"
	.size	.L.str15, 43

	.type	.L.str16,@object        # @.str16
.L.str16:
	.asciz	"%2d. 0x%lx<%lu> FAKE \"%s\"\n"
	.size	.L.str16, 27

	.type	.L.str17,@object        # @.str17
.L.str17:
	.asciz	"%2d. 0x%lx<%lu> (%lu,%lu) \"%s\"\n"
	.size	.L.str17, 32

	.type	.L.str18,@object        # @.str18
.L.str18:
	.asciz	"%2d. 0x%lx<%lu>\n"
	.size	.L.str18, 17


	.ident	"clang version 3.5 (trunk)"
	.section	".note.GNU-stack","",@progbits
