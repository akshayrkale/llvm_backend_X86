	.file	"av.c.bc"
	.text
	.globl	Perl_av_reify
	.align	16, 0x90
	.type	Perl_av_reify,@function
Perl_av_reify:                          # @Perl_av_reify
	.cfi_startproc
# BB#0:                                 # %entry
	movq	(%rdi), %rax
	movzbl	72(%rax), %ecx
	testq	$1, %rcx
	jne	.LBB0_12
# BB#1:                                 # %if.end
	movq	8(%rax), %rcx
	movq	16(%rax), %rdx
	incq	%rcx
	cmpq	%rcx, %rdx
	leaq	1(%rdx), %rcx
	jl	.LBB0_2
	.align	16, 0x90
.LBB0_13:                               # %while.body
                                        # =>This Inner Loop Header: Depth=1
	movq	(%rax), %rax
	movq	$PL_sv_undef, -8(%rax,%rcx,8)
	movq	(%rdi), %rax
	movq	8(%rax), %rdx
	decq	%rcx
	incq	%rdx
	cmpq	%rdx, %rcx
	jg	.LBB0_13
.LBB0_2:                                # %while.cond6.preheader
	testq	%rcx, %rcx
	je	.LBB0_6
# BB#3:
	movabsq	$PL_sv_undef, %rdx
	.align	16, 0x90
.LBB0_4:                                # %while.body8
                                        # =>This Inner Loop Header: Depth=1
	movq	(%rax), %rax
	movq	-8(%rax,%rcx,8), %rax
	cmpq	%rdx, %rax
	je	.LBB0_5
# BB#9:                                 # %if.then15
                                        #   in Loop: Header=BB0_4 Depth=1
	movq	%rax, PL_Sv(%rip)
	testq	%rax, %rax
	je	.LBB0_5
# BB#10:                                # %land.rhs
                                        #   in Loop: Header=BB0_4 Depth=1
	incq	8(%rax)
.LBB0_5:                                # %while.cond6.backedge
                                        #   in Loop: Header=BB0_4 Depth=1
	movq	(%rdi), %rax
	decq	%rcx
	jne	.LBB0_4
.LBB0_6:                                # %while.end19
	movq	(%rax), %rdx
	movq	56(%rax), %rsi
	subq	%rsi, %rdx
	movq	$3, %rcx
	sarq	%cl, %rdx
	testq	%rdx, %rdx
	jne	.LBB0_7
	jmp	.LBB0_11
	.align	16, 0x90
.LBB0_8:                                # %while.body25.while.body25_crit_edge
                                        #   in Loop: Header=BB0_7 Depth=1
	movq	56(%rax), %rsi
.LBB0_7:                                # %while.body25.preheader
                                        # =>This Inner Loop Header: Depth=1
	decq	%rdx
	movq	$PL_sv_undef, (%rsi,%rdx,8)
	movq	(%rdi), %rax
	testq	%rdx, %rdx
	jne	.LBB0_8
.LBB0_11:                               # %while.end30
	movzbl	72(%rax), %ecx
	andq	$252, %rcx
	orq	$1, %rcx
	movb	%cl, 72(%rax)
.LBB0_12:                               # %return
	retq
.Ltmp0:
	.size	Perl_av_reify, .Ltmp0-Perl_av_reify
	.cfi_endproc

	.globl	Perl_av_extend
	.align	16, 0x90
	.type	Perl_av_extend,@function
Perl_av_extend:                         # @Perl_av_extend
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
	movq	%rdi, %r12
	movzbl	17(%r12), %eax
	movq	%rsi, %r14
	testq	$128, %rax
	je	.LBB1_20
# BB#1:                                 # %cond.end
	movq	$80, %rsi
	movq	%r12, %rdi
	callq	Perl_mg_find
	movq	%rax, %r15
	testq	%r15, %r15
	je	.LBB1_20
# BB#2:                                 # %if.then
	movq	PL_stack_sp(%rip), %rbx
	callq	Perl_push_scope
	movabsq	$PL_tmps_floor, %rdi
	callq	Perl_save_int
	movq	PL_tmps_ix(%rip), %rax
	movq	PL_curstackinfo(%rip), %rcx
	movq	%rax, PL_tmps_floor(%rip)
	movq	48(%rcx), %rax
	testq	%rax, %rax
	jne	.LBB1_4
# BB#3:                                 # %if.then3
	movq	$32, %rdi
	movq	$12, %rsi
	callq	Perl_new_stackinfo
	movq	PL_curstackinfo(%rip), %rcx
	movq	%rcx, 40(%rax)
	movq	PL_curstackinfo(%rip), %rcx
	movq	%rax, 48(%rcx)
.LBB1_4:                                # %if.end
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
	jne	.LBB1_6
# BB#5:                                 # %if.then17
	callq	Perl_markstack_grow
	movq	PL_stack_base(%rip), %rcx
	movq	PL_markstack_ptr(%rip), %rsi
	movq	PL_stack_max(%rip), %rdx
.LBB1_6:                                # %if.end18
	movq	%rbx, %rax
	subq	%rcx, %rax
	movq	$3, %rcx
	sarq	%cl, %rax
	movq	%rax, (%rsi)
	subq	%rbx, %rdx
	cmpq	$15, %rdx
	jg	.LBB1_8
# BB#7:                                 # %if.then29
	movq	$2, %rdx
	movq	%rbx, %rdi
	movq	%rbx, %rsi
	callq	Perl_stack_grow
	movq	%rax, %rbx
.LBB1_8:                                # %if.end31
	movq	24(%r15), %rax
	testq	%rax, %rax
	jne	.LBB1_10
# BB#9:                                 # %cond.false35
	movq	%r12, %rdi
	callq	Perl_newRV
	movq	%rax, %rdi
	callq	Perl_sv_2mortal
.LBB1_10:                               # %cond.end38
	movq	%rax, 8(%rbx)
	incq	%r14
	movq	%r14, %rdi
	callq	Perl_newSViv
	movq	%rax, %rdi
	callq	Perl_sv_2mortal
	movq	%rax, 16(%rbx)
	addq	$16, %rbx
	movq	%rbx, PL_stack_sp(%rip)
	movabsq	$.L.str, %rdi
	movq	$2, %rsi
	callq	Perl_call_method
	movq	PL_curstackinfo(%rip), %rax
	movq	40(%rax), %r14
	movq	PL_stack_sp(%rip), %rbx
	testq	%r14, %r14
	jne	.LBB1_17
# BB#11:                                # %if.then50
	movq	PL_stderrgv(%rip), %rax
	testq	%rax, %rax
	je	.LBB1_15
# BB#12:                                # %land.lhs.true
	movzbl	16(%rax), %ecx
	cmpq	$13, %rcx
	jne	.LBB1_15
# BB#13:                                # %land.lhs.true55
	movq	(%rax), %rax
	movq	56(%rax), %rax
	movq	16(%rax), %rax
	testq	%rax, %rax
	je	.LBB1_15
# BB#14:                                # %land.lhs.true58
	movq	(%rax), %rax
	movq	64(%rax), %rdi
	testq	%rdi, %rdi
	jne	.LBB1_16
.LBB1_15:                               # %cond.false70
	callq	Perl_PerlIO_stderr
	movq	%rax, %rdi
.LBB1_16:                               # %cond.end72
	movabsq	$.L.str1, %rsi
	xorq	%rax, %rax
	callq	PerlIO_printf
	movq	$1, %rdi
	callq	Perl_my_exit
.LBB1_17:                               # %if.end75
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
	jle	.LBB1_19
# BB#18:                                # %if.then97
	callq	Perl_free_tmps
.LBB1_19:                               # %if.end98
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	Perl_pop_scope  # TAILCALL
.LBB1_20:                               # %if.end99
	movq	(%r12), %rcx
	movq	16(%rcx), %rax
	cmpq	%r14, %rax
	jge	.LBB1_48
# BB#21:                                # %if.then103
	movq	(%rcx), %rsi
	movq	56(%rcx), %r15
	cmpq	%rsi, %r15
	je	.LBB1_28
# BB#22:                                # %if.then112
	movq	8(%rcx), %r13
	movq	%rsi, %rbx
	subq	%r15, %rbx
	movq	$3, %rcx
	sarq	%cl, %rbx
	leaq	8(,%r13,8), %rdx
	movq	%r15, %rdi
	callq	memmove
	movq	(%r12), %rax
	movq	16(%rax), %rdx
	addq	%rbx, %rdx
	movq	%rdx, 16(%rax)
	movq	56(%rax), %rcx
	movq	%rcx, (%rax)
	movzbl	72(%rax), %eax
	testq	$1, %rax
	je	.LBB1_27
# BB#23:                                # %if.then112
	testq	%rbx, %rbx
	je	.LBB1_27
# BB#24:                                # %while.body.preheader
	leaq	(%r15,%r13,8), %rax
	.align	16, 0x90
.LBB1_25:                               # %while.body
                                        # =>This Inner Loop Header: Depth=1
	movq	$PL_sv_undef, (%rax,%rbx,8)
	decq	%rbx
	jne	.LBB1_25
# BB#26:                                # %if.end145.loopexit
	movq	(%r12), %rax
	movq	16(%rax), %rdx
.LBB1_27:                               # %if.end145
	leaq	-10(%rdx), %rax
	cmpq	%r14, %rax
	jl	.LBB1_30
	jmp	.LBB1_48
.LBB1_28:                               # %if.else
	testq	%r15, %r15
	je	.LBB1_40
# BB#29:                                # %if.then158
	movabsq	$7378697629483820647, %rcx # imm = 0x6666666666666667
	imulq	%rcx
	movq	$63, %rcx
	movq	%rdx, %rax
	shrq	%cl, %rax
	movq	$1, %rcx
	sarq	%cl, %rdx
	addq	%rax, %rdx
.LBB1_30:                               # %resize
	addq	%r14, %rdx
	leaq	8(,%rdx,8), %rax
	movq	$16, %rcx
	.align	16, 0x90
.LBB1_31:                               # %while.cond166
                                        # =>This Inner Loop Header: Depth=1
	leaq	-16(%rcx), %rdi
	addq	%rcx, %rcx
	cmpq	%rax, %rdi
	jb	.LBB1_31
# BB#32:                                # %while.end172
	movq	$3, %rcx
	shrq	%cl, %rdi
	movq	$3, %rcx
	leaq	-1(%rdi), %r15
	shlq	%cl, %rdi
	callq	Perl_safesysmalloc
	movq	(%r12), %rcx
	movq	16(%rcx), %rdx
	movq	56(%rcx), %rsi
	movq	%rax, %r13
	leaq	8(,%rdx,8), %rdx
	movq	%r13, %rdi
	callq	memcpy
	movq	(%r12), %rcx
	movq	16(%rcx), %rax
	movq	56(%rcx), %r14
	cmpq	$65, %rax
	jl	.LBB1_37
# BB#33:                                # %if.then189
	movq	PL_nice_chunk_size(%rip), %rcx
	leaq	8(,%rax,8), %rbx
	cmpq	%rcx, %rbx
	jbe	.LBB1_37
# BB#34:                                # %if.then200
	movq	PL_nice_chunk(%rip), %rdi
	testq	%rdi, %rdi
	je	.LBB1_36
# BB#35:                                # %if.then202
	callq	Perl_safesysfree
.LBB1_36:                               # %if.end203
	movq	%r14, PL_nice_chunk(%rip)
	movq	%rbx, PL_nice_chunk_size(%rip)
	jmp	.LBB1_38
.LBB1_37:                               # %if.else208
	movq	%r14, %rdi
	callq	Perl_safesysfree
.LBB1_38:                               # %if.end211
	movq	(%r12), %rax
	movq	%r13, 56(%rax)
	movq	16(%rax), %rax
	movq	PL_curstack(%rip), %rcx
	movq	%r15, %rbx
	subq	%rax, %rbx
	leaq	8(%r13,%rax,8), %rax
	cmpq	%r12, %rcx
	jne	.LBB1_43
# BB#39:                                # %if.then225
	movq	PL_stack_sp(%rip), %rcx
	movq	PL_stack_base(%rip), %rdx
	subq	%rdx, %rcx
	andq	$-8, %rcx
	addq	%r13, %rcx
	movq	%rcx, PL_stack_sp(%rip)
	movq	(%r12), %rcx
	movq	56(%rcx), %rcx
	movq	%rcx, PL_stack_base(%rip)
	leaq	(%rcx,%r15,8), %rcx
	movq	%rcx, PL_stack_max(%rip)
	jmp	.LBB1_43
.LBB1_40:                               # %if.else237
	cmpq	$3, %r14
	movq	$3, %rbx
	jl	.LBB1_42
# BB#41:                                # %if.else237
	movq	%r14, %rbx
.LBB1_42:                               # %if.else237
	leaq	8(,%rbx,8), %rdi
	callq	Perl_safesysmalloc
	movq	(%r12), %rcx
	movq	%rax, 56(%rcx)
	movq	$PL_sv_undef, (%rax)
	addq	$8, %rax
	movq	%rbx, %r15
.LBB1_43:                               # %if.end255
	movq	(%r12), %rcx
	movzbl	72(%rcx), %edx
	testq	$1, %rdx
	je	.LBB1_47
# BB#44:                                # %if.end255
	testq	%rbx, %rbx
	je	.LBB1_47
	.align	16, 0x90
.LBB1_45:                               # %while.body264
                                        # =>This Inner Loop Header: Depth=1
	movq	$PL_sv_undef, -8(%rax,%rbx,8)
	decq	%rbx
	jne	.LBB1_45
# BB#46:                                # %if.end268.loopexit
	movq	(%r12), %rcx
.LBB1_47:                               # %if.end268
	movq	56(%rcx), %rax
	movq	%rax, (%rcx)
	movq	%r15, 16(%rcx)
.LBB1_48:                               # %if.end276
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp17:
	.size	Perl_av_extend, .Ltmp17-Perl_av_extend
	.cfi_endproc

	.globl	Perl_av_fetch
	.align	16, 0x90
	.type	Perl_av_fetch,@function
Perl_av_fetch:                          # @Perl_av_fetch
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
	movq	%rdx, %r12
	movq	%rsi, %r14
	movq	%rdi, %r13
	xorq	%r15, %r15
	testq	%r13, %r13
	je	.LBB2_38
# BB#1:                                 # %if.end
	movzbl	17(%r13), %eax
	testq	$128, %rax
	je	.LBB2_25
# BB#2:                                 # %if.then2
	movq	$80, %rsi
	movq	%r13, %rdi
	callq	Perl_mg_find
	testq	%rax, %rax
	je	.LBB2_3
# BB#5:                                 # %if.then6
	testq	%r14, %r14
	jns	.LBB2_24
# BB#6:                                 # %if.then8
	movq	24(%rax), %rax
	testq	%rax, %rax
	jne	.LBB2_8
# BB#7:                                 # %cond.false
	movq	%r13, %rdi
	callq	Perl_newRV
	movq	%rax, %rdi
	callq	Perl_sv_2mortal
.LBB2_8:                                # %cond.end
	movq	(%rax), %rax
	movq	(%rax), %rax
	movq	(%rax), %rax
	movq	48(%rax), %rdi
	movabsq	$.L.str2, %rsi
	movq	$16, %rdx
	xorq	%rcx, %rcx
	callq	Perl_hv_fetch
	testq	%rax, %rax
	je	.LBB2_20
# BB#9:                                 # %land.lhs.true16
	movq	(%rax), %rax
	movq	(%rax), %rax
	movq	56(%rax), %rax
	movq	(%rax), %rdi
	testq	%rdi, %rdi
	je	.LBB2_20
# BB#10:                                # %cond.false20
	movq	16(%rdi), %rax
	testq	$262144, %rax           # imm = 0x40000
	je	.LBB2_15
# BB#11:                                # %cond.true27
	movq	(%rdi), %rax
	testq	%rax, %rax
	movq	%rax, PL_Xpv(%rip)
	je	.LBB2_20
# BB#12:                                # %land.lhs.true33
	movq	8(%rax), %rcx
	cmpq	$1, %rcx
	ja	.LBB2_24
# BB#13:                                # %lor.lhs.false35
	testq	%rcx, %rcx
	je	.LBB2_20
# BB#14:                                # %land.lhs.true38
	movq	(%rax), %rax
	movzbl	(%rax), %eax
	cmpq	$48, %rax
	je	.LBB2_20
	jmp	.LBB2_24
.LBB2_3:                                # %lor.lhs.false
	movq	$68, %rsi
	movq	%r13, %rdi
	callq	Perl_mg_find
	testq	%rax, %rax
	je	.LBB2_25
# BB#4:                                 # %if.end80
	testq	%r14, %r14
	jns	.LBB2_24
	jmp	.LBB2_20
.LBB2_25:                               # %if.end109
	testq	%r14, %r14
	jns	.LBB2_30
# BB#26:                                # %if.then112
	movzbl	17(%r13), %eax
	testq	$128, %rax
	je	.LBB2_28
# BB#27:                                # %cond.true116
	movq	%r13, %rdi
	callq	Perl_mg_size
	jmp	.LBB2_29
.LBB2_28:                               # %cond.false118
	movq	(%r13), %rax
	movq	8(%rax), %rax
.LBB2_29:                               # %cond.end121
	addq	%rax, %r14
	incq	%r14
	js	.LBB2_38
.LBB2_30:                               # %if.end129
	movq	(%r13), %rcx
	movq	8(%rcx), %rax
	cmpq	%rax, %r14
	jg	.LBB2_31
# BB#33:                                # %if.end140
	movq	(%rcx), %rdx
	movq	(%rdx,%r14,8), %rax
	movabsq	$PL_sv_undef, %rsi
	cmpq	%rsi, %rax
	je	.LBB2_31
# BB#34:                                # %if.else
	movzbl	72(%rcx), %esi
	leaq	(%rdx,%r14,8), %rcx
	testq	$2, %rsi
	je	.LBB2_37
# BB#35:                                # %land.lhs.true154
	testq	%rax, %rax
	je	.LBB2_39
# BB#36:                                # %lor.lhs.false159
	movzbl	16(%rax), %eax
	cmpq	$255, %rax
	jne	.LBB2_37
.LBB2_39:                               # %if.then167
	movq	$PL_sv_undef, (%rcx)
.LBB2_31:                               # %if.then134
	testq	%r12, %r12
	je	.LBB2_38
# BB#32:                                # %if.end137
	xorq	%rdi, %rdi
	callq	Perl_newSV
	movq	%r13, %rdi
	movq	%r14, %rsi
	movq	%rax, %rdx
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	Perl_av_store  # TAILCALL
.LBB2_37:
	movq	%rcx, %r15
	jmp	.LBB2_38
.LBB2_15:                               # %cond.false43
	testq	$65536, %rax            # imm = 0x10000
	je	.LBB2_17
# BB#16:                                # %cond.true50
	movq	(%rdi), %rax
	cmpq	$0, 24(%rax)
	jne	.LBB2_24
	jmp	.LBB2_20
.LBB2_17:                               # %cond.false57
	testq	$131072, %rax           # imm = 0x20000
	je	.LBB2_19
# BB#18:                                # %cond.true64
	movq	(%rdi), %rax
	movq	32(%rax), %rdi
	xorq	%rsi, %rsi
	callq	__nedf2
	testq	%rax, %rax
	jne	.LBB2_24
	jmp	.LBB2_20
.LBB2_19:                               # %cond.false71
	callq	Perl_sv_2bool
	testq	$255, %rax
	jne	.LBB2_24
.LBB2_20:                               # %if.then85
	movzbl	17(%r13), %eax
	testq	$128, %rax
	je	.LBB2_22
# BB#21:                                # %cond.true89
	movq	%r13, %rdi
	callq	Perl_mg_size
	jmp	.LBB2_23
.LBB2_22:                               # %cond.false91
	movq	(%r13), %rax
	movq	8(%rax), %rax
.LBB2_23:                               # %cond.end93
	addq	%rax, %r14
	incq	%r14
	js	.LBB2_38
.LBB2_24:                               # %if.end100
	callq	Perl_sv_newmortal
	movq	%rax, %rbx
	movq	$9, %rsi
	movq	%rbx, %rdi
	callq	Perl_sv_upgrade
	xorq	%rdx, %rdx
	movq	%r13, %rdi
	movq	%rbx, %rsi
	movq	%r14, %rcx
	callq	Perl_mg_copy
	movq	(%rbx), %r15
	movq	$116, %rax
	movb	%al, 80(%r15)
	movq	%rbx, 72(%r15)
	addq	$72, %r15
.LBB2_38:                               # %return
	movq	%r15, %rax
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp34:
	.size	Perl_av_fetch, .Ltmp34-Perl_av_fetch
	.cfi_endproc

	.globl	Perl_av_store
	.align	16, 0x90
	.type	Perl_av_store,@function
Perl_av_store:                          # @Perl_av_store
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
	movq	%rdx, %r15
	movq	%rsi, %r14
	movq	%rdi, %r13
	xorq	%r12, %r12
	testq	%r13, %r13
	je	.LBB3_56
# BB#1:                                 # %if.end
	testq	%r15, %r15
	jne	.LBB3_3
# BB#2:
	movabsq	$PL_sv_undef, %r15
.LBB3_3:                                # %if.end
	movzbl	17(%r13), %eax
	testq	$128, %rax
	je	.LBB3_26
# BB#4:                                 # %if.then5
	movq	$80, %rsi
	movq	%r13, %rdi
	callq	Perl_mg_find
	testq	%rax, %rax
	je	.LBB3_26
# BB#5:                                 # %if.then7
	testq	%r14, %r14
	jns	.LBB3_24
# BB#6:                                 # %if.then8
	movq	24(%rax), %rax
	testq	%rax, %rax
	jne	.LBB3_8
# BB#7:                                 # %cond.false
	movq	%r13, %rdi
	callq	Perl_newRV
	movq	%rax, %rdi
	callq	Perl_sv_2mortal
.LBB3_8:                                # %cond.end
	movq	(%rax), %rax
	movq	(%rax), %rax
	movq	(%rax), %rax
	movq	48(%rax), %rdi
	movabsq	$.L.str2, %rsi
	movq	$16, %rdx
	xorq	%rcx, %rcx
	callq	Perl_hv_fetch
	testq	%rax, %rax
	je	.LBB3_20
# BB#9:                                 # %land.lhs.true
	movq	(%rax), %rax
	movq	(%rax), %rax
	movq	56(%rax), %rax
	movq	(%rax), %rdi
	testq	%rdi, %rdi
	je	.LBB3_20
# BB#10:                                # %cond.false19
	movq	16(%rdi), %rax
	testq	$262144, %rax           # imm = 0x40000
	je	.LBB3_15
# BB#11:                                # %cond.true26
	movq	(%rdi), %rax
	testq	%rax, %rax
	movq	%rax, PL_Xpv(%rip)
	je	.LBB3_20
# BB#12:                                # %land.lhs.true32
	movq	8(%rax), %rcx
	cmpq	$1, %rcx
	ja	.LBB3_24
# BB#13:                                # %lor.lhs.false
	testq	%rcx, %rcx
	je	.LBB3_20
# BB#14:                                # %land.lhs.true36
	movq	(%rax), %rax
	movzbl	(%rax), %eax
	cmpq	$48, %rax
	je	.LBB3_20
	jmp	.LBB3_24
.LBB3_26:                               # %if.end102
	testq	%r14, %r14
	jns	.LBB3_31
# BB#27:                                # %if.then105
	movzbl	17(%r13), %eax
	testq	$128, %rax
	je	.LBB3_29
# BB#28:                                # %cond.true109
	movq	%r13, %rdi
	callq	Perl_mg_size
	jmp	.LBB3_30
.LBB3_29:                               # %cond.false111
	movq	(%r13), %rax
	movq	8(%rax), %rax
.LBB3_30:                               # %cond.end114
	addq	%rax, %r14
	incq	%r14
	js	.LBB3_56
.LBB3_31:                               # %if.end122
	movq	16(%r13), %rax
	testq	$8388608, %rax          # imm = 0x800000
	je	.LBB3_37
# BB#32:                                # %land.lhs.true126
	testq	$32768, %rax            # imm = 0x8000
	je	.LBB3_34
# BB#33:                                # %cond.true130
	movq	%r13, %rdi
	callq	Perl_mg_size
	jmp	.LBB3_35
.LBB3_34:                               # %cond.false132
	movq	(%r13), %rax
	movq	8(%rax), %rax
.LBB3_35:                               # %cond.end135
	cmpq	%rax, %r14
	jl	.LBB3_37
# BB#36:                                # %if.then139
	movabsq	$PL_no_modify, %rdi
	xorq	%rax, %rax
	callq	Perl_croak
.LBB3_37:                               # %if.end140
	movq	(%r13), %rax
	movzbl	72(%rax), %ecx
	andq	$3, %rcx
	cmpq	$2, %rcx
	jne	.LBB3_39
# BB#38:                                # %if.then151
	xorq	%rax, %rax
	movq	%r13, %rdi
	callq	av_reify
	movq	(%r13), %rax
.LBB3_39:                               # %if.end153
	movq	16(%rax), %rcx
	cmpq	%rcx, %r14
	jle	.LBB3_41
# BB#40:                                # %if.then157
	movq	%r13, %rdi
	movq	%r14, %rsi
	callq	Perl_av_extend
	movq	(%r13), %rax
.LBB3_41:                               # %if.end158
	movzbl	72(%rax), %ecx
	movq	8(%rax), %rdx
	movq	(%rax), %rbx
	andq	$1, %rcx
	cmpq	%r14, %rdx
	jge	.LBB3_48
# BB#42:                                # %if.then164
	testq	%rcx, %rcx
	jne	.LBB3_47
# BB#43:                                # %if.then170
	movq	PL_curstack(%rip), %rcx
	cmpq	%r13, %rcx
	jne	.LBB3_46
# BB#44:                                # %land.lhs.true173
	movq	PL_stack_sp(%rip), %rdi
	movq	PL_stack_base(%rip), %rsi
	subq	%rsi, %rdi
	movq	$3, %rcx
	sarq	%cl, %rdi
	cmpq	%rdi, %r14
	jle	.LBB3_46
# BB#45:                                # %if.then176
	leaq	(%rsi,%r14,8), %rax
	movq	%rax, PL_stack_sp(%rip)
	movq	(%r13), %rax
	movq	8(%rax), %rdx
	.align	16, 0x90
.LBB3_46:                               # %do.body
                                        # =>This Inner Loop Header: Depth=1
	leaq	1(%rdx), %rcx
	movq	%rcx, 8(%rax)
	movq	$PL_sv_undef, 8(%rbx,%rdx,8)
	movq	(%r13), %rax
	movq	8(%rax), %rdx
	cmpq	%r14, %rdx
	jl	.LBB3_46
.LBB3_47:                               # %if.end184
	movq	%r14, 8(%rax)
	jmp	.LBB3_50
.LBB3_48:                               # %if.else
	testq	%rcx, %rcx
	je	.LBB3_50
# BB#49:                                # %if.then192
	movq	(%rbx,%r14,8), %rdi
	callq	Perl_sv_free
.LBB3_50:                               # %if.end195
	movq	%r15, (%rbx,%r14,8)
	movzbl	17(%r13), %eax
	leaq	(%rbx,%r14,8), %r12
	testq	$64, %rax
	je	.LBB3_56
# BB#51:                                # %if.then200
	movabsq	$PL_sv_undef, %rax
	cmpq	%rax, %r15
	je	.LBB3_55
# BB#52:                                # %if.then203
	movq	(%r13), %rax
	movq	40(%rax), %rax
	movsbq	18(%rax), %rdx
	leaq	-65(%rdx), %rax
	andq	$255, %rax
	cmpq	$26, %rax
	jae	.LBB3_54
# BB#53:
	addq	$32, %rdx
.LBB3_54:                               # %if.then203
	xorq	%rcx, %rcx
	movq	%r15, %rdi
	movq	%r13, %rsi
	movq	%r14, %r8
	callq	Perl_sv_magic
.LBB3_55:                               # %if.end222
	movq	%r13, %rdi
	callq	Perl_mg_set
	jmp	.LBB3_56
.LBB3_15:                               # %cond.false41
	testq	$65536, %rax            # imm = 0x10000
	je	.LBB3_17
# BB#16:                                # %cond.true48
	movq	(%rdi), %rax
	cmpq	$0, 24(%rax)
	jne	.LBB3_24
	jmp	.LBB3_20
.LBB3_17:                               # %cond.false55
	testq	$131072, %rax           # imm = 0x20000
	je	.LBB3_19
# BB#18:                                # %cond.true62
	movq	(%rdi), %rax
	movq	32(%rax), %rdi
	xorq	%rsi, %rsi
	callq	__nedf2
	testq	%rax, %rax
	jne	.LBB3_24
	jmp	.LBB3_20
.LBB3_19:                               # %cond.false69
	callq	Perl_sv_2bool
	testq	$255, %rax
	jne	.LBB3_24
.LBB3_20:                               # %if.then79
	movzbl	17(%r13), %eax
	testq	$128, %rax
	je	.LBB3_22
# BB#21:                                # %cond.true83
	movq	%r13, %rdi
	callq	Perl_mg_size
	jmp	.LBB3_23
.LBB3_22:                               # %cond.false85
	movq	(%r13), %rax
	movq	8(%rax), %rax
.LBB3_23:                               # %cond.end87
	addq	%rax, %r14
	xorq	%r12, %r12
	incq	%r14
	js	.LBB3_56
.LBB3_24:                               # %if.end95
	movabsq	$PL_sv_undef, %rax
	cmpq	%rax, %r15
	movabsq	$0, %r12
	je	.LBB3_56
# BB#25:                                # %if.then98
	xorq	%rdx, %rdx
	xorq	%r12, %r12
	movq	%r13, %rdi
	movq	%r15, %rsi
	movq	%r14, %rcx
	callq	Perl_mg_copy
.LBB3_56:                               # %return
	movq	%r12, %rax
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp51:
	.size	Perl_av_store, .Ltmp51-Perl_av_store
	.cfi_endproc

	.globl	Perl_newAV
	.align	16, 0x90
	.type	Perl_newAV,@function
Perl_newAV:                             # @Perl_newAV
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp54:
	.cfi_def_cfa_offset 16
.Ltmp55:
	.cfi_offset %rbx, -16
	xorq	%rdi, %rdi
	callq	Perl_newSV
	movq	%rax, %rbx
	movq	$10, %rsi
	movq	%rbx, %rdi
	callq	Perl_sv_upgrade
	movq	(%rbx), %rax
	movzbl	72(%rax), %ecx
	orq	$1, %rcx
	movb	%cl, 72(%rax)
	movq	$0, 56(%rax)
	movq	$0, (%rax)
	movq	$-1, 16(%rax)
	movq	$-1, 8(%rax)
	movq	%rbx, %rax
	popq	%rbx
	retq
.Ltmp56:
	.size	Perl_newAV, .Ltmp56-Perl_newAV
	.cfi_endproc

	.globl	Perl_av_make
	.align	16, 0x90
	.type	Perl_av_make,@function
Perl_av_make:                           # @Perl_av_make
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp62:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp63:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp64:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp65:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp66:
	.cfi_def_cfa_offset 48
.Ltmp67:
	.cfi_offset %rbx, -40
.Ltmp68:
	.cfi_offset %r12, -32
.Ltmp69:
	.cfi_offset %r14, -24
.Ltmp70:
	.cfi_offset %r15, -16
	movq	%rsi, %r12
	movq	%rdi, %r14
	xorq	%rdi, %rdi
	callq	Perl_newSV
	movq	%rax, %r15
	movq	$10, %rsi
	movq	%r15, %rdi
	callq	Perl_sv_upgrade
	movq	(%r15), %rax
	testq	%r14, %r14
	movq	$1, %rcx
	movb	%cl, 72(%rax)
	je	.LBB5_3
# BB#1:                                 # %if.then
	movq	$3, %rcx
	movq	%r14, %rdi
	shlq	%cl, %rdi
	callq	Perl_safesysmalloc
	movq	%rax, %rbx
	movq	(%r15), %rax
	leaq	-1(%r14), %rcx
	testq	%r14, %r14
	movq	%rbx, 56(%rax)
	movq	%rbx, (%rax)
	movq	%rcx, 8(%rax)
	movq	%rcx, 16(%rax)
	jle	.LBB5_3
	.align	16, 0x90
.LBB5_2:                                # %for.body
                                        # =>This Inner Loop Header: Depth=1
	xorq	%rdi, %rdi
	callq	Perl_newSV
	movq	%rax, (%rbx)
	movq	(%r12), %rsi
	movq	$2, %rdx
	movq	%rax, %rdi
	callq	Perl_sv_setsv_flags
	addq	$8, %r12
	addq	$8, %rbx
	decq	%r14
	jne	.LBB5_2
.LBB5_3:                                # %if.end
	movq	%r15, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.Ltmp71:
	.size	Perl_av_make, .Ltmp71-Perl_av_make
	.cfi_endproc

	.globl	Perl_av_fake
	.align	16, 0x90
	.type	Perl_av_fake,@function
Perl_av_fake:                           # @Perl_av_fake
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
	movq	%rsi, %rbx
	movq	%rdi, %r14
	xorq	%rdi, %rdi
	callq	Perl_newSV
	movq	%rax, %r15
	movq	$10, %rsi
	movq	%r15, %rdi
	callq	Perl_sv_upgrade
	movq	$3, %rcx
	movq	%r14, %r12
	shlq	%cl, %r12
	leaq	8(,%r14,8), %rdi
	callq	Perl_safesysmalloc
	movq	(%r15), %rcx
	movq	%rax, %r13
	movq	%r13, 56(%rcx)
	movq	%r13, %rdi
	movq	%rbx, %rsi
	movq	%r12, %rdx
	callq	memcpy
	leaq	-1(%r14), %rax
	movq	(%r15), %rcx
	testq	%r14, %r14
	movq	$2, %rdx
	movb	%dl, 72(%rcx)
	movq	%r13, (%rcx)
	movq	%rax, 8(%rcx)
	movq	%rax, 16(%rcx)
	je	.LBB6_2
	.align	16, 0x90
.LBB6_1:                                # %while.body
                                        # =>This Inner Loop Header: Depth=1
	movq	(%rbx), %rax
	andq	$-2049, 16(%rax)        # imm = 0xFFFFFFFFFFFFF7FF
	addq	$8, %rbx
	decq	%r14
	jne	.LBB6_1
.LBB6_2:                                # %while.end
	movq	%r15, %rax
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp88:
	.size	Perl_av_fake, .Ltmp88-Perl_av_fake
	.cfi_endproc

	.globl	Perl_av_clear
	.align	16, 0x90
	.type	Perl_av_clear,@function
Perl_av_clear:                          # @Perl_av_clear
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
	movq	%rdi, %r14
	testq	%r14, %r14
	je	.LBB7_14
# BB#1:                                 # %if.end
	movq	16(%r14), %rax
	testq	$8388608, %rax          # imm = 0x800000
	je	.LBB7_3
# BB#2:                                 # %if.then2
	movabsq	$PL_no_modify, %rdi
	xorq	%rax, %rax
	callq	Perl_croak
	movq	16(%r14), %rax
.LBB7_3:                                # %if.end3
	testq	$32768, %rax            # imm = 0x8000
	je	.LBB7_5
# BB#4:                                 # %if.then7
	movq	%r14, %rdi
	callq	Perl_mg_clear
.LBB7_5:                                # %if.end8
	movq	(%r14), %rax
	cmpq	$0, 16(%rax)
	js	.LBB7_14
# BB#6:                                 # %if.end10
	movzbl	72(%rax), %ecx
	testq	$1, %rcx
	je	.LBB7_11
# BB#7:                                 # %if.then14
	movq	8(%rax), %rbx
	cmpq	$-1, %rbx
	je	.LBB7_11
# BB#8:
	movq	(%rax), %r15
	.align	16, 0x90
.LBB7_9:                                # %while.body
                                        # =>This Inner Loop Header: Depth=1
	movq	(%r15,%rbx,8), %rdi
	movq	$PL_sv_undef, (%r15,%rbx,8)
	callq	Perl_sv_free
	decq	%rbx
	cmpq	$-1, %rbx
	jne	.LBB7_9
# BB#10:                                # %while.cond.if.end19.loopexit_crit_edge
	movq	(%r14), %rax
.LBB7_11:                               # %if.end19
	movq	(%rax), %rsi
	movq	56(%rax), %rdx
	subq	%rdx, %rsi
	movq	$3, %rcx
	sarq	%cl, %rsi
	testq	%rsi, %rsi
	je	.LBB7_13
# BB#12:                                # %if.then24
	addq	%rsi, 16(%rax)
	movq	%rdx, (%rax)
.LBB7_13:                               # %if.end31
	movq	$-1, 8(%rax)
.LBB7_14:                               # %return
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp99:
	.size	Perl_av_clear, .Ltmp99-Perl_av_clear
	.cfi_endproc

	.globl	Perl_av_undef
	.align	16, 0x90
	.type	Perl_av_undef,@function
Perl_av_undef:                          # @Perl_av_undef
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp103:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp104:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp105:
	.cfi_def_cfa_offset 32
.Ltmp106:
	.cfi_offset %rbx, -24
.Ltmp107:
	.cfi_offset %r14, -16
	movq	%rdi, %r14
	testq	%r14, %r14
	je	.LBB8_10
# BB#1:                                 # %if.end
	movzbl	17(%r14), %eax
	testq	$128, %rax
	je	.LBB8_4
# BB#2:                                 # %cond.true
	movq	$80, %rsi
	movq	%r14, %rdi
	callq	Perl_mg_find
	testq	%rax, %rax
	je	.LBB8_4
# BB#3:                                 # %if.then3
	movq	$-1, %rsi
	movq	%r14, %rdi
	callq	Perl_av_fill
.LBB8_4:                                # %if.end4
	movq	(%r14), %rax
	movzbl	72(%rax), %ecx
	testq	$1, %rcx
	je	.LBB8_8
# BB#5:                                 # %if.then7
	movq	8(%rax), %rbx
	jmp	.LBB8_7
	.align	16, 0x90
.LBB8_6:                                # %while.body
                                        #   in Loop: Header=BB8_7 Depth=1
	movq	(%rax), %rax
	movq	(%rax,%rbx,8), %rdi
	callq	Perl_sv_free
	movq	(%r14), %rax
	decq	%rbx
.LBB8_7:                                # %while.body
                                        # =>This Inner Loop Header: Depth=1
	cmpq	$-1, %rbx
	jne	.LBB8_6
.LBB8_8:                                # %if.end11
	movq	56(%rax), %rdi
	callq	Perl_safesysfree
	movq	(%r14), %rax
	movq	$0, 56(%rax)
	movq	$0, (%rax)
	movq	$-1, 16(%rax)
	movq	$-1, 8(%rax)
	movq	64(%rax), %rdi
	testq	%rdi, %rdi
	je	.LBB8_10
# BB#9:                                 # %if.then21
	callq	Perl_sv_free
	movq	(%r14), %rax
	movq	$0, 64(%rax)
.LBB8_10:                               # %if.end26
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp108:
	.size	Perl_av_undef, .Ltmp108-Perl_av_undef
	.cfi_endproc

	.globl	Perl_av_fill
	.align	16, 0x90
	.type	Perl_av_fill,@function
Perl_av_fill:                           # @Perl_av_fill
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp115:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp116:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp117:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp118:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp119:
	.cfi_def_cfa_offset 48
.Ltmp120:
	.cfi_offset %rbx, -48
.Ltmp121:
	.cfi_offset %r12, -40
.Ltmp122:
	.cfi_offset %r13, -32
.Ltmp123:
	.cfi_offset %r14, -24
.Ltmp124:
	.cfi_offset %r15, -16
	movq	%rsi, %r13
	movq	%rdi, %r14
	testq	%r14, %r14
	jne	.LBB9_2
# BB#1:                                 # %if.then
	movabsq	$.L.str7, %rdi
	xorq	%rax, %rax
	callq	Perl_croak
.LBB9_2:                                # %if.end
	cmpq	$-2, %r13
	jg	.LBB9_4
# BB#3:                                 # %if.end
	movq	$-1, %r13
.LBB9_4:                                # %if.end
	movzbl	17(%r14), %eax
	testq	$128, %rax
	je	.LBB9_24
# BB#5:                                 # %cond.end
	movq	$80, %rsi
	movq	%r14, %rdi
	callq	Perl_mg_find
	movq	%rax, %r15
	testq	%r15, %r15
	je	.LBB9_24
# BB#6:                                 # %if.then5
	movq	PL_stack_sp(%rip), %r12
	callq	Perl_push_scope
	movabsq	$PL_tmps_floor, %rdi
	callq	Perl_save_int
	movq	PL_tmps_ix(%rip), %rax
	movq	PL_curstackinfo(%rip), %rcx
	movq	%rax, PL_tmps_floor(%rip)
	movq	48(%rcx), %rax
	testq	%rax, %rax
	jne	.LBB9_8
# BB#7:                                 # %if.then7
	movq	$32, %rdi
	movq	$12, %rsi
	callq	Perl_new_stackinfo
	movq	PL_curstackinfo(%rip), %rcx
	movq	%rcx, 40(%rax)
	movq	PL_curstackinfo(%rip), %rcx
	movq	%rax, 48(%rcx)
.LBB9_8:                                # %if.end10
	movq	$2, 32(%rax)
	movq	$-1, 16(%rax)
	movq	(%rax), %rcx
	movq	(%rcx), %rdx
	movq	$0, 8(%rdx)
	movq	PL_curstack(%rip), %rcx
	movq	PL_stack_base(%rip), %rsi
	movq	(%rcx), %rdi
	subq	%rsi, %r12
	movq	$3, %rcx
	sarq	%cl, %r12
	movq	%r12, 8(%rdi)
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
	leaq	(%rcx,%rsi,8), %r12
	movq	%r12, PL_stack_sp(%rip)
	movq	(%rax), %rsi
	movq	%rsi, PL_curstack(%rip)
	movq	PL_markstack_ptr(%rip), %rsi
	movq	PL_markstack_max(%rip), %rdi
	movq	%rax, PL_curstackinfo(%rip)
	addq	$8, %rsi
	movq	%rsi, PL_markstack_ptr(%rip)
	cmpq	%rdi, %rsi
	jne	.LBB9_10
# BB#9:                                 # %if.then23
	callq	Perl_markstack_grow
	movq	PL_stack_base(%rip), %rcx
	movq	PL_markstack_ptr(%rip), %rsi
	movq	PL_stack_max(%rip), %rdx
.LBB9_10:                               # %if.end24
	movq	%r12, %rax
	subq	%rcx, %rax
	movq	$3, %rcx
	sarq	%cl, %rax
	movq	%rax, (%rsi)
	subq	%r12, %rdx
	cmpq	$15, %rdx
	jg	.LBB9_12
# BB#11:                                # %if.then35
	movq	$2, %rdx
	movq	%r12, %rdi
	movq	%r12, %rsi
	callq	Perl_stack_grow
	movq	%rax, %r12
.LBB9_12:                               # %if.end37
	movq	24(%r15), %rax
	testq	%rax, %rax
	jne	.LBB9_14
# BB#13:                                # %cond.false41
	movq	%r14, %rdi
	callq	Perl_newRV
	movq	%rax, %rdi
	callq	Perl_sv_2mortal
.LBB9_14:                               # %cond.end44
	movq	%rax, 8(%r12)
	incq	%r13
	movq	%r13, %rdi
	callq	Perl_newSViv
	movq	%rax, %rdi
	callq	Perl_sv_2mortal
	movq	%rax, 16(%r12)
	addq	$16, %r12
	movq	%r12, PL_stack_sp(%rip)
	movabsq	$.L.str8, %rdi
	movq	$2, %rsi
	callq	Perl_call_method
	movq	PL_curstackinfo(%rip), %rax
	movq	40(%rax), %r14
	movq	PL_stack_sp(%rip), %rbx
	testq	%r14, %r14
	jne	.LBB9_21
# BB#15:                                # %if.then56
	movq	PL_stderrgv(%rip), %rax
	testq	%rax, %rax
	je	.LBB9_19
# BB#16:                                # %land.lhs.true
	movzbl	16(%rax), %ecx
	cmpq	$13, %rcx
	jne	.LBB9_19
# BB#17:                                # %land.lhs.true61
	movq	(%rax), %rax
	movq	56(%rax), %rax
	movq	16(%rax), %rax
	testq	%rax, %rax
	je	.LBB9_19
# BB#18:                                # %land.lhs.true64
	movq	(%rax), %rax
	movq	64(%rax), %rdi
	testq	%rdi, %rdi
	jne	.LBB9_20
.LBB9_19:                               # %cond.false76
	callq	Perl_PerlIO_stderr
	movq	%rax, %rdi
.LBB9_20:                               # %cond.end78
	movabsq	$.L.str1, %rsi
	xorq	%rax, %rax
	callq	PerlIO_printf
	movq	$1, %rdi
	callq	Perl_my_exit
.LBB9_21:                               # %if.end81
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
	jle	.LBB9_23
# BB#22:                                # %if.then103
	callq	Perl_free_tmps
.LBB9_23:                               # %if.end104
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	Perl_pop_scope  # TAILCALL
.LBB9_24:                               # %if.end105
	movq	(%r14), %rax
	movq	16(%rax), %rcx
	cmpq	%rcx, %r13
	jle	.LBB9_25
# BB#32:                                # %if.else138
	movabsq	$PL_sv_undef, %rdx
	movq	%r14, %rdi
	movq	%r13, %rsi
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	Perl_av_store  # TAILCALL
.LBB9_25:                               # %if.then109
	movq	(%rax), %r15
	movzbl	72(%rax), %ecx
	movq	8(%rax), %rbx
	testq	$1, %rcx
	jne	.LBB9_26
	jmp	.LBB9_29
	.align	16, 0x90
.LBB9_28:                               # %while.body126
                                        #   in Loop: Header=BB9_29 Depth=1
	movq	$PL_sv_undef, 8(%r15,%rbx,8)
	leaq	1(%rbx), %rbx
.LBB9_29:                               # %while.body126
                                        # =>This Inner Loop Header: Depth=1
	cmpq	%r13, %rbx
	jl	.LBB9_28
	jmp	.LBB9_30
.LBB9_27:                               # %while.body
                                        #   in Loop: Header=BB9_26 Depth=1
	movq	(%r15,%rbx,8), %rdi
	callq	Perl_sv_free
	movq	$PL_sv_undef, (%r15,%rbx,8)
	leaq	-1(%rbx), %rbx
.LBB9_26:                               # %while.cond.preheader
                                        # =>This Inner Loop Header: Depth=1
	cmpq	%r13, %rbx
	jg	.LBB9_27
.LBB9_30:                               # %if.end129
	movq	(%r14), %rax
	movq	%r13, 8(%rax)
	movzbl	17(%r14), %eax
	testq	$64, %rax
	je	.LBB9_33
# BB#31:                                # %if.then135
	movq	%r14, %rdi
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	Perl_mg_set  # TAILCALL
.LBB9_33:                               # %if.end140
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp125:
	.size	Perl_av_fill, .Ltmp125-Perl_av_fill
	.cfi_endproc

	.globl	Perl_av_push
	.align	16, 0x90
	.type	Perl_av_push,@function
Perl_av_push:                           # @Perl_av_push
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
	movq	%rsi, %r14
	movq	%rdi, %r12
	testq	%r12, %r12
	je	.LBB10_21
# BB#1:                                 # %if.end
	movq	16(%r12), %rax
	testq	$8388608, %rax          # imm = 0x800000
	je	.LBB10_3
# BB#2:                                 # %if.then2
	movabsq	$PL_no_modify, %rdi
	xorq	%rax, %rax
	callq	Perl_croak
	movq	16(%r12), %rax
.LBB10_3:                               # %if.end3
	testq	$32768, %rax            # imm = 0x8000
	je	.LBB10_22
# BB#4:                                 # %cond.end
	movq	$80, %rsi
	movq	%r12, %rdi
	callq	Perl_mg_find
	movq	%rax, %r15
	testq	%r15, %r15
	je	.LBB10_22
# BB#5:                                 # %if.then8
	movq	PL_curstackinfo(%rip), %rax
	movq	48(%rax), %rax
	movq	PL_stack_sp(%rip), %rbx
	testq	%rax, %rax
	jne	.LBB10_7
# BB#6:                                 # %if.then10
	movq	$32, %rdi
	movq	$12, %rsi
	callq	Perl_new_stackinfo
	movq	PL_curstackinfo(%rip), %rcx
	movq	%rcx, 40(%rax)
	movq	PL_curstackinfo(%rip), %rcx
	movq	%rax, 48(%rcx)
.LBB10_7:                               # %if.end13
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
	jne	.LBB10_9
# BB#8:                                 # %if.then25
	callq	Perl_markstack_grow
	movq	PL_stack_base(%rip), %rcx
	movq	PL_markstack_ptr(%rip), %rsi
	movq	PL_stack_max(%rip), %rdx
.LBB10_9:                               # %if.end26
	movq	%rbx, %rax
	subq	%rcx, %rax
	movq	$3, %rcx
	sarq	%cl, %rax
	movq	%rax, (%rsi)
	subq	%rbx, %rdx
	cmpq	$15, %rdx
	jg	.LBB10_11
# BB#10:                                # %if.then37
	movq	$2, %rdx
	movq	%rbx, %rdi
	movq	%rbx, %rsi
	callq	Perl_stack_grow
	movq	%rax, %rbx
.LBB10_11:                              # %if.end39
	movq	24(%r15), %rax
	testq	%rax, %rax
	jne	.LBB10_13
# BB#12:                                # %cond.false43
	movq	%r12, %rdi
	callq	Perl_newRV
	movq	%rax, %rdi
	callq	Perl_sv_2mortal
.LBB10_13:                              # %cond.end46
	movq	%rax, 8(%rbx)
	movq	%r14, 16(%rbx)
	addq	$16, %rbx
	movq	%rbx, PL_stack_sp(%rip)
	callq	Perl_push_scope
	movabsq	$.L.str3, %rdi
	movq	$2, %rsi
	callq	Perl_call_method
	callq	Perl_pop_scope
	movq	PL_curstackinfo(%rip), %rax
	movq	40(%rax), %r14
	movq	PL_stack_sp(%rip), %rbx
	testq	%r14, %r14
	jne	.LBB10_20
# BB#14:                                # %if.then56
	movq	PL_stderrgv(%rip), %rax
	testq	%rax, %rax
	je	.LBB10_18
# BB#15:                                # %land.lhs.true
	movzbl	16(%rax), %ecx
	cmpq	$13, %rcx
	jne	.LBB10_18
# BB#16:                                # %land.lhs.true61
	movq	(%rax), %rax
	movq	56(%rax), %rax
	movq	16(%rax), %rax
	testq	%rax, %rax
	je	.LBB10_18
# BB#17:                                # %land.lhs.true64
	movq	(%rax), %rax
	movq	64(%rax), %rdi
	testq	%rdi, %rdi
	jne	.LBB10_19
.LBB10_18:                              # %cond.false76
	callq	Perl_PerlIO_stderr
	movq	%rax, %rdi
.LBB10_19:                              # %cond.end78
	movabsq	$.L.str1, %rsi
	xorq	%rax, %rax
	callq	PerlIO_printf
	movq	$1, %rdi
	callq	Perl_my_exit
.LBB10_20:                              # %if.end81
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
.LBB10_21:                              # %return
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.LBB10_22:                              # %if.end102
	movq	(%r12), %rax
	movq	8(%rax), %rsi
	incq	%rsi
	movq	%r12, %rdi
	movq	%r14, %rdx
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	jmp	Perl_av_store  # TAILCALL
.Ltmp140:
	.size	Perl_av_push, .Ltmp140-Perl_av_push
	.cfi_endproc

	.globl	Perl_av_pop
	.align	16, 0x90
	.type	Perl_av_pop,@function
Perl_av_pop:                            # @Perl_av_pop
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
	movq	%rdi, %r12
	testq	%r12, %r12
	movabsq	$PL_sv_undef, %r14
	je	.LBB11_31
# BB#1:                                 # %if.end
	movq	16(%r12), %rax
	testq	$8388608, %rax          # imm = 0x800000
	je	.LBB11_3
# BB#2:                                 # %if.then3
	movabsq	$PL_no_modify, %rdi
	xorq	%rax, %rax
	callq	Perl_croak
	movq	16(%r12), %rax
.LBB11_3:                               # %if.end4
	testq	$32768, %rax            # imm = 0x8000
	je	.LBB11_6
# BB#4:                                 # %cond.end
	movq	$80, %rsi
	movq	%r12, %rdi
	callq	Perl_mg_find
	movq	%rax, %r15
	testq	%r15, %r15
	je	.LBB11_5
# BB#8:                                 # %if.then9
	movq	PL_curstackinfo(%rip), %rax
	movq	48(%rax), %rax
	movq	PL_stack_sp(%rip), %rbx
	testq	%rax, %rax
	jne	.LBB11_10
# BB#9:                                 # %if.then11
	movq	$32, %rdi
	movq	$12, %rsi
	callq	Perl_new_stackinfo
	movq	PL_curstackinfo(%rip), %rcx
	movq	%rcx, 40(%rax)
	movq	PL_curstackinfo(%rip), %rcx
	movq	%rax, 48(%rcx)
.LBB11_10:                              # %if.end14
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
	jne	.LBB11_12
# BB#11:                                # %if.then26
	callq	Perl_markstack_grow
	movq	PL_stack_base(%rip), %rcx
	movq	PL_markstack_ptr(%rip), %rsi
	movq	PL_stack_max(%rip), %rdx
.LBB11_12:                              # %if.end27
	movq	%rbx, %rax
	subq	%rcx, %rax
	movq	$3, %rcx
	sarq	%cl, %rax
	movq	%rax, (%rsi)
	subq	%rbx, %rdx
	cmpq	$7, %rdx
	jg	.LBB11_14
# BB#13:                                # %if.then38
	movq	$1, %rdx
	movq	%rbx, %rdi
	movq	%rbx, %rsi
	callq	Perl_stack_grow
	movq	%rax, %rbx
.LBB11_14:                              # %if.end40
	movq	24(%r15), %rax
	testq	%rax, %rax
	jne	.LBB11_16
# BB#15:                                # %cond.false45
	movq	%r12, %rdi
	callq	Perl_newRV
	movq	%rax, %rdi
	callq	Perl_sv_2mortal
.LBB11_16:                              # %cond.end48
	movq	%rax, 8(%rbx)
	addq	$8, %rbx
	movq	%rbx, PL_stack_sp(%rip)
	callq	Perl_push_scope
	movabsq	$.L.str4, %rdi
	xorq	%rsi, %rsi
	callq	Perl_call_method
	testq	%rax, %rax
	je	.LBB11_17
# BB#18:                                # %if.then53
	movq	PL_stack_sp(%rip), %rax
	leaq	-8(%rax), %rcx
	movq	%rcx, PL_stack_sp(%rip)
	movq	(%rax), %rdi
	callq	Perl_newSVsv
	movq	%rax, %r14
	jmp	.LBB11_19
.LBB11_5:                               # %cond.end.if.end108_crit_edge
	movq	16(%r12), %rax
.LBB11_6:                               # %if.end108
	testq	$32768, %rax            # imm = 0x8000
	je	.LBB11_27
# BB#7:                                 # %cond.true112
	movq	%r12, %rdi
	callq	Perl_mg_size
	jmp	.LBB11_28
.LBB11_27:                              # %cond.false114
	movq	(%r12), %rax
	movq	8(%rax), %rax
.LBB11_28:                              # %cond.end117
	testq	%rax, %rax
	js	.LBB11_31
# BB#29:                                # %if.end121
	movq	(%r12), %rax
	movq	8(%rax), %rcx
	movq	(%rax), %rdx
	movq	(%rdx,%rcx,8), %r14
	leaq	-1(%rcx), %rsi
	movq	%rsi, 8(%rax)
	movq	$PL_sv_undef, (%rdx,%rcx,8)
	movzbl	17(%r12), %eax
	testq	$64, %rax
	je	.LBB11_31
# BB#30:                                # %if.then134
	movq	%r12, %rdi
	callq	Perl_mg_set
	jmp	.LBB11_31
.LBB11_17:
	movabsq	$PL_sv_undef, %r14
.LBB11_19:                              # %if.end56
	callq	Perl_pop_scope
	movq	PL_curstackinfo(%rip), %rax
	movq	40(%rax), %r15
	movq	PL_stack_sp(%rip), %rbx
	testq	%r15, %r15
	jne	.LBB11_26
# BB#20:                                # %if.then62
	movq	PL_stderrgv(%rip), %rax
	testq	%rax, %rax
	je	.LBB11_24
# BB#21:                                # %land.lhs.true
	movzbl	16(%rax), %ecx
	cmpq	$13, %rcx
	jne	.LBB11_24
# BB#22:                                # %land.lhs.true67
	movq	(%rax), %rax
	movq	56(%rax), %rax
	movq	16(%rax), %rax
	testq	%rax, %rax
	je	.LBB11_24
# BB#23:                                # %land.lhs.true70
	movq	(%rax), %rax
	movq	64(%rax), %rdi
	testq	%rdi, %rdi
	jne	.LBB11_25
.LBB11_24:                              # %cond.false82
	callq	Perl_PerlIO_stderr
	movq	%rax, %rdi
.LBB11_25:                              # %cond.end84
	movabsq	$.L.str1, %rsi
	xorq	%rax, %rax
	callq	PerlIO_printf
	movq	$1, %rdi
	callq	Perl_my_exit
.LBB11_26:                              # %if.end87
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
.LBB11_31:                              # %return
	movq	%r14, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.Ltmp155:
	.size	Perl_av_pop, .Ltmp155-Perl_av_pop
	.cfi_endproc

	.globl	Perl_av_unshift
	.align	16, 0x90
	.type	Perl_av_unshift,@function
Perl_av_unshift:                        # @Perl_av_unshift
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
.Ltmp167:
	.cfi_offset %rbx, -48
.Ltmp168:
	.cfi_offset %r12, -40
.Ltmp169:
	.cfi_offset %r13, -32
.Ltmp170:
	.cfi_offset %r14, -24
.Ltmp171:
	.cfi_offset %r15, -16
	movq	%rsi, %rbx
	movq	%rdi, %r14
	testq	%r14, %r14
	je	.LBB12_39
# BB#1:                                 # %if.end
	movq	16(%r14), %rax
	testq	$8388608, %rax          # imm = 0x800000
	je	.LBB12_3
# BB#2:                                 # %if.then2
	movabsq	$PL_no_modify, %rdi
	xorq	%rax, %rax
	callq	Perl_croak
	movq	16(%r14), %rax
.LBB12_3:                               # %if.end3
	testq	$32768, %rax            # imm = 0x8000
	je	.LBB12_26
# BB#4:                                 # %cond.end
	movq	$80, %rsi
	movq	%r14, %rdi
	callq	Perl_mg_find
	movq	%rax, %r15
	testq	%r15, %r15
	je	.LBB12_26
# BB#5:                                 # %if.then8
	movq	PL_curstackinfo(%rip), %rax
	movq	48(%rax), %rax
	movq	PL_stack_sp(%rip), %r12
	testq	%rax, %rax
	jne	.LBB12_7
# BB#6:                                 # %if.then10
	movq	$32, %rdi
	movq	$12, %rsi
	callq	Perl_new_stackinfo
	movq	PL_curstackinfo(%rip), %rcx
	movq	%rcx, 40(%rax)
	movq	PL_curstackinfo(%rip), %rcx
	movq	%rax, 48(%rcx)
.LBB12_7:                               # %if.end13
	movq	$2, 32(%rax)
	movq	$-1, 16(%rax)
	movq	(%rax), %rcx
	movq	(%rcx), %rdx
	movq	$0, 8(%rdx)
	movq	PL_curstack(%rip), %rcx
	movq	PL_stack_base(%rip), %rsi
	movq	(%rcx), %rdi
	subq	%rsi, %r12
	movq	$3, %rcx
	sarq	%cl, %r12
	movq	%r12, 8(%rdi)
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
	leaq	(%rcx,%rsi,8), %r12
	movq	%r12, PL_stack_sp(%rip)
	movq	(%rax), %rsi
	movq	%rsi, PL_curstack(%rip)
	movq	PL_markstack_ptr(%rip), %rsi
	movq	PL_markstack_max(%rip), %rdi
	movq	%rax, PL_curstackinfo(%rip)
	addq	$8, %rsi
	movq	%rsi, PL_markstack_ptr(%rip)
	cmpq	%rdi, %rsi
	jne	.LBB12_9
# BB#8:                                 # %if.then25
	callq	Perl_markstack_grow
	movq	PL_stack_base(%rip), %rcx
	movq	PL_markstack_ptr(%rip), %rsi
	movq	PL_stack_max(%rip), %rdx
.LBB12_9:                               # %if.end26
	movq	%r12, %rax
	subq	%rcx, %rax
	movq	$3, %rcx
	sarq	%cl, %rax
	movq	%rax, (%rsi)
	subq	%r12, %rdx
	movq	$3, %rcx
	sarq	%cl, %rdx
	leaq	1(%rbx), %r13
	movq	$32, %rcx
	movq	%r13, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	%rax, %rdx
	jge	.LBB12_11
# BB#10:                                # %if.then39
	movq	%r12, %rdi
	movq	%r12, %rsi
	movq	%r13, %rdx
	callq	Perl_stack_grow
	movq	%rax, %r12
.LBB12_11:                              # %if.end43
	movq	24(%r15), %rax
	testq	%rax, %rax
	jne	.LBB12_13
# BB#12:                                # %cond.false47
	movq	%r14, %rdi
	callq	Perl_newRV
	movq	%rax, %rdi
	callq	Perl_sv_2mortal
.LBB12_13:                              # %cond.end50
	movq	%rax, 8(%r12)
	testq	%rbx, %rbx
	jle	.LBB12_14
# BB#15:                                # %while.body.preheader
	leaq	16(%r12), %rax
	.align	16, 0x90
.LBB12_16:                              # %while.body
                                        # =>This Inner Loop Header: Depth=1
	decq	%rbx
	testq	%rbx, %rbx
	movq	$PL_sv_undef, (%rax)
	leaq	8(%rax), %rax
	jg	.LBB12_16
# BB#17:                                # %while.cond.while.end_crit_edge
	leaq	(%r12,%r13,8), %r12
	jmp	.LBB12_18
.LBB12_26:                              # %if.end109
	testq	%rbx, %rbx
	jle	.LBB12_39
# BB#27:                                # %if.end113
	movq	(%r14), %rax
	movzbl	72(%rax), %ecx
	andq	$3, %rcx
	cmpq	$2, %rcx
	jne	.LBB12_29
# BB#28:                                # %if.then124
	xorq	%rax, %rax
	movq	%r14, %rdi
	callq	av_reify
	movq	(%r14), %rax
.LBB12_29:                              # %if.end126
	movq	(%rax), %rdx
	movq	56(%rax), %rcx
	movq	%rdx, %rdi
	subq	%rcx, %rdi
	movq	$3, %rcx
	sarq	%cl, %rdi
	testq	%rdi, %rdi
	je	.LBB12_33
# BB#30:                                # %if.then135
	cmpq	%rbx, %rdi
	movq	%rbx, %rsi
	jg	.LBB12_32
# BB#31:                                # %if.then135
	movq	%rdi, %rsi
.LBB12_32:                              # %if.then135
	subq	%rsi, %rbx
	movq	16(%rax), %rcx
	addq	%rsi, %rcx
	addq	%rsi, 8(%rax)
	movq	%rcx, 16(%rax)
	movq	$3, %rcx
	shlq	%cl, %rsi
	subq	%rsi, %rdx
	movq	%rdx, (%rax)
.LBB12_33:                              # %if.end150
	testq	%rbx, %rbx
	je	.LBB12_39
# BB#34:                                # %if.then152
	movq	8(%rax), %r13
	testq	%r13, %r13
	movq	%r13, %r12
	jns	.LBB12_36
# BB#35:                                # %if.then152
	xorq	%r12, %r12
.LBB12_36:                              # %if.then152
	addq	%r12, %rbx
	leaq	(%rbx,%r13), %rsi
	movq	%r14, %rdi
	callq	Perl_av_extend
	movq	(%r14), %rax
	addq	%rbx, 8(%rax)
	movq	(%rax), %r15
	leaq	(%r15,%rbx,8), %rdi
	leaq	8(,%r13,8), %rdx
	movq	%r15, %rsi
	callq	memmove
	.align	16, 0x90
.LBB12_37:                              # %do.body
                                        # =>This Inner Loop Header: Depth=1
	movq	$PL_sv_undef, -8(%r15,%rbx,8)
	decq	%rbx
	jne	.LBB12_37
# BB#38:                                # %do.end
	movq	(%r14), %rax
	movq	8(%rax), %rcx
	subq	%r12, %rcx
	subq	%r12, 16(%rax)
	movq	%rcx, 8(%rax)
	movq	$3, %rcx
	shlq	%cl, %r12
	addq	%r12, (%rax)
	jmp	.LBB12_39
.LBB12_14:
	addq	$8, %r12
.LBB12_18:                              # %while.end
	movq	%r12, PL_stack_sp(%rip)
	callq	Perl_push_scope
	movabsq	$.L.str5, %rdi
	movq	$2, %rsi
	callq	Perl_call_method
	callq	Perl_pop_scope
	movq	PL_curstackinfo(%rip), %rax
	movq	40(%rax), %r14
	movq	PL_stack_sp(%rip), %rbx
	testq	%r14, %r14
	jne	.LBB12_25
# BB#19:                                # %if.then62
	movq	PL_stderrgv(%rip), %rax
	testq	%rax, %rax
	je	.LBB12_23
# BB#20:                                # %land.lhs.true
	movzbl	16(%rax), %ecx
	cmpq	$13, %rcx
	jne	.LBB12_23
# BB#21:                                # %land.lhs.true68
	movq	(%rax), %rax
	movq	56(%rax), %rax
	movq	16(%rax), %rax
	testq	%rax, %rax
	je	.LBB12_23
# BB#22:                                # %land.lhs.true71
	movq	(%rax), %rax
	movq	64(%rax), %rdi
	testq	%rdi, %rdi
	jne	.LBB12_24
.LBB12_23:                              # %cond.false83
	callq	Perl_PerlIO_stderr
	movq	%rax, %rdi
.LBB12_24:                              # %cond.end85
	movabsq	$.L.str1, %rsi
	xorq	%rax, %rax
	callq	PerlIO_printf
	movq	$1, %rdi
	callq	Perl_my_exit
.LBB12_25:                              # %if.end88
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
.LBB12_39:                              # %if.end183
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp172:
	.size	Perl_av_unshift, .Ltmp172-Perl_av_unshift
	.cfi_endproc

	.globl	Perl_av_shift
	.align	16, 0x90
	.type	Perl_av_shift,@function
Perl_av_shift:                          # @Perl_av_shift
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp178:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp179:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp180:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp181:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp182:
	.cfi_def_cfa_offset 48
.Ltmp183:
	.cfi_offset %rbx, -40
.Ltmp184:
	.cfi_offset %r12, -32
.Ltmp185:
	.cfi_offset %r14, -24
.Ltmp186:
	.cfi_offset %r15, -16
	movq	%rdi, %r12
	testq	%r12, %r12
	movabsq	$PL_sv_undef, %r14
	je	.LBB13_33
# BB#1:                                 # %if.end
	movq	16(%r12), %rax
	testq	$8388608, %rax          # imm = 0x800000
	je	.LBB13_3
# BB#2:                                 # %if.then3
	movabsq	$PL_no_modify, %rdi
	xorq	%rax, %rax
	callq	Perl_croak
	movq	16(%r12), %rax
.LBB13_3:                               # %if.end4
	testq	$32768, %rax            # imm = 0x8000
	je	.LBB13_6
# BB#4:                                 # %cond.end
	movq	$80, %rsi
	movq	%r12, %rdi
	callq	Perl_mg_find
	movq	%rax, %r15
	testq	%r15, %r15
	je	.LBB13_5
# BB#8:                                 # %if.then9
	movq	PL_curstackinfo(%rip), %rax
	movq	48(%rax), %rax
	movq	PL_stack_sp(%rip), %rbx
	testq	%rax, %rax
	jne	.LBB13_10
# BB#9:                                 # %if.then11
	movq	$32, %rdi
	movq	$12, %rsi
	callq	Perl_new_stackinfo
	movq	PL_curstackinfo(%rip), %rcx
	movq	%rcx, 40(%rax)
	movq	PL_curstackinfo(%rip), %rcx
	movq	%rax, 48(%rcx)
.LBB13_10:                              # %if.end14
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
	jne	.LBB13_12
# BB#11:                                # %if.then26
	callq	Perl_markstack_grow
	movq	PL_stack_base(%rip), %rcx
	movq	PL_markstack_ptr(%rip), %rsi
	movq	PL_stack_max(%rip), %rdx
.LBB13_12:                              # %if.end27
	movq	%rbx, %rax
	subq	%rcx, %rax
	movq	$3, %rcx
	sarq	%cl, %rax
	movq	%rax, (%rsi)
	subq	%rbx, %rdx
	cmpq	$7, %rdx
	jg	.LBB13_14
# BB#13:                                # %if.then38
	movq	$1, %rdx
	movq	%rbx, %rdi
	movq	%rbx, %rsi
	callq	Perl_stack_grow
	movq	%rax, %rbx
.LBB13_14:                              # %if.end40
	movq	24(%r15), %rax
	testq	%rax, %rax
	jne	.LBB13_16
# BB#15:                                # %cond.false45
	movq	%r12, %rdi
	callq	Perl_newRV
	movq	%rax, %rdi
	callq	Perl_sv_2mortal
.LBB13_16:                              # %cond.end48
	movq	%rax, 8(%rbx)
	addq	$8, %rbx
	movq	%rbx, PL_stack_sp(%rip)
	callq	Perl_push_scope
	movabsq	$.L.str6, %rdi
	xorq	%rsi, %rsi
	callq	Perl_call_method
	testq	%rax, %rax
	je	.LBB13_17
# BB#18:                                # %if.then53
	movq	PL_stack_sp(%rip), %rax
	leaq	-8(%rax), %rcx
	movq	%rcx, PL_stack_sp(%rip)
	movq	(%rax), %rdi
	callq	Perl_newSVsv
	movq	%rax, %r14
	jmp	.LBB13_19
.LBB13_5:                               # %cond.end.if.end108_crit_edge
	movq	16(%r12), %rax
.LBB13_6:                               # %if.end108
	testq	$32768, %rax            # imm = 0x8000
	je	.LBB13_27
# BB#7:                                 # %cond.true112
	movq	%r12, %rdi
	callq	Perl_mg_size
	jmp	.LBB13_28
.LBB13_27:                              # %cond.false114
	movq	(%r12), %rax
	movq	8(%rax), %rax
.LBB13_28:                              # %cond.end117
	testq	%rax, %rax
	js	.LBB13_33
# BB#29:                                # %if.end121
	movq	(%r12), %rax
	movq	(%rax), %rcx
	movzbl	72(%rax), %edx
	movq	(%rcx), %r14
	testq	$1, %rdx
	je	.LBB13_31
# BB#30:                                # %if.then127
	movq	$PL_sv_undef, (%rcx)
	movq	(%r12), %rax
	movq	(%rax), %rcx
.LBB13_31:                              # %if.end130
	addq	$8, %rcx
	movq	%rcx, (%rax)
	movq	16(%rax), %rcx
	decq	%rcx
	decq	8(%rax)
	movq	%rcx, 16(%rax)
	movzbl	17(%r12), %eax
	testq	$64, %rax
	je	.LBB13_33
# BB#32:                                # %if.then143
	movq	%r12, %rdi
	callq	Perl_mg_set
	jmp	.LBB13_33
.LBB13_17:
	movabsq	$PL_sv_undef, %r14
.LBB13_19:                              # %if.end56
	callq	Perl_pop_scope
	movq	PL_curstackinfo(%rip), %rax
	movq	40(%rax), %r15
	movq	PL_stack_sp(%rip), %rbx
	testq	%r15, %r15
	jne	.LBB13_26
# BB#20:                                # %if.then62
	movq	PL_stderrgv(%rip), %rax
	testq	%rax, %rax
	je	.LBB13_24
# BB#21:                                # %land.lhs.true
	movzbl	16(%rax), %ecx
	cmpq	$13, %rcx
	jne	.LBB13_24
# BB#22:                                # %land.lhs.true67
	movq	(%rax), %rax
	movq	56(%rax), %rax
	movq	16(%rax), %rax
	testq	%rax, %rax
	je	.LBB13_24
# BB#23:                                # %land.lhs.true70
	movq	(%rax), %rax
	movq	64(%rax), %rdi
	testq	%rdi, %rdi
	jne	.LBB13_25
.LBB13_24:                              # %cond.false82
	callq	Perl_PerlIO_stderr
	movq	%rax, %rdi
.LBB13_25:                              # %cond.end84
	movabsq	$.L.str1, %rsi
	xorq	%rax, %rax
	callq	PerlIO_printf
	movq	$1, %rdi
	callq	Perl_my_exit
.LBB13_26:                              # %if.end87
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
.LBB13_33:                              # %return
	movq	%r14, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.Ltmp187:
	.size	Perl_av_shift, .Ltmp187-Perl_av_shift
	.cfi_endproc

	.globl	Perl_av_len
	.align	16, 0x90
	.type	Perl_av_len,@function
Perl_av_len:                            # @Perl_av_len
	.cfi_startproc
# BB#0:                                 # %entry
	movzbl	17(%rdi), %eax
	testq	$128, %rax
	je	.LBB14_1
# BB#2:                                 # %cond.true
	jmp	Perl_mg_size  # TAILCALL
.LBB14_1:                               # %cond.end
	movq	(%rdi), %rax
	movq	8(%rax), %rax
	retq
.Ltmp188:
	.size	Perl_av_len, .Ltmp188-Perl_av_len
	.cfi_endproc

	.globl	Perl_av_delete
	.align	16, 0x90
	.type	Perl_av_delete,@function
Perl_av_delete:                         # @Perl_av_delete
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
.Ltmp200:
	.cfi_offset %rbx, -48
.Ltmp201:
	.cfi_offset %r12, -40
.Ltmp202:
	.cfi_offset %r13, -32
.Ltmp203:
	.cfi_offset %r14, -24
.Ltmp204:
	.cfi_offset %r15, -16
	movq	%rdx, %r15
	movq	%rsi, %rbx
	movq	%rdi, %r12
	xorq	%r14, %r14
	testq	%r12, %r12
	je	.LBB15_48
# BB#1:                                 # %if.end
	movq	16(%r12), %rax
	testq	$8388608, %rax          # imm = 0x800000
	je	.LBB15_3
# BB#2:                                 # %if.then2
	movabsq	$PL_no_modify, %rdi
	xorq	%rax, %rax
	callq	Perl_croak
	movq	16(%r12), %rax
.LBB15_3:                               # %if.end3
	testq	$32768, %rax            # imm = 0x8000
	je	.LBB15_29
# BB#4:                                 # %if.then7
	movq	$80, %rsi
	movq	%r12, %rdi
	callq	Perl_mg_find
	movq	%rax, %r13
	testq	%r13, %r13
	jne	.LBB15_6
# BB#5:                                 # %lor.lhs.false
	movq	$68, %rsi
	movq	%r12, %rdi
	callq	Perl_mg_find
	testq	%rax, %rax
	je	.LBB15_29
.LBB15_6:                               # %if.then11
	testq	%rbx, %rbx
	jns	.LBB15_26
# BB#7:                                 # %if.then12
	testq	%r13, %r13
	je	.LBB15_22
# BB#8:                                 # %if.then14
	movq	24(%r13), %rax
	testq	%rax, %rax
	jne	.LBB15_10
# BB#9:                                 # %cond.false
	movq	%r12, %rdi
	callq	Perl_newRV
	movq	%rax, %rdi
	callq	Perl_sv_2mortal
.LBB15_10:                              # %cond.end
	movq	(%rax), %rax
	movq	(%rax), %rax
	movq	(%rax), %rax
	movq	48(%rax), %rdi
	movabsq	$.L.str2, %rsi
	movq	$16, %rdx
	xorq	%rcx, %rcx
	callq	Perl_hv_fetch
	testq	%rax, %rax
	je	.LBB15_22
# BB#11:                                # %land.lhs.true
	movq	(%rax), %rax
	movq	(%rax), %rax
	movq	56(%rax), %rax
	movq	(%rax), %rdi
	testq	%rdi, %rdi
	je	.LBB15_22
# BB#12:                                # %cond.false25
	movq	16(%rdi), %rax
	testq	$262144, %rax           # imm = 0x40000
	je	.LBB15_17
# BB#13:                                # %cond.true32
	movq	(%rdi), %rax
	testq	%rax, %rax
	movq	%rax, PL_Xpv(%rip)
	je	.LBB15_22
# BB#14:                                # %land.lhs.true38
	movq	8(%rax), %rcx
	cmpq	$1, %rcx
	ja	.LBB15_26
# BB#15:                                # %lor.lhs.false40
	testq	%rcx, %rcx
	je	.LBB15_22
# BB#16:                                # %land.lhs.true43
	movq	(%rax), %rax
	movzbl	(%rax), %eax
	cmpq	$48, %rax
	je	.LBB15_22
	jmp	.LBB15_26
.LBB15_17:                              # %cond.false48
	testq	$65536, %rax            # imm = 0x10000
	je	.LBB15_19
# BB#18:                                # %cond.true55
	movq	(%rdi), %rax
	cmpq	$0, 24(%rax)
	jne	.LBB15_26
	jmp	.LBB15_22
.LBB15_19:                              # %cond.false62
	testq	$131072, %rax           # imm = 0x20000
	je	.LBB15_21
# BB#20:                                # %cond.true69
	movq	(%rdi), %rax
	movq	32(%rax), %rdi
	xorq	%rsi, %rsi
	callq	__nedf2
	testq	%rax, %rax
	jne	.LBB15_26
	jmp	.LBB15_22
.LBB15_21:                              # %cond.false76
	callq	Perl_sv_2bool
	testq	$255, %rax
	jne	.LBB15_26
.LBB15_22:                              # %if.then87
	movzbl	17(%r12), %eax
	testq	$128, %rax
	je	.LBB15_24
# BB#23:                                # %cond.true91
	movq	%r12, %rdi
	callq	Perl_mg_size
	jmp	.LBB15_25
.LBB15_24:                              # %cond.false93
	movq	(%r12), %rax
	movq	8(%rax), %rax
.LBB15_25:                              # %cond.end95
	addq	%rax, %rbx
	incq	%rbx
	js	.LBB15_48
.LBB15_26:                              # %if.end103
	movq	$1, %rdx
	movq	%r12, %rdi
	movq	%rbx, %rsi
	callq	Perl_av_fetch
	testq	%rax, %rax
	je	.LBB15_29
# BB#27:                                # %if.then106
	movq	(%rax), %rbx
	movq	%rbx, %rdi
	callq	Perl_mg_clear
	movq	$112, %rsi
	movq	%rbx, %rdi
	callq	Perl_mg_find
	testq	%rax, %rax
	je	.LBB15_48
# BB#28:                                # %if.then110
	movq	$112, %rsi
	movq	%rbx, %rdi
	callq	Perl_sv_unmagic
	movq	%rbx, %r14
	jmp	.LBB15_48
.LBB15_29:                              # %if.end115
	testq	%rbx, %rbx
	jns	.LBB15_34
# BB#30:                                # %if.then118
	movzbl	17(%r12), %eax
	testq	$128, %rax
	je	.LBB15_32
# BB#31:                                # %cond.true122
	movq	%r12, %rdi
	callq	Perl_mg_size
	jmp	.LBB15_33
.LBB15_32:                              # %cond.false124
	movq	(%r12), %rax
	movq	8(%rax), %rax
.LBB15_33:                              # %cond.end127
	addq	%rax, %rbx
	incq	%rbx
	js	.LBB15_48
.LBB15_34:                              # %if.end135
	movq	(%r12), %rcx
	movq	8(%rcx), %rax
	cmpq	%rax, %rbx
	jg	.LBB15_48
# BB#35:                                # %if.else
	movzbl	72(%rcx), %edx
	andq	$3, %rdx
	cmpq	$2, %rdx
	jne	.LBB15_37
# BB#36:                                # %if.then151
	xorq	%rax, %rax
	movq	%r12, %rdi
	callq	av_reify
	movq	(%r12), %rcx
	movq	8(%rcx), %rax
.LBB15_37:                              # %if.end153
	movq	(%rcx), %rcx
	movq	(%rcx,%rbx,8), %r13
	cmpq	%rax, %rbx
	movq	$PL_sv_undef, (%rcx,%rbx,8)
	jne	.LBB15_42
# BB#38:                                # %do.body.preheader
	movq	(%r12), %rax
	movq	8(%rax), %rsi
	decq	%rsi
	movabsq	$PL_sv_undef, %rcx
	.align	16, 0x90
.LBB15_39:                              # %do.body
                                        # =>This Inner Loop Header: Depth=1
	movq	%rsi, %rdx
	testq	%rbx, %rbx
	jle	.LBB15_41
# BB#40:                                # %land.rhs
                                        #   in Loop: Header=BB15_39 Depth=1
	movq	(%rax), %rsi
	movq	-8(%rsi,%rbx,8), %rdi
	decq	%rbx
	leaq	-1(%rdx), %rsi
	cmpq	%rcx, %rdi
	je	.LBB15_39
.LBB15_41:                              # %if.end177.loopexit
	movq	%rdx, 8(%rax)
.LBB15_42:                              # %if.end177
	movzbl	17(%r12), %eax
	testq	$64, %rax
	je	.LBB15_44
# BB#43:                                # %if.then181
	movq	%r12, %rdi
	callq	Perl_mg_set
.LBB15_44:                              # %if.end184
	testq	$2, %r15
	je	.LBB15_46
# BB#45:                                # %if.then187
	movq	%r13, %rdi
	callq	Perl_sv_free
	jmp	.LBB15_48
.LBB15_46:                              # %if.else188
	movq	(%r12), %rax
	movzbl	72(%rax), %eax
	testq	$1, %rax
	je	.LBB15_47
# BB#49:                                # %if.then194
	movq	%r13, %rdi
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	Perl_sv_2mortal  # TAILCALL
.LBB15_47:
	movq	%r13, %r14
.LBB15_48:                              # %return
	movq	%r14, %rax
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp205:
	.size	Perl_av_delete, .Ltmp205-Perl_av_delete
	.cfi_endproc

	.globl	Perl_av_exists
	.align	16, 0x90
	.type	Perl_av_exists,@function
Perl_av_exists:                         # @Perl_av_exists
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp211:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp212:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp213:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp214:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp215:
	.cfi_def_cfa_offset 48
.Ltmp216:
	.cfi_offset %rbx, -40
.Ltmp217:
	.cfi_offset %r12, -32
.Ltmp218:
	.cfi_offset %r14, -24
.Ltmp219:
	.cfi_offset %r15, -16
	movq	%rsi, %r14
	movq	%rdi, %r12
	testq	%r12, %r12
	je	.LBB16_50
# BB#1:                                 # %if.end
	movzbl	17(%r12), %eax
	testq	$128, %rax
	je	.LBB16_42
# BB#2:                                 # %if.then2
	movq	$80, %rsi
	movq	%r12, %rdi
	callq	Perl_mg_find
	movq	%rax, %rbx
	testq	%rbx, %rbx
	jne	.LBB16_4
# BB#3:                                 # %lor.lhs.false
	movq	$68, %rsi
	movq	%r12, %rdi
	callq	Perl_mg_find
	testq	%rax, %rax
	je	.LBB16_42
.LBB16_4:                               # %if.then6
	callq	Perl_sv_newmortal
	movq	%rax, %r15
	testq	%r14, %r14
	jns	.LBB16_24
# BB#5:                                 # %if.then8
	testq	%rbx, %rbx
	je	.LBB16_20
# BB#6:                                 # %if.then10
	movq	24(%rbx), %rax
	testq	%rax, %rax
	jne	.LBB16_8
# BB#7:                                 # %cond.false
	movq	%r12, %rdi
	callq	Perl_newRV
	movq	%rax, %rdi
	callq	Perl_sv_2mortal
.LBB16_8:                               # %cond.end
	movq	(%rax), %rax
	movq	(%rax), %rax
	movq	(%rax), %rax
	movq	48(%rax), %rdi
	movabsq	$.L.str2, %rsi
	movq	$16, %rdx
	xorq	%rcx, %rcx
	callq	Perl_hv_fetch
	testq	%rax, %rax
	je	.LBB16_20
# BB#9:                                 # %land.lhs.true
	movq	(%rax), %rax
	movq	(%rax), %rax
	movq	56(%rax), %rax
	movq	(%rax), %rdi
	testq	%rdi, %rdi
	je	.LBB16_20
# BB#10:                                # %cond.false21
	movq	16(%rdi), %rax
	testq	$262144, %rax           # imm = 0x40000
	je	.LBB16_15
# BB#11:                                # %cond.true28
	movq	(%rdi), %rax
	testq	%rax, %rax
	movq	%rax, PL_Xpv(%rip)
	je	.LBB16_20
# BB#12:                                # %land.lhs.true34
	movq	8(%rax), %rcx
	cmpq	$1, %rcx
	ja	.LBB16_24
# BB#13:                                # %lor.lhs.false36
	testq	%rcx, %rcx
	je	.LBB16_20
# BB#14:                                # %land.lhs.true39
	movq	(%rax), %rax
	movzbl	(%rax), %eax
	cmpq	$48, %rax
	je	.LBB16_20
	jmp	.LBB16_24
.LBB16_15:                              # %cond.false44
	testq	$65536, %rax            # imm = 0x10000
	je	.LBB16_17
# BB#16:                                # %cond.true51
	movq	(%rdi), %rax
	cmpq	$0, 24(%rax)
	jne	.LBB16_24
	jmp	.LBB16_20
.LBB16_17:                              # %cond.false58
	testq	$131072, %rax           # imm = 0x20000
	je	.LBB16_19
# BB#18:                                # %cond.true65
	movq	(%rdi), %rax
	movq	32(%rax), %rdi
	xorq	%rsi, %rsi
	callq	__nedf2
	testq	%rax, %rax
	jne	.LBB16_24
	jmp	.LBB16_20
.LBB16_19:                              # %cond.false72
	callq	Perl_sv_2bool
	testq	$255, %rax
	jne	.LBB16_24
.LBB16_20:                              # %if.then83
	movzbl	17(%r12), %eax
	testq	$128, %rax
	je	.LBB16_22
# BB#21:                                # %cond.true87
	movq	%r12, %rdi
	callq	Perl_mg_size
	jmp	.LBB16_23
.LBB16_22:                              # %cond.false89
	movq	(%r12), %rax
	movq	8(%rax), %rax
.LBB16_23:                              # %cond.end91
	addq	%rax, %r14
	incq	%r14
	js	.LBB16_50
.LBB16_24:                              # %if.end99
	xorq	%rdx, %rdx
	xorq	%rbx, %rbx
	movq	%r12, %rdi
	movq	%r15, %rsi
	movq	%r14, %rcx
	callq	Perl_mg_copy
	movq	$112, %rsi
	movq	%r15, %rdi
	callq	Perl_mg_find
	movq	%rax, %rcx
	testq	%rcx, %rcx
	je	.LBB16_42
# BB#25:                                # %if.then103
	xorq	%rax, %rax
	movq	%r15, %rdi
	movq	%rcx, %rsi
	callq	magic_existspack
	testq	%r15, %r15
	je	.LBB16_51
# BB#26:                                # %cond.false107
	movq	16(%r15), %rax
	testq	$262144, %rax           # imm = 0x40000
	je	.LBB16_33
# BB#27:                                # %cond.true111
	movq	(%r15), %rax
	xorq	%rbx, %rbx
	testq	%rax, %rax
	movq	%rax, PL_Xpv(%rip)
	je	.LBB16_51
# BB#28:                                # %land.rhs
	movq	8(%rax), %rcx
	cmpq	$1, %rcx
	movq	$1, %rbx
	ja	.LBB16_51
# BB#29:                                # %lor.rhs
	testq	%rcx, %rcx
	movabsq	$0, %rbx
	je	.LBB16_51
# BB#30:                                # %land.rhs119
	movq	(%rax), %rax
	movzbl	(%rax), %eax
	cmpq	$48, %rax
	je	.LBB16_31
# BB#32:                                # %land.rhs119
	movabsq	$1, %rbx
	jmp	.LBB16_51
.LBB16_42:                              # %if.end158
	testq	%r14, %r14
	jns	.LBB16_47
# BB#43:                                # %if.then161
	movzbl	17(%r12), %eax
	testq	$128, %rax
	je	.LBB16_45
# BB#44:                                # %cond.true165
	movq	%r12, %rdi
	callq	Perl_mg_size
	jmp	.LBB16_46
.LBB16_45:                              # %cond.false167
	movq	(%r12), %rax
	movq	8(%rax), %rax
.LBB16_46:                              # %cond.end170
	addq	%rax, %r14
	incq	%r14
	js	.LBB16_50
.LBB16_47:                              # %if.end178
	movq	(%r12), %rax
	movq	8(%rax), %rcx
	cmpq	%rcx, %r14
	jg	.LBB16_50
# BB#48:                                # %land.lhs.true183
	movq	(%rax), %rax
	movq	(%rax,%r14,8), %rax
	movabsq	$PL_sv_undef, %rcx
	cmpq	%rcx, %rax
	je	.LBB16_50
# BB#49:                                # %land.lhs.true183
	movq	$1, %rbx
	testq	%rax, %rax
	jne	.LBB16_51
.LBB16_50:                              # %if.else
	xorq	%rbx, %rbx
.LBB16_51:                              # %return
	movq	%rbx, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.LBB16_33:                              # %cond.false126
	testq	$65536, %rax            # imm = 0x10000
	je	.LBB16_37
# BB#34:                                # %cond.true130
	movq	(%r15), %rax
	cmpq	$0, 24(%rax)
	je	.LBB16_35
# BB#36:                                # %cond.true130
	movabsq	$1, %rbx
	jmp	.LBB16_51
.LBB16_37:                              # %cond.false135
	testq	$131072, %rax           # imm = 0x20000
	je	.LBB16_41
# BB#38:                                # %cond.true139
	movq	(%r15), %rax
	movq	32(%rax), %rdi
	xorq	%rsi, %rsi
	callq	__nedf2
	testq	%rax, %rax
	je	.LBB16_39
# BB#40:                                # %cond.true139
	movabsq	$1, %rbx
	jmp	.LBB16_51
.LBB16_35:
	movabsq	$0, %rbx
	jmp	.LBB16_51
.LBB16_31:
	movabsq	$0, %rbx
	jmp	.LBB16_51
.LBB16_41:                              # %cond.false144
	movq	%r15, %rdi
	callq	Perl_sv_2bool
	movq	%rax, %rbx
	jmp	.LBB16_51
.LBB16_39:
	movabsq	$0, %rbx
	jmp	.LBB16_51
.Ltmp220:
	.size	Perl_av_exists, .Ltmp220-Perl_av_exists
	.cfi_endproc

	.globl	Perl_avhv_keys
	.align	16, 0x90
	.type	Perl_avhv_keys,@function
Perl_avhv_keys:                         # @Perl_avhv_keys
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp224:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp225:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp226:
	.cfi_def_cfa_offset 32
.Ltmp227:
	.cfi_offset %rbx, -24
.Ltmp228:
	.cfi_offset %r14, -16
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	xorq	%r14, %r14
	callq	Perl_av_fetch
	testq	%rax, %rax
	je	.LBB17_14
# BB#1:                                 # %if.then
	movq	(%rax), %rbx
	movq	16(%rbx), %rax
	testq	$8192, %rax             # imm = 0x2000
	je	.LBB17_3
# BB#2:                                 # %if.then2
	movq	%rbx, %rdi
	callq	Perl_mg_get
	movq	16(%rbx), %rax
.LBB17_3:                               # %if.end
	testq	$524288, %rax           # imm = 0x80000
	je	.LBB17_14
# BB#4:                                 # %if.then7
	movq	PL_curcop(%rip), %rax
	cmpq	$0, 88(%rax)
	je	.LBB17_8
# BB#5:                                 # %land.lhs.true
	movq	PL_curcop(%rip), %rax
	movq	88(%rax), %rax
	cmpq	$48, %rax
	je	.LBB17_8
# BB#6:                                 # %land.lhs.true10
	movq	PL_curcop(%rip), %rax
	movq	88(%rax), %rax
	cmpq	$24, %rax
	je	.LBB17_10
# BB#7:                                 # %lor.lhs.false
	movq	PL_curcop(%rip), %rax
	movq	88(%rax), %rax
	movq	(%rax), %rax
	movq	(%rax), %rax
	movzbl	(%rax), %eax
	testq	$16, %rax
	jne	.LBB17_10
.LBB17_8:                               # %lor.lhs.false16
	movq	PL_curcop(%rip), %rax
	cmpq	$0, 88(%rax)
	jne	.LBB17_12
# BB#9:                                 # %land.lhs.true20
	movzbl	PL_dowarn(%rip), %eax
	testq	$1, %rax
	je	.LBB17_12
.LBB17_10:                              # %land.lhs.true24
	movabsq	$.L.str9, %rsi
	movq	%rbx, %rdi
	callq	Perl_sv_isa
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB17_12
# BB#11:                                # %if.then27
	movabsq	$.L.str10, %rsi
	movq	$2, %rdi
	xorq	%rax, %rax
	callq	Perl_warner
.LBB17_12:                              # %if.end28
	movq	(%rbx), %rax
	movq	(%rax), %rax
	movzbl	16(%rax), %ecx
	cmpq	$11, %rcx
	jne	.LBB17_14
# BB#13:                                # %if.then34
	movq	%rax, %r14
	jmp	.LBB17_15
.LBB17_14:                              # %if.end37
	movabsq	$.L.str11, %rdi
	xorq	%rax, %rax
	callq	Perl_croak
.LBB17_15:                              # %return
	movq	%r14, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp229:
	.size	Perl_avhv_keys, .Ltmp229-Perl_avhv_keys
	.cfi_endproc

	.globl	Perl_avhv_store_ent
	.align	16, 0x90
	.type	Perl_avhv_store_ent,@function
Perl_avhv_store_ent:                    # @Perl_avhv_store_ent
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp233:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp234:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp235:
	.cfi_def_cfa_offset 32
.Ltmp236:
	.cfi_offset %rbx, -24
.Ltmp237:
	.cfi_offset %r14, -16
	movq	%rdx, %r14
	movq	%rdi, %rbx
	xorq	%rax, %rax
                                        # kill: RDI<def> RBX<kill>
	movq	%rcx, %rdx
	callq	avhv_index
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	%rbx, %rdi
	movq	%rax, %rsi
	movq	%r14, %rdx
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	jmp	Perl_av_store  # TAILCALL
.Ltmp238:
	.size	Perl_avhv_store_ent, .Ltmp238-Perl_avhv_store_ent
	.cfi_endproc

	.globl	Perl_avhv_fetch_ent
	.align	16, 0x90
	.type	Perl_avhv_fetch_ent,@function
Perl_avhv_fetch_ent:                    # @Perl_avhv_fetch_ent
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp242:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp243:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp244:
	.cfi_def_cfa_offset 32
.Ltmp245:
	.cfi_offset %rbx, -24
.Ltmp246:
	.cfi_offset %r14, -16
	movq	%rdx, %r14
	movq	%rdi, %rbx
	xorq	%rax, %rax
                                        # kill: RDI<def> RBX<kill>
	movq	%rcx, %rdx
	callq	avhv_index
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	%rbx, %rdi
	movq	%rax, %rsi
	movq	%r14, %rdx
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	jmp	Perl_av_fetch  # TAILCALL
.Ltmp247:
	.size	Perl_avhv_fetch_ent, .Ltmp247-Perl_avhv_fetch_ent
	.cfi_endproc

	.globl	Perl_avhv_delete_ent
	.align	16, 0x90
	.type	Perl_avhv_delete_ent,@function
Perl_avhv_delete_ent:                   # @Perl_avhv_delete_ent
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp253:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp254:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp255:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp256:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp257:
	.cfi_def_cfa_offset 48
.Ltmp258:
	.cfi_offset %rbx, -40
.Ltmp259:
	.cfi_offset %r12, -32
.Ltmp260:
	.cfi_offset %r14, -24
.Ltmp261:
	.cfi_offset %r15, -16
	movq	%rcx, %r12
	movq	%rdx, %r14
	movq	%rsi, %rbx
	movq	%rdi, %r15
                                        # kill: RDI<def> R15<kill>
	callq	Perl_avhv_keys
	xorq	%rdx, %rdx
	movq	%rax, %rdi
	movq	%rbx, %rsi
	movq	%r12, %rcx
	callq	Perl_hv_fetch_ent
	testq	%rax, %rax
	je	.LBB20_2
# BB#1:                                 # %lor.lhs.false
	movq	16(%rax), %rdi
	movzbl	18(%rdi), %eax
	testq	$1807, %rax             # imm = 0x70F
	je	.LBB20_2
# BB#3:                                 # %if.end
	xorq	%rax, %rax
	callq	avhv_index_sv
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	%r15, %rdi
	movq	%rax, %rsi
	movq	%r14, %rdx
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	jmp	Perl_av_delete  # TAILCALL
.LBB20_2:                               # %return
	xorq	%rax, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.Ltmp262:
	.size	Perl_avhv_delete_ent, .Ltmp262-Perl_avhv_delete_ent
	.cfi_endproc

	.globl	Perl_avhv_exists_ent
	.align	16, 0x90
	.type	Perl_avhv_exists_ent,@function
Perl_avhv_exists_ent:                   # @Perl_avhv_exists_ent
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp268:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp269:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp270:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp271:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp272:
	.cfi_def_cfa_offset 48
.Ltmp273:
	.cfi_offset %rbx, -40
.Ltmp274:
	.cfi_offset %r12, -32
.Ltmp275:
	.cfi_offset %r14, -24
.Ltmp276:
	.cfi_offset %r15, -16
	movq	%rdx, %r12
	movq	%rsi, %rbx
	movq	%rdi, %r14
                                        # kill: RDI<def> R14<kill>
	callq	Perl_avhv_keys
	xorq	%rdx, %rdx
	xorq	%r15, %r15
	movq	%rax, %rdi
	movq	%rbx, %rsi
	movq	%r12, %rcx
	callq	Perl_hv_fetch_ent
	testq	%rax, %rax
	je	.LBB21_3
# BB#1:                                 # %lor.lhs.false
	movq	16(%rax), %rdi
	movzbl	18(%rdi), %eax
	testq	$1807, %rax             # imm = 0x70F
	je	.LBB21_3
# BB#2:                                 # %if.end
	xorq	%rax, %rax
	callq	avhv_index_sv
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	%r14, %rdi
	movq	%rax, %rsi
	callq	Perl_av_exists
	movq	%rax, %r15
.LBB21_3:                               # %return
	movq	%r15, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.Ltmp277:
	.size	Perl_avhv_exists_ent, .Ltmp277-Perl_avhv_exists_ent
	.cfi_endproc

	.globl	Perl_avhv_iternext
	.align	16, 0x90
	.type	Perl_avhv_iternext,@function
Perl_avhv_iternext:                     # @Perl_avhv_iternext
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rax
.Ltmp279:
	.cfi_def_cfa_offset 16
	callq	Perl_avhv_keys
	movq	%rax, %rdi
	popq	%rax
	jmp	Perl_hv_iternext  # TAILCALL
.Ltmp280:
	.size	Perl_avhv_iternext, .Ltmp280-Perl_avhv_iternext
	.cfi_endproc

	.globl	Perl_avhv_iterval
	.align	16, 0x90
	.type	Perl_avhv_iterval,@function
Perl_avhv_iterval:                      # @Perl_avhv_iterval
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp284:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp285:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp286:
	.cfi_def_cfa_offset 32
.Ltmp287:
	.cfi_offset %rbx, -24
.Ltmp288:
	.cfi_offset %r14, -16
	movq	%rsi, %rbx
	movq	%rdi, %r14
                                        # kill: RDI<def> R14<kill>
	callq	Perl_avhv_keys
	movq	%rax, %rdi
	movq	%rbx, %rsi
	callq	Perl_hv_iterval
	movq	%rax, %rcx
	xorq	%rax, %rax
	movq	%rcx, %rdi
	callq	avhv_index_sv
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$1, %rdx
	movq	%r14, %rdi
	movq	%rax, %rsi
	callq	Perl_av_fetch
	movq	(%rax), %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp289:
	.size	Perl_avhv_iterval, .Ltmp289-Perl_avhv_iterval
	.cfi_endproc

	.type	.L.str,@object          # @.str
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.asciz	"EXTEND"
	.size	.L.str, 7

	.type	.L.str1,@object         # @.str1
.L.str1:
	.asciz	"panic: POPSTACK\n"
	.size	.L.str1, 17

	.type	.L.str2,@object         # @.str2
.L.str2:
	.asciz	"NEGATIVE_INDICES"
	.size	.L.str2, 17

	.type	.L.str3,@object         # @.str3
.L.str3:
	.asciz	"PUSH"
	.size	.L.str3, 5

	.type	.L.str4,@object         # @.str4
.L.str4:
	.asciz	"POP"
	.size	.L.str4, 4

	.type	.L.str5,@object         # @.str5
.L.str5:
	.asciz	"UNSHIFT"
	.size	.L.str5, 8

	.type	.L.str6,@object         # @.str6
.L.str6:
	.asciz	"SHIFT"
	.size	.L.str6, 6

	.type	.L.str7,@object         # @.str7
.L.str7:
	.asciz	"panic: null array"
	.size	.L.str7, 18

	.type	.L.str8,@object         # @.str8
.L.str8:
	.asciz	"STORESIZE"
	.size	.L.str8, 10

	.type	.L.str9,@object         # @.str9
.L.str9:
	.asciz	"pseudohash"
	.size	.L.str9, 11

	.type	.L.str10,@object        # @.str10
.L.str10:
	.asciz	"Pseudo-hashes are deprecated"
	.size	.L.str10, 29

	.type	.L.str11,@object        # @.str11
.L.str11:
	.asciz	"Can't coerce array into hash"
	.size	.L.str11, 29


	.ident	"clang version 3.5 (trunk)"
	.section	".note.GNU-stack","",@progbits
