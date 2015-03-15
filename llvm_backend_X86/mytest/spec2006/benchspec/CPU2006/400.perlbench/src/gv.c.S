	.file	"gv.c.bc"
	.text
	.globl	Perl_gv_AVadd
	.align	16, 0x90
	.type	Perl_gv_AVadd,@function
Perl_gv_AVadd:                          # @Perl_gv_AVadd
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp2:
	.cfi_def_cfa_offset 16
.Ltmp3:
	.cfi_offset %rbx, -16
	movq	%rdi, %rbx
	testq	%rbx, %rbx
	je	.LBB0_2
# BB#1:                                 # %lor.lhs.false
	movzbl	16(%rbx), %eax
	cmpq	$13, %rax
	je	.LBB0_3
.LBB0_2:                                # %if.then
	movabsq	$.L.str, %rdi
	xorq	%rax, %rax
	callq	Perl_croak
.LBB0_3:                                # %if.end
	movq	(%rbx), %rax
	movq	56(%rax), %rax
	cmpq	$0, 32(%rax)
	jne	.LBB0_5
# BB#4:                                 # %if.then2
	callq	Perl_newAV
	movq	(%rbx), %rcx
	movq	56(%rcx), %rcx
	movq	%rax, 32(%rcx)
.LBB0_5:                                # %if.end6
	movq	%rbx, %rax
	popq	%rbx
	retq
.Ltmp4:
	.size	Perl_gv_AVadd, .Ltmp4-Perl_gv_AVadd
	.cfi_endproc

	.globl	Perl_gv_HVadd
	.align	16, 0x90
	.type	Perl_gv_HVadd,@function
Perl_gv_HVadd:                          # @Perl_gv_HVadd
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp7:
	.cfi_def_cfa_offset 16
.Ltmp8:
	.cfi_offset %rbx, -16
	movq	%rdi, %rbx
	testq	%rbx, %rbx
	je	.LBB1_2
# BB#1:                                 # %lor.lhs.false
	movzbl	16(%rbx), %eax
	cmpq	$13, %rax
	je	.LBB1_3
.LBB1_2:                                # %if.then
	movabsq	$.L.str1, %rdi
	xorq	%rax, %rax
	callq	Perl_croak
.LBB1_3:                                # %if.end
	movq	(%rbx), %rax
	movq	56(%rax), %rax
	cmpq	$0, 40(%rax)
	jne	.LBB1_5
# BB#4:                                 # %if.then2
	callq	Perl_newHV
	movq	(%rbx), %rcx
	movq	56(%rcx), %rcx
	movq	%rax, 40(%rcx)
.LBB1_5:                                # %if.end6
	movq	%rbx, %rax
	popq	%rbx
	retq
.Ltmp9:
	.size	Perl_gv_HVadd, .Ltmp9-Perl_gv_HVadd
	.cfi_endproc

	.globl	Perl_gv_IOadd
	.align	16, 0x90
	.type	Perl_gv_IOadd,@function
Perl_gv_IOadd:                          # @Perl_gv_IOadd
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp12:
	.cfi_def_cfa_offset 16
.Ltmp13:
	.cfi_offset %rbx, -16
	movq	%rdi, %rbx
	testq	%rbx, %rbx
	je	.LBB2_2
# BB#1:                                 # %lor.lhs.false
	movzbl	16(%rbx), %eax
	cmpq	$13, %rax
	je	.LBB2_3
.LBB2_2:                                # %if.then
	movabsq	$.L.str2, %rdi
	xorq	%rax, %rax
	callq	Perl_croak
.LBB2_3:                                # %if.end
	movq	(%rbx), %rax
	movq	56(%rax), %rax
	cmpq	$0, 16(%rax)
	jne	.LBB2_5
# BB#4:                                 # %if.then2
	callq	Perl_newIO
	movq	(%rbx), %rcx
	movq	56(%rcx), %rcx
	movq	%rax, 16(%rcx)
.LBB2_5:                                # %if.end6
	movq	%rbx, %rax
	popq	%rbx
	retq
.Ltmp14:
	.size	Perl_gv_IOadd, .Ltmp14-Perl_gv_IOadd
	.cfi_endproc

	.globl	Perl_newIO
	.align	16, 0x90
	.type	Perl_newIO,@function
Perl_newIO:                             # @Perl_newIO
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp18:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp19:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp20:
	.cfi_def_cfa_offset 32
.Ltmp21:
	.cfi_offset %rbx, -24
.Ltmp22:
	.cfi_offset %r14, -16
	xorq	%rdi, %rdi
	xorq	%r14, %r14
	callq	Perl_newSV
	movq	%rax, %rbx
	movq	$15, %rsi
	movq	%rbx, %rdi
	callq	Perl_sv_upgrade
	movq	$1, 8(%rbx)
	orq	$4096, 16(%rbx)         # imm = 0x1000
	movq	PL_stashcache(%rip), %rdi
	callq	Perl_hv_clear
	movabsq	$.L.str58, %rdi
	xorq	%rsi, %rsi
	movq	$11, %rdx
	callq	Perl_gv_fetchpv
	testq	%rax, %rax
	je	.LBB3_4
# BB#1:                                 # %land.lhs.true
	movq	(%rax), %rax
	movq	56(%rax), %rax
	movq	40(%rax), %rax
	testq	%rax, %rax
	je	.LBB3_4
# BB#2:                                 # %land.lhs.true4
	movq	(%rax), %rcx
	cmpq	$0, (%rcx)
	je	.LBB3_4
# BB#3:                                 # %if.end.thread
	movq	%rax, PL_Sv(%rip)
	jmp	.LBB3_5
.LBB3_4:                                # %if.end
	movabsq	$.L.str59, %rdi
	movq	$1, %rsi
	movq	$11, %rdx
	callq	Perl_gv_fetchpv
	movq	(%rax), %rax
	movq	56(%rax), %rax
	movq	40(%rax), %rax
	testq	%rax, %rax
	movq	%rax, PL_Sv(%rip)
	je	.LBB3_6
.LBB3_5:                                # %land.rhs
	incq	8(%rax)
	movq	%rax, %r14
.LBB3_6:                                # %land.end
	movq	(%rbx), %rax
	movq	%r14, 48(%rax)
	movq	%rbx, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp23:
	.size	Perl_newIO, .Ltmp23-Perl_newIO
	.cfi_endproc

	.globl	Perl_gv_fetchfile
	.align	16, 0x90
	.type	Perl_gv_fetchfile,@function
Perl_gv_fetchfile:                      # @Perl_gv_fetchfile
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp29:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp30:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp31:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp32:
	.cfi_def_cfa_offset 40
	subq	$264, %rsp              # imm = 0x108
.Ltmp33:
	.cfi_def_cfa_offset 304
.Ltmp34:
	.cfi_offset %rbx, -40
.Ltmp35:
	.cfi_offset %r12, -32
.Ltmp36:
	.cfi_offset %r14, -24
.Ltmp37:
	.cfi_offset %r15, -16
	xorq	%r15, %r15
	cmpq	$0, PL_defstash(%rip)
	movq	%rdi, %r12
	je	.LBB4_25
# BB#1:                                 # %if.end
	movq	%r12, %rdi
	callq	strlen
	leaq	2(%rax), %rbx
	cmpq	$256, %rbx              # imm = 0x100
	jae	.LBB4_3
# BB#2:
	leaq	(%rsp), %r14
	jmp	.LBB4_4
.LBB4_3:                                # %if.else
	addq	$3, %rax
	movq	%rax, %rdi
	callq	Perl_safesysmalloc
	movq	%rax, %r14
.LBB4_4:                                # %if.end4
	movq	$15455, %rax            # imm = 0x3C5F
	movw	%ax, (%r14)
	leaq	2(%r14), %rdi
	movq	%r12, %rsi
	callq	strcpy
	movq	PL_defstash(%rip), %rdi
	movq	$1, %rcx
	movq	%r14, %rsi
	movq	%rbx, %rdx
	callq	Perl_hv_fetch
	movq	(%rax), %r15
	movzbl	16(%r15), %eax
	cmpq	$13, %rax
	je	.LBB4_23
# BB#5:                                 # %if.then9
	movq	PL_defstash(%rip), %rsi
	xorq	%r8, %r8
	movq	%r15, %rdi
	movq	%r14, %rdx
	movq	%rbx, %rcx
	callq	Perl_gv_init
	movq	(%r15), %rax
	movq	56(%rax), %rax
	movq	(%rax), %rdi
	movq	%r12, %rsi
	callq	Perl_sv_setpv
	movzbl	PL_perldb(%rip), %eax
	testq	$2, %rax
	je	.LBB4_23
# BB#6:                                 # %if.then13
	testq	%r15, %r15
	je	.LBB4_8
# BB#7:                                 # %lor.lhs.false.i
	movzbl	16(%r15), %eax
	cmpq	$13, %rax
	je	.LBB4_9
.LBB4_8:                                # %if.then.i
	movabsq	$.L.str, %rdi
	xorq	%rax, %rax
	callq	Perl_croak
.LBB4_9:                                # %if.end.i
	movq	(%r15), %rcx
	movq	56(%rcx), %rdx
	cmpq	$0, 32(%rdx)
	jne	.LBB4_11
# BB#10:                                # %if.then2.i
	callq	Perl_newAV
	movq	(%r15), %rcx
	movq	56(%rcx), %rdx
	movq	%rax, 32(%rdx)
.LBB4_11:                               # %Perl_gv_AVadd.exit
	movq	40(%rdx), %rbx
	testq	%r15, %r15
	je	.LBB4_13
# BB#12:                                # %lor.lhs.false.i53
	movzbl	16(%r15), %eax
	cmpq	$13, %rax
	je	.LBB4_14
.LBB4_13:                               # %if.then.i54
	movabsq	$.L.str, %rdi
	xorq	%rax, %rax
	callq	Perl_croak
	movq	(%r15), %rcx
	movq	56(%rcx), %rdx
.LBB4_14:                               # %if.end.i59
	cmpq	$0, 32(%rdx)
	jne	.LBB4_16
# BB#15:                                # %if.then2.i63
	callq	Perl_newAV
	movq	(%r15), %rcx
	movq	56(%rcx), %rdx
	movq	%rax, 32(%rdx)
.LBB4_16:                               # %Perl_gv_AVadd.exit64
	testq	%rbx, %rbx
	jne	.LBB4_22
# BB#17:                                # %cond.false
	testq	%r15, %r15
	je	.LBB4_19
# BB#18:                                # %lor.lhs.false.i68
	movzbl	16(%r15), %eax
	cmpq	$13, %rax
	je	.LBB4_20
.LBB4_19:                               # %if.then.i69
	movabsq	$.L.str1, %rdi
	xorq	%rax, %rax
	callq	Perl_croak
	movq	(%r15), %rcx
.LBB4_20:                               # %if.end.i73
	movq	56(%rcx), %rax
	cmpq	$0, 40(%rax)
	jne	.LBB4_22
# BB#21:                                # %if.then2.i76
	callq	Perl_newHV
	movq	(%r15), %rcx
	movq	56(%rcx), %rcx
	movq	%rax, 40(%rcx)
.LBB4_22:                               # %cond.end
	movq	(%r15), %rax
	movq	56(%rax), %rax
	movq	40(%rax), %rdi
	xorq	%rsi, %rsi
	movq	$76, %rdx
	callq	Perl_hv_magic
.LBB4_23:                               # %if.end28
	leaq	(%rsp), %rax
	cmpq	%rax, %r14
	je	.LBB4_25
# BB#24:                                # %if.then31
	movq	%r14, %rdi
	callq	Perl_safesysfree
.LBB4_25:                               # %cleanup
	movq	%r15, %rax
	addq	$264, %rsp              # imm = 0x108
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.Ltmp38:
	.size	Perl_gv_fetchfile, .Ltmp38-Perl_gv_fetchfile
	.cfi_endproc

	.globl	Perl_gv_init
	.align	16, 0x90
	.type	Perl_gv_init,@function
Perl_gv_init:                           # @Perl_gv_init
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp45:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp46:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp47:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp48:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp49:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp50:
	.cfi_def_cfa_offset 64
.Ltmp51:
	.cfi_offset %rbx, -48
.Ltmp52:
	.cfi_offset %r12, -40
.Ltmp53:
	.cfi_offset %r13, -32
.Ltmp54:
	.cfi_offset %r14, -24
.Ltmp55:
	.cfi_offset %r15, -16
	movq	%rdi, %rbx
	movq	16(%rbx), %rax
	movq	%r8, 8(%rsp)            # 8-byte Spill
	movq	%rcx, %r12
	movq	%rdx, (%rsp)            # 8-byte Spill
	movq	%rsi, %r15
	movq	%rax, %r13
	andq	$255, %r13
	jne	.LBB5_1
# BB#2:                                 # %entry
	movabsq	$1, %rcx
	jmp	.LBB5_3
.LBB5_1:
	movabsq	$0, %rcx
.LBB5_3:                                # %entry
	xorq	%r14, %r14
	testq	$262144, %rax           # imm = 0x40000
	je	.LBB5_6
# BB#4:                                 # %entry
	testq	%rcx, %rcx
	jne	.LBB5_6
# BB#5:                                 # %cond.true
	movq	(%rbx), %rax
	movq	(%rax), %r14
.LBB5_6:                                # %cond.end
	movq	$13, %rsi
	movq	%rbx, %rdi
	callq	Perl_sv_upgrade
	movq	(%rbx), %rax
	cmpq	$0, 16(%rax)
	je	.LBB5_10
# BB#7:                                 # %if.then
	testq	%r14, %r14
	je	.LBB5_9
# BB#8:                                 # %if.then9
	movq	$0, (%rax)
	movq	$0, 16(%rax)
	andq	$-67371009, 16(%rbx)    # imm = 0xFFFFFFFFFBFBFFFF
	jmp	.LBB5_10
.LBB5_9:                                # %if.else
	movq	(%rax), %rdi
	callq	Perl_safesysfree
.LBB5_10:                               # %if.end18
	movq	$96, %rdi
	callq	Perl_safesysmalloc
	xorq	%rcx, %rcx
	testq	%rax, %rax
	movq	$0, 88(%rax)
	movq	$0, 80(%rax)
	movq	$0, 72(%rax)
	movq	$0, 64(%rax)
	movq	$0, 56(%rax)
	movq	$0, 48(%rax)
	movq	$0, 40(%rax)
	movq	$0, 32(%rax)
	movq	$0, 24(%rax)
	movq	$0, 16(%rax)
	movq	$0, 8(%rax)
	movq	$0, (%rax)
	je	.LBB5_12
# BB#11:                                # %if.end.i
	movq	$1, 8(%rax)
	movq	%rax, %rcx
.LBB5_12:                               # %Perl_gp_ref.exit
	movq	(%rbx), %rax
	movq	%rcx, 56(%rax)
	xorq	%rdi, %rdi
	callq	Perl_newSV
	movq	(%rbx), %rcx
	movq	56(%rcx), %rcx
	movq	%rax, (%rcx)
	movq	PL_curcop(%rip), %rax
	movq	(%rbx), %rcx
	movq	80(%rax), %rax
	movq	56(%rcx), %rcx
	movq	%rax, 80(%rcx)
	movq	PL_curcop(%rip), %rax
	cmpq	$0, 56(%rax)
	movabsq	$.L.str3, %rax
	je	.LBB5_18
# BB#13:                                # %cond.true28
	movq	PL_curcop(%rip), %rcx
	movq	56(%rcx), %rcx
	movq	(%rcx), %rcx
	movq	56(%rcx), %rcx
	cmpq	$0, (%rcx)
	je	.LBB5_18
# BB#14:                                # %cond.true35
	movq	PL_curcop(%rip), %rcx
	movq	PL_curcop(%rip), %rcx
	movq	56(%rcx), %rcx
	movq	(%rcx), %rcx
	movq	56(%rcx), %rcx
	movq	(%rcx), %rcx
	movq	(%rcx), %rcx
	cmpq	$0, (%rcx)
	je	.LBB5_18
# BB#15:                                # %cond.true50
	movq	PL_curcop(%rip), %rcx
	xorq	%rax, %rax
	cmpq	$0, 56(%rcx)
	je	.LBB5_18
# BB#16:                                # %cond.true53
	movq	PL_curcop(%rip), %rcx
	movq	56(%rcx), %rcx
	movq	(%rcx), %rcx
	movq	56(%rcx), %rcx
	cmpq	$0, (%rcx)
	je	.LBB5_18
# BB#17:                                # %cond.true60
	movq	PL_curcop(%rip), %rax
	movq	PL_curcop(%rip), %rax
	movq	56(%rax), %rax
	movq	(%rax), %rax
	movq	56(%rax), %rax
	movq	(%rax), %rax
	movq	(%rax), %rax
	movq	(%rax), %rax
.LBB5_18:                               # %cond.end77
	movq	(%rbx), %rcx
	movq	56(%rcx), %rcx
	movq	%rax, 88(%rcx)
	movq	$0, 64(%rcx)
	movq	%rbx, 48(%rcx)
	movq	$42, %rdx
	xorq	%rcx, %rcx
	xorq	%r8, %r8
	movq	%rbx, %rdi
	movq	%rbx, %rsi
	callq	Perl_sv_magic
	testq	%r15, %r15
	movq	%r15, PL_Sv(%rip)
	je	.LBB5_20
# BB#19:                                # %land.rhs
	incq	8(%r15)
.LBB5_20:                               # %land.end
	movq	(%rbx), %rax
	movq	%r15, 80(%rax)
	movq	(%rsp), %rdi            # 8-byte Reload
	movq	%r12, %rsi
	callq	Perl_savepvn
	movq	(%rbx), %rcx
	movq	%rax, 64(%rcx)
	movq	%r12, 72(%rcx)
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	movq	8(%rsp), %rdx           # 8-byte Reload
	testq	%rax, %rdx
	jne	.LBB5_22
# BB#21:                                # %land.end
	testq	%r13, %r13
	je	.LBB5_23
.LBB5_22:                               # %if.then94
	movzbl	88(%rcx), %eax
	orq	$2, %rax
	movb	%al, 88(%rcx)
.LBB5_23:                               # %if.end98
	testq	%r13, %r13
	je	.LBB5_28
# BB#24:                                # %if.then100
	andq	$2130640895, 16(%rbx)   # imm = 0x7EFEFFFF
	callq	Perl_push_scope
	xorq	%rdi, %rdi
	xorq	%rsi, %rsi
	xorq	%r15, %r15
	callq	Perl_start_subparse
	movq	(%rbx), %rax
	movq	PL_compcv(%rip), %rcx
	movq	56(%rax), %rax
	movq	%rcx, 56(%rax)
	callq	Perl_pop_scope
	incq	PL_sub_generation(%rip)
	movq	(%rbx), %rax
	movq	56(%rax), %rax
	movq	56(%rax), %rax
	movq	(%rax), %rax
	movq	%rbx, 96(%rax)
	movq	PL_curcop(%rip), %rax
	cmpq	$0, 56(%rax)
	je	.LBB5_27
# BB#25:                                # %cond.true113
	movq	PL_curcop(%rip), %rax
	movq	56(%rax), %rax
	movq	(%rax), %rax
	movq	56(%rax), %rax
	cmpq	$0, (%rax)
	je	.LBB5_27
# BB#26:                                # %cond.true120
	movq	PL_curcop(%rip), %rax
	movq	PL_curcop(%rip), %rax
	movq	56(%rax), %rax
	movq	(%rax), %rax
	movq	56(%rax), %rax
	movq	(%rax), %rax
	movq	(%rax), %rax
	movq	(%rax), %r15
.LBB5_27:                               # %cond.end134
	movq	(%rbx), %rax
	movq	56(%rax), %rax
	movq	56(%rax), %rdi
	movq	(%rdi), %rax
	movq	%r15, 104(%rax)
	movq	PL_curstash(%rip), %rcx
	movq	%rcx, 56(%rax)
	testq	%r14, %r14
	je	.LBB5_28
# BB#29:                                # %if.then145
	movq	%r14, %rsi
	callq	Perl_sv_setpv
	movq	%r14, %rdi
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	Perl_safesysfree  # TAILCALL
.LBB5_28:                               # %if.end150
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp56:
	.size	Perl_gv_init, .Ltmp56-Perl_gv_init
	.cfi_endproc

	.globl	Perl_gp_ref
	.align	16, 0x90
	.type	Perl_gp_ref,@function
Perl_gp_ref:                            # @Perl_gp_ref
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp60:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp61:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp62:
	.cfi_def_cfa_offset 32
.Ltmp63:
	.cfi_offset %rbx, -24
.Ltmp64:
	.cfi_offset %r14, -16
	movq	%rdi, %rbx
	xorq	%rax, %rax
	testq	%rbx, %rbx
	je	.LBB6_6
# BB#1:                                 # %if.end
	incq	8(%rbx)
	movq	56(%rbx), %rdi
	testq	%rdi, %rdi
	je	.LBB6_5
# BB#2:                                 # %if.then2
	cmpq	$0, 64(%rbx)
	je	.LBB6_4
# BB#3:                                 # %if.then4
	leaq	56(%rbx), %r14
	callq	Perl_sv_free
	movq	$0, 8(%r14)
	movq	$0, (%r14)
	jmp	.LBB6_5
.LBB6_4:                                # %if.else
	incq	PL_sub_generation(%rip)
.LBB6_5:                                # %return
	movq	%rbx, %rax
.LBB6_6:                                # %return
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp65:
	.size	Perl_gp_ref, .Ltmp65-Perl_gp_ref
	.cfi_endproc

	.globl	Perl_gv_fetchmeth
	.align	16, 0x90
	.type	Perl_gv_fetchmeth,@function
Perl_gv_fetchmeth:                      # @Perl_gv_fetchmeth
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp72:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp73:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp74:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp75:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp76:
	.cfi_def_cfa_offset 48
	subq	$304, %rsp              # imm = 0x130
.Ltmp77:
	.cfi_def_cfa_offset 352
.Ltmp78:
	.cfi_offset %rbx, -48
.Ltmp79:
	.cfi_offset %r12, -40
.Ltmp80:
	.cfi_offset %r13, -32
.Ltmp81:
	.cfi_offset %r14, -24
.Ltmp82:
	.cfi_offset %r15, -16
	movq	%rcx, %r14
	movq	%rdx, 16(%rsp)          # 8-byte Spill
	movq	%rsi, 24(%rsp)          # 8-byte Spill
	movq	%rdi, %r13
	testq	%r13, %r13
	jne	.LBB7_7
# BB#1:                                 # %if.end10.i
	movq	$76, %rax
	movb	%al, 40(%rsp)
	movabsq	$4707196493286166101, %rax # imm = 0x4153524556494E55
	movq	%rax, 32(%rsp)
	movq	$58, %rax
	movb	%al, 41(%rsp)
	movb	%al, 42(%rsp)
	xorq	%rbx, %rbx
	movb	%bl, 43(%rsp)
	leaq	32(%rsp), %rdi
	xorq	%rsi, %rsi
	movq	$11, %rdx
	callq	Perl_gv_fetchpv
	movq	%rax, %r14
	testq	%r14, %r14
	je	.LBB7_75
# BB#2:                                 # %if.end12.i
	movq	(%r14), %rax
	movq	56(%rax), %rax
	movq	40(%rax), %r13
	testq	%r13, %r13
	jne	.LBB7_4
# BB#3:                                 # %if.then14.i
	callq	Perl_newHV
	movq	(%r14), %rcx
	movq	56(%rcx), %rcx
	movq	%rax, %r13
	movq	%r13, 40(%rcx)
.LBB7_4:                                # %if.end19.i
	movq	(%r13), %rax
	cmpq	$0, 80(%rax)
	je	.LBB7_5
# BB#6:                                 # %Perl_gv_stashpvn.exit
	movq	$-1, %r14
	testq	%r13, %r13
	jne	.LBB7_7
	jmp	.LBB7_75
.LBB7_5:                                # %Perl_gv_stashpvn.exit.thread342
	movabsq	$.L.str4, %rdi
	callq	Perl_savepv
	movq	(%r13), %rcx
	movq	%rax, 80(%rcx)
	movq	$-1, %r14
.LBB7_7:                                # %if.end3
	movq	(%r13), %rax
	cmpq	$0, 80(%rax)
	jne	.LBB7_9
# BB#8:                                 # %if.then5
	movabsq	$.L.str5, %rdi
	xorq	%rax, %rax
	callq	Perl_croak
.LBB7_9:                                # %if.end6
	leaq	100(%r14), %rax
	cmpq	$201, %rax
	jb	.LBB7_11
# BB#10:                                # %if.then8
	movq	(%r13), %rax
	movq	80(%rax), %rdx
	movabsq	$.L.str6, %rdi
	xorq	%rax, %rax
	movq	24(%rsp), %rsi          # 8-byte Reload
	callq	Perl_croak
.LBB7_11:                               # %if.end11
	movq	$63, %rcx
	movq	%r14, %rax
	shrq	%cl, %rax
	xorq	$1, %rax
	movq	%r13, %rdi
	movq	24(%rsp), %rsi          # 8-byte Reload
	movq	16(%rsp), %rdx          # 8-byte Reload
	movq	%rax, %rcx
	callq	Perl_hv_fetch
	xorq	%r15, %r15
	testq	%rax, %rax
	movabsq	$0, %rcx
	movq	%rcx, 8(%rsp)           # 8-byte Spill
	je	.LBB7_19
# BB#12:                                # %if.else
	movq	(%rax), %rbx
	movzbl	16(%rbx), %eax
	cmpq	$13, %rax
	je	.LBB7_14
# BB#13:                                # %if.then19
	movq	$1, %r8
	movq	%rbx, %rdi
	movq	%r13, %rsi
	movq	24(%rsp), %rdx          # 8-byte Reload
	movq	16(%rsp), %rcx          # 8-byte Reload
	callq	Perl_gv_init
.LBB7_14:                               # %if.end20
	movq	(%rbx), %rax
	movq	56(%rax), %rax
	movq	56(%rax), %rdi
	movq	64(%rax), %rax
	testq	%rdi, %rdi
	je	.LBB7_18
# BB#15:                                # %if.then23
	testq	%rax, %rax
	je	.LBB7_75
# BB#16:                                # %if.then23
	movq	PL_sub_generation(%rip), %rcx
	cmpq	%rcx, %rax
	je	.LBB7_75
# BB#17:                                # %if.end34
	callq	Perl_sv_free
	movq	(%rbx), %rax
	movq	56(%rax), %rax
	movq	$0, 64(%rax)
	movq	$0, 56(%rax)
	movq	%rbx, 8(%rsp)           # 8-byte Spill
	jmp	.LBB7_19
.LBB7_18:                               # %if.else41
	movq	PL_sub_generation(%rip), %rcx
	cmpq	%rcx, %rax
	movq	%rbx, 8(%rsp)           # 8-byte Spill
	movabsq	$0, %rbx
	je	.LBB7_75
.LBB7_19:                               # %if.end50
	movabsq	$.L.str7, %rsi
	movq	$3, %rdx
	xorq	%rcx, %rcx
	movq	%r13, %rdi
	callq	Perl_hv_fetch
	testq	%rax, %rax
	je	.LBB7_20
# BB#21:                                # %land.lhs.true
	movq	(%rax), %rax
	testq	%rax, %rax
	je	.LBB7_22
# BB#23:                                # %land.lhs.true
	movq	%r14, (%rsp)            # 8-byte Spill
	movabsq	$PL_sv_undef, %rcx
	cmpq	%rcx, %rax
	je	.LBB7_26
# BB#24:                                # %cond.end
	movq	(%rax), %rax
	movq	56(%rax), %rax
	movq	32(%rax), %rcx
	testq	%rcx, %rcx
	je	.LBB7_26
# BB#25:                                # %lor.lhs.false60
	movq	(%rcx), %rax
	cmpq	$0, 40(%rax)
	movq	%rcx, %r15
	jne	.LBB7_39
	jmp	.LBB7_26
.LBB7_20:
	movq	%r14, (%rsp)            # 8-byte Spill
	jmp	.LBB7_26
.LBB7_22:
	movq	%r14, (%rsp)            # 8-byte Spill
.LBB7_26:                               # %if.then63
	movq	(%r13), %rax
	movq	80(%rax), %r14
	movq	%r14, %rdi
	callq	strlen
	movq	%rax, %rbx
	cmpq	$7, %rbx
	jb	.LBB7_37
# BB#27:                                # %land.lhs.true69
	leaq	-7(%r14,%rbx), %rdi
	movabsq	$.L.str8, %rsi
	callq	strcmp
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB7_37
# BB#28:                                # %if.then73
	addq	$-7, %rbx
	movq	$1, %rdx
	movq	%r14, %rdi
	movq	%rbx, %rsi
	callq	Perl_gv_stashpvn
	movabsq	$.L.str7, %rsi
	movq	$3, %rdx
	xorq	%rcx, %rcx
	movq	%rax, %rdi
	callq	Perl_hv_fetch
	testq	%rax, %rax
	je	.LBB7_37
# BB#29:                                # %land.lhs.true77
	movq	(%rax), %rbx
	movabsq	$PL_sv_undef, %rax
	cmpq	%rax, %rbx
	je	.LBB7_37
# BB#30:                                # %land.lhs.true80
	movq	(%rbx), %rax
	movq	56(%rax), %rax
	movq	32(%rax), %r15
	testq	%r15, %r15
	je	.LBB7_53
# BB#31:                                # %if.then85
	movabsq	$.L.str7, %rsi
	movq	$3, %rdx
	movq	$1, %rcx
	movq	%r13, %rdi
	callq	Perl_hv_fetch
	testq	%rax, %rax
	je	.LBB7_33
# BB#32:                                # %lor.lhs.false88
	movq	(%rax), %rbx
	testq	%rbx, %rbx
	jne	.LBB7_34
.LBB7_33:                               # %if.then90
	movq	(%r13), %rax
	movq	80(%rax), %rsi
	movabsq	$.L.str9, %rdi
	xorq	%rax, %rax
	callq	Perl_croak
.LBB7_34:                               # %if.end93
	movzbl	16(%rbx), %eax
	cmpq	$13, %rax
	je	.LBB7_36
# BB#35:                                # %if.then98
	movabsq	$.L.str7, %rdx
	movq	$3, %rcx
	movq	$1, %r8
	movq	%rbx, %rdi
	movq	%r13, %rsi
	callq	Perl_gv_init
.LBB7_36:                               # %land.end
	movq	(%rbx), %rax
	movq	56(%rax), %rax
	movq	32(%rax), %rdi
	callq	Perl_sv_free
	movq	%r15, PL_Sv(%rip)
	incq	8(%r15)
	movq	(%rbx), %rax
	movq	56(%rax), %rax
	movq	%r15, 32(%rax)
.LBB7_37:                               # %if.end110
	testq	%r15, %r15
	je	.LBB7_53
# BB#38:                                # %if.end110.if.then112_crit_edge
	movq	(%r15), %rax
.LBB7_39:                               # %if.then112
	movq	8(%rax), %r12
	cmpq	$-1, %r12
	je	.LBB7_53
# BB#40:                                # %while.body.lr.ph
	movq	(%rax), %r14
	movq	$63, %rcx
	movq	(%rsp), %rax            # 8-byte Reload
	movq	%rax, %r15
	sarq	%cl, %r15
	orq	$1, %r15
	addq	%rax, %r15
	notq	%r12
	.align	16, 0x90
.LBB7_41:                               # %while.body
                                        # =>This Inner Loop Header: Depth=1
	movq	(%r14), %rbx
	movzbl	18(%rbx), %eax
	addq	$8, %r14
	testq	$4, %rax
	jne	.LBB7_42
# BB#43:                                # %cond.false.i
                                        #   in Loop: Header=BB7_41 Depth=1
	movq	$2, %rdx
	movq	%rbx, %rdi
	leaq	296(%rsp), %rsi
	callq	Perl_sv_2pv_flags
	movq	296(%rsp), %rsi
	movq	%rax, %rdi
	jmp	.LBB7_44
	.align	16, 0x90
.LBB7_42:                               # %cond.true.i
                                        #   in Loop: Header=BB7_41 Depth=1
	movq	(%rbx), %rax
	movq	8(%rax), %rsi
	movq	%rsi, 296(%rsp)
	movq	(%rax), %rdi
.LBB7_44:                               # %Perl_gv_stashsv.exit
                                        #   in Loop: Header=BB7_41 Depth=1
	xorq	%rdx, %rdx
	callq	Perl_gv_stashpvn
	testq	%rax, %rax
	jne	.LBB7_76
# BB#45:                                # %if.then119
                                        #   in Loop: Header=BB7_41 Depth=1
	movq	PL_curcop(%rip), %rax
	cmpq	$0, 88(%rax)
	je	.LBB7_49
# BB#46:                                # %land.lhs.true122
                                        #   in Loop: Header=BB7_41 Depth=1
	movq	PL_curcop(%rip), %rax
	movq	88(%rax), %rax
	cmpq	$48, %rax
	je	.LBB7_49
# BB#47:                                # %land.lhs.true126
                                        #   in Loop: Header=BB7_41 Depth=1
	movq	PL_curcop(%rip), %rax
	movq	88(%rax), %rax
	cmpq	$24, %rax
	je	.LBB7_51
# BB#48:                                # %lor.lhs.false130
                                        #   in Loop: Header=BB7_41 Depth=1
	movq	PL_curcop(%rip), %rax
	movq	88(%rax), %rax
	movq	(%rax), %rax
	movq	(%rax), %rax
	movzbl	3(%rax), %eax
	testq	$1, %rax
	jne	.LBB7_51
	.align	16, 0x90
.LBB7_49:                               # %lor.lhs.false136
                                        #   in Loop: Header=BB7_41 Depth=1
	movq	PL_curcop(%rip), %rax
	cmpq	$0, 88(%rax)
	jne	.LBB7_52
# BB#50:                                # %land.lhs.true140
                                        #   in Loop: Header=BB7_41 Depth=1
	movzbl	PL_dowarn(%rip), %eax
	testq	$1, %rax
	je	.LBB7_52
.LBB7_51:                               # %if.then144
                                        #   in Loop: Header=BB7_41 Depth=1
	movq	(%r13), %rax
	movq	80(%rax), %rcx
	movq	$12, %rdi
	xorq	%rax, %rax
	movabsq	$.L.str10, %rsi
	movq	%rbx, %rdx
	callq	Perl_warner
	jmp	.LBB7_52
	.align	16, 0x90
.LBB7_76:                               # %if.end148
                                        #   in Loop: Header=BB7_41 Depth=1
	movq	%rax, %rdi
	movq	24(%rsp), %rsi          # 8-byte Reload
	movq	16(%rsp), %rdx          # 8-byte Reload
	movq	%r15, %rcx
	callq	Perl_gv_fetchmeth
	movq	%rax, %rbx
	testq	%rbx, %rbx
	jne	.LBB7_61
.LBB7_52:                               # %while.cond.backedge
                                        #   in Loop: Header=BB7_41 Depth=1
	incq	%r12
	jne	.LBB7_41
.LBB7_53:                               # %if.end161
	xorq	%rbx, %rbx
	movq	(%rsp), %r15            # 8-byte Reload
	leaq	1(%r15), %rax
	cmpq	$1, %rax
	ja	.LBB7_75
# BB#54:                                # %if.end10.i322
	movq	$76, %rax
	movb	%al, 40(%rsp)
	movabsq	$4707196493286166101, %rax # imm = 0x4153524556494E55
	movq	%rax, 32(%rsp)
	movq	$58, %rax
	movb	%al, 41(%rsp)
	movb	%al, 42(%rsp)
	xorq	%rax, %rax
	movb	%al, 43(%rsp)
	leaq	32(%rsp), %rdi
	xorq	%rsi, %rsi
	movq	$11, %rdx
	callq	Perl_gv_fetchpv
	movq	%rax, %rbx
	testq	%rbx, %rbx
	je	.LBB7_74
# BB#55:                                # %if.end12.i327
	movq	(%rbx), %rax
	movq	56(%rax), %rax
	movq	40(%rax), %r14
	testq	%r14, %r14
	jne	.LBB7_57
# BB#56:                                # %if.then14.i331
	callq	Perl_newHV
	movq	(%rbx), %rcx
	movq	56(%rcx), %rcx
	movq	%rax, %r14
	movq	%r14, 40(%rcx)
.LBB7_57:                               # %if.end19.i335
	movq	(%r14), %rax
	cmpq	$0, 80(%rax)
	je	.LBB7_58
# BB#59:                                # %Perl_gv_stashpvn.exit340
	testq	%r14, %r14
	jne	.LBB7_60
	jmp	.LBB7_74
.LBB7_58:                               # %Perl_gv_stashpvn.exit340.thread351
	movabsq	$.L.str4, %rdi
	callq	Perl_savepv
	movq	(%r14), %rcx
	movq	%rax, 80(%rcx)
.LBB7_60:                               # %if.then170
	movq	$63, %rcx
	movq	%r15, %rax
	sarq	%cl, %rax
	orq	$1, %rax
	addq	%r15, %rax
	movq	%r14, %rdi
	movq	24(%rsp), %rsi          # 8-byte Reload
	movq	16(%rsp), %rdx          # 8-byte Reload
	movq	%rax, %rcx
	callq	Perl_gv_fetchmeth
	movq	%rax, %rbx
	testq	%rbx, %rbx
	je	.LBB7_71
.LBB7_61:                               # %gotcha
	movq	8(%rsp), %rsi           # 8-byte Reload
	testq	%rsi, %rsi
	je	.LBB7_75
# BB#62:                                # %land.lhs.true183
	movq	(%rsi), %rax
	movq	56(%rax), %rcx
	movq	8(%rcx), %rax
	cmpq	$1, %rax
	jne	.LBB7_75
# BB#63:                                # %land.lhs.true188
	movq	(%rbx), %rax
	movq	56(%rax), %rax
	movq	56(%rax), %rax
	testq	%rax, %rax
	je	.LBB7_75
# BB#64:                                # %land.lhs.true193
	movq	(%rax), %rdx
	cmpq	$0, 72(%rdx)
	jne	.LBB7_66
# BB#65:                                # %lor.lhs.false196
	cmpq	$0, 80(%rdx)
	je	.LBB7_75
.LBB7_66:                               # %if.then199
	movq	%rsi, %r14
	movq	56(%rcx), %rdi
	testq	%rdi, %rdi
	je	.LBB7_68
# BB#67:                                # %if.then204
	callq	Perl_sv_free
	movq	(%rbx), %rax
	movq	56(%rax), %rax
	movq	56(%rax), %rax
.LBB7_68:                               # %if.end205
	testq	%rax, %rax
	movq	%rax, PL_Sv(%rip)
	je	.LBB7_70
# BB#69:                                # %land.rhs210
	incq	8(%rax)
.LBB7_70:                               # %land.end214
	movq	(%r14), %rcx
	movq	56(%rcx), %rcx
	movq	%rax, 56(%rcx)
	movq	PL_sub_generation(%rip), %rax
	movq	%rax, 64(%rcx)
	jmp	.LBB7_75
.LBB7_71:                               # %if.else223
	movq	8(%rsp), %rax           # 8-byte Reload
	testq	%rax, %rax
	je	.LBB7_74
# BB#72:                                # %land.lhs.true225
	movq	(%rax), %rax
	movq	56(%rax), %rax
	movq	8(%rax), %rcx
	cmpq	$1, %rcx
	jne	.LBB7_74
# BB#73:                                # %if.then231
	movq	PL_sub_generation(%rip), %rcx
	movq	%rcx, 64(%rax)
.LBB7_74:                               # %return
	xorq	%rbx, %rbx
.LBB7_75:                               # %return
	movq	%rbx, %rax
	addq	$304, %rsp              # imm = 0x130
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp83:
	.size	Perl_gv_fetchmeth, .Ltmp83-Perl_gv_fetchmeth
	.cfi_endproc

	.globl	Perl_gv_stashpvn
	.align	16, 0x90
	.type	Perl_gv_stashpvn,@function
Perl_gv_stashpvn:                       # @Perl_gv_stashpvn
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp90:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp91:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp92:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp93:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp94:
	.cfi_def_cfa_offset 48
	subq	$272, %rsp              # imm = 0x110
.Ltmp95:
	.cfi_def_cfa_offset 320
.Ltmp96:
	.cfi_offset %rbx, -48
.Ltmp97:
	.cfi_offset %r12, -40
.Ltmp98:
	.cfi_offset %r13, -32
.Ltmp99:
	.cfi_offset %r14, -24
.Ltmp100:
	.cfi_offset %r15, -16
	movq	%rdx, %r15
	movq	%rsi, %r13
	movq	%rdi, %r12
	leaq	16(%rsp), %r14
	leaq	3(%r13), %rdi
	cmpq	$256, %rdi              # imm = 0x100
	movq	%r14, %rbx
	jb	.LBB8_2
# BB#1:                                 # %if.else
	callq	Perl_safesysmalloc
	movq	%rax, %rbx
.LBB8_2:                                # %if.end
	movq	%rbx, %rdi
	movq	%r12, %rsi
	movq	%r12, 8(%rsp)           # 8-byte Spill
	movq	%r13, %rdx
	callq	memcpy
	movq	$14906, %rax            # imm = 0x3A3A
	movw	%ax, (%rbx,%r13)
	xorq	%r12, %r12
	movb	%r12b, 2(%rbx,%r13)
	movq	$11, %rdx
	movq	%rbx, %rdi
	movq	%r15, %rsi
	callq	Perl_gv_fetchpv
	movq	%rax, %r15
	cmpq	%r14, %rbx
	je	.LBB8_4
# BB#3:                                 # %if.then9
	movq	%rbx, %rdi
	callq	Perl_safesysfree
.LBB8_4:                                # %if.end10
	testq	%r15, %r15
	je	.LBB8_9
# BB#5:                                 # %if.end12
	movq	(%r15), %rax
	movq	56(%rax), %rax
	movq	40(%rax), %r12
	testq	%r12, %r12
	jne	.LBB8_7
# BB#6:                                 # %if.then14
	callq	Perl_newHV
	movq	(%r15), %rcx
	movq	56(%rcx), %rcx
	movq	%rax, %r12
	movq	%r12, 40(%rcx)
.LBB8_7:                                # %if.end19
	movq	(%r12), %rax
	cmpq	$0, 80(%rax)
	jne	.LBB8_9
# BB#8:                                 # %if.then25
	movq	8(%rsp), %rdi           # 8-byte Reload
	callq	Perl_savepv
	movq	(%r12), %rcx
	movq	%rax, 80(%rcx)
.LBB8_9:                                # %cleanup
	movq	%r12, %rax
	addq	$272, %rsp              # imm = 0x110
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp101:
	.size	Perl_gv_stashpvn, .Ltmp101-Perl_gv_stashpvn
	.cfi_endproc

	.globl	Perl_gv_stashsv
	.align	16, 0x90
	.type	Perl_gv_stashsv,@function
Perl_gv_stashsv:                        # @Perl_gv_stashsv
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp104:
	.cfi_def_cfa_offset 16
	subq	$16, %rsp
.Ltmp105:
	.cfi_def_cfa_offset 32
.Ltmp106:
	.cfi_offset %rbx, -16
	movzbl	18(%rdi), %eax
	movq	%rsi, %rbx
	testq	$4, %rax
	je	.LBB9_2
# BB#1:                                 # %cond.true
	movq	(%rdi), %rax
	movq	8(%rax), %rsi
	movq	%rsi, 8(%rsp)
	movq	(%rax), %rdi
	jmp	.LBB9_3
.LBB9_2:                                # %cond.false
	leaq	8(%rsp), %rsi
	movq	$2, %rdx
	callq	Perl_sv_2pv_flags
	movq	8(%rsp), %rsi
	movq	%rax, %rdi
.LBB9_3:                                # %cond.end
	movq	%rbx, %rdx
	callq	Perl_gv_stashpvn
	addq	$16, %rsp
	popq	%rbx
	retq
.Ltmp107:
	.size	Perl_gv_stashsv, .Ltmp107-Perl_gv_stashsv
	.cfi_endproc

	.globl	Perl_gv_fetchmeth_autoload
	.align	16, 0x90
	.type	Perl_gv_fetchmeth_autoload,@function
Perl_gv_fetchmeth_autoload:             # @Perl_gv_fetchmeth_autoload
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp114:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp115:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp116:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp117:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp118:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp119:
	.cfi_def_cfa_offset 64
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
	movq	%rcx, %r14
	movq	%rdx, %r15
	movq	%rsi, %r12
	movq	%rdi, %r13
                                        # kill: RDI<def> R13<kill>
                                        # kill: RSI<def> R12<kill>
                                        # kill: RDX<def> R15<kill>
                                        # kill: RCX<def> R14<kill>
	callq	Perl_gv_fetchmeth
	movq	%rax, %rbx
	testq	%rbx, %rbx
	jne	.LBB10_11
# BB#1:                                 # %if.then
	xorq	%rbx, %rbx
	movb	%bl, 8(%rsp)
	movabsq	$4918299457232328001, %rax # imm = 0x44414F4C4F545541
	movq	%rax, (%rsp)
	testq	%r13, %r13
	je	.LBB10_11
# BB#2:                                 # %if.end
	cmpq	$8, %r15
	jne	.LBB10_4
# BB#3:                                 # %land.lhs.true
	leaq	(%rsp), %rsi
	movq	$8, %rdx
	movq	%r12, %rdi
	callq	strncmp
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB10_11
.LBB10_4:                               # %if.end6
	leaq	(%rsp), %rsi
	movq	$8, %rdx
	xorq	%rcx, %rcx
	xorq	%rbx, %rbx
	movq	%r13, %rdi
	callq	Perl_gv_fetchmeth
	testq	%rax, %rax
	je	.LBB10_11
# BB#5:                                 # %if.end11
	movq	(%rax), %rax
	movq	56(%rax), %rax
	movq	56(%rax), %rax
	movq	(%rax), %rax
	cmpq	$0, 72(%rax)
	jne	.LBB10_7
# BB#6:                                 # %lor.lhs.false
	cmpq	$0, 80(%rax)
	je	.LBB10_11
.LBB10_7:                               # %if.end17
	testq	%r14, %r14
	jns	.LBB10_9
# BB#8:                                 # %if.then19
	xorq	%rcx, %rcx
	movq	%r13, %rdi
	movq	%r12, %rsi
	movq	%r15, %rdx
	callq	Perl_gv_fetchmeth
.LBB10_9:                               # %if.end21
	movq	$63, %rcx
	shrq	%cl, %r14
	xorq	$1, %r14
	movq	%r13, %rdi
	movq	%r12, %rsi
	movq	%r15, %rdx
	movq	%r14, %rcx
	callq	Perl_hv_fetch
	testq	%rax, %rax
	je	.LBB10_11
# BB#10:                                # %if.end27
	movq	(%rax), %rbx
.LBB10_11:                              # %return
	movq	%rbx, %rax
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp125:
	.size	Perl_gv_fetchmeth_autoload, .Ltmp125-Perl_gv_fetchmeth_autoload
	.cfi_endproc

	.globl	Perl_gv_fetchmethod
	.align	16, 0x90
	.type	Perl_gv_fetchmethod,@function
Perl_gv_fetchmethod:                    # @Perl_gv_fetchmethod
	.cfi_startproc
# BB#0:                                 # %entry
	movq	$1, %rdx
	jmp	Perl_gv_fetchmethod_autoload  # TAILCALL
.Ltmp126:
	.size	Perl_gv_fetchmethod, .Ltmp126-Perl_gv_fetchmethod
	.cfi_endproc

	.globl	Perl_gv_fetchmethod_autoload
	.align	16, 0x90
	.type	Perl_gv_fetchmethod_autoload,@function
Perl_gv_fetchmethod_autoload:           # @Perl_gv_fetchmethod_autoload
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp133:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp134:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp135:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp136:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp137:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp138:
	.cfi_def_cfa_offset 64
.Ltmp139:
	.cfi_offset %rbx, -48
.Ltmp140:
	.cfi_offset %r12, -40
.Ltmp141:
	.cfi_offset %r13, -32
.Ltmp142:
	.cfi_offset %r14, -24
.Ltmp143:
	.cfi_offset %r15, -16
	movq	%rdx, %r15
	movq	%rsi, %r13
	movq	%rdi, %r12
	xorq	%r14, %r14
	testq	%r12, %r12
	movabsq	$0, %rdi
	je	.LBB12_3
# BB#1:                                 # %land.lhs.true
	movzbl	16(%r12), %eax
	xorq	%rdi, %rdi
	cmpq	$11, %rax
	jb	.LBB12_3
# BB#2:                                 # %land.lhs.true
	movq	%r12, %rdi
.LBB12_3:                               # %if.end
	movq	%r13, %rbx
	jmp	.LBB12_4
	.align	16, 0x90
.LBB12_14:                              # %for.inc
                                        #   in Loop: Header=BB12_4 Depth=1
	incq	%rbx
	movq	%rax, %r14
.LBB12_4:                               # %for.cond
                                        # =>This Inner Loop Header: Depth=1
	movzbl	(%rbx), %eax
	cmpq	$39, %rax
	jne	.LBB12_6
# BB#5:                                 #   in Loop: Header=BB12_4 Depth=1
	movq	%rbx, %rax
	jmp	.LBB12_14
.LBB12_6:                               # %for.cond
                                        #   in Loop: Header=BB12_4 Depth=1
	testq	%rax, %rax
	je	.LBB12_15
# BB#7:                                 # %if.else
                                        #   in Loop: Header=BB12_4 Depth=1
	cmpq	$58, %rax
	jne	.LBB12_8
# BB#9:                                 # %land.lhs.true8
                                        #   in Loop: Header=BB12_4 Depth=1
	movzbl	1(%rbx), %eax
	leaq	1(%rbx), %rcx
	cmpq	$58, %rax
	movq	%rcx, %rax
	je	.LBB12_11
# BB#10:                                # %land.lhs.true8
                                        #   in Loop: Header=BB12_4 Depth=1
	movq	%r14, %rax
.LBB12_11:                              # %land.lhs.true8
                                        #   in Loop: Header=BB12_4 Depth=1
	je	.LBB12_13
# BB#12:                                # %land.lhs.true8
                                        #   in Loop: Header=BB12_4 Depth=1
	movq	%rbx, %rcx
.LBB12_13:                              # %land.lhs.true8
                                        #   in Loop: Header=BB12_4 Depth=1
	movq	%rcx, %rbx
	jmp	.LBB12_14
.LBB12_8:                               #   in Loop: Header=BB12_4 Depth=1
	movq	%r14, %rax
	jmp	.LBB12_14
.LBB12_15:                              # %for.end
	testq	%r14, %r14
	je	.LBB12_32
# BB#16:                                # %if.then17
	movzbl	(%r14), %eax
	leaq	1(%r14), %rcx
	movq	%rcx, (%rsp)            # 8-byte Spill
	cmpq	$58, %rax
	jne	.LBB12_18
# BB#17:
	movq	%r15, 8(%rsp)           # 8-byte Spill
	decq	%r14
	jmp	.LBB12_19
.LBB12_18:                              # %select.mid
	movq	%r15, 8(%rsp)           # 8-byte Spill
.LBB12_19:                              # %select.end
	movq	%r14, %r15
	subq	%r13, %r15
	cmpq	$5, %r15
	jne	.LBB12_24
# BB#20:                                # %land.lhs.true27
	movabsq	$.L.str11, %rsi
	movq	$5, %rdx
	movq	%r13, %rdi
	callq	strncmp
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB12_21
.LBB12_24:                              # %if.else37
	xorq	%rdx, %rdx
	movq	%r13, %rdi
	movq	%r15, %rsi
	callq	Perl_gv_stashpvn
	movq	%rax, %r12
	testq	%r12, %r12
	jne	.LBB12_30
# BB#25:                                # %if.else37
	cmpq	$7, %r15
	jl	.LBB12_30
# BB#26:                                # %land.lhs.true49
	addq	$-7, %r14
	movabsq	$.L.str8, %rsi
	movq	$7, %rdx
	movq	%r14, %rdi
	callq	strncmp
	xorq	%r12, %r12
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB12_27
# BB#28:                                # %land.lhs.true53
	leaq	-7(%r15), %rsi
	xorq	%rdx, %rdx
	xorq	%r12, %r12
	movq	%r13, %rdi
	callq	Perl_gv_stashpvn
	testq	%rax, %rax
	je	.LBB12_27
# BB#29:                                # %if.then59
	movq	$1, %rdx
	movq	%r13, %rdi
	movq	%r15, %rsi
	callq	Perl_gv_stashpvn
	movq	%rax, %r12
.LBB12_30:                              # %if.end66
	movq	%r12, %rdi
	jmp	.LBB12_31
.LBB12_27:
	xorq	%rdi, %rdi
.LBB12_31:                              # %if.end66
	movq	8(%rsp), %r15           # 8-byte Reload
	movq	(%rsp), %r13            # 8-byte Reload
	jmp	.LBB12_32
.LBB12_21:                              # %if.then29
	movq	PL_curcop(%rip), %rax
	xorq	%rsi, %rsi
	cmpq	$0, 48(%rax)
	movq	8(%rsp), %r15           # 8-byte Reload
	movq	(%rsp), %r13            # 8-byte Reload
	je	.LBB12_23
# BB#22:                                # %cond.true
	movq	PL_curcop(%rip), %rax
	movq	48(%rax), %rax
	movq	(%rax), %rax
	movq	80(%rax), %rsi
.LBB12_23:                              # %cond.end
	movabsq	$.L.str12, %rdi
	xorq	%rax, %rax
	callq	Perl_newSVpvf
	movq	%rax, %rdi
	callq	Perl_sv_2mortal
	movq	(%rax), %rax
	movq	(%rax), %rdi
	movq	8(%rax), %rsi
	movq	$1, %rdx
	callq	Perl_gv_stashpvn
	movq	%rax, %r12
	movq	%r12, %rdi
.LBB12_32:                              # %if.end66
	subq	%r13, %rbx
	xorq	%rcx, %rcx
	movq	%r13, %rsi
	movq	%rbx, %rdx
	callq	Perl_gv_fetchmeth
	movq	%rax, %r14
	testq	%r14, %r14
	je	.LBB12_33
# BB#37:                                # %if.else87
	testq	%r15, %r15
	je	.LBB12_46
# BB#38:                                # %if.then89
	movq	(%r14), %rax
	movq	56(%rax), %rcx
	movq	56(%rcx), %rcx
	movq	(%rcx), %rdx
	cmpq	$0, 72(%rdx)
	jne	.LBB12_46
# BB#39:                                # %land.lhs.true93
	cmpq	$0, 80(%rdx)
	jne	.LBB12_46
# BB#40:                                # %if.then96
	movzbl	136(%rdx), %esi
	testq	$4, %rsi
	jne	.LBB12_44
# BB#41:                                # %if.else102
	movq	96(%rdx), %rax
	movq	(%rax), %rdx
	movq	56(%rdx), %rdx
	movq	56(%rdx), %rdx
	cmpq	%rcx, %rdx
	je	.LBB12_43
# BB#42:                                # %select.mid7
	movq	%r14, %rax
.LBB12_43:                              # %select.end6
	movq	(%rax), %rax
.LBB12_44:                              # %if.end111
	movq	80(%rax), %rdi
	movq	64(%rax), %rsi
	movq	72(%rax), %rdx
	movq	$1, %rcx
	callq	Perl_gv_autoload4
	testq	%rax, %rax
	je	.LBB12_46
# BB#45:                                # %if.end111
	movq	%rax, %r14
	jmp	.LBB12_46
.LBB12_33:                              # %if.then72
	movq	%r15, 8(%rsp)           # 8-byte Spill
	movabsq	$.L.str13, %rsi
	movq	%r13, %rdi
	callq	strcmp
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	testq	%r15, %rax
	movabsq	$PL_sv_yes, %r14
	je	.LBB12_46
# BB#34:                                # %lor.lhs.false
	movabsq	$.L.str14, %rsi
	movq	%r13, %rdi
	movq	%r13, (%rsp)            # 8-byte Spill
	callq	strcmp
	testq	%r15, %rax
	je	.LBB12_46
# BB#35:                                # %if.else78
	xorq	%r14, %r14
	cmpq	$0, 8(%rsp)             # 8-byte Folded Reload
	je	.LBB12_46
# BB#36:                                # %if.then80
	movq	$1, %rcx
	movq	%r12, %rdi
	movq	(%rsp), %rsi            # 8-byte Reload
	movq	%rbx, %rdx
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	Perl_gv_autoload4  # TAILCALL
.LBB12_46:                              # %if.end121
	movq	%r14, %rax
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp144:
	.size	Perl_gv_fetchmethod_autoload, .Ltmp144-Perl_gv_fetchmethod_autoload
	.cfi_endproc

	.globl	Perl_gv_autoload4
	.align	16, 0x90
	.type	Perl_gv_autoload4,@function
Perl_gv_autoload4:                      # @Perl_gv_autoload4
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp151:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp152:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp153:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp154:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp155:
	.cfi_def_cfa_offset 48
	subq	$48, %rsp
.Ltmp156:
	.cfi_def_cfa_offset 96
.Ltmp157:
	.cfi_offset %rbx, -48
.Ltmp158:
	.cfi_offset %r12, -40
.Ltmp159:
	.cfi_offset %r13, -32
.Ltmp160:
	.cfi_offset %r14, -24
.Ltmp161:
	.cfi_offset %r15, -16
	movq	%rcx, 16(%rsp)          # 8-byte Spill
	movq	%rdx, %r15
	movq	%rsi, 24(%rsp)          # 8-byte Spill
	movq	%rdi, %rbx
	xorq	%r13, %r13
	cmpq	$8, %r15
	movb	%r13b, 40(%rsp)
	movabsq	$4918299457232328001, %rax # imm = 0x44414F4C4F545541
	movq	%rax, 32(%rsp)
	jne	.LBB13_2
# BB#1:                                 # %land.lhs.true
	leaq	32(%rsp), %rsi
	movq	$8, %rdx
	movq	24(%rsp), %rdi          # 8-byte Reload
	callq	strncmp
	xorq	%r14, %r14
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB13_30
.LBB13_2:                               # %if.end
	testq	%rbx, %rbx
	je	.LBB13_3
# BB#4:                                 # %if.then2
	movq	16(%rbx), %rax
	movq	%rax, %rcx
	andq	$255, %rcx
	cmpq	$10, %rcx
	ja	.LBB13_8
# BB#5:                                 # %if.then4
	testq	$262144, %rax           # imm = 0x40000
	je	.LBB13_7
# BB#6:                                 # %cond.true
	movq	(%rbx), %rax
	movq	(%rax), %rax
	movq	%rax, 8(%rsp)           # 8-byte Spill
	jmp	.LBB13_9
.LBB13_3:
	movabsq	$.L.str3, %rax
	movq	%rax, 8(%rsp)           # 8-byte Spill
	jmp	.LBB13_9
.LBB13_8:                               # %if.else
	movq	(%rbx), %rax
	movq	80(%rax), %rax
	movq	%rax, 8(%rsp)           # 8-byte Spill
	movq	%rbx, %r13
	jmp	.LBB13_9
.LBB13_7:                               # %cond.false
	movq	%rbx, %rdi
	callq	Perl_sv_2pv_nolen
	movq	%rax, 8(%rsp)           # 8-byte Spill
.LBB13_9:                               # %if.end11
	leaq	32(%rsp), %rsi
	movq	$8, %rdx
	xorq	%rcx, %rcx
	xorq	%r14, %r14
	movq	%r13, %rdi
	callq	Perl_gv_fetchmeth
	movq	%rax, %r12
	testq	%r12, %r12
	je	.LBB13_30
# BB#10:                                # %if.end16
	movq	(%r12), %rax
	movq	56(%rax), %rax
	movq	56(%rax), %rbx
	movq	(%rbx), %rax
	cmpq	$0, 72(%rax)
	jne	.LBB13_12
# BB#11:                                # %lor.lhs.false
	cmpq	$0, 80(%rax)
	je	.LBB13_30
.LBB13_12:                              # %if.end23
	movq	PL_curcop(%rip), %rax
	cmpq	$0, 88(%rax)
	je	.LBB13_17
# BB#13:                                # %land.lhs.true25
	movq	PL_curcop(%rip), %rax
	movq	88(%rax), %rax
	cmpq	$48, %rax
	je	.LBB13_17
# BB#14:                                # %land.lhs.true28
	movq	PL_curcop(%rip), %rax
	movq	88(%rax), %rax
	cmpq	$24, %rax
	je	.LBB13_19
# BB#15:                                # %lor.lhs.false31
	movq	PL_curcop(%rip), %rax
	movq	88(%rax), %rax
	movq	(%rax), %rax
	movq	(%rax), %rax
	movzbl	(%rax), %eax
	testq	$16, %rax
	jne	.LBB13_19
# BB#16:                                # %lor.lhs.false37
	movq	PL_curcop(%rip), %rax
	movq	88(%rax), %rax
	movq	(%rax), %rax
	movq	(%rax), %rax
	movzbl	7(%rax), %eax
	testq	$1, %rax
	jne	.LBB13_19
.LBB13_17:                              # %lor.lhs.false45
	movq	PL_curcop(%rip), %rax
	cmpq	$0, 88(%rax)
	jne	.LBB13_23
# BB#18:                                # %land.lhs.true49
	movzbl	PL_dowarn(%rip), %eax
	testq	$1, %rax
	je	.LBB13_23
.LBB13_19:                              # %land.lhs.true53
	cmpq	$0, 16(%rsp)            # 8-byte Folded Reload
	jne	.LBB13_23
# BB#20:                                # %land.lhs.true55
	movq	(%r12), %rax
	movq	56(%rax), %rcx
	cmpq	$0, 64(%rcx)
	jne	.LBB13_22
# BB#21:                                # %lor.lhs.false59
	movq	80(%rax), %rax
	cmpq	%r13, %rax
	je	.LBB13_23
.LBB13_22:                              # %if.then63
	movabsq	$.L.str15, %rsi
	movq	$7170, %rdi             # imm = 0x1C02
	xorq	%rax, %rax
	movq	8(%rsp), %rdx           # 8-byte Reload
	movq	%r15, %rcx
	movq	24(%rsp), %r8           # 8-byte Reload
	callq	Perl_warner
.LBB13_23:                              # %if.end65
	movq	(%rbx), %rax
	cmpq	$0, 80(%rax)
	je	.LBB13_25
# BB#24:                                # %if.then69
	movq	%r13, 56(%rax)
	movq	24(%rsp), %rcx          # 8-byte Reload
	movq	%rcx, (%rax)
	movq	%r15, 8(%rax)
	jmp	.LBB13_29
.LBB13_25:                              # %if.end74
	movq	96(%rax), %rax
	movq	(%rax), %rax
	movq	80(%rax), %r14
	leaq	32(%rsp), %rsi
	movq	$8, %rdx
	movq	$1, %rcx
	movq	%r14, %rdi
	callq	Perl_hv_fetch
	movq	(%rax), %rbx
	callq	Perl_push_scope
	movzbl	16(%rbx), %eax
	cmpq	$13, %rax
	je	.LBB13_27
# BB#26:                                # %if.then84
	leaq	32(%rsp), %rdx
	movq	$8, %rcx
	xorq	%r8, %r8
	movq	%rbx, %rdi
	movq	%r14, %rsi
	callq	Perl_gv_init
.LBB13_27:                              # %if.end86
	callq	Perl_pop_scope
	movq	(%rbx), %rax
	movq	56(%rax), %rax
	movq	(%rax), %rbx
	movq	%rbx, %rdi
	movq	8(%rsp), %rsi           # 8-byte Reload
	callq	Perl_sv_setpv
	movabsq	$.L.str16, %rsi
	movq	$2, %rdx
	movq	$2, %rcx
	movq	%rbx, %rdi
	callq	Perl_sv_catpvn_flags
	movq	$2, %rcx
	movq	%rbx, %rdi
	movq	24(%rsp), %rsi          # 8-byte Reload
	movq	%r15, %rdx
	callq	Perl_sv_catpvn_flags
	movzbl	PL_tainting(%rip), %eax
	testq	%rax, %rax
	je	.LBB13_29
# BB#28:                                # %if.then90
	movq	%rbx, %rdi
	callq	Perl_sv_untaint
.LBB13_29:                              # %return
	movq	%r12, %r14
.LBB13_30:                              # %return
	movq	%r14, %rax
	addq	$48, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp162:
	.size	Perl_gv_autoload4, .Ltmp162-Perl_gv_autoload4
	.cfi_endproc

	.globl	Perl_gv_stashpv
	.align	16, 0x90
	.type	Perl_gv_stashpv,@function
Perl_gv_stashpv:                        # @Perl_gv_stashpv
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp166:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp167:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp168:
	.cfi_def_cfa_offset 32
.Ltmp169:
	.cfi_offset %rbx, -24
.Ltmp170:
	.cfi_offset %r14, -16
	movq	%rsi, %r14
	movq	%rdi, %rbx
                                        # kill: RDI<def> RBX<kill>
	callq	strlen
	movq	%rbx, %rdi
	movq	%rax, %rsi
	movq	%r14, %rdx
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	jmp	Perl_gv_stashpvn  # TAILCALL
.Ltmp171:
	.size	Perl_gv_stashpv, .Ltmp171-Perl_gv_stashpv
	.cfi_endproc

	.globl	Perl_gv_fetchpv
	.align	16, 0x90
	.type	Perl_gv_fetchpv,@function
Perl_gv_fetchpv:                        # @Perl_gv_fetchpv
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp178:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp179:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp180:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp181:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp182:
	.cfi_def_cfa_offset 48
	subq	$560, %rsp              # imm = 0x230
.Ltmp183:
	.cfi_def_cfa_offset 608
.Ltmp184:
	.cfi_offset %rbx, -48
.Ltmp185:
	.cfi_offset %r12, -40
.Ltmp186:
	.cfi_offset %r13, -32
.Ltmp187:
	.cfi_offset %r14, -24
.Ltmp188:
	.cfi_offset %r15, -16
	movzbl	(%rdi), %eax
	movq	%rdx, 16(%rsp)          # 8-byte Spill
	movq	%rsi, 40(%rsp)          # 8-byte Spill
	cmpq	$42, %rax
	movq	%rdi, %r14
	jne	.LBB15_4
# BB#1:                                 # %land.lhs.true
	movzbl	1(%rdi), %eax
	leaq	-65(%rax), %rcx
	andq	$255, %rcx
	cmpq	$26, %rcx
	sbbq	%rcx, %rcx
	andq	$1, %rcx
	addq	$-97, %rax
	andq	$255, %rax
	cmpq	$26, %rax
	sbbq	%rax, %rax
	andq	$1, %rax
	orq	%rcx, %rax
	jne	.LBB15_2
# BB#3:                                 # %land.lhs.true
	movq	%rdi, %r14
	jmp	.LBB15_4
.LBB15_2:
	leaq	1(%rdi), %r14
.LBB15_4:                               # %for.cond.preheader
	movq	%rdi, 32(%rsp)          # 8-byte Spill
	movq	40(%rsp), %rax          # 8-byte Reload
	andq	$2, %rax
	movq	%rax, 24(%rsp)          # 8-byte Spill
	xorq	%r15, %r15
	xorq	%r12, %r12
	movq	%r14, %rsi
	jmp	.LBB15_5
	.align	16, 0x90
.LBB15_35:                              # %for.inc
                                        #   in Loop: Header=BB15_5 Depth=1
	incq	%r14
.LBB15_5:                               # %for.cond
                                        # =>This Inner Loop Header: Depth=1
	movzbl	(%r14), %eax
	cmpq	$39, %rax
	je	.LBB15_9
# BB#6:                                 # %for.cond
                                        #   in Loop: Header=BB15_5 Depth=1
	testq	%rax, %rax
	je	.LBB15_38
# BB#7:                                 # %for.cond
                                        #   in Loop: Header=BB15_5 Depth=1
	cmpq	$58, %rax
	jne	.LBB15_35
# BB#8:                                 # %land.lhs.true22
                                        #   in Loop: Header=BB15_5 Depth=1
	movzbl	1(%r14), %eax
	cmpq	$58, %rax
	je	.LBB15_10
	jmp	.LBB15_35
	.align	16, 0x90
.LBB15_9:                               # %land.lhs.true31
                                        #   in Loop: Header=BB15_5 Depth=1
	movzbl	1(%r14), %eax
	testq	%rax, %rax
	je	.LBB15_35
.LBB15_10:                              # %if.then35
                                        #   in Loop: Header=BB15_5 Depth=1
	testq	%r15, %r15
	jne	.LBB15_12
# BB#11:                                #   in Loop: Header=BB15_5 Depth=1
	movq	PL_defstash(%rip), %r15
.LBB15_12:                              # %if.then35
                                        #   in Loop: Header=BB15_5 Depth=1
	xorq	%r13, %r13
	testq	%r15, %r15
	je	.LBB15_271
# BB#13:                                # %lor.lhs.false40
                                        #   in Loop: Header=BB15_5 Depth=1
	cmpq	$0, 8(%r15)
	je	.LBB15_271
# BB#14:                                # %if.end43
                                        #   in Loop: Header=BB15_5 Depth=1
	movq	%r14, %rbx
	subq	%rsi, %rbx
	testq	%rbx, %rbx
	jle	.LBB15_31
# BB#15:                                # %if.then46
                                        #   in Loop: Header=BB15_5 Depth=1
	leaq	3(%rbx), %rdi
	cmpq	$256, %rdi              # imm = 0x100
	leaq	48(%rsp), %r13
	jb	.LBB15_17
# BB#16:                                # %if.else
                                        #   in Loop: Header=BB15_5 Depth=1
	movq	%rsi, %r12
	callq	Perl_safesysmalloc
	movq	%r12, %rsi
	movq	%rax, %r13
.LBB15_17:                              # %if.end52
                                        #   in Loop: Header=BB15_5 Depth=1
	movq	%r13, %rdi
	movq	%rbx, %rdx
	callq	memcpy
	movq	$14906, %rax            # imm = 0x3A3A
	movw	%ax, (%r13,%rbx)
	xorq	%r12, %r12
	movb	%r12b, 2(%r13,%rbx)
	addq	$2, %rbx
	movq	%r15, %rdi
	movq	%r13, %rsi
	movq	%rbx, %rdx
	movq	40(%rsp), %rcx          # 8-byte Reload
	callq	Perl_hv_fetch
	testq	%rax, %rax
	je	.LBB15_23
# BB#18:                                # %cond.end
                                        #   in Loop: Header=BB15_5 Depth=1
	movq	(%rax), %r12
	testq	%r12, %r12
	je	.LBB15_23
# BB#19:                                # %cond.end
                                        #   in Loop: Header=BB15_5 Depth=1
	movabsq	$PL_sv_undef, %rax
	cmpq	%rax, %r12
	je	.LBB15_23
# BB#20:                                # %if.then64
                                        #   in Loop: Header=BB15_5 Depth=1
	movzbl	16(%r12), %eax
	cmpq	$13, %rax
	jne	.LBB15_21
# BB#22:                                # %if.else70
                                        #   in Loop: Header=BB15_5 Depth=1
	movq	(%r12), %rax
	movzbl	88(%rax), %ecx
	orq	$2, %rcx
	movb	%cl, 88(%rax)
	jmp	.LBB15_23
.LBB15_21:                              # %if.then67
                                        #   in Loop: Header=BB15_5 Depth=1
	movq	%r12, %rdi
	movq	%r15, %rsi
	movq	%r13, %rdx
	movq	%rbx, %rcx
	movq	24(%rsp), %r8           # 8-byte Reload
	callq	Perl_gv_init
.LBB15_23:                              # %if.end74
                                        #   in Loop: Header=BB15_5 Depth=1
	leaq	48(%rsp), %rax
	cmpq	%rax, %r13
	je	.LBB15_25
# BB#24:                                # %if.then78
                                        #   in Loop: Header=BB15_5 Depth=1
	movq	%r13, %rdi
	callq	Perl_safesysfree
.LBB15_25:                              # %if.end79
                                        #   in Loop: Header=BB15_5 Depth=1
	xorq	%r13, %r13
	testq	%r12, %r12
	je	.LBB15_271
# BB#26:                                # %if.end79
                                        #   in Loop: Header=BB15_5 Depth=1
	movabsq	$PL_sv_undef, %rax
	cmpq	%rax, %r12
	je	.LBB15_271
# BB#27:                                # %if.end85
                                        #   in Loop: Header=BB15_5 Depth=1
	movq	(%r12), %rax
	movq	56(%rax), %rax
	movq	40(%rax), %r15
	testq	%r15, %r15
	jne	.LBB15_29
# BB#28:                                # %if.then88
                                        #   in Loop: Header=BB15_5 Depth=1
	callq	Perl_newHV
	movq	(%r12), %rcx
	movq	56(%rcx), %rcx
	movq	%rax, %r15
	movq	%r15, 40(%rcx)
.LBB15_29:                              # %if.end93
                                        #   in Loop: Header=BB15_5 Depth=1
	movq	(%r15), %rax
	cmpq	$0, 80(%rax)
	jne	.LBB15_31
# BB#30:                                # %if.then96
                                        #   in Loop: Header=BB15_5 Depth=1
	movq	%r14, %rsi
	movq	32(%rsp), %rdi          # 8-byte Reload
	subq	%rdi, %rsi
	callq	Perl_savepvn
	movq	(%r15), %rcx
	movq	%rax, 80(%rcx)
.LBB15_31:                              # %if.end104
                                        #   in Loop: Header=BB15_5 Depth=1
	movzbl	(%r14), %eax
	cmpq	$58, %rax
	jne	.LBB15_33
# BB#32:                                #   in Loop: Header=BB15_5 Depth=1
	incq	%r14
.LBB15_33:                              # %select.end
                                        #   in Loop: Header=BB15_5 Depth=1
	movzbl	1(%r14), %eax
	testq	%rax, %rax
	je	.LBB15_36
# BB#34:                                #   in Loop: Header=BB15_5 Depth=1
	incq	%r14
	movq	%r14, %rsi
	jmp	.LBB15_35
.LBB15_38:                              # %for.end
	movq	%r14, %rax
	subq	%rsi, %rax
	movq	%rax, %r12
	testq	%r15, %r15
	jne	.LBB15_103
# BB#39:                                # %if.then127
	movq	PL_curcop(%rip), %rax
	movzbl	37(%rax), %ecx
	movzbl	(%rsi), %eax
	testq	$8, %rcx
	jne	.LBB15_41
# BB#40:                                # %if.then127
	movq	%rax, %rcx
	andq	$255, %rcx
	cmpq	$191, %rcx
	jbe	.LBB15_41
# BB#45:                                # %cond.false155
	movq	%rsi, %rdi
	movq	%rsi, %rbx
	callq	Perl_is_utf8_idcont
	testq	$255, %rax
	je	.LBB15_44
# BB#46:                                # %land.lhs.true159
	movq	%rbx, %rdi
	callq	Perl_is_utf8_digit
	testq	$255, %rax
	jne	.LBB15_44
	jmp	.LBB15_47
.LBB15_36:                              # %if.then113
	testq	%r12, %r12
	movq	%r12, %r13
	jne	.LBB15_271
# BB#37:                                # %cond.false116
	movq	PL_defstash(%rip), %rdi
	movabsq	$.L.str17, %rsi
	movq	$6, %rdx
	movq	$1, %rcx
	callq	Perl_hv_fetch
	movq	(%rax), %r13
	jmp	.LBB15_271
.LBB15_41:                              # %cond.true135
	movq	%rsi, %rbx
	leaq	-65(%rax), %rcx
	andq	$255, %rcx
	cmpq	$26, %rcx
	jb	.LBB15_47
# BB#42:                                # %lor.lhs.false143
	leaq	-97(%rax), %rcx
	andq	$255, %rcx
	cmpq	$26, %rcx
	jb	.LBB15_47
# BB#43:                                # %lor.lhs.false143
	andq	$255, %rax
	cmpq	$95, %rax
	jne	.LBB15_44
.LBB15_47:                              # %if.then162
	movsbq	1(%rbx), %rax
	cmpq	$72, %rax
	jg	.LBB15_65
# BB#48:                                # %if.then162
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB15_50
# BB#49:                                # %sw.epilog
	movzbl	(%rbx), %eax
	cmpq	$95, %rax
	je	.LBB15_44
	jmp	.LBB15_50
.LBB15_65:                              # %if.then162
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	cmpq	$77, %rax
	jg	.LBB15_68
# BB#66:                                # %if.then162
	andq	%r15, %rax
	cmpq	$73, %rax
	jne	.LBB15_50
# BB#67:                                # %sw.bb178
	movabsq	$.L.str20, %rsi
	jmp	.LBB15_73
.LBB15_68:                              # %if.then162
	andq	%r15, %rax
	cmpq	$84, %rax
	je	.LBB15_74
# BB#69:                                # %if.then162
	cmpq	$82, %rax
	jne	.LBB15_70
# BB#77:                                # %sw.bb194
	movabsq	$.L.str24, %rsi
	movq	%rbx, %rdi
	callq	strcmp
	testq	%r15, %rax
	je	.LBB15_44
# BB#78:                                # %lor.lhs.false197
	movabsq	$.L.str25, %rsi
	jmp	.LBB15_73
.LBB15_74:                              # %sw.bb183
	movabsq	$.L.str21, %rsi
	movq	%rbx, %rdi
	callq	strcmp
	testq	%r15, %rax
	je	.LBB15_44
# BB#75:                                # %lor.lhs.false186
	movabsq	$.L.str22, %rsi
	movq	%rbx, %rdi
	callq	strcmp
	testq	%r15, %rax
	je	.LBB15_44
# BB#76:                                # %lor.lhs.false189
	movabsq	$.L.str23, %rsi
	jmp	.LBB15_73
.LBB15_70:                              # %if.then162
	cmpq	$78, %rax
	jne	.LBB15_50
# BB#71:                                # %sw.bb170
	movabsq	$.L.str18, %rsi
	movq	%rbx, %rdi
	callq	strcmp
	testq	%r15, %rax
	je	.LBB15_44
# BB#72:                                # %lor.lhs.false173
	movabsq	$.L.str19, %rsi
.LBB15_73:                              # %lor.lhs.false173
	movq	%rbx, %rdi
	callq	strcmp
	testq	%r15, %rax
	je	.LBB15_44
.LBB15_50:                              # %if.else204
	movq	PL_curcop(%rip), %rax
	movabsq	$PL_compiling, %rcx
	cmpq	%rcx, %rax
	je	.LBB15_51
# BB#90:                                # %if.else300
	movq	PL_curcop(%rip), %rax
	movq	48(%rax), %r15
	jmp	.LBB15_91
.LBB15_44:                              # %if.else303
	movq	PL_defstash(%rip), %r15
.LBB15_91:                              # %if.end305
	testq	%r15, %r15
	movq	%rbx, %rsi
	jne	.LBB15_103
	jmp	.LBB15_92
.LBB15_51:                              # %if.then207
	movq	PL_curstash(%rip), %r15
	cmpq	$0, 40(%rsp)            # 8-byte Folded Reload
	je	.LBB15_91
# BB#52:                                # %land.lhs.true209
	testq	$1024, PL_hints(%rip)   # imm = 0x400
	je	.LBB15_91
# BB#53:                                # %land.lhs.true209
	movq	16(%rsp), %rax          # 8-byte Reload
	cmpq	$12, %rax
	je	.LBB15_91
# BB#54:                                # %land.lhs.true209
	cmpq	$13, %rax
	je	.LBB15_91
# BB#55:                                # %land.lhs.true209
	cmpq	$14, %rax
	je	.LBB15_91
# BB#56:                                # %land.lhs.true209
	cmpq	$15, %rax
	je	.LBB15_91
# BB#57:                                # %land.lhs.true224
	cmpq	$1, %r12
	jne	.LBB15_60
# BB#58:                                # %land.lhs.true224
	cmpq	$4, 16(%rsp)            # 8-byte Folded Reload
	jne	.LBB15_60
# BB#59:                                # %land.lhs.true230
	movzbl	(%rbx), %eax
	addq	$-97, %rax
	andq	$255, %rax
	cmpq	$2, %rax
	jb	.LBB15_91
.LBB15_60:                              # %if.then238
	xorq	%rcx, %rcx
	movq	%r15, %rdi
	movq	%rbx, %rsi
	movq	%r12, %rdx
	callq	Perl_hv_fetch
	movq	%rbx, %rsi
	movq	%rax, %r13
	testq	%r13, %r13
	je	.LBB15_92
# BB#61:                                # %lor.lhs.false241
	movq	(%r13), %rax
	movabsq	$PL_sv_undef, %rcx
	cmpq	%rcx, %rax
	je	.LBB15_92
# BB#62:                                # %lor.lhs.false244
	movzbl	16(%rax), %ecx
	cmpq	$13, %rcx
	jne	.LBB15_92
# BB#63:                                # %if.else250
	movq	16(%rsp), %rcx          # 8-byte Reload
	movq	%rsi, %rbx
	cmpq	$4, %rcx
	jne	.LBB15_79
# BB#64:                                # %land.lhs.true253
	movq	(%rax), %rax
	movzbl	88(%rax), %eax
	testq	$16, %rax
	jne	.LBB15_91
	jmp	.LBB15_83
.LBB15_79:                              # %lor.lhs.false259
	cmpq	$11, %rcx
	jne	.LBB15_80
# BB#82:                                # %land.lhs.true271
	movq	(%rax), %rax
	movzbl	88(%rax), %eax
	testq	$64, %rax
	jne	.LBB15_91
.LBB15_83:                              # %cond.false281
	movq	16(%rsp), %rax          # 8-byte Reload
	cmpq	$11, %rax
	jne	.LBB15_84
# BB#85:                                # %cond.false281
	movabsq	$1, %rsi
	jmp	.LBB15_86
.LBB15_80:                              # %lor.lhs.false259
	cmpq	$10, %rcx
	jne	.LBB15_91
# BB#81:                                # %land.lhs.true262
	movq	(%rax), %rax
	movzbl	88(%rax), %eax
	testq	$32, %rax
	movq	$64, %rsi
	jne	.LBB15_91
	jmp	.LBB15_87
.LBB15_84:
	movabsq	$0, %rsi
.LBB15_86:                              # %cond.false281
	orq	$36, %rsi
.LBB15_87:                              # %cond.end285
	movabsq	$.L.str26, %rdi
	xorq	%rax, %rax
	movq	%rbx, %rdx
	callq	Perl_warn
	movq	%rbx, %rsi
	movq	(%r13), %rax
	movq	(%rax), %rax
	movq	56(%rax), %rax
	cmpq	$0, 64(%rax)
	jne	.LBB15_92
# BB#88:                                # %cond.false291
	cmpq	$0, 56(%rax)
	je	.LBB15_92
# BB#89:                                # %if.then295
	movabsq	$.L.str27, %rcx
	xorq	%rax, %rax
	movq	%rcx, %rdi
	movq	%rsi, %rbx
                                        # kill: RSI<def> RBX<kill>
	callq	Perl_warn
	movq	%rbx, %rsi
.LBB15_92:                              # %if.then307
	xorq	%r13, %r13
	cmpq	$0, 40(%rsp)            # 8-byte Folded Reload
	je	.LBB15_271
# BB#93:                                # %if.then309
	movq	16(%rsp), %rax          # 8-byte Reload
	movq	%rsi, %rbx
	cmpq	$4, %rax
	jne	.LBB15_95
# BB#94:
	movabsq	$.L.str29, %rsi
	jmp	.LBB15_100
.LBB15_95:                              # %cond.false313
	cmpq	$10, %rax
	jne	.LBB15_97
# BB#96:
	movabsq	$.L.str30, %rsi
	jmp	.LBB15_100
.LBB15_97:                              # %cond.false317
	cmpq	$11, %rax
	je	.LBB15_98
# BB#99:                                # %cond.false317
	movabsq	$.L.str3, %rsi
	jmp	.LBB15_100
.LBB15_98:
	movabsq	$.L.str31, %rsi
.LBB15_100:                             # %cond.end323
	movabsq	$.L.str28, %rdi
	xorq	%rax, %rax
	movq	%rbx, %rdx
	callq	Perl_mess
	movzbl	PL_hints+2(%rip), %edx
	movq	%rax, %rcx
	testq	$128, %rdx
	je	.LBB15_102
# BB#101:                               # %if.then328
	orq	$536870912, 16(%rcx)    # imm = 0x20000000
.LBB15_102:                             # %if.end331
	xorq	%rax, %rax
	movq	%rcx, %rdi
	callq	qerror
	movq	PL_nullstash(%rip), %r15
	movq	%rbx, %rsi
.LBB15_103:                             # %if.end335
	xorq	%r13, %r13
	cmpq	$0, 8(%r15)
	je	.LBB15_271
# BB#104:                               # %if.end339
	movq	%r15, %rdi
	movq	%rsi, %rbx
	movq	%r12, %rdx
	movq	40(%rsp), %rcx          # 8-byte Reload
	callq	Perl_hv_fetch
	testq	%rax, %rax
	je	.LBB15_271
# BB#105:                               # %lor.lhs.false342
	movq	(%rax), %rdx
	movabsq	$PL_sv_undef, %rax
	cmpq	%rax, %rdx
	movq	%rbx, %rsi
	je	.LBB15_271
# BB#106:                               # %if.end346
	movzbl	16(%rdx), %eax
	cmpq	$13, %rax
	jne	.LBB15_115
# BB#107:                               # %if.then351
	cmpq	$0, 40(%rsp)            # 8-byte Folded Reload
	movq	%rdx, %r13
	je	.LBB15_271
# BB#108:                               # %if.then353
	movq	(%rdx), %rax
	movzbl	88(%rax), %ecx
	orq	$2, %rcx
	movb	%cl, 88(%rax)
	xorq	%rax, %rax
	movq	%rdx, %rdi
	movq	%rdi, %r14
	movq	%rsi, %r15
	movq	16(%rsp), %rbx          # 8-byte Reload
	movq	%rbx, %rsi
	callq	gv_init_sv
	movzbl	(%r15), %eax
	cmpq	$33, %rax
	jne	.LBB15_109
# BB#110:                               # %if.then353
	cmpq	$11, %rbx
	jne	.LBB15_111
# BB#112:                               # %if.then353
	cmpq	$1, %r12
	movq	%r14, %rbx
	movq	%rbx, %r13
	jne	.LBB15_271
# BB#113:                               # %if.then369
	xorq	%rax, %rax
.LBB15_114:                             # %return
	movq	%rbx, %rdi
	callq	require_errno
	jmp	.LBB15_270
.LBB15_115:                             # %if.else373
	testq	$16, 40(%rsp)           # 8-byte Folded Reload
	movq	%rdx, %r13
	jne	.LBB15_271
# BB#116:                               # %if.end378
	movq	%rsi, %r13
	testq	$4, 40(%rsp)            # 8-byte Folded Reload
	je	.LBB15_122
# BB#117:                               # %land.lhs.true381
	movq	PL_curcop(%rip), %rax
	cmpq	$0, 88(%rax)
	je	.LBB15_121
# BB#118:                               # %lor.lhs.false384
	movq	PL_curcop(%rip), %rax
	movq	88(%rax), %rax
	cmpq	$24, %rax
	je	.LBB15_121
# BB#119:                               # %lor.lhs.false388
	movq	PL_curcop(%rip), %rax
	movq	88(%rax), %rax
	cmpq	$48, %rax
	je	.LBB15_122
# BB#120:                               # %land.lhs.true392
	movq	PL_curcop(%rip), %rax
	movq	88(%rax), %rax
	movq	(%rax), %rax
	movq	(%rax), %rax
	movzbl	6(%rax), %eax
	testq	$1, %rax
	je	.LBB15_122
.LBB15_121:                             # %if.then399
	movabsq	$.L.str32, %rsi
	movq	%rdx, %rbx
	movq	$24, %rdi
	xorq	%rax, %rax
	movq	32(%rsp), %rdx          # 8-byte Reload
	callq	Perl_warner
	movq	%rbx, %rdx
.LBB15_122:                             # %if.end400
	movq	%rdx, %rbx
	movq	%rbx, %rdi
	movq	%r15, %rsi
	movq	%r13, %r15
	movq	%r15, %rdx
	movq	%r12, %rcx
	movq	24(%rsp), %r8           # 8-byte Reload
	callq	Perl_gv_init
	xorq	%rax, %rax
	movq	%rbx, %rdi
	movq	16(%rsp), %rsi          # 8-byte Reload
	callq	gv_init_sv
	movq	%r15, %rsi
	movzbl	(%rsi), %eax
	leaq	-65(%rax), %rcx
	andq	$255, %rcx
	cmpq	$26, %rcx
	jb	.LBB15_124
# BB#123:                               # %if.end400
	addq	$-97, %rax
	andq	$255, %rax
	cmpq	$25, %rax
	ja	.LBB15_132
.LBB15_124:                             # %land.lhs.true423
	movq	PL_curcop(%rip), %rax
	cmpq	$0, 88(%rax)
	je	.LBB15_130
# BB#125:                               # %cond.true427
	movq	PL_curcop(%rip), %rax
	cmpq	$0, 88(%rax)
	je	.LBB15_129
# BB#126:                               # %land.lhs.true431
	movq	PL_curcop(%rip), %rax
	movq	88(%rax), %rax
	cmpq	$48, %rax
	je	.LBB15_129
# BB#127:                               # %land.lhs.true435
	movq	PL_curcop(%rip), %rax
	movq	88(%rax), %rax
	cmpq	$24, %rax
	je	.LBB15_132
# BB#128:                               # %lor.lhs.false439
	movq	PL_curcop(%rip), %rax
	movq	88(%rax), %rax
	movq	(%rax), %rax
	movq	(%rax), %rax
	movzbl	3(%rax), %eax
	testq	$16, %rax
	jne	.LBB15_132
.LBB15_129:                             # %lor.lhs.false447
	movq	PL_curcop(%rip), %rax
	cmpq	$0, 88(%rax)
	jne	.LBB15_131
.LBB15_130:                             # %cond.false455
	movzbl	PL_dowarn(%rip), %eax
	testq	$1, %rax
	jne	.LBB15_132
.LBB15_131:                             # %if.then459
	movq	(%rbx), %rax
	movzbl	88(%rax), %ecx
	orq	$2, %rcx
	movb	%cl, 88(%rax)
.LBB15_132:                             # %if.end465
	movsbq	(%rsi), %rax
	cmpq	$2, %r12
	jl	.LBB15_227
# BB#133:                               # %if.then468
	cmpq	$86, %rax
	movq	%rbx, %rdi
	movq	%rdi, %r13
	jg	.LBB15_271
# BB#134:                               # %if.else473
	movq	%rdi, %r13
	leaq	1(%rsi), %r15
	cmpq	$14, %rax
	jg	.LBB15_138
# BB#135:                               # %if.else473
	movabsq	$4294967295, %r14       # imm = 0xFFFFFFFF
	andq	%r14, %rax
	cmpq	$5, %rax
	jne	.LBB15_271
# BB#136:                               # %sw.bb649
	movq	%rsi, %r13
	movabsq	$.L.str45, %rsi
.LBB15_137:                             # %sw.bb649
	movq	%rdi, %rbx
	movq	%r15, %rdi
	callq	strcmp
	movq	%r13, %rsi
	movq	%rbx, %rdi
	testq	%r14, %rax
	movq	%rdi, %r13
	jne	.LBB15_271
	jmp	.LBB15_261
.LBB15_227:                             # %if.else694
	movq	%rbx, %rdi
	movq	%rdi, %r13
	cmpq	$123, %rax
	jg	.LBB15_230
# BB#228:                               # %if.else694
	leaq	-1(%rax), %rcx
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	cmpq	$95, %rax
	ja	.LBB15_271
# BB#229:                               # %if.else694
	jmpq	*.LJTI15_0(,%rax,8)
.LBB15_232:                             # %sw.bb696
	cmpq	$15, 16(%rsp)           # 8-byte Folded Reload
	ja	.LBB15_234
# BB#233:                               # %sw.bb696
	movq	$1, %rax
	movq	16(%rsp), %rcx          # 8-byte Reload
	shlq	%cl, %rax
	testq	$56320, %rax            # imm = 0xDC00
	movq	%rdi, %r13
	jne	.LBB15_271
.LBB15_234:                             # %if.end712
	movq	$1, %rax
	movb	%al, PL_sawampersand(%rip)
	jmp	.LBB15_260
.LBB15_230:                             # %if.else694
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rax, %rcx
	cmpq	$124, %rcx
	jne	.LBB15_231
# BB#254:                               # %sw.bb786
	movq	PL_defoutgv(%rip), %rax
	movq	(%rax), %rax
	movq	56(%rax), %rax
	movq	16(%rax), %rax
	movq	(%rdi), %rcx
	movq	(%rax), %rax
	movq	56(%rcx), %rcx
	movq	%rsi, %r14
	movzbl	163(%rax), %esi
	movq	%rdi, %rbx
	movq	(%rcx), %rdi
	movq	$2, %rcx
	shrq	%cl, %rsi
	andq	$1, %rsi
	callq	Perl_sv_setiv
.LBB15_253:                             # %magicalize
	movq	%r14, %rsi
	movq	%rbx, %rdi
	jmp	.LBB15_261
.LBB15_109:
	movq	%r14, %r13
	jmp	.LBB15_271
.LBB15_111:
	movq	%r14, %r13
	jmp	.LBB15_271
.LBB15_138:                             # %if.else473
	cmpq	$48, %rax
	jg	.LBB15_141
# BB#139:                               # %if.else473
	leaq	-15(%rax), %rax
	movabsq	$4294967295, %r14       # imm = 0xFFFFFFFF
	andq	%r14, %rax
	cmpq	$8, %rax
	ja	.LBB15_271
# BB#140:                               # %if.else473
	jmpq	*.LJTI15_1(,%rax,8)
.LBB15_221:                             # %sw.bb654
	movq	%rsi, %r13
	movabsq	$.L.str46, %rsi
	jmp	.LBB15_137
.LBB15_231:                             # %if.else694
	cmpq	$126, %rcx
	je	.LBB15_261
	jmp	.LBB15_271
.LBB15_141:                             # %if.else473
	cmpq	$64, %rax
	jg	.LBB15_146
# BB#142:                               # %if.else473
	leaq	-49(%rax), %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rax, %rcx
	cmpq	$9, %rcx
	jae	.LBB15_271
# BB#143:                               # %sw.bb678
	decq	%r14
.LBB15_144:                             # %while.cond
                                        # =>This Inner Loop Header: Depth=1
	cmpq	%rsi, %r14
	jbe	.LBB15_260
# BB#145:                               # %while.body
                                        #   in Loop: Header=BB15_144 Depth=1
	movzbl	(%r14), %eax
	addq	$-48, %rax
	andq	$255, %rax
	decq	%r14
	cmpq	$10, %rax
	movq	%rdi, %r13
	jb	.LBB15_144
	jmp	.LBB15_271
.LBB15_146:                             # %if.else473
	cmpq	$68, %rax
	jg	.LBB15_153
# BB#147:                               # %if.else473
	movabsq	$4294967295, %r14       # imm = 0xFFFFFFFF
	andq	%r14, %rax
	cmpq	$65, %rax
	jne	.LBB15_271
# BB#148:                               # %sw.bb475
	movabsq	$.L.str33, %rsi
	movq	%rdi, %rbx
	movq	%r15, %rdi
	callq	strcmp
	movq	%rbx, %rcx
	testq	%r14, %rax
	movq	%rcx, %r13
	jne	.LBB15_271
# BB#149:                               # %if.then478
	testq	%rcx, %rcx
	movq	%rcx, %rbx
	je	.LBB15_198
# BB#150:                               # %land.lhs.true480
	movzbl	16(%rbx), %eax
	cmpq	$13, %rax
	jne	.LBB15_198
# BB#151:                               # %land.lhs.true485
	movq	(%rbx), %rax
	movq	56(%rax), %rax
	testq	%rax, %rax
	je	.LBB15_199
# BB#152:                               # %cond.true489
	movq	16(%rax), %rax
	testq	%rax, %rax
	jne	.LBB15_201
	jmp	.LBB15_199
.LBB15_153:                             # %if.else473
	cmpq	$72, %rax
	jg	.LBB15_158
# BB#154:                               # %if.else473
	movabsq	$4294967295, %r14       # imm = 0xFFFFFFFF
	andq	%r14, %rax
	cmpq	$69, %rax
	jne	.LBB15_271
# BB#155:                               # %sw.bb510
	movabsq	$.L.str34, %rsi
	movq	$5, %rdx
	movq	%rdi, %rbx
	movq	%r15, %rdi
	callq	strncmp
	jmp	.LBB15_156
.LBB15_222:                             # %sw.bb659
	movq	%rsi, %r13
	movabsq	$.L.str47, %rsi
	jmp	.LBB15_223
.LBB15_224:                             # %sw.bb664
	movq	%rsi, %r13
	movabsq	$.L.str48, %rsi
	movq	%rdi, %rbx
	movq	%r15, %rdi
	callq	strcmp
	movq	%r13, %rsi
	movq	%rbx, %rdi
	testq	%r14, %rax
	je	.LBB15_260
# BB#225:                               # %if.end668
	movq	%rsi, %r13
	movabsq	$.L.str49, %rsi
.LBB15_223:                             # %sw.bb659
	movq	%rdi, %rbx
	movq	%r15, %rdi
	callq	strcmp
	movq	%r13, %rsi
	movq	%rbx, %rdi
	testq	%r14, %rax
	movq	%rdi, %r13
	jne	.LBB15_271
	jmp	.LBB15_260
.LBB15_226:                             # %sw.bb673
	movq	%rsi, %r13
	movabsq	$.L.str50, %rsi
	jmp	.LBB15_137
.LBB15_158:                             # %if.else473
	cmpq	$78, %rax
	jg	.LBB15_194
# BB#159:                               # %if.else473
	movabsq	$4294967295, %r14       # imm = 0xFFFFFFFF
	andq	%r14, %rax
	cmpq	$73, %rax
	jne	.LBB15_271
# BB#160:                               # %sw.bb520
	movabsq	$.L.str35, %rax
	movq	%rdi, %rbx
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	strcmp
	movq	%rbx, %rdx
	testq	%r14, %rax
	movq	%rdx, %r13
	jne	.LBB15_271
# BB#161:                               # %if.then523
	movq	(%rdx), %rcx
	movq	%rdx, %r15
	movq	56(%rcx), %rax
	movq	32(%rax), %rbx
	testq	%rbx, %rbx
	jne	.LBB15_166
# BB#162:                               # %cond.false531
	testq	%r15, %r15
	je	.LBB15_164
# BB#163:                               # %lor.lhs.false.i1164
	movzbl	16(%r15), %eax
	cmpq	$13, %rax
	je	.LBB15_165
.LBB15_164:                             # %if.end.i1170
	movabsq	$.L.str, %rcx
	xorq	%rax, %rax
	movq	%rcx, %rdi
	callq	Perl_croak
	movq	(%r15), %rcx
	movq	56(%rcx), %rax
	movq	32(%rax), %rbx
	testq	%rbx, %rbx
	jne	.LBB15_166
.LBB15_165:                             # %if.then2.i1174
	callq	Perl_newAV
	movq	(%r15), %rcx
	movq	56(%rcx), %rdx
	movq	%rax, %rbx
	movq	%rbx, 32(%rdx)
.LBB15_166:                             # %cond.end536
	movzbl	88(%rcx), %eax
	orq	$2, %rax
	movb	%al, 88(%rcx)
	movq	$73, %rdx
	xorq	%rcx, %rcx
	xorq	%r8, %r8
	movq	%rbx, %rdi
	movq	%r15, %rsi
	callq	Perl_sv_magic
	movq	%r15, %rcx
	cmpq	$0, 24(%rsp)            # 8-byte Folded Reload
	movq	%rcx, %r13
	je	.LBB15_271
# BB#167:                               # %land.lhs.true545
	movabsq	$.L.str36, %rax
	movq	%rcx, %r15
	movq	32(%rsp), %rdi          # 8-byte Reload
	movq	%rax, %rsi
	callq	strcmp
	movq	%r15, %rcx
	testq	%r14, %rax
	movq	%rcx, %r13
	jne	.LBB15_271
# BB#168:                               # %land.lhs.true548
	movq	(%rbx), %rax
	movq	8(%rax), %rax
	cmpq	$-1, %rax
	movq	%rcx, %r13
	jne	.LBB15_271
# BB#169:                               # %if.end10.i1312
	movq	%rcx, 8(%rsp)           # 8-byte Spill
	movabsq	$.L.str37, %rax
	movq	$9, %rsi
	movq	%rax, %rdi
	callq	Perl_newSVpvn
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	Perl_av_push
	movq	$101, %r15
	movb	%r15b, 312(%rsp)
	movabsq	$7811852403782861902, %rax # imm = 0x6C69465F4D42444E
	movq	%rax, 304(%rsp)
	movq	$58, %r14
	movb	%r14b, 313(%rsp)
	movb	%r14b, 314(%rsp)
	xorq	%r12, %r12
	movb	%r12b, 315(%rsp)
	leaq	304(%rsp), %rax
	movq	$1, %rsi
	movq	$11, %rdx
	movq	%rax, %rdi
	callq	Perl_gv_fetchpv
	movq	%rax, 40(%rsp)          # 8-byte Spill
	testq	%rax, %rax
	je	.LBB15_174
# BB#170:                               # %if.end12.i1317
	movq	40(%rsp), %rax          # 8-byte Reload
	movq	(%rax), %rax
	movq	56(%rax), %rax
	movq	40(%rax), %r13
	testq	%r13, %r13
	jne	.LBB15_172
# BB#171:                               # %if.then14.i1321
	callq	Perl_newHV
	movq	40(%rsp), %rcx          # 8-byte Reload
	movq	(%rcx), %rcx
	movq	56(%rcx), %rcx
	movq	%rax, %r13
	movq	%r13, 40(%rcx)
.LBB15_172:                             # %if.end19.i1327
	movq	(%r13), %rax
	cmpq	$0, 80(%rax)
	jne	.LBB15_174
# BB#173:                               # %if.then25.i1330
	movabsq	$.L.str37, %rax
	movq	%rax, %rdi
	callq	Perl_savepv
	movq	(%r13), %rcx
	movq	%rax, 80(%rcx)
.LBB15_174:                             # %if.end10.i1281
	movabsq	$.L.str38, %rax
	movq	$7, %rsi
	movq	%rax, %rdi
	callq	Perl_newSVpvn
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	Perl_av_push
	movb	%r15b, 310(%rsp)
	movq	$27753, %rax            # imm = 0x6C69
	movw	%ax, 308(%rsp)
	movq	$1180648004, %rax       # imm = 0x465F4244
	movl	%eax, 304(%rsp)
	movb	%r14b, 311(%rsp)
	movb	%r14b, 312(%rsp)
	movb	%r12b, 313(%rsp)
	leaq	304(%rsp), %rax
	movq	$1, %rsi
	movq	$11, %rdx
	movq	%rax, %rdi
	callq	Perl_gv_fetchpv
	movq	%rax, %r15
	testq	%r15, %r15
	je	.LBB15_179
# BB#175:                               # %if.end12.i1286
	movq	(%r15), %rax
	movq	56(%rax), %rax
	movq	40(%rax), %r14
	testq	%r14, %r14
	jne	.LBB15_177
# BB#176:                               # %if.then14.i1290
	callq	Perl_newHV
	movq	(%r15), %rcx
	movq	56(%rcx), %rcx
	movq	%rax, %r14
	movq	%r14, 40(%rcx)
.LBB15_177:                             # %if.end19.i1296
	movq	(%r14), %rax
	cmpq	$0, 80(%rax)
	jne	.LBB15_179
# BB#178:                               # %if.then25.i1299
	movabsq	$.L.str38, %rax
	movq	%rax, %rdi
	callq	Perl_savepv
	movq	(%r14), %rcx
	movq	%rax, 80(%rcx)
.LBB15_179:                             # %if.end10.i1250
	movabsq	$.L.str39, %rax
	movq	$9, %rsi
	movq	%rax, %rdi
	callq	Perl_newSVpvn
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	Perl_av_push
	movq	$101, %r14
	movb	%r14b, 312(%rsp)
	movabsq	$7811852403782861895, %rax # imm = 0x6C69465F4D424447
	movq	%rax, 304(%rsp)
	movq	$58, %r15
	movb	%r15b, 313(%rsp)
	movb	%r15b, 314(%rsp)
	xorq	%r12, %r12
	movb	%r12b, 315(%rsp)
	leaq	304(%rsp), %rax
	movq	$1, %rsi
	movq	$11, %rdx
	movq	%rax, %rdi
	callq	Perl_gv_fetchpv
	movq	%rax, 40(%rsp)          # 8-byte Spill
	testq	%rax, %rax
	je	.LBB15_184
# BB#180:                               # %if.end12.i1255
	movq	40(%rsp), %rax          # 8-byte Reload
	movq	(%rax), %rax
	movq	56(%rax), %rax
	movq	40(%rax), %r13
	testq	%r13, %r13
	jne	.LBB15_182
# BB#181:                               # %if.then14.i1259
	callq	Perl_newHV
	movq	40(%rsp), %rcx          # 8-byte Reload
	movq	(%rcx), %rcx
	movq	56(%rcx), %rcx
	movq	%rax, %r13
	movq	%r13, 40(%rcx)
.LBB15_182:                             # %if.end19.i1265
	movq	(%r13), %rax
	cmpq	$0, 80(%rax)
	jne	.LBB15_184
# BB#183:                               # %if.then25.i1268
	movabsq	$.L.str39, %rax
	movq	%rax, %rdi
	callq	Perl_savepv
	movq	(%r13), %rcx
	movq	%rax, 80(%rcx)
.LBB15_184:                             # %if.end10.i1219
	movabsq	$.L.str40, %rax
	movq	$9, %rsi
	movq	%rax, %rdi
	callq	Perl_newSVpvn
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	Perl_av_push
	movb	%r14b, 312(%rsp)
	movabsq	$7811852403782861907, %rax # imm = 0x6C69465F4D424453
	movq	%rax, 304(%rsp)
	movb	%r15b, 313(%rsp)
	movb	%r15b, 314(%rsp)
	movb	%r12b, 315(%rsp)
	leaq	304(%rsp), %rax
	movq	$1, %rsi
	movq	$11, %rdx
	movq	%rax, %rdi
	callq	Perl_gv_fetchpv
	movq	%rax, %r15
	testq	%r15, %r15
	je	.LBB15_189
# BB#185:                               # %if.end12.i1224
	movq	(%r15), %rax
	movq	56(%rax), %rax
	movq	40(%rax), %r14
	testq	%r14, %r14
	jne	.LBB15_187
# BB#186:                               # %if.then14.i1228
	callq	Perl_newHV
	movq	(%r15), %rcx
	movq	56(%rcx), %rcx
	movq	%rax, %r14
	movq	%r14, 40(%rcx)
.LBB15_187:                             # %if.end19.i1234
	movq	(%r14), %rax
	cmpq	$0, 80(%rax)
	jne	.LBB15_189
# BB#188:                               # %if.then25.i1237
	movabsq	$.L.str40, %rax
	movq	%rax, %rdi
	callq	Perl_savepv
	movq	(%r14), %rcx
	movq	%rax, 80(%rcx)
.LBB15_189:                             # %if.end10.i
	movabsq	$.L.str41, %rax
	movq	$9, %rsi
	movq	%rax, %rdi
	callq	Perl_newSVpvn
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	Perl_av_push
	movq	$101, %rax
	movb	%al, 312(%rsp)
	movabsq	$7811852403782861903, %rax # imm = 0x6C69465F4D42444F
	movq	%rax, 304(%rsp)
	movq	$58, %rax
	movb	%al, 313(%rsp)
	movb	%al, 314(%rsp)
	xorq	%rax, %rax
	movb	%al, 315(%rsp)
	leaq	304(%rsp), %rax
	movq	$1, %rsi
	movq	$11, %rdx
	movq	%rax, %rdi
	callq	Perl_gv_fetchpv
	movq	%rax, %r14
	testq	%r14, %r14
	movq	8(%rsp), %rax           # 8-byte Reload
	movq	%rax, %r13
	je	.LBB15_271
# BB#190:                               # %if.end12.i
	movq	%rax, %r15
	movq	(%r14), %rax
	movq	56(%rax), %rax
	movq	40(%rax), %rbx
	testq	%rbx, %rbx
	jne	.LBB15_192
# BB#191:                               # %if.then14.i
	callq	Perl_newHV
	movq	(%r14), %rcx
	movq	56(%rcx), %rcx
	movq	%rax, %rbx
	movq	%rbx, 40(%rcx)
.LBB15_192:                             # %if.end19.i
	movq	(%rbx), %rax
	cmpq	$0, 80(%rax)
	movq	%r15, %rcx
	movq	%rcx, %r13
	jne	.LBB15_271
# BB#193:                               # %if.then25.i
	movabsq	$.L.str41, %rax
	movq	%rcx, %r14
	movq	%rax, %rdi
	callq	Perl_savepv
	movq	(%rbx), %rcx
	movq	%rax, 80(%rcx)
	movq	%r14, %r13
	jmp	.LBB15_271
.LBB15_244:                             # %sw.bb745
	cmpq	$4, 16(%rsp)            # 8-byte Folded Reload
	jne	.LBB15_261
# BB#245:                               # %land.lhs.true748
	movq	PL_curcop(%rip), %rax
	cmpq	$0, 88(%rax)
	je	.LBB15_250
# BB#246:                               # %land.lhs.true752
	movq	PL_curcop(%rip), %rax
	movq	88(%rax), %rax
	cmpq	$48, %rax
	je	.LBB15_250
# BB#247:                               # %land.lhs.true756
	movq	PL_curcop(%rip), %rax
	movq	88(%rax), %rax
	cmpq	$24, %rax
	je	.LBB15_252
# BB#248:                               # %lor.lhs.false760
	movq	PL_curcop(%rip), %rax
	movq	88(%rax), %rax
	movq	(%rax), %rax
	movq	(%rax), %rax
	movzbl	(%rax), %eax
	testq	$16, %rax
	jne	.LBB15_252
# BB#249:                               # %lor.lhs.false768
	movq	PL_curcop(%rip), %rax
	movq	88(%rax), %rax
	movq	(%rax), %rax
	movq	(%rax), %rax
	movzbl	7(%rax), %eax
	testq	$1, %rax
	jne	.LBB15_252
.LBB15_250:                             # %lor.lhs.false776
	movq	PL_curcop(%rip), %rax
	cmpq	$0, 88(%rax)
	jne	.LBB15_261
# BB#251:                               # %land.lhs.true780
	movzbl	PL_dowarn(%rip), %eax
	testq	$1, %rax
	je	.LBB15_261
.LBB15_252:                             # %if.then784
	movabsq	$.L.str51, %rcx
	movq	%rdi, %rbx
	movq	$7170, %rdi             # imm = 0x1C02
	xorq	%rax, %rax
	movq	%rsi, %r14
	movq	%rcx, %rsi
	movq	%r14, %rdx
	callq	Perl_warner
	jmp	.LBB15_253
.LBB15_194:                             # %if.else473
	movq	%rdi, %rbx
	movabsq	$4294967295, %r14       # imm = 0xFFFFFFFF
	andq	%r14, %rax
	cmpq	$79, %rax
	je	.LBB15_202
# BB#195:                               # %if.else473
	cmpq	$83, %rax
	jne	.LBB15_196
# BB#209:                               # %sw.bb590
	movabsq	$.L.str43, %rax
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	strcmp
	testq	%r14, %rax
	movq	%rbx, %rax
	movq	%rax, %r13
	jne	.LBB15_271
# BB#210:                               # %if.then593
	movq	%rax, %r13
	cmpq	$0, PL_psig_ptr(%rip)
	jne	.LBB15_212
# BB#211:                               # %if.then596
	movq	$128, %rdi
	callq	Perl_safesysmalloc
	movq	%rax, PL_psig_ptr(%rip)
	movq	$0, 120(%rax)
	movq	$0, 112(%rax)
	movq	$0, 104(%rax)
	movq	$0, 96(%rax)
	movq	$0, 88(%rax)
	movq	$0, 80(%rax)
	movq	$0, 72(%rax)
	movq	$0, 64(%rax)
	movq	$0, 56(%rax)
	movq	$0, 48(%rax)
	movq	$0, 40(%rax)
	movq	$0, 32(%rax)
	movq	$0, 24(%rax)
	movq	$0, 16(%rax)
	movq	$0, 8(%rax)
	movq	$0, (%rax)
	movq	$128, %rdi
	callq	Perl_safesysmalloc
	movq	%rax, PL_psig_name(%rip)
	movq	$0, 120(%rax)
	movq	$0, 112(%rax)
	movq	$0, 104(%rax)
	movq	$0, 96(%rax)
	movq	$0, 88(%rax)
	movq	$0, 80(%rax)
	movq	$0, 72(%rax)
	movq	$0, 64(%rax)
	movq	$0, 56(%rax)
	movq	$0, 48(%rax)
	movq	$0, 40(%rax)
	movq	$0, 32(%rax)
	movq	$0, 24(%rax)
	movq	$0, 16(%rax)
	movq	$0, 8(%rax)
	movq	$0, (%rax)
	movq	$64, %rdi
	callq	Perl_safesysmalloc
	movq	%rax, PL_psig_pend(%rip)
	movq	$0, 56(%rax)
	movq	$0, 48(%rax)
	movq	$0, 40(%rax)
	movq	$0, 32(%rax)
	movq	$0, 24(%rax)
	movq	$0, 16(%rax)
	movq	$0, 8(%rax)
	movq	$0, (%rax)
.LBB15_212:                             # %if.end600
	movq	(%r13), %rax
	movzbl	88(%rax), %ecx
	orq	$2, %rcx
	movb	%cl, 88(%rax)
	movq	56(%rax), %rax
	movq	40(%rax), %rbx
	testq	%rbx, %rbx
	jne	.LBB15_217
# BB#213:                               # %cond.false614
	testq	%r13, %r13
	je	.LBB15_215
# BB#214:                               # %lor.lhs.false.i1179
	movzbl	16(%r13), %eax
	cmpq	$13, %rax
	je	.LBB15_216
.LBB15_215:                             # %if.end.i1184
	movabsq	$.L.str1, %rcx
	xorq	%rax, %rax
	movq	%rcx, %rdi
	callq	Perl_croak
	movq	(%r13), %rax
	movq	56(%rax), %rax
	movq	40(%rax), %rbx
	testq	%rbx, %rbx
	jne	.LBB15_217
.LBB15_216:                             # %if.then2.i1187
	callq	Perl_newHV
	movq	(%r13), %rcx
	movq	56(%rcx), %rcx
	movq	%rax, %rbx
	movq	%rbx, 40(%rcx)
.LBB15_217:                             # %cond.end619
	xorq	%rsi, %rsi
	movq	$83, %rdx
	xorq	%r14, %r14
	movq	%rbx, %rdi
	callq	Perl_hv_magic
	movabsq	$PL_sv_undef, %r15
	xorq	%r12, %r12
	movq	%r13, %rax
	movq	%rax, 8(%rsp)           # 8-byte Spill
.LBB15_218:                             # %for.body624
                                        # =>This Inner Loop Header: Depth=1
	movq	PL_sig_name+8(%r12,%r12), %r13
	movq	%r13, %rdi
	callq	strlen
	movq	$1, %rcx
	movq	%rbx, %rdi
	movq	%r13, %rsi
	movq	%rax, %rdx
	callq	Perl_hv_fetch
	testq	%rax, %rax
	je	.LBB15_220
# BB#219:                               # %if.then630
                                        #   in Loop: Header=BB15_218 Depth=1
	movq	(%rax), %rax
	movq	$2, %rdx
	movq	%rax, %rdi
	movq	%r15, %rsi
	callq	Perl_sv_setsv_flags
.LBB15_220:                             # %if.end631
                                        #   in Loop: Header=BB15_218 Depth=1
	movq	PL_psig_ptr(%rip), %rax
	movq	$0, 8(%rax,%r12,2)
	movq	PL_psig_name(%rip), %rax
	movq	$0, 8(%rax,%r12,2)
	movq	PL_psig_pend(%rip), %rax
	movl	%r14d, 4(%rax,%r12)
	addq	$4, %r12
	cmpq	$60, %r12
	movq	8(%rsp), %rax           # 8-byte Reload
	movq	%rax, %r13
	jne	.LBB15_218
	jmp	.LBB15_271
.LBB15_262:                             # %sw.bb830
	movq	(%rdi), %rax
	movq	56(%rax), %rax
	movq	(%rax), %rax
	movabsq	$.L.str52, %rcx
	movq	%rdi, %rbx
	movq	%rax, %rdi
	movq	%rcx, %rsi
	callq	Perl_sv_setpv
	movq	(%rbx), %rax
	movq	56(%rax), %rax
	movq	(%rax), %rax
	movq	%rax, PL_formfeed(%rip)
	jmp	.LBB15_270
.LBB15_267:                             # %sw.bb861
	movq	(%rdi), %rax
	movq	%rdi, %rdx
	movq	56(%rax), %rax
	movq	PL_patchlevel(%rip), %rcx
	movq	(%rax), %rax
	testq	%rcx, %rcx
	movq	%rcx, PL_Sv(%rip)
	je	.LBB15_269
# BB#268:                               # %land.rhs
	incq	8(%rcx)
.LBB15_269:                             # %land.end
	movq	%rdx, %rbx
	movq	(%rbx), %rdx
	movq	56(%rdx), %rdx
	movq	%rcx, (%rdx)
	movq	%rax, %rdi
	callq	Perl_sv_free
	jmp	.LBB15_270
.LBB15_236:                             # %sw.bb717
	movq	(%rdi), %rax
	movq	56(%rax), %rax
	movq	(%rax), %rax
	xorq	%rdx, %rdx
	movq	%rdi, %rbx
	movq	%rax, %rdi
	movq	%rsi, %rax
	movq	%rbx, %rsi
	movq	%rax, %rcx
	movq	%r12, %r8
	callq	Perl_sv_magic
	movq	%rbx, %rcx
	cmpq	$11, 16(%rsp)           # 8-byte Folded Reload
	movq	%rcx, %r13
	jne	.LBB15_271
# BB#237:                               # %if.then723
	xorq	%rax, %rax
	movq	%rcx, %rbx
	jmp	.LBB15_114
.LBB15_255:                             # %sw.bb801
	movq	%rsi, %r15
	movq	(%rdi), %rax
	movq	%rdi, %r14
	movq	56(%rax), %rax
	movq	32(%rax), %rbx
	testq	%rbx, %rbx
	jne	.LBB15_259
# BB#256:                               # %lor.lhs.false.i
	movzbl	16(%r14), %eax
	cmpq	$13, %rax
	je	.LBB15_258
# BB#257:                               # %if.end.i
	movabsq	$.L.str, %rcx
	xorq	%rax, %rax
	movq	%rcx, %rdi
	callq	Perl_croak
	movq	(%r14), %rax
	movq	56(%rax), %rax
	movq	32(%rax), %rbx
	testq	%rbx, %rbx
	jne	.LBB15_259
.LBB15_258:                             # %if.then2.i
	callq	Perl_newAV
	movq	(%r14), %rcx
	movq	56(%rcx), %rcx
	movq	%rax, %rbx
	movq	%rbx, 32(%rcx)
.LBB15_259:                             # %cond.end816
	movq	$68, %rdx
	xorq	%rcx, %rcx
	xorq	%r8, %r8
	movq	%rbx, %rdi
	movq	%rbx, %rsi
	callq	Perl_sv_magic
	orq	$8388608, 16(%rbx)      # imm = 0x800000
	movq	%r14, %rdi
	movq	%r15, %rsi
.LBB15_260:                             # %ro_magicalize
	movq	(%rdi), %rax
	movq	56(%rax), %rax
	movq	(%rax), %rax
	orq	$8388608, 16(%rax)      # imm = 0x800000
.LBB15_261:                             # %magicalize
	movq	(%rdi), %rax
	movq	56(%rax), %rax
	movq	%rdi, %rbx
	movq	(%rax), %rdi
	xorq	%rdx, %rdx
	movq	%rsi, %rcx
	movq	%rbx, %rsi
	movq	%r12, %r8
	callq	Perl_sv_magic
.LBB15_270:                             # %return
	movq	%rbx, %r13
.LBB15_271:                             # %return
	movq	%r13, %rax
	addq	$560, %rsp              # imm = 0x230
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB15_238:                             # %sw.bb726
	movq	%rsi, %r15
	movq	(%rdi), %rax
	movq	%rdi, %r14
	movq	56(%rax), %rax
	movq	32(%rax), %rbx
	testq	%rbx, %rbx
	jne	.LBB15_243
# BB#239:                               # %cond.false736
	testq	%r14, %r14
	je	.LBB15_241
# BB#240:                               # %lor.lhs.false.i1149
	movzbl	16(%r14), %eax
	cmpq	$13, %rax
	je	.LBB15_242
.LBB15_241:                             # %if.end.i1155
	movabsq	$.L.str, %rcx
	xorq	%rax, %rax
	movq	%rcx, %rdi
	callq	Perl_croak
	movq	(%r14), %rax
	movq	56(%rax), %rax
	movq	32(%rax), %rbx
	testq	%rbx, %rbx
	jne	.LBB15_243
.LBB15_242:                             # %if.then2.i1159
	callq	Perl_newAV
	movq	(%r14), %rcx
	movq	56(%rcx), %rcx
	movq	%rax, %rbx
	movq	%rbx, 32(%rcx)
.LBB15_243:                             # %cond.end741
	xorq	%rsi, %rsi
	movq	$68, %rdx
	xorq	%rcx, %rcx
	xorq	%r8, %r8
	movq	%rbx, %rdi
	callq	Perl_sv_magic
	orq	$8388608, 16(%rbx)      # imm = 0x800000
	movq	%r14, %rdi
	movq	%r15, %rsi
	jmp	.LBB15_261
.LBB15_235:                             # %sw.bb713
	movq	(%rdi), %rax
	movq	56(%rax), %rax
	movq	(%rax), %rax
	movq	PL_chopset(%rip), %rcx
	movq	%rdi, %rbx
	movq	%rax, %rdi
	movq	%rsi, %r14
	movq	%rcx, %rsi
	callq	Perl_sv_setpv
	jmp	.LBB15_253
.LBB15_263:                             # %sw.bb837
	movq	(%rdi), %rax
	movq	56(%rax), %rax
	movq	(%rax), %rax
	movabsq	$.L.str53, %rcx
	movq	%rdi, %rbx
	movq	%rax, %rdi
	movq	%rcx, %rsi
	callq	Perl_sv_setpv
	jmp	.LBB15_270
.LBB15_264:                             # %sw.bb841
	movq	(%rdi), %rax
	movq	%rdi, %r14
	movq	56(%rax), %rax
	movq	(%rax), %rbx
	movq	16(%rbx), %rax
	andq	$254, %rax
	cmpq	$5, %rax
	ja	.LBB15_266
# BB#265:                               # %lor.rhs
	movq	$6, %rsi
	movq	%rbx, %rdi
	callq	Perl_sv_upgrade
.LBB15_266:                             # %lor.end
	movq	PL_patchlevel(%rip), %rax
	movq	(%rax), %rax
	movq	32(%rax), %rcx
	movabsq	$.L.str54, %rdx
	xorq	%rax, %rax
	movq	%rbx, %rdi
	movq	%rdx, %rsi
	movq	%rcx, %rdx
	callq	Perl_sv_setpvf
	movq	PL_patchlevel(%rip), %rax
	movq	(%rax), %rax
	movq	32(%rax), %rax
	movq	(%rbx), %rcx
	movq	%rax, 32(%rcx)
	orq	$42074112, 16(%rbx)     # imm = 0x2820000
	movq	%r14, %r13
	jmp	.LBB15_271
.LBB15_198:                             # %if.then.i1337
	movabsq	$.L.str2, %rcx
	xorq	%rax, %rax
	movq	%rcx, %rdi
	callq	Perl_croak
.LBB15_199:                             # %if.end.i1341
	movq	(%rbx), %rax
	movq	56(%rax), %rax
	movq	16(%rax), %rax
	testq	%rax, %rax
	jne	.LBB15_201
# BB#200:                               # %if.then2.i1344
	callq	Perl_newIO
	movq	(%rbx), %rcx
	movq	56(%rcx), %rcx
	movq	%rax, 16(%rcx)
.LBB15_201:                             # %cond.end503
	movq	(%rax), %rax
	movzbl	163(%rax), %ecx
	orq	$3, %rcx
	movb	%cl, 163(%rax)
	jmp	.LBB15_270
.LBB15_202:                             # %sw.bb565
	movabsq	$.L.str42, %rax
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	strcmp
	testq	%r14, %rax
	movq	%rbx, %rax
	movq	%rax, %r13
	jne	.LBB15_271
# BB#203:                               # %if.then568
	movq	(%rax), %rcx
	movq	%rax, %rbx
	movq	56(%rcx), %rax
	movq	40(%rax), %rax
	testq	%rax, %rax
	jne	.LBB15_208
# BB#204:                               # %cond.false577
	testq	%rbx, %rbx
	je	.LBB15_206
# BB#205:                               # %lor.lhs.false.i1191
	movzbl	16(%rbx), %eax
	cmpq	$13, %rax
	je	.LBB15_207
.LBB15_206:                             # %if.end.i1197
	movabsq	$.L.str1, %rcx
	xorq	%rax, %rax
	movq	%rcx, %rdi
	callq	Perl_croak
	movq	(%rbx), %rcx
	movq	56(%rcx), %rax
	movq	40(%rax), %rax
	testq	%rax, %rax
	jne	.LBB15_208
.LBB15_207:                             # %if.then2.i1201
	callq	Perl_newHV
	movq	(%rbx), %rcx
	movq	56(%rcx), %rdx
	movq	%rax, 40(%rdx)
.LBB15_208:                             # %cond.end582
	movzbl	88(%rcx), %edx
	orq	$2, %rdx
	movb	%dl, 88(%rcx)
	xorq	%rsi, %rsi
	movq	$65, %rdx
	movq	%rax, %rdi
	callq	Perl_hv_magic
	jmp	.LBB15_270
.LBB15_196:                             # %if.else473
	cmpq	$86, %rax
	movq	%rbx, %rcx
	jne	.LBB15_271
# BB#197:                               # %sw.bb639
	movabsq	$.L.str44, %rax
	movq	%rcx, %rbx
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	strcmp
.LBB15_156:                             # %sw.bb510
	movq	%rbx, %rdx
	testq	%r14, %rax
	movq	%rdx, %r13
	jne	.LBB15_271
# BB#157:                               # %if.then513
	movq	(%rdx), %rax
	movzbl	88(%rax), %ecx
	orq	$2, %rcx
	movb	%cl, 88(%rax)
	movq	%rdx, %r13
	jmp	.LBB15_271
.Ltmp189:
	.size	Perl_gv_fetchpv, .Ltmp189-Perl_gv_fetchpv
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI15_0:
	.quad	.LBB15_261
	.quad	.LBB15_271
	.quad	.LBB15_261
	.quad	.LBB15_261
	.quad	.LBB15_261
	.quad	.LBB15_261
	.quad	.LBB15_271
	.quad	.LBB15_261
	.quad	.LBB15_261
	.quad	.LBB15_271
	.quad	.LBB15_271
	.quad	.LBB15_262
	.quad	.LBB15_271
	.quad	.LBB15_261
	.quad	.LBB15_261
	.quad	.LBB15_261
	.quad	.LBB15_271
	.quad	.LBB15_271
	.quad	.LBB15_260
	.quad	.LBB15_261
	.quad	.LBB15_271
	.quad	.LBB15_267
	.quad	.LBB15_261
	.quad	.LBB15_271
	.quad	.LBB15_271
	.quad	.LBB15_271
	.quad	.LBB15_271
	.quad	.LBB15_271
	.quad	.LBB15_271
	.quad	.LBB15_271
	.quad	.LBB15_271
	.quad	.LBB15_271
	.quad	.LBB15_236
	.quad	.LBB15_271
	.quad	.LBB15_244
	.quad	.LBB15_271
	.quad	.LBB15_261
	.quad	.LBB15_232
	.quad	.LBB15_232
	.quad	.LBB15_261
	.quad	.LBB15_261
	.quad	.LBB15_244
	.quad	.LBB15_255
	.quad	.LBB15_261
	.quad	.LBB15_238
	.quad	.LBB15_261
	.quad	.LBB15_261
	.quad	.LBB15_271
	.quad	.LBB15_260
	.quad	.LBB15_260
	.quad	.LBB15_260
	.quad	.LBB15_260
	.quad	.LBB15_260
	.quad	.LBB15_260
	.quad	.LBB15_260
	.quad	.LBB15_260
	.quad	.LBB15_260
	.quad	.LBB15_235
	.quad	.LBB15_263
	.quad	.LBB15_261
	.quad	.LBB15_261
	.quad	.LBB15_261
	.quad	.LBB15_261
	.quad	.LBB15_271
	.quad	.LBB15_271
	.quad	.LBB15_271
	.quad	.LBB15_271
	.quad	.LBB15_271
	.quad	.LBB15_271
	.quad	.LBB15_271
	.quad	.LBB15_271
	.quad	.LBB15_271
	.quad	.LBB15_271
	.quad	.LBB15_271
	.quad	.LBB15_271
	.quad	.LBB15_271
	.quad	.LBB15_271
	.quad	.LBB15_271
	.quad	.LBB15_271
	.quad	.LBB15_271
	.quad	.LBB15_271
	.quad	.LBB15_271
	.quad	.LBB15_271
	.quad	.LBB15_271
	.quad	.LBB15_271
	.quad	.LBB15_271
	.quad	.LBB15_271
	.quad	.LBB15_271
	.quad	.LBB15_271
	.quad	.LBB15_271
	.quad	.LBB15_261
	.quad	.LBB15_261
	.quad	.LBB15_264
	.quad	.LBB15_261
	.quad	.LBB15_271
	.quad	.LBB15_232
.LJTI15_1:
	.quad	.LBB15_221
	.quad	.LBB15_271
	.quad	.LBB15_271
	.quad	.LBB15_271
	.quad	.LBB15_271
	.quad	.LBB15_222
	.quad	.LBB15_224
	.quad	.LBB15_271
	.quad	.LBB15_226

	.text
	.globl	Perl_gv_fullname4
	.align	16, 0x90
	.type	Perl_gv_fullname4,@function
Perl_gv_fullname4:                      # @Perl_gv_fullname4
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp195:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp196:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp197:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp198:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp199:
	.cfi_def_cfa_offset 48
.Ltmp200:
	.cfi_offset %rbx, -40
.Ltmp201:
	.cfi_offset %r12, -32
.Ltmp202:
	.cfi_offset %r14, -24
.Ltmp203:
	.cfi_offset %r15, -16
	movq	%rsi, %r14
	movq	(%r14), %rax
	movq	80(%rax), %rbx
	movq	%rcx, %r15
	movq	%rdi, %r12
	testq	%rbx, %rbx
	je	.LBB16_1
# BB#3:                                 # %if.end
	testq	%rdx, %rdx
	jne	.LBB16_5
# BB#4:                                 # %if.end
	movabsq	$.L.str3, %rdx
.LBB16_5:                               # %if.end
	movq	%r12, %rdi
	movq	%rdx, %rsi
	callq	Perl_sv_setpv
	movq	(%rbx), %rax
	movq	80(%rax), %rbx
	testq	%rbx, %rbx
	jne	.LBB16_7
# BB#6:                                 # %if.end
	movabsq	$.L.str55, %rbx
.LBB16_7:                               # %if.end
	testq	$255, %r15
	jne	.LBB16_9
# BB#8:                                 # %lor.lhs.false
	movabsq	$.L.str56, %rsi
	movq	%rbx, %rdi
	callq	strcmp
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB16_10
.LBB16_9:                               # %if.then13
	movq	%r12, %rdi
	movq	%rbx, %rsi
	callq	Perl_sv_catpv
	movabsq	$.L.str16, %rsi
	movq	$2, %rdx
	movq	$2, %rcx
	movq	%r12, %rdi
	callq	Perl_sv_catpvn_flags
.LBB16_10:                              # %if.end14
	movq	(%r14), %rax
	movq	64(%rax), %rsi
	movq	72(%rax), %rdx
	movq	$2, %rcx
	movq	%r12, %rdi
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	jmp	Perl_sv_catpvn_flags  # TAILCALL
.LBB16_1:                               # %if.then
	movq	16(%r12), %rax
	movq	%rax, %rcx
	andq	$1223753727, %rcx       # imm = 0x48F0FFFF
	movq	%rcx, 16(%r12)
	testq	$2097152, %rax          # imm = 0x200000
	je	.LBB16_11
# BB#2:                                 # %land.rhs
	movq	%r12, %rdi
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	jmp	Perl_sv_backoff  # TAILCALL
.LBB16_11:                              # %return
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.Ltmp204:
	.size	Perl_gv_fullname4, .Ltmp204-Perl_gv_fullname4
	.cfi_endproc

	.globl	Perl_gv_fullname3
	.align	16, 0x90
	.type	Perl_gv_fullname3,@function
Perl_gv_fullname3:                      # @Perl_gv_fullname3
	.cfi_startproc
# BB#0:                                 # %entry
	movq	$1, %rcx
	jmp	Perl_gv_fullname4  # TAILCALL
.Ltmp205:
	.size	Perl_gv_fullname3, .Ltmp205-Perl_gv_fullname3
	.cfi_endproc

	.globl	Perl_gv_efullname4
	.align	16, 0x90
	.type	Perl_gv_efullname4,@function
Perl_gv_efullname4:                     # @Perl_gv_efullname4
	.cfi_startproc
# BB#0:                                 # %entry
	movq	(%rsi), %rax
	movq	56(%rax), %rax
	movq	48(%rax), %rax
	testq	%rax, %rax
	je	.LBB18_2
# BB#1:                                 # %entry
	movq	%rax, %rsi
.LBB18_2:                               # %entry
	jmp	Perl_gv_fullname4  # TAILCALL
.Ltmp206:
	.size	Perl_gv_efullname4, .Ltmp206-Perl_gv_efullname4
	.cfi_endproc

	.globl	Perl_gv_efullname3
	.align	16, 0x90
	.type	Perl_gv_efullname3,@function
Perl_gv_efullname3:                     # @Perl_gv_efullname3
	.cfi_startproc
# BB#0:                                 # %entry
	movq	(%rsi), %rax
	movq	56(%rax), %rax
	movq	48(%rax), %rax
	testq	%rax, %rax
	je	.LBB19_2
# BB#1:                                 # %entry
	movq	%rax, %rsi
.LBB19_2:                               # %entry
	movq	$1, %rcx
	jmp	Perl_gv_fullname4  # TAILCALL
.Ltmp207:
	.size	Perl_gv_efullname3, .Ltmp207-Perl_gv_efullname3
	.cfi_endproc

	.globl	Perl_gv_fullname
	.align	16, 0x90
	.type	Perl_gv_fullname,@function
Perl_gv_fullname:                       # @Perl_gv_fullname
	.cfi_startproc
# BB#0:                                 # %entry
	cmpq	%rdi, %rsi
	je	.LBB20_1
# BB#2:                                 # %entry
	movabsq	$.L.str3, %rdx
	movq	$1, %rcx
	jmp	Perl_gv_fullname4  # TAILCALL
.LBB20_1:
	movabsq	$.L.str57, %rdx
	movq	$1, %rcx
	jmp	Perl_gv_fullname4  # TAILCALL
.Ltmp208:
	.size	Perl_gv_fullname, .Ltmp208-Perl_gv_fullname
	.cfi_endproc

	.globl	Perl_gv_efullname
	.align	16, 0x90
	.type	Perl_gv_efullname,@function
Perl_gv_efullname:                      # @Perl_gv_efullname
	.cfi_startproc
# BB#0:                                 # %entry
	cmpq	%rdi, %rsi
	je	.LBB21_1
# BB#2:                                 # %entry
	movabsq	$.L.str3, %rdx
	jmp	.LBB21_3
.LBB21_1:
	movabsq	$.L.str57, %rdx
.LBB21_3:                               # %entry
	movq	(%rsi), %rax
	movq	56(%rax), %rax
	movq	48(%rax), %rax
	testq	%rax, %rax
	je	.LBB21_5
# BB#4:                                 # %entry
	movq	%rax, %rsi
.LBB21_5:                               # %entry
	movq	$1, %rcx
	jmp	Perl_gv_fullname4  # TAILCALL
.Ltmp209:
	.size	Perl_gv_efullname, .Ltmp209-Perl_gv_efullname
	.cfi_endproc

	.globl	Perl_gv_check
	.align	16, 0x90
	.type	Perl_gv_check,@function
Perl_gv_check:                          # @Perl_gv_check
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
	movq	%rdi, %r13
	movq	(%r13), %rcx
	movq	(%rcx), %rax
	testq	%rax, %rax
	je	.LBB22_25
# BB#1:                                 # %for.cond.preheader
	cmpq	$0, 16(%rcx)
	js	.LBB22_25
# BB#2:
	xorq	%r12, %r12
	jmp	.LBB22_3
	.align	16, 0x90
.LBB22_24:                              # %for.inc86.for.body_crit_edge
                                        #   in Loop: Header=BB22_3 Depth=1
	movq	(%rax), %rax
	incq	%r12
.LBB22_3:                               # %for.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB22_4 Depth 2
	movq	(%rax,%r12,8), %r15
	jmp	.LBB22_4
	.align	16, 0x90
.LBB22_22:                              # %for.inc
                                        #   in Loop: Header=BB22_4 Depth=2
	movq	(%r15), %r15
.LBB22_4:                               # %for.body
                                        #   Parent Loop BB22_3 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	testq	%r15, %r15
	je	.LBB22_23
# BB#5:                                 # %for.body7
                                        #   in Loop: Header=BB22_4 Depth=2
	movq	8(%r15), %rax
	movq	8(%rax), %rcx
	movzbl	15(%rcx,%rax), %ecx
	cmpq	$58, %rcx
	jne	.LBB22_12
# BB#6:                                 # %land.lhs.true
                                        #   in Loop: Header=BB22_4 Depth=2
	movq	16(%r15), %rcx
	testq	%rcx, %rcx
	je	.LBB22_12
# BB#7:                                 # %land.lhs.true13
                                        #   in Loop: Header=BB22_4 Depth=2
	movzbl	16(%rcx), %edx
	cmpq	$13, %rdx
	jne	.LBB22_12
# BB#8:                                 # %land.lhs.true16
                                        #   in Loop: Header=BB22_4 Depth=2
	movq	(%rcx), %rcx
	movq	56(%rcx), %rcx
	movq	40(%rcx), %rdi
	testq	%rdi, %rdi
	jne	.LBB22_9
	.align	16, 0x90
.LBB22_12:                              # %if.else
                                        #   in Loop: Header=BB22_4 Depth=2
	movzbl	16(%rax), %eax
	leaq	-65(%rax), %rcx
	andq	$255, %rcx
	cmpq	$26, %rcx
	jb	.LBB22_14
# BB#13:                                # %if.else
                                        #   in Loop: Header=BB22_4 Depth=2
	addq	$-97, %rax
	andq	$255, %rax
	cmpq	$25, %rax
	ja	.LBB22_22
.LBB22_14:                              # %if.then52
                                        #   in Loop: Header=BB22_4 Depth=2
	movq	16(%r15), %r14
	movzbl	16(%r14), %eax
	cmpq	$13, %rax
	jne	.LBB22_22
# BB#15:                                # %lor.lhs.false58
                                        #   in Loop: Header=BB22_4 Depth=2
	movq	(%r14), %rax
	movzbl	88(%rax), %ecx
	testq	$2, %rcx
	jne	.LBB22_22
# BB#16:                                # %if.end64
                                        #   in Loop: Header=BB22_4 Depth=2
	movq	56(%rax), %rax
	movq	88(%rax), %rbx
	testq	%rbx, %rbx
	je	.LBB22_21
# BB#17:                                # %land.lhs.true68
                                        #   in Loop: Header=BB22_4 Depth=2
	movzbl	(%rbx), %ecx
	cmpq	$47, %rcx
	jne	.LBB22_21
# BB#18:                                # %land.lhs.true72
                                        #   in Loop: Header=BB22_4 Depth=2
	movq	%rbx, %rdi
	movabsq	$.L.str60, %rsi
	callq	Perl_instr
	testq	%rax, %rax
	jne	.LBB22_22
# BB#19:                                # %lor.lhs.false74
                                        #   in Loop: Header=BB22_4 Depth=2
	movq	%rbx, %rdi
	movabsq	$.L.str61, %rsi
	callq	Perl_instr
	testq	%rax, %rax
	jne	.LBB22_22
# BB#20:                                # %lor.lhs.false74.if.end78_crit_edge
                                        #   in Loop: Header=BB22_4 Depth=2
	movq	(%r14), %rax
	movq	56(%rax), %rax
.LBB22_21:                              # %if.end78
                                        #   in Loop: Header=BB22_4 Depth=2
	movq	80(%rax), %rax
	movq	PL_curcop(%rip), %rcx
	movq	%rax, 80(%rcx)
	movq	%rbx, %rdi
	callq	Perl_gv_fetchfile
	movq	PL_curcop(%rip), %rcx
	movq	%rax, 56(%rcx)
	movq	(%r13), %rax
	movq	(%r14), %rcx
	movq	80(%rax), %rdx
	movq	64(%rcx), %rcx
	movq	$14, %rdi
	xorq	%rax, %rax
	movabsq	$.L.str62, %rsi
	callq	Perl_warner
	jmp	.LBB22_22
.LBB22_9:                               # %if.then19
                                        #   in Loop: Header=BB22_4 Depth=2
	movq	PL_defstash(%rip), %rax
	cmpq	%rax, %rdi
	je	.LBB22_22
# BB#10:                                # %if.then19
                                        #   in Loop: Header=BB22_4 Depth=2
	cmpq	%r13, %rdi
	je	.LBB22_22
# BB#11:                                # %if.then25
                                        #   in Loop: Header=BB22_4 Depth=2
	callq	Perl_gv_check
	movq	(%r15), %r15
	jmp	.LBB22_4
	.align	16, 0x90
.LBB22_23:                              # %for.inc86
                                        #   in Loop: Header=BB22_3 Depth=1
	movq	(%r13), %rax
	movq	16(%rax), %rcx
	cmpq	%rcx, %r12
	jl	.LBB22_24
.LBB22_25:                              # %for.end87
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp226:
	.size	Perl_gv_check, .Ltmp226-Perl_gv_check
	.cfi_endproc

	.globl	Perl_newGVgen
	.align	16, 0x90
	.type	Perl_newGVgen,@function
Perl_newGVgen:                          # @Perl_newGVgen
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rax
.Ltmp228:
	.cfi_def_cfa_offset 16
	movq	PL_gensym(%rip), %rdx
	movq	%rdi, %rcx
	leaq	1(%rdx), %rax
	movq	%rax, PL_gensym(%rip)
	movabsq	$.L.str63, %rdi
	xorq	%rax, %rax
	movq	%rcx, %rsi
	callq	Perl_form
	movq	$1, %rsi
	movq	$13, %rdx
	movq	%rax, %rdi
	popq	%rax
	jmp	Perl_gv_fetchpv  # TAILCALL
.Ltmp229:
	.size	Perl_newGVgen, .Ltmp229-Perl_newGVgen
	.cfi_endproc

	.globl	Perl_gp_free
	.align	16, 0x90
	.type	Perl_gp_free,@function
Perl_gp_free:                           # @Perl_gp_free
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp235:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp236:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp237:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp238:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp239:
	.cfi_def_cfa_offset 48
.Ltmp240:
	.cfi_offset %rbx, -40
.Ltmp241:
	.cfi_offset %r12, -32
.Ltmp242:
	.cfi_offset %r14, -24
.Ltmp243:
	.cfi_offset %r15, -16
	movq	%rdi, %r14
	testq	%r14, %r14
	je	.LBB24_29
# BB#1:                                 # %lor.lhs.false
	movq	(%r14), %rax
	movq	56(%rax), %rbx
	testq	%rbx, %rbx
	je	.LBB24_29
# BB#2:                                 # %if.end
	movq	8(%rbx), %rax
	testq	%rax, %rax
	je	.LBB24_3
# BB#8:                                 # %if.end15
	cmpq	$0, 56(%rbx)
	je	.LBB24_10
# BB#9:                                 # %if.then17
	incq	PL_sub_generation(%rip)
	movq	8(%rbx), %rax
.LBB24_10:                              # %if.end18
	decq	%rax
	movq	%rax, 8(%rbx)
	je	.LBB24_13
# BB#11:                                # %if.then22
	movq	48(%rbx), %rax
	cmpq	%r14, %rax
	jne	.LBB24_29
# BB#12:                                # %if.then25
	movq	$0, 48(%rbx)
	jmp	.LBB24_29
.LBB24_3:                               # %if.then2
	movq	PL_curcop(%rip), %rax
	cmpq	$0, 88(%rax)
	je	.LBB24_7
# BB#4:                                 # %lor.lhs.false4
	movq	PL_curcop(%rip), %rax
	movq	88(%rax), %rax
	cmpq	$24, %rax
	je	.LBB24_7
# BB#5:                                 # %lor.lhs.false7
	movq	PL_curcop(%rip), %rax
	movq	88(%rax), %rax
	cmpq	$48, %rax
	je	.LBB24_29
# BB#6:                                 # %land.lhs.true
	movq	PL_curcop(%rip), %rax
	movq	88(%rax), %rax
	movq	(%rax), %rax
	movq	(%rax), %rax
	movzbl	6(%rax), %eax
	testq	$1, %rax
	je	.LBB24_29
.LBB24_7:                               # %if.then13
	movabsq	$.L.str64, %rsi
	movq	$24, %rdi
	xorq	%rax, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	jmp	Perl_warner  # TAILCALL
.LBB24_13:                              # %if.end28
	movq	(%rbx), %rdi
	testq	%rdi, %rdi
	je	.LBB24_15
# BB#14:                                # %if.then30
	callq	Perl_sv_free
.LBB24_15:                              # %if.end32
	movq	32(%rbx), %rdi
	testq	%rdi, %rdi
	je	.LBB24_17
# BB#16:                                # %if.then34
	callq	Perl_sv_free
.LBB24_17:                              # %if.end36
	movq	40(%rbx), %rdi
	testq	%rdi, %rdi
	je	.LBB24_22
# BB#18:                                # %if.then38
	movq	PL_stashcache(%rip), %r15
	testq	%r15, %r15
	je	.LBB24_21
# BB#19:                                # %land.lhs.true40
	movq	(%rdi), %rax
	movq	80(%rax), %r12
	testq	%r12, %r12
	je	.LBB24_21
# BB#20:                                # %if.then44
	movq	%r12, %rdi
	callq	strlen
	movq	$2, %rcx
	movq	%r15, %rdi
	movq	%r12, %rsi
	movq	%rax, %rdx
	callq	Perl_hv_delete
	movq	40(%rbx), %rdi
.LBB24_21:                              # %if.end52
	callq	Perl_sv_free
.LBB24_22:                              # %if.end54
	movq	16(%rbx), %rdi
	testq	%rdi, %rdi
	je	.LBB24_24
# BB#23:                                # %if.then56
	callq	Perl_sv_free
.LBB24_24:                              # %if.end58
	movq	56(%rbx), %rdi
	testq	%rdi, %rdi
	je	.LBB24_26
# BB#25:                                # %if.then61
	callq	Perl_sv_free
.LBB24_26:                              # %if.end63
	movq	24(%rbx), %rdi
	testq	%rdi, %rdi
	je	.LBB24_28
# BB#27:                                # %if.then65
	callq	Perl_sv_free
.LBB24_28:                              # %if.end67
	movq	%rbx, %rdi
	callq	Perl_safesysfree
	movq	(%r14), %rax
	movq	$0, 56(%rax)
.LBB24_29:                              # %return
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.Ltmp244:
	.size	Perl_gp_free, .Ltmp244-Perl_gp_free
	.cfi_endproc

	.globl	Perl_magic_freeovrld
	.align	16, 0x90
	.type	Perl_magic_freeovrld,@function
Perl_magic_freeovrld:                   # @Perl_magic_freeovrld
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp248:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp249:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp250:
	.cfi_def_cfa_offset 32
.Ltmp251:
	.cfi_offset %rbx, -24
.Ltmp252:
	.cfi_offset %r14, -16
	movq	32(%rsi), %r14
	testq	%r14, %r14
	je	.LBB25_6
# BB#1:                                 # %land.lhs.true
	movzbl	16(%r14), %eax
	testq	$1, %rax
	je	.LBB25_6
# BB#2:
	movq	$4, %rbx
	.align	16, 0x90
.LBB25_3:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	(%r14,%rbx,8), %rdi
	testq	%rdi, %rdi
	je	.LBB25_5
# BB#4:                                 # %if.then3
                                        #   in Loop: Header=BB25_3 Depth=1
	callq	Perl_sv_free
	movq	$0, (%r14,%rbx,8)
.LBB25_5:                               # %for.inc
                                        #   in Loop: Header=BB25_3 Depth=1
	incq	%rbx
	cmpq	$69, %rbx
	jne	.LBB25_3
.LBB25_6:                               # %if.end7
	xorq	%rax, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp253:
	.size	Perl_magic_freeovrld, .Ltmp253-Perl_magic_freeovrld
	.cfi_endproc

	.globl	Perl_Gv_AMupdate
	.align	16, 0x90
	.type	Perl_Gv_AMupdate,@function
Perl_Gv_AMupdate:                       # @Perl_Gv_AMupdate
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp260:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp261:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp262:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp263:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp264:
	.cfi_def_cfa_offset 48
	subq	$608, %rsp              # imm = 0x260
.Ltmp265:
	.cfi_def_cfa_offset 656
.Ltmp266:
	.cfi_offset %rbx, -48
.Ltmp267:
	.cfi_offset %r12, -40
.Ltmp268:
	.cfi_offset %r13, -32
.Ltmp269:
	.cfi_offset %r14, -24
.Ltmp270:
	.cfi_offset %r15, -16
	movq	$99, %rsi
	movq	%rdi, %rbx
	callq	Perl_mg_find
	testq	%rax, %rax
	je	.LBB26_1
# BB#11:                                # %land.lhs.true
	movq	32(%rax), %rax
	movq	8(%rax), %rcx
	movq	PL_amagic_generation(%rip), %rdx
	leaq	48(%rsp), %r14
	cmpq	%rdx, %rcx
	jne	.LBB26_2
# BB#12:                                # %land.lhs.true2
	movq	(%rax), %rcx
	movq	PL_sub_generation(%rip), %rdx
	cmpq	%rdx, %rcx
	jne	.LBB26_2
# BB#13:                                # %if.then
	movq	16(%rax), %rbx
	andq	$2, %rbx
	jmp	.LBB26_60
.LBB26_1:                               # %cond.end
	leaq	48(%rsp), %r14
.LBB26_2:                               # %if.end
	movq	$99, %rsi
	movq	%rbx, 40(%rsp)          # 8-byte Spill
	movq	%rbx, %rdi
	callq	Perl_sv_unmagic
	xorq	%rsi, %rsi
	movq	$560, %rdx              # imm = 0x230
	movq	%r14, %rdi
	callq	memset
	movq	PL_amagic_generation(%rip), %rax
	movq	%rax, 56(%rsp)
	movq	PL_sub_generation(%rip), %rax
	movq	PL_AMG_names(%rip), %rsi
	movq	%rax, 48(%rsp)
	movq	$2, 600(%rsp)
	movq	$0, 64(%rsp)
	movq	$2, %rdx
	movq	$-1, %rcx
	movq	%rbx, %rdi
	callq	Perl_gv_fetchmeth
	testq	%rax, %rax
	je	.LBB26_21
# BB#3:                                 # %if.else
	movq	(%rax), %rax
	movq	56(%rax), %rax
	movq	(%rax), %rbx
	testq	%rbx, %rbx
	je	.LBB26_9
# BB#4:                                 # %cond.false16
	movq	16(%rbx), %rax
	testq	$262144, %rax           # imm = 0x40000
	je	.LBB26_14
# BB#5:                                 # %cond.true19
	movq	(%rbx), %rax
	testq	%rax, %rax
	movq	%rax, PL_Xpv(%rip)
	je	.LBB26_9
# BB#6:                                 # %land.lhs.true22
	movq	8(%rax), %rcx
	cmpq	$1, %rcx
	ja	.LBB26_19
# BB#7:                                 # %lor.lhs.false
	testq	%rcx, %rcx
	je	.LBB26_9
# BB#8:                                 # %land.lhs.true27
	movq	(%rax), %rax
	movzbl	(%rax), %eax
	cmpq	$48, %rax
	jne	.LBB26_19
	jmp	.LBB26_9
.LBB26_21:                              # %for.cond66.preheader
	movq	%r14, 16(%rsp)          # 8-byte Spill
	leaq	80(%rsp), %rdi
	xorq	%rsi, %rsi
	movq	$512, %rdx              # imm = 0x200
	callq	memset
	movq	$65, %r12
	jmp	.LBB26_22
.LBB26_14:                              # %cond.false33
	testq	$65536, %rax            # imm = 0x10000
	je	.LBB26_16
# BB#15:                                # %cond.true37
	movq	(%rbx), %rax
	cmpq	$0, 24(%rax)
	jne	.LBB26_19
	jmp	.LBB26_9
.LBB26_16:                              # %cond.false41
	testq	$131072, %rax           # imm = 0x20000
	je	.LBB26_18
# BB#17:                                # %cond.true45
	movq	(%rbx), %rax
	movq	32(%rax), %rdi
	xorq	%rsi, %rsi
	callq	__nedf2
	testq	%rax, %rax
	jne	.LBB26_19
	jmp	.LBB26_9
.LBB26_18:                              # %cond.false49
	movq	%rbx, %rdi
	callq	Perl_sv_2bool
	testq	$255, %rax
	je	.LBB26_9
.LBB26_19:                              # %if.then53
	movq	%r14, 16(%rsp)          # 8-byte Spill
	movq	$3, 600(%rsp)
	movq	$1, %r12
	jmp	.LBB26_22
.LBB26_9:                               # %if.else55
	movzbl	18(%rbx), %eax
	movq	$1, %r12
	testq	$1807, %rax             # imm = 0x70F
	je	.LBB26_10
# BB#20:                                # %if.then59
	movq	%r14, 16(%rsp)          # 8-byte Spill
	movq	$1, 600(%rsp)
	jmp	.LBB26_22
.LBB26_10:
	movq	%r14, 16(%rsp)          # 8-byte Spill
.LBB26_22:                              # %for.body69.preheader
	xorq	%r15, %r15
	movq	$0, 32(%rsp)            # 8-byte Folded Spill
	.align	16, 0x90
.LBB26_23:                              # %for.body69
                                        # =>This Inner Loop Header: Depth=1
	movq	PL_AMG_names(,%r12,8), %r13
	movq	$32, %rcx
	movq	%r12, %rbx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	movq	%r13, %rdi
	callq	strlen
	cmpq	$65, %rbx
	jl	.LBB26_25
# BB#24:                                # %if.then83
                                        #   in Loop: Header=BB26_23 Depth=1
	xorq	%rcx, %rcx
	movq	40(%rsp), %rdi          # 8-byte Reload
	movq	%r13, %rsi
	movq	%rax, %rdx
	callq	Perl_gv_fetchmeth_autoload
	jmp	.LBB26_26
	.align	16, 0x90
.LBB26_25:                              # %if.else85
                                        #   in Loop: Header=BB26_23 Depth=1
	movq	$-1, %rcx
	movq	40(%rsp), %rdi          # 8-byte Reload
	movq	%r13, %rsi
	movq	%rax, %rdx
	callq	Perl_gv_fetchmeth
.LBB26_26:                              # %if.end87
                                        #   in Loop: Header=BB26_23 Depth=1
	movq	%rax, %rbx
	testq	%rbx, %rbx
	jne	.LBB26_28
# BB#27:                                # %if.end158.thread
                                        #   in Loop: Header=BB26_23 Depth=1
	movq	$0, PL_Sv(%rip)
	xorq	%r14, %r14
	jmp	.LBB26_52
	.align	16, 0x90
.LBB26_28:                              # %land.lhs.true89
                                        #   in Loop: Header=BB26_23 Depth=1
	movq	(%rbx), %rax
	movq	56(%rax), %rcx
	movq	56(%rcx), %r14
	testq	%r14, %r14
	jne	.LBB26_29
# BB#49:                                # %if.then156
                                        #   in Loop: Header=BB26_23 Depth=1
	movq	%rbx, %r14
	jmp	.LBB26_50
	.align	16, 0x90
.LBB26_29:                              # %if.then93
                                        #   in Loop: Header=BB26_23 Depth=1
	movq	(%r14), %rax
	movq	96(%rax), %rax
	movq	(%rax), %r15
	movq	72(%r15), %rax
	cmpq	$3, %rax
	jne	.LBB26_46
# BB#30:                                # %land.lhs.true98
                                        #   in Loop: Header=BB26_23 Depth=1
	movq	%rcx, 24(%rsp)          # 8-byte Spill
	movq	64(%r15), %rdi
	movabsq	$.L.str65, %rsi
	callq	strcmp
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB26_46
# BB#31:                                # %land.lhs.true104
                                        #   in Loop: Header=BB26_23 Depth=1
	movq	80(%r15), %rax
	movq	(%rax), %rax
	movq	80(%rax), %rdi
	movabsq	$.L.str66, %rsi
	movq	%rcx, %r15
	callq	strcmp
	testq	%r15, %rax
	jne	.LBB26_46
# BB#32:                                # %if.then111
                                        #   in Loop: Header=BB26_23 Depth=1
	movq	24(%rsp), %rcx          # 8-byte Reload
	movq	(%rcx), %rax
	movq	16(%rax), %rdx
	testq	$262144, %rdx           # imm = 0x40000
	je	.LBB26_33
# BB#34:                                # %lor.lhs.false118
                                        #   in Loop: Header=BB26_23 Depth=1
	movq	(%rax), %rax
	movq	(%rax), %rsi
	xorq	%rdx, %rdx
	movq	40(%rsp), %rdi          # 8-byte Reload
	callq	Perl_gv_fetchmethod_autoload
	testq	%rax, %rax
	je	.LBB26_35
# BB#45:                                # %if.end145
                                        #   in Loop: Header=BB26_23 Depth=1
	movq	(%rax), %rax
	movq	56(%rax), %rax
	movq	56(%rax), %r14
	.align	16, 0x90
.LBB26_46:                              # %if.end149
                                        #   in Loop: Header=BB26_23 Depth=1
	movq	$32, %rcx
	movq	%r12, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$65, %rax
	movq	$1, %rax
	jl	.LBB26_48
# BB#47:                                # %if.end149
                                        #   in Loop: Header=BB26_23 Depth=1
	movq	32(%rsp), %rax          # 8-byte Reload
.LBB26_48:                              # %if.end149
                                        #   in Loop: Header=BB26_23 Depth=1
	movq	%rax, 32(%rsp)          # 8-byte Spill
.LBB26_50:                              # %if.end158
                                        #   in Loop: Header=BB26_23 Depth=1
	testq	%r14, %r14
	movq	%r14, PL_Sv(%rip)
	movq	$1, %r15
	je	.LBB26_52
# BB#51:                                # %land.rhs
                                        #   in Loop: Header=BB26_23 Depth=1
	incq	8(%r14)
.LBB26_52:                              # %land.end
                                        #   in Loop: Header=BB26_23 Depth=1
	movq	%r14, 72(%rsp,%r12,8)
	incq	%r12
	movq	$32, %rcx
	movq	%r12, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$66, %rax
	jl	.LBB26_23
# BB#53:                                # %for.end167
	movq	64(%rsp), %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %r15
	je	.LBB26_58
# BB#54:                                # %if.then169
	movq	32(%rsp), %rbx          # 8-byte Reload
	testq	%rcx, %rbx
	movq	16(%rsp), %rcx          # 8-byte Reload
	je	.LBB26_55
# BB#56:                                # %if.then169
	movabsq	$1, %rdx
	jmp	.LBB26_57
.LBB26_58:                              # %if.end177
	andq	$-2, %rax
	movq	%rax, 64(%rsp)
	xorq	%rsi, %rsi
	movq	$99, %rdx
	movq	$24, %r8
	xorq	%rbx, %rbx
	movq	40(%rsp), %rdi          # 8-byte Reload
	movq	16(%rsp), %rcx          # 8-byte Reload
	jmp	.LBB26_59
.LBB26_55:
	movabsq	$0, %rdx
.LBB26_57:                              # %if.then169
	addq	%rdx, %rdx
	orq	%rax, %rdx
	orq	$1, %rdx
	movq	%rdx, 64(%rsp)
	xorq	%rsi, %rsi
	movq	$99, %rdx
	movq	$560, %r8               # imm = 0x230
	movq	40(%rsp), %rdi          # 8-byte Reload
.LBB26_59:                              # %cleanup
	callq	Perl_sv_magic
.LBB26_60:                              # %cleanup
	movq	$56, %rcx
	shlq	%cl, %rbx
	movq	$56, %rcx
	sarq	%cl, %rbx
	movq	%rbx, %rax
	addq	$608, %rsp              # imm = 0x260
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB26_33:
	movq	%rcx, %rsi
	jmp	.LBB26_36
.LBB26_35:                              # %lor.lhs.false118.if.then126_crit_edge
	movq	(%rbx), %rax
	movq	56(%rax), %rcx
	movq	(%rcx), %rax
	movq	%rcx, %rsi
	movq	16(%rax), %rdx
.LBB26_36:                              # %if.then126
	movq	$32, %rcx
	shlq	%cl, %r12
	movq	$32, %rcx
	sarq	%cl, %r12
	cmpq	$64, %r12
	jg	.LBB26_38
# BB#37:                                # %if.then126
	incq	%r13
.LBB26_38:                              # %if.then126
	testq	$262144, %rdx           # imm = 0x40000
	jne	.LBB26_40
# BB#39:
	movabsq	$.L.str67, %rcx
	jmp	.LBB26_41
.LBB26_40:                              # %cond.true133
	movq	(%rax), %rax
	movq	(%rax), %rcx
.LBB26_41:                              # %cond.end137
	cmpq	$0, 64(%rsi)
	je	.LBB26_43
# BB#42:
	movabsq	$.L.str69, %rdx
	jmp	.LBB26_44
.LBB26_43:                              # %select.mid
	movabsq	$.L.str70, %rdx
.LBB26_44:                              # %select.end
	movq	40(%rsp), %rax          # 8-byte Reload
	movq	(%rax), %rax
	movq	80(%rax), %rbx
	movabsq	$.L.str68, %rsi
	xorq	%rax, %rax
	movq	%rsi, %rdi
	movq	%rdx, %rsi
	movq	%rcx, %rdx
	movq	%r13, %rcx
	movq	%rbx, %r8
	callq	Perl_croak
.Ltmp271:
	.size	Perl_Gv_AMupdate, .Ltmp271-Perl_Gv_AMupdate
	.cfi_endproc

	.globl	Perl_gv_handler
	.align	16, 0x90
	.type	Perl_gv_handler,@function
Perl_gv_handler:                        # @Perl_gv_handler
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp277:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp278:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp279:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp280:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp281:
	.cfi_def_cfa_offset 48
.Ltmp282:
	.cfi_offset %rbx, -40
.Ltmp283:
	.cfi_offset %r12, -32
.Ltmp284:
	.cfi_offset %r14, -24
.Ltmp285:
	.cfi_offset %r15, -16
	movq	%rsi, %r15
	movq	%rdi, %rbx
	xorq	%r14, %r14
	testq	%rbx, %rbx
	je	.LBB27_12
# BB#1:                                 # %lor.lhs.false
	movq	(%rbx), %rax
	cmpq	$0, 80(%rax)
	je	.LBB27_12
# BB#2:                                 # %if.end
	movq	$99, %rsi
	movq	%rbx, %rdi
	callq	Perl_mg_find
	testq	%rax, %rax
	jne	.LBB27_4
.LBB27_3:                               # %do_update
	movq	%rbx, %rdi
	callq	Perl_Gv_AMupdate
	movq	$99, %rsi
	movq	%rbx, %rdi
	callq	Perl_mg_find
.LBB27_4:                               # %if.end6
	movq	32(%rax), %rax
	movq	8(%rax), %rcx
	movq	PL_amagic_generation(%rip), %rdx
	cmpq	%rdx, %rcx
	jne	.LBB27_3
# BB#5:                                 # %lor.lhs.false7
	movq	(%rax), %rcx
	movq	PL_sub_generation(%rip), %rdx
	cmpq	%rdx, %rcx
	jne	.LBB27_3
# BB#6:                                 # %if.end10
	movzbl	16(%rax), %ecx
	testq	$1, %rcx
	je	.LBB27_12
# BB#7:                                 # %if.then12
	movq	24(%rax,%r15,8), %r12
	testq	%r12, %r12
	je	.LBB27_11
# BB#8:                                 # %land.lhs.true
	movzbl	16(%r12), %eax
	cmpq	$13, %rax
	jne	.LBB27_11
# BB#9:                                 # %if.then16
	movq	PL_AMG_names(,%r15,8), %rsi
	movq	$1, %rdx
	movq	%rbx, %rdi
	callq	Perl_gv_fetchmethod_autoload
	testq	%rax, %rax
	je	.LBB27_11
# BB#10:                                # %land.lhs.true20
	movq	(%rax), %rax
	movq	56(%rax), %rax
	movq	56(%rax), %r14
	testq	%r14, %r14
	jne	.LBB27_12
.LBB27_11:                              # %if.end28
	movq	%r12, %r14
.LBB27_12:                              # %return
	movq	%r14, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.Ltmp286:
	.size	Perl_gv_handler, .Ltmp286-Perl_gv_handler
	.cfi_endproc

	.globl	Perl_amagic_call
	.align	16, 0x90
	.type	Perl_amagic_call,@function
Perl_amagic_call:                       # @Perl_amagic_call
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp293:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp294:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp295:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp296:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp297:
	.cfi_def_cfa_offset 48
	subq	$144, %rsp
.Ltmp298:
	.cfi_def_cfa_offset 192
.Ltmp299:
	.cfi_offset %rbx, -48
.Ltmp300:
	.cfi_offset %r12, -40
.Ltmp301:
	.cfi_offset %r13, -32
.Ltmp302:
	.cfi_offset %r14, -24
.Ltmp303:
	.cfi_offset %r15, -16
	movq	%rcx, %r10
	movq	%rdx, %r9
	movq	%rdi, %rdx
	movq	%r10, %rax
	andq	$4, %rax
	movq	%rax, 72(%rsp)          # 8-byte Spill
	movq	$2, %rcx
	movq	%rax, %r15
	shrq	%cl, %r15
	testq	$2, %r10
	movq	$1, 64(%rsp)            # 8-byte Folded Spill
	jne	.LBB28_1
# BB#4:                                 # %land.lhs.true
	movzbl	19(%rdx), %eax
	testq	$16, %rax
	je	.LBB28_1
# BB#5:                                 # %land.lhs.true5
	movq	%r9, %r12
	movq	(%rdx), %rax
	movq	(%rax), %rax
	movq	(%rax), %rax
	movq	48(%rax), %rdi
	testq	%rdi, %rdi
	je	.LBB28_6
# BB#7:                                 # %land.lhs.true8
	movq	%r10, 24(%rsp)          # 8-byte Spill
	movq	%r15, %r13
	movq	%rsi, %r15
	movq	%rdx, %r14
	movq	$99, %rsi
	callq	Perl_mg_find
	testq	%rax, %rax
	je	.LBB28_8
# BB#9:                                 # %land.lhs.true10
	movq	32(%rax), %rdx
	movzbl	16(%rdx), %eax
	testq	$1, %rax
	movq	%r14, 80(%rsp)          # 8-byte Spill
	movq	%r12, %r9
	movq	%r15, %rsi
	je	.LBB28_10
# BB#12:                                # %cond.end
	movq	%r13, %r15
	leaq	(%r15,%r9), %r11
	movq	$32, %rcx
	movq	%r11, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	24(%rdx,%rax,8), %rcx
	testq	%rcx, %rcx
	je	.LBB28_14
# BB#13:
	xorq	%rbx, %rbx
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	movq	%rax, 48(%rsp)          # 8-byte Spill
	xorq	%r14, %r14
	xorq	%r13, %r13
	jmp	.LBB28_162
.LBB28_1:
	movq	%rdx, 80(%rsp)          # 8-byte Spill
	xorq	%rdi, %rdi
	xorq	%rbx, %rbx
	xorq	%r11, %r11
.LBB28_2:                               # %if.else207
	testq	$1, %r10
	jne	.LBB28_3
# BB#106:                               # %land.lhs.true210
	movzbl	19(%rsi), %eax
	testq	$16, %rax
	je	.LBB28_3
# BB#107:                               # %land.lhs.true214
	movq	%rdi, %r13
	movq	(%rsi), %rax
	movq	(%rax), %rax
	movq	(%rax), %rax
	movq	48(%rax), %rdi
	testq	%rdi, %rdi
	je	.LBB28_108
# BB#109:                               # %land.lhs.true220
	movq	%r11, 48(%rsp)          # 8-byte Spill
	movq	%r10, 24(%rsp)          # 8-byte Spill
	movq	%r15, 56(%rsp)          # 8-byte Spill
	movq	%rsi, %r15
	movq	%r9, %r12
	movq	$99, %rsi
	callq	Perl_mg_find
	testq	%rax, %rax
	je	.LBB28_110
# BB#111:                               # %land.lhs.true223
	movq	32(%rax), %r8
	movzbl	16(%r8), %ecx
	xorq	%rax, %rax
	testq	$1, %rcx
	movq	%rbx, %rcx
	movq	%r12, %r9
	movq	%r15, %rsi
	movq	56(%rsp), %r15          # 8-byte Reload
	movq	24(%rsp), %r10          # 8-byte Reload
	movq	%rcx, %rdx
	movq	48(%rsp), %r11          # 8-byte Reload
	movq	%r13, %rdi
	je	.LBB28_114
# BB#112:                               # %cond.end233
	movq	$32, %rcx
	movq	%r9, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	24(%r8,%rax,8), %rcx
	testq	%rcx, %rcx
	je	.LBB28_113
# BB#263:
	movq	$1, 48(%rsp)            # 8-byte Folded Spill
	xorq	%rbx, %rbx
.LBB28_19:                              # %if.end370
	xorq	%r14, %r14
	xorq	%r13, %r13
	movq	%r9, %r11
	jmp	.LBB28_162
.LBB28_3:
	movq	%rbx, %rdx
	movq	%rdi, %rax
.LBB28_114:                             # %if.else241
	testq	%rdi, %rdi
	je	.LBB28_121
# BB#115:                               # %land.lhs.true243
	movq	552(%rbx), %rcx
	cmpq	$2, %rcx
	jl	.LBB28_117
# BB#116:                               # %land.lhs.true243
	movq	%rdi, %rax
.LBB28_117:                             # %land.lhs.true243
	cmpq	$1, %rcx
	jg	.LBB28_118
.LBB28_121:                             # %lor.lhs.false250
	xorq	%r14, %r14
	testq	%rax, %rax
	je	.LBB28_122
# BB#123:                               # %land.lhs.true252
	movq	552(%rdx), %rcx
	movq	%rdx, 56(%rsp)          # 8-byte Spill
	xorq	%r14, %r14
	cmpq	$2, %rcx
	movq	$1, %rcx
	jge	.LBB28_119
# BB#124:
	xorq	%r13, %r13
	jmp	.LBB28_135
.LBB28_118:
	movq	%rdx, 56(%rsp)          # 8-byte Spill
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	movq	%rdi, %rax
.LBB28_119:                             # %land.lhs.true257
	xorq	%r14, %r14
	testq	$8, %r10
	je	.LBB28_125
# BB#120:
	xorq	%r13, %r13
	jmp	.LBB28_135
.LBB28_122:
	movq	%rdx, 56(%rsp)          # 8-byte Spill
	xorq	%r13, %r13
	xorq	%rax, %rax
	jmp	.LBB28_135
.LBB28_125:                             # %if.then260
	movq	%rcx, 48(%rsp)          # 8-byte Spill
	xorq	%r13, %r13
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	movq	%r9, %rcx
	andq	%rdx, %rcx
	cmpq	$55, %rcx
	jbe	.LBB28_127
# BB#126:
	xorq	%r14, %r14
	movq	%rdx, %r11
	jmp	.LBB28_135
.LBB28_127:                             # %if.then260
	movq	%rdi, 16(%rsp)          # 8-byte Spill
	movq	$34, %r11
	movq	$1, %r14
	movq	$1, %rbx
	shlq	%cl, %rbx
	movabsq	$16911433728, %rdi      # imm = 0x3F0000000
	testq	%rdi, %rbx
	jne	.LBB28_131
# BB#128:                               # %if.then260
	movq	$1, %rbx
	shlq	%cl, %rbx
	movabsq	$4329327034368, %rdi    # imm = 0x3F000000000
	testq	%rdi, %rbx
	je	.LBB28_129
# BB#130:                               # %sw.bb275
	movq	$35, %r11
.LBB28_131:                             # %if.end282
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%r11, %rcx
	movq	(%rax,%rcx,8), %rcx
	xorq	%r13, %r13
	testq	%rcx, %rcx
	movq	16(%rsp), %rdi          # 8-byte Reload
	je	.LBB28_135
# BB#132:
	movq	$1, %rbx
	xorq	%r14, %r14
	jmp	.LBB28_162
.LBB28_6:
	movq	%rdx, 80(%rsp)          # 8-byte Spill
	xorq	%rdi, %rdi
	xorq	%rbx, %rbx
	xorq	%r11, %r11
	movq	%r12, %r9
	jmp	.LBB28_2
.LBB28_108:
	movq	%rbx, %rdx
	movq	%r13, %rdi
	movq	%rdi, %rax
	jmp	.LBB28_114
.LBB28_8:
	xorq	%rdi, %rdi
	xorq	%rbx, %rbx
	xorq	%r11, %r11
	movq	%r14, 80(%rsp)          # 8-byte Spill
	movq	%r12, %r9
	movq	%r15, %rsi
	jmp	.LBB28_11
.LBB28_110:
	movq	%rbx, %rcx
	movq	%r13, %rdi
	movq	%rdi, %rax
	movq	%r12, %r9
	movq	%r15, %rsi
	movq	56(%rsp), %r15          # 8-byte Reload
	movq	24(%rsp), %r10          # 8-byte Reload
	movq	%rcx, %rdx
	movq	48(%rsp), %r11          # 8-byte Reload
	jmp	.LBB28_114
.LBB28_10:
	xorq	%rdi, %rdi
	xorq	%rbx, %rbx
	xorq	%r11, %r11
.LBB28_11:                              # %if.else207
	movq	%r13, %r15
	movq	24(%rsp), %r10          # 8-byte Reload
	jmp	.LBB28_2
.LBB28_129:                             # %if.then260
	movq	$1, %rdi
	shlq	%cl, %rdi
	movabsq	$67553994410557440, %rcx # imm = 0xF0000000000000
	testq	%rcx, %rdi
	movabsq	$0, %r14
	movq	%rdx, %r11
	movq	16(%rsp), %rdi          # 8-byte Reload
	jne	.LBB28_261
	jmp	.LBB28_135
.LBB28_14:                              # %lor.lhs.false
	movq	552(%rdx), %rax
	leaq	24(%rdx), %rdi
	movq	%rdx, 56(%rsp)          # 8-byte Spill
	movabsq	$4294967295, %r8        # imm = 0xFFFFFFFF
	movq	72(%rsp), %rcx          # 8-byte Reload
	testq	%r8, %rcx
	movq	24(%rsp), %r10          # 8-byte Reload
	je	.LBB28_20
# BB#15:                                # %land.lhs.true20
	cmpq	$2, %rax
	jl	.LBB28_16
# BB#17:                                # %land.lhs.true21
	movq	$32, %rcx
	movq	%r9, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	(%rdi,%rdx,8), %rcx
	xorq	%rbx, %rbx
	testq	%rcx, %rcx
	movq	%r9, %r11
	je	.LBB28_20
# BB#18:
	movq	%r8, 48(%rsp)           # 8-byte Spill
	jmp	.LBB28_19
.LBB28_113:
	movq	%r8, %rax
	addq	$24, %rax
	movq	%r9, %r11
	movq	%r8, %rdx
	jmp	.LBB28_114
.LBB28_20:                              # %land.lhs.true26
	cmpq	$2, %rax
	jl	.LBB28_16
# BB#21:                                # %land.lhs.true26
	movq	$0, 64(%rsp)            # 8-byte Folded Spill
	movq	%r10, %rax
	andq	$8, %rax
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	testq	%rdx, %rax
	je	.LBB28_22
# BB#23:                                # %if.then32
	xorq	%r14, %r14
	movq	$0, 64(%rsp)            # 8-byte Folded Spill
	movq	$32, %rcx
	movq	%r9, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$41, %rax
	jg	.LBB28_26
# BB#24:                                # %if.then32
	leaq	-1(%r9), %rcx
	movabsq	$4294967295, %r12       # imm = 0xFFFFFFFF
	andq	%r12, %rcx
	cmpq	$4, %rcx
	ja	.LBB28_27
# BB#25:                                # %if.then32
	xorq	%r13, %r13
	movq	%rdi, %rax
	jmpq	*.LJTI28_1(,%rcx,8)
.LBB28_59:                              # %sw.bb101
	movq	56(%rsp), %rax          # 8-byte Reload
	cmpq	$0, 248(%rax)
	movq	$28, %rbx
	jne	.LBB28_62
# BB#60:                                # %lor.lhs.false104
	cmpq	$0, 296(%rax)
	movq	$34, %rbx
	je	.LBB28_61
.LBB28_62:                              # %land.lhs.true107
	movq	%rdi, 16(%rsp)          # 8-byte Spill
	movq	%rsi, 40(%rsp)          # 8-byte Spill
	movq	%r9, 32(%rsp)           # 8-byte Spill
	movq	480(%rax), %r13
	testq	%r13, %r13
	movq	$57, 48(%rsp)           # 8-byte Folded Spill
	jne	.LBB28_65
# BB#63:                                # %lor.lhs.false110
	movq	%r10, %rdx
	movq	%r15, %rcx
	movq	56(%rsp), %rax          # 8-byte Reload
	movq	88(%rax), %rax
	testq	%rax, %rax
	movq	%rax, %r13
	movq	$8, 48(%rsp)            # 8-byte Folded Spill
	je	.LBB28_262
# BB#64:
	movq	%rcx, %r15
	movq	%rdx, %r10
.LBB28_65:                              # %if.then113
	movq	%r10, 24(%rsp)          # 8-byte Spill
	xorq	%rdi, %rdi
	callq	Perl_newSViv
	movq	%rax, %rdi
	callq	Perl_sv_2mortal
	movq	%rax, 64(%rsp)          # 8-byte Spill
	andq	%r12, %rbx
	cmpq	$28, %rbx
	jne	.LBB28_81
# BB#66:                                # %if.then117
	movq	$28, %rdx
	movq	$1, %rcx
	movq	80(%rsp), %rdi          # 8-byte Reload
	movq	64(%rsp), %rsi          # 8-byte Reload
	callq	Perl_amagic_call
	testq	%rax, %rax
	je	.LBB28_67
# BB#68:                                # %cond.false121
	movq	16(%rax), %rcx
	testq	$262144, %rcx           # imm = 0x40000
	je	.LBB28_74
# BB#69:                                # %cond.true125
	movq	(%rax), %rcx
	xorq	%rax, %rax
	testq	%rcx, %rcx
	movq	%rcx, PL_Xpv(%rip)
	je	.LBB28_87
# BB#70:                                # %land.rhs128
	movq	8(%rcx), %rdx
	cmpq	$1, %rdx
	movq	$1, %rax
	ja	.LBB28_87
# BB#71:                                # %lor.rhs130
	testq	%rdx, %rdx
	movabsq	$0, %rax
	je	.LBB28_87
# BB#72:                                # %land.rhs133
	movq	(%rcx), %rax
	movzbl	(%rax), %eax
	cmpq	$48, %rax
	jne	.LBB28_86
# BB#73:
	movabsq	$0, %rax
	jmp	.LBB28_87
.LBB28_16:
	movq	$0, 64(%rsp)            # 8-byte Folded Spill
	movq	56(%rsp), %rbx          # 8-byte Reload
	jmp	.LBB28_2
.LBB28_22:
	movq	56(%rsp), %rbx          # 8-byte Reload
	jmp	.LBB28_2
.LBB28_26:                              # %if.then32
	leaq	-42(%r9), %rcx
	andq	%rdx, %rcx
	cmpq	$22, %rcx
	ja	.LBB28_27
# BB#28:                                # %if.then32
	movq	%rdx, 48(%rsp)          # 8-byte Spill
	xorq	%r13, %r13
	movq	%rdi, %rax
	jmpq	*.LJTI28_0(,%rcx,8)
.LBB28_100:                             # %sw.bb203
	movq	80(%rsp), %r14          # 8-byte Reload
	jmp	.LBB28_261
.LBB28_39:                              # %sw.bb49
	movq	56(%rsp), %rax          # 8-byte Reload
	movq	64(%rax), %rcx
	testq	%rcx, %rcx
	jne	.LBB28_40
# BB#42:                                # %lor.rhs
	movq	%r9, 32(%rsp)           # 8-byte Spill
	movq	56(%rax), %rcx
	movq	$4, %r11
	jmp	.LBB28_43
.LBB28_46:                              # %sw.bb62
	movq	56(%rsp), %rax          # 8-byte Reload
	movq	64(%rax), %rcx
	testq	%rcx, %rcx
	je	.LBB28_47
.LBB28_40:
	movq	$5, %r11
	jmp	.LBB28_41
.LBB28_44:                              # %sw.bb54
	movq	56(%rsp), %rax          # 8-byte Reload
	movq	56(%rax), %rcx
	testq	%rcx, %rcx
	je	.LBB28_47
# BB#45:
	movq	$4, %r11
.LBB28_41:                              # %if.end370
	xorq	%rbx, %rbx
	xorq	%r14, %r14
	xorq	%r13, %r13
	movq	$0, 48(%rsp)            # 8-byte Folded Spill
	jmp	.LBB28_162
.LBB28_47:                              # %lor.rhs57
	movq	%r9, 32(%rsp)           # 8-byte Spill
	movq	40(%rax), %rcx
	movq	$2, %r11
.LBB28_43:                              # %sw.epilog
	xorq	%r14, %r14
	xorq	%rax, %rax
	jmp	.LBB28_102
.LBB28_48:                              # %sw.bb70
	movq	%r9, 32(%rsp)           # 8-byte Spill
	movq	56(%rsp), %rcx          # 8-byte Reload
	movq	40(%rcx), %rax
	movq	%rcx, %r13
	xorq	%r14, %r14
	testq	%rax, %rax
	je	.LBB28_51
# BB#49:
	xorq	%r13, %r13
	movq	$0, 48(%rsp)            # 8-byte Folded Spill
	movq	32(%rsp), %r9           # 8-byte Reload
	movq	$2, %r11
	jmp	.LBB28_50
.LBB28_29:                              # %sw.bb
	movq	56(%rsp), %rax          # 8-byte Reload
	movq	80(%rax), %rcx
	movq	%rax, %rdx
	testq	%rcx, %rcx
	movabsq	$PL_sv_yes, %rax
	je	.LBB28_32
# BB#30:
	movq	$1, %r14
	xorq	%rbx, %rbx
	movq	$7, %r11
	jmp	.LBB28_31
.LBB28_34:                              # %sw.bb40
	movq	56(%rsp), %rax          # 8-byte Reload
	movq	96(%rax), %rcx
	movq	%rax, %rdx
	testq	%rcx, %rcx
	movabsq	$PL_sv_yes, %rax
	je	.LBB28_36
# BB#35:
	movq	$1, %r14
	xorq	%rbx, %rbx
	movq	$9, %r11
.LBB28_31:                              # %if.end370
	xorq	%r13, %r13
	movq	$1, 72(%rsp)            # 8-byte Folded Spill
	movq	%rax, %rsi
	jmp	.LBB28_162
.LBB28_53:                              # %sw.bb81
	movq	80(%rsp), %rax          # 8-byte Reload
	movq	(%rax), %rax
	movq	(%rax), %rbx
	movq	16(%rbx), %rax
	xorq	%r14, %r14
	testq	$524288, %rax           # imm = 0x80000
	jne	.LBB28_54
# BB#55:                                # %sw.bb81
	andq	$248, %rax
	cmpq	$7, %rax
	movabsq	$0, %rcx
	movq	%rdi, %rax
	movq	%rcx, %r13
	ja	.LBB28_138
# BB#56:                                # %if.then91
	movq	%rbx, %rdi
	callq	Perl_newSVsv
	movq	%rax, %r14
	orq	$4096, 16(%r14)         # imm = 0x1000
	movq	(%rbx), %rax
	movq	48(%rax), %rax
	testq	%rax, %rax
	movq	%rax, PL_Sv(%rip)
	je	.LBB28_58
# BB#57:                                # %land.rhs
	incq	8(%rax)
.LBB28_58:                              # %land.end
	movq	(%r14), %rcx
	movq	%rax, 48(%rcx)
	jmp	.LBB28_261
.LBB28_97:                              # %sw.bb195
	movq	%r9, 32(%rsp)           # 8-byte Spill
	movq	80(%rsp), %r14          # 8-byte Reload
	movq	56(%rsp), %rax          # 8-byte Reload
	movq	88(%rax), %rbx
	testq	%rbx, %rbx
	je	.LBB28_98
# BB#99:                                # %if.then198
	xorq	%rdi, %rdi
	callq	Perl_newSViv
	movq	%rax, %rdi
	callq	Perl_sv_2mortal
	movq	%r14, %rsi
	movq	$8, %r11
	movq	$1, 48(%rsp)            # 8-byte Folded Spill
	movabsq	$0, %r14
	movabsq	$0, %r13
	movq	%rax, 80(%rsp)          # 8-byte Spill
	movq	32(%rsp), %r9           # 8-byte Reload
	movq	%rbx, %rcx
	xorq	%rbx, %rbx
	jmp	.LBB28_162
.LBB28_61:
	movq	$0, 64(%rsp)            # 8-byte Folded Spill
	xorq	%r14, %r14
.LBB28_27:
	xorq	%r13, %r13
	movq	%rdi, %rax
.LBB28_135:                             # %not_found
	leaq	-58(%r9), %rcx
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	andq	%rcx, %rdx
	cmpq	$5, %rdx
	jae	.LBB28_137
# BB#136:
	movq	80(%rsp), %r14          # 8-byte Reload
	jmp	.LBB28_261
.LBB28_137:                             # %sw.epilog288
	cmpq	$0, 64(%rsp)            # 8-byte Folded Reload
	jne	.LBB28_142
	jmp	.LBB28_138
.LBB28_262:
	movq	$0, 64(%rsp)            # 8-byte Folded Spill
	xorq	%r14, %r14
	xorq	%r13, %r13
	movq	16(%rsp), %rdi          # 8-byte Reload
	movq	%rdi, %rax
	movq	32(%rsp), %r9           # 8-byte Reload
	movq	40(%rsp), %rsi          # 8-byte Reload
	movq	%rcx, %r15
	movq	%rdx, %r10
	movq	$8, %r11
	jmp	.LBB28_135
.LBB28_81:                              # %if.else170
	movq	$34, %rdx
	movq	$1, %rcx
	movq	80(%rsp), %rdi          # 8-byte Reload
	movq	64(%rsp), %rsi          # 8-byte Reload
	callq	Perl_amagic_call
	movzbl	18(%rax), %ecx
	testq	$2, %rcx
	je	.LBB28_83
# BB#82:                                # %cond.true176
	movq	(%rax), %rax
	movq	32(%rax), %rax
	jmp	.LBB28_84
.LBB28_67:
	movq	80(%rsp), %r14          # 8-byte Reload
	jmp	.LBB28_261
.LBB28_74:                              # %cond.false143
	testq	$65536, %rcx            # imm = 0x10000
	je	.LBB28_77
# BB#75:                                # %cond.true147
	movq	(%rax), %rax
	cmpq	$0, 24(%rax)
	jne	.LBB28_86
# BB#76:
	movabsq	$0, %rax
	jmp	.LBB28_87
.LBB28_51:                              # %lor.lhs.false73
	movq	64(%r13), %rax
	testq	%rax, %rax
	je	.LBB28_101
# BB#52:
	xorq	%r13, %r13
	movq	$0, 48(%rsp)            # 8-byte Folded Spill
	movq	32(%rsp), %r9           # 8-byte Reload
	movq	$5, %r11
.LBB28_50:                              # %if.end370
	movq	%rax, %rcx
	movq	$1, %rbx
	jmp	.LBB28_162
.LBB28_32:                              # %lor.lhs.false35
	movq	72(%rdx), %rcx
	xorq	%r14, %r14
	testq	%rcx, %rcx
	je	.LBB28_133
# BB#33:
	movq	$1, %rbx
	xorq	%r13, %r13
	movq	$1, 72(%rsp)            # 8-byte Folded Spill
	movq	%rax, %rsi
	movq	$6, %r11
	jmp	.LBB28_162
.LBB28_36:                              # %lor.lhs.false43
	movq	88(%rdx), %rcx
	xorq	%r14, %r14
	testq	%rcx, %rcx
	je	.LBB28_38
# BB#37:
	movq	$1, %rbx
	xorq	%r13, %r13
	movq	$1, 72(%rsp)            # 8-byte Folded Spill
	movq	%rax, %rsi
	movq	$8, %r11
	jmp	.LBB28_162
.LBB28_54:
	movq	%rdi, %rax
	xorq	%r13, %r13
	jmp	.LBB28_138
.LBB28_98:
	movq	$8, %r11
	xorq	%r14, %r14
	movq	%rdi, %rax
	movq	32(%rsp), %r9           # 8-byte Reload
	xorq	%r13, %r13
.LBB28_138:                             # %land.lhs.true290
	movq	24(%rdi), %rcx
	testq	%rcx, %rcx
	je	.LBB28_142
# BB#139:
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	jmp	.LBB28_140
.LBB28_142:                             # %if.else294
	testq	%rax, %rax
	je	.LBB28_144
# BB#143:                               # %land.lhs.true296
	movq	24(%rax), %rcx
	testq	%rcx, %rcx
	movq	$1, %rax
	je	.LBB28_144
.LBB28_140:                             # %if.end361
	movq	%r14, %rbx
	movq	%rax, 48(%rsp)          # 8-byte Spill
	movq	72(%rsp), %rax          # 8-byte Reload
	orq	%rax, %r13
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %r13
	je	.LBB28_141
# BB#160:                               # %if.end361
	movabsq	$1, %r14
	jmp	.LBB28_161
.LBB28_144:                             # %if.else300
	movq	%rsi, %rdi
	addq	%r9, %r15
	movq	$32, %rcx
	shlq	%cl, %r15
	movq	$32, %rcx
	sarq	%cl, %r15
	movq	PL_AMG_names(,%r15,8), %rsi
	incq	%rsi
	andq	$8, %r10
	movq	$3, %rcx
	movq	%r10, %rax
	shrq	%cl, %rax
	testq	%rax, %rax
	movq	80(%rsp), %rbx          # 8-byte Reload
	jne	.LBB28_145
# BB#146:                               # %if.else300
	movabsq	$.L.str73, %rdx
	jmp	.LBB28_147
.LBB28_141:
	movabsq	$0, %r14
.LBB28_161:                             # %if.end361
	movq	$1, %r13
.LBB28_162:                             # %if.end370
	movq	%rcx, 56(%rsp)          # 8-byte Spill
	movq	%rsi, 40(%rsp)          # 8-byte Spill
	addq	%r9, %r15
	movabsq	$4294967295, %r12       # imm = 0xFFFFFFFF
	movq	%r15, %rax
	andq	%r12, %rax
	andq	%r12, %r11
	cmpq	%r11, %rax
	jne	.LBB28_167
# BB#163:                               # %land.lhs.true374
	movq	%rbx, 24(%rsp)          # 8-byte Spill
	movq	%r13, 64(%rsp)          # 8-byte Spill
	movq	72(%rsp), %rax          # 8-byte Reload
	testq	%r12, %rax
	movq	80(%rsp), %rdi          # 8-byte Reload
	jne	.LBB28_168
# BB#164:                               # %land.lhs.true374
	movq	%r9, %rax
	andq	%r12, %rax
	cmpq	$44, %rax
	je	.LBB28_168
# BB#165:                               # %land.lhs.true374
	cmpq	$45, %rax
	je	.LBB28_168
# BB#166:                               # %land.lhs.true374
	testq	%r14, %r14
	je	.LBB28_170
	jmp	.LBB28_168
.LBB28_167:                             # %lor.lhs.false382
	movq	%rbx, 24(%rsp)          # 8-byte Spill
	movq	%r13, 64(%rsp)          # 8-byte Spill
	testq	%r14, %r14
	movq	80(%rsp), %rdi          # 8-byte Reload
	je	.LBB28_170
.LBB28_168:                             # %if.then384
	movq	(%rdi), %rax
	movq	(%rax), %rbx
	movq	8(%rbx), %rax
	cmpq	$2, %rax
	jb	.LBB28_170
# BB#169:                               # %if.then391
	movabsq	$PL_sv_undef, %rsi
	movq	$56, %rdx
	movq	$9, %rcx
	movq	%rdi, %r14
	movq	%r9, %r13
	callq	Perl_amagic_call
	movq	(%r14), %rcx
	movq	%rax, (%rcx)
	movq	%rbx, %rdi
	callq	Perl_sv_free
	movq	%r13, %r9
	movq	%r14, %rdi
.LBB28_170:                             # %if.end396
	movq	%r9, 32(%rsp)           # 8-byte Spill
	movq	%rdi, %r13
	movq	PL_top_env(%rip), %rax
	movq	PL_stack_sp(%rip), %rbx
	movzbl	212(%rax), %ecx
	movq	%rcx, 80(%rsp)          # 8-byte Spill
	movq	$1, %rcx
	movb	%cl, 212(%rax)
	movq	$0, 128(%rsp)
	movq	$0, 120(%rsp)
	movq	$0, 112(%rsp)
	movq	$0, 104(%rsp)
	movq	$0, 96(%rsp)
	movq	$0, 88(%rsp)
	leaq	88(%rsp), %r14
	movq	%r14, 136(%rsp)
	movq	PL_curstackinfo(%rip), %rax
	movq	$0, 88(%rsp)
	movq	$66, %rcx
	movb	%cl, 124(%rsp)
	movq	48(%rax), %rax
	testq	%rax, %rax
	jne	.LBB28_172
# BB#171:                               # %if.then399
	movq	$32, %rdi
	movq	$12, %rsi
	callq	Perl_new_stackinfo
	movq	PL_curstackinfo(%rip), %rcx
	movq	%rcx, 40(%rax)
	movq	PL_curstackinfo(%rip), %rcx
	movq	%rax, 48(%rcx)
.LBB28_172:                             # %if.end402
	movq	$5, 32(%rax)
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
	leaq	(%rcx,%rdx,8), %rbx
	movq	%rbx, PL_stack_sp(%rip)
	movq	(%rax), %rcx
	movq	%rcx, PL_curstack(%rip)
	movq	%rax, PL_curstackinfo(%rip)
	callq	Perl_push_scope
	xorq	%rax, %rax
	callq	save_op
	movzbl	PL_perldb(%rip), %eax
	movq	%r14, PL_op(%rip)
	testq	$1, %rax
	je	.LBB28_175
# BB#173:                               # %land.lhs.true421
	movq	PL_curstash(%rip), %rax
	movq	PL_debstash(%rip), %rcx
	cmpq	%rcx, %rax
	je	.LBB28_175
# BB#174:                               # %if.then424
	movzbl	125(%rsp), %eax
	orq	$16, %rax
	movb	%al, 125(%rsp)
.LBB28_175:                             # %if.end428
	movq	%rbx, PL_stack_sp(%rip)
	callq	Perl_pp_pushmark
	movq	PL_stack_max(%rip), %rax
	subq	%rbx, %rax
	movq	$3, %rcx
	movq	64(%rsp), %rdx          # 8-byte Reload
	leaq	5(%rdx), %rdx
	sarq	%cl, %rax
	movq	%rdx, %rcx
	andq	%r12, %rcx
	cmpq	%rcx, %rax
	jge	.LBB28_177
# BB#176:                               # %if.then438
	movq	%rbx, %rdi
	movq	%rbx, %rsi
	callq	Perl_stack_grow
	movq	%rax, %rbx
.LBB28_177:                             # %if.end441
	movq	40(%rsp), %rdx          # 8-byte Reload
	movq	48(%rsp), %rax          # 8-byte Reload
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	movq	%rdx, %rax
	jg	.LBB28_179
# BB#178:                               # %if.end441
	movq	%r13, %rax
.LBB28_179:                             # %if.end441
	movq	%rax, 8(%rbx)
	movq	%r13, %rax
	movq	%r15, %rsi
	jg	.LBB28_181
# BB#180:                               # %if.end441
	movq	%rdx, %rax
.LBB28_181:                             # %if.end441
	movq	%rax, 16(%rbx)
	jle	.LBB28_183
# BB#182:
	movabsq	$PL_sv_yes, %rax
	jmp	.LBB28_186
.LBB28_183:                             # %cond.false458
	movq	72(%rsp), %rax          # 8-byte Reload
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB28_184
# BB#185:                               # %cond.false458
	movabsq	$PL_sv_no, %rax
	jmp	.LBB28_186
.LBB28_145:
	movabsq	$.L.str72, %rdx
.LBB28_147:                             # %if.else300
	movq	%rdi, %r9
	movq	56(%rsp), %rax          # 8-byte Reload
	movq	%rax, %r13
	movq	16(%rbx), %rax
	movq	%rax, %rdi
	andq	$268435456, %rdi        # imm = 0x10000000
	movq	$28, %rcx
	shrq	%cl, %rdi
	testq	%rdi, %rdi
	jne	.LBB28_148
# BB#149:                               # %if.else300
	movabsq	$.L.str75, %rcx
	jmp	.LBB28_150
.LBB28_148:
	movabsq	$.L.str74, %rcx
.LBB28_150:                             # %if.else300
	movq	%r13, %r15
	andq	$268435456, %rax        # imm = 0x10000000
	je	.LBB28_151
# BB#152:                               # %cond.true318
	movq	(%rbx), %rax
	movq	(%rax), %rax
	movq	(%rax), %rax
	movq	48(%rax), %rax
	movq	(%rax), %rax
	movq	80(%rax), %r8
	jmp	.LBB28_153
.LBB28_184:
	movabsq	$PL_sv_undef, %rax
.LBB28_186:                             # %cond.end461
	movq	%r13, %r12
	movq	%rax, 24(%rbx)
	cmpq	$0, 64(%rsp)            # 8-byte Folded Reload
	je	.LBB28_187
# BB#188:                               # %if.then465
	movq	$32, %rcx
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	movq	PL_AMG_names(,%rsi,8), %rdi
	incq	%rdi
	xorq	%rsi, %rsi
	callq	Perl_newSVpv
	movq	%rax, %rdi
	callq	Perl_sv_2mortal
	movq	%rax, 32(%rbx)
	addq	$32, %rbx
	jmp	.LBB28_189
.LBB28_187:
	addq	$24, %rbx
.LBB28_189:                             # %if.end473
	movq	56(%rsp), %rax          # 8-byte Reload
	movq	%rax, 8(%rbx)
	addq	$8, %rbx
	movq	%rbx, PL_stack_sp(%rip)
	callq	Perl_pp_entersub
	testq	%rax, %rax
	movq	%rax, PL_op(%rip)
	je	.LBB28_191
# BB#190:                               # %if.then477
	callq	*PL_runops(%rip)
.LBB28_191:                             # %if.end479
	callq	Perl_pop_scope
	movq	PL_stack_sp(%rip), %rbx
	movq	PL_curstackinfo(%rip), %rax
	movq	(%rbx), %r14
	addq	$-8, %rbx
	movq	%rbx, PL_stack_sp(%rip)
	movq	40(%rax), %r15
	testq	%r15, %r15
	jne	.LBB28_198
# BB#192:                               # %if.then486
	movq	PL_stderrgv(%rip), %rax
	testq	%rax, %rax
	je	.LBB28_196
# BB#193:                               # %land.lhs.true488
	movzbl	16(%rax), %ecx
	cmpq	$13, %rcx
	jne	.LBB28_196
# BB#194:                               # %land.lhs.true493
	movq	(%rax), %rax
	movq	56(%rax), %rax
	movq	16(%rax), %rax
	testq	%rax, %rax
	je	.LBB28_196
# BB#195:                               # %land.lhs.true496
	movq	(%rax), %rax
	movq	64(%rax), %rdi
	testq	%rdi, %rdi
	jne	.LBB28_197
.LBB28_196:                             # %cond.false508
	callq	Perl_PerlIO_stderr
	movq	%rax, %rdi
.LBB28_197:                             # %cond.end510
	movabsq	$.L.str79, %rsi
	xorq	%rax, %rax
	callq	PerlIO_printf
	movq	$1, %rdi
	callq	Perl_my_exit
.LBB28_198:                             # %if.end513
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
	movq	PL_top_env(%rip), %rcx
	movq	%rax, PL_curstack(%rip)
	movq	%r15, PL_curstackinfo(%rip)
	movq	80(%rsp), %rax          # 8-byte Reload
	movb	%al, 212(%rcx)
	cmpq	$0, 24(%rsp)            # 8-byte Folded Reload
	je	.LBB28_256
# BB#199:                               # %if.then536
	xorq	%rax, %rax
	movq	32(%rsp), %rcx          # 8-byte Reload
	addq	$-28, %rcx
	movabsq	$4294967295, %rsi       # imm = 0xFFFFFFFF
	andq	%rsi, %rcx
	cmpq	$17, %rcx
	ja	.LBB28_253
# BB#200:                               # %if.then536
	jmpq	*.LJTI28_2(,%rcx,8)
.LBB28_206:                             # %sw.bb551
	movzbl	18(%r14), %eax
	testq	$1, %rax
	je	.LBB28_208
# BB#207:                               # %cond.true555
	movq	(%r14), %rax
	movq	24(%rax), %rax
	jmp	.LBB28_209
.LBB28_256:                             # %if.else683
	movq	32(%rsp), %rax          # 8-byte Reload
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	cmpq	$56, %rax
	jne	.LBB28_261
# BB#257:                               # %if.then686
	movzbl	18(%r14), %eax
	testq	$8, %rax
	jne	.LBB28_259
# BB#258:                               # %if.then690
	movabsq	$.L.str80, %rdi
	xorq	%rax, %rax
	callq	Perl_croak
.LBB28_259:                             # %if.end691
	movq	(%r14), %rax
	movq	(%rax), %rax
	xorq	%r14, %r14
	testq	%rax, %rax
	movq	%rax, PL_Sv(%rip)
	je	.LBB28_261
# BB#260:                               # %land.rhs695
	incq	8(%rax)
	movq	%rax, %r14
	jmp	.LBB28_261
.LBB28_151:
	movabsq	$.L.str3, %r8
.LBB28_153:                             # %cond.end325
	movzbl	19(%r9), %eax
	testq	$16, %rax
	je	.LBB28_154
# BB#156:                               # %cond.true340
	movq	(%r9), %rax
	movq	(%rax), %rax
	movq	(%rax), %rax
	movq	48(%rax), %rax
	movq	(%rax), %rax
	movq	80(%rax), %rax
	movabsq	$.L.str76, %r9
	jmp	.LBB28_157
.LBB28_154:                             # %cond.end335.thread
	movabsq	$4294967295, %rdi       # imm = 0xFFFFFFFF
	movabsq	$.L.str3, %rax
	testq	%rdi, %r10
	movq	%rax, %r9
	jne	.LBB28_157
# BB#155:                               # %cond.end335.thread
	movabsq	$.L.str77, %r9
.LBB28_157:                             # %cond.end348
	movq	%rax, (%rsp)
	movabsq	$.L.str71, %rdi
	xorq	%rax, %rax
	xorq	%r14, %r14
	callq	Perl_newSVpvf
	movq	%rax, %rdi
	callq	Perl_sv_2mortal
	movq	%rax, %rcx
	testq	%r15, %r15
	je	.LBB28_159
# BB#158:                               # %land.lhs.true353
	movq	552(%r15), %rax
	cmpq	$2, %rax
	jg	.LBB28_261
.LBB28_159:                             # %if.else358
	movabsq	$.L.str78, %rdi
	xorq	%rax, %rax
	xorq	%r14, %r14
	movq	%rcx, %rsi
	callq	Perl_croak
	jmp	.LBB28_261
.LBB28_201:                             # %sw.bb538
	movzbl	18(%r14), %eax
	testq	$1, %rax
	je	.LBB28_203
# BB#202:                               # %cond.true542
	movq	(%r14), %rax
	movq	24(%rax), %rax
	jmp	.LBB28_204
.LBB28_214:                             # %sw.bb577
	movzbl	18(%r14), %eax
	testq	$1, %rax
	je	.LBB28_216
# BB#215:                               # %cond.true581
	movq	(%r14), %rax
	movq	24(%rax), %rax
	jmp	.LBB28_217
.LBB28_210:                             # %sw.bb564
	movzbl	18(%r14), %eax
	testq	$1, %rax
	je	.LBB28_212
# BB#211:                               # %cond.true568
	movq	(%r14), %rax
	movq	24(%rax), %rax
	jmp	.LBB28_213
.LBB28_219:                             # %sw.bb590
	movzbl	18(%r14), %eax
	testq	$1, %rax
	je	.LBB28_221
# BB#220:                               # %cond.true594
	movq	(%r14), %rax
	movq	24(%rax), %rax
	jmp	.LBB28_222
.LBB28_224:                             # %sw.bb603
	movzbl	18(%r14), %eax
	testq	$1, %rax
	je	.LBB28_226
# BB#225:                               # %cond.true607
	movq	(%r14), %rax
	movq	24(%rax), %rax
	jmp	.LBB28_227
.LBB28_229:                             # %sw.bb616
	movq	%r12, %rdi
	cmpq	%r14, %rdi
	je	.LBB28_230
# BB#231:                               # %if.then619
	movq	$2, %rdx
	movq	%r14, %rsi
	movq	%rdi, %rbx
	callq	Perl_sv_setsv_flags
	movq	%rbx, %r14
	jmp	.LBB28_261
.LBB28_232:                             # %sw.bb621
	xorq	%rax, %rax
	testq	%r14, %r14
	je	.LBB28_250
# BB#233:                               # %cond.false624
	movq	16(%r14), %rax
	testq	$262144, %rax           # imm = 0x40000
	je	.LBB28_240
# BB#234:                               # %cond.true628
	movq	(%r14), %rcx
	xorq	%rax, %rax
	testq	%rcx, %rcx
	movq	%rcx, PL_Xpv(%rip)
	je	.LBB28_250
# BB#235:                               # %land.rhs631
	movq	8(%rcx), %rdx
	cmpq	$1, %rdx
	movq	$1, %rax
	ja	.LBB28_250
# BB#236:                               # %lor.rhs635
	testq	%rdx, %rdx
	movabsq	$0, %rax
	je	.LBB28_250
# BB#237:                               # %land.rhs638
	movq	(%rcx), %rax
	movzbl	(%rax), %eax
	cmpq	$48, %rax
	je	.LBB28_238
# BB#239:                               # %land.rhs638
	movabsq	$1, %rax
	jmp	.LBB28_250
.LBB28_208:                             # %cond.false558
	movq	%r14, %rdi
	callq	Perl_sv_2iv
.LBB28_209:                             # %cond.end560
	movq	$63, %rcx
	shrq	%cl, %rax
	jmp	.LBB28_253
.LBB28_203:                             # %cond.false545
	movq	%r14, %rdi
	callq	Perl_sv_2iv
.LBB28_204:                             # %cond.end547
	testq	%rax, %rax
	jle	.LBB28_252
# BB#205:
	movabsq	$0, %rax
	jmp	.LBB28_253
.LBB28_216:                             # %cond.false584
	movq	%r14, %rdi
	callq	Perl_sv_2iv
.LBB28_217:                             # %cond.end586
	testq	%rax, %rax
	jg	.LBB28_252
# BB#218:
	movabsq	$0, %rax
	jmp	.LBB28_253
.LBB28_212:                             # %cond.false571
	movq	%r14, %rdi
	callq	Perl_sv_2iv
.LBB28_213:                             # %cond.end573
	movq	$63, %rcx
	shrq	%cl, %rax
	xorq	$1, %rax
	jmp	.LBB28_253
.LBB28_221:                             # %cond.false597
	movq	%r14, %rdi
	callq	Perl_sv_2iv
.LBB28_222:                             # %cond.end599
	testq	%rax, %rax
	je	.LBB28_252
# BB#223:
	movabsq	$0, %rax
	jmp	.LBB28_253
.LBB28_226:                             # %cond.false610
	movq	%r14, %rdi
	callq	Perl_sv_2iv
.LBB28_227:                             # %cond.end612
	testq	%rax, %rax
	jne	.LBB28_252
# BB#228:
	movabsq	$0, %rax
	jmp	.LBB28_253
.LBB28_230:
	movq	%rdi, %r14
	jmp	.LBB28_261
.LBB28_240:                             # %cond.false650
	testq	$65536, %rax            # imm = 0x10000
	je	.LBB28_244
# BB#241:                               # %cond.true654
	movq	(%r14), %rax
	cmpq	$0, 24(%rax)
	je	.LBB28_242
# BB#243:                               # %cond.true654
	movabsq	$1, %rax
	jmp	.LBB28_250
.LBB28_244:                             # %cond.false659
	movq	%rsi, %rbx
	testq	$131072, %rax           # imm = 0x20000
	je	.LBB28_248
# BB#245:                               # %cond.true663
	movq	(%r14), %rax
	movq	32(%rax), %rdi
	xorq	%rsi, %rsi
	callq	__nedf2
	testq	%rax, %rax
	je	.LBB28_246
# BB#247:                               # %cond.true663
	movabsq	$1, %rax
	jmp	.LBB28_249
.LBB28_242:
	movabsq	$0, %rax
	jmp	.LBB28_250
.LBB28_238:
	movabsq	$0, %rax
	jmp	.LBB28_250
.LBB28_248:                             # %cond.false668
	movq	%r14, %rdi
	callq	Perl_sv_2bool
	jmp	.LBB28_249
.LBB28_246:
	movabsq	$0, %rax
.LBB28_249:                             # %cond.end677
	movq	%rbx, %rsi
.LBB28_250:                             # %cond.end677
	testq	%rsi, %rax
	jne	.LBB28_251
.LBB28_252:                             # %cond.end677
	movabsq	$1, %rax
.LBB28_253:                             # %sw.epilog680
	testq	%rax, %rax
	jne	.LBB28_254
# BB#255:                               # %sw.epilog680
	movabsq	$PL_sv_no, %r14
	jmp	.LBB28_261
.LBB28_254:
	movabsq	$PL_sv_yes, %r14
.LBB28_261:                             # %return
	movq	%r14, %rax
	addq	$144, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB28_251:
	movabsq	$0, %rax
	jmp	.LBB28_253
.LBB28_83:                              # %cond.false179
	movq	%rax, %rdi
	callq	Perl_sv_2nv
.LBB28_84:                              # %cond.end181
	xorq	%rsi, %rsi
	movq	%rax, %rdi
	callq	__ltdf2
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	js	.LBB28_86
# BB#85:
	movabsq	$0, %rax
	jmp	.LBB28_87
.LBB28_77:                              # %cond.false151
	testq	$131072, %rcx           # imm = 0x20000
	je	.LBB28_80
# BB#78:                                # %cond.true155
	movq	(%rax), %rax
	movq	32(%rax), %rax
	xorq	%rsi, %rsi
	movq	%rax, %rdi
	callq	__nedf2
	testq	%rax, %rax
	je	.LBB28_79
.LBB28_86:                              # %cond.end181
	movabsq	$1, %rax
	jmp	.LBB28_87
.LBB28_101:                             # %lor.rhs76
	movq	%r13, %rax
	movq	56(%rax), %rcx
	movq	$4, %r11
	xorq	%rax, %rax
	movq	$1, %r14
.LBB28_102:                             # %sw.epilog
	movq	%rsi, %rdx
	jmp	.LBB28_103
.LBB28_133:
	movq	$0, 64(%rsp)            # 8-byte Folded Spill
	movq	%rdi, %rax
	movq	$6, %r11
	jmp	.LBB28_134
.LBB28_38:
	movq	$0, 64(%rsp)            # 8-byte Folded Spill
	movq	%rdi, %rax
	movq	$8, %r11
.LBB28_134:                             # %not_found
	movq	$1, %r13
	jmp	.LBB28_135
.LBB28_80:                              # %cond.false159
	movq	%rax, %rdi
	callq	Perl_sv_2bool
	jmp	.LBB28_87
.LBB28_79:
	movabsq	$0, %rax
.LBB28_87:                              # %if.end185
	testq	%r12, %rax
	je	.LBB28_88
# BB#89:                                # %if.then187
	movq	48(%rsp), %rax          # 8-byte Reload
	andq	%rax, %r12
	cmpq	$8, %r12
	jne	.LBB28_90
# BB#91:                                # %if.then187
	movabsq	$1, %rax
	jmp	.LBB28_92
.LBB28_88:
	movq	80(%rsp), %r14          # 8-byte Reload
	jmp	.LBB28_261
.LBB28_90:
	movabsq	$0, %rax
.LBB28_92:                              # %if.then187
	movq	80(%rsp), %rdx          # 8-byte Reload
	je	.LBB28_94
# BB#93:                                # %if.then187
	movq	40(%rsp), %rdx          # 8-byte Reload
.LBB28_94:                              # %if.then187
	je	.LBB28_96
# BB#95:                                # %if.then187
	movq	80(%rsp), %rcx          # 8-byte Reload
	movq	%rcx, 64(%rsp)          # 8-byte Spill
.LBB28_96:                              # %if.then187
	xorq	%r14, %r14
	movq	64(%rsp), %rcx          # 8-byte Reload
	movq	%rcx, 80(%rsp)          # 8-byte Spill
	movq	24(%rsp), %r10          # 8-byte Reload
	movq	48(%rsp), %r11          # 8-byte Reload
	movq	16(%rsp), %rdi          # 8-byte Reload
	movq	%r13, %rcx
.LBB28_103:                             # %sw.epilog
	xorq	%r13, %r13
	testq	%rcx, %rcx
	movq	32(%rsp), %r9           # 8-byte Reload
	je	.LBB28_105
# BB#104:
	movq	%rax, 48(%rsp)          # 8-byte Spill
	movq	%r14, %rbx
	xorq	%r14, %r14
	xorq	%r13, %r13
	movq	%rdx, %rsi
	jmp	.LBB28_162
.LBB28_105:
	movq	$0, 64(%rsp)            # 8-byte Folded Spill
	movq	%rdi, %rax
	movq	%rdx, %rsi
	jmp	.LBB28_135
.Ltmp304:
	.size	Perl_amagic_call, .Ltmp304-Perl_amagic_call
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI28_0:
	.quad	.LBB28_48
	.quad	.LBB28_135
	.quad	.LBB28_29
	.quad	.LBB28_34
	.quad	.LBB28_135
	.quad	.LBB28_135
	.quad	.LBB28_135
	.quad	.LBB28_135
	.quad	.LBB28_135
	.quad	.LBB28_135
	.quad	.LBB28_135
	.quad	.LBB28_135
	.quad	.LBB28_135
	.quad	.LBB28_135
	.quad	.LBB28_53
	.quad	.LBB28_97
	.quad	.LBB28_100
	.quad	.LBB28_100
	.quad	.LBB28_100
	.quad	.LBB28_100
	.quad	.LBB28_100
	.quad	.LBB28_261
	.quad	.LBB28_261
.LJTI28_1:
	.quad	.LBB28_59
	.quad	.LBB28_39
	.quad	.LBB28_135
	.quad	.LBB28_46
	.quad	.LBB28_44
.LJTI28_2:
	.quad	.LBB28_206
	.quad	.LBB28_201
	.quad	.LBB28_214
	.quad	.LBB28_210
	.quad	.LBB28_219
	.quad	.LBB28_224
	.quad	.LBB28_253
	.quad	.LBB28_253
	.quad	.LBB28_206
	.quad	.LBB28_201
	.quad	.LBB28_214
	.quad	.LBB28_210
	.quad	.LBB28_219
	.quad	.LBB28_224
	.quad	.LBB28_232
	.quad	.LBB28_253
	.quad	.LBB28_229
	.quad	.LBB28_229

	.text
	.globl	Perl_is_gv_magical
	.align	16, 0x90
	.type	Perl_is_gv_magical,@function
Perl_is_gv_magical:                     # @Perl_is_gv_magical
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp309:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp310:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp311:
	.cfi_def_cfa_offset 32
.Ltmp312:
	.cfi_offset %rbx, -32
.Ltmp313:
	.cfi_offset %r14, -24
.Ltmp314:
	.cfi_offset %r15, -16
	cmpq	$2, %rsi
	jb	.LBB29_22
# BB#1:                                 # %if.then
	movsbq	(%rdi), %rax
	leaq	1(%rdi), %r14
	cmpq	$48, %rax
	jg	.LBB29_4
# BB#2:                                 # %if.then
	addq	$-15, %rax
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	andq	%r15, %rax
	cmpq	$8, %rax
	ja	.LBB29_27
# BB#3:                                 # %if.then
	jmpq	*.LJTI29_1(,%rax,8)
.LBB29_13:                              # %sw.bb33
	movabsq	$.L.str46, %rsi
	jmp	.LBB29_14
.LBB29_22:                              # %if.else
	movsbq	(%rdi), %rax
	cmpq	$123, %rax
	movq	$1, %rbx
	jg	.LBB29_25
# BB#23:                                # %if.else
	decq	%rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rax, %rcx
	cmpq	$95, %rcx
	ja	.LBB29_27
# BB#24:                                # %if.else
	jmpq	*.LJTI29_0(,%rcx,8)
.LBB29_4:                               # %if.then
	leaq	-49(%rax), %rcx
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	andq	%rbx, %rcx
	cmpq	$9, %rcx
	jae	.LBB29_5
# BB#19:                                # %sw.bb57
	leaq	-1(%rsi,%rdi), %rax
	.align	16, 0x90
.LBB29_20:                              # %while.cond
                                        # =>This Inner Loop Header: Depth=1
	movq	$1, %rbx
	cmpq	%rdi, %rax
	jbe	.LBB29_28
# BB#21:                                # %while.body
                                        #   in Loop: Header=BB29_20 Depth=1
	movzbl	(%rax), %ecx
	addq	$-48, %rcx
	andq	$255, %rcx
	decq	%rax
	xorq	%rbx, %rbx
	cmpq	$10, %rcx
	jb	.LBB29_20
	jmp	.LBB29_28
.LBB29_25:                              # %if.else
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	cmpq	$124, %rax
	je	.LBB29_28
# BB#26:                                # %if.else
	cmpq	$126, %rax
	je	.LBB29_28
	jmp	.LBB29_27
.LBB29_5:                               # %if.then
	andq	%rbx, %rax
	cmpq	$83, %rax
	jne	.LBB29_6
# BB#9:                                 # %sw.bb18
	cmpq	$3, %rsi
	jne	.LBB29_27
# BB#10:                                # %land.lhs.true21
	movzbl	(%r14), %eax
	cmpq	$73, %rax
	jne	.LBB29_27
# BB#11:                                # %land.lhs.true26
	movzbl	2(%rdi), %eax
	cmpq	$71, %rax
	jmp	.LBB29_12
.LBB29_15:                              # %sw.bb38
	movabsq	$.L.str47, %rsi
	jmp	.LBB29_14
.LBB29_16:                              # %sw.bb43
	movabsq	$.L.str48, %rsi
	movq	%r14, %rdi
	callq	strcmp
	testq	%r15, %rax
	movq	$1, %rbx
	je	.LBB29_28
# BB#17:                                # %if.end47
	movabsq	$.L.str49, %rsi
	movq	%r14, %rdi
	callq	strcmp
	testq	%r15, %rax
	jne	.LBB29_27
	jmp	.LBB29_28
.LBB29_18:                              # %sw.bb52
	movabsq	$.L.str50, %rsi
.LBB29_14:                              # %sw.bb33
	movq	%r14, %rdi
	callq	strcmp
	testq	%r15, %rax
.LBB29_12:                              # %land.lhs.true26
	movq	$1, %rbx
	jne	.LBB29_27
	jmp	.LBB29_28
.LBB29_6:                               # %if.then
	cmpq	$79, %rax
	jne	.LBB29_27
# BB#7:                                 # %sw.bb12
	cmpq	$8, %rsi
	jne	.LBB29_27
# BB#8:                                 # %land.lhs.true15
	movabsq	$.L.str42, %rsi
	movq	%r14, %rdi
	callq	strcmp
	testq	%rbx, %rax
	jmp	.LBB29_12
.LBB29_27:                              # %if.end73
	xorq	%rbx, %rbx
.LBB29_28:                              # %return
	movq	%rbx, %rax
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp315:
	.size	Perl_is_gv_magical, .Ltmp315-Perl_is_gv_magical
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI29_0:
	.quad	.LBB29_28
	.quad	.LBB29_27
	.quad	.LBB29_28
	.quad	.LBB29_28
	.quad	.LBB29_28
	.quad	.LBB29_28
	.quad	.LBB29_27
	.quad	.LBB29_28
	.quad	.LBB29_28
	.quad	.LBB29_27
	.quad	.LBB29_27
	.quad	.LBB29_28
	.quad	.LBB29_27
	.quad	.LBB29_28
	.quad	.LBB29_28
	.quad	.LBB29_28
	.quad	.LBB29_27
	.quad	.LBB29_27
	.quad	.LBB29_28
	.quad	.LBB29_28
	.quad	.LBB29_27
	.quad	.LBB29_28
	.quad	.LBB29_28
	.quad	.LBB29_27
	.quad	.LBB29_27
	.quad	.LBB29_27
	.quad	.LBB29_27
	.quad	.LBB29_27
	.quad	.LBB29_27
	.quad	.LBB29_27
	.quad	.LBB29_27
	.quad	.LBB29_27
	.quad	.LBB29_28
	.quad	.LBB29_27
	.quad	.LBB29_28
	.quad	.LBB29_27
	.quad	.LBB29_28
	.quad	.LBB29_28
	.quad	.LBB29_28
	.quad	.LBB29_28
	.quad	.LBB29_28
	.quad	.LBB29_28
	.quad	.LBB29_28
	.quad	.LBB29_28
	.quad	.LBB29_28
	.quad	.LBB29_28
	.quad	.LBB29_28
	.quad	.LBB29_27
	.quad	.LBB29_28
	.quad	.LBB29_28
	.quad	.LBB29_28
	.quad	.LBB29_28
	.quad	.LBB29_28
	.quad	.LBB29_28
	.quad	.LBB29_28
	.quad	.LBB29_28
	.quad	.LBB29_28
	.quad	.LBB29_28
	.quad	.LBB29_28
	.quad	.LBB29_28
	.quad	.LBB29_28
	.quad	.LBB29_28
	.quad	.LBB29_28
	.quad	.LBB29_27
	.quad	.LBB29_27
	.quad	.LBB29_27
	.quad	.LBB29_27
	.quad	.LBB29_27
	.quad	.LBB29_27
	.quad	.LBB29_27
	.quad	.LBB29_27
	.quad	.LBB29_27
	.quad	.LBB29_27
	.quad	.LBB29_27
	.quad	.LBB29_27
	.quad	.LBB29_27
	.quad	.LBB29_27
	.quad	.LBB29_27
	.quad	.LBB29_27
	.quad	.LBB29_27
	.quad	.LBB29_27
	.quad	.LBB29_27
	.quad	.LBB29_27
	.quad	.LBB29_27
	.quad	.LBB29_27
	.quad	.LBB29_27
	.quad	.LBB29_27
	.quad	.LBB29_27
	.quad	.LBB29_27
	.quad	.LBB29_27
	.quad	.LBB29_28
	.quad	.LBB29_28
	.quad	.LBB29_28
	.quad	.LBB29_28
	.quad	.LBB29_27
	.quad	.LBB29_28
.LJTI29_1:
	.quad	.LBB29_13
	.quad	.LBB29_27
	.quad	.LBB29_27
	.quad	.LBB29_27
	.quad	.LBB29_27
	.quad	.LBB29_15
	.quad	.LBB29_16
	.quad	.LBB29_27
	.quad	.LBB29_18

	.type	.L.str,@object          # @.str
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.asciz	"Bad symbol for array"
	.size	.L.str, 21

	.type	.L.str1,@object         # @.str1
.L.str1:
	.asciz	"Bad symbol for hash"
	.size	.L.str1, 20

	.type	.L.str2,@object         # @.str2
.L.str2:
	.asciz	"Bad symbol for filehandle"
	.size	.L.str2, 26

	.type	.L.str3,@object         # @.str3
.L.str3:
	.zero	1
	.size	.L.str3, 1

	.type	.L.str4,@object         # @.str4
.L.str4:
	.asciz	"UNIVERSAL"
	.size	.L.str4, 10

	.type	.L.str5,@object         # @.str5
.L.str5:
	.asciz	"Can't use anonymous symbol table for method lookup"
	.size	.L.str5, 51

	.type	.L.str6,@object         # @.str6
.L.str6:
	.asciz	"Recursive inheritance detected while looking for method '%s' in package '%s'"
	.size	.L.str6, 77

	.type	.L.str7,@object         # @.str7
.L.str7:
	.asciz	"ISA"
	.size	.L.str7, 4

	.type	.L.str8,@object         # @.str8
.L.str8:
	.asciz	"::SUPER"
	.size	.L.str8, 8

	.type	.L.str9,@object         # @.str9
.L.str9:
	.asciz	"Cannot create %s::ISA"
	.size	.L.str9, 22

	.type	.L.str10,@object        # @.str10
.L.str10:
	.asciz	"Can't locate package %_ for @%s::ISA"
	.size	.L.str10, 37

	.type	.L.str11,@object        # @.str11
.L.str11:
	.asciz	"SUPER"
	.size	.L.str11, 6

	.type	.L.str12,@object        # @.str12
.L.str12:
	.asciz	"%s::SUPER"
	.size	.L.str12, 10

	.type	.L.str13,@object        # @.str13
.L.str13:
	.asciz	"import"
	.size	.L.str13, 7

	.type	.L.str14,@object        # @.str14
.L.str14:
	.asciz	"unimport"
	.size	.L.str14, 9

	.type	.LPerl_gv_autoload4.autoload,@object # @Perl_gv_autoload4.autoload
.LPerl_gv_autoload4.autoload:
	.asciz	"AUTOLOAD"
	.size	.LPerl_gv_autoload4.autoload, 9

	.type	.L.str15,@object        # @.str15
.L.str15:
	.asciz	"Use of inherited AUTOLOAD for non-method %s::%.*s() is deprecated"
	.size	.L.str15, 66

	.type	.L.str16,@object        # @.str16
.L.str16:
	.asciz	"::"
	.size	.L.str16, 3

	.type	.L.str17,@object        # @.str17
.L.str17:
	.asciz	"main::"
	.size	.L.str17, 7

	.type	.L.str18,@object        # @.str18
.L.str18:
	.asciz	"INC"
	.size	.L.str18, 4

	.type	.L.str19,@object        # @.str19
.L.str19:
	.asciz	"ENV"
	.size	.L.str19, 4

	.type	.L.str20,@object        # @.str20
.L.str20:
	.asciz	"SIG"
	.size	.L.str20, 4

	.type	.L.str21,@object        # @.str21
.L.str21:
	.asciz	"STDIN"
	.size	.L.str21, 6

	.type	.L.str22,@object        # @.str22
.L.str22:
	.asciz	"STDOUT"
	.size	.L.str22, 7

	.type	.L.str23,@object        # @.str23
.L.str23:
	.asciz	"STDERR"
	.size	.L.str23, 7

	.type	.L.str24,@object        # @.str24
.L.str24:
	.asciz	"ARGV"
	.size	.L.str24, 5

	.type	.L.str25,@object        # @.str25
.L.str25:
	.asciz	"ARGVOUT"
	.size	.L.str25, 8

	.type	.L.str26,@object        # @.str26
.L.str26:
	.asciz	"Variable \"%c%s\" is not imported"
	.size	.L.str26, 32

	.type	.L.str27,@object        # @.str27
.L.str27:
	.asciz	"\t(Did you mean &%s instead?)\n"
	.size	.L.str27, 30

	.type	.L.str28,@object        # @.str28
.L.str28:
	.asciz	"Global symbol \"%s%s\" requires explicit package name"
	.size	.L.str28, 52

	.type	.L.str29,@object        # @.str29
.L.str29:
	.asciz	"$"
	.size	.L.str29, 2

	.type	.L.str30,@object        # @.str30
.L.str30:
	.asciz	"@"
	.size	.L.str30, 2

	.type	.L.str31,@object        # @.str31
.L.str31:
	.asciz	"%"
	.size	.L.str31, 2

	.type	.L.str32,@object        # @.str32
.L.str32:
	.asciz	"Had to create %s unexpectedly"
	.size	.L.str32, 30

	.type	.L.str33,@object        # @.str33
.L.str33:
	.asciz	"RGV"
	.size	.L.str33, 4

	.type	.L.str34,@object        # @.str34
.L.str34:
	.asciz	"XPORT"
	.size	.L.str34, 6

	.type	.L.str35,@object        # @.str35
.L.str35:
	.asciz	"SA"
	.size	.L.str35, 3

	.type	.L.str36,@object        # @.str36
.L.str36:
	.asciz	"AnyDBM_File::ISA"
	.size	.L.str36, 17

	.type	.L.str37,@object        # @.str37
.L.str37:
	.asciz	"NDBM_File"
	.size	.L.str37, 10

	.type	.L.str38,@object        # @.str38
.L.str38:
	.asciz	"DB_File"
	.size	.L.str38, 8

	.type	.L.str39,@object        # @.str39
.L.str39:
	.asciz	"GDBM_File"
	.size	.L.str39, 10

	.type	.L.str40,@object        # @.str40
.L.str40:
	.asciz	"SDBM_File"
	.size	.L.str40, 10

	.type	.L.str41,@object        # @.str41
.L.str41:
	.asciz	"ODBM_File"
	.size	.L.str41, 10

	.type	.L.str42,@object        # @.str42
.L.str42:
	.asciz	"VERLOAD"
	.size	.L.str42, 8

	.type	.L.str43,@object        # @.str43
.L.str43:
	.asciz	"IG"
	.size	.L.str43, 3

	.type	.L.str44,@object        # @.str44
.L.str44:
	.asciz	"ERSION"
	.size	.L.str44, 7

	.type	.L.str45,@object        # @.str45
.L.str45:
	.asciz	"NCODING"
	.size	.L.str45, 8

	.type	.L.str46,@object        # @.str46
.L.str46:
	.asciz	"PEN"
	.size	.L.str46, 4

	.type	.L.str47,@object        # @.str47
.L.str47:
	.asciz	"AINT"
	.size	.L.str47, 5

	.type	.L.str48,@object        # @.str48
.L.str48:
	.asciz	"NICODE"
	.size	.L.str48, 7

	.type	.L.str49,@object        # @.str49
.L.str49:
	.asciz	"TF8LOCALE"
	.size	.L.str49, 10

	.type	.L.str50,@object        # @.str50
.L.str50:
	.asciz	"ARNING_BITS"
	.size	.L.str50, 12

	.type	.L.str51,@object        # @.str51
.L.str51:
	.asciz	"Use of $%s is deprecated"
	.size	.L.str51, 25

	.type	.L.str52,@object        # @.str52
.L.str52:
	.asciz	"\f"
	.size	.L.str52, 2

	.type	.L.str53,@object        # @.str53
.L.str53:
	.asciz	"\034"
	.size	.L.str53, 2

	.type	.L.str54,@object        # @.str54
.L.str54:
	.asciz	"%8.6f"
	.size	.L.str54, 6

	.type	.L.str55,@object        # @.str55
.L.str55:
	.asciz	"__ANON__"
	.size	.L.str55, 9

	.type	.L.str56,@object        # @.str56
.L.str56:
	.asciz	"main"
	.size	.L.str56, 5

	.type	.L.str57,@object        # @.str57
.L.str57:
	.asciz	"*"
	.size	.L.str57, 2

	.type	.L.str58,@object        # @.str58
.L.str58:
	.asciz	"FileHandle::"
	.size	.L.str58, 13

	.type	.L.str59,@object        # @.str59
.L.str59:
	.asciz	"IO::Handle::"
	.size	.L.str59, 13

	.type	.L.str60,@object        # @.str60
.L.str60:
	.asciz	"/lib/"
	.size	.L.str60, 6

	.type	.L.str61,@object        # @.str61
.L.str61:
	.asciz	".pm"
	.size	.L.str61, 4

	.type	.L.str62,@object        # @.str62
.L.str62:
	.asciz	"Name \"%s::%s\" used only once: possible typo"
	.size	.L.str62, 44

	.type	.L.str63,@object        # @.str63
.L.str63:
	.asciz	"%s::_GEN_%ld"
	.size	.L.str63, 13

	.type	.L.str64,@object        # @.str64
.L.str64:
	.asciz	"Attempt to free unreferenced glob pointers"
	.size	.L.str64, 43

	.type	.L.str65,@object        # @.str65
.L.str65:
	.asciz	"nil"
	.size	.L.str65, 4

	.type	.L.str66,@object        # @.str66
.L.str66:
	.asciz	"overload"
	.size	.L.str66, 9

	.type	.L.str67,@object        # @.str67
.L.str67:
	.asciz	"???"
	.size	.L.str67, 4

	.type	.L.str68,@object        # @.str68
.L.str68:
	.asciz	"%s method `%.256s' overloading `%s' in package `%.256s'"
	.size	.L.str68, 56

	.type	.L.str69,@object        # @.str69
.L.str69:
	.asciz	"Stub found while resolving"
	.size	.L.str69, 27

	.type	.L.str70,@object        # @.str70
.L.str70:
	.asciz	"Can't resolve"
	.size	.L.str70, 14

	.type	.L.str71,@object        # @.str71
.L.str71:
	.asciz	"Operation `%s': no method found,%sargument %s%s%s%s"
	.size	.L.str71, 52

	.type	.L.str72,@object        # @.str72
.L.str72:
	.asciz	" "
	.size	.L.str72, 2

	.type	.L.str73,@object        # @.str73
.L.str73:
	.asciz	"\n\tleft "
	.size	.L.str73, 8

	.type	.L.str74,@object        # @.str74
.L.str74:
	.asciz	"in overloaded package "
	.size	.L.str74, 23

	.type	.L.str75,@object        # @.str75
.L.str75:
	.asciz	"has no overloaded magic"
	.size	.L.str75, 24

	.type	.L.str76,@object        # @.str76
.L.str76:
	.asciz	",\n\tright argument in overloaded package "
	.size	.L.str76, 41

	.type	.L.str77,@object        # @.str77
.L.str77:
	.asciz	",\n\tright argument has no overloaded magic"
	.size	.L.str77, 42

	.type	.L.str78,@object        # @.str78
.L.str78:
	.asciz	"%_"
	.size	.L.str78, 3

	.type	.L.str79,@object        # @.str79
.L.str79:
	.asciz	"panic: POPSTACK\n"
	.size	.L.str79, 17

	.type	.L.str80,@object        # @.str80
.L.str80:
	.asciz	"Copy method did not return a reference"
	.size	.L.str80, 39


	.ident	"clang version 3.5 (trunk)"
	.section	".note.GNU-stack","",@progbits
