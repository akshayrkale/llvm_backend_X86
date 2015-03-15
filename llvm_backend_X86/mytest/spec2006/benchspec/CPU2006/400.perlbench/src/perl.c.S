	.file	"perl.c.bc"
	.text
	.globl	perl_alloc
	.align	16, 0x90
	.type	perl_alloc,@function
perl_alloc:                             # @perl_alloc
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rax
.Ltmp1:
	.cfi_def_cfa_offset 16
	movq	$1, %rdi
	callq	malloc
	cmpq	$0, PL_curinterp(%rip)
	jne	.LBB0_2
# BB#1:                                 # %if.then
	movq	%rax, PL_curinterp(%rip)
.LBB0_2:                                # %if.end
	xorq	%rcx, %rcx
	movb	%cl, (%rax)
	popq	%rdx
	retq
.Ltmp2:
	.size	perl_alloc, .Ltmp2-perl_alloc
	.cfi_endproc

	.globl	perl_construct
	.align	16, 0x90
	.type	perl_construct,@function
perl_construct:                         # @perl_construct
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp6:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp7:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp8:
	.cfi_def_cfa_offset 32
.Ltmp9:
	.cfi_offset %rbx, -24
.Ltmp10:
	.cfi_offset %r14, -16
	movslq	PL_perl_destruct_level(%rip), %rax
	testq	%rax, %rax
	jle	.LBB1_2
# BB#1:                                 # %if.then
	xorq	%rax, %rax
	callq	init_interp
.LBB1_2:                                # %if.end
	cmpq	$0, PL_linestr(%rip)
	jne	.LBB1_14
# BB#3:                                 # %if.then1
	movq	$PL_compiling, PL_curcop(%rip)
	movq	$79, %rdi
	callq	Perl_newSV
	movq	%rax, PL_linestr(%rip)
	movq	$5, %rsi
	movq	%rax, %rdi
	callq	Perl_sv_upgrade
	movq	PL_sv_undef+16(%rip), %rax
	testq	$8388608, %rax          # imm = 0x800000
	jne	.LBB1_13
# BB#4:                                 # %if.then5
	orq	$8388608, %rax          # imm = 0x800000
	movq	PL_No(%rip), %rsi
	movq	%rax, PL_sv_undef+16(%rip)
	movabsq	$9223372036854775807, %rbx # imm = 0x7FFFFFFFFFFFFFFF
	movq	%rbx, PL_sv_undef+8(%rip)
	movabsq	$PL_sv_no, %rdi
	callq	Perl_sv_setpv
	movq	PL_sv_no+16(%rip), %rax
	testq	$65536, %rax            # imm = 0x10000
	jne	.LBB1_6
# BB#5:                                 # %cond.false
	movabsq	$PL_sv_no, %rdi
	callq	Perl_sv_2iv
	movq	PL_sv_no+16(%rip), %rax
.LBB1_6:                                # %cond.end
	testq	$131072, %rax           # imm = 0x20000
	jne	.LBB1_8
# BB#7:                                 # %cond.false12
	movabsq	$PL_sv_no, %rdi
	callq	Perl_sv_2nv
	movq	PL_sv_no+16(%rip), %rax
.LBB1_8:                                # %cond.end14
	movq	PL_Yes(%rip), %rsi
	orq	$8388608, %rax          # imm = 0x800000
	movq	%rax, PL_sv_no+16(%rip)
	movq	%rbx, PL_sv_no+8(%rip)
	movabsq	$PL_sv_yes, %rdi
	callq	Perl_sv_setpv
	movq	PL_sv_yes+16(%rip), %rax
	testq	$65536, %rax            # imm = 0x10000
	jne	.LBB1_10
# BB#9:                                 # %cond.false21
	movabsq	$PL_sv_yes, %rdi
	callq	Perl_sv_2iv
	movq	PL_sv_yes+16(%rip), %rax
.LBB1_10:                               # %cond.end23
	testq	$131072, %rax           # imm = 0x20000
	jne	.LBB1_12
# BB#11:                                # %cond.false29
	movabsq	$PL_sv_yes, %rdi
	callq	Perl_sv_2nv
	movq	PL_sv_yes+16(%rip), %rax
.LBB1_12:                               # %cond.end31
	orq	$8388608, %rax          # imm = 0x800000
	movq	%rax, PL_sv_yes+16(%rip)
	movq	%rbx, PL_sv_yes+8(%rip)
	orq	$8388608, PL_sv_placeholder+16(%rip) # imm = 0x800000
	movq	%rbx, PL_sv_placeholder+8(%rip)
.LBB1_13:                               # %if.end35
	movq	$Perl_sighandler, PL_sighandlerp(%rip)
	callq	Perl_newHV
	movq	%rax, PL_pidstatus(%rip)
.LBB1_14:                               # %if.end37
	movabsq	$.L.str, %rdi
	movq	$1, %rsi
	movq	$1, %r14
	callq	Perl_newSVpvn
	movq	%rax, PL_rs(%rip)
	movq	$128, %rdi
	movq	$52, %rsi
	callq	Perl_new_stackinfo
	movq	%rax, PL_curstackinfo(%rip)
	movq	$1, 32(%rax)
	movq	(%rax), %rax
	movq	%rax, PL_curstack(%rip)
	movq	%rax, PL_mainstack(%rip)
	movq	(%rax), %rcx
	movq	(%rcx), %rcx
	movq	%rcx, PL_stack_base(%rip)
	movq	%rcx, PL_stack_sp(%rip)
	movq	(%rax), %rax
	movq	16(%rax), %rax
	leaq	(%rcx,%rax,8), %rax
	movq	%rax, PL_stack_max(%rip)
	movq	$1024, %rdi             # imm = 0x400
	callq	Perl_safesysmalloc
	movq	%rax, PL_tmps_stack(%rip)
	movq	$-1, PL_tmps_floor(%rip)
	movq	$-1, PL_tmps_ix(%rip)
	movq	$128, PL_tmps_max(%rip)
	movq	$256, %rdi              # imm = 0x100
	callq	Perl_safesysmalloc
	movq	%rax, PL_markstack(%rip)
	movq	%rax, PL_markstack_ptr(%rip)
	addq	$256, %rax              # imm = 0x100
	movq	%rax, PL_markstack_max(%rip)
	movq	$256, %rdi              # imm = 0x100
	callq	Perl_safesysmalloc
	movq	%rax, PL_scopestack(%rip)
	movq	$0, PL_scopestack_ix(%rip)
	movq	$32, PL_scopestack_max(%rip)
	movq	$1024, %rdi             # imm = 0x400
	callq	Perl_safesysmalloc
	movq	%rax, PL_savestack(%rip)
	movq	$0, PL_savestack_ix(%rip)
	movq	$128, PL_savestack_max(%rip)
	movq	$128, %rdi
	callq	Perl_safesysmalloc
	movq	%rax, PL_retstack(%rip)
	movq	$0, PL_retstack_ix(%rip)
	movq	$16, PL_retstack_max(%rip)
	xorq	%rax, %rax
	xorq	%rbx, %rbx
	callq	init_ids
	movq	$11, PL_lex_state(%rip)
	movabsq	$PL_start_env, %rdi
	xorq	%rsi, %rsi
	movq	$216, %rdx
	callq	memset
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	movl	%eax, PL_start_env+208(%rip)
	movb	%r14b, PL_start_env+212(%rip)
	movq	$PL_start_env, PL_top_env(%rip)
	movq	$0, PL_statusvalue(%rip)
	movq	$1, %rdi
	callq	Perl_init_i18nl10n
	movq	$4, %rdi
	callq	Perl_newSV
	movq	%rax, PL_patchlevel(%rip)
	movq	16(%rax), %rcx
	andq	$254, %rcx
	cmpq	$5, %rcx
	ja	.LBB1_16
# BB#15:                                # %lor.rhs
	movq	$6, %rsi
	movq	%rax, %rdi
	callq	Perl_sv_upgrade
	movq	PL_patchlevel(%rip), %rax
.LBB1_16:                               # %lor.end
	movq	(%rax), %rax
	movq	(%rax), %rdi
	movq	$5, %rsi
	callq	Perl_uvuni_to_utf8
	movq	$8, %rsi
	movq	%rax, %rdi
	callq	Perl_uvuni_to_utf8
	movq	$7, %rsi
	movq	%rax, %rdi
	callq	Perl_uvuni_to_utf8
	movb	%bl, (%rax)
	movq	PL_patchlevel(%rip), %rcx
	movq	(%rcx), %rdx
	movq	(%rdx), %rsi
	subq	%rsi, %rax
	movq	%rax, 8(%rdx)
	movq	16(%rcx), %rax
	movabsq	$4617324533042155113, %rsi # imm = 0x40140832FCAC8E69
	movq	%rsi, 32(%rdx)
	orq	$646316032, %rax        # imm = 0x26860000
	movq	%rax, 16(%rcx)
	movq	$local_patches, PL_localpatches(%rip)
	callq	PerlIO_init
	callq	Perl_newAV
	movq	%rax, PL_fdpid(%rip)
	callq	Perl_newHV
	movq	%rax, PL_modglobal(%rip)
	movabsq	$.L.str1, %rbx
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	Perl_newSVpvn
	movq	%rax, PL_errors(%rip)
	movabsq	$PL_debug_pad, %rdi
	xorq	%rdx, %rdx
	movq	%rbx, %rsi
	callq	Perl_sv_setpvn
	movabsq	$PL_debug_pad+24, %rdi
	xorq	%rdx, %rdx
	movq	%rbx, %rsi
	callq	Perl_sv_setpvn
	movabsq	$PL_debug_pad+48, %rdi
	xorq	%rdx, %rdx
	movq	%rbx, %rsi
	callq	Perl_sv_setpvn
	callq	Perl_newHV
	movq	%rax, PL_strtab(%rip)
	andq	$-536870913, 16(%rax)   # imm = 0xFFFFFFFFDFFFFFFF
	movq	$512, %rsi              # imm = 0x200
	movq	%rax, %rdi
	callq	Perl_hv_ksplit
	movq	environ(%rip), %rax
	movq	%rax, PL_origenviron(%rip)
	movq	$60, PL_clocktick(%rip)
	callq	Perl_newHV
	movq	%rax, PL_stashcache(%rip)
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	jmp	Perl_push_scope  # TAILCALL
.Ltmp11:
	.size	perl_construct, .Ltmp11-perl_construct
	.cfi_endproc

	.globl	Perl_init_stacks
	.align	16, 0x90
	.type	Perl_init_stacks,@function
Perl_init_stacks:                       # @Perl_init_stacks
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rax
.Ltmp13:
	.cfi_def_cfa_offset 16
	movq	$128, %rdi
	movq	$52, %rsi
	callq	Perl_new_stackinfo
	movq	%rax, PL_curstackinfo(%rip)
	movq	$1, 32(%rax)
	movq	(%rax), %rax
	movq	%rax, PL_curstack(%rip)
	movq	%rax, PL_mainstack(%rip)
	movq	(%rax), %rcx
	movq	(%rcx), %rcx
	movq	%rcx, PL_stack_base(%rip)
	movq	%rcx, PL_stack_sp(%rip)
	movq	(%rax), %rax
	movq	16(%rax), %rax
	leaq	(%rcx,%rax,8), %rax
	movq	%rax, PL_stack_max(%rip)
	movq	$1024, %rdi             # imm = 0x400
	callq	Perl_safesysmalloc
	movq	%rax, PL_tmps_stack(%rip)
	movq	$-1, PL_tmps_floor(%rip)
	movq	$-1, PL_tmps_ix(%rip)
	movq	$128, PL_tmps_max(%rip)
	movq	$256, %rdi              # imm = 0x100
	callq	Perl_safesysmalloc
	movq	%rax, PL_markstack(%rip)
	movq	%rax, PL_markstack_ptr(%rip)
	addq	$256, %rax              # imm = 0x100
	movq	%rax, PL_markstack_max(%rip)
	movq	$256, %rdi              # imm = 0x100
	callq	Perl_safesysmalloc
	movq	%rax, PL_scopestack(%rip)
	movq	$0, PL_scopestack_ix(%rip)
	movq	$32, PL_scopestack_max(%rip)
	movq	$1024, %rdi             # imm = 0x400
	callq	Perl_safesysmalloc
	movq	%rax, PL_savestack(%rip)
	movq	$0, PL_savestack_ix(%rip)
	movq	$128, PL_savestack_max(%rip)
	movq	$128, %rdi
	callq	Perl_safesysmalloc
	movq	%rax, PL_retstack(%rip)
	movq	$0, PL_retstack_ix(%rip)
	movq	$16, PL_retstack_max(%rip)
	popq	%rax
	retq
.Ltmp14:
	.size	Perl_init_stacks, .Ltmp14-Perl_init_stacks
	.cfi_endproc

	.globl	Perl_nothreadhook
	.align	16, 0x90
	.type	Perl_nothreadhook,@function
Perl_nothreadhook:                      # @Perl_nothreadhook
	.cfi_startproc
# BB#0:                                 # %entry
	xorq	%rax, %rax
	retq
.Ltmp15:
	.size	Perl_nothreadhook, .Ltmp15-Perl_nothreadhook
	.cfi_endproc

	.globl	perl_destruct
	.align	16, 0x90
	.type	perl_destruct,@function
perl_destruct:                          # @perl_destruct
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp22:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp23:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp24:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp25:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp26:
	.cfi_def_cfa_offset 48
	subq	$240, %rsp
.Ltmp27:
	.cfi_def_cfa_offset 288
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
	movl	PL_perl_destruct_level(%rip), %eax
	movl	%eax, 236(%rsp)
	movzbl	PL_exit_flags(%rip), %eax
	testq	$2, %rax
	je	.LBB4_5
# BB#1:                                 # %if.then
	movq	PL_top_env(%rip), %rax
	movq	%rax, 216(%rsp)
	leaq	16(%rsp), %rbx
	xorq	%rsi, %rsi
	xorq	%r14, %r14
	movq	%rbx, %rdi
	callq	sigsetjmp
	movq	PL_endav(%rip), %rsi
	movl	%eax, 224(%rsp)
	movq	%rbx, PL_top_env(%rip)
	movb	%r14b, 228(%rsp)
	testq	%rsi, %rsi
	je	.LBB4_4
# BB#2:                                 # %if.then
	movzbl	PL_minus_c(%rip), %eax
	testq	$255, %rax
	jne	.LBB4_4
# BB#3:                                 # %if.then4
	movq	PL_scopestack_ix(%rip), %rdi
	callq	Perl_call_list
.LBB4_4:                                # %if.end
	movq	216(%rsp), %rax
	movq	%rax, PL_top_env(%rip)
.LBB4_5:                                # %if.end7
	callq	Perl_pop_scope
	movq	PL_tmps_ix(%rip), %rax
	movq	PL_tmps_floor(%rip), %rcx
	cmpq	%rcx, %rax
	jle	.LBB4_7
# BB#6:                                 # %if.then9
	callq	Perl_free_tmps
.LBB4_7:                                # %if.end10
	callq	Perl_my_fflush_all
	callq	*PL_threadhook(%rip)
	movabsq	$4294967295, %r14       # imm = 0xFFFFFFFF
	testq	%r14, %rax
	jne	.LBB4_84
# BB#8:                                 # %if.end16
	movq	PL_main_root(%rip), %rdi
	testq	%rdi, %rdi
	je	.LBB4_12
# BB#9:                                 # %if.then18
	movq	PL_main_cv(%rip), %rax
	movq	(%rax), %rax
	movq	120(%rax), %rax
	testq	%rax, %rax
	je	.LBB4_11
# BB#10:                                # %if.then20
	movq	(%rax), %rax
	movq	(%rax), %rax
	movq	8(%rax), %rax
	movq	%rax, PL_comppad(%rip)
	movq	(%rax), %rax
	movq	(%rax), %rax
	movq	%rax, PL_curpad(%rip)
.LBB4_11:                               # %if.end26
	callq	Perl_op_free
	movq	$0, PL_main_root(%rip)
.LBB4_12:                               # %if.end27
	movq	$PL_compiling, PL_curcop(%rip)
	movq	PL_main_cv(%rip), %rdi
	movq	$0, PL_main_start(%rip)
	callq	Perl_sv_free
	movq	$0, PL_main_cv(%rip)
	movq	$1, %rax
	movb	%al, PL_dirty(%rip)
	callq	PerlIO_destruct
	cmpq	$0, PL_sv_objcount(%rip)
	je	.LBB4_14
# BB#13:                                # %if.then29
	xorq	%rax, %rax
	callq	sv_clean_objs
	movq	$0, PL_sv_objcount(%rip)
.LBB4_14:                               # %if.end31
	movq	PL_warnhook(%rip), %rdi
	callq	Perl_sv_free
	movq	PL_diehook(%rip), %rdi
	movq	$0, PL_warnhook(%rip)
	callq	Perl_sv_free
	movq	PL_exitlistlen(%rip), %rcx
	movq	$0, PL_diehook(%rip)
	jmp	.LBB4_16
	.align	16, 0x90
.LBB4_15:                               # %while.body
                                        #   in Loop: Header=BB4_16 Depth=1
	movq	$4, %rcx
	shlq	%cl, %rdx
	movq	8(%rax,%rdx), %rdi
	callq	*(%rax,%rdx)
	movq	PL_exitlistlen(%rip), %rcx
.LBB4_16:                               # %while.body
                                        # =>This Inner Loop Header: Depth=1
	movq	PL_exitlist(%rip), %rax
	testq	%rcx, %rcx
	leaq	-1(%rcx), %rdx
	movq	%rdx, PL_exitlistlen(%rip)
	jg	.LBB4_15
# BB#17:                                # %while.end
	movq	%rax, %rdi
	callq	Perl_safesysfree
	movq	$0, PL_exitlist(%rip)
	movq	$0, PL_exitlistlen(%rip)
	movl	236(%rsp), %eax
	testq	%rax, %rax
	je	.LBB4_18
# BB#19:                                # %if.end40
	movq	environ(%rip), %rdi
	movq	PL_origenviron(%rip), %rax
	cmpq	%rax, %rdi
	je	.LBB4_25
# BB#20:                                # %if.end40
	movl	PL_use_safe_putenv(%rip), %eax
	testq	%r14, %rax
	jne	.LBB4_25
# BB#21:                                # %for.cond.preheader
	movq	(%rdi), %rax
	testq	%rax, %rax
	je	.LBB4_24
# BB#22:
	movq	$8, %rbx
	.align	16, 0x90
.LBB4_23:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	%rax, %rdi
	callq	Perl_safesysfree
	movq	environ(%rip), %rdi
	movq	(%rdi,%rbx), %rax
	addq	$8, %rbx
	testq	%rax, %rax
	jne	.LBB4_23
.LBB4_24:                               # %for.end
	callq	Perl_safesysfree
	movq	PL_origenviron(%rip), %rax
	movq	%rax, environ(%rip)
.LBB4_25:                               # %if.end50
	xorq	%rdi, %rdi
	xorq	%rax, %rax
	xorq	%rbx, %rbx
	callq	setdefout
	movq	PL_stashcache(%rip), %rdi
	callq	Perl_sv_free
	movq	$0, PL_stashcache(%rip)
	cmpq	$0, PL_rsfp(%rip)
	je	.LBB4_27
# BB#26:                                # %if.then53
	movq	PL_rsfp(%rip), %rdi
	callq	Perl_PerlIO_close
	movq	$0, PL_rsfp(%rip)
.LBB4_27:                               # %if.end55
	movq	PL_rsfp_filters(%rip), %rdi
	callq	Perl_sv_free
	movb	%bl, PL_preprocess(%rip)
	movb	%bl, PL_minus_n(%rip)
	movb	%bl, PL_minus_p(%rip)
	movb	%bl, PL_minus_l(%rip)
	movb	%bl, PL_minus_a(%rip)
	movb	%bl, PL_minus_F(%rip)
	movb	%bl, PL_doswitches(%rip)
	movb	%bl, PL_dowarn(%rip)
	movb	%bl, PL_doextract(%rip)
	movq	PL_inplace(%rip), %rdi
	movb	%bl, PL_sawampersand(%rip)
	movb	%bl, PL_unsafe(%rip)
	movq	$0, PL_rsfp_filters(%rip)
	callq	Perl_safesysfree
	movq	PL_patchlevel(%rip), %rdi
	movq	$0, PL_inplace(%rip)
	callq	Perl_sv_free
	movq	PL_e_script(%rip), %rdi
	testq	%rdi, %rdi
	je	.LBB4_29
# BB#28:                                # %if.then57
	callq	Perl_sv_free
	movq	$0, PL_e_script(%rip)
.LBB4_29:                               # %if.end58
	movq	PL_ofs_sv(%rip), %rdi
	movq	$0, PL_perldb(%rip)
	callq	Perl_sv_free
	movq	PL_ors_sv(%rip), %rdi
	movq	$0, PL_ofs_sv(%rip)
	callq	Perl_sv_free
	movq	PL_rs(%rip), %rdi
	movq	$0, PL_ors_sv(%rip)
	callq	Perl_sv_free
	movq	PL_osname(%rip), %rdi
	xorq	%rax, %rax
	movl	%eax, PL_multiline(%rip)
	movq	$0, PL_rs(%rip)
	callq	Perl_safesysfree
	movq	PL_statname(%rip), %rdi
	movq	$0, PL_osname(%rip)
	callq	Perl_sv_free
	movq	PL_lastscream(%rip), %rdi
	movq	$0, PL_statname(%rip)
	movq	$0, PL_statgv(%rip)
	callq	Perl_sv_free
	movq	PL_screamfirst(%rip), %rdi
	movq	$0, PL_lastscream(%rip)
	callq	Perl_safesysfree
	movq	PL_screamnext(%rip), %rdi
	movq	$0, PL_screamfirst(%rip)
	callq	Perl_safesysfree
	movq	PL_efloatbuf(%rip), %rdi
	movq	$0, PL_screamnext(%rip)
	callq	Perl_safesysfree
	movq	PL_beginav(%rip), %rdi
	movq	$0, PL_efloatbuf(%rip)
	movq	$0, PL_efloatsize(%rip)
	callq	Perl_sv_free
	movq	PL_beginav_save(%rip), %rdi
	callq	Perl_sv_free
	movq	PL_endav(%rip), %rdi
	callq	Perl_sv_free
	movq	PL_checkav(%rip), %rdi
	callq	Perl_sv_free
	movq	PL_checkav_save(%rip), %rdi
	callq	Perl_sv_free
	movq	PL_initav(%rip), %rdi
	callq	Perl_sv_free
	movq	$0, PL_beginav(%rip)
	movq	$0, PL_beginav_save(%rip)
	movq	$0, PL_endav(%rip)
	movq	$0, PL_checkav(%rip)
	movq	$0, PL_checkav_save(%rip)
	movq	$0, PL_initav(%rip)
	movq	$0, PL_envgv(%rip)
	movq	$0, PL_incgv(%rip)
	movq	$0, PL_hintgv(%rip)
	movq	$0, PL_errgv(%rip)
	movq	$0, PL_argvgv(%rip)
	movq	$0, PL_argvoutgv(%rip)
	movq	$0, PL_stdingv(%rip)
	movq	$0, PL_stderrgv(%rip)
	movq	$0, PL_last_in_gv(%rip)
	movq	$0, PL_replgv(%rip)
	movq	$0, PL_DBgv(%rip)
	movq	$0, PL_DBline(%rip)
	movq	$0, PL_DBsub(%rip)
	movq	$0, PL_DBsingle(%rip)
	movq	$0, PL_DBtrace(%rip)
	movq	$0, PL_DBsignal(%rip)
	movq	PL_argvout_stack(%rip), %rdi
	movq	$0, PL_DBcv(%rip)
	movq	$0, PL_dbargs(%rip)
	movq	$0, PL_debstash(%rip)
	callq	Perl_sv_free
	movq	PL_modglobal(%rip), %rdi
	movq	$0, PL_argvout_stack(%rip)
	callq	Perl_sv_free
	movq	PL_preambleav(%rip), %rdi
	movq	$0, PL_modglobal(%rip)
	callq	Perl_sv_free
	movq	PL_subname(%rip), %rdi
	movq	$0, PL_preambleav(%rip)
	callq	Perl_sv_free
	movq	PL_linestr(%rip), %rdi
	movq	$0, PL_subname(%rip)
	callq	Perl_sv_free
	movq	PL_pidstatus(%rip), %rdi
	movq	$0, PL_linestr(%rip)
	callq	Perl_sv_free
	movq	PL_toptarget(%rip), %rdi
	movq	$0, PL_pidstatus(%rip)
	callq	Perl_sv_free
	movq	PL_bodytarget(%rip), %rdi
	movq	$0, PL_toptarget(%rip)
	callq	Perl_sv_free
	movq	PL_utf8_alnum(%rip), %rdi
	movq	$0, PL_bodytarget(%rip)
	movq	$0, PL_formtarget(%rip)
	callq	Perl_sv_free
	movq	PL_utf8_alnumc(%rip), %rdi
	callq	Perl_sv_free
	movq	PL_utf8_ascii(%rip), %rdi
	callq	Perl_sv_free
	movq	PL_utf8_alpha(%rip), %rdi
	callq	Perl_sv_free
	movq	PL_utf8_space(%rip), %rdi
	callq	Perl_sv_free
	movq	PL_utf8_cntrl(%rip), %rdi
	callq	Perl_sv_free
	movq	PL_utf8_graph(%rip), %rdi
	callq	Perl_sv_free
	movq	PL_utf8_digit(%rip), %rdi
	callq	Perl_sv_free
	movq	PL_utf8_upper(%rip), %rdi
	callq	Perl_sv_free
	movq	PL_utf8_lower(%rip), %rdi
	callq	Perl_sv_free
	movq	PL_utf8_print(%rip), %rdi
	callq	Perl_sv_free
	movq	PL_utf8_punct(%rip), %rdi
	callq	Perl_sv_free
	movq	PL_utf8_xdigit(%rip), %rdi
	callq	Perl_sv_free
	movq	PL_utf8_mark(%rip), %rdi
	callq	Perl_sv_free
	movq	PL_utf8_toupper(%rip), %rdi
	callq	Perl_sv_free
	movq	PL_utf8_totitle(%rip), %rdi
	callq	Perl_sv_free
	movq	PL_utf8_tolower(%rip), %rdi
	callq	Perl_sv_free
	movq	PL_utf8_tofold(%rip), %rdi
	callq	Perl_sv_free
	movq	PL_utf8_idstart(%rip), %rdi
	callq	Perl_sv_free
	movq	PL_utf8_idcont(%rip), %rdi
	callq	Perl_sv_free
	movq	$0, PL_utf8_alnum(%rip)
	movq	$0, PL_utf8_alnumc(%rip)
	movq	$0, PL_utf8_ascii(%rip)
	movq	$0, PL_utf8_alpha(%rip)
	movq	$0, PL_utf8_space(%rip)
	movq	$0, PL_utf8_cntrl(%rip)
	movq	$0, PL_utf8_graph(%rip)
	movq	$0, PL_utf8_digit(%rip)
	movq	$0, PL_utf8_upper(%rip)
	movq	$0, PL_utf8_lower(%rip)
	movq	$0, PL_utf8_print(%rip)
	movq	$0, PL_utf8_punct(%rip)
	movq	$0, PL_utf8_xdigit(%rip)
	movq	$0, PL_utf8_mark(%rip)
	movq	$0, PL_utf8_toupper(%rip)
	movq	$0, PL_utf8_totitle(%rip)
	movq	$0, PL_utf8_tolower(%rip)
	movq	PL_compiling+88(%rip), %rdi
	movq	$0, PL_utf8_tofold(%rip)
	movq	$0, PL_utf8_idstart(%rip)
	movq	$0, PL_utf8_idcont(%rip)
	cmpq	$48, %rdi
	ja	.LBB4_31
# BB#30:                                # %if.end58
	movq	$1, %rax
	movq	%rdi, %rcx
	shlq	%cl, %rax
	movabsq	$281474993487873, %rcx  # imm = 0x1000001000001
	testq	%rcx, %rax
	jne	.LBB4_32
.LBB4_31:                               # %if.then66
	callq	Perl_sv_free
.LBB4_32:                               # %if.end67
	movq	$0, PL_compiling+88(%rip)
	movq	PL_compiling+96(%rip), %rdi
	testq	%rdi, %rdi
	je	.LBB4_34
# BB#33:                                # %if.then70
	callq	Perl_sv_free
.LBB4_34:                               # %if.end71
	movq	$0, PL_compiling+96(%rip)
	movq	PL_compiling+56(%rip), %rdi
	callq	Perl_sv_free
	movq	PL_defstash(%rip), %rdi
	movq	$0, PL_compiling+56(%rip)
	movq	$0, PL_defstash(%rip)
	callq	Perl_sv_free
	movq	PL_curstname(%rip), %rdi
	callq	Perl_sv_free
	movq	PL_errors(%rip), %rdi
	movq	$0, PL_curstname(%rip)
	callq	Perl_sv_free
	movq	PL_tmps_ix(%rip), %rax
	movq	PL_tmps_floor(%rip), %rcx
	movq	$0, PL_errors(%rip)
	cmpq	%rcx, %rax
	jle	.LBB4_36
# BB#35:                                # %if.then74
	callq	Perl_free_tmps
.LBB4_36:                               # %if.end75
	movslq	236(%rsp), %rax
	cmpq	$2, %rax
	jl	.LBB4_49
# BB#37:                                # %land.lhs.true78
	movq	PL_curcop(%rip), %rax
	cmpq	$0, 88(%rax)
	je	.LBB4_41
# BB#38:                                # %lor.lhs.false81
	movq	PL_curcop(%rip), %rax
	movq	88(%rax), %rax
	cmpq	$24, %rax
	je	.LBB4_41
# BB#39:                                # %lor.lhs.false85
	movq	PL_curcop(%rip), %rax
	movq	88(%rax), %rax
	cmpq	$48, %rax
	je	.LBB4_49
# BB#40:                                # %land.lhs.true89
	movq	PL_curcop(%rip), %rax
	movq	88(%rax), %rax
	movq	(%rax), %rax
	movq	(%rax), %rax
	movzbl	6(%rax), %eax
	testq	$1, %rax
	je	.LBB4_49
.LBB4_41:                               # %if.then96
	movq	PL_scopestack_ix(%rip), %rdx
	testq	%rdx, %rdx
	je	.LBB4_43
# BB#42:                                # %if.then99
	movabsq	$.L.str2, %rsi
	movq	$24, %rdi
	xorq	%rax, %rax
	callq	Perl_warner
.LBB4_43:                               # %if.end100
	movq	PL_savestack_ix(%rip), %rdx
	testq	%rdx, %rdx
	je	.LBB4_45
# BB#44:                                # %if.then103
	movabsq	$.L.str3, %rsi
	movq	$24, %rdi
	xorq	%rax, %rax
	callq	Perl_warner
.LBB4_45:                               # %if.end104
	movq	PL_tmps_floor(%rip), %rdx
	cmpq	$-1, %rdx
	je	.LBB4_47
# BB#46:                                # %if.then107
	incq	%rdx
	movabsq	$.L.str4, %rsi
	movq	$24, %rdi
	xorq	%rax, %rax
	callq	Perl_warner
.LBB4_47:                               # %if.end108
	movq	PL_curstackinfo(%rip), %rax
	movq	16(%rax), %rdx
	cmpq	$-1, %rdx
	je	.LBB4_49
# BB#48:                                # %if.then111
	incq	%rdx
	movabsq	$.L.str5, %rsi
	movq	$24, %rdi
	xorq	%rax, %rax
	callq	Perl_warner
.LBB4_49:                               # %if.end115
	movq	PL_fdpid(%rip), %rdi
	movq	16(%rdi), %rcx
	orq	$255, %rcx
	movq	%rcx, 16(%rdi)
	movq	PL_strtab(%rip), %rax
	movq	16(%rax), %rdx
	orq	$255, %rdx
	movq	%rdx, 16(%rax)
	movq	PL_sv_count(%rip), %rsi
	cmpq	$3, %rsi
	jl	.LBB4_53
	.align	16, 0x90
.LBB4_50:                               # %land.rhs
                                        # =>This Inner Loop Header: Depth=1
	xorq	%rax, %rax
	callq	sv_clean_all
	testq	%r14, %rax
	je	.LBB4_52
# BB#51:                                # %land.rhs
                                        #   in Loop: Header=BB4_50 Depth=1
	movq	PL_sv_count(%rip), %rax
	cmpq	$2, %rax
	jg	.LBB4_50
.LBB4_52:                               # %while.end124.loopexit
	movq	PL_fdpid(%rip), %rdi
	movq	PL_strtab(%rip), %rax
	movq	16(%rdi), %rcx
	movq	16(%rax), %rdx
.LBB4_53:                               # %while.end124
	andq	$-256, %rcx
	orq	$10, %rcx
	movq	%rcx, 16(%rdi)
	andq	$-256, %rdx
	orq	$11, %rdx
	movq	%rdx, 16(%rax)
	movq	(%rdi), %rax
	movzbl	72(%rax), %ecx
	andq	$254, %rcx
	movb	%cl, 72(%rax)
	callq	Perl_sv_free
	movq	PL_strtab(%rip), %rax
	movq	$0, PL_fdpid(%rip)
	movq	(%rax), %rax
	movq	(%rax), %rcx
	movq	%rcx, 8(%rsp)           # 8-byte Spill
	movq	16(%rax), %rax
	movq	%rax, (%rsp)            # 8-byte Spill
	xorq	%r13, %r13
	movabsq	$-6148914691236517205, %r12 # imm = 0xAAAAAAAAAAAAAAAB
	movabsq	$.L.str6, %r14
	movq	%rcx, %rax
	jmp	.LBB4_54
	.align	16, 0x90
.LBB4_63:                               # %if.end176
                                        #   in Loop: Header=BB4_54 Depth=1
	incq	%r13
	movq	8(%rsp), %rax           # 8-byte Reload
	leaq	(%rax,%r13,8), %rax
.LBB4_54:                               # %for.cond144.outer
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB4_55 Depth 2
	movq	(%rax), %r15
	.align	16, 0x90
.LBB4_55:                               # %for.cond144
                                        #   Parent Loop BB4_54 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	testq	%r15, %r15
	je	.LBB4_62
# BB#56:                                # %land.lhs.true146
                                        #   in Loop: Header=BB4_55 Depth=2
	movq	PL_curcop(%rip), %rax
	cmpq	$0, 88(%rax)
	je	.LBB4_60
# BB#57:                                # %lor.lhs.false150
                                        #   in Loop: Header=BB4_55 Depth=2
	movq	PL_curcop(%rip), %rax
	movq	88(%rax), %rax
	cmpq	$24, %rax
	je	.LBB4_60
# BB#58:                                # %lor.lhs.false154
                                        #   in Loop: Header=BB4_55 Depth=2
	movq	PL_curcop(%rip), %rax
	movq	88(%rax), %rax
	cmpq	$48, %rax
	je	.LBB4_61
# BB#59:                                # %land.lhs.true158
                                        #   in Loop: Header=BB4_55 Depth=2
	movq	PL_curcop(%rip), %rax
	movq	88(%rax), %rax
	movq	(%rax), %rax
	movq	(%rax), %rax
	movzbl	6(%rax), %eax
	testq	$1, %rax
	je	.LBB4_61
	.align	16, 0x90
.LBB4_60:                               # %if.then166
                                        #   in Loop: Header=BB4_55 Depth=2
	movq	16(%r15), %rdx
	movq	8(%r15), %rbx
	movq	$3, %rcx
	sarq	%cl, %rdx
	imulq	%r12, %rdx
	addq	$16, %rbx
	movq	$24, %rdi
	xorq	%rax, %rax
	movq	%r14, %rsi
	movq	%rbx, %rcx
	callq	Perl_warner
	movq	$0, 16(%r15)
	movq	(%r15), %r15
.LBB4_61:                               # %if.end169
                                        #   in Loop: Header=BB4_55 Depth=2
	testq	%r15, %r15
	jne	.LBB4_55
.LBB4_62:                               # %if.then171
                                        #   in Loop: Header=BB4_54 Depth=1
	movq	(%rsp), %rax            # 8-byte Reload
	cmpq	%rax, %r13
	jl	.LBB4_63
# BB#64:                                # %for.end179
	movq	PL_strtab(%rip), %rdi
	callq	Perl_sv_free
	movq	$0, PL_sv_yes+8(%rip)
	movabsq	$PL_sv_yes, %rdi
	callq	Perl_sv_clear
	movq	$0, PL_sv_yes(%rip)
	movq	$0, PL_sv_yes+16(%rip)
	movq	$0, PL_sv_no+8(%rip)
	movabsq	$PL_sv_no, %rdi
	callq	Perl_sv_clear
	movq	$0, PL_sv_no(%rip)
	movq	$0, PL_sv_no+16(%rip)
	movq	$0, PL_debug_pad+8(%rip)
	movabsq	$PL_debug_pad, %rdi
	callq	Perl_sv_clear
	movq	$0, PL_debug_pad(%rip)
	movq	$0, PL_debug_pad+16(%rip)
	movq	$0, PL_debug_pad+32(%rip)
	movabsq	$PL_debug_pad+24, %rdi
	callq	Perl_sv_clear
	movq	$0, PL_debug_pad+24(%rip)
	movq	$0, PL_debug_pad+40(%rip)
	movq	$0, PL_debug_pad+56(%rip)
	movabsq	$PL_debug_pad+48, %rdi
	callq	Perl_sv_clear
	cmpq	$0, PL_sv_count(%rip)
	movq	$0, PL_debug_pad+48(%rip)
	movq	$0, PL_debug_pad+64(%rip)
	je	.LBB4_70
# BB#65:                                # %land.lhs.true200
	movq	PL_curcop(%rip), %rax
	cmpq	$0, 88(%rax)
	je	.LBB4_69
# BB#66:                                # %lor.lhs.false204
	movq	PL_curcop(%rip), %rax
	movq	88(%rax), %rax
	cmpq	$24, %rax
	je	.LBB4_69
# BB#67:                                # %lor.lhs.false208
	movq	PL_curcop(%rip), %rax
	movq	88(%rax), %rax
	cmpq	$48, %rax
	je	.LBB4_70
# BB#68:                                # %land.lhs.true212
	movq	PL_curcop(%rip), %rax
	movq	88(%rax), %rax
	movq	(%rax), %rax
	movq	(%rax), %rax
	movzbl	6(%rax), %eax
	testq	$1, %rax
	je	.LBB4_70
.LBB4_69:                               # %if.then220
	movq	PL_sv_count(%rip), %rdx
	movabsq	$.L.str7, %rsi
	movq	$24, %rdi
	xorq	%rax, %rax
	callq	Perl_warner
.LBB4_70:                               # %if.end221
	movq	$0, PL_sv_count(%rip)
	callq	PerlIO_cleanup
	movq	$0, PL_sv_undef+8(%rip)
	andq	$-8388609, PL_sv_undef+16(%rip) # imm = 0xFFFFFFFFFF7FFFFF
	movq	PL_origfilename(%rip), %rdi
	callq	Perl_safesysfree
	movq	PL_reg_start_tmp(%rip), %rdi
	movq	$0, PL_origfilename(%rip)
	callq	Perl_safesysfree
	movq	PL_reg_curpm(%rip), %rdi
	movq	$0, PL_reg_start_tmp(%rip)
	movq	$0, PL_reg_start_tmpl(%rip)
	testq	%rdi, %rdi
	je	.LBB4_72
# BB#71:                                # %if.then224
	callq	Perl_safesysfree
.LBB4_72:                               # %if.end225
	movq	PL_reg_poscache(%rip), %rdi
	callq	Perl_safesysfree
	xorq	%rax, %rax
	xorq	%rbx, %rbx
	callq	free_tied_hv_pool
	movq	PL_op_mask(%rip), %rdi
	callq	Perl_safesysfree
	movq	PL_psig_ptr(%rip), %rdi
	callq	Perl_safesysfree
	movq	PL_psig_name(%rip), %rdi
	movq	$0, PL_psig_ptr(%rip)
	callq	Perl_safesysfree
	movq	PL_bitcount(%rip), %rdi
	movq	$0, PL_psig_name(%rip)
	callq	Perl_safesysfree
	movq	PL_psig_pend(%rip), %rdi
	movq	$0, PL_bitcount(%rip)
	callq	Perl_safesysfree
	movq	PL_ofmt(%rip), %rdi
	movq	$0, PL_psig_pend(%rip)
	movq	$0, PL_formfeed(%rip)
	callq	Perl_safesysfree
	movq	$0, PL_ofmt(%rip)
	xorq	%rax, %rax
	callq	nuke_stacks
	movb	%bl, PL_tainting(%rip)
	movb	%bl, PL_taint_warn(%rip)
	movq	$0, PL_hints(%rip)
	movq	$0, PL_debug(%rip)
	xorq	%rax, %rax
	callq	sv_free_arenas
	movq	PL_mess_sv(%rip), %rdi
	testq	%rdi, %rdi
	je	.LBB4_84
# BB#73:                                # %if.then230
	movq	16(%rdi), %rax
	movq	%rax, %rcx
	andq	$255, %rcx
	cmpq	$7, %rcx
	jb	.LBB4_81
# BB#74:                                # %if.then235
	movq	(%rdi), %rcx
	movq	40(%rcx), %r14
	testq	%r14, %r14
	je	.LBB4_81
	.align	16, 0x90
.LBB4_75:                               # %for.body241
                                        # =>This Inner Loop Header: Depth=1
	movq	%r14, %rbx
	movq	32(%rbx), %rdi
	movq	(%rbx), %r14
	testq	%rdi, %rdi
	je	.LBB4_79
# BB#76:                                # %land.lhs.true243
                                        #   in Loop: Header=BB4_75 Depth=1
	movzbl	18(%rbx), %eax
	cmpq	$103, %rax
	je	.LBB4_79
# BB#77:                                # %land.lhs.true247
                                        #   in Loop: Header=BB4_75 Depth=1
	cmpq	$0, 40(%rbx)
	js	.LBB4_79
# BB#78:                                # %if.then250
                                        #   in Loop: Header=BB4_75 Depth=1
	callq	Perl_safesysfree
	.align	16, 0x90
.LBB4_79:                               # %if.end252
                                        #   in Loop: Header=BB4_75 Depth=1
	movq	%rbx, %rdi
	callq	Perl_safesysfree
	testq	%r14, %r14
	jne	.LBB4_75
# BB#80:                                # %for.cond239.if.end255.loopexit_crit_edge
	movq	PL_mess_sv(%rip), %rdi
	movq	16(%rdi), %rax
.LBB4_81:                               # %if.end255
	testq	$2097152, %rax          # imm = 0x200000
	je	.LBB4_83
# BB#82:                                # %land.rhs259
	callq	Perl_sv_backoff
	movq	PL_mess_sv(%rip), %rdi
.LBB4_83:                               # %land.end262
	movq	(%rdi), %rax
	movq	(%rax), %rdi
	callq	Perl_safesysfree
	movq	PL_mess_sv(%rip), %rax
	movq	(%rax), %rdi
	callq	Perl_safesysfree
	movq	PL_mess_sv(%rip), %rdi
	callq	Perl_safesysfree
	movq	$0, PL_mess_sv(%rip)
	jmp	.LBB4_84
.LBB4_18:                               # %if.then38
	callq	PerlIO_cleanup
.LBB4_84:                               # %return
	movq	PL_statusvalue(%rip), %rax
	addq	$240, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp33:
	.size	perl_destruct, .Ltmp33-perl_destruct
	.cfi_endproc

	.globl	Perl_call_list
	.align	16, 0x90
	.type	Perl_call_list,@function
Perl_call_list:                         # @Perl_call_list
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
	subq	$240, %rsp
.Ltmp45:
	.cfi_def_cfa_offset 288
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
	movq	%rsi, %r14
	movq	%rdi, %r12
	movq	PL_curcop(%rip), %rax
	movq	80(%rax), %rax
	movq	%rax, 8(%rsp)           # 8-byte Spill
	leaq	16(%rsp), %r15
	jmp	.LBB5_1
	.align	16, 0x90
.LBB5_53:                               # %sw.epilog
                                        #   in Loop: Header=BB5_1 Depth=1
	movq	216(%rsp), %rax
	movq	%rax, PL_top_env(%rip)
.LBB5_1:                                # %while.cond
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB5_31 Depth 2
                                        #     Child Loop BB5_27 Depth 2
	movzbl	17(%r14), %eax
	testq	$128, %rax
	jne	.LBB5_2
# BB#3:                                 # %cond.false
                                        #   in Loop: Header=BB5_1 Depth=1
	movq	(%r14), %rax
	movq	8(%rax), %rax
	jmp	.LBB5_4
	.align	16, 0x90
.LBB5_2:                                # %cond.true
                                        #   in Loop: Header=BB5_1 Depth=1
	movq	%r14, %rdi
	callq	Perl_mg_size
.LBB5_4:                                # %cond.end
                                        #   in Loop: Header=BB5_1 Depth=1
	testq	%rax, %rax
	js	.LBB5_54
# BB#5:                                 # %while.body
                                        #   in Loop: Header=BB5_1 Depth=1
	movq	%r14, %rdi
	callq	Perl_av_shift
	movzbl	PL_savebegin(%rip), %ecx
	movq	%rax, %rbx
	testq	%rcx, %rcx
	jne	.LBB5_6
# BB#13:                                # %if.else16
                                        #   in Loop: Header=BB5_1 Depth=1
	movq	%rbx, %rdi
	callq	Perl_save_freesv
	jmp	.LBB5_14
	.align	16, 0x90
.LBB5_6:                                # %if.then
                                        #   in Loop: Header=BB5_1 Depth=1
	movq	PL_beginav(%rip), %rax
	cmpq	%r14, %rax
	jne	.LBB5_10
# BB#7:                                 # %if.then4
                                        #   in Loop: Header=BB5_1 Depth=1
	movq	PL_beginav_save(%rip), %rdi
	testq	%rdi, %rdi
	jne	.LBB5_9
# BB#8:                                 # %if.then6
                                        #   in Loop: Header=BB5_1 Depth=1
	callq	Perl_newAV
	movq	%rax, %rdi
	movq	%rdi, PL_beginav_save(%rip)
	jmp	.LBB5_9
	.align	16, 0x90
.LBB5_10:                               # %if.else
                                        #   in Loop: Header=BB5_1 Depth=1
	movq	PL_checkav(%rip), %rax
	cmpq	%r14, %rax
	jne	.LBB5_14
# BB#11:                                # %if.then9
                                        #   in Loop: Header=BB5_1 Depth=1
	movq	PL_checkav_save(%rip), %rdi
	testq	%rdi, %rdi
	jne	.LBB5_9
# BB#12:                                # %if.then11
                                        #   in Loop: Header=BB5_1 Depth=1
	callq	Perl_newAV
	movq	%rax, %rdi
	movq	%rdi, PL_checkav_save(%rip)
	.align	16, 0x90
.LBB5_9:                                # %if.end
                                        #   in Loop: Header=BB5_1 Depth=1
	movq	%rbx, %rsi
	callq	Perl_av_push
.LBB5_14:                               # %if.end17
                                        #   in Loop: Header=BB5_1 Depth=1
	movq	PL_top_env(%rip), %rax
	movq	%rax, 216(%rsp)
	xorq	%rsi, %rsi
	movq	%r15, %rdi
	callq	sigsetjmp
	movl	%eax, 224(%rsp)
	movq	%r15, PL_top_env(%rip)
	xorq	%rcx, %rcx
	movb	%cl, 228(%rsp)
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	cmpq	$3, %rax
	ja	.LBB5_53
# BB#15:                                # %if.end17
                                        #   in Loop: Header=BB5_1 Depth=1
	jmpq	*.LJTI5_0(,%rax,8)
.LBB5_16:                               # %sw.bb
                                        #   in Loop: Header=BB5_1 Depth=1
	xorq	%rax, %rax
	movq	%rbx, %rdi
	callq	call_list_body
	movq	PL_errgv(%rip), %rax
	movq	(%rax), %rax
	movq	56(%rax), %rax
	movq	(%rax), %r13
	movzbl	18(%r13), %eax
	testq	$4, %rax
	jne	.LBB5_17
# BB#18:                                # %cond.false28
                                        #   in Loop: Header=BB5_1 Depth=1
	movq	$2, %rdx
	movq	%r13, %rdi
	leaq	232(%rsp), %rsi
	callq	Perl_sv_2pv_flags
	movq	232(%rsp), %rax
	jmp	.LBB5_19
.LBB5_29:                               # %sw.bb52
                                        #   in Loop: Header=BB5_1 Depth=1
	movq	$1, PL_statusvalue(%rip)
	jmp	.LBB5_31
	.align	16, 0x90
.LBB5_30:                               # %while.body56
                                        #   in Loop: Header=BB5_31 Depth=2
	callq	Perl_pop_scope
.LBB5_31:                               # %while.body56
                                        #   Parent Loop BB5_1 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	PL_scopestack_ix(%rip), %rax
	cmpq	%r12, %rax
	jg	.LBB5_30
# BB#32:                                # %while.end57
                                        #   in Loop: Header=BB5_1 Depth=1
	movq	PL_tmps_ix(%rip), %rax
	movq	PL_tmps_floor(%rip), %rcx
	cmpq	%rcx, %rax
	jle	.LBB5_34
# BB#33:                                # %if.then59
                                        #   in Loop: Header=BB5_1 Depth=1
	callq	Perl_free_tmps
.LBB5_34:                               # %if.end60
                                        #   in Loop: Header=BB5_1 Depth=1
	movq	PL_defstash(%rip), %rax
	movq	%rax, PL_curstash(%rip)
	movq	$PL_compiling, PL_curcop(%rip)
	movq	PL_curcop(%rip), %rax
	movq	8(%rsp), %rcx           # 8-byte Reload
	movq	%rcx, 80(%rax)
	movq	216(%rsp), %rax
	cmpq	$0, PL_statusvalue(%rip)
	movq	%rax, PL_top_env(%rip)
	je	.LBB5_42
# BB#35:                                # %land.lhs.true
                                        #   in Loop: Header=BB5_1 Depth=1
	movzbl	PL_exit_flags(%rip), %eax
	testq	$1, %rax
	jne	.LBB5_42
# BB#36:                                # %if.then67
                                        #   in Loop: Header=BB5_1 Depth=1
	movq	PL_beginav(%rip), %rax
	cmpq	%r14, %rax
	jne	.LBB5_38
# BB#37:                                # %if.then70
                                        #   in Loop: Header=BB5_1 Depth=1
	xorq	%rax, %rax
	movabsq	$.L.str53, %rdi
	callq	Perl_croak
	jmp	.LBB5_42
.LBB5_17:                               # %cond.true25
                                        #   in Loop: Header=BB5_1 Depth=1
	movq	(%r13), %rax
	movq	8(%rax), %rax
	movq	%rax, 232(%rsp)
.LBB5_19:                               # %cond.end30
                                        #   in Loop: Header=BB5_1 Depth=1
	testq	%rax, %rax
	je	.LBB5_53
# BB#20:                                # %if.then33
                                        #   in Loop: Header=BB5_1 Depth=1
	movq	$PL_compiling, PL_curcop(%rip)
	movq	PL_curcop(%rip), %rax
	movq	8(%rsp), %rcx           # 8-byte Reload
	movq	%rcx, 80(%rax)
	movq	PL_beginav(%rip), %rax
	cmpq	%r14, %rax
	jne	.LBB5_22
# BB#21:                                # %if.then36
                                        #   in Loop: Header=BB5_1 Depth=1
	movq	%r13, %rdi
	movabsq	$.L.str53, %rsi
	callq	Perl_sv_catpv
	jmp	.LBB5_27
.LBB5_38:                               # %if.else71
                                        #   in Loop: Header=BB5_1 Depth=1
	movq	PL_checkav(%rip), %rax
	cmpq	%r14, %rax
	movabsq	$.L.str55, %rsi
	je	.LBB5_41
# BB#39:                                # %cond.false75
                                        #   in Loop: Header=BB5_1 Depth=1
	movq	PL_initav(%rip), %rax
	cmpq	%r14, %rax
	movabsq	$.L.str56, %rsi
	je	.LBB5_41
# BB#40:                                # %select.mid24
                                        #   in Loop: Header=BB5_1 Depth=1
	movabsq	$.L.str57, %rsi
.LBB5_41:                               # %cond.end79
                                        #   in Loop: Header=BB5_1 Depth=1
	xorq	%rax, %rax
	movabsq	$.L.str54, %rdi
	callq	Perl_croak
.LBB5_42:                               # %if.end82
                                        #   in Loop: Header=BB5_1 Depth=1
	xorq	%rax, %rax
	callq	my_exit_jump
.LBB5_43:                               # %sw.bb84
                                        #   in Loop: Header=BB5_1 Depth=1
	cmpq	$0, PL_restartop(%rip)
	jne	.LBB5_44
# BB#46:                                # %if.end96
                                        #   in Loop: Header=BB5_1 Depth=1
	movq	PL_stderrgv(%rip), %rax
	testq	%rax, %rax
	je	.LBB5_50
# BB#47:                                # %land.lhs.true98
                                        #   in Loop: Header=BB5_1 Depth=1
	movzbl	16(%rax), %ecx
	cmpq	$13, %rcx
	jne	.LBB5_50
# BB#48:                                # %land.lhs.true103
                                        #   in Loop: Header=BB5_1 Depth=1
	movq	(%rax), %rax
	movq	56(%rax), %rax
	movq	16(%rax), %rax
	testq	%rax, %rax
	je	.LBB5_50
# BB#49:                                # %land.lhs.true107
                                        #   in Loop: Header=BB5_1 Depth=1
	movq	(%rax), %rax
	movq	64(%rax), %rdi
	testq	%rdi, %rdi
	jne	.LBB5_51
	.align	16, 0x90
.LBB5_50:                               # %cond.false119
                                        #   in Loop: Header=BB5_1 Depth=1
	callq	Perl_PerlIO_stderr
	movq	%rax, %rdi
.LBB5_51:                               # %cond.end121
                                        #   in Loop: Header=BB5_1 Depth=1
	xorq	%rax, %rax
	movabsq	$.L.str11, %rsi
	callq	PerlIO_printf
	movq	PL_tmps_ix(%rip), %rax
	movq	PL_tmps_floor(%rip), %rcx
	cmpq	%rcx, %rax
	jle	.LBB5_53
# BB#52:                                # %if.then126
                                        #   in Loop: Header=BB5_1 Depth=1
	callq	Perl_free_tmps
	jmp	.LBB5_53
.LBB5_22:                               # %if.else37
                                        #   in Loop: Header=BB5_1 Depth=1
	movq	PL_checkav(%rip), %rax
	cmpq	%r14, %rax
	movabsq	$.L.str55, %rdx
	je	.LBB5_25
# BB#23:                                # %cond.false40
                                        #   in Loop: Header=BB5_1 Depth=1
	movq	PL_initav(%rip), %rax
	cmpq	%r14, %rax
	movabsq	$.L.str56, %rdx
	je	.LBB5_25
# BB#24:                                # %select.mid
                                        #   in Loop: Header=BB5_1 Depth=1
	movabsq	$.L.str57, %rdx
.LBB5_25:                               # %cond.end43
                                        #   in Loop: Header=BB5_1 Depth=1
	xorq	%rax, %rax
	movq	%r13, %rdi
	movabsq	$.L.str54, %rsi
	callq	Perl_sv_catpvf
	jmp	.LBB5_27
	.align	16, 0x90
.LBB5_26:                               # %while.body48
                                        #   in Loop: Header=BB5_27 Depth=2
	callq	Perl_pop_scope
.LBB5_27:                               # %while.body48
                                        #   Parent Loop BB5_1 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	PL_scopestack_ix(%rip), %rax
	cmpq	%r12, %rax
	jg	.LBB5_26
# BB#28:                                # %while.end
                                        #   in Loop: Header=BB5_1 Depth=1
	movq	216(%rsp), %rax
	movq	%rax, PL_top_env(%rip)
	xorq	%rax, %rax
	movabsq	$.L.str58, %rdi
	movq	%r13, %rsi
	callq	Perl_croak
	jmp	.LBB5_53
.LBB5_54:                               # %while.end130
	addq	$240, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB5_44:                               # %if.then86
	movq	$PL_compiling, PL_curcop(%rip)
	movq	PL_curcop(%rip), %rax
	movq	8(%rsp), %rcx           # 8-byte Reload
	movq	%rcx, 80(%rax)
	movq	PL_top_env(%rip), %rax
	cmpq	$0, 200(%rax)
	jne	.LBB5_55
# BB#45:                                # %if.end93
	callq	Perl_PerlIO_stderr
	movq	%rax, %rcx
	movabsq	$.L.str9, %rdx
	xorq	%rax, %rax
	movq	%rcx, %rdi
	movq	%rdx, %rsi
	callq	PerlIO_printf
	movq	$1, %rdi
	callq	exit
.LBB5_55:                               # %if.then90
	movq	$3, %rsi
	movq	%rax, %rdi
	callq	siglongjmp
.Ltmp51:
	.size	Perl_call_list, .Ltmp51-Perl_call_list
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI5_0:
	.quad	.LBB5_16
	.quad	.LBB5_29
	.quad	.LBB5_31
	.quad	.LBB5_43

	.text
	.globl	perl_free
	.align	16, 0x90
	.type	perl_free,@function
perl_free:                              # @perl_free
	.cfi_startproc
# BB#0:                                 # %entry
	jmp	free  # TAILCALL
.Ltmp52:
	.size	perl_free, .Ltmp52-perl_free
	.cfi_endproc

	.globl	Perl_call_atexit
	.align	16, 0x90
	.type	Perl_call_atexit,@function
Perl_call_atexit:                       # @Perl_call_atexit
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp56:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp57:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp58:
	.cfi_def_cfa_offset 32
.Ltmp59:
	.cfi_offset %rbx, -24
.Ltmp60:
	.cfi_offset %r14, -16
	movq	PL_exitlistlen(%rip), %rax
	movq	%rsi, %r14
	movq	%rdi, %rbx
	movq	PL_exitlist(%rip), %rdi
	movq	$4, %rcx
	shlq	%cl, %rax
	addq	$16, %rax
	movq	%rax, %rsi
	callq	Perl_safesysrealloc
	movq	PL_exitlistlen(%rip), %rdx
	movq	%rax, PL_exitlist(%rip)
	movq	$4, %rcx
	movq	%rdx, %rsi
	shlq	%cl, %rsi
	movq	%rbx, (%rax,%rsi)
	movq	PL_exitlist(%rip), %rax
	movq	%r14, 8(%rax,%rsi)
	incq	%rdx
	movq	%rdx, PL_exitlistlen(%rip)
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp61:
	.size	Perl_call_atexit, .Ltmp61-Perl_call_atexit
	.cfi_endproc

	.globl	perl_parse
	.align	16, 0x90
	.type	perl_parse,@function
perl_parse:                             # @perl_parse
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp68:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp69:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp70:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp71:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp72:
	.cfi_def_cfa_offset 48
	subq	$240, %rsp
.Ltmp73:
	.cfi_def_cfa_offset 288
.Ltmp74:
	.cfi_offset %rbx, -48
.Ltmp75:
	.cfi_offset %r12, -40
.Ltmp76:
	.cfi_offset %r13, -32
.Ltmp77:
	.cfi_offset %r14, -24
.Ltmp78:
	.cfi_offset %r15, -16
	movzbl	PL_rehash_seed_set(%rip), %eax
	movq	%r8, 16(%rsp)           # 8-byte Spill
	movq	%rcx, %r13
	movq	%rdx, %r12
	movq	%rsi, 8(%rsp)           # 8-byte Spill
	testq	%rax, %rax
	jne	.LBB8_2
# BB#1:                                 # %if.then
	xorq	%rax, %rax
	callq	get_hash_seed
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	%rax, PL_rehash_seed(%rip)
.LBB8_2:                                # %if.end
	movl	%r12d, PL_origargc(%rip)
	movq	$32, %rcx
	movq	%r12, %rax
	movq	%r13, PL_origargv(%rip)
	movq	(%r13), %r14
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	%r14, %r15
	andq	$-4, %r15
	xorq	%rbx, %rbx
	testq	%rax, %rax
	jle	.LBB8_19
# BB#3:                                 # %if.end
	testq	%r13, %r13
	je	.LBB8_19
# BB#4:                                 # %if.end
	testq	%r14, %r14
	je	.LBB8_19
# BB#5:                                 # %while.cond.preheader
	leaq	-1(%r14), %rbx
	.align	16, 0x90
.LBB8_6:                                # %while.cond
                                        # =>This Inner Loop Header: Depth=1
	movzbl	1(%rbx), %eax
	incq	%rbx
	testq	%rax, %rax
	jne	.LBB8_6
# BB#7:                                 # %for.cond.preheader
	movq	$32, %rcx
	movq	%r12, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$2, %rax
	jl	.LBB8_19
# BB#8:                                 # %for.body.lr.ph
	movq	$1, %rax
	cmpq	%r14, %r15
	jne	.LBB8_15
	.align	16, 0x90
.LBB8_9:                                # %for.body.us
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB8_13 Depth 2
	movq	(%r13,%rax,8), %rcx
	leaq	1(%rbx), %rdx
	cmpq	%rdx, %rcx
	je	.LBB8_12
# BB#10:                                # %lor.lhs.false.us
                                        #   in Loop: Header=BB8_9 Depth=1
	cmpq	%rbx, %rcx
	jbe	.LBB8_19
# BB#11:                                # %land.lhs.true25.us
                                        #   in Loop: Header=BB8_9 Depth=1
	leaq	4(%rbx), %rdx
	andq	$-4, %rdx
	cmpq	%rdx, %rcx
	ja	.LBB8_19
.LBB8_12:                               # %while.cond35.us.preheader
                                        #   in Loop: Header=BB8_9 Depth=1
	decq	%rcx
	movq	%rcx, %rbx
	.align	16, 0x90
.LBB8_13:                               # %while.cond35.us
                                        #   Parent Loop BB8_9 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movzbl	1(%rbx), %ecx
	incq	%rbx
	testq	%rcx, %rcx
	jne	.LBB8_13
# BB#14:                                # %for.inc.us
                                        #   in Loop: Header=BB8_9 Depth=1
	incq	%rax
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	$32, %rcx
	movq	%r12, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	cmpq	%rsi, %rdx
	jl	.LBB8_9
	jmp	.LBB8_19
	.align	16, 0x90
.LBB8_15:                               # %for.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB8_17 Depth 2
	movq	(%r13,%rax,8), %rcx
	leaq	1(%rbx), %rdx
	cmpq	%rdx, %rcx
	jne	.LBB8_19
# BB#16:                                # %while.cond35.preheader
                                        #   in Loop: Header=BB8_15 Depth=1
	decq	%rcx
	movq	%rcx, %rbx
	.align	16, 0x90
.LBB8_17:                               # %while.cond35
                                        #   Parent Loop BB8_15 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movzbl	1(%rbx), %ecx
	incq	%rbx
	testq	%rcx, %rcx
	jne	.LBB8_17
# BB#18:                                # %for.inc
                                        #   in Loop: Header=BB8_15 Depth=1
	incq	%rax
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	$32, %rcx
	movq	%r12, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	cmpq	%rsi, %rdx
	jl	.LBB8_15
.LBB8_19:                               # %if.end41
	movq	PL_origenviron(%rip), %rax
	testq	%rax, %rax
	je	.LBB8_39
# BB#20:                                # %if.then43
	movq	(%rax), %rax
	leaq	1(%rbx), %rcx
	cmpq	%rcx, %rax
	je	.LBB8_24
# BB#21:                                # %lor.lhs.false48
	cmpq	%r14, %r15
	jne	.LBB8_39
# BB#22:                                # %lor.lhs.false48
	cmpq	%rbx, %rax
	jbe	.LBB8_39
# BB#23:                                # %land.lhs.true54
	leaq	4(%rbx), %rcx
	andq	$-4, %rcx
	cmpq	%rcx, %rax
	ja	.LBB8_39
.LBB8_24:                               # %while.cond62.preheader
	decq	%rax
	movq	%rax, %rbx
	.align	16, 0x90
.LBB8_25:                               # %while.cond62
                                        # =>This Inner Loop Header: Depth=1
	movzbl	1(%rbx), %eax
	incq	%rbx
	testq	%rax, %rax
	jne	.LBB8_25
# BB#26:                                # %while.end66
	movabsq	$.L.str8, %rdi
	xorq	%rsi, %rsi
	callq	Perl_my_setenv
	movq	PL_origenviron(%rip), %rax
	movq	8(%rax), %rdx
	testq	%rdx, %rdx
	je	.LBB8_38
# BB#27:                                # %for.body71.lr.ph
	movq	$1, %rcx
	cmpq	%r14, %r15
	jne	.LBB8_34
	.align	16, 0x90
.LBB8_28:                               # %for.body71.us
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB8_32 Depth 2
	leaq	1(%rbx), %rsi
	cmpq	%rsi, %rdx
	je	.LBB8_31
# BB#29:                                # %lor.lhs.false77.us
                                        #   in Loop: Header=BB8_28 Depth=1
	cmpq	%rbx, %rdx
	jbe	.LBB8_38
# BB#30:                                # %land.lhs.true84.us
                                        #   in Loop: Header=BB8_28 Depth=1
	leaq	4(%rbx), %rsi
	andq	$-4, %rsi
	cmpq	%rsi, %rdx
	ja	.LBB8_38
.LBB8_31:                               # %while.cond94.us.preheader
                                        #   in Loop: Header=BB8_28 Depth=1
	decq	%rdx
	movq	%rdx, %rbx
	.align	16, 0x90
.LBB8_32:                               # %while.cond94.us
                                        #   Parent Loop BB8_28 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movzbl	1(%rbx), %edx
	incq	%rbx
	testq	%rdx, %rdx
	jne	.LBB8_32
# BB#33:                                # %for.inc101.us
                                        #   in Loop: Header=BB8_28 Depth=1
	movq	8(%rax,%rcx,8), %rdx
	incq	%rcx
	testq	%rdx, %rdx
	jne	.LBB8_28
	jmp	.LBB8_38
	.align	16, 0x90
.LBB8_34:                               # %for.body71
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB8_36 Depth 2
	leaq	1(%rbx), %rsi
	cmpq	%rsi, %rdx
	jne	.LBB8_38
# BB#35:                                # %while.cond94.preheader
                                        #   in Loop: Header=BB8_34 Depth=1
	decq	%rdx
	movq	%rdx, %rbx
	.align	16, 0x90
.LBB8_36:                               # %while.cond94
                                        #   Parent Loop BB8_34 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movzbl	1(%rbx), %edx
	incq	%rbx
	testq	%rdx, %rdx
	jne	.LBB8_36
# BB#37:                                # %for.inc101
                                        #   in Loop: Header=BB8_34 Depth=1
	movq	8(%rax,%rcx,8), %rdx
	incq	%rcx
	testq	%rdx, %rdx
	jne	.LBB8_34
.LBB8_38:                               # %if.end105.loopexit
	movq	PL_origargv(%rip), %rax
	movq	(%rax), %r14
.LBB8_39:                               # %if.end105
	movzbl	PL_do_undump(%rip), %eax
	subq	%r14, %rbx
	movq	%rbx, PL_origalen(%rip)
	testq	%rax, %rax
	je	.LBB8_41
# BB#40:                                # %if.then108
	movq	(%r13), %rdi
	callq	Perl_savepv
	movq	PL_curstackinfo(%rip), %rcx
	movq	%rax, PL_origfilename(%rip)
	xorq	%rbx, %rbx
	movb	%bl, PL_do_undump(%rip)
	movq	$-1, 16(%rcx)
	xorq	%rax, %rax
	callq	init_ids
	xorq	%rax, %rax
	movq	%r12, %rdi
	movq	%r13, %rsi
	movq	16(%rsp), %rdx          # 8-byte Reload
	callq	init_postdump_symbols
	jmp	.LBB8_62
.LBB8_41:                               # %if.end113
	movq	PL_main_root(%rip), %rdi
	testq	%rdi, %rdi
	je	.LBB8_43
# BB#42:                                # %if.then115
	callq	Perl_op_free
	movq	$0, PL_main_root(%rip)
.LBB8_43:                               # %if.end116
	movq	PL_main_cv(%rip), %rdi
	movq	$0, PL_main_start(%rip)
	callq	Perl_sv_free
	movq	$0, PL_main_cv(%rip)
	movabsq	$PL_basetime, %rdi
	xorq	%rax, %rax
	xorq	%r14, %r14
	callq	time
	movq	PL_top_env(%rip), %rax
	movq	PL_scopestack_ix(%rip), %r12
	movb	%r14b, PL_dowarn(%rip)
	movq	%rax, 224(%rsp)
	leaq	24(%rsp), %r13
	xorq	%rsi, %rsi
	movq	%r13, %rdi
	callq	sigsetjmp
	movq	%rax, %rbx
	movl	%ebx, 232(%rsp)
	movq	%r13, PL_top_env(%rip)
	movb	%r14b, 236(%rsp)
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rbx, %rax
	cmpq	$3, %rax
	ja	.LBB8_61
# BB#44:                                # %if.end116
	jmpq	*.LJTI8_0(,%rax,8)
.LBB8_45:                               # %sw.bb
	xorq	%rax, %rax
	xorq	%rbx, %rbx
	movq	16(%rsp), %rdi          # 8-byte Reload
	movq	8(%rsp), %rsi           # 8-byte Reload
	callq	parse_body
	movq	PL_checkav(%rip), %rsi
	testq	%rsi, %rsi
	je	.LBB8_61
# BB#46:                                # %if.then122
	movq	%r12, %rdi
	callq	Perl_call_list
	jmp	.LBB8_61
.LBB8_47:                               # %sw.bb124
	movq	$1, PL_statusvalue(%rip)
	jmp	.LBB8_49
	.align	16, 0x90
.LBB8_48:                               # %while.body129
                                        #   in Loop: Header=BB8_49 Depth=1
	callq	Perl_pop_scope
.LBB8_49:                               # %while.body129
                                        # =>This Inner Loop Header: Depth=1
	movq	PL_scopestack_ix(%rip), %rax
	cmpq	%r12, %rax
	jg	.LBB8_48
# BB#50:                                # %while.end130
	movq	PL_tmps_ix(%rip), %rax
	movq	PL_tmps_floor(%rip), %rcx
	cmpq	%rcx, %rax
	jle	.LBB8_52
# BB#51:                                # %if.then133
	callq	Perl_free_tmps
.LBB8_52:                               # %if.end134
	movq	PL_defstash(%rip), %rax
	movq	PL_checkav(%rip), %rsi
	movq	%rax, PL_curstash(%rip)
	testq	%rsi, %rsi
	je	.LBB8_54
# BB#53:                                # %if.then136
	movq	%r12, %rdi
	callq	Perl_call_list
.LBB8_54:                               # %if.end137
	movq	PL_statusvalue(%rip), %rbx
	jmp	.LBB8_61
.LBB8_55:                               # %sw.bb139
	movq	PL_stderrgv(%rip), %rax
	testq	%rax, %rax
	je	.LBB8_59
# BB#56:                                # %land.lhs.true141
	movzbl	16(%rax), %ecx
	cmpq	$13, %rcx
	jne	.LBB8_59
# BB#57:                                # %land.lhs.true145
	movq	(%rax), %rax
	movq	56(%rax), %rax
	movq	16(%rax), %rax
	testq	%rax, %rax
	je	.LBB8_59
# BB#58:                                # %land.lhs.true147
	movq	(%rax), %rax
	movq	64(%rax), %rdi
	testq	%rdi, %rdi
	jne	.LBB8_60
.LBB8_59:                               # %cond.false
	callq	Perl_PerlIO_stderr
	movq	%rax, %rdi
.LBB8_60:                               # %cond.end
	movabsq	$.L.str9, %rsi
	xorq	%rax, %rax
	callq	PerlIO_printf
	movq	$1, %rbx
.LBB8_61:                               # %sw.epilog
	movq	224(%rsp), %rax
	movq	%rax, PL_top_env(%rip)
.LBB8_62:                               # %cleanup
	movq	%rbx, %rax
	addq	$240, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp79:
	.size	perl_parse, .Ltmp79-perl_parse
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI8_0:
	.quad	.LBB8_45
	.quad	.LBB8_47
	.quad	.LBB8_49
	.quad	.LBB8_55

	.text
	.globl	perl_run
	.align	16, 0x90
	.type	perl_run,@function
perl_run:                               # @perl_run
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp85:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp86:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp87:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp88:
	.cfi_def_cfa_offset 40
	subq	$216, %rsp
.Ltmp89:
	.cfi_def_cfa_offset 256
.Ltmp90:
	.cfi_offset %rbx, -40
.Ltmp91:
	.cfi_offset %r12, -32
.Ltmp92:
	.cfi_offset %r14, -24
.Ltmp93:
	.cfi_offset %r15, -16
	movq	PL_top_env(%rip), %rax
	movq	PL_scopestack_ix(%rip), %r15
	movq	%rax, 200(%rsp)
	leaq	(%rsp), %r14
	xorq	%rsi, %rsi
	xorq	%r12, %r12
	movq	%r14, %rdi
	callq	sigsetjmp
	movq	%rax, %rbx
	movl	%ebx, 208(%rsp)
	movq	%r14, PL_top_env(%rip)
	movb	%r12b, 212(%rsp)
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rbx, %rax
	cmpq	$3, %rax
	ja	.LBB9_31
# BB#1:                                 # %entry
	jmpq	*.LJTI9_0(,%rax,8)
.LBB9_2:                                # %sw.bb
	movq	PL_curstackinfo(%rip), %rax
	movq	$-1, 16(%rax)
	jmp	.LBB9_3
.LBB9_13:                               # %sw.bb12
	cmpq	$0, PL_restartop(%rip)
	je	.LBB9_24
# BB#14:                                # %while.cond15.preheader
	movq	PL_curstack(%rip), %rax
	movq	PL_mainstack(%rip), %rcx
	cmpq	%rcx, %rax
	je	.LBB9_3
# BB#15:
	movabsq	$.L.str10, %r14
	.align	16, 0x90
.LBB9_16:                               # %while.body18
                                        # =>This Inner Loop Header: Depth=1
	movq	$-1, %rdi
	callq	Perl_dounwind
	movq	PL_curstackinfo(%rip), %rax
	movq	40(%rax), %r12
	movq	PL_stack_sp(%rip), %rbx
	testq	%r12, %r12
	jne	.LBB9_23
# BB#17:                                # %if.then22
                                        #   in Loop: Header=BB9_16 Depth=1
	movq	PL_stderrgv(%rip), %rax
	testq	%rax, %rax
	je	.LBB9_21
# BB#18:                                # %land.lhs.true24
                                        #   in Loop: Header=BB9_16 Depth=1
	movzbl	16(%rax), %ecx
	cmpq	$13, %rcx
	jne	.LBB9_21
# BB#19:                                # %land.lhs.true28
                                        #   in Loop: Header=BB9_16 Depth=1
	movq	(%rax), %rax
	movq	56(%rax), %rax
	movq	16(%rax), %rax
	testq	%rax, %rax
	je	.LBB9_21
# BB#20:                                # %land.lhs.true30
                                        #   in Loop: Header=BB9_16 Depth=1
	movq	(%rax), %rax
	movq	64(%rax), %rdi
	testq	%rdi, %rdi
	jne	.LBB9_22
	.align	16, 0x90
.LBB9_21:                               # %cond.false
                                        #   in Loop: Header=BB9_16 Depth=1
	callq	Perl_PerlIO_stderr
	movq	%rax, %rdi
.LBB9_22:                               # %cond.end
                                        #   in Loop: Header=BB9_16 Depth=1
	xorq	%rax, %rax
	movq	%r14, %rsi
	callq	PerlIO_printf
	movq	$1, PL_statusvalue(%rip)
	xorq	%rax, %rax
	callq	my_exit_jump
.LBB9_23:                               # %if.end43
                                        #   in Loop: Header=BB9_16 Depth=1
	movq	PL_curstack(%rip), %rax
	movq	PL_stack_base(%rip), %rcx
	movq	(%rax), %rax
	subq	%rcx, %rbx
	movq	$3, %rcx
	sarq	%cl, %rbx
	movq	%rbx, 8(%rax)
	movq	(%r12), %rax
	movq	(%rax), %rax
	movq	(%rax), %rax
	movq	%rax, PL_stack_base(%rip)
	movq	(%r12), %rcx
	movq	(%rcx), %rcx
	movq	16(%rcx), %rcx
	leaq	(%rax,%rcx,8), %rcx
	movq	%rcx, PL_stack_max(%rip)
	movq	(%r12), %rcx
	movq	(%rcx), %rcx
	movq	8(%rcx), %rcx
	leaq	(%rax,%rcx,8), %rax
	movq	%rax, PL_stack_sp(%rip)
	movq	(%r12), %rax
	movq	PL_mainstack(%rip), %rcx
	movq	%rax, PL_curstack(%rip)
	movq	%r12, PL_curstackinfo(%rip)
	cmpq	%rcx, %rax
	jne	.LBB9_16
.LBB9_3:                                # %redo_body
	xorq	%rax, %rax
	movq	%r15, %rdi
	callq	run_body
	jmp	.LBB9_5
	.align	16, 0x90
.LBB9_4:                                # %while.body
                                        #   in Loop: Header=BB9_5 Depth=1
	callq	Perl_pop_scope
.LBB9_5:                                # %while.body
                                        # =>This Inner Loop Header: Depth=1
	movq	PL_scopestack_ix(%rip), %rax
	cmpq	%r15, %rax
	jg	.LBB9_4
# BB#6:                                 # %while.end
	movq	PL_tmps_ix(%rip), %rax
	movq	PL_tmps_floor(%rip), %rcx
	cmpq	%rcx, %rax
	jle	.LBB9_8
# BB#7:                                 # %if.then
	callq	Perl_free_tmps
.LBB9_8:                                # %if.end
	movq	PL_defstash(%rip), %rax
	movq	PL_endav(%rip), %rsi
	movq	%rax, PL_curstash(%rip)
	testq	%rsi, %rsi
	je	.LBB9_12
# BB#9:                                 # %if.end
	movzbl	PL_exit_flags(%rip), %eax
	andq	$2, %rax
	testq	$255, %rax
	jne	.LBB9_12
# BB#10:                                # %if.end
	movzbl	PL_minus_c(%rip), %eax
	testq	$255, %rax
	jne	.LBB9_12
# BB#11:                                # %if.then9
	movq	%r15, %rdi
	callq	Perl_call_list
.LBB9_12:                               # %if.end10
	movq	PL_statusvalue(%rip), %rbx
.LBB9_31:                               # %sw.epilog
	movq	200(%rsp), %rax
	movq	%rax, PL_top_env(%rip)
	movq	%rbx, %rax
	addq	$216, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.LBB9_24:                               # %if.end56
	movq	PL_stderrgv(%rip), %rax
	testq	%rax, %rax
	je	.LBB9_28
# BB#25:                                # %land.lhs.true58
	movzbl	16(%rax), %ecx
	cmpq	$13, %rcx
	jne	.LBB9_28
# BB#26:                                # %land.lhs.true63
	movq	(%rax), %rax
	movq	56(%rax), %rax
	movq	16(%rax), %rax
	testq	%rax, %rax
	je	.LBB9_28
# BB#27:                                # %land.lhs.true68
	movq	(%rax), %rax
	movq	64(%rax), %rdi
	testq	%rdi, %rdi
	jne	.LBB9_29
.LBB9_28:                               # %cond.false81
	callq	Perl_PerlIO_stderr
	movq	%rax, %rdi
.LBB9_29:                               # %cond.end83
	movabsq	$.L.str11, %rsi
	xorq	%rax, %rax
	callq	PerlIO_printf
	movq	PL_tmps_ix(%rip), %rax
	movq	PL_tmps_floor(%rip), %rcx
	cmpq	%rcx, %rax
	movq	$1, %rbx
	jle	.LBB9_31
# BB#30:                                # %if.then88
	callq	Perl_free_tmps
	jmp	.LBB9_31
.Ltmp94:
	.size	perl_run, .Ltmp94-perl_run
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI9_0:
	.quad	.LBB9_3
	.quad	.LBB9_2
	.quad	.LBB9_5
	.quad	.LBB9_13

	.text
	.globl	Perl_my_exit
	.align	16, 0x90
	.type	Perl_my_exit,@function
Perl_my_exit:                           # @Perl_my_exit
	.cfi_startproc
# BB#0:                                 # %entry
	cmpq	$1, %rdi
	jne	.LBB10_1
# BB#3:                                 # %sw.bb1
	movq	$1, PL_statusvalue(%rip)
	xorq	%rax, %rax
	jmp	my_exit_jump  # TAILCALL
.LBB10_1:                               # %entry
	testq	%rdi, %rdi
	jne	.LBB10_4
# BB#2:                                 # %sw.bb
	movq	$0, PL_statusvalue(%rip)
	xorq	%rax, %rax
	jmp	my_exit_jump  # TAILCALL
.LBB10_4:                               # %sw.default
	movq	%rdi, PL_statusvalue(%rip)
	cmpq	$-1, %rdi
	je	.LBB10_6
# BB#5:                                 # %if.then
	andq	$65535, %rdi            # imm = 0xFFFF
	movq	%rdi, PL_statusvalue(%rip)
.LBB10_6:                               # %sw.epilog
	xorq	%rax, %rax
	jmp	my_exit_jump  # TAILCALL
.Ltmp95:
	.size	Perl_my_exit, .Ltmp95-Perl_my_exit
	.cfi_endproc

	.globl	Perl_get_sv
	.align	16, 0x90
	.type	Perl_get_sv,@function
Perl_get_sv:                            # @Perl_get_sv
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rax
.Ltmp97:
	.cfi_def_cfa_offset 16
	movq	$4, %rdx
	callq	Perl_gv_fetchpv
	movq	%rax, %rcx
	xorq	%rax, %rax
	testq	%rcx, %rcx
	je	.LBB11_2
# BB#1:                                 # %if.then
	movq	(%rcx), %rax
	movq	56(%rax), %rax
	movq	(%rax), %rax
.LBB11_2:                               # %return
	popq	%rdx
	retq
.Ltmp98:
	.size	Perl_get_sv, .Ltmp98-Perl_get_sv
	.cfi_endproc

	.globl	Perl_get_av
	.align	16, 0x90
	.type	Perl_get_av,@function
Perl_get_av:                            # @Perl_get_av
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp101:
	.cfi_def_cfa_offset 16
.Ltmp102:
	.cfi_offset %rbx, -16
	movq	%rsi, %rbx
	movq	$10, %rdx
                                        # kill: RSI<def> RBX<kill>
	callq	Perl_gv_fetchpv
	testq	%rbx, %rbx
	je	.LBB12_3
# BB#1:                                 # %if.then
	movq	(%rax), %rcx
	movq	56(%rcx), %rcx
	movq	32(%rcx), %rcx
	testq	%rcx, %rcx
	jne	.LBB12_5
# BB#2:                                 # %cond.false
	movq	%rax, %rdi
	callq	Perl_gv_AVadd
	jmp	.LBB12_4
.LBB12_3:                               # %if.end
	xorq	%rcx, %rcx
	testq	%rax, %rax
	je	.LBB12_5
.LBB12_4:                               # %if.then10
	movq	(%rax), %rax
	movq	56(%rax), %rax
	movq	32(%rax), %rcx
.LBB12_5:                               # %return
	movq	%rcx, %rax
	popq	%rbx
	retq
.Ltmp103:
	.size	Perl_get_av, .Ltmp103-Perl_get_av
	.cfi_endproc

	.globl	Perl_get_hv
	.align	16, 0x90
	.type	Perl_get_hv,@function
Perl_get_hv:                            # @Perl_get_hv
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp106:
	.cfi_def_cfa_offset 16
.Ltmp107:
	.cfi_offset %rbx, -16
	movq	%rsi, %rbx
	movq	$11, %rdx
                                        # kill: RSI<def> RBX<kill>
	callq	Perl_gv_fetchpv
	testq	%rbx, %rbx
	je	.LBB13_3
# BB#1:                                 # %if.then
	movq	(%rax), %rcx
	movq	56(%rcx), %rcx
	movq	40(%rcx), %rcx
	testq	%rcx, %rcx
	jne	.LBB13_5
# BB#2:                                 # %cond.false
	movq	%rax, %rdi
	callq	Perl_gv_HVadd
	jmp	.LBB13_4
.LBB13_3:                               # %if.end
	xorq	%rcx, %rcx
	testq	%rax, %rax
	je	.LBB13_5
.LBB13_4:                               # %if.then10
	movq	(%rax), %rax
	movq	56(%rax), %rax
	movq	40(%rax), %rcx
.LBB13_5:                               # %return
	movq	%rcx, %rax
	popq	%rbx
	retq
.Ltmp108:
	.size	Perl_get_hv, .Ltmp108-Perl_get_hv
	.cfi_endproc

	.globl	Perl_get_cv
	.align	16, 0x90
	.type	Perl_get_cv,@function
Perl_get_cv:                            # @Perl_get_cv
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp112:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp113:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp114:
	.cfi_def_cfa_offset 32
.Ltmp115:
	.cfi_offset %rbx, -24
.Ltmp116:
	.cfi_offset %r14, -16
	movq	%rsi, %rbx
	movq	%rdi, %r14
	movq	$12, %rdx
                                        # kill: RDI<def> R14<kill>
                                        # kill: RSI<def> RBX<kill>
	callq	Perl_gv_fetchpv
	testq	%rbx, %rbx
	je	.LBB14_3
# BB#1:                                 # %land.lhs.true
	movq	(%rax), %rcx
	movq	56(%rcx), %rcx
	cmpq	$0, 64(%rcx)
	jne	.LBB14_7
# BB#2:                                 # %cond.false
	cmpq	$0, 56(%rcx)
	je	.LBB14_7
.LBB14_3:                               # %if.end
	xorq	%rcx, %rcx
	testq	%rax, %rax
	je	.LBB14_6
# BB#4:                                 # %if.then10
	movq	(%rax), %rax
	movq	56(%rax), %rax
	cmpq	$0, 64(%rax)
	jne	.LBB14_6
# BB#5:                                 # %cond.false16
	movq	56(%rax), %rcx
.LBB14_6:                               # %return
	movq	%rcx, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.LBB14_7:                               # %if.then
	xorq	%rdi, %rdi
	xorq	%rsi, %rsi
	callq	Perl_start_subparse
	movq	%rax, %rbx
	xorq	%rsi, %rsi
	movq	%r14, %rdi
	callq	Perl_newSVpv
	movq	$5, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	Perl_newSVOP
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	movq	%rbx, %rdi
	movq	%rax, %rsi
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	jmp	Perl_newSUB  # TAILCALL
.Ltmp117:
	.size	Perl_get_cv, .Ltmp117-Perl_get_cv
	.cfi_endproc

	.globl	Perl_call_argv
	.align	16, 0x90
	.type	Perl_call_argv,@function
Perl_call_argv:                         # @Perl_call_argv
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp123:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp124:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp125:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp126:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp127:
	.cfi_def_cfa_offset 48
.Ltmp128:
	.cfi_offset %rbx, -40
.Ltmp129:
	.cfi_offset %r12, -32
.Ltmp130:
	.cfi_offset %r14, -24
.Ltmp131:
	.cfi_offset %r15, -16
	movq	%rdx, %r12
	movq	%rsi, %r14
	movq	%rdi, %r15
	movq	PL_markstack_ptr(%rip), %rax
	movq	PL_markstack_max(%rip), %rcx
	movq	PL_stack_sp(%rip), %rbx
	addq	$8, %rax
	movq	%rax, PL_markstack_ptr(%rip)
	cmpq	%rcx, %rax
	jne	.LBB15_2
# BB#1:                                 # %if.then
	callq	Perl_markstack_grow
	movq	PL_markstack_ptr(%rip), %rax
.LBB15_2:                               # %if.end
	movq	PL_stack_base(%rip), %rcx
	movq	%rbx, %rdx
	subq	%rcx, %rdx
	movq	$3, %rcx
	sarq	%cl, %rdx
	movq	%rdx, (%rax)
	testq	%r12, %r12
	je	.LBB15_9
# BB#3:                                 # %while.cond.preheader
	movq	(%r12), %rdi
	testq	%rdi, %rdi
	je	.LBB15_8
# BB#4:                                 # %while.body.preheader
	addq	$8, %r12
	.align	16, 0x90
.LBB15_5:                               # %while.body
                                        # =>This Inner Loop Header: Depth=1
	movq	PL_stack_max(%rip), %rax
	subq	%rbx, %rax
	cmpq	$7, %rax
	jg	.LBB15_7
# BB#6:                                 # %if.then8
                                        #   in Loop: Header=BB15_5 Depth=1
	movq	$1, %rdx
	movq	%rbx, %rdi
	movq	%rbx, %rsi
	callq	Perl_stack_grow
	movq	-8(%r12), %rdi
	movq	%rax, %rbx
.LBB15_7:                               # %if.end9
                                        #   in Loop: Header=BB15_5 Depth=1
	xorq	%rsi, %rsi
	callq	Perl_newSVpv
	movq	%rax, %rdi
	callq	Perl_sv_2mortal
	movq	%rax, 8(%rbx)
	movq	(%r12), %rdi
	addq	$8, %rbx
	addq	$8, %r12
	testq	%rdi, %rdi
	jne	.LBB15_5
.LBB15_8:                               # %while.end
	movq	%rbx, PL_stack_sp(%rip)
.LBB15_9:                               # %if.end15
	movq	$1, %rsi
	movq	%r15, %rdi
	callq	Perl_get_cv
	movq	%rax, %rdi
	movq	%r14, %rsi
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	jmp	Perl_call_sv  # TAILCALL
.Ltmp132:
	.size	Perl_call_argv, .Ltmp132-Perl_call_argv
	.cfi_endproc

	.globl	Perl_call_pv
	.align	16, 0x90
	.type	Perl_call_pv,@function
Perl_call_pv:                           # @Perl_call_pv
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp135:
	.cfi_def_cfa_offset 16
.Ltmp136:
	.cfi_offset %rbx, -16
	movq	%rsi, %rbx
	movq	$1, %rsi
	callq	Perl_get_cv
	movq	%rax, %rdi
	movq	%rbx, %rsi
	popq	%rbx
	jmp	Perl_call_sv  # TAILCALL
.Ltmp137:
	.size	Perl_call_pv, .Ltmp137-Perl_call_pv
	.cfi_endproc

	.globl	Perl_call_sv
	.align	16, 0x90
	.type	Perl_call_sv,@function
Perl_call_sv:                           # @Perl_call_sv
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp144:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp145:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp146:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp147:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp148:
	.cfi_def_cfa_offset 48
	subq	$368, %rsp              # imm = 0x170
.Ltmp149:
	.cfi_def_cfa_offset 416
.Ltmp150:
	.cfi_offset %rbx, -48
.Ltmp151:
	.cfi_offset %r12, -40
.Ltmp152:
	.cfi_offset %r13, -32
.Ltmp153:
	.cfi_offset %r14, -24
.Ltmp154:
	.cfi_offset %r15, -16
	movq	PL_stack_sp(%rip), %r15
	movq	$0, 256(%rsp)
	movq	PL_top_env(%rip), %rax
	movzbl	212(%rax), %eax
	movq	%rax, 8(%rsp)           # 8-byte Spill
	movq	PL_op(%rip), %rax
	movq	%rax, 32(%rsp)          # 8-byte Spill
	movq	%rsi, %r12
	movq	%rdi, %r14
	movq	%r12, %rax
	andq	$2, %rax
	movq	%rax, 24(%rsp)          # 8-byte Spill
	je	.LBB17_2
# BB#1:                                 # %if.then
	callq	Perl_push_scope
	movabsq	$PL_tmps_floor, %rdi
	callq	Perl_save_int
	movq	PL_tmps_ix(%rip), %rax
	movq	%rax, PL_tmps_floor(%rip)
.LBB17_2:                               # %if.end
	movq	$0, 360(%rsp)
	movq	$0, 352(%rsp)
	movq	$0, 344(%rsp)
	movq	$0, 336(%rsp)
	movq	$0, 328(%rsp)
	movq	$0, 320(%rsp)
	movq	$0, 312(%rsp)
	movq	$0, 312(%rsp)
	xorq	%rax, %rax
	testq	$8, %r12
	jne	.LBB17_4
# BB#3:                                 # %if.then4
	movq	$64, %rax
	movb	%al, 348(%rsp)
.LBB17_4:                               # %if.end6
	testq	$128, %r12
	movq	$1, %rcx
	jne	.LBB17_6
# BB#5:                                 # %cond.false
	movq	%r12, %rcx
	andq	$1, %rcx
	orq	$2, %rcx
.LBB17_6:                               # %cond.end
	orq	%rcx, %rax
	movb	%al, 348(%rsp)
	xorq	%rax, %rax
	callq	save_op
	movq	PL_stack_max(%rip), %rax
	movq	PL_stack_sp(%rip), %rsi
	leaq	312(%rsp), %rbx
	movq	%rbx, PL_op(%rip)
	subq	%rsi, %rax
	cmpq	$7, %rax
	jg	.LBB17_8
# BB#7:                                 # %if.then17
	movq	$1, %rdx
	movq	%r15, %rdi
	callq	Perl_stack_grow
	movq	PL_stack_sp(%rip), %rsi
.LBB17_8:                               # %if.end19
	leaq	8(%rsi), %rax
	movq	%rax, PL_stack_sp(%rip)
	movq	%r14, 8(%rsi)
	movq	PL_markstack_ptr(%rip), %rax
	movq	(%rax), %r13
	movzbl	PL_perldb(%rip), %ecx
	movq	PL_scopestack_ix(%rip), %rsi
	testq	$1, %rcx
	je	.LBB17_17
# BB#9:                                 # %land.lhs.true23
	movq	PL_curstash(%rip), %rdx
	movq	PL_debstash(%rip), %rcx
	cmpq	%rcx, %rdx
	je	.LBB17_17
# BB#10:                                # %land.lhs.true26
	cmpq	$0, PL_DBcv(%rip)
	jne	.LBB17_12
# BB#11:                                # %lor.lhs.false
	movq	PL_DBsub(%rip), %rdx
	movq	(%rdx), %rdx
	movq	56(%rdx), %rdx
	movq	56(%rdx), %rdx
	testq	%rdx, %rdx
	movq	%rdx, PL_DBcv(%rip)
	je	.LBB17_17
.LBB17_12:                              # %land.lhs.true29
	movzbl	16(%r14), %edx
	cmpq	$12, %rdx
	jne	.LBB17_15
# BB#13:                                # %lor.lhs.false33
	movq	(%r14), %rdx
	movq	56(%rdx), %rdx
	cmpq	%rcx, %rdx
	je	.LBB17_17
# BB#14:                                # %lor.lhs.false33
	movq	%r12, %rcx
	andq	$32, %rcx
	je	.LBB17_16
	jmp	.LBB17_17
.LBB17_15:                              # %land.lhs.true37
	testq	$32, %r12
	jne	.LBB17_17
.LBB17_16:                              # %if.then40
	movq	PL_op(%rip), %rcx
	movzbl	37(%rcx), %edx
	orq	$16, %rdx
	movb	%dl, 37(%rcx)
.LBB17_17:                              # %if.end44
	testq	$64, %r12
	je	.LBB17_19
# BB#18:                                # %if.then47
	movq	$0, 304(%rsp)
	movq	$0, 296(%rsp)
	movq	$0, 288(%rsp)
	movq	$0, 280(%rsp)
	movq	$0, 272(%rsp)
	movq	$0, 264(%rsp)
	movq	PL_op(%rip), %rcx
	movq	%rcx, 264(%rsp)
	movq	PL_ppaddr+1320(%rip), %rcx
	movq	PL_ppaddr+1328(%rip), %rdx
	movq	%rcx, 280(%rsp)
	movq	%rdx, 328(%rsp)
	leaq	264(%rsp), %rcx
	movq	%rcx, PL_op(%rip)
.LBB17_19:                              # %if.end50
	movq	%rsi, 16(%rsp)          # 8-byte Spill
	testq	$4, %r12
	je	.LBB17_20
# BB#21:                                # %if.else
	movq	PL_op(%rip), %rcx
	movq	%rbx, 360(%rsp)
	addq	$-8, %rax
	movq	%rax, PL_markstack_ptr(%rip)
	movzbl	36(%rcx), %eax
	andq	$3, %rax
	cmpq	$1, %rax
	movq	$128, %r14
	je	.LBB17_25
# BB#22:                                # %cond.false68
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	xorq	%r14, %r14
	cmpq	$2, %rax
	je	.LBB17_25
# BB#23:                                # %cond.false75
	movq	$1, %r14
	cmpq	$3, %rax
	je	.LBB17_25
# BB#24:                                # %cond.false82
	callq	Perl_block_gimme
	movq	%rax, %r14
.LBB17_25:                              # %cond.end88
	callq	Perl_push_scope
	movabsq	$PL_tmps_floor, %rdi
	callq	Perl_save_int
	movq	PL_tmps_ix(%rip), %rax
	movq	%rax, PL_tmps_floor(%rip)
	xorq	%rdi, %rdi
	xorq	%rax, %rax
	callq	push_return
	movq	PL_curstackinfo(%rip), %rdx
	movq	16(%rdx), %rax
	movq	24(%rdx), %rcx
	cmpq	%rcx, %rax
	jge	.LBB17_27
# BB#26:                                # %cond.true93
	incq	%rax
	jmp	.LBB17_28
.LBB17_20:                              # %if.then53
	movq	PL_top_env(%rip), %rax
	movq	$1, %rcx
	movb	%cl, 212(%rax)
	leaq	312(%rsp), %rdi
	xorq	%rsi, %rsi
	xorq	%rax, %rax
	callq	call_body
	movq	PL_stack_base(%rip), %rax
	movq	PL_stack_sp(%rip), %rdx
	leaq	(%rax,%r13,8), %rax
	subq	%rax, %rdx
	movq	$3, %rcx
	sarq	%cl, %rdx
	movq	%rdx, 256(%rsp)
	movq	PL_top_env(%rip), %rax
	movq	8(%rsp), %rcx           # 8-byte Reload
	movb	%cl, 212(%rax)
	jmp	.LBB17_52
.LBB17_27:                              # %cond.false95
	xorq	%rax, %rax
	callq	cxinc
	movq	$32, %rcx
	movq	PL_curstackinfo(%rip), %rdx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
.LBB17_28:                              # %cond.end99
	movq	%rax, 16(%rdx)
	movq	8(%rdx), %rdx
	imulq	$152, %rax, %rax
	movq	$514, (%rdx,%rax)       # imm = 0x202
	movq	PL_stack_sp(%rip), %rsi
	movq	PL_stack_base(%rip), %rcx
	subq	%rcx, %rsi
	movq	$3, %rcx
	sarq	%cl, %rsi
	movq	%rsi, 8(%rdx,%rax)
	movq	PL_curcop(%rip), %rcx
	movq	%rcx, 16(%rdx,%rax)
	movq	PL_markstack_ptr(%rip), %rsi
	movq	PL_markstack(%rip), %rcx
	subq	%rcx, %rsi
	movq	$3, %rcx
	sarq	%cl, %rsi
	movq	%rsi, 32(%rdx,%rax)
	movq	PL_scopestack_ix(%rip), %rcx
	movq	%rcx, 40(%rdx,%rax)
	movq	PL_retstack_ix(%rip), %rcx
	movq	%rcx, 24(%rdx,%rax)
	movq	PL_curpm(%rip), %rcx
	movq	%rcx, 48(%rdx,%rax)
	movb	%r14b, 56(%rdx,%rax)
	movslq	PL_in_eval(%rip), %rcx
	movq	%rcx, 64(%rdx,%rax)
	movq	PL_op(%rip), %rcx
	movzbl	32(%rcx), %ecx
	movq	%rcx, 72(%rdx,%rax)
	movq	$0, 80(%rdx,%rax)
	movq	PL_eval_root(%rip), %rcx
	movq	%rcx, 88(%rdx,%rax)
	movq	PL_linestr(%rip), %rcx
	movq	%rcx, 96(%rdx,%rax)
	movq	$0, 104(%rdx,%rax)
	movq	PL_op(%rip), %rax
	movq	%rax, PL_eval_root(%rip)
	movq	$1, %rax
	movl	%eax, PL_in_eval(%rip)
	movq	%r12, %r14
	andq	$16, %r14
	je	.LBB17_30
# BB#29:                                # %if.then149
	movl	PL_in_eval(%rip), %eax
	orq	$4, %rax
	movl	%eax, PL_in_eval(%rip)
	jmp	.LBB17_31
.LBB17_30:                              # %if.else151
	movq	PL_errgv(%rip), %rax
	movq	(%rax), %rax
	movq	56(%rax), %rax
	movq	(%rax), %rdi
	movabsq	$.L.str1, %rsi
	callq	Perl_sv_setpv
.LBB17_31:                              # %if.end154
	addq	$8, PL_markstack_ptr(%rip)
	movq	PL_top_env(%rip), %rax
	movq	%rax, 240(%rsp)
	leaq	40(%rsp), %rbx
	xorq	%rsi, %rsi
	xorq	%r15, %r15
	movq	%rbx, %rdi
	callq	sigsetjmp
	movl	%eax, 248(%rsp)
	movq	%rbx, PL_top_env(%rip)
	movb	%r15b, 252(%rsp)
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rax, %rcx
	cmpq	$3, %rcx
	ja	.LBB17_47
# BB#32:                                # %if.end154
	jmpq	*.LJTI17_0(,%rcx,8)
.LBB17_35:                              # %sw.bb173
	movq	$1, PL_statusvalue(%rip)
.LBB17_36:                              # %sw.bb174
	movq	PL_defstash(%rip), %rax
	movq	PL_tmps_ix(%rip), %rcx
	movq	PL_tmps_floor(%rip), %rdx
	movq	%rax, PL_curstash(%rip)
	cmpq	%rdx, %rcx
	jle	.LBB17_38
# BB#37:                                # %if.then177
	callq	Perl_free_tmps
.LBB17_38:                              # %if.end178
	movq	240(%rsp), %rax
	cmpq	$0, PL_statusvalue(%rip)
	movq	%rax, PL_top_env(%rip)
	je	.LBB17_41
# BB#39:                                # %land.lhs.true182
	movzbl	PL_exit_flags(%rip), %eax
	testq	$1, %rax
	jne	.LBB17_41
# BB#40:                                # %if.then186
	movabsq	$.L.str12, %rdi
	xorq	%rax, %rax
	callq	Perl_croak
.LBB17_41:                              # %if.end187
	xorq	%rax, %rax
	callq	my_exit_jump
.LBB17_42:                              # %sw.bb189
	movq	PL_restartop(%rip), %rax
	testq	%rax, %rax
	je	.LBB17_44
# BB#43:                                # %if.then191
	movq	%rax, PL_op(%rip)
	movq	$0, PL_restartop(%rip)
.LBB17_33:                              # %redo_body
	leaq	312(%rsp), %rdi
	xorq	%rsi, %rsi
	xorq	%rax, %rax
	callq	call_body
	movq	PL_stack_base(%rip), %rax
	movq	PL_stack_sp(%rip), %rdx
	leaq	(%rax,%r13,8), %rax
	subq	%rax, %rdx
	movq	$3, %rcx
	sarq	%cl, %rdx
	testq	%r14, %r14
	movq	%rdx, 256(%rsp)
	jne	.LBB17_47
# BB#34:                                # %if.then168
	movq	PL_errgv(%rip), %rax
	movq	(%rax), %rax
	movq	56(%rax), %rax
	movq	(%rax), %rdi
	movabsq	$.L.str1, %rsi
	callq	Perl_sv_setpv
	jmp	.LBB17_47
.LBB17_44:                              # %if.end192
	movq	PL_stack_base(%rip), %rax
	leaq	(%rax,%r13,8), %rax
	movq	%rax, PL_stack_sp(%rip)
	testq	$1, %r12
	je	.LBB17_46
# BB#45:                                # %if.then196
	movq	$0, 256(%rsp)
	jmp	.LBB17_47
.LBB17_46:                              # %if.else197
	movq	$1, 256(%rsp)
	movq	PL_stack_sp(%rip), %rax
	leaq	8(%rax), %rcx
	movq	%rcx, PL_stack_sp(%rip)
	movq	$PL_sv_undef, 8(%rax)
.LBB17_47:                              # %sw.epilog
	movq	PL_scopestack_ix(%rip), %rax
	movq	16(%rsp), %rcx          # 8-byte Reload
	cmpq	%rcx, %rax
	jle	.LBB17_51
# BB#48:                                # %if.then202
	movq	PL_curstackinfo(%rip), %rax
	movq	16(%rax), %rcx
	leaq	-1(%rcx), %rdx
	movq	%rdx, 16(%rax)
	movq	8(%rax), %rax
	imulq	$152, %rcx, %rcx
	movq	16(%rax,%rcx), %rdx
	movq	%rdx, PL_curcop(%rip)
	movq	PL_markstack(%rip), %rdx
	movq	32(%rax,%rcx), %rsi
	leaq	(%rdx,%rsi,8), %rdx
	movq	%rdx, PL_markstack_ptr(%rip)
	movq	40(%rax,%rcx), %rdx
	movq	%rdx, PL_scopestack_ix(%rip)
	movq	24(%rax,%rcx), %rdx
	movq	%rdx, PL_retstack_ix(%rip)
	movq	64(%rax,%rcx), %rdx
	movq	48(%rax,%rcx), %rbx
	movl	%edx, PL_in_eval(%rip)
	movq	88(%rax,%rcx), %rdx
	movq	%rdx, PL_eval_root(%rip)
	movq	80(%rax,%rcx), %rdi
	testq	%rdi, %rdi
	je	.LBB17_50
# BB#49:                                # %if.then259
	callq	Perl_sv_2mortal
.LBB17_50:                              # %if.end266
	xorq	%rax, %rax
	callq	pop_return
	movq	%rbx, PL_curpm(%rip)
	callq	Perl_pop_scope
.LBB17_51:                              # %if.end268
	movq	240(%rsp), %rax
	movq	%rax, PL_top_env(%rip)
.LBB17_52:                              # %if.end271
	cmpq	$0, 24(%rsp)            # 8-byte Folded Reload
	je	.LBB17_56
# BB#53:                                # %if.then274
	movq	PL_stack_base(%rip), %rax
	leaq	(%rax,%r13,8), %rax
	movq	%rax, PL_stack_sp(%rip)
	movq	$0, 256(%rsp)
	movq	PL_tmps_ix(%rip), %rax
	movq	PL_tmps_floor(%rip), %rcx
	cmpq	%rcx, %rax
	jle	.LBB17_55
# BB#54:                                # %if.then278
	callq	Perl_free_tmps
.LBB17_55:                              # %if.end279
	callq	Perl_pop_scope
.LBB17_56:                              # %if.end280
	movq	32(%rsp), %rax          # 8-byte Reload
	movq	%rax, PL_op(%rip)
	movq	256(%rsp), %rax
	addq	$368, %rsp              # imm = 0x170
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp155:
	.size	Perl_call_sv, .Ltmp155-Perl_call_sv
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI17_0:
	.quad	.LBB17_33
	.quad	.LBB17_35
	.quad	.LBB17_36
	.quad	.LBB17_42

	.text
	.globl	Perl_call_method
	.align	16, 0x90
	.type	Perl_call_method,@function
Perl_call_method:                       # @Perl_call_method
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp158:
	.cfi_def_cfa_offset 16
.Ltmp159:
	.cfi_offset %rbx, -16
	movq	%rsi, %rbx
	xorq	%rsi, %rsi
	callq	Perl_newSVpv
	movq	%rax, %rdi
	callq	Perl_sv_2mortal
	orq	$64, %rbx
	movq	%rax, %rdi
	movq	%rbx, %rsi
	popq	%rbx
	jmp	Perl_call_sv  # TAILCALL
.Ltmp160:
	.size	Perl_call_method, .Ltmp160-Perl_call_method
	.cfi_endproc

	.globl	Perl_eval_sv
	.align	16, 0x90
	.type	Perl_eval_sv,@function
Perl_eval_sv:                           # @Perl_eval_sv
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp167:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp168:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp169:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp170:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp171:
	.cfi_def_cfa_offset 48
	subq	$288, %rsp              # imm = 0x120
.Ltmp172:
	.cfi_def_cfa_offset 336
.Ltmp173:
	.cfi_offset %rbx, -48
.Ltmp174:
	.cfi_offset %r12, -40
.Ltmp175:
	.cfi_offset %r13, -32
.Ltmp176:
	.cfi_offset %r14, -24
.Ltmp177:
	.cfi_offset %r15, -16
	movq	PL_stack_sp(%rip), %rbx
	movq	PL_stack_base(%rip), %rax
	movq	%rbx, %rdx
	subq	%rax, %rdx
	movq	$3, %rcx
	sarq	%cl, %rdx
	movq	%rdx, 232(%rsp)
	movq	$0, 224(%rsp)
	movq	PL_op(%rip), %rax
	movq	%rax, (%rsp)            # 8-byte Spill
	movq	%rsi, %r15
	movq	%rdi, %r14
	movq	%r15, %r13
	andq	$2, %r13
	je	.LBB19_2
# BB#1:                                 # %if.then
	callq	Perl_push_scope
	movabsq	$PL_tmps_floor, %rdi
	callq	Perl_save_int
	movq	PL_tmps_ix(%rip), %rax
	movq	%rax, PL_tmps_floor(%rip)
.LBB19_2:                               # %if.end
	xorq	%rax, %rax
	callq	save_op
	leaq	240(%rsp), %rax
	movq	%rax, PL_op(%rip)
	movq	$0, 280(%rsp)
	movq	$0, 272(%rsp)
	movq	$0, 264(%rsp)
	movq	PL_stack_max(%rip), %rax
	movq	PL_stack_sp(%rip), %rsi
	movq	$0, 256(%rsp)
	movq	$0, 248(%rsp)
	movq	$0, 240(%rsp)
	subq	%rsi, %rax
	cmpq	$7, %rax
	jg	.LBB19_4
# BB#3:                                 # %if.then6
	movq	$1, %rdx
	movq	%rbx, %rdi
	callq	Perl_stack_grow
	movq	PL_stack_sp(%rip), %rsi
.LBB19_4:                               # %if.end8
	leaq	8(%rsi), %rax
	movq	%rax, PL_stack_sp(%rip)
	movq	%r14, 8(%rsi)
	testq	$8, %r15
	jne	.LBB19_6
# BB#5:                                 # %if.then11
	movq	$64, %rax
	movb	%al, 276(%rsp)
.LBB19_6:                               # %if.end12
	movq	$0, 240(%rsp)
	movq	$311, %rax              # imm = 0x137
	movw	%ax, 272(%rsp)
	testq	$128, %r15
	movq	$1, %rax
	jne	.LBB19_8
# BB#7:                                 # %cond.false
	movq	%r15, %rax
	andq	$1, %rax
	orq	$2, %rax
.LBB19_8:                               # %cond.end
	movzbl	276(%rsp), %edx
	movq	%r15, %r14
	andq	$16, %r14
	movq	$3, %rcx
	movq	%r14, %rsi
	shlq	%cl, %rsi
	movzbl	PL_tainting(%rip), %ecx
	orq	%rax, %rsi
	orq	%rdx, %rsi
	movb	%sil, 276(%rsp)
	testq	%rcx, %rcx
	je	.LBB19_10
# BB#9:                                 # %if.then29
	movabsq	$.L.str13, %rsi
	xorq	%rdi, %rdi
	callq	Perl_taint_proper
.LBB19_10:                              # %if.end30
	movq	PL_top_env(%rip), %rax
	movq	%rax, 208(%rsp)
	leaq	8(%rsp), %rbx
	xorq	%rsi, %rsi
	xorq	%r12, %r12
	movq	%rbx, %rdi
	callq	sigsetjmp
	movl	%eax, 216(%rsp)
	movq	%rbx, PL_top_env(%rip)
	movb	%r12b, 220(%rsp)
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rax, %rcx
	cmpq	$3, %rcx
	ja	.LBB19_26
# BB#11:                                # %if.end30
	jmpq	*.LJTI19_0(,%rcx,8)
.LBB19_14:                              # %sw.bb42
	movq	$1, PL_statusvalue(%rip)
.LBB19_15:                              # %sw.bb43
	movq	PL_defstash(%rip), %rax
	movq	PL_tmps_ix(%rip), %rcx
	movq	PL_tmps_floor(%rip), %rdx
	movq	%rax, PL_curstash(%rip)
	cmpq	%rdx, %rcx
	jle	.LBB19_17
# BB#16:                                # %if.then46
	callq	Perl_free_tmps
.LBB19_17:                              # %if.end47
	movq	208(%rsp), %rax
	cmpq	$0, PL_statusvalue(%rip)
	movq	%rax, PL_top_env(%rip)
	je	.LBB19_20
# BB#18:                                # %land.lhs.true
	movzbl	PL_exit_flags(%rip), %eax
	testq	$1, %rax
	jne	.LBB19_20
# BB#19:                                # %if.then54
	movabsq	$.L.str12, %rdi
	xorq	%rax, %rax
	callq	Perl_croak
.LBB19_20:                              # %if.end55
	xorq	%rax, %rax
	callq	my_exit_jump
.LBB19_21:                              # %sw.bb57
	movq	PL_restartop(%rip), %rax
	testq	%rax, %rax
	je	.LBB19_23
# BB#22:                                # %if.then59
	movq	%rax, PL_op(%rip)
	movq	$0, PL_restartop(%rip)
.LBB19_12:                              # %redo_body
	leaq	240(%rsp), %rdi
	movq	$1, %rsi
	xorq	%rax, %rax
	callq	call_body
	movq	PL_stack_base(%rip), %rax
	movq	PL_stack_sp(%rip), %rdx
	movq	232(%rsp), %rcx
	leaq	(%rax,%rcx,8), %rax
	subq	%rax, %rdx
	movq	$3, %rcx
	sarq	%cl, %rdx
	testq	%r14, %r14
	movq	%rdx, 224(%rsp)
	jne	.LBB19_26
# BB#13:                                # %if.then40
	movq	PL_errgv(%rip), %rax
	movq	(%rax), %rax
	movq	56(%rax), %rax
	movq	(%rax), %rdi
	movabsq	$.L.str1, %rsi
	callq	Perl_sv_setpv
	jmp	.LBB19_26
.LBB19_23:                              # %if.end60
	movq	PL_stack_base(%rip), %rax
	movq	232(%rsp), %rcx
	leaq	(%rax,%rcx,8), %rax
	movq	%rax, PL_stack_sp(%rip)
	testq	$1, %r15
	je	.LBB19_25
# BB#24:                                # %if.then64
	movq	$0, 224(%rsp)
	jmp	.LBB19_26
.LBB19_25:                              # %if.else
	movq	$1, 224(%rsp)
	movq	PL_stack_sp(%rip), %rax
	leaq	8(%rax), %rcx
	movq	%rcx, PL_stack_sp(%rip)
	movq	$PL_sv_undef, 8(%rax)
.LBB19_26:                              # %sw.epilog
	movq	208(%rsp), %rax
	testq	%r13, %r13
	movq	%rax, PL_top_env(%rip)
	je	.LBB19_30
# BB#27:                                # %if.then71
	movq	PL_stack_base(%rip), %rax
	movq	232(%rsp), %rcx
	leaq	(%rax,%rcx,8), %rax
	movq	%rax, PL_stack_sp(%rip)
	movq	$0, 224(%rsp)
	movq	PL_tmps_ix(%rip), %rax
	movq	PL_tmps_floor(%rip), %rcx
	cmpq	%rcx, %rax
	jle	.LBB19_29
# BB#28:                                # %if.then75
	callq	Perl_free_tmps
.LBB19_29:                              # %if.end76
	callq	Perl_pop_scope
.LBB19_30:                              # %if.end77
	movq	(%rsp), %rax            # 8-byte Reload
	movq	%rax, PL_op(%rip)
	movq	224(%rsp), %rax
	addq	$288, %rsp              # imm = 0x120
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp178:
	.size	Perl_eval_sv, .Ltmp178-Perl_eval_sv
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI19_0:
	.quad	.LBB19_12
	.quad	.LBB19_14
	.quad	.LBB19_15
	.quad	.LBB19_21

	.text
	.globl	Perl_eval_pv
	.align	16, 0x90
	.type	Perl_eval_pv,@function
Perl_eval_pv:                           # @Perl_eval_pv
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp183:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp184:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp185:
	.cfi_def_cfa_offset 32
	subq	$16, %rsp
.Ltmp186:
	.cfi_def_cfa_offset 48
.Ltmp187:
	.cfi_offset %rbx, -32
.Ltmp188:
	.cfi_offset %r14, -24
.Ltmp189:
	.cfi_offset %r15, -16
	movq	%rsi, %r15
	xorq	%rsi, %rsi
	callq	Perl_newSVpv
	movq	%rax, %rbx
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	Perl_eval_sv
	movq	%rbx, %rdi
	callq	Perl_sv_free
	movq	PL_stack_sp(%rip), %rax
	movq	(%rax), %r14
	testq	%r15, %r15
	leaq	-8(%rax), %rax
	movq	%rax, PL_stack_sp(%rip)
	je	.LBB20_17
# BB#1:                                 # %land.lhs.true
	movq	PL_errgv(%rip), %rbx
	movq	(%rbx), %rax
	movq	56(%rax), %rax
	movq	(%rax), %rdi
	testq	%rdi, %rdi
	je	.LBB20_17
# BB#2:                                 # %cond.false
	movq	16(%rdi), %rax
	testq	$262144, %rax           # imm = 0x40000
	je	.LBB20_7
# BB#3:                                 # %cond.true7
	movq	(%rdi), %rax
	testq	%rax, %rax
	movq	%rax, PL_Xpv(%rip)
	je	.LBB20_17
# BB#4:                                 # %land.lhs.true13
	movq	8(%rax), %rcx
	cmpq	$1, %rcx
	ja	.LBB20_13
# BB#5:                                 # %lor.lhs.false
	testq	%rcx, %rcx
	je	.LBB20_17
# BB#6:                                 # %land.lhs.true16
	movq	(%rax), %rax
	movzbl	(%rax), %eax
	cmpq	$48, %rax
	jne	.LBB20_13
	jmp	.LBB20_17
.LBB20_7:                               # %cond.false21
	testq	$65536, %rax            # imm = 0x10000
	je	.LBB20_9
# BB#8:                                 # %cond.true28
	movq	(%rdi), %rax
	cmpq	$0, 24(%rax)
	jne	.LBB20_13
	jmp	.LBB20_17
.LBB20_9:                               # %cond.false35
	testq	$131072, %rax           # imm = 0x20000
	je	.LBB20_11
# BB#10:                                # %cond.true42
	movq	(%rdi), %rax
	movq	32(%rax), %rdi
	xorq	%rsi, %rsi
	callq	__nedf2
	testq	%rax, %rax
	jne	.LBB20_13
	jmp	.LBB20_17
.LBB20_11:                              # %cond.false49
	callq	Perl_sv_2bool
	testq	$255, %rax
	je	.LBB20_17
# BB#12:                                # %cond.false49.if.then_crit_edge
	movq	PL_errgv(%rip), %rbx
.LBB20_13:                              # %if.then
	movq	(%rbx), %rax
	movq	56(%rax), %rax
	movq	(%rax), %rdi
	movq	%rdi, PL_Sv(%rip)
	movzbl	18(%rdi), %eax
	testq	$4, %rax
	je	.LBB20_15
# BB#14:                                # %cond.true63
	movq	(%rdi), %rax
	movq	8(%rax), %rcx
	movq	%rcx, 8(%rsp)
	movq	(%rax), %rdi
	jmp	.LBB20_16
.LBB20_15:                              # %cond.false68
	leaq	8(%rsp), %rsi
	movq	$2, %rdx
	callq	Perl_sv_2pv_flags
	movq	%rax, %rdi
.LBB20_16:                              # %cond.end
	xorq	%rax, %rax
	callq	Perl_croak
.LBB20_17:                              # %if.end
	movq	%r14, %rax
	addq	$16, %rsp
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp190:
	.size	Perl_eval_pv, .Ltmp190-Perl_eval_pv
	.cfi_endproc

	.globl	Perl_require_pv
	.align	16, 0x90
	.type	Perl_require_pv,@function
Perl_require_pv:                        # @Perl_require_pv
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp194:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp195:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp196:
	.cfi_def_cfa_offset 32
.Ltmp197:
	.cfi_offset %rbx, -24
.Ltmp198:
	.cfi_offset %r14, -16
	movq	PL_curstackinfo(%rip), %rax
	movq	48(%rax), %rax
	movq	PL_stack_sp(%rip), %rbx
	movq	%rdi, %r14
	testq	%rax, %rax
	jne	.LBB21_2
# BB#1:                                 # %if.then
	movq	$32, %rdi
	movq	$12, %rsi
	callq	Perl_new_stackinfo
	movq	PL_curstackinfo(%rip), %rcx
	movq	%rcx, 40(%rax)
	movq	PL_curstackinfo(%rip), %rcx
	movq	%rax, 48(%rcx)
.LBB21_2:                               # %if.end
	movq	$9, 32(%rax)
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
	movq	(%rax), %rdx
	movq	%rdx, PL_curstack(%rip)
	movq	%rax, PL_curstackinfo(%rip)
	movq	%rcx, PL_stack_sp(%rip)
	callq	Perl_sv_newmortal
	movq	%rax, %rbx
	movabsq	$.L.str14, %rsi
	movq	%rbx, %rdi
	callq	Perl_sv_setpv
	movq	%rbx, %rdi
	movq	%r14, %rsi
	callq	Perl_sv_catpv
	movabsq	$.L.str15, %rsi
	movq	%rbx, %rdi
	callq	Perl_sv_catpv
	movq	$2, %rsi
	movq	%rbx, %rdi
	callq	Perl_eval_sv
	movq	PL_curstackinfo(%rip), %rax
	movq	40(%rax), %r14
	movq	PL_stack_sp(%rip), %rbx
	testq	%r14, %r14
	jne	.LBB21_9
# BB#3:                                 # %if.then20
	movq	PL_stderrgv(%rip), %rax
	testq	%rax, %rax
	je	.LBB21_7
# BB#4:                                 # %land.lhs.true
	movzbl	16(%rax), %ecx
	cmpq	$13, %rcx
	jne	.LBB21_7
# BB#5:                                 # %land.lhs.true22
	movq	(%rax), %rax
	movq	56(%rax), %rax
	movq	16(%rax), %rax
	testq	%rax, %rax
	je	.LBB21_7
# BB#6:                                 # %land.lhs.true25
	movq	(%rax), %rax
	movq	64(%rax), %rdi
	testq	%rdi, %rdi
	jne	.LBB21_8
.LBB21_7:                               # %cond.false
	callq	Perl_PerlIO_stderr
	movq	%rax, %rdi
.LBB21_8:                               # %cond.end
	movabsq	$.L.str10, %rsi
	xorq	%rax, %rax
	callq	PerlIO_printf
	movq	$1, PL_statusvalue(%rip)
	xorq	%rax, %rax
	callq	my_exit_jump
.LBB21_9:                               # %if.end38
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
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp199:
	.size	Perl_require_pv, .Ltmp199-Perl_require_pv
	.cfi_endproc

	.globl	Perl_magicname
	.align	16, 0x90
	.type	Perl_magicname,@function
Perl_magicname:                         # @Perl_magicname
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp203:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp204:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp205:
	.cfi_def_cfa_offset 32
.Ltmp206:
	.cfi_offset %rbx, -24
.Ltmp207:
	.cfi_offset %r14, -16
	movq	%rdx, %r14
	movq	%rsi, %rbx
	movq	$1, %rsi
	movq	$4, %rdx
	callq	Perl_gv_fetchpv
	testq	%rax, %rax
	je	.LBB22_1
# BB#2:                                 # %if.then
	movq	(%rax), %rcx
	movq	56(%rcx), %rcx
	movq	(%rcx), %rdi
	xorq	%rdx, %rdx
	movq	%rax, %rsi
	movq	%rbx, %rcx
	movq	%r14, %r8
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	jmp	Perl_sv_magic  # TAILCALL
.LBB22_1:                               # %if.end
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp208:
	.size	Perl_magicname, .Ltmp208-Perl_magicname
	.cfi_endproc

	.globl	Perl_moreswitches
	.align	16, 0x90
	.type	Perl_moreswitches,@function
Perl_moreswitches:                      # @Perl_moreswitches
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp213:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp214:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp215:
	.cfi_def_cfa_offset 32
	subq	$48, %rsp
.Ltmp216:
	.cfi_def_cfa_offset 80
.Ltmp217:
	.cfi_offset %rbx, -32
.Ltmp218:
	.cfi_offset %r14, -24
.Ltmp219:
	.cfi_offset %r15, -16
	movq	%rdi, %rdx
	movq	%rdx, 40(%rsp)
	movsbq	(%rdx), %rcx
	xorq	%rbx, %rbx
	cmpq	$8, %rcx
	jg	.LBB23_2
# BB#1:                                 # %entry
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %rcx
	jne	.LBB23_5
	jmp	.LBB23_198
.LBB23_2:                               # %entry
	cmpq	$31, %rcx
	jg	.LBB23_6
# BB#3:                                 # %entry
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %rcx
	cmpq	$13, %rcx
	ja	.LBB23_5
# BB#4:                                 # %entry
	movq	$1, %rax
	shlq	%cl, %rax
	testq	$9728, %rax             # imm = 0x2600
	jne	.LBB23_198
	jmp	.LBB23_5
.LBB23_6:                               # %entry
	cmpq	$41, %rcx
	jg	.LBB23_10
# BB#7:                                 # %entry
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %rcx
	cmpq	$32, %rcx
	jne	.LBB23_5
	jmp	.LBB23_8
.LBB23_10:                              # %entry
	cmpq	$66, %rcx
	jg	.LBB23_20
# BB#11:                                # %entry
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %rcx
	cmpq	$42, %rcx
	je	.LBB23_8
# BB#12:                                # %entry
	cmpq	$45, %rcx
	je	.LBB23_198
# BB#13:                                # %entry
	cmpq	$48, %rcx
	jne	.LBB23_5
# BB#14:                                # %sw.bb
	movq	PL_rs(%rip), %rdi
	movq	$0, 24(%rsp)
	callq	Perl_sv_free
	movq	40(%rsp), %rbx
	movzbl	1(%rbx), %eax
	cmpq	$120, %rax
	jne	.LBB23_53
# BB#15:                                # %land.lhs.true
	movzbl	2(%rbx), %eax
	testq	%rax, %rax
	je	.LBB23_53
# BB#16:                                # %if.then
	leaq	2(%rbx), %rdi
	movq	%rdi, 40(%rsp)
	incq	%rbx
.LBB23_17:                              # %for.cond
                                        # =>This Inner Loop Header: Depth=1
	movzbl	1(%rbx), %eax
	incq	%rbx
	testq	%rax, %rax
	jne	.LBB23_17
# BB#18:                                # %for.end
	movq	%rbx, %rax
	subq	%rdi, %rax
	movq	%rax, 32(%rsp)
	movq	$4, 24(%rsp)
	leaq	32(%rsp), %rsi
	leaq	24(%rsp), %rdx
	xorq	%rcx, %rcx
	xorq	%r14, %r14
	callq	Perl_grok_hex
	movq	40(%rsp), %rcx
	movq	32(%rsp), %rdx
	addq	%rcx, %rdx
	cmpq	%rbx, %rdx
	jae	.LBB23_19
# BB#22:                                # %if.then9
	movq	$0, 32(%rsp)
	decq	%rcx
	movq	%rcx, 40(%rsp)
	jmp	.LBB23_23
.LBB23_8:                               # %sw.bb705
	movzbl	1(%rdx), %eax
	cmpq	$45, %rax
	jne	.LBB23_192
# BB#9:
	addq	$2, %rdx
	jmp	.LBB23_193
.LBB23_20:                              # %entry
	addq	$-67, %rcx
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	cmpq	$52, %rax
	ja	.LBB23_5
# BB#21:                                # %entry
	jmpq	*.LJTI23_0(,%rax,8)
.LBB23_63:                              # %sw.bb142
	incq	%rdx
	movq	%rdx, 40(%rsp)
	leaq	40(%rsp), %rdi
	xorq	%rax, %rax
	callq	parse_unicode_opts
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	40(%rsp), %rbx
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	%rax, PL_unicode(%rip)
	movq	%rbx, %rax
	jmp	.LBB23_194
.LBB23_192:                             # %select.mid38
	xorq	%rdx, %rdx
.LBB23_193:                             # %select.end37
	movq	%rdx, %rax
	jmp	.LBB23_194
.LBB23_53:                              # %if.else
	movq	$4, 32(%rsp)
	leaq	32(%rsp), %rsi
	leaq	24(%rsp), %rdx
	xorq	%rcx, %rcx
	movq	%rbx, %rdi
	callq	Perl_grok_oct
	cmpq	$256, %rax              # imm = 0x100
	jb	.LBB23_55
# BB#54:                                # %if.then125
	movq	$PL_sv_undef, PL_rs(%rip)
	jmp	.LBB23_60
.LBB23_55:                              # %if.else126
	testq	%rax, %rax
	jne	.LBB23_58
# BB#56:                                # %if.else126
	movq	32(%rsp), %rcx
	cmpq	$2, %rcx
	jb	.LBB23_58
# BB#57:                                # %if.then131
	movabsq	$.L.str1, %rdi
	xorq	%rsi, %rsi
	jmp	.LBB23_59
.LBB23_58:                              # %if.else133
	movb	%al, 23(%rsp)
	leaq	23(%rsp), %rdi
	movq	$1, %rsi
.LBB23_59:                              # %if.end139
	callq	Perl_newSVpvn
	movq	%rax, PL_rs(%rip)
	jmp	.LBB23_60
.LBB23_19:
	movq	%rax, %r14
.LBB23_23:                              # %if.end
	movabsq	$.L.str1, %rdi
	xorq	%rsi, %rsi
	callq	Perl_newSVpvn
	movq	%rax, PL_rs(%rip)
	movq	(%rax), %rdx
	movq	16(%rdx), %rsi
	movq	$2, %rcx
	cmpq	$128, %r14
	jb	.LBB23_32
# BB#24:                                # %cond.false
	movq	$3, %rcx
	cmpq	$2048, %r14             # imm = 0x800
	jb	.LBB23_32
# BB#25:                                # %cond.false17
	movq	$4, %rcx
	cmpq	$65536, %r14            # imm = 0x10000
	jb	.LBB23_32
# BB#26:                                # %cond.false21
	movq	$5, %rcx
	cmpq	$2097152, %r14          # imm = 0x200000
	jb	.LBB23_32
# BB#27:                                # %cond.false25
	movq	$6, %rcx
	cmpq	$67108864, %r14         # imm = 0x4000000
	jb	.LBB23_32
# BB#28:                                # %cond.false29
	movq	$31, %rcx
	movq	%r14, %rdi
	shrq	%cl, %rdi
	testq	%rdi, %rdi
	je	.LBB23_29
# BB#30:                                # %cond.false29
	movabsq	$1, %rcx
	jmp	.LBB23_31
.LBB23_95:                              # %sw.bb295
	movq	PL_curcop(%rip), %rax
	cmpq	$0, 88(%rax)
	je	.LBB23_99
# BB#96:                                # %lor.lhs.false298
	movq	PL_curcop(%rip), %rax
	movq	88(%rax), %rax
	cmpq	$24, %rax
	je	.LBB23_99
# BB#97:                                # %lor.lhs.false302
	movq	PL_curcop(%rip), %rax
	movq	88(%rax), %rax
	cmpq	$48, %rax
	je	.LBB23_100
# BB#98:                                # %land.lhs.true306
	movq	PL_curcop(%rip), %rax
	movq	88(%rax), %rax
	movq	(%rax), %rax
	movq	(%rax), %rax
	movzbl	5(%rax), %eax
	testq	$16, %rax
	je	.LBB23_100
.LBB23_99:                              # %if.then314
	movabsq	$.L.str24, %rsi
	movq	$22, %rdi
	xorq	%rax, %rax
	callq	Perl_warner
.LBB23_100:                             # %for.cond317.preheader
	movq	40(%rsp), %rbx
.LBB23_101:                             # %for.cond317
                                        # =>This Inner Loop Header: Depth=1
	incq	%rbx
	movq	%rbx, 40(%rsp)
	movzbl	(%rbx), %eax
	leaq	-65(%rax), %rcx
	andq	$255, %rcx
	cmpq	$26, %rcx
	jb	.LBB23_101
# BB#102:                               # %for.cond317
                                        #   in Loop: Header=BB23_101 Depth=1
	leaq	-97(%rax), %rcx
	andq	$255, %rcx
	cmpq	$25, %rcx
	jbe	.LBB23_101
# BB#103:                               # %lor.lhs.false333
                                        #   in Loop: Header=BB23_101 Depth=1
	leaq	-48(%rax), %rcx
	andq	$255, %rcx
	cmpq	$10, %rcx
	jb	.LBB23_101
# BB#104:                               # %lor.lhs.false333
                                        #   in Loop: Header=BB23_101 Depth=1
	andq	$255, %rax
	cmpq	$95, %rax
	je	.LBB23_101
	jmp	.LBB23_198
.LBB23_64:                              # %sw.bb146
	movq	$1, %rax
	movb	%al, PL_minus_F(%rip)
	leaq	1(%rdx), %rax
	movq	%rax, 40(%rsp)
	movq	%rax, PL_splitstr(%rip)
	addq	$2, %rdx
	movabsq	$4294953470, %rax       # imm = 0xFFFFC9FE
	jmp	.LBB23_65
.LBB23_67:                              # %while.body
                                        #   in Loop: Header=BB23_65 Depth=1
	movq	%rdx, 40(%rsp)
	incq	%rdx
.LBB23_65:                              # %while.cond
                                        # =>This Inner Loop Header: Depth=1
	movzbl	-1(%rdx), %ecx
	cmpq	$32, %rcx
	ja	.LBB23_67
# BB#66:                                # %land.end
                                        #   in Loop: Header=BB23_65 Depth=1
	movq	%rax, %rsi
	shrq	%cl, %rsi
	testq	$1, %rsi
	jne	.LBB23_67
# BB#68:                                # %while.end
	xorq	%rax, %rax
	movb	%al, -1(%rdx)
	movq	PL_splitstr(%rip), %rdi
	callq	Perl_savepv
	movq	40(%rsp), %rbx
	movq	%rax, PL_splitstr(%rip)
	movq	%rbx, %rax
	jmp	.LBB23_194
.LBB23_115:                             # %sw.bb399
	movabsq	$.L.str25, %rdi
	xorq	%rax, %rax
	callq	forbid_setid
	movq	40(%rsp), %rcx
	incq	%rcx
.LBB23_116:                             # %while.cond402
                                        # =>This Inner Loop Header: Depth=1
	movq	%rcx, %rdi
	movq	%rdi, 40(%rsp)
	movzbl	(%rdi), %eax
	testq	%rax, %rax
	je	.LBB23_132
# BB#117:                               # %land.rhs405
                                        #   in Loop: Header=BB23_116 Depth=1
	leaq	-9(%rax), %rcx
	andq	$255, %rcx
	cmpq	$23, %rcx
	ja	.LBB23_118
# BB#119:                               # %land.end426
                                        #   in Loop: Header=BB23_116 Depth=1
	movq	$8388635, %rdx          # imm = 0x80001B
	shrq	%cl, %rdx
	leaq	1(%rdi), %rcx
	testq	$1, %rdx
	jne	.LBB23_116
# BB#120:                               # %while.end429
	testq	%rax, %rax
	movq	%rdi, %rsi
	jne	.LBB23_121
.LBB23_132:                             # %if.else507
	movabsq	$.L.str26, %rdi
	xorq	%rax, %rax
	callq	Perl_croak
	movq	40(%rsp), %rbx
	movq	%rbx, %rax
	jmp	.LBB23_194
.LBB23_144:                             # %sw.bb549
	movabsq	$.L.str28, %rdi
	xorq	%rax, %rax
	callq	forbid_setid
.LBB23_145:                             # %sw.bb551
	movabsq	$.L.str29, %rdi
	xorq	%rax, %rax
	callq	forbid_setid
	movq	40(%rsp), %rax
	leaq	1(%rax), %rcx
	movq	%rcx, 40(%rsp)
	movzbl	1(%rax), %ecx
	testq	%rcx, %rcx
	jne	.LBB23_146
# BB#168:                               # %if.else638
	movsbq	(%rax), %rsi
	movabsq	$.L.str37, %rdi
	xorq	%rax, %rax
	callq	Perl_croak
	movq	40(%rsp), %rbx
	movq	%rbx, %rax
	jmp	.LBB23_194
.LBB23_195:                             # %sw.bb714
	movzbl	PL_preprocess(%rip), %eax
	testq	%rax, %rax
	je	.LBB23_5
# BB#196:                               # %if.then716
	incq	%rdx
	jmp	.LBB23_197
.LBB23_5:                               # %sw.default
	movabsq	$.L.str44, %rdi
	xorq	%rax, %rax
	xorq	%rbx, %rbx
	movq	%rdx, %rsi
	callq	Perl_croak
	movq	%rbx, %rax
	jmp	.LBB23_194
.LBB23_176:                             # %sw.bb654
	movzbl	PL_tainting(%rip), %eax
	testq	%rax, %rax
	jne	.LBB23_175
# BB#177:                               # %if.then656
	movabsq	$.L.str39, %rdi
	movabsq	$.L.str1, %rdx
	movq	$84, %rsi
	jmp	.LBB23_174
.LBB23_179:                             # %sw.bb661
	movq	$1, %rax
	movb	%al, PL_unsafe(%rip)
	jmp	.LBB23_175
.LBB23_184:                             # %sw.bb681
	movq	PL_compiling+88(%rip), %rdi
	movq	$3, %rax
	movb	%al, PL_dowarn(%rip)
	cmpq	$48, %rdi
	ja	.LBB23_186
# BB#185:                               # %sw.bb681
	movq	$1, %rax
	movq	%rdi, %rcx
	shlq	%cl, %rax
	movabsq	$281474993487873, %rcx  # imm = 0x1000001000001
	testq	%rcx, %rax
	jne	.LBB23_187
.LBB23_186:                             # %if.then690
	callq	Perl_sv_free
	movq	40(%rsp), %rdx
.LBB23_187:                             # %if.end691
	movq	$24, PL_compiling+88(%rip)
	jmp	.LBB23_175
.LBB23_188:                             # %sw.bb693
	movq	PL_compiling+88(%rip), %rdi
	movq	$4, %rax
	movb	%al, PL_dowarn(%rip)
	cmpq	$48, %rdi
	ja	.LBB23_190
# BB#189:                               # %sw.bb693
	movq	$1, %rax
	movq	%rdi, %rcx
	shlq	%cl, %rax
	movabsq	$281474993487873, %rcx  # imm = 0x1000001000001
	testq	%rcx, %rax
	jne	.LBB23_191
.LBB23_190:                             # %if.then702
	callq	Perl_sv_free
	movq	40(%rsp), %rdx
.LBB23_191:                             # %if.end703
	movq	$48, PL_compiling+88(%rip)
	jmp	.LBB23_175
.LBB23_69:                              # %sw.bb169
	movq	$1, %rax
	movb	%al, PL_minus_a(%rip)
	jmp	.LBB23_175
.LBB23_70:                              # %sw.bb171
	movq	$1, %rax
	movb	%al, PL_minus_c(%rip)
	jmp	.LBB23_175
.LBB23_71:                              # %sw.bb173
	movabsq	$.L.str17, %rdi
	xorq	%rax, %rax
	callq	forbid_setid
	movq	40(%rsp), %rax
	leaq	1(%rax), %rcx
	movq	%rcx, 40(%rsp)
	movzbl	1(%rax), %ecx
	cmpq	$116, %rcx
	jne	.LBB23_77
# BB#72:                                # %land.lhs.true179
	movzbl	2(%rax), %ecx
	leaq	-65(%rcx), %rdx
	andq	$255, %rdx
	cmpq	$26, %rdx
	jb	.LBB23_92
# BB#73:                                # %land.lhs.true179
	leaq	-97(%rcx), %rdx
	andq	$255, %rdx
	cmpq	$26, %rdx
	jb	.LBB23_92
# BB#74:                                # %lor.lhs.false199
	leaq	-48(%rcx), %rdx
	andq	$255, %rdx
	cmpq	$10, %rdx
	jb	.LBB23_92
# BB#75:                                # %lor.lhs.false199
	andq	$255, %rcx
	cmpq	$95, %rcx
	je	.LBB23_92
# BB#76:                                # %if.then214
	addq	$2, %rax
	movq	%rax, 40(%rsp)
	movabsq	$.L.str18, %rdi
	movabsq	$.L.str19, %rsi
	callq	Perl_my_setenv
	movq	40(%rsp), %rax
	movzbl	(%rax), %ecx
.LBB23_77:                              # %if.end216
	andq	$255, %rcx
	cmpq	$58, %rcx
	je	.LBB23_79
# BB#78:                                # %if.end216
	cmpq	$61, %rcx
	jne	.LBB23_92
.LBB23_79:                              # %if.then224
	movabsq	$.L.str20, %rdi
	xorq	%rsi, %rsi
	callq	Perl_newSVpv
	movq	40(%rsp), %rsi
	incq	%rsi
	movq	%rax, %r14
	movq	%rsi, %rdx
	jmp	.LBB23_80
.LBB23_86:                              # %while.body262
                                        #   in Loop: Header=BB23_80 Depth=1
	incq	%rdx
.LBB23_80:                              # %while.cond229
                                        # =>This Inner Loop Header: Depth=1
	movq	%rdx, 40(%rsp)
	movzbl	(%rdx), %eax
	leaq	-65(%rax), %rcx
	andq	$255, %rcx
	cmpq	$26, %rcx
	jb	.LBB23_86
# BB#81:                                # %while.cond229
                                        #   in Loop: Header=BB23_80 Depth=1
	leaq	-97(%rax), %rcx
	andq	$255, %rcx
	cmpq	$26, %rcx
	jb	.LBB23_86
# BB#82:                                # %while.cond229
                                        #   in Loop: Header=BB23_80 Depth=1
	leaq	-48(%rax), %rcx
	andq	$255, %rcx
	cmpq	$10, %rcx
	jb	.LBB23_86
# BB#83:                                # %lor.end261
                                        #   in Loop: Header=BB23_80 Depth=1
	andq	$255, %rax
	cmpq	$58, %rax
	je	.LBB23_86
# BB#84:                                # %lor.end261
                                        #   in Loop: Header=BB23_80 Depth=1
	cmpq	$61, %rax
	je	.LBB23_87
# BB#85:                                # %lor.end261
                                        #   in Loop: Header=BB23_80 Depth=1
	cmpq	$95, %rax
	je	.LBB23_86
	jmp	.LBB23_88
.LBB23_105:                             # %sw.bb350
	movq	PL_origargv(%rip), %rax
	movq	(%rax), %rdi
	xorq	%rax, %rax
	callq	usage
	movq	$0, PL_statusvalue(%rip)
	xorq	%rax, %rax
	callq	my_exit_jump
.LBB23_106:                             # %sw.bb353
	movq	PL_inplace(%rip), %rdi
	testq	%rdi, %rdi
	je	.LBB23_108
# BB#107:                               # %if.then355
	callq	Perl_safesysfree
.LBB23_108:                             # %if.end356
	movq	40(%rsp), %rdi
	incq	%rdi
	callq	Perl_savepv
	movq	%rax, %rbx
	movq	%rbx, PL_inplace(%rip)
	movabsq	$4294981120, %rax       # imm = 0x100003600
	jmp	.LBB23_109
.LBB23_112:                             # %for.inc386.critedge
                                        #   in Loop: Header=BB23_109 Depth=1
	incq	%rbx
.LBB23_109:                             # %for.cond359
                                        # =>This Inner Loop Header: Depth=1
	movq	%rbx, 40(%rsp)
	movzbl	(%rbx), %ecx
	cmpq	$32, %rcx
	ja	.LBB23_112
# BB#110:                               # %for.cond359
                                        #   in Loop: Header=BB23_109 Depth=1
	movq	$1, %rdx
	shlq	%cl, %rdx
	testq	%rax, %rdx
	jne	.LBB23_113
# BB#111:                               # %for.cond359
                                        #   in Loop: Header=BB23_109 Depth=1
	testq	%rcx, %rcx
	jne	.LBB23_112
	jmp	.LBB23_198
.LBB23_113:                             # %if.then390
	leaq	1(%rbx), %rax
	movq	%rax, 40(%rsp)
	xorq	%rax, %rax
	movb	%al, (%rbx)
	movq	40(%rsp), %rbx
	movzbl	(%rbx), %eax
	cmpq	$45, %rax
	jne	.LBB23_198
	jmp	.LBB23_114
.LBB23_133:                             # %sw.bb509
	movq	$1, %rax
	movq	PL_ors_sv(%rip), %rdi
	movb	%al, PL_minus_l(%rip)
	incq	%rdx
	movq	%rdx, 40(%rsp)
	testq	%rdi, %rdi
	je	.LBB23_135
# BB#134:                               # %if.then512
	callq	Perl_sv_free
	movq	40(%rsp), %rdx
	movq	$0, PL_ors_sv(%rip)
.LBB23_135:                             # %if.end513
	movzbl	(%rdx), %eax
	addq	$-48, %rax
	andq	$255, %rax
	cmpq	$9, %rax
	ja	.LBB23_139
# BB#136:                               # %if.then521
	movq	$0, 8(%rsp)
	movabsq	$.L.str, %rdi
	movq	$1, %rsi
	callq	Perl_newSVpvn
	movq	40(%rsp), %rdi
	movq	%rax, PL_ors_sv(%rip)
	movzbl	(%rdi), %eax
	cmpq	$48, %rax
	movq	$4, %rax
	je	.LBB23_138
# BB#137:                               # %select.mid
	movq	$3, %rax
.LBB23_138:                             # %select.end
	movq	%rax, 32(%rsp)
	leaq	32(%rsp), %rsi
	leaq	8(%rsp), %rdx
	xorq	%rcx, %rcx
	callq	Perl_grok_oct
	movq	PL_ors_sv(%rip), %rcx
	movq	(%rcx), %rcx
	movq	(%rcx), %rcx
	movb	%al, (%rcx)
	movq	32(%rsp), %rax
	addq	%rax, 40(%rsp)
	movq	40(%rsp), %rbx
	movq	%rbx, %rax
	jmp	.LBB23_194
.LBB23_169:                             # %sw.bb642
	movq	$1, %rax
	movb	%al, PL_minus_n(%rip)
	jmp	.LBB23_175
.LBB23_170:                             # %sw.bb644
	movq	$1, %rax
	movb	%al, PL_minus_p(%rip)
	jmp	.LBB23_175
.LBB23_171:                             # %sw.bb646
	movabsq	$.L.str38, %rdi
	xorq	%rax, %rax
	callq	forbid_setid
	movq	40(%rsp), %rbx
	movq	$1, %rax
	movb	%al, PL_doswitches(%rip)
	jmp	.LBB23_114
.LBB23_172:                             # %sw.bb649
	movzbl	PL_tainting(%rip), %eax
	testq	%rax, %rax
	jne	.LBB23_175
# BB#173:                               # %if.then651
	movabsq	$.L.str39, %rdi
	movabsq	$.L.str1, %rdx
	movq	$116, %rsi
.LBB23_174:                             # %if.end652
	xorq	%rax, %rax
	callq	Perl_croak
	movq	40(%rsp), %rdx
	jmp	.LBB23_175
.LBB23_178:                             # %sw.bb659
	movq	$1, %rax
	movb	%al, PL_do_undump(%rip)
.LBB23_175:                             # %if.end652
	incq	%rdx
	movq	%rdx, 40(%rsp)
.LBB23_197:                             # %return
	movq	%rdx, %rbx
.LBB23_198:                             # %return
	movq	%rbx, %rax
.LBB23_194:                             # %select.end37
	addq	$48, %rsp
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.LBB23_180:                             # %sw.bb663
	callq	Perl_PerlIO_stdout
	movq	PL_patchlevel(%rip), %rsi
	movq	%rax, %rbx
	movabsq	$.L.str40, %rdi
	movabsq	$.L.str41, %rdx
	xorq	%rax, %rax
	callq	Perl_form
	movq	%rax, %rcx
	xorq	%rax, %rax
	movq	%rbx, %rdi
	movq	%rcx, %rsi
	callq	PerlIO_printf
	callq	Perl_PerlIO_stdout
	movq	%rax, %rcx
	movabsq	$.L.str42, %rsi
	xorq	%rax, %rax
	movq	%rcx, %rdi
	callq	PerlIO_printf
	callq	Perl_PerlIO_stdout
	movq	%rax, %rcx
	movabsq	$.L.str43, %rsi
	xorq	%rax, %rax
	movq	%rcx, %rdi
	callq	PerlIO_printf
	movq	$0, PL_statusvalue(%rip)
	xorq	%rax, %rax
	callq	my_exit_jump
.LBB23_181:                             # %sw.bb671
	movzbl	PL_dowarn(%rip), %eax
	testq	$6, %rax
	jne	.LBB23_183
# BB#182:                               # %if.then675
	orq	$1, %rax
	movb	%al, PL_dowarn(%rip)
.LBB23_183:                             # %if.end679
	movq	40(%rsp), %rbx
.LBB23_114:                             # %if.then395
	incq	%rbx
	movq	%rbx, 40(%rsp)
	movq	%rbx, %rax
	jmp	.LBB23_194
.LBB23_146:                             # %sw.bb551
	cmpq	$45, %rcx
	jne	.LBB23_147
# BB#148:                               # %if.then564
	addq	$2, %rax
	movq	%rax, 40(%rsp)
	movabsq	$.L.str31, %rdi
	jmp	.LBB23_149
.LBB23_147:
	movabsq	$.L.str30, %rdi
.LBB23_149:                             # %if.end566
	xorq	%rsi, %rsi
	xorq	%rbx, %rbx
	callq	Perl_newSVpv
	movq	40(%rsp), %r15
	movq	%rax, %r14
	jmp	.LBB23_150
.LBB23_156:                             # %while.body601
                                        #   in Loop: Header=BB23_150 Depth=1
	leaq	1(%r15,%rbx), %rax
	movq	%rax, 40(%rsp)
	incq	%rbx
.LBB23_150:                             # %while.cond568
                                        # =>This Inner Loop Header: Depth=1
	movzbl	(%r15,%rbx), %eax
	leaq	-65(%rax), %rcx
	andq	$255, %rcx
	cmpq	$26, %rcx
	jb	.LBB23_156
# BB#151:                               # %while.cond568
                                        #   in Loop: Header=BB23_150 Depth=1
	leaq	-97(%rax), %rcx
	andq	$255, %rcx
	cmpq	$26, %rcx
	jb	.LBB23_156
# BB#152:                               # %while.cond568
                                        #   in Loop: Header=BB23_150 Depth=1
	leaq	-48(%rax), %rcx
	andq	$255, %rcx
	cmpq	$10, %rcx
	jb	.LBB23_156
# BB#153:                               # %lor.end600
                                        #   in Loop: Header=BB23_150 Depth=1
	andq	$255, %rax
	cmpq	$58, %rax
	je	.LBB23_156
# BB#154:                               # %lor.end600
                                        #   in Loop: Header=BB23_150 Depth=1
	cmpq	$61, %rax
	je	.LBB23_161
# BB#155:                               # %lor.end600
                                        #   in Loop: Header=BB23_150 Depth=1
	cmpq	$95, %rax
	je	.LBB23_156
# BB#157:                               # %if.then607
	movq	%r14, %rdi
	movq	%r15, %rsi
	callq	Perl_sv_catpv
	movzbl	-1(%r15), %eax
	cmpq	$109, %rax
	jne	.LBB23_165
# BB#158:                               # %if.then612
	movq	40(%rsp), %rax
	movzbl	(%rax), %esi
	testq	%rsi, %rsi
	je	.LBB23_160
# BB#159:                               # %if.then616
	movq	$56, %rcx
	shlq	%cl, %rsi
	movq	$56, %rcx
	sarq	%cl, %rsi
	movabsq	$.L.str32, %rdi
	xorq	%rax, %rax
	callq	Perl_croak
.LBB23_160:                             # %if.end618
	movabsq	$.L.str33, %rsi
	movq	%r14, %rdi
	callq	Perl_sv_catpv
	jmp	.LBB23_165
.LBB23_161:                             # %if.else620
	testq	%rbx, %rbx
	je	.LBB23_163
# BB#162:
	addq	%r15, %rbx
	jmp	.LBB23_164
.LBB23_139:                             # %if.else535
	movq	PL_rs(%rip), %rdi
	movzbl	18(%rdi), %eax
	testq	$4, %rax
	je	.LBB23_142
# BB#140:                               # %land.lhs.true539
	movq	(%rdi), %rax
	cmpq	$0, 8(%rax)
	je	.LBB23_141
.LBB23_142:                             # %if.else545
	callq	Perl_newSVsv
	jmp	.LBB23_143
.LBB23_118:
	movq	%rdi, %rsi
	jmp	.LBB23_121
.LBB23_128:                             # %for.inc462.critedge
                                        #   in Loop: Header=BB23_121 Depth=1
	incq	%rsi
	movzbl	(%rsi), %eax
.LBB23_121:                             # %for.cond435
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB23_124 Depth 2
	movq	%rax, %rcx
	andq	$255, %rcx
	cmpq	$32, %rcx
	ja	.LBB23_128
# BB#122:                               # %for.cond435
                                        #   in Loop: Header=BB23_121 Depth=1
	movq	$1, %rdx
	shlq	%cl, %rdx
	movabsq	$4294981121, %rcx       # imm = 0x100003601
	testq	%rcx, %rdx
	je	.LBB23_128
# BB#123:                               # %while.cond465.preheader
                                        #   in Loop: Header=BB23_121 Depth=1
	leaq	1(%rsi), %r14
	movabsq	$4294981120, %rdx       # imm = 0x100003600
	jmp	.LBB23_124
.LBB23_129:                             # %while.body486
                                        #   in Loop: Header=BB23_124 Depth=2
	movzbl	(%r14), %eax
	incq	%r14
.LBB23_124:                             # %while.cond465
                                        #   Parent Loop BB23_121 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	andq	$255, %rax
	cmpq	$45, %rax
	ja	.LBB23_127
# BB#125:                               # %while.cond465
                                        #   in Loop: Header=BB23_124 Depth=2
	movq	$1, %rbx
	movq	%rax, %rcx
	shlq	%cl, %rbx
	testq	%rdx, %rbx
	jne	.LBB23_129
# BB#126:                               # %while.cond465
                                        #   in Loop: Header=BB23_121 Depth=1
	movq	$1, %rdx
	movq	%rax, %rcx
	shlq	%cl, %rdx
	movabsq	$35184372088833, %rax   # imm = 0x200000000001
	testq	%rax, %rdx
	jne	.LBB23_130
.LBB23_127:                             # %for.cond435.backedge.loopexit
                                        #   in Loop: Header=BB23_121 Depth=1
	decq	%r14
	movzbl	(%r14), %eax
	movq	%r14, %rsi
	jmp	.LBB23_121
.LBB23_130:                             # %do.end
	subq	%rdi, %rsi
	callq	Perl_savepvn
	movq	%rax, %rbx
	movq	$1, %rsi
	movq	$1, %rdx
	xorq	%rcx, %rcx
	xorq	%rax, %rax
	movq	%rbx, %rdi
	callq	incpush
	movq	%rbx, %rdi
	callq	Perl_safesysfree
	leaq	-1(%r14), %rbx
	movq	%rbx, 40(%rsp)
	movzbl	-1(%r14), %eax
	cmpq	$45, %rax
	jne	.LBB23_198
# BB#131:                               # %if.then504
	movq	%r14, 40(%rsp)
	movq	%r14, %rbx
	movq	%rbx, %rax
	jmp	.LBB23_194
.LBB23_87:                              # %if.else269
	subq	%rsi, %rdx
	movq	$2, %rcx
	movq	%r14, %rdi
	callq	Perl_sv_catpvn_flags
	movabsq	$.L.str21, %rsi
	movq	%r14, %rdi
	callq	Perl_sv_catpv
	movq	40(%rsp), %rsi
	incq	%rsi
	movq	%rsi, 40(%rsp)
	movq	%r14, %rdi
	callq	Perl_sv_catpv
	movabsq	$.L.str22, %rsi
.LBB23_88:                              # %if.end274
	movq	%r14, %rdi
	callq	Perl_sv_catpv
	movq	40(%rsp), %rbx
	movq	%rbx, %rdi
	callq	strlen
	addq	%rbx, %rax
	movq	%rax, 40(%rsp)
	movzbl	18(%r14), %eax
	testq	$4, %rax
	je	.LBB23_90
# BB#89:                                # %cond.true281
	movq	(%r14), %rax
	movq	8(%rax), %rcx
	movq	%rcx, PL_na(%rip)
	movq	(%rax), %rsi
	jmp	.LBB23_91
.LBB23_163:                             # %if.then623
	movsbq	-1(%r15), %rsi
	movabsq	$.L.str34, %rdi
	xorq	%rax, %rax
	callq	Perl_croak
	movq	40(%rsp), %rbx
.LBB23_164:                             # %if.end626
	subq	%r15, %rbx
	movq	$2, %rcx
	movq	%r14, %rdi
	movq	%r15, %rsi
	movq	%rbx, %rdx
	callq	Perl_sv_catpvn_flags
	movabsq	$.L.str35, %rsi
	movq	%r14, %rdi
	callq	Perl_sv_catpv
	movabsq	$.L.str36, %rbx
	movq	$1, %rdx
	movq	$2, %rcx
	movq	%r14, %rdi
	movq	%rbx, %rsi
	callq	Perl_sv_catpvn_flags
	movq	40(%rsp), %rsi
	incq	%rsi
	movq	%rsi, 40(%rsp)
	movq	%r14, %rdi
	callq	Perl_sv_catpv
	movq	$2, %rdx
	movq	$2, %rcx
	movq	%r14, %rdi
	movq	%rbx, %rsi
	callq	Perl_sv_catpvn_flags
.LBB23_165:                             # %if.end631
	movq	40(%rsp), %rbx
	movq	%rbx, %rdi
	callq	strlen
	movq	PL_preambleav(%rip), %rdi
	addq	%rbx, %rax
	movq	%rax, 40(%rsp)
	testq	%rdi, %rdi
	jne	.LBB23_167
# BB#166:                               # %if.then635
	callq	Perl_newAV
	movq	%rax, %rdi
	movq	%rdi, PL_preambleav(%rip)
.LBB23_167:                             # %if.end637
	movq	%r14, %rsi
	callq	Perl_av_push
	movq	40(%rsp), %rbx
	movq	%rbx, %rax
	jmp	.LBB23_194
.LBB23_90:                              # %cond.false286
	movabsq	$PL_na, %rsi
	movq	$2, %rdx
	movq	%r14, %rdi
	callq	Perl_sv_2pv_flags
	movq	%rax, %rsi
.LBB23_91:                              # %cond.end288
	movabsq	$.L.str23, %rdi
	callq	Perl_my_setenv
.LBB23_92:                              # %if.end290
	cmpq	$0, PL_perldb(%rip)
	jne	.LBB23_94
# BB#93:                                # %if.then292
	movq	$831, PL_perldb(%rip)   # imm = 0x33F
	xorq	%rax, %rax
	callq	init_debugger
.LBB23_94:                              # %if.end294
	movq	40(%rsp), %rbx
	movq	%rbx, %rax
	jmp	.LBB23_194
.LBB23_141:                             # %if.then543
	movabsq	$.L.str27, %rdi
	movq	$2, %rsi
	callq	Perl_newSVpvn
.LBB23_143:                             # %return
	movq	40(%rsp), %rbx
	movq	%rax, PL_ors_sv(%rip)
	movq	%rbx, %rax
	jmp	.LBB23_194
.LBB23_29:
	movabsq	$0, %rcx
.LBB23_31:                              # %cond.false29
	addq	$7, %rcx
.LBB23_32:                              # %cond.end39
	cmpq	%rcx, %rsi
	jae	.LBB23_43
# BB#33:                                # %cond.true44
	movq	$2, %rsi
	cmpq	$128, %r14
	jb	.LBB23_42
# BB#34:                                # %cond.false48
	movq	$3, %rsi
	cmpq	$2048, %r14             # imm = 0x800
	jb	.LBB23_42
# BB#35:                                # %cond.false52
	movq	$4, %rsi
	cmpq	$65536, %r14            # imm = 0x10000
	jb	.LBB23_42
# BB#36:                                # %cond.false56
	movq	$5, %rsi
	cmpq	$2097152, %r14          # imm = 0x200000
	jb	.LBB23_42
# BB#37:                                # %cond.false60
	movq	$6, %rsi
	cmpq	$67108864, %r14         # imm = 0x4000000
	jb	.LBB23_42
# BB#38:                                # %cond.false64
	movq	$31, %rcx
	movq	%r14, %rdx
	shrq	%cl, %rdx
	testq	%rdx, %rdx
	je	.LBB23_39
# BB#40:                                # %cond.false64
	movabsq	$1, %rsi
	jmp	.LBB23_41
.LBB23_39:
	movabsq	$0, %rsi
.LBB23_41:                              # %cond.false64
	addq	$7, %rsi
.LBB23_42:                              # %cond.end76
	movq	%rax, %rdi
	callq	Perl_sv_grow
	movq	PL_rs(%rip), %rax
	movq	(%rax), %rdx
.LBB23_43:                              # %cond.end83
	movq	(%rdx), %rdi
	movq	%r14, %rsi
	callq	Perl_uvuni_to_utf8
	cmpq	$128, %r14
	movq	$1, %rax
	jb	.LBB23_52
# BB#44:                                # %cond.false91
	movq	$2, %rax
	cmpq	$2048, %r14             # imm = 0x800
	jb	.LBB23_52
# BB#45:                                # %cond.false95
	movq	$3, %rax
	cmpq	$65536, %r14            # imm = 0x10000
	jb	.LBB23_52
# BB#46:                                # %cond.false99
	movq	$4, %rax
	cmpq	$2097152, %r14          # imm = 0x200000
	jb	.LBB23_52
# BB#47:                                # %cond.false103
	movq	$5, %rax
	cmpq	$67108864, %r14         # imm = 0x4000000
	jb	.LBB23_52
# BB#48:                                # %cond.false107
	movq	$31, %rcx
	shrq	%cl, %r14
	testq	%r14, %r14
	je	.LBB23_49
# BB#50:                                # %cond.false107
	movabsq	$1, %rax
	jmp	.LBB23_51
.LBB23_49:
	movabsq	$0, %rax
.LBB23_51:                              # %cond.false107
	orq	$6, %rax
.LBB23_52:                              # %cond.end119
	movq	PL_rs(%rip), %rcx
	movq	(%rcx), %rdx
	movq	%rax, 8(%rdx)
	orq	$536870912, 16(%rcx)    # imm = 0x20000000
.LBB23_60:                              # %if.end139
	movabsq	$.L.str16, %rdi
	movq	$1, %rsi
	movq	$4, %rdx
	callq	Perl_gv_fetchpv
	xorq	%rdi, %rdi
	testq	%rax, %rax
	je	.LBB23_62
# BB#61:                                # %if.then.i
	movq	(%rax), %rax
	movq	56(%rax), %rax
	movq	(%rax), %rdi
.LBB23_62:                              # %Perl_get_sv.exit
	movq	PL_rs(%rip), %rsi
	movq	$2, %rdx
	callq	Perl_sv_setsv_flags
	movq	40(%rsp), %rax
	movq	32(%rsp), %rbx
	addq	%rax, %rbx
	movq	%rbx, %rax
	jmp	.LBB23_194
.Ltmp220:
	.size	Perl_moreswitches, .Ltmp220-Perl_moreswitches
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI23_0:
	.quad	.LBB23_63
	.quad	.LBB23_95
	.quad	.LBB23_5
	.quad	.LBB23_64
	.quad	.LBB23_5
	.quad	.LBB23_5
	.quad	.LBB23_115
	.quad	.LBB23_5
	.quad	.LBB23_5
	.quad	.LBB23_5
	.quad	.LBB23_144
	.quad	.LBB23_5
	.quad	.LBB23_5
	.quad	.LBB23_195
	.quad	.LBB23_5
	.quad	.LBB23_5
	.quad	.LBB23_5
	.quad	.LBB23_176
	.quad	.LBB23_179
	.quad	.LBB23_5
	.quad	.LBB23_184
	.quad	.LBB23_188
	.quad	.LBB23_5
	.quad	.LBB23_5
	.quad	.LBB23_5
	.quad	.LBB23_5
	.quad	.LBB23_5
	.quad	.LBB23_5
	.quad	.LBB23_5
	.quad	.LBB23_5
	.quad	.LBB23_69
	.quad	.LBB23_5
	.quad	.LBB23_70
	.quad	.LBB23_71
	.quad	.LBB23_5
	.quad	.LBB23_5
	.quad	.LBB23_5
	.quad	.LBB23_105
	.quad	.LBB23_106
	.quad	.LBB23_5
	.quad	.LBB23_5
	.quad	.LBB23_133
	.quad	.LBB23_145
	.quad	.LBB23_169
	.quad	.LBB23_5
	.quad	.LBB23_170
	.quad	.LBB23_5
	.quad	.LBB23_5
	.quad	.LBB23_171
	.quad	.LBB23_172
	.quad	.LBB23_178
	.quad	.LBB23_180
	.quad	.LBB23_181

	.text
	.globl	Perl_my_unexec
	.align	16, 0x90
	.type	Perl_my_unexec,@function
Perl_my_unexec:                         # @Perl_my_unexec
	.cfi_startproc
# BB#0:                                 # %entry
	retq
.Ltmp221:
	.size	Perl_my_unexec, .Ltmp221-Perl_my_unexec
	.cfi_endproc

	.globl	Perl_doing_taint
	.align	16, 0x90
	.type	Perl_doing_taint,@function
Perl_doing_taint:                       # @Perl_doing_taint
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp228:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp229:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp230:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp231:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp232:
	.cfi_def_cfa_offset 48
.Ltmp233:
	.cfi_offset %rbx, -48
.Ltmp234:
	.cfi_offset %r12, -40
.Ltmp235:
	.cfi_offset %r13, -32
.Ltmp236:
	.cfi_offset %r14, -24
.Ltmp237:
	.cfi_offset %r15, -16
	movq	%rsi, %r14
	movq	%rdi, %r15
	callq	getuid
	movq	%rax, %rbx
	callq	geteuid
	movq	%rax, %r13
	callq	getgid
	movq	%rax, %r12
	callq	getegid
	movq	%rax, %rcx
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	testq	%rdx, %rbx
	je	.LBB25_3
# BB#1:                                 # %land.lhs.true
	andq	%rdx, %rbx
	andq	%rdx, %r13
	cmpq	%rbx, %r13
	movq	$1, %rax
	jne	.LBB25_7
# BB#2:                                 # %land.lhs.true
	andq	%rdx, %r12
	andq	%rdx, %rcx
	cmpq	%r12, %rcx
	jne	.LBB25_7
.LBB25_3:                               # %if.end
	movq	$32, %rcx
	shlq	%cl, %r15
	movq	$32, %rcx
	sarq	%cl, %r15
	cmpq	$2, %r15
	jl	.LBB25_6
# BB#4:                                 # %land.lhs.true6
	movq	8(%r14), %rax
	movzbl	(%rax), %ecx
	cmpq	$45, %rcx
	jne	.LBB25_6
# BB#5:                                 # %land.lhs.true10
	movzbl	1(%rax), %ecx
	movq	$1, %rax
	orq	$32, %rcx
	andq	$255, %rcx
	cmpq	$116, %rcx
	je	.LBB25_7
.LBB25_6:                               # %if.end23
	xorq	%rax, %rax
.LBB25_7:                               # %return
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp238:
	.size	Perl_doing_taint, .Ltmp238-Perl_doing_taint
	.cfi_endproc

	.globl	Perl_init_debugger
	.align	16, 0x90
	.type	Perl_init_debugger,@function
Perl_init_debugger:                     # @Perl_init_debugger
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp241:
	.cfi_def_cfa_offset 16
.Ltmp242:
	.cfi_offset %rbx, -16
	movq	PL_debstash(%rip), %rax
	movq	PL_curstash(%rip), %rbx
	movq	%rax, PL_curstash(%rip)
	movabsq	$.L.str45, %rdi
	movq	$2, %rsi
	movq	$10, %rdx
	callq	Perl_gv_fetchpv
	movq	%rax, %rdi
	callq	Perl_gv_AVadd
	movq	(%rax), %rax
	movq	56(%rax), %rax
	movq	32(%rax), %rax
	movq	%rax, PL_dbargs(%rip)
	movq	(%rax), %rax
	movzbl	72(%rax), %ecx
	andq	$254, %rcx
	movb	%cl, 72(%rax)
	movabsq	$.L.str46, %rdi
	movq	$2, %rsi
	movq	$13, %rdx
	callq	Perl_gv_fetchpv
	movq	%rax, PL_DBgv(%rip)
	movabsq	$.L.str47, %rdi
	movq	$2, %rsi
	movq	$10, %rdx
	callq	Perl_gv_fetchpv
	movq	%rax, PL_DBline(%rip)
	movabsq	$.L.str48, %rdi
	movq	$2, %rsi
	movq	$11, %rdx
	callq	Perl_gv_fetchpv
	movq	%rax, %rdi
	callq	Perl_gv_HVadd
	movq	%rax, PL_DBsub(%rip)
	movq	(%rax), %rax
	movq	56(%rax), %rax
	movq	(%rax), %rdi
	movq	$1, %rsi
	callq	Perl_sv_upgrade
	movabsq	$.L.str49, %rdi
	movq	$2, %rsi
	movq	$4, %rdx
	callq	Perl_gv_fetchpv
	movq	(%rax), %rax
	movq	56(%rax), %rax
	movq	(%rax), %rdi
	movq	%rdi, PL_DBsingle(%rip)
	xorq	%rsi, %rsi
	callq	Perl_sv_setiv
	movabsq	$.L.str50, %rdi
	movq	$2, %rsi
	movq	$4, %rdx
	callq	Perl_gv_fetchpv
	movq	(%rax), %rax
	movq	56(%rax), %rax
	movq	(%rax), %rdi
	movq	%rdi, PL_DBtrace(%rip)
	xorq	%rsi, %rsi
	callq	Perl_sv_setiv
	movabsq	$.L.str51, %rdi
	movq	$2, %rsi
	movq	$4, %rdx
	callq	Perl_gv_fetchpv
	movq	(%rax), %rax
	movq	56(%rax), %rax
	movq	(%rax), %rdi
	movq	%rdi, PL_DBsignal(%rip)
	xorq	%rsi, %rsi
	callq	Perl_sv_setiv
	movq	%rbx, PL_curstash(%rip)
	popq	%rbx
	retq
.Ltmp243:
	.size	Perl_init_debugger, .Ltmp243-Perl_init_debugger
	.cfi_endproc

	.globl	Perl_init_argv_symbols
	.align	16, 0x90
	.type	Perl_init_argv_symbols,@function
Perl_init_argv_symbols:                 # @Perl_init_argv_symbols
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp250:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp251:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp252:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp253:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp254:
	.cfi_def_cfa_offset 48
.Ltmp255:
	.cfi_offset %rbx, -48
.Ltmp256:
	.cfi_offset %r12, -40
.Ltmp257:
	.cfi_offset %r13, -32
.Ltmp258:
	.cfi_offset %r14, -24
.Ltmp259:
	.cfi_offset %r15, -16
	movzbl	PL_doswitches(%rip), %eax
	movq	%rsi, %r15
	movq	%rdi, %r14
	decq	%r14
	addq	$8, %r15
	testq	%rax, %rax
	je	.LBB27_12
# BB#1:                                 # %entry
	movq	$32, %rcx
	movq	%r14, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jle	.LBB27_12
# BB#2:
	xorq	%r13, %r13
	.align	16, 0x90
.LBB27_3:                               # %land.rhs
                                        # =>This Inner Loop Header: Depth=1
	movq	(%r15), %rbx
	movzbl	(%rbx), %eax
	cmpq	$45, %rax
	jne	.LBB27_12
# BB#4:                                 # %for.body
                                        #   in Loop: Header=BB27_3 Depth=1
	movzbl	1(%rbx), %eax
	testq	%rax, %rax
	je	.LBB27_12
# BB#5:                                 # %for.body
                                        #   in Loop: Header=BB27_3 Depth=1
	cmpq	$45, %rax
	jne	.LBB27_8
# BB#6:                                 # %land.lhs.true
                                        #   in Loop: Header=BB27_3 Depth=1
	movzbl	2(%rbx), %eax
	testq	%rax, %rax
	je	.LBB27_7
.LBB27_8:                               # %if.end17
                                        #   in Loop: Header=BB27_3 Depth=1
	movq	$61, %rsi
	movq	%rbx, %rdi
	callq	strchr
	movq	%rax, %r12
	testq	%r12, %r12
	jne	.LBB27_9
# BB#10:                                # %if.else
                                        #   in Loop: Header=BB27_3 Depth=1
	incq	%rbx
	movq	$1, %rsi
	movq	$4, %rdx
	movq	%rbx, %rdi
	callq	Perl_gv_fetchpv
	movq	(%rax), %rax
	movq	56(%rax), %rax
	movq	(%rax), %rdi
	movq	$1, %rsi
	callq	Perl_sv_setiv
	jmp	.LBB27_11
	.align	16, 0x90
.LBB27_9:                               # %if.then20
                                        #   in Loop: Header=BB27_3 Depth=1
	movb	%r13b, (%r12)
	movq	(%r15), %rdi
	incq	%r12
	incq	%rdi
	movq	$1, %rsi
	movq	$4, %rdx
	callq	Perl_gv_fetchpv
	movq	(%rax), %rax
	movq	56(%rax), %rax
	movq	(%rax), %rdi
	movq	%r12, %rsi
	callq	Perl_sv_setpv
.LBB27_11:                              # %for.inc
                                        #   in Loop: Header=BB27_3 Depth=1
	decq	%r14
	movq	$32, %rcx
	movq	%r14, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	addq	$8, %r15
	testq	%rax, %rax
	jg	.LBB27_3
	jmp	.LBB27_12
.LBB27_7:                               # %if.then14
	decq	%r14
	addq	$8, %r15
.LBB27_12:                              # %if.end33
	movabsq	$.L.str52, %rdi
	movq	$1, %rsi
	movq	$10, %rdx
	callq	Perl_gv_fetchpv
	testq	%rax, %rax
	movq	%rax, PL_argvgv(%rip)
	je	.LBB27_27
# BB#13:                                # %if.then36
	movq	(%rax), %rcx
	movzbl	88(%rcx), %edx
	orq	$2, %rdx
	movb	%dl, 88(%rcx)
	movq	%rax, %rdi
	callq	Perl_gv_AVadd
	movq	PL_argvgv(%rip), %rdi
	movq	(%rdi), %rax
	movq	56(%rax), %rax
	movq	32(%rax), %rax
	testq	%rax, %rax
	jne	.LBB27_15
# BB#14:                                # %cond.false
	callq	Perl_gv_AVadd
	movq	(%rax), %rax
	movq	56(%rax), %rax
	movq	32(%rax), %rax
.LBB27_15:                              # %cond.end
	movq	%rax, %rdi
	callq	Perl_av_clear
	movq	$32, %rcx
	movq	%r14, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jle	.LBB27_27
# BB#16:
	movabsq	$0, %r12
	movabsq	$1, %r13
	.align	16, 0x90
.LBB27_17:                              # %for.body54
                                        # =>This Inner Loop Header: Depth=1
	movq	(%r15), %rdi
	xorq	%rsi, %rsi
	callq	Perl_newSVpv
	movq	PL_argvgv(%rip), %rdi
	movq	(%rdi), %rcx
	movq	56(%rcx), %rcx
	movq	32(%rcx), %rcx
	movq	%rax, %rbx
	testq	%rcx, %rcx
	jne	.LBB27_19
# BB#18:                                # %cond.false65
                                        #   in Loop: Header=BB27_17 Depth=1
	callq	Perl_gv_AVadd
	movq	(%rax), %rax
	movq	56(%rax), %rax
	movq	32(%rax), %rcx
.LBB27_19:                              # %cond.end70
                                        #   in Loop: Header=BB27_17 Depth=1
	movq	%rcx, %rdi
	movq	%rbx, %rsi
	callq	Perl_av_push
	movq	PL_unicode(%rip), %rax
	movzbl	PL_utf8locale(%rip), %esi
	movq	$6, %rcx
	movq	%rax, %rdx
	shrq	%cl, %rdx
	testq	%rsi, %rsi
	movq	%r12, %rcx
	jne	.LBB27_21
# BB#20:                                # %cond.end70
                                        #   in Loop: Header=BB27_17 Depth=1
	movq	%r13, %rcx
.LBB27_21:                              # %cond.end70
                                        #   in Loop: Header=BB27_17 Depth=1
	testq	%rcx, %rdx
	jne	.LBB27_24
# BB#22:                                # %cond.end70
                                        #   in Loop: Header=BB27_17 Depth=1
	movq	%rax, %rcx
	andq	$32, %rcx
	je	.LBB27_24
# BB#23:                                # %if.then78
                                        #   in Loop: Header=BB27_17 Depth=1
	orq	$536870912, 16(%rbx)    # imm = 0x20000000
	movq	PL_unicode(%rip), %rax
.LBB27_24:                              # %if.end81
                                        #   in Loop: Header=BB27_17 Depth=1
	testq	$128, %rax
	je	.LBB27_26
# BB#25:                                # %if.then84
                                        #   in Loop: Header=BB27_17 Depth=1
	movq	%rbx, %rdi
	callq	Perl_sv_utf8_decode
.LBB27_26:                              # %for.inc87
                                        #   in Loop: Header=BB27_17 Depth=1
	decq	%r14
	movq	$32, %rcx
	movq	%r14, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	addq	$8, %r15
	testq	%rax, %rax
	jg	.LBB27_17
.LBB27_27:                              # %if.end91
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp260:
	.size	Perl_init_argv_symbols, .Ltmp260-Perl_init_argv_symbols
	.cfi_endproc

	.globl	Perl_my_failure_exit
	.align	16, 0x90
	.type	Perl_my_failure_exit,@function
Perl_my_failure_exit:                   # @Perl_my_failure_exit
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rax
.Ltmp262:
	.cfi_def_cfa_offset 16
	callq	__errno_location
	movzbl	(%rax), %eax
	testq	%rax, %rax
	je	.LBB28_3
# BB#1:                                 # %if.then
	callq	__errno_location
	movslq	(%rax), %rax
	cmpq	$-1, %rax
	movq	%rax, PL_statusvalue(%rip)
	je	.LBB28_7
# BB#2:                                 # %if.then3
	andq	$65535, %rax            # imm = 0xFFFF
	movq	%rax, PL_statusvalue(%rip)
	jmp	.LBB28_7
.LBB28_3:                               # %if.else
	movq	PL_statusvalue(%rip), %rax
	movq	$8, %rcx
	shrq	%cl, %rax
	testq	$255, %rax
	je	.LBB28_6
# BB#4:                                 # %if.then8
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	%rdx, PL_statusvalue(%rip)
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	cmpq	%rcx, %rax
	je	.LBB28_7
# BB#5:                                 # %if.then12
	andq	$65535, %rdx            # imm = 0xFFFF
	movq	%rdx, PL_statusvalue(%rip)
	jmp	.LBB28_7
.LBB28_6:                               # %if.else15
	movq	$255, PL_statusvalue(%rip)
.LBB28_7:                               # %if.end22
	xorq	%rax, %rax
	popq	%rdx
	jmp	my_exit_jump  # TAILCALL
.Ltmp263:
	.size	Perl_my_failure_exit, .Ltmp263-Perl_my_failure_exit
	.cfi_endproc

	.type	.L.str,@object          # @.str
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.asciz	"\n"
	.size	.L.str, 2

	.type	local_patches,@object   # @local_patches
	.local	local_patches
	.comm	local_patches,16,16
	.type	.L.str1,@object         # @.str1
.L.str1:
	.zero	1
	.size	.L.str1, 1

	.type	.L.str2,@object         # @.str2
.L.str2:
	.asciz	"Unbalanced scopes: %ld more ENTERs than LEAVEs\n"
	.size	.L.str2, 48

	.type	.L.str3,@object         # @.str3
.L.str3:
	.asciz	"Unbalanced saves: %ld more saves than restores\n"
	.size	.L.str3, 48

	.type	.L.str4,@object         # @.str4
.L.str4:
	.asciz	"Unbalanced tmps: %ld more allocs than frees\n"
	.size	.L.str4, 45

	.type	.L.str5,@object         # @.str5
.L.str5:
	.asciz	"Unbalanced context: %ld more PUSHes than POPs\n"
	.size	.L.str5, 47

	.type	.L.str6,@object         # @.str6
.L.str6:
	.asciz	"Unbalanced string table refcount: (%d) for \"%s\""
	.size	.L.str6, 48

	.type	.L.str7,@object         # @.str7
.L.str7:
	.asciz	"Scalars leaked: %ld\n"
	.size	.L.str7, 21

	.type	.L.str8,@object         # @.str8
.L.str8:
	.asciz	"NoNe  SuCh"
	.size	.L.str8, 11

	.type	.L.str9,@object         # @.str9
.L.str9:
	.asciz	"panic: top_env\n"
	.size	.L.str9, 16

	.type	.L.str10,@object        # @.str10
.L.str10:
	.asciz	"panic: POPSTACK\n"
	.size	.L.str10, 17

	.type	.L.str11,@object        # @.str11
.L.str11:
	.asciz	"panic: restartop\n"
	.size	.L.str11, 18

	.type	.L.str12,@object        # @.str12
.L.str12:
	.asciz	"Callback called exit"
	.size	.L.str12, 21

	.type	.L.str13,@object        # @.str13
.L.str13:
	.asciz	"eval_sv()"
	.size	.L.str13, 10

	.type	.L.str14,@object        # @.str14
.L.str14:
	.asciz	"require '"
	.size	.L.str14, 10

	.type	.L.str15,@object        # @.str15
.L.str15:
	.asciz	"'"
	.size	.L.str15, 2

	.type	.L.str16,@object        # @.str16
.L.str16:
	.asciz	"/"
	.size	.L.str16, 2

	.type	.L.str17,@object        # @.str17
.L.str17:
	.asciz	"-d"
	.size	.L.str17, 3

	.type	.L.str18,@object        # @.str18
.L.str18:
	.asciz	"PERL5DB_THREADED"
	.size	.L.str18, 17

	.type	.L.str19,@object        # @.str19
.L.str19:
	.asciz	"1"
	.size	.L.str19, 2

	.type	.L.str20,@object        # @.str20
.L.str20:
	.asciz	"use Devel::"
	.size	.L.str20, 12

	.type	.L.str21,@object        # @.str21
.L.str21:
	.asciz	" split(/,/,q{"
	.size	.L.str21, 14

	.type	.L.str22,@object        # @.str22
.L.str22:
	.asciz	"})"
	.size	.L.str22, 3

	.type	.L.str23,@object        # @.str23
.L.str23:
	.asciz	"PERL5DB"
	.size	.L.str23, 8

	.type	.L.str24,@object        # @.str24
.L.str24:
	.asciz	"Recompile perl with -DDEBUGGING to use -D switch (did you mean -d ?)\n"
	.size	.L.str24, 70

	.type	.L.str25,@object        # @.str25
.L.str25:
	.asciz	"-I"
	.size	.L.str25, 3

	.type	.L.str26,@object        # @.str26
.L.str26:
	.asciz	"No directory specified for -I"
	.size	.L.str26, 30

	.type	.L.str27,@object        # @.str27
.L.str27:
	.asciz	"\n\n"
	.size	.L.str27, 3

	.type	.L.str28,@object        # @.str28
.L.str28:
	.asciz	"-M"
	.size	.L.str28, 3

	.type	.L.str29,@object        # @.str29
.L.str29:
	.asciz	"-m"
	.size	.L.str29, 3

	.type	.L.str30,@object        # @.str30
.L.str30:
	.asciz	"use "
	.size	.L.str30, 5

	.type	.L.str31,@object        # @.str31
.L.str31:
	.asciz	"no "
	.size	.L.str31, 4

	.type	.L.str32,@object        # @.str32
.L.str32:
	.asciz	"Can't use '%c' after -mname"
	.size	.L.str32, 28

	.type	.L.str33,@object        # @.str33
.L.str33:
	.asciz	" ()"
	.size	.L.str33, 4

	.type	.L.str34,@object        # @.str34
.L.str34:
	.asciz	"Module name required with -%c option"
	.size	.L.str34, 37

	.type	.L.str35,@object        # @.str35
.L.str35:
	.asciz	" split(/,/,q"
	.size	.L.str35, 13

	.type	.L.str36,@object        # @.str36
	.section	.rodata,"a",@progbits
.L.str36:
	.asciz	"\000)"
	.size	.L.str36, 3

	.type	.L.str37,@object        # @.str37
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str37:
	.asciz	"Missing argument to -%c"
	.size	.L.str37, 24

	.type	.L.str38,@object        # @.str38
.L.str38:
	.asciz	"-s"
	.size	.L.str38, 3

	.type	.L.str39,@object        # @.str39
.L.str39:
	.asciz	"\"-%c\" is on the #! line, it must also be used on the command line%s"
	.size	.L.str39, 68

	.type	.L.str40,@object        # @.str40
.L.str40:
	.asciz	"\nThis is perl, v%vd built for %s"
	.size	.L.str40, 33

	.type	.L.str41,@object        # @.str41
.L.str41:
	.asciz	"spec-cpu"
	.size	.L.str41, 9

	.type	.L.str42,@object        # @.str42
.L.str42:
	.asciz	"\n\nCopyright 1987-2005, Larry Wall\n"
	.size	.L.str42, 35

	.type	.L.str43,@object        # @.str43
.L.str43:
	.asciz	"\nPerl may be copied only under the terms of either the Artistic License or the\nGNU General Public License, which may be found in the Perl 5 source kit.\n\nComplete documentation for Perl, including FAQ lists, should be found on\nthis system using `man perl' or `perldoc perl'.  If you have access to the\nInternet, point your browser at http://www.perl.org/, the Perl Home Page.\n\n"
	.size	.L.str43, 377

	.type	.L.str44,@object        # @.str44
.L.str44:
	.asciz	"Can't emulate -%.1s on #! line"
	.size	.L.str44, 31

	.type	.L.str45,@object        # @.str45
.L.str45:
	.asciz	"DB::args"
	.size	.L.str45, 9

	.type	.L.str46,@object        # @.str46
.L.str46:
	.asciz	"DB::DB"
	.size	.L.str46, 7

	.type	.L.str47,@object        # @.str47
.L.str47:
	.asciz	"DB::dbline"
	.size	.L.str47, 11

	.type	.L.str48,@object        # @.str48
.L.str48:
	.asciz	"DB::sub"
	.size	.L.str48, 8

	.type	.L.str49,@object        # @.str49
.L.str49:
	.asciz	"DB::single"
	.size	.L.str49, 11

	.type	.L.str50,@object        # @.str50
.L.str50:
	.asciz	"DB::trace"
	.size	.L.str50, 10

	.type	.L.str51,@object        # @.str51
.L.str51:
	.asciz	"DB::signal"
	.size	.L.str51, 11

	.type	.L.str52,@object        # @.str52
.L.str52:
	.asciz	"ARGV"
	.size	.L.str52, 5

	.type	.L.str53,@object        # @.str53
.L.str53:
	.asciz	"BEGIN failed--compilation aborted"
	.size	.L.str53, 34

	.type	.L.str54,@object        # @.str54
.L.str54:
	.asciz	"%s failed--call queue aborted"
	.size	.L.str54, 30

	.type	.L.str55,@object        # @.str55
.L.str55:
	.asciz	"CHECK"
	.size	.L.str55, 6

	.type	.L.str56,@object        # @.str56
.L.str56:
	.asciz	"INIT"
	.size	.L.str56, 5

	.type	.L.str57,@object        # @.str57
.L.str57:
	.asciz	"END"
	.size	.L.str57, 4

	.type	.L.str58,@object        # @.str58
.L.str58:
	.asciz	"%_"
	.size	.L.str58, 3


	.ident	"clang version 3.5 (trunk)"
	.section	".note.GNU-stack","",@progbits
