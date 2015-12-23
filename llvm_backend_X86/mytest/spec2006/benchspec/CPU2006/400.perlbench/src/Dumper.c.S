	.file	"Dumper.c.bc"
	.text
	.globl	XS_Data__Dumper_Dumpxs
	.align	16, 0x90
	.type	XS_Data__Dumper_Dumpxs,@function
XS_Data__Dumper_Dumpxs:                 # @XS_Data__Dumper_Dumpxs
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
	subq	$1376, %rsp             # imm = 0x560
.Ltmp11:
	.cfi_def_cfa_offset 1424
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
	movq	PL_markstack_ptr(%rip), %rax
	movq	PL_stack_sp(%rip), %r13
	leaq	-8(%rax), %rcx
	movq	%rcx, PL_markstack_ptr(%rip)
	movq	(%rax), %r12
	movq	PL_stack_base(%rip), %rax
	leaq	(%rax,%r12,8), %rdx
	movq	$3, %rcx
	shlq	%cl, %r12
	movq	$3, %rcx
	sarq	%cl, %r12
	movq	%r13, %r15
	subq	%rdx, %r15
	movq	$3, %rcx
	movq	%r15, %rbx
	sarq	%cl, %rbx
	cmpq	$7, %r15
	jg	.LBB0_2
# BB#1:                                 # %if.then
	movabsq	$.L.str, %rdi
	xorq	%rax, %rax
	callq	Perl_croak
	movq	PL_stack_base(%rip), %rax
.LBB0_2:                                # %if.end
	movq	PL_op(%rip), %rcx
	movq	8(%rax,%r12,8), %r14
	movq	$0, 1368(%rsp)
	movzbl	36(%rcx), %eax
	movq	$3, %rcx
	shlq	%cl, %rbx
	subq	%rbx, %r13
	andq	$3, %rax
	je	.LBB0_6
# BB#3:                                 # %cond.true
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	cmpq	$3, %rax
	jne	.LBB0_4
# BB#5:                                 # %cond.true
	movabsq	$1, %rax
	jmp	.LBB0_7
.LBB0_6:                                # %cond.false
	callq	Perl_dowantarray
	jmp	.LBB0_7
.LBB0_4:
	movabsq	$0, %rax
.LBB0_7:                                # %cond.end
	movq	%rax, 272(%rsp)         # 8-byte Spill
	movzbl	18(%r14), %eax
	testq	$8, %rax
	jne	.LBB0_26
# BB#8:                                 # %if.then16
	cmpq	$15, %r15
	jg	.LBB0_10
# BB#9:                                 # %if.then19
	movabsq	$.L.str1, %rdi
	xorq	%rax, %rax
	callq	Perl_croak
.LBB0_10:                               # %if.end20
	callq	Perl_push_scope
	movabsq	$PL_tmps_floor, %rdi
	callq	Perl_save_int
	movq	PL_tmps_ix(%rip), %rcx
	movq	PL_markstack_ptr(%rip), %rax
	movq	PL_markstack_max(%rip), %rdx
	movq	%rcx, PL_tmps_floor(%rip)
	addq	$8, %rax
	movq	%rax, PL_markstack_ptr(%rip)
	cmpq	%rdx, %rax
	jne	.LBB0_12
# BB#11:                                # %if.then24
	callq	Perl_markstack_grow
	movq	PL_markstack_ptr(%rip), %rax
.LBB0_12:                               # %if.end25
	movq	PL_stack_base(%rip), %rcx
	movq	%r13, %rdx
	subq	%rcx, %rdx
	movq	$3, %rcx
	sarq	%cl, %rdx
	movq	%rdx, (%rax)
	movq	PL_stack_max(%rip), %rax
	subq	%r13, %rax
	cmpq	$7, %rax
	jg	.LBB0_14
# BB#13:                                # %if.then36
	movq	$1, %rdx
	movq	%r13, %rdi
	movq	%r13, %rsi
	callq	Perl_stack_grow
	movq	%rax, %r13
.LBB0_14:                               # %if.end38
	movq	%r14, 8(%r13)
	movq	PL_stack_max(%rip), %rax
	addq	$8, %r13
	subq	%r13, %rax
	cmpq	$7, %rax
	jg	.LBB0_16
# BB#15:                                # %if.then47
	movq	$1, %rdx
	movq	%r13, %rdi
	movq	%r13, %rsi
	callq	Perl_stack_grow
	movq	%rax, %r13
.LBB0_16:                               # %if.end49
	movq	PL_stack_base(%rip), %rax
	movq	16(%rax,%r12,8), %rdi
	callq	Perl_newSVsv
	movq	%rax, %rdi
	callq	Perl_sv_2mortal
	movq	%rax, 8(%r13)
	addq	$8, %r13
	cmpq	$17, %r15
	jl	.LBB0_20
# BB#17:                                # %if.then58
	movq	PL_stack_max(%rip), %rax
	subq	%r13, %rax
	cmpq	$7, %rax
	jg	.LBB0_19
# BB#18:                                # %if.then65
	movq	$1, %rdx
	movq	%r13, %rdi
	movq	%r13, %rsi
	callq	Perl_stack_grow
	movq	%rax, %r13
.LBB0_19:                               # %if.end67
	movq	PL_stack_base(%rip), %rax
	movq	24(%rax,%r12,8), %rdi
	callq	Perl_newSVsv
	movq	%rax, %rdi
	callq	Perl_sv_2mortal
	movq	%rax, 8(%r13)
	addq	$8, %r13
.LBB0_20:                               # %if.end74
	movq	%r13, PL_stack_sp(%rip)
	movabsq	$.L.str2, %rdi
	xorq	%rsi, %rsi
	callq	Perl_call_method
	movq	PL_stack_sp(%rip), %r13
	movq	%rax, %rbx
	testq	%rbx, %rbx
	je	.LBB0_22
# BB#21:                                # %if.then77
	movq	(%r13), %rdi
	addq	$-8, %r13
	callq	Perl_newSVsv
	movq	%rax, %r14
.LBB0_22:                               # %if.end80
	movq	PL_tmps_ix(%rip), %rax
	movq	PL_tmps_floor(%rip), %rcx
	movq	%r13, PL_stack_sp(%rip)
	cmpq	%rcx, %rax
	jle	.LBB0_24
# BB#23:                                # %if.then83
	callq	Perl_free_tmps
.LBB0_24:                               # %if.end84
	callq	Perl_pop_scope
	testq	%rbx, %rbx
	je	.LBB0_26
# BB#25:                                # %if.then86
	movq	%r14, %rdi
	callq	Perl_sv_2mortal
.LBB0_26:                               # %if.end89
	callq	Perl_sv_newmortal
	movq	%rax, %r12
	movabsq	$.L.str3, %rdi
	xorq	%rsi, %rsi
	callq	Perl_newSVpvn
	movzbl	18(%r14), %ecx
	movq	%rax, 328(%rsp)         # 8-byte Spill
	testq	$8, %rcx
	je	.LBB0_205
# BB#27:                                # %land.lhs.true
	movq	(%r14), %rax
	movq	(%rax), %rdi
	testq	%rdi, %rdi
	je	.LBB0_205
# BB#28:                                # %land.lhs.true96
	movzbl	16(%rdi), %eax
	cmpq	$11, %rax
	jne	.LBB0_205
# BB#29:                                # %if.then101
	movabsq	$.L.str4, %rsi
	movq	$4, %rdx
	xorq	%rcx, %rcx
	xorq	%r14, %r14
	movq	%rdi, %r15
	callq	Perl_hv_fetch
	testq	%rax, %rax
	movabsq	$0, %rcx
	je	.LBB0_32
# BB#30:                                # %land.lhs.true104
	movq	(%rax), %rax
	movzbl	18(%rax), %ecx
	testq	$8, %rcx
	movabsq	$0, %rcx
	je	.LBB0_32
# BB#31:                                # %if.then108
	movq	(%rax), %rax
	movq	(%rax), %rcx
.LBB0_32:                               # %if.end111
	movq	%rcx, 168(%rsp)         # 8-byte Spill
	movabsq	$.L.str5, %rsi
	movq	$6, %rdx
	xorq	%rcx, %rcx
	movq	%r15, %rdi
	callq	Perl_hv_fetch
	testq	%rax, %rax
	je	.LBB0_35
# BB#33:                                # %land.lhs.true114
	movq	(%rax), %rax
	movzbl	18(%rax), %ecx
	testq	$8, %rcx
	je	.LBB0_35
# BB#34:                                # %if.then118
	movq	(%rax), %rax
	movq	(%rax), %r14
.LBB0_35:                               # %if.end121
	movabsq	$.L.str6, %rsi
	movq	$5, %rdx
	xorq	%rcx, %rcx
	movq	$0, 184(%rsp)           # 8-byte Folded Spill
	movq	%r15, %rdi
	callq	Perl_hv_fetch
	testq	%rax, %rax
	je	.LBB0_38
# BB#36:                                # %land.lhs.true124
	movq	(%rax), %rax
	movzbl	18(%rax), %ecx
	testq	$8, %rcx
	je	.LBB0_38
# BB#37:                                # %if.then128
	movq	(%rax), %rax
	movq	(%rax), %rax
	movq	%rax, 184(%rsp)         # 8-byte Spill
.LBB0_38:                               # %if.end131
	movabsq	$.L.str7, %rsi
	movq	$6, %rdx
	xorq	%rcx, %rcx
	movq	$0, 240(%rsp)           # 8-byte Folded Spill
	movq	%r15, %rdi
	callq	Perl_hv_fetch
	testq	%rax, %rax
	movq	$2, 288(%rsp)           # 8-byte Folded Spill
	je	.LBB0_43
# BB#39:                                # %if.then134
	movq	(%rax), %rdi
	movzbl	18(%rdi), %eax
	testq	$1, %rax
	je	.LBB0_41
# BB#40:                                # %cond.true138
	movq	(%rdi), %rax
	movq	24(%rax), %rax
	jmp	.LBB0_42
.LBB0_205:                              # %if.else602
	movabsq	$.L.str28, %rdi
	xorq	%rax, %rax
	callq	Perl_croak
	jmp	.LBB0_206
.LBB0_41:                               # %cond.false140
	callq	Perl_sv_2iv
.LBB0_42:                               # %if.end144
	movq	%rax, 288(%rsp)         # 8-byte Spill
.LBB0_43:                               # %if.end144
	movabsq	$.L.str8, %rsi
	movq	$6, %rdx
	xorq	%rcx, %rcx
	movq	%r15, %rdi
	callq	Perl_hv_fetch
	testq	%rax, %rax
	je	.LBB0_48
# BB#44:                                # %if.then147
	movq	(%rax), %rdi
	movzbl	18(%rdi), %eax
	testq	$1, %rax
	je	.LBB0_46
# BB#45:                                # %cond.true151
	movq	(%rdi), %rax
	movq	24(%rax), %rax
	jmp	.LBB0_47
.LBB0_46:                               # %cond.false154
	callq	Perl_sv_2iv
.LBB0_47:                               # %if.end158
	movq	%rax, 240(%rsp)         # 8-byte Spill
.LBB0_48:                               # %if.end158
	movabsq	$.L.str9, %rsi
	movq	$5, %rdx
	xorq	%rcx, %rcx
	xorq	%rbx, %rbx
	movq	%r15, %rdi
	callq	Perl_hv_fetch
	testq	%rax, %rax
	je	.LBB0_66
# BB#49:                                # %if.then161
	movq	(%rax), %rdi
	xorq	%rbx, %rbx
	testq	%rdi, %rdi
	je	.LBB0_66
# BB#50:                                # %cond.false164
	movq	16(%rdi), %rax
	testq	$262144, %rax           # imm = 0x40000
	je	.LBB0_57
# BB#51:                                # %cond.true168
	movq	(%rdi), %rax
	xorq	%rbx, %rbx
	testq	%rax, %rax
	movq	%rax, PL_Xpv(%rip)
	je	.LBB0_66
# BB#52:                                # %land.rhs
	movq	8(%rax), %rcx
	cmpq	$1, %rcx
	movq	$1, %rbx
	ja	.LBB0_66
# BB#53:                                # %lor.rhs
	testq	%rcx, %rcx
	movabsq	$0, %rbx
	je	.LBB0_66
# BB#54:                                # %land.rhs175
	movq	(%rax), %rax
	movzbl	(%rax), %eax
	cmpq	$48, %rax
	je	.LBB0_55
# BB#56:                                # %land.rhs175
	movabsq	$1, %rbx
	jmp	.LBB0_66
.LBB0_57:                               # %cond.false181
	testq	$65536, %rax            # imm = 0x10000
	je	.LBB0_61
# BB#58:                                # %cond.true185
	movq	(%rdi), %rax
	cmpq	$0, 24(%rax)
	je	.LBB0_59
# BB#60:                                # %cond.true185
	movabsq	$1, %rbx
	jmp	.LBB0_66
.LBB0_61:                               # %cond.false190
	testq	$131072, %rax           # imm = 0x20000
	je	.LBB0_65
# BB#62:                                # %cond.true194
	movq	(%rdi), %rax
	movq	32(%rax), %rdi
	xorq	%rsi, %rsi
	callq	__nedf2
	testq	%rax, %rax
	je	.LBB0_63
# BB#64:                                # %cond.true194
	movabsq	$1, %rbx
	jmp	.LBB0_66
.LBB0_59:
	movabsq	$0, %rbx
	jmp	.LBB0_66
.LBB0_55:
	movabsq	$0, %rbx
	jmp	.LBB0_66
.LBB0_65:                               # %cond.false198
	callq	Perl_sv_2bool
	movq	%rax, %rbx
	jmp	.LBB0_66
.LBB0_63:
	movabsq	$0, %rbx
.LBB0_66:                               # %if.end210
	movabsq	$.L.str10, %rsi
	movq	$3, %rdx
	xorq	%rcx, %rcx
	movq	%r15, %rdi
	callq	Perl_hv_fetch
	movabsq	$PL_sv_undef, %rcx
	movq	%rcx, 232(%rsp)         # 8-byte Spill
	testq	%rax, %rax
	movq	%rcx, 320(%rsp)         # 8-byte Spill
	je	.LBB0_68
# BB#67:                                # %if.then213
	movq	(%rax), %rax
	movq	%rax, 320(%rsp)         # 8-byte Spill
.LBB0_68:                               # %if.end214
	movq	%rbx, 136(%rsp)         # 8-byte Spill
	movabsq	$.L.str11, %rsi
	movq	$4, %rdx
	xorq	%rcx, %rcx
	movq	%r15, %rdi
	callq	Perl_hv_fetch
	testq	%rax, %rax
	movabsq	$PL_sv_undef, %rcx
	movq	%rcx, 224(%rsp)         # 8-byte Spill
	je	.LBB0_70
# BB#69:                                # %if.then217
	movq	(%rax), %rax
	movq	%rax, 224(%rsp)         # 8-byte Spill
.LBB0_70:                               # %if.end218
	movq	%r12, %rbx
	movabsq	$.L.str12, %rsi
	movq	$4, %rdx
	xorq	%rcx, %rcx
	movq	%r15, %rdi
	callq	Perl_hv_fetch
	testq	%rax, %rax
	movabsq	$PL_sv_undef, %rcx
	movq	%rcx, 264(%rsp)         # 8-byte Spill
	movq	%r14, %r12
	movq	%r12, 160(%rsp)         # 8-byte Spill
	je	.LBB0_72
# BB#71:                                # %if.then221
	movq	(%rax), %rax
	movq	%rax, 264(%rsp)         # 8-byte Spill
.LBB0_72:                               # %if.end222
	movabsq	$.L.str13, %rsi
	movq	$3, %rdx
	xorq	%rcx, %rcx
	movq	%r15, %r14
	movq	%r14, %rdi
	callq	Perl_hv_fetch
	testq	%rax, %rax
	movabsq	$PL_sv_undef, %rcx
	movq	%rcx, 312(%rsp)         # 8-byte Spill
	movq	%rbx, 296(%rsp)         # 8-byte Spill
	je	.LBB0_74
# BB#73:                                # %if.then225
	movq	(%rax), %rax
	movq	%rax, 312(%rsp)         # 8-byte Spill
.LBB0_74:                               # %if.end226
	movabsq	$.L.str14, %rsi
	movq	$4, %rdx
	xorq	%rcx, %rcx
	movq	%r14, %rdi
	callq	Perl_hv_fetch
	testq	%rax, %rax
	movabsq	$PL_sv_undef, %rcx
	movq	%rcx, 216(%rsp)         # 8-byte Spill
	je	.LBB0_76
# BB#75:                                # %if.then229
	movq	(%rax), %rax
	movq	%rax, 216(%rsp)         # 8-byte Spill
.LBB0_76:                               # %if.end230
	movq	%rbx, 296(%rsp)         # 8-byte Spill
	movabsq	$.L.str15, %rsi
	movq	$7, %rdx
	xorq	%rcx, %rcx
	movq	%r14, %rdi
	callq	Perl_hv_fetch
	testq	%rax, %rax
	movabsq	$PL_sv_undef, %rcx
	movq	%rcx, 128(%rsp)         # 8-byte Spill
	je	.LBB0_78
# BB#77:                                # %if.then233
	movq	(%rax), %rax
	movq	%rax, 128(%rsp)         # 8-byte Spill
.LBB0_78:                               # %if.end234
	movabsq	$.L.str16, %rsi
	movq	$7, %rdx
	xorq	%rcx, %rcx
	movq	%r14, %rdi
	callq	Perl_hv_fetch
	testq	%rax, %rax
	movabsq	$PL_sv_undef, %rcx
	movq	%rcx, 208(%rsp)         # 8-byte Spill
	je	.LBB0_80
# BB#79:                                # %if.then237
	movq	(%rax), %rax
	movq	%rax, 208(%rsp)         # 8-byte Spill
.LBB0_80:                               # %if.end238
	movabsq	$.L.str17, %rsi
	movq	$7, %rdx
	xorq	%rcx, %rcx
	xorq	%rbx, %rbx
	movq	%r14, %rdi
	callq	Perl_hv_fetch
	testq	%rax, %rax
	je	.LBB0_82
# BB#81:                                # %if.then241
	movq	(%rax), %rax
	movq	%rax, 232(%rsp)         # 8-byte Spill
.LBB0_82:                               # %if.end242
	movabsq	$.L.str18, %rsi
	movq	$8, %rdx
	xorq	%rcx, %rcx
	movq	%r14, %rdi
	callq	Perl_hv_fetch
	testq	%rax, %rax
	je	.LBB0_100
# BB#83:                                # %if.then245
	movq	(%rax), %rdi
	xorq	%rbx, %rbx
	testq	%rdi, %rdi
	je	.LBB0_100
# BB#84:                                # %cond.false248
	movq	16(%rdi), %rax
	testq	$262144, %rax           # imm = 0x40000
	je	.LBB0_91
# BB#85:                                # %cond.true252
	movq	(%rdi), %rax
	xorq	%rbx, %rbx
	testq	%rax, %rax
	movq	%rax, PL_Xpv(%rip)
	je	.LBB0_100
# BB#86:                                # %land.rhs255
	movq	8(%rax), %rcx
	cmpq	$1, %rcx
	movq	$1, %rbx
	ja	.LBB0_100
# BB#87:                                # %lor.rhs259
	testq	%rcx, %rcx
	movabsq	$0, %rbx
	je	.LBB0_100
# BB#88:                                # %land.rhs262
	movq	(%rax), %rax
	movzbl	(%rax), %eax
	cmpq	$48, %rax
	je	.LBB0_89
# BB#90:                                # %land.rhs262
	movabsq	$1, %rbx
	jmp	.LBB0_100
.LBB0_91:                               # %cond.false271
	testq	$65536, %rax            # imm = 0x10000
	je	.LBB0_95
# BB#92:                                # %cond.true275
	movq	(%rdi), %rax
	cmpq	$0, 24(%rax)
	je	.LBB0_93
# BB#94:                                # %cond.true275
	movabsq	$1, %rbx
	jmp	.LBB0_100
.LBB0_95:                               # %cond.false280
	testq	$131072, %rax           # imm = 0x20000
	je	.LBB0_99
# BB#96:                                # %cond.true284
	movq	(%rdi), %rax
	movq	32(%rax), %rdi
	xorq	%rsi, %rsi
	callq	__nedf2
	testq	%rax, %rax
	je	.LBB0_97
# BB#98:                                # %cond.true284
	movabsq	$1, %rbx
	jmp	.LBB0_100
.LBB0_93:
	movabsq	$0, %rbx
	jmp	.LBB0_100
.LBB0_89:
	movabsq	$0, %rbx
	jmp	.LBB0_100
.LBB0_99:                               # %cond.false289
	callq	Perl_sv_2bool
	movq	%rax, %rbx
	jmp	.LBB0_100
.LBB0_97:
	movabsq	$0, %rbx
.LBB0_100:                              # %if.end301
	movq	%rbx, 152(%rsp)         # 8-byte Spill
	movabsq	$.L.str19, %rsi
	movq	$9, %rdx
	xorq	%rcx, %rcx
	xorq	%rbx, %rbx
	movq	%r14, %rdi
	callq	Perl_hv_fetch
	testq	%rax, %rax
	movq	$1, 200(%rsp)           # 8-byte Folded Spill
	je	.LBB0_119
# BB#101:                               # %if.then304
	movq	(%rax), %rdi
	testq	%rdi, %rdi
	je	.LBB0_118
# BB#102:                               # %cond.false307
	movq	16(%rdi), %rax
	testq	$262144, %rax           # imm = 0x40000
	je	.LBB0_109
# BB#103:                               # %cond.true311
	movq	(%rdi), %rax
	xorq	%rbx, %rbx
	testq	%rax, %rax
	movq	%rax, PL_Xpv(%rip)
	je	.LBB0_118
# BB#104:                               # %land.rhs314
	movq	8(%rax), %rcx
	cmpq	$1, %rcx
	movq	$1, %rbx
	ja	.LBB0_118
# BB#105:                               # %lor.rhs318
	testq	%rcx, %rcx
	movabsq	$0, %rbx
	je	.LBB0_118
# BB#106:                               # %land.rhs321
	movq	(%rax), %rax
	movzbl	(%rax), %eax
	cmpq	$48, %rax
	je	.LBB0_107
# BB#108:                               # %land.rhs321
	movabsq	$1, %rbx
	jmp	.LBB0_118
.LBB0_109:                              # %cond.false330
	testq	$65536, %rax            # imm = 0x10000
	je	.LBB0_113
# BB#110:                               # %cond.true334
	movq	(%rdi), %rax
	cmpq	$0, 24(%rax)
	je	.LBB0_111
# BB#112:                               # %cond.true334
	movabsq	$1, %rbx
	jmp	.LBB0_118
.LBB0_113:                              # %cond.false339
	testq	$131072, %rax           # imm = 0x20000
	je	.LBB0_117
# BB#114:                               # %cond.true343
	movq	(%rdi), %rax
	movq	32(%rax), %rdi
	xorq	%rsi, %rsi
	callq	__nedf2
	testq	%rax, %rax
	je	.LBB0_115
# BB#116:                               # %cond.true343
	movabsq	$1, %rbx
	jmp	.LBB0_118
.LBB0_111:
	movabsq	$0, %rbx
	jmp	.LBB0_118
.LBB0_107:
	movabsq	$0, %rbx
	jmp	.LBB0_118
.LBB0_117:                              # %cond.false348
	callq	Perl_sv_2bool
	movq	%rax, %rbx
	jmp	.LBB0_118
.LBB0_115:
	movabsq	$0, %rbx
.LBB0_118:                              # %cond.end357
	movq	%rbx, 200(%rsp)         # 8-byte Spill
.LBB0_119:                              # %if.end360
	movabsq	$.L.str20, %rsi
	movq	$5, %rdx
	xorq	%rcx, %rcx
	movq	$0, 192(%rsp)           # 8-byte Folded Spill
	movq	%r14, %rdi
	callq	Perl_hv_fetch
	testq	%rax, %rax
	je	.LBB0_120
# BB#121:                               # %if.then363
	movq	%r13, 248(%rsp)         # 8-byte Spill
	movq	(%rax), %rax
	jmp	.LBB0_122
.LBB0_120:
	movq	%r13, 248(%rsp)         # 8-byte Spill
	movabsq	$PL_sv_undef, %rax
.LBB0_122:                              # %if.end364
	movq	%rax, 144(%rsp)         # 8-byte Spill
	movabsq	$.L.str21, %rsi
	movq	$8, %rdx
	xorq	%rcx, %rcx
	movq	%r14, %rdi
	callq	Perl_hv_fetch
	testq	%rax, %rax
	je	.LBB0_127
# BB#123:                               # %if.then367
	movq	(%rax), %rdi
	movzbl	18(%rdi), %eax
	testq	$1, %rax
	je	.LBB0_125
# BB#124:                               # %cond.true371
	movq	(%rdi), %rax
	movq	24(%rax), %rax
	jmp	.LBB0_126
.LBB0_125:                              # %cond.false374
	callq	Perl_sv_2iv
.LBB0_126:                              # %if.end378
	movq	%rax, 192(%rsp)         # 8-byte Spill
.LBB0_127:                              # %if.end378
	movabsq	$.L.str22, %rsi
	movq	$8, %rdx
	xorq	%rcx, %rcx
	movq	$0, 176(%rsp)           # 8-byte Folded Spill
	movq	%r14, %rdi
	callq	Perl_hv_fetch
	testq	%rax, %rax
	je	.LBB0_143
# BB#128:                               # %if.then381
	movq	(%rax), %rbx
	testq	%rbx, %rbx
	je	.LBB0_143
# BB#129:                               # %cond.false384
	movq	16(%rbx), %r14
	testq	$262144, %r14           # imm = 0x40000
	je	.LBB0_134
# BB#130:                               # %cond.true388
	movq	(%rbx), %rax
	testq	%rax, %rax
	movq	%rax, PL_Xpv(%rip)
	je	.LBB0_143
# BB#131:                               # %land.lhs.true391
	movq	8(%rax), %rcx
	cmpq	$1, %rcx
	ja	.LBB0_140
# BB#132:                               # %lor.lhs.false
	testq	%rcx, %rcx
	je	.LBB0_143
# BB#133:                               # %land.lhs.true397
	movq	(%rax), %rax
	movzbl	(%rax), %eax
	cmpq	$48, %rax
	jne	.LBB0_140
	jmp	.LBB0_143
.LBB0_134:                              # %cond.false404
	testq	$65536, %r14            # imm = 0x10000
	je	.LBB0_136
# BB#135:                               # %cond.true408
	movq	(%rbx), %rax
	cmpq	$0, 24(%rax)
	jne	.LBB0_140
	jmp	.LBB0_143
.LBB0_136:                              # %cond.false413
	testq	$131072, %r14           # imm = 0x20000
	je	.LBB0_138
# BB#137:                               # %cond.true417
	movq	(%rbx), %rax
	movq	32(%rax), %rdi
	xorq	%rsi, %rsi
	movq	$0, 176(%rsp)           # 8-byte Folded Spill
	callq	__nedf2
	testq	%rax, %rax
	jne	.LBB0_140
	jmp	.LBB0_143
.LBB0_138:                              # %cond.false422
	movq	%rbx, %rdi
	callq	Perl_sv_2bool
	testq	$255, %rax
	je	.LBB0_143
# BB#139:                               # %cond.false422.if.else_crit_edge
	movq	16(%rbx), %r14
.LBB0_140:                              # %if.else
	testq	$524288, %r14           # imm = 0x80000
	je	.LBB0_142
# BB#141:                               # %land.lhs.true430
	movq	(%rbx), %rax
	movq	(%rax), %rax
	movzbl	16(%rax), %eax
	cmpq	$12, %rax
	movq	%rbx, 176(%rsp)         # 8-byte Spill
	je	.LBB0_143
.LBB0_142:                              # %if.then437
	movabsq	$PL_sv_yes, %rax
	movq	%rax, 176(%rsp)         # 8-byte Spill
.LBB0_143:                              # %if.end440
	callq	Perl_newAV
	movq	%rax, %r14
	testq	%r12, %r12
	je	.LBB0_144
# BB#145:                               # %if.end446
	movq	%r12, %rdi
	callq	Perl_av_len
	movq	%rax, %rbx
	movq	%rbx, 256(%rsp)         # 8-byte Spill
	movabsq	$.L.str3, %rdi
	xorq	%rsi, %rsi
	callq	Perl_newSVpvn
	movq	%rax, %r13
	testq	%rbx, %rbx
	js	.LBB0_203
# BB#146:                               # %for.body.lr.ph
	xorq	%rbx, %rbx
	.align	16, 0x90
.LBB0_147:                              # %for.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB0_177 Depth 2
                                        #     Child Loop BB0_181 Depth 2
                                        #     Child Loop BB0_190 Depth 2
	movq	%r14, %rdi
	callq	Perl_av_clear
	xorq	%rdx, %rdx
	movq	%r12, %rdi
	movq	%rbx, %rsi
	callq	Perl_av_fetch
	testq	%rax, %rax
	movabsq	$PL_sv_undef, %rcx
	je	.LBB0_149
# BB#148:                               # %if.then453
                                        #   in Loop: Header=BB0_147 Depth=1
	movq	(%rax), %rcx
.LBB0_149:                              # %if.end455
                                        #   in Loop: Header=BB0_147 Depth=1
	movq	%rcx, 304(%rsp)         # 8-byte Spill
	movq	$1, %rdx
	movq	184(%rsp), %rdi         # 8-byte Reload
	movq	%rbx, %rsi
	callq	Perl_av_fetch
	testq	%rax, %rax
	jne	.LBB0_150
# BB#151:                               # %if.else459
                                        #   in Loop: Header=BB0_147 Depth=1
	movq	%rbx, 280(%rsp)         # 8-byte Spill
	movq	296(%rsp), %rbx         # 8-byte Reload
	movq	16(%rbx), %rax
	movq	%rax, %rcx
	andq	$1223753727, %rcx       # imm = 0x48F0FFFF
	movq	%rcx, 16(%rbx)
	testq	$2097152, %rax          # imm = 0x200000
	je	.LBB0_153
# BB#152:                               # %land.rhs465
                                        #   in Loop: Header=BB0_147 Depth=1
	movq	%rbx, %rdi
	callq	Perl_sv_backoff
	jmp	.LBB0_153
	.align	16, 0x90
.LBB0_150:                              # %if.then458
                                        #   in Loop: Header=BB0_147 Depth=1
	movq	%rbx, 280(%rsp)         # 8-byte Spill
	movq	(%rax), %rsi
	movq	$2, %rdx
	movq	296(%rsp), %rbx         # 8-byte Reload
	movq	%rbx, %rdi
	callq	Perl_sv_setsv_flags
.LBB0_153:                              # %if.end469
                                        #   in Loop: Header=BB0_147 Depth=1
	movzbl	18(%rbx), %eax
	testq	$1807, %rax             # imm = 0x70F
	movq	288(%rsp), %r15         # 8-byte Reload
	jne	.LBB0_154
# BB#165:                               # %if.else518
                                        #   in Loop: Header=BB0_147 Depth=1
	movq	$1, %rdx
	movq	%rbx, %rdi
	movabsq	$.L.str23, %rsi
	callq	Perl_sv_setpvn
	movq	$2, %rdx
	movq	%rbx, %rdi
	movq	128(%rsp), %rsi         # 8-byte Reload
	callq	Perl_sv_catsv_flags
	movq	280(%rsp), %rax         # 8-byte Reload
	leaq	1(%rax), %rdx
	xorq	%rax, %rax
	movq	%rbx, %r12
	leaq	336(%rsp), %rbx
	movq	%rbx, %rdi
	movabsq	$.L.str24, %rsi
	callq	sprintf
	movq	%rbx, %rdi
	callq	strlen
	movq	$2, %rcx
	movq	%r12, %rdi
	movq	%rbx, %rsi
	movq	%r12, %rbx
	movq	%rax, %rdx
	callq	Perl_sv_catpvn_flags
	jmp	.LBB0_166
	.align	16, 0x90
.LBB0_154:                              # %if.then473
                                        #   in Loop: Header=BB0_147 Depth=1
	movq	(%rbx), %rax
	movq	(%rax), %rax
	movzbl	(%rax), %ecx
	cmpq	$36, %rcx
	je	.LBB0_166
# BB#155:                               # %if.then473
                                        #   in Loop: Header=BB0_147 Depth=1
	cmpq	$42, %rcx
	jne	.LBB0_164
# BB#156:                               # %if.then480
                                        #   in Loop: Header=BB0_147 Depth=1
	movq	304(%rsp), %rdx         # 8-byte Reload
	movzbl	18(%rdx), %ecx
	testq	$8, %rcx
	je	.LBB0_163
# BB#157:                               # %if.then484
                                        #   in Loop: Header=BB0_147 Depth=1
	movq	(%rdx), %rcx
	movq	(%rcx), %rcx
	movzbl	16(%rcx), %ecx
	cmpq	$12, %rcx
	je	.LBB0_162
# BB#158:                               # %if.then484
                                        #   in Loop: Header=BB0_147 Depth=1
	cmpq	$11, %rcx
	jne	.LBB0_159
# BB#161:                               # %sw.bb492
                                        #   in Loop: Header=BB0_147 Depth=1
	movq	$37, %rcx
	movb	%cl, (%rax)
	jmp	.LBB0_166
.LBB0_164:                              # %if.then515
                                        #   in Loop: Header=BB0_147 Depth=1
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	movq	$1, %r8
	movq	%rbx, %rdi
	movabsq	$.L.str23, %rcx
	callq	Perl_sv_insert
	jmp	.LBB0_166
.LBB0_162:                              # %sw.bb496
                                        #   in Loop: Header=BB0_147 Depth=1
	movq	$42, %rcx
	movb	%cl, (%rax)
	jmp	.LBB0_166
.LBB0_159:                              # %if.then484
                                        #   in Loop: Header=BB0_147 Depth=1
	cmpq	$10, %rcx
	jne	.LBB0_163
# BB#160:                               # %sw.bb
                                        #   in Loop: Header=BB0_147 Depth=1
	movq	$64, %rcx
	movb	%cl, (%rax)
	jmp	.LBB0_166
.LBB0_163:                              # %if.else503
                                        #   in Loop: Header=BB0_147 Depth=1
	movq	$36, %rcx
	movb	%cl, (%rax)
	.align	16, 0x90
.LBB0_166:                              # %if.end525
                                        #   in Loop: Header=BB0_147 Depth=1
	cmpq	$2, %r15
	movq	264(%rsp), %r12         # 8-byte Reload
	jl	.LBB0_183
# BB#167:                               # %if.then528
                                        #   in Loop: Header=BB0_147 Depth=1
	movq	(%rbx), %rax
	movq	8(%rax), %r12
	leaq	3(%r12), %rbx
	xorq	%rsi, %rsi
	movabsq	$.L.str3, %rdi
	callq	Perl_newSVpvn
	movq	%rax, %r15
	testq	%rbx, %rbx
	jle	.LBB0_182
# BB#168:                               # %if.then2.i
                                        #   in Loop: Header=BB0_147 Depth=1
	movq	(%r15), %rdi
	movq	8(%rdi), %r9
	movq	16(%rdi), %rax
	leaq	4(%r12,%r9), %rsi
	cmpq	%rsi, %rax
	jae	.LBB0_170
# BB#169:                               # %cond.true.i
                                        #   in Loop: Header=BB0_147 Depth=1
	movq	%r15, %rdi
	callq	Perl_sv_grow
	movq	(%r15), %rdi
	movq	8(%rdi), %r9
.LBB0_170:                              # %cond.end.i
                                        #   in Loop: Header=BB0_147 Depth=1
	leaq	(%r9,%rbx), %rax
	movq	$-3, %rcx
	subq	%r12, %rcx
	cmpq	$-2, %rcx
	jg	.LBB0_172
# BB#171:                               # %cond.end.i
                                        #   in Loop: Header=BB0_147 Depth=1
	movq	$-1, %rcx
.LBB0_172:                              # %cond.end.i
                                        #   in Loop: Header=BB0_147 Depth=1
	leaq	4(%r12,%rcx), %r10
	movq	%r10, %rcx
	andq	$-4, %rcx
	movq	(%rdi), %r8
	xorq	%rsi, %rsi
	movq	%r10, %rdx
	andq	$-4, %rdx
	movq	%rax, 8(%rdi)
	movb	%sil, (%r8,%rax)
	jne	.LBB0_174
# BB#173:                               #   in Loop: Header=BB0_147 Depth=1
	movq	$32, %rcx
	jmp	.LBB0_179
.LBB0_174:                              # %vector.body.preheader
                                        #   in Loop: Header=BB0_147 Depth=1
	movq	$-3, %rax
	subq	%r12, %rax
	cmpq	$-2, %rax
	jg	.LBB0_176
# BB#175:                               # %vector.body.preheader
                                        #   in Loop: Header=BB0_147 Depth=1
	movq	$-1, %rax
.LBB0_176:                              # %vector.body.preheader
                                        #   in Loop: Header=BB0_147 Depth=1
	subq	%rcx, %rbx
	leaq	4(%r12,%rax), %rdi
	andq	$-4, %rdi
	addq	%r9, %r12
	leaq	-1(%r8,%r12), %rax
	movq	$32, %rcx
	.align	16, 0x90
.LBB0_177:                              # %vector.body
                                        #   Parent Loop BB0_147 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movb	%cl, 3(%rax)
	movb	%cl, 2(%rax)
	movb	%cl, 1(%rax)
	movb	%cl, (%rax)
	addq	$-4, %rax
	addq	$-4, %rdi
	jne	.LBB0_177
# BB#178:                               #   in Loop: Header=BB0_147 Depth=1
	movq	%rdx, %rsi
.LBB0_179:                              # %middle.block
                                        #   in Loop: Header=BB0_147 Depth=1
	cmpq	%rsi, %r10
	je	.LBB0_182
# BB#180:                               # %while.body.i.preheader
                                        #   in Loop: Header=BB0_147 Depth=1
	leaq	-1(%r8,%r9), %rax
	.align	16, 0x90
.LBB0_181:                              # %while.body.i
                                        #   Parent Loop BB0_147 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movb	%cl, (%rax,%rbx)
	leaq	-1(%rbx), %rbx
	testq	%rbx, %rbx
	jg	.LBB0_181
.LBB0_182:                              # %sv_x.exit
                                        #   in Loop: Header=BB0_147 Depth=1
	movq	264(%rsp), %rdi         # 8-byte Reload
	callq	Perl_newSVsv
	movq	%rax, %r12
	movq	$2, %rdx
	movq	%r12, %rdi
	movq	%r15, %rsi
	callq	Perl_sv_catsv_flags
	movq	%r15, %rdi
	callq	Perl_sv_free
	movq	288(%rsp), %r15         # 8-byte Reload
	movq	296(%rsp), %rbx         # 8-byte Reload
.LBB0_183:                              # %if.end536
                                        #   in Loop: Header=BB0_147 Depth=1
	movq	(%rbx), %rax
	movq	(%rax), %rsi
	movq	8(%rax), %rdx
	movq	176(%rsp), %rax         # 8-byte Reload
	movq	%rax, 112(%rsp)
	movq	192(%rsp), %rax         # 8-byte Reload
	movq	%rax, 104(%rsp)
	movq	144(%rsp), %rax         # 8-byte Reload
	movq	%rax, 96(%rsp)
	movq	200(%rsp), %rax         # 8-byte Reload
	movq	%rax, 88(%rsp)
	movq	152(%rsp), %rax         # 8-byte Reload
	movq	%rax, 80(%rsp)
	movq	240(%rsp), %rax         # 8-byte Reload
	movq	%rax, 72(%rsp)
	movq	232(%rsp), %rax         # 8-byte Reload
	movq	%rax, 64(%rsp)
	movq	208(%rsp), %rax         # 8-byte Reload
	movq	%rax, 56(%rsp)
	movq	216(%rsp), %rax         # 8-byte Reload
	movq	%rax, 48(%rsp)
	movq	312(%rsp), %rax         # 8-byte Reload
	movq	%rax, 40(%rsp)
	movq	%r12, 32(%rsp)
	movq	224(%rsp), %rax         # 8-byte Reload
	movq	%rax, 24(%rsp)
	movq	320(%rsp), %rax         # 8-byte Reload
	movq	%rax, 16(%rsp)
	movq	%r15, 8(%rsp)
	leaq	1368(%rsp), %rax
	movq	%rax, (%rsp)
	movq	304(%rsp), %rdi         # 8-byte Reload
	movq	%r13, %rcx
	movq	168(%rsp), %r8          # 8-byte Reload
	movq	%r14, %r9
	callq	DD_dump
	cmpq	$2, %r15
	jl	.LBB0_185
# BB#184:                               # %if.then544
                                        #   in Loop: Header=BB0_147 Depth=1
	movq	%r12, %rdi
	callq	Perl_sv_free
.LBB0_185:                              # %if.end545
                                        #   in Loop: Header=BB0_147 Depth=1
	movq	%r14, %rdi
	callq	Perl_av_len
	movq	%rax, %r12
	movq	%r12, 304(%rsp)         # 8-byte Spill
	testq	%r12, %r12
	jns	.LBB0_187
# BB#186:                               # %if.end545
                                        #   in Loop: Header=BB0_147 Depth=1
	cmpq	$0, 136(%rsp)           # 8-byte Folded Reload
	jne	.LBB0_188
.LBB0_187:                              # %if.then551
                                        #   in Loop: Header=BB0_147 Depth=1
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	movq	$3, %r8
	movq	%r13, %rdi
	movabsq	$.L.str26, %rcx
	callq	Perl_sv_insert
	movq	(%rbx), %rax
	movq	(%rax), %rcx
	movq	8(%rax), %r8
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	movq	%r13, %rdi
	callq	Perl_sv_insert
	movq	$1, %rdx
	movq	$2, %rcx
	movq	%r13, %rdi
	movabsq	$.L.str27, %rsi
	callq	Perl_sv_catpvn_flags
.LBB0_188:                              # %if.end556
                                        #   in Loop: Header=BB0_147 Depth=1
	movq	%rbx, 296(%rsp)         # 8-byte Spill
	movq	%r15, 288(%rsp)         # 8-byte Spill
	movq	$2, %rdx
	movq	328(%rsp), %rbx         # 8-byte Reload
	movq	%rbx, %rdi
	movq	320(%rsp), %rsi         # 8-byte Reload
	callq	Perl_sv_catsv_flags
	movq	$2, %rdx
	movq	%rbx, %rdi
	movq	%r13, %rsi
	callq	Perl_sv_catsv_flags
	movq	$2, %rdx
	movq	%rbx, %rdi
	movq	312(%rsp), %rsi         # 8-byte Reload
	callq	Perl_sv_catsv_flags
	testq	%r12, %r12
	js	.LBB0_196
# BB#189:                               # %for.body565.lr.ph
                                        #   in Loop: Header=BB0_147 Depth=1
	movq	$2, %rdx
	movq	328(%rsp), %rdi         # 8-byte Reload
	movq	320(%rsp), %rsi         # 8-byte Reload
	callq	Perl_sv_catsv_flags
	leaq	1(%r12), %r15
	xorq	%rbx, %rbx
	.align	16, 0x90
.LBB0_190:                              # %for.body565
                                        #   Parent Loop BB0_147 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	xorq	%rdx, %rdx
	movq	%r14, %rdi
	movq	%rbx, %rsi
	callq	Perl_av_fetch
	testq	%rax, %rax
	je	.LBB0_194
# BB#191:                               # %land.lhs.true569
                                        #   in Loop: Header=BB0_190 Depth=2
	movq	(%rax), %rsi
	testq	%rsi, %rsi
	je	.LBB0_194
# BB#192:                               # %if.then571
                                        #   in Loop: Header=BB0_190 Depth=2
	movq	$2, %rdx
	movq	328(%rsp), %rdi         # 8-byte Reload
	callq	Perl_sv_catsv_flags
	cmpq	%r12, %rbx
	jge	.LBB0_194
# BB#193:                               # %if.then574
                                        #   in Loop: Header=BB0_190 Depth=2
	movq	$1, %rdx
	movq	$2, %rcx
	movq	%r14, %r12
	movq	%r13, %r14
	movq	328(%rsp), %r13         # 8-byte Reload
	movq	%r13, %rdi
	movabsq	$.L.str27, %rsi
	callq	Perl_sv_catpvn_flags
	movq	$2, %rdx
	movq	%r13, %rdi
	movq	312(%rsp), %rsi         # 8-byte Reload
	callq	Perl_sv_catsv_flags
	movq	$2, %rdx
	movq	%r13, %rdi
	movq	%r14, %r13
	movq	%r12, %r14
	movq	304(%rsp), %r12         # 8-byte Reload
	movq	320(%rsp), %rsi         # 8-byte Reload
	callq	Perl_sv_catsv_flags
	.align	16, 0x90
.LBB0_194:                              # %for.inc
                                        #   in Loop: Header=BB0_190 Depth=2
	incq	%rbx
	cmpq	%rbx, %r15
	jne	.LBB0_190
# BB#195:                               # %for.end
                                        #   in Loop: Header=BB0_147 Depth=1
	movq	$1, %rdx
	movq	$2, %rcx
	movq	328(%rsp), %rbx         # 8-byte Reload
	movq	%rbx, %rdi
	movabsq	$.L.str27, %rsi
	callq	Perl_sv_catpvn_flags
	movq	$2, %rdx
	movq	%rbx, %rdi
	movq	312(%rsp), %rsi         # 8-byte Reload
	callq	Perl_sv_catsv_flags
.LBB0_196:                              # %if.end577
                                        #   in Loop: Header=BB0_147 Depth=1
	xorq	%rdx, %rdx
	movq	%r13, %rdi
	movabsq	$.L.str3, %rsi
	callq	Perl_sv_setpvn
	cmpq	$1, 272(%rsp)           # 8-byte Folded Reload
	jne	.LBB0_197
# BB#198:                               # %if.then580
                                        #   in Loop: Header=BB0_147 Depth=1
	movq	PL_stack_max(%rip), %rax
	movq	248(%rsp), %rbx         # 8-byte Reload
	subq	%rbx, %rax
	cmpq	$7, %rax
	movq	160(%rsp), %r12         # 8-byte Reload
	jg	.LBB0_200
# BB#199:                               # %if.then587
                                        #   in Loop: Header=BB0_147 Depth=1
	movq	$1, %rdx
	movq	%rbx, %rdi
	movq	%rbx, %rsi
	callq	Perl_stack_grow
	movq	%rax, %rbx
.LBB0_200:                              # %if.end589
                                        #   in Loop: Header=BB0_147 Depth=1
	movq	328(%rsp), %rdi         # 8-byte Reload
	callq	Perl_sv_2mortal
	movq	%rax, 8(%rbx)
	addq	$8, %rbx
	movq	%rbx, 248(%rsp)         # 8-byte Spill
	movq	256(%rsp), %rax         # 8-byte Reload
	movq	280(%rsp), %rbx         # 8-byte Reload
	cmpq	%rax, %rbx
	jge	.LBB0_202
# BB#201:                               # %if.then595
                                        #   in Loop: Header=BB0_147 Depth=1
	xorq	%rsi, %rsi
	movabsq	$.L.str3, %rdi
	callq	Perl_newSVpvn
	movq	%rax, 328(%rsp)         # 8-byte Spill
	jmp	.LBB0_202
	.align	16, 0x90
.LBB0_197:                              #   in Loop: Header=BB0_147 Depth=1
	movq	160(%rsp), %r12         # 8-byte Reload
	movq	280(%rsp), %rbx         # 8-byte Reload
.LBB0_202:                              # %for.inc599
                                        #   in Loop: Header=BB0_147 Depth=1
	movq	256(%rsp), %rax         # 8-byte Reload
	cmpq	%rax, %rbx
	leaq	1(%rbx), %rbx
	jne	.LBB0_147
.LBB0_203:
	movq	%r13, %rbx
	jmp	.LBB0_204
.LBB0_144:                              # %if.end446.thread
	movabsq	$.L.str3, %rdi
	xorq	%rsi, %rsi
	callq	Perl_newSVpvn
	movq	%rax, %rbx
.LBB0_204:                              # %for.end601
	movq	%r14, %rdi
	callq	Perl_sv_free
	movq	%rbx, %rdi
	callq	Perl_sv_free
	movq	248(%rsp), %r13         # 8-byte Reload
.LBB0_206:                              # %if.end603
	cmpq	$0, 272(%rsp)           # 8-byte Folded Reload
	jne	.LBB0_210
# BB#207:                               # %if.then606
	movq	PL_stack_max(%rip), %rax
	subq	%r13, %rax
	cmpq	$7, %rax
	jg	.LBB0_209
# BB#208:                               # %if.then613
	movq	$1, %rdx
	movq	%r13, %rdi
	movq	%r13, %rsi
	callq	Perl_stack_grow
	movq	%rax, %r13
.LBB0_209:                              # %if.end615
	movq	328(%rsp), %rdi         # 8-byte Reload
	callq	Perl_sv_2mortal
	movq	%rax, 8(%r13)
	addq	$8, %r13
.LBB0_210:                              # %if.end619
	movq	%r13, PL_stack_sp(%rip)
	addq	$1376, %rsp             # imm = 0x560
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp17:
	.size	XS_Data__Dumper_Dumpxs, .Ltmp17-XS_Data__Dumper_Dumpxs
	.cfi_endproc

	.align	16, 0x90
	.type	DD_dump,@function
DD_dump:                                # @DD_dump
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
	subq	$576, %rsp              # imm = 0x240
.Ltmp29:
	.cfi_def_cfa_offset 624
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
	movq	%r9, 248(%rsp)          # 8-byte Spill
	movq	%r8, %r14
	movq	%r14, 256(%rsp)         # 8-byte Spill
	movq	%rcx, %r15
	movq	%rdx, 264(%rsp)         # 8-byte Spill
	movq	%rsi, %r13
	movq	%rdi, %rbx
	testq	%rbx, %rbx
	je	.LBB1_432
# BB#1:                                 # %if.end
	movq	%rdx, 264(%rsp)         # 8-byte Spill
	movq	16(%rbx), %r12
	testq	$8192, %r12             # imm = 0x2000
	movq	%r12, %rax
	je	.LBB1_3
# BB#2:                                 # %if.then5
	movq	%rbx, %rdi
	callq	Perl_mg_get
	movq	16(%rbx), %rax
.LBB1_3:                                # %if.end6
	testq	$524288, %rax           # imm = 0x80000
	je	.LBB1_297
# BB#4:                                 # %if.then10
	movq	(%rbx), %rax
	movq	(%rax), %rax
	testq	$4096, 16(%rax)         # imm = 0x1000
	je	.LBB1_30
# BB#5:                                 # %if.then10
	movq	680(%rsp), %rcx
	testq	%rcx, %rcx
	je	.LBB1_30
# BB#6:                                 # %land.lhs.true15
	movq	680(%rsp), %rcx
	movzbl	18(%rcx), %ecx
	testq	$4, %rcx
	je	.LBB1_30
# BB#7:                                 # %land.lhs.true19
	movq	680(%rsp), %rcx
	movq	(%rcx), %rcx
	movq	8(%rcx), %rdx
	testq	%rdx, %rdx
	je	.LBB1_30
# BB#8:                                 # %land.lhs.true22
	movq	(%rax), %rax
	movq	48(%rax), %rdi
	movq	(%rcx), %rsi
	movq	$-1, %rcx
	callq	Perl_gv_fetchmeth
	testq	%rax, %rax
	je	.LBB1_30
# BB#9:                                 # %if.then30
	movq	%rbx, %r12
	movq	PL_stack_sp(%rip), %rbx
	callq	Perl_push_scope
	movabsq	$PL_tmps_floor, %rdi
	callq	Perl_save_int
	movq	PL_tmps_ix(%rip), %rcx
	movq	PL_markstack_ptr(%rip), %rax
	movq	PL_markstack_max(%rip), %rdx
	movq	%rcx, PL_tmps_floor(%rip)
	addq	$8, %rax
	movq	%rax, PL_markstack_ptr(%rip)
	cmpq	%rdx, %rax
	jne	.LBB1_11
# BB#10:                                # %if.then32
	callq	Perl_markstack_grow
	movq	PL_markstack_ptr(%rip), %rax
.LBB1_11:                               # %if.end33
	movq	PL_stack_base(%rip), %rcx
	movq	%rbx, %rdx
	subq	%rcx, %rdx
	movq	$3, %rcx
	sarq	%cl, %rdx
	movq	%rdx, (%rax)
	movq	PL_stack_max(%rip), %rax
	subq	%rbx, %rax
	cmpq	$7, %rax
	jg	.LBB1_12
# BB#13:                                # %if.then39
	movq	%r14, 256(%rsp)         # 8-byte Spill
	movq	$1, %rdx
	movq	%rbx, %rdi
	movq	%rbx, %rsi
	callq	Perl_stack_grow
	movq	%rax, %rbx
	jmp	.LBB1_14
.LBB1_297:                              # %if.else907
	movq	%r14, 256(%rsp)         # 8-byte Spill
	movq	%r15, 240(%rsp)         # 8-byte Spill
	movq	$0, 232(%rsp)           # 8-byte Folded Spill
	cmpq	$0, 264(%rsp)           # 8-byte Folded Reload
	movabsq	$0, %rax
	je	.LBB1_319
# BB#298:                               # %if.then911
	movabsq	$.L.str33, %rsi
	leaq	320(%rsp), %r15
	xorq	%rax, %rax
	xorq	%r14, %r14
	movq	%r15, %rdi
	movq	%rbx, %rdx
	callq	sprintf
	movq	%r15, %rdi
	callq	strlen
	movq	%rax, 232(%rsp)         # 8-byte Spill
	xorq	%rcx, %rcx
	movq	256(%rsp), %rdi         # 8-byte Reload
	movq	%r15, %rsi
	movq	%rax, %rdx
	callq	Perl_hv_fetch
	testq	%rax, %rax
	je	.LBB1_303
# BB#299:                               # %land.lhs.true919
	movq	(%rax), %rax
	testq	%rax, %rax
	je	.LBB1_303
# BB#300:                               # %land.lhs.true921
	movzbl	18(%rax), %ecx
	testq	$8, %rcx
	je	.LBB1_303
# BB#301:                               # %land.lhs.true925
	movq	(%rax), %rax
	movq	(%rax), %rdi
	testq	%rdi, %rdi
	je	.LBB1_302
# BB#305:                               # %if.then929
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	movq	%rdi, %r14
	callq	Perl_av_fetch
	testq	%rax, %rax
	je	.LBB1_306
# BB#307:                               # %land.lhs.true934
	movq	(%rax), %r15
	testq	%r15, %r15
	je	.LBB1_308
# BB#309:                               # %land.lhs.true936
	movq	$2, %rsi
	xorq	%rdx, %rdx
	movq	%r14, %rdi
	callq	Perl_av_fetch
	testq	%rax, %rax
	je	.LBB1_310
# BB#311:                               # %land.lhs.true939
	movq	(%rax), %rdi
	testq	%rdi, %rdi
	je	.LBB1_312
# BB#313:                               # %land.lhs.true941
	movzbl	18(%rdi), %eax
	testq	$1, %rax
	je	.LBB1_315
# BB#314:                               # %cond.true945
	movq	(%rdi), %rax
	movq	24(%rax), %rax
	jmp	.LBB1_316
.LBB1_12:
	movq	%r14, 256(%rsp)         # 8-byte Spill
.LBB1_14:                               # %if.end41
	movq	%r12, 8(%rbx)
	addq	$8, %rbx
	movq	%rbx, PL_stack_sp(%rip)
	movq	680(%rsp), %rax
	movq	(%rax), %rax
	movq	(%rax), %rdi
	movq	$132, %rsi
	callq	Perl_call_method
	movq	PL_errgv(%rip), %rbx
	movq	(%rbx), %rax
	movq	56(%rax), %rax
	movq	(%rax), %rdi
	movq	PL_stack_sp(%rip), %r14
	testq	%rdi, %rdi
	je	.LBB1_27
# BB#15:                                # %cond.false
	movq	16(%rdi), %rax
	testq	$262144, %rax           # imm = 0x40000
	je	.LBB1_20
# BB#16:                                # %cond.true55
	movq	(%rdi), %rax
	testq	%rax, %rax
	movq	%rax, PL_Xpv(%rip)
	je	.LBB1_27
# BB#17:                                # %land.lhs.true61
	movq	8(%rax), %rcx
	cmpq	$1, %rcx
	ja	.LBB1_26
# BB#18:                                # %lor.lhs.false
	testq	%rcx, %rcx
	je	.LBB1_27
# BB#19:                                # %land.lhs.true66
	movq	(%rax), %rax
	movzbl	(%rax), %eax
	cmpq	$48, %rax
	jne	.LBB1_26
	jmp	.LBB1_27
.LBB1_302:
	movq	%rdi, %r14
.LBB1_303:                              # %if.else956
	movabsq	$PL_sv_undef, %rax
	cmpq	%rax, %rbx
	je	.LBB1_304
# BB#318:                               # %if.then959
	movabsq	$.L.str40, %rdi
	movq	$1, %rsi
	movq	%rbx, 224(%rsp)         # 8-byte Spill
	callq	Perl_newSVpvn
	movq	%rax, %r15
	movq	$2, %rcx
	movq	%r15, %rdi
	movq	%r13, %rsi
	movq	264(%rsp), %rdx         # 8-byte Reload
	callq	Perl_sv_catpvn_flags
	callq	Perl_newAV
	movq	%rax, %r13
	movq	%r13, %rdi
	movq	%r15, %rsi
	callq	Perl_av_push
	movq	224(%rsp), %rdi         # 8-byte Reload
	callq	Perl_newRV
	movq	%r13, %rdi
	movq	%rax, %rsi
	callq	Perl_av_push
	leaq	320(%rsp), %rbx
	movq	%rbx, %rdi
	callq	strlen
	movq	%rax, %r15
	movq	%r13, %rdi
	callq	Perl_newRV
	xorq	%r8, %r8
	movq	256(%rsp), %rdi         # 8-byte Reload
	movq	%rbx, %rsi
	movq	%r15, %rdx
	movq	%rax, %rcx
	callq	Perl_hv_store
	movq	%r13, %rdi
	callq	Perl_sv_free
	movq	224(%rsp), %rbx         # 8-byte Reload
	movq	%r13, %rax
	jmp	.LBB1_319
.LBB1_304:
	movq	%r14, %rax
.LBB1_319:                              # %if.end972
	movq	16(%rbx), %r14
	testq	$65536, %r14            # imm = 0x10000
	je	.LBB1_331
# BB#320:                               # %if.then976
	movq	%rax, 192(%rsp)         # 8-byte Spill
	movq	(%rbx), %rax
	movq	24(%rax), %rdx
	movabsq	$2147483648, %rax       # imm = 0x80000000
	testq	%rax, %r14
	movq	240(%rsp), %r15         # 8-byte Reload
	je	.LBB1_322
# BB#321:                               # %cond.end990
	movabsq	$.L.str68, %rsi
	jmp	.LBB1_323
.LBB1_331:                              # %if.else1044
	movq	%rax, 192(%rsp)         # 8-byte Spill
	andq	$255, %r12
	cmpq	$13, %r12
	movq	240(%rsp), %r15         # 8-byte Reload
	jne	.LBB1_408
# BB#332:                               # %if.then1047
	testq	$262144, %r14           # imm = 0x40000
	je	.LBB1_334
# BB#333:                               # %cond.true1052
	movq	(%rbx), %rax
	movq	8(%rax), %r14
	movq	%r14, 280(%rsp)
	movq	(%rax), %r12
	jmp	.LBB1_335
.LBB1_322:                              # %cond.end1003
	movabsq	$.L.str24, %rsi
.LBB1_323:                              # %if.end1006
	leaq	448(%rsp), %rdi
	xorq	%rax, %rax
	callq	sprintf
	movq	232(%rsp), %r13         # 8-byte Reload
	leaq	448(%rsp), %rdi
	callq	strlen
	movq	16(%rbx), %r14
	movq	%rax, %r12
	testq	$262144, %r14           # imm = 0x40000
	je	.LBB1_326
# BB#324:                               # %cond.true1019
	movq	(%rbx), %rax
	movq	8(%rax), %rcx
	cmpq	%r12, %rcx
	jne	.LBB1_411
# BB#325:                               # %lor.lhs.false1030
	movq	(%rax), %rdi
	leaq	448(%rsp), %rsi
	movq	%r12, %rdx
	callq	memcmp
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB1_411
.LBB1_326:                              # %if.end1036
	cmpq	$11, %r12
	jb	.LBB1_328
# BB#327:                               # %if.then1039
	movabsq	$.L.str69, %rsi
	leaq	448(%rsp), %rdx
	xorq	%rax, %rax
	movq	%r15, %rdi
	callq	Perl_sv_catpvf
	jmp	.LBB1_330
.LBB1_408:                              # %if.else1190
	movabsq	$PL_sv_undef, %rax
	cmpq	%rax, %rbx
	movq	232(%rsp), %r13         # 8-byte Reload
	je	.LBB1_410
# BB#409:                               # %if.else1190
	movq	%r14, %rax
	andq	$118423552, %rax        # imm = 0x70F0000
	je	.LBB1_410
.LBB1_411:                              # %integer_came_from_string
	testq	$262144, %r14           # imm = 0x40000
	je	.LBB1_413
# BB#412:                               # %cond.true1203
	movq	(%rbx), %rax
	movq	8(%rax), %rcx
	movq	%rcx, 280(%rsp)
	movq	(%rax), %rbx
	jmp	.LBB1_414
.LBB1_413:                              # %cond.false1208
	leaq	280(%rsp), %rsi
	movq	$2, %rdx
	movq	%rbx, %rdi
	callq	Perl_sv_2pv_flags
	movq	16(%rbx), %r14
	movq	%rax, %rbx
.LBB1_414:                              # %cond.end1210
	movq	264(%rsp), %r12         # 8-byte Reload
	testq	$536870912, %r14        # imm = 0x20000000
	je	.LBB1_417
# BB#415:                               # %land.lhs.true1215
	movq	PL_curcop(%rip), %rax
	movzbl	37(%rax), %eax
	testq	$8, %rax
	je	.LBB1_416
.LBB1_417:                              # %if.else1223
	movq	(%r15), %rax
	movq	8(%rax), %rax
	movq	280(%rsp), %rcx
	leaq	3(%rax,%rcx,2), %rsi
	movq	%r15, %rdi
	callq	Perl_sv_grow
	movq	(%r15), %rax
	movq	(%rax), %r11
	movq	8(%rax), %r10
	movq	$39, %rdi
	movb	%dil, (%r11,%r10)
	movq	280(%rsp), %rsi
	leaq	1(%r10), %r14
	xorq	%r8, %r8
	testq	%rsi, %rsi
	movabsq	$0, %rax
	movabsq	$0, %rdx
	je	.LBB1_424
# BB#418:                               # %while.body.i40.preheader
	movq	%r15, %r9
	movq	%r12, %r13
	leaq	(%r11,%r14), %rcx
	xorq	%rdx, %rdx
	movabsq	$4294967295, %r12       # imm = 0xFFFFFFFF
	movq	$92, %r15
	.align	16, 0x90
.LBB1_419:                              # %while.body.i40
                                        # =>This Inner Loop Header: Depth=1
	movsbq	(%rbx), %rax
	movq	%rax, %rdi
	andq	%r12, %rdi
	cmpq	$39, %rdi
	je	.LBB1_421
# BB#420:                               # %while.body.i40
                                        #   in Loop: Header=BB1_419 Depth=1
	cmpq	$92, %rdi
	jne	.LBB1_422
.LBB1_421:                              # %sw.bb.i
                                        #   in Loop: Header=BB1_419 Depth=1
	movb	%r15b, (%rcx)
	movzbl	(%rbx), %eax
	incq	%rcx
	incq	%rdx
.LBB1_422:                              # %sw.default.i
                                        #   in Loop: Header=BB1_419 Depth=1
	movb	%al, (%rcx)
	incq	%rcx
	incq	%rbx
	decq	%rsi
	jne	.LBB1_419
# BB#423:                               # %esc_q.exit.loopexit
	movq	280(%rsp), %rax
	movq	%r13, %r12
	movq	%r9, %r15
	movq	232(%rsp), %r13         # 8-byte Reload
	movq	$39, %rdi
.LBB1_424:                              # %esc_q.exit
	leaq	(%rax,%rdx), %rcx
	leaq	2(%rax,%rdx), %rax
	movq	%rax, 280(%rsp)
	addq	%rcx, %r14
	movb	%dil, (%r11,%r14)
	movq	280(%rsp), %rax
	addq	%r10, %rax
	movb	%r8b, (%r11,%rax)
	movq	(%r15), %rax
	movq	280(%rsp), %rcx
	addq	%rcx, 8(%rax)
	jmp	.LBB1_425
.LBB1_328:                              # %if.else1041
	leaq	448(%rsp), %rsi
	movq	$2, %rcx
	movq	%r15, %rdi
	movq	%r12, %rdx
	jmp	.LBB1_329
.LBB1_416:                              # %if.then1220
	movq	280(%rsp), %rdx
	movq	%r15, %rdi
	movq	%rbx, %rsi
	callq	esc_q_utf8
	addq	%rax, 280(%rsp)
.LBB1_425:                              # %if.end1253
	movq	256(%rsp), %r14         # 8-byte Reload
	jmp	.LBB1_426
.LBB1_410:                              # %if.then1197
	movabsq	$.L.str71, %rsi
	movq	$5, %rdx
	movq	$2, %rcx
	movq	%r15, %rdi
.LBB1_329:                              # %if.end1253
	callq	Perl_sv_catpvn_flags
.LBB1_330:                              # %if.end1253
	movq	256(%rsp), %r14         # 8-byte Reload
	movq	264(%rsp), %r12         # 8-byte Reload
	jmp	.LBB1_426
.LBB1_334:                              # %cond.false1057
	leaq	280(%rsp), %rsi
	movq	$2, %rdx
	movq	%rbx, %rdi
	callq	Perl_sv_2pv_flags
	movq	280(%rsp), %r14
	movq	%rax, %r12
.LBB1_335:                              # %cond.end1059
	leaq	1(%r12), %r15
	leaq	-1(%r14), %r13
	cmpq	$6, %r13
	movq	%r13, 280(%rsp)
	jb	.LBB1_336
# BB#337:                               # %land.lhs.true1065
	movq	%rbx, 224(%rsp)         # 8-byte Spill
	movabsq	$.L.str70, %rsi
	movq	$6, %rdx
	movq	%r15, %rdi
	callq	strncmp
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB1_339
# BB#338:                               # %if.then1069
	addq	$5, %r12
	addq	$-5, %r14
	movq	%r14, 280(%rsp)
	movq	%r14, %r13
	movq	%r12, %r15
	jmp	.LBB1_339
.LBB1_336:
	movq	%rbx, 224(%rsp)         # 8-byte Spill
.LBB1_339:                              # %if.end1072
	movq	%r13, %rsi
	movzbl	(%r15), %edx
	movq	%r15, %rax
	movq	256(%rsp), %r14         # 8-byte Reload
.LBB1_340:                              # %TOP.i164
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB1_349 Depth 2
	movq	%rdx, %rcx
	andq	$255, %rcx
	cmpq	$58, %rcx
	jne	.LBB1_344
# BB#341:                               # %if.then.i167
                                        #   in Loop: Header=BB1_340 Depth=1
	movzbl	1(%rax), %ecx
	testq	%rcx, %rcx
	je	.LBB1_356
# BB#342:                               # %if.then2.i170
                                        #   in Loop: Header=BB1_340 Depth=1
	cmpq	$58, %rcx
	jne	.LBB1_356
# BB#343:                               # %if.then2.if.end9_crit_edge.i172
                                        #   in Loop: Header=BB1_340 Depth=1
	addq	$2, %rax
	movzbl	(%rax), %edx
.LBB1_344:                              # %if.end9.i175
                                        #   in Loop: Header=BB1_340 Depth=1
	leaq	-65(%rdx), %rcx
	andq	$255, %rcx
	cmpq	$26, %rcx
	jb	.LBB1_347
# BB#345:                               # %lor.lhs.false.i179
                                        #   in Loop: Header=BB1_340 Depth=1
	leaq	-97(%rdx), %rcx
	andq	$255, %rcx
	cmpq	$26, %rcx
	jb	.LBB1_347
# BB#346:                               # %lor.lhs.false.i179
                                        #   in Loop: Header=BB1_340 Depth=1
	andq	$255, %rdx
	cmpq	$95, %rdx
	jne	.LBB1_356
.LBB1_347:                              # %while.cond.preheader.i182
                                        #   in Loop: Header=BB1_340 Depth=1
	movzbl	1(%rax), %ecx
	testq	%rcx, %rcx
	je	.LBB1_353
# BB#348:                               #   in Loop: Header=BB1_340 Depth=1
	incq	%rax
	.align	16, 0x90
.LBB1_349:                              # %while.body.i189
                                        #   Parent Loop BB1_340 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	leaq	-65(%rcx), %rdx
	andq	$255, %rdx
	cmpq	$26, %rdx
	jb	.LBB1_352
# BB#350:                               # %while.body.i189
                                        #   in Loop: Header=BB1_349 Depth=2
	leaq	-97(%rcx), %rdx
	andq	$255, %rdx
	cmpq	$26, %rdx
	jb	.LBB1_352
# BB#351:                               # %while.body.i189
                                        #   in Loop: Header=BB1_349 Depth=2
	leaq	-48(%rcx), %rdx
	andq	$255, %rdx
	cmpq	$9, %rdx
	jbe	.LBB1_352
# BB#354:                               # %lor.lhs.false53.i193
                                        #   in Loop: Header=BB1_349 Depth=2
	andq	$255, %rcx
	cmpq	$95, %rcx
	jne	.LBB1_355
.LBB1_352:                              # %while.cond.backedge.i192
                                        #   in Loop: Header=BB1_349 Depth=2
	movzbl	1(%rax), %ecx
	incq	%rax
	testq	%rcx, %rcx
	jne	.LBB1_349
	jmp	.LBB1_353
.LBB1_355:                              # %lor.lhs.false53.i193
                                        #   in Loop: Header=BB1_340 Depth=1
	movq	$58, %rdx
	cmpq	$58, %rcx
	je	.LBB1_340
.LBB1_356:                              # %if.then1075
	movq	%r14, %r13
	movq	240(%rsp), %rcx         # 8-byte Reload
	movq	(%rcx), %rax
	movq	8(%rax), %rax
	leaq	6(%rax,%rsi,2), %rsi
	movq	%rcx, %rbx
	movq	%rbx, %rdi
	callq	Perl_sv_grow
	movq	(%rbx), %rax
	movq	(%rax), %r14
	movq	8(%rax), %r11
	movq	$31530, %rax            # imm = 0x7B2A
	movw	%ax, (%r14,%r11)
	movq	$39, %r9
	movb	%r9b, 2(%r14,%r11)
	movq	280(%rsp), %rsi
	leaq	3(%r11), %r10
	xorq	%rdi, %rdi
	testq	%rsi, %rsi
	movabsq	$0, %rax
	movabsq	$0, %rdx
	je	.LBB1_363
# BB#357:                               # %while.body.i77.preheader
	movq	%rbx, %r8
	leaq	(%r14,%r10), %rdi
	xorq	%rdx, %rdx
	movabsq	$4294967295, %r12       # imm = 0xFFFFFFFF
	movq	$92, %rbx
.LBB1_358:                              # %while.body.i77
                                        # =>This Inner Loop Header: Depth=1
	movsbq	(%r15), %rcx
	movq	%rcx, %rax
	andq	%r12, %rax
	cmpq	$39, %rax
	je	.LBB1_360
# BB#359:                               # %while.body.i77
                                        #   in Loop: Header=BB1_358 Depth=1
	cmpq	$92, %rax
	jne	.LBB1_361
.LBB1_360:                              # %sw.bb.i81
                                        #   in Loop: Header=BB1_358 Depth=1
	movb	%bl, (%rdi)
	movzbl	(%r15), %ecx
	incq	%rdi
	incq	%rdx
.LBB1_361:                              # %sw.default.i88
                                        #   in Loop: Header=BB1_358 Depth=1
	movb	%cl, (%rdi)
	incq	%rdi
	incq	%r15
	decq	%rsi
	jne	.LBB1_358
# BB#362:                               # %esc_q.exit90.loopexit
	movq	280(%rsp), %rax
	movq	%r8, %rbx
	xorq	%rdi, %rdi
.LBB1_363:                              # %esc_q.exit90
	leaq	(%rax,%rdx), %rcx
	leaq	4(%rax,%rdx), %rax
	movq	%rax, 280(%rsp)
	addq	%rcx, %r10
	movb	%r9b, (%r14,%r10)
	movq	280(%rsp), %rax
	leaq	1(%rax), %rcx
	movq	%rcx, 280(%rsp)
	addq	%r11, %rax
	movq	$125, %rcx
	movb	%cl, (%r14,%rax)
	movq	280(%rsp), %rax
	addq	%r11, %rax
	movb	%dil, (%r14,%rax)
	movq	280(%rsp), %rax
	leaq	(%r14,%r11), %rcx
	movq	%rcx, 208(%rsp)         # 8-byte Spill
	movq	%r13, %r14
	jmp	.LBB1_364
.LBB1_353:                              # %if.else1099
	movq	240(%rsp), %r13         # 8-byte Reload
	movq	(%r13), %rax
	movq	8(%rax), %rax
	leaq	2(%rax,%rsi), %rsi
	movq	%r13, %rbx
	movq	%rbx, %rdi
	callq	Perl_sv_grow
	movq	(%rbx), %rax
	movq	(%rax), %rcx
	movq	8(%rax), %rax
	leaq	(%rcx,%rax), %rdx
	movq	%rdx, 208(%rsp)         # 8-byte Spill
	movq	$42, %rdx
	movb	%dl, (%rcx,%rax)
	leaq	1(%rcx,%rax), %rdi
	movq	%r15, %rsi
	callq	strcpy
	movq	280(%rsp), %rax
	incq	%rax
	movq	%rax, 280(%rsp)
.LBB1_364:                              # %if.end1114
	movq	(%rbx), %rcx
	addq	%rax, 8(%rcx)
	movq	696(%rsp), %rax
	testq	%rax, %rax
	je	.LBB1_365
# BB#366:                               # %if.then1122
	movq	%r14, 256(%rsp)         # 8-byte Spill
	movabsq	$.L.str3, %rbx
	xorq	%rsi, %rsi
	xorq	%r12, %r12
	movq	%rbx, %rdi
	callq	Perl_newSVpvn
	movq	%rax, 240(%rsp)         # 8-byte Spill
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	Perl_newSVpvn
	movq	%rax, 216(%rsp)         # 8-byte Spill
	movq	632(%rsp), %rax
	cmpq	$2, %rax
	jl	.LBB1_395
# BB#367:
	movq	$32, %r13
	movq	224(%rsp), %rcx         # 8-byte Reload
	jmp	.LBB1_368
.LBB1_376:                              # %for.inc1186.us.thread
                                        #   in Loop: Header=BB1_368 Depth=1
	incq	%r12
	.align	16, 0x90
.LBB1_368:                              # %for.body1134.us
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB1_388 Depth 2
                                        #     Child Loop BB1_392 Depth 2
	testq	%r12, %r12
	jne	.LBB1_369
# BB#372:                               # %cond.true1137.us
                                        #   in Loop: Header=BB1_368 Depth=1
	movq	(%rcx), %rax
	movq	56(%rax), %rax
	movq	(%rax), %r14
	jmp	.LBB1_373
	.align	16, 0x90
.LBB1_369:                              # %cond.false1141.us
                                        #   in Loop: Header=BB1_368 Depth=1
	movq	(%rcx), %rax
	movq	56(%rax), %rax
	cmpq	$1, %r12
	jne	.LBB1_370
# BB#371:                               # %cond.true1144.us
                                        #   in Loop: Header=BB1_368 Depth=1
	movq	32(%rax), %r14
	jmp	.LBB1_373
.LBB1_370:                              # %cond.false1147.us
                                        #   in Loop: Header=BB1_368 Depth=1
	movq	40(%rax), %r14
.LBB1_373:                              # %cond.end1152.us
                                        #   in Loop: Header=BB1_368 Depth=1
	testq	%r14, %r14
	je	.LBB1_394
# BB#374:                               # %if.end1156.us
                                        #   in Loop: Header=BB1_368 Depth=1
	testq	%r12, %r12
	jne	.LBB1_377
# BB#375:                               # %land.lhs.true1159.us
                                        #   in Loop: Header=BB1_368 Depth=1
	movzbl	18(%r14), %eax
	testq	$1807, %rax             # imm = 0x70F
	je	.LBB1_376
.LBB1_377:                              # %if.then1176.us
                                        #   in Loop: Header=BB1_368 Depth=1
	movq	280(%rsp), %rsi
	movq	$0, 272(%rsp)
	movq	208(%rsp), %rdi         # 8-byte Reload
	callq	Perl_newSVpvn
	movq	%rax, %r15
	movq	$2, %rdx
	movq	240(%rsp), %rbx         # 8-byte Reload
	movq	%rbx, %rdi
	movq	%r15, %rsi
	callq	Perl_sv_setsv_flags
	movq	DD_dump.entries(,%r12,8), %rsi
	movq	DD_dump.sizes(,%r12,8), %rdx
	movq	$2, %rcx
	movq	%rbx, %rdi
	callq	Perl_sv_catpvn_flags
	movq	$3, %rdx
	movq	$2, %rcx
	movq	%r15, %rdi
	movabsq	$.L.str26, %rsi
	callq	Perl_sv_catpvn_flags
	movq	248(%rsp), %rdi         # 8-byte Reload
	movq	%r15, %rsi
	callq	Perl_av_push
	movq	%r14, %rdi
	callq	Perl_newRV
	movq	216(%rsp), %rdx         # 8-byte Reload
	movq	(%rdx), %rcx
	movq	$0, 8(%rcx)
	movq	(%r15), %rcx
	movq	8(%rcx), %r14
	movq	%rax, 200(%rsp)         # 8-byte Spill
	testq	%rdx, %rdx
	movq	%rdx, %rbx
	jne	.LBB1_379
# BB#378:                               # %if.then.i45.us
                                        #   in Loop: Header=BB1_368 Depth=1
	xorq	%rsi, %rsi
	movabsq	$.L.str3, %rdi
	callq	Perl_newSVpvn
	movq	%rax, %rbx
.LBB1_379:                              # %if.end.i48.us
                                        #   in Loop: Header=BB1_368 Depth=1
	testq	%r14, %r14
	jle	.LBB1_393
# BB#380:                               # %if.then2.i55.us
                                        #   in Loop: Header=BB1_368 Depth=1
	movq	(%rbx), %rax
	movq	8(%rax), %r8
	movq	16(%rax), %rcx
	leaq	1(%r14,%r8), %rsi
	cmpq	%rsi, %rcx
	jae	.LBB1_382
# BB#381:                               # %cond.true.i57.us
                                        #   in Loop: Header=BB1_368 Depth=1
	movq	%rbx, %rdi
	callq	Perl_sv_grow
	movq	(%rbx), %rax
	movq	8(%rax), %r8
.LBB1_382:                              # %cond.end.i58.us
                                        #   in Loop: Header=BB1_368 Depth=1
	leaq	(%r8,%r14), %rdi
	movq	%r14, %rcx
	negq	%rcx
	cmpq	$-2, %rcx
	jg	.LBB1_384
# BB#383:                               # %cond.end.i58.us
                                        #   in Loop: Header=BB1_368 Depth=1
	movq	$-1, %rcx
.LBB1_384:                              # %cond.end.i58.us
                                        #   in Loop: Header=BB1_368 Depth=1
	leaq	1(%r14,%rcx), %rdx
	movq	%rdx, %rcx
	andq	$-4, %rcx
	movq	(%rax), %r9
	xorq	%rsi, %rsi
	movq	%rdx, %rbx
	andq	$-4, %rbx
	movq	%rdi, 8(%rax)
	movb	%sil, (%r9,%rdi)
	je	.LBB1_390
# BB#385:                               # %vector.body323.preheader
                                        #   in Loop: Header=BB1_368 Depth=1
	movq	%r14, %rax
	negq	%rax
	cmpq	$-2, %rax
	jg	.LBB1_387
# BB#386:                               # %vector.body323.preheader
                                        #   in Loop: Header=BB1_368 Depth=1
	movq	$-1, %rax
.LBB1_387:                              # %vector.body323.preheader
                                        #   in Loop: Header=BB1_368 Depth=1
	leaq	1(%r14,%rax), %rax
	subq	%rcx, %r14
	andq	$-4, %rax
	leaq	-4(%r9,%rdi), %rcx
	.align	16, 0x90
.LBB1_388:                              # %vector.body323
                                        #   Parent Loop BB1_368 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movb	%r13b, 3(%rcx)
	movb	%r13b, 2(%rcx)
	movb	%r13b, 1(%rcx)
	movb	%r13b, (%rcx)
	addq	$-4, %rcx
	addq	$-4, %rax
	jne	.LBB1_388
# BB#389:                               #   in Loop: Header=BB1_368 Depth=1
	movq	%rbx, %rsi
.LBB1_390:                              # %middle.block324
                                        #   in Loop: Header=BB1_368 Depth=1
	cmpq	%rsi, %rdx
	je	.LBB1_393
# BB#391:                               # %while.body.i69.us.preheader
                                        #   in Loop: Header=BB1_368 Depth=1
	leaq	-1(%r9,%r8), %rax
	.align	16, 0x90
.LBB1_392:                              # %while.body.i69.us
                                        #   Parent Loop BB1_368 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movb	%r13b, (%rax,%r14)
	leaq	-1(%r14), %r14
	testq	%r14, %r14
	jg	.LBB1_392
.LBB1_393:                              # %if.end1180.us
                                        #   in Loop: Header=BB1_368 Depth=1
	movq	240(%rsp), %rax         # 8-byte Reload
	movq	(%rax), %rax
	movq	(%rax), %rsi
	movq	8(%rax), %rdx
	movq	736(%rsp), %rax
	movq	%rax, 112(%rsp)
	movq	728(%rsp), %rax
	movq	%rax, 104(%rsp)
	movq	720(%rsp), %rax
	movq	%rax, 96(%rsp)
	movq	712(%rsp), %rax
	movq	%rax, 88(%rsp)
	movq	704(%rsp), %rax
	movq	%rax, 80(%rsp)
	movq	696(%rsp), %rax
	movq	%rax, 72(%rsp)
	movq	688(%rsp), %rax
	movq	%rax, 64(%rsp)
	movq	680(%rsp), %rax
	movq	%rax, 56(%rsp)
	movq	672(%rsp), %rax
	movq	%rax, 48(%rsp)
	movq	664(%rsp), %rax
	movq	%rax, 40(%rsp)
	movq	216(%rsp), %rax         # 8-byte Reload
	movq	%rax, 32(%rsp)
	movq	648(%rsp), %rax
	movq	%rax, 24(%rsp)
	movq	640(%rsp), %rax
	movq	%rax, 16(%rsp)
	movq	632(%rsp), %rax
	movq	%rax, 8(%rsp)
	leaq	272(%rsp), %rax
	movq	%rax, (%rsp)
	movq	200(%rsp), %rbx         # 8-byte Reload
	movq	%rbx, %rdi
	movq	%r15, %rcx
	movq	256(%rsp), %r8          # 8-byte Reload
	movq	248(%rsp), %r9          # 8-byte Reload
	callq	DD_dump
	movq	%rbx, %rdi
	callq	Perl_sv_free
	movq	224(%rsp), %rcx         # 8-byte Reload
.LBB1_394:                              # %for.inc1186.us
                                        #   in Loop: Header=BB1_368 Depth=1
	incq	%r12
	cmpq	$3, %r12
	jne	.LBB1_368
	jmp	.LBB1_407
.LBB1_365:
	movq	264(%rsp), %r12         # 8-byte Reload
	movq	232(%rsp), %r13         # 8-byte Reload
	jmp	.LBB1_426
.LBB1_395:
	movq	224(%rsp), %rcx         # 8-byte Reload
	movq	240(%rsp), %r13         # 8-byte Reload
	movq	248(%rsp), %r14         # 8-byte Reload
	jmp	.LBB1_396
.LBB1_404:                              # %for.inc1186.thread
                                        #   in Loop: Header=BB1_396 Depth=1
	incq	%r12
	.align	16, 0x90
.LBB1_396:                              # %for.body1134
                                        # =>This Inner Loop Header: Depth=1
	testq	%r12, %r12
	jne	.LBB1_398
# BB#397:                               # %cond.true1137
                                        #   in Loop: Header=BB1_396 Depth=1
	movq	(%rcx), %rax
	movq	56(%rax), %rax
	movq	(%rax), %rbx
	jmp	.LBB1_401
	.align	16, 0x90
.LBB1_398:                              # %cond.false1141
                                        #   in Loop: Header=BB1_396 Depth=1
	movq	(%rcx), %rax
	movq	56(%rax), %rax
	cmpq	$1, %r12
	jne	.LBB1_400
# BB#399:                               # %cond.true1144
                                        #   in Loop: Header=BB1_396 Depth=1
	movq	32(%rax), %rbx
	jmp	.LBB1_401
.LBB1_400:                              # %cond.false1147
                                        #   in Loop: Header=BB1_396 Depth=1
	movq	40(%rax), %rbx
.LBB1_401:                              # %cond.end1152
                                        #   in Loop: Header=BB1_396 Depth=1
	testq	%rbx, %rbx
	je	.LBB1_406
# BB#402:                               # %if.end1156
                                        #   in Loop: Header=BB1_396 Depth=1
	testq	%r12, %r12
	jne	.LBB1_405
# BB#403:                               # %land.lhs.true1159
                                        #   in Loop: Header=BB1_396 Depth=1
	movzbl	18(%rbx), %eax
	testq	$1807, %rax             # imm = 0x70F
	je	.LBB1_404
.LBB1_405:                              # %if.end1180
                                        #   in Loop: Header=BB1_396 Depth=1
	movq	280(%rsp), %rsi
	movq	$0, 272(%rsp)
	movq	208(%rsp), %rdi         # 8-byte Reload
	callq	Perl_newSVpvn
	movq	%rax, %r15
	movq	$2, %rdx
	movq	%r13, %rdi
	movq	%r15, %rsi
	callq	Perl_sv_setsv_flags
	movq	DD_dump.entries(,%r12,8), %rsi
	movq	DD_dump.sizes(,%r12,8), %rdx
	movq	$2, %rcx
	movq	%r13, %rdi
	callq	Perl_sv_catpvn_flags
	movq	$3, %rdx
	movq	$2, %rcx
	movq	%r15, %rdi
	movabsq	$.L.str26, %rsi
	callq	Perl_sv_catpvn_flags
	movq	%r14, %rdi
	movq	%r15, %rsi
	callq	Perl_av_push
	movq	%rbx, %rdi
	callq	Perl_newRV
	movq	216(%rsp), %rdi         # 8-byte Reload
	movq	(%rdi), %rcx
	movq	$0, 8(%rcx)
	movq	(%r13), %rcx
	movq	(%rcx), %rsi
	movq	8(%rcx), %rdx
	movq	%rax, %rbx
	movq	736(%rsp), %rax
	movq	%rax, 112(%rsp)
	movq	728(%rsp), %rax
	movq	%rax, 104(%rsp)
	movq	720(%rsp), %rax
	movq	%rax, 96(%rsp)
	movq	712(%rsp), %rax
	movq	%rax, 88(%rsp)
	movq	704(%rsp), %rax
	movq	%rax, 80(%rsp)
	movq	696(%rsp), %rax
	movq	%rax, 72(%rsp)
	movq	688(%rsp), %rax
	movq	%rax, 64(%rsp)
	movq	680(%rsp), %rax
	movq	%rax, 56(%rsp)
	movq	672(%rsp), %rax
	movq	%rax, 48(%rsp)
	movq	664(%rsp), %rax
	movq	%rax, 40(%rsp)
	movq	%rdi, 32(%rsp)
	movq	648(%rsp), %rax
	movq	%rax, 24(%rsp)
	movq	640(%rsp), %rax
	movq	%rax, 16(%rsp)
	movq	632(%rsp), %rax
	movq	%rax, 8(%rsp)
	leaq	272(%rsp), %rax
	movq	%rax, (%rsp)
	movq	%rbx, %rdi
	movq	%r15, %rcx
	movq	256(%rsp), %r8          # 8-byte Reload
	movq	%r14, %r9
	callq	DD_dump
	movq	%rbx, %rdi
	callq	Perl_sv_free
	movq	224(%rsp), %rcx         # 8-byte Reload
.LBB1_406:                              # %for.inc1186
                                        #   in Loop: Header=BB1_396 Depth=1
	incq	%r12
	cmpq	$3, %r12
	jne	.LBB1_396
.LBB1_407:                              # %for.end1188
	movq	216(%rsp), %rdi         # 8-byte Reload
	callq	Perl_sv_free
	movq	240(%rsp), %rdi         # 8-byte Reload
	callq	Perl_sv_free
	movq	264(%rsp), %r12         # 8-byte Reload
	movq	256(%rsp), %r14         # 8-byte Reload
	movq	232(%rsp), %r13         # 8-byte Reload
	jmp	.LBB1_426
.LBB1_20:                               # %cond.false72
	testq	$65536, %rax            # imm = 0x10000
	je	.LBB1_22
# BB#21:                                # %cond.true79
	movq	(%rdi), %rax
	cmpq	$0, 24(%rax)
	jne	.LBB1_26
	jmp	.LBB1_27
.LBB1_306:
	movq	%r14, %rax
	jmp	.LBB1_319
.LBB1_308:
	movq	%r14, %rax
	jmp	.LBB1_319
.LBB1_22:                               # %cond.false86
	testq	$131072, %rax           # imm = 0x20000
	je	.LBB1_24
# BB#23:                                # %cond.true93
	movq	(%rdi), %rax
	movq	32(%rax), %rdi
	xorq	%rsi, %rsi
	callq	__nedf2
	testq	%rax, %rax
	jne	.LBB1_26
	jmp	.LBB1_27
.LBB1_310:
	movq	%r14, %rax
	jmp	.LBB1_319
.LBB1_312:
	movq	%r14, %rax
	jmp	.LBB1_319
.LBB1_24:                               # %cond.false100
	callq	Perl_sv_2bool
	testq	$255, %rax
	je	.LBB1_27
# BB#25:                                # %cond.false100.if.then107_crit_edge
	movq	PL_errgv(%rip), %rbx
.LBB1_26:                               # %if.then107
	movq	(%rbx), %rax
	movq	56(%rax), %rax
	movq	(%rax), %rsi
	movabsq	$.L.str32, %rdi
	xorq	%rax, %rax
	callq	Perl_warn
.LBB1_27:                               # %if.end111
	movq	PL_tmps_ix(%rip), %rax
	movq	PL_tmps_floor(%rip), %rcx
	movq	%r14, PL_stack_sp(%rip)
	cmpq	%rcx, %rax
	jle	.LBB1_29
# BB#28:                                # %if.then114
	callq	Perl_free_tmps
.LBB1_29:                               # %if.end115
	callq	Perl_pop_scope
	movq	256(%rsp), %r14         # 8-byte Reload
	movq	%r12, %rbx
.LBB1_30:                               # %if.end116
	movq	%rbx, 224(%rsp)         # 8-byte Spill
	movq	(%rbx), %rax
	movq	(%rax), %r12
	movzbl	16(%r12), %eax
	movq	%rax, 184(%rsp)         # 8-byte Spill
	movabsq	$.L.str33, %rsi
	leaq	320(%rsp), %rbx
	xorq	%rax, %rax
	movq	$0, 192(%rsp)           # 8-byte Folded Spill
	movq	%rbx, %rdi
	movq	%r12, %rdx
	callq	sprintf
	movq	%rbx, %rdi
	callq	strlen
	movzbl	17(%r12), %ecx
	movq	%rax, 232(%rsp)         # 8-byte Spill
	testq	$16, %rcx
	movabsq	$0, %rbx
	je	.LBB1_32
# BB#31:                                # %if.then127
	movq	(%r12), %rax
	movq	48(%rax), %rax
	movq	(%rax), %rax
	movq	80(%rax), %rbx
.LBB1_32:                               # %if.end131
	movq	%r12, 216(%rsp)         # 8-byte Spill
	cmpq	$0, 264(%rsp)           # 8-byte Folded Reload
	je	.LBB1_64
# BB#33:                                # %if.then133
	leaq	320(%rsp), %rsi
	xorq	%rcx, %rcx
	movq	%r14, %rdi
	movq	232(%rsp), %rdx         # 8-byte Reload
	callq	Perl_hv_fetch
	testq	%rax, %rax
	je	.LBB1_56
# BB#34:                                # %land.lhs.true137
	movq	(%rax), %rax
	testq	%rax, %rax
	je	.LBB1_56
# BB#35:                                # %land.lhs.true139
	movzbl	18(%rax), %ecx
	testq	$8, %rcx
	je	.LBB1_56
# BB#36:                                # %land.lhs.true143
	movq	(%rax), %rax
	movq	(%rax), %rdi
	testq	%rdi, %rdi
	je	.LBB1_56
# BB#37:                                # %if.then147
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	callq	Perl_av_fetch
	testq	%rax, %rax
	je	.LBB1_55
# BB#38:                                # %land.lhs.true151
	movq	(%rax), %r14
	testq	%r14, %r14
	je	.LBB1_55
# BB#39:                                # %if.then153
	movq	696(%rsp), %rax
	testq	%rax, %rax
	je	.LBB1_48
# BB#40:                                # %land.lhs.true155
	movq	624(%rsp), %rax
	cmpq	$0, (%rax)
	jle	.LBB1_48
# BB#41:                                # %if.then158
	movq	184(%rsp), %rax         # 8-byte Reload
	cmpq	$10, %rax
	jne	.LBB1_42
# BB#44:                                # %if.then166
	movabsq	$.L.str35, %rsi
	movq	$2, %rdx
	movq	$2, %rcx
	movq	%r15, %rdi
	callq	Perl_sv_catpvn_flags
	movq	264(%rsp), %rbx         # 8-byte Reload
	jmp	.LBB1_47
.LBB1_56:                               # %if.else208
	movzbl	(%r13), %eax
	cmpq	$37, %rax
	je	.LBB1_58
# BB#57:                                # %if.else208
	cmpq	$64, %rax
	jne	.LBB1_59
.LBB1_58:                               # %if.then219
	movq	%rbx, 200(%rsp)         # 8-byte Spill
	movq	%r13, %rbx
	movq	%r15, 240(%rsp)         # 8-byte Spill
	movabsq	$.L.str40, %rdi
	movq	$1, %rsi
	callq	Perl_newSVpvn
	movq	%rax, %r15
	movq	$2, %rcx
	movq	%r15, %rdi
	movq	%rbx, %rsi
	movq	%rbx, %r13
	movq	264(%rsp), %rdx         # 8-byte Reload
	callq	Perl_sv_catpvn_flags
.LBB1_63:                               # %land.end
	callq	Perl_newAV
	movq	%rax, %r12
	movq	%r12, 192(%rsp)         # 8-byte Spill
	movq	%r12, %rdi
	movq	%r15, %rsi
	callq	Perl_av_push
	movq	224(%rsp), %rsi         # 8-byte Reload
	movq	%rsi, PL_Sv(%rip)
	incq	8(%rsi)
	movq	%r12, %rdi
	callq	Perl_av_push
	leaq	320(%rsp), %r15
	movq	%r15, %rdi
	callq	strlen
	movq	%rax, %rbx
	movq	%r12, %rdi
	callq	Perl_newRV
	xorq	%r8, %r8
	movq	%r14, %rdi
	movq	%r15, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	Perl_hv_store
	movq	%r12, %rdi
	callq	Perl_sv_free
	movq	240(%rsp), %r15         # 8-byte Reload
	movq	200(%rsp), %rbx         # 8-byte Reload
.LBB1_64:                               # %if.end247
	testq	%rbx, %rbx
	je	.LBB1_76
# BB#65:                                # %land.lhs.true249
	movzbl	(%rbx), %eax
	cmpq	$82, %rax
	jne	.LBB1_76
# BB#66:                                # %land.lhs.true253
	movabsq	$.L.str41, %rsi
	movq	%rbx, %rdi
	callq	strcmp
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB1_67
.LBB1_76:                               # %if.end282
	movq	696(%rsp), %rax
	testq	%rax, %rax
	jne	.LBB1_83
# BB#77:                                # %if.end282
	movq	728(%rsp), %rax
	testq	%rax, %rax
	jle	.LBB1_83
# BB#78:                                # %land.lhs.true287
	movq	624(%rsp), %rax
	movq	(%rax), %rax
	movq	728(%rsp), %rcx
	cmpq	%rcx, %rax
	jge	.LBB1_79
.LBB1_83:                               # %if.end306
	movq	656(%rsp), %rax
	movq	%rax, 224(%rsp)         # 8-byte Spill
	testq	%rbx, %rbx
	je	.LBB1_84
# BB#85:                                # %if.then308
	movq	%r13, 176(%rsp)         # 8-byte Spill
	movq	720(%rsp), %rdi
	movzbl	18(%rdi), %eax
	testq	$4, %rax
	je	.LBB1_87
# BB#86:                                # %cond.true315
	movq	%rbx, 200(%rsp)         # 8-byte Spill
	movq	(%rdi), %rax
	movq	8(%rax), %rdx
	movq	%rdx, 296(%rsp)
	movq	(%rax), %rsi
	jmp	.LBB1_88
.LBB1_84:
	movq	%rbx, 200(%rsp)         # 8-byte Spill
	movq	%r13, 176(%rsp)         # 8-byte Spill
	movq	%r15, 240(%rsp)         # 8-byte Spill
	jmp	.LBB1_89
.LBB1_79:                               # %if.then290
	movq	224(%rsp), %rdi         # 8-byte Reload
	movzbl	18(%rdi), %eax
	testq	$4, %rax
	je	.LBB1_81
# BB#80:                                # %cond.true297
	movq	(%rdi), %rax
	movq	8(%rax), %rcx
	movq	%rcx, 304(%rsp)
	movq	(%rax), %rbx
	jmp	.LBB1_82
.LBB1_87:                               # %cond.false320
	movq	%rbx, 200(%rsp)         # 8-byte Spill
	leaq	296(%rsp), %rsi
	movq	$2, %rdx
	callq	Perl_sv_2pv_flags
	movq	296(%rsp), %rdx
	movq	%rax, %rsi
.LBB1_88:                               # %cond.end322
	movq	$2, %rcx
	movq	%r15, %rdi
	callq	Perl_sv_catpvn_flags
	movabsq	$.L.str46, %rsi
	movq	$2, %rdx
	movq	$2, %rcx
	movq	%r15, %rdi
	movq	%r15, 240(%rsp)         # 8-byte Spill
	callq	Perl_sv_catpvn_flags
	movq	632(%rsp), %rax
	cmpq	$2, %rax
	jl	.LBB1_89
# BB#90:                                # %if.then326
	movq	%r14, %r12
	movq	656(%rsp), %rdi
	callq	Perl_newSVsv
	movq	296(%rsp), %r15
	movq	%rax, 224(%rsp)         # 8-byte Spill
	leaq	2(%r15), %rbx
	testq	%rax, %rax
	movq	%rax, %r14
	jne	.LBB1_92
# BB#91:                                # %if.then.i
	movabsq	$.L.str3, %rdi
	xorq	%rsi, %rsi
	callq	Perl_newSVpvn
	movq	%rax, %r14
.LBB1_92:                               # %if.end.i
	movq	%r12, 256(%rsp)         # 8-byte Spill
	testq	%rbx, %rbx
	jle	.LBB1_93
# BB#94:                                # %if.then2.i
	movq	264(%rsp), %r13         # 8-byte Reload
	movq	(%r14), %rdi
	movq	8(%rdi), %r9
	movq	16(%rdi), %rax
	leaq	3(%r15,%r9), %rsi
	cmpq	%rsi, %rax
	jae	.LBB1_96
# BB#95:                                # %cond.true.i
	movq	%r14, %rdi
	callq	Perl_sv_grow
	movq	(%r14), %rdi
	movq	8(%rdi), %r9
.LBB1_96:                               # %cond.end.i
	leaq	(%r9,%rbx), %rax
	movq	$-2, %rcx
	subq	%r15, %rcx
	cmpq	$-2, %rcx
	jg	.LBB1_98
# BB#97:                                # %cond.end.i
	movq	$-1, %rcx
.LBB1_98:                               # %cond.end.i
	leaq	3(%r15,%rcx), %r10
	movq	%r10, %rcx
	andq	$-4, %rcx
	movq	(%rdi), %r8
	xorq	%rsi, %rsi
	movq	%r10, %rdx
	andq	$-4, %rdx
	movq	%rax, 8(%rdi)
	movb	%sil, (%r8,%rax)
	je	.LBB1_104
# BB#99:                                # %vector.body.preheader
	movq	$-2, %rax
	subq	%r15, %rax
	cmpq	$-2, %rax
	jg	.LBB1_101
# BB#100:                               # %vector.body.preheader
	movq	$-1, %rax
.LBB1_101:                              # %vector.body.preheader
	subq	%rcx, %rbx
	leaq	3(%r15,%rax), %rdi
	andq	$-4, %rdi
	addq	%r9, %r15
	leaq	-2(%r8,%r15), %rax
	movq	$32, %rcx
	.align	16, 0x90
.LBB1_102:                              # %vector.body
                                        # =>This Inner Loop Header: Depth=1
	movb	%cl, 3(%rax)
	movb	%cl, 2(%rax)
	movb	%cl, 1(%rax)
	movb	%cl, (%rax)
	addq	$-4, %rax
	addq	$-4, %rdi
	jne	.LBB1_102
# BB#103:
	movq	%rdx, %rsi
.LBB1_104:                              # %middle.block
	cmpq	%rsi, %r10
	je	.LBB1_107
# BB#105:                               # %while.body.i.preheader
	leaq	-1(%r9,%r8), %rax
	movq	$32, %rcx
	.align	16, 0x90
.LBB1_106:                              # %while.body.i
                                        # =>This Inner Loop Header: Depth=1
	movb	%cl, (%rax,%rbx)
	leaq	-1(%rbx), %rbx
	testq	%rbx, %rbx
	jg	.LBB1_106
	jmp	.LBB1_107
.LBB1_89:
	movq	%r14, 256(%rsp)         # 8-byte Spill
	movq	264(%rsp), %r13         # 8-byte Reload
.LBB1_107:                              # %if.end331
	movq	624(%rsp), %rax
	movq	(%rax), %r12
	leaq	1(%r12), %rbx
	movq	%rbx, (%rax)
	movq	648(%rsp), %rax
	movq	(%rax), %rax
	movq	(%rax), %r15
	movq	8(%rax), %r14
	movabsq	$.L.str3, %rdi
	xorq	%rsi, %rsi
	callq	Perl_newSVpvn
	testq	%r12, %r12
	movq	%rax, %r12
	js	.LBB1_114
# BB#108:                               # %if.then2.i25
	movq	(%r12), %rax
	movq	8(%rax), %rcx
	movq	16(%rax), %rax
	movq	%r14, %rdx
	imulq	%rbx, %rdx
	leaq	1(%rdx,%rcx), %rsi
	cmpq	%rsi, %rax
	jae	.LBB1_110
# BB#109:                               # %cond.true.i27
	movq	%r12, %rdi
	callq	Perl_sv_grow
.LBB1_110:                              # %cond.end.i28
	cmpq	$1, %r14
	jne	.LBB1_113
# BB#111:                               # %while.body.lr.ph.i
	movq	(%r12), %rcx
	movq	8(%rcx), %rdx
	movq	(%rcx), %rax
	leaq	(%rdx,%rbx), %rsi
	movq	%rsi, 8(%rcx)
	xorq	%rcx, %rcx
	movb	%cl, (%rax,%rsi)
	addq	%rdx, %rax
	.align	16, 0x90
.LBB1_112:                              # %while.body.i38
                                        # =>This Inner Loop Header: Depth=1
	movzbl	(%r15), %ecx
	movb	%cl, -1(%rax,%rbx)
	leaq	-1(%rbx), %rbx
	testq	%rbx, %rbx
	jg	.LBB1_112
	jmp	.LBB1_114
	.align	16, 0x90
.LBB1_113:                              # %while.body28.i
                                        # =>This Inner Loop Header: Depth=1
	movq	$2, %rcx
	movq	%r12, %rdi
	movq	%r15, %rsi
	movq	%r14, %rdx
	callq	Perl_sv_catpvn_flags
	decq	%rbx
	testq	%rbx, %rbx
	jg	.LBB1_113
.LBB1_114:                              # %sv_x.exit39
	movq	%r12, 208(%rsp)         # 8-byte Spill
	movq	184(%rsp), %rsi         # 8-byte Reload
	cmpq	$8, %rsi
	ja	.LBB1_120
# BB#115:                               # %if.then340
	movabsq	$.L.str47, %rdi
	movq	$2, %rsi
	callq	Perl_newSVpvn
	movq	%rax, %rbx
	movq	$2, %rcx
	movq	%rbx, %rdi
	movq	176(%rsp), %rsi         # 8-byte Reload
	movq	%r13, %r12
	movq	%r12, %rdx
	callq	Perl_sv_catpvn_flags
	movabsq	$.L.str38, %rsi
	movq	$1, %rdx
	movq	$2, %rcx
	movq	%rbx, %rdi
	callq	Perl_sv_catpvn_flags
	cmpq	$0, 200(%rsp)           # 8-byte Folded Reload
	movq	240(%rsp), %r15         # 8-byte Reload
	movq	720(%rsp), %r14
	je	.LBB1_117
# BB#116:                               # %if.then345
	movabsq	$.L.str48, %rsi
	movq	$13, %rdx
	movq	$2, %rcx
	movq	%r15, %rdi
	callq	Perl_sv_catpvn_flags
	movq	(%rbx), %rax
	movq	(%rax), %rsi
	movq	8(%rax), %rdx
	movq	736(%rsp), %rax
	movq	%rax, 112(%rsp)
	movq	728(%rsp), %rax
	movq	%rax, 104(%rsp)
	movq	%r14, 96(%rsp)
	movq	712(%rsp), %rax
	movq	%rax, 88(%rsp)
	movq	704(%rsp), %rax
	movq	%rax, 80(%rsp)
	movq	696(%rsp), %rax
	movq	%rax, 72(%rsp)
	movq	688(%rsp), %rax
	movq	%rax, 64(%rsp)
	movq	680(%rsp), %rax
	movq	%rax, 56(%rsp)
	movq	672(%rsp), %rax
	movq	%rax, 48(%rsp)
	movq	664(%rsp), %rax
	movq	%rax, 40(%rsp)
	movq	224(%rsp), %rax         # 8-byte Reload
	movq	%rax, 32(%rsp)
	movq	648(%rsp), %rax
	movq	%rax, 24(%rsp)
	movq	640(%rsp), %rax
	movq	%rax, 16(%rsp)
	movq	632(%rsp), %rax
	movq	%rax, 8(%rsp)
	movq	624(%rsp), %rax
	movq	%rax, (%rsp)
	movq	216(%rsp), %rdi         # 8-byte Reload
	movq	%r15, %rcx
	movq	256(%rsp), %r14         # 8-byte Reload
	movq	%r14, %r8
	movq	248(%rsp), %r9          # 8-byte Reload
	callq	DD_dump
	movabsq	$.L.str49, %rsi
	movq	$2, %rdx
	movq	$2, %rcx
	movq	%r15, %rdi
	callq	Perl_sv_catpvn_flags
	jmp	.LBB1_118
.LBB1_120:                              # %if.else358
	leaq	-10(%rsi), %rax
	cmpq	$3, %rax
	movq	%r13, %r12
	movq	240(%rsp), %r15         # 8-byte Reload
	movq	720(%rsp), %r14
	jbe	.LBB1_121
# BB#287:                               # %if.else882
	movabsq	$.L.str64, %rdi
	xorq	%rax, %rax
	callq	Perl_warn
	movq	256(%rsp), %r14         # 8-byte Reload
	movq	232(%rsp), %r13         # 8-byte Reload
	jmp	.LBB1_288
.LBB1_121:                              # %if.else358
	movq	232(%rsp), %r13         # 8-byte Reload
	jmpq	*.LJTI1_0(,%rax,8)
.LBB1_123:                              # %if.then373
	movq	216(%rsp), %rdi         # 8-byte Reload
	callq	Perl_av_len
	movq	%rax, 152(%rsp)         # 8-byte Spill
	xorq	%rdi, %rdi
	movq	%r12, %rbx
	xorq	%r12, %r12
	callq	Perl_newSViv
	movq	%rax, 160(%rsp)         # 8-byte Spill
	leaq	28(%rbx), %rdi
	callq	Perl_safesysmalloc
	movq	%rax, %r13
	movq	%r13, %rdi
	movq	176(%rsp), %r14         # 8-byte Reload
	movq	%r14, %rsi
	callq	strcpy
	movzbl	(%r14), %eax
	cmpq	$64, %rax
	jne	.LBB1_125
# BB#124:                               # %if.end429.thread
	movabsq	$.L.str51, %rsi
	movq	$1, %rdx
	movq	$2, %rcx
	movq	%r15, %rdi
	callq	Perl_sv_catpvn_flags
	movq	$36, %rax
	movb	%al, (%r13)
	movq	%rbx, %r12
	movq	152(%rsp), %rbx         # 8-byte Reload
	jmp	.LBB1_139
.LBB1_59:                               # %if.else221
	movq	%rbx, 200(%rsp)         # 8-byte Spill
	cmpq	$12, 184(%rsp)          # 8-byte Folded Reload
	jne	.LBB1_62
# BB#60:                                # %if.else221
	cmpq	$42, %rax
	jne	.LBB1_62
# BB#61:                                # %if.then229
	movq	%r15, 240(%rsp)         # 8-byte Spill
	movabsq	$.L.str40, %rdi
	movq	$2, %rsi
	callq	Perl_newSVpvn
	movq	%rax, %r15
	movq	$2, %rcx
	movq	%r15, %rdi
	movq	%r13, %rsi
	movq	264(%rsp), %rdx         # 8-byte Reload
	callq	Perl_sv_catpvn_flags
	movq	(%r15), %rax
	movq	(%rax), %rax
	movq	$38, %rcx
	movb	%cl, 1(%rax)
	jmp	.LBB1_63
.LBB1_117:                              # %if.else351
	movabsq	$.L.str40, %rsi
	movq	$1, %rdx
	movq	$2, %rcx
	movq	%r15, %rdi
	callq	Perl_sv_catpvn_flags
	movq	(%rbx), %rax
	movq	(%rax), %rsi
	movq	8(%rax), %rdx
	movq	736(%rsp), %rax
	movq	%rax, 112(%rsp)
	movq	728(%rsp), %rax
	movq	%rax, 104(%rsp)
	movq	%r14, 96(%rsp)
	movq	712(%rsp), %rax
	movq	%rax, 88(%rsp)
	movq	704(%rsp), %rax
	movq	%rax, 80(%rsp)
	movq	696(%rsp), %rax
	movq	%rax, 72(%rsp)
	movq	688(%rsp), %rax
	movq	%rax, 64(%rsp)
	movq	680(%rsp), %rax
	movq	%rax, 56(%rsp)
	movq	672(%rsp), %rax
	movq	%rax, 48(%rsp)
	movq	664(%rsp), %rax
	movq	%rax, 40(%rsp)
	movq	224(%rsp), %rax         # 8-byte Reload
	movq	%rax, 32(%rsp)
	movq	648(%rsp), %rax
	movq	%rax, 24(%rsp)
	movq	640(%rsp), %rax
	movq	%rax, 16(%rsp)
	movq	632(%rsp), %rax
	movq	%rax, 8(%rsp)
	movq	624(%rsp), %rax
	movq	%rax, (%rsp)
	movq	216(%rsp), %rdi         # 8-byte Reload
	movq	%r15, %rcx
	movq	256(%rsp), %r14         # 8-byte Reload
	movq	%r14, %r8
	movq	248(%rsp), %r9          # 8-byte Reload
	callq	DD_dump
.LBB1_118:                              # %if.end357
	movq	232(%rsp), %r13         # 8-byte Reload
.LBB1_119:                              # %if.end357
	movq	%rbx, %rdi
	callq	Perl_sv_free
.LBB1_288:                              # %if.end887
	movq	200(%rsp), %rbx         # 8-byte Reload
.LBB1_289:                              # %if.end887
	testq	%rbx, %rbx
	je	.LBB1_296
# BB#290:                               # %if.then889
	movq	632(%rsp), %rax
	cmpq	$2, %rax
	jl	.LBB1_292
# BB#291:                               # %if.then892
	movq	224(%rsp), %rdi         # 8-byte Reload
	callq	Perl_sv_free
.LBB1_292:                              # %if.end893
	movabsq	$.L.str65, %rsi
	movq	$3, %rdx
	movq	$2, %rcx
	movq	%r15, %rdi
	callq	Perl_sv_catpvn_flags
	movq	%rbx, %rdi
	callq	strlen
	movq	$2, %rcx
	movq	%r15, %rdi
	movq	%rbx, %rsi
	movq	%rax, %rdx
	callq	Perl_sv_catpvn_flags
	movabsq	$.L.str66, %rsi
	movq	$3, %rdx
	movq	$2, %rcx
	movq	%r15, %rdi
	callq	Perl_sv_catpvn_flags
	movq	688(%rsp), %rax
	testq	%rax, %rax
	je	.LBB1_296
# BB#293:                               # %land.lhs.true896
	movq	688(%rsp), %rax
	movzbl	18(%rax), %eax
	testq	$4, %rax
	je	.LBB1_296
# BB#294:                               # %land.lhs.true900
	movq	688(%rsp), %rax
	movq	(%rax), %rax
	cmpq	$0, 8(%rax)
	je	.LBB1_296
# BB#295:                               # %if.then904
	movabsq	$.L.str60, %rsi
	movq	$2, %rdx
	movq	$2, %rcx
	movq	%r15, %rdi
	callq	Perl_sv_catpvn_flags
	movq	$2, %rdx
	movq	%r15, %rdi
	movq	688(%rsp), %rsi
	callq	Perl_sv_catsv_flags
	movabsq	$.L.str67, %rsi
	movq	$2, %rdx
	movq	$2, %rcx
	movq	%r15, %rdi
	callq	Perl_sv_catpvn_flags
.LBB1_296:                              # %if.end906
	movq	208(%rsp), %rdi         # 8-byte Reload
	callq	Perl_sv_free
	movq	624(%rsp), %rax
	decq	(%rax)
.LBB1_426:                              # %if.end1253
	testq	%r13, %r13
	je	.LBB1_432
# BB#427:                               # %if.then1255
	movq	704(%rsp), %rax
	testq	%rax, %rax
	je	.LBB1_429
# BB#428:                               # %if.then1257
	leaq	320(%rsp), %rsi
	movq	$2, %rcx
	movq	%r14, %rdi
	movq	%r13, %rdx
	callq	Perl_hv_delete
	jmp	.LBB1_432
.LBB1_429:                              # %if.else1260
	testq	%r12, %r12
	je	.LBB1_432
# BB#430:                               # %if.else1260
	cmpq	$0, 192(%rsp)           # 8-byte Folded Reload
	je	.LBB1_432
# BB#431:                               # %if.then1264
	movq	$2, %rsi
	movq	$1, %rdx
	movq	192(%rsp), %rdi         # 8-byte Reload
	callq	Perl_av_fetch
	movq	(%rax), %rdi
	movq	$1, %rsi
	callq	Perl_sv_setiv
	jmp	.LBB1_432
.LBB1_67:                               # %if.then256
	movq	224(%rsp), %rdi         # 8-byte Reload
	movzbl	18(%rdi), %eax
	testq	$4, %rax
	je	.LBB1_69
# BB#68:                                # %cond.true263
	movq	(%rdi), %rax
	movq	8(%rax), %rcx
	movq	%rcx, 312(%rsp)
	movq	(%rax), %r12
	jmp	.LBB1_70
.LBB1_81:                               # %cond.false302
	leaq	304(%rsp), %rsi
	movq	$2, %rdx
	callq	Perl_sv_2pv_flags
	movq	%rax, %rbx
.LBB1_82:                               # %cond.end304
	movabsq	$.L.str45, %r14
	movq	$1, %rdx
	movq	$2, %rcx
	movq	%r15, %rdi
	movq	%r14, %rsi
	callq	Perl_sv_catpvn_flags
	movq	304(%rsp), %rdx
	movq	$2, %rcx
	movq	%r15, %rdi
	movq	%rbx, %rsi
	callq	Perl_sv_catpvn_flags
	movq	$1, %rdx
	movq	$2, %rcx
	movq	%r15, %rdi
	movq	%r14, %rsi
	callq	Perl_sv_catpvn_flags
	jmp	.LBB1_432
.LBB1_93:
	movq	264(%rsp), %r13         # 8-byte Reload
	jmp	.LBB1_107
.LBB1_62:                               # %if.else234
	movq	%r15, 240(%rsp)         # 8-byte Spill
	movq	%r13, %rdi
	movq	264(%rsp), %rsi         # 8-byte Reload
	callq	Perl_newSVpvn
	movq	%rax, %r15
	jmp	.LBB1_63
.LBB1_55:                               # %if.else206
	movabsq	$.L.str39, %rdi
	leaq	320(%rsp), %rsi
	xorq	%rax, %rax
	callq	Perl_warn
	jmp	.LBB1_432
.LBB1_69:                               # %cond.false268
	leaq	312(%rsp), %rsi
	movq	$2, %rdx
	callq	Perl_sv_2pv_flags
	movq	%rax, %r12
.LBB1_70:                               # %cond.end
	movq	$47, %rsi
	movq	%r12, %rdi
	callq	strchr
	movq	%rax, %r14
	movabsq	$.L.str42, %rsi
	movq	$3, %rdx
	movq	$2, %rcx
	movq	%r15, %rdi
	callq	Perl_sv_catpvn_flags
	testq	%r14, %r14
	je	.LBB1_433
# BB#71:
	movabsq	$.L.str43, %r13
	.align	16, 0x90
.LBB1_72:                               # %while.body
                                        # =>This Inner Loop Header: Depth=1
	movq	%r14, %rbx
	subq	%r12, %rbx
	movq	$2, %rcx
	movq	%r15, %rdi
	movq	%r12, %rsi
	movq	%rbx, %rdx
	callq	Perl_sv_catpvn_flags
	movq	$2, %rdx
	movq	$2, %rcx
	movq	%r15, %rdi
	movq	%r13, %rsi
	callq	Perl_sv_catpvn_flags
	movq	312(%rsp), %rax
	notq	%rbx
	addq	%rax, %rbx
	movq	%rbx, 312(%rsp)
	movq	%r14, %r12
	incq	%r12
	movq	$47, %rsi
	movq	%r12, %rdi
	callq	strchr
	movq	%rax, %r14
	testq	%r14, %r14
	jne	.LBB1_72
	jmp	.LBB1_73
.LBB1_433:                              # %cond.end.while.end_crit_edge
	movq	312(%rsp), %rbx
.LBB1_73:                               # %while.end
	movq	$2, %rcx
	movq	%r15, %rdi
	movq	%r12, %rsi
	movq	%rbx, %rdx
	callq	Perl_sv_catpvn_flags
	movabsq	$.L.str44, %rsi
.LBB1_74:                               # %cleanup
	movq	$1, %rdx
.LBB1_75:                               # %cleanup
	movq	$2, %rcx
	movq	%r15, %rdi
	callq	Perl_sv_catpvn_flags
.LBB1_432:                              # %cleanup
	addq	$576, %rsp              # imm = 0x240
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB1_161:                              # %if.then514
	movq	176(%rsp), %rbx         # 8-byte Reload
	movq	%rbx, %rdi
	movq	%r12, %rsi
	callq	Perl_newSVpvn
	movzbl	(%rbx), %ecx
	movq	%rax, %rbx
	cmpq	$37, %rcx
	jne	.LBB1_163
# BB#162:                               # %if.then532
	movabsq	$.L.str51, %rsi
	movq	$1, %rdx
	movq	$2, %rcx
	movq	%r15, %rdi
	callq	Perl_sv_catpvn_flags
	movq	(%rbx), %rax
	movq	(%rax), %rax
	movq	$36, %rcx
	movb	%cl, (%rax)
	movq	256(%rsp), %r14         # 8-byte Reload
	jmp	.LBB1_170
.LBB1_285:                              # %if.then878
	movabsq	$.L.str62, %rsi
	movq	$15, %rdx
	movq	$2, %rcx
	movq	%r15, %rdi
	callq	Perl_sv_catpvn_flags
	movq	696(%rsp), %rax
	testq	%rax, %rax
	movq	200(%rsp), %rbx         # 8-byte Reload
	movq	256(%rsp), %r14         # 8-byte Reload
	je	.LBB1_289
# BB#286:                               # %if.then880
	movabsq	$.L.str63, %rdi
	xorq	%rax, %rax
	callq	Perl_warn
	jmp	.LBB1_289
.LBB1_122:                              # %if.then361
	movabsq	$.L.str50, %rdi
	movq	$2, %rsi
	callq	Perl_newSVpvn
	movq	%rax, %rbx
	movq	$2, %rcx
	movq	%rbx, %rdi
	movq	176(%rsp), %rsi         # 8-byte Reload
	movq	%r12, %rdx
	callq	Perl_sv_catpvn_flags
	movabsq	$.L.str38, %rsi
	movq	$1, %rdx
	movq	$2, %rcx
	movq	%rbx, %rdi
	callq	Perl_sv_catpvn_flags
	movabsq	$.L.str40, %rsi
	movq	$1, %rdx
	movq	$2, %rcx
	movq	%r15, %rdi
	callq	Perl_sv_catpvn_flags
	movq	(%rbx), %rax
	movq	(%rax), %rsi
	movq	8(%rax), %rdx
	movq	736(%rsp), %rax
	movq	%rax, 112(%rsp)
	movq	728(%rsp), %rax
	movq	%rax, 104(%rsp)
	movq	%r14, 96(%rsp)
	movq	712(%rsp), %rax
	movq	%rax, 88(%rsp)
	movq	704(%rsp), %rax
	movq	%rax, 80(%rsp)
	movq	696(%rsp), %rax
	movq	%rax, 72(%rsp)
	movq	688(%rsp), %rax
	movq	%rax, 64(%rsp)
	movq	680(%rsp), %rax
	movq	%rax, 56(%rsp)
	movq	672(%rsp), %rax
	movq	%rax, 48(%rsp)
	movq	664(%rsp), %rax
	movq	%rax, 40(%rsp)
	movq	224(%rsp), %rax         # 8-byte Reload
	movq	%rax, 32(%rsp)
	movq	648(%rsp), %rax
	movq	%rax, 24(%rsp)
	movq	640(%rsp), %rax
	movq	%rax, 16(%rsp)
	movq	632(%rsp), %rax
	movq	%rax, 8(%rsp)
	movq	624(%rsp), %rax
	movq	%rax, (%rsp)
	movq	216(%rsp), %rdi         # 8-byte Reload
	movq	%r15, %rcx
	movq	256(%rsp), %r14         # 8-byte Reload
	movq	%r14, %r8
	movq	248(%rsp), %r9          # 8-byte Reload
	callq	DD_dump
	jmp	.LBB1_119
.LBB1_48:                               # %if.else171
	movzbl	(%r13), %eax
	cmpq	$37, %rax
	je	.LBB1_50
# BB#49:                                # %if.else171
	cmpq	$64, %rax
	jne	.LBB1_54
.LBB1_50:                               # %if.then180
	movq	(%r14), %rcx
	movq	(%rcx), %rsi
	movzbl	(%rsi), %edx
	cmpq	$92, %rdx
	jne	.LBB1_53
# BB#51:                                # %land.lhs.true187
	movzbl	1(%rsi), %edx
	cmpq	%rax, %rdx
	jne	.LBB1_53
# BB#52:                                # %if.then196
	movq	8(%rcx), %rdx
	incq	%rsi
	decq	%rdx
	jmp	.LBB1_75
.LBB1_125:                              # %if.else389
	movabsq	$.L.str52, %rsi
	movq	$1, %rdx
	movq	$2, %rcx
	movq	%r15, %rdi
	callq	Perl_sv_catpvn_flags
	testq	%rbx, %rbx
	je	.LBB1_132
# BB#126:                               # %land.lhs.true392
	movzbl	-1(%rbx,%r14), %eax
	orq	$32, %rax
	andq	$255, %rax
	cmpq	$125, %rax
	jne	.LBB1_131
# BB#127:                               # %lor.lhs.false404
	cmpq	$5, %rbx
	movq	%rbx, %r12
	jb	.LBB1_132
# BB#128:                               # %land.lhs.true407
	movzbl	1(%r14), %eax
	cmpq	$123, %rax
	je	.LBB1_131
# BB#129:                               # %lor.lhs.false412
	movzbl	(%r14), %eax
	cmpq	$92, %rax
	movq	%rbx, %r12
	jne	.LBB1_132
# BB#130:                               # %land.lhs.true417
	movzbl	2(%r14), %eax
	cmpq	$123, %rax
	movq	%rbx, %r12
	jne	.LBB1_132
.LBB1_131:                              # %if.then422
	movq	$15917, %rax            # imm = 0x3E2D
	movw	%ax, (%r13,%rbx)
	leaq	2(%rbx), %r12
	xorq	%rax, %rax
	movb	%al, 2(%r13,%rbx)
.LBB1_132:                              # %if.end429
	movzbl	(%r13), %eax
	cmpq	$42, %rax
	movq	152(%rsp), %rbx         # 8-byte Reload
	jne	.LBB1_139
# BB#133:                               # %land.lhs.true434
	movzbl	-1(%r12,%r13), %eax
	cmpq	$125, %rax
	jne	.LBB1_139
# BB#134:                               # %land.lhs.true434
	cmpq	$8, %r12
	jb	.LBB1_139
# BB#135:                               # %land.lhs.true443
	leaq	-8(%r12,%r13), %rdi
	movabsq	$.L.str53, %rsi
	callq	Perl_instr
	testq	%rax, %rax
	jne	.LBB1_138
# BB#136:                               # %lor.lhs.false448
	leaq	-7(%r12,%r13), %rdi
	movabsq	$.L.str54, %rsi
	callq	Perl_instr
	testq	%rax, %rax
	jne	.LBB1_138
# BB#137:                               # %lor.lhs.false453
	leaq	-6(%r12,%r13), %rdi
	movabsq	$.L.str55, %rsi
	callq	Perl_instr
	testq	%rax, %rax
	je	.LBB1_139
.LBB1_138:                              # %if.then458
	movq	$15917, %rax            # imm = 0x3E2D
	movw	%ax, (%r13,%r12)
	addq	$2, %r12
.LBB1_139:                              # %if.end463
	movq	$91, %rax
	movw	%ax, (%r13,%r12)
	movq	664(%rsp), %rdi
	callq	Perl_newSVsv
	movq	%rax, %r14
	movq	%r14, 168(%rsp)         # 8-byte Spill
	movq	$2, %rdx
	movq	%r14, %rdi
	movq	640(%rsp), %rsi
	callq	Perl_sv_catsv_flags
	movq	$2, %rdx
	movq	%r14, %rdi
	movq	224(%rsp), %rsi         # 8-byte Reload
	callq	Perl_sv_catsv_flags
	testq	%rbx, %rbx
	js	.LBB1_157
# BB#140:                               # %for.body.lr.ph
	movq	%r15, %r14
	leaq	1(%r13,%r12), %rax
	movq	%rax, 144(%rsp)         # 8-byte Spill
	movq	%r13, 184(%rsp)         # 8-byte Spill
	leaq	1(%rbx), %rax
	movq	%rax, 136(%rsp)         # 8-byte Spill
	xorq	%r13, %r13
	.align	16, 0x90
.LBB1_141:                              # %for.body
                                        # =>This Inner Loop Header: Depth=1
	xorq	%rdx, %rdx
	movq	216(%rsp), %rdi         # 8-byte Reload
	movq	%r13, %rsi
	callq	Perl_av_fetch
	testq	%rax, %rax
	movabsq	$PL_sv_undef, %r15
	je	.LBB1_143
# BB#142:                               # %if.then474
                                        #   in Loop: Header=BB1_141 Depth=1
	movq	(%rax), %r15
.LBB1_143:                              # %if.end476
                                        #   in Loop: Header=BB1_141 Depth=1
	movq	160(%rsp), %rdi         # 8-byte Reload
	movq	%r13, %rsi
	callq	Perl_sv_setiv
	xorq	%rax, %rax
	movq	144(%rsp), %rdi         # 8-byte Reload
	movabsq	$.L.str24, %rsi
	movq	%r13, %rdx
	callq	sprintf
	movq	184(%rsp), %rbx         # 8-byte Reload
	movq	%rbx, %rdi
	callq	strlen
	movq	632(%rsp), %rcx
	cmpq	$3, %rcx
	movq	$93, %rcx
	movw	%cx, (%rbx,%rax)
	leaq	1(%rax), %r12
	jl	.LBB1_145
# BB#144:                               # %if.then485
                                        #   in Loop: Header=BB1_141 Depth=1
	movq	$2, %rdx
	movq	%r14, %rbx
	movq	%rbx, %rdi
	movq	168(%rsp), %rsi         # 8-byte Reload
	callq	Perl_sv_catsv_flags
	movq	$2, %rdx
	movq	%rbx, %rdi
	movq	208(%rsp), %rsi         # 8-byte Reload
	callq	Perl_sv_catsv_flags
	movq	$1, %rdx
	movq	$2, %rcx
	movq	%rbx, %rdi
	movabsq	$.L.str56, %rsi
	callq	Perl_sv_catpvn_flags
	movq	$2, %rdx
	movq	%rbx, %rdi
	movq	%rbx, %r14
	movq	160(%rsp), %rsi         # 8-byte Reload
	callq	Perl_sv_catsv_flags
.LBB1_145:                              # %if.end486
                                        #   in Loop: Header=BB1_141 Depth=1
	movq	$2, %rdx
	movq	%r14, %rbx
	movq	%rbx, %rdi
	movq	168(%rsp), %rsi         # 8-byte Reload
	callq	Perl_sv_catsv_flags
	movq	$2, %rdx
	movq	%rbx, %rdi
	movq	208(%rsp), %rsi         # 8-byte Reload
	callq	Perl_sv_catsv_flags
	movq	736(%rsp), %rax
	movq	%rax, 112(%rsp)
	movq	728(%rsp), %rax
	movq	%rax, 104(%rsp)
	movq	720(%rsp), %rax
	movq	%rax, 96(%rsp)
	movq	712(%rsp), %rax
	movq	%rax, 88(%rsp)
	movq	704(%rsp), %rax
	movq	%rax, 80(%rsp)
	movq	696(%rsp), %rax
	movq	%rax, 72(%rsp)
	movq	688(%rsp), %rax
	movq	%rax, 64(%rsp)
	movq	680(%rsp), %rax
	movq	%rax, 56(%rsp)
	movq	672(%rsp), %rax
	movq	%rax, 48(%rsp)
	movq	664(%rsp), %rax
	movq	%rax, 40(%rsp)
	movq	224(%rsp), %rax         # 8-byte Reload
	movq	%rax, 32(%rsp)
	movq	648(%rsp), %rax
	movq	%rax, 24(%rsp)
	movq	640(%rsp), %rax
	movq	%rax, 16(%rsp)
	movq	632(%rsp), %rax
	movq	%rax, 8(%rsp)
	movq	624(%rsp), %rax
	movq	%rax, (%rsp)
	movq	%r15, %rdi
	movq	184(%rsp), %rsi         # 8-byte Reload
	movq	%r12, %rdx
	movq	%rbx, %rcx
	movq	%rbx, %r14
	movq	256(%rsp), %r8          # 8-byte Reload
	movq	248(%rsp), %r9          # 8-byte Reload
	callq	DD_dump
	movq	152(%rsp), %rbx         # 8-byte Reload
	cmpq	%rbx, %r13
	jge	.LBB1_147
# BB#146:                               # %if.then490
                                        #   in Loop: Header=BB1_141 Depth=1
	movq	$1, %rdx
	movq	$2, %rcx
	movq	%r14, %rdi
	movabsq	$.L.str57, %rsi
	callq	Perl_sv_catpvn_flags
.LBB1_147:                              # %for.inc
                                        #   in Loop: Header=BB1_141 Depth=1
	incq	%r13
	cmpq	%r13, 136(%rsp)         # 8-byte Folded Reload
	jne	.LBB1_141
# BB#148:                               # %for.end
	testq	%rbx, %rbx
	movq	%r14, %r15
	movq	184(%rsp), %r13         # 8-byte Reload
	js	.LBB1_157
# BB#149:                               # %if.then495
	movq	%r15, %r14
	movq	648(%rsp), %rax
	movq	(%rax), %rax
	movq	(%rax), %r15
	movq	624(%rsp), %rcx
	movq	(%rcx), %rbx
	movq	8(%rax), %r13
	decq	%rbx
	movabsq	$.L.str3, %rdi
	xorq	%rsi, %rsi
	callq	Perl_newSVpvn
	movq	%rax, %r12
	testq	%rbx, %rbx
	jle	.LBB1_156
# BB#150:                               # %if.then2.i103
	movq	(%r12), %rax
	movq	8(%rax), %rcx
	movq	16(%rax), %rax
	movq	%rbx, %rdx
	imulq	%r13, %rdx
	leaq	1(%rdx,%rcx), %rsi
	cmpq	%rsi, %rax
	jae	.LBB1_152
# BB#151:                               # %cond.true.i105
	movq	%r12, %rdi
	callq	Perl_sv_grow
.LBB1_152:                              # %cond.end.i107
	cmpq	$1, %r13
	jne	.LBB1_155
# BB#153:                               # %while.body.lr.ph.i112
	movq	(%r12), %rcx
	movq	8(%rcx), %rdx
	movq	(%rcx), %rax
	leaq	(%rdx,%rbx), %rsi
	movq	%rsi, 8(%rcx)
	xorq	%rcx, %rcx
	movb	%cl, (%rax,%rsi)
	addq	%rdx, %rax
.LBB1_154:                              # %while.body.i118
                                        # =>This Inner Loop Header: Depth=1
	movzbl	(%r15), %ecx
	movb	%cl, -1(%rax,%rbx)
	leaq	-1(%rbx), %rbx
	testq	%rbx, %rbx
	jg	.LBB1_154
	jmp	.LBB1_156
.LBB1_155:                              # %while.body28.i122
                                        # =>This Inner Loop Header: Depth=1
	movq	$2, %rcx
	movq	%r12, %rdi
	movq	%r15, %rsi
	movq	%r13, %rdx
	callq	Perl_sv_catpvn_flags
	decq	%rbx
	testq	%rbx, %rbx
	jg	.LBB1_155
.LBB1_156:                              # %sv_x.exit123
	movq	$2, %rdx
	movq	%r14, %r15
	movq	%r15, %rdi
	movq	168(%rsp), %rsi         # 8-byte Reload
	callq	Perl_sv_catsv_flags
	movq	$2, %rdx
	movq	%r15, %rdi
	movq	%r12, %rsi
	callq	Perl_sv_catsv_flags
	movq	%r12, %rdi
	callq	Perl_sv_free
	movq	184(%rsp), %r13         # 8-byte Reload
.LBB1_157:                              # %if.end503
	movq	%r13, %r12
	movq	176(%rsp), %rax         # 8-byte Reload
	movzbl	(%rax), %eax
	cmpq	$64, %rax
	jne	.LBB1_159
# BB#158:                               # %if.then508
	movabsq	$.L.str58, %rsi
	jmp	.LBB1_160
.LBB1_163:                              # %if.else536
	movabsq	$.L.str37, %rsi
	movq	$1, %rdx
	movq	$2, %rcx
	movq	%r15, %rdi
	callq	Perl_sv_catpvn_flags
	testq	%r12, %r12
	movq	256(%rsp), %r14         # 8-byte Reload
	je	.LBB1_170
# BB#164:                               # %land.lhs.true539
	movq	176(%rsp), %rax         # 8-byte Reload
	movzbl	-1(%r12,%rax), %eax
	orq	$32, %rax
	andq	$255, %rax
	cmpq	$125, %rax
	jne	.LBB1_169
# BB#165:                               # %lor.lhs.false551
	cmpq	$5, %r12
	jb	.LBB1_170
# BB#166:                               # %land.lhs.true554
	movq	176(%rsp), %rax         # 8-byte Reload
	movzbl	1(%rax), %eax
	cmpq	$123, %rax
	je	.LBB1_169
# BB#167:                               # %lor.lhs.false559
	movq	176(%rsp), %rax         # 8-byte Reload
	movzbl	(%rax), %eax
	cmpq	$92, %rax
	jne	.LBB1_171
# BB#168:                               # %land.lhs.true564
	movq	176(%rsp), %rax         # 8-byte Reload
	movzbl	2(%rax), %eax
	cmpq	$123, %rax
	jne	.LBB1_170
.LBB1_169:                              # %if.then569
	movabsq	$.L.str60, %rsi
	movq	$2, %rdx
	movq	$2, %rcx
	movq	%rbx, %rdi
	callq	Perl_sv_catpvn_flags
.LBB1_170:                              # %if.end571thread-pre-split
	movq	176(%rsp), %rax         # 8-byte Reload
	movzbl	(%rax), %eax
.LBB1_171:                              # %if.end571
	andq	$255, %rax
	cmpq	$42, %rax
	jne	.LBB1_178
# BB#172:                               # %land.lhs.true576
	movq	176(%rsp), %rax         # 8-byte Reload
	movzbl	-1(%r12,%rax), %eax
	cmpq	$125, %rax
	jne	.LBB1_178
# BB#173:                               # %land.lhs.true576
	cmpq	$8, %r12
	jb	.LBB1_178
# BB#174:                               # %land.lhs.true585
	movq	176(%rsp), %rax         # 8-byte Reload
	leaq	-8(%r12,%rax), %rdi
	movabsq	$.L.str53, %rsi
	callq	Perl_instr
	testq	%rax, %rax
	jne	.LBB1_177
# BB#175:                               # %lor.lhs.false590
	movq	176(%rsp), %rax         # 8-byte Reload
	leaq	-7(%r12,%rax), %rdi
	movabsq	$.L.str54, %rsi
	callq	Perl_instr
	testq	%rax, %rax
	jne	.LBB1_177
# BB#176:                               # %lor.lhs.false595
	movq	176(%rsp), %rax         # 8-byte Reload
	leaq	-6(%r12,%rax), %rdi
	movabsq	$.L.str55, %rsi
	callq	Perl_instr
	testq	%rax, %rax
	je	.LBB1_178
.LBB1_177:                              # %if.then600
	movabsq	$.L.str60, %rsi
	movq	$2, %rdx
	movq	$2, %rcx
	movq	%rbx, %rdi
	callq	Perl_sv_catpvn_flags
.LBB1_178:                              # %if.end601
	movabsq	$.L.str37, %rsi
	movq	$1, %rdx
	movq	$2, %rcx
	movq	%rbx, %rdi
	callq	Perl_sv_catpvn_flags
	movq	664(%rsp), %rdi
	callq	Perl_newSVsv
	movq	%rbx, %r13
	movq	%rax, %rbx
	movq	%rbx, 144(%rsp)         # 8-byte Spill
	movq	$2, %rdx
	movq	%rbx, %rdi
	movq	640(%rsp), %rsi
	callq	Perl_sv_catsv_flags
	movq	$2, %rdx
	movq	%rbx, %rdi
	movq	224(%rsp), %rsi         # 8-byte Reload
	callq	Perl_sv_catsv_flags
	movq	736(%rsp), %rcx
	testq	%rcx, %rcx
	je	.LBB1_202
# BB#179:                               # %if.then604
	movq	%r13, 136(%rsp)         # 8-byte Spill
	movabsq	$PL_sv_yes, %rax
	cmpq	%rax, %rcx
	je	.LBB1_180
# BB#186:                               # %if.then629
	movq	PL_stack_sp(%rip), %rbx
	callq	Perl_push_scope
	movabsq	$PL_tmps_floor, %rdi
	callq	Perl_save_int
	movq	PL_tmps_ix(%rip), %rcx
	movq	PL_markstack_ptr(%rip), %rax
	movq	PL_markstack_max(%rip), %rdx
	movq	%rcx, PL_tmps_floor(%rip)
	addq	$8, %rax
	movq	%rax, PL_markstack_ptr(%rip)
	cmpq	%rdx, %rax
	jne	.LBB1_188
# BB#187:                               # %if.then635
	callq	Perl_markstack_grow
	movq	PL_markstack_ptr(%rip), %rax
.LBB1_188:                              # %if.end636
	movq	%r12, %r13
	movq	PL_stack_base(%rip), %rcx
	movq	%rbx, %rdx
	subq	%rcx, %rdx
	movq	$3, %rcx
	sarq	%cl, %rdx
	movq	%rdx, (%rax)
	movq	PL_stack_max(%rip), %rax
	subq	%rbx, %rax
	movq	%r14, %r12
	cmpq	$7, %rax
	jg	.LBB1_190
# BB#189:                               # %if.then648
	movq	$1, %rdx
	movq	%rbx, %rdi
	movq	%rbx, %rsi
	callq	Perl_stack_grow
	movq	%rax, %rbx
.LBB1_190:                              # %if.end650
	movq	216(%rsp), %rdi         # 8-byte Reload
	callq	Perl_newRV
	movq	%rax, %rdi
	callq	Perl_sv_2mortal
	movq	%rax, 8(%rbx)
	addq	$8, %rbx
	movq	%rbx, PL_stack_sp(%rip)
	movq	$4, %rsi
	movq	736(%rsp), %rdi
	callq	Perl_call_sv
	movq	PL_stack_sp(%rip), %r14
	testq	%rax, %rax
	je	.LBB1_196
# BB#191:                               # %if.then657
	movq	(%r14), %rax
	movzbl	18(%rax), %ecx
	addq	$-8, %r14
	testq	$8, %rcx
	je	.LBB1_196
# BB#192:                               # %land.lhs.true662
	movq	(%rax), %rax
	movq	(%rax), %rbx
	movzbl	16(%rbx), %eax
	cmpq	$10, %rax
	jne	.LBB1_196
# BB#193:                               # %if.then669
	movq	%rbx, PL_Sv(%rip)
	testq	%rbx, %rbx
	je	.LBB1_195
# BB#194:                               # %land.rhs673
	incq	8(%rbx)
.LBB1_195:                              # %if.end680
	testq	%rbx, %rbx
	jne	.LBB1_197
.LBB1_196:                              # %if.then682
	movabsq	$.L.str61, %rdi
	xorq	%rax, %rax
	xorq	%rbx, %rbx
	callq	Perl_warn
.LBB1_197:                              # %if.end683
	movq	PL_tmps_ix(%rip), %rax
	movq	PL_tmps_floor(%rip), %rcx
	movq	%r14, PL_stack_sp(%rip)
	cmpq	%rcx, %rax
	jle	.LBB1_199
# BB#198:                               # %if.then686
	callq	Perl_free_tmps
.LBB1_199:                              # %if.end687
	callq	Perl_pop_scope
	jmp	.LBB1_200
.LBB1_159:                              # %if.else509
	movabsq	$.L.str59, %rsi
.LBB1_160:                              # %if.end510
	movq	$1, %rdx
	movq	$2, %rcx
	movq	%r15, %rdi
	callq	Perl_sv_catpvn_flags
	movq	232(%rsp), %r13         # 8-byte Reload
	movq	200(%rsp), %rbx         # 8-byte Reload
	movq	256(%rsp), %r14         # 8-byte Reload
	movq	160(%rsp), %rdi         # 8-byte Reload
	callq	Perl_sv_free
	movq	168(%rsp), %rdi         # 8-byte Reload
	callq	Perl_sv_free
	movq	%r12, %rdi
	callq	Perl_safesysfree
	movq	264(%rsp), %r12         # 8-byte Reload
	jmp	.LBB1_289
.LBB1_202:                              # %if.else693
	movq	%r13, 136(%rsp)         # 8-byte Spill
	movq	216(%rsp), %r13         # 8-byte Reload
	movq	%r13, %rdi
	callq	Perl_hv_iterinit
	movq	$0, 152(%rsp)           # 8-byte Folded Spill
	jmp	.LBB1_203
.LBB1_53:                               # %if.else201
	movq	$1, %rdx
	movq	$2, %rcx
	movq	%r15, %rdi
	movq	%r13, %rsi
	callq	Perl_sv_catpvn_flags
	movabsq	$.L.str37, %rsi
	movq	$1, %rdx
	movq	$2, %rcx
	movq	%r15, %rdi
	callq	Perl_sv_catpvn_flags
	movq	$2, %rdx
	movq	%r15, %rdi
	movq	%r14, %rsi
	callq	Perl_sv_catsv_flags
	movabsq	$.L.str38, %rsi
	jmp	.LBB1_74
.LBB1_180:                              # %if.then607
	movq	%r12, %r13
	movq	%r14, %r12
	callq	Perl_newAV
	movq	%rax, %rbx
	movq	216(%rsp), %r14         # 8-byte Reload
	movq	%r14, %rdi
	callq	Perl_hv_iterinit
	movq	%r14, %rdi
	jmp	.LBB1_181
.LBB1_184:                              # %land.end620
                                        #   in Loop: Header=BB1_181 Depth=1
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	Perl_av_push
	movq	216(%rsp), %rdi         # 8-byte Reload
.LBB1_181:                              # %if.then607
                                        # =>This Inner Loop Header: Depth=1
	callq	Perl_hv_iternext
	testq	%rax, %rax
	je	.LBB1_185
# BB#182:                               # %while.body613
                                        #   in Loop: Header=BB1_181 Depth=1
	movq	%rax, %rdi
	callq	Perl_hv_iterkeysv
	testq	%rax, %rax
	movq	%rax, PL_Sv(%rip)
	je	.LBB1_184
# BB#183:                               # %land.rhs616
                                        #   in Loop: Header=BB1_181 Depth=1
	incq	8(%rax)
	jmp	.LBB1_184
.LBB1_185:                              # %if.end626
	movq	(%rbx), %rax
	movq	(%rax), %r14
	movq	%rbx, %rdi
	callq	Perl_av_len
	leaq	1(%rax), %rsi
	movabsq	$Perl_sv_cmp, %rdx
	movq	%r14, %rdi
	callq	Perl_sortsv
.LBB1_200:                              # %if.end688
	movq	%r12, %r14
	movq	$0, 152(%rsp)           # 8-byte Folded Spill
	testq	%rbx, %rbx
	movq	%r13, %r12
	movq	216(%rsp), %r13         # 8-byte Reload
	je	.LBB1_203
# BB#201:                               # %if.then690
	movq	%rbx, %rdi
	callq	Perl_sv_2mortal
	movq	%rbx, 152(%rsp)         # 8-byte Spill
.LBB1_203:                              # %if.end695
	movq	$0, 184(%rsp)           # 8-byte Folded Spill
	jmp	.LBB1_204
	.align	16, 0x90
.LBB1_271:                              # %for.inc854
                                        #   in Loop: Header=BB1_204 Depth=1
	incq	184(%rsp)               # 8-byte Folded Spill
	movq	264(%rsp), %r12         # 8-byte Reload
	movq	%r13, %r14
	movq	216(%rsp), %r13         # 8-byte Reload
.LBB1_204:                              # %for.body697
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB1_232 Depth 2
                                        #       Child Loop BB1_241 Depth 3
                                        #     Child Loop BB1_251 Depth 2
                                        #     Child Loop BB1_257 Depth 2
	movq	736(%rsp), %rax
	testq	%rax, %rax
	je	.LBB1_207
# BB#205:                               # %land.lhs.true706
                                        #   in Loop: Header=BB1_204 Depth=1
	cmpq	$0, 152(%rsp)           # 8-byte Folded Reload
	je	.LBB1_272
# BB#206:                               # %land.lhs.true708
                                        #   in Loop: Header=BB1_204 Depth=1
	movq	152(%rsp), %rdi         # 8-byte Reload
	callq	Perl_av_len
	cmpq	%rax, 184(%rsp)         # 8-byte Folded Reload
	jg	.LBB1_272
.LBB1_207:                              # %lor.lhs.false712
                                        #   in Loop: Header=BB1_204 Depth=1
	movq	%r13, %rdi
	callq	Perl_hv_iternext
	movq	%rax, %rbx
	testq	%rbx, %rbx
	je	.LBB1_272
# BB#208:                               # %if.end716
                                        #   in Loop: Header=BB1_204 Depth=1
	cmpq	$0, 184(%rsp)           # 8-byte Folded Reload
	je	.LBB1_210
# BB#209:                               # %if.then718
                                        #   in Loop: Header=BB1_204 Depth=1
	movq	$1, %rdx
	movq	$2, %rcx
	movq	%r15, %rdi
	movabsq	$.L.str57, %rsi
	callq	Perl_sv_catpvn_flags
.LBB1_210:                              # %if.end719
                                        #   in Loop: Header=BB1_204 Depth=1
	movq	736(%rsp), %rax
	testq	%rax, %rax
	je	.LBB1_222
# BB#211:                               # %if.then721
                                        #   in Loop: Header=BB1_204 Depth=1
	xorq	%rdx, %rdx
	movq	152(%rsp), %rdi         # 8-byte Reload
	movq	184(%rsp), %rsi         # 8-byte Reload
	callq	Perl_av_fetch
	testq	%rax, %rax
	jne	.LBB1_212
# BB#213:                               # %cond.false727
                                        #   in Loop: Header=BB1_204 Depth=1
	movabsq	$PL_sv_undef, %rdi
	callq	Perl_sv_mortalcopy
	movq	%rax, %r14
	jmp	.LBB1_214
.LBB1_222:                              # %if.else759
                                        #   in Loop: Header=BB1_204 Depth=1
	movq	%rbx, %rdi
	callq	Perl_hv_iterkeysv
	movq	%rax, %r14
	movq	%r13, %rdi
	movq	%rbx, %rsi
	callq	Perl_hv_iterval
	jmp	.LBB1_223
.LBB1_212:                              # %cond.true726
                                        #   in Loop: Header=BB1_204 Depth=1
	movq	(%rax), %r14
.LBB1_214:                              # %cond.end729
                                        #   in Loop: Header=BB1_204 Depth=1
	movzbl	18(%r14), %eax
	testq	$4, %rax
	jne	.LBB1_215
# BB#216:                               # %cond.false740
                                        #   in Loop: Header=BB1_204 Depth=1
	movq	$2, %rdx
	movq	%r14, %rdi
	leaq	288(%rsp), %rsi
	callq	Perl_sv_2pv_flags
	movq	288(%rsp), %rdx
	movq	%rax, %rsi
	jmp	.LBB1_217
.LBB1_215:                              # %cond.true735
                                        #   in Loop: Header=BB1_204 Depth=1
	movq	(%r14), %rax
	movq	8(%rax), %rdx
	movq	%rdx, 288(%rsp)
	movq	(%rax), %rsi
.LBB1_217:                              # %cond.end742
                                        #   in Loop: Header=BB1_204 Depth=1
	movq	16(%r14), %rax
	andq	$536870912, %rax        # imm = 0x20000000
	movq	$29, %rcx
	shrq	%cl, %rax
	testq	%rax, %rax
	je	.LBB1_219
# BB#218:                               #   in Loop: Header=BB1_204 Depth=1
	negq	%rdx
.LBB1_219:                              # %cond.end742
                                        #   in Loop: Header=BB1_204 Depth=1
	xorq	%rcx, %rcx
	movq	%r13, %rdi
	callq	Perl_hv_fetch
	testq	%rax, %rax
	jne	.LBB1_220
# BB#221:                               # %cond.false755
                                        #   in Loop: Header=BB1_204 Depth=1
	movabsq	$PL_sv_undef, %rdi
	callq	Perl_sv_mortalcopy
	jmp	.LBB1_223
.LBB1_220:                              # %cond.true754
                                        #   in Loop: Header=BB1_204 Depth=1
	movq	(%rax), %rax
.LBB1_223:                              # %if.end762
                                        #   in Loop: Header=BB1_204 Depth=1
	movq	%rax, 168(%rsp)         # 8-byte Spill
	movq	16(%r14), %rax
	xorq	%rcx, %rcx
	testq	$536870912, %rax        # imm = 0x20000000
	je	.LBB1_227
# BB#224:                               # %land.rhs766
                                        #   in Loop: Header=BB1_204 Depth=1
	movq	PL_curcop(%rip), %rax
	movzbl	37(%rax), %eax
	testq	$8, %rax
	movabsq	$0, %rax
	movq	%rax, %rcx
	jne	.LBB1_226
# BB#225:                               # %land.rhs766
                                        #   in Loop: Header=BB1_204 Depth=1
	movabsq	$1, %rax
	movq	%rax, %rcx
.LBB1_226:                              # %land.rhs766
                                        #   in Loop: Header=BB1_204 Depth=1
	movq	16(%r14), %rax
.LBB1_227:                              # %land.end770
                                        #   in Loop: Header=BB1_204 Depth=1
	movq	%rcx, 160(%rsp)         # 8-byte Spill
	testq	$262144, %rax           # imm = 0x40000
	jne	.LBB1_228
# BB#229:                               # %cond.false782
                                        #   in Loop: Header=BB1_204 Depth=1
	movq	$2, %rdx
	movq	%r14, %rdi
	leaq	288(%rsp), %rsi
	callq	Perl_sv_2pv_flags
	movq	288(%rsp), %r13
	movq	%rax, %r12
	jmp	.LBB1_230
.LBB1_228:                              # %cond.true777
                                        #   in Loop: Header=BB1_204 Depth=1
	movq	(%r14), %rax
	movq	8(%rax), %r13
	movq	%r13, 288(%rsp)
	movq	(%rax), %r12
.LBB1_230:                              # %cond.end784
                                        #   in Loop: Header=BB1_204 Depth=1
	movq	$2, %rdx
	movq	%r15, %rbx
	movq	%rbx, %rdi
	movq	144(%rsp), %rsi         # 8-byte Reload
	callq	Perl_sv_catsv_flags
	movq	$2, %rdx
	movq	%rbx, %rdi
	movq	208(%rsp), %rsi         # 8-byte Reload
	callq	Perl_sv_catsv_flags
	movq	712(%rsp), %rax
	testq	%rax, %rax
	jne	.LBB1_248
# BB#231:                               # %lor.lhs.false787
                                        #   in Loop: Header=BB1_204 Depth=1
	movzbl	(%r12), %edx
	movq	%r12, %rax
.LBB1_232:                              # %TOP.i
                                        #   Parent Loop BB1_204 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB1_241 Depth 3
	movq	%rdx, %rcx
	andq	$255, %rcx
	cmpq	$58, %rcx
	jne	.LBB1_236
# BB#233:                               # %if.then.i127
                                        #   in Loop: Header=BB1_232 Depth=2
	movzbl	1(%rax), %ecx
	testq	%rcx, %rcx
	je	.LBB1_248
# BB#234:                               # %if.then2.i129
                                        #   in Loop: Header=BB1_232 Depth=2
	cmpq	$58, %rcx
	jne	.LBB1_248
# BB#235:                               # %if.then2.if.end9_crit_edge.i
                                        #   in Loop: Header=BB1_232 Depth=2
	addq	$2, %rax
	movzbl	(%rax), %edx
.LBB1_236:                              # %if.end9.i
                                        #   in Loop: Header=BB1_232 Depth=2
	leaq	-65(%rdx), %rcx
	andq	$255, %rcx
	cmpq	$26, %rcx
	jb	.LBB1_239
# BB#237:                               # %lor.lhs.false.i
                                        #   in Loop: Header=BB1_232 Depth=2
	leaq	-97(%rdx), %rcx
	andq	$255, %rcx
	cmpq	$26, %rcx
	jb	.LBB1_239
# BB#238:                               # %lor.lhs.false.i
                                        #   in Loop: Header=BB1_232 Depth=2
	andq	$255, %rdx
	cmpq	$95, %rdx
	jne	.LBB1_248
.LBB1_239:                              # %while.cond.preheader.i
                                        #   in Loop: Header=BB1_232 Depth=2
	movzbl	1(%rax), %ecx
	testq	%rcx, %rcx
	je	.LBB1_245
# BB#240:                               #   in Loop: Header=BB1_232 Depth=2
	incq	%rax
	.align	16, 0x90
.LBB1_241:                              # %while.body.i130
                                        #   Parent Loop BB1_204 Depth=1
                                        #     Parent Loop BB1_232 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	leaq	-65(%rcx), %rdx
	andq	$255, %rdx
	cmpq	$26, %rdx
	jb	.LBB1_244
# BB#242:                               # %while.body.i130
                                        #   in Loop: Header=BB1_241 Depth=3
	leaq	-97(%rcx), %rdx
	andq	$255, %rdx
	cmpq	$26, %rdx
	jb	.LBB1_244
# BB#243:                               # %while.body.i130
                                        #   in Loop: Header=BB1_241 Depth=3
	leaq	-48(%rcx), %rdx
	andq	$255, %rdx
	cmpq	$9, %rdx
	jbe	.LBB1_244
# BB#246:                               # %lor.lhs.false53.i
                                        #   in Loop: Header=BB1_241 Depth=3
	andq	$255, %rcx
	cmpq	$95, %rcx
	jne	.LBB1_247
.LBB1_244:                              # %while.cond.backedge.i
                                        #   in Loop: Header=BB1_241 Depth=3
	movzbl	1(%rax), %ecx
	incq	%rax
	testq	%rcx, %rcx
	jne	.LBB1_241
	jmp	.LBB1_245
.LBB1_247:                              # %lor.lhs.false53.i
                                        #   in Loop: Header=BB1_232 Depth=2
	movq	$58, %rdx
	cmpq	$58, %rcx
	je	.LBB1_232
.LBB1_248:                              # %if.then790
                                        #   in Loop: Header=BB1_204 Depth=1
	cmpq	$0, 160(%rsp)           # 8-byte Folded Reload
	je	.LBB1_250
# BB#249:                               # %if.then792
                                        #   in Loop: Header=BB1_204 Depth=1
	movq	240(%rsp), %rbx         # 8-byte Reload
	movq	(%rbx), %rax
	movq	8(%rax), %rax
	movq	%rax, 160(%rsp)         # 8-byte Spill
	movq	%rbx, %rdi
	movq	%r12, %rsi
	movq	%r13, %rdx
	callq	esc_q_utf8
	movq	(%rbx), %rcx
	movq	(%rcx), %r14
	movq	%rax, %r15
	movq	160(%rsp), %rax         # 8-byte Reload
	addq	%rax, %r14
	movq	$0, 160(%rsp)           # 8-byte Folded Spill
	jmp	.LBB1_265
.LBB1_245:                              # %if.else821
                                        #   in Loop: Header=BB1_204 Depth=1
	movq	$2, %rcx
	movq	240(%rsp), %rbx         # 8-byte Reload
	movq	%rbx, %rdi
	movq	%r12, %rsi
	movq	%r13, %rdx
	callq	Perl_sv_catpvn_flags
	movq	$0, 160(%rsp)           # 8-byte Folded Spill
	movq	%r12, %r14
	jmp	.LBB1_264
.LBB1_250:                              # %if.else800
                                        #   in Loop: Header=BB1_204 Depth=1
	xorq	%rbx, %rbx
	testq	%r13, %r13
	movq	%r13, %rax
	movq	%r12, %rcx
	je	.LBB1_255
	.align	16, 0x90
.LBB1_251:                              # %while.body.i132
                                        #   Parent Loop BB1_204 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movzbl	(%rcx), %edx
	cmpq	$39, %rdx
	je	.LBB1_253
# BB#252:                               # %while.body.i132
                                        #   in Loop: Header=BB1_251 Depth=2
	cmpq	$92, %rdx
	jne	.LBB1_254
.LBB1_253:                              # %if.then.i134
                                        #   in Loop: Header=BB1_251 Depth=2
	incq	%rbx
.LBB1_254:                              # %if.end.i139
                                        #   in Loop: Header=BB1_251 Depth=2
	incq	%rcx
	decq	%rax
	jne	.LBB1_251
.LBB1_255:                              # %num_q.exit
                                        #   in Loop: Header=BB1_204 Depth=1
	leaq	3(%r13,%rbx), %rdi
	callq	Perl_safesysmalloc
	movq	%rax, %r14
	testq	%rbx, %rbx
	movq	$39, %rax
	movb	%al, (%r14)
	leaq	1(%r14), %rdi
	jne	.LBB1_256
# BB#262:                               # %if.else812
                                        #   in Loop: Header=BB1_204 Depth=1
	movq	%r12, %rsi
	movq	%r13, %rdx
	callq	memcpy
	movq	240(%rsp), %r15         # 8-byte Reload
	jmp	.LBB1_263
.LBB1_256:                              # %if.then808
                                        #   in Loop: Header=BB1_204 Depth=1
	xorq	%rax, %rax
	testq	%r13, %r13
	movq	%r13, %rcx
	movq	240(%rsp), %r15         # 8-byte Reload
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	movq	$92, %r8
	je	.LBB1_261
	.align	16, 0x90
.LBB1_257:                              # %while.body.i147
                                        #   Parent Loop BB1_204 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movsbq	(%r12), %rdx
	movq	%rdx, %rsi
	andq	%rbx, %rsi
	cmpq	$39, %rsi
	je	.LBB1_259
# BB#258:                               # %while.body.i147
                                        #   in Loop: Header=BB1_257 Depth=2
	cmpq	$92, %rsi
	jne	.LBB1_260
.LBB1_259:                              # %sw.bb.i151
                                        #   in Loop: Header=BB1_257 Depth=2
	movb	%r8b, (%rdi)
	movzbl	(%r12), %edx
	incq	%rdi
	incq	%rax
.LBB1_260:                              # %sw.default.i158
                                        #   in Loop: Header=BB1_257 Depth=2
	movb	%dl, (%rdi)
	incq	%rdi
	incq	%r12
	decq	%rcx
	jne	.LBB1_257
.LBB1_261:                              # %esc_q.exit160
                                        #   in Loop: Header=BB1_204 Depth=1
	addq	%rax, %r13
.LBB1_263:                              # %if.end815
                                        #   in Loop: Header=BB1_204 Depth=1
	movq	$39, %rax
	movw	%ax, 1(%r13,%r14)
	addq	$2, %r13
	movq	$2, %rcx
	movq	%r15, %rdi
	movq	%r14, %rsi
	movq	%r13, %rdx
	callq	Perl_sv_catpvn_flags
	movq	%r14, 160(%rsp)         # 8-byte Spill
	movq	%r15, %rbx
.LBB1_264:                              # %if.end822
                                        #   in Loop: Header=BB1_204 Depth=1
	movq	%r13, %r15
.LBB1_265:                              # %if.end822
                                        #   in Loop: Header=BB1_204 Depth=1
	movq	136(%rsp), %rdi         # 8-byte Reload
	callq	Perl_newSVsv
	movq	%rax, %r12
	movq	$2, %rcx
	movq	%r12, %rdi
	movq	%r14, %rsi
	movq	%r15, %rdx
	callq	Perl_sv_catpvn_flags
	movq	$1, %rdx
	movq	$2, %rcx
	movq	%r12, %rdi
	movabsq	$.L.str38, %rsi
	callq	Perl_sv_catpvn_flags
	movq	$2, %rdx
	movq	%rbx, %rdi
	movq	672(%rsp), %rsi
	callq	Perl_sv_catsv_flags
	movq	632(%rsp), %rax
	cmpq	$2, %rax
	movq	224(%rsp), %r14         # 8-byte Reload
	movq	%rbx, %r15
	jl	.LBB1_269
# BB#266:                               # %if.then826
                                        #   in Loop: Header=BB1_204 Depth=1
	movq	224(%rsp), %rdi         # 8-byte Reload
	callq	Perl_newSVsv
	movq	%rax, %r14
	leaq	4(%r13), %r15
	addq	$5, %r13
	movq	%r13, %rdi
	callq	Perl_safesysmalloc
	movq	%rax, %rbx
	xorq	%rdx, %rdx
	testq	%r15, %r15
	jle	.LBB1_268
# BB#267:                               # %while.body838.lr.ph
                                        #   in Loop: Header=BB1_204 Depth=1
	movq	$32, %rsi
	movq	%rbx, %rdi
	movq	%r15, %rdx
	callq	memset
	movq	%r15, %rdx
.LBB1_268:                              # %while.end841
                                        #   in Loop: Header=BB1_204 Depth=1
	xorq	%rax, %rax
	movb	%al, (%rbx,%rdx)
	movq	$2, %rcx
	movq	%r14, %rdi
	movq	%rbx, %rsi
	callq	Perl_sv_catpvn_flags
	movq	%rbx, %rdi
	callq	Perl_safesysfree
	movq	240(%rsp), %r15         # 8-byte Reload
.LBB1_269:                              # %if.end844
                                        #   in Loop: Header=BB1_204 Depth=1
	movq	(%r12), %rax
	movq	(%rax), %rsi
	movq	8(%rax), %rdx
	movq	736(%rsp), %r13
	movq	%r13, 112(%rsp)
	movq	728(%rsp), %rax
	movq	%rax, 104(%rsp)
	movq	720(%rsp), %rax
	movq	%rax, 96(%rsp)
	movq	712(%rsp), %rax
	movq	%rax, 88(%rsp)
	movq	704(%rsp), %rax
	movq	%rax, 80(%rsp)
	movq	696(%rsp), %rax
	movq	%rax, 72(%rsp)
	movq	688(%rsp), %rax
	movq	%rax, 64(%rsp)
	movq	680(%rsp), %rax
	movq	%rax, 56(%rsp)
	movq	672(%rsp), %rax
	movq	%rax, 48(%rsp)
	movq	664(%rsp), %rax
	movq	%rax, 40(%rsp)
	movq	%r14, 32(%rsp)
	movq	648(%rsp), %rax
	movq	%rax, 24(%rsp)
	movq	640(%rsp), %rax
	movq	%rax, 16(%rsp)
	movq	632(%rsp), %rax
	movq	%rax, %rbx
	movq	%rbx, 8(%rsp)
	movq	624(%rsp), %rax
	movq	%rax, (%rsp)
	movq	168(%rsp), %rdi         # 8-byte Reload
	movq	%r15, %rcx
	movq	256(%rsp), %r8          # 8-byte Reload
	movq	%r8, %r13
	movq	248(%rsp), %r9          # 8-byte Reload
	callq	DD_dump
	movq	%r12, %rdi
	callq	Perl_sv_free
	movq	160(%rsp), %rdi         # 8-byte Reload
	callq	Perl_safesysfree
	cmpq	$2, %rbx
	jl	.LBB1_271
# BB#270:                               # %if.then852
                                        #   in Loop: Header=BB1_204 Depth=1
	movq	%r14, %rdi
	callq	Perl_sv_free
	jmp	.LBB1_271
.LBB1_272:                              # %for.end856
	cmpq	$0, 184(%rsp)           # 8-byte Folded Reload
	je	.LBB1_281
# BB#273:                               # %if.then858
	movq	%r15, %r13
	movq	%r14, 256(%rsp)         # 8-byte Spill
	movq	%r12, 264(%rsp)         # 8-byte Spill
	movq	648(%rsp), %rax
	movq	(%rax), %rax
	movq	(%rax), %r15
	movq	624(%rsp), %rcx
	movq	(%rcx), %rbx
	movq	8(%rax), %r12
	decq	%rbx
	movabsq	$.L.str3, %rdi
	xorq	%rsi, %rsi
	callq	Perl_newSVpvn
	movq	%rax, %r14
	testq	%rbx, %rbx
	jle	.LBB1_280
# BB#274:                               # %if.then2.i208
	movq	(%r14), %rax
	movq	8(%rax), %rcx
	movq	16(%rax), %rax
	movq	%rbx, %rdx
	imulq	%r12, %rdx
	leaq	1(%rdx,%rcx), %rsi
	cmpq	%rsi, %rax
	jae	.LBB1_276
# BB#275:                               # %cond.true.i210
	movq	%r14, %rdi
	callq	Perl_sv_grow
.LBB1_276:                              # %cond.end.i212
	cmpq	$1, %r12
	jne	.LBB1_279
# BB#277:                               # %while.body.lr.ph.i217
	movq	(%r14), %rcx
	movq	8(%rcx), %rdx
	movq	(%rcx), %rax
	leaq	(%rdx,%rbx), %rsi
	movq	%rsi, 8(%rcx)
	xorq	%rcx, %rcx
	movb	%cl, (%rax,%rsi)
	addq	%rdx, %rax
.LBB1_278:                              # %while.body.i223
                                        # =>This Inner Loop Header: Depth=1
	movzbl	(%r15), %ecx
	movb	%cl, -1(%rax,%rbx)
	leaq	-1(%rbx), %rbx
	testq	%rbx, %rbx
	jg	.LBB1_278
	jmp	.LBB1_280
.LBB1_279:                              # %while.body28.i227
                                        # =>This Inner Loop Header: Depth=1
	movq	$2, %rcx
	movq	%r14, %rdi
	movq	%r15, %rsi
	movq	%r12, %rdx
	callq	Perl_sv_catpvn_flags
	decq	%rbx
	testq	%rbx, %rbx
	jg	.LBB1_279
.LBB1_280:                              # %sv_x.exit228
	movq	$2, %rdx
	movq	%r13, %r15
	movq	%r15, %rdi
	movq	144(%rsp), %rsi         # 8-byte Reload
	callq	Perl_sv_catsv_flags
	movq	$2, %rdx
	movq	%r15, %rdi
	movq	%r14, %rsi
	callq	Perl_sv_catsv_flags
	movq	%r14, %rdi
	callq	Perl_sv_free
	movq	264(%rsp), %r12         # 8-byte Reload
	movq	256(%rsp), %r14         # 8-byte Reload
.LBB1_281:                              # %if.end867
	movq	176(%rsp), %rax         # 8-byte Reload
	movzbl	(%rax), %eax
	cmpq	$37, %rax
	jne	.LBB1_283
# BB#282:                               # %if.then872
	movabsq	$.L.str58, %rsi
	jmp	.LBB1_284
.LBB1_283:                              # %if.else873
	movabsq	$.L.str38, %rsi
.LBB1_284:                              # %if.end874
	movq	$1, %rdx
	movq	$2, %rcx
	movq	%r15, %rdi
	callq	Perl_sv_catpvn_flags
	movq	232(%rsp), %r13         # 8-byte Reload
	movq	200(%rsp), %rbx         # 8-byte Reload
	movq	136(%rsp), %rdi         # 8-byte Reload
	callq	Perl_sv_free
	movq	144(%rsp), %rdi         # 8-byte Reload
	callq	Perl_sv_free
	jmp	.LBB1_289
.LBB1_42:                               # %if.then158
	cmpq	$11, %rax
	movq	264(%rsp), %rbx         # 8-byte Reload
	jne	.LBB1_45
# BB#43:                                # %if.then162
	movabsq	$.L.str34, %rsi
	movq	$2, %rdx
	jmp	.LBB1_46
.LBB1_54:                               # %if.else203
	movq	$2, %rdx
	movq	%r15, %rdi
	movq	%r14, %rsi
	callq	Perl_sv_catsv_flags
	jmp	.LBB1_432
.LBB1_45:                               # %if.else167
	movabsq	$.L.str36, %rsi
	movq	$9, %rdx
.LBB1_46:                               # %if.end169
	movq	$2, %rcx
	movq	%r15, %rdi
	callq	Perl_sv_catpvn_flags
.LBB1_47:                               # %if.end169
	movq	%r13, %rdi
	movq	%rbx, %rsi
	callq	Perl_newSVpvn
	movq	%rax, %rbx
	movabsq	$.L.str26, %rsi
	movq	$3, %rdx
	movq	$2, %rcx
	movq	%rbx, %rdi
	callq	Perl_sv_catpvn_flags
	movq	$2, %rdx
	movq	%rbx, %rdi
	movq	%r14, %rsi
	callq	Perl_sv_catsv_flags
	movq	248(%rsp), %rdi         # 8-byte Reload
	movq	%rbx, %rsi
	callq	Perl_av_push
	jmp	.LBB1_432
.LBB1_315:                              # %cond.false948
	callq	Perl_sv_2iv
.LBB1_316:                              # %cond.end950
	testq	%rax, %rax
	movq	%r14, %rax
	jle	.LBB1_319
# BB#317:                               # %if.then954
	movabsq	$.L.str47, %rsi
	movq	$2, %rdx
	movq	$2, %rcx
	movq	240(%rsp), %rbx         # 8-byte Reload
	movq	%rbx, %rdi
	callq	Perl_sv_catpvn_flags
	movq	$2, %rdx
	movq	%rbx, %rdi
	movq	%r15, %rsi
	callq	Perl_sv_catsv_flags
	movabsq	$.L.str38, %rsi
	movq	$1, %rdx
	movq	$2, %rcx
	movq	%rbx, %rdi
	callq	Perl_sv_catpvn_flags
	jmp	.LBB1_432
.Ltmp35:
	.size	DD_dump, .Ltmp35-DD_dump
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI1_0:
	.quad	.LBB1_123
	.quad	.LBB1_161
	.quad	.LBB1_285
	.quad	.LBB1_122

	.text
	.globl	boot_Data__Dumper
	.align	16, 0x90
	.type	boot_Data__Dumper,@function
boot_Data__Dumper:                      # @boot_Data__Dumper
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp38:
	.cfi_def_cfa_offset 16
.Ltmp39:
	.cfi_offset %rbx, -16
	movq	PL_markstack_ptr(%rip), %rax
	leaq	-8(%rax), %rcx
	movq	%rcx, PL_markstack_ptr(%rip)
	movq	(%rax), %rbx
	movabsq	$.L.str30, %rdi
	movabsq	$XS_Data__Dumper_Dumpxs, %rsi
	movabsq	$.L.str29, %rdx
	callq	Perl_newXS
	movabsq	$.L.str31, %rsi
	movq	%rax, %rdi
	callq	Perl_sv_setpv
	movq	PL_stack_base(%rip), %rax
	movq	$PL_sv_yes, 8(%rax,%rbx,8)
	movq	PL_stack_base(%rip), %rax
	leaq	8(%rax,%rbx,8), %rax
	movq	%rax, PL_stack_sp(%rip)
	popq	%rbx
	retq
.Ltmp40:
	.size	boot_Data__Dumper, .Ltmp40-boot_Data__Dumper
	.cfi_endproc

	.align	16, 0x90
	.type	esc_q_utf8,@function
esc_q_utf8:                             # @esc_q_utf8
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp47:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp48:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp49:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp50:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp51:
	.cfi_def_cfa_offset 48
	subq	$48, %rsp
.Ltmp52:
	.cfi_def_cfa_offset 96
.Ltmp53:
	.cfi_offset %rbx, -48
.Ltmp54:
	.cfi_offset %r12, -40
.Ltmp55:
	.cfi_offset %r13, -32
.Ltmp56:
	.cfi_offset %r14, -24
.Ltmp57:
	.cfi_offset %r15, -16
	movq	%rdi, 24(%rsp)          # 8-byte Spill
	movq	(%rdi), %rax
	movq	8(%rax), %rcx
	movq	%rdx, 16(%rsp)          # 8-byte Spill
	movq	%rsi, %r12
	leaq	(%r12,%rdx), %r14
	testq	%rdx, %rdx
	jle	.LBB3_33
# BB#1:
	movq	%rcx, (%rsp)            # 8-byte Spill
	xorq	%r15, %r15
	movq	%r12, %rbx
	movq	$0, 40(%rsp)            # 8-byte Folded Spill
	xorq	%r13, %r13
	movq	$0, 8(%rsp)             # 8-byte Folded Spill
	movq	$0, 32(%rsp)            # 8-byte Folded Spill
	.align	16, 0x90
.LBB3_2:                                # %for.body
                                        # =>This Inner Loop Header: Depth=1
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	Perl_utf8_to_uvchr
	cmpq	$128, %rax
	jb	.LBB3_9
# BB#3:                                 # %if.then
                                        #   in Loop: Header=BB3_2 Depth=1
	movq	$6, %rcx
	cmpq	$256, %rax              # imm = 0x100
	jb	.LBB3_8
# BB#4:                                 # %cond.false
                                        #   in Loop: Header=BB3_2 Depth=1
	movq	$7, %rcx
	cmpq	$4096, %rax             # imm = 0x1000
	jb	.LBB3_8
# BB#5:                                 # %cond.false5
                                        #   in Loop: Header=BB3_2 Depth=1
	cmpq	$65535, %rax            # imm = 0xFFFF
	movabsq	$0, %rax
	jbe	.LBB3_7
# BB#6:                                 # %cond.false5
                                        #   in Loop: Header=BB3_2 Depth=1
	movabsq	$1, %rax
.LBB3_7:                                # %cond.false5
                                        #   in Loop: Header=BB3_2 Depth=1
	leaq	8(,%rax,4), %rcx
.LBB3_8:                                # %cond.end8
                                        #   in Loop: Header=BB3_2 Depth=1
	addq	%rcx, %r15
	jmp	.LBB3_20
	.align	16, 0x90
.LBB3_9:                                # %if.else
                                        #   in Loop: Header=BB3_2 Depth=1
	cmpq	$91, %rax
	jg	.LBB3_15
# BB#10:                                # %if.else
                                        #   in Loop: Header=BB3_2 Depth=1
	cmpq	$63, %rax
	jg	.LBB3_17
# BB#11:                                # %if.else
                                        #   in Loop: Header=BB3_2 Depth=1
	cmpq	$34, %rax
	je	.LBB3_18
# BB#12:                                # %if.else
                                        #   in Loop: Header=BB3_2 Depth=1
	cmpq	$36, %rax
	je	.LBB3_18
# BB#13:                                # %if.else
                                        #   in Loop: Header=BB3_2 Depth=1
	cmpq	$39, %rax
	jne	.LBB3_19
# BB#14:                                # %if.then17
                                        #   in Loop: Header=BB3_2 Depth=1
	incq	8(%rsp)                 # 8-byte Folded Spill
	jmp	.LBB3_20
	.align	16, 0x90
.LBB3_15:                               # %if.else
                                        #   in Loop: Header=BB3_2 Depth=1
	cmpq	$92, %rax
	jne	.LBB3_19
# BB#16:                                # %if.then13
                                        #   in Loop: Header=BB3_2 Depth=1
	incq	32(%rsp)                # 8-byte Folded Spill
	jmp	.LBB3_20
.LBB3_17:                               # %if.else
                                        #   in Loop: Header=BB3_2 Depth=1
	cmpq	$64, %rax
	jne	.LBB3_19
.LBB3_18:                               # %if.then27
                                        #   in Loop: Header=BB3_2 Depth=1
	incq	%r13
	jmp	.LBB3_20
	.align	16, 0x90
.LBB3_19:                               # %if.else29
                                        #   in Loop: Header=BB3_2 Depth=1
	incq	40(%rsp)                # 8-byte Folded Spill
.LBB3_20:                               # %for.inc
                                        #   in Loop: Header=BB3_2 Depth=1
	movzbl	(%rbx), %eax
	movzbl	PL_utf8skip(%rax), %eax
	addq	%rax, %rbx
	cmpq	%r14, %rbx
	jb	.LBB3_2
# BB#21:                                # %for.end
	movq	%r13, %rcx
	movq	(%rsp), %r13            # 8-byte Reload
	leaq	3(%r13), %rax
	testq	%r15, %r15
	je	.LBB3_22
# BB#23:                                # %if.then36
	movq	32(%rsp), %rdx          # 8-byte Reload
	addq	%rdx, %rcx
	movq	8(%rsp), %rdx           # 8-byte Reload
	addq	%rax, %rdx
	movq	40(%rsp), %rax          # 8-byte Reload
	addq	%rax, %rdx
	addq	%r15, %rdx
	leaq	(%rdx,%rcx,2), %rsi
	movq	24(%rsp), %rbx          # 8-byte Reload
	movq	%rbx, %rdi
	callq	Perl_sv_grow
	movq	(%rbx), %rax
	movq	(%rax), %r15
	cmpq	$0, 16(%rsp)            # 8-byte Folded Reload
	leaq	1(%r15,%r13), %rbx
	movq	$34, %rax
	movb	%al, (%r15,%r13)
	jle	.LBB3_32
# BB#24:
	movabsq	$288230377225453573, %r13 # imm = 0x400000040000005
	.align	16, 0x90
.LBB3_25:                               # %for.body50
                                        # =>This Inner Loop Header: Depth=1
	xorq	%rsi, %rsi
	movq	%r12, %rdi
	callq	Perl_utf8_to_uvchr
	movq	%rax, %rdx
	movq	%rdx, %rcx
	addq	$-34, %rcx
	cmpq	$58, %rcx
	ja	.LBB3_28
# BB#26:                                # %for.body50
                                        #   in Loop: Header=BB3_25 Depth=1
	movq	$1, %rax
	shlq	%cl, %rax
	testq	%r13, %rax
	je	.LBB3_28
# BB#27:                                # %if.then64
                                        #   in Loop: Header=BB3_25 Depth=1
	movq	$92, %rax
	movb	%al, (%rbx)
	movb	%dl, 1(%rbx)
	addq	$2, %rbx
	jmp	.LBB3_31
	.align	16, 0x90
.LBB3_28:                               # %if.else68
                                        #   in Loop: Header=BB3_25 Depth=1
	cmpq	$127, %rdx
	ja	.LBB3_30
# BB#29:                                # %if.then71
                                        #   in Loop: Header=BB3_25 Depth=1
	movb	%dl, (%rbx)
	incq	%rbx
	jmp	.LBB3_31
	.align	16, 0x90
.LBB3_30:                               # %if.else74
                                        #   in Loop: Header=BB3_25 Depth=1
	xorq	%rax, %rax
	movq	%rbx, %rdi
	movabsq	$.L.str72, %rsi
	callq	sprintf
	movq	%rbx, %rdi
	callq	strlen
	addq	%rax, %rbx
.LBB3_31:                               # %for.inc80
                                        #   in Loop: Header=BB3_25 Depth=1
	movzbl	(%r12), %eax
	movzbl	PL_utf8skip(%rax), %eax
	addq	%rax, %r12
	cmpq	%r14, %r12
	jb	.LBB3_25
.LBB3_32:                               # %for.end86
	movq	(%rsp), %r13            # 8-byte Reload
	addq	%r13, %r15
	movq	$34, %rax
	jmp	.LBB3_41
.LBB3_33:                               # %for.end.thread
	leaq	3(%rcx), %rax
	xorq	%rdi, %rdi
	xorq	%rbx, %rbx
	xorq	%rdx, %rdx
	xorq	%rsi, %rsi
	movq	%rcx, %r13
	jmp	.LBB3_34
.LBB3_22:
	movq	%rcx, %rdx
	movq	40(%rsp), %rsi          # 8-byte Reload
	movq	32(%rsp), %rdi          # 8-byte Reload
	movq	8(%rsp), %rbx           # 8-byte Reload
.LBB3_34:                               # %if.else88
	addq	%rbx, %rdi
	addq	%rdx, %rax
	addq	%rsi, %rax
	leaq	(%rax,%rdi,2), %rsi
	movq	24(%rsp), %rbx          # 8-byte Reload
	movq	%rbx, %rdi
	callq	Perl_sv_grow
	movq	(%rbx), %rax
	movq	(%rax), %r15
	cmpq	$0, 16(%rsp)            # 8-byte Folded Reload
	leaq	1(%r15,%r13), %rbx
	movq	$39, %rax
	movb	%al, (%r15,%r13)
	jle	.LBB3_40
# BB#35:
	movq	$92, %rcx
	.align	16, 0x90
.LBB3_36:                               # %for.body104
                                        # =>This Inner Loop Header: Depth=1
	movzbl	(%r12), %edx
	cmpq	$39, %rdx
	je	.LBB3_38
# BB#37:                                # %for.body104
                                        #   in Loop: Header=BB3_36 Depth=1
	cmpq	$92, %rdx
	jne	.LBB3_39
.LBB3_38:                               # %if.then113
                                        #   in Loop: Header=BB3_36 Depth=1
	movb	%cl, (%rbx)
	incq	%rbx
.LBB3_39:                               # %if.end115
                                        #   in Loop: Header=BB3_36 Depth=1
	movb	%dl, (%rbx)
	incq	%rbx
	incq	%r12
	cmpq	%r14, %r12
	jb	.LBB3_36
.LBB3_40:                               # %for.end119
	addq	%r13, %r15
.LBB3_41:                               # %if.end121
	movb	%al, (%rbx)
	xorq	%rax, %rax
	movb	%al, 1(%rbx)
	movq	24(%rsp), %rax          # 8-byte Reload
	movq	(%rax), %rax
	incq	%rbx
	subq	%r15, %rbx
	addq	%rbx, %r13
	movq	%r13, 8(%rax)
	movq	%rbx, %rax
	addq	$48, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp58:
	.size	esc_q_utf8, .Ltmp58-esc_q_utf8
	.cfi_endproc

	.type	.L.str,@object          # @.str
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.asciz	"Usage: Data::Dumper::Dumpxs(href, ...)"
	.size	.L.str, 39

	.type	.L.str1,@object         # @.str1
.L.str1:
	.asciz	"Usage: Data::Dumper::Dumpxs(PACKAGE, VAL_ARY_REF, [NAME_ARY_REF])"
	.size	.L.str1, 66

	.type	.L.str2,@object         # @.str2
.L.str2:
	.asciz	"new"
	.size	.L.str2, 4

	.type	.L.str3,@object         # @.str3
.L.str3:
	.zero	1
	.size	.L.str3, 1

	.type	.L.str4,@object         # @.str4
.L.str4:
	.asciz	"seen"
	.size	.L.str4, 5

	.type	.L.str5,@object         # @.str5
.L.str5:
	.asciz	"todump"
	.size	.L.str5, 7

	.type	.L.str6,@object         # @.str6
.L.str6:
	.asciz	"names"
	.size	.L.str6, 6

	.type	.L.str7,@object         # @.str7
.L.str7:
	.asciz	"indent"
	.size	.L.str7, 7

	.type	.L.str8,@object         # @.str8
.L.str8:
	.asciz	"purity"
	.size	.L.str8, 7

	.type	.L.str9,@object         # @.str9
.L.str9:
	.asciz	"terse"
	.size	.L.str9, 6

	.type	.L.str10,@object        # @.str10
.L.str10:
	.asciz	"pad"
	.size	.L.str10, 4

	.type	.L.str11,@object        # @.str11
.L.str11:
	.asciz	"xpad"
	.size	.L.str11, 5

	.type	.L.str12,@object        # @.str12
.L.str12:
	.asciz	"apad"
	.size	.L.str12, 5

	.type	.L.str13,@object        # @.str13
.L.str13:
	.asciz	"sep"
	.size	.L.str13, 4

	.type	.L.str14,@object        # @.str14
.L.str14:
	.asciz	"pair"
	.size	.L.str14, 5

	.type	.L.str15,@object        # @.str15
.L.str15:
	.asciz	"varname"
	.size	.L.str15, 8

	.type	.L.str16,@object        # @.str16
.L.str16:
	.asciz	"freezer"
	.size	.L.str16, 8

	.type	.L.str17,@object        # @.str17
.L.str17:
	.asciz	"toaster"
	.size	.L.str17, 8

	.type	.L.str18,@object        # @.str18
.L.str18:
	.asciz	"deepcopy"
	.size	.L.str18, 9

	.type	.L.str19,@object        # @.str19
.L.str19:
	.asciz	"quotekeys"
	.size	.L.str19, 10

	.type	.L.str20,@object        # @.str20
.L.str20:
	.asciz	"bless"
	.size	.L.str20, 6

	.type	.L.str21,@object        # @.str21
.L.str21:
	.asciz	"maxdepth"
	.size	.L.str21, 9

	.type	.L.str22,@object        # @.str22
.L.str22:
	.asciz	"sortkeys"
	.size	.L.str22, 9

	.type	.L.str23,@object        # @.str23
.L.str23:
	.asciz	"$"
	.size	.L.str23, 2

	.type	.L.str24,@object        # @.str24
.L.str24:
	.asciz	"%ld"
	.size	.L.str24, 4

	.type	.L.str26,@object        # @.str26
.L.str26:
	.asciz	" = "
	.size	.L.str26, 4

	.type	.L.str27,@object        # @.str27
.L.str27:
	.asciz	";"
	.size	.L.str27, 2

	.type	.L.str28,@object        # @.str28
.L.str28:
	.asciz	"Call to new() method failed to return HASH ref"
	.size	.L.str28, 47

	.type	.L.str29,@object        # @.str29
.L.str29:
	.asciz	"Dumper.c"
	.size	.L.str29, 9

	.type	.L.str30,@object        # @.str30
.L.str30:
	.asciz	"Data::Dumper::Dumpxs"
	.size	.L.str30, 21

	.type	.L.str31,@object        # @.str31
.L.str31:
	.asciz	"$;$$"
	.size	.L.str31, 5

	.type	.L.str32,@object        # @.str32
.L.str32:
	.asciz	"WARNING(Freezer method call failed): %_"
	.size	.L.str32, 40

	.type	.L.str33,@object        # @.str33
.L.str33:
	.asciz	"0x%lx"
	.size	.L.str33, 6

	.type	.L.str34,@object        # @.str34
.L.str34:
	.asciz	"{}"
	.size	.L.str34, 3

	.type	.L.str35,@object        # @.str35
.L.str35:
	.asciz	"[]"
	.size	.L.str35, 3

	.type	.L.str36,@object        # @.str36
.L.str36:
	.asciz	"do{my $o}"
	.size	.L.str36, 10

	.type	.L.str37,@object        # @.str37
.L.str37:
	.asciz	"{"
	.size	.L.str37, 2

	.type	.L.str38,@object        # @.str38
.L.str38:
	.asciz	"}"
	.size	.L.str38, 2

	.type	.L.str39,@object        # @.str39
.L.str39:
	.asciz	"ref name not found for %s"
	.size	.L.str39, 26

	.type	.L.str40,@object        # @.str40
.L.str40:
	.asciz	"\\"
	.size	.L.str40, 2

	.type	.L.str41,@object        # @.str41
.L.str41:
	.asciz	"Regexp"
	.size	.L.str41, 7

	.type	.L.str42,@object        # @.str42
.L.str42:
	.asciz	"qr/"
	.size	.L.str42, 4

	.type	.L.str43,@object        # @.str43
.L.str43:
	.asciz	"\\/"
	.size	.L.str43, 3

	.type	.L.str44,@object        # @.str44
.L.str44:
	.asciz	"/"
	.size	.L.str44, 2

	.type	.L.str45,@object        # @.str45
.L.str45:
	.asciz	"'"
	.size	.L.str45, 2

	.type	.L.str46,@object        # @.str46
.L.str46:
	.asciz	"( "
	.size	.L.str46, 3

	.type	.L.str47,@object        # @.str47
.L.str47:
	.asciz	"${"
	.size	.L.str47, 3

	.type	.L.str48,@object        # @.str48
.L.str48:
	.asciz	"do{\\(my $o = "
	.size	.L.str48, 14

	.type	.L.str49,@object        # @.str49
.L.str49:
	.asciz	")}"
	.size	.L.str49, 3

	.type	.L.str50,@object        # @.str50
.L.str50:
	.asciz	"*{"
	.size	.L.str50, 3

	.type	.L.str51,@object        # @.str51
.L.str51:
	.asciz	"("
	.size	.L.str51, 2

	.type	.L.str52,@object        # @.str52
.L.str52:
	.asciz	"["
	.size	.L.str52, 2

	.type	.L.str53,@object        # @.str53
.L.str53:
	.asciz	"{SCALAR}"
	.size	.L.str53, 9

	.type	.L.str54,@object        # @.str54
.L.str54:
	.asciz	"{ARRAY}"
	.size	.L.str54, 8

	.type	.L.str55,@object        # @.str55
.L.str55:
	.asciz	"{HASH}"
	.size	.L.str55, 7

	.type	.L.str56,@object        # @.str56
.L.str56:
	.asciz	"#"
	.size	.L.str56, 2

	.type	.L.str57,@object        # @.str57
.L.str57:
	.asciz	","
	.size	.L.str57, 2

	.type	.L.str58,@object        # @.str58
.L.str58:
	.asciz	")"
	.size	.L.str58, 2

	.type	.L.str59,@object        # @.str59
.L.str59:
	.asciz	"]"
	.size	.L.str59, 2

	.type	.L.str60,@object        # @.str60
.L.str60:
	.asciz	"->"
	.size	.L.str60, 3

	.type	.L.str61,@object        # @.str61
.L.str61:
	.asciz	"Sortkeys subroutine did not return ARRAYREF\n"
	.size	.L.str61, 45

	.type	.L.str62,@object        # @.str62
.L.str62:
	.asciz	"sub { \"DUMMY\" }"
	.size	.L.str62, 16

	.type	.L.str63,@object        # @.str63
.L.str63:
	.asciz	"Encountered CODE ref, using dummy placeholder"
	.size	.L.str63, 46

	.type	.L.str64,@object        # @.str64
.L.str64:
	.asciz	"cannot handle ref type %ld"
	.size	.L.str64, 27

	.type	.L.str65,@object        # @.str65
.L.str65:
	.asciz	", '"
	.size	.L.str65, 4

	.type	.L.str66,@object        # @.str66
.L.str66:
	.asciz	"' )"
	.size	.L.str66, 4

	.type	.L.str67,@object        # @.str67
.L.str67:
	.asciz	"()"
	.size	.L.str67, 3

	.type	.L.str68,@object        # @.str68
.L.str68:
	.asciz	"%lu"
	.size	.L.str68, 4

	.type	.L.str69,@object        # @.str69
.L.str69:
	.asciz	"'%s'"
	.size	.L.str69, 5

	.type	.L.str70,@object        # @.str70
.L.str70:
	.asciz	"main::"
	.size	.L.str70, 7

	.type	DD_dump.entries,@object # @DD_dump.entries
	.section	.rodata,"a",@progbits
	.align	16
DD_dump.entries:
	.quad	.L.str53
	.quad	.L.str54
	.quad	.L.str55
	.size	DD_dump.entries, 24

	.type	DD_dump.sizes,@object   # @DD_dump.sizes
	.align	16
DD_dump.sizes:
	.quad	8                       # 0x8
	.quad	7                       # 0x7
	.quad	6                       # 0x6
	.size	DD_dump.sizes, 24

	.type	.L.str71,@object        # @.str71
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str71:
	.asciz	"undef"
	.size	.L.str71, 6

	.type	.L.str72,@object        # @.str72
.L.str72:
	.asciz	"\\x{%lx}"
	.size	.L.str72, 8


	.ident	"clang version 3.5 (trunk)"
	.section	".note.GNU-stack","",@progbits
