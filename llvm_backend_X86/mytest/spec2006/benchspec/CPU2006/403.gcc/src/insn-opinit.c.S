	.file	"insn-opinit.c.bc"
	.text
	.globl	init_all_optabs
	.align	16, 0x90
	.type	init_all_optabs,@function
init_all_optabs:                        # @init_all_optabs
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
	movl	target_flags(%rip), %ebx
	movq	%rbx, %r15
	andq	$33554432, %r15         # imm = 0x2000000
	je	.LBB0_2
# BB#1:                                 # %if.then11
	movq	$117, %rax
	movl	%eax, extendtab+2388(%rip)
	movq	$118, %rax
	movl	%eax, extendtab+2380(%rip)
	movq	$121, %rax
	movl	%eax, extendtab+2384(%rip)
	movq	$122, %rax
	movl	%eax, extendtab+2376(%rip)
.LBB0_2:                                # %if.end12
	movq	$123, %rax
	movl	%eax, extendtab+1912(%rip)
	movq	$125, %rax
	movl	%eax, extendtab+1432(%rip)
	movq	$126, %rax
	movl	%eax, extendtab+1904(%rip)
	movq	%rbx, %r12
	andq	$327681, %r12           # imm = 0x50001
	cmpq	$1, %r12
	jne	.LBB0_4
# BB#3:                                 # %if.then16
	movq	$167, %rax
	movl	%eax, floattab+7104(%rip)
.LBB0_4:                                # %if.end17
	movq	%rbx, %r9
	andq	$262145, %r9            # imm = 0x40001
	cmpq	$1, %r9
	jne	.LBB0_6
# BB#5:                                 # %if.then23
	movq	$173, %rax
	movl	%eax, floattab+7576(%rip)
.LBB0_6:                                # %if.end24
	movq	%rbx, %r14
	andq	$33554433, %r14         # imm = 0x2000001
	cmpq	$1, %r14
	jne	.LBB0_8
# BB#7:                                 # %if.then30
	movq	$179, %rax
	movl	%eax, floattab+8048(%rip)
.LBB0_8:                                # %if.end31
	movq	%rbx, %r10
	andq	$1, %r10
	je	.LBB0_10
# BB#9:                                 # %if.then34
	movq	$180, %rax
	movl	%eax, floattab+8520(%rip)
.LBB0_10:                               # %if.end35
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	andq	%rdx, %r14
	cmpq	$1, %r14
	jne	.LBB0_12
# BB#11:                                # %if.then41
	movq	$181, %rax
	movl	%eax, floattab+8056(%rip)
.LBB0_12:                               # %if.end42
	testq	%rdx, %r10
	je	.LBB0_14
# BB#13:                                # %if.then45
	movq	$182, %rax
	movl	%eax, floattab+8528(%rip)
.LBB0_14:                               # %if.end46
	cmpq	$1, %r14
	jne	.LBB0_16
# BB#15:                                # %if.then52
	movq	$183, %rax
	movl	%eax, floattab+8064(%rip)
.LBB0_16:                               # %if.end53
	testq	%rdx, %r10
	je	.LBB0_18
# BB#17:                                # %if.then56
	movq	$184, %rax
	movl	%eax, floattab+8536(%rip)
.LBB0_18:                               # %if.end57
	movl	ix86_cpu(%rip), %ecx
	movl	x86_qimode_math(%rip), %r13d
	movq	$1, %rax
	movq	$1, %rsi
	shlq	%cl, %rsi
	andq	%r13, %rsi
	testq	%rdx, %rsi
	je	.LBB0_20
# BB#19:                                # %if.then60
	movq	optab_table+80(%rip), %rcx
	movq	$266, %rsi              # imm = 0x10A
	movl	%esi, 40(%rcx)
	movl	ix86_cpu(%rip), %ecx
.LBB0_20:                               # %if.end61
	shlq	%cl, %rax
	andq	%r13, %rax
	testq	%rdx, %rax
	je	.LBB0_22
# BB#21:                                # %if.then65
	movq	optab_table+104(%rip), %rax
	movq	$267, %rcx              # imm = 0x10B
	movl	%ecx, 40(%rax)
	movl	ix86_cpu(%rip), %ecx
.LBB0_22:                               # %if.end69
	movl	x86_himode_math(%rip), %esi
	movq	$1, %rax
	shlq	%cl, %rax
	andq	%rsi, %rax
	testq	%rdx, %rax
	je	.LBB0_24
# BB#23:                                # %if.then73
	movq	optab_table+96(%rip), %rax
	movq	$274, %rcx              # imm = 0x112
	movl	%ecx, 56(%rax)
.LBB0_24:                               # %if.end77
	movq	optab_table+112(%rip), %rcx
	testq	%rdx, %r15
	je	.LBB0_26
# BB#25:                                # %if.then80
	movq	$275, %rax              # imm = 0x113
	movl	%eax, 88(%rcx)
.LBB0_26:                               # %if.end84
	movq	$277, %rax              # imm = 0x115
	movl	%eax, 72(%rcx)
	movq	%rcx, -40(%rsp)         # 8-byte Spill
	movq	%rbx, %rax
	andq	$33554497, %rax         # imm = 0x2000041
	cmpq	$1, %rax
	jne	.LBB0_29
# BB#27:                                # %land.lhs.true96
	movl	flag_unsafe_math_optimizations(%rip), %eax
	movq	%rbx, %rcx
	andq	$16, %rcx
	orq	%rcx, %rax
	testq	%rdx, %rax
	je	.LBB0_29
# BB#28:                                # %if.then100
	movq	optab_table+304(%rip), %rax
	movq	$566, %rcx              # imm = 0x236
	movl	%ecx, 280(%rax)
.LBB0_29:                               # %if.end104
	movq	%rbx, %rax
	andq	$65, %rax
	movq	%rax, -24(%rsp)         # 8-byte Spill
	cmpq	$1, %rax
	jne	.LBB0_32
# BB#30:                                # %land.lhs.true110
	movl	flag_unsafe_math_optimizations(%rip), %eax
	movq	%rbx, %rcx
	andq	$16, %rcx
	orq	%rcx, %rax
	testq	%rdx, %rax
	je	.LBB0_32
# BB#31:                                # %if.then115
	movq	optab_table+304(%rip), %rax
	movq	$567, %rcx              # imm = 0x237
	movl	%ecx, 296(%rax)
.LBB0_32:                               # %if.end119
	movq	%rbx, %rax
	andq	$64, %rax
	jne	.LBB0_39
# BB#33:                                # %land.lhs.true122
	testq	%rdx, %r10
	je	.LBB0_36
# BB#34:                                # %land.lhs.true122
	movl	flag_unsafe_math_optimizations(%rip), %ecx
	testq	%rdx, %rcx
	je	.LBB0_36
# BB#35:                                # %if.end131
	movq	optab_table+312(%rip), %rcx
	movq	$572, %rsi              # imm = 0x23C
	movl	%esi, 264(%rcx)
.LBB0_36:                               # %land.lhs.true134
	testq	%rdx, %r10
	je	.LBB0_39
# BB#37:                                # %land.lhs.true134
	movl	flag_unsafe_math_optimizations(%rip), %ecx
	testq	%rdx, %rcx
	je	.LBB0_39
# BB#38:                                # %if.then139
	movq	optab_table+312(%rip), %rcx
	movq	$573, %rsi              # imm = 0x23D
	movl	%esi, 248(%rcx)
.LBB0_39:                               # %if.end143
	cmpq	$1, %r14
	jne	.LBB0_43
# BB#40:                                # %land.lhs.true149
	testq	%rdx, %rax
	jne	.LBB0_43
# BB#41:                                # %land.lhs.true149
	movl	flag_unsafe_math_optimizations(%rip), %ecx
	testq	%rdx, %rcx
	je	.LBB0_43
# BB#42:                                # %if.then154
	movq	optab_table+312(%rip), %rcx
	movq	$575, %rsi              # imm = 0x23F
	movl	%esi, 280(%rcx)
.LBB0_43:                               # %if.end158
	testq	%rdx, %rax
	jne	.LBB0_61
# BB#44:                                # %land.lhs.true161
	testq	%rdx, %r10
	je	.LBB0_47
# BB#45:                                # %land.lhs.true161
	movl	flag_unsafe_math_optimizations(%rip), %ecx
	testq	%rdx, %rcx
	je	.LBB0_47
# BB#46:                                # %if.end170
	movq	optab_table+312(%rip), %rcx
	movq	$576, %rsi              # imm = 0x240
	movl	%esi, 296(%rcx)
.LBB0_47:                               # %land.lhs.true173
	testq	%rdx, %r10
	je	.LBB0_50
# BB#48:                                # %land.lhs.true173
	movl	flag_unsafe_math_optimizations(%rip), %ecx
	testq	%rdx, %rcx
	je	.LBB0_50
# BB#49:                                # %if.then178
	movq	optab_table+320(%rip), %rcx
	movq	$577, %rsi              # imm = 0x241
	movl	%esi, 264(%rcx)
.LBB0_50:                               # %if.end182
	testq	%rdx, %rax
	jne	.LBB0_61
# BB#51:                                # %land.lhs.true185
	testq	%rdx, %r10
	je	.LBB0_54
# BB#52:                                # %land.lhs.true185
	movl	flag_unsafe_math_optimizations(%rip), %ecx
	testq	%rdx, %rcx
	je	.LBB0_54
# BB#53:                                # %if.end194
	movq	optab_table+320(%rip), %rcx
	movq	$578, %rsi              # imm = 0x242
	movl	%esi, 248(%rcx)
.LBB0_54:                               # %land.lhs.true197
	testq	%rdx, %r10
	je	.LBB0_57
# BB#55:                                # %land.lhs.true197
	movl	flag_unsafe_math_optimizations(%rip), %ecx
	testq	%rdx, %rcx
	je	.LBB0_57
# BB#56:                                # %if.then202
	movq	optab_table+320(%rip), %rcx
	movq	$580, %rsi              # imm = 0x244
	movl	%esi, 280(%rcx)
.LBB0_57:                               # %if.end206
	testq	%rdx, %rax
	jne	.LBB0_61
# BB#58:                                # %land.lhs.true209
	testq	%rdx, %r10
	je	.LBB0_61
# BB#59:                                # %land.lhs.true209
	movl	flag_unsafe_math_optimizations(%rip), %eax
	testq	%rdx, %rax
	je	.LBB0_61
# BB#60:                                # %if.then214
	movq	optab_table+320(%rip), %rax
	movq	$581, %rcx              # imm = 0x245
	movl	%ecx, 296(%rax)
.LBB0_61:                               # %if.end218
	movq	%rbx, %rax
	andq	$327680, %rax           # imm = 0x50000
	movq	%rax, -8(%rsp)          # 8-byte Spill
	je	.LBB0_63
# BB#62:                                # %if.then258
	movq	optab_table(%rip), %rax
	movq	$690, %rcx              # imm = 0x2B2
	movl	%ecx, 744(%rax)
	movq	optab_table+8(%rip), %rax
	movl	%ecx, 744(%rax)
	movq	optab_table+16(%rip), %rax
	movq	$692, %rcx              # imm = 0x2B4
	movl	%ecx, 744(%rax)
	movq	optab_table+24(%rip), %rax
	movl	%ecx, 744(%rax)
	movq	optab_table+32(%rip), %rax
	movq	$694, %rcx              # imm = 0x2B6
	movl	%ecx, 744(%rax)
	movq	optab_table+40(%rip), %rax
	movl	%ecx, 744(%rax)
	movq	optab_table+80(%rip), %rax
	movq	$696, %rcx              # imm = 0x2B8
	movl	%ecx, 744(%rax)
	movq	optab_table+304(%rip), %rax
	movq	$702, %rcx              # imm = 0x2BE
	movl	%ecx, 744(%rax)
.LBB0_63:                               # %if.end262
	movq	%rbx, %rsi
	andq	$16384, %rsi            # imm = 0x4000
	je	.LBB0_65
# BB#64:                                # %if.then314
	movq	optab_table(%rip), %rax
	movq	$745, %rcx              # imm = 0x2E9
	movl	%ecx, 632(%rax)
	movq	optab_table(%rip), %rax
	movq	$746, %rcx              # imm = 0x2EA
	movl	%ecx, 584(%rax)
	movq	optab_table(%rip), %rax
	movq	$747, %rcx              # imm = 0x2EB
	movl	%ecx, 536(%rax)
	movq	optab_table+16(%rip), %rax
	movq	$752, %rcx              # imm = 0x2F0
	movl	%ecx, 632(%rax)
	movq	optab_table+16(%rip), %rax
	movq	$753, %rcx              # imm = 0x2F1
	movl	%ecx, 584(%rax)
	movq	optab_table+16(%rip), %rax
	movq	$754, %rcx              # imm = 0x2F2
	movl	%ecx, 536(%rax)
	movq	optab_table+32(%rip), %rax
	movq	$759, %rcx              # imm = 0x2F7
	movl	%ecx, 584(%rax)
	movq	optab_table+48(%rip), %rax
	movq	$760, %rcx              # imm = 0x2F8
	movl	%ecx, 584(%rax)
.LBB0_65:                               # %if.end318
	testq	$4521984, %rbx          # imm = 0x450000
	je	.LBB0_67
# BB#66:                                # %if.then364
	movq	optab_table+56(%rip), %rax
	movq	$761, %rcx              # imm = 0x2F9
	movl	%ecx, 584(%rax)
	movq	optab_table+232(%rip), %rax
	movq	$780, %rcx              # imm = 0x30C
	movl	%ecx, 632(%rax)
	movq	optab_table+216(%rip), %rax
	movq	$781, %rcx              # imm = 0x30D
	movl	%ecx, 584(%rax)
	movq	optab_table+224(%rip), %rax
	movq	$782, %rcx              # imm = 0x30E
	movl	%ecx, 632(%rax)
	movq	optab_table+208(%rip), %rax
	movq	$783, %rcx              # imm = 0x30F
	movl	%ecx, 584(%rax)
.LBB0_67:                               # %if.end368
	testq	%rdx, %rsi
	je	.LBB0_69
# BB#68:                                # %if.then406
	movq	optab_table+184(%rip), %rax
	movq	$784, %rcx              # imm = 0x310
	movl	%ecx, 584(%rax)
	movq	optab_table+184(%rip), %rax
	movq	$785, %rcx              # imm = 0x311
	movl	%ecx, 536(%rax)
	movq	optab_table+176(%rip), %rax
	movq	$786, %rcx              # imm = 0x312
	movl	%ecx, 584(%rax)
	movq	optab_table+176(%rip), %rax
	movq	$787, %rcx              # imm = 0x313
	movl	%ecx, 536(%rax)
	movq	optab_table+168(%rip), %rax
	movq	$789, %rcx              # imm = 0x315
	movl	%ecx, 584(%rax)
	movq	optab_table+168(%rip), %rax
	movq	$790, %rcx              # imm = 0x316
	movl	%ecx, 536(%rax)
.LBB0_69:                               # %if.end410
	movq	%rsi, -16(%rsp)         # 8-byte Spill
	movq	%rbx, %rax
	andq	$1048576, %rax          # imm = 0x100000
	movq	%rax, -32(%rsp)         # 8-byte Spill
	je	.LBB0_71
# BB#70:                                # %if.then433
	movq	optab_table(%rip), %rax
	movq	$806, %rcx              # imm = 0x326
	movl	%ecx, 712(%rax)
	movq	optab_table+8(%rip), %rax
	movl	%ecx, 712(%rax)
	movq	optab_table+16(%rip), %rax
	movq	$807, %rcx              # imm = 0x327
	movl	%ecx, 712(%rax)
	movq	optab_table+24(%rip), %rax
	movl	%ecx, 712(%rax)
	movq	optab_table+32(%rip), %rax
	movq	$814, %rcx              # imm = 0x32E
	movl	%ecx, 712(%rax)
	movq	optab_table+40(%rip), %rax
	movl	%ecx, 712(%rax)
.LBB0_71:                               # %if.end440
	movq	optab_table+328(%rip), %rax
	movq	$834, %rcx              # imm = 0x342
	movl	%ecx, 88(%rax)
	movq	$835, %rcx              # imm = 0x343
	movl	%ecx, 72(%rax)
	movq	$836, %rcx              # imm = 0x344
	movl	%ecx, 56(%rax)
	movl	ix86_cpu(%rip), %ecx
	movq	$1, %rsi
	shlq	%cl, %rsi
	andq	%r13, %rsi
	testq	%rdx, %rsi
	je	.LBB0_73
# BB#72:                                # %if.then453
	movq	$837, %rcx              # imm = 0x345
	movl	%ecx, 40(%rax)
.LBB0_73:                               # %if.end457
	cmpq	$1, %r14
	jne	.LBB0_75
# BB#74:                                # %if.then463
	movq	$841, %rcx              # imm = 0x349
	movl	%ecx, 280(%rax)
.LBB0_75:                               # %if.end467
	testq	%rdx, %r10
	je	.LBB0_77
# BB#76:                                # %if.then470
	movq	$842, %rcx              # imm = 0x34A
	movl	%ecx, 296(%rax)
.LBB0_77:                               # %if.end474
	testq	%rdx, %r9
	je	.LBB0_79
# BB#78:                                # %if.then480
	movq	$843, %rcx              # imm = 0x34B
	movl	%ecx, 264(%rax)
.LBB0_79:                               # %if.end484
	testq	%rdx, %r12
	je	.LBB0_81
# BB#80:                                # %if.then490
	movq	$844, %rcx              # imm = 0x34C
	movl	%ecx, 248(%rax)
.LBB0_81:                               # %if.end494
	movq	optab_table+240(%rip), %r11
	movq	$846, %rax              # imm = 0x34E
	movl	%eax, 72(%r11)
	movq	$847, %rax              # imm = 0x34F
	movl	%eax, 56(%r11)
	movl	ix86_cpu(%rip), %ecx
	movl	x86_partial_reg_stall(%rip), %eax
	movq	$1, %rsi
	shlq	%cl, %rsi
	andq	%rax, %rsi
	testq	%rdx, %rsi
	je	.LBB0_83
# BB#82:                                # %if.end494
	movl	optimize_size(%rip), %ecx
	testq	%rdx, %rcx
	je	.LBB0_84
.LBB0_83:                               # %if.then506
	movq	optab_table+248(%rip), %rcx
	movq	$848, %rsi              # imm = 0x350
	movl	%esi, 56(%rcx)
.LBB0_84:                               # %if.end510
	movq	$849, %rcx              # imm = 0x351
	movl	%ecx, 40(%r11)
	movl	ix86_cpu(%rip), %ecx
	movq	$850, %rsi              # imm = 0x352
	movl	%esi, reload_out_optab+8(%rip)
	movq	$1, %rsi
	shlq	%cl, %rsi
	andq	%rax, %rsi
	testq	%rdx, %rsi
	jne	.LBB0_86
# BB#85:                                # %if.then517
	movq	optab_table+248(%rip), %rax
	movq	$851, %rcx              # imm = 0x353
	movl	%ecx, 40(%rax)
.LBB0_86:                               # %if.end521
	testq	%rdx, %r15
	movq	$852, %rax              # imm = 0x354
	movl	%eax, 88(%r11)
	movq	$861, %rax              # imm = 0x35D
	movl	%eax, 248(%r11)
	movq	$865, %rax              # imm = 0x361
	movl	%eax, 264(%r11)
	jne	.LBB0_88
# BB#87:                                # %if.then533
	movq	$870, %rax              # imm = 0x366
	movl	%eax, 280(%r11)
.LBB0_88:                               # %if.end537
	testq	%rdx, %r9
	movq	$871, %rax              # imm = 0x367
	movl	%eax, 296(%r11)
	movq	$878, %rax              # imm = 0x36E
	movl	%eax, extendtab+1916(%rip)
	movq	$880, %rax              # imm = 0x370
	movl	%eax, extendtab+1436(%rip)
	movq	$884, %rax              # imm = 0x374
	movl	%eax, extendtab+1908(%rip)
	movq	$888, %rax              # imm = 0x378
	movl	%eax, extendtab+2396(%rip)
	movq	$892, %rax              # imm = 0x37C
	movl	%eax, extendtab+2392(%rip)
	je	.LBB0_90
# BB#89:                                # %if.then546
	movq	$904, %rax              # imm = 0x388
	movl	%eax, extendtab+7672(%rip)
.LBB0_90:                               # %if.end547
	cmpq	$1, %r14
	jne	.LBB0_92
# BB#91:                                # %if.then553
	movq	$905, %rax              # imm = 0x389
	movl	%eax, extendtab+8144(%rip)
.LBB0_92:                               # %if.end554
	testq	%rdx, %r10
	je	.LBB0_94
# BB#93:                                # %if.then557
	movq	$906, %rax              # imm = 0x38A
	movl	%eax, extendtab+8616(%rip)
.LBB0_94:                               # %if.end558
	cmpq	$1, %r14
	jne	.LBB0_96
# BB#95:                                # %if.then564
	movq	$907, %rax              # imm = 0x38B
	movl	%eax, extendtab+8152(%rip)
.LBB0_96:                               # %if.end565
	testq	%rdx, %r10
	je	.LBB0_98
# BB#97:                                # %if.then568
	movq	$908, %rax              # imm = 0x38C
	movl	%eax, extendtab+8624(%rip)
.LBB0_98:                               # %if.end569
	cmpq	$1, %r14
	jne	.LBB0_100
# BB#99:                                # %if.then575
	movq	$925, %rax              # imm = 0x39D
	movl	%eax, fixtrunctab+8064(%rip)
.LBB0_100:                              # %if.end576
	testq	%rdx, %r10
	je	.LBB0_102
# BB#101:                               # %if.then579
	movq	$926, %rax              # imm = 0x39E
	movl	%eax, fixtrunctab+8536(%rip)
.LBB0_102:                              # %if.end580
	movq	%rbx, %rax
	andq	$33816576, %rax         # imm = 0x2040000
	testq	%rdx, %r10
	jne	.LBB0_104
# BB#103:                               # %if.end580
	movq	%rax, %rcx
	andq	%rdx, %rcx
	cmpq	$33816576, %rcx         # imm = 0x2040000
	jne	.LBB0_105
.LBB0_104:                              # %if.then589
	movq	$927, %rcx              # imm = 0x39F
	movl	%ecx, fixtrunctab+7592(%rip)
.LBB0_105:                              # %if.end590
	movq	-8(%rsp), %rcx          # 8-byte Reload
	testq	%rdx, %rcx
	movabsq	$0, %rcx
	movabsq	$1, %r8
	movq	%rcx, %rsi
	jne	.LBB0_107
# BB#106:                               # %if.end590
	movq	%r8, %rsi
.LBB0_107:                              # %if.end590
	testq	%rdx, %r15
	jne	.LBB0_109
# BB#108:                               # %if.end590
	movq	%r8, %rcx
.LBB0_109:                              # %if.end590
	testq	%rdx, %r10
	jne	.LBB0_111
# BB#110:                               # %if.end590
	orq	%rcx, %rsi
	jne	.LBB0_112
.LBB0_111:                              # %if.then599
	movq	$928, %rcx              # imm = 0x3A0
	movl	%ecx, fixtrunctab+7120(%rip)
.LBB0_112:                              # %if.end600
	cmpq	$1, %r14
	jne	.LBB0_114
# BB#113:                               # %if.then606
	movq	$932, %rcx              # imm = 0x3A4
	movl	%ecx, fixtrunctab+8056(%rip)
.LBB0_114:                              # %if.end607
	testq	%rdx, %r10
	movq	%rbx, %r8
	je	.LBB0_116
# BB#115:                               # %if.then610
	movq	$933, %rcx              # imm = 0x3A5
	movl	%ecx, fixtrunctab+8528(%rip)
.LBB0_116:                              # %if.end611
	testq	%rdx, %r9
	je	.LBB0_118
# BB#117:                               # %if.then617
	movq	$934, %rcx              # imm = 0x3A6
	movl	%ecx, fixtrunctab+7584(%rip)
.LBB0_118:                              # %if.end618
	testq	%rdx, %r12
	je	.LBB0_120
# BB#119:                               # %if.then624
	movq	$935, %rcx              # imm = 0x3A7
	movl	%ecx, fixtrunctab+7112(%rip)
.LBB0_120:                              # %if.end625
	cmpq	$1, %r14
	jne	.LBB0_122
# BB#121:                               # %if.then631
	movq	$939, %rcx              # imm = 0x3AB
	movl	%ecx, fixtrunctab+8048(%rip)
.LBB0_122:                              # %if.end632
	testq	%rdx, %r10
	je	.LBB0_124
# BB#123:                               # %if.then635
	movq	$940, %rcx              # imm = 0x3AC
	movl	%ecx, fixtrunctab+8520(%rip)
.LBB0_124:                              # %if.end636
	movq	%r9, %rcx
	andq	%rdx, %rcx
	cmpq	$1, %rcx
	jne	.LBB0_126
# BB#125:                               # %if.then642
	movq	$941, %rcx              # imm = 0x3AD
	movl	%ecx, fixtrunctab+7576(%rip)
.LBB0_126:                              # %if.end643
	movq	%r12, %rcx
	andq	%rdx, %rcx
	cmpq	$1, %rcx
	jne	.LBB0_128
# BB#127:                               # %if.then649
	movq	$942, %rcx              # imm = 0x3AE
	movl	%ecx, fixtrunctab+7104(%rip)
.LBB0_128:                              # %if.end650
	testq	%rdx, %r12
	je	.LBB0_130
# BB#129:                               # %if.then656
	movq	$946, %rcx              # imm = 0x3B2
	movl	%ecx, floattab+7112(%rip)
.LBB0_130:                              # %if.end657
	movq	-8(%rsp), %rcx          # 8-byte Reload
	testq	%rdx, %rcx
	movabsq	$0, %rcx
	movabsq	$1, %rbx
	movq	%rcx, %rsi
	jne	.LBB0_132
# BB#131:                               # %if.end657
	movq	%rbx, %rsi
.LBB0_132:                              # %if.end657
	testq	%rdx, %r15
	jne	.LBB0_134
# BB#133:                               # %if.end657
	movq	%rbx, %rcx
.LBB0_134:                              # %if.end657
	orq	%rsi, %rcx
	cmpq	$1, %rcx
	jne	.LBB0_136
# BB#135:                               # %if.end657
	testq	%rdx, %r10
	je	.LBB0_137
.LBB0_136:                              # %if.then666
	movq	$947, %rcx              # imm = 0x3B3
	movl	%ecx, floattab+7120(%rip)
.LBB0_137:                              # %if.end667
	testq	%rdx, %r9
	je	.LBB0_139
# BB#138:                               # %if.then673
	movq	$948, %rcx              # imm = 0x3B4
	movl	%ecx, floattab+7584(%rip)
.LBB0_139:                              # %if.end674
	andq	%rdx, %rax
	cmpq	$33816576, %rax         # imm = 0x2040000
	movl	x86_himode_math(%rip), %edi
	je	.LBB0_141
# BB#140:                               # %if.end674
	testq	%rdx, %r10
	je	.LBB0_142
.LBB0_141:                              # %if.then683
	movq	$949, %rax              # imm = 0x3B5
	movl	%eax, floattab+7592(%rip)
.LBB0_142:                              # %if.end684
	movq	optab_table(%rip), %rax
	movq	$951, %rcx              # imm = 0x3B7
	movl	%ecx, 88(%rax)
	movq	$953, %rcx              # imm = 0x3B9
	movl	%ecx, 72(%rax)
	movl	ix86_cpu(%rip), %ecx
	movq	$1, %rsi
	movq	$1, %rbx
	shlq	%cl, %rbx
	andq	%rdi, %rbx
	testq	%rdx, %rbx
	je	.LBB0_144
# BB#143:                               # %if.then694
	movq	$963, %rcx              # imm = 0x3C3
	movl	%ecx, 56(%rax)
	movl	ix86_cpu(%rip), %ecx
.LBB0_144:                              # %if.end698
	shlq	%cl, %rsi
	andq	%r13, %rsi
	testq	%rdx, %rsi
	je	.LBB0_146
# BB#145:                               # %if.then702
	movq	$964, %rcx              # imm = 0x3C4
	movl	%ecx, 40(%rax)
.LBB0_146:                              # %if.end706
	cmpq	$1, %r14
	jne	.LBB0_148
# BB#147:                               # %if.then712
	movq	$965, %rcx              # imm = 0x3C5
	movl	%ecx, 280(%rax)
	movq	optab_table+8(%rip), %rsi
	movl	%ecx, 280(%rsi)
.LBB0_148:                              # %if.end719
	testq	%rdx, %r10
	je	.LBB0_150
# BB#149:                               # %if.end729
	movq	$966, %rcx              # imm = 0x3C6
	movl	%ecx, 296(%rax)
	movq	optab_table+8(%rip), %rsi
	movl	%ecx, 296(%rsi)
	jmp	.LBB0_152
.LBB0_150:                              # %lor.lhs.false732
	testq	$262144, %r8            # imm = 0x40000
	je	.LBB0_154
# BB#151:                               # %land.lhs.true735
	movzbl	ix86_fpmath(%rip), %ecx
	testq	$2, %rcx
	je	.LBB0_153
.LBB0_152:                              # %if.then738
	movq	$967, %rcx              # imm = 0x3C7
	movl	%ecx, 264(%rax)
	movq	optab_table+8(%rip), %rsi
	movl	%ecx, 264(%rsi)
.LBB0_153:                              # %if.end745
	testq	%rdx, %r10
	jne	.LBB0_155
.LBB0_154:                              # %lor.lhs.false748
	movzbl	ix86_fpmath(%rip), %ecx
	testq	$2, %rcx
	je	.LBB0_156
.LBB0_155:                              # %if.then751
	movq	$968, %rcx              # imm = 0x3C8
	movl	%ecx, 248(%rax)
	movq	optab_table+8(%rip), %rax
	movl	%ecx, 248(%rax)
.LBB0_156:                              # %if.end758
	movq	optab_table+16(%rip), %rax
	movq	$969, %rcx              # imm = 0x3C9
	movl	%ecx, 88(%rax)
	movq	$971, %rcx              # imm = 0x3CB
	movl	%ecx, 72(%rax)
	movl	ix86_cpu(%rip), %ecx
	movq	$1, %rsi
	movq	$1, %rbx
	shlq	%cl, %rbx
	andq	%rdi, %rbx
	testq	%rdx, %rbx
	je	.LBB0_158
# BB#157:                               # %if.then768
	movq	$972, %rcx              # imm = 0x3CC
	movl	%ecx, 56(%rax)
	movl	ix86_cpu(%rip), %ecx
.LBB0_158:                              # %if.end772
	shlq	%cl, %rsi
	andq	%r13, %rsi
	testq	%rdx, %rsi
	je	.LBB0_160
# BB#159:                               # %if.then776
	movq	$973, %rcx              # imm = 0x3CD
	movl	%ecx, 40(%rax)
.LBB0_160:                              # %if.end780
	cmpq	$1, %r14
	jne	.LBB0_162
# BB#161:                               # %if.then786
	movq	$974, %rcx              # imm = 0x3CE
	movl	%ecx, 280(%rax)
	movq	optab_table+24(%rip), %rsi
	movl	%ecx, 280(%rsi)
.LBB0_162:                              # %if.end793
	testq	%rdx, %r10
	je	.LBB0_164
# BB#163:                               # %if.end803
	movq	$975, %rcx              # imm = 0x3CF
	movl	%ecx, 296(%rax)
	movq	optab_table+24(%rip), %rsi
	movl	%ecx, 296(%rsi)
	jmp	.LBB0_166
.LBB0_164:                              # %lor.lhs.false806
	testq	$262144, %r8            # imm = 0x40000
	je	.LBB0_168
# BB#165:                               # %land.lhs.true809
	movzbl	ix86_fpmath(%rip), %ecx
	testq	$2, %rcx
	je	.LBB0_167
.LBB0_166:                              # %if.then812
	movq	$976, %rcx              # imm = 0x3D0
	movl	%ecx, 264(%rax)
	movq	optab_table+24(%rip), %rsi
	movl	%ecx, 264(%rsi)
.LBB0_167:                              # %if.end819
	testq	%rdx, %r10
	jne	.LBB0_169
.LBB0_168:                              # %lor.lhs.false822
	movzbl	ix86_fpmath(%rip), %ecx
	testq	$2, %rcx
	je	.LBB0_170
.LBB0_169:                              # %if.then825
	movq	$977, %rcx              # imm = 0x3D1
	movl	%ecx, 248(%rax)
	movq	optab_table+24(%rip), %rax
	movl	%ecx, 248(%rax)
.LBB0_170:                              # %if.end832
	movq	optab_table+32(%rip), %rax
	testq	%rdx, %r15
	je	.LBB0_172
# BB#171:                               # %if.then835
	movq	$978, %rcx              # imm = 0x3D2
	movl	%ecx, 88(%rax)
.LBB0_172:                              # %if.end839
	movq	$979, %rcx              # imm = 0x3D3
	movl	%ecx, 72(%rax)
	movl	ix86_cpu(%rip), %ecx
	movq	$1, %rsi
	movq	$1, %rbx
	shlq	%cl, %rbx
	andq	%rdi, %rbx
	testq	%rdx, %rbx
	je	.LBB0_174
# BB#173:                               # %if.then846
	movq	$980, %rcx              # imm = 0x3D4
	movl	%ecx, 56(%rax)
	movl	ix86_cpu(%rip), %ecx
.LBB0_174:                              # %if.end850
	shlq	%cl, %rsi
	andq	%r13, %rsi
	testq	%rdx, %rsi
	je	.LBB0_176
# BB#175:                               # %if.then854
	movq	$981, %rcx              # imm = 0x3D5
	movl	%ecx, 40(%rax)
	movl	ix86_cpu(%rip), %ecx
.LBB0_176:                              # %if.end858
	movq	$1, %rsi
	movq	$1, %rbx
	shlq	%cl, %rbx
	andq	%r13, %rbx
	testq	%rdx, %rbx
	je	.LBB0_178
# BB#177:                               # %if.then862
	movq	optab_table+72(%rip), %rcx
	movq	$982, %rbx              # imm = 0x3D6
	movl	%ebx, 56(%rcx)
	movl	ix86_cpu(%rip), %ecx
.LBB0_178:                              # %if.end866
	shlq	%cl, %rsi
	andq	%r13, %rsi
	testq	%rdx, %rsi
	je	.LBB0_180
# BB#179:                               # %if.then870
	movq	optab_table+64(%rip), %rcx
	movq	$983, %rsi              # imm = 0x3D7
	movl	%esi, 56(%rcx)
.LBB0_180:                              # %if.end874
	movq	optab_table+72(%rip), %rcx
	testq	%rdx, %r15
	je	.LBB0_181
# BB#182:                               # %if.then915
	movq	$984, %rsi              # imm = 0x3D8
	movl	%esi, 104(%rcx)
	movq	optab_table+64(%rip), %rcx
	movq	$986, %rsi              # imm = 0x3DA
	movl	%esi, 104(%rcx)
	movq	optab_table+56(%rip), %rcx
	movq	$988, %rsi              # imm = 0x3DC
	movl	%esi, 88(%rcx)
	movq	$989, %rsi              # imm = 0x3DD
	movl	%esi, 72(%rcx)
	movq	optab_table+48(%rip), %rcx
	movq	$990, %rsi              # imm = 0x3DE
	movl	%esi, 88(%rcx)
	jmp	.LBB0_183
.LBB0_181:                              # %if.end909.if.end919_crit_edge
	movq	$985, %rsi              # imm = 0x3D9
	movl	%esi, 88(%rcx)
	movq	optab_table+64(%rip), %rcx
	movq	$987, %rsi              # imm = 0x3DB
	movl	%esi, 88(%rcx)
	movq	optab_table+56(%rip), %rcx
	movq	$989, %rsi              # imm = 0x3DD
	movl	%esi, 72(%rcx)
	movq	optab_table+48(%rip), %rcx
.LBB0_183:                              # %if.end919
	cmpq	$1, %r14
	movq	$991, %rsi              # imm = 0x3DF
	movl	%esi, 72(%rcx)
	movq	%rdi, %r9
	jne	.LBB0_185
# BB#184:                               # %if.then928
	movq	$992, %rcx              # imm = 0x3E0
	movl	%ecx, 280(%rax)
	movq	optab_table+40(%rip), %rsi
	movl	%ecx, 280(%rsi)
.LBB0_185:                              # %if.end935
	testq	%rdx, %r10
	je	.LBB0_187
# BB#186:                               # %if.end945
	movq	$993, %rcx              # imm = 0x3E1
	movl	%ecx, 296(%rax)
	movq	optab_table+40(%rip), %rsi
	movl	%ecx, 296(%rsi)
	jmp	.LBB0_189
.LBB0_187:                              # %lor.lhs.false948
	testq	$262144, %r8            # imm = 0x40000
	je	.LBB0_191
# BB#188:                               # %land.lhs.true951
	movzbl	ix86_fpmath(%rip), %ecx
	testq	$2, %rcx
	je	.LBB0_190
.LBB0_189:                              # %if.then954
	movq	$994, %rcx              # imm = 0x3E2
	movl	%ecx, 264(%rax)
	movq	optab_table+40(%rip), %rsi
	movl	%ecx, 264(%rsi)
.LBB0_190:                              # %if.end961
	testq	%rdx, %r10
	jne	.LBB0_192
.LBB0_191:                              # %lor.lhs.false964
	movzbl	ix86_fpmath(%rip), %ecx
	testq	$2, %rcx
	je	.LBB0_193
.LBB0_192:                              # %if.then967
	movq	$995, %rcx              # imm = 0x3E3
	movl	%ecx, 248(%rax)
	movq	optab_table+40(%rip), %rax
	movl	%ecx, 248(%rax)
.LBB0_193:                              # %if.end974
	cmpq	$1, %r14
	jne	.LBB0_195
# BB#194:                               # %if.then980
	movq	optab_table+80(%rip), %rax
	movq	$996, %rcx              # imm = 0x3E4
	movl	%ecx, 280(%rax)
.LBB0_195:                              # %if.end984
	testq	%rdx, %r10
	je	.LBB0_197
# BB#196:                               # %if.end991
	movq	optab_table+80(%rip), %rax
	movq	$997, %rcx              # imm = 0x3E5
	movl	%ecx, 296(%rax)
	jmp	.LBB0_199
.LBB0_197:                              # %lor.lhs.false994
	testq	$262144, %r8            # imm = 0x40000
	je	.LBB0_201
# BB#198:                               # %land.lhs.true997
	movzbl	ix86_fpmath(%rip), %eax
	testq	$2, %rax
	je	.LBB0_200
.LBB0_199:                              # %if.then1000
	movq	optab_table+80(%rip), %rax
	movq	$998, %rcx              # imm = 0x3E6
	movl	%ecx, 264(%rax)
.LBB0_200:                              # %if.end1004
	testq	%rdx, %r10
	jne	.LBB0_202
.LBB0_201:                              # %lor.lhs.false1007
	movzbl	ix86_fpmath(%rip), %eax
	testq	$2, %rax
	je	.LBB0_203
.LBB0_202:                              # %if.then1010
	movq	optab_table+80(%rip), %rax
	movq	$999, %rcx              # imm = 0x3E7
	movl	%ecx, 248(%rax)
.LBB0_203:                              # %if.end1014
	movq	optab_table+96(%rip), %rax
	testq	%rdx, %r15
	je	.LBB0_205
# BB#204:                               # %if.then1017
	movq	$1000, %rcx             # imm = 0x3E8
	movl	%ecx, 88(%rax)
.LBB0_205:                              # %if.end1021
	movq	$1002, %rcx             # imm = 0x3EA
	movl	%ecx, 72(%rax)
	movl	ix86_cpu(%rip), %ecx
	movq	$1, %rax
	shlq	%cl, %rax
	andq	%r9, %rax
	testq	%rdx, %rax
	je	.LBB0_207
# BB#206:                               # %if.then1028
	movq	$1006, %rax             # imm = 0x3EE
	movq	-40(%rsp), %rcx         # 8-byte Reload
	movl	%eax, 56(%rcx)
.LBB0_207:                              # %if.end1032
	movq	optab_table+144(%rip), %rax
	testq	%rdx, %r15
	je	.LBB0_209
# BB#208:                               # %if.then1035
	movq	$1011, %rcx             # imm = 0x3F3
	movl	%ecx, 88(%rax)
.LBB0_209:                              # %if.end1039
	movq	$1012, %rcx             # imm = 0x3F4
	movl	%ecx, 72(%rax)
	movl	ix86_cpu(%rip), %ecx
	movq	$1, %rsi
	movq	$1, %rbx
	shlq	%cl, %rbx
	andq	%r9, %rbx
	testq	%rdx, %rbx
	je	.LBB0_211
# BB#210:                               # %if.then1046
	movq	$1016, %rcx             # imm = 0x3F8
	movl	%ecx, 56(%rax)
	movl	ix86_cpu(%rip), %ecx
.LBB0_211:                              # %if.end1050
	shlq	%cl, %rsi
	andq	%r13, %rsi
	testq	%rdx, %rsi
	je	.LBB0_213
# BB#212:                               # %if.then1054
	movq	$1017, %rcx             # imm = 0x3F9
	movl	%ecx, 40(%rax)
.LBB0_213:                              # %if.end1058
	movq	optab_table+152(%rip), %rax
	testq	%rdx, %r15
	je	.LBB0_215
# BB#214:                               # %if.then1061
	movq	$1018, %rcx             # imm = 0x3FA
	movl	%ecx, 88(%rax)
.LBB0_215:                              # %if.end1065
	movq	$1019, %rcx             # imm = 0x3FB
	movl	%ecx, 72(%rax)
	movl	ix86_cpu(%rip), %ecx
	movq	$1, %rsi
	movq	$1, %rbx
	shlq	%cl, %rbx
	andq	%r9, %rbx
	testq	%rdx, %rbx
	je	.LBB0_217
# BB#216:                               # %if.then1072
	movq	$1020, %rcx             # imm = 0x3FC
	movl	%ecx, 56(%rax)
	movl	ix86_cpu(%rip), %ecx
.LBB0_217:                              # %if.end1076
	shlq	%cl, %rsi
	andq	%r13, %rsi
	testq	%rdx, %rsi
	je	.LBB0_219
# BB#218:                               # %if.then1080
	movq	$1021, %rcx             # imm = 0x3FD
	movl	%ecx, 40(%rax)
.LBB0_219:                              # %if.end1084
	movq	optab_table+160(%rip), %rax
	testq	%rdx, %r15
	je	.LBB0_221
# BB#220:                               # %if.then1087
	movq	$1022, %rcx             # imm = 0x3FE
	movl	%ecx, 88(%rax)
.LBB0_221:                              # %if.end1091
	movq	$1023, %rcx             # imm = 0x3FF
	movl	%ecx, 72(%rax)
	movl	ix86_cpu(%rip), %ecx
	movq	$1, %rsi
	movq	$1, %rbx
	shlq	%cl, %rbx
	andq	%r9, %rbx
	testq	%rdx, %rbx
	je	.LBB0_223
# BB#222:                               # %if.then1098
	movq	$1024, %rcx             # imm = 0x400
	movl	%ecx, 56(%rax)
	movl	ix86_cpu(%rip), %ecx
.LBB0_223:                              # %if.end1102
	shlq	%cl, %rsi
	andq	%r13, %rsi
	testq	%rdx, %rsi
	je	.LBB0_225
# BB#224:                               # %if.then1106
	movq	$1025, %rcx             # imm = 0x401
	movl	%ecx, 40(%rax)
.LBB0_225:                              # %if.end1110
	movq	optab_table+256(%rip), %rax
	movq	$1027, %rcx             # imm = 0x403
	movl	%ecx, 88(%rax)
	movq	$1029, %rcx             # imm = 0x405
	movl	%ecx, 72(%rax)
	movl	ix86_cpu(%rip), %ecx
	movq	$1, %rsi
	movq	$1, %rbx
	shlq	%cl, %rbx
	andq	%r9, %rbx
	testq	%rdx, %rbx
	je	.LBB0_227
# BB#226:                               # %if.then1120
	movq	$1030, %rcx             # imm = 0x406
	movl	%ecx, 56(%rax)
	movl	ix86_cpu(%rip), %ecx
.LBB0_227:                              # %if.end1124
	shlq	%cl, %rsi
	andq	%r13, %rsi
	testq	%rdx, %rsi
	je	.LBB0_229
# BB#228:                               # %if.then1128
	movq	$1031, %rcx             # imm = 0x407
	movl	%ecx, 40(%rax)
.LBB0_229:                              # %if.end1132
	testq	%rdx, %r10
	je	.LBB0_231
# BB#230:                               # %if.then1145
	movq	$1032, %rcx             # imm = 0x408
	movl	%ecx, 248(%rax)
	movq	optab_table+264(%rip), %rsi
	movl	%ecx, 248(%rsi)
	movq	$1039, %rcx             # imm = 0x40F
	movl	%ecx, 264(%rax)
	movq	optab_table+264(%rip), %rsi
	movl	%ecx, 264(%rsi)
.LBB0_231:                              # %if.end1152
	cmpq	$1, %r14
	jne	.LBB0_233
# BB#232:                               # %if.then1158
	movq	$1046, %rcx             # imm = 0x416
	movl	%ecx, 280(%rax)
	movq	optab_table+264(%rip), %rsi
	movl	%ecx, 280(%rsi)
.LBB0_233:                              # %if.end1165
	testq	%rdx, %r10
	je	.LBB0_235
# BB#234:                               # %if.then1188
	movq	$1047, %rcx             # imm = 0x417
	movl	%ecx, 296(%rax)
	movq	optab_table+264(%rip), %rax
	movl	%ecx, 296(%rax)
	movq	optab_table+272(%rip), %rax
	movq	$1052, %rcx             # imm = 0x41C
	movl	%ecx, 248(%rax)
	movq	optab_table+280(%rip), %rax
	movl	%ecx, 248(%rax)
	movq	optab_table+272(%rip), %rax
	movq	$1059, %rcx             # imm = 0x423
	movl	%ecx, 264(%rax)
	movq	optab_table+280(%rip), %rax
	movl	%ecx, 264(%rax)
.LBB0_235:                              # %if.end1195
	cmpq	$1, %r14
	jne	.LBB0_237
# BB#236:                               # %if.then1201
	movq	optab_table+272(%rip), %rax
	movq	$1065, %rcx             # imm = 0x429
	movl	%ecx, 280(%rax)
	movq	optab_table+280(%rip), %rax
	movl	%ecx, 280(%rax)
.LBB0_237:                              # %if.end1208
	testq	%rdx, %r10
	je	.LBB0_239
# BB#238:                               # %if.then1211
	movq	optab_table+272(%rip), %rax
	movq	$1066, %rcx             # imm = 0x42A
	movl	%ecx, 296(%rax)
	movq	optab_table+280(%rip), %rax
	movl	%ecx, 296(%rax)
.LBB0_239:                              # %if.end1218
	movq	optab_table+288(%rip), %rax
	testq	%rdx, %r15
	je	.LBB0_241
# BB#240:                               # %if.then1221
	movq	$1071, %rcx             # imm = 0x42F
	movl	%ecx, 88(%rax)
.LBB0_241:                              # %if.end1225
	movq	$1073, %rcx             # imm = 0x431
	movl	%ecx, 72(%rax)
	movl	ix86_cpu(%rip), %ecx
	movq	$1, %rsi
	movq	$1, %rbx
	shlq	%cl, %rbx
	andq	%r9, %rbx
	testq	%rdx, %rbx
	je	.LBB0_243
# BB#242:                               # %if.then1232
	movq	$1076, %rcx             # imm = 0x434
	movl	%ecx, 56(%rax)
	movl	ix86_cpu(%rip), %ecx
.LBB0_243:                              # %if.end1236
	shlq	%cl, %rsi
	andq	%r13, %rsi
	testq	%rdx, %rsi
	je	.LBB0_245
# BB#244:                               # %if.then1240
	movq	$1078, %rcx             # imm = 0x436
	movl	%ecx, 40(%rax)
.LBB0_245:                              # %if.end1244
	movq	optab_table+168(%rip), %rax
	movq	$1080, %rcx             # imm = 0x438
	movl	%ecx, 88(%rax)
	movq	$1086, %rcx             # imm = 0x43E
	movl	%ecx, 72(%rax)
	movl	ix86_cpu(%rip), %ecx
	movq	$1, %rsi
	movq	$1, %rbx
	shlq	%cl, %rbx
	andq	%r9, %rbx
	testq	%rdx, %rbx
	je	.LBB0_247
# BB#246:                               # %if.then1254
	movq	$1089, %rcx             # imm = 0x441
	movl	%ecx, 56(%rax)
	movl	ix86_cpu(%rip), %ecx
.LBB0_247:                              # %if.end1258
	shlq	%cl, %rsi
	andq	%r13, %rsi
	testq	%rdx, %rsi
	je	.LBB0_249
# BB#248:                               # %if.then1262
	movq	$1090, %rcx             # imm = 0x442
	movl	%ecx, 40(%rax)
.LBB0_249:                              # %if.end1266
	movq	optab_table+184(%rip), %rax
	movq	$1091, %rcx             # imm = 0x443
	movl	%ecx, 88(%rax)
	movq	$1095, %rcx             # imm = 0x447
	movl	%ecx, 72(%rax)
	movl	ix86_cpu(%rip), %ecx
	movq	$1, %rsi
	movq	$1, %rbx
	shlq	%cl, %rbx
	andq	%r9, %rbx
	testq	%rdx, %rbx
	je	.LBB0_251
# BB#250:                               # %if.then1276
	movq	$1096, %rcx             # imm = 0x448
	movl	%ecx, 56(%rax)
	movl	ix86_cpu(%rip), %ecx
.LBB0_251:                              # %if.end1280
	shlq	%cl, %rsi
	andq	%r13, %rsi
	testq	%rdx, %rsi
	je	.LBB0_253
# BB#252:                               # %if.then1284
	movq	$1097, %rcx             # imm = 0x449
	movl	%ecx, 40(%rax)
.LBB0_253:                              # %if.end1288
	movq	optab_table+176(%rip), %rax
	movq	$1098, %rcx             # imm = 0x44A
	movl	%ecx, 88(%rax)
	movq	$1101, %rcx             # imm = 0x44D
	movl	%ecx, 72(%rax)
	movl	ix86_cpu(%rip), %ecx
	movq	$1, %rsi
	movq	$1, %rbx
	shlq	%cl, %rbx
	andq	%r9, %rbx
	testq	%rdx, %rbx
	je	.LBB0_255
# BB#254:                               # %if.then1298
	movq	$1102, %rcx             # imm = 0x44E
	movl	%ecx, 56(%rax)
	movl	ix86_cpu(%rip), %ecx
.LBB0_255:                              # %if.end1302
	shlq	%cl, %rsi
	andq	%r13, %rsi
	testq	%rdx, %rsi
	je	.LBB0_257
# BB#256:                               # %if.then1306
	movq	$1103, %rcx             # imm = 0x44F
	movl	%ecx, 40(%rax)
.LBB0_257:                              # %if.end1310
	movq	optab_table+192(%rip), %rax
	testq	%rdx, %r15
	je	.LBB0_259
# BB#258:                               # %if.then1313
	movq	$1104, %rcx             # imm = 0x450
	movl	%ecx, 88(%rax)
.LBB0_259:                              # %if.end1317
	movq	$1105, %rcx             # imm = 0x451
	movl	%ecx, 72(%rax)
	movl	ix86_cpu(%rip), %ecx
	movq	$1, %rsi
	movq	$1, %rbx
	shlq	%cl, %rbx
	andq	%r9, %rbx
	testq	%rdx, %rbx
	je	.LBB0_261
# BB#260:                               # %if.then1324
	movq	$1106, %rcx             # imm = 0x452
	movl	%ecx, 56(%rax)
	movl	ix86_cpu(%rip), %ecx
.LBB0_261:                              # %if.end1328
	shlq	%cl, %rsi
	andq	%r13, %rsi
	testq	%rdx, %rsi
	je	.LBB0_263
# BB#262:                               # %if.then1332
	movq	$1107, %rcx             # imm = 0x453
	movl	%ecx, 40(%rax)
.LBB0_263:                              # %if.end1336
	movq	optab_table+200(%rip), %rax
	testq	%rdx, %r15
	je	.LBB0_265
# BB#264:                               # %if.then1339
	movq	$1108, %rcx             # imm = 0x454
	movl	%ecx, 88(%rax)
.LBB0_265:                              # %if.end1343
	movq	$1109, %rcx             # imm = 0x455
	movl	%ecx, 72(%rax)
	movl	ix86_cpu(%rip), %ecx
	movq	$1, %rsi
	movq	$1, %rbx
	shlq	%cl, %rbx
	andq	%r9, %rbx
	testq	%rdx, %rbx
	je	.LBB0_267
# BB#266:                               # %if.then1350
	movq	$1110, %rcx             # imm = 0x456
	movl	%ecx, 56(%rax)
	movl	ix86_cpu(%rip), %ecx
.LBB0_267:                              # %if.end1354
	shlq	%cl, %rsi
	andq	%r13, %rsi
	testq	%rdx, %rsi
	je	.LBB0_269
# BB#268:                               # %if.then1358
	movq	$1111, %rcx             # imm = 0x457
	movl	%ecx, 40(%rax)
.LBB0_269:                              # %if.end1362
	testq	%rdx, %r12
	movabsq	$4788888536156, %rax    # imm = 0x45B0000045C
	movq	%rax, setcc_gen_code+408(%rip)
	movabsq	$4797478470753, %rax    # imm = 0x45D00000461
	movq	%rax, setcc_gen_code+416(%rip)
	movq	$1118, %rax             # imm = 0x45E
	movl	%eax, setcc_gen_code+436(%rip)
	movabsq	$4806068405347, %rax    # imm = 0x45F00000463
	movq	%rax, setcc_gen_code+424(%rip)
	movq	$1120, %rax             # imm = 0x460
	movl	%eax, setcc_gen_code+444(%rip)
	movq	$1122, %rax             # imm = 0x462
	movl	%eax, setcc_gen_code+432(%rip)
	movq	$1124, %rax             # imm = 0x464
	movl	%eax, setcc_gen_code+440(%rip)
	je	.LBB0_271
# BB#270:                               # %if.then1368
	movq	$1125, %rax             # imm = 0x465
	movl	%eax, setcc_gen_code+448(%rip)
.LBB0_271:                              # %if.end1369
	testq	%rdx, %r10
	je	.LBB0_273
# BB#272:                               # %if.then1372
	movq	$1126, %rax             # imm = 0x466
	movl	%eax, setcc_gen_code+452(%rip)
.LBB0_273:                              # %if.end1373
	testq	%rdx, %r12
	je	.LBB0_275
# BB#274:                               # %if.then1414
	movq	$1127, %rax             # imm = 0x467
	movl	%eax, setcc_gen_code+456(%rip)
	movq	$1128, %rax             # imm = 0x468
	movl	%eax, setcc_gen_code+460(%rip)
	movq	$1129, %rax             # imm = 0x469
	movl	%eax, setcc_gen_code+464(%rip)
	movq	$1130, %rax             # imm = 0x46A
	movl	%eax, setcc_gen_code+468(%rip)
	movq	$1131, %rax             # imm = 0x46B
	movl	%eax, setcc_gen_code+472(%rip)
	movq	$1132, %rax             # imm = 0x46C
	movl	%eax, setcc_gen_code+476(%rip)
	movq	$gen_beq, bcc_gen_fctn+824(%rip)
	movq	$gen_bne, bcc_gen_fctn+816(%rip)
	movq	$gen_bgt, bcc_gen_fctn+840(%rip)
	movq	$gen_bgtu, bcc_gen_fctn+872(%rip)
	movq	$gen_blt, bcc_gen_fctn+856(%rip)
	movq	$gen_bltu, bcc_gen_fctn+888(%rip)
	movq	$gen_bge, bcc_gen_fctn+832(%rip)
	movq	$gen_bgeu, bcc_gen_fctn+864(%rip)
	movq	$gen_ble, bcc_gen_fctn+848(%rip)
	movq	$gen_bleu, bcc_gen_fctn+880(%rip)
	movq	$gen_bunordered, bcc_gen_fctn+896(%rip)
	movq	$gen_bordered, bcc_gen_fctn+904(%rip)
	movq	$gen_buneq, bcc_gen_fctn+912(%rip)
	movq	$gen_bunge, bcc_gen_fctn+920(%rip)
	movq	$gen_bungt, bcc_gen_fctn+928(%rip)
	movq	$gen_bunle, bcc_gen_fctn+936(%rip)
	movq	$gen_bunlt, bcc_gen_fctn+944(%rip)
	movq	$gen_bltgt, bcc_gen_fctn+952(%rip)
	jmp	.LBB0_276
.LBB0_275:                              # %if.end1471.critedge
	movq	$gen_beq, bcc_gen_fctn+824(%rip)
	movq	$gen_bne, bcc_gen_fctn+816(%rip)
	movq	$gen_bgt, bcc_gen_fctn+840(%rip)
	movq	$gen_bgtu, bcc_gen_fctn+872(%rip)
	movq	$gen_blt, bcc_gen_fctn+856(%rip)
	movq	$gen_bltu, bcc_gen_fctn+888(%rip)
	movq	$gen_bge, bcc_gen_fctn+832(%rip)
	movq	$gen_bgeu, bcc_gen_fctn+864(%rip)
	movq	$gen_ble, bcc_gen_fctn+848(%rip)
	movq	$gen_bleu, bcc_gen_fctn+880(%rip)
.LBB0_276:                              # %if.end1471
	movq	optab_table+296(%rip), %rax
	movq	-24(%rsp), %rcx         # 8-byte Reload
	andq	%rdx, %rcx
	cmpq	$1, %rcx
	movq	$1180, %rcx             # imm = 0x49C
	movl	%ecx, 72(%rax)
	jne	.LBB0_278
# BB#277:                               # %if.end1487.thread1756
	movq	optab_table+304(%rip), %rax
	movq	$1183, %rcx             # imm = 0x49F
	movl	%ecx, 248(%rax)
	jmp	.LBB0_282
.LBB0_278:                              # %lor.lhs.false1480
	movzbl	ix86_fpmath(%rip), %eax
	testq	$2, %rax
	je	.LBB0_280
# BB#279:                               # %if.end1487.thread1758
	movq	optab_table+304(%rip), %rax
	movq	$1183, %rcx             # imm = 0x49F
	movl	%ecx, 248(%rax)
.LBB0_280:                              # %lor.lhs.false1493
	testq	$262144, %r8            # imm = 0x40000
	je	.LBB0_283
# BB#281:                               # %land.lhs.true1496
	movzbl	ix86_fpmath(%rip), %eax
	testq	$2, %rax
	je	.LBB0_283
.LBB0_282:                              # %if.then1499
	movq	optab_table+304(%rip), %rax
	movq	$1184, %rcx             # imm = 0x4A0
	movl	%ecx, 264(%rax)
.LBB0_283:                              # %if.end1503
	testq	%rdx, %r15
	movq	$1185, %rax             # imm = 0x4A1
	movl	%eax, movstr_optab+16(%rip)
	movq	-8(%rsp), %rbx          # 8-byte Reload
	je	.LBB0_284
# BB#285:                               # %if.then1520
	movq	$1186, %rax             # imm = 0x4A2
	movl	%eax, movstr_optab+20(%rip)
	movabsq	$5132485919914, %rax    # imm = 0x4AB000004AA
	movq	%rax, clrstr_optab+16(%rip)
	movq	optab_table+352(%rip), %rax
	movq	$1205, %rcx             # imm = 0x4B5
	movl	%ecx, 72(%rax)
	movq	$1206, %rcx             # imm = 0x4B6
	movl	%ecx, 88(%rax)
	movq	$1209, %rax             # imm = 0x4B9
	movl	%eax, movcc_gen_code+20(%rip)
	jmp	.LBB0_286
.LBB0_284:                              # %if.end1511.thread
	movq	$1194, %rax             # imm = 0x4AA
	movl	%eax, clrstr_optab+16(%rip)
	movq	optab_table+352(%rip), %rax
	movq	$1205, %rcx             # imm = 0x4B5
	movl	%ecx, 72(%rax)
	movq	$1206, %rcx             # imm = 0x4B6
	movl	%ecx, 88(%rax)
.LBB0_286:                              # %if.end1521
	movq	$1210, %rax             # imm = 0x4BA
	movl	ix86_arch(%rip), %ecx
	movl	x86_cmove(%rip), %esi
	movl	%eax, movcc_gen_code+16(%rip)
	movq	$1, %rax
	shlq	%cl, %rax
	andq	%rsi, %rax
	movq	$1, %rsi
	movq	%rax, %rdi
	orq	%rbx, %rdi
	testq	%rdx, %rdi
	je	.LBB0_295
# BB#287:                               # %land.lhs.true1528
	movl	ix86_cpu(%rip), %ecx
	shlq	%cl, %rsi
	andq	%r9, %rsi
	testq	%rdx, %rsi
	je	.LBB0_289
# BB#288:                               # %if.then1532
	movq	$1211, %rcx             # imm = 0x4BB
	movl	%ecx, movcc_gen_code+12(%rip)
.LBB0_289:                              # %if.end1533
	testq	%rdx, %rbx
	je	.LBB0_290
# BB#291:                               # %if.end1533
	movabsq	$1, %rcx
	jmp	.LBB0_292
.LBB0_290:
	movabsq	$0, %rcx
.LBB0_292:                              # %if.end1533
	testq	%rdx, %rax
	jne	.LBB0_294
# BB#293:                               # %if.end1533
	testq	%rcx, %rcx
	je	.LBB0_295
.LBB0_294:                              # %if.then1548
	movabsq	$5209795331260, %rcx    # imm = 0x4BD000004BC
	movq	%rcx, movcc_gen_code+60(%rip)
.LBB0_295:                              # %if.end1549
	testq	%rdx, %r15
	je	.LBB0_296
# BB#297:                               # %if.end1549
	movabsq	$1, %rcx
	jmp	.LBB0_298
.LBB0_296:
	movabsq	$0, %rcx
.LBB0_298:                              # %if.end1549
	testq	%rdx, %rdi
	je	.LBB0_301
# BB#299:                               # %if.end1549
	testq	%rcx, %rcx
	jne	.LBB0_301
# BB#300:                               # %if.then1559
	movq	$1215, %rcx             # imm = 0x4BF
	movl	%ecx, movcc_gen_code+68(%rip)
.LBB0_301:                              # %if.end1560
	testq	%rdx, %rax
	je	.LBB0_302
# BB#304:                               # %if.end1568
	testq	%rdx, %rbx
	movq	$1216, %rax             # imm = 0x4C0
	movl	%eax, movcc_gen_code+72(%rip)
	jne	.LBB0_305
	jmp	.LBB0_306
.LBB0_302:                              # %lor.lhs.false1564
	testq	%rdx, %rbx
	je	.LBB0_306
# BB#303:                               # %if.end1568.thread1762
	movq	$1216, %rax             # imm = 0x4C0
	movl	%eax, movcc_gen_code+72(%rip)
.LBB0_305:                              # %if.then1571
	movq	optab_table+208(%rip), %rax
	movq	$1217, %rcx             # imm = 0x4C1
	movl	%ecx, 248(%rax)
.LBB0_306:                              # %if.end1575
	movq	%r8, %rax
	andq	$262144, %rax           # imm = 0x40000
	je	.LBB0_309
# BB#307:                               # %land.lhs.true1578
	movzbl	ix86_fpmath(%rip), %ecx
	testq	$2, %rcx
	je	.LBB0_309
# BB#308:                               # %if.then1581
	movq	optab_table+208(%rip), %rcx
	movq	$1220, %rsi             # imm = 0x4C4
	movl	%esi, 264(%rcx)
.LBB0_309:                              # %if.end1585
	testq	%rdx, %rbx
	je	.LBB0_311
# BB#310:                               # %if.then1588
	movq	optab_table+216(%rip), %rcx
	movq	$1223, %rsi             # imm = 0x4C7
	movl	%esi, 248(%rcx)
.LBB0_311:                              # %if.end1592
	testq	%rdx, %rax
	je	.LBB0_314
# BB#312:                               # %land.lhs.true1595
	movzbl	ix86_fpmath(%rip), %eax
	testq	$2, %rax
	je	.LBB0_314
# BB#313:                               # %if.then1598
	movq	optab_table+216(%rip), %rax
	movq	$1226, %rcx             # imm = 0x4CA
	movl	%ecx, 264(%rax)
.LBB0_314:                              # %if.end1602
	testq	$33882112, %r8          # imm = 0x2050000
	movq	-16(%rsp), %rcx         # 8-byte Reload
	je	.LBB0_316
# BB#315:                               # %if.then1608
	movq	$1299, %rax             # imm = 0x513
	movl	%eax, 104(%r11)
.LBB0_316:                              # %if.end1612
	testq	%rdx, %rbx
	je	.LBB0_318
# BB#317:                               # %if.then1615
	movq	$1300, %rax             # imm = 0x514
	movl	%eax, 744(%r11)
.LBB0_318:                              # %if.end1619
	testq	%rdx, %rcx
	je	.LBB0_320
# BB#319:                               # %if.then1643
	movq	$1301, %rax             # imm = 0x515
	movl	%eax, 600(%r11)
	movq	$1302, %rax             # imm = 0x516
	movl	%eax, 536(%r11)
	movq	$1303, %rax             # imm = 0x517
	movl	%eax, 584(%r11)
	movq	$1304, %rax             # imm = 0x518
	movl	%eax, 632(%r11)
.LBB0_320:                              # %if.end1647
	movq	-32(%rsp), %rax         # 8-byte Reload
	testq	%rdx, %rax
	je	.LBB0_322
# BB#321:                               # %if.then1650
	movq	$1305, %rax             # imm = 0x519
	movl	%eax, 712(%r11)
.LBB0_322:                              # %if.end1654
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp16:
	.size	init_all_optabs, .Ltmp16-init_all_optabs
	.cfi_endproc


	.ident	"clang version 3.5 (trunk)"
	.section	".note.GNU-stack","",@progbits
