	.file	"insn-extract.c.bc"
	.text
	.globl	insn_extract
	.align	16, 0x90
	.type	insn_extract,@function
insn_extract:                           # @insn_extract
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp3:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp4:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp5:
	.cfi_def_cfa_offset 32
.Ltmp6:
	.cfi_offset %rbx, -24
.Ltmp7:
	.cfi_offset %r14, -16
	movq	%rdi, %rbx
	movq	32(%rbx), %r14
	movabsq	$recog_data, %rdi
	xorq	%rsi, %rsi
	movq	$480, %rdx              # imm = 0x1E0
	callq	memset
	movl	40(%rbx), %ecx
	incq	%rcx
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	cmpq	$834, %rax              # imm = 0x342
	ja	.LBB0_220
# BB#1:                                 # %entry
	jmpq	*.LJTI0_0(,%rax,8)
.LBB0_200:                              # %sw.bb5585
	leaq	8(%r14), %rax
	movq	%rax, recog_data+240(%rip)
	movq	8(%r14), %rax
.LBB0_114:                              # %sw.epilog
	movq	%rax, recog_data(%rip)
	movq	16(%r14), %rax
	jmp	.LBB0_16
.LBB0_184:                              # %sw.bb5196
	movq	8(%r14), %rax
	movq	8(%rax), %rax
.LBB0_170:                              # %sw.epilog
	leaq	8(%rax), %rcx
	movq	%rcx, recog_data+240(%rip)
	movq	8(%rax), %rax
	movq	%rax, recog_data(%rip)
	movq	8(%r14), %rax
	movq	8(%rax), %rax
	movq	16(%rax), %rax
.LBB0_171:                              # %sw.epilog
	leaq	8(%rax), %rcx
	movq	%rcx, recog_data+248(%rip)
	movq	8(%rax), %rax
.LBB0_172:                              # %sw.epilog
	movq	%rax, recog_data+8(%rip)
	movq	8(%r14), %rax
	movq	8(%rax), %rax
.LBB0_102:                              # %sw.epilog
	movq	16(%rax), %rax
	jmp	.LBB0_103
.LBB0_52:                               # %sw.bb1006
	leaq	8(%r14), %rax
	movq	%rax, recog_data+240(%rip)
	movq	8(%r14), %rax
	movq	%rax, recog_data(%rip)
	movq	16(%r14), %rax
	jmp	.LBB0_58
.LBB0_220:                              # %sw.default
	movabsq	$.L.str, %rdi
	movabsq	$.L__FUNCTION__.insn_extract, %rdx
	movq	$1713, %rsi             # imm = 0x6B1
	callq	fancy_abort
.LBB0_206:                              # %sw.bb5645
	leaq	8(%r14), %rax
	movq	%rax, recog_data+240(%rip)
	movq	8(%r14), %rax
	jmp	.LBB0_207
.LBB0_182:                              # %sw.bb4783
	movq	8(%r14), %rax
	movq	16(%rax), %rax
	leaq	8(%rax), %rcx
	movq	%rcx, recog_data+240(%rip)
	movq	8(%rax), %rax
	movq	%rax, recog_data(%rip)
	movq	8(%r14), %rax
	movq	8(%rax), %rax
	movq	16(%rax), %rax
	movq	8(%rax), %rax
	leaq	8(%rax), %rcx
	movq	%rcx, recog_data+248(%rip)
	movq	8(%rax), %rax
	movq	%rax, recog_data+8(%rip)
	movq	8(%r14), %rax
	movq	8(%rax), %rax
	movq	16(%rax), %rax
	movq	8(%rax), %rax
.LBB0_154:                              # %sw.epilog
	leaq	16(%rax), %rcx
	movq	%rcx, recog_data+256(%rip)
	movq	16(%rax), %rax
	movq	%rax, recog_data+16(%rip)
.LBB0_155:                              # %sw.epilog
	movq	8(%r14), %rax
	movq	16(%rax), %rax
	movq	16(%rax), %rax
.LBB0_156:                              # %sw.epilog
	addq	$8, %rax
	movq	%rax, recog_data+960(%rip)
	movq	$1, %rax
	movb	%al, recog_data+992(%rip)
	movq	8(%r14), %rax
.LBB0_157:                              # %sw.epilog
	movq	16(%rax), %rax
	movq	16(%rax), %rax
	addq	$16, %rax
.LBB0_12:                               # %sw.epilog
	movq	%rax, recog_data+968(%rip)
	movq	$2, %rax
	movb	%al, recog_data+993(%rip)
	jmp	.LBB0_223
.LBB0_84:                               # %sw.bb2328
	leaq	8(%r14), %rax
	movq	%rax, recog_data+240(%rip)
	movq	8(%r14), %rax
.LBB0_85:                               # %sw.epilog
	movq	%rax, recog_data(%rip)
	movq	16(%r14), %rax
.LBB0_15:                               # %sw.epilog
	movq	8(%rax), %rax
	jmp	.LBB0_16
.LBB0_194:                              # %sw.bb5463
	movq	8(%r14), %rax
	movq	8(%rax), %rax
	leaq	8(%rax), %rcx
	movq	%rcx, recog_data+240(%rip)
	movq	8(%rax), %rax
	movq	%rax, recog_data(%rip)
	movq	8(%r14), %rax
.LBB0_195:                              # %sw.epilog
	movq	8(%rax), %rax
	movq	16(%rax), %rax
.LBB0_16:                               # %sw.epilog
	leaq	8(%rax), %rcx
	movq	%rcx, recog_data+248(%rip)
	movq	8(%rax), %rax
	jmp	.LBB0_222
.LBB0_218:                              # %sw.bb5823
	movq	16(%r14), %rax
.LBB0_219:                              # %sw.epilog
	leaq	8(%rax), %rcx
	movq	%rcx, recog_data+240(%rip)
	movq	8(%rax), %rax
	movq	%rax, recog_data(%rip)
	movq	16(%r14), %rax
	jmp	.LBB0_97
.LBB0_88:                               # %sw.bb2404
	leaq	8(%r14), %rax
	movq	%rax, recog_data+240(%rip)
	movq	8(%r14), %rax
	movq	%rax, recog_data(%rip)
	movq	16(%r14), %rax
	jmp	.LBB0_89
.LBB0_164:                              # %sw.bb4620
	movq	8(%r14), %rax
	movq	8(%rax), %rax
	jmp	.LBB0_165
.LBB0_28:                               # %sw.bb405
	leaq	8(%r14), %rax
	movq	%rax, recog_data+240(%rip)
	movq	8(%r14), %rax
	movq	%rax, recog_data(%rip)
	movq	16(%r14), %rax
.LBB0_29:                               # %sw.epilog
	movq	8(%rax), %rax
	movq	8(%rax), %rax
	leaq	8(%rax), %rcx
	movq	%rcx, recog_data+248(%rip)
	movq	8(%rax), %rax
	movq	%rax, recog_data+8(%rip)
	movq	16(%r14), %rax
	movq	8(%rax), %rax
	movq	8(%rax), %rax
	jmp	.LBB0_103
.LBB0_201:                              # %sw.bb5599
	movq	8(%r14), %rax
	leaq	8(%rax), %rcx
	movq	%rcx, recog_data+240(%rip)
	movq	8(%rax), %rax
.LBB0_207:                              # %sw.epilog
	movq	%rax, recog_data(%rip)
	leaq	16(%r14), %rax
	movq	%rax, recog_data+248(%rip)
	movq	16(%r14), %rax
	jmp	.LBB0_222
.LBB0_192:                              # %sw.bb5395
	movq	8(%r14), %rax
	movq	8(%rax), %rax
	leaq	8(%rax), %rcx
	movq	%rcx, recog_data+240(%rip)
	movq	8(%rax), %rax
	movq	%rax, recog_data(%rip)
	movq	8(%r14), %rax
	movq	8(%rax), %rax
	movq	16(%rax), %rax
	leaq	8(%rax), %rcx
	movq	%rcx, recog_data+248(%rip)
	movq	8(%rax), %rax
	movq	%rax, recog_data+8(%rip)
	movq	8(%r14), %rax
	jmp	.LBB0_34
.LBB0_168:                              # %sw.bb4663
	movq	8(%r14), %rax
	movq	16(%rax), %rax
.LBB0_165:                              # %sw.epilog
	leaq	8(%rax), %rcx
	movq	%rcx, recog_data+240(%rip)
	movq	8(%rax), %rax
	movq	%rax, recog_data(%rip)
	movq	8(%r14), %rax
	movq	8(%rax), %rax
	movq	16(%rax), %rax
	movq	8(%rax), %rax
	leaq	8(%rax), %rcx
	movq	%rcx, recog_data+248(%rip)
	movq	8(%rax), %rax
.LBB0_166:                              # %sw.epilog
	movq	%rax, recog_data+8(%rip)
	movq	8(%r14), %rax
	movq	8(%rax), %rax
	movq	16(%rax), %rax
	movq	8(%rax), %rax
	jmp	.LBB0_103
.LBB0_37:                               # %sw.bb631
	leaq	8(%r14), %rax
	movq	%rax, recog_data+240(%rip)
	movq	8(%r14), %rax
	jmp	.LBB0_38
.LBB0_199:                              # %sw.bb5548
	movq	8(%r14), %rax
	movq	8(%rax), %rax
	leaq	8(%rax), %rcx
	movq	%rcx, recog_data+240(%rip)
	movq	8(%rax), %rax
	movq	%rax, recog_data(%rip)
	movq	8(%r14), %rax
	movq	8(%rax), %rax
	leaq	16(%rax), %rcx
	movq	%rcx, recog_data+248(%rip)
	movq	16(%rax), %rax
	movq	%rax, recog_data+8(%rip)
	movq	8(%r14), %rax
	movq	16(%rax), %rax
	addq	$8, %rax
	movq	%rax, recog_data+960(%rip)
	movq	$1, %rax
	movb	%al, recog_data+992(%rip)
	movq	8(%r14), %rax
	movq	16(%rax), %rax
	addq	$16, %rax
	movq	%rax, recog_data+968(%rip)
	xorq	%rax, %rax
	movb	%al, recog_data+993(%rip)
	jmp	.LBB0_223
.LBB0_87:                               # %sw.bb2374
	leaq	8(%r14), %rax
	movq	%rax, recog_data+240(%rip)
	movq	8(%r14), %rax
	movq	%rax, recog_data(%rip)
	movq	16(%r14), %rax
	movq	8(%rax), %rax
.LBB0_89:                               # %sw.epilog
	leaq	8(%rax), %rcx
	movq	%rcx, recog_data+248(%rip)
	movq	8(%rax), %rax
	movq	%rax, recog_data+8(%rip)
	movq	16(%r14), %rax
	leaq	16(%rax), %rcx
	movq	%rcx, recog_data+256(%rip)
	movq	16(%rax), %rax
	jmp	.LBB0_90
.LBB0_86:                               # %sw.bb2344
	leaq	8(%r14), %rax
	movq	%rax, recog_data+240(%rip)
	movq	8(%r14), %rax
	movq	%rax, recog_data(%rip)
	movq	16(%r14), %rax
	leaq	8(%rax), %rcx
	movq	%rcx, recog_data+248(%rip)
	movq	8(%rax), %rax
	movq	%rax, recog_data+8(%rip)
	movq	16(%r14), %rax
	movq	16(%rax), %rax
	leaq	8(%rax), %rcx
	movq	%rcx, recog_data+256(%rip)
	movq	8(%rax), %rax
.LBB0_90:                               # %sw.epilog
	leaq	16(%r14), %rcx
	movq	%rax, recog_data+16(%rip)
	movq	%rcx, recog_data+264(%rip)
	movq	16(%r14), %rax
	movq	%rax, recog_data+24(%rip)
	jmp	.LBB0_223
.LBB0_216:                              # %sw.bb5800
	movq	16(%r14), %rax
.LBB0_217:                              # %sw.epilog
	movq	8(%rax), %rax
	leaq	8(%rax), %rcx
	movq	%rcx, recog_data+240(%rip)
	movq	8(%rax), %rax
	movq	%rax, recog_data(%rip)
	movq	16(%r14), %rax
	jmp	.LBB0_96
.LBB0_163:                              # %sw.bb4549
	movq	8(%r14), %rax
	movq	16(%rax), %rax
	leaq	8(%rax), %rcx
	movq	%rcx, recog_data+240(%rip)
	movq	8(%rax), %rax
	movq	%rax, recog_data(%rip)
	movq	8(%r14), %rax
	movq	8(%rax), %rax
	movq	16(%rax), %rax
	movq	8(%rax), %rax
	leaq	8(%rax), %rcx
	movq	%rcx, recog_data+248(%rip)
	movq	8(%rax), %rax
	movq	%rax, recog_data+8(%rip)
	movq	8(%r14), %rax
	movq	8(%rax), %rax
	movq	16(%rax), %rax
	movq	8(%rax), %rax
.LBB0_150:                              # %sw.epilog
	leaq	16(%rax), %rcx
	movq	%rcx, recog_data+256(%rip)
	movq	16(%rax), %rax
.LBB0_151:                              # %sw.epilog
	movq	%rax, recog_data+16(%rip)
	movq	8(%r14), %rax
	movq	16(%rax), %rax
	movq	16(%rax), %rax
	movq	8(%rax), %rax
	addq	$8, %rax
	movq	%rax, recog_data+960(%rip)
	movq	$1, %rax
	movb	%al, recog_data+992(%rip)
	movq	8(%r14), %rax
	movq	16(%rax), %rax
	movq	16(%rax), %rax
	movq	8(%rax), %rax
	addq	$16, %rax
	jmp	.LBB0_12
.LBB0_124:                              # %sw.bb3296
	movq	8(%r14), %rax
	movq	16(%rax), %rax
	leaq	8(%rax), %rcx
	movq	%rcx, recog_data+240(%rip)
	movq	8(%rax), %rax
	movq	%rax, recog_data(%rip)
	movq	8(%r14), %rax
	movq	8(%rax), %rax
	movq	16(%rax), %rax
	movq	8(%rax), %rax
	leaq	8(%rax), %rcx
	movq	%rcx, recog_data+248(%rip)
	movq	8(%rax), %rax
	movq	%rax, recog_data+8(%rip)
	jmp	.LBB0_125
.LBB0_67:                               # %sw.bb1520
	leaq	8(%r14), %rax
	movq	%rax, recog_data+240(%rip)
	movq	8(%r14), %rax
	movq	%rax, recog_data(%rip)
	movq	16(%r14), %rax
	leaq	8(%rax), %rcx
	movq	%rcx, recog_data+248(%rip)
	movq	8(%rax), %rax
	movq	%rax, recog_data+8(%rip)
	movq	16(%r14), %rax
	leaq	16(%rax), %rcx
	movq	%rcx, recog_data+256(%rip)
	movq	16(%rax), %rax
.LBB0_68:                               # %sw.epilog
	movq	%rax, recog_data+16(%rip)
	movq	16(%r14), %rax
.LBB0_69:                               # %sw.epilog
	leaq	24(%rax), %rcx
	movq	%rcx, recog_data+264(%rip)
	movq	24(%rax), %rax
	movq	%rax, recog_data+24(%rip)
	jmp	.LBB0_223
.LBB0_66:                               # %sw.bb1455
	movq	8(%r14), %rax
	movq	8(%rax), %rax
	leaq	8(%rax), %rcx
	movq	%rcx, recog_data+240(%rip)
	movq	8(%rax), %rax
	movq	%rax, recog_data(%rip)
	movq	8(%r14), %rax
	movq	8(%rax), %rax
	movq	16(%rax), %rax
	movq	8(%rax), %rax
	leaq	8(%rax), %rcx
	movq	%rcx, recog_data+248(%rip)
	movq	8(%rax), %rax
	movq	%rax, recog_data+8(%rip)
	movq	8(%r14), %rax
	movq	8(%rax), %rax
	movq	16(%rax), %rax
	movq	8(%rax), %rax
	leaq	16(%rax), %rcx
	movq	%rcx, recog_data+256(%rip)
	movq	16(%rax), %rax
	movq	%rax, recog_data+16(%rip)
	movq	8(%r14), %rax
	movq	8(%rax), %rax
	movq	16(%rax), %rax
	addq	$16, %rax
	movq	%rax, recog_data+960(%rip)
	movq	$1, %rax
	movb	%al, recog_data+992(%rip)
	movq	8(%r14), %rax
	movq	8(%rax), %rax
	movq	16(%rax), %rax
	addq	$24, %rax
	jmp	.LBB0_12
.LBB0_61:                               # %sw.bb1192
	leaq	8(%r14), %rax
	movq	%rax, recog_data+240(%rip)
	movq	8(%r14), %rax
	movq	%rax, recog_data(%rip)
	movq	16(%r14), %rax
	leaq	8(%rax), %rcx
	movq	%rcx, recog_data+248(%rip)
	movq	8(%rax), %rax
	movq	%rax, recog_data+8(%rip)
	movq	16(%r14), %rax
	leaq	16(%rax), %rcx
	movq	%rcx, recog_data+256(%rip)
	movq	16(%rax), %rax
	movq	%rax, recog_data+16(%rip)
	movq	16(%r14), %rax
	leaq	24(%rax), %rcx
	movq	%rcx, recog_data+264(%rip)
	movq	24(%rax), %rax
	movq	%rax, recog_data+24(%rip)
	movq	16(%r14), %rax
	movq	8(%rax), %rax
	leaq	8(%rax), %rcx
	movq	%rcx, recog_data+272(%rip)
	movq	8(%rax), %rax
	movq	%rax, recog_data+32(%rip)
	movq	16(%r14), %rax
	movq	8(%rax), %rax
	leaq	16(%rax), %rcx
	movq	%rcx, recog_data+280(%rip)
	movq	16(%rax), %rax
	movq	%rax, recog_data+40(%rip)
	jmp	.LBB0_223
.LBB0_83:                               # %sw.bb2240
	movq	8(%r14), %rax
	movq	16(%rax), %rax
	leaq	8(%rax), %rcx
	movq	%rcx, recog_data+240(%rip)
	movq	8(%rax), %rax
	movq	%rax, recog_data(%rip)
	movq	8(%r14), %rax
	movq	24(%rax), %rax
	leaq	8(%rax), %rcx
	movq	%rcx, recog_data+248(%rip)
	movq	8(%rax), %rax
	movq	%rax, recog_data+8(%rip)
	movq	8(%r14), %rax
	movq	8(%rax), %rax
	movq	8(%rax), %rax
	leaq	8(%rax), %rcx
	movq	%rcx, recog_data+256(%rip)
	movq	8(%rax), %rax
	movq	%rax, recog_data+16(%rip)
	movq	8(%r14), %rax
	movq	8(%rax), %rax
	movq	16(%rax), %rax
	leaq	8(%rax), %rcx
	movq	%rcx, recog_data+264(%rip)
	movq	8(%rax), %rax
	movq	%rax, recog_data+24(%rip)
	movq	8(%r14), %rax
	movq	24(%rax), %rax
	movq	16(%rax), %rax
	addq	$8, %rax
	movq	%rax, recog_data+960(%rip)
	movq	$3, %rax
	movb	%al, recog_data+992(%rip)
	movq	8(%r14), %rax
	jmp	.LBB0_10
.LBB0_79:                               # %sw.bb1967
	movq	8(%r14), %rax
	movq	16(%rax), %rax
	leaq	8(%rax), %rcx
	movq	%rcx, recog_data+240(%rip)
	movq	8(%rax), %rax
	movq	%rax, recog_data(%rip)
	movq	8(%r14), %rax
	movq	8(%rax), %rax
	movq	8(%rax), %rax
	leaq	8(%rax), %rcx
	movq	%rcx, recog_data+248(%rip)
	movq	8(%rax), %rax
	movq	%rax, recog_data+8(%rip)
	movq	8(%r14), %rax
	movq	8(%rax), %rax
	leaq	16(%rax), %rcx
	movq	%rcx, recog_data+256(%rip)
	movq	16(%rax), %rax
	movq	%rax, recog_data+16(%rip)
.LBB0_125:                              # %sw.epilog
	movq	8(%r14), %rax
	movq	16(%rax), %rax
	movq	16(%rax), %rax
	jmp	.LBB0_7
.LBB0_55:                               # %sw.bb1082
	leaq	8(%r14), %rax
	movq	%rax, recog_data+240(%rip)
	movq	8(%r14), %rax
	jmp	.LBB0_56
.LBB0_204:                              # %sw.bb5624
	movq	8(%r14), %rax
	jmp	.LBB0_205
.LBB0_202:                              # %sw.bb5613
	movq	8(%r14), %rax
	movq	8(%rax), %rax
	jmp	.LBB0_94
.LBB0_147:                              # %sw.bb4139
	movq	8(%r14), %rax
	jmp	.LBB0_148
.LBB0_49:                               # %sw.bb936
	leaq	8(%r14), %rax
	movq	%rax, recog_data+240(%rip)
	movq	8(%r14), %rax
	movq	%rax, recog_data(%rip)
	movq	16(%r14), %rax
	movq	8(%rax), %rax
	leaq	8(%rax), %rcx
	movq	%rcx, recog_data+248(%rip)
	movq	8(%rax), %rax
	movq	%rax, recog_data+8(%rip)
	movq	16(%r14), %rax
	movq	8(%rax), %rax
	leaq	16(%rax), %rcx
	movq	%rcx, recog_data+256(%rip)
	movq	16(%rax), %rax
	movq	%rax, recog_data+16(%rip)
.LBB0_50:                               # %sw.epilog
	movq	16(%r14), %rax
	addq	$16, %rax
	jmp	.LBB0_8
.LBB0_46:                               # %sw.bb878
	movq	8(%r14), %rax
	leaq	8(%rax), %rcx
	movq	%rcx, recog_data+240(%rip)
	movq	8(%rax), %rax
.LBB0_38:                               # %sw.epilog
	movq	%rax, recog_data(%rip)
	movq	16(%r14), %rax
	movq	8(%rax), %rax
.LBB0_39:                               # %sw.epilog
	leaq	8(%rax), %rcx
	movq	%rcx, recog_data+248(%rip)
	movq	8(%rax), %rax
	movq	%rax, recog_data+8(%rip)
	movq	16(%r14), %rax
	jmp	.LBB0_34
.LBB0_45:                               # %sw.bb850
	movq	8(%r14), %rax
	leaq	8(%rax), %rcx
	movq	%rcx, recog_data+240(%rip)
	movq	8(%rax), %rax
.LBB0_56:                               # %sw.epilog
	movq	%rax, recog_data(%rip)
	movq	16(%r14), %rax
	jmp	.LBB0_57
.LBB0_22:                               # %sw.bb282
	leaq	8(%r14), %rax
	movq	%rax, recog_data+240(%rip)
	movq	8(%r14), %rax
	jmp	.LBB0_23
.LBB0_209:                              # %sw.bb5667
	leaq	8(%r14), %rax
	movq	%rax, recog_data+240(%rip)
	movq	8(%r14), %rax
	movq	%rax, recog_data(%rip)
	jmp	.LBB0_223
.LBB0_160:                              # %sw.bb4392
	movq	8(%r14), %rax
	movq	8(%rax), %rax
.LBB0_148:                              # %sw.epilog
	movq	8(%rax), %rax
	leaq	8(%rax), %rcx
	movq	%rcx, recog_data+240(%rip)
	movq	8(%rax), %rax
	movq	%rax, recog_data(%rip)
	movq	8(%r14), %rax
	movq	8(%rax), %rax
	movq	16(%rax), %rax
	movq	8(%rax), %rax
	leaq	8(%rax), %rcx
	movq	%rcx, recog_data+248(%rip)
	movq	8(%rax), %rax
	movq	%rax, recog_data+8(%rip)
	movq	8(%r14), %rax
	movq	8(%rax), %rax
	movq	16(%rax), %rax
	jmp	.LBB0_34
.LBB0_13:                               # %sw.bb162
	leaq	8(%r14), %rax
	movq	%rax, recog_data+240(%rip)
	movq	8(%r14), %rax
	movq	%rax, recog_data(%rip)
	movq	16(%r14), %rax
	jmp	.LBB0_14
.LBB0_174:                              # %sw.bb4743
	movq	8(%r14), %rax
	movq	16(%rax), %rax
	leaq	8(%rax), %rcx
	movq	%rcx, recog_data+240(%rip)
	movq	8(%rax), %rax
	movq	%rax, recog_data(%rip)
	movq	8(%r14), %rax
	movq	8(%rax), %rax
	movq	16(%rax), %rax
	leaq	16(%rax), %rcx
	movq	%rcx, recog_data+248(%rip)
	movq	16(%rax), %rax
	movq	%rax, recog_data+8(%rip)
	movq	8(%r14), %rax
	jmp	.LBB0_25
.LBB0_169:                              # %sw.bb4706
	movq	8(%r14), %rax
	movq	16(%rax), %rax
	jmp	.LBB0_170
.LBB0_153:                              # %sw.bb4293
	movq	8(%r14), %rax
	movq	16(%rax), %rax
	leaq	8(%rax), %rcx
	movq	%rcx, recog_data+240(%rip)
	movq	8(%rax), %rax
	movq	%rax, recog_data(%rip)
	movq	8(%r14), %rax
	movq	8(%rax), %rax
	movq	16(%rax), %rax
	leaq	8(%rax), %rcx
	movq	%rcx, recog_data+248(%rip)
	movq	8(%rax), %rax
	movq	%rax, recog_data+8(%rip)
	movq	8(%r14), %rax
	movq	8(%rax), %rax
	movq	16(%rax), %rax
	jmp	.LBB0_154
.LBB0_142:                              # %sw.bb4074
	movq	8(%r14), %rax
	movq	8(%rax), %rax
	leaq	8(%rax), %rcx
	movq	%rcx, recog_data+240(%rip)
	movq	8(%rax), %rax
	movq	%rax, recog_data(%rip)
	movq	8(%r14), %rax
	movq	8(%rax), %rax
	movq	16(%rax), %rax
	movq	8(%rax), %rax
	movq	8(%rax), %rax
	movq	8(%rax), %rax
	leaq	8(%rax), %rcx
	movq	%rcx, recog_data+248(%rip)
	movq	8(%rax), %rax
	movq	%rax, recog_data+8(%rip)
	movq	8(%r14), %rax
	movq	8(%rax), %rax
	movq	16(%rax), %rax
	jmp	.LBB0_143
.LBB0_140:                              # %sw.bb3934
	movq	8(%r14), %rax
	movq	8(%rax), %rax
	leaq	8(%rax), %rcx
	movq	%rcx, recog_data+240(%rip)
	movq	8(%rax), %rax
	movq	%rax, recog_data(%rip)
	movq	8(%r14), %rax
	movq	16(%rax), %rax
	leaq	8(%rax), %rcx
	movq	%rcx, recog_data+248(%rip)
	movq	8(%rax), %rax
	movq	%rax, recog_data+8(%rip)
	movq	8(%r14), %rax
	movq	8(%rax), %rax
	movq	16(%rax), %rax
	leaq	8(%rax), %rcx
	movq	%rcx, recog_data+256(%rip)
	movq	8(%rax), %rax
	movq	%rax, recog_data+16(%rip)
	movq	8(%r14), %rax
	movq	8(%rax), %rax
	movq	16(%rax), %rax
	leaq	16(%rax), %rcx
	movq	%rcx, recog_data+264(%rip)
	movq	16(%rax), %rax
	movq	%rax, recog_data+24(%rip)
	movq	8(%r14), %rax
	movq	16(%rax), %rax
	movq	16(%rax), %rax
	addq	$8, %rax
	movq	%rax, recog_data+960(%rip)
	movq	$2, %rax
	movb	%al, recog_data+992(%rip)
	movq	8(%r14), %rax
	movq	16(%rax), %rax
	movq	16(%rax), %rax
	addq	$16, %rax
	jmp	.LBB0_77
.LBB0_127:                              # %sw.bb3334
	movq	8(%r14), %rax
	movq	16(%rax), %rax
	leaq	8(%rax), %rcx
	movq	%rcx, recog_data+240(%rip)
	movq	8(%rax), %rax
	movq	%rax, recog_data(%rip)
	movq	8(%r14), %rax
	movq	8(%rax), %rax
	movq	16(%rax), %rax
	jmp	.LBB0_128
.LBB0_64:                               # %sw.bb1415
	leaq	8(%r14), %rax
	movq	%rax, recog_data+240(%rip)
	movq	8(%r14), %rax
	movq	%rax, recog_data(%rip)
	movq	16(%r14), %rax
	movq	8(%rax), %rax
	leaq	8(%rax), %rcx
	movq	%rcx, recog_data+248(%rip)
	movq	8(%rax), %rax
	movq	%rax, recog_data+8(%rip)
	movq	16(%r14), %rax
	movq	8(%rax), %rax
	leaq	16(%rax), %rcx
	movq	%rcx, recog_data+256(%rip)
	movq	16(%rax), %rax
	movq	%rax, recog_data+16(%rip)
	movq	16(%r14), %rax
	addq	$16, %rax
	movq	%rax, recog_data+960(%rip)
	movq	$1, %rax
	movb	%al, recog_data+992(%rip)
	movq	16(%r14), %rax
	addq	$24, %rax
	jmp	.LBB0_12
.LBB0_212:                              # %sw.bb5754
	movq	16(%r14), %rax
	movq	8(%rax), %rax
	movq	8(%rax), %rax
	jmp	.LBB0_219
.LBB0_187:                              # %sw.bb5277
	movq	8(%r14), %rax
	movq	8(%rax), %rax
	leaq	8(%rax), %rcx
	movq	%rcx, recog_data+240(%rip)
	movq	8(%rax), %rax
	movq	%rax, recog_data(%rip)
	movq	8(%r14), %rax
	movq	8(%rax), %rax
	movq	16(%rax), %rax
	leaq	8(%rax), %rcx
	movq	%rcx, recog_data+248(%rip)
	movq	8(%rax), %rax
	movq	%rax, recog_data+8(%rip)
	movq	8(%r14), %rax
	movq	16(%rax), %rax
	leaq	8(%rax), %rcx
	movq	%rcx, recog_data+256(%rip)
	movq	8(%rax), %rax
	movq	%rax, recog_data+16(%rip)
	movq	8(%r14), %rax
	jmp	.LBB0_188
.LBB0_161:                              # %sw.bb4438
	movq	8(%r14), %rax
	movq	8(%rax), %rax
	jmp	.LBB0_130
.LBB0_139:                              # %sw.bb3855
	movq	8(%r14), %rax
	movq	8(%rax), %rax
	leaq	8(%rax), %rcx
	movq	%rcx, recog_data+240(%rip)
	movq	8(%rax), %rax
	movq	%rax, recog_data(%rip)
	movq	8(%r14), %rax
	movq	8(%rax), %rax
	movq	16(%rax), %rax
	leaq	8(%rax), %rcx
	movq	%rcx, recog_data+248(%rip)
	movq	8(%rax), %rax
	movq	%rax, recog_data+8(%rip)
	movq	8(%r14), %rax
	movq	8(%rax), %rax
	movq	16(%rax), %rax
	leaq	16(%rax), %rcx
	movq	%rcx, recog_data+256(%rip)
	movq	16(%rax), %rax
	movq	%rax, recog_data+16(%rip)
	movq	8(%r14), %rax
	movq	16(%rax), %rax
	leaq	8(%rax), %rcx
	movq	%rcx, recog_data+264(%rip)
	movq	8(%rax), %rax
	movq	%rax, recog_data+24(%rip)
	movq	8(%r14), %rax
	movq	24(%rax), %rax
	leaq	8(%rax), %rcx
	movq	%rcx, recog_data+272(%rip)
	movq	8(%rax), %rax
	movq	%rax, recog_data+32(%rip)
	jmp	.LBB0_155
.LBB0_138:                              # %sw.bb3786
	movq	8(%r14), %rax
	movq	8(%rax), %rax
	leaq	8(%rax), %rcx
	movq	%rcx, recog_data+240(%rip)
	movq	8(%rax), %rax
	movq	%rax, recog_data(%rip)
	movq	8(%r14), %rax
	movq	8(%rax), %rax
	movq	16(%rax), %rax
	leaq	8(%rax), %rcx
	movq	%rcx, recog_data+248(%rip)
	movq	8(%rax), %rax
	movq	%rax, recog_data+8(%rip)
	movq	8(%r14), %rax
	movq	8(%rax), %rax
	movq	16(%rax), %rax
	leaq	16(%rax), %rcx
	movq	%rcx, recog_data+256(%rip)
	movq	16(%rax), %rax
	movq	%rax, recog_data+16(%rip)
	movq	8(%r14), %rax
	movq	16(%rax), %rax
	leaq	8(%rax), %rcx
	movq	%rcx, recog_data+264(%rip)
	movq	8(%rax), %rax
	movq	%rax, recog_data+24(%rip)
	jmp	.LBB0_155
.LBB0_135:                              # %sw.bb3654
	movq	16(%r14), %rax
	movq	8(%rax), %rax
	movq	8(%rax), %rax
	leaq	8(%rax), %rcx
	movq	%rcx, recog_data+240(%rip)
	movq	8(%rax), %rax
	movq	%rax, recog_data(%rip)
	movq	16(%r14), %rax
	jmp	.LBB0_195
.LBB0_131:                              # %sw.bb3442
	movq	8(%r14), %rax
	movq	16(%rax), %rax
	movq	8(%rax), %rax
	leaq	8(%rax), %rcx
	movq	%rcx, recog_data+240(%rip)
	movq	8(%rax), %rax
	movq	%rax, recog_data(%rip)
	movq	8(%r14), %rax
	movq	8(%rax), %rax
	movq	16(%rax), %rax
	movq	8(%rax), %rax
.LBB0_128:                              # %sw.epilog
	movq	8(%rax), %rax
	leaq	8(%rax), %rcx
	movq	%rcx, recog_data+248(%rip)
	movq	8(%rax), %rax
	movq	%rax, recog_data+8(%rip)
	movq	8(%r14), %rax
	movq	8(%rax), %rax
	movq	16(%rax), %rax
	movq	8(%rax), %rax
	leaq	16(%rax), %rcx
	movq	%rcx, recog_data+256(%rip)
	movq	16(%rax), %rax
	movq	%rax, recog_data+16(%rip)
	movq	8(%r14), %rax
	movq	16(%rax), %rax
	movq	16(%rax), %rax
	movq	8(%rax), %rax
	jmp	.LBB0_156
.LBB0_129:                              # %sw.bb3402
	movq	8(%r14), %rax
.LBB0_130:                              # %sw.epilog
	movq	8(%rax), %rax
	leaq	8(%rax), %rcx
	movq	%rcx, recog_data+240(%rip)
	movq	8(%rax), %rax
	movq	%rax, recog_data(%rip)
	movq	8(%r14), %rax
	movq	8(%rax), %rax
	movq	16(%rax), %rax
	movq	8(%rax), %rax
	jmp	.LBB0_171
.LBB0_120:                              # %sw.bb3128
	movq	8(%r14), %rax
	movq	8(%rax), %rax
	leaq	8(%rax), %rcx
	movq	%rcx, recog_data+240(%rip)
	movq	8(%rax), %rax
	movq	%rax, recog_data(%rip)
	movq	8(%r14), %rax
	movq	8(%rax), %rax
	movq	16(%rax), %rax
	leaq	8(%rax), %rcx
	movq	%rcx, recog_data+248(%rip)
	movq	8(%rax), %rax
	movq	%rax, recog_data+8(%rip)
	movq	8(%r14), %rax
	movq	8(%rax), %rax
	movq	16(%rax), %rax
	leaq	16(%rax), %rcx
	movq	%rcx, recog_data+256(%rip)
	movq	16(%rax), %rax
	jmp	.LBB0_144
.LBB0_111:                              # %sw.bb2925
	movq	8(%r14), %rax
	movq	8(%rax), %rax
	movq	16(%rax), %rax
	leaq	8(%rax), %rcx
	movq	%rcx, recog_data+240(%rip)
	movq	8(%rax), %rax
	movq	%rax, recog_data(%rip)
	movq	8(%r14), %rax
	movq	8(%rax), %rax
	movq	16(%rax), %rax
	movq	8(%rax), %rax
	leaq	8(%rax), %rcx
	movq	%rcx, recog_data+248(%rip)
	movq	8(%rax), %rax
	movq	%rax, recog_data+8(%rip)
	movq	8(%r14), %rax
	movq	8(%rax), %rax
	movq	16(%rax), %rax
	movq	8(%rax), %rax
	leaq	16(%rax), %rcx
	movq	%rcx, recog_data+256(%rip)
	movq	16(%rax), %rax
	movq	%rax, recog_data+16(%rip)
	movq	8(%r14), %rax
	movq	8(%rax), %rax
	movq	16(%rax), %rax
	jmp	.LBB0_145
.LBB0_110:                              # %sw.bb2863
	movq	8(%r14), %rax
	movq	8(%rax), %rax
	movq	16(%rax), %rax
	leaq	8(%rax), %rcx
	movq	%rcx, recog_data+240(%rip)
	movq	8(%rax), %rax
	movq	%rax, recog_data(%rip)
	movq	8(%r14), %rax
	movq	8(%rax), %rax
	movq	16(%rax), %rax
	movq	8(%rax), %rax
	leaq	8(%rax), %rcx
	movq	%rcx, recog_data+248(%rip)
	movq	8(%rax), %rax
	movq	%rax, recog_data+8(%rip)
	movq	8(%r14), %rax
	movq	8(%rax), %rax
	movq	16(%rax), %rax
	movq	8(%rax), %rax
	leaq	16(%rax), %rcx
	movq	%rcx, recog_data+256(%rip)
	movq	16(%rax), %rax
	movq	%rax, recog_data+16(%rip)
	movq	8(%r14), %rax
	movq	8(%rax), %rax
	movq	16(%rax), %rax
	movq	24(%rax), %rax
	jmp	.LBB0_146
.LBB0_93:                               # %sw.bb2468
	movq	8(%r14), %rax
	jmp	.LBB0_94
.LBB0_82:                               # %sw.bb2123
	movq	8(%r14), %rax
	movq	16(%rax), %rax
	leaq	8(%rax), %rcx
	movq	%rcx, recog_data+240(%rip)
	movq	8(%rax), %rax
	movq	%rax, recog_data(%rip)
	movq	8(%r14), %rax
	movq	24(%rax), %rax
	leaq	8(%rax), %rcx
	movq	%rcx, recog_data+248(%rip)
	movq	8(%rax), %rax
	movq	%rax, recog_data+8(%rip)
	movq	8(%r14), %rax
	movq	8(%rax), %rax
	leaq	8(%rax), %rcx
	movq	%rcx, recog_data+256(%rip)
	movq	8(%rax), %rax
	movq	%rax, recog_data+16(%rip)
	movq	8(%r14), %rax
	movq	16(%rax), %rax
	movq	16(%rax), %rax
	leaq	16(%rax), %rcx
	movq	%rcx, recog_data+264(%rip)
	movq	16(%rax), %rax
	movq	%rax, recog_data+24(%rip)
	movq	8(%r14), %rax
	movq	24(%rax), %rax
	movq	16(%rax), %rax
	leaq	16(%rax), %rcx
	movq	%rcx, recog_data+272(%rip)
	movq	16(%rax), %rax
	movq	%rax, recog_data+32(%rip)
	movq	8(%r14), %rax
	movq	16(%rax), %rax
	movq	16(%rax), %rax
	movq	8(%rax), %rax
	leaq	8(%rax), %rcx
	movq	%rcx, recog_data+280(%rip)
	movq	8(%rax), %rax
	movq	%rax, recog_data+40(%rip)
	movq	8(%r14), %rax
	movq	40(%rax), %rax
	addq	$8, %rax
	movq	%rax, recog_data+960(%rip)
	movq	$5, %rax
	movb	%al, recog_data+992(%rip)
	movq	8(%r14), %rcx
	movq	32(%rcx), %rcx
	movq	8(%rcx), %rcx
	addq	$8, %rcx
	movq	%rcx, recog_data+968(%rip)
	movq	$3, %rcx
	movb	%cl, recog_data+993(%rip)
	movq	8(%r14), %rcx
	movq	32(%rcx), %rcx
	movq	16(%rcx), %rcx
	addq	$8, %rcx
	movq	%rcx, recog_data+976(%rip)
	movq	$4, %rcx
	movb	%cl, recog_data+994(%rip)
	movq	8(%r14), %rcx
	movq	24(%rcx), %rcx
	movq	16(%rcx), %rcx
	movq	8(%rcx), %rcx
	addq	$8, %rcx
	jmp	.LBB0_81
.LBB0_78:                               # %sw.bb1888
	movq	8(%r14), %rax
	movq	16(%rax), %rax
	leaq	8(%rax), %rcx
	movq	%rcx, recog_data+240(%rip)
	movq	8(%rax), %rax
	movq	%rax, recog_data(%rip)
	movq	8(%r14), %rax
	movq	8(%rax), %rax
	leaq	8(%rax), %rcx
	movq	%rcx, recog_data+248(%rip)
	movq	8(%rax), %rax
	movq	%rax, recog_data+8(%rip)
	movq	8(%r14), %rax
	movq	32(%rax), %rax
	leaq	8(%rax), %rcx
	movq	%rcx, recog_data+256(%rip)
	movq	8(%rax), %rax
	movq	%rax, recog_data+16(%rip)
	movq	8(%r14), %rax
	movq	16(%rax), %rax
	movq	16(%rax), %rax
	leaq	16(%rax), %rcx
	movq	%rcx, recog_data+264(%rip)
	movq	16(%rax), %rax
	movq	%rax, recog_data+24(%rip)
	movq	8(%r14), %rax
	movq	16(%rax), %rax
	movq	16(%rax), %rax
	movq	8(%rax), %rax
	leaq	8(%rax), %rcx
	movq	%rcx, recog_data+272(%rip)
	movq	8(%rax), %rax
	jmp	.LBB0_76
.LBB0_9:                                # %sw.bb104
	leaq	8(%r14), %rax
	movq	%rax, recog_data+240(%rip)
	movq	8(%r14), %rax
	movq	%rax, recog_data(%rip)
	movq	16(%r14), %rax
	movq	8(%rax), %rax
	movq	8(%rax), %rax
	leaq	8(%rax), %rcx
	movq	%rcx, recog_data+248(%rip)
	movq	8(%rax), %rax
	movq	%rax, recog_data+8(%rip)
	movq	16(%r14), %rax
	movq	16(%rax), %rax
	movq	8(%rax), %rax
	leaq	8(%rax), %rcx
	movq	%rcx, recog_data+256(%rip)
	movq	8(%rax), %rax
	movq	%rax, recog_data+16(%rip)
	movq	16(%r14), %rax
	movq	8(%rax), %rax
	movq	16(%rax), %rax
	addq	$8, %rax
	movq	%rax, recog_data+960(%rip)
	movq	$1, %rax
	movb	%al, recog_data+992(%rip)
	movq	16(%r14), %rax
.LBB0_10:                               # %sw.epilog
	movq	16(%rax), %rax
	movq	16(%rax), %rax
.LBB0_11:                               # %sw.epilog
	addq	$8, %rax
	jmp	.LBB0_12
.LBB0_213:                              # %sw.bb5777
	movq	16(%r14), %rax
	jmp	.LBB0_214
.LBB0_208:                              # %sw.bb5656
	movq	16(%r14), %rax
	movq	8(%rax), %rax
	jmp	.LBB0_94
.LBB0_198:                              # %sw.bb5524
	movq	8(%r14), %rax
	movq	8(%rax), %rax
.LBB0_205:                              # %sw.epilog
	movq	8(%rax), %rax
	leaq	8(%rax), %rcx
	movq	%rcx, recog_data+240(%rip)
	movq	8(%rax), %rax
	movq	%rax, recog_data(%rip)
	movq	8(%r14), %rax
.LBB0_96:                               # %sw.epilog
	movq	8(%rax), %rax
.LBB0_97:                               # %sw.epilog
	leaq	16(%rax), %rcx
	movq	%rcx, recog_data+248(%rip)
	movq	16(%rax), %rax
.LBB0_222:                              # %sw.epilog
	movq	%rax, recog_data+8(%rip)
	jmp	.LBB0_223
.LBB0_185:                              # %sw.bb5233
	movq	8(%r14), %rax
	movq	8(%rax), %rax
	leaq	8(%rax), %rcx
	movq	%rcx, recog_data+240(%rip)
	movq	8(%rax), %rax
	movq	%rax, recog_data(%rip)
	movq	8(%r14), %rax
	movq	8(%rax), %rax
	movq	16(%rax), %rax
	leaq	8(%rax), %rcx
	movq	%rcx, recog_data+248(%rip)
	movq	8(%rax), %rax
	movq	%rax, recog_data+8(%rip)
	movq	8(%r14), %rax
	movq	16(%rax), %rax
	leaq	8(%rax), %rcx
	movq	%rcx, recog_data+256(%rip)
	movq	8(%rax), %rax
	movq	%rax, recog_data+16(%rip)
	movq	8(%r14), %rax
	movq	24(%rax), %rax
	jmp	.LBB0_146
.LBB0_183:                              # %sw.bb5156
	movq	8(%r14), %rax
	movq	8(%rax), %rax
	leaq	8(%rax), %rcx
	movq	%rcx, recog_data+240(%rip)
	movq	8(%rax), %rax
	movq	%rax, recog_data(%rip)
	movq	8(%r14), %rax
	movq	8(%rax), %rax
	movq	16(%rax), %rax
	movq	8(%rax), %rax
	leaq	16(%rax), %rcx
	movq	%rcx, recog_data+248(%rip)
	movq	16(%rax), %rax
	jmp	.LBB0_172
.LBB0_180:                              # %sw.bb5011
	leaq	8(%r14), %rax
	movq	%rax, recog_data+240(%rip)
	movq	8(%r14), %rax
	movq	%rax, recog_data(%rip)
	movq	16(%r14), %rax
	movq	8(%rax), %rax
	leaq	8(%rax), %rcx
	movq	%rcx, recog_data+248(%rip)
	movq	8(%rax), %rax
	movq	%rax, recog_data+8(%rip)
	movq	16(%r14), %rax
	movq	8(%rax), %rax
	leaq	16(%rax), %rcx
	movq	%rcx, recog_data+256(%rip)
	movq	16(%rax), %rax
	jmp	.LBB0_117
.LBB0_179:                              # %sw.bb4966
	leaq	8(%r14), %rax
	movq	%rax, recog_data+240(%rip)
	movq	8(%r14), %rax
	movq	%rax, recog_data(%rip)
	movq	16(%r14), %rax
	movq	8(%rax), %rax
	movq	8(%rax), %rax
	leaq	8(%rax), %rcx
	movq	%rcx, recog_data+248(%rip)
	movq	8(%rax), %rax
	movq	%rax, recog_data+8(%rip)
	movq	16(%r14), %rax
	movq	8(%rax), %rax
	movq	8(%rax), %rax
	leaq	16(%rax), %rcx
	movq	%rcx, recog_data+256(%rip)
	movq	16(%rax), %rax
	movq	%rax, recog_data+16(%rip)
	movq	16(%r14), %rax
	movq	8(%rax), %rax
	jmp	.LBB0_118
.LBB0_159:                              # %sw.bb4352
	movq	8(%r14), %rax
	movq	8(%rax), %rax
	leaq	8(%rax), %rcx
	movq	%rcx, recog_data+240(%rip)
	movq	8(%rax), %rax
	movq	%rax, recog_data(%rip)
	movq	8(%r14), %rax
	movq	8(%rax), %rax
	movq	16(%rax), %rax
	leaq	8(%rax), %rcx
	movq	%rcx, recog_data+248(%rip)
	movq	8(%rax), %rax
	movq	%rax, recog_data+8(%rip)
	movq	8(%r14), %rax
	movq	8(%rax), %rax
	jmp	.LBB0_101
.LBB0_141:                              # %sw.bb4003
	movq	8(%r14), %rax
	movq	8(%rax), %rax
	leaq	8(%rax), %rcx
	movq	%rcx, recog_data+240(%rip)
	movq	8(%rax), %rax
	movq	%rax, recog_data(%rip)
	movq	8(%r14), %rax
	movq	8(%rax), %rax
	movq	16(%rax), %rax
	movq	8(%rax), %rax
	movq	8(%rax), %rax
	movq	8(%rax), %rax
	movq	8(%rax), %rax
	leaq	8(%rax), %rcx
	movq	%rcx, recog_data+248(%rip)
	movq	8(%rax), %rax
	movq	%rax, recog_data+8(%rip)
	movq	8(%r14), %rax
	movq	8(%rax), %rax
	movq	16(%rax), %rax
	movq	8(%rax), %rax
.LBB0_143:                              # %sw.epilog
	movq	8(%rax), %rax
	movq	8(%rax), %rax
	movq	16(%rax), %rax
	leaq	8(%rax), %rcx
	movq	%rcx, recog_data+256(%rip)
	movq	8(%rax), %rax
.LBB0_144:                              # %sw.epilog
	movq	%rax, recog_data+16(%rip)
	movq	8(%r14), %rax
.LBB0_145:                              # %sw.epilog
	movq	16(%rax), %rax
.LBB0_146:                              # %sw.epilog
	leaq	8(%rax), %rcx
	movq	%rcx, recog_data+264(%rip)
	movq	8(%rax), %rax
	movq	%rax, recog_data+24(%rip)
	jmp	.LBB0_223
.LBB0_137:                              # %sw.bb3709
	movq	8(%r14), %rax
	movq	8(%rax), %rax
	leaq	8(%rax), %rcx
	movq	%rcx, recog_data+240(%rip)
	movq	8(%rax), %rax
	movq	%rax, recog_data(%rip)
	movq	8(%r14), %rax
	movq	8(%rax), %rax
	movq	16(%rax), %rax
	leaq	8(%rax), %rcx
	movq	%rcx, recog_data+248(%rip)
	movq	8(%rax), %rax
	movq	%rax, recog_data+8(%rip)
	movq	8(%r14), %rax
	movq	8(%rax), %rax
	movq	16(%rax), %rax
	leaq	16(%rax), %rcx
	movq	%rcx, recog_data+256(%rip)
	movq	16(%rax), %rax
	movq	%rax, recog_data+16(%rip)
	movq	8(%r14), %rax
	movq	16(%rax), %rax
	leaq	8(%rax), %rcx
	movq	%rcx, recog_data+264(%rip)
	movq	8(%rax), %rax
	movq	%rax, recog_data+24(%rip)
	movq	8(%r14), %rax
	movq	24(%rax), %rax
	addq	$8, %rax
	movq	%rax, recog_data+960(%rip)
	movq	$3, %rax
	movb	%al, recog_data+992(%rip)
	movq	8(%r14), %rax
	movq	16(%rax), %rax
	movq	16(%rax), %rax
	addq	$8, %rax
	movq	%rax, recog_data+968(%rip)
	movq	$1, %rax
	movb	%al, recog_data+993(%rip)
	movq	8(%r14), %rax
	movq	16(%rax), %rax
	movq	16(%rax), %rax
	addq	$16, %rax
	movq	%rax, recog_data+976(%rip)
	movq	$2, %rax
	movb	%al, recog_data+994(%rip)
	jmp	.LBB0_223
.LBB0_134:                              # %sw.bb3620
	movq	16(%r14), %rax
	movq	8(%rax), %rax
	leaq	8(%rax), %rcx
	movq	%rcx, recog_data+240(%rip)
	movq	8(%rax), %rax
	movq	%rax, recog_data(%rip)
	movq	16(%r14), %rax
	movq	8(%rax), %rax
	leaq	16(%rax), %rcx
	movq	%rcx, recog_data+248(%rip)
	movq	16(%rax), %rax
	movq	%rax, recog_data+8(%rip)
	movq	16(%r14), %rax
	movq	8(%rax), %rax
	leaq	24(%rax), %rcx
	movq	%rcx, recog_data+256(%rip)
	movq	24(%rax), %rax
	movq	%rax, recog_data+16(%rip)
	jmp	.LBB0_223
.LBB0_133:                              # %sw.bb3582
	movq	8(%r14), %rax
	movq	8(%rax), %rax
	movq	8(%rax), %rax
	leaq	8(%rax), %rcx
	movq	%rcx, recog_data+240(%rip)
	movq	8(%rax), %rax
	movq	%rax, recog_data(%rip)
	movq	8(%r14), %rax
	movq	8(%rax), %rax
	movq	16(%rax), %rax
	leaq	16(%rax), %rcx
	movq	%rcx, recog_data+248(%rip)
	movq	16(%rax), %rax
	movq	%rax, recog_data+8(%rip)
	movq	8(%r14), %rax
	movq	8(%rax), %rax
	movq	16(%rax), %rax
	jmp	.LBB0_21
.LBB0_132:                              # %sw.bb3516
	movq	8(%r14), %rax
	movq	8(%rax), %rax
	movq	16(%rax), %rax
	movq	8(%rax), %rax
	leaq	8(%rax), %rcx
	movq	%rcx, recog_data+240(%rip)
	movq	8(%rax), %rax
	movq	%rax, recog_data(%rip)
	movq	8(%r14), %rax
	movq	8(%rax), %rax
	movq	16(%rax), %rax
	movq	8(%rax), %rax
	leaq	16(%rax), %rcx
	movq	%rcx, recog_data+248(%rip)
	movq	16(%rax), %rax
	movq	%rax, recog_data+8(%rip)
	movq	8(%r14), %rax
	movq	16(%rax), %rax
	movq	8(%rax), %rax
	addq	$8, %rax
	movq	%rax, recog_data+960(%rip)
	xorq	%rax, %rax
	movb	%al, recog_data+992(%rip)
	movq	8(%r14), %rcx
	movq	16(%rcx), %rcx
	movq	16(%rcx), %rcx
	addq	$8, %rcx
	movq	%rcx, recog_data+968(%rip)
	movb	%al, recog_data+993(%rip)
	movq	8(%r14), %rax
	movq	16(%rax), %rax
	movq	16(%rax), %rax
	addq	$16, %rax
	movq	%rax, recog_data+976(%rip)
	movq	$1, %rax
	movb	%al, recog_data+994(%rip)
	jmp	.LBB0_223
.LBB0_119:                              # %sw.bb3054
	movq	8(%r14), %rax
	movq	8(%rax), %rax
	leaq	8(%rax), %rcx
	movq	%rcx, recog_data+240(%rip)
	movq	8(%rax), %rax
	movq	%rax, recog_data(%rip)
	movq	8(%r14), %rax
	movq	8(%rax), %rax
	movq	16(%rax), %rax
	movq	16(%rax), %rax
	leaq	8(%rax), %rcx
	movq	%rcx, recog_data+248(%rip)
	movq	8(%rax), %rax
	movq	%rax, recog_data+8(%rip)
	movq	8(%r14), %rax
	movq	8(%rax), %rax
	movq	16(%rax), %rax
	movq	8(%rax), %rax
	leaq	16(%rax), %rcx
	movq	%rcx, recog_data+256(%rip)
	movq	16(%rax), %rax
	movq	%rax, recog_data+16(%rip)
	movq	8(%r14), %rax
	movq	8(%rax), %rax
	movq	16(%rax), %rax
	movq	8(%rax), %rax
	addq	$8, %rax
	movq	%rax, recog_data+960(%rip)
	xorq	%rax, %rax
	movb	%al, recog_data+992(%rip)
	movq	8(%r14), %rax
	movq	8(%rax), %rax
	movq	16(%rax), %rax
	jmp	.LBB0_157
.LBB0_116:                              # %sw.bb3027
	leaq	8(%r14), %rax
	movq	%rax, recog_data+240(%rip)
	movq	8(%r14), %rax
	movq	%rax, recog_data(%rip)
	leaq	16(%r14), %rax
	movq	%rax, recog_data+248(%rip)
	movq	16(%r14), %rax
	movq	%rax, recog_data+8(%rip)
	movq	16(%r14), %rax
	leaq	8(%rax), %rcx
	movq	%rcx, recog_data+256(%rip)
	movq	8(%rax), %rax
.LBB0_117:                              # %sw.epilog
	movq	%rax, recog_data+16(%rip)
	movq	16(%r14), %rax
	jmp	.LBB0_118
.LBB0_109:                              # %sw.bb2791
	movq	8(%r14), %rax
	movq	8(%rax), %rax
	movq	16(%rax), %rax
	leaq	8(%rax), %rcx
	movq	%rcx, recog_data+240(%rip)
	movq	8(%rax), %rax
	movq	%rax, recog_data(%rip)
	movq	8(%r14), %rax
	movq	8(%rax), %rax
	movq	16(%rax), %rax
	movq	8(%rax), %rax
	leaq	8(%rax), %rcx
	movq	%rcx, recog_data+248(%rip)
	movq	8(%rax), %rax
	movq	%rax, recog_data+8(%rip)
	movq	8(%r14), %rax
	movq	8(%rax), %rax
	movq	16(%rax), %rax
	movq	8(%rax), %rax
	leaq	16(%rax), %rcx
	movq	%rcx, recog_data+256(%rip)
	movq	16(%rax), %rax
	movq	%rax, recog_data+16(%rip)
	movq	8(%r14), %rax
	movq	8(%rax), %rax
	movq	16(%rax), %rax
	movq	16(%rax), %rax
	jmp	.LBB0_189
.LBB0_108:                              # %sw.bb2719
	movq	8(%r14), %rax
	movq	8(%rax), %rax
	movq	16(%rax), %rax
	leaq	8(%rax), %rcx
	movq	%rcx, recog_data+240(%rip)
	movq	8(%rax), %rax
	movq	%rax, recog_data(%rip)
	movq	8(%r14), %rax
	movq	8(%rax), %rax
	movq	16(%rax), %rax
	movq	8(%rax), %rax
	leaq	8(%rax), %rcx
	movq	%rcx, recog_data+248(%rip)
	movq	8(%rax), %rax
	movq	%rax, recog_data+8(%rip)
	movq	8(%r14), %rax
	movq	8(%rax), %rax
	movq	16(%rax), %rax
	movq	8(%rax), %rax
	leaq	16(%rax), %rcx
	movq	%rcx, recog_data+256(%rip)
	movq	16(%rax), %rax
	movq	%rax, recog_data+16(%rip)
	movq	8(%r14), %rax
	movq	8(%rax), %rax
	movq	16(%rax), %rax
.LBB0_188:                              # %sw.epilog
	movq	24(%rax), %rax
.LBB0_189:                              # %sw.epilog
	leaq	8(%rax), %rcx
	movq	%rcx, recog_data+264(%rip)
	movq	8(%rax), %rax
	movq	%rax, recog_data+24(%rip)
	movq	8(%r14), %rax
	movq	32(%rax), %rax
	leaq	8(%rax), %rcx
	movq	%rcx, recog_data+272(%rip)
	movq	8(%rax), %rax
	movq	%rax, recog_data+32(%rip)
	jmp	.LBB0_223
.LBB0_106:                              # %sw.bb2704
	leaq	16(%r14), %rax
	movq	%rax, recog_data+240(%rip)
	movq	16(%r14), %rax
	movq	%rax, recog_data(%rip)
	jmp	.LBB0_223
.LBB0_105:                              # %sw.bb2680
	movq	8(%r14), %rax
	movq	8(%rax), %rax
	leaq	16(%rax), %rcx
	movq	%rcx, recog_data+240(%rip)
	movq	16(%rax), %rax
	movq	%rax, recog_data(%rip)
	movq	8(%r14), %rax
	movq	16(%rax), %rax
	jmp	.LBB0_15
.LBB0_100:                              # %sw.bb2579
	movq	8(%r14), %rax
	movq	8(%rax), %rax
	movq	8(%rax), %rax
	leaq	8(%rax), %rcx
	movq	%rcx, recog_data+240(%rip)
	movq	8(%rax), %rax
	movq	%rax, recog_data(%rip)
	movq	8(%r14), %rax
	movq	8(%rax), %rax
	leaq	16(%rax), %rcx
	movq	%rcx, recog_data+248(%rip)
	movq	16(%rax), %rax
	movq	%rax, recog_data+8(%rip)
	movq	8(%r14), %rax
.LBB0_101:                              # %sw.epilog
	movq	16(%rax), %rax
	jmp	.LBB0_102
.LBB0_99:                               # %sw.bb2567
	movq	8(%r14), %rax
	movq	16(%rax), %rax
	jmp	.LBB0_94
.LBB0_80:                               # %sw.bb2012
	movq	8(%r14), %rax
	movq	16(%rax), %rax
	leaq	8(%rax), %rcx
	movq	%rcx, recog_data+240(%rip)
	movq	8(%rax), %rax
	movq	%rax, recog_data(%rip)
	movq	8(%r14), %rax
	movq	24(%rax), %rax
	leaq	8(%rax), %rcx
	movq	%rcx, recog_data+248(%rip)
	movq	8(%rax), %rax
	movq	%rax, recog_data+8(%rip)
	movq	8(%r14), %rax
	movq	8(%rax), %rax
	leaq	8(%rax), %rcx
	movq	%rcx, recog_data+256(%rip)
	movq	8(%rax), %rax
	movq	%rax, recog_data+16(%rip)
	movq	8(%r14), %rax
	movq	16(%rax), %rax
	movq	16(%rax), %rax
	leaq	8(%rax), %rcx
	movq	%rcx, recog_data+264(%rip)
	movq	8(%rax), %rax
	movq	%rax, recog_data+24(%rip)
	movq	8(%r14), %rax
	movq	24(%rax), %rax
	movq	16(%rax), %rax
	leaq	8(%rax), %rcx
	movq	%rcx, recog_data+272(%rip)
	movq	8(%rax), %rax
	movq	%rax, recog_data+32(%rip)
	movq	8(%r14), %rax
	movq	16(%rax), %rax
	movq	16(%rax), %rax
	leaq	16(%rax), %rcx
	movq	%rcx, recog_data+280(%rip)
	movq	16(%rax), %rax
	movq	%rax, recog_data+40(%rip)
	movq	8(%r14), %rax
	movq	40(%rax), %rax
	addq	$8, %rax
	movq	%rax, recog_data+960(%rip)
	movq	$5, %rax
	movb	%al, recog_data+992(%rip)
	movq	8(%r14), %rcx
	movq	32(%rcx), %rcx
	movq	8(%rcx), %rcx
	addq	$8, %rcx
	movq	%rcx, recog_data+968(%rip)
	movq	$3, %rcx
	movb	%cl, recog_data+993(%rip)
	movq	8(%r14), %rcx
	movq	32(%rcx), %rcx
	movq	16(%rcx), %rcx
	addq	$8, %rcx
	movq	%rcx, recog_data+976(%rip)
	movq	$4, %rcx
	movb	%cl, recog_data+994(%rip)
	movq	8(%r14), %rcx
	movq	24(%rcx), %rcx
	movq	16(%rcx), %rcx
	addq	$16, %rcx
.LBB0_81:                               # %sw.epilog
	movq	%rcx, recog_data+984(%rip)
	movb	%al, recog_data+995(%rip)
	jmp	.LBB0_223
.LBB0_75:                               # %sw.bb1812
	movq	8(%r14), %rax
	movq	16(%rax), %rax
	leaq	8(%rax), %rcx
	movq	%rcx, recog_data+240(%rip)
	movq	8(%rax), %rax
	movq	%rax, recog_data(%rip)
	movq	8(%r14), %rax
	movq	8(%rax), %rax
	leaq	8(%rax), %rcx
	movq	%rcx, recog_data+248(%rip)
	movq	8(%rax), %rax
	movq	%rax, recog_data+8(%rip)
	movq	8(%r14), %rax
	movq	32(%rax), %rax
	leaq	8(%rax), %rcx
	movq	%rcx, recog_data+256(%rip)
	movq	8(%rax), %rax
	movq	%rax, recog_data+16(%rip)
	movq	8(%r14), %rax
	movq	16(%rax), %rax
	movq	16(%rax), %rax
	leaq	8(%rax), %rcx
	movq	%rcx, recog_data+264(%rip)
	movq	8(%rax), %rax
	movq	%rax, recog_data+24(%rip)
	movq	8(%r14), %rax
	movq	16(%rax), %rax
	movq	16(%rax), %rax
	leaq	16(%rax), %rcx
	movq	%rcx, recog_data+272(%rip)
	movq	16(%rax), %rax
.LBB0_76:                               # %sw.epilog
	movq	%rax, recog_data+32(%rip)
	movq	8(%r14), %rax
	movq	40(%rax), %rax
	addq	$8, %rax
	movq	%rax, recog_data+960(%rip)
	movq	$4, %rax
	movb	%al, recog_data+992(%rip)
	movq	8(%r14), %rax
	movq	24(%rax), %rax
	movq	8(%rax), %rax
	addq	$8, %rax
.LBB0_77:                               # %sw.epilog
	movq	%rax, recog_data+968(%rip)
	movq	$3, %rax
	movb	%al, recog_data+993(%rip)
	jmp	.LBB0_223
.LBB0_72:                               # %sw.bb1729
	movq	8(%r14), %rax
	movq	40(%rax), %rax
	leaq	8(%rax), %rcx
	movq	%rcx, recog_data+240(%rip)
	movq	8(%rax), %rax
	movq	%rax, recog_data(%rip)
	movq	8(%r14), %rax
	movq	48(%rax), %rax
	leaq	8(%rax), %rcx
	movq	%rcx, recog_data+248(%rip)
	movq	8(%rax), %rax
	movq	%rax, recog_data+8(%rip)
	movq	8(%r14), %rax
	movq	56(%rax), %rax
	leaq	8(%rax), %rcx
	movq	%rcx, recog_data+256(%rip)
	movq	8(%rax), %rax
	movq	%rax, recog_data+16(%rip)
	movq	8(%r14), %rax
	movq	24(%rax), %rax
	leaq	8(%rax), %rcx
	movq	%rcx, recog_data+264(%rip)
	movq	8(%rax), %rax
	movq	%rax, recog_data+24(%rip)
	movq	8(%r14), %rax
	movq	8(%rax), %rax
	movq	16(%rax), %rax
	movq	8(%rax), %rax
	leaq	8(%rax), %rcx
	movq	%rcx, recog_data+272(%rip)
	movq	8(%rax), %rax
	movq	%rax, recog_data+32(%rip)
	movq	8(%r14), %rax
	movq	8(%rax), %rax
	movq	16(%rax), %rax
	movq	16(%rax), %rax
	leaq	8(%rax), %rcx
	movq	%rcx, recog_data+280(%rip)
	movq	8(%rax), %rax
	jmp	.LBB0_73
.LBB0_71:                               # %sw.bb1634
	movq	8(%r14), %rax
	movq	40(%rax), %rax
	leaq	8(%rax), %rcx
	movq	%rcx, recog_data+240(%rip)
	movq	8(%rax), %rax
	movq	%rax, recog_data(%rip)
	movq	8(%r14), %rax
	movq	48(%rax), %rax
	leaq	8(%rax), %rcx
	movq	%rcx, recog_data+248(%rip)
	movq	8(%rax), %rax
	movq	%rax, recog_data+8(%rip)
	movq	8(%r14), %rax
	movq	56(%rax), %rax
	leaq	8(%rax), %rcx
	movq	%rcx, recog_data+256(%rip)
	movq	8(%rax), %rax
	movq	%rax, recog_data+16(%rip)
	movq	8(%r14), %rax
	movq	16(%rax), %rax
	leaq	8(%rax), %rcx
	movq	%rcx, recog_data+264(%rip)
	movq	8(%rax), %rax
	movq	%rax, recog_data+24(%rip)
	movq	8(%r14), %rax
	movq	8(%rax), %rax
	movq	16(%rax), %rax
	movq	16(%rax), %rax
	movq	8(%rax), %rax
	leaq	8(%rax), %rcx
	movq	%rcx, recog_data+272(%rip)
	movq	8(%rax), %rax
	movq	%rax, recog_data+32(%rip)
	movq	8(%r14), %rax
	movq	8(%rax), %rax
	movq	16(%rax), %rax
	movq	16(%rax), %rax
	movq	16(%rax), %rax
	leaq	8(%rax), %rcx
	movq	%rcx, recog_data+280(%rip)
	movq	8(%rax), %rax
	movq	%rax, recog_data+40(%rip)
	movq	8(%r14), %rax
	movq	8(%rax), %rax
	movq	16(%rax), %rax
	movq	8(%rax), %rax
	jmp	.LBB0_74
.LBB0_70:                               # %sw.bb1550
	movq	8(%r14), %rax
	movq	8(%rax), %rax
	leaq	8(%rax), %rcx
	movq	%rcx, recog_data+240(%rip)
	movq	8(%rax), %rax
	movq	%rax, recog_data(%rip)
	movq	8(%r14), %rax
	movq	24(%rax), %rax
	leaq	8(%rax), %rcx
	movq	%rcx, recog_data+248(%rip)
	movq	8(%rax), %rax
	movq	%rax, recog_data+8(%rip)
	movq	8(%r14), %rax
	movq	8(%rax), %rax
	movq	16(%rax), %rax
	movq	8(%rax), %rax
	leaq	16(%rax), %rcx
	movq	%rcx, recog_data+256(%rip)
	movq	16(%rax), %rax
	movq	%rax, recog_data+16(%rip)
	movq	8(%r14), %rax
	movq	8(%rax), %rax
	movq	16(%rax), %rax
	movq	8(%rax), %rax
	leaq	24(%rax), %rcx
	movq	%rcx, recog_data+264(%rip)
	movq	24(%rax), %rax
	movq	%rax, recog_data+24(%rip)
	movq	8(%r14), %rax
	movq	8(%rax), %rax
	movq	16(%rax), %rax
	movq	8(%rax), %rax
	leaq	32(%rax), %rcx
	movq	%rcx, recog_data+272(%rip)
	movq	32(%rax), %rax
	movq	%rax, recog_data+32(%rip)
	movq	8(%r14), %rax
	movq	8(%rax), %rax
	movq	16(%rax), %rax
	movq	8(%rax), %rax
	movq	8(%rax), %rax
	jmp	.LBB0_191
.LBB0_63:                               # %sw.bb1323
	movq	8(%r14), %rax
	movq	8(%rax), %rax
	leaq	8(%rax), %rcx
	movq	%rcx, recog_data+240(%rip)
	movq	8(%rax), %rax
	movq	%rax, recog_data(%rip)
	movq	8(%r14), %rax
	movq	8(%rax), %rax
	movq	16(%rax), %rax
	leaq	8(%rax), %rcx
	movq	%rcx, recog_data+248(%rip)
	movq	8(%rax), %rax
	movq	%rax, recog_data+8(%rip)
	movq	8(%r14), %rax
	movq	8(%rax), %rax
	movq	16(%rax), %rax
	leaq	16(%rax), %rcx
	movq	%rcx, recog_data+256(%rip)
	movq	16(%rax), %rax
	movq	%rax, recog_data+16(%rip)
	movq	8(%r14), %rax
	movq	8(%rax), %rax
	movq	16(%rax), %rax
	leaq	24(%rax), %rcx
	movq	%rcx, recog_data+264(%rip)
	movq	24(%rax), %rax
	movq	%rax, recog_data+24(%rip)
	movq	8(%r14), %rax
	movq	8(%rax), %rax
	movq	16(%rax), %rax
	movq	8(%rax), %rax
	leaq	8(%rax), %rcx
	movq	%rcx, recog_data+272(%rip)
	movq	8(%rax), %rax
	movq	%rax, recog_data+32(%rip)
	movq	8(%r14), %rax
	movq	8(%rax), %rax
	movq	16(%rax), %rax
	movq	8(%rax), %rax
	leaq	16(%rax), %rcx
	movq	%rcx, recog_data+280(%rip)
	movq	16(%rax), %rax
.LBB0_73:                               # %sw.epilog
	movq	%rax, recog_data+40(%rip)
	movq	8(%r14), %rax
	movq	16(%rax), %rax
.LBB0_74:                               # %sw.epilog
	leaq	8(%rax), %rcx
	movq	%rcx, recog_data+288(%rip)
	movq	8(%rax), %rax
	movq	%rax, recog_data+48(%rip)
	jmp	.LBB0_223
.LBB0_62:                               # %sw.bb1244
	movq	8(%r14), %rax
	movq	8(%rax), %rax
	leaq	8(%rax), %rcx
	movq	%rcx, recog_data+240(%rip)
	movq	8(%rax), %rax
	movq	%rax, recog_data(%rip)
	movq	8(%r14), %rax
	movq	8(%rax), %rax
	movq	16(%rax), %rax
	leaq	16(%rax), %rcx
	movq	%rcx, recog_data+248(%rip)
	movq	16(%rax), %rax
	movq	%rax, recog_data+8(%rip)
	movq	8(%r14), %rax
	movq	8(%rax), %rax
	movq	16(%rax), %rax
	leaq	24(%rax), %rcx
	movq	%rcx, recog_data+256(%rip)
	movq	24(%rax), %rax
	movq	%rax, recog_data+16(%rip)
	movq	8(%r14), %rax
	movq	8(%rax), %rax
	movq	16(%rax), %rax
	movq	8(%rax), %rax
	leaq	8(%rax), %rcx
	movq	%rcx, recog_data+264(%rip)
	movq	8(%rax), %rax
	movq	%rax, recog_data+24(%rip)
	movq	8(%r14), %rax
	movq	8(%rax), %rax
	movq	16(%rax), %rax
	movq	8(%rax), %rax
	leaq	16(%rax), %rcx
	movq	%rcx, recog_data+272(%rip)
	movq	16(%rax), %rax
	movq	%rax, recog_data+32(%rip)
	movq	8(%r14), %rax
	movq	16(%rax), %rax
	jmp	.LBB0_191
.LBB0_60:                               # %sw.bb1160
	movq	8(%r14), %rax
	movq	8(%rax), %rax
	movq	8(%rax), %rax
	leaq	8(%rax), %rcx
	movq	%rcx, recog_data+240(%rip)
	movq	8(%rax), %rax
	movq	%rax, recog_data(%rip)
	movq	8(%r14), %rax
	movq	24(%rax), %rax
	addq	$8, %rax
	movq	%rax, recog_data+960(%rip)
	xorq	%rax, %rax
	movb	%al, recog_data+992(%rip)
	movq	8(%r14), %rcx
	movq	16(%rcx), %rcx
	movq	16(%rcx), %rcx
	addq	$16, %rcx
	movq	%rcx, recog_data+968(%rip)
	movb	%al, recog_data+993(%rip)
	jmp	.LBB0_223
.LBB0_59:                               # %sw.bb1107
	movq	8(%r14), %rax
	movq	8(%rax), %rax
	leaq	8(%rax), %rcx
	movq	%rcx, recog_data+240(%rip)
	movq	8(%rax), %rax
	movq	%rax, recog_data(%rip)
	movq	8(%r14), %rax
	movq	8(%rax), %rax
	movq	16(%rax), %rax
	movq	8(%rax), %rax
	leaq	8(%rax), %rcx
	movq	%rcx, recog_data+248(%rip)
	movq	8(%rax), %rax
	movq	%rax, recog_data+8(%rip)
	movq	8(%r14), %rax
	movq	8(%rax), %rax
	movq	16(%rax), %rax
	leaq	16(%rax), %rcx
	movq	%rcx, recog_data+256(%rip)
	movq	16(%rax), %rax
	movq	%rax, recog_data+16(%rip)
	movq	8(%r14), %rax
	movq	16(%rax), %rax
	movq	16(%rax), %rax
.LBB0_118:                              # %sw.epilog
	leaq	16(%rax), %rcx
	movq	%rcx, recog_data+264(%rip)
	movq	16(%rax), %rax
	movq	%rax, recog_data+24(%rip)
	jmp	.LBB0_223
.LBB0_54:                               # %sw.bb1053
	movq	8(%r14), %rax
	leaq	8(%rax), %rcx
	movq	%rcx, recog_data+240(%rip)
	movq	8(%rax), %rax
.LBB0_23:                               # %sw.epilog
	movq	%rax, recog_data(%rip)
	movq	16(%r14), %rax
	movq	8(%rax), %rax
	leaq	8(%rax), %rcx
	movq	%rcx, recog_data+248(%rip)
	movq	8(%rax), %rax
	movq	%rax, recog_data+8(%rip)
	movq	16(%r14), %rax
	movq	8(%rax), %rax
	jmp	.LBB0_103
.LBB0_53:                               # %sw.bb1028
	leaq	8(%r14), %rax
	movq	%rax, recog_data+240(%rip)
	movq	8(%r14), %rax
	movq	%rax, recog_data(%rip)
	movq	16(%r14), %rax
	jmp	.LBB0_39
.LBB0_47:                               # %sw.bb909
	leaq	8(%r14), %rax
	movq	%rax, recog_data+240(%rip)
	movq	8(%r14), %rax
	jmp	.LBB0_48
.LBB0_44:                               # %sw.bb819
	movq	8(%r14), %rax
	leaq	8(%rax), %rcx
	movq	%rcx, recog_data+240(%rip)
	movq	8(%rax), %rax
.LBB0_48:                               # %sw.epilog
	movq	%rax, recog_data(%rip)
	movq	16(%r14), %rax
	movq	8(%rax), %rax
.LBB0_57:                               # %sw.epilog
	movq	8(%rax), %rax
.LBB0_58:                               # %sw.epilog
	leaq	8(%rax), %rcx
	movq	%rcx, recog_data+248(%rip)
	movq	8(%rax), %rax
	movq	%rax, recog_data+8(%rip)
	movq	16(%r14), %rax
.LBB0_103:                              # %sw.epilog
	leaq	16(%rax), %rcx
	movq	%rcx, recog_data+256(%rip)
	movq	16(%rax), %rax
	movq	%rax, recog_data+16(%rip)
	jmp	.LBB0_223
.LBB0_40:                               # %sw.bb659
	movq	16(%r14), %rax
	movq	8(%rax), %rax
	leaq	8(%rax), %rcx
	movq	%rcx, recog_data+240(%rip)
	movq	8(%rax), %rax
	movq	%rax, recog_data(%rip)
	movq	16(%r14), %rax
	movq	16(%rax), %rax
	leaq	8(%rax), %rcx
	movq	%rcx, recog_data+248(%rip)
	movq	8(%rax), %rax
	leaq	16(%r14), %rcx
	movq	%rax, recog_data+8(%rip)
	movq	%rcx, recog_data+256(%rip)
	movq	16(%r14), %rax
	movq	%rax, recog_data+16(%rip)
	jmp	.LBB0_223
.LBB0_36:                               # %sw.bb603
	leaq	8(%r14), %rax
	movq	%rax, recog_data+240(%rip)
	movq	8(%r14), %rax
	movq	%rax, recog_data(%rip)
	movq	16(%r14), %rax
	leaq	8(%rax), %rcx
	movq	%rcx, recog_data+248(%rip)
	movq	8(%rax), %rax
	movq	%rax, recog_data+8(%rip)
	movq	16(%r14), %rax
	jmp	.LBB0_26
.LBB0_32:                               # %sw.bb544
	leaq	8(%r14), %rax
	movq	%rax, recog_data+240(%rip)
	movq	8(%r14), %rax
	movq	%rax, recog_data(%rip)
	movq	16(%r14), %rax
	movq	8(%rax), %rax
	movq	8(%rax), %rax
	movq	8(%rax), %rax
	leaq	8(%rax), %rcx
	movq	%rcx, recog_data+248(%rip)
	movq	8(%rax), %rax
	movq	%rax, recog_data+8(%rip)
	movq	16(%r14), %rax
	jmp	.LBB0_33
.LBB0_211:                              # %sw.bb5725
	movq	16(%r14), %rax
	movq	8(%rax), %rax
	movq	8(%rax), %rax
.LBB0_214:                              # %sw.epilog
	leaq	8(%rax), %rcx
	movq	%rcx, recog_data+240(%rip)
	movq	8(%rax), %rax
	movq	%rax, recog_data(%rip)
	movq	16(%r14), %rax
	movq	16(%rax), %rax
	jmp	.LBB0_15
.LBB0_210:                              # %sw.bb5673
	movq	16(%r14), %rax
	leaq	8(%rax), %rcx
	movq	%rcx, recog_data+240(%rip)
	movq	8(%rax), %rax
	movq	%rax, recog_data(%rip)
	movq	16(%r14), %rax
	movq	16(%rax), %rax
	jmp	.LBB0_16
.LBB0_197:                              # %sw.bb5487
	movq	8(%r14), %rax
	leaq	8(%rax), %rcx
	movq	%rcx, recog_data+240(%rip)
	movq	8(%rax), %rax
	jmp	.LBB0_85
.LBB0_193:                              # %sw.bb5429
	movq	8(%r14), %rax
	movq	8(%rax), %rax
	leaq	8(%rax), %rcx
	movq	%rcx, recog_data+240(%rip)
	movq	8(%rax), %rax
	movq	%rax, recog_data(%rip)
	movq	8(%r14), %rax
	movq	8(%rax), %rax
	movq	16(%rax), %rax
	leaq	8(%rax), %rcx
	movq	%rcx, recog_data+248(%rip)
	movq	8(%rax), %rax
	movq	%rax, recog_data+8(%rip)
	movq	8(%r14), %rax
	movq	24(%rax), %rax
	jmp	.LBB0_35
.LBB0_190:                              # %sw.bb5331
	movq	8(%r14), %rax
	movq	8(%rax), %rax
	leaq	8(%rax), %rcx
	movq	%rcx, recog_data+240(%rip)
	movq	8(%rax), %rax
	movq	%rax, recog_data(%rip)
	movq	8(%r14), %rax
	movq	8(%rax), %rax
	movq	16(%rax), %rax
	leaq	8(%rax), %rcx
	movq	%rcx, recog_data+248(%rip)
	movq	8(%rax), %rax
	movq	%rax, recog_data+8(%rip)
	movq	8(%r14), %rax
	movq	16(%rax), %rax
	leaq	8(%rax), %rcx
	movq	%rcx, recog_data+256(%rip)
	movq	8(%rax), %rax
	movq	%rax, recog_data+16(%rip)
	movq	8(%r14), %rax
	movq	24(%rax), %rax
	leaq	8(%rax), %rcx
	movq	%rcx, recog_data+264(%rip)
	movq	8(%rax), %rax
	movq	%rax, recog_data+24(%rip)
	movq	8(%r14), %rax
	movq	32(%rax), %rax
	leaq	8(%rax), %rcx
	movq	%rcx, recog_data+272(%rip)
	movq	8(%rax), %rax
	movq	%rax, recog_data+32(%rip)
	movq	8(%r14), %rax
	movq	40(%rax), %rax
.LBB0_191:                              # %sw.epilog
	leaq	8(%rax), %rcx
	movq	%rcx, recog_data+280(%rip)
	movq	8(%rax), %rax
	movq	%rax, recog_data+40(%rip)
	jmp	.LBB0_223
.LBB0_181:                              # %sw.bb5047
	movq	8(%r14), %rax
	movq	8(%rax), %rax
	leaq	8(%rax), %rcx
	movq	%rcx, recog_data+240(%rip)
	movq	8(%rax), %rax
	movq	%rax, recog_data(%rip)
	movq	8(%r14), %rax
	movq	8(%rax), %rax
	movq	16(%rax), %rax
	movq	8(%rax), %rax
	movq	8(%rax), %rax
	leaq	16(%rax), %rcx
	movq	%rcx, recog_data+248(%rip)
	movq	16(%rax), %rax
	jmp	.LBB0_166
.LBB0_178:                              # %sw.bb4913
	leaq	8(%r14), %rax
	movq	%rax, recog_data+240(%rip)
	movq	8(%r14), %rax
	movq	%rax, recog_data(%rip)
	movq	16(%r14), %rax
	movq	8(%rax), %rax
	movq	8(%rax), %rax
	leaq	8(%rax), %rcx
	movq	%rcx, recog_data+248(%rip)
	movq	8(%rax), %rax
	movq	%rax, recog_data+8(%rip)
	movq	16(%r14), %rax
	movq	8(%rax), %rax
	movq	8(%rax), %rax
	leaq	16(%rax), %rcx
	movq	%rcx, recog_data+256(%rip)
	movq	16(%rax), %rax
	movq	%rax, recog_data+16(%rip)
	movq	16(%r14), %rax
	movq	8(%rax), %rax
	leaq	16(%rax), %rcx
	movq	%rcx, recog_data+264(%rip)
	movq	16(%rax), %rax
	movq	%rax, recog_data+24(%rip)
	movq	16(%r14), %rax
	jmp	.LBB0_177
.LBB0_175:                              # %sw.bb4848
	leaq	8(%r14), %rax
	movq	%rax, recog_data+240(%rip)
	movq	8(%r14), %rax
	movq	%rax, recog_data(%rip)
	movq	16(%r14), %rax
	movq	8(%rax), %rax
	movq	8(%rax), %rax
	movq	8(%rax), %rax
	leaq	8(%rax), %rcx
	movq	%rcx, recog_data+248(%rip)
	movq	8(%rax), %rax
	movq	%rax, recog_data+8(%rip)
	movq	16(%r14), %rax
	movq	8(%rax), %rax
	movq	8(%rax), %rax
	movq	8(%rax), %rax
	leaq	16(%rax), %rcx
	movq	%rcx, recog_data+256(%rip)
	movq	16(%rax), %rax
	movq	%rax, recog_data+16(%rip)
	movq	16(%r14), %rax
	movq	8(%rax), %rax
	movq	8(%rax), %rax
	leaq	16(%rax), %rcx
	movq	%rcx, recog_data+264(%rip)
	movq	16(%rax), %rax
	movq	%rax, recog_data+24(%rip)
	movq	16(%r14), %rax
	jmp	.LBB0_176
.LBB0_162:                              # %sw.bb4481
	movq	8(%r14), %rax
	movq	16(%rax), %rax
	leaq	8(%rax), %rcx
	movq	%rcx, recog_data+240(%rip)
	movq	8(%rax), %rax
	movq	%rax, recog_data(%rip)
	movq	8(%r14), %rax
	movq	8(%rax), %rax
	movq	16(%rax), %rax
	leaq	16(%rax), %rcx
	movq	%rcx, recog_data+248(%rip)
	movq	16(%rax), %rax
	movq	%rax, recog_data+8(%rip)
	movq	8(%r14), %rax
	movq	8(%rax), %rax
	movq	16(%rax), %rax
	movq	8(%rax), %rax
	leaq	8(%rax), %rcx
	movq	%rcx, recog_data+256(%rip)
	movq	8(%rax), %rax
	jmp	.LBB0_151
.LBB0_152:                              # %sw.bb4247
	movq	8(%r14), %rax
	movq	8(%rax), %rax
	leaq	8(%rax), %rcx
	movq	%rcx, recog_data+240(%rip)
	movq	8(%rax), %rax
	movq	%rax, recog_data(%rip)
	movq	8(%r14), %rax
	movq	8(%rax), %rax
	movq	16(%rax), %rax
	movq	8(%rax), %rax
	leaq	8(%rax), %rcx
	movq	%rcx, recog_data+248(%rip)
	movq	8(%rax), %rax
	movq	%rax, recog_data+8(%rip)
	movq	8(%r14), %rax
	movq	8(%rax), %rax
	movq	16(%rax), %rax
	movq	8(%rax), %rax
	jmp	.LBB0_102
.LBB0_149:                              # %sw.bb4182
	movq	8(%r14), %rax
	movq	16(%rax), %rax
	leaq	8(%rax), %rcx
	movq	%rcx, recog_data+240(%rip)
	movq	8(%rax), %rax
	movq	%rax, recog_data(%rip)
	movq	8(%r14), %rax
	movq	8(%rax), %rax
	movq	16(%rax), %rax
	leaq	8(%rax), %rcx
	movq	%rcx, recog_data+248(%rip)
	movq	8(%rax), %rax
	movq	%rax, recog_data+8(%rip)
	movq	8(%r14), %rax
	movq	8(%rax), %rax
	movq	16(%rax), %rax
	jmp	.LBB0_150
.LBB0_136:                              # %sw.bb3683
	movq	16(%r14), %rax
	movq	8(%rax), %rax
	jmp	.LBB0_217
.LBB0_123:                              # %sw.bb3266
	movq	8(%r14), %rax
	movq	8(%rax), %rax
	leaq	8(%rax), %rcx
	movq	%rcx, recog_data+240(%rip)
	movq	8(%rax), %rax
	movq	%rax, recog_data(%rip)
	movq	8(%r14), %rax
	movq	8(%rax), %rax
	movq	16(%rax), %rax
.LBB0_14:                               # %sw.epilog
	movq	8(%rax), %rax
	jmp	.LBB0_15
.LBB0_122:                              # %sw.bb3216
	movq	8(%r14), %rax
	movq	16(%rax), %rax
	leaq	8(%rax), %rcx
	movq	%rcx, recog_data+240(%rip)
	movq	8(%rax), %rax
	movq	%rax, recog_data(%rip)
	movq	8(%r14), %rax
	movq	8(%rax), %rax
	movq	16(%rax), %rax
	movq	8(%rax), %rax
	movq	8(%rax), %rax
	movq	8(%rax), %rax
	leaq	8(%rax), %rcx
	movq	%rcx, recog_data+248(%rip)
	movq	8(%rax), %rax
	movq	%rax, recog_data+8(%rip)
	movq	8(%r14), %rax
	movq	16(%rax), %rax
	jmp	.LBB0_5
.LBB0_121:                              # %sw.bb3175
	movq	8(%r14), %rax
	movq	16(%rax), %rax
	leaq	8(%rax), %rcx
	movq	%rcx, recog_data+240(%rip)
	movq	8(%rax), %rax
	movq	%rax, recog_data(%rip)
	movq	8(%r14), %rax
	movq	8(%rax), %rax
	movq	16(%rax), %rax
	movq	8(%rax), %rax
	jmp	.LBB0_92
.LBB0_115:                              # %sw.bb3007
	movq	16(%r14), %rax
	movq	16(%rax), %rax
	jmp	.LBB0_113
.LBB0_112:                              # %sw.bb2987
	movq	16(%r14), %rax
	movq	24(%rax), %rax
.LBB0_113:                              # %sw.epilog
	leaq	8(%rax), %rcx
	movq	%rcx, recog_data+240(%rip)
	movq	8(%rax), %rax
	jmp	.LBB0_114
.LBB0_107:                              # %sw.bb2710
	movq	16(%r14), %rax
.LBB0_94:                               # %sw.epilog
	leaq	8(%rax), %rcx
	movq	%rcx, recog_data+240(%rip)
	movq	8(%rax), %rax
	movq	%rax, recog_data(%rip)
	jmp	.LBB0_223
.LBB0_104:                              # %sw.bb2616
	movq	8(%r14), %rax
	movq	8(%rax), %rax
	movq	16(%rax), %rax
	movq	16(%rax), %rax
	leaq	8(%rax), %rcx
	movq	%rcx, recog_data+240(%rip)
	movq	8(%rax), %rax
	movq	%rax, recog_data(%rip)
	movq	8(%r14), %rax
	movq	8(%rax), %rax
	movq	16(%rax), %rax
	movq	8(%rax), %rax
	leaq	8(%rax), %rcx
	movq	%rcx, recog_data+248(%rip)
	movq	8(%rax), %rax
	movq	%rax, recog_data+8(%rip)
	movq	8(%r14), %rax
	movq	16(%rax), %rax
	leaq	8(%rax), %rcx
	movq	%rcx, recog_data+256(%rip)
	movq	8(%rax), %rax
	movq	%rax, recog_data+16(%rip)
	movq	8(%r14), %rax
	movq	24(%rax), %rax
	leaq	8(%rax), %rcx
	movq	%rcx, recog_data+264(%rip)
	movq	8(%rax), %rax
	movq	%rax, recog_data+24(%rip)
	jmp	.LBB0_125
.LBB0_98:                               # %sw.bb2495
	movq	8(%r14), %rax
	movq	8(%rax), %rax
	leaq	8(%rax), %rcx
	movq	%rcx, recog_data+240(%rip)
	movq	8(%rax), %rax
	movq	%rax, recog_data(%rip)
	movq	8(%r14), %rax
	movq	8(%rax), %rax
	movq	16(%rax), %rax
	movq	8(%rax), %rax
	movq	8(%rax), %rax
	movq	16(%rax), %rax
	leaq	8(%rax), %rcx
	movq	%rcx, recog_data+248(%rip)
	movq	8(%rax), %rax
	movq	%rax, recog_data+8(%rip)
	movq	8(%r14), %rax
	movq	8(%rax), %rax
	movq	16(%rax), %rax
	movq	8(%rax), %rax
	movq	8(%rax), %rax
	movq	16(%rax), %rax
	movq	16(%rax), %rax
	leaq	16(%rax), %rcx
	movq	%rcx, recog_data+256(%rip)
	movq	16(%rax), %rax
	movq	%rax, recog_data+16(%rip)
	movq	8(%r14), %rax
	movq	8(%rax), %rax
	movq	16(%rax), %rax
	movq	8(%rax), %rax
	jmp	.LBB0_20
.LBB0_95:                               # %sw.bb2476
	leaq	8(%r14), %rax
	movq	%rax, recog_data+240(%rip)
	movq	8(%r14), %rax
	movq	%rax, recog_data(%rip)
	movq	16(%r14), %rax
	movq	8(%rax), %rax
	jmp	.LBB0_96
.LBB0_91:                               # %sw.bb2431
	movq	8(%r14), %rax
	movq	16(%rax), %rax
	leaq	8(%rax), %rcx
	movq	%rcx, recog_data+240(%rip)
	movq	8(%rax), %rax
	movq	%rax, recog_data(%rip)
	movq	8(%r14), %rax
	movq	8(%rax), %rax
	movq	16(%rax), %rax
.LBB0_92:                               # %sw.epilog
	leaq	8(%rax), %rcx
	movq	%rcx, recog_data+248(%rip)
	movq	8(%rax), %rax
	movq	%rax, recog_data+8(%rip)
	movq	8(%r14), %rax
	movq	16(%rax), %rax
	movq	16(%rax), %rax
	jmp	.LBB0_6
.LBB0_51:                               # %sw.bb970
	leaq	8(%r14), %rax
	movq	%rax, recog_data+240(%rip)
	movq	8(%r14), %rax
	movq	%rax, recog_data(%rip)
	movq	16(%r14), %rax
	movq	8(%rax), %rax
	leaq	8(%rax), %rcx
	movq	%rcx, recog_data+248(%rip)
	movq	8(%rax), %rax
	movq	%rax, recog_data+8(%rip)
	movq	16(%r14), %rax
	movq	8(%rax), %rax
	leaq	16(%rax), %rcx
	movq	%rcx, recog_data+256(%rip)
	movq	16(%rax), %rax
	movq	%rax, recog_data+16(%rip)
	movq	16(%r14), %rax
	movq	8(%rax), %rax
	jmp	.LBB0_69
.LBB0_43:                               # %sw.bb783
	leaq	8(%r14), %rax
	movq	%rax, recog_data+240(%rip)
	movq	8(%r14), %rax
	movq	%rax, recog_data(%rip)
	movq	16(%r14), %rax
	movq	8(%rax), %rax
	leaq	8(%rax), %rcx
	movq	%rcx, recog_data+248(%rip)
	movq	8(%rax), %rax
	movq	%rax, recog_data+8(%rip)
	movq	16(%r14), %rax
	movq	8(%rax), %rax
	leaq	16(%rax), %rcx
	movq	%rcx, recog_data+256(%rip)
	movq	16(%rax), %rax
	movq	%rax, recog_data+16(%rip)
	movq	16(%r14), %rax
	jmp	.LBB0_146
.LBB0_42:                               # %sw.bb741
	leaq	8(%r14), %rax
	movq	%rax, recog_data+240(%rip)
	movq	8(%r14), %rax
	movq	%rax, recog_data(%rip)
	movq	16(%r14), %rax
	movq	8(%rax), %rax
	leaq	8(%rax), %rcx
	movq	%rcx, recog_data+248(%rip)
	movq	8(%rax), %rax
	movq	%rax, recog_data+8(%rip)
	movq	16(%r14), %rax
	movq	8(%rax), %rax
	leaq	16(%rax), %rcx
	movq	%rcx, recog_data+256(%rip)
	movq	16(%rax), %rax
	movq	%rax, recog_data+16(%rip)
	movq	16(%r14), %rax
	leaq	8(%rax), %rcx
	movq	%rcx, recog_data+264(%rip)
	movq	8(%rax), %rax
	movq	%rax, recog_data+24(%rip)
	jmp	.LBB0_50
.LBB0_41:                               # %sw.bb687
	leaq	8(%r14), %rax
	movq	%rax, recog_data+240(%rip)
	movq	8(%r14), %rax
	movq	%rax, recog_data(%rip)
	movq	16(%r14), %rax
	movq	8(%rax), %rax
	movq	8(%rax), %rax
	leaq	8(%rax), %rcx
	movq	%rcx, recog_data+248(%rip)
	movq	8(%rax), %rax
	movq	%rax, recog_data+8(%rip)
	movq	16(%r14), %rax
	movq	8(%rax), %rax
	movq	8(%rax), %rax
	leaq	16(%rax), %rcx
	movq	%rcx, recog_data+256(%rip)
	movq	16(%rax), %rax
	movq	%rax, recog_data+16(%rip)
	movq	16(%r14), %rax
	movq	8(%rax), %rax
	leaq	8(%rax), %rcx
	movq	%rcx, recog_data+264(%rip)
	movq	8(%rax), %rax
	movq	%rax, recog_data+24(%rip)
	movq	16(%r14), %rax
	movq	16(%rax), %rax
	jmp	.LBB0_7
.LBB0_31:                               # %sw.bb474
	leaq	8(%r14), %rax
	movq	%rax, recog_data+240(%rip)
	movq	8(%r14), %rax
	movq	%rax, recog_data(%rip)
	movq	16(%r14), %rax
	movq	8(%rax), %rax
	movq	8(%rax), %rax
	movq	8(%rax), %rax
	leaq	8(%rax), %rcx
	movq	%rcx, recog_data+248(%rip)
	movq	8(%rax), %rax
	movq	%rax, recog_data+8(%rip)
	movq	16(%r14), %rax
	movq	8(%rax), %rax
	movq	16(%rax), %rax
	movq	8(%rax), %rax
	leaq	8(%rax), %rcx
	movq	%rcx, recog_data+256(%rip)
	movq	8(%rax), %rax
	movq	%rax, recog_data+16(%rip)
	movq	16(%r14), %rax
	movq	16(%rax), %rax
	movq	8(%rax), %rax
	movq	8(%rax), %rax
	addq	$8, %rax
	movq	%rax, recog_data+960(%rip)
	movq	$1, %rax
	movb	%al, recog_data+992(%rip)
	movq	16(%r14), %rax
	movq	16(%rax), %rax
	movq	16(%rax), %rax
	movq	8(%rax), %rax
	jmp	.LBB0_11
.LBB0_30:                               # %sw.bb439
	leaq	8(%r14), %rax
	movq	%rax, recog_data+240(%rip)
	movq	8(%r14), %rax
	movq	%rax, recog_data(%rip)
	movq	16(%r14), %rax
	movq	8(%rax), %rax
	jmp	.LBB0_29
.LBB0_27:                               # %sw.bb341
	leaq	8(%r14), %rax
	movq	%rax, recog_data+240(%rip)
	movq	8(%r14), %rax
	movq	%rax, recog_data(%rip)
	movq	16(%r14), %rax
	leaq	8(%rax), %rcx
	movq	%rcx, recog_data+248(%rip)
	movq	8(%rax), %rax
	movq	%rax, recog_data+8(%rip)
	movq	16(%r14), %rax
	movq	16(%rax), %rax
	movq	8(%rax), %rax
	leaq	8(%rax), %rcx
	movq	%rcx, recog_data+256(%rip)
	movq	8(%rax), %rax
	jmp	.LBB0_68
.LBB0_24:                               # %sw.bb308
	leaq	8(%r14), %rax
	movq	%rax, recog_data+240(%rip)
	movq	8(%r14), %rax
	movq	%rax, recog_data(%rip)
	movq	16(%r14), %rax
	movq	8(%rax), %rax
	leaq	8(%rax), %rcx
	movq	%rcx, recog_data+248(%rip)
	movq	8(%rax), %rax
	movq	%rax, recog_data+8(%rip)
	movq	16(%r14), %rax
.LBB0_25:                               # %sw.epilog
	movq	8(%rax), %rax
.LBB0_26:                               # %sw.epilog
	movq	16(%rax), %rax
	movq	8(%rax), %rax
	jmp	.LBB0_35
.LBB0_19:                               # %sw.bb268
	leaq	8(%r14), %rax
	movq	%rax, recog_data+240(%rip)
	movq	8(%r14), %rax
	movq	%rax, recog_data(%rip)
	movq	16(%r14), %rax
.LBB0_20:                               # %sw.epilog
	movq	8(%rax), %rax
.LBB0_21:                               # %sw.epilog
	addq	$8, %rax
	movq	%rax, recog_data+960(%rip)
	xorq	%rax, %rax
	movb	%al, recog_data+992(%rip)
	jmp	.LBB0_223
.LBB0_18:                               # %sw.bb204
	movq	8(%r14), %rax
	movq	8(%rax), %rax
	movq	8(%rax), %rax
	movq	8(%rax), %rax
	leaq	8(%rax), %rcx
	movq	%rcx, recog_data+240(%rip)
	movq	8(%rax), %rax
	movq	%rax, recog_data(%rip)
	movq	8(%r14), %rax
	movq	16(%rax), %rax
	leaq	8(%rax), %rcx
	movq	%rcx, recog_data+248(%rip)
	movq	8(%rax), %rax
	movq	%rax, recog_data+8(%rip)
	movq	8(%r14), %rax
	movq	24(%rax), %rax
	leaq	8(%rax), %rcx
	movq	%rcx, recog_data+256(%rip)
	movq	8(%rax), %rax
	movq	%rax, recog_data+16(%rip)
	movq	8(%r14), %rax
	movq	32(%rax), %rax
	movq	8(%rax), %rax
	leaq	8(%rax), %rcx
	movq	%rcx, recog_data+264(%rip)
	movq	8(%rax), %rax
	movq	%rax, recog_data+24(%rip)
	movq	8(%r14), %rax
	movq	8(%rax), %rax
	movq	8(%rax), %rax
.LBB0_176:                              # %sw.epilog
	movq	8(%rax), %rax
.LBB0_177:                              # %sw.epilog
	leaq	16(%rax), %rcx
	movq	%rcx, recog_data+272(%rip)
	movq	16(%rax), %rax
	movq	%rax, recog_data+32(%rip)
	jmp	.LBB0_223
.LBB0_17:                               # %sw.bb182
	leaq	8(%r14), %rax
	movq	%rax, recog_data+240(%rip)
	movq	8(%r14), %rax
	movq	%rax, recog_data(%rip)
	movq	16(%r14), %rax
	leaq	16(%rax), %rcx
	movq	%rcx, recog_data+248(%rip)
	movq	16(%rax), %rax
	movq	%rax, recog_data+8(%rip)
	movq	16(%r14), %rax
	jmp	.LBB0_35
.LBB0_4:                                # %sw.bb60
	leaq	8(%r14), %rax
	movq	%rax, recog_data+240(%rip)
	movq	8(%r14), %rax
	movq	%rax, recog_data(%rip)
	movq	16(%r14), %rax
	movq	8(%rax), %rax
	movq	8(%rax), %rax
	movq	8(%rax), %rax
	movq	8(%rax), %rax
	leaq	8(%rax), %rcx
	movq	%rcx, recog_data+248(%rip)
	movq	8(%rax), %rax
	movq	%rax, recog_data+8(%rip)
	movq	16(%r14), %rax
	movq	8(%rax), %rax
.LBB0_5:                                # %sw.epilog
	movq	16(%rax), %rax
	movq	8(%rax), %rax
.LBB0_6:                                # %sw.epilog
	movq	8(%rax), %rax
.LBB0_7:                                # %sw.epilog
	addq	$8, %rax
.LBB0_8:                                # %sw.epilog
	movq	%rax, recog_data+960(%rip)
	movq	$1, %rax
	movb	%al, recog_data+992(%rip)
	jmp	.LBB0_223
.LBB0_3:                                # %sw.bb14
	leaq	8(%r14), %rax
	movq	%rax, recog_data+240(%rip)
	movq	8(%r14), %rax
	movq	%rax, recog_data(%rip)
	movq	16(%r14), %rax
	movq	8(%rax), %rax
	movq	8(%rax), %rax
	movq	8(%rax), %rax
	movq	8(%rax), %rax
	leaq	8(%rax), %rcx
	movq	%rcx, recog_data+248(%rip)
	movq	8(%rax), %rax
	movq	%rax, recog_data+8(%rip)
	movq	16(%r14), %rax
	movq	8(%rax), %rax
.LBB0_33:                               # %sw.epilog
	movq	8(%rax), %rax
	movq	8(%rax), %rax
.LBB0_34:                               # %sw.epilog
	movq	16(%rax), %rax
.LBB0_35:                               # %sw.epilog
	leaq	8(%rax), %rcx
	movq	%rcx, recog_data+256(%rip)
	movq	8(%rax), %rax
	movq	%rax, recog_data+16(%rip)
.LBB0_223:                              # %sw.epilog
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.LBB0_221:                              # %sw.bb3
	leaq	8(%r14), %rax
	movq	%rax, recog_data+240(%rip)
	movq	8(%r14), %rax
	movq	%rax, recog_data(%rip)
	leaq	24(%r14), %rax
	movq	%rax, recog_data+248(%rip)
	movq	24(%r14), %rax
	jmp	.LBB0_222
.LBB0_2:                                # %sw.bb
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.insn_extract, %rcx
	movq	$26, %rdx
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	_fatal_insn_not_found
.Ltmp8:
	.size	insn_extract, .Ltmp8-insn_extract
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI0_0:
	.quad	.LBB0_2
	.quad	.LBB0_218
	.quad	.LBB0_216
	.quad	.LBB0_218
	.quad	.LBB0_218
	.quad	.LBB0_216
	.quad	.LBB0_218
	.quad	.LBB0_218
	.quad	.LBB0_216
	.quad	.LBB0_218
	.quad	.LBB0_218
	.quad	.LBB0_218
	.quad	.LBB0_216
	.quad	.LBB0_213
	.quad	.LBB0_213
	.quad	.LBB0_212
	.quad	.LBB0_212
	.quad	.LBB0_212
	.quad	.LBB0_211
	.quad	.LBB0_28
	.quad	.LBB0_218
	.quad	.LBB0_28
	.quad	.LBB0_218
	.quad	.LBB0_28
	.quad	.LBB0_218
	.quad	.LBB0_218
	.quad	.LBB0_28
	.quad	.LBB0_28
	.quad	.LBB0_218
	.quad	.LBB0_28
	.quad	.LBB0_210
	.quad	.LBB0_209
	.quad	.LBB0_208
	.quad	.LBB0_218
	.quad	.LBB0_218
	.quad	.LBB0_218
	.quad	.LBB0_218
	.quad	.LBB0_218
	.quad	.LBB0_218
	.quad	.LBB0_206
	.quad	.LBB0_206
	.quad	.LBB0_204
	.quad	.LBB0_202
	.quad	.LBB0_202
	.quad	.LBB0_204
	.quad	.LBB0_204
	.quad	.LBB0_206
	.quad	.LBB0_201
	.quad	.LBB0_200
	.quad	.LBB0_199
	.quad	.LBB0_206
	.quad	.LBB0_206
	.quad	.LBB0_206
	.quad	.LBB0_201
	.quad	.LBB0_200
	.quad	.LBB0_199
	.quad	.LBB0_199
	.quad	.LBB0_201
	.quad	.LBB0_198
	.quad	.LBB0_206
	.quad	.LBB0_206
	.quad	.LBB0_206
	.quad	.LBB0_199
	.quad	.LBB0_201
	.quad	.LBB0_198
	.quad	.LBB0_200
	.quad	.LBB0_200
	.quad	.LBB0_200
	.quad	.LBB0_200
	.quad	.LBB0_201
	.quad	.LBB0_200
	.quad	.LBB0_200
	.quad	.LBB0_84
	.quad	.LBB0_84
	.quad	.LBB0_201
	.quad	.LBB0_201
	.quad	.LBB0_197
	.quad	.LBB0_206
	.quad	.LBB0_206
	.quad	.LBB0_204
	.quad	.LBB0_202
	.quad	.LBB0_202
	.quad	.LBB0_204
	.quad	.LBB0_204
	.quad	.LBB0_206
	.quad	.LBB0_206
	.quad	.LBB0_201
	.quad	.LBB0_200
	.quad	.LBB0_199
	.quad	.LBB0_206
	.quad	.LBB0_206
	.quad	.LBB0_206
	.quad	.LBB0_199
	.quad	.LBB0_206
	.quad	.LBB0_206
	.quad	.LBB0_206
	.quad	.LBB0_206
	.quad	.LBB0_199
	.quad	.LBB0_206
	.quad	.LBB0_206
	.quad	.LBB0_206
	.quad	.LBB0_206
	.quad	.LBB0_206
	.quad	.LBB0_206
	.quad	.LBB0_206
	.quad	.LBB0_206
	.quad	.LBB0_199
	.quad	.LBB0_199
	.quad	.LBB0_194
	.quad	.LBB0_200
	.quad	.LBB0_194
	.quad	.LBB0_194
	.quad	.LBB0_200
	.quad	.LBB0_194
	.quad	.LBB0_194
	.quad	.LBB0_200
	.quad	.LBB0_194
	.quad	.LBB0_200
	.quad	.LBB0_200
	.quad	.LBB0_200
	.quad	.LBB0_193
	.quad	.LBB0_200
	.quad	.LBB0_200
	.quad	.LBB0_200
	.quad	.LBB0_200
	.quad	.LBB0_84
	.quad	.LBB0_200
	.quad	.LBB0_200
	.quad	.LBB0_84
	.quad	.LBB0_200
	.quad	.LBB0_200
	.quad	.LBB0_200
	.quad	.LBB0_200
	.quad	.LBB0_200
	.quad	.LBB0_200
	.quad	.LBB0_200
	.quad	.LBB0_200
	.quad	.LBB0_200
	.quad	.LBB0_200
	.quad	.LBB0_200
	.quad	.LBB0_192
	.quad	.LBB0_192
	.quad	.LBB0_200
	.quad	.LBB0_200
	.quad	.LBB0_200
	.quad	.LBB0_192
	.quad	.LBB0_200
	.quad	.LBB0_192
	.quad	.LBB0_200
	.quad	.LBB0_192
	.quad	.LBB0_200
	.quad	.LBB0_192
	.quad	.LBB0_200
	.quad	.LBB0_200
	.quad	.LBB0_190
	.quad	.LBB0_187
	.quad	.LBB0_200
	.quad	.LBB0_200
	.quad	.LBB0_200
	.quad	.LBB0_187
	.quad	.LBB0_185
	.quad	.LBB0_200
	.quad	.LBB0_200
	.quad	.LBB0_200
	.quad	.LBB0_187
	.quad	.LBB0_185
	.quad	.LBB0_209
	.quad	.LBB0_208
	.quad	.LBB0_200
	.quad	.LBB0_200
	.quad	.LBB0_200
	.quad	.LBB0_200
	.quad	.LBB0_200
	.quad	.LBB0_200
	.quad	.LBB0_200
	.quad	.LBB0_200
	.quad	.LBB0_200
	.quad	.LBB0_200
	.quad	.LBB0_200
	.quad	.LBB0_200
	.quad	.LBB0_200
	.quad	.LBB0_200
	.quad	.LBB0_200
	.quad	.LBB0_200
	.quad	.LBB0_200
	.quad	.LBB0_200
	.quad	.LBB0_184
	.quad	.LBB0_183
	.quad	.LBB0_182
	.quad	.LBB0_183
	.quad	.LBB0_181
	.quad	.LBB0_182
	.quad	.LBB0_182
	.quad	.LBB0_206
	.quad	.LBB0_200
	.quad	.LBB0_84
	.quad	.LBB0_206
	.quad	.LBB0_180
	.quad	.LBB0_179
	.quad	.LBB0_180
	.quad	.LBB0_179
	.quad	.LBB0_178
	.quad	.LBB0_175
	.quad	.LBB0_184
	.quad	.LBB0_182
	.quad	.LBB0_174
	.quad	.LBB0_169
	.quad	.LBB0_168
	.quad	.LBB0_184
	.quad	.LBB0_164
	.quad	.LBB0_182
	.quad	.LBB0_163
	.quad	.LBB0_174
	.quad	.LBB0_162
	.quad	.LBB0_169
	.quad	.LBB0_168
	.quad	.LBB0_184
	.quad	.LBB0_184
	.quad	.LBB0_182
	.quad	.LBB0_174
	.quad	.LBB0_169
	.quad	.LBB0_168
	.quad	.LBB0_184
	.quad	.LBB0_184
	.quad	.LBB0_182
	.quad	.LBB0_174
	.quad	.LBB0_169
	.quad	.LBB0_168
	.quad	.LBB0_161
	.quad	.LBB0_161
	.quad	.LBB0_160
	.quad	.LBB0_184
	.quad	.LBB0_159
	.quad	.LBB0_184
	.quad	.LBB0_182
	.quad	.LBB0_153
	.quad	.LBB0_159
	.quad	.LBB0_152
	.quad	.LBB0_184
	.quad	.LBB0_164
	.quad	.LBB0_182
	.quad	.LBB0_163
	.quad	.LBB0_153
	.quad	.LBB0_149
	.quad	.LBB0_184
	.quad	.LBB0_182
	.quad	.LBB0_153
	.quad	.LBB0_184
	.quad	.LBB0_182
	.quad	.LBB0_153
	.quad	.LBB0_184
	.quad	.LBB0_184
	.quad	.LBB0_164
	.quad	.LBB0_184
	.quad	.LBB0_184
	.quad	.LBB0_147
	.quad	.LBB0_147
	.quad	.LBB0_147
	.quad	.LBB0_147
	.quad	.LBB0_147
	.quad	.LBB0_147
	.quad	.LBB0_142
	.quad	.LBB0_142
	.quad	.LBB0_141
	.quad	.LBB0_142
	.quad	.LBB0_142
	.quad	.LBB0_141
	.quad	.LBB0_184
	.quad	.LBB0_184
	.quad	.LBB0_140
	.quad	.LBB0_140
	.quad	.LBB0_139
	.quad	.LBB0_140
	.quad	.LBB0_140
	.quad	.LBB0_139
	.quad	.LBB0_138
	.quad	.LBB0_138
	.quad	.LBB0_137
	.quad	.LBB0_138
	.quad	.LBB0_137
	.quad	.LBB0_139
	.quad	.LBB0_216
	.quad	.LBB0_216
	.quad	.LBB0_216
	.quad	.LBB0_216
	.quad	.LBB0_136
	.quad	.LBB0_135
	.quad	.LBB0_135
	.quad	.LBB0_135
	.quad	.LBB0_134
	.quad	.LBB0_134
	.quad	.LBB0_184
	.quad	.LBB0_182
	.quad	.LBB0_184
	.quad	.LBB0_164
	.quad	.LBB0_182
	.quad	.LBB0_163
	.quad	.LBB0_184
	.quad	.LBB0_182
	.quad	.LBB0_184
	.quad	.LBB0_133
	.quad	.LBB0_182
	.quad	.LBB0_132
	.quad	.LBB0_161
	.quad	.LBB0_131
	.quad	.LBB0_160
	.quad	.LBB0_160
	.quad	.LBB0_160
	.quad	.LBB0_184
	.quad	.LBB0_182
	.quad	.LBB0_168
	.quad	.LBB0_184
	.quad	.LBB0_164
	.quad	.LBB0_129
	.quad	.LBB0_182
	.quad	.LBB0_163
	.quad	.LBB0_127
	.quad	.LBB0_168
	.quad	.LBB0_184
	.quad	.LBB0_182
	.quad	.LBB0_168
	.quad	.LBB0_184
	.quad	.LBB0_133
	.quad	.LBB0_182
	.quad	.LBB0_132
	.quad	.LBB0_168
	.quad	.LBB0_184
	.quad	.LBB0_182
	.quad	.LBB0_168
	.quad	.LBB0_184
	.quad	.LBB0_164
	.quad	.LBB0_129
	.quad	.LBB0_182
	.quad	.LBB0_163
	.quad	.LBB0_127
	.quad	.LBB0_168
	.quad	.LBB0_184
	.quad	.LBB0_182
	.quad	.LBB0_168
	.quad	.LBB0_184
	.quad	.LBB0_160
	.quad	.LBB0_182
	.quad	.LBB0_168
	.quad	.LBB0_131
	.quad	.LBB0_131
	.quad	.LBB0_194
	.quad	.LBB0_194
	.quad	.LBB0_124
	.quad	.LBB0_194
	.quad	.LBB0_123
	.quad	.LBB0_124
	.quad	.LBB0_122
	.quad	.LBB0_194
	.quad	.LBB0_124
	.quad	.LBB0_194
	.quad	.LBB0_124
	.quad	.LBB0_194
	.quad	.LBB0_192
	.quad	.LBB0_194
	.quad	.LBB0_194
	.quad	.LBB0_192
	.quad	.LBB0_192
	.quad	.LBB0_194
	.quad	.LBB0_194
	.quad	.LBB0_194
	.quad	.LBB0_194
	.quad	.LBB0_200
	.quad	.LBB0_200
	.quad	.LBB0_84
	.quad	.LBB0_200
	.quad	.LBB0_84
	.quad	.LBB0_84
	.quad	.LBB0_200
	.quad	.LBB0_84
	.quad	.LBB0_84
	.quad	.LBB0_194
	.quad	.LBB0_192
	.quad	.LBB0_194
	.quad	.LBB0_194
	.quad	.LBB0_192
	.quad	.LBB0_192
	.quad	.LBB0_194
	.quad	.LBB0_194
	.quad	.LBB0_194
	.quad	.LBB0_194
	.quad	.LBB0_200
	.quad	.LBB0_200
	.quad	.LBB0_84
	.quad	.LBB0_200
	.quad	.LBB0_84
	.quad	.LBB0_84
	.quad	.LBB0_200
	.quad	.LBB0_84
	.quad	.LBB0_84
	.quad	.LBB0_200
	.quad	.LBB0_124
	.quad	.LBB0_200
	.quad	.LBB0_84
	.quad	.LBB0_124
	.quad	.LBB0_121
	.quad	.LBB0_200
	.quad	.LBB0_124
	.quad	.LBB0_200
	.quad	.LBB0_124
	.quad	.LBB0_184
	.quad	.LBB0_182
	.quad	.LBB0_120
	.quad	.LBB0_184
	.quad	.LBB0_119
	.quad	.LBB0_184
	.quad	.LBB0_164
	.quad	.LBB0_182
	.quad	.LBB0_163
	.quad	.LBB0_184
	.quad	.LBB0_184
	.quad	.LBB0_182
	.quad	.LBB0_184
	.quad	.LBB0_184
	.quad	.LBB0_182
	.quad	.LBB0_184
	.quad	.LBB0_184
	.quad	.LBB0_184
	.quad	.LBB0_182
	.quad	.LBB0_182
	.quad	.LBB0_120
	.quad	.LBB0_184
	.quad	.LBB0_119
	.quad	.LBB0_184
	.quad	.LBB0_164
	.quad	.LBB0_184
	.quad	.LBB0_164
	.quad	.LBB0_184
	.quad	.LBB0_164
	.quad	.LBB0_182
	.quad	.LBB0_163
	.quad	.LBB0_182
	.quad	.LBB0_163
	.quad	.LBB0_184
	.quad	.LBB0_184
	.quad	.LBB0_182
	.quad	.LBB0_182
	.quad	.LBB0_184
	.quad	.LBB0_184
	.quad	.LBB0_182
	.quad	.LBB0_182
	.quad	.LBB0_184
	.quad	.LBB0_184
	.quad	.LBB0_182
	.quad	.LBB0_182
	.quad	.LBB0_120
	.quad	.LBB0_184
	.quad	.LBB0_184
	.quad	.LBB0_129
	.quad	.LBB0_184
	.quad	.LBB0_164
	.quad	.LBB0_182
	.quad	.LBB0_127
	.quad	.LBB0_182
	.quad	.LBB0_127
	.quad	.LBB0_184
	.quad	.LBB0_184
	.quad	.LBB0_182
	.quad	.LBB0_182
	.quad	.LBB0_184
	.quad	.LBB0_184
	.quad	.LBB0_182
	.quad	.LBB0_182
	.quad	.LBB0_184
	.quad	.LBB0_184
	.quad	.LBB0_184
	.quad	.LBB0_164
	.quad	.LBB0_184
	.quad	.LBB0_164
	.quad	.LBB0_184
	.quad	.LBB0_184
	.quad	.LBB0_184
	.quad	.LBB0_184
	.quad	.LBB0_184
	.quad	.LBB0_184
	.quad	.LBB0_184
	.quad	.LBB0_164
	.quad	.LBB0_184
	.quad	.LBB0_164
	.quad	.LBB0_184
	.quad	.LBB0_184
	.quad	.LBB0_184
	.quad	.LBB0_184
	.quad	.LBB0_206
	.quad	.LBB0_201
	.quad	.LBB0_116
	.quad	.LBB0_116
	.quad	.LBB0_115
	.quad	.LBB0_112
	.quad	.LBB0_111
	.quad	.LBB0_111
	.quad	.LBB0_111
	.quad	.LBB0_110
	.quad	.LBB0_110
	.quad	.LBB0_110
	.quad	.LBB0_109
	.quad	.LBB0_108
	.quad	.LBB0_109
	.quad	.LBB0_108
	.quad	.LBB0_107
	.quad	.LBB0_106
	.quad	.LBB0_106
	.quad	.LBB0_105
	.quad	.LBB0_105
	.quad	.LBB0_104
	.quad	.LBB0_100
	.quad	.LBB0_100
	.quad	.LBB0_201
	.quad	.LBB0_201
	.quad	.LBB0_201
	.quad	.LBB0_223
	.quad	.LBB0_223
	.quad	.LBB0_99
	.quad	.LBB0_99
	.quad	.LBB0_223
	.quad	.LBB0_98
	.quad	.LBB0_95
	.quad	.LBB0_93
	.quad	.LBB0_93
	.quad	.LBB0_223
	.quad	.LBB0_223
	.quad	.LBB0_91
	.quad	.LBB0_88
	.quad	.LBB0_88
	.quad	.LBB0_88
	.quad	.LBB0_88
	.quad	.LBB0_88
	.quad	.LBB0_88
	.quad	.LBB0_88
	.quad	.LBB0_88
	.quad	.LBB0_88
	.quad	.LBB0_88
	.quad	.LBB0_88
	.quad	.LBB0_87
	.quad	.LBB0_86
	.quad	.LBB0_88
	.quad	.LBB0_88
	.quad	.LBB0_88
	.quad	.LBB0_87
	.quad	.LBB0_86
	.quad	.LBB0_87
	.quad	.LBB0_86
	.quad	.LBB0_88
	.quad	.LBB0_88
	.quad	.LBB0_87
	.quad	.LBB0_87
	.quad	.LBB0_86
	.quad	.LBB0_86
	.quad	.LBB0_87
	.quad	.LBB0_87
	.quad	.LBB0_86
	.quad	.LBB0_86
	.quad	.LBB0_87
	.quad	.LBB0_87
	.quad	.LBB0_86
	.quad	.LBB0_86
	.quad	.LBB0_200
	.quad	.LBB0_200
	.quad	.LBB0_200
	.quad	.LBB0_200
	.quad	.LBB0_200
	.quad	.LBB0_200
	.quad	.LBB0_84
	.quad	.LBB0_200
	.quad	.LBB0_200
	.quad	.LBB0_84
	.quad	.LBB0_84
	.quad	.LBB0_84
	.quad	.LBB0_84
	.quad	.LBB0_84
	.quad	.LBB0_84
	.quad	.LBB0_13
	.quad	.LBB0_84
	.quad	.LBB0_84
	.quad	.LBB0_84
	.quad	.LBB0_84
	.quad	.LBB0_13
	.quad	.LBB0_84
	.quad	.LBB0_84
	.quad	.LBB0_223
	.quad	.LBB0_83
	.quad	.LBB0_83
	.quad	.LBB0_83
	.quad	.LBB0_83
	.quad	.LBB0_83
	.quad	.LBB0_83
	.quad	.LBB0_83
	.quad	.LBB0_82
	.quad	.LBB0_82
	.quad	.LBB0_82
	.quad	.LBB0_80
	.quad	.LBB0_80
	.quad	.LBB0_79
	.quad	.LBB0_79
	.quad	.LBB0_79
	.quad	.LBB0_79
	.quad	.LBB0_79
	.quad	.LBB0_79
	.quad	.LBB0_79
	.quad	.LBB0_78
	.quad	.LBB0_78
	.quad	.LBB0_78
	.quad	.LBB0_75
	.quad	.LBB0_75
	.quad	.LBB0_72
	.quad	.LBB0_72
	.quad	.LBB0_71
	.quad	.LBB0_71
	.quad	.LBB0_70
	.quad	.LBB0_70
	.quad	.LBB0_202
	.quad	.LBB0_67
	.quad	.LBB0_202
	.quad	.LBB0_67
	.quad	.LBB0_67
	.quad	.LBB0_67
	.quad	.LBB0_67
	.quad	.LBB0_67
	.quad	.LBB0_67
	.quad	.LBB0_67
	.quad	.LBB0_66
	.quad	.LBB0_66
	.quad	.LBB0_64
	.quad	.LBB0_66
	.quad	.LBB0_66
	.quad	.LBB0_64
	.quad	.LBB0_66
	.quad	.LBB0_66
	.quad	.LBB0_64
	.quad	.LBB0_66
	.quad	.LBB0_66
	.quad	.LBB0_64
	.quad	.LBB0_184
	.quad	.LBB0_184
	.quad	.LBB0_63
	.quad	.LBB0_62
	.quad	.LBB0_63
	.quad	.LBB0_62
	.quad	.LBB0_61
	.quad	.LBB0_61
	.quad	.LBB0_61
	.quad	.LBB0_61
	.quad	.LBB0_61
	.quad	.LBB0_61
	.quad	.LBB0_61
	.quad	.LBB0_61
	.quad	.LBB0_60
	.quad	.LBB0_60
	.quad	.LBB0_59
	.quad	.LBB0_59
	.quad	.LBB0_55
	.quad	.LBB0_55
	.quad	.LBB0_55
	.quad	.LBB0_55
	.quad	.LBB0_223
	.quad	.LBB0_206
	.quad	.LBB0_206
	.quad	.LBB0_206
	.quad	.LBB0_206
	.quad	.LBB0_206
	.quad	.LBB0_206
	.quad	.LBB0_206
	.quad	.LBB0_206
	.quad	.LBB0_206
	.quad	.LBB0_206
	.quad	.LBB0_206
	.quad	.LBB0_206
	.quad	.LBB0_206
	.quad	.LBB0_206
	.quad	.LBB0_206
	.quad	.LBB0_206
	.quad	.LBB0_84
	.quad	.LBB0_84
	.quad	.LBB0_84
	.quad	.LBB0_84
	.quad	.LBB0_54
	.quad	.LBB0_54
	.quad	.LBB0_84
	.quad	.LBB0_84
	.quad	.LBB0_53
	.quad	.LBB0_53
	.quad	.LBB0_52
	.quad	.LBB0_52
	.quad	.LBB0_200
	.quad	.LBB0_52
	.quad	.LBB0_200
	.quad	.LBB0_51
	.quad	.LBB0_52
	.quad	.LBB0_49
	.quad	.LBB0_52
	.quad	.LBB0_49
	.quad	.LBB0_52
	.quad	.LBB0_49
	.quad	.LBB0_52
	.quad	.LBB0_49
	.quad	.LBB0_84
	.quad	.LBB0_47
	.quad	.LBB0_84
	.quad	.LBB0_47
	.quad	.LBB0_200
	.quad	.LBB0_55
	.quad	.LBB0_46
	.quad	.LBB0_45
	.quad	.LBB0_46
	.quad	.LBB0_45
	.quad	.LBB0_52
	.quad	.LBB0_52
	.quad	.LBB0_44
	.quad	.LBB0_44
	.quad	.LBB0_55
	.quad	.LBB0_55
	.quad	.LBB0_46
	.quad	.LBB0_45
	.quad	.LBB0_46
	.quad	.LBB0_45
	.quad	.LBB0_52
	.quad	.LBB0_52
	.quad	.LBB0_46
	.quad	.LBB0_45
	.quad	.LBB0_46
	.quad	.LBB0_45
	.quad	.LBB0_52
	.quad	.LBB0_52
	.quad	.LBB0_209
	.quad	.LBB0_88
	.quad	.LBB0_43
	.quad	.LBB0_42
	.quad	.LBB0_41
	.quad	.LBB0_40
	.quad	.LBB0_40
	.quad	.LBB0_37
	.quad	.LBB0_37
	.quad	.LBB0_52
	.quad	.LBB0_49
	.quad	.LBB0_52
	.quad	.LBB0_49
	.quad	.LBB0_36
	.quad	.LBB0_84
	.quad	.LBB0_13
	.quad	.LBB0_36
	.quad	.LBB0_84
	.quad	.LBB0_13
	.quad	.LBB0_52
	.quad	.LBB0_52
	.quad	.LBB0_52
	.quad	.LBB0_52
	.quad	.LBB0_52
	.quad	.LBB0_52
	.quad	.LBB0_52
	.quad	.LBB0_52
	.quad	.LBB0_52
	.quad	.LBB0_52
	.quad	.LBB0_52
	.quad	.LBB0_52
	.quad	.LBB0_52
	.quad	.LBB0_52
	.quad	.LBB0_52
	.quad	.LBB0_32
	.quad	.LBB0_32
	.quad	.LBB0_31
	.quad	.LBB0_28
	.quad	.LBB0_28
	.quad	.LBB0_209
	.quad	.LBB0_28
	.quad	.LBB0_30
	.quad	.LBB0_28
	.quad	.LBB0_28
	.quad	.LBB0_22
	.quad	.LBB0_27
	.quad	.LBB0_24
	.quad	.LBB0_22
	.quad	.LBB0_52
	.quad	.LBB0_52
	.quad	.LBB0_52
	.quad	.LBB0_52
	.quad	.LBB0_52
	.quad	.LBB0_52
	.quad	.LBB0_52
	.quad	.LBB0_52
	.quad	.LBB0_52
	.quad	.LBB0_52
	.quad	.LBB0_52
	.quad	.LBB0_52
	.quad	.LBB0_52
	.quad	.LBB0_52
	.quad	.LBB0_28
	.quad	.LBB0_52
	.quad	.LBB0_52
	.quad	.LBB0_28
	.quad	.LBB0_37
	.quad	.LBB0_37
	.quad	.LBB0_37
	.quad	.LBB0_37
	.quad	.LBB0_37
	.quad	.LBB0_37
	.quad	.LBB0_37
	.quad	.LBB0_37
	.quad	.LBB0_37
	.quad	.LBB0_223
	.quad	.LBB0_93
	.quad	.LBB0_209
	.quad	.LBB0_19
	.quad	.LBB0_18
	.quad	.LBB0_52
	.quad	.LBB0_52
	.quad	.LBB0_17
	.quad	.LBB0_52
	.quad	.LBB0_52
	.quad	.LBB0_52
	.quad	.LBB0_52
	.quad	.LBB0_52
	.quad	.LBB0_52
	.quad	.LBB0_223
	.quad	.LBB0_200
	.quad	.LBB0_13
	.quad	.LBB0_9
	.quad	.LBB0_9
	.quad	.LBB0_9
	.quad	.LBB0_4
	.quad	.LBB0_200
	.quad	.LBB0_22
	.quad	.LBB0_84
	.quad	.LBB0_22
	.quad	.LBB0_22
	.quad	.LBB0_84
	.quad	.LBB0_22
	.quad	.LBB0_3
	.quad	.LBB0_200
	.quad	.LBB0_200
	.quad	.LBB0_221
	.quad	.LBB0_206

	.type	.L.str,@object          # @.str
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.asciz	"insn-extract.c"
	.size	.L.str, 15

	.type	.L__FUNCTION__.insn_extract,@object # @__FUNCTION__.insn_extract
.L__FUNCTION__.insn_extract:
	.asciz	"insn_extract"
	.size	.L__FUNCTION__.insn_extract, 13


	.ident	"clang version 3.5 (trunk)"
	.section	".note.GNU-stack","",@progbits
