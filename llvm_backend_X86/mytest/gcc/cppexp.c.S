	.file	"cppexp.c.bc"
	.text
	.globl	_cpp_parse_expr
	.align	16, 0x90
	.type	_cpp_parse_expr,@function
_cpp_parse_expr:                        # @_cpp_parse_expr
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
	subq	$464, %rsp              # imm = 0x1D0
.Ltmp11:
	.cfi_def_cfa_offset 512
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
	movq	%rdi, %r15
	movq	%r15, 96(%rsp)          # 8-byte Spill
	leaq	112(%rsp), %rcx
	leaq	432(%rsp), %rax
	movq	%rax, 48(%rsp)          # 8-byte Spill
	leaq	128(%rsp), %r14
	movq	$0, 176(%r15)
	movq	$67, %rax
	movl	%eax, 128(%rsp)
	movq	$2, %rax
	movb	%al, 132(%rsp)
	movq	$4, %rax
	movb	%al, 133(%rsp)
	movabsq	$4294967295, %r8        # imm = 0xFFFFFFFF
	movabsq	$0, %rax
	movq	%rax, 40(%rsp)          # 8-byte Spill
	xorq	%r11, %r11
	xorq	%r9, %r9
	movq	%rcx, 80(%rsp)          # 8-byte Spill
	jmp	.LBB0_1
	.align	16, 0x90
.LBB0_310:                              # %if.end533
                                        #   in Loop: Header=BB0_1 Depth=1
	movq	56(%rsp), %rax          # 8-byte Reload
	movb	%bl, 5(%r14)
	andq	$254, %r12
	movb	%r12b, 4(%r14)
	movl	%eax, (%r14)
	movq	96(%rsp), %r15          # 8-byte Reload
.LBB0_1:                                # %for.cond
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB0_15 Depth 2
                                        #     Child Loop BB0_34 Depth 2
                                        #     Child Loop BB0_46 Depth 2
                                        #     Child Loop BB0_135 Depth 2
	movq	%r11, 88(%rsp)          # 8-byte Spill
	movq	%r9, 104(%rsp)          # 8-byte Spill
	movq	%r8, %rbx
	movq	%r15, %rdi
	callq	cpp_get_token
	movq	%rax, %r12
	movzbl	6(%r12), %r10d
	cmpq	$55, %r10
	jg	.LBB0_6
# BB#2:                                 # %for.cond
                                        #   in Loop: Header=BB0_1 Depth=1
	cmpq	$40, %r10
	movq	%rbx, %r8
	jne	.LBB0_108
# BB#3:                                 # %sw.bb81.i
                                        #   in Loop: Header=BB0_1 Depth=1
	movq	%r15, %rdi
	leaq	432(%rsp), %rsi
	movq	%r8, %rbx
	callq	_cpp_test_assertion
	movq	%rbx, %r8
	testq	%r8, %rax
	movq	$57, %r10
	je	.LBB0_5
# BB#4:                                 # %sw.bb81.i
                                        #   in Loop: Header=BB0_1 Depth=1
	movq	$64, %r10
.LBB0_5:                                # %sw.bb81.i
                                        #   in Loop: Header=BB0_1 Depth=1
	movslq	432(%rsp), %r13
	jmp	.LBB0_115
	.align	16, 0x90
.LBB0_6:                                # %for.cond
                                        #   in Loop: Header=BB0_1 Depth=1
	movq	%r10, %rax
	addq	$-56, %rax
	movq	%rbx, %r8
	andq	%r8, %rax
	cmpq	$6, %rax
	jbe	.LBB0_7
.LBB0_108:                              # %sw.default.i
                                        #   in Loop: Header=BB0_1 Depth=1
	testq	%r10, %r10
	movabsq	$0, %rax
	je	.LBB0_110
# BB#109:                               # %sw.default.i
                                        #   in Loop: Header=BB0_1 Depth=1
	movabsq	$1, %rax
.LBB0_110:                              # %sw.default.i
                                        #   in Loop: Header=BB0_1 Depth=1
	cmpq	$28, %r10
	sbbq	%rcx, %rcx
	testq	%rax, %rcx
	jne	.LBB0_114
# BB#111:                               # %sw.default.i
                                        #   in Loop: Header=BB0_1 Depth=1
	cmpq	$67, %r10
	je	.LBB0_114
# BB#112:                               # %do.body114.i
                                        #   in Loop: Header=BB0_1 Depth=1
	movq	%r15, %rdi
	movq	%r12, %rsi
	movq	%r8, %rbx
	callq	cpp_token_as_text
	movq	%rax, %rcx
	xorq	%rax, %rax
	movq	%r15, %rdi
	movabsq	$.L.str22, %rsi
	movq	%rcx, %rdx
	callq	cpp_error
	jmp	.LBB0_113
	.align	16, 0x90
.LBB0_7:                                # %for.cond
                                        #   in Loop: Header=BB0_1 Depth=1
	jmpq	*.LJTI0_0(,%rax,8)
.LBB0_69:                               # %sw.bb2.i
                                        #   in Loop: Header=BB0_1 Depth=1
	movq	$1, %rdx
	xorq	%rcx, %rcx
	movq	%r15, %rdi
	movq	%r12, %rsi
	movq	%r8, %rbx
	leaq	436(%rsp), %r8
	callq	cpp_interpret_charconst
	movq	%rbx, %r8
	movq	%rax, %r13
	movq	$57, %r10
	jmp	.LBB0_115
.LBB0_70:                               # %do.body.i
                                        #   in Loop: Header=BB0_1 Depth=1
	xorq	%rax, %rax
	movq	%r15, %rdi
	movabsq	$.L.str16, %rsi
	movq	%r8, %rbx
	callq	cpp_error
.LBB0_113:                              # %lex.exit
                                        #   in Loop: Header=BB0_1 Depth=1
	movq	%rbx, %r8
	movq	$64, %r10
.LBB0_114:                              # %lex.exit
                                        #   in Loop: Header=BB0_1 Depth=1
                                        # implicit-def: R13
.LBB0_115:                              # %lex.exit
                                        #   in Loop: Header=BB0_1 Depth=1
	movq	88(%rsp), %rbx          # 8-byte Reload
.LBB0_116:                              # %lex.exit
                                        #   in Loop: Header=BB0_1 Depth=1
	movq	%r10, 64(%rsp)          # 8-byte Spill
	leaq	1(%rbx), %r11
	movq	%r11, 72(%rsp)          # 8-byte Spill
	movq	$32, %rcx
	movq	%r10, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$56, %rax
	jg	.LBB0_124
# BB#117:                               # %lex.exit
                                        #   in Loop: Header=BB0_1 Depth=1
	leaq	-4(%r10), %rax
	andq	%r8, %rax
	cmpq	$2, %rax
	jae	.LBB0_118
# BB#132:                               # %sw.bb18
                                        #   in Loop: Header=BB0_1 Depth=1
	movzbl	5(%r14), %eax
	testq	$1, %rax
	movq	$7680, %r12             # imm = 0x1E00
	movq	104(%rsp), %r9          # 8-byte Reload
	je	.LBB0_122
	jmp	.LBB0_133
	.align	16, 0x90
.LBB0_124:                              # %lex.exit
                                        #   in Loop: Header=BB0_1 Depth=1
	xorq	%r12, %r12
	movq	%r10, %rax
	andq	%r8, %rax
	cmpq	$64, %rax
	je	.LBB0_125
# BB#126:                               # %lex.exit
                                        #   in Loop: Header=BB0_1 Depth=1
	cmpq	$67, %rax
	movq	104(%rsp), %r9          # 8-byte Reload
	jne	.LBB0_127
.LBB0_133:                              # %sw.epilog.thread
                                        #   in Loop: Header=BB0_1 Depth=1
	movq	%r12, 88(%rsp)          # 8-byte Spill
	movq	%r12, %rdx
	movabsq	$4294967040, %rax       # imm = 0xFFFFFF00
	andq	%rax, %rdx
	movq	$8, %rcx
	shrq	%cl, %rdx
	jmp	.LBB0_134
	.align	16, 0x90
.LBB0_127:                              # %lex.exit
                                        #   in Loop: Header=BB0_1 Depth=1
	cmpq	$57, %rax
	jne	.LBB0_122
	jmp	.LBB0_128
.LBB0_118:                              # %lex.exit
                                        #   in Loop: Header=BB0_1 Depth=1
	movq	%r10, %rax
	andq	%r8, %rax
	cmpq	$1, %rax
	movq	104(%rsp), %r9          # 8-byte Reload
	jne	.LBB0_122
# BB#119:                               # %sw.bb14
                                        #   in Loop: Header=BB0_1 Depth=1
	testq	%r8, %rbx
	movabsq	$0, %rax
	jne	.LBB0_121
# BB#120:                               # %sw.bb14
                                        #   in Loop: Header=BB0_1 Depth=1
	movabsq	$1, %rax
.LBB0_121:                              # %sw.bb14
                                        #   in Loop: Header=BB0_1 Depth=1
	movq	%rax, 40(%rsp)          # 8-byte Spill
	.align	16, 0x90
.LBB0_122:                              # %sw.epilog
                                        #   in Loop: Header=BB0_1 Depth=1
	movq	%r10, %rax
	andq	%r8, %rax
	movswq	op_to_prio(%rax,%rax), %rbx
	movq	%rbx, %rdx
	movabsq	$4294967040, %rax       # imm = 0xFFFFFF00
	andq	%rax, %rdx
	movq	$8, %rcx
	shrq	%cl, %rdx
	cmpq	$4, %rdx
	je	.LBB0_325
# BB#123:                               #   in Loop: Header=BB0_1 Depth=1
	movq	%rbx, 88(%rsp)          # 8-byte Spill
.LBB0_134:                              # %while.cond.preheader
                                        #   in Loop: Header=BB0_1 Depth=1
	movq	%rdx, 56(%rsp)          # 8-byte Spill
	movzbl	4(%r14), %eax
	movq	%rdx, %r12
	andq	%r8, %r12
	cmpq	%rax, %r12
	ja	.LBB0_252
	.align	16, 0x90
.LBB0_135:                              # %while.body
                                        #   Parent Loop BB0_1 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movzbl	5(%r14), %ecx
	testq	$5, %rcx
	je	.LBB0_136
# BB#141:                               # %if.end57
                                        #   in Loop: Header=BB0_135 Depth=2
	movl	(%r14), %edx
	leaq	-1(%rdx), %rdi
	andq	%r8, %rdi
	cmpq	$66, %rdi
	ja	.LBB0_143
# BB#142:                               # %if.end57
                                        #   in Loop: Header=BB0_135 Depth=2
	movzbl	6(%r14), %r15d
	movq	-8(%r14), %rsi
	movq	8(%r14), %r13
	movzbl	-10(%r14), %eax
	leaq	-16(%r14), %rbx
	jmpq	*.LJTI0_1(,%rdi,8)
.LBB0_245:                              # %sw.bb334
                                        #   in Loop: Header=BB0_135 Depth=2
	testq	%r8, %r9
	jne	.LBB0_251
# BB#246:                               # %if.end337
                                        #   in Loop: Header=BB0_135 Depth=2
	testq	%r13, %r13
	je	.LBB0_247
# BB#248:                               # %if.end344
                                        #   in Loop: Header=BB0_135 Depth=2
	orq	%r15, %rax
	movb	%al, -10(%r14)
	andq	%r8, %rdx
	cmpq	$7, %rdx
	jne	.LBB0_254
# BB#249:                               # %if.then352
                                        #   in Loop: Header=BB0_135 Depth=2
	testq	$255, %rax
	je	.LBB0_326
# BB#250:                               # %if.then355
                                        #   in Loop: Header=BB0_135 Depth=2
	xorq	%r9, %r9
	xorq	%rdx, %rdx
	movq	%rsi, %rax
	divq	%r13
	movq	%rax, -8(%r14)
	jmp	.LBB0_251
.LBB0_144:                              # %sw.bb70
                                        #   in Loop: Header=BB0_135 Depth=2
	testq	%r13, %r13
	jne	.LBB0_145
# BB#146:                               # %sw.bb70
                                        #   in Loop: Header=BB0_135 Depth=2
	movabsq	$1, %rax
	jmp	.LBB0_147
.LBB0_156:                              # %sw.bb98
                                        #   in Loop: Header=BB0_135 Depth=2
	xorq	%rcx, %rcx
	movb	%cl, -10(%r14)
	orq	%r15, %rax
	cmpq	%r13, %rsi
	movabsq	$0, %rcx
	movabsq	$1, %rsi
	movq	%rcx, %rdx
	jbe	.LBB0_158
# BB#157:                               # %sw.bb98
                                        #   in Loop: Header=BB0_135 Depth=2
	movq	%rsi, %rdx
.LBB0_158:                              # %sw.bb98
                                        #   in Loop: Header=BB0_135 Depth=2
	jle	.LBB0_160
# BB#159:                               # %sw.bb98
                                        #   in Loop: Header=BB0_135 Depth=2
	movq	%rsi, %rcx
.LBB0_160:                              # %sw.bb98
                                        #   in Loop: Header=BB0_135 Depth=2
	testq	$255, %rax
	jne	.LBB0_162
# BB#161:                               # %sw.bb98
                                        #   in Loop: Header=BB0_135 Depth=2
	movq	%rcx, %rdx
.LBB0_162:                              # %sw.bb98
                                        #   in Loop: Header=BB0_135 Depth=2
	movq	%rdx, -8(%r14)
	jmp	.LBB0_251
.LBB0_150:                              # %sw.bb88
                                        #   in Loop: Header=BB0_135 Depth=2
	cmpq	%r13, %rsi
	sbbq	%rcx, %rcx
	cmpq	%r13, %rsi
	jge	.LBB0_151
# BB#152:                               # %sw.bb88
                                        #   in Loop: Header=BB0_135 Depth=2
	movabsq	$1, %rdx
	jmp	.LBB0_153
.LBB0_223:                              # %sw.bb225
                                        #   in Loop: Header=BB0_135 Depth=2
	movzbl	-11(%r14), %ecx
	testq	$1, %rcx
	jne	.LBB0_226
# BB#224:                               # %if.then230
                                        #   in Loop: Header=BB0_135 Depth=2
	movq	%r13, -8(%r14)
	movb	%r15b, -10(%r14)
	orq	$1, %rcx
	movb	%cl, -11(%r14)
	movq	96(%rsp), %rdi          # 8-byte Reload
	movzbl	761(%rdi), %eax
	testq	%rax, %rax
	je	.LBB0_251
# BB#225:                               # %if.then239
                                        #   in Loop: Header=BB0_135 Depth=2
	movabsq	$.L.str4, %rsi
	xorq	%rax, %rax
	movq	%r8, %r14
	movq	%r9, %r15
	movq	%r10, %r13
	callq	cpp_warning
	movq	72(%rsp), %r11          # 8-byte Reload
	movq	%r13, %r10
	movq	%r15, %r9
	movq	%r14, %r8
	jmp	.LBB0_251
.LBB0_232:                              # %sw.bb261
                                        #   in Loop: Header=BB0_135 Depth=2
	movzbl	-11(%r14), %ecx
	testq	$1, %rcx
	jne	.LBB0_236
# BB#233:                               # %if.then266
                                        #   in Loop: Header=BB0_135 Depth=2
	movq	%r13, %rax
	negq	%rax
	movq	%rax, -8(%r14)
	movb	%r15b, -10(%r14)
	orq	$1, %rcx
	movb	%cl, -11(%r14)
	testq	%r8, %r9
	jne	.LBB0_251
# BB#234:                               # %land.lhs.true276
                                        #   in Loop: Header=BB0_135 Depth=2
	xorq	%r9, %r9
	testq	%rax, %r13
	jns	.LBB0_251
# BB#235:                               # %land.lhs.true276
                                        #   in Loop: Header=BB0_135 Depth=2
	testq	%r15, %r15
	je	.LBB0_230
	jmp	.LBB0_251
.LBB0_239:                              # %sw.bb307
                                        #   in Loop: Header=BB0_135 Depth=2
	orq	%r15, %rax
	testq	$255, %rax
	movb	%al, -10(%r14)
	jne	.LBB0_240
# BB#241:                               # %if.else315
                                        #   in Loop: Header=BB0_135 Depth=2
	testq	%r8, %r9
	jne	.LBB0_251
# BB#242:                               # %if.then317
                                        #   in Loop: Header=BB0_135 Depth=2
	movq	%rsi, %rax
	imulq	%r13, %rax
	xorq	%r9, %r9
	testq	%rsi, %rsi
	movq	%rax, -8(%r14)
	je	.LBB0_251
# BB#243:                               # %lor.lhs.false
                                        #   in Loop: Header=BB0_135 Depth=2
	andq	%r13, %rsi
	jmp	.LBB0_244
.LBB0_183:                              # %sw.bb152
                                        #   in Loop: Header=BB0_135 Depth=2
	andq	%r13, %rsi
	jmp	.LBB0_221
.LBB0_185:                              # %sw.bb163
                                        #   in Loop: Header=BB0_135 Depth=2
	orq	%r13, %rsi
	jmp	.LBB0_221
.LBB0_184:                              # %sw.bb158
                                        #   in Loop: Header=BB0_135 Depth=2
	xorq	%r13, %rsi
	jmp	.LBB0_221
.LBB0_210:                              # %sw.bb185
                                        #   in Loop: Header=BB0_135 Depth=2
	testq	%r8, %r9
	jne	.LBB0_251
# BB#211:                               # %if.end188
                                        #   in Loop: Header=BB0_135 Depth=2
	movb	%al, -10(%r14)
	testq	%r13, %r13
	jns	.LBB0_216
# BB#212:                               # %if.end188
                                        #   in Loop: Header=BB0_135 Depth=2
	testq	%r15, %r15
	jne	.LBB0_216
# BB#213:                               # %if.then195
                                        #   in Loop: Header=BB0_135 Depth=2
	negq	%r13
	cmpq	$64, %r13
	jb	.LBB0_204
# BB#214:                               # %if.then.i918
                                        #   in Loop: Header=BB0_135 Depth=2
	testq	$255, %rax
	je	.LBB0_199
# BB#215:                               #   in Loop: Header=BB0_135 Depth=2
	movabsq	$0, %rax
	jmp	.LBB0_200
.LBB0_186:                              # %sw.bb169
                                        #   in Loop: Header=BB0_135 Depth=2
	testq	%r8, %r9
	jne	.LBB0_251
# BB#187:                               # %if.end172
                                        #   in Loop: Header=BB0_135 Depth=2
	movb	%al, -10(%r14)
	testq	%r13, %r13
	jns	.LBB0_196
# BB#188:                               # %if.end172
                                        #   in Loop: Header=BB0_135 Depth=2
	testq	%r15, %r15
	jne	.LBB0_196
# BB#189:                               # %if.then178
                                        #   in Loop: Header=BB0_135 Depth=2
	negq	%r13
	cmpq	$64, %r13
	jae	.LBB0_190
# BB#191:                               # %if.else.i
                                        #   in Loop: Header=BB0_135 Depth=2
	movq	%r13, %rcx
	testq	$255, %rax
	je	.LBB0_193
# BB#192:                               # %if.then2.i
                                        #   in Loop: Header=BB0_135 Depth=2
	shrq	%cl, %rsi
	jmp	.LBB0_194
.LBB0_219:                              # %sw.bb203
                                        #   in Loop: Header=BB0_135 Depth=2
	cmpq	%r13, %rsi
	jle	.LBB0_221
	jmp	.LBB0_220
.LBB0_222:                              # %sw.bb214
                                        #   in Loop: Header=BB0_135 Depth=2
	cmpq	%r13, %rsi
	jge	.LBB0_221
.LBB0_220:                              # %sw.bb203
                                        #   in Loop: Header=BB0_135 Depth=2
	movq	%r13, %rsi
.LBB0_221:                              # %sw.bb203
                                        #   in Loop: Header=BB0_135 Depth=2
	movq	%rsi, -8(%r14)
	orq	%r15, %rax
	movb	%al, -10(%r14)
	jmp	.LBB0_251
.LBB0_149:                              # %sw.bb80
                                        #   in Loop: Header=BB0_135 Depth=2
	notq	%r13
	movq	%r13, -8(%r14)
	jmp	.LBB0_148
.LBB0_263:                              # %sw.bb388
                                        #   in Loop: Header=BB0_135 Depth=2
	testq	%r13, %r13
	movabsq	$0, %rax
	movabsq	$1, %rdx
	movq	%rax, %rcx
	je	.LBB0_265
# BB#264:                               # %sw.bb388
                                        #   in Loop: Header=BB0_135 Depth=2
	movq	%rdx, %rcx
.LBB0_265:                              # %sw.bb388
                                        #   in Loop: Header=BB0_135 Depth=2
	testq	%rsi, %rsi
	movq	%rax, %rsi
	je	.LBB0_267
# BB#266:                               # %sw.bb388
                                        #   in Loop: Header=BB0_135 Depth=2
	movq	%rdx, %rsi
.LBB0_267:                              # %sw.bb388
                                        #   in Loop: Header=BB0_135 Depth=2
	jne	.LBB0_269
# BB#268:                               # %sw.bb388
                                        #   in Loop: Header=BB0_135 Depth=2
	movq	%rdx, %rax
.LBB0_269:                              # %sw.bb388
                                        #   in Loop: Header=BB0_135 Depth=2
	andq	%rcx, %rsi
	movq	%rsi, -8(%r14)
	jmp	.LBB0_262
.LBB0_257:                              # %sw.bb379
                                        #   in Loop: Header=BB0_135 Depth=2
	testq	%rsi, %rsi
	movabsq	$0, %rcx
	movabsq	$1, %rdx
	movq	%rcx, %rax
	je	.LBB0_259
# BB#258:                               # %sw.bb379
                                        #   in Loop: Header=BB0_135 Depth=2
	movq	%rdx, %rax
.LBB0_259:                              # %sw.bb379
                                        #   in Loop: Header=BB0_135 Depth=2
	orq	%r13, %rsi
	je	.LBB0_261
# BB#260:                               # %sw.bb379
                                        #   in Loop: Header=BB0_135 Depth=2
	movq	%rdx, %rcx
.LBB0_261:                              # %sw.bb379
                                        #   in Loop: Header=BB0_135 Depth=2
	movq	%rcx, -8(%r14)
.LBB0_262:                              # %while.cond.backedge
                                        #   in Loop: Header=BB0_135 Depth=2
	xorq	%rcx, %rcx
	movb	%cl, -10(%r14)
	subq	%rax, %r9
	jmp	.LBB0_251
.LBB0_274:                              # %sw.bb410
                                        #   in Loop: Header=BB0_135 Depth=2
	movl	(%rbx), %ecx
	cmpq	$19, %rcx
	jne	.LBB0_275
# BB#276:                               # %if.end419
                                        #   in Loop: Header=BB0_135 Depth=2
	cmpq	$0, -24(%r14)
	leaq	-32(%r14), %rbx
	je	.LBB0_277
# BB#278:                               # %if.end419
                                        #   in Loop: Header=BB0_135 Depth=2
	movabsq	$1, %rcx
	jmp	.LBB0_279
.LBB0_270:                              # %sw.bb398
                                        #   in Loop: Header=BB0_135 Depth=2
	movq	96(%rsp), %rax          # 8-byte Reload
	movzbl	770(%rax), %eax
	testq	%rax, %rax
	je	.LBB0_272
# BB#271:                               # %if.then401
                                        #   in Loop: Header=BB0_135 Depth=2
	movabsq	$.L.str6, %rsi
	xorq	%rax, %rax
	movq	96(%rsp), %rdi          # 8-byte Reload
	movq	%r9, 104(%rsp)          # 8-byte Spill
	callq	cpp_pedwarn
	movq	72(%rsp), %r11          # 8-byte Reload
	movq	64(%rsp), %r10          # 8-byte Reload
	movq	104(%rsp), %r9          # 8-byte Reload
	movabsq	$4294967295, %r8        # imm = 0xFFFFFFFF
.LBB0_272:                              # %if.end402
                                        #   in Loop: Header=BB0_135 Depth=2
	movq	%r13, -8(%r14)
	movb	%r15b, -10(%r14)
	jmp	.LBB0_251
.LBB0_177:                              # %sw.bb140
                                        #   in Loop: Header=BB0_135 Depth=2
	cmpq	%r13, %rsi
	je	.LBB0_179
# BB#178:                               #   in Loop: Header=BB0_135 Depth=2
	movabsq	$0, %rax
	jmp	.LBB0_180
.LBB0_181:                              # %sw.bb146
                                        #   in Loop: Header=BB0_135 Depth=2
	cmpq	%r13, %rsi
	je	.LBB0_182
.LBB0_179:                              # %sw.bb140
                                        #   in Loop: Header=BB0_135 Depth=2
	movabsq	$1, %rax
	jmp	.LBB0_180
.LBB0_170:                              # %sw.bb126
                                        #   in Loop: Header=BB0_135 Depth=2
	xorq	%rcx, %rcx
	movb	%cl, -10(%r14)
	orq	%r15, %rax
	cmpq	%r13, %rsi
	movabsq	$0, %rcx
	movabsq	$1, %rsi
	movq	%rcx, %rdx
	jb	.LBB0_172
# BB#171:                               # %sw.bb126
                                        #   in Loop: Header=BB0_135 Depth=2
	movq	%rsi, %rdx
.LBB0_172:                              # %sw.bb126
                                        #   in Loop: Header=BB0_135 Depth=2
	jl	.LBB0_174
# BB#173:                               # %sw.bb126
                                        #   in Loop: Header=BB0_135 Depth=2
	movq	%rsi, %rcx
.LBB0_174:                              # %sw.bb126
                                        #   in Loop: Header=BB0_135 Depth=2
	testq	$255, %rax
	jne	.LBB0_176
# BB#175:                               # %sw.bb126
                                        #   in Loop: Header=BB0_135 Depth=2
	movq	%rcx, %rdx
.LBB0_176:                              # %sw.bb126
                                        #   in Loop: Header=BB0_135 Depth=2
	movq	%rdx, -8(%r14)
	jmp	.LBB0_251
.LBB0_163:                              # %sw.bb112
                                        #   in Loop: Header=BB0_135 Depth=2
	xorq	%rcx, %rcx
	movb	%cl, -10(%r14)
	orq	%r15, %rax
	cmpq	%r13, %rsi
	movabsq	$0, %rcx
	movabsq	$1, %rsi
	movq	%rcx, %rdx
	ja	.LBB0_165
# BB#164:                               # %sw.bb112
                                        #   in Loop: Header=BB0_135 Depth=2
	movq	%rsi, %rdx
.LBB0_165:                              # %sw.bb112
                                        #   in Loop: Header=BB0_135 Depth=2
	jg	.LBB0_167
# BB#166:                               # %sw.bb112
                                        #   in Loop: Header=BB0_135 Depth=2
	movq	%rsi, %rcx
.LBB0_167:                              # %sw.bb112
                                        #   in Loop: Header=BB0_135 Depth=2
	testq	$255, %rax
	jne	.LBB0_169
# BB#168:                               # %sw.bb112
                                        #   in Loop: Header=BB0_135 Depth=2
	movq	%rcx, %rdx
.LBB0_169:                              # %sw.bb112
                                        #   in Loop: Header=BB0_135 Depth=2
	movq	%rdx, -8(%r14)
	jmp	.LBB0_251
.LBB0_145:                              #   in Loop: Header=BB0_135 Depth=2
	movabsq	$0, %rax
.LBB0_147:                              # %sw.bb70
                                        #   in Loop: Header=BB0_135 Depth=2
	movq	%rax, -8(%r14)
.LBB0_148:                              # %while.cond.backedge
                                        #   in Loop: Header=BB0_135 Depth=2
	movb	%r15b, -10(%r14)
	movzbl	-11(%r14), %eax
	orq	$1, %rax
	movb	%al, -11(%r14)
	jmp	.LBB0_251
.LBB0_151:                              #   in Loop: Header=BB0_135 Depth=2
	movabsq	$0, %rdx
.LBB0_153:                              # %sw.bb88
                                        #   in Loop: Header=BB0_135 Depth=2
	xorq	%rsi, %rsi
	movb	%sil, -10(%r14)
	orq	%r15, %rax
	testq	$255, %rax
	je	.LBB0_155
# BB#154:                               #   in Loop: Header=BB0_135 Depth=2
	andq	$1, %rcx
	movq	%rcx, %rdx
.LBB0_155:                              # %sw.bb88
                                        #   in Loop: Header=BB0_135 Depth=2
	movq	%rdx, -8(%r14)
	jmp	.LBB0_251
.LBB0_226:                              # %if.else241
                                        #   in Loop: Header=BB0_135 Depth=2
	leaq	(%rsi,%r13), %rcx
	movq	%rcx, -8(%r14)
	orq	%r15, %rax
	testq	$255, %rax
	movb	%al, -10(%r14)
	jne	.LBB0_251
# BB#227:                               # %if.else241
                                        #   in Loop: Header=BB0_135 Depth=2
	testq	%r8, %r9
	jne	.LBB0_251
# BB#228:                               # %land.lhs.true250
                                        #   in Loop: Header=BB0_135 Depth=2
	xorq	%rsi, %r13
	jmp	.LBB0_229
.LBB0_236:                              # %if.else285
                                        #   in Loop: Header=BB0_135 Depth=2
	movq	%rsi, %rcx
	subq	%r13, %rcx
	movq	%rcx, -8(%r14)
	orq	%r15, %rax
	testq	$255, %rax
	movb	%al, -10(%r14)
	jne	.LBB0_251
# BB#237:                               # %if.else285
                                        #   in Loop: Header=BB0_135 Depth=2
	testq	%r8, %r9
	jne	.LBB0_251
# BB#238:                               # %land.lhs.true295
                                        #   in Loop: Header=BB0_135 Depth=2
	xorq	%rcx, %r13
.LBB0_229:                              # %land.lhs.true250
                                        #   in Loop: Header=BB0_135 Depth=2
	movabsq	$-9223372036854775808, %rax # imm = 0x8000000000000000
	xorq	%rax, %rsi
	xorq	%rcx, %rsi
	xorq	%r9, %r9
	orq	%r13, %rsi
	js	.LBB0_251
	jmp	.LBB0_230
.LBB0_240:                              # %if.then313
                                        #   in Loop: Header=BB0_135 Depth=2
	imulq	%r13, %rsi
	movq	%rsi, -8(%r14)
	jmp	.LBB0_251
.LBB0_182:                              #   in Loop: Header=BB0_135 Depth=2
	movabsq	$0, %rax
.LBB0_180:                              # %sw.bb140
                                        #   in Loop: Header=BB0_135 Depth=2
	movq	%rax, -8(%r14)
	xorq	%rax, %rax
	movb	%al, -10(%r14)
	jmp	.LBB0_251
.LBB0_254:                              # %if.else369
                                        #   in Loop: Header=BB0_135 Depth=2
	testq	$255, %rax
	je	.LBB0_256
# BB#255:                               # %if.then372
                                        #   in Loop: Header=BB0_135 Depth=2
	xorq	%r9, %r9
	xorq	%rdx, %rdx
	movq	%rsi, %rax
	divq	%r13
	movq	%rdx, -8(%r14)
	jmp	.LBB0_251
.LBB0_277:                              #   in Loop: Header=BB0_135 Depth=2
	movabsq	$0, %rcx
.LBB0_279:                              # %if.end419
                                        #   in Loop: Header=BB0_135 Depth=2
	jne	.LBB0_281
# BB#280:                               # %if.end419
                                        #   in Loop: Header=BB0_135 Depth=2
	movq	%r13, %rsi
.LBB0_281:                              # %if.end419
                                        #   in Loop: Header=BB0_135 Depth=2
	subq	%rcx, %r9
	movq	%rsi, -24(%r14)
	orq	%r15, %rax
	movb	%al, -26(%r14)
	jmp	.LBB0_251
.LBB0_216:                              # %if.else199
                                        #   in Loop: Header=BB0_135 Depth=2
	cmpq	$64, %r13
	jb	.LBB0_217
.LBB0_190:                              # %if.then.i
                                        #   in Loop: Header=BB0_135 Depth=2
	movq	$63, %rcx
	sarq	%cl, %rsi
	xorq	%rcx, %rcx
	testq	$255, %rax
	je	.LBB0_194
	jmp	.LBB0_195
.LBB0_196:                              # %if.else181
                                        #   in Loop: Header=BB0_135 Depth=2
	cmpq	$64, %r13
	jb	.LBB0_204
# BB#197:                               # %if.then.i905
                                        #   in Loop: Header=BB0_135 Depth=2
	testq	$255, %rax
	jne	.LBB0_198
.LBB0_199:                              # %if.then.i905
                                        #   in Loop: Header=BB0_135 Depth=2
	movabsq	$1, %rax
	jmp	.LBB0_200
.LBB0_204:                              # %if.else.i911
                                        #   in Loop: Header=BB0_135 Depth=2
	movq	%rsi, %r15
	movq	%r13, %rcx
	shlq	%cl, %r15
	movq	%r15, %rdx
	movq	%r13, %rcx
	sarq	%cl, %rdx
	testq	$255, %rax
	jne	.LBB0_209
# BB#205:                               # %if.else.i911
                                        #   in Loop: Header=BB0_135 Depth=2
	cmpq	%rsi, %rdx
	je	.LBB0_209
# BB#206:                               # %if.then8.i
                                        #   in Loop: Header=BB0_135 Depth=2
	movq	96(%rsp), %rdi          # 8-byte Reload
	movzbl	770(%rdi), %eax
	testq	%rax, %rax
	je	.LBB0_209
# BB#207:                               # %if.then.i19.i
                                        #   in Loop: Header=BB0_135 Depth=2
	movabsq	$.L.str15, %rsi
	xorq	%rax, %rax
	jmp	.LBB0_208
.LBB0_326:                              # %if.else358
                                        #   in Loop: Header=BB0_135 Depth=2
	movq	%rsi, %rax
	cqto
	idivq	%r13
	movq	%rax, -8(%r14)
	andq	%r13, %rsi
	xorq	%r9, %r9
.LBB0_244:                              # %lor.lhs.false
                                        #   in Loop: Header=BB0_135 Depth=2
	testq	%rax, %rsi
	jns	.LBB0_251
.LBB0_230:                              # %if.then258
                                        #   in Loop: Header=BB0_135 Depth=2
	movq	96(%rsp), %rdi          # 8-byte Reload
	movzbl	770(%rdi), %eax
	testq	%rax, %rax
	je	.LBB0_251
# BB#231:                               # %if.then.i948
                                        #   in Loop: Header=BB0_135 Depth=2
	movabsq	$.L.str15, %rsi
	xorq	%rax, %rax
	movq	%r8, %r14
	movq	%r10, %r15
	movq	%r11, %r13
	callq	cpp_pedwarn
	movq	%r13, %r11
	movq	%r15, %r10
	xorq	%r9, %r9
	movq	%r14, %r8
	jmp	.LBB0_251
.LBB0_256:                              # %if.else374
                                        #   in Loop: Header=BB0_135 Depth=2
	movq	%rsi, %rax
	cqto
	idivq	%r13
	movq	%rdx, -8(%r14)
	xorq	%r9, %r9
	jmp	.LBB0_251
.LBB0_217:                              # %if.else.i939
                                        #   in Loop: Header=BB0_135 Depth=2
	movq	%r13, %rcx
	testq	$255, %rax
	je	.LBB0_193
# BB#218:                               # %if.then2.i941
                                        #   in Loop: Header=BB0_135 Depth=2
	shrq	%cl, %rsi
	jmp	.LBB0_194
.LBB0_193:                              # %if.else4.i
                                        #   in Loop: Header=BB0_135 Depth=2
	sarq	%cl, %rsi
.LBB0_194:                              # %right_shift.exit
                                        #   in Loop: Header=BB0_135 Depth=2
	movq	%rsi, %rcx
.LBB0_195:                              # %right_shift.exit
                                        #   in Loop: Header=BB0_135 Depth=2
	movq	%rcx, -8(%r14)
	xorq	%r9, %r9
	jmp	.LBB0_251
.LBB0_198:                              #   in Loop: Header=BB0_135 Depth=2
	movabsq	$0, %rax
.LBB0_200:                              # %if.then.i905
                                        #   in Loop: Header=BB0_135 Depth=2
	xorq	%r15, %r15
	testq	%rsi, %rsi
	je	.LBB0_209
# BB#201:                               # %if.then.i905
                                        #   in Loop: Header=BB0_135 Depth=2
	testq	%rax, %rax
	je	.LBB0_209
# BB#202:                               # %if.then2.i908
                                        #   in Loop: Header=BB0_135 Depth=2
	movq	96(%rsp), %rax          # 8-byte Reload
	movzbl	770(%rax), %eax
	testq	%rax, %rax
	je	.LBB0_209
# BB#203:                               # %if.then.i.i909
                                        #   in Loop: Header=BB0_135 Depth=2
	movabsq	$.L.str15, %rsi
	xorq	%rax, %rax
	xorq	%r15, %r15
	movq	96(%rsp), %rdi          # 8-byte Reload
.LBB0_208:                              # %left_shift.exit
                                        #   in Loop: Header=BB0_135 Depth=2
	movq	%r8, %r13
	callq	cpp_pedwarn
	movq	72(%rsp), %r11          # 8-byte Reload
	movq	64(%rsp), %r10          # 8-byte Reload
	movq	%r13, %r8
.LBB0_209:                              # %left_shift.exit
                                        #   in Loop: Header=BB0_135 Depth=2
	movq	%r15, -8(%r14)
	xorq	%r9, %r9
	.align	16, 0x90
.LBB0_251:                              # %while.cond.backedge
                                        #   in Loop: Header=BB0_135 Depth=2
	movzbl	4(%rbx), %eax
	cmpq	%rax, %r12
	movq	%rbx, %r14
	jbe	.LBB0_135
.LBB0_252:                              # %while.end
                                        #   in Loop: Header=BB0_1 Depth=1
	movq	88(%rsp), %rbx          # 8-byte Reload
	testq	$8, %rbx
	jne	.LBB0_287
# BB#253:                               #   in Loop: Header=BB0_1 Depth=1
	movq	%r10, %rdx
	movq	56(%rsp), %rcx          # 8-byte Reload
	jmp	.LBB0_300
.LBB0_287:                              # %if.then460
                                        #   in Loop: Header=BB0_1 Depth=1
	leaq	-17(%r10), %rax
	andq	%r8, %rax
	cmpq	$3, %rax
	movq	56(%rsp), %rcx          # 8-byte Reload
	jbe	.LBB0_289
# BB#288:                               #   in Loop: Header=BB0_1 Depth=1
	movq	%r10, %rdx
	jmp	.LBB0_300
.LBB0_289:                              # %if.then460
                                        #   in Loop: Header=BB0_1 Depth=1
	jmpq	*.LJTI0_2(,%rax,8)
.LBB0_293:                              # %sw.bb468
                                        #   in Loop: Header=BB0_1 Depth=1
	cmpq	$0, 8(%r14)
	movabsq	$0, %rax
	jne	.LBB0_295
# BB#294:                               # %sw.bb468
                                        #   in Loop: Header=BB0_1 Depth=1
	movabsq	$1, %rax
.LBB0_295:                              # %sw.bb468
                                        #   in Loop: Header=BB0_1 Depth=1
	addq	%rax, %r9
	movq	%r10, %rdx
	jmp	.LBB0_300
.LBB0_75:                               # %sw.bb26.i
                                        #   in Loop: Header=BB0_1 Depth=1
	movq	8(%r12), %rax
	movq	784(%r15), %rcx
	cmpq	%rcx, %rax
	jne	.LBB0_94
# BB#76:                                # %if.then30.i
                                        #   in Loop: Header=BB0_1 Depth=1
	movzbl	15(%r15), %eax
	movq	%r15, %rbx
	movq	152(%rbx), %rcx
	movq	%rcx, 72(%rsp)          # 8-byte Spill
	incq	%rax
	movb	%al, 15(%rbx)
	movq	%rbx, %rdi
	callq	cpp_get_token
	movq	%rax, %r13
	movzbl	6(%r13), %eax
	cmpq	$22, %rax
	jne	.LBB0_77
# BB#78:                                # %if.then.i319.i
                                        #   in Loop: Header=BB0_1 Depth=1
	movq	%rbx, %rdi
	movq	%rbx, %r12
	callq	cpp_get_token
	movq	%rax, %r13
	movzbl	6(%r13), %eax
	movq	$1, %rcx
	jmp	.LBB0_79
.LBB0_8:                                # %sw.bb.i
                                        #   in Loop: Header=BB0_1 Depth=1
	movq	16(%r12), %r9
	movzbl	(%r9), %ecx
	movl	8(%r12), %eax
	cmpq	$48, %rcx
	movq	$10, %r11
	movq	%r9, %r12
	jne	.LBB0_12
# BB#9:                                 # %if.then.i.i
                                        #   in Loop: Header=BB0_1 Depth=1
	leaq	1(%r9), %r12
	cmpq	$3, %rax
	movq	$8, %r11
	jb	.LBB0_12
# BB#10:                                # %land.lhs.true.i.i
                                        #   in Loop: Header=BB0_1 Depth=1
	movzbl	(%r12), %ecx
	orq	$32, %rcx
	andq	$255, %rcx
	cmpq	$120, %rcx
	jne	.LBB0_12
# BB#11:                                # %if.then14.i.i
                                        #   in Loop: Header=BB0_1 Depth=1
	leaq	2(%r9), %r12
	movq	$16, %r11
.LBB0_12:                               # %if.end17.i.i
                                        #   in Loop: Header=BB0_1 Depth=1
	movq	%r11, 32(%rsp)          # 8-byte Spill
	addq	%rax, %r9
	andq	%r8, %r11
	movq	$-1, %rax
	xorq	%rdx, %rdx
	divq	%r11
	cmpq	%r9, %r12
	jae	.LBB0_13
# BB#14:                                # %for.body.lr.ph.i.i
                                        #   in Loop: Header=BB0_1 Depth=1
	xorq	%rdx, %rdx
	cmpq	$16, %r11
	movabsq	$0, %r10
	movabsq	$0, %rdi
	movabsq	$0, %rbx
	jne	.LBB0_15
	.align	16, 0x90
.LBB0_34:                               # %for.body.i.i
                                        #   Parent Loop BB0_1 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movzbl	(%r12), %esi
	movzbl	_sch_istable(%rsi,%rsi), %ecx
	testq	$260, %rcx              # imm = 0x104
	je	.LBB0_35
# BB#36:                                # %if.then35.i.i
                                        #   in Loop: Header=BB0_34 Depth=2
	movsbq	_hex_value(%rsi), %r13
	movq	$32, %rcx
	movq	%rdx, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	cmpq	%r13, %rsi
	movq	%r13, %rsi
	jl	.LBB0_38
# BB#37:                                # %if.then35.i.i
                                        #   in Loop: Header=BB0_34 Depth=2
	movq	%rdx, %rsi
.LBB0_38:                               # %if.then35.i.i
                                        #   in Loop: Header=BB0_34 Depth=2
	movq	%rbx, %rcx
	imulq	%r11, %rcx
	addq	%rcx, %r13
	cmpq	%rbx, %rax
	sbbq	%rcx, %rcx
	cmpq	%rbx, %r13
	sbbq	%rdx, %rdx
	orq	%rcx, %rdx
	andq	$1, %rdx
	orq	%rdx, %r10
	incq	%r12
	xorq	%rcx, %rcx
	cmpq	%r9, %r12
	movq	%rsi, %rdx
	movq	%r13, %rbx
	jb	.LBB0_34
	jmp	.LBB0_58
	.align	16, 0x90
.LBB0_15:                               # %for.body.us.i.i
                                        #   Parent Loop BB0_1 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movzbl	(%r12), %esi
	movzbl	_sch_istable(%rsi,%rsi), %ecx
	testq	$4, %rcx
	je	.LBB0_16
# BB#31:                                # %if.then35.us.i.i
                                        #   in Loop: Header=BB0_15 Depth=2
	movsbq	_hex_value(%rsi), %r13
	movq	$32, %rcx
	movq	%rdx, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	cmpq	%r13, %rsi
	movq	%r13, %rsi
	jl	.LBB0_33
# BB#32:                                # %if.then35.us.i.i
                                        #   in Loop: Header=BB0_15 Depth=2
	movq	%rdx, %rsi
.LBB0_33:                               # %if.then35.us.i.i
                                        #   in Loop: Header=BB0_15 Depth=2
	movq	%rdi, %rcx
	imulq	%r11, %rcx
	addq	%rcx, %r13
	cmpq	%rdi, %rax
	sbbq	%rcx, %rcx
	cmpq	%rdi, %r13
	sbbq	%rdx, %rdx
	orq	%rcx, %rdx
	andq	$1, %rdx
	orq	%rdx, %r10
	incq	%r12
	xorq	%rcx, %rcx
	cmpq	%r9, %r12
	movq	%rsi, %rdx
	movq	%r13, %rdi
	jb	.LBB0_15
	jmp	.LBB0_58
.LBB0_71:                               # %sw.bb10.i
                                        #   in Loop: Header=BB0_1 Depth=1
	movq	%r8, %rbx
	movzbl	8(%r12), %edx
	movzbl	_sch_istable(%rdx,%rdx), %eax
	testq	$172, %rax
	jne	.LBB0_72
# BB#74:                                # %do.body20.i
                                        #   in Loop: Header=BB0_1 Depth=1
	xorq	%rax, %rax
	movq	%r15, %rdi
	movabsq	$.L.str18, %rsi
	jmp	.LBB0_73
.LBB0_94:                               # %if.else32.i
                                        #   in Loop: Header=BB0_1 Depth=1
	movzbl	742(%r15), %ecx
	testq	%rcx, %rcx
	je	.LBB0_104
# BB#95:                                # %land.lhs.true.i
                                        #   in Loop: Header=BB0_1 Depth=1
	movq	792(%r15), %rcx
	cmpq	%rcx, %rax
	je	.LBB0_97
# BB#96:                                # %lor.lhs.false.i
                                        #   in Loop: Header=BB0_1 Depth=1
	movq	800(%r15), %rdx
	cmpq	%rdx, %rax
	jne	.LBB0_104
.LBB0_97:                               # %if.then45.i
                                        #   in Loop: Header=BB0_1 Depth=1
	movq	%r8, %rbx
	cmpq	%rcx, %rax
	movabsq	$0, %r13
	jne	.LBB0_99
# BB#98:                                # %if.then45.i
                                        #   in Loop: Header=BB0_1 Depth=1
	movabsq	$1, %r13
.LBB0_99:                               # %if.then45.i
                                        #   in Loop: Header=BB0_1 Depth=1
	movzbl	770(%r15), %eax
	testq	%rax, %rax
	movq	$57, %r10
	jne	.LBB0_101
# BB#100:                               #   in Loop: Header=BB0_1 Depth=1
	movq	%rbx, %r8
	jmp	.LBB0_115
.LBB0_72:                               # %do.body14.i
                                        #   in Loop: Header=BB0_1 Depth=1
	xorq	%rax, %rax
	movq	%r15, %rdi
	movabsq	$.L.str17, %rsi
.LBB0_73:                               # %lex.exit
                                        #   in Loop: Header=BB0_1 Depth=1
	callq	cpp_error
	movq	$64, %r10
                                        # implicit-def: R13
	movq	%rbx, %r8
	jmp	.LBB0_115
.LBB0_13:                               #   in Loop: Header=BB0_1 Depth=1
	xorq	%rsi, %rsi
	xorq	%r10, %r10
	xorq	%r13, %r13
	xorq	%rcx, %rcx
.LBB0_58:                               # %if.end165.i.i
                                        #   in Loop: Header=BB0_1 Depth=1
	movq	%rcx, 64(%rsp)          # 8-byte Spill
	movq	$32, %rcx
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	cmpq	%rsi, 32(%rsp)          # 8-byte Folded Reload
	jg	.LBB0_60
# BB#59:                                # %if.then168.i.i
                                        #   in Loop: Header=BB0_1 Depth=1
	xorq	%rax, %rax
	movq	%r15, %rdi
	movabsq	$.L.str30, %rsi
	movq	%r8, %rbx
	movq	%r11, %r12
	movq	%r10, 24(%rsp)          # 8-byte Spill
	callq	cpp_pedwarn
	movq	24(%rsp), %r10          # 8-byte Reload
	movq	%r12, %r11
	movq	%rbx, %r8
.LBB0_60:                               # %if.end169.i.i
                                        #   in Loop: Header=BB0_1 Depth=1
	testq	%r8, %r10
	jne	.LBB0_61
# BB#62:                                # %if.else172.i.i
                                        #   in Loop: Header=BB0_1 Depth=1
	testq	%r13, %r13
	movq	64(%rsp), %r10          # 8-byte Reload
	jns	.LBB0_63
# BB#64:                                # %if.else172.i.i
                                        #   in Loop: Header=BB0_1 Depth=1
	testq	$255, %r10
	movq	88(%rsp), %rbx          # 8-byte Reload
	jne	.LBB0_67
# BB#65:                                # %if.then178.i.i
                                        #   in Loop: Header=BB0_1 Depth=1
	movq	$1, %r10
	cmpq	$10, %r11
	jne	.LBB0_67
# BB#66:                                # %if.then181.i.i
                                        #   in Loop: Header=BB0_1 Depth=1
	xorq	%rax, %rax
	movq	%r15, %rdi
	movabsq	$.L.str32, %rsi
	callq	cpp_warning
	movq	$1, %r10
	jmp	.LBB0_67
.LBB0_282:                              # %sw.bb436
                                        #   in Loop: Header=BB0_1 Depth=1
	movq	%r10, %rax
	andq	%r8, %rax
	cmpq	$23, %rax
	jne	.LBB0_283
# BB#284:                               # %if.end444
                                        #   in Loop: Header=BB0_1 Depth=1
	movq	$48, %rcx
	shlq	%cl, %r15
	movabsq	$-71776119061217281, %rax # imm = 0xFF00FFFFFFFFFFFF
	andq	%rax, %r10
	orq	%r15, %r10
	movq	%rbx, %r14
	movq	96(%rsp), %r15          # 8-byte Reload
.LBB0_128:                              # %sw.bb6
                                        #   in Loop: Header=BB0_1 Depth=1
	movzbl	5(%r14), %eax
	testq	$1, %rax
	jne	.LBB0_129
# BB#131:                               # %if.end
                                        #   in Loop: Header=BB0_1 Depth=1
	movq	%r13, 8(%r14)
	movq	$48, %rcx
	shrq	%cl, %r10
	movb	%r10b, 6(%r14)
	orq	$1, %rax
	movb	%al, 5(%r14)
	jmp	.LBB0_1
.LBB0_61:                               # %if.then171.i.i
                                        #   in Loop: Header=BB0_1 Depth=1
	xorq	%rax, %rax
	movq	%r15, %rdi
	movabsq	$.L.str31, %rsi
	callq	cpp_pedwarn
	movq	64(%rsp), %r10          # 8-byte Reload
	movq	88(%rsp), %rbx          # 8-byte Reload
	jmp	.LBB0_67
.LBB0_325:                              #   in Loop: Header=BB0_1 Depth=1
	movq	$4, %rcx
	movq	%r10, %rdx
	jmp	.LBB0_300
.LBB0_104:                              # %if.else66.i
                                        #   in Loop: Header=BB0_1 Depth=1
	movzbl	768(%r15), %ecx
	xorq	%r13, %r13
	testq	%rcx, %rcx
	movq	$57, %r10
	je	.LBB0_115
# BB#105:                               # %if.else66.i
                                        #   in Loop: Header=BB0_1 Depth=1
	movq	104(%rsp), %rcx         # 8-byte Reload
	testq	%r8, %rcx
	movq	88(%rsp), %rbx          # 8-byte Reload
	jne	.LBB0_106
# BB#107:                               # %if.then75.i
                                        #   in Loop: Header=BB0_1 Depth=1
	movq	8(%rax), %rdx
	xorq	%rax, %rax
	xorq	%r13, %r13
	movq	%r15, %rdi
	movabsq	$.L.str21, %rsi
	movq	%r8, %r12
	callq	cpp_warning
	movq	%r12, %r8
	movq	$57, %r10
	jmp	.LBB0_116
.LBB0_290:                              # %sw.bb462
                                        #   in Loop: Header=BB0_1 Depth=1
	cmpq	$0, 8(%r14)
	movabsq	$0, %rax
	je	.LBB0_292
# BB#291:                               # %sw.bb462
                                        #   in Loop: Header=BB0_1 Depth=1
	movabsq	$1, %rax
.LBB0_292:                              # %sw.bb462
                                        #   in Loop: Header=BB0_1 Depth=1
	addq	%rax, %r9
	movq	$18, %rdx
	jmp	.LBB0_300
.LBB0_296:                              # %sw.bb474
                                        #   in Loop: Header=BB0_1 Depth=1
	cmpq	$0, -8(%r14)
	jne	.LBB0_297
# BB#298:                               # %if.else480
                                        #   in Loop: Header=BB0_1 Depth=1
	decq	%r9
	jmp	.LBB0_299
.LBB0_77:                               #   in Loop: Header=BB0_1 Depth=1
	movq	%rbx, %r12
	xorq	%rcx, %rcx
.LBB0_79:                               # %if.end.i.i
                                        #   in Loop: Header=BB0_1 Depth=1
	andq	$255, %rax
	cmpq	$56, %rax
	jne	.LBB0_83
# BB#80:                                # %if.then6.i.i
                                        #   in Loop: Header=BB0_1 Depth=1
	movq	8(%r13), %r15
	testq	%rcx, %rcx
	je	.LBB0_87
# BB#81:                                # %land.lhs.true.i320.i
                                        #   in Loop: Header=BB0_1 Depth=1
	movq	%r12, %rdi
	callq	cpp_get_token
	movzbl	6(%rax), %eax
	cmpq	$23, %rax
	jne	.LBB0_82
.LBB0_87:                               # %if.end26.i.i
                                        #   in Loop: Header=BB0_1 Depth=1
	movq	$64, %r10
	testq	%r15, %r15
                                        # implicit-def: R13
	movabsq	$4294967295, %r8        # imm = 0xFFFFFFFF
	movq	88(%rsp), %rbx          # 8-byte Reload
	je	.LBB0_93
# BB#88:                                # %if.else30.i.i
                                        #   in Loop: Header=BB0_1 Depth=1
	movq	152(%r12), %rax
	movq	72(%rsp), %rcx          # 8-byte Reload
	cmpq	%rcx, %rax
	je	.LBB0_90
# BB#89:                                # %if.then34.i.i
                                        #   in Loop: Header=BB0_1 Depth=1
	xorq	%rax, %rax
	movq	%r12, %rdi
	movabsq	$.L.str26, %rsi
	movq	%rbx, %r13
	movq	%r8, %rbx
	callq	cpp_warning
	movq	%rbx, %r8
	movq	%r13, %rbx
.LBB0_90:                               # %if.end35.i.i
                                        #   in Loop: Header=BB0_1 Depth=1
	movzbl	20(%r15), %eax
	cmpq	$1, %rax
	movabsq	$0, %r13
	jne	.LBB0_92
# BB#91:                                # %if.end35.i.i
                                        #   in Loop: Header=BB0_1 Depth=1
	movabsq	$1, %r13
.LBB0_92:                               # %if.end35.i.i
                                        #   in Loop: Header=BB0_1 Depth=1
	movq	%r15, 176(%r12)
	movq	$57, %r10
	jmp	.LBB0_93
.LBB0_83:                               # %if.else.i.i
                                        #   in Loop: Header=BB0_1 Depth=1
	xorq	%rax, %rax
	movq	%r12, %r15
	movq	%r15, %rdi
	movabsq	$.L.str24, %rsi
	callq	cpp_error
	movzbl	7(%r13), %eax
	testq	$16, %rax
	movq	$64, %r10
	je	.LBB0_84
# BB#86:                                # %if.then16.i.i
                                        #   in Loop: Header=BB0_1 Depth=1
	xorq	%rax, %rax
	movb	%al, 447(%rsp)
	movzbl	6(%r13), %eax
	movb	%al, 446(%rsp)
	movq	%r15, %rdi
	movq	%r13, %rsi
	callq	cpp_token_as_text
	movq	%rax, %rbx
	movq	%r15, %rdi
	leaq	440(%rsp), %rsi
	callq	cpp_token_as_text
	movq	%rax, %rcx
	xorq	%rax, %rax
	movq	%r15, %rdi
	movabsq	$.L.str25, %rsi
	movq	%rbx, %rdx
	callq	cpp_error
	movq	$64, %r10
.LBB0_84:                               #   in Loop: Header=BB0_1 Depth=1
                                        # implicit-def: R13
	movq	%r15, %r12
	jmp	.LBB0_85
.LBB0_297:                              # %if.then478
                                        #   in Loop: Header=BB0_1 Depth=1
	incq	%r9
.LBB0_299:                              # %skip_reduction
                                        #   in Loop: Header=BB0_1 Depth=1
	movq	$20, %rdx
	.align	16, 0x90
.LBB0_300:                              # %skip_reduction
                                        #   in Loop: Header=BB0_1 Depth=1
	movzbl	5(%r14), %eax
	andq	$1, %rax
	testq	$2, %rbx
	jne	.LBB0_301
# BB#303:                               # %if.else500
                                        #   in Loop: Header=BB0_1 Depth=1
	movq	%rdx, 56(%rsp)          # 8-byte Spill
	testq	%rax, %rax
	jne	.LBB0_305
	jmp	.LBB0_304
.LBB0_301:                              # %if.then488
                                        #   in Loop: Header=BB0_1 Depth=1
	movq	%rdx, 56(%rsp)          # 8-byte Spill
	testq	%rax, %rax
	jne	.LBB0_302
.LBB0_305:                              # %if.end512
                                        #   in Loop: Header=BB0_1 Depth=1
	addq	$16, %r14
	movq	48(%rsp), %rax          # 8-byte Reload
	cmpq	%rax, %r14
	movq	%rcx, %r12
	jne	.LBB0_310
# BB#306:                               # %if.then516
                                        #   in Loop: Header=BB0_1 Depth=1
	movq	%r11, 72(%rsp)          # 8-byte Spill
	movq	%r9, 104(%rsp)          # 8-byte Spill
	movq	80(%rsp), %r14          # 8-byte Reload
	subq	%r14, %rax
	movq	$33, %rcx
	movq	%rax, %r13
	shlq	%cl, %r13
	movq	$32, %rcx
	sarq	%cl, %r13
	movq	%rax, %r15
	leaq	112(%rsp), %rax
	cmpq	%rax, %r14
	jne	.LBB0_307
# BB#308:                               # %if.else525
                                        #   in Loop: Header=BB0_1 Depth=1
	movq	%r13, %rdi
	callq	xmalloc
	movq	%rbx, 88(%rsp)          # 8-byte Spill
	movq	%r12, %rbx
	movq	%rax, %r12
	movq	$32, %rcx
	shlq	%cl, %r15
	movq	$32, %rcx
	sarq	%cl, %r15
	movq	%r12, %rdi
	movq	%r14, %rsi
	movq	%r15, %rdx
	callq	memcpy
	movq	%r15, %r14
	movq	%r12, %rax
	movq	%rbx, %r12
	movq	88(%rsp), %rbx          # 8-byte Reload
	jmp	.LBB0_309
.LBB0_307:                              # %if.then522
                                        #   in Loop: Header=BB0_1 Depth=1
	movq	%r14, %rdi
	movq	%r13, %rsi
	callq	xrealloc
	movq	$32, %rcx
	shlq	%cl, %r15
	movq	$32, %rcx
	sarq	%cl, %r15
	movq	%r15, %r14
.LBB0_309:                              # %if.end529
                                        #   in Loop: Header=BB0_1 Depth=1
	movq	%rax, 80(%rsp)          # 8-byte Spill
	addq	%rax, %r13
	addq	%rax, %r14
	movq	%r13, 48(%rsp)          # 8-byte Spill
	movabsq	$4294967295, %r8        # imm = 0xFFFFFFFF
	movq	104(%rsp), %r9          # 8-byte Reload
	movq	72(%rsp), %r11          # 8-byte Reload
	jmp	.LBB0_310
.LBB0_35:                               #   in Loop: Header=BB0_1 Depth=1
	movq	%rdx, 16(%rsp)          # 8-byte Spill
	movq	%rbx, %r13
	jmp	.LBB0_17
.LBB0_16:                               #   in Loop: Header=BB0_1 Depth=1
	movq	%rdx, 16(%rsp)          # 8-byte Spill
	movq	%rdi, %r13
.LBB0_17:                               # %if.then54.i.i
                                        #   in Loop: Header=BB0_1 Depth=1
	movq	%r10, 24(%rsp)          # 8-byte Spill
	leaq	-46(%rsi), %rcx
	andq	%r8, %rcx
	cmpq	$56, %rcx
	ja	.LBB0_19
# BB#18:                                # %if.then54.i.i
                                        #   in Loop: Header=BB0_1 Depth=1
	movq	$1, %rax
	shlq	%cl, %rax
	movabsq	$72057594054705153, %rcx # imm = 0x100000001000001
	testq	%rcx, %rax
	jne	.LBB0_29
.LBB0_19:                               # %lor.lhs.false63.i.i
                                        #   in Loop: Header=BB0_1 Depth=1
	andq	$223, %rsi
	cmpq	$10, %r11
	jne	.LBB0_24
# BB#20:                                # %lor.lhs.false63.i.i
                                        #   in Loop: Header=BB0_1 Depth=1
	cmpq	$69, %rsi
	jne	.LBB0_24
# BB#21:                                # %land.lhs.true72.i.i
                                        #   in Loop: Header=BB0_1 Depth=1
	leaq	1(%r12), %rax
	cmpq	%r9, %rax
	jae	.LBB0_24
# BB#22:                                # %land.lhs.true76.i.i
                                        #   in Loop: Header=BB0_1 Depth=1
	movzbl	(%rax), %eax
	cmpq	$43, %rax
	je	.LBB0_29
# BB#23:                                # %land.lhs.true76.i.i
                                        #   in Loop: Header=BB0_1 Depth=1
	cmpq	$45, %rax
	je	.LBB0_29
.LBB0_24:                               # %lor.lhs.false86.i.i
                                        #   in Loop: Header=BB0_1 Depth=1
	cmpq	$16, %r11
	jne	.LBB0_39
# BB#25:                                # %lor.lhs.false86.i.i
                                        #   in Loop: Header=BB0_1 Depth=1
	cmpq	$80, %rsi
	jne	.LBB0_39
# BB#26:                                # %land.lhs.true95.i.i
                                        #   in Loop: Header=BB0_1 Depth=1
	leaq	1(%r12), %rax
	cmpq	%r9, %rax
	jae	.LBB0_39
# BB#27:                                # %land.lhs.true99.i.i
                                        #   in Loop: Header=BB0_1 Depth=1
	movzbl	(%rax), %eax
	cmpq	$43, %rax
	je	.LBB0_29
# BB#28:                                # %land.lhs.true99.i.i
                                        #   in Loop: Header=BB0_1 Depth=1
	cmpq	$45, %rax
	jne	.LBB0_39
.LBB0_29:                               # %do.body.i.i
                                        #   in Loop: Header=BB0_1 Depth=1
	xorq	%rax, %rax
	movq	%r15, %rdi
	movabsq	$.L.str27, %rsi
	movq	%r8, %rbx
	callq	cpp_error
	jmp	.LBB0_30
.LBB0_39:                               # %if.end110.i.i
                                        #   in Loop: Header=BB0_1 Depth=1
	movq	%r9, %rcx
	subq	%r12, %rcx
	movq	$4, 56(%rsp)            # 8-byte Folded Spill
	cmpq	$1, %rcx
	movabsq	$vsuf_1, %rax
	je	.LBB0_44
# BB#40:                                # %if.end110.i.i
                                        #   in Loop: Header=BB0_1 Depth=1
	cmpq	$3, %rcx
	jne	.LBB0_41
# BB#43:                                # %sw.bb115.i.i
                                        #   in Loop: Header=BB0_1 Depth=1
	movq	$8, 56(%rsp)            # 8-byte Folded Spill
	movabsq	$vsuf_3, %rax
	jmp	.LBB0_44
.LBB0_63:                               #   in Loop: Header=BB0_1 Depth=1
	movq	88(%rsp), %rbx          # 8-byte Reload
.LBB0_67:                               # %if.end185.i.i
                                        #   in Loop: Header=BB0_1 Depth=1
	andq	$255, %r10
	movq	$48, %rcx
	shlq	%cl, %r10
	orq	$57, %r10
	movabsq	$4294967295, %r8        # imm = 0xFFFFFFFF
	jmp	.LBB0_116
.LBB0_101:                              # %land.lhs.true59.i
                                        #   in Loop: Header=BB0_1 Depth=1
	movq	$29, %rdx
	movq	%r15, %rdi
	movabsq	$.L.str19, %rsi
	callq	cpp_defined
	movq	%rbx, %r8
	testq	%r8, %rax
	jne	.LBB0_102
# BB#103:                               # %if.then62.i
                                        #   in Loop: Header=BB0_1 Depth=1
	movq	8(%r12), %rax
	movq	8(%rax), %rdx
	xorq	%rax, %rax
	movq	%r15, %rdi
	movabsq	$.L.str20, %rsi
	movq	%r8, %rbx
	callq	cpp_pedwarn
	movq	%rbx, %r8
	movq	$57, %r10
	jmp	.LBB0_115
.LBB0_106:                              #   in Loop: Header=BB0_1 Depth=1
	movq	$57, %r10
	jmp	.LBB0_116
.LBB0_82:                               # %if.then13.i.i
                                        #   in Loop: Header=BB0_1 Depth=1
	xorq	%rax, %rax
	movq	%r12, %rdi
	movabsq	$.L.str23, %rsi
	callq	cpp_error
	movq	$64, %r10
                                        # implicit-def: R13
.LBB0_85:                               # %parse_defined.exit.i
                                        #   in Loop: Header=BB0_1 Depth=1
	movabsq	$4294967295, %r8        # imm = 0xFFFFFFFF
	movq	88(%rsp), %rbx          # 8-byte Reload
.LBB0_93:                               # %parse_defined.exit.i
                                        #   in Loop: Header=BB0_1 Depth=1
	movzbl	15(%r12), %eax
	decq	%rax
	movb	%al, 15(%r12)
	movq	%r12, %r15
	jmp	.LBB0_116
.LBB0_102:                              #   in Loop: Header=BB0_1 Depth=1
	movq	$57, %r10
	jmp	.LBB0_115
.LBB0_41:                               # %if.end110.i.i
                                        #   in Loop: Header=BB0_1 Depth=1
	cmpq	$2, %rcx
	movq	%rcx, 64(%rsp)          # 8-byte Spill
	jne	.LBB0_68
# BB#42:                                # %sw.bb114.i.i
                                        #   in Loop: Header=BB0_1 Depth=1
	movq	$10, 56(%rsp)           # 8-byte Folded Spill
	movabsq	$vsuf_2, %rax
	movq	64(%rsp), %rcx          # 8-byte Reload
.LBB0_44:                               # %sw.epilog.i.i
                                        #   in Loop: Header=BB0_1 Depth=1
	movq	%rax, 8(%rsp)           # 8-byte Spill
	movq	%rcx, 64(%rsp)          # 8-byte Spill
	movq	%r11, (%rsp)            # 8-byte Spill
	xorq	%r15, %r15
	movq	%rax, %rdx
	xorq	%rbx, %rbx
.LBB0_46:                               # %for.body119.i.i
                                        #   Parent Loop BB0_1 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	%rdx, 72(%rsp)          # 8-byte Spill
	movq	%r12, %rdi
	movq	72(%rsp), %rsi          # 8-byte Reload
	movq	64(%rsp), %rdx          # 8-byte Reload
	callq	memcmp
	movq	72(%rsp), %rdx          # 8-byte Reload
	movabsq	$4294967295, %r8        # imm = 0xFFFFFFFF
	testq	%r8, %rax
	movq	%r15, %rax
	je	.LBB0_47
# BB#45:                                # %for.cond116.i.i
                                        #   in Loop: Header=BB0_46 Depth=2
	incq	%r15
	incq	%rbx
	movq	$32, %rcx
	movq	%r15, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	addq	$6, %rdx
	movq	56(%rsp), %rcx          # 8-byte Reload
	cmpq	%rcx, %rax
	movq	%rbx, %rax
	jl	.LBB0_46
.LBB0_47:                               # %for.end130.i.i
                                        #   in Loop: Header=BB0_1 Depth=1
	movq	56(%rsp), %rdx          # 8-byte Reload
	andq	%r8, %rdx
	movq	%rax, %rcx
	andq	%r8, %rcx
	cmpq	%rdx, %rcx
	movq	96(%rsp), %r15          # 8-byte Reload
	jne	.LBB0_48
.LBB0_68:                               # %invalid_suffix.i.i
                                        #   in Loop: Header=BB0_1 Depth=1
	xorq	%rax, %rax
	movq	%r15, %rdi
	movabsq	$.L.str33, %rsi
	movq	64(%rsp), %rdx          # 8-byte Reload
	movq	%r12, %rcx
	movq	%r8, %rbx
	callq	cpp_error
.LBB0_30:                               # %lex.exit
                                        #   in Loop: Header=BB0_1 Depth=1
	movq	%rbx, %r8
	movq	$64, %r10
	jmp	.LBB0_115
.LBB0_48:                               # %if.end134.i.i
                                        #   in Loop: Header=BB0_1 Depth=1
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	leaq	(%rax,%rax,2), %rbx
	movzbl	761(%r15), %eax
	movq	8(%rsp), %rdx           # 8-byte Reload
	movzbl	4(%rdx,%rbx,2), %ecx
	testq	%rax, %rax
	je	.LBB0_52
# BB#49:                                # %if.end134.i.i
                                        #   in Loop: Header=BB0_1 Depth=1
	testq	$255, %rcx
	je	.LBB0_52
# BB#50:                                # %land.lhs.true146.i.i
                                        #   in Loop: Header=BB0_1 Depth=1
	movq	%r15, %rdi
	movq	%rcx, %r12
	callq	cpp_sys_macro_p
	movq	8(%rsp), %rdx           # 8-byte Reload
	movq	%r12, %rcx
	movabsq	$4294967295, %r8        # imm = 0xFFFFFFFF
	testq	%r8, %rax
	jne	.LBB0_52
# BB#51:                                # %if.then149.i.i
                                        #   in Loop: Header=BB0_1 Depth=1
	xorq	%rax, %rax
	movq	%r15, %rdi
	movabsq	$.L.str28, %rsi
	movq	%rcx, %r12
	callq	cpp_warning
	movq	8(%rsp), %rdx           # 8-byte Reload
	movq	%r12, %rcx
	movabsq	$4294967295, %r8        # imm = 0xFFFFFFFF
.LBB0_52:                               # %if.end150.i.i
                                        #   in Loop: Header=BB0_1 Depth=1
	movzbl	5(%rdx,%rbx,2), %eax
	cmpq	$2, %rax
	movq	(%rsp), %r11            # 8-byte Reload
	movq	16(%rsp), %rsi          # 8-byte Reload
	jne	.LBB0_53
# BB#54:                                # %land.lhs.true156.i.i
                                        #   in Loop: Header=BB0_1 Depth=1
	movzbl	770(%r15), %eax
	testq	%rax, %rax
	movq	24(%rsp), %r10          # 8-byte Reload
	je	.LBB0_58
# BB#55:                                # %land.lhs.true160.i.i
                                        #   in Loop: Header=BB0_1 Depth=1
	movq	%r8, %rbx
	movzbl	769(%r15), %eax
	testq	%rax, %rax
	jne	.LBB0_57
# BB#56:                                # %if.then163.i.i
                                        #   in Loop: Header=BB0_1 Depth=1
	xorq	%rax, %rax
	movq	%r15, %rdi
	movabsq	$.L.str29, %rsi
	movq	%rcx, 64(%rsp)          # 8-byte Spill
	callq	cpp_pedwarn
	movq	24(%rsp), %r10          # 8-byte Reload
	movq	(%rsp), %r11            # 8-byte Reload
	movq	64(%rsp), %rcx          # 8-byte Reload
.LBB0_57:                               # %if.end165.i.i
                                        #   in Loop: Header=BB0_1 Depth=1
	movq	%rbx, %r8
	movq	16(%rsp), %rsi          # 8-byte Reload
	jmp	.LBB0_58
.LBB0_53:                               #   in Loop: Header=BB0_1 Depth=1
	movq	24(%rsp), %r10          # 8-byte Reload
	jmp	.LBB0_58
.LBB0_143:                              # %sw.default66
	movb	%dl, 446(%rsp)
	xorq	%r12, %r12
	movb	%r12b, 447(%rsp)
	leaq	440(%rsp), %rsi
	movq	96(%rsp), %rbx          # 8-byte Reload
	movq	%rbx, %rdi
	callq	cpp_token_as_text
	movq	%rax, %rcx
	movabsq	$.L.str3, %rsi
	xorq	%rax, %rax
	movq	%rbx, %rdi
	movq	%rcx, %rdx
	callq	cpp_ice
	movq	80(%rsp), %r14          # 8-byte Reload
	jmp	.LBB0_322
.LBB0_136:                              # %if.then43
	movl	(%r14), %eax
	cmpq	$22, %rax
	jne	.LBB0_139
# BB#137:                               # %do.body48
	movabsq	$.L.str1, %rsi
.LBB0_138:                              # %if.end572
	xorq	%rax, %rax
	xorq	%r12, %r12
	movq	96(%rsp), %rdi          # 8-byte Reload
.LBB0_130:                              # %if.end572
	callq	cpp_error
	movq	80(%rsp), %r14          # 8-byte Reload
	jmp	.LBB0_322
.LBB0_139:                              # %do.body51
	movb	%al, 446(%rsp)
	xorq	%r12, %r12
	movb	%r12b, 447(%rsp)
	leaq	440(%rsp), %rsi
	movq	96(%rsp), %rbx          # 8-byte Reload
	movq	%rbx, %rdi
	callq	cpp_token_as_text
	movq	%rax, %rcx
	movabsq	$.L.str2, %rsi
.LBB0_140:                              # %if.end572
	xorq	%rax, %rax
	movq	%rbx, %rdi
	movq	%rcx, %rdx
	callq	cpp_error
	movq	80(%rsp), %r14          # 8-byte Reload
	jmp	.LBB0_322
.LBB0_125:
	movq	80(%rsp), %r14          # 8-byte Reload
.LBB0_322:                              # %if.end572
	leaq	112(%rsp), %rax
	cmpq	%rax, %r14
	je	.LBB0_324
# BB#323:                               # %if.then576
	movq	%r14, %rdi
	callq	free
.LBB0_324:                              # %if.end577
	movq	%r12, %rax
	addq	$464, %rsp              # imm = 0x1D0
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB0_273:                              # %do.body407
	movabsq	$.L.str7, %rsi
	jmp	.LBB0_138
.LBB0_285:                              # %sw.bb448
	andq	%r8, %r10
	cmpq	$23, %r10
	jne	.LBB0_311
# BB#286:                               # %do.body453
	movabsq	$.L.str10, %rsi
	jmp	.LBB0_138
.LBB0_247:                              # %do.body341
	movabsq	$.L.str5, %rsi
	jmp	.LBB0_138
.LBB0_275:                              # %do.body416
	movabsq	$.L.str8, %rsi
	jmp	.LBB0_138
.LBB0_311:                              # %done
	movq	96(%rsp), %rdi          # 8-byte Reload
	cmpq	$0, 176(%rdi)
	movq	80(%rsp), %r14          # 8-byte Reload
	je	.LBB0_315
# BB#312:                               # %land.lhs.true543
	movq	40(%rsp), %rax          # 8-byte Reload
	testq	%r8, %rax
	je	.LBB0_314
# BB#313:                               # %land.lhs.true543
	andq	%r8, %r11
	cmpq	$3, %r11
	je	.LBB0_315
.LBB0_314:                              # %if.then548
	movq	$0, 176(%rdi)
.LBB0_315:                              # %if.end550
	cmpq	%r14, %rbx
	je	.LBB0_317
# BB#316:                               # %do.body558
	movabsq	$.L.str13, %rsi
	xorq	%rax, %rax
	xorq	%r12, %r12
	callq	cpp_ice
	jmp	.LBB0_322
.LBB0_129:                              # %do.body
	movabsq	$.L.str, %rsi
	xorq	%rax, %rax
	xorq	%r12, %r12
	movq	%r15, %rdi
	jmp	.LBB0_130
.LBB0_304:                              # %do.body506
	movb	%r10b, 446(%rsp)
	xorq	%r12, %r12
	movb	%r12b, 447(%rsp)
	leaq	440(%rsp), %rsi
	movq	96(%rsp), %rbx          # 8-byte Reload
	movq	%rbx, %rdi
	callq	cpp_token_as_text
	movq	%rax, %rcx
	movabsq	$.L.str12, %rsi
	jmp	.LBB0_140
.LBB0_302:                              # %do.body494
	movb	%r10b, 446(%rsp)
	xorq	%r12, %r12
	movb	%r12b, 447(%rsp)
	leaq	440(%rsp), %rsi
	movq	96(%rsp), %rbx          # 8-byte Reload
	movq	%rbx, %rdi
	callq	cpp_token_as_text
	movq	%rax, %rcx
	movabsq	$.L.str11, %rsi
	jmp	.LBB0_140
.LBB0_317:                              # %if.else561
	testq	%r13, %r13
	je	.LBB0_318
# BB#319:                               # %if.else561
	movabsq	$1, %r12
	jmp	.LBB0_320
.LBB0_318:
	movabsq	$0, %r12
.LBB0_320:                              # %if.else561
	testq	$1, %rcx
	jne	.LBB0_322
# BB#321:                               # %do.body568
	movabsq	$.L.str14, %rsi
	xorq	%rax, %rax
	xorq	%r12, %r12
	callq	cpp_error
	jmp	.LBB0_322
.LBB0_283:                              # %do.body441
	movabsq	$.L.str9, %rsi
	jmp	.LBB0_138
.Ltmp17:
	.size	_cpp_parse_expr, .Ltmp17-_cpp_parse_expr
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI0_0:
	.quad	.LBB0_75
	.quad	.LBB0_8
	.quad	.LBB0_69
	.quad	.LBB0_69
	.quad	.LBB0_71
	.quad	.LBB0_70
	.quad	.LBB0_70
.LJTI0_1:
	.quad	.LBB0_144
	.quad	.LBB0_156
	.quad	.LBB0_150
	.quad	.LBB0_223
	.quad	.LBB0_232
	.quad	.LBB0_239
	.quad	.LBB0_245
	.quad	.LBB0_245
	.quad	.LBB0_183
	.quad	.LBB0_185
	.quad	.LBB0_184
	.quad	.LBB0_210
	.quad	.LBB0_186
	.quad	.LBB0_219
	.quad	.LBB0_222
	.quad	.LBB0_149
	.quad	.LBB0_263
	.quad	.LBB0_257
	.quad	.LBB0_273
	.quad	.LBB0_274
	.quad	.LBB0_270
	.quad	.LBB0_282
	.quad	.LBB0_143
	.quad	.LBB0_177
	.quad	.LBB0_181
	.quad	.LBB0_170
	.quad	.LBB0_163
	.quad	.LBB0_143
	.quad	.LBB0_143
	.quad	.LBB0_143
	.quad	.LBB0_143
	.quad	.LBB0_143
	.quad	.LBB0_143
	.quad	.LBB0_143
	.quad	.LBB0_143
	.quad	.LBB0_143
	.quad	.LBB0_143
	.quad	.LBB0_143
	.quad	.LBB0_143
	.quad	.LBB0_143
	.quad	.LBB0_143
	.quad	.LBB0_143
	.quad	.LBB0_143
	.quad	.LBB0_143
	.quad	.LBB0_143
	.quad	.LBB0_143
	.quad	.LBB0_143
	.quad	.LBB0_143
	.quad	.LBB0_143
	.quad	.LBB0_143
	.quad	.LBB0_143
	.quad	.LBB0_143
	.quad	.LBB0_143
	.quad	.LBB0_143
	.quad	.LBB0_143
	.quad	.LBB0_143
	.quad	.LBB0_143
	.quad	.LBB0_143
	.quad	.LBB0_143
	.quad	.LBB0_143
	.quad	.LBB0_143
	.quad	.LBB0_143
	.quad	.LBB0_143
	.quad	.LBB0_143
	.quad	.LBB0_143
	.quad	.LBB0_143
	.quad	.LBB0_285
.LJTI0_2:
	.quad	.LBB0_293
	.quad	.LBB0_290
	.quad	.LBB0_293
	.quad	.LBB0_296

	.type	.L.str,@object          # @.str
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.asciz	"missing binary operator"
	.size	.L.str, 24

	.type	op_to_prio,@object      # @op_to_prio
	.section	.rodata,"a",@progbits
	.align	16
op_to_prio:
	.short	0                       # 0x0
	.short	8962                    # 0x2302
	.short	6656                    # 0x1a00
	.short	6656                    # 0x1a00
	.short	8962                    # 0x2302
	.short	8962                    # 0x2302
	.short	8192                    # 0x2000
	.short	8192                    # 0x2000
	.short	8192                    # 0x2000
	.short	5120                    # 0x1400
	.short	4096                    # 0x1000
	.short	4608                    # 0x1200
	.short	7168                    # 0x1c00
	.short	7168                    # 0x1c00
	.short	5632                    # 0x1600
	.short	5632                    # 0x1600
	.short	8962                    # 0x2302
	.short	3592                    # 0xe08
	.short	3080                    # 0xc08
	.short	2312                    # 0x908
	.short	2568                    # 0xa08
	.short	1536                    # 0x600
	.short	1026                    # 0x402
	.short	512                     # 0x200
	.short	6144                    # 0x1800
	.short	6144                    # 0x1800
	.short	6656                    # 0x1a00
	.short	6656                    # 0x1a00
	.size	op_to_prio, 56

	.type	.L.str1,@object         # @.str1
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str1:
	.asciz	"void expression between '(' and ')'"
	.size	.L.str1, 36

	.type	.L.str2,@object         # @.str2
.L.str2:
	.asciz	"operator '%s' has no right operand"
	.size	.L.str2, 35

	.type	.L.str3,@object         # @.str3
.L.str3:
	.asciz	"impossible operator '%s'"
	.size	.L.str3, 25

	.type	.L.str4,@object         # @.str4
.L.str4:
	.asciz	"traditional C rejects the unary plus operator"
	.size	.L.str4, 46

	.type	.L.str5,@object         # @.str5
.L.str5:
	.asciz	"division by zero in #if"
	.size	.L.str5, 24

	.type	.L.str6,@object         # @.str6
.L.str6:
	.asciz	"comma operator in operand of #if"
	.size	.L.str6, 33

	.type	.L.str7,@object         # @.str7
.L.str7:
	.asciz	"syntax error '?' without following ':'"
	.size	.L.str7, 39

	.type	.L.str8,@object         # @.str8
.L.str8:
	.asciz	"syntax error ':' without preceding '?'"
	.size	.L.str8, 39

	.type	.L.str9,@object         # @.str9
.L.str9:
	.asciz	"missing ')' in expression"
	.size	.L.str9, 26

	.type	.L.str10,@object        # @.str10
.L.str10:
	.asciz	"missing '(' in expression"
	.size	.L.str10, 26

	.type	.L.str11,@object        # @.str11
.L.str11:
	.asciz	"missing binary operator before '%s'"
	.size	.L.str11, 36

	.type	.L.str12,@object        # @.str12
.L.str12:
	.asciz	"operator '%s' has no left operand"
	.size	.L.str12, 34

	.type	.L.str13,@object        # @.str13
.L.str13:
	.asciz	"unbalanced stack in #if"
	.size	.L.str13, 24

	.type	.L.str14,@object        # @.str14
.L.str14:
	.asciz	"#if with no expression"
	.size	.L.str14, 23

	.type	.L.str15,@object        # @.str15
.L.str15:
	.asciz	"integer overflow in preprocessor expression"
	.size	.L.str15, 44

	.type	.L.str16,@object        # @.str16
.L.str16:
	.asciz	"string constants are not valid in #if"
	.size	.L.str16, 38

	.type	.L.str17,@object        # @.str17
.L.str17:
	.asciz	"invalid character '%c' in #if"
	.size	.L.str17, 30

	.type	.L.str18,@object        # @.str18
.L.str18:
	.asciz	"invalid character '\\%03o' in #if"
	.size	.L.str18, 33

	.type	.L.str19,@object        # @.str19
.L.str19:
	.asciz	"__bool_true_false_are_defined"
	.size	.L.str19, 30

	.type	.L.str20,@object        # @.str20
.L.str20:
	.asciz	"ISO C++ does not permit \"%s\" in #if"
	.size	.L.str20, 36

	.type	.L.str21,@object        # @.str21
.L.str21:
	.asciz	"\"%s\" is not defined"
	.size	.L.str21, 20

	.type	.L.str22,@object        # @.str22
.L.str22:
	.asciz	"\"%s\" is not valid in #if expressions"
	.size	.L.str22, 37

	.type	.L.str23,@object        # @.str23
.L.str23:
	.asciz	"missing ')' after \"defined\""
	.size	.L.str23, 28

	.type	.L.str24,@object        # @.str24
.L.str24:
	.asciz	"operator \"defined\" requires an identifier"
	.size	.L.str24, 42

	.type	.L.str25,@object        # @.str25
.L.str25:
	.asciz	"(\"%s\" is an alternative token for \"%s\" in C++)"
	.size	.L.str25, 47

	.type	.L.str26,@object        # @.str26
.L.str26:
	.asciz	"this use of \"defined\" may not be portable"
	.size	.L.str26, 42

	.type	.L.str27,@object        # @.str27
.L.str27:
	.asciz	"floating point numbers are not valid in #if"
	.size	.L.str27, 44

	.type	vsuf_1,@object          # @vsuf_1
	.section	.rodata,"a",@progbits
	.align	16
vsuf_1:
	.asciz	"u\000\000"
	.byte	1                       # 0x1
	.byte	0                       # 0x0
	.asciz	"U\000\000"
	.byte	1                       # 0x1
	.byte	0                       # 0x0
	.asciz	"l\000\000"
	.byte	0                       # 0x0
	.byte	1                       # 0x1
	.asciz	"L\000\000"
	.byte	0                       # 0x0
	.byte	1                       # 0x1
	.size	vsuf_1, 24

	.type	vsuf_2,@object          # @vsuf_2
	.align	16
vsuf_2:
	.asciz	"ul\000"
	.byte	1                       # 0x1
	.byte	1                       # 0x1
	.asciz	"UL\000"
	.byte	1                       # 0x1
	.byte	1                       # 0x1
	.asciz	"uL\000"
	.byte	1                       # 0x1
	.byte	1                       # 0x1
	.asciz	"Ul\000"
	.byte	1                       # 0x1
	.byte	1                       # 0x1
	.asciz	"lu\000"
	.byte	1                       # 0x1
	.byte	1                       # 0x1
	.asciz	"LU\000"
	.byte	1                       # 0x1
	.byte	1                       # 0x1
	.asciz	"Lu\000"
	.byte	1                       # 0x1
	.byte	1                       # 0x1
	.asciz	"lU\000"
	.byte	1                       # 0x1
	.byte	1                       # 0x1
	.asciz	"ll\000"
	.byte	0                       # 0x0
	.byte	2                       # 0x2
	.asciz	"LL\000"
	.byte	0                       # 0x0
	.byte	2                       # 0x2
	.size	vsuf_2, 60

	.type	vsuf_3,@object          # @vsuf_3
	.align	16
vsuf_3:
	.asciz	"ull"
	.byte	1                       # 0x1
	.byte	2                       # 0x2
	.asciz	"ULL"
	.byte	1                       # 0x1
	.byte	2                       # 0x2
	.asciz	"uLL"
	.byte	1                       # 0x1
	.byte	2                       # 0x2
	.asciz	"Ull"
	.byte	1                       # 0x1
	.byte	2                       # 0x2
	.asciz	"llu"
	.byte	1                       # 0x1
	.byte	2                       # 0x2
	.asciz	"LLU"
	.byte	1                       # 0x1
	.byte	2                       # 0x2
	.asciz	"LLu"
	.byte	1                       # 0x1
	.byte	2                       # 0x2
	.asciz	"llU"
	.byte	1                       # 0x1
	.byte	2                       # 0x2
	.size	vsuf_3, 48

	.type	.L.str28,@object        # @.str28
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str28:
	.asciz	"traditional C rejects the `U' suffix"
	.size	.L.str28, 37

	.type	.L.str29,@object        # @.str29
.L.str29:
	.asciz	"too many 'l' suffixes in integer constant"
	.size	.L.str29, 42

	.type	.L.str30,@object        # @.str30
.L.str30:
	.asciz	"integer constant contains digits beyond the radix"
	.size	.L.str30, 50

	.type	.L.str31,@object        # @.str31
.L.str31:
	.asciz	"integer constant out of range"
	.size	.L.str31, 30

	.type	.L.str32,@object        # @.str32
.L.str32:
	.asciz	"integer constant is so large that it is unsigned"
	.size	.L.str32, 49

	.type	.L.str33,@object        # @.str33
.L.str33:
	.asciz	"invalid suffix '%.*s' on integer constant"
	.size	.L.str33, 42


	.ident	"clang version 3.5 (trunk)"
	.section	".note.GNU-stack","",@progbits
