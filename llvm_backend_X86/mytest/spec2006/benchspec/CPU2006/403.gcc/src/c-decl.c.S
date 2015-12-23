	.file	"c-decl.c.bc"
	.text
	.globl	c_decode_option
	.align	16, 0x90
	.type	c_decode_option,@function
c_decode_option:                        # @c_decode_option
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp5:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp6:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp7:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp8:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp9:
	.cfi_def_cfa_offset 48
.Ltmp10:
	.cfi_offset %rbx, -40
.Ltmp11:
	.cfi_offset %r12, -32
.Ltmp12:
	.cfi_offset %r14, -24
.Ltmp13:
	.cfi_offset %r15, -16
	movq	%rsi, %rax
	movq	(%rax), %r15
	movq	parse_in(%rip), %rdx
	movq	%rdi, %rsi
	xorq	%rcx, %rcx
	movq	%rdx, %rdi
	movq	%rax, %rdx
	callq	cpp_handle_option
	movq	%rax, %r14
	movabsq	$.L.str, %rsi
	movq	%r15, %rdi
	callq	strcmp
	movabsq	$4294967295, %r12       # imm = 0xFFFFFFFF
	testq	%r12, %rax
	je	.LBB0_2
# BB#1:                                 # %lor.lhs.false
	movabsq	$.L.str1, %rsi
	movq	%r15, %rdi
	callq	strcmp
	testq	%r12, %rax
	je	.LBB0_2
# BB#3:                                 # %if.else
	movabsq	$.L.str3, %rsi
	movq	%r15, %rdi
	callq	strcmp
	testq	%r12, %rax
	je	.LBB0_4
# BB#5:                                 # %if.else7
	movabsq	$.L.str4, %rsi
	movq	%r15, %rdi
	callq	strcmp
	testq	%r12, %rax
	je	.LBB0_7
# BB#6:                                 # %lor.lhs.false10
	movabsq	$.L.str5, %rsi
	movq	%r15, %rdi
	callq	strcmp
	testq	%r12, %rax
	je	.LBB0_7
# BB#8:                                 # %if.else14
	movabsq	$.L.str6, %rsi
	movq	%r15, %rdi
	callq	strcmp
	testq	%r12, %rax
	je	.LBB0_10
# BB#9:                                 # %lor.lhs.false17
	movabsq	$.L.str7, %rsi
	movq	%r15, %rdi
	callq	strcmp
	testq	%r12, %rax
	je	.LBB0_10
# BB#12:                                # %if.else22
	movabsq	$.L.str8, %rsi
	movq	%r15, %rdi
	callq	strcmp
	testq	%r12, %rax
	je	.LBB0_14
# BB#13:                                # %lor.lhs.false25
	movabsq	$.L.str9, %rsi
	movq	%r15, %rdi
	callq	strcmp
	testq	%r12, %rax
	je	.LBB0_14
# BB#15:                                # %if.else29
	movabsq	$.L.str10, %rsi
	movq	$5, %rdx
	movq	%r15, %rdi
	callq	strncmp
	testq	%r12, %rax
	je	.LBB0_16
# BB#32:                                # %if.else74
	movabsq	$.L.str22, %rsi
	movq	%r15, %rdi
	callq	strcmp
	testq	%r12, %rax
	je	.LBB0_33
# BB#34:                                # %if.else78
	movabsq	$.L.str23, %rsi
	movq	%r15, %rdi
	callq	strcmp
	testq	%r12, %rax
	je	.LBB0_35
# BB#36:                                # %if.else82
	movabsq	$.L.str24, %rsi
	movq	%r15, %rdi
	callq	strcmp
	testq	%r12, %rax
	je	.LBB0_37
# BB#38:                                # %if.else86
	movabsq	$.L.str25, %rsi
	movq	%r15, %rdi
	callq	strcmp
	testq	%r12, %rax
	je	.LBB0_39
# BB#40:                                # %if.else90
	movabsq	$.L.str26, %rsi
	movq	%r15, %rdi
	callq	strcmp
	testq	%r12, %rax
	je	.LBB0_39
# BB#41:                                # %if.else94
	movabsq	$.L.str27, %rsi
	movq	%r15, %rdi
	callq	strcmp
	testq	%r12, %rax
	je	.LBB0_37
# BB#42:                                # %if.else98
	movabsq	$.L.str28, %rsi
	movq	%r15, %rdi
	callq	strcmp
	testq	%r12, %rax
	je	.LBB0_44
# BB#43:                                # %lor.lhs.false101
	movabsq	$.L.str29, %rsi
	movq	%r15, %rdi
	callq	strcmp
	testq	%r12, %rax
	je	.LBB0_44
# BB#45:                                # %if.else105
	movabsq	$.L.str30, %rsi
	movq	%r15, %rdi
	callq	strcmp
	testq	%r12, %rax
	je	.LBB0_47
# BB#46:                                # %lor.lhs.false108
	movabsq	$.L.str31, %rsi
	movq	%r15, %rdi
	callq	strcmp
	testq	%r12, %rax
	je	.LBB0_47
# BB#48:                                # %if.else112
	movabsq	$.L.str32, %rax
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	strcmp
	testq	%r12, %rax
	je	.LBB0_49
# BB#50:                                # %if.else116
	movabsq	$.L.str33, %rax
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	strcmp
	testq	%r12, %rax
	je	.LBB0_51
# BB#52:                                # %if.else120
	movabsq	$.L.str34, %rax
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	strcmp
	testq	%r12, %rax
	je	.LBB0_53
# BB#54:                                # %if.else124
	movabsq	$.L.str35, %rax
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	strcmp
	testq	%r12, %rax
	je	.LBB0_55
# BB#56:                                # %if.else128
	movabsq	$.L.str36, %rax
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	strcmp
	testq	%r12, %rax
	je	.LBB0_57
# BB#58:                                # %if.else132
	movabsq	$.L.str37, %rax
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	strcmp
	testq	%r12, %rax
	je	.LBB0_59
# BB#60:                                # %if.else136
	movabsq	$.L.str38, %rax
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	strcmp
	testq	%r12, %rax
	je	.LBB0_61
# BB#62:                                # %if.else140
	movabsq	$.L.str39, %rax
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	strcmp
	testq	%r12, %rax
	je	.LBB0_63
# BB#64:                                # %if.else144
	movabsq	$.L.str40, %rax
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	strcmp
	testq	%r12, %rax
	je	.LBB0_65
# BB#66:                                # %if.else148
	movabsq	$.L.str41, %rax
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	strcmp
	testq	%r12, %rax
	je	.LBB0_67
# BB#68:                                # %if.else152
	movabsq	$.L.str42, %rax
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	strcmp
	testq	%r12, %rax
	je	.LBB0_69
# BB#70:                                # %if.else156
	movabsq	$.L.str43, %rax
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	strcmp
	testq	%r12, %rax
	je	.LBB0_71
# BB#72:                                # %if.else160
	movabsq	$.L.str44, %rax
	movq	$13, %rdx
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	strncmp
	testq	%r12, %rax
	je	.LBB0_73
# BB#74:                                # %if.else164
	movzbl	(%r15), %eax
	cmpq	$45, %rax
	jne	.LBB0_77
# BB#75:                                # %land.lhs.true
	movzbl	1(%r15), %eax
	cmpq	$102, %rax
	jne	.LBB0_77
# BB#76:                                # %land.lhs.true172
	leaq	2(%r15), %rax
	movq	%rax, %rdi
	callq	dump_switch_p
	testq	%r12, %rax
	movq	$1, %rbx
	jne	.LBB0_228
.LBB0_77:                               # %if.else177
	movabsq	$.L.str45, %rax
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	strcmp
	xorq	%rbx, %rbx
	testq	%r12, %rax
	je	.LBB0_18
# BB#78:                                # %if.else181
	movabsq	$.L.str46, %rax
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	strcmp
	testq	%r12, %rax
	je	.LBB0_79
# BB#81:                                # %if.else185
	movabsq	$.L.str47, %rax
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	strcmp
	testq	%r12, %rax
	je	.LBB0_82
# BB#83:                                # %if.else189
	movabsq	$.L.str48, %rax
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	strcmp
	testq	%r12, %rax
	je	.LBB0_84
# BB#85:                                # %if.else193
	movabsq	$.L.str49, %rax
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	strcmp
	testq	%r12, %rax
	je	.LBB0_86
# BB#87:                                # %if.else197
	movabsq	$.L.str50, %rax
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	strcmp
	testq	%r12, %rax
	je	.LBB0_88
# BB#89:                                # %if.else201
	movabsq	$.L.str51, %rax
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	strcmp
	testq	%r12, %rax
	je	.LBB0_90
# BB#91:                                # %if.else209
	movabsq	$.L.str52, %rax
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	strcmp
	testq	%r12, %rax
	je	.LBB0_92
# BB#93:                                # %if.else213
	movabsq	$.L.str53, %rax
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	strcmp
	testq	%r12, %rax
	je	.LBB0_94
# BB#95:                                # %if.else217
	movabsq	$.L.str54, %rax
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	strcmp
	testq	%r12, %rax
	je	.LBB0_96
# BB#97:                                # %if.else221
	movabsq	$.L.str55, %rax
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	strcmp
	testq	%r12, %rax
	je	.LBB0_98
# BB#99:                                # %if.else225
	movabsq	$.L.str56, %rax
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	strcmp
	testq	%r12, %rax
	je	.LBB0_100
# BB#101:                               # %if.else229
	movabsq	$.L.str57, %rax
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	strcmp
	testq	%r12, %rax
	je	.LBB0_102
# BB#103:                               # %if.else233
	movabsq	$.L.str58, %rax
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	strcmp
	testq	%r12, %rax
	je	.LBB0_104
# BB#105:                               # %if.else237
	movabsq	$.L.str59, %rax
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	strcmp
	testq	%r12, %rax
	je	.LBB0_106
# BB#107:                               # %if.else241
	movabsq	$.L.str60, %rax
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	strcmp
	testq	%r12, %rax
	je	.LBB0_108
# BB#109:                               # %if.else245
	movabsq	$.L.str61, %rax
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	strcmp
	testq	%r12, %rax
	je	.LBB0_110
# BB#111:                               # %if.else249
	movabsq	$.L.str62, %rax
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	strcmp
	testq	%r12, %rax
	je	.LBB0_112
# BB#113:                               # %if.else253
	movabsq	$.L.str63, %rax
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	strcmp
	testq	%r12, %rax
	je	.LBB0_114
# BB#115:                               # %if.else257
	movabsq	$.L.str64, %rax
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	strcmp
	testq	%r12, %rax
	je	.LBB0_116
# BB#117:                               # %if.else261
	movabsq	$.L.str65, %rax
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	strcmp
	testq	%r12, %rax
	je	.LBB0_118
# BB#119:                               # %if.else265
	movabsq	$.L.str66, %rax
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	strcmp
	testq	%r12, %rax
	je	.LBB0_120
# BB#121:                               # %if.else269
	movabsq	$.L.str67, %rax
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	strcmp
	testq	%r12, %rax
	je	.LBB0_122
# BB#123:                               # %if.else273
	movabsq	$.L.str68, %rax
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	strcmp
	testq	%r12, %rax
	je	.LBB0_124
# BB#125:                               # %if.else277
	movabsq	$.L.str69, %rax
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	strcmp
	testq	%r12, %rax
	je	.LBB0_126
# BB#127:                               # %if.else281
	movabsq	$.L.str70, %rax
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	strcmp
	testq	%r12, %rax
	je	.LBB0_128
# BB#129:                               # %if.else285
	movabsq	$.L.str71, %rax
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	strcmp
	testq	%r12, %rax
	je	.LBB0_130
# BB#131:                               # %if.else289
	movabsq	$.L.str72, %rax
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	strcmp
	testq	%r12, %rax
	je	.LBB0_132
# BB#133:                               # %if.else293
	movabsq	$.L.str73, %rax
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	strcmp
	testq	%r12, %rax
	je	.LBB0_134
# BB#135:                               # %if.else297
	movabsq	$.L.str74, %rax
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	strcmp
	testq	%r12, %rax
	je	.LBB0_136
# BB#137:                               # %if.else301
	movabsq	$.L.str75, %rax
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	strcmp
	testq	%r12, %rax
	je	.LBB0_138
# BB#139:                               # %if.else305
	movabsq	$.L.str76, %rax
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	strcmp
	testq	%r12, %rax
	je	.LBB0_140
# BB#141:                               # %if.else309
	movabsq	$.L.str77, %rax
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	strcmp
	testq	%r12, %rax
	je	.LBB0_142
# BB#143:                               # %if.else313
	movabsq	$.L.str78, %rax
	movq	$9, %rdx
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	strncmp
	testq	%r12, %rax
	je	.LBB0_144
# BB#145:                               # %if.else319
	movabsq	$.L.str79, %rax
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	strcmp
	testq	%r12, %rax
	je	.LBB0_146
# BB#147:                               # %if.else323
	movabsq	$.L.str80, %rax
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	strcmp
	testq	%r12, %rax
	je	.LBB0_148
# BB#150:                               # %if.else327
	movabsq	$.L.str81, %rax
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	strcmp
	testq	%r12, %rax
	je	.LBB0_151
# BB#152:                               # %if.else331
	movabsq	$.L.str82, %rax
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	strcmp
	testq	%r12, %rax
	je	.LBB0_153
# BB#154:                               # %if.else335
	movabsq	$.L.str83, %rax
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	strcmp
	testq	%r12, %rax
	je	.LBB0_155
# BB#156:                               # %if.else339
	movabsq	$.L.str84, %rax
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	strcmp
	testq	%r12, %rax
	je	.LBB0_157
# BB#158:                               # %if.else343
	movabsq	$.L.str85, %rax
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	strcmp
	testq	%r12, %rax
	je	.LBB0_159
# BB#160:                               # %if.else347
	movabsq	$.L.str86, %rax
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	strcmp
	testq	%r12, %rax
	je	.LBB0_161
# BB#162:                               # %if.else351
	movabsq	$.L.str87, %rax
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	strcmp
	testq	%r12, %rax
	je	.LBB0_163
# BB#164:                               # %if.else355
	movabsq	$.L.str88, %rax
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	strcmp
	testq	%r12, %rax
	je	.LBB0_165
# BB#166:                               # %if.else359
	movabsq	$.L.str89, %rax
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	strcmp
	testq	%r12, %rax
	je	.LBB0_167
# BB#168:                               # %if.else363
	movabsq	$.L.str90, %rax
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	strcmp
	testq	%r12, %rax
	je	.LBB0_169
# BB#170:                               # %if.else367
	movabsq	$.L.str91, %rax
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	strcmp
	testq	%r12, %rax
	je	.LBB0_171
# BB#172:                               # %if.else371
	movabsq	$.L.str92, %rax
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	strcmp
	testq	%r12, %rax
	je	.LBB0_173
# BB#174:                               # %if.else375
	movabsq	$.L.str93, %rax
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	strcmp
	testq	%r12, %rax
	je	.LBB0_175
# BB#176:                               # %if.else379
	movabsq	$.L.str94, %rax
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	strcmp
	testq	%r12, %rax
	je	.LBB0_177
# BB#178:                               # %if.else383
	movabsq	$.L.str95, %rax
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	strcmp
	testq	%r12, %rax
	je	.LBB0_179
# BB#180:                               # %if.else387
	movabsq	$.L.str96, %rax
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	strcmp
	testq	%r12, %rax
	je	.LBB0_181
# BB#182:                               # %if.else391
	movabsq	$.L.str97, %rax
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	strcmp
	testq	%r12, %rax
	je	.LBB0_183
# BB#184:                               # %if.else395
	movabsq	$.L.str98, %rax
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	strcmp
	testq	%r12, %rax
	je	.LBB0_185
# BB#186:                               # %if.else399
	movabsq	$.L.str99, %rax
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	strcmp
	testq	%r12, %rax
	movq	$1, %rbx
	je	.LBB0_228
# BB#187:                               # %if.else403
	movabsq	$.L.str100, %rax
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	strcmp
	testq	%r12, %rax
	je	.LBB0_228
# BB#188:                               # %if.else407
	movabsq	$.L.str101, %rax
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	strcmp
	testq	%r12, %rax
	je	.LBB0_228
# BB#189:                               # %if.else411
	movabsq	$.L.str102, %rax
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	strcmp
	testq	%r12, %rax
	je	.LBB0_228
# BB#190:                               # %if.else415
	movabsq	$.L.str103, %rax
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	strcmp
	testq	%r12, %rax
	je	.LBB0_228
# BB#191:                               # %if.else419
	movabsq	$.L.str104, %rax
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	strcmp
	testq	%r12, %rax
	je	.LBB0_228
# BB#192:                               # %if.else423
	movabsq	$.L.str105, %rax
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	strcmp
	testq	%r12, %rax
	je	.LBB0_228
# BB#193:                               # %if.else427
	movabsq	$.L.str106, %rax
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	strcmp
	testq	%r12, %rax
	je	.LBB0_228
# BB#194:                               # %if.else431
	movabsq	$.L.str107, %rax
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	strcmp
	testq	%r12, %rax
	je	.LBB0_228
# BB#195:                               # %if.else435
	movabsq	$.L.str108, %rax
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	strcmp
	testq	%r12, %rax
	je	.LBB0_228
# BB#196:                               # %if.else439
	movabsq	$.L.str109, %rax
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	strcmp
	testq	%r12, %rax
	je	.LBB0_197
# BB#198:                               # %if.else443
	movabsq	$.L.str110, %rax
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	strcmp
	testq	%r12, %rax
	je	.LBB0_199
# BB#200:                               # %if.else447
	movabsq	$.L.str111, %rax
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	strcmp
	testq	%r12, %rax
	je	.LBB0_201
# BB#202:                               # %if.else451
	movabsq	$.L.str112, %rax
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	strcmp
	testq	%r12, %rax
	je	.LBB0_203
# BB#204:                               # %if.else455
	movabsq	$.L.str113, %rax
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	strcmp
	testq	%r12, %rax
	je	.LBB0_205
# BB#206:                               # %if.else459
	movabsq	$.L.str114, %rax
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	strcmp
	testq	%r12, %rax
	je	.LBB0_207
# BB#208:                               # %if.else463
	movabsq	$.L.str115, %rax
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	strcmp
	testq	%r12, %rax
	je	.LBB0_209
# BB#210:                               # %if.else467
	movabsq	$.L.str116, %rax
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	strcmp
	testq	%r12, %rax
	je	.LBB0_211
# BB#212:                               # %if.else471
	movabsq	$.L.str117, %rax
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	strcmp
	testq	%r12, %rax
	je	.LBB0_213
# BB#214:                               # %if.else475
	movabsq	$.L.str118, %rax
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	strcmp
	testq	%r12, %rax
	je	.LBB0_215
# BB#216:                               # %if.else479
	movabsq	$.L.str119, %rax
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	strcmp
	testq	%r12, %rax
	je	.LBB0_217
# BB#218:                               # %if.else483
	movabsq	$.L.str120, %rax
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	strcmp
	testq	%r12, %rax
	je	.LBB0_219
# BB#220:                               # %if.else487
	movabsq	$.L.str121, %rax
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	strcmp
	testq	%r12, %rax
	je	.LBB0_221
# BB#222:                               # %if.else491
	movabsq	$.L.str122, %rax
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	strcmp
	testq	%r12, %rax
	je	.LBB0_223
# BB#224:                               # %if.else495
	movabsq	$.L.str123, %rax
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	strcmp
	testq	%r12, %rax
	movq	%r14, %rbx
	jne	.LBB0_228
# BB#225:                               # %if.then498
	movl	warn_uninitialized(%rip), %eax
	cmpq	$1, %rax
	je	.LBB0_227
# BB#226:                               # %if.then501
	movq	$2, %rax
	movl	%eax, warn_uninitialized(%rip)
.LBB0_227:                              # %if.end502
	movq	$1, %rbx
	movl	%ebx, warn_implicit_int(%rip)
	movl	%ebx, mesg_implicit_function_declaration(%rip)
	movl	%ebx, warn_return_type(%rip)
	movq	$1, %rdi
	callq	set_Wunused
	movl	%ebx, warn_switch(%rip)
	movq	$1, %rdi
	callq	set_Wformat
	movl	%ebx, warn_char_subscripts(%rip)
	movl	%ebx, warn_parentheses(%rip)
	movl	%ebx, warn_sequence_point(%rip)
	movl	%ebx, warn_missing_braces(%rip)
	movq	$2, %rax
	movl	%eax, warn_main(%rip)
	movl	%ebx, warn_unknown_pragmas(%rip)
	jmp	.LBB0_228
.LBB0_2:                                # %if.then
	movabsq	$.L.str2, %rdi
	xorq	%rax, %rax
	callq	warning
	movq	$1, %rbx
	movl	%ebx, flag_traditional(%rip)
	movl	%ebx, flag_writable_strings(%rip)
	jmp	.LBB0_228
.LBB0_7:                                # %if.then13
	movq	$1, %rbx
	movl	%ebx, flag_hosted(%rip)
	xorq	%rax, %rax
	movl	%eax, flag_no_builtin(%rip)
	jmp	.LBB0_228
.LBB0_4:                                # %if.then6
	movq	$1, %rbx
	movl	%ebx, flag_allow_single_precision(%rip)
.LBB0_228:                              # %return
	movq	%rbx, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.LBB0_10:                               # %if.then20
	xorq	%rax, %rax
	movl	warn_main(%rip), %ecx
	movl	%eax, flag_hosted(%rip)
	movq	$1, %rbx
	movl	%ebx, flag_no_builtin(%rip)
	cmpq	$2, %rcx
	jne	.LBB0_228
# BB#11:                                # %if.then21
	movl	%eax, warn_main(%rip)
	jmp	.LBB0_228
.LBB0_14:                               # %if.then28
	xorq	%rax, %rax
	movl	%eax, flag_traditional(%rip)
	movl	%eax, flag_writable_strings(%rip)
	movq	$1, %rbx
	jmp	.LBB0_228
.LBB0_16:                               # %if.then32
	addq	$5, %r15
	movabsq	$.L.str11, %rsi
	movq	%r15, %rdi
	callq	strcmp
	xorq	%rbx, %rbx
	testq	%r12, %rax
	je	.LBB0_18
# BB#17:                                # %lor.lhs.false36
	movabsq	$.L.str12, %rsi
	movq	%r15, %rdi
	callq	strcmp
	testq	%r12, %rax
	je	.LBB0_18
# BB#19:                                # %if.else40
	movabsq	$.L.str13, %rsi
	movq	%r15, %rdi
	callq	strcmp
	testq	%r12, %rax
	movq	$1, %rbx
	je	.LBB0_18
# BB#20:                                # %if.else44
	movabsq	$.L.str14, %rsi
	movq	%r15, %rdi
	callq	strcmp
	testq	%r12, %rax
	je	.LBB0_24
# BB#21:                                # %lor.lhs.false47
	movabsq	$.L.str15, %rsi
	movq	%r15, %rdi
	callq	strcmp
	testq	%r12, %rax
	je	.LBB0_24
# BB#22:                                # %lor.lhs.false50
	movabsq	$.L.str16, %rsi
	movq	%r15, %rdi
	callq	strcmp
	testq	%r12, %rax
	je	.LBB0_24
# BB#23:                                # %lor.lhs.false53
	movabsq	$.L.str17, %rsi
	movq	%r15, %rdi
	callq	strcmp
	testq	%r12, %rax
	je	.LBB0_24
# BB#26:                                # %if.else57
	movabsq	$.L.str18, %rsi
	movq	%r15, %rdi
	callq	strcmp
	testq	%r12, %rax
	je	.LBB0_27
# BB#28:                                # %if.else61
	movabsq	$.L.str19, %rsi
	movq	%r15, %rdi
	callq	strcmp
	testq	%r12, %rax
	je	.LBB0_30
# BB#29:                                # %lor.lhs.false64
	movabsq	$.L.str20, %rax
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	strcmp
	testq	%r12, %rax
	je	.LBB0_30
# BB#31:                                # %if.else68
	movabsq	$.L.str21, %rcx
	xorq	%rax, %rax
	movq	%rcx, %rdi
	movq	%r15, %rsi
	callq	error
	movq	$1, %rbx
	jmp	.LBB0_228
.LBB0_18:                               # %iso_1994
	movl	%ebx, flag_isoc94(%rip)
	xorq	%rax, %rax
	movl	%eax, flag_traditional(%rip)
	movl	%eax, flag_writable_strings(%rip)
	movq	$1, %rbx
	movl	%ebx, flag_no_asm(%rip)
	movl	%ebx, flag_no_nonansi_builtin(%rip)
	movl	%eax, flag_noniso_default_format_attributes(%rip)
	movl	%eax, flag_isoc99(%rip)
	jmp	.LBB0_228
.LBB0_33:                               # %if.then77
	movq	$1, %rbx
	movl	%ebx, dollars_in_ident(%rip)
	jmp	.LBB0_228
.LBB0_35:                               # %if.then81
	xorq	%rax, %rax
	movl	%eax, dollars_in_ident(%rip)
	movq	$1, %rbx
	jmp	.LBB0_228
.LBB0_37:                               # %if.then85
	movq	$1, %rbx
	movl	%ebx, flag_signed_char(%rip)
	jmp	.LBB0_228
.LBB0_39:                               # %if.then89
	xorq	%rax, %rax
	movl	%eax, flag_signed_char(%rip)
	movq	$1, %rbx
	jmp	.LBB0_228
.LBB0_24:                               # %if.then56
	xorq	%rax, %rax
	movl	%eax, flag_traditional(%rip)
	movl	%eax, flag_writable_strings(%rip)
	movq	$1, %rbx
	movl	%ebx, flag_no_asm(%rip)
	movl	%ebx, flag_no_nonansi_builtin(%rip)
	movl	%eax, flag_noniso_default_format_attributes(%rip)
.LBB0_25:                               # %return
	movl	%ebx, flag_isoc99(%rip)
	movl	%ebx, flag_isoc94(%rip)
	jmp	.LBB0_228
.LBB0_44:                               # %if.then104
	movq	$1, %rbx
	movl	%ebx, flag_signed_bitfields(%rip)
	movl	%ebx, explicit_flag_signed_bitfields(%rip)
	jmp	.LBB0_228
.LBB0_47:                               # %if.then111
	xorq	%rax, %rax
	movl	%eax, flag_signed_bitfields(%rip)
	movq	$1, %rbx
	movl	%ebx, explicit_flag_signed_bitfields(%rip)
	jmp	.LBB0_228
.LBB0_27:                               # %if.then60
	xorq	%rax, %rax
	movl	%eax, flag_traditional(%rip)
	movl	%eax, flag_writable_strings(%rip)
	movl	%eax, flag_no_asm(%rip)
	movl	%eax, flag_no_nonansi_builtin(%rip)
	movq	$1, %rbx
	movl	%ebx, flag_noniso_default_format_attributes(%rip)
	movl	%eax, flag_isoc99(%rip)
	movl	%eax, flag_isoc94(%rip)
	jmp	.LBB0_228
.LBB0_49:                               # %if.then115
	movq	$1, %rbx
	movl	%ebx, flag_short_enums(%rip)
	jmp	.LBB0_228
.LBB0_51:                               # %if.then119
	xorq	%rax, %rax
	movl	%eax, flag_short_enums(%rip)
	movq	$1, %rbx
	jmp	.LBB0_228
.LBB0_53:                               # %if.then123
	movq	$1, %rbx
	movl	%ebx, flag_short_wchar(%rip)
	jmp	.LBB0_228
.LBB0_55:                               # %if.then127
	xorq	%rax, %rax
	movl	%eax, flag_short_wchar(%rip)
	movq	$1, %rbx
	jmp	.LBB0_228
.LBB0_57:                               # %if.then131
	movq	$1, %rbx
	movl	%ebx, flag_cond_mismatch(%rip)
	jmp	.LBB0_228
.LBB0_59:                               # %if.then135
	xorq	%rax, %rax
	movl	%eax, flag_cond_mismatch(%rip)
	movq	$1, %rbx
	jmp	.LBB0_228
.LBB0_61:                               # %if.then139
	movq	$1, %rbx
	movl	%ebx, flag_short_double(%rip)
	jmp	.LBB0_228
.LBB0_63:                               # %if.then143
	xorq	%rax, %rax
	movl	%eax, flag_short_double(%rip)
	movq	$1, %rbx
	jmp	.LBB0_228
.LBB0_65:                               # %if.then147
	xorq	%rax, %rax
	movl	%eax, flag_no_asm(%rip)
	movq	$1, %rbx
	jmp	.LBB0_228
.LBB0_67:                               # %if.then151
	movq	$1, %rbx
	movl	%ebx, flag_no_asm(%rip)
	jmp	.LBB0_228
.LBB0_69:                               # %if.then155
	xorq	%rax, %rax
	movl	%eax, flag_no_builtin(%rip)
	movq	$1, %rbx
	jmp	.LBB0_228
.LBB0_71:                               # %if.then159
	movq	$1, %rbx
	movl	%ebx, flag_no_builtin(%rip)
	jmp	.LBB0_228
.LBB0_73:                               # %if.then163
	addq	$13, %r15
	movq	%r15, %rdi
	callq	disable_builtin_function
	movq	$1, %rbx
	jmp	.LBB0_228
.LBB0_79:                               # %if.then184
	movq	$2, %rax
	jmp	.LBB0_80
.LBB0_84:                               # %if.then192
	xorq	%rax, %rax
	jmp	.LBB0_80
.LBB0_86:                               # %if.then196
	movq	$1, %rbx
	movl	%ebx, warn_implicit_int(%rip)
	jmp	.LBB0_228
.LBB0_88:                               # %if.then200
	xorq	%rax, %rax
	movl	%eax, warn_implicit_int(%rip)
	movq	$1, %rbx
	jmp	.LBB0_228
.LBB0_90:                               # %if.then204
	movl	mesg_implicit_function_declaration(%rip), %eax
	movq	$1, %rbx
	movl	%ebx, warn_implicit_int(%rip)
	cmpq	$2, %rax
	je	.LBB0_228
.LBB0_82:                               # %if.then188
	movq	$1, %rbx
	movl	%ebx, mesg_implicit_function_declaration(%rip)
	jmp	.LBB0_228
.LBB0_92:                               # %if.then212
	xorq	%rax, %rax
	movl	%eax, warn_implicit_int(%rip)
.LBB0_80:                               # %return
	movl	%eax, mesg_implicit_function_declaration(%rip)
	movq	$1, %rbx
	jmp	.LBB0_228
.LBB0_94:                               # %if.then216
	movq	$1, %rbx
	movl	%ebx, warn_long_long(%rip)
	jmp	.LBB0_228
.LBB0_96:                               # %if.then220
	xorq	%rax, %rax
	movl	%eax, warn_long_long(%rip)
	movq	$1, %rbx
	jmp	.LBB0_228
.LBB0_98:                               # %if.then224
	movq	$1, %rbx
	movl	%ebx, flag_const_strings(%rip)
	jmp	.LBB0_228
.LBB0_100:                              # %if.then228
	xorq	%rax, %rax
	movl	%eax, flag_const_strings(%rip)
	movq	$1, %rbx
	jmp	.LBB0_228
.LBB0_102:                              # %if.then232
	movq	$1, %rbx
	movl	%ebx, warn_cast_qual(%rip)
	jmp	.LBB0_228
.LBB0_104:                              # %if.then236
	xorq	%rax, %rax
	movl	%eax, warn_cast_qual(%rip)
	movq	$1, %rbx
	jmp	.LBB0_228
.LBB0_106:                              # %if.then240
	movq	$1, %rbx
	movl	%ebx, warn_bad_function_cast(%rip)
	jmp	.LBB0_228
.LBB0_108:                              # %if.then244
	xorq	%rax, %rax
	movl	%eax, warn_bad_function_cast(%rip)
	movq	$1, %rbx
	jmp	.LBB0_228
.LBB0_110:                              # %if.then248
	xorq	%rax, %rax
	movl	%eax, warn_missing_noreturn(%rip)
	movq	$1, %rbx
	jmp	.LBB0_228
.LBB0_112:                              # %if.then252
	movq	$1, %rbx
	movl	%ebx, warn_missing_format_attribute(%rip)
	jmp	.LBB0_228
.LBB0_114:                              # %if.then256
	xorq	%rax, %rax
	movl	%eax, warn_missing_format_attribute(%rip)
	movq	$1, %rbx
	jmp	.LBB0_228
.LBB0_116:                              # %if.then260
	movq	$1, %rbx
	movl	%ebx, warn_pointer_arith(%rip)
	jmp	.LBB0_228
.LBB0_118:                              # %if.then264
	xorq	%rax, %rax
	movl	%eax, warn_pointer_arith(%rip)
	movq	$1, %rbx
	jmp	.LBB0_228
.LBB0_120:                              # %if.then268
	movq	$1, %rbx
	movl	%ebx, warn_strict_prototypes(%rip)
	jmp	.LBB0_228
.LBB0_122:                              # %if.then272
	xorq	%rax, %rax
	movl	%eax, warn_strict_prototypes(%rip)
	movq	$1, %rbx
	jmp	.LBB0_228
.LBB0_124:                              # %if.then276
	movq	$1, %rbx
	movl	%ebx, warn_missing_prototypes(%rip)
	jmp	.LBB0_228
.LBB0_126:                              # %if.then280
	xorq	%rax, %rax
	movl	%eax, warn_missing_prototypes(%rip)
	movq	$1, %rbx
	jmp	.LBB0_228
.LBB0_128:                              # %if.then284
	movq	$1, %rbx
	movl	%ebx, warn_missing_declarations(%rip)
	jmp	.LBB0_228
.LBB0_130:                              # %if.then288
	xorq	%rax, %rax
	movl	%eax, warn_missing_declarations(%rip)
	movq	$1, %rbx
	jmp	.LBB0_228
.LBB0_132:                              # %if.then292
	movq	$1, %rbx
	movl	%ebx, warn_redundant_decls(%rip)
	jmp	.LBB0_228
.LBB0_134:                              # %if.then296
	xorq	%rax, %rax
	movl	%eax, warn_redundant_decls(%rip)
	movq	$1, %rbx
	jmp	.LBB0_228
.LBB0_136:                              # %if.then300
	movq	$1, %rbx
	movl	%ebx, warn_nested_externs(%rip)
	jmp	.LBB0_228
.LBB0_138:                              # %if.then304
	xorq	%rax, %rax
	movl	%eax, warn_nested_externs(%rip)
	movq	$1, %rbx
	jmp	.LBB0_228
.LBB0_140:                              # %if.then308
	movq	$1, %rbx
	movl	%ebx, warn_traditional(%rip)
	jmp	.LBB0_228
.LBB0_142:                              # %if.then312
	xorq	%rax, %rax
	movl	%eax, warn_traditional(%rip)
	movq	$1, %rbx
	jmp	.LBB0_228
.LBB0_144:                              # %if.then316
	addq	$9, %r15
	movq	%r15, %rdi
	callq	atoi
	movq	%rax, %rdi
	jmp	.LBB0_149
.LBB0_146:                              # %if.then322
	movq	$1, %rdi
	movq	$1, %rbx
	callq	set_Wformat
	jmp	.LBB0_228
.LBB0_148:                              # %if.then326
	xorq	%rdi, %rdi
.LBB0_149:                              # %return
	callq	set_Wformat
	movq	$1, %rbx
	jmp	.LBB0_228
.LBB0_151:                              # %if.then330
	movq	$1, %rbx
	movl	%ebx, warn_format_y2k(%rip)
	jmp	.LBB0_228
.LBB0_153:                              # %if.then334
	xorq	%rax, %rax
	movl	%eax, warn_format_y2k(%rip)
	movq	$1, %rbx
	jmp	.LBB0_228
.LBB0_155:                              # %if.then338
	movq	$1, %rbx
	movl	%ebx, warn_format_extra_args(%rip)
	jmp	.LBB0_228
.LBB0_157:                              # %if.then342
	xorq	%rax, %rax
	movl	%eax, warn_format_extra_args(%rip)
	movq	$1, %rbx
	jmp	.LBB0_228
.LBB0_159:                              # %if.then346
	movq	$1, %rbx
	movl	%ebx, warn_format_nonliteral(%rip)
	jmp	.LBB0_228
.LBB0_161:                              # %if.then350
	xorq	%rax, %rax
	movl	%eax, warn_format_nonliteral(%rip)
	movq	$1, %rbx
	jmp	.LBB0_228
.LBB0_163:                              # %if.then354
	movq	$1, %rbx
	movl	%ebx, warn_format_security(%rip)
	jmp	.LBB0_228
.LBB0_165:                              # %if.then358
	xorq	%rax, %rax
	movl	%eax, warn_format_security(%rip)
	movq	$1, %rbx
	jmp	.LBB0_228
.LBB0_167:                              # %if.then362
	movq	$1, %rbx
	movl	%ebx, warn_char_subscripts(%rip)
	jmp	.LBB0_228
.LBB0_169:                              # %if.then366
	xorq	%rax, %rax
	movl	%eax, warn_char_subscripts(%rip)
	movq	$1, %rbx
	jmp	.LBB0_228
.LBB0_171:                              # %if.then370
	movq	$1, %rbx
	movl	%ebx, warn_conversion(%rip)
	jmp	.LBB0_228
.LBB0_173:                              # %if.then374
	xorq	%rax, %rax
	movl	%eax, warn_conversion(%rip)
	movq	$1, %rbx
	jmp	.LBB0_228
.LBB0_175:                              # %if.then378
	movq	$1, %rbx
	movl	%ebx, warn_parentheses(%rip)
	jmp	.LBB0_228
.LBB0_177:                              # %if.then382
	xorq	%rax, %rax
	movl	%eax, warn_parentheses(%rip)
	movq	$1, %rbx
	jmp	.LBB0_228
.LBB0_179:                              # %if.then386
	movq	$1, %rbx
	movl	%ebx, warn_return_type(%rip)
	jmp	.LBB0_228
.LBB0_181:                              # %if.then390
	xorq	%rax, %rax
	movl	%eax, warn_return_type(%rip)
	movq	$1, %rbx
	jmp	.LBB0_228
.LBB0_183:                              # %if.then394
	movq	$1, %rbx
	movl	%ebx, warn_sequence_point(%rip)
	jmp	.LBB0_228
.LBB0_185:                              # %if.then398
	xorq	%rax, %rax
	movl	%eax, warn_sequence_point(%rip)
	movq	$1, %rbx
	jmp	.LBB0_228
.LBB0_197:                              # %if.then442
	movq	$1, %rbx
	movl	%ebx, warn_missing_braces(%rip)
	jmp	.LBB0_228
.LBB0_199:                              # %if.then446
	xorq	%rax, %rax
	movl	%eax, warn_missing_braces(%rip)
	jmp	.LBB0_228
.LBB0_201:                              # %if.then450
	movq	$1, %rbx
	movl	%ebx, warn_main(%rip)
	jmp	.LBB0_228
.LBB0_203:                              # %if.then454
	movl	%r12d, warn_main(%rip)
	jmp	.LBB0_228
.LBB0_205:                              # %if.then458
	movq	$1, %rbx
	movl	%ebx, warn_sign_compare(%rip)
	jmp	.LBB0_228
.LBB0_207:                              # %if.then462
	xorq	%rax, %rax
	movl	%eax, warn_sign_compare(%rip)
	jmp	.LBB0_228
.LBB0_209:                              # %if.then466
	movq	$1, %rbx
	movl	%ebx, warn_float_equal(%rip)
	jmp	.LBB0_228
.LBB0_211:                              # %if.then470
	xorq	%rax, %rax
	movl	%eax, warn_float_equal(%rip)
	jmp	.LBB0_228
.LBB0_213:                              # %if.then474
	movq	$1, %rbx
	movl	%ebx, warn_multichar(%rip)
	jmp	.LBB0_228
.LBB0_215:                              # %if.then478
	xorq	%rax, %rax
	movl	%eax, warn_multichar(%rip)
	jmp	.LBB0_228
.LBB0_217:                              # %if.then482
	movq	$1, %rbx
	movl	%ebx, warn_div_by_zero(%rip)
	jmp	.LBB0_228
.LBB0_219:                              # %if.then486
	xorq	%rax, %rax
	movl	%eax, warn_div_by_zero(%rip)
	jmp	.LBB0_228
.LBB0_221:                              # %if.then490
	movq	$2, %rax
	movl	%eax, warn_unknown_pragmas(%rip)
	jmp	.LBB0_228
.LBB0_223:                              # %if.then494
	xorq	%rax, %rax
	movl	%eax, warn_unknown_pragmas(%rip)
	jmp	.LBB0_228
.LBB0_30:                               # %if.then67
	xorq	%rax, %rax
	movl	%eax, flag_traditional(%rip)
	movl	%eax, flag_writable_strings(%rip)
	movl	%eax, flag_no_asm(%rip)
	movl	%eax, flag_no_nonansi_builtin(%rip)
	movq	$1, %rbx
	movl	%ebx, flag_noniso_default_format_attributes(%rip)
	jmp	.LBB0_25
.Ltmp14:
	.size	c_decode_option, .Ltmp14-c_decode_option
	.cfi_endproc

	.globl	c_print_identifier
	.align	16, 0x90
	.type	c_print_identifier,@function
c_print_identifier:                     # @c_print_identifier
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp19:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp20:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp21:
	.cfi_def_cfa_offset 32
.Ltmp22:
	.cfi_offset %rbx, -32
.Ltmp23:
	.cfi_offset %r14, -24
.Ltmp24:
	.cfi_offset %r15, -16
	movq	%rsi, %r15
	movq	56(%r15), %rax
	movq	%rdx, %rbx
	movq	%rdi, %r14
	addq	$4, %rbx
	movabsq	$.L.str124, %rsi
                                        # kill: RDI<def> R14<kill>
	movq	%rax, %rdx
	movq	%rbx, %rcx
	callq	print_node
	movq	64(%r15), %rdx
	movabsq	$.L.str125, %rsi
	movq	%r14, %rdi
	movq	%rbx, %rcx
	callq	print_node
	movq	72(%r15), %rdx
	movabsq	$.L.str126, %rsi
	movq	%r14, %rdi
	movq	%rbx, %rcx
	callq	print_node
	movq	80(%r15), %rdx
	movabsq	$.L.str127, %rsi
	movq	%r14, %rdi
	movq	%rbx, %rcx
	callq	print_node
	movq	88(%r15), %rdx
	movabsq	$.L.str128, %rsi
	movq	%r14, %rdi
	movq	%rbx, %rcx
	callq	print_node
	movq	96(%r15), %rdx
	movabsq	$.L.str129, %rsi
	movq	%r14, %rdi
	movq	%rbx, %rcx
	callq	print_node
	movzbl	19(%r15), %eax
	testq	$1, %rax
	je	.LBB1_1
# BB#2:                                 # %if.then
	movzbl	43(%r15), %eax
	movq	ridpointers(%rip), %rcx
	movq	(%rcx,%rax,8), %r15
	movq	%r14, %rdi
	movq	%rbx, %rsi
	callq	indent_to
	movabsq	$.L.str130, %rdi
	movq	$4, %rsi
	movq	$1, %rdx
	movq	%r14, %rcx
	callq	fwrite
	movabsq	$.L.str131, %rsi
	xorq	%rax, %rax
	movq	%r14, %rdi
	movq	%r15, %rdx
	callq	fprintf
	movq	32(%r15), %rdx
	movabsq	$.L.str132, %rsi
	xorq	%rax, %rax
	movq	%r14, %rdi
	popq	%rbx
	popq	%r14
	popq	%r15
	jmp	fprintf  # TAILCALL
.LBB1_1:                                # %if.end
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp25:
	.size	c_print_identifier, .Ltmp25-c_print_identifier
	.cfi_endproc

	.globl	finish_incomplete_decl
	.align	16, 0x90
	.type	finish_incomplete_decl,@function
finish_incomplete_decl:                 # @finish_incomplete_decl
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp30:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp31:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp32:
	.cfi_def_cfa_offset 32
.Ltmp33:
	.cfi_offset %rbx, -32
.Ltmp34:
	.cfi_offset %r14, -24
.Ltmp35:
	.cfi_offset %r15, -16
	movq	%rdi, %rbx
	movzbl	16(%rbx), %eax
	cmpq	$34, %rax
	jne	.LBB2_9
# BB#1:                                 # %if.then
	movq	8(%rbx), %r14
	movq	global_trees(%rip), %rax
	cmpq	%rax, %r14
	je	.LBB2_9
# BB#2:                                 # %land.lhs.true
	movzbl	16(%r14), %eax
	cmpq	$18, %rax
	jne	.LBB2_9
# BB#3:                                 # %land.lhs.true9
	movzbl	49(%rbx), %eax
	testq	$1, %rax
	jne	.LBB2_9
# BB#4:                                 # %land.lhs.true13
	cmpq	$0, 24(%r14)
	je	.LBB2_5
.LBB2_9:                                # %if.end17
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.LBB2_5:                                # %if.then16
	movabsq	$.L.str133, %rsi
	xorq	%rax, %rax
	movq	%rbx, %rdi
	callq	warning_with_decl
	xorq	%rdi, %rdi
	xorq	%rsi, %rsi
	callq	build_int_2_wide
	movq	%rax, %r15
	testq	%r15, %r15
	je	.LBB2_8
# BB#6:                                 # %if.then43.i
	movq	%r15, %rdi
	callq	build_index_type
	movq	%rax, 24(%r14)
	cmpq	$0, 8(%r15)
	jne	.LBB2_8
# BB#7:                                 # %if.then49.i
	movq	%rax, 8(%r15)
.LBB2_8:                                # %complete_array_type.exit
	movq	%r14, %rdi
	callq	layout_type
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	popq	%rbx
	popq	%r14
	popq	%r15
	jmp	layout_decl  # TAILCALL
.Ltmp36:
	.size	finish_incomplete_decl, .Ltmp36-finish_incomplete_decl
	.cfi_endproc

	.globl	complete_array_type
	.align	16, 0x90
	.type	complete_array_type,@function
complete_array_type:                    # @complete_array_type
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp42:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp43:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp44:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp45:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp46:
	.cfi_def_cfa_offset 48
.Ltmp47:
	.cfi_offset %rbx, -40
.Ltmp48:
	.cfi_offset %r12, -32
.Ltmp49:
	.cfi_offset %r14, -24
.Ltmp50:
	.cfi_offset %r15, -16
	movq	%rdx, %r14
	movq	%rsi, %rbx
	movq	%rdi, %r15
	testq	%rbx, %rbx
	je	.LBB3_16
# BB#1:                                 # %if.then
	movzbl	16(%rbx), %eax
	cmpq	$46, %rax
	jne	.LBB3_2
# BB#4:                                 # %if.then14
	movq	40(%rbx), %rbx
	movq	$-1, %rdi
	movq	$-1, %rsi
	callq	build_int_2_wide
	movq	%rax, %rdx
	jmp	.LBB3_5
	.align	16, 0x90
.LBB3_8:                                # %for.inc
                                        #   in Loop: Header=BB3_5 Depth=1
	movq	%rax, %rdx
	movq	(%rbx), %rbx
.LBB3_5:                                # %if.then14
                                        # =>This Inner Loop Header: Depth=1
	testq	%rbx, %rbx
	je	.LBB3_9
# BB#6:                                 # %for.body
                                        #   in Loop: Header=BB3_5 Depth=1
	movq	24(%rbx), %rax
	testq	%rax, %rax
	jne	.LBB3_8
# BB#7:                                 # %if.else21
                                        #   in Loop: Header=BB3_5 Depth=1
	movq	integer_types+40(%rip), %rsi
	movq	global_trees+96(%rip), %rcx
	movq	$59, %rdi
	xorq	%rax, %rax
	callq	build
	movq	%rax, %rdi
	callq	fold
	jmp	.LBB3_8
.LBB3_2:                                # %if.then
	cmpq	$29, %rax
	jne	.LBB3_10
# BB#3:                                 # %if.then1
	movq	8(%rbx), %rax
	movq	8(%rax), %rdi
	callq	int_size_in_bytes
	movq	%rax, %rsi
	movq	$32, %rcx
	shlq	%cl, %rsi
	movslq	32(%rbx), %rax
	movq	$32, %rcx
	sarq	%cl, %rsi
	cqto
	idivq	%rsi
	leaq	-1(%rax), %rdi
	movq	$32, %rcx
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	xorq	%rsi, %rsi
	xorq	%r12, %r12
	jmp	.LBB3_14
.LBB3_9:                                # %for.end
	movq	%rdx, %rdi
	callq	copy_node
	movq	%rax, %rbx
	xorq	%r12, %r12
	jmp	.LBB3_15
.LBB3_10:                               # %if.else26
	movq	global_trees(%rip), %rax
	cmpq	%rbx, %rax
	je	.LBB3_11
# BB#12:                                # %if.else26
	movabsq	$1, %r12
	jmp	.LBB3_13
.LBB3_11:
	movabsq	$0, %r12
.LBB3_13:                               # %if.else26
	xorq	%rdi, %rdi
	xorq	%rsi, %rsi
.LBB3_14:                               # %if.end34
	callq	build_int_2_wide
	movq	%rax, %rbx
.LBB3_15:                               # %if.end34
	testq	%rbx, %rbx
	jne	.LBB3_18
.LBB3_16:                               # %if.then36
	movq	$2, %r12
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %r14
	je	.LBB3_20
# BB#17:                                # %if.end41
	xorq	%rdi, %rdi
	xorq	%rsi, %rsi
	callq	build_int_2_wide
	movq	%rax, %rbx
	testq	%rbx, %rbx
	movq	$2, %r12
	je	.LBB3_20
.LBB3_18:                               # %if.then43
	movq	%rbx, %rdi
	callq	build_index_type
	movq	%rax, 24(%r15)
	cmpq	$0, 8(%rbx)
	jne	.LBB3_20
# BB#19:                                # %if.then49
	movq	%rax, 8(%rbx)
.LBB3_20:                               # %if.end55
	movq	%r15, %rdi
	callq	layout_type
	movq	%r12, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.Ltmp51:
	.size	complete_array_type, .Ltmp51-complete_array_type
	.cfi_endproc

	.globl	global_bindings_p
	.align	16, 0x90
	.type	global_bindings_p,@function
global_bindings_p:                      # @global_bindings_p
	.cfi_startproc
# BB#0:                                 # %entry
	movq	current_binding_level(%rip), %rax
	movq	global_binding_level(%rip), %rcx
	cmpq	%rcx, %rax
	jne	.LBB4_1
# BB#2:                                 # %entry
	movabsq	$1, %rax
	retq
.LBB4_1:
	movabsq	$0, %rax
	retq
.Ltmp52:
	.size	global_bindings_p, .Ltmp52-global_bindings_p
	.cfi_endproc

	.globl	keep_next_level
	.align	16, 0x90
	.type	keep_next_level,@function
keep_next_level:                        # @keep_next_level
	.cfi_startproc
# BB#0:                                 # %entry
	movq	$1, %rax
	movb	%al, keep_next_level_flag(%rip)
	retq
.Ltmp53:
	.size	keep_next_level, .Ltmp53-keep_next_level
	.cfi_endproc

	.globl	kept_level_p
	.align	16, 0x90
	.type	kept_level_p,@function
kept_level_p:                           # @kept_level_p
	.cfi_startproc
# BB#0:                                 # %entry
	movq	current_binding_level(%rip), %rcx
	movzbl	52(%rcx), %eax
	testq	%rax, %rax
	je	.LBB6_2
# BB#1:                                 # %land.lhs.true
	cmpq	$0, 24(%rcx)
	movq	$1, %rax
	jne	.LBB6_8
.LBB6_2:                                # %lor.lhs.false
	movzbl	51(%rcx), %eax
	testq	%rax, %rax
	movq	$1, %rax
	jne	.LBB6_8
# BB#3:                                 # %lor.lhs.false4
	cmpq	$0, (%rcx)
	jne	.LBB6_8
# BB#4:                                 # %lor.rhs
	xorq	%rax, %rax
	cmpq	$0, 8(%rcx)
	je	.LBB6_8
# BB#5:                                 # %land.rhs
	movzbl	49(%rcx), %eax
	testq	%rax, %rax
	jne	.LBB6_6
# BB#7:                                 # %land.rhs
	movabsq	$1, %rax
.LBB6_8:                                # %lor.end
	retq
.LBB6_6:
	movabsq	$0, %rax
	retq
.Ltmp54:
	.size	kept_level_p, .Ltmp54-kept_level_p
	.cfi_endproc

	.globl	declare_parm_level
	.align	16, 0x90
	.type	declare_parm_level,@function
declare_parm_level:                     # @declare_parm_level
	.cfi_startproc
# BB#0:                                 # %entry
	movq	current_binding_level(%rip), %rax
	movq	$1, %rcx
	movb	%cl, 48(%rax)
	retq
.Ltmp55:
	.size	declare_parm_level, .Ltmp55-declare_parm_level
	.cfi_endproc

	.globl	in_parm_level_p
	.align	16, 0x90
	.type	in_parm_level_p,@function
in_parm_level_p:                        # @in_parm_level_p
	.cfi_startproc
# BB#0:                                 # %entry
	movq	current_binding_level(%rip), %rax
	movsbq	48(%rax), %rax
	retq
.Ltmp56:
	.size	in_parm_level_p, .Ltmp56-in_parm_level_p
	.cfi_endproc

	.globl	pushlevel
	.align	16, 0x90
	.type	pushlevel,@function
pushlevel:                              # @pushlevel
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp59:
	.cfi_def_cfa_offset 16
.Ltmp60:
	.cfi_offset %rbx, -16
	movq	current_binding_level(%rip), %rax
	movq	global_binding_level(%rip), %rcx
	movq	%rdi, %rbx
	cmpq	%rcx, %rax
	jne	.LBB9_2
# BB#1:                                 # %if.then
	movq	$0, named_labels(%rip)
.LBB9_2:                                # %if.end
	movq	free_binding_level(%rip), %rax
	testq	%rax, %rax
	je	.LBB9_4
# BB#3:                                 # %if.then1
	movq	40(%rax), %rcx
	movq	%rcx, free_binding_level(%rip)
	jmp	.LBB9_5
.LBB9_4:                                # %if.else
	movq	$72, %rdi
	callq	xmalloc
.LBB9_5:                                # %if.end2
	movq	$0, 64(%rax)
	movq	$0, 56(%rax)
	movq	$0, 48(%rax)
	movq	$0, 40(%rax)
	movq	$0, 32(%rax)
	movq	$0, 24(%rax)
	movq	$0, 16(%rax)
	movq	$0, 8(%rax)
	movq	$0, (%rax)
	movq	current_binding_level(%rip), %rcx
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	testq	%rdx, %rbx
	movq	$1, %rdx
	jne	.LBB9_10
# BB#6:                                 # %lor.rhs
	xorq	%rsi, %rsi
	testq	%rcx, %rcx
	movabsq	$0, %rdx
	je	.LBB9_11
# BB#7:                                 # %cond.true
	movzbl	50(%rcx), %edx
	testq	%rdx, %rdx
	je	.LBB9_8
# BB#9:                                 # %cond.true
	movabsq	$1, %rdx
	jmp	.LBB9_10
.LBB9_8:
	movabsq	$0, %rdx
.LBB9_10:                               # %cond.true
	movq	%rcx, %rsi
.LBB9_11:                               # %lor.end
	movb	%dl, 49(%rax)
	movq	%rsi, 40(%rax)
	movzbl	keep_next_level_flag(%rip), %ecx
	movq	%rax, current_binding_level(%rip)
	movb	%cl, 51(%rax)
	movzbl	keep_next_if_subblocks(%rip), %ecx
	xorq	%rdx, %rdx
	movb	%dl, keep_next_level_flag(%rip)
	movb	%cl, 52(%rax)
	movb	%dl, keep_next_if_subblocks(%rip)
	popq	%rbx
	retq
.Ltmp61:
	.size	pushlevel, .Ltmp61-pushlevel
	.cfi_endproc

	.globl	poplevel
	.align	16, 0x90
	.type	poplevel,@function
poplevel:                               # @poplevel
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
	subq	$32, %rsp
.Ltmp73:
	.cfi_def_cfa_offset 80
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
	movq	current_binding_level(%rip), %rax
	movq	(%rax), %rbx
	movq	8(%rax), %r13
	movq	24(%rax), %r15
	movsbq	51(%rax), %rax
	movq	%rax, 16(%rsp)          # 8-byte Spill
	movq	%rdx, 24(%rsp)          # 8-byte Spill
	movq	%rdi, %r12
	movabsq	$4294967295, %r14       # imm = 0xFFFFFFFF
	testq	%r14, %rsi
	je	.LBB10_2
# BB#1:                                 # %if.then
	movq	%rbx, %rdi
	callq	nreverse
	movq	current_binding_level(%rip), %rcx
	movq	%rax, %rbx
	movq	%rbx, (%rcx)
.LBB10_2:                               # %if.end
	testq	%rbx, %rbx
	je	.LBB10_11
# BB#3:
	movq	%rbx, %rax
	.align	16, 0x90
.LBB10_4:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movl	16(%rax), %ecx
	movq	%rcx, %rdx
	andq	$16639, %rdx            # imm = 0x40FF
	cmpq	$30, %rdx
	jne	.LBB10_10
# BB#5:                                 # %land.lhs.true11
                                        #   in Loop: Header=BB10_4 Depth=1
	cmpq	$0, 104(%rax)
	je	.LBB10_10
# BB#6:                                 # %land.lhs.true11
                                        #   in Loop: Header=BB10_4 Depth=1
	andq	$1024, %rcx             # imm = 0x400
	testq	%r14, %rcx
	je	.LBB10_10
# BB#7:                                 # %if.then21
                                        #   in Loop: Header=BB10_4 Depth=1
	movq	112(%rax), %rcx
	testq	%rcx, %rcx
	je	.LBB10_10
# BB#8:                                 # %if.then21
                                        #   in Loop: Header=BB10_4 Depth=1
	cmpq	%rax, %rcx
	je	.LBB10_10
# BB#9:                                 # %if.then30
                                        #   in Loop: Header=BB10_4 Depth=1
	movl	16(%rcx), %edx
	orq	$1024, %rdx             # imm = 0x400
	movl	%edx, 16(%rcx)
	.align	16, 0x90
.LBB10_10:                              # %for.inc
                                        #   in Loop: Header=BB10_4 Depth=1
	movq	(%rax), %rax
	testq	%rax, %rax
	jne	.LBB10_4
.LBB10_11:                              # %for.end
	movq	current_binding_level(%rip), %rax
	movq	(%rax), %rdi
	callq	warn_about_unused_variables
	movq	current_binding_level(%rip), %rcx
	movq	32(%rcx), %rax
	movq	%rax, (%rsp)            # 8-byte Spill
	testq	%rax, %rax
	jne	.LBB10_17
# BB#12:                                # %if.else46
	movq	24(%rsp), %rax          # 8-byte Reload
	orq	%rax, %r12
	movq	16(%rsp), %rax          # 8-byte Reload
	orq	%rax, %r12
	testq	%r14, %r12
	jne	.LBB10_16
# BB#13:                                # %lor.lhs.false49
	movzbl	52(%rcx), %eax
	xorq	%rcx, %rcx
	testq	%rax, %rax
	je	.LBB10_14
# BB#15:                                # %lor.lhs.false49
	testq	%r15, %r15
	movabsq	$0, %r12
	je	.LBB10_18
.LBB10_16:                              # %if.end58
	movq	$4, %rdi
	callq	make_node
	xorq	%rcx, %rcx
	testq	%rax, %rax
	movabsq	$0, %r12
	je	.LBB10_18
.LBB10_17:                              # %if.then61
	movq	%rbx, 32(%rax)
	movq	%r15, 40(%rax)
	movq	$1, %rcx
	movq	%rax, %r12
.LBB10_18:                              # %for.cond66.preheader
	testq	%r15, %r15
	je	.LBB10_21
# BB#19:
	movq	%r15, %rax
	.align	16, 0x90
.LBB10_20:                              # %for.body68
                                        # =>This Inner Loop Header: Depth=1
	movq	%r12, 48(%rax)
	movq	(%rax), %rax
	testq	%rax, %rax
	jne	.LBB10_20
.LBB10_21:                              # %for.cond74.preheader
	movq	%r15, 8(%rsp)           # 8-byte Spill
	movq	%rcx, 16(%rsp)          # 8-byte Spill
	jmp	.LBB10_22
	.align	16, 0x90
.LBB10_32:                              # %for.inc123
                                        #   in Loop: Header=BB10_22 Depth=1
	movq	(%rbx), %rbx
.LBB10_22:                              # %for.cond74.preheader
                                        # =>This Inner Loop Header: Depth=1
	testq	%rbx, %rbx
	je	.LBB10_33
# BB#23:                                # %for.body76
                                        #   in Loop: Header=BB10_22 Depth=1
	movq	72(%rbx), %rax
	testq	%rax, %rax
	je	.LBB10_32
# BB#24:                                # %if.then80
                                        #   in Loop: Header=BB10_22 Depth=1
	movzbl	49(%rbx), %ecx
	testq	$1, %rcx
	je	.LBB10_31
# BB#25:                                # %if.then86
                                        #   in Loop: Header=BB10_22 Depth=1
	movl	16(%rbx), %ecx
	testq	$65536, %rcx            # imm = 0x10000
	je	.LBB10_27
# BB#26:                                # %if.then92
                                        #   in Loop: Header=BB10_22 Depth=1
	movl	16(%rax), %ecx
	orq	$65536, %rcx            # imm = 0x10000
	movl	%ecx, 16(%rax)
	movl	16(%rbx), %ecx
.LBB10_27:                              # %if.end100
                                        #   in Loop: Header=BB10_22 Depth=1
	testq	$1024, %rcx             # imm = 0x400
	je	.LBB10_31
# BB#28:                                # %if.then107
                                        #   in Loop: Header=BB10_22 Depth=1
	movq	120(%rbx), %rax
	testq	%rax, %rax
	jne	.LBB10_30
# BB#29:                                # %cond.false
                                        #   in Loop: Header=BB10_22 Depth=1
	movq	%rbx, %rdi
	callq	*lang_set_decl_assembler_name(%rip)
	movq	120(%rbx), %rax
.LBB10_30:                              # %cond.end
                                        #   in Loop: Header=BB10_22 Depth=1
	movl	16(%rax), %ecx
	orq	$1024, %rcx             # imm = 0x400
	movl	%ecx, 16(%rax)
.LBB10_31:                              # %if.end119
                                        #   in Loop: Header=BB10_22 Depth=1
	movq	72(%rbx), %rax
	movq	$0, 64(%rax)
	jmp	.LBB10_32
.LBB10_33:                              # %for.end126
	movq	current_binding_level(%rip), %rax
	movq	16(%rax), %rax
	jmp	.LBB10_35
	.align	16, 0x90
.LBB10_34:                              # %for.body129
                                        #   in Loop: Header=BB10_35 Depth=1
	movq	24(%rax), %rcx
	movq	32(%rax), %rdx
	movq	%rdx, 64(%rcx)
	movq	(%rax), %rax
.LBB10_35:                              # %for.body129
                                        # =>This Inner Loop Header: Depth=1
	testq	%rax, %rax
	jne	.LBB10_34
# BB#36:                                # %for.end135
	movq	24(%rsp), %rax          # 8-byte Reload
	testq	%r14, %rax
	je	.LBB10_46
# BB#37:                                # %if.then137
	movq	%r12, %rdi
	callq	clear_limbo_values
	movq	$0, 32(%r12)
	movq	named_labels(%rip), %r14
	testq	%r14, %r14
	je	.LBB10_45
# BB#38:
	movabsq	$.L.str134, %r15
	.align	16, 0x90
.LBB10_39:                              # %for.body142
                                        # =>This Inner Loop Header: Depth=1
	movq	32(%r14), %rbx
	cmpq	$0, 104(%rbx)
	jne	.LBB10_41
# BB#40:                                # %if.then149
                                        #   in Loop: Header=BB10_39 Depth=1
	xorq	%rax, %rax
	movq	%rbx, %rdi
	movq	%r15, %rsi
	callq	error_with_decl
	movq	input_filename(%rip), %rdi
	movq	72(%rbx), %rdx
	movl	lineno(%rip), %esi
	callq	define_label
	jmp	.LBB10_44
	.align	16, 0x90
.LBB10_41:                              # %if.else153
                                        #   in Loop: Header=BB10_39 Depth=1
	movl	warn_unused_label(%rip), %eax
	testq	%rax, %rax
	je	.LBB10_44
# BB#42:                                # %land.lhs.true155
                                        #   in Loop: Header=BB10_39 Depth=1
	movzbl	18(%rbx), %eax
	testq	$1, %rax
	jne	.LBB10_44
# BB#43:                                # %if.then162
                                        #   in Loop: Header=BB10_39 Depth=1
	xorq	%rax, %rax
	movq	%rbx, %rdi
	movabsq	$.L.str135, %rsi
	callq	warning_with_decl
	.align	16, 0x90
.LBB10_44:                              # %if.end164
                                        #   in Loop: Header=BB10_39 Depth=1
	movq	72(%rbx), %rax
	movq	$0, 72(%rax)
	movq	32(%r12), %rax
	movq	%rax, (%rbx)
	movq	%rbx, 32(%r12)
	movq	(%r14), %r14
	testq	%r14, %r14
	jne	.LBB10_39
.LBB10_45:                              # %if.end200.thread
	movq	current_binding_level(%rip), %rax
	movq	40(%rax), %rcx
	movq	free_binding_level(%rip), %rdx
	movq	%rcx, current_binding_level(%rip)
	movq	%rdx, 40(%rax)
	movq	current_function_decl(%rip), %rcx
	movq	%rax, free_binding_level(%rip)
	movq	%r12, 104(%rcx)
	movq	16(%rsp), %rdx          # 8-byte Reload
	jmp	.LBB10_53
.LBB10_46:                              # %if.else183
	movq	%rax, %rbx
	movq	current_binding_level(%rip), %rax
	movq	40(%rax), %rcx
	movq	free_binding_level(%rip), %rdx
	movq	%rcx, current_binding_level(%rip)
	movq	%rdx, 40(%rax)
	movq	%rax, free_binding_level(%rip)
	movq	16(%rsp), %rdx          # 8-byte Reload
	testq	%rdx, %rdx
	je	.LBB10_49
# BB#47:                                # %if.then185
	cmpq	$0, (%rsp)              # 8-byte Folded Reload
	jne	.LBB10_56
# BB#48:                                # %if.then187
	movq	current_binding_level(%rip), %rax
	movq	24(%rax), %rdi
	movq	%r12, %rsi
	jmp	.LBB10_51
.LBB10_49:                              # %if.else192
	movq	8(%rsp), %rsi           # 8-byte Reload
	testq	%rsi, %rsi
	je	.LBB10_52
# BB#50:                                # %if.then194
	movq	current_binding_level(%rip), %rax
	movq	24(%rax), %rdi
.LBB10_51:                              # %if.end200
	movq	%rdx, %r15
	callq	chainon
	movq	%r15, %rdx
	movq	current_binding_level(%rip), %rcx
	movq	%rax, 24(%rcx)
.LBB10_52:                              # %if.end200
	testq	%r14, %rbx
	jne	.LBB10_53
# BB#55:                                # %if.else212
	testq	%rdx, %rdx
	je	.LBB10_60
.LBB10_56:                              # %for.cond215.preheader
	testq	%r13, %r13
	je	.LBB10_59
	.align	16, 0x90
.LBB10_57:                              # %for.body217
                                        # =>This Inner Loop Header: Depth=1
	movq	32(%r13), %rax
	movq	%r12, 144(%rax)
	movq	(%r13), %r13
	testq	%r13, %r13
	jne	.LBB10_57
	jmp	.LBB10_58
	.align	16, 0x90
.LBB10_54:                              # %for.body205
                                        #   in Loop: Header=BB10_53 Depth=1
	movq	current_function_decl(%rip), %rax
	movq	32(%r13), %rcx
	movq	%rax, 144(%rcx)
	movq	(%r13), %r13
.LBB10_53:                              # %for.cond203.preheader
                                        # =>This Inner Loop Header: Depth=1
	testq	%r13, %r13
	jne	.LBB10_54
.LBB10_58:                              # %if.end227
	testq	%rdx, %rdx
	je	.LBB10_60
.LBB10_59:                              # %if.then229
	movl	16(%r12), %eax
	orq	$65536, %rax            # imm = 0x10000
	movl	%eax, 16(%r12)
.LBB10_60:                              # %if.end235
	movq	%r12, %rax
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB10_14:
	xorq	%r12, %r12
	jmp	.LBB10_18
.Ltmp79:
	.size	poplevel, .Ltmp79-poplevel
	.cfi_endproc

	.globl	getdecls
	.align	16, 0x90
	.type	getdecls,@function
getdecls:                               # @getdecls
	.cfi_startproc
# BB#0:                                 # %entry
	movq	current_binding_level(%rip), %rax
	movq	(%rax), %rax
	retq
.Ltmp80:
	.size	getdecls, .Ltmp80-getdecls
	.cfi_endproc

	.align	16, 0x90
	.type	clear_limbo_values,@function
clear_limbo_values:                     # @clear_limbo_values
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp83:
	.cfi_def_cfa_offset 16
.Ltmp84:
	.cfi_offset %rbx, -16
	movq	32(%rdi), %rax
	jmp	.LBB12_1
	.align	16, 0x90
.LBB12_4:                               # %for.inc
                                        #   in Loop: Header=BB12_1 Depth=1
	movq	(%rax), %rax
.LBB12_1:                               # %entry
                                        # =>This Inner Loop Header: Depth=1
	testq	%rax, %rax
	je	.LBB12_5
# BB#2:                                 # %for.body
                                        #   in Loop: Header=BB12_1 Depth=1
	movq	72(%rax), %rcx
	testq	%rcx, %rcx
	je	.LBB12_4
# BB#3:                                 # %if.then
                                        #   in Loop: Header=BB12_1 Depth=1
	movq	$0, 96(%rcx)
	jmp	.LBB12_4
.LBB12_5:                               # %for.end
	movq	40(%rdi), %rbx
	jmp	.LBB12_7
	.align	16, 0x90
.LBB12_6:                               # %for.body7
                                        #   in Loop: Header=BB12_7 Depth=1
	movq	%rbx, %rdi
	callq	clear_limbo_values
	movq	(%rbx), %rbx
.LBB12_7:                               # %for.body7
                                        # =>This Inner Loop Header: Depth=1
	testq	%rbx, %rbx
	jne	.LBB12_6
# BB#8:                                 # %for.end11
	popq	%rbx
	retq
.Ltmp85:
	.size	clear_limbo_values, .Ltmp85-clear_limbo_values
	.cfi_endproc

	.globl	define_label
	.align	16, 0x90
	.type	define_label,@function
define_label:                           # @define_label
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp91:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp92:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp93:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp94:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp95:
	.cfi_def_cfa_offset 48
.Ltmp96:
	.cfi_offset %rbx, -40
.Ltmp97:
	.cfi_offset %r12, -32
.Ltmp98:
	.cfi_offset %r14, -24
.Ltmp99:
	.cfi_offset %r15, -16
	movq	%rdx, %r12
	movq	%rsi, %r14
	movq	%rdi, %r15
	movq	%r12, %rdi
	callq	lookup_label
	xorq	%rbx, %rbx
	testq	%rax, %rax
	je	.LBB13_4
# BB#1:                                 # %land.lhs.true
	movq	80(%rax), %rcx
	movq	current_function_decl(%rip), %rdx
	cmpq	%rdx, %rcx
	je	.LBB13_3
# BB#2:                                 # %if.then
	movq	shadowed_labels(%rip), %rdx
	xorq	%rdi, %rdi
	movq	%rax, %rsi
	callq	tree_cons
	movq	%rax, shadowed_labels(%rip)
	movq	$0, 72(%r12)
	movq	%r12, %rdi
	callq	lookup_label
.LBB13_3:                               # %if.end
	movq	%rax, %rbx
.LBB13_4:                               # %if.end
	movl	warn_traditional(%rip), %eax
	testq	%rax, %rax
	je	.LBB13_10
# BB#5:                                 # %if.end
	movl	in_system_header(%rip), %eax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB13_10
# BB#6:                                 # %land.lhs.true7
	movq	current_binding_level(%rip), %rax
	movq	global_binding_level(%rip), %rcx
	cmpq	%rcx, %rax
	je	.LBB13_8
# BB#7:                                 # %land.lhs.true.i
	cmpq	$0, 64(%r12)
	jne	.LBB13_9
.LBB13_8:                               # %lookup_name.exit
	cmpq	$0, 56(%r12)
	je	.LBB13_10
.LBB13_9:                               # %if.then10
	movq	32(%r12), %rcx
	movabsq	$.L.str153, %rdx
	xorq	%rax, %rax
	movq	%r15, %rdi
	movq	%r14, %rsi
	callq	warning_with_file_and_line
.LBB13_10:                              # %if.end11
	cmpq	$0, 104(%rbx)
	je	.LBB13_12
# BB#11:                                # %if.then14
	movq	32(%r12), %rcx
	movabsq	$.L.str154, %rdx
	xorq	%rax, %rax
	xorq	%rbx, %rbx
	movq	%r15, %rdi
	movq	%r14, %rsi
	callq	error_with_file_and_line
	jmp	.LBB13_13
.LBB13_12:                              # %if.else
	movq	global_trees(%rip), %rax
	movq	%rax, 104(%rbx)
	movq	%r15, 24(%rbx)
	movl	%r14d, 32(%rbx)
.LBB13_13:                              # %return
	movq	%rbx, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.Ltmp100:
	.size	define_label, .Ltmp100-define_label
	.cfi_endproc

	.globl	insert_block
	.align	16, 0x90
	.type	insert_block,@function
insert_block:                           # @insert_block
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rax
.Ltmp102:
	.cfi_def_cfa_offset 16
	movq	%rdi, %rax
	movl	16(%rax), %ecx
	orq	$65536, %rcx            # imm = 0x10000
	movl	%ecx, 16(%rax)
	movq	current_binding_level(%rip), %rcx
	movq	24(%rcx), %rdi
	movq	%rax, %rsi
	callq	chainon
	movq	current_binding_level(%rip), %rcx
	movq	%rax, 24(%rcx)
	popq	%rax
	retq
.Ltmp103:
	.size	insert_block, .Ltmp103-insert_block
	.cfi_endproc

	.globl	set_block
	.align	16, 0x90
	.type	set_block,@function
set_block:                              # @set_block
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp106:
	.cfi_def_cfa_offset 16
.Ltmp107:
	.cfi_offset %rbx, -16
	movq	current_binding_level(%rip), %rax
	movq	%rdi, %rbx
	movq	%rbx, 32(%rax)
	movq	current_binding_level(%rip), %rax
	movq	(%rax), %rdi
	movq	32(%rbx), %rsi
	callq	chainon
	movq	current_binding_level(%rip), %rcx
	movq	%rax, (%rcx)
	movq	current_binding_level(%rip), %rax
	movq	24(%rax), %rdi
	movq	40(%rbx), %rsi
	callq	chainon
	movq	current_binding_level(%rip), %rcx
	movq	%rax, 24(%rcx)
	popq	%rbx
	retq
.Ltmp108:
	.size	set_block, .Ltmp108-set_block
	.cfi_endproc

	.globl	push_label_level
	.align	16, 0x90
	.type	push_label_level,@function
push_label_level:                       # @push_label_level
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rax
.Ltmp110:
	.cfi_def_cfa_offset 16
	movq	free_binding_level(%rip), %rax
	testq	%rax, %rax
	je	.LBB16_2
# BB#1:                                 # %if.then
	movq	40(%rax), %rcx
	movq	%rcx, free_binding_level(%rip)
	jmp	.LBB16_3
.LBB16_2:                               # %if.else
	movq	$72, %rdi
	callq	xmalloc
.LBB16_3:                               # %if.end
	movq	label_level_chain(%rip), %rcx
	movq	%rcx, 40(%rax)
	movq	named_labels(%rip), %rcx
	movq	%rax, label_level_chain(%rip)
	movq	%rcx, (%rax)
	movq	shadowed_labels(%rip), %rcx
	movq	%rcx, 16(%rax)
	movq	$0, named_labels(%rip)
	movq	$0, shadowed_labels(%rip)
	popq	%rax
	retq
.Ltmp111:
	.size	push_label_level, .Ltmp111-push_label_level
	.cfi_endproc

	.globl	pop_label_level
	.align	16, 0x90
	.type	pop_label_level,@function
pop_label_level:                        # @pop_label_level
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp118:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp119:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp120:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp121:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp122:
	.cfi_def_cfa_offset 48
.Ltmp123:
	.cfi_offset %rbx, -48
.Ltmp124:
	.cfi_offset %r12, -40
.Ltmp125:
	.cfi_offset %r13, -32
.Ltmp126:
	.cfi_offset %r14, -24
.Ltmp127:
	.cfi_offset %r15, -16
	movq	named_labels(%rip), %rbx
	movq	label_level_chain(%rip), %r15
	testq	%rbx, %rbx
	je	.LBB17_8
# BB#1:
	xorq	%r14, %r14
	movabsq	$.L.str134, %r12
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
.LBB17_2:                               # %for.body.lr.ph
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB17_14 Depth 2
	testq	%r14, %r14
	jne	.LBB17_14
# BB#3:                                 # %for.body.us
                                        #   in Loop: Header=BB17_2 Depth=1
	movq	32(%rbx), %rdi
	movl	16(%rdi), %eax
	testq	$33554432, %rax         # imm = 0x2000000
	je	.LBB17_24
# BB#4:                                 # %if.then.us
                                        #   in Loop: Header=BB17_2 Depth=1
	movl	32(%rdi), %ecx
	testq	%rcx, %rcx
	jne	.LBB17_5
# BB#21:                                # %if.then4.us
                                        #   in Loop: Header=BB17_2 Depth=1
	xorq	%rax, %rax
	movq	%r12, %rsi
	callq	error_with_decl
	movq	32(%rbx), %rax
	movq	input_filename(%rip), %rdi
	movq	72(%rax), %rdx
	movl	lineno(%rip), %esi
	callq	define_label
	jmp	.LBB17_22
	.align	16, 0x90
.LBB17_14:                              # %for.body
                                        #   Parent Loop BB17_2 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	32(%rbx), %rdi
	movl	16(%rdi), %eax
	testq	$33554432, %rax         # imm = 0x2000000
	je	.LBB17_24
# BB#15:                                # %if.then
                                        #   in Loop: Header=BB17_14 Depth=2
	movl	32(%rdi), %ecx
	testq	%rcx, %rcx
	jne	.LBB17_17
# BB#16:                                # %if.then4
                                        #   in Loop: Header=BB17_14 Depth=2
	xorq	%rax, %rax
	movq	%r12, %rsi
	callq	error_with_decl
	movq	32(%rbx), %rax
	movq	input_filename(%rip), %rdi
	movq	72(%rax), %rdx
	movl	lineno(%rip), %esi
	callq	define_label
	jmp	.LBB17_20
	.align	16, 0x90
.LBB17_17:                              # %if.else
                                        #   in Loop: Header=BB17_14 Depth=2
	movl	warn_unused_label(%rip), %ecx
	testq	%rcx, %rcx
	je	.LBB17_20
# BB#18:                                # %if.else
                                        #   in Loop: Header=BB17_14 Depth=2
	andq	$65536, %rax            # imm = 0x10000
	testq	%r13, %rax
	jne	.LBB17_20
# BB#19:                                # %if.then18
                                        #   in Loop: Header=BB17_14 Depth=2
	xorq	%rax, %rax
	movabsq	$.L.str135, %rsi
	callq	warning_with_decl
.LBB17_20:                              # %if.then28
                                        #   in Loop: Header=BB17_14 Depth=2
	movq	32(%rbx), %rax
	movq	72(%rax), %rax
	movq	$0, 72(%rax)
	movq	(%rbx), %rbx
	testq	%rbx, %rbx
	movq	%rbx, (%r14)
	jne	.LBB17_14
	jmp	.LBB17_8
	.align	16, 0x90
.LBB17_24:                              # %if.else33
                                        #   in Loop: Header=BB17_2 Depth=1
	movq	%rbx, %r14
	movq	(%r14), %rbx
	jmp	.LBB17_23
.LBB17_5:                               # %if.else.us
                                        #   in Loop: Header=BB17_2 Depth=1
	movl	warn_unused_label(%rip), %ecx
	testq	%rcx, %rcx
	je	.LBB17_22
# BB#6:                                 # %if.else.us
                                        #   in Loop: Header=BB17_2 Depth=1
	andq	$65536, %rax            # imm = 0x10000
	testq	%r13, %rax
	jne	.LBB17_22
# BB#7:                                 # %if.then18.us
                                        #   in Loop: Header=BB17_2 Depth=1
	xorq	%rax, %rax
	movabsq	$.L.str135, %rsi
	callq	warning_with_decl
.LBB17_22:                              # %if.else31
                                        #   in Loop: Header=BB17_2 Depth=1
	movq	32(%rbx), %rax
	movq	72(%rax), %rax
	movq	$0, 72(%rax)
	movq	(%rbx), %rbx
	movq	%rbx, named_labels(%rip)
	xorq	%r14, %r14
.LBB17_23:                              # %for.cond.outer.backedge
                                        #   in Loop: Header=BB17_2 Depth=1
	testq	%rbx, %rbx
	jne	.LBB17_2
.LBB17_8:                               # %for.cond37.preheader
	movq	shadowed_labels(%rip), %rax
	jmp	.LBB17_9
	.align	16, 0x90
.LBB17_12:                              # %for.inc
                                        #   in Loop: Header=BB17_9 Depth=1
	movq	(%rax), %rax
.LBB17_9:                               # %for.cond37.preheader
                                        # =>This Inner Loop Header: Depth=1
	testq	%rax, %rax
	je	.LBB17_13
# BB#10:                                # %for.body39
                                        #   in Loop: Header=BB17_9 Depth=1
	movq	32(%rax), %rcx
	movq	72(%rcx), %rdx
	testq	%rdx, %rdx
	je	.LBB17_12
# BB#11:                                # %if.then45
                                        #   in Loop: Header=BB17_9 Depth=1
	movq	%rcx, 72(%rdx)
	jmp	.LBB17_12
.LBB17_13:                              # %for.end56
	movq	named_labels(%rip), %rdi
	movq	(%r15), %rsi
	callq	chainon
	movq	%rax, named_labels(%rip)
	movq	16(%r15), %rax
	movq	label_level_chain(%rip), %rcx
	movq	%rax, shadowed_labels(%rip)
	movq	40(%rcx), %rax
	movq	free_binding_level(%rip), %rcx
	movq	%rax, label_level_chain(%rip)
	movq	%rcx, 40(%r15)
	movq	%r15, free_binding_level(%rip)
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp128:
	.size	pop_label_level, .Ltmp128-pop_label_level
	.cfi_endproc

	.globl	pushtag
	.align	16, 0x90
	.type	pushtag,@function
pushtag:                                # @pushtag
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp132:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp133:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp134:
	.cfi_def_cfa_offset 32
.Ltmp135:
	.cfi_offset %rbx, -24
.Ltmp136:
	.cfi_offset %r14, -16
	movq	%rsi, %r14
	movabsq	$current_binding_level, %rax
	.align	16, 0x90
.LBB18_1:                               # %for.cond
                                        # =>This Inner Loop Header: Depth=1
	movq	(%rax), %rbx
	movzbl	49(%rbx), %ecx
	leaq	40(%rbx), %rax
	testq	%rcx, %rcx
	jne	.LBB18_1
# BB#2:                                 # %for.end
	testq	%rdi, %rdi
	je	.LBB18_5
# BB#3:                                 # %if.then
	cmpq	$0, 96(%r14)
	jne	.LBB18_5
# BB#4:                                 # %if.then4
	movq	%rdi, 96(%r14)
.LBB18_5:                               # %if.end7
	movq	8(%rbx), %rdx
	movq	%r14, %rsi
	callq	tree_cons
	movq	%rax, 8(%rbx)
	movq	$33, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	callq	build_decl
	movq	%rax, %rdi
	callq	pushdecl
	movq	%rax, (%r14)
	movq	80(%rax), %rax
	movq	%rax, 144(%r14)
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp137:
	.size	pushtag, .Ltmp137-pushtag
	.cfi_endproc

	.globl	pushdecl
	.align	16, 0x90
	.type	pushdecl,@function
pushdecl:                               # @pushdecl
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
	subq	$80, %rsp
.Ltmp149:
	.cfi_def_cfa_offset 128
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
	movq	%rdi, %r15
	movl	16(%r15), %eax
	movq	72(%r15), %r14
	movq	current_binding_level(%rip), %r13
	movq	%rax, %rcx
	andq	$255, %rcx
	cmpq	$30, %rcx
	jne	.LBB19_3
# BB#1:                                 # %land.lhs.true
	cmpq	$0, 200(%r15)
	jne	.LBB19_3
# BB#2:                                 # %if.then
	movq	$16, %rdi
	callq	ggc_alloc_cleared
	movq	%rax, 200(%r15)
	movl	16(%r15), %eax
.LBB19_3:                               # %if.end
	movq	current_function_decl(%rip), %rcx
	movq	%rcx, 80(%r15)
	andq	$255, %rax
	cmpq	$30, %rax
	je	.LBB19_5
# BB#4:                                 # %if.end
	cmpq	$34, %rax
	jne	.LBB19_8
.LBB19_5:                               # %land.lhs.true16
	cmpq	$0, 104(%r15)
	jne	.LBB19_8
# BB#6:                                 # %land.lhs.true19
	movzbl	49(%r15), %eax
	testq	$1, %rax
	je	.LBB19_8
# BB#7:                                 # %if.then24
	movq	$0, 80(%r15)
.LBB19_8:                               # %if.end27
	testq	%r14, %r14
	je	.LBB19_502
# BB#9:                                 # %if.then29
	movl	warn_nested_externs(%rip), %eax
	testq	%rax, %rax
	je	.LBB19_15
# BB#10:                                # %land.lhs.true31
	movq	48(%r15), %rax
	testq	$256, %rax              # imm = 0x100
	je	.LBB19_15
# BB#11:                                # %land.lhs.true31
	movq	global_binding_level(%rip), %rcx
	cmpq	%rcx, %r13
	je	.LBB19_15
# BB#12:                                # %land.lhs.true41
	movq	80(%r14), %rcx
	cmpq	%r15, %rcx
	je	.LBB19_15
# BB#13:                                # %land.lhs.true41
	andq	$65536, %rax            # imm = 0x10000
	jne	.LBB19_15
# BB#14:                                # %if.then50
	movq	32(%r14), %rsi
	movabsq	$.L.str136, %rdi
	xorq	%rax, %rax
	callq	warning
.LBB19_15:                              # %if.end51
	movq	current_binding_level(%rip), %rax
	movq	global_binding_level(%rip), %rcx
	cmpq	%rcx, %rax
	je	.LBB19_21
# BB#16:                                # %if.end.i
	cmpq	$0, 64(%r14)
	je	.LBB19_22
# BB#17:                                # %if.end3.i
	movq	(%rax), %r12
	testq	%r12, %r12
	je	.LBB19_22
# BB#18:
	xorq	%rbx, %rbx
	.align	16, 0x90
.LBB19_20:                              # %for.body.i
                                        # =>This Inner Loop Header: Depth=1
	movq	72(%r12), %rax
	cmpq	%r14, %rax
	je	.LBB19_29
# BB#19:                                # %for.cond.i
                                        #   in Loop: Header=BB19_20 Depth=1
	movq	(%r12), %r12
	testq	%r12, %r12
	jne	.LBB19_20
	jmp	.LBB19_22
.LBB19_21:                              # %lookup_name_current_level.exit
	movq	56(%r14), %r12
	xorq	%rbx, %rbx
	testq	%r12, %r12
	jne	.LBB19_29
.LBB19_22:                              # %land.lhs.true54
	movzbl	49(%r15), %eax
	xorq	%rbx, %rbx
	testq	$1, %rax
	movabsq	$0, %r12
	je	.LBB19_32
# BB#23:                                # %land.lhs.true62
	movzbl	18(%r15), %ecx
	movl	flag_traditional(%rip), %eax
	xorq	%rbx, %rbx
	testq	$8, %rcx
	je	.LBB19_24
# BB#25:                                # %land.lhs.true62
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	movabsq	$0, %r12
	jne	.LBB19_33
# BB#26:                                # %if.then70
	movq	56(%r14), %r12
	testq	%r12, %r12
	je	.LBB19_28
# BB#27:                                # %lor.lhs.false72
	movzbl	16(%r12), %eax
	cmpq	$33, %rax
	movq	$1, %rbx
	jne	.LBB19_29
.LBB19_28:                              # %if.then78
	movq	96(%r14), %r12
	movq	$1, %rbx
.LBB19_29:                              # %if.end80
	testq	%r12, %r12
	je	.LBB19_32
# BB#30:                                # %if.end80
	movq	global_trees(%rip), %rax
	cmpq	%rax, %r12
	jne	.LBB19_32
# BB#31:                                # %if.then84
	movabsq	$.L.str137, %rsi
	xorq	%rax, %rax
	xorq	%r12, %r12
	movq	%r15, %rdi
	callq	error_with_decl
.LBB19_32:                              # %if.end85thread-pre-split
	movl	flag_traditional(%rip), %eax
.LBB19_33:                              # %if.end85
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB19_40
# BB#34:                                # %land.lhs.true87
	movzbl	18(%r14), %eax
	testq	$8, %rax
	je	.LBB19_40
# BB#35:                                # %land.lhs.true94
	movl	16(%r15), %eax
	testq	$524288, %rax           # imm = 0x80000
	jne	.LBB19_40
# BB#36:                                # %land.lhs.true101
	andq	$255, %rax
	cmpq	$30, %rax
	je	.LBB19_38
# BB#37:                                # %land.lhs.true101
	movq	global_binding_level(%rip), %rax
	cmpq	%rax, %r13
	jne	.LBB19_40
.LBB19_38:                              # %land.lhs.true109
	cmpq	$0, 80(%r14)
	je	.LBB19_40
# BB#39:                                # %if.then112
	movq	32(%r14), %rsi
	movabsq	$.L.str138, %rdi
	xorq	%rax, %rax
	callq	pedwarn
	movq	80(%r14), %rax
	movq	32(%r14), %rcx
	movq	24(%rax), %rdi
	movl	32(%rax), %esi
	movabsq	$.L.str139, %rdx
	xorq	%rax, %rax
	callq	pedwarn_with_file_and_line
	movl	16(%r14), %eax
	orq	$2048, %rax             # imm = 0x800
	movl	%eax, 16(%r14)
.LBB19_40:                              # %if.end126
	testq	%r12, %r12
	je	.LBB19_97
# BB#41:                                # %land.lhs.true128
	movq	8(%r15), %rdi
	movq	8(%r12), %rsi
	callq	comptypes
	movzbl	16(%r15), %ecx
	movq	%rax, %r8
	xorq	%rax, %rax
	cmpq	$30, %rcx
	jne	.LBB19_45
# BB#42:                                # %land.rhs.i
	cmpq	$0, 104(%r15)
	je	.LBB19_43
# BB#44:                                # %land.rhs.i
	movabsq	$1, %rax
	jmp	.LBB19_45
.LBB19_43:
	movabsq	$0, %rax
.LBB19_45:                              # %land.end.i
	movq	%rax, 40(%rsp)          # 8-byte Spill
	movzbl	16(%r12), %eax
	movq	8(%r12), %r9
	movq	%r9, 64(%rsp)           # 8-byte Spill
	movzbl	tree_code_type(%rax), %edx
	movq	8(%r15), %r10
	movq	%r10, 72(%rsp)          # 8-byte Spill
	cmpq	$100, %rdx
	jne	.LBB19_67
# BB#46:                                # %if.then.i926
	movq	%r10, 72(%rsp)          # 8-byte Spill
	movq	%r9, 64(%rsp)           # 8-byte Spill
	movq	%r13, 48(%rsp)          # 8-byte Spill
	movq	%r8, %r13
	cmpq	$30, %rcx
	jne	.LBB19_66
# BB#47:                                # %if.then.i926
	cmpq	$30, %rax
	jne	.LBB19_66
# BB#48:                                # %land.lhs.true27.i
	movq	48(%r15), %rax
	movabsq	$68719476736, %rcx      # imm = 0x1000000000
	andq	%rcx, %rax
	jne	.LBB19_50
# BB#49:                                # %lor.lhs.false.i
	movq	48(%r12), %rcx
	movabsq	$68719476736, %rdx      # imm = 0x1000000000
	testq	%rdx, %rcx
	je	.LBB19_66
.LBB19_50:                              # %if.then38.i
	movq	200(%r15), %rcx
	movzbl	(%rcx), %ecx
	testq	$1, %rcx
	je	.LBB19_53
# BB#51:                                # %if.then38.i
	testq	%rax, %rax
	je	.LBB19_53
# BB#52:                                # %land.lhs.true52.i
	movq	136(%r15), %rsi
	movabsq	$.L.str177, %rdi
	callq	lookup_attribute
	testq	%rax, %rax
	jne	.LBB19_66
.LBB19_53:                              # %if.else.i929
	movq	200(%r12), %rax
	movzbl	(%rax), %eax
	testq	$1, %rax
	je	.LBB19_56
# BB#54:                                # %land.lhs.true64.i
	movq	48(%r12), %rax
	movabsq	$68719476736, %rcx      # imm = 0x1000000000
	testq	%rcx, %rax
	je	.LBB19_56
# BB#55:                                # %land.lhs.true72.i
	movq	136(%r12), %rsi
	movabsq	$.L.str177, %rdi
	callq	lookup_attribute
	testq	%rax, %rax
	jne	.LBB19_66
.LBB19_56:                              # %if.else78.i
	movq	200(%r15), %rax
	movzbl	(%rax), %eax
	testq	$1, %rax
	je	.LBB19_61
# BB#57:                                # %land.lhs.true86.i
	movq	200(%r12), %rax
	movzbl	(%rax), %eax
	testq	$1, %rax
	jne	.LBB19_61
# BB#58:                                # %land.lhs.true94.i
	movq	48(%r12), %rax
	movabsq	$68719476736, %rcx      # imm = 0x1000000000
	testq	%rcx, %rax
	je	.LBB19_61
# BB#59:                                # %land.lhs.true102.i
	movq	136(%r12), %rsi
	movabsq	$.L.str177, %rdi
	callq	lookup_attribute
	testq	%rax, %rax
	je	.LBB19_61
# BB#60:                                # %if.then107.i
	movabsq	$.L.str328, %rsi
	xorq	%rax, %rax
	movq	%r15, %rdi
	callq	warning_with_decl
	movabsq	$.L.str329, %rsi
	jmp	.LBB19_65
.LBB19_24:
	xorq	%r12, %r12
	jmp	.LBB19_33
.LBB19_61:                              # %if.else108.i
	movq	200(%r12), %rax
	movzbl	(%rax), %eax
	testq	$1, %rax
	je	.LBB19_66
# BB#62:                                # %land.lhs.true116.i
	movq	48(%r15), %rax
	movabsq	$68719476736, %rcx      # imm = 0x1000000000
	testq	%rcx, %rax
	je	.LBB19_66
# BB#63:                                # %land.lhs.true124.i
	movq	136(%r15), %rsi
	movabsq	$.L.str177, %rdi
	callq	lookup_attribute
	testq	%rax, %rax
	je	.LBB19_66
# BB#64:                                # %if.then129.i
	movabsq	$.L.str330, %rsi
	xorq	%rax, %rax
	movq	%r15, %rdi
	callq	warning_with_decl
	movabsq	$.L.str331, %rsi
.LBB19_65:                              # %if.end133.i
	xorq	%rax, %rax
	movq	%r12, %rdi
	callq	warning_with_decl
.LBB19_66:                              # %if.end133.i
	movq	%r12, %rdi
	movq	%r15, %rsi
	callq	*targetm+240(%rip)
	movq	%rax, 136(%r15)
	movq	%r13, %r8
	movq	48(%rsp), %r13          # 8-byte Reload
	movq	64(%rsp), %r9           # 8-byte Reload
	movq	72(%rsp), %r10          # 8-byte Reload
.LBB19_67:                              # %if.end137.i
	movzbl	16(%r10), %eax
	testq	%rax, %rax
	je	.LBB19_69
# BB#68:                                # %lor.lhs.false144.i
	movzbl	16(%r9), %eax
	testq	%rax, %rax
	jne	.LBB19_70
.LBB19_69:                              # %if.then151.i
	xorq	%r8, %r8
.LBB19_70:                              # %if.end152.i
	movq	%r8, 56(%rsp)           # 8-byte Spill
	movl	16(%r12), %eax
	movl	16(%r15), %ecx
	movq	%rax, %rdx
	andq	$255, %rdx
	movq	%rcx, %rsi
	andq	$255, %rsi
	cmpq	%rsi, %rdx
	jne	.LBB19_71
# BB#84:                                # %if.end230.i
	movabsq	$4294967295, %rsi       # imm = 0xFFFFFFFF
	testq	%rsi, %r8
	je	.LBB19_88
# BB#85:                                # %if.end230.i
	cmpq	$35, %rdx
	jne	.LBB19_88
# BB#86:                                # %if.end230.i
	movq	%rax, %rsi
	andq	$16384, %rsi            # imm = 0x4000
	movabsq	$4294967295, %rdi       # imm = 0xFFFFFFFF
	testq	%rdi, %rsi
	je	.LBB19_88
# BB#87:                                # %if.end230.i
	movq	%rcx, %rsi
	andq	$16384, %rsi            # imm = 0x4000
	movabsq	$4294967295, %rdi       # imm = 0xFFFFFFFF
	testq	%rdi, %rsi
	je	.LBB19_375
.LBB19_88:                              # %if.end253.i
	movl	flag_traditional(%rip), %esi
	testq	%rsi, %rsi
	je	.LBB19_93
# BB#89:                                # %if.end253.i
	cmpq	$30, %rdx
	jne	.LBB19_93
# BB#90:                                # %land.lhs.true262.i
	movq	72(%r15), %rsi
	movq	80(%rsi), %rsi
	cmpq	%r12, %rsi
	jne	.LBB19_93
# BB#91:                                # %land.lhs.true266.i
	cmpq	$0, 104(%r12)
	je	.LBB19_92
.LBB19_93:                              # %if.else272.i
	cmpq	$30, %rdx
	jne	.LBB19_132
# BB#94:                                # %land.lhs.true279.i
	movzbl	51(%r12), %esi
	testq	$96, %rsi
	je	.LBB19_127
# BB#95:                                # %if.then288.i
	testq	$524288, %rcx           # imm = 0x80000
	je	.LBB19_96
# BB#101:                               # %if.else299.i
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %r8
	je	.LBB19_102
# BB#118:                               # %if.else398.i
	cmpq	$0, 24(%r9)
	je	.LBB19_120
# BB#119:
	movq	%r9, 64(%rsp)           # 8-byte Spill
	jmp	.LBB19_125
.LBB19_71:                              # %if.then163.i
	cmpq	$30, %rdx
	jne	.LBB19_83
# BB#72:                                # %land.lhs.true170.i
	movq	48(%r12), %rdx
	andq	$1610612736, %rdx       # imm = 0x60000000
	jne	.LBB19_74
# BB#73:                                # %land.lhs.true170.i
	andq	$8192, %rax             # imm = 0x2000
	movabsq	$4294967295, %rsi       # imm = 0xFFFFFFFF
	testq	%rsi, %rax
	jne	.LBB19_74
.LBB19_83:                              # %if.else228.i
	movabsq	$.L.str336, %rsi
	xorq	%rax, %rax
	movq	%r15, %rdi
	callq	error_with_decl
	movabsq	$.L.str139, %rsi
.LBB19_208:                             # %land.end686.i
	xorq	%rax, %rax
	movq	%r12, %rdi
	callq	error_with_decl
	jmp	.LBB19_97
.LBB19_74:                              # %if.then184.i
	testq	$524288, %rcx           # imm = 0x80000
	je	.LBB19_75
# BB#80:                                # %if.else206.i
	testq	%rdx, %rdx
	je	.LBB19_81
# BB#82:                                # %if.else216.i
	movabsq	$.L.str335, %rsi
	jmp	.LBB19_78
.LBB19_75:                              # %if.then190.i
	movl	warn_shadow(%rip), %eax
	testq	%rax, %rax
	je	.LBB19_97
# BB#76:                                # %if.else193.i
	testq	%rdx, %rdx
	jne	.LBB19_77
# BB#79:                                # %if.else203.i
	movabsq	$.L.str333, %rsi
	jmp	.LBB19_78
.LBB19_127:                             # %land.lhs.true436.i
	movl	32(%r12), %esi
	testq	%rsi, %rsi
	je	.LBB19_128
.LBB19_132:                             # %if.else464.i
	movabsq	$4294967295, %rsi       # imm = 0xFFFFFFFF
	testq	%rsi, %r8
	jne	.LBB19_157
# BB#133:                               # %land.lhs.true466.i
	cmpq	$30, %rdx
	jne	.LBB19_152
# BB#134:                               # %land.lhs.true480.i
	movq	8(%r9), %rax
	movzbl	16(%rax), %eax
	cmpq	$13, %rax
	jne	.LBB19_151
# BB#135:                               # %land.lhs.true489.i
	movq	8(%r10), %rax
	movzbl	16(%rax), %ecx
	cmpq	$13, %rcx
	jne	.LBB19_151
# BB#136:                               # %land.lhs.true498.i
	movzbl	50(%r12), %ecx
	testq	$1, %rcx
	jne	.LBB19_138
# BB#137:                               # %lor.lhs.false505.i
	movzbl	50(%r15), %ecx
	testq	$1, %rcx
	je	.LBB19_151
.LBB19_138:                             # %land.lhs.true513.i
	movq	8(%rax), %rax
	movq	128(%rax), %rax
	movq	global_trees+216(%rip), %rcx
	cmpq	%rcx, %rax
	jne	.LBB19_142
# BB#139:                               # %land.lhs.true521.i
	cmpq	$0, 24(%r9)
	jne	.LBB19_142
# BB#140:                               # %land.lhs.true526.i
	movq	24(%r10), %rdi
	movq	%r13, 48(%rsp)          # 8-byte Spill
	movq	%r8, 56(%rsp)           # 8-byte Spill
	movq	%r9, %r13
	movq	%r10, 72(%rsp)          # 8-byte Spill
	callq	self_promoting_args_p
	movq	72(%rsp), %r10          # 8-byte Reload
	movq	%r13, %r9
	movq	56(%rsp), %r8           # 8-byte Reload
	movq	48(%rsp), %r13          # 8-byte Reload
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB19_142
# BB#141:                               # %land.lhs.true531.i
	movq	8(%r9), %rax
	movq	8(%rax), %rcx
	movq	integer_types(%rip), %rdx
	cmpq	%rdx, %rcx
	je	.LBB19_146
.LBB19_142:                             # %lor.lhs.false538.i
	movq	8(%r10), %rax
	movq	8(%rax), %rax
	movq	integer_types(%rip), %rcx
	cmpq	%rcx, %rax
	jne	.LBB19_151
# BB#143:                               # %land.lhs.true545.i
	cmpq	$0, 24(%r10)
	jne	.LBB19_151
# BB#144:                               # %land.lhs.true550.i
	movq	24(%r9), %rdi
	movq	%r13, 48(%rsp)          # 8-byte Spill
	movq	%r8, 56(%rsp)           # 8-byte Spill
	movq	%r9, %r13
	movq	%r10, 72(%rsp)          # 8-byte Spill
	callq	self_promoting_args_p
	movq	72(%rsp), %r10          # 8-byte Reload
	movq	%r13, %r9
	movq	56(%rsp), %r8           # 8-byte Reload
	movq	48(%rsp), %r13          # 8-byte Reload
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB19_151
# BB#145:                               # %land.lhs.true555.i
	movq	8(%r9), %rax
	movq	8(%rax), %rcx
	movq	128(%rcx), %rcx
	movq	global_trees+216(%rip), %rdx
	cmpq	%rdx, %rcx
	je	.LBB19_146
.LBB19_151:                             # %if.else585.land.lhs.true587_crit_edge.i
	movl	16(%r12), %eax
.LBB19_152:                             # %land.lhs.true587.i
	andq	$255, %rax
	cmpq	$30, %rax
	jne	.LBB19_166
# BB#153:                               # %land.lhs.true587.i
	movl	pedantic(%rip), %eax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB19_166
# BB#154:                               # %land.lhs.true596.i
	movq	8(%r9), %rdi
	movq	8(%r10), %rsi
	movq	%r13, 48(%rsp)          # 8-byte Spill
	movq	%r8, 56(%rsp)           # 8-byte Spill
	movq	%r9, %r13
	movq	%r10, 72(%rsp)          # 8-byte Spill
	callq	comptypes
	movq	72(%rsp), %r10          # 8-byte Reload
	movq	%r13, %r9
	movq	56(%rsp), %r8           # 8-byte Reload
	movq	48(%rsp), %r13          # 8-byte Reload
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB19_166
# BB#155:                               # %land.lhs.true603.i
	cmpq	$0, 24(%r10)
	je	.LBB19_156
.LBB19_166:                             # %if.then608.i
	movq	%r10, 72(%rsp)          # 8-byte Spill
	movq	%r9, 64(%rsp)           # 8-byte Spill
	movq	%r13, 48(%rsp)          # 8-byte Spill
	movabsq	$.L.str338, %rsi
	xorq	%rax, %rax
	movq	$0, 56(%rsp)            # 8-byte Folded Spill
	movq	%r15, %rdi
	callq	error_with_decl
	movzbl	16(%r12), %eax
	cmpq	$30, %rax
	jne	.LBB19_172
# BB#167:                               # %land.lhs.true615.i
	movq	64(%rsp), %rax          # 8-byte Reload
	movq	8(%rax), %rdi
	movq	72(%rsp), %rax          # 8-byte Reload
	movq	8(%rax), %rsi
	callq	comptypes
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB19_172
# BB#168:                               # %land.lhs.true622.i
	movq	64(%rsp), %rax          # 8-byte Reload
	movq	24(%rax), %r13
	testq	%r13, %r13
	jne	.LBB19_170
# BB#169:                               # %land.lhs.true627.i
	cmpq	$0, 104(%r12)
	je	.LBB19_173
.LBB19_170:                             # %lor.lhs.false632.i
	movq	72(%rsp), %rax          # 8-byte Reload
	cmpq	$0, 24(%rax)
	jne	.LBB19_172
# BB#171:                               # %land.lhs.true637.i
	cmpq	$0, 104(%r15)
	je	.LBB19_174
	jmp	.LBB19_172
.LBB19_81:                              # %if.then215.i
	movabsq	$.L.str334, %rsi
	jmp	.LBB19_78
.LBB19_96:                              # %if.then295.i
	movl	warn_shadow(%rip), %eax
	testq	%rax, %rax
	je	.LBB19_97
.LBB19_77:                              # %if.then202.i
	movabsq	$.L.str332, %rsi
	jmp	.LBB19_78
.LBB19_92:
	movq	%r10, 72(%rsp)          # 8-byte Spill
	movq	%r9, 64(%rsp)           # 8-byte Spill
	jmp	.LBB19_265
.LBB19_102:                             # %if.then301.i
	movq	8(%r9), %rax
	movq	8(%r10), %rdi
	movl	60(%rax), %eax
	movl	60(%rdi), %ecx
	xorq	%rax, %rcx
	xorq	%r8, %r8
	testq	$65024, %rcx            # imm = 0xFE00
	jne	.LBB19_106
# BB#103:                               # %if.then317.i
	movq	24(%r9), %rsi
	movq	%r9, 64(%rsp)           # 8-byte Spill
	movq	%r10, 72(%rsp)          # 8-byte Spill
	callq	build_function_type
	movq	64(%rsp), %rcx          # 8-byte Reload
	movq	48(%rcx), %rsi
	movq	%rax, %rdi
	callq	build_type_attribute_variant
	movq	%rax, 56(%rsp)          # 8-byte Spill
	movq	72(%rsp), %rdi          # 8-byte Reload
	movq	%rax, %rsi
	callq	comptypes
	movq	%rax, %r8
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %r8
	je	.LBB19_105
# BB#104:                               # %if.then317.i
	movq	56(%rsp), %rax          # 8-byte Reload
	movq	%rax, 64(%rsp)          # 8-byte Spill
.LBB19_105:                             # %if.then317.i
	movq	64(%rsp), %r9           # 8-byte Reload
	movq	72(%rsp), %r10          # 8-byte Reload
.LBB19_106:                             # %if.end327.i
	movq	8(%r15), %rax
	cmpq	$0, 24(%rax)
	je	.LBB19_115
# BB#107:                               # %land.lhs.true334.i
	movq	24(%r9), %rax
	testq	%rax, %rax
	je	.LBB19_115
# BB#108:                               # %land.lhs.true339.i
	movq	24(%r10), %rcx
	movq	32(%rcx), %rsi
	testq	%rsi, %rsi
	je	.LBB19_115
# BB#109:                               # %land.lhs.true344.i
	movq	32(%rax), %rcx
	testq	%rcx, %rcx
	je	.LBB19_115
# BB#110:                               # %land.lhs.true351.i
	movq	%r10, 72(%rsp)          # 8-byte Spill
	movl	60(%rsi), %edx
	movl	60(%rcx), %ecx
	xorq	%rdx, %rcx
	testq	$65024, %rcx            # imm = 0xFE00
	jne	.LBB19_114
# BB#111:                               # %if.then372.i
	movq	8(%r9), %rcx
	movq	%rcx, 56(%rsp)          # 8-byte Spill
	movq	(%rax), %rdx
	xorq	%rdi, %rdi
	movq	%r9, 64(%rsp)           # 8-byte Spill
	callq	tree_cons
	movq	56(%rsp), %rdi          # 8-byte Reload
	movq	%rax, %rsi
	callq	build_function_type
	movq	64(%rsp), %rcx          # 8-byte Reload
	movq	48(%rcx), %rsi
	movq	%rax, %rdi
	callq	build_type_attribute_variant
	movq	%rax, 56(%rsp)          # 8-byte Spill
	movq	72(%rsp), %rdi          # 8-byte Reload
	movq	%rax, %rsi
	callq	comptypes
	movq	%rax, %r8
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %r8
	je	.LBB19_113
# BB#112:                               # %if.then372.i
	movq	56(%rsp), %rax          # 8-byte Reload
	movq	%rax, 64(%rsp)          # 8-byte Spill
.LBB19_113:                             # %if.then372.i
	movq	64(%rsp), %r9           # 8-byte Reload
.LBB19_114:                             # %if.end392.i
	movq	72(%rsp), %r10          # 8-byte Reload
.LBB19_115:                             # %if.end392.i
	testq	%rbx, %rbx
	je	.LBB19_117
# BB#116:
	movq	%r9, 64(%rsp)           # 8-byte Spill
	jmp	.LBB19_125
.LBB19_128:                             # %if.then440.i
	testq	$524288, %rcx           # imm = 0x80000
	je	.LBB19_97
# BB#129:                               # %if.else448.i
	movq	%r9, 64(%rsp)           # 8-byte Spill
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	testq	%rdx, %r8
	je	.LBB19_131
# BB#130:
	movq	%r10, 72(%rsp)          # 8-byte Spill
	jmp	.LBB19_265
.LBB19_120:                             # %land.lhs.true403.i
	movq	24(%r10), %rsi
	testq	%rsi, %rsi
	je	.LBB19_121
# BB#122:                               # %if.then408.i
	movq	%r10, 72(%rsp)          # 8-byte Spill
	movq	%r13, 48(%rsp)          # 8-byte Spill
	movq	%r8, 56(%rsp)           # 8-byte Spill
	movq	8(%r9), %rdi
	movq	%r9, %r13
	callq	build_function_type
	movq	48(%r13), %rsi
	movq	%rax, %rdi
	callq	build_type_attribute_variant
	testq	%rbx, %rbx
	jne	.LBB19_124
# BB#123:                               # %if.then419.i
	movq	%rax, 8(%r12)
.LBB19_124:                             # %if.end425.i
	movq	%rax, 64(%rsp)          # 8-byte Spill
	movq	56(%rsp), %r8           # 8-byte Reload
	movq	48(%rsp), %r13          # 8-byte Reload
	movq	72(%rsp), %r10          # 8-byte Reload
	jmp	.LBB19_125
.LBB19_156:                             # %land.lhs.true603.i.if.else674.i_crit_edge
	movl	16(%r15), %ecx
.LBB19_157:                             # %if.else674.i
	movq	%rcx, %rax
	andq	$255, %rax
	cmpq	$30, %rax
	jne	.LBB19_158
# BB#182:                               # %if.then33.i.i
	cmpq	$0, 104(%r12)
	je	.LBB19_217
# BB#183:                               # %land.lhs.true36.i.i
	cmpq	$0, 104(%r15)
	je	.LBB19_217
# BB#184:                               # %land.lhs.true40.i.i
	movq	200(%r12), %rcx
	movzbl	(%rcx), %ecx
	testq	$1, %rcx
	je	.LBB19_188
# BB#185:                               # %land.lhs.true46.i.i
	movzbl	49(%r12), %ecx
	testq	$1, %rcx
	je	.LBB19_188
# BB#186:                               # %land.lhs.true53.i.i
	movq	200(%r15), %rcx
	movzbl	(%rcx), %ecx
	testq	$1, %rcx
	je	.LBB19_209
# BB#187:                               # %land.lhs.true61.i.i
	movzbl	49(%r15), %ecx
	testq	$1, %rcx
	jne	.LBB19_188
	jmp	.LBB19_209
.LBB19_158:                             # %if.else674.i
	cmpq	$33, %rax
	jne	.LBB19_192
# BB#159:                               # %if.then.i.i
	movl	flag_traditional(%rip), %ecx
	testq	%rcx, %rcx
	je	.LBB19_162
# BB#160:                               # %land.lhs.true.i.i
	movq	8(%r15), %rcx
	movq	8(%r12), %rdx
	cmpq	%rdx, %rcx
	je	.LBB19_210
# BB#161:                               # %land.lhs.true7.i.i
	movq	128(%rdx), %rdx
	cmpq	%rcx, %rdx
	je	.LBB19_209
.LBB19_162:                             # %if.end15.i.i
	movzbl	50(%r12), %ecx
	testq	$1, %rcx
	jne	.LBB19_209
# BB#163:                               # %lor.lhs.false.i.i
	movq	48(%r15), %rdx
	movq	$16, %rcx
	shrq	%cl, %rdx
	notq	%rdx
	andq	$1, %rdx
.LBB19_164:                             # %redeclaration_error_message.exit.i
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rdx
	cmpq	$3, %rdx
	ja	.LBB19_206
# BB#165:                               # %redeclaration_error_message.exit.i
	jmpq	*.LJTI19_0(,%rdx,8)
.LBB19_205:                             # %sw.bb679.i
	movabsq	$.L.str343, %rsi
	jmp	.LBB19_189
.LBB19_192:                             # %if.else71.i.i
	movq	80(%r15), %rdx
	testq	%rdx, %rdx
	je	.LBB19_193
# BB#198:                               # %if.else113.i.i
	movq	current_binding_level(%rip), %rsi
	movzbl	48(%rsi), %esi
	testq	%rsi, %rsi
	je	.LBB19_201
# BB#199:                               # %land.lhs.true115.i.i
	movl	16(%r12), %esi
	testq	$16384, %rsi            # imm = 0x4000
	je	.LBB19_201
# BB#200:                               # %land.lhs.true115.i.i
	andq	$16384, %rcx            # imm = 0x4000
	movabsq	$4294967295, %rsi       # imm = 0xFFFFFFFF
	testq	%rsi, %rcx
	je	.LBB19_209
.LBB19_201:                             # %if.else129.i.i
	movzbl	49(%r15), %ecx
	testq	$1, %rcx
	je	.LBB19_203
# BB#202:                               # %land.lhs.true137.i.i
	movzbl	49(%r12), %ecx
	testq	$1, %rcx
	jne	.LBB19_209
.LBB19_203:                             # %land.lhs.true145.i.i
	movq	80(%r12), %rcx
	cmpq	%rcx, %rdx
	jne	.LBB19_209
.LBB19_204:                             # %sw.bb678.i
	movabsq	$.L.str342, %rsi
	jmp	.LBB19_189
.LBB19_193:                             # %if.then74.i.i
	movzbl	49(%r15), %edx
	testq	$1, %rdx
	jne	.LBB19_209
# BB#194:                               # %lor.lhs.false82.i.i
	movzbl	49(%r12), %edx
	testq	$1, %rdx
	je	.LBB19_195
.LBB19_209:                             # %if.else688.i
	cmpq	$33, %rax
	jne	.LBB19_217
.LBB19_210:                             # %land.lhs.true695.i
	movzbl	50(%r12), %eax
	testq	$1, %rax
	jne	.LBB19_212
# BB#211:                               # %lor.lhs.false703.i
	movzbl	50(%r15), %eax
	testq	$1, %rax
	je	.LBB19_217
.LBB19_212:                             # %if.then711.i
	movq	%r10, 72(%rsp)          # 8-byte Spill
	movq	%r9, 64(%rsp)           # 8-byte Spill
	movq	%r8, 56(%rsp)           # 8-byte Spill
	movabsq	$.L.str341, %rsi
	xorq	%rax, %rax
	movq	%r15, %rdi
	callq	warning_with_decl
	cmpq	$0, 104(%r12)
	je	.LBB19_215
# BB#213:                               # %land.rhs715.i
	movq	current_binding_level(%rip), %rax
	movq	global_binding_level(%rip), %rcx
	cmpq	%rcx, %rax
	je	.LBB19_214
.LBB19_215:                             # %select.mid107
	movabsq	$.L.str345, %rsi
	jmp	.LBB19_216
.LBB19_217:                             # %if.else721.i
	movl	16(%r12), %eax
	movq	%rax, %rcx
	andq	$255, %rcx
	cmpq	$30, %rcx
	jne	.LBB19_239
# BB#218:                               # %land.lhs.true728.i
	cmpq	$0, 104(%r12)
	je	.LBB19_235
# BB#219:                               # %land.lhs.true733.i
	cmpq	$0, 24(%r9)
	jne	.LBB19_235
# BB#220:                               # %land.lhs.true738.i
	movq	24(%r10), %rcx
	testq	%rcx, %rcx
	je	.LBB19_235
# BB#221:                               # %land.lhs.true743.i
	movq	136(%r9), %rdx
	testq	%rdx, %rdx
	je	.LBB19_235
# BB#222:                               # %for.cond753.preheader.i
	movq	32(%rdx), %rdi
	movq	32(%rcx), %rsi
	movq	%rdx, 48(%rsp)          # 8-byte Spill
	movq	128(%rdi), %rdx
	movq	global_trees+216(%rip), %r11
	movq	128(%rsi), %rax
	cmpq	%r11, %rdx
	je	.LBB19_227
# BB#223:                               # %lor.lhs.false775.i.lr.ph
	movq	%rax, %rdx
	movq	48(%rsp), %rax          # 8-byte Reload
	leaq	32(%rax), %rax
	movq	%rax, 24(%rsp)          # 8-byte Spill
	leaq	32(%rcx), %rax
	movq	%rax, 16(%rsp)          # 8-byte Spill
	movq	%rdx, %rax
	movq	$1, 8(%rsp)             # 8-byte Folded Spill
.LBB19_224:                             # %lor.lhs.false775.i
                                        # =>This Inner Loop Header: Depth=1
	movq	%rcx, 32(%rsp)          # 8-byte Spill
	cmpq	%r11, %rax
	je	.LBB19_225
# BB#229:                               # %if.end783.i
                                        #   in Loop: Header=BB19_224 Depth=1
	callq	comptypes
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB19_226
# BB#230:                               # %land.lhs.true790.i
                                        #   in Loop: Header=BB19_224 Depth=1
	movl	flag_traditional(%rip), %eax
	testq	%rax, %rax
	je	.LBB19_233
# BB#231:                               # %land.lhs.true792.i
                                        #   in Loop: Header=BB19_224 Depth=1
	movq	24(%rsp), %rax          # 8-byte Reload
	movq	(%rax), %rax
	movq	128(%rax), %rax
	movq	integer_types+40(%rip), %rcx
	cmpq	%rcx, %rax
	jne	.LBB19_233
# BB#232:                               # %land.lhs.true799.i
                                        #   in Loop: Header=BB19_224 Depth=1
	movq	16(%rsp), %rax          # 8-byte Reload
	movq	(%rax), %rax
	movq	128(%rax), %rax
	movq	integer_types+48(%rip), %rcx
	cmpq	%rcx, %rax
	jne	.LBB19_233
.LBB19_226:                             # %for.inc808.i
                                        #   in Loop: Header=BB19_224 Depth=1
	movq	48(%rsp), %rax          # 8-byte Reload
	movq	(%rax), %rax
	movq	%rax, 48(%rsp)          # 8-byte Spill
	movq	32(%rsp), %rcx          # 8-byte Reload
	movq	(%rcx), %rcx
	movq	32(%rax), %rdi
	movq	32(%rcx), %rsi
	incq	8(%rsp)                 # 8-byte Folded Spill
	movq	128(%rdi), %rdx
	movq	global_trees+216(%rip), %r11
	movq	128(%rsi), %r8
	leaq	32(%rax), %rax
	movq	%rax, 24(%rsp)          # 8-byte Spill
	leaq	32(%rcx), %rax
	movq	%rax, 16(%rsp)          # 8-byte Spill
	movq	%r8, %rax
	cmpq	%r11, %rdx
	movq	56(%rsp), %r8           # 8-byte Reload
	movq	64(%rsp), %r9           # 8-byte Reload
	movq	72(%rsp), %r10          # 8-byte Reload
	jne	.LBB19_224
.LBB19_227:                             # %land.lhs.true760.i
	movq	%r8, 56(%rsp)           # 8-byte Spill
	cmpq	%rdx, %rax
	je	.LBB19_228
.LBB19_225:                             # %if.then782.i
	movq	%r10, 72(%rsp)          # 8-byte Spill
	movq	%r9, 64(%rsp)           # 8-byte Spill
	movabsq	$.L.str348, %rsi
	xorq	%rax, %rax
	movq	$0, 32(%rsp)            # 8-byte Folded Spill
	movq	%r15, %rdi
	callq	error_with_decl
	jmp	.LBB19_234
.LBB19_235:                             # %land.lhs.true821.i
	movq	200(%r12), %rcx
	movzbl	(%rcx), %ecx
	testq	$1, %rcx
	jne	.LBB19_239
# BB#236:                               # %land.lhs.true829.i
	movq	200(%r15), %rcx
	movzbl	(%rcx), %ecx
	testq	$1, %rcx
	je	.LBB19_239
# BB#237:                               # %land.lhs.true829.i
	movq	%rax, %rcx
	andq	$65536, %rcx            # imm = 0x10000
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	testq	%rdx, %rcx
	je	.LBB19_239
# BB#238:                               # %if.then843.i
	movabsq	$.L.str350, %rsi
	xorq	%rax, %rax
	movq	%r15, %rdi
	movq	%r13, 48(%rsp)          # 8-byte Spill
	movq	%r8, 56(%rsp)           # 8-byte Spill
	movq	%r9, %r13
	movq	%r10, 72(%rsp)          # 8-byte Spill
	callq	warning_with_decl
	movq	72(%rsp), %r10          # 8-byte Reload
	movq	%r13, %r9
	movq	56(%rsp), %r8           # 8-byte Reload
	movq	48(%rsp), %r13          # 8-byte Reload
	movl	16(%r12), %eax
.LBB19_239:                             # %if.end844.i
	movq	%rax, %rcx
	andq	$255, %rcx
	cmpq	$30, %rcx
	jne	.LBB19_244
# BB#240:                               # %land.lhs.true851.i
	movq	200(%r12), %rcx
	movzbl	(%rcx), %ecx
	testq	$1, %rcx
	jne	.LBB19_244
# BB#241:                               # %land.lhs.true859.i
	movq	200(%r15), %rcx
	movzbl	(%rcx), %ecx
	testq	$1, %rcx
	je	.LBB19_244
# BB#242:                               # %land.lhs.true867.i
	cmpq	$0, 104(%r12)
	je	.LBB19_244
# BB#243:                               # %if.then872.i
	movabsq	$.L.str351, %rsi
	xorq	%rax, %rax
	movq	%r15, %rdi
	movq	%r13, 48(%rsp)          # 8-byte Spill
	movq	%r8, 56(%rsp)           # 8-byte Spill
	movq	%r9, %r13
	movq	%r10, 72(%rsp)          # 8-byte Spill
	callq	warning_with_decl
	movq	72(%rsp), %r10          # 8-byte Reload
	movq	%r13, %r9
	movq	56(%rsp), %r8           # 8-byte Reload
	movq	48(%rsp), %r13          # 8-byte Reload
	movl	16(%r12), %eax
.LBB19_244:                             # %if.end873.i
	movq	%r9, 64(%rsp)           # 8-byte Spill
	movq	%r8, 56(%rsp)           # 8-byte Spill
	movl	pedantic(%rip), %ecx
	movq	%rax, %rdi
	andq	$255, %rdi
	testq	%rcx, %rcx
	movabsq	$0, %rcx
	movabsq	$1, %rsi
	movq	%rcx, %rdx
	jne	.LBB19_246
# BB#245:                               # %if.end873.i
	movq	%rsi, %rdx
.LBB19_246:                             # %if.end873.i
	movq	%r10, 72(%rsp)          # 8-byte Spill
	cmpq	$30, %rdi
	je	.LBB19_248
# BB#247:                               # %if.end873.i
	movq	%rsi, %rcx
.LBB19_248:                             # %if.end873.i
	testq	%rdx, %rcx
	jne	.LBB19_252
# BB#249:                               # %if.end873.i
	andq	$524288, %rax           # imm = 0x80000
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB19_252
# BB#250:                               # %land.lhs.true889.i
	movzbl	18(%r15), %eax
	testq	$8, %rax
	jne	.LBB19_252
# BB#251:                               # %if.then896.i
	movabsq	$.L.str352, %rsi
	xorq	%rax, %rax
	movq	%r15, %rdi
	callq	warning_with_decl
.LBB19_252:                             # %if.end897.i
	movl	warn_traditional(%rip), %eax
	testq	%rax, %rax
	je	.LBB19_257
# BB#253:                               # %if.end897.i
	movl	in_system_header(%rip), %eax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB19_257
# BB#254:                               # %land.lhs.true901.i
	movl	16(%r12), %eax
	andq	$524543, %rax           # imm = 0x800FF
	cmpq	$30, %rax
	jne	.LBB19_257
# BB#255:                               # %land.lhs.true915.i
	movzbl	18(%r15), %eax
	testq	$8, %rax
	je	.LBB19_257
# BB#256:                               # %if.then922.i
	movabsq	$.L.str353, %rsi
	xorq	%rax, %rax
	movq	%r15, %rdi
	callq	warning_with_decl
.LBB19_257:                             # %if.end923.i
	movl	16(%r12), %eax
	movq	%rax, %rcx
	andq	$255, %rcx
	cmpq	$30, %rcx
	je	.LBB19_261
# BB#258:                               # %if.end923.i
	movq	%rax, %rdx
	andq	$4096, %rdx             # imm = 0x1000
	movabsq	$4294967295, %rsi       # imm = 0xFFFFFFFF
	testq	%rsi, %rdx
	jne	.LBB19_261
# BB#259:                               # %land.lhs.true936.i
	movzbl	17(%r15), %edx
	testq	$16, %rdx
	je	.LBB19_261
# BB#260:                               # %if.then943.i
	movabsq	$.L.str354, %rsi
	xorq	%rax, %rax
	movq	%r15, %rdi
	callq	warning_with_decl
	jmp	.LBB19_265
.LBB19_261:                             # %if.else944.i
	movl	pedantic(%rip), %edx
	testq	%rdx, %rdx
	je	.LBB19_265
# BB#262:                               # %if.else944.i
	cmpq	$30, %rcx
	je	.LBB19_265
# BB#263:                               # %land.lhs.true953.i
	movl	16(%r15), %ecx
	xorq	%rax, %rcx
	testq	$6144, %rcx             # imm = 0x1800
	je	.LBB19_265
# BB#264:                               # %if.then979.i
	movabsq	$.L.str355, %rsi
	xorq	%rax, %rax
	movq	%r15, %rdi
	callq	pedwarn_with_decl
	jmp	.LBB19_265
.LBB19_117:                             # %if.then394.i
	movq	%r9, 8(%r12)
	movq	%r9, 64(%rsp)           # 8-byte Spill
	jmp	.LBB19_125
.LBB19_214:
	movabsq	$.L.str344, %rsi
.LBB19_216:                             # %land.end718.i
	xorq	%rax, %rax
	movq	%r12, %rdi
	callq	warning_with_decl
	jmp	.LBB19_265
.LBB19_121:
	movq	%r9, 64(%rsp)           # 8-byte Spill
.LBB19_125:                             # %if.end425.i
	movq	%r10, 72(%rsp)          # 8-byte Spill
	movq	%r8, 56(%rsp)           # 8-byte Spill
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %r8
	jne	.LBB19_265
# BB#126:                               # %if.then427.i
	movabsq	$.L.str337, %rsi
.LBB19_78:                              # %if.end148
	xorq	%rax, %rax
	movq	%r15, %rdi
	callq	warning_with_decl
	jmp	.LBB19_97
.LBB19_131:                             # %if.then450.i
	movq	%r10, 72(%rsp)          # 8-byte Spill
	andq	$2048, %rax             # imm = 0x800
	orq	%rax, %rcx
	movl	%ecx, 16(%r15)
	movq	$0, 56(%rsp)            # 8-byte Folded Spill
	jmp	.LBB19_265
.LBB19_173:                             # %for.cond.preheader.i
	movq	72(%rsp), %rax          # 8-byte Reload
	movq	24(%rax), %r13
	jmp	.LBB19_174
.LBB19_181:                             # %for.inc.i
                                        #   in Loop: Header=BB19_174 Depth=1
	movq	(%r13), %r13
.LBB19_174:                             # %for.cond.preheader.i
                                        # =>This Inner Loop Header: Depth=1
	testq	%r13, %r13
	je	.LBB19_172
# BB#175:                               # %for.body.i933
                                        #   in Loop: Header=BB19_174 Depth=1
	movq	32(%r13), %rdi
	cmpq	$0, (%r13)
	jne	.LBB19_179
# BB#176:                               # %land.lhs.true659.i
                                        #   in Loop: Header=BB19_174 Depth=1
	movq	128(%rdi), %rax
	movq	global_trees+216(%rip), %rcx
	cmpq	%rcx, %rax
	jne	.LBB19_177
.LBB19_179:                             # %if.end665.i
                                        #   in Loop: Header=BB19_174 Depth=1
	callq	simple_type_promotes_to
	testq	%rax, %rax
	je	.LBB19_181
# BB#180:                               # %if.then669.i
	movabsq	$.L.str340, %rdi
	jmp	.LBB19_178
.LBB19_195:                             # %if.end91.i.i
	cmpq	$0, 104(%r12)
	je	.LBB19_197
# BB#196:                               # %land.lhs.true95.i.i
	cmpq	$0, 104(%r15)
	je	.LBB19_197
.LBB19_188:                             # %sw.bb.i
	movabsq	$.L.str341, %rsi
.LBB19_189:                             # %sw.epilog.i
	xorq	%rax, %rax
	movq	%r15, %rdi
	callq	error_with_decl
	cmpq	$0, 104(%r12)
	je	.LBB19_207
# BB#190:                               # %land.rhs683.i
	movq	current_binding_level(%rip), %rax
	movq	global_binding_level(%rip), %rcx
	cmpq	%rcx, %rax
	je	.LBB19_191
.LBB19_207:                             # %select.mid
	movabsq	$.L.str345, %rsi
	jmp	.LBB19_208
.LBB19_191:
	movabsq	$.L.str344, %rsi
	jmp	.LBB19_208
.LBB19_233:                             # %if.then806.i
	movabsq	$.L.str349, %rsi
	xorq	%rax, %rax
	movq	$0, 32(%rsp)            # 8-byte Folded Spill
	movq	%r15, %rdi
	movq	8(%rsp), %rdx           # 8-byte Reload
	callq	error_with_decl
.LBB19_234:                             # %if.end1032.i
	movabsq	$.L.str347, %rsi
	xorq	%rax, %rax
	movq	%r12, %rdi
	callq	error_with_decl
	movq	56(%rsp), %rcx          # 8-byte Reload
	jmp	.LBB19_276
.LBB19_197:                             # %if.end100.i.i
	movl	16(%r12), %edx
	xorq	%rcx, %rdx
	movq	$44, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	$31, %rcx
	shrq	%cl, %rdx
	andq	$3, %rdx
	jmp	.LBB19_164
.LBB19_206:                             # %sw.default.i
	movabsq	$.L.str185, %rdi
	movabsq	$.L__FUNCTION__.duplicate_decls, %rdx
	movq	$1686, %rsi             # imm = 0x696
	callq	fancy_abort
.LBB19_228:                             # %if.then767.i
	movq	%r10, 72(%rsp)          # 8-byte Spill
	movq	%r9, 64(%rsp)           # 8-byte Spill
	movabsq	$.L.str346, %rsi
	xorq	%rax, %rax
	movq	%r15, %rdi
	callq	warning_with_decl
	movabsq	$.L.str347, %rsi
	jmp	.LBB19_216
.LBB19_146:                             # %if.then564.i
	movq	%r10, 72(%rsp)          # 8-byte Spill
	movq	%r9, 64(%rsp)           # 8-byte Spill
	movl	pedantic(%rip), %ecx
	testq	%rcx, %rcx
	je	.LBB19_148
# BB#147:                               # %if.then566.i
	movabsq	$.L.str338, %rcx
	xorq	%rax, %rax
	movq	%r15, %rdi
	movq	%rcx, %rsi
	callq	pedwarn_with_decl
	movq	64(%rsp), %rax          # 8-byte Reload
	movq	8(%rax), %rax
.LBB19_148:                             # %if.end567.i
	movq	8(%rax), %rax
	movq	128(%rax), %rax
	movq	global_trees+216(%rip), %rcx
	cmpq	%rcx, %rax
	jne	.LBB19_150
# BB#149:                               # %if.then576.i
	movq	64(%rsp), %rax          # 8-byte Reload
	movq	%rax, 8(%r15)
	movq	%rax, 72(%rsp)          # 8-byte Spill
.LBB19_150:                             # %if.end579.i
	orq	$65536, 48(%r15)        # imm = 0x10000
	movq	$0, 56(%rsp)            # 8-byte Folded Spill
	jmp	.LBB19_265
.LBB19_177:                             # %if.then664.i
	movabsq	$.L.str339, %rdi
.LBB19_178:                             # %if.end673.i
	xorq	%rax, %rax
	callq	error
.LBB19_172:                             # %if.end673.i
	movabsq	$.L.str139, %rsi
	xorq	%rax, %rax
	movq	%r12, %rdi
	callq	error_with_decl
	movq	48(%rsp), %r13          # 8-byte Reload
.LBB19_265:                             # %if.end989.i
	movl	warn_redundant_decls(%rip), %eax
	testq	%rax, %rax
	je	.LBB19_266
# BB#267:                               # %land.lhs.true994.i
	movq	$1, 32(%rsp)            # 8-byte Folded Spill
	movl	32(%r12), %eax
	testq	%rax, %rax
	je	.LBB19_268
# BB#269:                               # %land.lhs.true999.i
	movzbl	16(%r15), %eax
	cmpq	$30, %rax
	movq	56(%rsp), %rcx          # 8-byte Reload
	jne	.LBB19_272
# BB#270:                               # %land.lhs.true1006.i
	cmpq	$0, 104(%r15)
	je	.LBB19_272
# BB#271:                               # %land.lhs.true1011.i
	cmpq	$0, 104(%r12)
	je	.LBB19_276
.LBB19_272:                             # %land.lhs.true1016.i
	movq	%r13, 48(%rsp)          # 8-byte Spill
	movq	%rcx, %r13
	movzbl	49(%r12), %eax
	testq	$1, %rax
	je	.LBB19_274
# BB#273:                               # %land.lhs.true1023.i
	movzbl	49(%r15), %eax
	testq	$1, %rax
	je	.LBB19_275
.LBB19_274:                             # %if.then1031.i
	movabsq	$.L.str356, %rsi
	xorq	%rax, %rax
	movq	%r15, %rdi
	callq	warning_with_decl
	movabsq	$.L.str139, %rsi
	xorq	%rax, %rax
	movq	%r12, %rdi
	callq	warning_with_decl
.LBB19_275:                             # %if.end1032.i
	movq	%r13, %rcx
	movq	48(%rsp), %r13          # 8-byte Reload
	jmp	.LBB19_276
.LBB19_266:
	movq	$1, 32(%rsp)            # 8-byte Folded Spill
	movq	56(%rsp), %rcx          # 8-byte Reload
	jmp	.LBB19_276
.LBB19_268:
	movq	56(%rsp), %rcx          # 8-byte Reload
.LBB19_276:                             # %if.end1032.i
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %rcx
	movq	64(%rsp), %rsi          # 8-byte Reload
	movq	72(%rsp), %rdi          # 8-byte Reload
	je	.LBB19_327
# BB#277:                               # %if.then1034.i
	testq	%rbx, %rbx
	movq	%r15, %rdx
	jne	.LBB19_279
# BB#278:                               # %if.then1034.i
	movq	%r12, %rdx
.LBB19_279:                             # %if.then1034.i
	movzbl	16(%r15), %eax
	cmpq	$30, %rax
	jne	.LBB19_282
# BB#280:                               # %lor.lhs.false1043.i
	movzbl	51(%r12), %eax
	testq	$96, %rax
	je	.LBB19_282
# BB#281:                               # %lor.lhs.false1043.if.end1085_crit_edge.i
	movq	%rdx, 48(%rsp)          # 8-byte Spill
	movq	%rcx, 56(%rsp)          # 8-byte Spill
	movq	8(%r15), %rax
	jmp	.LBB19_290
.LBB19_282:                             # %if.then1052.i
	movq	%rdx, 48(%rsp)          # 8-byte Spill
	testq	%rbx, %rbx
	je	.LBB19_288
# BB#283:                               # %if.then1054.i
	movq	%rcx, 56(%rsp)          # 8-byte Spill
	cmpq	$0, 24(%rsi)
	je	.LBB19_287
# BB#284:                               # %land.lhs.true1059.i
	cmpq	$0, 24(%rdi)
	je	.LBB19_285
.LBB19_287:                             # %if.else1068.i
	movq	48(%rdi), %rdi
	movq	48(%rsi), %rsi
	callq	merge_attributes
	movq	72(%rsp), %rdi          # 8-byte Reload
	movq	%rax, %rsi
	callq	build_type_attribute_variant
	movq	64(%rsp), %rsi          # 8-byte Reload
	jmp	.LBB19_289
.LBB19_327:                             # %if.else1429.i
	movq	%rcx, 56(%rsp)          # 8-byte Spill
	testq	%rbx, %rbx
	jne	.LBB19_329
# BB#328:                               # %if.then1431.i
	movq	8(%r15), %rax
	movq	%rax, 8(%r12)
	movl	16(%r15), %eax
	movl	16(%r12), %ecx
	andq	$4096, %rax             # imm = 0x1000
	andq	$-4097, %rcx            # imm = 0xFFFFFFFFFFFFEFFF
	orq	%rax, %rcx
	movl	%ecx, 16(%r12)
	movl	16(%r15), %eax
	andq	$2048, %rax             # imm = 0x800
	andq	$-2049, %rcx            # imm = 0xFFFFFFFFFFFFF7FF
	orq	%rax, %rcx
	movl	%ecx, 16(%r12)
	movl	16(%r15), %eax
	andq	$256, %rax              # imm = 0x100
	movabsq	$4294967039, %rdx       # imm = 0xFFFFFEFF
	andq	%rcx, %rdx
	orq	%rax, %rdx
	movl	%edx, 16(%r12)
	jmp	.LBB19_329
.LBB19_288:                             # %if.else1078.i
	movq	%rcx, 56(%rsp)          # 8-byte Spill
	callq	common_type
	movq	64(%rsp), %rsi          # 8-byte Reload
	movq	%rax, 8(%r12)
	jmp	.LBB19_289
.LBB19_285:                             # %if.then1064.i
	callq	common_type
	movq	64(%rsp), %rsi          # 8-byte Reload
.LBB19_289:                             # %if.end1085.i
	movq	%rax, 8(%r15)
.LBB19_290:                             # %if.end1085.i
	cmpq	%rax, %rsi
	je	.LBB19_296
# BB#291:                               # %if.then1090.i
	movq	global_trees(%rip), %rcx
	cmpq	%rcx, %rax
	je	.LBB19_293
# BB#292:                               # %if.then1095.i
	movq	%rax, %rdi
	callq	layout_type
.LBB19_293:                             # %if.end1098.i
	movzbl	16(%r15), %ecx
	cmpq	$33, %rcx
	ja	.LBB19_295
# BB#294:                               # %if.end1098.i
	movq	$1, %rax
	shlq	%cl, %rax
	movabsq	$13958643712, %rcx      # imm = 0x340000000
	testq	%rcx, %rax
	jne	.LBB19_299
.LBB19_295:                             # %if.then1119.i
	xorq	%rsi, %rsi
	movq	%r15, %rdi
	callq	layout_decl
	jmp	.LBB19_299
.LBB19_296:                             # %if.else1121.i
	movq	40(%r12), %rax
	movq	%rax, 40(%r15)
	movq	64(%r12), %rax
	movq	%rax, 64(%r15)
	movq	48(%r15), %rdx
	movzbl	48(%r12), %ecx
	movq	%rdx, %rax
	andq	$-256, %rax
	orq	%rcx, %rax
	movq	%rax, 48(%r15)
	movzbl	16(%r12), %ecx
	cmpq	$30, %rcx
	je	.LBB19_299
# BB#297:                               # %if.then1145.i
	movl	56(%r15), %ecx
	movl	56(%r12), %esi
	andq	$16777215, %rsi         # imm = 0xFFFFFF
	movq	%rcx, %rdi
	andq	$16777215, %rdi         # imm = 0xFFFFFF
	cmpq	%rdi, %rsi
	jbe	.LBB19_299
# BB#298:                               # %if.then1156.i
	movabsq	$4278190080, %rdi       # imm = 0xFF000000
	andq	%rdi, %rcx
	orq	%rsi, %rcx
	movl	%ecx, 56(%r15)
	movl	56(%r12), %esi
	movq	$35, %rcx
	shrq	%cl, %rdx
	orq	%rsi, %rdx
	andq	$1, %rdx
	shlq	$35, %rdx
	movabsq	$-34359738369, %rcx     # imm = 0xFFFFFFF7FFFFFFFF
	andq	%rcx, %rax
	orq	%rdx, %rax
	movq	%rax, 48(%r15)
.LBB19_299:                             # %if.end1188.i
	movq	144(%r12), %rax
	movq	%rax, 144(%r15)
	movl	16(%r12), %eax
	andq	$10495, %rax            # imm = 0x28FF
	cmpq	$10270, %rax            # imm = 0x281E
	movq	48(%rsp), %rsi          # 8-byte Reload
	jne	.LBB19_302
# BB#300:                               # %land.lhs.true1212.i
	movzbl	17(%r15), %eax
	testq	$8, %rax
	jne	.LBB19_302
# BB#301:                               # %if.then1219.i
	movl	16(%rsi), %eax
	movabsq	$4294965247, %rcx       # imm = 0xFFFFF7FF
	andq	%rax, %rcx
	movl	%ecx, 16(%rsi)
.LBB19_302:                             # %if.end1224.i
	movl	16(%r15), %eax
	testq	$4096, %rax             # imm = 0x1000
	je	.LBB19_304
# BB#303:                               # %if.then1231.i
	movl	16(%rsi), %eax
	orq	$4096, %rax             # imm = 0x1000
	movl	%eax, 16(%rsi)
	movl	16(%r15), %eax
.LBB19_304:                             # %if.end1237.i
	testq	$2048, %rax             # imm = 0x800
	je	.LBB19_310
# BB#305:                               # %if.then1244.i
	movl	16(%rsi), %eax
	orq	$2048, %rax             # imm = 0x800
	movl	%eax, 16(%rsi)
	movzbl	16(%r15), %eax
	cmpq	$34, %rax
	movq	32(%rsp), %rcx          # 8-byte Reload
	jne	.LBB19_306
# BB#307:                               # %if.then1244.i
	movabsq	$1, %rax
	jmp	.LBB19_308
.LBB19_306:
	movabsq	$0, %rax
.LBB19_308:                             # %if.then1244.i
	andq	%rcx, %rax
	cmpq	$1, %rax
	jne	.LBB19_310
# BB#309:                               # %if.then1259.i
	movq	%r15, %rdi
	callq	make_var_volatile
	movq	48(%rsp), %rsi          # 8-byte Reload
.LBB19_310:                             # %if.end1261.i
	movq	104(%r15), %rax
	testq	%rax, %rax
	jne	.LBB19_314
# BB#311:                               # %land.lhs.true1266.i
	cmpq	$0, 104(%r12)
	je	.LBB19_314
# BB#312:                               # %land.lhs.true1266.i
	testq	%rbx, %rbx
	jne	.LBB19_314
# BB#313:                               # %if.then1273.i
	movl	32(%r12), %ecx
	movl	%ecx, 32(%r15)
	movq	24(%r12), %rcx
	movq	%rcx, 24(%r15)
.LBB19_314:                             # %if.end1281.i
	movzbl	50(%r12), %edx
	movq	48(%r15), %rcx
	testq	$1, %rdx
	je	.LBB19_316
# BB#315:                               # %if.then1289.i
	orq	$65536, %rcx            # imm = 0x10000
	movq	%rcx, 48(%r15)
	jmp	.LBB19_318
.LBB19_316:                             # %if.else1295.i
	testq	$65536, %rcx            # imm = 0x10000
	je	.LBB19_318
# BB#317:                               # %if.then1303.i
	orq	$65536, 48(%rsi)        # imm = 0x10000
	movq	104(%r15), %rax
.LBB19_318:                             # %if.end1310.i
	testq	%rax, %rax
	jne	.LBB19_321
# BB#319:                               # %if.end1310.i
	testq	%rbx, %rbx
	jne	.LBB19_321
# BB#320:                               # %if.then1317.i
	movq	104(%r12), %rax
	movq	%rax, 104(%r15)
.LBB19_321:                             # %if.end1322.i
	cmpq	$0, 128(%r15)
	jne	.LBB19_323
# BB#322:                               # %if.then1326.i
	movq	128(%r12), %rax
	movq	%rax, 128(%r15)
.LBB19_323:                             # %if.end1331.i
	movq	120(%r12), %rax
	testq	%rax, %rax
	je	.LBB19_325
# BB#324:                               # %cond.end1342.i
	movq	%rax, 120(%r15)
.LBB19_325:                             # %cond.end1348.i
	movzbl	16(%r15), %eax
	cmpq	$30, %rax
	jne	.LBB19_329
# BB#326:                               # %if.then1355.i
	movq	48(%r12), %rax
	movq	48(%r15), %rcx
	andq	$1048576, %rax          # imm = 0x100000
	orq	%rcx, %rax
	movq	%rax, 48(%r15)
	movq	48(%r12), %rdx
	orq	%rcx, %rdx
	andq	$2097152, %rdx          # imm = 0x200000
	andq	$-2097153, %rax         # imm = 0xFFFFFFFFFFDFFFFF
	orq	%rdx, %rax
	movq	%rax, 48(%r15)
	movq	48(%r12), %rdx
	orq	%rcx, %rdx
	andq	$268435456, %rdx        # imm = 0x10000000
	andq	$-268435457, %rax       # imm = 0xFFFFFFFFEFFFFFFF
	orq	%rdx, %rax
	movq	%rax, 48(%r15)
	movq	48(%r12), %rdx
	orq	%rcx, %rdx
	andq	$33554432, %rdx         # imm = 0x2000000
	andq	$-33554433, %rax        # imm = 0xFFFFFFFFFDFFFFFF
	orq	%rdx, %rax
	movq	%rax, 48(%r15)
.LBB19_329:                             # %if.end1472.i
	movq	%r15, %rdi
	movq	%r12, %rsi
	callq	merge_weak
	movl	16(%r15), %ecx
	movq	%rcx, %rax
	andq	$255, %rax
	cmpq	$30, %rax
	jne	.LBB19_332
# BB#330:                               # %if.then1479.i
	movl	16(%r12), %edx
	movabsq	$4294443007, %rax       # imm = 0xFFF7FFFF
	orq	%rax, %rdx
	andq	%rcx, %rdx
	movl	%edx, 16(%r15)
	movl	16(%r12), %ecx
	movq	%rdx, %rsi
	andq	$524288, %rsi           # imm = 0x80000
	andq	%rax, %rcx
	orq	%rsi, %rcx
	andq	$524288, %rdx           # imm = 0x80000
	movl	%ecx, 16(%r12)
	jne	.LBB19_332
# BB#331:                               # %if.then1513.i
	movq	72(%r12), %rcx
	movl	16(%rcx), %edx
	andq	%rax, %rdx
	movl	%edx, 16(%rcx)
.LBB19_332:                             # %if.end1521.i
	movq	48(%r15), %rcx
	testq	$256, %rcx              # imm = 0x100
	je	.LBB19_338
# BB#333:                               # %if.then1529.i
	testq	%rbx, %rbx
	je	.LBB19_335
# BB#334:                               # %if.then1529.if.end1557_crit_edge.i
	movl	16(%r15), %eax
	jmp	.LBB19_336
.LBB19_338:                             # %if.else1582.i
	movl	16(%r15), %eax
	movl	16(%r12), %ecx
	andq	$262144, %rax           # imm = 0x40000
	andq	$-262145, %rcx          # imm = 0xFFFFFFFFFFFBFFFF
	orq	%rax, %rcx
	movl	%ecx, 16(%r12)
	movl	16(%r15), %eax
	andq	$524288, %rax           # imm = 0x80000
	movabsq	$4294443007, %rdx       # imm = 0xFFF7FFFF
	andq	%rcx, %rdx
	orq	%rax, %rdx
	movl	%edx, 16(%r12)
	movl	16(%r15), %eax
	jmp	.LBB19_339
.LBB19_335:                             # %if.then1531.i
	movl	16(%r12), %edx
	movl	16(%r15), %eax
	andq	$262144, %rdx           # imm = 0x40000
	andq	$-262145, %rax          # imm = 0xFFFFFFFFFFFBFFFF
	orq	%rdx, %rax
	movl	%eax, 16(%r15)
	movq	48(%r12), %rdx
	andq	$256, %rdx              # imm = 0x100
	andq	$-257, %rcx             # imm = 0xFFFFFFFFFFFFFEFF
	orq	%rdx, %rcx
	movq	%rcx, 48(%r15)
.LBB19_336:                             # %if.end1557.i
	movl	16(%r12), %edx
	andq	$524288, %rdx           # imm = 0x80000
	andq	$-524289, %rax          # imm = 0xFFFFFFFFFFF7FFFF
	orq	%rdx, %rax
	movl	%eax, 16(%r15)
	testq	$256, %rcx              # imm = 0x100
	jne	.LBB19_339
# BB#337:                               # %if.then1577.i
	movq	80(%r12), %rcx
	movq	%rcx, 80(%r15)
.LBB19_339:                             # %if.end1607.i
	andq	$255, %rax
	cmpq	$30, %rax
	jne	.LBB19_373
# BB#340:                               # %if.then1614.i
	movabsq	$-68719476737, %r8      # imm = 0xFFFFFFEFFFFFFFFF
	cmpq	$0, 40(%rsp)            # 8-byte Folded Reload
	je	.LBB19_344
# BB#341:                               # %land.lhs.true1616.i
	cmpq	$0, 104(%r12)
	je	.LBB19_344
# BB#342:                               # %land.lhs.true1620.i
	movzbl	18(%r12), %eax
	testq	$1, %rax
	je	.LBB19_344
# BB#343:                               # %if.then1627.i
	movq	debug_hooks(%rip), %rax
	movq	%r12, %rdi
	callq	*144(%rax)
	movq	48(%r15), %rax
	movabsq	$-68719476737, %rcx     # imm = 0xFFFFFFEFFFFFFFFF
	addq	$-2048, %rcx            # imm = 0xFFFFFFFFFFFFF800
	andq	%rax, %rcx
	movabsq	$68719476736, %rax      # imm = 0x1000000000
	orq	%rax, %rcx
	leaq	48(%r12), %rsi
	movq	%rcx, %rax
	movq	56(%rsp), %rdi          # 8-byte Reload
	jmp	.LBB19_352
.LBB19_344:                             # %if.else1636.i
	movq	200(%r15), %rax
	movzbl	(%rax), %ecx
	testq	$1, %rcx
	movq	56(%rsp), %rdi          # 8-byte Reload
	jne	.LBB19_346
# BB#345:                               # %lor.lhs.false1644.i
	movq	200(%r12), %rdx
	movzbl	(%rdx), %edx
	testq	$1, %rdx
	je	.LBB19_347
.LBB19_346:                             # %if.then1652.i
	orq	$1, %rcx
	movb	%cl, (%rax)
.LBB19_347:                             # %if.end1659.i
	movq	48(%r15), %rcx
	movq	48(%r12), %rax
	movabsq	$68719476736, %rdx      # imm = 0x1000000000
	testq	%rdx, %rcx
	movq	$1, %rdx
	jne	.LBB19_351
# BB#348:                               # %lor.rhs.i
	movabsq	$68719476736, %rcx      # imm = 0x1000000000
	testq	%rcx, %rax
	je	.LBB19_349
# BB#350:                               # %lor.rhs.i
	movabsq	$1, %rdx
	jmp	.LBB19_351
.LBB19_349:
	movabsq	$0, %rdx
.LBB19_351:                             # %lor.end.i
	leaq	48(%r12), %rsi
	movq	$36, %rcx
	shlq	%cl, %rdx
	andq	%r8, %rax
	orq	%rdx, %rax
	movq	%rax, (%rsi)
	movq	48(%r15), %rax
	andq	%r8, %rax
	orq	%rdx, %rax
.LBB19_352:                             # %if.end1690.i
	movq	%rax, 48(%r15)
	movq	(%rsi), %rcx
	movq	%rcx, %rdx
	andq	$1610612736, %rdx       # imm = 0x60000000
	je	.LBB19_360
# BB#353:                               # %if.then1699.i
	movabsq	$4294967295, %rsi       # imm = 0xFFFFFFFF
	testq	%rsi, %rdi
	jne	.LBB19_354
# BB#355:                               # %if.then1699.i
	movabsq	$1, %rsi
	jmp	.LBB19_356
.LBB19_354:
	movabsq	$0, %rsi
.LBB19_356:                             # %if.then1699.i
	movq	40(%rsp), %rdi          # 8-byte Reload
	orq	%rdi, %rsi
	cmpq	$1, %rsi
	jne	.LBB19_359
# BB#357:                               # %if.then1703.i
	testq	%rbx, %rbx
	jne	.LBB19_360
# BB#358:                               # %if.then1705.i
	movq	8(%r15), %rax
	movq	%rax, 8(%r12)
	andq	$-1610612737, %rcx      # imm = 0xFFFFFFFF9FFFFFFF
	movq	%rcx, 48(%r12)
	jmp	.LBB19_360
.LBB19_359:                             # %if.else1715.i
	andq	$-1610612737, %rax      # imm = 0xFFFFFFFF9FFFFFFF
	orq	%rax, %rdx
	movq	%rdx, 48(%r15)
	movl	56(%r12), %eax
	movl	%eax, 56(%r15)
.LBB19_360:                             # %if.end1736.i
	cmpq	$0, 40(%rsp)            # 8-byte Folded Reload
	je	.LBB19_361
# BB#370:                               # %if.else1804.i
	movq	200(%r15), %rax
	movzbl	(%rax), %eax
	testq	$1, %rax
	je	.LBB19_373
# BB#371:                               # %land.lhs.true1812.i
	movq	48(%r15), %rax
	movabsq	$68719476736, %rcx      # imm = 0x1000000000
	testq	%rcx, %rax
	jne	.LBB19_373
# BB#372:                               # %if.then1820.i
	orq	$2048, %rax             # imm = 0x800
	movq	%rax, 48(%r15)
	jmp	.LBB19_373
.LBB19_361:                             # %if.then1738.i
	movq	96(%r12), %rax
	testq	%rbx, %rbx
	movq	%rax, 96(%r15)
	jne	.LBB19_363
# BB#362:                               # %if.then1743.i
	movq	104(%r12), %rax
	movq	%rax, 104(%r15)
.LBB19_363:                             # %if.end1748.i
	movq	160(%r12), %rax
	movq	%rax, 160(%r15)
	movq	168(%r12), %rax
	movq	%rax, 168(%r15)
	movq	56(%r12), %rax
	movq	%rax, 56(%r15)
	movq	88(%r12), %rax
	movq	%rax, 88(%r15)
	movzbl	49(%r12), %eax
	testq	$8, %rax
	je	.LBB19_373
# BB#364:                               # %land.lhs.true1772.i
	movq	48(%r15), %rax
	movabsq	$68719476736, %rcx      # imm = 0x1000000000
	testq	%rcx, %rax
	jne	.LBB19_373
# BB#365:                               # %if.then1780.i
	orq	$2048, %rax             # imm = 0x800
	movq	%rax, 48(%r15)
	movq	112(%r12), %rax
	testq	%rbx, %rbx
	je	.LBB19_369
# BB#366:                               # %cond.true1787.i
	testq	%rax, %rax
	movq	%r12, %rcx
	je	.LBB19_368
# BB#367:                               # %cond.true1787.i
	movq	%rax, %rcx
.LBB19_368:                             # %cond.true1787.i
	movq	%rcx, %rax
.LBB19_369:                             # %cond.end1799.i
	movq	%rax, 112(%r15)
.LBB19_373:                             # %if.end1828.i
	testq	%rbx, %rbx
	je	.LBB19_374
.LBB19_97:                              # %if.end148
	movzbl	16(%r15), %eax
	cmpq	$33, %rax
	jne	.LBB19_381
# BB#98:                                # %if.then154
	movl	32(%r15), %eax
	movq	8(%r15), %rdi
	testq	%rax, %rax
	je	.LBB19_99
# BB#378:                               # %if.else
	movq	global_trees(%rip), %rax
	cmpq	%rax, %rdi
	je	.LBB19_381
# BB#379:                               # %land.lhs.true172
	cmpq	$0, 96(%r15)
	jne	.LBB19_381
# BB#380:                               # %if.then175
	movq	%rdi, 96(%r15)
	callq	build_type_copy
	movq	%r15, 96(%rax)
	movl	16(%r15), %ecx
	movl	16(%rax), %edx
	andq	$65536, %rcx            # imm = 0x10000
	movabsq	$4294901759, %rsi       # imm = 0xFFFEFFFF
	andq	%rdx, %rsi
	orq	%rcx, %rsi
	movl	%esi, 16(%rax)
	movq	%rax, 8(%r15)
	jmp	.LBB19_381
.LBB19_99:                              # %if.then158
	cmpq	$0, 96(%rdi)
	jne	.LBB19_381
# BB#100:                               # %if.then163
	movq	%r15, 96(%rdi)
.LBB19_381:                             # %if.end198
	movl	16(%r15), %eax
	testq	$524288, %rax           # imm = 0x80000
	je	.LBB19_392
# BB#382:                               # %land.lhs.true205
	andq	$255, %rax
	cmpq	$30, %rax
	jne	.LBB19_384
# BB#383:                               # %land.lhs.true211
	movzbl	49(%r15), %eax
	testq	$8, %rax
	jne	.LBB19_392
.LBB19_384:                             # %if.then218
	movl	flag_traditional(%rip), %eax
	testq	%rax, %rax
	je	.LBB19_388
# BB#385:                               # %land.lhs.true221
	movq	56(%r14), %rbx
	testq	%rbx, %rbx
	je	.LBB19_388
# BB#386:                               # %land.lhs.true224
	movzbl	49(%rbx), %eax
	testq	$1, %rax
	jne	.LBB19_389
# BB#387:                               # %lor.lhs.false233
	movzbl	18(%rbx), %eax
	testq	$8, %rax
	jne	.LBB19_389
.LBB19_388:                             # %if.else243
	movq	96(%r14), %rbx
	testq	%rbx, %rbx
	je	.LBB19_392
.LBB19_389:                             # %land.lhs.true252
	movq	8(%r15), %rdi
	movq	8(%rbx), %rsi
	callq	comptypes
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB19_392
# BB#390:                               # %land.lhs.true259
	movzbl	51(%rbx), %eax
	testq	$96, %rax
	jne	.LBB19_392
# BB#391:                               # %if.then266
	movabsq	$.L.str140, %rsi
	xorq	%rax, %rax
	movq	%r15, %rdi
	callq	pedwarn_with_decl
	movabsq	$.L.str141, %rsi
	xorq	%rax, %rax
	movq	%rbx, %rdi
	callq	pedwarn_with_decl
.LBB19_392:                             # %if.end268
	movq	80(%r14), %rax
	testq	%rax, %rax
	je	.LBB19_397
# BB#393:                               # %land.lhs.true271
	cmpq	$0, 56(%r14)
	jne	.LBB19_397
# BB#394:                               # %land.lhs.true274
	movzbl	16(%r15), %ecx
	cmpq	$30, %rcx
	jne	.LBB19_397
# BB#395:                               # %land.lhs.true280
	movq	8(%r15), %rdi
	movq	8(%rax), %rsi
	callq	comptypes
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB19_397
# BB#396:                               # %if.then288
	movabsq	$.L.str142, %rsi
	xorq	%rax, %rax
	movq	%r15, %rdi
	callq	warning_with_decl
	movq	80(%r14), %rdi
	movabsq	$.L.str143, %rsi
	xorq	%rax, %rax
	callq	warning_with_decl
.LBB19_397:                             # %if.end290
	movl	flag_traditional(%rip), %eax
	testq	%rax, %rax
	je	.LBB19_419
# BB#398:                               # %land.lhs.true292
	movzbl	49(%r15), %eax
	testq	$1, %rax
	je	.LBB19_419
# BB#399:                               # %land.lhs.true300
	movq	current_binding_level(%rip), %rcx
	movq	global_binding_level(%rip), %rax
	cmpq	%rax, %rcx
	je	.LBB19_401
# BB#400:                               # %land.lhs.true.i939
	cmpq	$0, 64(%r14)
	jne	.LBB19_419
.LBB19_401:                             # %lookup_name.exit
	cmpq	$0, 56(%r14)
	jne	.LBB19_419
# BB#402:                               # %while.cond.preheader
	movq	8(%r15), %rcx
	testq	%rcx, %rcx
	jne	.LBB19_403
# BB#404:                               # %while.cond.preheader
	movabsq	$1, %rbx
	jmp	.LBB19_405
.LBB19_374:                             # %if.end1831.i
	movl	36(%r12), %ebx
	leaq	24(%r12), %rdi
	leaq	24(%r15), %rsi
	movq	$184, %rdx
	callq	memcpy
	movl	%ebx, 36(%r12)
	movq	136(%r15), %rax
	movq	%rax, 136(%r12)
	movl	16(%r12), %eax
.LBB19_375:                             # %if.then131
	movq	%rax, %rcx
	andq	$255, %rcx
	cmpq	$35, %rcx
	jne	.LBB19_376
# BB#377:                               # %if.then137
	movl	16(%r15), %ecx
	andq	$16384, %rcx            # imm = 0x4000
	movabsq	$4294950911, %rdx       # imm = 0xFFFFBFFF
	andq	%rdx, %rax
	orq	%rcx, %rax
	movl	%eax, 16(%r12)
	movq	%r12, %r15
	jmp	.LBB19_503
.LBB19_376:
	movq	%r12, %r15
	jmp	.LBB19_503
.LBB19_403:
	movabsq	$0, %rbx
.LBB19_405:                             # %while.cond.preheader
	je	.LBB19_416
# BB#406:                               # %while.cond.preheader
	movq	global_trees(%rip), %rdx
	cmpq	%rdx, %rcx
	je	.LBB19_416
# BB#407:
	movabsq	$0, %rsi
	movabsq	$1, %rdi
	.align	16, 0x90
.LBB19_408:                             # %if.end310
                                        # =>This Inner Loop Header: Depth=1
	cmpq	$0, 144(%rcx)
	jne	.LBB19_409
# BB#410:                               # %if.else315
                                        #   in Loop: Header=BB19_408 Depth=1
	movzbl	16(%rcx), %ebx
	cmpq	$23, %rbx
	jne	.LBB19_412
# BB#411:                               # %land.lhs.true321
                                        #   in Loop: Header=BB19_408 Depth=1
	xorq	%rbx, %rbx
	cmpq	$0, 24(%rcx)
	jne	.LBB19_416
.LBB19_412:                             # %while.cond.backedge
                                        #   in Loop: Header=BB19_408 Depth=1
	movq	8(%rcx), %rcx
	testq	%rcx, %rcx
	movq	%rsi, %rbx
	jne	.LBB19_414
# BB#413:                               # %while.cond.backedge
                                        #   in Loop: Header=BB19_408 Depth=1
	movq	%rdi, %rbx
.LBB19_414:                             # %while.cond.backedge
                                        #   in Loop: Header=BB19_408 Depth=1
	je	.LBB19_416
# BB#415:                               # %while.cond.backedge
                                        #   in Loop: Header=BB19_408 Depth=1
	cmpq	%rdx, %rcx
	jne	.LBB19_408
	jmp	.LBB19_416
.LBB19_409:                             # %if.then314
	movabsq	$.L.str144, %rsi
	xorq	%rax, %rax
	xorq	%rbx, %rbx
	movq	%r15, %rdi
	callq	warning_with_decl
	movq	global_binding_level(%rip), %rax
.LBB19_416:                             # %while.end
	testq	%rbx, %rbx
	jne	.LBB19_418
# BB#417:                               # %while.end
	movq	%r13, %rax
.LBB19_418:                             # %while.end
	movq	%rax, %r13
.LBB19_419:                             # %if.end332
	movq	global_binding_level(%rip), %rax
	cmpq	%rax, %r13
	je	.LBB19_420
# BB#444:                               # %if.else483
	movq	64(%r14), %rbx
	movq	56(%r14), %r12
	testq	%rbx, %rbx
	movq	%r15, 64(%r14)
	jne	.LBB19_463
# BB#445:                               # %if.else483
	testq	%r12, %r12
	je	.LBB19_463
# BB#446:                               # %land.lhs.true489
	movzbl	16(%r15), %eax
	cmpq	$30, %rax
	jne	.LBB19_463
# BB#447:                               # %land.lhs.true495
	movzbl	16(%r12), %eax
	cmpq	$30, %rax
	jne	.LBB19_463
# BB#448:                               # %land.lhs.true501
	movzbl	49(%r15), %eax
	testq	$1, %rax
	je	.LBB19_463
# BB#449:                               # %land.lhs.true509
	movq	200(%r15), %rax
	movzbl	(%rax), %eax
	testq	$1, %rax
	jne	.LBB19_463
# BB#450:                               # %if.then516
	movq	8(%r15), %rdi
	movq	8(%r12), %rsi
	callq	comptypes
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB19_451
# BB#452:                               # %if.else525
	movq	200(%r12), %rax
	movzbl	(%rax), %eax
	andq	$1, %rax
	je	.LBB19_453
# BB#454:                               # %if.then533
	movq	200(%r15), %rcx
	movzbl	(%rcx), %edx
	andq	$254, %rdx
	orq	%rax, %rdx
	movb	%dl, (%rcx)
	movq	48(%r12), %rax
	movq	48(%r15), %rcx
	andq	$2048, %rax             # imm = 0x800
	andq	$-2049, %rcx            # imm = 0xFFFFFFFFFFFFF7FF
	orq	%rax, %rcx
	movq	%rcx, 48(%r15)
	movq	current_function_decl(%rip), %rdx
	leaq	48(%r12), %rax
	xorq	%rcx, %rcx
	cmpq	%r12, %rdx
	je	.LBB19_456
# BB#455:                               # %cond.false
	movq	104(%r12), %rcx
.LBB19_456:                             # %cond.end
	movq	%rcx, 104(%r15)
	movq	160(%r12), %rcx
	movq	%rcx, 160(%r15)
	movq	56(%r12), %rcx
	movq	%rcx, 56(%r15)
	movq	88(%r12), %rcx
	movq	96(%r12), %rdx
	movq	%rdx, 96(%r15)
	movq	%rcx, 88(%r15)
	movl	16(%r12), %ecx
	movl	16(%r15), %edx
	andq	$16384, %rcx            # imm = 0x4000
	movabsq	$4294950911, %rsi       # imm = 0xFFFFBFFF
	andq	%rdx, %rsi
	orq	%rcx, %rsi
	movl	%esi, 16(%r15)
	movq	112(%r12), %rcx
	movq	%rcx, 112(%r15)
	jmp	.LBB19_457
.LBB19_420:                             # %if.then334
	cmpq	$0, 56(%r14)
	jne	.LBB19_423
# BB#421:                               # %land.lhs.true337
	movzbl	18(%r15), %eax
	testq	$8, %rax
	je	.LBB19_423
# BB#422:                               # %if.then344
	movl	16(%r14), %eax
	orq	$524288, %rax           # imm = 0x80000
	movl	%eax, 16(%r14)
.LBB19_423:                             # %if.end350
	movq	%r15, 56(%r14)
	movq	$0, 96(%r14)
	movq	80(%r14), %rax
	testq	%rax, %rax
	je	.LBB19_432
# BB#424:                               # %land.lhs.true355
	movzbl	18(%rax), %ecx
	testq	$1, %rcx
	je	.LBB19_426
# BB#425:                               # %if.end374
	movl	16(%r15), %eax
	orq	$65536, %rax            # imm = 0x10000
	movl	%eax, 16(%r15)
	movl	16(%r14), %eax
	orq	$65536, %rax            # imm = 0x10000
	movl	%eax, 16(%r14)
	movq	80(%r14), %rax
	testq	%rax, %rax
	je	.LBB19_432
.LBB19_426:                             # %land.lhs.true377
	movzbl	17(%rax), %ecx
	testq	$4, %rcx
	je	.LBB19_428
# BB#427:                               # %if.then384
	movl	16(%r15), %eax
	orq	$1024, %rax             # imm = 0x400
	movl	%eax, 16(%r15)
	movq	80(%r14), %rax
.LBB19_428:                             # %if.end390
	testq	%rax, %rax
	je	.LBB19_432
# BB#429:                               # %land.lhs.true393
	movzbl	16(%r15), %eax
	cmpq	$30, %rax
	jne	.LBB19_431
# BB#430:                               # %land.lhs.true399
	movq	8(%r15), %rax
	movq	8(%rax), %rax
	movq	128(%rax), %rax
	movq	integer_types+40(%rip), %rcx
	cmpq	%rcx, %rax
	je	.LBB19_432
.LBB19_431:                             # %if.then406
	movq	32(%r14), %rsi
	movabsq	$.L.str145, %rdi
	xorq	%rax, %rax
	callq	pedwarn
.LBB19_432:                             # %if.end410
	movl	16(%r14), %eax
	testq	$524288, %rax           # imm = 0x80000
	je	.LBB19_496
# BB#433:                               # %land.lhs.true417
	movl	16(%r15), %ecx
	testq	$524288, %rcx           # imm = 0x80000
	jne	.LBB19_496
# BB#434:                               # %land.lhs.true424
	movzbl	49(%r15), %edx
	testq	$1, %rdx
	jne	.LBB19_496
# BB#435:                               # %if.then432
	testq	%r12, %r12
	je	.LBB19_438
# BB#436:                               # %land.lhs.true434
	movzbl	51(%r12), %edx
	testq	$96, %rdx
	jne	.LBB19_496
# BB#437:                               # %land.lhs.true445
	movzbl	17(%r12), %edx
	testq	$32, %rdx
	jne	.LBB19_496
.LBB19_438:                             # %if.else452
	andq	$255, %rcx
	cmpq	$33, %rcx
	je	.LBB19_496
# BB#439:                               # %if.else459
	cmpq	$0, 80(%r14)
	je	.LBB19_443
# BB#440:                               # %if.then462
	testq	$2048, %rax             # imm = 0x800
	jne	.LBB19_496
# BB#441:                               # %if.then469
	movq	32(%r14), %rsi
	movabsq	$.L.str138, %rdi
	jmp	.LBB19_442
.LBB19_451:                             # %if.then524
	movabsq	$.L.str147, %rsi
	xorq	%rax, %rax
	movq	%r15, %rdi
	callq	pedwarn_with_decl
	jmp	.LBB19_463
.LBB19_453:                             # %if.else525.if.end596_crit_edge
	leaq	48(%r12), %rax
.LBB19_457:                             # %if.end596
	movq	(%rax), %rax
	andq	$1610612736, %rax       # imm = 0x60000000
	je	.LBB19_459
# BB#458:                               # %if.then604
	movq	48(%r15), %rcx
	andq	$-1610612737, %rcx      # imm = 0xFFFFFFFF9FFFFFFF
	orq	%rax, %rcx
	movq	%rcx, 48(%r15)
	movl	56(%r12), %eax
	movl	%eax, 56(%r15)
.LBB19_459:                             # %if.end625
	movq	8(%r12), %rax
	cmpq	$0, 24(%rax)
	je	.LBB19_463
# BB#460:                               # %land.lhs.true631
	cmpq	$0, 104(%r12)
	je	.LBB19_463
# BB#461:                               # %land.lhs.true635
	movq	8(%r15), %rcx
	cmpq	$0, 24(%rcx)
	jne	.LBB19_463
# BB#462:                               # %if.then641
	movq	%rax, 8(%r15)
.LBB19_463:                             # %if.end648
	testq	%rbx, %rbx
	jne	.LBB19_470
# BB#464:                               # %land.lhs.true650
	movzbl	49(%r15), %eax
	testq	$1, %rax
	je	.LBB19_470
# BB#465:                               # %land.lhs.true658
	movzbl	18(%r15), %eax
	testq	$8, %rax
	je	.LBB19_470
# BB#466:                               # %if.then665
	testq	%r12, %r12
	jne	.LBB19_468
# BB#467:                               # %if.then667
	movl	16(%r14), %eax
	orq	$524288, %rax           # imm = 0x80000
	movl	%eax, 16(%r14)
.LBB19_468:                             # %if.end673
	cmpq	$0, 96(%r14)
	jne	.LBB19_470
# BB#469:                               # %if.then676
	movq	%r15, 96(%r14)
.LBB19_470:                             # %if.end679
	movzbl	49(%r15), %eax
	testq	$1, %rax
	jne	.LBB19_494
# BB#471:                               # %if.end.i916
	movq	72(%r15), %r12
	testq	%rbx, %rbx
	je	.LBB19_479
# BB#472:                               # %land.lhs.true.i
	movq	current_binding_level(%rip), %rax
	movzbl	48(%rax), %ecx
	testq	%rcx, %rcx
	jne	.LBB19_479
# BB#473:                               # %land.lhs.true4.i
	movq	40(%rax), %rax
	movzbl	48(%rax), %ecx
	testq	%rcx, %rcx
	je	.LBB19_479
# BB#474:                               # %land.lhs.true7.i
	movq	(%rax), %rsi
	movq	%rbx, %rdi
	callq	chain_member
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB19_479
# BB#475:                               # %if.then10.i
	movzbl	16(%rbx), %eax
	movq	32(%r12), %rsi
	cmpq	$35, %rax
	jne	.LBB19_478
# BB#476:                               # %if.then15.i
	movabsq	$.L.str323, %rdi
	jmp	.LBB19_477
.LBB19_479:                             # %if.else20.i
	movl	warn_shadow(%rip), %eax
	testq	%rax, %rax
	je	.LBB19_494
# BB#480:                               # %land.lhs.true22.i
	movl	32(%r15), %eax
	testq	%rax, %rax
	je	.LBB19_494
# BB#481:                               # %land.lhs.true26.i
	movq	112(%r15), %rax
	testq	%rax, %rax
	je	.LBB19_483
# BB#482:                               # %land.lhs.true26.i
	cmpq	%r15, %rax
	jne	.LBB19_494
.LBB19_483:                             # %if.then35.i
	movzbl	16(%r15), %eax
	cmpq	$35, %rax
	jne	.LBB19_485
# BB#484:                               # %land.lhs.true42.i
	movq	current_binding_level(%rip), %rax
	movq	40(%rax), %rax
	movzbl	48(%rax), %eax
	testq	%rax, %rax
	jne	.LBB19_494
.LBB19_485:                             # %if.else48.i
	testq	%rbx, %rbx
	je	.LBB19_490
# BB#486:                               # %if.then50.i
	movzbl	16(%rbx), %eax
	cmpq	$35, %rax
	jne	.LBB19_489
# BB#487:                               # %if.then57.i
	movabsq	$.L.str325, %rdi
	jmp	.LBB19_488
.LBB19_478:                             # %if.else.i
	movabsq	$.L.str324, %rdi
.LBB19_477:                             # %warn_if_shadowing.exit
	xorq	%rax, %rax
	callq	pedwarn
	jmp	.LBB19_494
.LBB19_490:                             # %if.else60.i
	movq	56(%r12), %rdx
	testq	%rdx, %rdx
	je	.LBB19_494
# BB#491:                               # %if.else60.i
	movq	global_trees(%rip), %rax
	cmpq	%rax, %rdx
	je	.LBB19_494
# BB#492:                               # %if.then67.i
	movabsq	$.L.str327, %rdi
	movq	%r12, %rsi
	jmp	.LBB19_493
.LBB19_489:                             # %if.else58.i
	movabsq	$.L.str326, %rdi
.LBB19_488:                             # %warn_if_shadowing.exit
	movq	%r12, %rsi
	movq	%rbx, %rdx
.LBB19_493:                             # %warn_if_shadowing.exit
	callq	shadow_warning
.LBB19_494:                             # %warn_if_shadowing.exit
	testq	%rbx, %rbx
	je	.LBB19_496
# BB#495:                               # %if.then681
	movq	16(%r13), %rdx
	movq	%r14, %rdi
	movq	%rbx, %rsi
	callq	tree_cons
	movq	%rax, 16(%r13)
.LBB19_496:                             # %if.end685
	movq	8(%r15), %rax
	movq	global_trees(%rip), %rcx
	cmpq	%rcx, %rax
	je	.LBB19_502
# BB#497:                               # %land.lhs.true689
	cmpq	$0, 32(%rax)
	movabsq	$4294967295, %rsi       # imm = 0xFFFFFFFF
	jne	.LBB19_502
	jmp	.LBB19_498
	.align	16, 0x90
.LBB19_500:                             # %while.body703
                                        #   in Loop: Header=BB19_498 Depth=1
	movq	8(%rax), %rax
.LBB19_498:                             # %while.cond697
                                        # =>This Inner Loop Header: Depth=1
	movzbl	16(%rax), %ecx
	leaq	-20(%rcx), %rdx
	andq	%rsi, %rdx
	cmpq	$2, %rdx
	jb	.LBB19_501
# BB#499:                               # %while.cond697
                                        #   in Loop: Header=BB19_498 Depth=1
	cmpq	$18, %rcx
	je	.LBB19_500
	jmp	.LBB19_502
.LBB19_501:                             # %if.then718
	movl	56(%r13), %eax
	incq	%rax
	movl	%eax, 56(%r13)
.LBB19_502:                             # %if.end721
	movq	(%r13), %rax
	movq	%rax, (%r15)
	movq	%r15, (%r13)
.LBB19_503:                             # %return
	movq	%r15, %rax
	addq	$80, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB19_443:                             # %if.else474
	movq	32(%r14), %rsi
	movabsq	$.L.str146, %rdi
.LBB19_442:                             # %if.end685
	xorq	%rax, %rax
	callq	pedwarn
	jmp	.LBB19_496
.Ltmp155:
	.size	pushdecl, .Ltmp155-pushdecl
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI19_0:
	.quad	.LBB19_209
	.quad	.LBB19_188
	.quad	.LBB19_204
	.quad	.LBB19_205

	.text
	.globl	lookup_name_current_level
	.align	16, 0x90
	.type	lookup_name_current_level,@function
lookup_name_current_level:              # @lookup_name_current_level
	.cfi_startproc
# BB#0:                                 # %entry
	movq	current_binding_level(%rip), %rcx
	movq	global_binding_level(%rip), %rax
	cmpq	%rax, %rcx
	je	.LBB20_1
# BB#2:                                 # %if.end
	xorq	%rax, %rax
	cmpq	$0, 64(%rdi)
	je	.LBB20_6
	.align	16, 0x90
.LBB20_3:                               # %for.cond
                                        # =>This Inner Loop Header: Depth=1
	movq	(%rcx), %rcx
	testq	%rcx, %rcx
	je	.LBB20_6
# BB#4:                                 # %for.body
                                        #   in Loop: Header=BB20_3 Depth=1
	movq	72(%rcx), %rdx
	cmpq	%rdi, %rdx
	jne	.LBB20_3
# BB#5:
	movq	%rcx, %rax
.LBB20_6:                               # %return
	retq
.LBB20_1:                               # %if.then
	movq	56(%rdi), %rax
	retq
.Ltmp156:
	.size	lookup_name_current_level, .Ltmp156-lookup_name_current_level
	.cfi_endproc

	.globl	lookup_name
	.align	16, 0x90
	.type	lookup_name,@function
lookup_name:                            # @lookup_name
	.cfi_startproc
# BB#0:                                 # %entry
	movq	current_binding_level(%rip), %rax
	movq	global_binding_level(%rip), %rcx
	cmpq	%rcx, %rax
	je	.LBB21_2
# BB#1:                                 # %land.lhs.true
	movq	64(%rdi), %rax
	testq	%rax, %rax
	jne	.LBB21_3
.LBB21_2:                               # %if.else
	movq	56(%rdi), %rax
.LBB21_3:                               # %if.end
	retq
.Ltmp157:
	.size	lookup_name, .Ltmp157-lookup_name
	.cfi_endproc

	.globl	pushdecl_top_level
	.align	16, 0x90
	.type	pushdecl_top_level,@function
pushdecl_top_level:                     # @pushdecl_top_level
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp160:
	.cfi_def_cfa_offset 16
.Ltmp161:
	.cfi_offset %rbx, -16
	movq	global_binding_level(%rip), %rax
	movq	current_binding_level(%rip), %rbx
	movq	%rax, current_binding_level(%rip)
	callq	pushdecl
	movq	%rbx, current_binding_level(%rip)
	popq	%rbx
	retq
.Ltmp162:
	.size	pushdecl_top_level, .Ltmp162-pushdecl_top_level
	.cfi_endproc

	.globl	implicitly_declare
	.align	16, 0x90
	.type	implicitly_declare,@function
implicitly_declare:                     # @implicitly_declare
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp168:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp169:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp170:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp171:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp172:
	.cfi_def_cfa_offset 48
.Ltmp173:
	.cfi_offset %rbx, -40
.Ltmp174:
	.cfi_offset %r12, -32
.Ltmp175:
	.cfi_offset %r14, -24
.Ltmp176:
	.cfi_offset %r15, -16
	movq	c_global_trees+176(%rip), %rdx
	movq	%rdi, %r14
	movq	$30, %rdi
	movq	%r14, %rsi
	callq	build_decl
	movq	%rax, (%rsp)
	movzbl	18(%r14), %ecx
	xorq	%r15, %r15
	testq	$8, %rcx
	je	.LBB23_4
# BB#1:                                 # %land.lhs.true
	cmpq	$0, 56(%r14)
	jne	.LBB23_2
# BB#3:                                 # %land.lhs.true
	movabsq	$1, %r15
	jmp	.LBB23_4
.LBB23_2:
	movabsq	$0, %r15
.LBB23_4:                               # %if.end
	movq	80(%r14), %r12
	orq	$256, 48(%rax)          # imm = 0x100
	movq	(%rsp), %rax
	movl	16(%rax), %ecx
	orq	$524288, %rcx           # imm = 0x80000
	movl	%ecx, 16(%rax)
	movq	(%rsp), %rbx
	movq	%rbx, 80(%r14)
	movq	%rbx, %rdi
	callq	pushdecl
	movq	%rbx, %rdi
	callq	maybe_objc_check_decl
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	movq	%rbx, %rdi
	callq	rest_of_decl_compilation
	cmpq	$0, %r12
	je	.LBB23_5
# BB#9:                                 # %if.else
	movl	warn_traditional(%rip), %eax
	testq	%rax, %rax
	je	.LBB23_13
# BB#10:                                # %if.else
	testq	%r15, %r15
	je	.LBB23_13
# BB#11:                                # %if.then17
	movq	32(%r14), %rsi
	movabsq	$.L.str148, %rdi
	jmp	.LBB23_12
.LBB23_5:                               # %if.then13
	movl	mesg_implicit_function_declaration(%rip), %eax
	movq	32(%r14), %rsi
	cmpq	$1, %rax
	jne	.LBB23_6
# BB#8:                                 # %if.then3.i
	movabsq	$.L.str149, %rdi
.LBB23_12:                              # %if.end19
	xorq	%rax, %rax
	callq	warning
.LBB23_13:                              # %if.end19
	movq	(%rsp), %rdi
	xorq	%rsi, %rsi
	movq	$1, %rdx
	xorq	%rcx, %rcx
	callq	gen_aux_info_record
	leaq	(%rsp), %rdi
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	callq	decl_attributes
	movq	(%rsp), %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.LBB23_6:                               # %if.then13
	cmpq	$2, %rax
	jne	.LBB23_13
# BB#7:                                 # %if.then.i
	movabsq	$.L.str149, %rdi
	xorq	%rax, %rax
	callq	error
	jmp	.LBB23_13
.Ltmp177:
	.size	implicitly_declare, .Ltmp177-implicitly_declare
	.cfi_endproc

	.globl	implicit_decl_warning
	.align	16, 0x90
	.type	implicit_decl_warning,@function
implicit_decl_warning:                  # @implicit_decl_warning
	.cfi_startproc
# BB#0:                                 # %entry
	movl	mesg_implicit_function_declaration(%rip), %eax
	movq	32(%rdi), %rsi
	cmpq	$1, %rax
	jne	.LBB24_1
# BB#3:                                 # %if.then3
	movabsq	$.L.str149, %rdi
	xorq	%rax, %rax
	jmp	warning  # TAILCALL
.LBB24_1:                               # %entry
	cmpq	$2, %rax
	jne	.LBB24_4
# BB#2:                                 # %if.then
	movabsq	$.L.str149, %rdi
	xorq	%rax, %rax
	jmp	error  # TAILCALL
.LBB24_4:                               # %if.end4
	retq
.Ltmp178:
	.size	implicit_decl_warning, .Ltmp178-implicit_decl_warning
	.cfi_endproc

	.globl	lookup_label
	.align	16, 0x90
	.type	lookup_label,@function
lookup_label:                           # @lookup_label
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp185:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp186:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp187:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp188:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp189:
	.cfi_def_cfa_offset 48
.Ltmp190:
	.cfi_offset %rbx, -48
.Ltmp191:
	.cfi_offset %r12, -40
.Ltmp192:
	.cfi_offset %r13, -32
.Ltmp193:
	.cfi_offset %r14, -24
.Ltmp194:
	.cfi_offset %r15, -16
	movq	current_function_decl(%rip), %rax
	movq	%rdi, %r12
	testq	%rax, %rax
	je	.LBB25_11
# BB#1:
	movabsq	$.L.str151, %r14
	movabsq	$.L.str152, %r15
	.align	16, 0x90
.LBB25_2:                               # %if.end
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB25_7 Depth 2
	movq	72(%r12), %r13
	testq	%r13, %r13
	je	.LBB25_13
# BB#3:                                 # %if.then3
                                        #   in Loop: Header=BB25_2 Depth=1
	movq	80(%r13), %rcx
	cmpq	%rax, %rcx
	je	.LBB25_14
# BB#4:                                 # %land.lhs.true
                                        #   in Loop: Header=BB25_2 Depth=1
	movzbl	19(%r13), %eax
	testq	$2, %rax
	jne	.LBB25_14
# BB#5:                                 # %for.cond.i.preheader
                                        #   in Loop: Header=BB25_2 Depth=1
	movq	named_labels(%rip), %rbx
	jmp	.LBB25_7
	.align	16, 0x90
.LBB25_6:                               # %for.cond.i
                                        #   in Loop: Header=BB25_7 Depth=2
	movq	(%rbx), %rbx
.LBB25_7:                               # %for.cond.i
                                        #   Parent Loop BB25_2 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	testq	%rbx, %rbx
	je	.LBB25_12
# BB#8:                                 # %for.body.i
                                        #   in Loop: Header=BB25_7 Depth=2
	movq	32(%rbx), %rax
	cmpq	%r13, %rax
	jne	.LBB25_6
# BB#9:                                 # %if.then2.i
                                        #   in Loop: Header=BB25_2 Depth=1
	movq	32(%r12), %rsi
	xorq	%rax, %rax
	movq	%r14, %rdi
	callq	error
	movq	32(%rbx), %rdi
	xorq	%rax, %rax
	movq	%r15, %rsi
	callq	error_with_decl
	jmp	.LBB25_10
	.align	16, 0x90
.LBB25_12:                              # %for.end.i
                                        #   in Loop: Header=BB25_2 Depth=1
	movq	shadowed_labels(%rip), %rdx
	xorq	%rdi, %rdi
	movq	%r13, %rsi
	callq	tree_cons
	movq	%rax, shadowed_labels(%rip)
	movq	$0, 72(%r12)
.LBB25_10:                              # %tailrecurse.backedge
                                        #   in Loop: Header=BB25_2 Depth=1
	movq	current_function_decl(%rip), %rax
	testq	%rax, %rax
	jne	.LBB25_2
.LBB25_11:                              # %if.then
	movq	32(%r12), %rsi
	movabsq	$.L.str150, %rdi
	xorq	%rax, %rax
	xorq	%r13, %r13
	callq	error
	jmp	.LBB25_14
.LBB25_13:                              # %if.end8
	movq	global_trees+216(%rip), %rdx
	movq	$31, %rdi
	movq	%r12, %rsi
	callq	build_decl
	movq	current_function_decl(%rip), %rcx
	movq	%rax, %r13
	movq	%rcx, 80(%r13)
	andq	$-256, 48(%r13)
	movl	lineno(%rip), %eax
	movl	%eax, 32(%r13)
	movq	input_filename(%rip), %rax
	movq	%rax, 24(%r13)
	movq	%r13, 72(%r12)
	movq	named_labels(%rip), %rdx
	xorq	%rdi, %rdi
	movq	%r13, %rsi
	callq	tree_cons
	movq	%rax, named_labels(%rip)
.LBB25_14:                              # %return
	movq	%r13, %rax
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp195:
	.size	lookup_label, .Ltmp195-lookup_label
	.cfi_endproc

	.globl	shadow_label
	.align	16, 0x90
	.type	shadow_label,@function
shadow_label:                           # @shadow_label
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp199:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp200:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp201:
	.cfi_def_cfa_offset 32
.Ltmp202:
	.cfi_offset %rbx, -24
.Ltmp203:
	.cfi_offset %r14, -16
	movq	%rdi, %r14
	movq	72(%r14), %rsi
	testq	%rsi, %rsi
	je	.LBB26_7
# BB#1:                                 # %for.cond.preheader
	movq	named_labels(%rip), %rbx
	jmp	.LBB26_2
	.align	16, 0x90
.LBB26_4:                               # %for.cond
                                        #   in Loop: Header=BB26_2 Depth=1
	movq	(%rbx), %rbx
.LBB26_2:                               # %for.cond.preheader
                                        # =>This Inner Loop Header: Depth=1
	testq	%rbx, %rbx
	je	.LBB26_3
# BB#5:                                 # %for.body
                                        #   in Loop: Header=BB26_2 Depth=1
	movq	32(%rbx), %rax
	cmpq	%rsi, %rax
	jne	.LBB26_4
# BB#6:                                 # %if.then2
	movq	32(%r14), %rsi
	movabsq	$.L.str151, %rdi
	xorq	%rax, %rax
	callq	error
	movq	32(%rbx), %rdi
	movabsq	$.L.str152, %rsi
	xorq	%rax, %rax
	callq	error_with_decl
	jmp	.LBB26_7
.LBB26_3:                               # %for.end
	movq	shadowed_labels(%rip), %rdx
	xorq	%rdi, %rdi
	callq	tree_cons
	movq	%rax, shadowed_labels(%rip)
	movq	$0, 72(%r14)
.LBB26_7:                               # %if.end7
	movq	%r14, %rdi
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	jmp	lookup_label  # TAILCALL
.Ltmp204:
	.size	shadow_label, .Ltmp204-shadow_label
	.cfi_endproc

	.globl	gettags
	.align	16, 0x90
	.type	gettags,@function
gettags:                                # @gettags
	.cfi_startproc
# BB#0:                                 # %entry
	movq	current_binding_level(%rip), %rax
	movq	8(%rax), %rax
	retq
.Ltmp205:
	.size	gettags, .Ltmp205-gettags
	.cfi_endproc

	.globl	pending_xref_error
	.align	16, 0x90
	.type	pending_xref_error,@function
pending_xref_error:                     # @pending_xref_error
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rax
.Ltmp207:
	.cfi_def_cfa_offset 16
	movq	pending_invalid_xref(%rip), %rax
	testq	%rax, %rax
	je	.LBB28_2
# BB#1:                                 # %if.then
	movq	pending_invalid_xref_file(%rip), %rdi
	movq	32(%rax), %rcx
	movl	pending_invalid_xref_line(%rip), %esi
	movabsq	$.L.str155, %rdx
	xorq	%rax, %rax
	callq	error_with_file_and_line
.LBB28_2:                               # %if.end
	movq	$0, pending_invalid_xref(%rip)
	popq	%rax
	retq
.Ltmp208:
	.size	pending_xref_error, .Ltmp208-pending_xref_error
	.cfi_endproc

	.globl	c_init_decl_processing
	.align	16, 0x90
	.type	c_init_decl_processing,@function
c_init_decl_processing:                 # @c_init_decl_processing
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp212:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp213:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp214:
	.cfi_def_cfa_offset 32
.Ltmp215:
	.cfi_offset %rbx, -24
.Ltmp216:
	.cfi_offset %r14, -16
	callq	c_parse_init
	movq	$0, current_function_decl(%rip)
	cmpq	$0, global_binding_level(%rip)
	movq	$0, named_labels(%rip)
	movq	$0, current_binding_level(%rip)
	movq	$0, free_binding_level(%rip)
	jne	.LBB29_2
# BB#1:                                 # %if.end.i
	movq	$0, named_labels(%rip)
.LBB29_2:                               # %if.end2.i
	movq	$72, %rdi
	callq	xmalloc
	movq	current_binding_level(%rip), %rdx
	xorq	%rcx, %rcx
	testq	%rdx, %rdx
	movq	$0, 64(%rax)
	movq	$0, 56(%rax)
	movq	$0, 48(%rax)
	movq	$0, 40(%rax)
	movq	$0, 32(%rax)
	movq	$0, 24(%rax)
	movq	$0, 16(%rax)
	movq	$0, 8(%rax)
	movq	$0, (%rax)
	movabsq	$0, %rdi
	movabsq	$0, %rsi
	je	.LBB29_7
# BB#3:                                 # %cond.true.i
	movzbl	50(%rdx), %esi
	testq	%rsi, %rsi
	je	.LBB29_4
# BB#5:                                 # %cond.true.i
	movabsq	$1, %rsi
	jmp	.LBB29_6
.LBB29_4:
	movabsq	$0, %rsi
.LBB29_6:                               # %cond.true.i
	movq	%rdx, %rdi
.LBB29_7:                               # %pushlevel.exit
	movb	%sil, 49(%rax)
	movq	%rdi, 40(%rax)
	movq	%rax, current_binding_level(%rip)
	movzbl	keep_next_level_flag(%rip), %edx
	movb	%dl, 51(%rax)
	movb	%cl, keep_next_level_flag(%rip)
	movzbl	keep_next_if_subblocks(%rip), %edx
	movl	flag_signed_char(%rip), %edi
	movb	%dl, 52(%rax)
	movb	%cl, keep_next_if_subblocks(%rip)
	movq	%rax, global_binding_level(%rip)
	callq	build_common_tree_nodes
	callq	c_common_nodes_and_builtins
	movq	integer_types+40(%rip), %rax
	movq	global_trees+96(%rip), %rcx
	movq	global_trees+88(%rip), %rdx
	movq	%rax, c_global_trees+128(%rip)
	movq	%rcx, c_global_trees+136(%rip)
	movq	%rdx, c_global_trees+144(%rip)
	movq	$8, %rdi
	callq	make_unsigned_type
	movq	%rax, c_global_trees+152(%rip)
	movq	$11, %rcx
	movb	%cl, 16(%rax)
	movq	$1, %rdi
	xorq	%rsi, %rsi
	callq	build_int_2_wide
	movq	c_global_trees+152(%rip), %rcx
	movq	%rax, 112(%rcx)
	movq	c_global_trees+152(%rip), %rax
	movq	112(%rax), %rcx
	movq	%rax, 8(%rcx)
	movq	c_global_trees+152(%rip), %rax
	movl	60(%rax), %ecx
	movabsq	$4294966784, %rdx       # imm = 0xFFFFFE00
	andq	%rcx, %rdx
	orq	$1, %rdx
	movl	%edx, 60(%rax)
	movabsq	$.L.str156, %rdi
	callq	get_identifier
	movq	c_global_trees+152(%rip), %rdx
	movq	$33, %rdi
	movq	%rax, %rsi
	callq	build_decl
	movq	%rax, %rdi
	callq	pushdecl
	xorq	%rdi, %rdi
	xorq	%rsi, %rsi
	callq	build_int_2_wide
	movq	%rax, c_global_trees+168(%rip)
	movq	c_global_trees+152(%rip), %rcx
	movq	%rcx, 8(%rax)
	movq	$1, %rdi
	xorq	%rsi, %rsi
	callq	build_int_2_wide
	movq	%rax, c_global_trees+160(%rip)
	movq	c_global_trees+152(%rip), %rcx
	movq	%rcx, 8(%rax)
	movq	global_trees+256(%rip), %r14
	movq	global_trees+224(%rip), %rdi
	movq	%r14, %rsi
	callq	build_function_type
	movq	global_trees+224(%rip), %rbx
	xorq	%rdi, %rdi
	movq	%rbx, %rsi
	movq	%r14, %rdx
	callq	tree_cons
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	build_function_type
	movq	global_trees+192(%rip), %rcx
	movq	integer_types+56(%rip), %rax
	movl	60(%rcx), %esi
	movl	60(%rax), %edx
	andq	$511, %rsi              # imm = 0x1FF
	andq	$511, %rdx              # imm = 0x1FF
	cmpq	%rdx, %rsi
	jne	.LBB29_9
# BB#8:
	movabsq	$integer_types+64, %rsi
	movq	%rax, %rdx
	jmp	.LBB29_11
.LBB29_9:                               # %if.else
	movq	integer_types+40(%rip), %rdx
	movl	60(%rdx), %edi
	andq	$511, %rdi              # imm = 0x1FF
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	andq	%rbx, %rsi
	cmpq	%rdi, %rsi
	jne	.LBB29_20
# BB#10:
	movabsq	$integer_types+48, %rsi
.LBB29_11:                              # %if.end41
	movq	(%rsi), %rsi
	movq	%rdx, c_global_trees+184(%rip)
	testq	%rdx, %rdx
	movq	%rsi, c_global_trees+192(%rip)
	je	.LBB29_13
# BB#12:                                # %if.then43
	movabsq	$.L.str157, %rdi
	callq	get_identifier
	movq	c_global_trees+184(%rip), %rdx
	movq	$33, %rdi
	movq	%rax, %rsi
	callq	build_decl
	movq	%rax, %rdi
	callq	pushdecl
	movabsq	$.L.str158, %rdi
	callq	get_identifier
	movq	c_global_trees+192(%rip), %rdx
	movq	$33, %rdi
	movq	%rax, %rsi
	callq	build_decl
	movq	%rax, %rdi
	callq	pushdecl
	movq	global_trees+192(%rip), %rcx
	movq	integer_types+56(%rip), %rax
	jmp	.LBB29_13
.LBB29_20:                              # %if.end41.thread
	movq	$0, c_global_trees+192(%rip)
	movq	$0, c_global_trees+184(%rip)
.LBB29_13:                              # %if.end50
	movl	60(%rcx), %ecx
	movl	60(%rax), %edx
	addq	%rcx, %rcx
	andq	$1022, %rcx             # imm = 0x3FE
	andq	$511, %rdx              # imm = 0x1FF
	cmpq	%rdx, %rcx
	jne	.LBB29_15
# BB#14:
	movabsq	$integer_types+64, %rcx
	jmp	.LBB29_17
.LBB29_15:                              # %if.else61
	movq	integer_types+72(%rip), %rax
	movl	60(%rax), %edx
	andq	$511, %rdx              # imm = 0x1FF
	movabsq	$4294967295, %rsi       # imm = 0xFFFFFFFF
	andq	%rsi, %rcx
	cmpq	%rdx, %rcx
	jne	.LBB29_21
# BB#16:
	movabsq	$integer_types+80, %rcx
.LBB29_17:                              # %if.end75
	movq	(%rcx), %rcx
	movq	%rax, c_global_trees+200(%rip)
	testq	%rax, %rax
	movq	%rcx, c_global_trees+208(%rip)
	je	.LBB29_19
# BB#18:                                # %if.then77
	movabsq	$.L.str159, %rdi
	callq	get_identifier
	movq	c_global_trees+200(%rip), %rdx
	movq	$33, %rdi
	movq	%rax, %rsi
	callq	build_decl
	movq	%rax, %rdi
	callq	pushdecl
	movabsq	$.L.str160, %rdi
	callq	get_identifier
	movq	c_global_trees+208(%rip), %rdx
	movq	$33, %rdi
	movq	%rax, %rsi
	callq	build_decl
	movq	%rax, %rdi
	callq	pushdecl
	jmp	.LBB29_19
.LBB29_21:                              # %if.end75.thread
	movq	$0, c_global_trees+208(%rip)
	movq	$0, c_global_trees+200(%rip)
.LBB29_19:                              # %if.end84
	movl	pedantic(%rip), %eax
	movl	%eax, pedantic_lvalues(%rip)
	movq	$c_make_fname_decl, make_fname_decl(%rip)
	callq	start_fname_decls
	movq	$finish_incomplete_decl, incomplete_decl_finalize_hook(%rip)
	movabsq	$c_global_trees, %rdi
	movq	$32, %rsi
	callq	ggc_add_tree_root
	movabsq	$c_stmt_tree, %rdi
	movabsq	$mark_stmt_tree, %rcx
	movq	$1, %rsi
	movq	$32, %rdx
	callq	ggc_add_root
	movabsq	$c_scope_stmt_stack, %rdi
	movq	$1, %rsi
	callq	ggc_add_tree_root
	movabsq	$named_labels, %rdi
	movq	$1, %rsi
	callq	ggc_add_tree_root
	movabsq	$shadowed_labels, %rdi
	movq	$1, %rsi
	callq	ggc_add_tree_root
	movabsq	$current_binding_level, %rdi
	movabsq	$mark_binding_level, %rbx
	movq	$1, %rsi
	movq	$8, %rdx
	movq	%rbx, %rcx
	callq	ggc_add_root
	movabsq	$label_level_chain, %rdi
	movq	$1, %rsi
	movq	$8, %rdx
	movq	%rbx, %rcx
	callq	ggc_add_root
	movabsq	$static_ctors, %rdi
	movq	$1, %rsi
	callq	ggc_add_tree_root
	movabsq	$static_dtors, %rdi
	movq	$1, %rsi
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	jmp	ggc_add_tree_root  # TAILCALL
.Ltmp217:
	.size	c_init_decl_processing, .Ltmp217-c_init_decl_processing
	.cfi_endproc

	.align	16, 0x90
	.type	c_make_fname_decl,@function
c_make_fname_decl:                      # @c_make_fname_decl
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp224:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp225:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp226:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp227:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp228:
	.cfi_def_cfa_offset 48
.Ltmp229:
	.cfi_offset %rbx, -48
.Ltmp230:
	.cfi_offset %r12, -40
.Ltmp231:
	.cfi_offset %r13, -32
.Ltmp232:
	.cfi_offset %r14, -24
.Ltmp233:
	.cfi_offset %r15, -16
	movq	%rdi, %r15
	movq	%rsi, %rdi
	callq	fname_as_string
	movq	%rax, %r14
	movq	%r14, %rdi
	callq	strlen
	movq	integer_types(%rip), %rdi
	movq	%rax, %r12
	movq	$1, %rsi
	callq	build_qualified_type
	movq	%rax, %rbx
	xorq	%rsi, %rsi
	movq	%r12, %rdi
	callq	size_int_wide
	movq	%rax, %rdi
	callq	build_index_type
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	build_array_type
	movq	%rax, %r13
	movq	$34, %rdi
	movq	%r15, %rsi
	movq	%r13, %rdx
	callq	build_decl
	movq	current_function_decl(%rip), %rcx
	movq	%rax, %rbx
	movq	%rcx, 80(%rbx)
	movl	16(%rbx), %eax
	orq	$266240, %rax           # imm = 0x41000
	movl	%eax, 16(%rbx)
	orq	$4194304, 48(%rbx)      # imm = 0x400000
	leaq	1(%r12), %rdi
	movq	%r14, %rsi
	callq	build_string
	movq	%r13, 8(%rax)
	movq	%rax, 104(%rbx)
	movl	16(%rbx), %ecx
	orq	$65536, %rcx            # imm = 0x10000
	movl	%ecx, 16(%rbx)
	xorq	%rdx, %rdx
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	finish_decl
	movq	%rbx, %rax
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp234:
	.size	c_make_fname_decl, .Ltmp234-c_make_fname_decl
	.cfi_endproc

	.align	16, 0x90
	.type	mark_binding_level,@function
mark_binding_level:                     # @mark_binding_level
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp239:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp240:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp241:
	.cfi_def_cfa_offset 32
.Ltmp242:
	.cfi_offset %rbx, -32
.Ltmp243:
	.cfi_offset %r14, -24
.Ltmp244:
	.cfi_offset %r15, -16
	movq	(%rdi), %rbx
	testq	%rbx, %rbx
	je	.LBB31_33
# BB#1:
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB31_2:                               # %do.body
                                        # =>This Inner Loop Header: Depth=1
	movq	(%rbx), %r14
	testq	%r14, %r14
	je	.LBB31_7
# BB#3:                                 # %land.lhs.true
                                        #   in Loop: Header=BB31_2 Depth=1
	movq	%r14, %rdi
	callq	ggc_set_mark
	testq	%r15, %rax
	jne	.LBB31_7
# BB#4:                                 # %do.body2
                                        #   in Loop: Header=BB31_2 Depth=1
	movq	ggc_pending_trees(%rip), %rax
	movq	(%rax), %rsi
	movq	8(%rax), %rcx
	cmpq	%rsi, %rcx
	jb	.LBB31_6
# BB#5:                                 # %if.then4
                                        #   in Loop: Header=BB31_2 Depth=1
	addq	%rsi, %rsi
	movq	%rax, %rdi
	callq	varray_grow
	movq	%rax, ggc_pending_trees(%rip)
	movq	8(%rax), %rcx
.LBB31_6:                               # %if.end
                                        #   in Loop: Header=BB31_2 Depth=1
	leaq	1(%rcx), %rdx
	movq	%rdx, 8(%rax)
	movq	%r14, 32(%rax,%rcx,8)
.LBB31_7:                               # %do.body10
                                        #   in Loop: Header=BB31_2 Depth=1
	movq	8(%rbx), %r14
	testq	%r14, %r14
	je	.LBB31_12
# BB#8:                                 # %land.lhs.true13
                                        #   in Loop: Header=BB31_2 Depth=1
	movq	%r14, %rdi
	callq	ggc_set_mark
	testq	%r15, %rax
	jne	.LBB31_12
# BB#9:                                 # %do.body17
                                        #   in Loop: Header=BB31_2 Depth=1
	movq	ggc_pending_trees(%rip), %rax
	movq	(%rax), %rsi
	movq	8(%rax), %rcx
	cmpq	%rsi, %rcx
	jb	.LBB31_11
# BB#10:                                # %if.then21
                                        #   in Loop: Header=BB31_2 Depth=1
	addq	%rsi, %rsi
	movq	%rax, %rdi
	callq	varray_grow
	movq	%rax, ggc_pending_trees(%rip)
	movq	8(%rax), %rcx
.LBB31_11:                              # %if.end25
                                        #   in Loop: Header=BB31_2 Depth=1
	leaq	1(%rcx), %rdx
	movq	%rdx, 8(%rax)
	movq	%r14, 32(%rax,%rcx,8)
.LBB31_12:                              # %do.body34
                                        #   in Loop: Header=BB31_2 Depth=1
	movq	16(%rbx), %r14
	testq	%r14, %r14
	je	.LBB31_17
# BB#13:                                # %land.lhs.true37
                                        #   in Loop: Header=BB31_2 Depth=1
	movq	%r14, %rdi
	callq	ggc_set_mark
	testq	%r15, %rax
	jne	.LBB31_17
# BB#14:                                # %do.body41
                                        #   in Loop: Header=BB31_2 Depth=1
	movq	ggc_pending_trees(%rip), %rax
	movq	(%rax), %rsi
	movq	8(%rax), %rcx
	cmpq	%rsi, %rcx
	jb	.LBB31_16
# BB#15:                                # %if.then45
                                        #   in Loop: Header=BB31_2 Depth=1
	addq	%rsi, %rsi
	movq	%rax, %rdi
	callq	varray_grow
	movq	%rax, ggc_pending_trees(%rip)
	movq	8(%rax), %rcx
.LBB31_16:                              # %if.end49
                                        #   in Loop: Header=BB31_2 Depth=1
	leaq	1(%rcx), %rdx
	movq	%rdx, 8(%rax)
	movq	%r14, 32(%rax,%rcx,8)
.LBB31_17:                              # %do.body58
                                        #   in Loop: Header=BB31_2 Depth=1
	movq	24(%rbx), %r14
	testq	%r14, %r14
	je	.LBB31_22
# BB#18:                                # %land.lhs.true61
                                        #   in Loop: Header=BB31_2 Depth=1
	movq	%r14, %rdi
	callq	ggc_set_mark
	testq	%r15, %rax
	jne	.LBB31_22
# BB#19:                                # %do.body65
                                        #   in Loop: Header=BB31_2 Depth=1
	movq	ggc_pending_trees(%rip), %rax
	movq	(%rax), %rsi
	movq	8(%rax), %rcx
	cmpq	%rsi, %rcx
	jb	.LBB31_21
# BB#20:                                # %if.then69
                                        #   in Loop: Header=BB31_2 Depth=1
	addq	%rsi, %rsi
	movq	%rax, %rdi
	callq	varray_grow
	movq	%rax, ggc_pending_trees(%rip)
	movq	8(%rax), %rcx
.LBB31_21:                              # %if.end73
                                        #   in Loop: Header=BB31_2 Depth=1
	leaq	1(%rcx), %rdx
	movq	%rdx, 8(%rax)
	movq	%r14, 32(%rax,%rcx,8)
.LBB31_22:                              # %do.body82
                                        #   in Loop: Header=BB31_2 Depth=1
	movq	32(%rbx), %r14
	testq	%r14, %r14
	je	.LBB31_27
# BB#23:                                # %land.lhs.true85
                                        #   in Loop: Header=BB31_2 Depth=1
	movq	%r14, %rdi
	callq	ggc_set_mark
	testq	%r15, %rax
	jne	.LBB31_27
# BB#24:                                # %do.body89
                                        #   in Loop: Header=BB31_2 Depth=1
	movq	ggc_pending_trees(%rip), %rax
	movq	(%rax), %rsi
	movq	8(%rax), %rcx
	cmpq	%rsi, %rcx
	jb	.LBB31_26
# BB#25:                                # %if.then93
                                        #   in Loop: Header=BB31_2 Depth=1
	addq	%rsi, %rsi
	movq	%rax, %rdi
	callq	varray_grow
	movq	%rax, ggc_pending_trees(%rip)
	movq	8(%rax), %rcx
.LBB31_26:                              # %if.end97
                                        #   in Loop: Header=BB31_2 Depth=1
	leaq	1(%rcx), %rdx
	movq	%rdx, 8(%rax)
	movq	%r14, 32(%rax,%rcx,8)
.LBB31_27:                              # %do.body106
                                        #   in Loop: Header=BB31_2 Depth=1
	movq	64(%rbx), %r14
	testq	%r14, %r14
	je	.LBB31_32
# BB#28:                                # %land.lhs.true109
                                        #   in Loop: Header=BB31_2 Depth=1
	movq	%r14, %rdi
	callq	ggc_set_mark
	testq	%r15, %rax
	jne	.LBB31_32
# BB#29:                                # %do.body113
                                        #   in Loop: Header=BB31_2 Depth=1
	movq	ggc_pending_trees(%rip), %rax
	movq	(%rax), %rsi
	movq	8(%rax), %rcx
	cmpq	%rsi, %rcx
	jb	.LBB31_31
# BB#30:                                # %if.then117
                                        #   in Loop: Header=BB31_2 Depth=1
	addq	%rsi, %rsi
	movq	%rax, %rdi
	callq	varray_grow
	movq	%rax, ggc_pending_trees(%rip)
	movq	8(%rax), %rcx
.LBB31_31:                              # %if.end121
                                        #   in Loop: Header=BB31_2 Depth=1
	leaq	1(%rcx), %rdx
	movq	%rdx, 8(%rax)
	movq	%r14, 32(%rax,%rcx,8)
.LBB31_32:                              # %for.inc
                                        #   in Loop: Header=BB31_2 Depth=1
	movq	40(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB31_2
.LBB31_33:                              # %for.end
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp245:
	.size	mark_binding_level, .Ltmp245-mark_binding_level
	.cfi_endproc

	.globl	builtin_function
	.align	16, 0x90
	.type	builtin_function,@function
builtin_function:                       # @builtin_function
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp252:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp253:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp254:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp255:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp256:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp257:
	.cfi_def_cfa_offset 64
.Ltmp258:
	.cfi_offset %rbx, -48
.Ltmp259:
	.cfi_offset %r12, -40
.Ltmp260:
	.cfi_offset %r13, -32
.Ltmp261:
	.cfi_offset %r14, -24
.Ltmp262:
	.cfi_offset %r15, -16
	movq	%r8, %r12
	movq	%rcx, %r14
	movq	%rdx, (%rsp)            # 8-byte Spill
	movq	%rsi, %r13
	movq	%rdi, %r15
                                        # kill: RDI<def> R15<kill>
	callq	get_identifier
	movq	$30, %rdi
	movq	%rax, %rsi
	movq	%r13, %rdx
	callq	build_decl
	movq	%rax, 8(%rsp)
	orq	$256, 48(%rax)          # imm = 0x100
	movq	8(%rsp), %rax
	movl	16(%rax), %ecx
	orq	$524288, %rcx           # imm = 0x80000
	movl	%ecx, 16(%rax)
	movl	flag_traditional(%rip), %eax
	testq	%rax, %rax
	je	.LBB32_3
# BB#1:                                 # %land.lhs.true
	movzbl	(%r15), %eax
	cmpq	$95, %rax
	je	.LBB32_3
# BB#2:                                 # %if.then
	movq	8(%rsp), %rax
	movl	16(%rax), %ecx
	orq	$8192, %rcx             # imm = 0x2000
	movl	%ecx, 16(%rax)
.LBB32_3:                               # %if.end
	testq	%r12, %r12
	je	.LBB32_5
# BB#4:                                 # %if.then12
	movq	%r12, %rdi
	callq	get_identifier
	movq	8(%rsp), %rcx
	movq	%rax, 120(%rcx)
.LBB32_5:                               # %if.end15
	movq	8(%rsp), %rbx
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	make_decl_rtl
	movq	%rbx, %rdi
	callq	pushdecl
	movq	48(%rbx), %rax
	andq	$3, %r14
	shlq	$29, %r14
	andq	$-1610612737, %rax      # imm = 0xFFFFFFFF9FFFFFFF
	orq	%r14, %rax
	movq	%rax, 48(%rbx)
	movq	8(%rsp), %rax
	movq	(%rsp), %rcx            # 8-byte Reload
	movl	%ecx, 56(%rax)
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	cmpq	$57, %rax
	je	.LBB32_7
# BB#6:                                 # %if.end15
	cmpq	$88, %rax
	jne	.LBB32_8
.LBB32_7:                               # %if.then26
	movq	8(%rsp), %rax
	movl	16(%rax), %ecx
	orq	$2048, %rcx             # imm = 0x800
	movl	%ecx, 16(%rax)
.LBB32_8:                               # %if.end31
	movzbl	(%r15), %eax
	cmpq	$95, %rax
	jne	.LBB32_10
# BB#9:                                 # %lor.lhs.false36
	movzbl	1(%r15), %eax
	cmpq	$95, %rax
	je	.LBB32_11
.LBB32_10:                              # %if.then41
	movq	8(%rsp), %rax
	movabsq	$1099511627776, %rcx    # imm = 0x10000000000
	orq	%rcx, 48(%rax)
.LBB32_11:                              # %if.end46
	leaq	8(%rsp), %rdi
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	callq	decl_attributes
	movq	8(%rsp), %rax
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp263:
	.size	builtin_function, .Ltmp263-builtin_function
	.cfi_endproc

	.globl	insert_default_attributes
	.align	16, 0x90
	.type	insert_default_attributes,@function
insert_default_attributes:              # @insert_default_attributes
	.cfi_startproc
# BB#0:                                 # %entry
	movzbl	18(%rdi), %eax
	testq	$8, %rax
	je	.LBB33_1
# BB#2:                                 # %if.end
	jmp	c_common_insert_default_attributes  # TAILCALL
.LBB33_1:                               # %return
	retq
.Ltmp264:
	.size	insert_default_attributes, .Ltmp264-insert_default_attributes
	.cfi_endproc

	.globl	shadow_tag
	.align	16, 0x90
	.type	shadow_tag,@function
shadow_tag:                             # @shadow_tag
	.cfi_startproc
# BB#0:                                 # %entry
	xorq	%rsi, %rsi
	jmp	shadow_tag_warned  # TAILCALL
.Ltmp265:
	.size	shadow_tag, .Ltmp265-shadow_tag
	.cfi_endproc

	.globl	shadow_tag_warned
	.align	16, 0x90
	.type	shadow_tag_warned,@function
shadow_tag_warned:                      # @shadow_tag_warned
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp272:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp273:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp274:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp275:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp276:
	.cfi_def_cfa_offset 48
	subq	$32, %rsp
.Ltmp277:
	.cfi_def_cfa_offset 80
.Ltmp278:
	.cfi_offset %rbx, -48
.Ltmp279:
	.cfi_offset %r12, -40
.Ltmp280:
	.cfi_offset %r13, -32
.Ltmp281:
	.cfi_offset %r14, -24
.Ltmp282:
	.cfi_offset %r15, -16
	movq	%rsi, %r13
	movq	$0, pending_invalid_xref(%rip)
	leaq	24(%rsp), %rsi
	leaq	16(%rsp), %rdx
	callq	split_specs_attrs
	movq	24(%rsp), %r12
	xorq	%rbx, %rbx
	testq	%r12, %r12
	je	.LBB35_34
# BB#1:
	movq	$0, 8(%rsp)             # 8-byte Folded Spill
	.align	16, 0x90
.LBB35_2:                               # %for.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB35_5 Depth 2
                                        #       Child Loop BB35_7 Depth 3
                                        #     Child Loop BB35_10 Depth 2
                                        #       Child Loop BB35_17 Depth 3
                                        #     Child Loop BB35_25 Depth 2
	movq	32(%r12), %rcx
	movl	16(%rcx), %eax
	movq	%rax, %rbx
	andq	$255, %rbx
	andq	$254, %rax
	cmpq	$20, %rax
	je	.LBB35_4
# BB#3:                                 # %for.body
                                        #   in Loop: Header=BB35_2 Depth=1
	cmpq	$10, %rbx
	je	.LBB35_4
# BB#29:                                # %if.else17
                                        #   in Loop: Header=BB35_2 Depth=1
	movl	in_system_header(%rip), %eax
	orq	%r13, %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB35_31
# BB#30:                                # %if.then21
                                        #   in Loop: Header=BB35_2 Depth=1
	xorq	%rax, %rax
	movabsq	$.L.str162, %rdi
	callq	warning
	movq	$2, %r13
	jmp	.LBB35_31
	.align	16, 0x90
.LBB35_4:                               # %if.then
                                        #   in Loop: Header=BB35_2 Depth=1
	movq	current_binding_level(%rip), %rax
	testq	%rax, %rax
	movq	%rax, %rdx
	je	.LBB35_12
.LBB35_5:                               # %for.body.i
                                        #   Parent Loop BB35_2 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB35_7 Depth 3
	movq	8(%rdx), %rsi
	jmp	.LBB35_7
	.align	16, 0x90
.LBB35_6:                               # %for.cond1.i
                                        #   in Loop: Header=BB35_7 Depth=3
	movq	(%rsi), %rsi
.LBB35_7:                               # %for.cond1.i
                                        #   Parent Loop BB35_2 Depth=1
                                        #     Parent Loop BB35_5 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	testq	%rsi, %rsi
	je	.LBB35_11
# BB#8:                                 # %for.body3.i
                                        #   in Loop: Header=BB35_7 Depth=3
	movq	32(%rsi), %rdi
	cmpq	%rcx, %rdi
	jne	.LBB35_6
	jmp	.LBB35_9
	.align	16, 0x90
.LBB35_11:                              # %for.inc5.i
                                        #   in Loop: Header=BB35_5 Depth=2
	movq	40(%rdx), %rdx
	testq	%rdx, %rdx
	jne	.LBB35_5
	.align	16, 0x90
.LBB35_12:                              # %lookup_tag_reverse.exit.thread
                                        #   in Loop: Header=BB35_2 Depth=1
	incq	8(%rsp)                 # 8-byte Folded Spill
	jmp	.LBB35_13
	.align	16, 0x90
.LBB35_9:                               # %lookup_tag_reverse.exit
                                        #   in Loop: Header=BB35_2 Depth=1
	movq	24(%rsi), %r15
	incq	8(%rsp)                 # 8-byte Folded Spill
	testq	%r15, %r15
	jne	.LBB35_10
.LBB35_13:                              # %if.then7
                                        #   in Loop: Header=BB35_2 Depth=1
	movq	%r13, %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	cmpq	$1, %rax
	je	.LBB35_31
# BB#14:                                # %if.then7
                                        #   in Loop: Header=BB35_2 Depth=1
	cmpq	$10, %rbx
	je	.LBB35_31
# BB#15:                                # %if.then10
                                        #   in Loop: Header=BB35_2 Depth=1
	xorq	%rax, %rax
	movabsq	$.L.str161, %rdi
	callq	pedwarn
	movq	$1, %r13
	jmp	.LBB35_31
	.align	16, 0x90
.LBB35_10:                              # %for.body.i60
                                        #   Parent Loop BB35_2 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB35_17 Depth 3
	movq	8(%rax), %r14
	jmp	.LBB35_17
	.align	16, 0x90
.LBB35_16:                              # %for.cond1.i64
                                        #   in Loop: Header=BB35_17 Depth=3
	movq	(%r14), %r14
.LBB35_17:                              # %for.cond1.i64
                                        #   Parent Loop BB35_2 Depth=1
                                        #     Parent Loop BB35_10 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	testq	%r14, %r14
	je	.LBB35_22
# BB#18:                                # %for.body3.i67
                                        #   in Loop: Header=BB35_17 Depth=3
	movq	24(%r14), %rcx
	cmpq	%r15, %rcx
	jne	.LBB35_16
	jmp	.LBB35_19
	.align	16, 0x90
.LBB35_22:                              # %for.end.i71
                                        #   in Loop: Header=BB35_10 Depth=2
	movzbl	49(%rax), %ecx
	testq	%rcx, %rcx
	je	.LBB35_24
# BB#23:                                # %for.inc21.i
                                        #   in Loop: Header=BB35_10 Depth=2
	movq	40(%rax), %rax
	testq	%rax, %rax
	jne	.LBB35_10
	jmp	.LBB35_24
.LBB35_19:                              # %if.then.i69
                                        #   in Loop: Header=BB35_2 Depth=1
	movq	32(%r14), %rax
	movzbl	16(%rax), %ecx
	cmpq	%rbx, %rcx
	je	.LBB35_21
# BB#20:                                # %pending_xref_error.exit.i
                                        #   in Loop: Header=BB35_2 Depth=1
	movq	input_filename(%rip), %rdi
	movl	lineno(%rip), %esi
	movq	%r15, pending_invalid_xref(%rip)
	movq	%rdi, pending_invalid_xref_file(%rip)
	movl	%esi, pending_invalid_xref_line(%rip)
	movq	32(%r15), %rcx
	xorq	%rax, %rax
	movabsq	$.L.str155, %rdx
	callq	error_with_file_and_line
	movq	$0, pending_invalid_xref(%rip)
	movq	32(%r14), %rax
.LBB35_21:                              # %lookup_tag.exit
                                        #   in Loop: Header=BB35_2 Depth=1
	testq	%rax, %rax
	jne	.LBB35_31
.LBB35_24:                              # %if.then13
                                        #   in Loop: Header=BB35_2 Depth=1
	movq	%rbx, %rdi
	callq	make_node
	movq	%rax, %r14
	movabsq	$current_binding_level, %rax
	.align	16, 0x90
.LBB35_25:                              # %for.cond.i
                                        #   Parent Loop BB35_2 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	(%rax), %rbx
	movzbl	49(%rbx), %ecx
	leaq	40(%rbx), %rax
	testq	%rcx, %rcx
	jne	.LBB35_25
# BB#26:                                # %if.then.i56
                                        #   in Loop: Header=BB35_2 Depth=1
	cmpq	$0, 96(%r14)
	jne	.LBB35_28
# BB#27:                                # %if.then4.i
                                        #   in Loop: Header=BB35_2 Depth=1
	movq	%r15, 96(%r14)
.LBB35_28:                              # %pushtag.exit
                                        #   in Loop: Header=BB35_2 Depth=1
	movq	8(%rbx), %rdx
	movq	%r15, %rdi
	movq	%r14, %rsi
	callq	tree_cons
	movq	%rax, 8(%rbx)
	movq	$33, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	callq	build_decl
	movq	%rax, %rdi
	callq	pushdecl
	movq	%rax, (%r14)
	movq	80(%rax), %rax
	movq	%rax, 144(%r14)
	.align	16, 0x90
.LBB35_31:                              # %for.inc
                                        #   in Loop: Header=BB35_2 Depth=1
	movq	(%r12), %r12
	testq	%r12, %r12
	jne	.LBB35_2
# BB#32:                                # %for.end
	movq	$32, %rcx
	movq	8(%rsp), %rbx           # 8-byte Reload
	movq	%rbx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$2, %rax
	jl	.LBB35_34
# BB#33:                                # %if.then26
	movabsq	$.L.str163, %rdi
	xorq	%rax, %rax
	callq	error
.LBB35_34:                              # %if.end27
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %r13
	cmpq	$1, %r13
	je	.LBB35_37
# BB#35:                                # %if.end27
	testq	%rax, %rbx
	jne	.LBB35_37
# BB#36:                                # %if.then31
	movabsq	$.L.str164, %rdi
	xorq	%rax, %rax
	callq	pedwarn
.LBB35_37:                              # %if.end33
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp283:
	.size	shadow_tag_warned, .Ltmp283-shadow_tag_warned
	.cfi_endproc

	.globl	build_array_declarator
	.align	16, 0x90
	.type	build_array_declarator,@function
build_array_declarator:                 # @build_array_declarator
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp290:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp291:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp292:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp293:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp294:
	.cfi_def_cfa_offset 48
.Ltmp295:
	.cfi_offset %rbx, -48
.Ltmp296:
	.cfi_offset %r12, -40
.Ltmp297:
	.cfi_offset %r13, -32
.Ltmp298:
	.cfi_offset %r14, -24
.Ltmp299:
	.cfi_offset %r15, -16
	movq	%rcx, %r14
	movq	%rdx, %r12
	movq	%rsi, %r15
	movq	%rdi, %rcx
	movq	$43, %rdi
	xorq	%rsi, %rsi
	xorq	%rax, %rax
	movq	%rcx, %rdx
	callq	build_nt
	movq	%rax, %rbx
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	testq	%r13, %r12
	movq	%r15, 8(%rbx)
	je	.LBB36_1
# BB#2:                                 # %entry
	movabsq	$1, %rax
	jmp	.LBB36_3
.LBB36_1:
	movabsq	$0, %rax
.LBB36_3:                               # %entry
	movl	16(%rbx), %edx
	movq	$18, %rcx
	shlq	%cl, %rax
	movabsq	$4294705151, %rcx       # imm = 0xFFFBFFFF
	andq	%rdx, %rcx
	orq	%rax, %rcx
	movl	%ecx, 16(%rbx)
	movl	pedantic(%rip), %eax
	testq	%rax, %rax
	je	.LBB36_10
# BB#4:                                 # %entry
	movl	flag_isoc99(%rip), %eax
	testq	%r13, %rax
	jne	.LBB36_10
# BB#5:                                 # %if.then
	testq	%r13, %r12
	jne	.LBB36_7
# BB#6:                                 # %if.then
	testq	%r15, %r15
	je	.LBB36_8
.LBB36_7:                               # %if.then5
	movabsq	$.L.str165, %rdi
	xorq	%rax, %rax
	callq	pedwarn
.LBB36_8:                               # %if.end
	testq	%r13, %r14
	je	.LBB36_12
# BB#9:                                 # %if.then7
	movabsq	$.L.str166, %rdi
	xorq	%rax, %rax
	callq	pedwarn
.LBB36_10:                              # %if.end9
	testq	%r13, %r14
	je	.LBB36_12
# BB#11:                                # %if.then11
	movabsq	$.L.str167, %rdi
	xorq	%rax, %rax
	callq	warning
.LBB36_12:                              # %if.end12
	movq	%rbx, %rax
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp300:
	.size	build_array_declarator, .Ltmp300-build_array_declarator
	.cfi_endproc

	.globl	set_array_declarator_type
	.align	16, 0x90
	.type	set_array_declarator_type,@function
set_array_declarator_type:              # @set_array_declarator_type
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp303:
	.cfi_def_cfa_offset 16
.Ltmp304:
	.cfi_offset %rbx, -16
	movq	%rdi, %rbx
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %rdx
	movq	%rsi, 32(%rbx)
	je	.LBB37_4
# BB#1:                                 # %land.lhs.true
	cmpq	$0, 8(%rbx)
	jne	.LBB37_3
# BB#2:                                 # %lor.lhs.false
	movzbl	18(%rbx), %eax
	testq	$4, %rax
	je	.LBB37_4
.LBB37_3:                               # %if.then
	movabsq	$.L.str168, %rdi
	xorq	%rax, %rax
	callq	error
.LBB37_4:                               # %if.end
	movq	%rbx, %rax
	popq	%rbx
	retq
.Ltmp305:
	.size	set_array_declarator_type, .Ltmp305-set_array_declarator_type
	.cfi_endproc

	.globl	groktypename
	.align	16, 0x90
	.type	groktypename,@function
groktypename:                           # @groktypename
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp308:
	.cfi_def_cfa_offset 16
	subq	$32, %rsp
.Ltmp309:
	.cfi_def_cfa_offset 48
.Ltmp310:
	.cfi_offset %rbx, -16
	movq	%rdi, %rbx
	movq	%rbx, 24(%rsp)
	movzbl	16(%rbx), %eax
	cmpq	$2, %rax
	jne	.LBB38_2
# BB#1:                                 # %if.end
	movq	24(%rbx), %rdi
	leaq	16(%rsp), %rsi
	leaq	8(%rsp), %rdx
	callq	split_specs_attrs
	movq	32(%rbx), %rdi
	movq	16(%rsp), %rsi
	movq	$5, %rdx
	xorq	%rcx, %rcx
	callq	grokdeclarator
	movq	8(%rsp), %rsi
	movq	%rax, 24(%rsp)
	leaq	24(%rsp), %rdi
	xorq	%rdx, %rdx
	callq	decl_attributes
	movq	24(%rsp), %rbx
.LBB38_2:                               # %return
	movq	%rbx, %rax
	addq	$32, %rsp
	popq	%rbx
	retq
.Ltmp311:
	.size	groktypename, .Ltmp311-groktypename
	.cfi_endproc

	.align	16, 0x90
	.type	grokdeclarator,@function
grokdeclarator:                         # @grokdeclarator
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp318:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp319:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp320:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp321:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp322:
	.cfi_def_cfa_offset 48
	subq	$192, %rsp
.Ltmp323:
	.cfi_def_cfa_offset 240
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
	movq	%rcx, 80(%rsp)          # 8-byte Spill
	movq	%rsi, %rbx
	movabsq	$4294967295, %r9        # imm = 0xFFFFFFFF
	movq	%rdx, %rax
	andq	%r9, %rax
	cmpq	$4, %rax
	movq	$3, %rcx
	je	.LBB39_2
# BB#1:                                 # %entry
	movq	%rdx, %rcx
.LBB39_2:                               # %entry
	movq	%rcx, %rsi
	andq	%r9, %rsi
	movq	%rsi, 128(%rsp)         # 8-byte Spill
	xorq	%rax, %rax
	cmpq	$1, %rsi
	jne	.LBB39_3
# BB#4:                                 # %entry
	movabsq	$1, %r10
	jmp	.LBB39_5
.LBB39_3:
	movabsq	$0, %r10
.LBB39_5:                               # %entry
	movabsq	$0, %r14
	je	.LBB39_7
# BB#6:                                 # %entry
	movq	%rcx, %r14
.LBB39_7:                               # %entry
	testq	%rdi, %rdi
	movq	$0, 184(%rsp)
	je	.LBB39_8
# BB#9:
	movabsq	$9018194371018752, %r8  # imm = 0x200A0000000000
	movq	%rdi, %rsi
	movq	%rdi, 96(%rsp)          # 8-byte Spill
.LBB39_10:                              # %while.body
                                        # =>This Inner Loop Header: Depth=1
	movzbl	16(%rsi), %ecx
	cmpq	$40, %rcx
	jg	.LBB39_14
# BB#11:                                # %while.body
                                        #   in Loop: Header=BB39_10 Depth=1
	cmpq	$1, %rcx
	je	.LBB39_18
# BB#12:                                # %while.body
                                        #   in Loop: Header=BB39_10 Depth=1
	cmpq	$2, %rcx
	jne	.LBB39_17
# BB#13:                                # %sw.bb8
                                        #   in Loop: Header=BB39_10 Depth=1
	movq	32(%rsi), %rsi
	xorq	%rdi, %rdi
	testq	%rsi, %rsi
	jne	.LBB39_10
	jmp	.LBB39_19
	.align	16, 0x90
.LBB39_14:                              # %while.body
                                        #   in Loop: Header=BB39_10 Depth=1
	cmpq	$53, %rcx
	ja	.LBB39_17
# BB#15:                                # %while.body
                                        #   in Loop: Header=BB39_10 Depth=1
	movq	$1, %rax
	shlq	%cl, %rax
	testq	%r8, %rax
	je	.LBB39_17
# BB#16:                                # %sw.bb
                                        #   in Loop: Header=BB39_10 Depth=1
	movq	32(%rsi), %rsi
	xorq	%rdi, %rdi
	testq	%rsi, %rsi
	movq	%rcx, %rax
	jne	.LBB39_10
	jmp	.LBB39_20
.LBB39_18:                              # %while.cond.outer.while.cond.outer.while.cond.outer2207.while.end_crit_edge_crit_edge_crit_edge
	movq	32(%rsi), %rdi
.LBB39_19:                              # %while.end
	movq	%rax, %rcx
	jmp	.LBB39_20
.LBB39_8:
	movq	%rdi, 96(%rsp)          # 8-byte Spill
	xorq	%rdi, %rdi
	xorq	%rcx, %rcx
.LBB39_20:                              # %while.end
	testq	%rdi, %rdi
	jne	.LBB39_22
# BB#21:
	movabsq	$.L.str262, %rdi
.LBB39_22:                              # %while.end
	movq	%rdi, 136(%rsp)         # 8-byte Spill
	cmpq	$1, 128(%rsp)           # 8-byte Folded Reload
	jne	.LBB39_24
# BB#23:                                # %while.end
	xorq	%rax, %rax
	andq	%r9, %rcx
	cmpq	$53, %rcx
	jne	.LBB39_617
.LBB39_24:                              # %if.end16
	orq	%r14, %r10
	testq	%r9, %r10
	jne	.LBB39_27
# BB#25:                                # %land.lhs.true20
	movq	current_binding_level(%rip), %rax
	movzbl	48(%rax), %eax
	testq	%rax, %rax
	je	.LBB39_27
# BB#26:                                # %select.mid
	movq	$2, %r14
.LBB39_27:                              # %if.end23
	movq	%rdx, 56(%rsp)          # 8-byte Spill
	xorq	%r12, %r12
	testq	%rbx, %rbx
	je	.LBB39_28
# BB#29:
	movq	%r14, 112(%rsp)         # 8-byte Spill
	movq	$0, 120(%rsp)           # 8-byte Folded Spill
	xorq	%r15, %r15
	movq	$0, 144(%rsp)           # 8-byte Folded Spill
	movq	$0, 40(%rsp)            # 8-byte Folded Spill
	movq	$0, 72(%rsp)            # 8-byte Folded Spill
	.align	16, 0x90
.LBB39_30:                              # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	32(%rbx), %r14
	testq	%r14, %r14
	je	.LBB39_34
# BB#31:                                # %land.lhs.true29
                                        #   in Loop: Header=BB39_30 Depth=1
	movl	16(%r14), %eax
	testq	$8388608, %rax          # imm = 0x800000
	je	.LBB39_34
# BB#32:                                # %land.lhs.true29
                                        #   in Loop: Header=BB39_30 Depth=1
	movzbl	deprecated_state(%rip), %eax
	xorq	$1, %rax
	testq	$1, %rax
	je	.LBB39_34
# BB#33:                                # %if.then37
                                        #   in Loop: Header=BB39_30 Depth=1
	movq	%r14, %rdi
	callq	warn_deprecated_use
.LBB39_34:                              # %if.end39
                                        #   in Loop: Header=BB39_30 Depth=1
	movq	ridpointers(%rip), %rax
	movq	216(%rax), %rdx
	movq	$1, %rcx
	cmpq	%rdx, %r14
	movq	$1, %rdx
	je	.LBB39_36
# BB#35:                                # %select.mid8
                                        #   in Loop: Header=BB39_30 Depth=1
	movq	%r15, %rdx
.LBB39_36:                              # %select.end7
                                        #   in Loop: Header=BB39_30 Depth=1
	movq	224(%rax), %rax
	movq	%rdx, %r15
	cmpq	%rax, %r14
	je	.LBB39_38
# BB#37:                                # %select.mid37
                                        #   in Loop: Header=BB39_30 Depth=1
	movq	144(%rsp), %rcx         # 8-byte Reload
.LBB39_38:                              # %select.end36
                                        #   in Loop: Header=BB39_30 Depth=1
	movl	16(%r14), %eax
	movq	%rcx, 144(%rsp)         # 8-byte Spill
	movq	%rax, %rcx
	andq	$16777471, %rcx         # imm = 0x10000FF
	cmpq	$16777217, %rcx         # imm = 0x1000001
	jne	.LBB39_56
# BB#39:                                # %if.then62
                                        #   in Loop: Header=BB39_30 Depth=1
	movzbl	43(%r14), %r13d
	cmpq	$26, %r13
	ja	.LBB39_56
# BB#40:                                # %if.then66
                                        #   in Loop: Header=BB39_30 Depth=1
	movq	$1, %rax
	movq	%r13, %rcx
	shlq	%cl, %rax
	andq	%r12, %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	cmpq	$2, %r13
	jne	.LBB39_53
# BB#41:                                # %land.lhs.true69
                                        #   in Loop: Header=BB39_30 Depth=1
	testq	%rcx, %rax
	je	.LBB39_55
# BB#42:                                # %if.then71
                                        #   in Loop: Header=BB39_30 Depth=1
	movq	120(%rsp), %rax         # 8-byte Reload
	testq	%rcx, %rax
	jne	.LBB39_43
# BB#44:                                # %if.else
                                        #   in Loop: Header=BB39_30 Depth=1
	movl	pedantic(%rip), %eax
	testq	%rax, %rax
	movabsq	$0, %rsi
	movq	%rsi, %rax
	je	.LBB39_46
# BB#45:                                # %if.else
                                        #   in Loop: Header=BB39_30 Depth=1
	movabsq	$1, %rax
.LBB39_46:                              # %if.else
                                        #   in Loop: Header=BB39_30 Depth=1
	movl	flag_isoc99(%rip), %ecx
	testq	%rcx, %rcx
	movq	%rsi, %rcx
	jne	.LBB39_48
# BB#47:                                # %if.else
                                        #   in Loop: Header=BB39_30 Depth=1
	movabsq	$1, %rcx
.LBB39_48:                              # %if.else
                                        #   in Loop: Header=BB39_30 Depth=1
	movl	in_system_header(%rip), %edx
	testq	%rdx, %rdx
	movq	%rsi, %rdx
	jne	.LBB39_50
# BB#49:                                # %if.else
                                        #   in Loop: Header=BB39_30 Depth=1
	movabsq	$1, %rdx
.LBB39_50:                              # %if.else
                                        #   in Loop: Header=BB39_30 Depth=1
	movl	warn_long_long(%rip), %esi
	testq	%rsi, %rsi
	movq	$1, 120(%rsp)           # 8-byte Folded Spill
	je	.LBB39_55
# BB#51:                                # %if.else
                                        #   in Loop: Header=BB39_30 Depth=1
	andq	%rcx, %rax
	andq	%rdx, %rax
	je	.LBB39_55
# BB#52:                                # %if.then81
                                        #   in Loop: Header=BB39_30 Depth=1
	xorq	%rax, %rax
	movabsq	$.L.str264, %rdi
	callq	pedwarn
	jmp	.LBB39_55
	.align	16, 0x90
.LBB39_56:                              # %if.end96
                                        #   in Loop: Header=BB39_30 Depth=1
	cmpq	$0, 184(%rsp)
	jne	.LBB39_57
# BB#58:                                # %if.else99
                                        #   in Loop: Header=BB39_30 Depth=1
	andq	$255, %rax
	je	.LBB39_72
# BB#59:                                # %if.else99
                                        #   in Loop: Header=BB39_30 Depth=1
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	cmpq	$1, %rax
	jne	.LBB39_60
# BB#63:                                # %if.then124
                                        #   in Loop: Header=BB39_30 Depth=1
	movq	current_binding_level(%rip), %rax
	movq	global_binding_level(%rip), %rcx
	cmpq	%rcx, %rax
	je	.LBB39_65
# BB#64:                                # %land.lhs.true.i
                                        #   in Loop: Header=BB39_30 Depth=1
	movq	64(%r14), %rax
	testq	%rax, %rax
	jne	.LBB39_66
.LBB39_65:                              # %if.else.i
                                        #   in Loop: Header=BB39_30 Depth=1
	movq	56(%r14), %rax
.LBB39_66:                              # %lookup_name.exit
                                        #   in Loop: Header=BB39_30 Depth=1
	movq	8(%rax), %rcx
	movq	global_trees(%rip), %rdx
	cmpq	%rdx, %rcx
	je	.LBB39_72
# BB#67:                                # %if.else130
                                        #   in Loop: Header=BB39_30 Depth=1
	testq	%rax, %rax
	je	.LBB39_69
# BB#68:                                # %lor.lhs.false
                                        #   in Loop: Header=BB39_30 Depth=1
	movzbl	16(%rax), %edx
	cmpq	$33, %rdx
	jne	.LBB39_69
# BB#70:                                # %if.else142
                                        #   in Loop: Header=BB39_30 Depth=1
	movq	%rcx, 184(%rsp)
	movq	%rax, 72(%rsp)          # 8-byte Spill
	jmp	.LBB39_72
	.align	16, 0x90
.LBB39_57:                              # %if.then98
                                        #   in Loop: Header=BB39_30 Depth=1
	xorq	%rax, %rax
	movabsq	$.L.str266, %rdi
	movq	136(%rsp), %rsi         # 8-byte Reload
	callq	error
	jmp	.LBB39_72
.LBB39_53:                              # %if.else84
                                        #   in Loop: Header=BB39_30 Depth=1
	testq	%rcx, %rax
	je	.LBB39_55
# BB#54:                                # %if.then88
                                        #   in Loop: Header=BB39_30 Depth=1
	movq	32(%r14), %rsi
	xorq	%rax, %rax
	movabsq	$.L.str265, %rdi
	callq	pedwarn
	jmp	.LBB39_55
.LBB39_60:                              # %if.else99
                                        #   in Loop: Header=BB39_30 Depth=1
	cmpq	$33, %rax
	jne	.LBB39_71
# BB#61:                                # %if.then106
                                        #   in Loop: Header=BB39_30 Depth=1
	movq	8(%r14), %rax
	movq	global_trees(%rip), %rcx
	cmpq	%rcx, %rax
	je	.LBB39_72
# BB#62:                                # %if.else112
                                        #   in Loop: Header=BB39_30 Depth=1
	movq	%rax, 184(%rsp)
	movq	136(%r14), %rax
	movq	%rax, 40(%rsp)          # 8-byte Spill
	movq	%r14, 72(%rsp)          # 8-byte Spill
	jmp	.LBB39_72
.LBB39_71:                              # %if.then154
                                        #   in Loop: Header=BB39_30 Depth=1
	movq	%r14, 184(%rsp)
	jmp	.LBB39_72
.LBB39_69:                              # %if.then138
                                        #   in Loop: Header=BB39_30 Depth=1
	movq	32(%r14), %rsi
	xorq	%rax, %rax
	movabsq	$.L.str267, %rdi
	callq	error
	jmp	.LBB39_72
.LBB39_43:                              # %if.then73
                                        #   in Loop: Header=BB39_30 Depth=1
	xorq	%rax, %rax
	movabsq	$.L.str263, %rdi
	callq	error
	.align	16, 0x90
.LBB39_55:                              # %if.end93
                                        #   in Loop: Header=BB39_30 Depth=1
	movq	$1, %rax
	movq	%r13, %rcx
	shlq	%cl, %rax
	orq	%rax, %r12
.LBB39_72:                              # %for.inc
                                        #   in Loop: Header=BB39_30 Depth=1
	movq	(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB39_30
# BB#73:                                # %for.end
	movq	184(%rsp), %rax
	testq	%rax, %rax
	je	.LBB39_75
# BB#74:                                # %if.end166
	movl	60(%rax), %esi
	movq	$25, %rcx
	shrq	%cl, %rsi
	andq	$1, %rsi
	movq	$0, 48(%rsp)            # 8-byte Folded Spill
	movq	$1, 32(%rsp)            # 8-byte Folded Spill
	movq	%rax, 16(%rsp)          # 8-byte Spill
	movq	112(%rsp), %r14         # 8-byte Reload
	jmp	.LBB39_87
.LBB39_28:
	movq	$0, 120(%rsp)           # 8-byte Folded Spill
	xorq	%r15, %r15
	movq	$0, 144(%rsp)           # 8-byte Folded Spill
	movq	$0, 40(%rsp)            # 8-byte Folded Spill
	movq	$0, 72(%rsp)            # 8-byte Folded Spill
	jmp	.LBB39_76
.LBB39_75:                              # %if.then169
	testq	$33926, %r12            # imm = 0x8486
	movq	112(%rsp), %r14         # 8-byte Reload
	jne	.LBB39_86
.LBB39_76:                              # %land.lhs.true172
	movq	$6, %rcx
	movq	%r12, %rax
	shrq	%cl, %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	movq	80(%rsp), %rdx          # 8-byte Reload
	testq	%rcx, %rdx
	je	.LBB39_77
# BB#78:                                # %land.lhs.true172
	movabsq	$1, %rcx
	jmp	.LBB39_79
.LBB39_77:
	movabsq	$0, %rcx
.LBB39_79:                              # %land.lhs.true172
	testq	%rcx, %rax
	jne	.LBB39_86
# BB#80:                                # %land.lhs.true172
	movl	in_system_header(%rip), %eax
	movabsq	$4294967295, %rsi       # imm = 0xFFFFFFFF
	testq	%rsi, %rax
	jne	.LBB39_86
# BB#81:                                # %if.then179
	movl	warn_implicit_int(%rip), %eax
	movl	warn_return_type(%rip), %edx
	movl	flag_isoc99(%rip), %ecx
	orq	%rax, %rdx
	orq	%rcx, %rdx
	testq	%rsi, %rdx
	je	.LBB39_84
# BB#82:                                # %if.then179
	cmpq	$1, 128(%rsp)           # 8-byte Folded Reload
	jne	.LBB39_84
# BB#83:                                # %if.then187
	movq	$1, %rax
	movl	%eax, warn_about_return_type(%rip)
	jmp	.LBB39_86
.LBB39_17:                              # %sw.default
	movabsq	$.L.str185, %rdi
	movabsq	$.L__FUNCTION__.grokdeclarator, %rdx
	movq	$4116, %rsi             # imm = 0x1014
	callq	fancy_abort
.LBB39_84:                              # %if.else188
	orq	%rax, %rcx
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %rcx
	je	.LBB39_86
# BB#85:                                # %if.then192
	movabsq	$.L.str268, %rdi
	xorq	%rax, %rax
	movq	136(%rsp), %rsi         # 8-byte Reload
	callq	pedwarn_c99
.LBB39_86:                              # %if.end195
	movq	integer_types+40(%rip), %rax
	movq	%rax, 184(%rsp)
	movq	$1, 48(%rsp)            # 8-byte Folded Spill
	movq	$0, 32(%rsp)            # 8-byte Folded Spill
	movq	$0, 16(%rsp)            # 8-byte Folded Spill
	xorq	%rsi, %rsi
.LBB39_87:                              # %if.end196
	movq	%r12, %rbx
	movabsq	$4294967295, %r12       # imm = 0xFFFFFFFF
	testq	$4, %rbx
	je	.LBB39_91
# BB#88:                                # %if.end196
	movq	120(%rsp), %rcx         # 8-byte Reload
	testq	%r12, %rcx
	jne	.LBB39_91
# BB#89:                                # %land.lhs.true201
	movq	128(%rax), %rcx
	movq	global_trees+200(%rip), %rdx
	cmpq	%rdx, %rcx
	jne	.LBB39_91
# BB#90:                                # %if.then205
	movq	global_trees+208(%rip), %rax
	movabsq	$4294967291, %rcx       # imm = 0xFFFFFFFB
	andq	%rcx, %rbx
	movq	%rax, 184(%rsp)
.LBB39_91:                              # %if.end207
	movq	%rsi, 88(%rsp)          # 8-byte Spill
	testq	$1158, %rbx             # imm = 0x486
	je	.LBB39_92
# BB#93:                                # %if.then210
	movq	%rbx, %rsi
	andq	$4, %rsi
	jne	.LBB39_94
# BB#95:                                # %if.then210
	movabsq	$1, %rcx
	jmp	.LBB39_96
.LBB39_92:
	movq	%r14, 112(%rsp)         # 8-byte Spill
	movq	120(%rsp), %r14         # 8-byte Reload
	jmp	.LBB39_125
.LBB39_94:
	movabsq	$0, %rcx
.LBB39_96:                              # %if.then210
	movq	%r14, 112(%rsp)         # 8-byte Spill
	movq	%rbx, %rdx
	andq	$128, %rdx
	je	.LBB39_100
# BB#97:                                # %if.then210
	testq	%rcx, %rcx
	jne	.LBB39_100
# BB#98:                                # %if.then216
	movabsq	$.L.str269, %rdi
	jmp	.LBB39_99
.LBB39_100:                             # %if.else217
	testq	%r12, %rsi
	jne	.LBB39_102
# BB#101:                               # %lor.lhs.false220
	testq	%r12, %rdx
	je	.LBB39_104
.LBB39_102:                             # %land.lhs.true223
	movq	144(%rsp), %rsi         # 8-byte Reload
	testq	%r12, %rsi
	je	.LBB39_104
# BB#103:                               # %if.then225
	movabsq	$.L.str270, %rdi
	jmp	.LBB39_99
.LBB39_104:                             # %if.else226
	testq	%r12, %rdx
	jne	.LBB39_105
# BB#106:                               # %if.else226
	movabsq	$1, %rdx
	jmp	.LBB39_107
.LBB39_105:
	movabsq	$0, %rdx
.LBB39_107:                             # %if.else226
	testq	%rcx, %rdx
	jne	.LBB39_112
# BB#108:                               # %land.lhs.true232
	movzbl	16(%rax), %ecx
	cmpq	$7, %rcx
	jne	.LBB39_112
# BB#109:                               # %if.then239
	movabsq	$.L.str271, %rdi
	xorq	%rax, %rax
	xorq	%r14, %r14
	movq	136(%rsp), %rsi         # 8-byte Reload
	callq	error
	movzbl	grokdeclarator.already(%rip), %eax
	testq	%rax, %rax
	jne	.LBB39_121
# BB#110:                               # %if.then239
	movl	pedantic(%rip), %eax
	testq	%r12, %rax
	jne	.LBB39_121
# BB#111:                               # %if.then243
	movabsq	$.L.str272, %rdi
	xorq	%rax, %rax
	xorq	%r14, %r14
	callq	error
	movq	$1, %rax
	movb	%al, grokdeclarator.already(%rip)
	jmp	.LBB39_121
.LBB39_112:                             # %if.else245
	movq	%rbx, %rcx
	andq	$1026, %rcx             # imm = 0x402
	cmpq	$1026, %rcx             # imm = 0x402
	jne	.LBB39_114
# BB#113:                               # %if.then251
	movabsq	$.L.str273, %rdi
	jmp	.LBB39_99
.LBB39_114:                             # %if.else252
	movzbl	16(%rax), %eax
	cmpq	$6, %rax
	jne	.LBB39_115
# BB#116:                               # %if.else260
	movq	%r15, %rax
	movq	144(%rsp), %rcx         # 8-byte Reload
	orq	%rcx, %rax
	movq	48(%rsp), %rcx          # 8-byte Reload
	orq	%rcx, %rax
	testq	%r12, %rax
	movq	$1, %r14
	jne	.LBB39_121
# BB#117:                               # %if.else260
	movl	pedantic(%rip), %eax
	testq	%r12, %rax
	je	.LBB39_121
# BB#118:                               # %if.then268
	movabsq	$.L.str275, %rdi
	xorq	%rax, %rax
	movq	136(%rsp), %rsi         # 8-byte Reload
	callq	pedwarn
	movl	flag_pedantic_errors(%rip), %eax
	testq	%rax, %rax
	jne	.LBB39_119
# BB#120:                               # %if.then268
	movabsq	$1, %r14
	jmp	.LBB39_121
.LBB39_115:                             # %if.then259
	movabsq	$.L.str274, %rdi
.LBB39_99:                              # %if.end277
	xorq	%rax, %rax
	xorq	%r14, %r14
	movq	136(%rsp), %rsi         # 8-byte Reload
	callq	error
.LBB39_121:                             # %if.end277
	movq	%rbx, %rax
	andq	$-1159, %rax            # imm = 0xFFFFFFFFFFFFFB79
	testq	%r14, %r14
	je	.LBB39_123
# BB#122:                               # %if.end277
	movq	120(%rsp), %r14         # 8-byte Reload
.LBB39_123:                             # %if.end277
	jne	.LBB39_125
# BB#124:                               # %if.end277
	movq	%rax, %rbx
.LBB39_125:                             # %if.end282
	testq	$32768, %rbx            # imm = 0x8000
	je	.LBB39_128
# BB#126:                               # %land.lhs.true285
	movq	184(%rsp), %rax
	movl	16(%rax), %eax
	andq	$254, %rax
	cmpq	$6, %rax
	je	.LBB39_128
# BB#127:                               # %if.then299
	movabsq	$.L.str276, %rdi
	xorq	%rax, %rax
	movq	136(%rsp), %rsi         # 8-byte Reload
	callq	error
	movabsq	$4294934527, %rax       # imm = 0xFFFF7FFF
	andq	%rax, %rbx
.LBB39_128:                             # %if.end301
	movabsq	$274877906944, %rdx     # imm = 0x4000000000
	testq	$2, %rbx
	jne	.LBB39_144
# BB#129:                               # %lor.lhs.false304
	movq	56(%rsp), %rax          # 8-byte Reload
	andq	%r12, %rax
	cmpq	$4, %rax
	jne	.LBB39_131
# BB#130:                               # %lor.lhs.false304
	movl	flag_traditional(%rip), %ecx
	testq	%r12, %rcx
	je	.LBB39_131
# BB#132:                               # %land.lhs.true308
	movl	explicit_flag_signed_bitfields(%rip), %ecx
	testq	%rcx, %rcx
	je	.LBB39_144
# BB#133:                               # %land.lhs.true308
	movl	flag_signed_bitfields(%rip), %ecx
	testq	%r12, %rcx
	jne	.LBB39_134
	jmp	.LBB39_144
.LBB39_131:                             # %lor.lhs.false304.lor.lhs.false312_crit_edge
	movl	flag_signed_bitfields(%rip), %ecx
.LBB39_134:                             # %lor.lhs.false312
	cmpq	$4, %rax
	je	.LBB39_135
# BB#136:                               # %lor.lhs.false312
	movabsq	$1, %rax
	jmp	.LBB39_137
.LBB39_135:
	movabsq	$0, %rax
.LBB39_137:                             # %lor.lhs.false312
	testq	%r12, %rcx
	jne	.LBB39_155
# BB#138:                               # %lor.lhs.false312
	testq	%rax, %rax
	jne	.LBB39_155
# BB#139:                               # %land.lhs.true316
	movq	144(%rsp), %rax         # 8-byte Reload
	orq	%rax, %r15
	movq	48(%rsp), %rax          # 8-byte Reload
	orq	%rax, %r15
	testq	%r12, %r15
	jne	.LBB39_142
# BB#140:                               # %land.lhs.true316
	cmpq	$0, 72(%rsp)            # 8-byte Folded Reload
	je	.LBB39_142
# BB#141:                               # %land.lhs.true325
	movq	72(%rsp), %rax          # 8-byte Reload
	movq	48(%rax), %rax
	testq	%rdx, %rax
	jne	.LBB39_155
.LBB39_142:                             # %land.lhs.true332
	movq	184(%rsp), %rax
	movzbl	16(%rax), %eax
	cmpq	$10, %rax
	je	.LBB39_155
# BB#143:                               # %land.lhs.true332
	movq	%rbx, %rax
	andq	$1024, %rax             # imm = 0x400
	testq	%r12, %rax
	jne	.LBB39_155
.LBB39_144:                             # %if.then342
	testq	%r12, %r14
	je	.LBB39_146
# BB#145:                               # %if.then344
	movq	integer_types+80(%rip), %rax
	movq	%rax, 184(%rsp)
	movq	%rbx, %rcx
	jmp	.LBB39_165
.LBB39_146:                             # %if.else345
	movq	%rbx, %rcx
	testq	$4, %rcx
	je	.LBB39_148
# BB#147:                               # %if.then348
	movq	integer_types+64(%rip), %rax
	jmp	.LBB39_164
.LBB39_155:                             # %if.else367
	movq	%rbx, %rcx
	testq	$1024, %rcx             # imm = 0x400
	je	.LBB39_158
# BB#156:                               # %land.lhs.true370
	movq	184(%rsp), %rax
	movq	%rcx, %rdx
	movq	integer_types(%rip), %rcx
	cmpq	%rcx, %rax
	movq	%rdx, %rcx
	je	.LBB39_157
.LBB39_158:                             # %if.else374
	testq	%r12, %r14
	je	.LBB39_160
# BB#159:                               # %if.then376
	movq	integer_types+72(%rip), %rax
	jmp	.LBB39_164
.LBB39_148:                             # %if.else349
	testq	$128, %rcx
	je	.LBB39_150
# BB#149:                               # %if.then352
	movq	integer_types+32(%rip), %rax
	jmp	.LBB39_164
.LBB39_160:                             # %if.else377
	testq	$4, %rcx
	je	.LBB39_162
# BB#161:                               # %if.then380
	movq	integer_types+56(%rip), %rax
	jmp	.LBB39_164
.LBB39_157:                             # %if.then373
	movq	integer_types+8(%rip), %rax
	jmp	.LBB39_164
.LBB39_150:                             # %if.else353
	movq	184(%rsp), %rdi
	movq	integer_types(%rip), %rax
	cmpq	%rax, %rdi
	je	.LBB39_151
# BB#152:                               # %if.else357
	cmpq	$0, 72(%rsp)            # 8-byte Folded Reload
	je	.LBB39_154
# BB#153:                               # %if.then359
	movq	%rcx, %rbx
	callq	unsigned_type
	movq	%rbx, %rcx
	jmp	.LBB39_164
.LBB39_162:                             # %if.else381
	testq	$128, %rcx
	je	.LBB39_165
# BB#163:                               # %if.then384
	movq	integer_types+24(%rip), %rax
	jmp	.LBB39_164
.LBB39_151:                             # %if.then356
	movq	integer_types+16(%rip), %rax
.LBB39_164:                             # %if.end389
	movq	%rax, 184(%rsp)
.LBB39_165:                             # %if.end389
	testq	$32768, %rcx            # imm = 0x8000
	je	.LBB39_166
# BB#167:                               # %if.then392
	movl	pedantic(%rip), %eax
	testq	%rax, %rax
	je	.LBB39_170
# BB#168:                               # %if.then392
	movl	flag_isoc99(%rip), %eax
	testq	%r12, %rax
	jne	.LBB39_170
# BB#169:                               # %if.then396
	movabsq	$.L.str277, %rdi
	xorq	%rax, %rax
	movq	%rcx, %rbx
	callq	pedwarn
	movq	%rbx, %rcx
.LBB39_170:                             # %if.end397
	testq	%r12, %r14
	jne	.LBB39_176
# BB#171:                               # %if.end397
	cmpq	$0, 48(%rsp)            # 8-byte Folded Reload
	je	.LBB39_176
# BB#172:                               # %if.end397
	movq	%rcx, %rax
	andq	$1158, %rax             # imm = 0x486
	testq	%r12, %rax
	jne	.LBB39_176
# BB#173:                               # %if.then404
	movl	pedantic(%rip), %eax
	testq	%rax, %rax
	je	.LBB39_175
# BB#174:                               # %if.then406
	movabsq	$.L.str278, %rdi
	xorq	%rax, %rax
	movq	%rcx, %rbx
	callq	pedwarn
	movq	%rbx, %rcx
	jmp	.LBB39_175
.LBB39_166:                             # %if.end389.if.end437_crit_edge
	movq	184(%rsp), %rax
	jmp	.LBB39_188
.LBB39_176:                             # %if.else408
	movq	184(%rsp), %rdi
	movq	integer_types+40(%rip), %rax
	cmpq	%rax, %rdi
	je	.LBB39_177
# BB#180:                               # %if.else415
	movq	global_trees+192(%rip), %rax
	cmpq	%rax, %rdi
	je	.LBB39_181
# BB#182:                               # %if.else419
	movq	global_trees+200(%rip), %rax
	cmpq	%rax, %rdi
	je	.LBB39_175
# BB#183:                               # %if.else423
	movq	global_trees+208(%rip), %rax
	cmpq	%rax, %rdi
	je	.LBB39_184
# BB#185:                               # %if.else427
	movq	%rcx, %rbx
	movl	pedantic(%rip), %eax
	testq	%rax, %rax
	je	.LBB39_187
# BB#186:                               # %if.then429
	movabsq	$.L.str279, %rdi
	xorq	%rax, %rax
	callq	pedwarn
	movq	184(%rsp), %rdi
.LBB39_187:                             # %if.end430
	callq	build_complex_type
	movq	%rax, 184(%rsp)
	movq	%rbx, %rcx
	jmp	.LBB39_188
.LBB39_175:                             # %if.end407
	movq	global_trees+176(%rip), %rax
	movq	%rax, 184(%rsp)
	jmp	.LBB39_188
.LBB39_177:                             # %if.then411
	movl	pedantic(%rip), %eax
	testq	%rax, %rax
	je	.LBB39_179
# BB#178:                               # %if.then413
	movabsq	$.L.str279, %rdi
	xorq	%rax, %rax
	movq	%rcx, %rbx
	callq	pedwarn
	movq	%rbx, %rcx
.LBB39_179:                             # %if.end414
	movq	global_trees+160(%rip), %rax
	movq	%rax, 184(%rsp)
	jmp	.LBB39_188
.LBB39_181:                             # %if.then418
	movq	global_trees+168(%rip), %rax
	movq	%rax, 184(%rsp)
	jmp	.LBB39_188
.LBB39_184:                             # %if.then426
	movq	global_trees+184(%rip), %rax
	movq	%rax, 184(%rsp)
.LBB39_188:                             # %if.end437
	movq	%rcx, 104(%rsp)         # 8-byte Spill
	movq	%rcx, %r13
	andq	%r12, %r13
	movq	$3, %rcx
	movq	%r13, %rdx
	shrq	%cl, %rdx
	andq	$1, %rdx
	movl	16(%rax), %r14d
	movq	$12, %rcx
	movq	%r12, %rsi
	movq	%r14, %r12
	shrq	%cl, %r12
	andq	$1, %r12
	addq	%rdx, %r12
	movq	$12, %rcx
	movq	%r13, %rbx
	shrq	%cl, %rbx
	movl	60(%rax), %r15d
	movq	$21, %rcx
	shrq	%cl, %r15
	movq	$9, %rcx
	shrq	%cl, %r13
	movl	flag_isoc99(%rip), %eax
	movq	$11, %rcx
	shrq	%cl, %r14
	cmpq	$2, %r12
	jl	.LBB39_191
# BB#189:                               # %if.end437
	testq	%rsi, %rax
	jne	.LBB39_191
# BB#190:                               # %if.then477
	movabsq	$.L.str280, %rdi
	xorq	%rax, %rax
	callq	pedwarn
	movl	flag_isoc99(%rip), %eax
.LBB39_191:                             # %if.end478
	andq	$1, %rbx
	andq	$1, %r15
	addq	%rbx, %r15
	cmpq	$2, %r15
	jl	.LBB39_194
# BB#192:                               # %if.end478
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB39_194
# BB#193:                               # %if.then483
	movabsq	$.L.str281, %rdi
	xorq	%rax, %rax
	callq	pedwarn
	movl	flag_isoc99(%rip), %eax
.LBB39_194:                             # %if.end484
	andq	$1, %r13
	andq	$1, %r14
	addq	%r13, %r14
	cmpq	$2, %r14
	jl	.LBB39_197
# BB#195:                               # %if.end484
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB39_197
# BB#196:                               # %if.then489
	movabsq	$.L.str282, %rdi
	xorq	%rax, %rax
	callq	pedwarn
.LBB39_197:                             # %if.end490
	movl	flag_gen_aux_info(%rip), %eax
	testq	%rax, %rax
	jne	.LBB39_204
# BB#198:                               # %land.lhs.true492
	movq	184(%rsp), %r8
	movl	16(%r8), %esi
	movq	$12, %rcx
	movq	%rsi, %rax
	shrq	%cl, %rax
	movq	$10, %rcx
	movq	%rsi, %rdi
	shrq	%cl, %rdi
	movl	60(%r8), %edx
	movq	$19, %rcx
	shrq	%cl, %rdx
	andq	$255, %rsi
	xorq	%rbx, %rbx
	cmpq	$20, %rsi
	jne	.LBB39_202
# BB#199:                               # %land.rhs
	cmpq	$0, 8(%r8)
	je	.LBB39_200
# BB#201:                               # %land.rhs
	movabsq	$1, %rbx
	jmp	.LBB39_202
.LBB39_200:
	movabsq	$0, %rbx
.LBB39_202:                             # %land.end
	andq	$1, %rax
	andq	$2, %rdi
	orq	%rax, %rdi
	andq	$4, %rdx
	orq	%rdi, %rdx
	movq	$3, %rcx
	shlq	%cl, %rbx
	orq	%rdx, %rbx
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %rbx
	je	.LBB39_204
# BB#203:                               # %if.then524
	movq	128(%r8), %rax
	movq	%rax, 184(%rsp)
.LBB39_204:                             # %if.end527
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %r12
	movq	%rax, %r12
	movabsq	$0, %rdi
	movabsq	$1, %rdx
	movq	%rdi, %rax
	je	.LBB39_206
# BB#205:                               # %if.end527
	movq	%rdx, %rax
.LBB39_206:                             # %if.end527
	testq	%r12, %r15
	movq	%rdi, %rsi
	je	.LBB39_208
# BB#207:                               # %if.end527
	movq	%rdx, %rsi
.LBB39_208:                             # %if.end527
	movq	$2, %rcx
	shlq	%cl, %rsi
	testq	%r12, %r14
	je	.LBB39_210
# BB#209:                               # %if.end527
	movq	%rdx, %rdi
.LBB39_210:                             # %if.end527
	movq	104(%rsp), %rbx         # 8-byte Reload
	movq	%rbx, %r9
	andq	$256, %r9               # imm = 0x100
	addq	%rdi, %rdi
	orq	%rax, %rdi
	orq	%rsi, %rdi
	movq	%rdi, 144(%rsp)         # 8-byte Spill
	movq	%rbx, %r8
	andq	$2048, %r8              # imm = 0x800
	movq	$11, %rcx
	movq	%r8, %rdx
	shrq	%cl, %rdx
	movq	%rbx, %rsi
	andq	$1, %rsi
	addq	%rdx, %rsi
	movq	%rbx, %rdx
	andq	$16, %rdx
	movq	$4, %rcx
	movq	%rdx, %rdi
	shrq	%cl, %rdi
	addq	%rsi, %rdi
	movq	%rbx, %r14
	andq	$32, %r14
	movq	$5, %rcx
	movq	%r14, %rax
	shrq	%cl, %rax
	addq	%rdi, %rax
	andq	$64, %rbx
	movq	$6, %rcx
	movq	%rbx, %rsi
	shrq	%cl, %rsi
	addq	%rax, %rsi
	movq	$32, %rcx
	movq	%rsi, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$2, %rax
	jl	.LBB39_212
# BB#211:                               # %if.then561
	movq	%r9, 24(%rsp)           # 8-byte Spill
	movabsq	$.L.str283, %rdi
	xorq	%rax, %rax
	movq	136(%rsp), %rsi         # 8-byte Reload
	callq	error
	movq	96(%rsp), %rbx          # 8-byte Reload
	jmp	.LBB39_258
.LBB39_212:                             # %if.else562
	cmpq	$1, 128(%rsp)           # 8-byte Folded Reload
	je	.LBB39_213
# BB#214:                               # %if.else562
	movabsq	$1, %rcx
	jmp	.LBB39_215
.LBB39_213:
	movabsq	$0, %rcx
.LBB39_215:                             # %if.else562
	testq	$2144, 104(%rsp)        # 8-byte Folded Reload
                                        # imm = 0x860
	je	.LBB39_226
# BB#216:                               # %if.else562
	testq	%rcx, %rcx
	jne	.LBB39_226
# BB#217:                               # %if.then567
	movq	%r9, 24(%rsp)           # 8-byte Spill
	testq	%r12, %r8
	je	.LBB39_221
# BB#218:                               # %land.lhs.true570
	movl	pedantic(%rip), %eax
	testq	%rax, %rax
	jne	.LBB39_220
# BB#219:                               # %lor.lhs.false572
	movq	current_binding_level(%rip), %rax
	movq	global_binding_level(%rip), %rcx
	cmpq	%rcx, %rax
	jne	.LBB39_221
.LBB39_220:                             # %if.then575
	movabsq	$.L.str284, %rdi
	xorq	%rax, %rax
	callq	pedwarn
.LBB39_221:                             # %if.end576
	testq	%r12, %r14
	je	.LBB39_223
# BB#222:                               # %if.then579
	movabsq	$.L.str285, %rdi
	xorq	%rax, %rax
	callq	error
.LBB39_223:                             # %if.end580
	testq	%r12, %rbx
	je	.LBB39_225
# BB#224:                               # %if.then583
	movabsq	$.L.str286, %rdi
	xorq	%rax, %rax
	callq	error
.LBB39_225:                             # %if.end584
	movabsq	$4294965151, %rax       # imm = 0xFFFFF79F
	andq	%rax, 104(%rsp)         # 8-byte Folded Spill
	movq	96(%rsp), %rbx          # 8-byte Reload
	jmp	.LBB39_258
.LBB39_226:                             # %if.else586
	movq	112(%rsp), %rax         # 8-byte Reload
	testq	%r12, %rax
	movq	96(%rsp), %rbx          # 8-byte Reload
	je	.LBB39_241
# BB#227:                               # %if.else586
	movq	$32, %rcx
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	testq	%rsi, %rsi
	jle	.LBB39_241
# BB#228:                               # %if.then592
	movq	112(%rsp), %rax         # 8-byte Reload
	andq	%r12, %rax
	testq	%r12, %r14
	jne	.LBB39_229
# BB#230:                               # %if.then592
	movabsq	$1, %rcx
	jmp	.LBB39_231
.LBB39_241:                             # %if.else606
	testq	%r12, %rdx
	movabsq	$0, %rcx
	movabsq	$1, %rdi
	movq	%rcx, %rsi
	jne	.LBB39_243
# BB#242:                               # %if.else606
	movq	%rdi, %rsi
.LBB39_243:                             # %if.else606
	movq	80(%rsp), %rax          # 8-byte Reload
	testq	%r12, %rax
	jne	.LBB39_245
# BB#244:                               # %if.else606
	movq	%rdi, %rcx
.LBB39_245:                             # %if.else606
	cmpq	$1, 128(%rsp)           # 8-byte Folded Reload
	je	.LBB39_250
# BB#246:                               # %if.else606
	orq	%rcx, %rsi
	jne	.LBB39_250
# BB#247:                               # %if.then613
	movq	%r9, 24(%rsp)           # 8-byte Spill
	movq	current_binding_level(%rip), %rax
	movq	global_binding_level(%rip), %rcx
	cmpq	%rcx, %rax
	je	.LBB39_248
# BB#249:                               # %if.else617
	movabsq	$.L.str291, %rdi
	jmp	.LBB39_257
.LBB39_229:
	movabsq	$0, %rcx
.LBB39_231:                             # %if.then592
	cmpq	$2, %rax
	jne	.LBB39_234
# BB#232:                               # %if.then592
	testq	%rcx, %rcx
	jne	.LBB39_234
# BB#233:
	movq	%r9, 24(%rsp)           # 8-byte Spill
	jmp	.LBB39_258
.LBB39_250:                             # %if.else619
	movq	%r9, 24(%rsp)           # 8-byte Spill
	movq	current_binding_level(%rip), %rcx
	movq	global_binding_level(%rip), %rsi
	testq	%r12, %rdx
	je	.LBB39_254
# BB#251:                               # %if.else619
	cmpq	$1, 128(%rsp)           # 8-byte Folded Reload
	jne	.LBB39_254
# BB#252:                               # %land.lhs.true624
	cmpq	%rsi, %rcx
	je	.LBB39_255
# BB#253:                               # %if.then627
	movabsq	$.L.str292, %rdi
	jmp	.LBB39_257
.LBB39_234:                             # %if.else599
	cmpq	$2, %rax
	jne	.LBB39_235
# BB#238:                               # %sw.bb601
	movq	%r9, 24(%rsp)           # 8-byte Spill
	movabsq	$.L.str288, %rdi
	jmp	.LBB39_237
.LBB39_254:                             # %if.else628
	cmpq	%rsi, %rcx
	jne	.LBB39_258
.LBB39_255:                             # %land.lhs.true631
	testq	%r12, %r8
	je	.LBB39_258
# BB#256:                               # %if.then634
	movabsq	$.L.str293, %rdi
.LBB39_257:                             # %while.cond641.preheader
	xorq	%rax, %rax
	movq	136(%rsp), %rsi         # 8-byte Reload
	callq	error
	jmp	.LBB39_258
.LBB39_235:                             # %if.else599
	movq	%r9, 24(%rsp)           # 8-byte Spill
	cmpq	$3, %rax
	jne	.LBB39_239
# BB#236:                               # %sw.bb600
	movabsq	$.L.str287, %rdi
.LBB39_237:                             # %sw.epilog603
	xorq	%rax, %rax
	movq	136(%rsp), %rsi         # 8-byte Reload
	callq	error
	jmp	.LBB39_240
.LBB39_248:                             # %if.then616
	movabsq	$.L.str290, %rdi
	xorq	%rax, %rax
	movq	136(%rsp), %rsi         # 8-byte Reload
	callq	warning
	jmp	.LBB39_258
.LBB39_239:                             # %sw.default602
	movabsq	$.L.str289, %rdi
	xorq	%rax, %rax
	callq	error
.LBB39_240:                             # %sw.epilog603
	movabsq	$4294965134, %rax       # imm = 0xFFFFF78E
	andq	%rax, 104(%rsp)         # 8-byte Folded Spill
.LBB39_258:                             # %while.cond641.preheader
	movq	$0, 64(%rsp)            # 8-byte Folded Spill
	xorq	%r15, %r15
	testq	%rbx, %rbx
	je	.LBB39_259
# BB#277:                               # %land.end650.lr.ph
	xorq	%r13, %r13
	xorq	%rdi, %rdi
	jmp	.LBB39_278
	.align	16, 0x90
.LBB39_280:                             # %while.body652
                                        #   in Loop: Header=BB39_278 Depth=1
	movq	global_trees(%rip), %rdx
	cmpq	%rdx, %rax
	jne	.LBB39_283
# BB#281:                               # %if.then655
                                        #   in Loop: Header=BB39_278 Depth=1
	movq	32(%rbx), %r14
	jmp	.LBB39_282
	.align	16, 0x90
.LBB39_283:                             # %if.end659
                                        #   in Loop: Header=BB39_278 Depth=1
	testq	%rdi, %rdi
	jne	.LBB39_285
# BB#284:                               # %if.end659
                                        #   in Loop: Header=BB39_278 Depth=1
	testq	%r12, %r13
	je	.LBB39_286
.LBB39_285:                             # %if.then664
                                        #   in Loop: Header=BB39_278 Depth=1
	xorq	%rax, %rax
	movabsq	$.L.str294, %rdi
	callq	error
	movl	16(%rbx), %ecx
.LBB39_286:                             # %if.end665
                                        #   in Loop: Header=BB39_278 Depth=1
	andq	$255, %rcx
	cmpq	$40, %rcx
	jg	.LBB39_293
# BB#287:                               # %if.end665
                                        #   in Loop: Header=BB39_278 Depth=1
	cmpq	$2, %rcx
	jne	.LBB39_459
# BB#288:                               # %if.then672
                                        #   in Loop: Header=BB39_278 Depth=1
	movq	32(%rbx), %r14
	movq	24(%rbx), %rsi
	testq	%r14, %r14
	movq	$1, %rbx
	movq	%r14, %rax
	je	.LBB39_302
	.align	16, 0x90
.LBB39_289:                             # %land.rhs679
                                        #   Parent Loop BB39_278 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movzbl	16(%rax), %ecx
	cmpq	$53, %rcx
	je	.LBB39_301
# BB#290:                               # %land.rhs679
                                        #   in Loop: Header=BB39_289 Depth=2
	cmpq	$1, %rcx
	je	.LBB39_302
# BB#291:                               # %land.rhs679
                                        #   in Loop: Header=BB39_289 Depth=2
	cmpq	$2, %rcx
	jne	.LBB39_298
# BB#292:                               # %while.body688
                                        #   in Loop: Header=BB39_289 Depth=2
	movq	32(%rax), %rax
	testq	%rax, %rax
	jne	.LBB39_289
	jmp	.LBB39_302
	.align	16, 0x90
.LBB39_293:                             # %if.end665
                                        #   in Loop: Header=BB39_278 Depth=1
	cmpq	$41, %rcx
	je	.LBB39_424
# BB#294:                               # %if.end665
                                        #   in Loop: Header=BB39_278 Depth=1
	cmpq	$43, %rcx
	jne	.LBB39_295
# BB#303:                               # %if.then733
                                        #   in Loop: Header=BB39_278 Depth=1
	movq	%r15, 120(%rsp)         # 8-byte Spill
	movq	%rbx, %r14
	movq	40(%r14), %rbx
	movq	sizetype_tab(%rip), %rdi
	callq	signed_type
	movq	184(%rsp), %rdx
	movq	%rax, 96(%rsp)          # 8-byte Spill
	movq	8(%r14), %r15
	movl	16(%rdx), %eax
	movl	16(%r14), %r13d
	movq	32(%r14), %r14
	movq	$18, %rcx
	shrq	%cl, %r13
	movq	%rax, %rcx
	andq	$255, %rcx
	cmpq	$5, %rcx
	jne	.LBB39_305
# BB#304:                               # %if.then753
                                        #   in Loop: Header=BB39_278 Depth=1
	xorq	%rax, %rax
	movabsq	$.L.str295, %rdi
	movq	136(%rsp), %rsi         # 8-byte Reload
	callq	error
	movq	global_trees(%rip), %rdx
	movq	%rdx, 184(%rsp)
	movl	16(%rdx), %eax
.LBB39_305:                             # %if.end754
                                        #   in Loop: Header=BB39_278 Depth=1
	andq	$1, %r13
	andq	$255, %rax
	cmpq	$23, %rax
	jne	.LBB39_306
# BB#307:                               # %if.then761
                                        #   in Loop: Header=BB39_278 Depth=1
	xorq	%rax, %rax
	movabsq	$.L.str296, %rdi
	movq	136(%rsp), %rsi         # 8-byte Reload
	callq	error
	movq	global_trees(%rip), %rdx
	movq	%rdx, 184(%rsp)
	movq	%rdx, %rax
	jmp	.LBB39_308
.LBB39_424:                             # %if.then1065
                                        #   in Loop: Header=BB39_278 Depth=1
	movl	pedantic(%rip), %eax
	testq	%rax, %rax
	je	.LBB39_428
# BB#425:                               # %land.lhs.true1067
                                        #   in Loop: Header=BB39_278 Depth=1
	movq	184(%rsp), %rax
	movzbl	16(%rax), %eax
	cmpq	$23, %rax
	jne	.LBB39_428
# BB#426:                               # %land.lhs.true1067
                                        #   in Loop: Header=BB39_278 Depth=1
	movq	144(%rsp), %rax         # 8-byte Reload
	testq	%r12, %rax
	je	.LBB39_428
# BB#427:                               # %if.then1076
                                        #   in Loop: Header=BB39_278 Depth=1
	xorq	%rax, %rax
	movabsq	$.L.str309, %rdi
	callq	pedwarn
.LBB39_428:                             # %if.end1077
                                        #   in Loop: Header=BB39_278 Depth=1
	movq	%r15, 120(%rsp)         # 8-byte Spill
	movq	184(%rsp), %rdi
	movq	144(%rsp), %rsi         # 8-byte Reload
	movq	%r12, %r15
	testq	%r12, %rsi
	je	.LBB39_430
# BB#429:                               # %if.then1079
                                        #   in Loop: Header=BB39_278 Depth=1
	callq	c_build_qualified_type
	movq	%rax, %rdi
	movq	%rdi, 184(%rsp)
.LBB39_430:                             # %if.end1081
                                        #   in Loop: Header=BB39_278 Depth=1
	callq	build_pointer_type
	movq	%rax, 184(%rsp)
	movq	8(%rbx), %rax
	movq	%rbx, %r13
	xorq	%rdi, %rdi
	testq	%rax, %rax
	movabsq	$0, %rcx
	movabsq	$0, %r12
	movabsq	$0, %rbx
	movabsq	$0, %r14
	movabsq	$0, %rsi
	je	.LBB39_458
	.align	16, 0x90
.LBB39_431:                             # %for.body1091
                                        #   Parent Loop BB39_278 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	32(%rax), %rdx
	movzbl	19(%rdx), %esi
	testq	$1, %rsi
	je	.LBB39_438
# BB#432:                               # %if.then1100
                                        #   in Loop: Header=BB39_431 Depth=2
	movzbl	43(%rdx), %edx
	cmpq	$12, %rdx
	je	.LBB39_437
# BB#433:                               # %if.then1100
                                        #   in Loop: Header=BB39_431 Depth=2
	cmpq	$9, %rdx
	jne	.LBB39_434
# BB#436:                               # %if.then1114
                                        #   in Loop: Header=BB39_431 Depth=2
	incq	%r14
	jmp	.LBB39_439
.LBB39_437:                             # %if.then1122
                                        #   in Loop: Header=BB39_431 Depth=2
	incq	%rbx
	jmp	.LBB39_439
.LBB39_434:                             # %if.then1100
                                        #   in Loop: Header=BB39_431 Depth=2
	cmpq	$3, %rdx
	jne	.LBB39_438
# BB#435:                               # %if.then1106
                                        #   in Loop: Header=BB39_431 Depth=2
	incq	%r12
	jmp	.LBB39_439
	.align	16, 0x90
.LBB39_438:                             # %if.else1129
                                        #   in Loop: Header=BB39_431 Depth=2
	incq	%rcx
.LBB39_439:                             # %for.inc1132
                                        #   in Loop: Header=BB39_431 Depth=2
	movq	(%rax), %rax
	testq	%rax, %rax
	jne	.LBB39_431
# BB#440:                               # %for.end1135
                                        #   in Loop: Header=BB39_278 Depth=1
	testq	%r15, %rcx
	je	.LBB39_442
# BB#441:                               # %if.then1137
                                        #   in Loop: Header=BB39_278 Depth=1
	xorq	%rax, %rax
	movabsq	$.L.str310, %rdi
	callq	error
.LBB39_442:                             # %if.end1138
                                        #   in Loop: Header=BB39_278 Depth=1
	movq	$32, %rcx
	movq	%r12, %rdx
	shlq	%cl, %rdx
	movl	flag_isoc99(%rip), %eax
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	$2, %rdx
	jl	.LBB39_445
# BB#443:                               # %if.end1138
                                        #   in Loop: Header=BB39_278 Depth=1
	testq	%r15, %rax
	jne	.LBB39_445
# BB#444:                               # %if.then1143
                                        #   in Loop: Header=BB39_278 Depth=1
	xorq	%rax, %rax
	movabsq	$.L.str280, %rdi
	callq	pedwarn
	movl	flag_isoc99(%rip), %eax
.LBB39_445:                             # %if.end1144
                                        #   in Loop: Header=BB39_278 Depth=1
	movq	$32, %rcx
	movq	%r14, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	$2, %rdx
	jl	.LBB39_448
# BB#446:                               # %if.end1144
                                        #   in Loop: Header=BB39_278 Depth=1
	testq	%r15, %rax
	jne	.LBB39_448
# BB#447:                               # %if.then1149
                                        #   in Loop: Header=BB39_278 Depth=1
	xorq	%rax, %rax
	movabsq	$.L.str282, %rdi
	callq	pedwarn
	movl	flag_isoc99(%rip), %eax
.LBB39_448:                             # %if.end1150
                                        #   in Loop: Header=BB39_278 Depth=1
	movq	$32, %rcx
	movq	%rbx, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	$2, %rdx
	jl	.LBB39_451
# BB#449:                               # %if.end1150
                                        #   in Loop: Header=BB39_278 Depth=1
	testq	%r15, %rax
	jne	.LBB39_451
# BB#450:                               # %if.then1155
                                        #   in Loop: Header=BB39_278 Depth=1
	xorq	%rax, %rax
	movabsq	$.L.str281, %rdi
	callq	pedwarn
.LBB39_451:                             # %if.end1156
                                        #   in Loop: Header=BB39_278 Depth=1
	testq	%r15, %r12
	movq	%r15, %rdx
	movabsq	$0, %rax
	movabsq	$0, %rdi
	je	.LBB39_453
# BB#452:                               # %if.end1156
                                        #   in Loop: Header=BB39_278 Depth=1
	movabsq	$1, %rax
.LBB39_453:                             # %if.end1156
                                        #   in Loop: Header=BB39_278 Depth=1
	testq	%rdx, %rbx
	movabsq	$0, %rsi
	je	.LBB39_455
# BB#454:                               # %if.end1156
                                        #   in Loop: Header=BB39_278 Depth=1
	movabsq	$1, %rsi
.LBB39_455:                             # %if.end1156
                                        #   in Loop: Header=BB39_278 Depth=1
	movq	$2, %rcx
	shlq	%cl, %rsi
	testq	%rdx, %r14
	movq	%rdx, %r15
	movabsq	$0, %rcx
	je	.LBB39_457
# BB#456:                               # %if.end1156
                                        #   in Loop: Header=BB39_278 Depth=1
	movabsq	$1, %rcx
.LBB39_457:                             # %if.end1156
                                        #   in Loop: Header=BB39_278 Depth=1
	addq	%rcx, %rcx
	orq	%rcx, %rsi
	orq	%rax, %rsi
.LBB39_458:                             # %if.end1165
                                        #   in Loop: Header=BB39_278 Depth=1
	movq	%rsi, 144(%rsp)         # 8-byte Spill
	movq	32(%r13), %r14
	movq	$0, 88(%rsp)            # 8-byte Folded Spill
	xorq	%r13, %r13
	movq	%r15, %r12
	movq	120(%rsp), %r15         # 8-byte Reload
	jmp	.LBB39_282
.LBB39_295:                             # %if.end665
                                        #   in Loop: Header=BB39_278 Depth=1
	cmpq	$53, %rcx
	jne	.LBB39_459
# BB#296:                               # %if.then968
                                        #   in Loop: Header=BB39_278 Depth=1
	movq	184(%rsp), %rax
	movq	global_trees(%rip), %rcx
	xorq	%rdi, %rdi
	cmpq	%rcx, %rax
	jne	.LBB39_365
# BB#297:                               #   in Loop: Header=BB39_278 Depth=1
	xorq	%r13, %r13
	movq	%rbx, %r14
	jmp	.LBB39_282
.LBB39_306:                             # %if.end754.if.end762_crit_edge
                                        #   in Loop: Header=BB39_278 Depth=1
	movq	global_trees(%rip), %rax
.LBB39_308:                             # %if.end762
                                        #   in Loop: Header=BB39_278 Depth=1
	movq	%r15, %rdi
	cmpq	%rax, %rbx
	movq	120(%rsp), %r15         # 8-byte Reload
	jne	.LBB39_310
# BB#309:                               # %if.then765
                                        #   in Loop: Header=BB39_278 Depth=1
	movq	%rbx, 184(%rsp)
	movq	%rbx, %rdx
.LBB39_310:                             # %if.end766
                                        #   in Loop: Header=BB39_278 Depth=1
	cmpq	%rax, %rdx
	je	.LBB39_282
# BB#311:                               # %if.end770
                                        #   in Loop: Header=BB39_278 Depth=1
	movq	%r12, %r15
	testq	%rbx, %rbx
	jne	.LBB39_312
# BB#339:                               # %if.else909
                                        #   in Loop: Header=BB39_278 Depth=1
	movq	%rdi, 8(%rsp)           # 8-byte Spill
	movq	112(%rsp), %rax         # 8-byte Reload
	andq	%r15, %rax
	xorq	%rbx, %rbx
	cmpq	$3, %rax
	jne	.LBB39_340
# BB#341:                               # %if.then912
                                        #   in Loop: Header=BB39_278 Depth=1
	movl	pedantic(%rip), %eax
	testq	%rax, %rax
	je	.LBB39_345
# BB#342:                               # %if.then912
                                        #   in Loop: Header=BB39_278 Depth=1
	movl	flag_isoc99(%rip), %eax
	testq	%r15, %rax
	jne	.LBB39_345
# BB#343:                               # %if.then912
                                        #   in Loop: Header=BB39_278 Depth=1
	movl	in_system_header(%rip), %eax
	testq	%r15, %rax
	jne	.LBB39_345
# BB#344:                               # %if.then918
                                        #   in Loop: Header=BB39_278 Depth=1
	xorq	%rax, %rax
	movabsq	$.L.str303, %rdi
	callq	pedwarn
.LBB39_345:                             # %if.end919
                                        #   in Loop: Header=BB39_278 Depth=1
	movq	sizetype_tab(%rip), %rdi
	movq	global_trees+120(%rip), %rsi
	xorq	%rdx, %rdx
	xorq	%r12, %r12
	jmp	.LBB39_346
	.align	16, 0x90
.LBB39_312:                             # %while.cond773
                                        #   Parent Loop BB39_278 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	%rbx, %r12
	movzbl	16(%r12), %ecx
	addq	$-114, %rcx
	andq	%r15, %rcx
	cmpq	$2, %rcx
	ja	.LBB39_315
# BB#313:                               # %land.lhs.true794
                                        #   in Loop: Header=BB39_312 Depth=2
	movq	32(%r12), %rbx
	cmpq	%rax, %rbx
	je	.LBB39_315
# BB#314:                               # %land.rhs800
                                        #   in Loop: Header=BB39_312 Depth=2
	movq	8(%r12), %rcx
	movq	8(%rbx), %rdx
	cmpq	%rdx, %rcx
	je	.LBB39_312
.LBB39_315:                             # %while.end816
                                        #   in Loop: Header=BB39_278 Depth=1
	movq	%rdi, 8(%rsp)           # 8-byte Spill
	movq	8(%r12), %rax
	movzbl	16(%rax), %ecx
	cmpq	$12, %rcx
	ja	.LBB39_317
# BB#316:                               # %while.end816
                                        #   in Loop: Header=BB39_278 Depth=1
	movq	$1, %rax
	shlq	%cl, %rax
	testq	$7232, %rax             # imm = 0x1C40
	jne	.LBB39_318
.LBB39_317:                             # %if.then852
                                        #   in Loop: Header=BB39_278 Depth=1
	xorq	%rax, %rax
	movabsq	$.L.str297, %rdi
	movq	136(%rsp), %rsi         # 8-byte Reload
	callq	error
	movq	global_trees+96(%rip), %r12
.LBB39_318:                             # %if.end853
                                        #   in Loop: Header=BB39_278 Depth=1
	movl	pedantic(%rip), %eax
	testq	%rax, %rax
	je	.LBB39_321
# BB#319:                               # %land.lhs.true855
                                        #   in Loop: Header=BB39_278 Depth=1
	movq	%r12, %rdi
	callq	integer_zerop
	testq	%r15, %rax
	je	.LBB39_321
# BB#320:                               # %if.then858
                                        #   in Loop: Header=BB39_278 Depth=1
	xorq	%rax, %rax
	movabsq	$.L.str298, %rdi
	movq	136(%rsp), %rsi         # 8-byte Reload
	callq	pedwarn
.LBB39_321:                             # %if.end859
                                        #   in Loop: Header=BB39_278 Depth=1
	movl	16(%r12), %eax
	movq	%rax, %rcx
	andq	$255, %rcx
	cmpq	$25, %rcx
	jne	.LBB39_324
# BB#322:                               # %if.then866
                                        #   in Loop: Header=BB39_278 Depth=1
	movq	%r12, %rdi
	callq	constant_expression_warning
	movq	%r12, %rdi
	callq	tree_int_cst_sgn
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jns	.LBB39_332
# BB#323:                               # %if.then870
                                        #   in Loop: Header=BB39_278 Depth=1
	xorq	%rax, %rax
	movabsq	$.L.str299, %rdi
	movq	136(%rsp), %rsi         # 8-byte Reload
	callq	error
	movq	global_trees+96(%rip), %r12
	jmp	.LBB39_332
.LBB39_365:                             # %if.end972
                                        #   in Loop: Header=BB39_278 Depth=1
	movl	16(%rax), %ecx
	movq	%rcx, %rdx
	andq	$255, %rdx
	cmpq	$23, %rdx
	jne	.LBB39_367
# BB#366:                               # %if.then979
                                        #   in Loop: Header=BB39_278 Depth=1
	xorq	%rax, %rax
	movabsq	$.L.str305, %rdi
	movq	136(%rsp), %rsi         # 8-byte Reload
	callq	error
	movq	integer_types+40(%rip), %rax
	movq	%rax, 184(%rsp)
	movl	16(%rax), %ecx
.LBB39_367:                             # %if.end980
                                        #   in Loop: Header=BB39_278 Depth=1
	andq	$255, %rcx
	cmpq	$18, %rcx
	jne	.LBB39_369
# BB#368:                               # %if.then987
                                        #   in Loop: Header=BB39_278 Depth=1
	xorq	%rax, %rax
	movabsq	$.L.str306, %rdi
	movq	136(%rsp), %rsi         # 8-byte Reload
	callq	error
	movq	integer_types+40(%rip), %rax
	movq	%rax, 184(%rsp)
.LBB39_369:                             # %if.end988
                                        #   in Loop: Header=BB39_278 Depth=1
	movl	flag_traditional(%rip), %ecx
	testq	%rcx, %rcx
	je	.LBB39_372
# BB#370:                               # %land.lhs.true990
                                        #   in Loop: Header=BB39_278 Depth=1
	movq	128(%rax), %rax
	movq	global_trees+192(%rip), %rcx
	cmpq	%rcx, %rax
	jne	.LBB39_372
# BB#371:                               # %if.then995
                                        #   in Loop: Header=BB39_278 Depth=1
	movq	global_trees+200(%rip), %rax
	movq	%rax, 184(%rsp)
.LBB39_372:                             # %if.end996
                                        #   in Loop: Header=BB39_278 Depth=1
	movq	%r12, %r13
	movq	40(%rbx), %rax
	cmpq	$1, 128(%rsp)           # 8-byte Folded Reload
	jne	.LBB39_373
# BB#374:                               # %land.rhs1001
                                        #   in Loop: Header=BB39_278 Depth=1
	movq	32(%rbx), %rcx
	movzbl	16(%rcx), %ecx
	cmpq	$1, %rcx
	movabsq	$0, %r12
	jne	.LBB39_376
# BB#375:                               # %land.rhs1001
                                        #   in Loop: Header=BB39_278 Depth=1
	movabsq	$1, %r12
	jmp	.LBB39_376
.LBB39_373:                             #   in Loop: Header=BB39_278 Depth=1
	xorq	%r12, %r12
.LBB39_376:                             # %land.end1011
                                        #   in Loop: Header=BB39_278 Depth=1
	movq	24(%rax), %rcx
	movq	(%rax), %r14
	movq	%rcx, last_function_parms(%rip)
	movq	32(%rax), %rax
	testq	%r14, %r14
	movq	%rax, last_function_parm_tags(%rip)
	jne	.LBB39_381
# BB#377:                               # %land.end1011
                                        #   in Loop: Header=BB39_278 Depth=1
	movl	warn_strict_prototypes(%rip), %eax
	testq	%r13, %rax
	je	.LBB39_381
# BB#378:                               # %land.end1011
                                        #   in Loop: Header=BB39_278 Depth=1
	testq	%r12, %r12
	jne	.LBB39_381
# BB#379:                               # %land.end1011
                                        #   in Loop: Header=BB39_278 Depth=1
	movl	in_system_header(%rip), %eax
	testq	%r13, %rax
	jne	.LBB39_381
# BB#380:                               # %if.then.i
                                        #   in Loop: Header=BB39_278 Depth=1
	xorq	%rax, %rax
	movabsq	$.L.str228, %rdi
	callq	warning
.LBB39_381:                             # %if.end.i
                                        #   in Loop: Header=BB39_278 Depth=1
	movq	%r15, 120(%rsp)         # 8-byte Spill
	xorq	%r15, %r15
	testq	%r14, %r14
	je	.LBB39_383
# BB#382:                               # %land.lhs.true7.i
                                        #   in Loop: Header=BB39_278 Depth=1
	movq	32(%r14), %rax
	movzbl	16(%rax), %eax
	cmpq	$1, %rax
	movq	%r14, %r15
	jne	.LBB39_383
# BB#394:                               # %if.then12.i
                                        #   in Loop: Header=BB39_278 Depth=1
	testq	%r12, %r12
	jne	.LBB39_396
# BB#395:                               # %if.then14.i
                                        #   in Loop: Header=BB39_278 Depth=1
	xorq	%rax, %rax
	movabsq	$.L.str321, %rdi
	callq	pedwarn
.LBB39_396:                             # %if.end15.i
                                        #   in Loop: Header=BB39_278 Depth=1
	movq	%r14, last_function_parms(%rip)
	xorq	%r14, %r14
	movq	120(%rsp), %r15         # 8-byte Reload
	jmp	.LBB39_404
.LBB39_383:                             # %for.cond.preheader.i
                                        #   in Loop: Header=BB39_278 Depth=1
	movq	%rbx, 96(%rsp)          # 8-byte Spill
	movq	last_function_parms(%rip), %rbx
	testq	%rbx, %rbx
	je	.LBB39_403
# BB#384:                               # %for.body.lr.ph.i
                                        #   in Loop: Header=BB39_278 Depth=1
	testq	%r12, %r12
	je	.LBB39_397
	.align	16, 0x90
.LBB39_385:                             # %for.body.us.i
                                        #   Parent Loop BB39_278 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movzbl	16(%rbx), %eax
	cmpq	$35, %rax
	jne	.LBB39_393
# BB#386:                               # %if.then22.us.i
                                        #   in Loop: Header=BB39_385 Depth=2
	movq	32(%r15), %rax
	movq	global_trees(%rip), %rcx
	cmpq	%rcx, %rax
	je	.LBB39_393
# BB#387:                               # %if.end27.us.i
                                        #   in Loop: Header=BB39_385 Depth=2
	cmpq	$0, 32(%rax)
	jne	.LBB39_392
# BB#388:                               # %land.lhs.true32.us.i
                                        #   in Loop: Header=BB39_385 Depth=2
	movq	72(%rbx), %rax
	testq	%rax, %rax
	jne	.LBB39_389
# BB#390:                               # %if.end38.thread72.us.i
                                        #   in Loop: Header=BB39_385 Depth=2
	xorq	%rax, %rax
	movabsq	$.L.str322, %rdi
	callq	warning
	jmp	.LBB39_391
.LBB39_389:                             # %if.end38.thread.us.i
                                        #   in Loop: Header=BB39_385 Depth=2
	movq	32(%rax), %rsi
	xorq	%rax, %rax
	movabsq	$.L.str245, %rdi
	callq	error
.LBB39_391:                             # %if.then40.us.i
                                        #   in Loop: Header=BB39_385 Depth=2
	movq	global_trees(%rip), %rax
	movq	%rax, 32(%r15)
	movq	global_trees(%rip), %rax
	movq	%rax, 8(%rbx)
.LBB39_392:                             # %if.end46.us.i
                                        #   in Loop: Header=BB39_385 Depth=2
	movq	(%r15), %r15
.LBB39_393:                             # %for.inc.us.i
                                        #   in Loop: Header=BB39_385 Depth=2
	movq	(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB39_385
	jmp	.LBB39_403
	.align	16, 0x90
.LBB39_397:                             # %for.body.i
                                        #   Parent Loop BB39_278 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movzbl	16(%rbx), %eax
	cmpq	$35, %rax
	jne	.LBB39_402
# BB#398:                               # %if.then22.i
                                        #   in Loop: Header=BB39_397 Depth=2
	movq	32(%r15), %rax
	movq	global_trees(%rip), %rcx
	cmpq	%rcx, %rax
	je	.LBB39_402
# BB#399:                               # %if.end27.i
                                        #   in Loop: Header=BB39_397 Depth=2
	cmpq	$0, 32(%rax)
	jne	.LBB39_401
# BB#400:                               # %if.end38.i
                                        #   in Loop: Header=BB39_397 Depth=2
	xorq	%rax, %rax
	movabsq	$.L.str322, %rdi
	callq	warning
.LBB39_401:                             # %if.end46.i
                                        #   in Loop: Header=BB39_397 Depth=2
	movq	(%r15), %r15
.LBB39_402:                             # %for.inc.i
                                        #   in Loop: Header=BB39_397 Depth=2
	movq	(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB39_397
.LBB39_403:                             #   in Loop: Header=BB39_278 Depth=1
	movq	120(%rsp), %r15         # 8-byte Reload
	movq	96(%rsp), %rbx          # 8-byte Reload
.LBB39_404:                             # %grokparms.exit
                                        #   in Loop: Header=BB39_278 Depth=1
	movq	184(%rsp), %rdi
	movq	144(%rsp), %rax         # 8-byte Reload
	testq	%r13, %rax
	je	.LBB39_420
# BB#405:                               # %if.then1015
                                        #   in Loop: Header=BB39_278 Depth=1
	movzbl	16(%rdi), %eax
	cmpq	$5, %rax
	movabsq	$0, %rcx
	je	.LBB39_407
# BB#406:                               # %if.then1015
                                        #   in Loop: Header=BB39_278 Depth=1
	movabsq	$1, %rcx
.LBB39_407:                             # %if.then1015
                                        #   in Loop: Header=BB39_278 Depth=1
	movl	pedantic(%rip), %edx
	testq	%rdx, %rdx
	movabsq	$0, %rdx
	jne	.LBB39_409
# BB#408:                               # %if.then1015
                                        #   in Loop: Header=BB39_278 Depth=1
	movabsq	$1, %rdx
.LBB39_409:                             # %if.then1015
                                        #   in Loop: Header=BB39_278 Depth=1
	movl	in_system_header(%rip), %esi
	testq	%rsi, %rsi
	jne	.LBB39_412
# BB#410:                               # %if.then1015
                                        #   in Loop: Header=BB39_278 Depth=1
	orq	%rdx, %rcx
	jne	.LBB39_412
# BB#411:                               # %if.then1026
                                        #   in Loop: Header=BB39_278 Depth=1
	xorq	%rax, %rax
	movabsq	$.L.str307, %rdi
	callq	pedwarn
	jmp	.LBB39_419
.LBB39_301:                             # %if.end723.loopexit1
                                        #   in Loop: Header=BB39_278 Depth=1
	movq	$2, %rbx
	jmp	.LBB39_302
.LBB39_298:                             # %if.else712
                                        #   in Loop: Header=BB39_278 Depth=1
	cmpq	$43, %rcx
	movabsq	$0, %rbx
	jne	.LBB39_300
# BB#299:                               # %if.else712
                                        #   in Loop: Header=BB39_278 Depth=1
	movabsq	$1, %rbx
.LBB39_300:                             # %if.else712
                                        #   in Loop: Header=BB39_278 Depth=1
	movq	$2, %rcx
	shlq	%cl, %rbx
	.align	16, 0x90
.LBB39_302:                             # %if.end723
                                        #   in Loop: Header=BB39_278 Depth=1
	movq	%r15, %rdi
	callq	chainon
	leaq	184(%rsp), %rdi
	movq	%rax, %rsi
	movq	%rbx, %rdx
	callq	decl_attributes
	movq	%rax, %r15
.LBB39_364:                             # %while.cond641.backedge
                                        #   in Loop: Header=BB39_278 Depth=1
	xorq	%rdi, %rdi
	xorq	%r13, %r13
.LBB39_282:                             # %while.cond641.backedge
                                        #   in Loop: Header=BB39_278 Depth=1
	testq	%r14, %r14
	movq	%r14, %rbx
	jne	.LBB39_278
	jmp	.LBB39_260
.LBB39_340:                             #   in Loop: Header=BB39_278 Depth=1
	xorq	%r12, %r12
	jmp	.LBB39_348
.LBB39_324:                             # %if.else872
                                        #   in Loop: Header=BB39_278 Depth=1
	movl	flag_isoc99(%rip), %ecx
	testq	%rcx, %rcx
	jne	.LBB39_325
# BB#326:                               # %if.else872
                                        #   in Loop: Header=BB39_278 Depth=1
	movl	pedantic(%rip), %ecx
	testq	%r15, %rcx
	je	.LBB39_327
# BB#328:                               # %if.then876
                                        #   in Loop: Header=BB39_278 Depth=1
	movq	$1, 88(%rsp)            # 8-byte Folded Spill
	testq	$512, %rax              # imm = 0x200
	jne	.LBB39_329
# BB#330:                               # %if.else883
                                        #   in Loop: Header=BB39_278 Depth=1
	xorq	%rax, %rax
	movabsq	$.L.str301, %rdi
	jmp	.LBB39_331
.LBB39_412:                             # %if.else1027
                                        #   in Loop: Header=BB39_278 Depth=1
	cmpq	$5, %rax
	movabsq	$0, %rax
	movq	144(%rsp), %rdx         # 8-byte Reload
	jne	.LBB39_414
# BB#413:                               # %if.else1027
                                        #   in Loop: Header=BB39_278 Depth=1
	movabsq	$1, %rax
.LBB39_414:                             # %if.else1027
                                        #   in Loop: Header=BB39_278 Depth=1
	movl	extra_warnings(%rip), %ecx
	andq	%r13, %rdx
	cmpq	$2, %rdx
	movabsq	$0, %rdx
	jne	.LBB39_416
# BB#415:                               # %if.else1027
                                        #   in Loop: Header=BB39_278 Depth=1
	movabsq	$1, %rdx
.LBB39_416:                             # %if.else1027
                                        #   in Loop: Header=BB39_278 Depth=1
	testq	%rcx, %rcx
	je	.LBB39_419
# BB#417:                               # %if.else1027
                                        #   in Loop: Header=BB39_278 Depth=1
	andq	%rdx, %rax
	jne	.LBB39_419
# BB#418:                               # %if.then1039
                                        #   in Loop: Header=BB39_278 Depth=1
	xorq	%rax, %rax
	movabsq	$.L.str308, %rdi
	callq	warning
.LBB39_419:                             # %if.end1041
                                        #   in Loop: Header=BB39_278 Depth=1
	movq	184(%rsp), %rdi
	movq	144(%rsp), %rsi         # 8-byte Reload
	callq	c_build_qualified_type
	movq	%rax, %rdi
	movq	%rdi, 184(%rsp)
.LBB39_420:                             # %if.end1043
                                        #   in Loop: Header=BB39_278 Depth=1
	movq	%r14, %rsi
	callq	build_function_type
	movq	%rax, 184(%rsp)
	movq	last_function_parm_tags(%rip), %rcx
	movq	32(%rbx), %r14
	testq	%rcx, %rcx
	jne	.LBB39_422
	jmp	.LBB39_421
	.align	16, 0x90
.LBB39_423:                             # %for.body1050.for.body1050_crit_edge
                                        #   in Loop: Header=BB39_422 Depth=2
	movq	184(%rsp), %rax
.LBB39_422:                             # %for.body1050
                                        #   Parent Loop BB39_278 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	32(%rcx), %rdx
	movq	%rax, 144(%rdx)
	movq	(%rcx), %rcx
	testq	%rcx, %rcx
	jne	.LBB39_423
.LBB39_421:                             #   in Loop: Header=BB39_278 Depth=1
	movq	%r13, %r12
	movq	$0, 88(%rsp)            # 8-byte Folded Spill
	xorq	%rdi, %rdi
	xorq	%r13, %r13
	movq	$0, 144(%rsp)           # 8-byte Folded Spill
	jmp	.LBB39_282
.LBB39_325:                             #   in Loop: Header=BB39_278 Depth=1
	movq	$1, 88(%rsp)            # 8-byte Folded Spill
	jmp	.LBB39_332
.LBB39_327:                             #   in Loop: Header=BB39_278 Depth=1
	movq	$1, 88(%rsp)            # 8-byte Folded Spill
	jmp	.LBB39_332
.LBB39_329:                             # %if.then882
                                        #   in Loop: Header=BB39_278 Depth=1
	xorq	%rax, %rax
	movabsq	$.L.str300, %rdi
.LBB39_331:                             # %if.end886
                                        #   in Loop: Header=BB39_278 Depth=1
	movq	136(%rsp), %rsi         # 8-byte Reload
	callq	pedwarn
.LBB39_332:                             # %if.end886
                                        #   in Loop: Header=BB39_278 Depth=1
	movq	%r12, %rdi
	callq	integer_zerop
	testq	%r15, %rax
	jne	.LBB39_333
# BB#334:                               # %if.else891
                                        #   in Loop: Header=BB39_278 Depth=1
	movq	96(%rsp), %rbx          # 8-byte Reload
	movq	%rbx, %rdi
	movq	%r12, %rsi
	callq	convert
	movq	global_trees+128(%rip), %rsi
	movq	%rax, (%rsp)            # 8-byte Spill
	movq	%rbx, %rdi
	callq	convert
	movq	%rax, %rcx
	movq	$60, %rdi
	xorq	%rax, %rax
	movq	%rbx, %rsi
	movq	(%rsp), %rdx            # 8-byte Reload
	callq	build
	movq	%rax, %rdi
	callq	fold
	movzbl	18(%rax), %ecx
	testq	$8, %rcx
	jne	.LBB39_335
# BB#336:                               # %if.end902
                                        #   in Loop: Header=BB39_278 Depth=1
	movq	88(%rsp), %rcx          # 8-byte Reload
	testq	%r15, %rcx
	je	.LBB39_338
# BB#337:                               # %if.then904
                                        #   in Loop: Header=BB39_278 Depth=1
	movq	%rax, %rdi
	callq	variable_size
.LBB39_338:                             # %if.end906
                                        #   in Loop: Header=BB39_278 Depth=1
	movq	%rax, %rdi
	callq	build_index_type
	jmp	.LBB39_347
.LBB39_333:                             # %if.then889
                                        #   in Loop: Header=BB39_278 Depth=1
	movq	sizetype_tab(%rip), %rdi
	xorq	%rdx, %rdx
	movq	%r12, %rsi
.LBB39_346:                             # %if.end922
                                        #   in Loop: Header=BB39_278 Depth=1
	callq	build_range_type
.LBB39_347:                             # %if.end922
                                        #   in Loop: Header=BB39_278 Depth=1
	movq	%rax, %rbx
.LBB39_348:                             # %if.end922
                                        #   in Loop: Header=BB39_278 Depth=1
	movl	pedantic(%rip), %eax
	testq	%rax, %rax
	je	.LBB39_351
# BB#349:                               # %land.lhs.true924
                                        #   in Loop: Header=BB39_278 Depth=1
	movq	184(%rsp), %rax
	cmpq	$0, 32(%rax)
	jne	.LBB39_351
# BB#350:                               # %if.then929
                                        #   in Loop: Header=BB39_278 Depth=1
	xorq	%rax, %rax
	movabsq	$.L.str304, %rdi
	callq	pedwarn
.LBB39_351:                             # %if.end930
                                        #   in Loop: Header=BB39_278 Depth=1
	movq	184(%rsp), %rdi
	movq	%rbx, %rsi
	callq	build_array_type
	movq	%rax, 184(%rsp)
	movq	144(%rsp), %rsi         # 8-byte Reload
	testq	%r15, %rsi
	je	.LBB39_353
# BB#352:                               # %if.then933
                                        #   in Loop: Header=BB39_278 Depth=1
	movq	%rax, %rdi
	callq	c_build_qualified_type
	movq	%rax, 184(%rsp)
.LBB39_353:                             # %if.end935
                                        #   in Loop: Header=BB39_278 Depth=1
	movq	88(%rsp), %rcx          # 8-byte Reload
	movq	%rcx, 88(%rsp)          # 8-byte Spill
	testq	%r15, %rcx
	je	.LBB39_355
# BB#354:                               # %if.then937
                                        #   in Loop: Header=BB39_278 Depth=1
	movl	60(%rax), %ecx
	orq	$33554432, %rcx         # imm = 0x2000000
	movl	%ecx, 60(%rax)
.LBB39_355:                             # %if.end942
                                        #   in Loop: Header=BB39_278 Depth=1
	testq	%r12, %r12
	je	.LBB39_358
# BB#356:                               # %land.lhs.true944
                                        #   in Loop: Header=BB39_278 Depth=1
	movq	%r12, %rdi
	callq	integer_zerop
	testq	%r15, %rax
	je	.LBB39_358
# BB#357:                               # %if.then947
                                        #   in Loop: Header=BB39_278 Depth=1
	movq	184(%rsp), %rdi
	callq	layout_type
	movq	global_trees+136(%rip), %rax
	movq	184(%rsp), %rcx
	movq	%rax, 32(%rcx)
	movq	global_trees+120(%rip), %rax
	movq	184(%rsp), %rcx
	movq	%rax, 40(%rcx)
.LBB39_358:                             # %if.end951
                                        #   in Loop: Header=BB39_278 Depth=1
	movq	112(%rsp), %rax         # 8-byte Reload
	andq	%r15, %rax
	movq	%r15, %r12
	cmpq	$2, %rax
	je	.LBB39_359
# BB#360:                               # %land.lhs.true954
                                        #   in Loop: Header=BB39_278 Depth=1
	cmpq	$0, 8(%rsp)             # 8-byte Folded Reload
	movq	120(%rsp), %r15         # 8-byte Reload
	jne	.LBB39_363
# BB#361:                               # %land.lhs.true954
                                        #   in Loop: Header=BB39_278 Depth=1
	xorq	%rdi, %rdi
	testq	%r13, %r13
	jne	.LBB39_363
# BB#362:                               #   in Loop: Header=BB39_278 Depth=1
	xorq	%r13, %r13
	jmp	.LBB39_282
.LBB39_363:                             # %if.then959
                                        #   in Loop: Header=BB39_278 Depth=1
	xorq	%rax, %rax
	movabsq	$.L.str294, %rdi
	callq	error
	jmp	.LBB39_364
.LBB39_335:                             # %if.then901
                                        #   in Loop: Header=BB39_278 Depth=1
	xorq	%rax, %rax
	movabsq	$.L.str302, %rdi
	movq	136(%rsp), %rsi         # 8-byte Reload
	callq	error
	movq	global_trees(%rip), %rax
	movq	%rax, 184(%rsp)
	movq	%r15, %r12
.LBB39_359:                             #   in Loop: Header=BB39_278 Depth=1
	movq	120(%rsp), %r15         # 8-byte Reload
	movq	8(%rsp), %rdi           # 8-byte Reload
	jmp	.LBB39_282
	.align	16, 0x90
.LBB39_278:                             # %land.end650
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB39_397 Depth 2
                                        #     Child Loop BB39_385 Depth 2
                                        #     Child Loop BB39_422 Depth 2
                                        #     Child Loop BB39_312 Depth 2
                                        #     Child Loop BB39_431 Depth 2
                                        #     Child Loop BB39_289 Depth 2
	movl	16(%rbx), %ecx
	movq	184(%rsp), %rax
	movq	%rcx, %rdx
	andq	$255, %rdx
	cmpq	$1, %rdx
	jne	.LBB39_280
# BB#279:
	movq	%rbx, 64(%rsp)          # 8-byte Spill
	jmp	.LBB39_261
.LBB39_259:
	xorq	%rdi, %rdi
.LBB39_260:                             # %land.end650.thread
	movq	184(%rsp), %rax
.LBB39_261:                             # %while.end1174
	movq	144(%rsp), %rsi         # 8-byte Reload
	movq	%r12, %rbx
	movzbl	16(%rax), %ecx
	cmpq	$18, %rcx
	movq	112(%rsp), %r13         # 8-byte Reload
	jne	.LBB39_265
# BB#262:                               # %land.lhs.true1181
	movq	32(%rax), %rcx
	testq	%rcx, %rcx
	je	.LBB39_265
# BB#263:                               # %land.lhs.true1186
	movzbl	18(%rcx), %ecx
	testq	$8, %rcx
	je	.LBB39_265
# BB#264:                               # %if.then1195
	movq	%rsi, %r14
	movq	%r15, %r12
	movq	%rdi, %r15
	movabsq	$.L.str302, %rdi
	xorq	%rax, %rax
	movq	136(%rsp), %rsi         # 8-byte Reload
	callq	error
	movq	%r15, %rdi
	movq	%r12, %r15
	movq	%r14, %rsi
	movq	global_trees(%rip), %rax
	movq	%rax, 184(%rsp)
.LBB39_265:                             # %if.end1196
	movq	104(%rsp), %rcx         # 8-byte Reload
	testq	$64, %rcx
	je	.LBB39_460
# BB#266:                               # %if.then1199
	movq	%rcx, %r12
	movl	pedantic(%rip), %ecx
	testq	%rcx, %rcx
	je	.LBB39_270
# BB#267:                               # %land.lhs.true1202
	movzbl	16(%rax), %eax
	cmpq	$23, %rax
	jne	.LBB39_270
# BB#268:                               # %land.lhs.true1202
	testq	%rbx, %rsi
	je	.LBB39_270
# BB#269:                               # %if.then1211
	movq	%rsi, %r14
	movabsq	$.L.str309, %rdi
	xorq	%rax, %rax
	callq	pedwarn
	movq	%r14, %rsi
.LBB39_270:                             # %if.end1212
	movq	184(%rsp), %rdx
	testq	%rbx, %rsi
	je	.LBB39_272
# BB#271:                               # %if.then1214
	movq	%rdx, %rdi
	callq	c_build_qualified_type
	movq	%rax, %rdx
	movq	%rdx, 184(%rsp)
.LBB39_272:                             # %if.end1216
	movq	$33, %rdi
	movq	64(%rsp), %rsi          # 8-byte Reload
	callq	build_decl
	movq	%rax, 176(%rsp)
	testq	$1024, %r12             # imm = 0x400
	movabsq	$274877906944, %rdx     # imm = 0x4000000000
	jne	.LBB39_275
# BB#273:                               # %lor.lhs.false1220
	cmpq	$0, 72(%rsp)            # 8-byte Folded Reload
	je	.LBB39_276
# BB#274:                               # %land.lhs.true1222
	movq	72(%rsp), %rcx          # 8-byte Reload
	movq	48(%rcx), %rcx
	testq	%rdx, %rcx
	je	.LBB39_276
.LBB39_275:                             # %if.then1230
	orq	%rdx, 48(%rax)
.LBB39_276:                             # %if.end1236
	leaq	176(%rsp), %rdi
	xorq	%rdx, %rdx
	movq	%r15, %rsi
	callq	decl_attributes
	movq	176(%rsp), %rax
	jmp	.LBB39_617
.LBB39_460:                             # %if.end1238
	movq	%r15, 120(%rsp)         # 8-byte Spill
	testq	%rax, %rax
	movq	56(%rsp), %r15          # 8-byte Reload
	movq	%rdi, %r12
	je	.LBB39_461
# BB#462:                               # %if.end1238
	movabsq	$1, %rcx
	jmp	.LBB39_463
.LBB39_461:
	movabsq	$0, %rcx
.LBB39_463:                             # %if.end1238
	movq	32(%rsp), %rdx          # 8-byte Reload
	andq	%rdx, %rcx
	cmpq	$1, %rcx
	jne	.LBB39_469
# BB#464:                               # %land.lhs.true1244
	movzbl	16(%rax), %ecx
	cmpq	$18, %rcx
	jne	.LBB39_469
# BB#465:                               # %land.lhs.true1251
	cmpq	$0, 24(%rax)
	jne	.LBB39_469
# BB#466:                               # %land.lhs.true1255
	movq	128(%rax), %rcx
	movq	16(%rsp), %rdx          # 8-byte Reload
	movq	128(%rdx), %rdx
	cmpq	%rdx, %rcx
	jne	.LBB39_469
# BB#467:                               # %if.then1262
	movq	%rsi, %r14
	movq	8(%rax), %rdi
	xorq	%rsi, %rsi
	callq	build_array_type
	movq	%r14, %rsi
	movq	%rax, 184(%rsp)
	movq	88(%rsp), %rcx          # 8-byte Reload
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	testq	%rdx, %rcx
	je	.LBB39_469
# BB#468:                               # %if.then1267
	movl	60(%rax), %ecx
	orq	$33554432, %rcx         # imm = 0x2000000
	movl	%ecx, 60(%rax)
.LBB39_469:                             # %if.end1274
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %r13
	cmpq	$5, %r13
	jne	.LBB39_477
# BB#470:                               # %if.then1277
	movl	pedantic(%rip), %eax
	testq	%rax, %rax
	movabsq	$4294967295, %r12       # imm = 0xFFFFFFFF
	je	.LBB39_474
# BB#471:                               # %land.lhs.true1279
	movq	184(%rsp), %rax
	movzbl	16(%rax), %eax
	cmpq	$23, %rax
	jne	.LBB39_474
# BB#472:                               # %land.lhs.true1279
	testq	%r12, %rsi
	je	.LBB39_474
# BB#473:                               # %if.then1288
	movq	%rsi, %rbx
	movabsq	$.L.str311, %rdi
	xorq	%rax, %rax
	callq	pedwarn
	movq	%rbx, %rsi
.LBB39_474:                             # %if.end1289
	testq	%r12, %rsi
	movq	120(%rsp), %rbx         # 8-byte Reload
	je	.LBB39_476
# BB#475:                               # %if.then1291
	movq	184(%rsp), %rdi
	callq	c_build_qualified_type
	movq	%rax, 184(%rsp)
.LBB39_476:                             # %if.end1293
	leaq	184(%rsp), %rdi
	xorq	%rdx, %rdx
	movq	%rbx, %rsi
	callq	decl_attributes
	movq	184(%rsp), %rax
	jmp	.LBB39_617
.LBB39_477:                             # %if.end1295
	movq	184(%rsp), %r14
	movzbl	16(%r14), %eax
	cmpq	$5, %rax
	jne	.LBB39_478
# BB#479:                               # %if.end1295
	cmpq	$2, %r13
	je	.LBB39_480
# BB#481:                               # %land.lhs.true1305
	cmpq	$3, %r13
	movq	%r13, %rbx
	je	.LBB39_485
# BB#482:                               # %land.lhs.true1315
	testq	$16, 104(%rsp)          # 8-byte Folded Reload
	jne	.LBB39_486
# BB#483:                               # %lor.lhs.false1318
	movq	current_binding_level(%rip), %rax
	movq	global_binding_level(%rip), %rcx
	cmpq	%rcx, %rax
	jne	.LBB39_485
# BB#484:                               # %lor.lhs.false1318
	movq	104(%rsp), %rax         # 8-byte Reload
	andq	$33, %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB39_486
.LBB39_485:                             # %if.then1324
	movq	%rsi, %r14
	movabsq	$.L.str312, %rdi
	xorq	%rax, %rax
	movq	136(%rsp), %rsi         # 8-byte Reload
	callq	error
	movq	%r14, %rsi
	movq	integer_types+40(%rip), %r14
	movq	%r14, 184(%rsp)
	jmp	.LBB39_486
.LBB39_478:
	movq	%r13, %rbx
	jmp	.LBB39_486
.LBB39_480:
	movq	%r13, %rbx
.LBB39_486:                             # %if.end1325
	movabsq	$137438953472, %r13     # imm = 0x2000000000
	cmpq	$2, %rbx
	jne	.LBB39_532
# BB#487:                               # %if.then1329
	movzbl	16(%r14), %eax
	cmpq	$23, %rax
	jne	.LBB39_488
# BB#517:                               # %if.then1418
	movq	%rsi, %rbx
	movl	pedantic(%rip), %eax
	testq	%rax, %rax
	movq	120(%rsp), %r15         # 8-byte Reload
	je	.LBB39_520
# BB#518:                               # %if.then1418
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %rbx
	je	.LBB39_520
# BB#519:                               # %if.then1422
	movabsq	$.L.str309, %rdi
	xorq	%rax, %rax
	callq	pedwarn
.LBB39_520:                             # %if.end1423
	movq	184(%rsp), %rdi
	movq	%rbx, %rsi
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %rsi
	je	.LBB39_522
# BB#521:                               # %if.then1425
	callq	c_build_qualified_type
	movq	%rax, %rdi
	movq	%rdi, 184(%rsp)
.LBB39_522:                             # %if.end1427
	callq	build_pointer_type
	movq	%rax, %r14
	movq	%r14, 184(%rsp)
	xorq	%rbx, %rbx
	jmp	.LBB39_526
.LBB39_532:                             # %if.else1456
	movzbl	16(%r14), %eax
	cmpq	$3, %rbx
	jne	.LBB39_551
# BB#533:                               # %if.then1459
	movq	%rsi, %rbx
	cmpq	$23, %rax
	jne	.LBB39_535
# BB#534:                               # %if.then1466
	movabsq	$.L.str315, %rdi
	xorq	%rax, %rax
	movq	136(%rsp), %rsi         # 8-byte Reload
	callq	error
	movq	184(%rsp), %rdi
	callq	build_pointer_type
	movq	%rax, %r14
	movq	%r14, 184(%rsp)
	movq	%rbx, %rcx
	jmp	.LBB39_540
.LBB39_488:                             # %if.then1329
	cmpq	$18, %rax
	movq	120(%rsp), %r15         # 8-byte Reload
	jne	.LBB39_523
# BB#489:                               # %if.then1336
	movq	8(%r14), %rdi
	movq	%rdi, 184(%rsp)
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %rsi
	je	.LBB39_491
# BB#490:                               # %if.then1340
	callq	c_build_qualified_type
	movq	%rax, %rdi
	movq	%rdi, 184(%rsp)
.LBB39_491:                             # %if.end1342
	callq	build_pointer_type
	movq	%rax, %r14
	movq	%r14, 184(%rsp)
	movq	%r12, %rdi
	testq	%rdi, %rdi
	je	.LBB39_492
# BB#493:                               # %if.then1345
	leaq	160(%rsp), %rsi
	leaq	152(%rsp), %rdx
	callq	split_specs_attrs
	cmpq	$0, 152(%rsp)
	je	.LBB39_495
# BB#494:                               # %if.then1349
	movabsq	$.L.str313, %rdi
	xorq	%rax, %rax
	callq	warning
.LBB39_495:                             # %for.cond1351thread-pre-split
	movq	160(%rsp), %rcx
	xorq	%rax, %rax
	testq	%rcx, %rcx
	je	.LBB39_496
# BB#497:
	xorq	%r15, %r15
	xorq	%rbx, %rbx
	xorq	%r14, %r14
.LBB39_498:                             # %for.body1353
                                        # =>This Inner Loop Header: Depth=1
	movq	32(%rcx), %rdx
	movzbl	19(%rdx), %esi
	testq	$1, %rsi
	je	.LBB39_505
# BB#499:                               # %if.then1363
                                        #   in Loop: Header=BB39_498 Depth=1
	movzbl	43(%rdx), %edx
	cmpq	$12, %rdx
	je	.LBB39_504
# BB#500:                               # %if.then1363
                                        #   in Loop: Header=BB39_498 Depth=1
	cmpq	$9, %rdx
	jne	.LBB39_501
# BB#503:                               # %if.then1377
                                        #   in Loop: Header=BB39_498 Depth=1
	incq	%r14
	jmp	.LBB39_506
.LBB39_504:                             # %if.then1385
                                        #   in Loop: Header=BB39_498 Depth=1
	incq	%rbx
	jmp	.LBB39_506
.LBB39_501:                             # %if.then1363
                                        #   in Loop: Header=BB39_498 Depth=1
	cmpq	$3, %rdx
	jne	.LBB39_505
# BB#502:                               # %if.then1369
                                        #   in Loop: Header=BB39_498 Depth=1
	incq	%r15
	jmp	.LBB39_506
.LBB39_505:                             # %if.else1392
                                        #   in Loop: Header=BB39_498 Depth=1
	incq	%rax
.LBB39_506:                             # %for.inc1395
                                        #   in Loop: Header=BB39_498 Depth=1
	movq	(%rcx), %rcx
	testq	%rcx, %rcx
	movq	%rcx, 160(%rsp)
	jne	.LBB39_498
# BB#507:                               # %for.end1398
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB39_509
# BB#508:                               # %if.then1400
	movabsq	$.L.str314, %rdi
	xorq	%rax, %rax
	callq	error
	jmp	.LBB39_509
.LBB39_551:                             # %if.else1521
	cmpq	$23, %rax
	jne	.LBB39_593
# BB#552:                               # %if.then1528
	testq	$2048, 104(%rsp)        # 8-byte Folded Reload
                                        # imm = 0x800
	movq	$1, %r14
	je	.LBB39_559
# BB#553:                               # %land.lhs.true1535
	movq	current_binding_level(%rip), %rax
	movq	global_binding_level(%rip), %rcx
	cmpq	%rcx, %rax
	jne	.LBB39_554
# BB#555:                               # %land.lhs.true1535
	movabsq	$1, %r15
	jmp	.LBB39_556
.LBB39_535:                             # %if.else1468
	testq	%rax, %rax
	movq	%rbx, %rcx
	je	.LBB39_540
# BB#536:                               # %if.else1468
	cmpq	$18, %rax
	movq	%r14, %rax
	jne	.LBB39_538
# BB#537:                               # %cond.true
	movq	8(%r14), %rax
.LBB39_538:                             # %cond.end
	cmpq	$0, 32(%rax)
	jne	.LBB39_540
# BB#539:                               # %if.then1489
	movq	%rcx, %rbx
	movabsq	$.L.str316, %rdi
	xorq	%rax, %rax
	movq	136(%rsp), %rsi         # 8-byte Reload
	callq	error
	movq	%rbx, %rcx
	movq	global_trees(%rip), %r14
	movq	%r14, 184(%rsp)
.LBB39_540:                             # %if.end1491
	movzbl	16(%r14), %eax
	cmpq	$18, %rax
	jne	.LBB39_541
# BB#542:                               # %if.end1491
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %rcx
	movq	%rcx, %rsi
	je	.LBB39_543
# BB#544:                               # %if.then1500
	movq	8(%r14), %rdi
	movq	%rsi, %rbx
	callq	c_build_qualified_type
	movq	184(%rsp), %rcx
	movq	24(%rcx), %rsi
	movq	%rax, %rdi
	callq	build_array_type
	movq	%rax, %r14
	movq	%r14, 184(%rsp)
	jmp	.LBB39_545
.LBB39_523:                             # %if.else1429
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %rsi
	je	.LBB39_524
# BB#525:                               # %if.then1431
	movq	%r14, %rdi
	movq	%rsi, %rbx
	callq	c_build_qualified_type
	movq	%rax, %r14
	movq	%r14, 184(%rsp)
	jmp	.LBB39_526
.LBB39_593:                             # %if.else1656
	movq	%r13, %rbx
	xorq	%r15, %r15
	movq	80(%rsp), %rcx          # 8-byte Reload
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	testq	%rdx, %rcx
	movq	104(%rsp), %r13         # 8-byte Reload
	jne	.LBB39_595
# BB#594:                               # %land.rhs1659
	movq	%r13, %r15
	andq	$16, %r15
	movq	$4, %rcx
	shrq	%cl, %r15
.LBB39_595:                             # %land.end1662
	cmpq	$18, %rax
	jne	.LBB39_598
# BB#596:                               # %land.end1662
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %rsi
	je	.LBB39_598
# BB#597:                               # %if.then1672
	movl	64(%r14), %r12d
	movq	%rsi, 144(%rsp)         # 8-byte Spill
	movq	8(%r14), %rdi
	callq	c_build_qualified_type
	movq	184(%rsp), %rcx
	movq	24(%rcx), %rsi
	movq	%rax, %rdi
	callq	build_array_type
	movq	%rax, %r14
	movq	%r14, 184(%rsp)
	movl	%r12d, 64(%r14)
	jmp	.LBB39_601
.LBB39_541:
	movq	%rcx, %rbx
	jmp	.LBB39_545
.LBB39_543:
	movq	%rsi, %rbx
.LBB39_545:                             # %if.end1507
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %r15
	cmpq	$4, %r15
	jne	.LBB39_546
# BB#547:                               # %if.end1507
	movabsq	$1, %r15
	jmp	.LBB39_548
.LBB39_546:
	movabsq	$0, %r15
.LBB39_548:                             # %if.end1507
	movq	$37, %rdi
	movq	64(%rsp), %rsi          # 8-byte Reload
	movq	%r14, %rdx
	callq	build_decl
	movq	%rax, 168(%rsp)
	movq	48(%rax), %rdx
	movq	$34, %rcx
	shlq	%cl, %r15
	movabsq	$-17179869185, %rcx     # imm = 0xFFFFFFFBFFFFFFFF
	andq	%rdx, %rcx
	orq	%r15, %rcx
	movq	%rcx, 48(%rax)
	movq	88(%rsp), %rax          # 8-byte Reload
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB39_549
# BB#550:                               # %if.then1514
	movq	168(%rsp), %rax
	orq	%r13, 48(%rax)
.LBB39_549:
	movq	104(%rsp), %r12         # 8-byte Reload
	movq	120(%rsp), %r15         # 8-byte Reload
	movq	%rbx, %rdi
	jmp	.LBB39_612
.LBB39_598:                             # %if.else1682
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %rsi
	je	.LBB39_599
# BB#600:                               # %if.then1684
	movq	%r14, %rdi
	movq	%rsi, 144(%rsp)         # 8-byte Spill
	callq	c_build_qualified_type
	movq	%rax, %r14
	movq	%r14, 184(%rsp)
	jmp	.LBB39_601
.LBB39_154:                             # %if.else361
	movq	integer_types+48(%rip), %rax
	movq	%rax, 184(%rsp)
	movq	$0, 72(%rsp)            # 8-byte Folded Spill
	jmp	.LBB39_165
.LBB39_524:
	xorq	%rbx, %rbx
.LBB39_526:                             # %if.end1435
	movq	$35, %rdi
	movq	64(%rsp), %rsi          # 8-byte Reload
	movq	%r14, %rdx
	callq	build_decl
	movq	%rax, 168(%rsp)
	movq	88(%rsp), %rcx          # 8-byte Reload
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	testq	%rdx, %rcx
	je	.LBB39_528
# BB#527:                               # %if.then1438
	orq	%r13, 48(%rax)
	jmp	.LBB39_528
.LBB39_492:
	xorq	%rbx, %rbx
	jmp	.LBB39_516
.LBB39_554:
	movabsq	$0, %r15
.LBB39_556:                             # %land.lhs.true1535
	movl	pedantic(%rip), %edx
	testq	%rdx, %rdx
	jne	.LBB39_558
# BB#557:                               # %land.lhs.true1535
	xorq	%r14, %r14
	cmpq	%rcx, %rax
	jne	.LBB39_559
.LBB39_558:                             # %if.then1540
	movq	%rsi, %rbx
	movabsq	$.L.str317, %rdi
	xorq	%rax, %rax
	movq	136(%rsp), %rsi         # 8-byte Reload
	callq	pedwarn
	movq	%rbx, %rsi
	movq	%r15, %r14
.LBB39_559:                             # %if.end1541
	movq	%rsi, %rbx
	testq	$32, 104(%rsp)          # 8-byte Folded Reload
	je	.LBB39_561
# BB#560:                               # %if.then1544
	movabsq	$.L.str317, %rdi
	xorq	%rax, %rax
	movq	136(%rsp), %rsi         # 8-byte Reload
	callq	error
.LBB39_561:                             # %if.end1545
	movq	current_binding_level(%rip), %rax
	movq	global_binding_level(%rip), %rcx
	cmpq	%rcx, %rax
	je	.LBB39_565
# BB#562:                               # %land.lhs.true1548
	testq	$257, 104(%rsp)         # 8-byte Folded Reload
                                        # imm = 0x101
	je	.LBB39_565
# BB#563:                               # %land.lhs.true1548
	movl	pedantic(%rip), %eax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB39_565
# BB#564:                               # %if.then1553
	movabsq	$.L.str317, %rdi
	xorq	%rax, %rax
	movq	136(%rsp), %rsi         # 8-byte Reload
	callq	pedwarn
.LBB39_565:                             # %if.end1554
	movq	184(%rsp), %rdx
	movq	$30, %rdi
	movq	64(%rsp), %rsi          # 8-byte Reload
	callq	build_decl
	movq	%rax, 168(%rsp)
	movq	%rax, %rdi
	movq	40(%rsp), %rsi          # 8-byte Reload
	callq	build_decl_attribute_variant
	movq	%rax, 168(%rsp)
	movq	$16, %rdi
	callq	ggc_alloc_cleared
	movq	168(%rsp), %rcx
	movq	%rax, 200(%rcx)
	movl	pedantic(%rip), %eax
	testq	%rax, %rax
	movq	%rbx, %rdi
	je	.LBB39_569
# BB#566:                               # %if.end1554
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %rdi
	je	.LBB39_569
# BB#567:                               # %land.lhs.true1562
	movq	168(%rsp), %rax
	movzbl	50(%rax), %eax
	testq	$1, %rax
	jne	.LBB39_569
# BB#568:                               # %if.then1569
	movq	%rdi, %rbx
	movabsq	$.L.str309, %rdi
	xorq	%rax, %rax
	callq	pedwarn
	movq	%rbx, %rdi
.LBB39_569:                             # %if.end1570
	testq	$2, %rdi
	movq	104(%rsp), %r13         # 8-byte Reload
	movq	120(%rsp), %r15         # 8-byte Reload
	je	.LBB39_572
# BB#570:                               # %land.lhs.true1573
	movq	168(%rsp), %rax
	movq	8(%rax), %rax
	movq	8(%rax), %rax
	movzbl	16(%rax), %eax
	cmpq	$5, %rax
	je	.LBB39_572
# BB#571:                               # %if.then1584
	movq	%rdi, %rbx
	movabsq	$.L.str318, %rdi
	xorq	%rax, %rax
	callq	warning
	movq	%rbx, %rdi
.LBB39_572:                             # %if.end1585
	testq	%r14, %r14
	je	.LBB39_574
# BB#573:                               # %if.then1587
	movq	168(%rsp), %rax
	orq	$256, 48(%rax)          # imm = 0x100
.LBB39_574:                             # %if.end1592
	testq	$2049, %r13             # imm = 0x801
	movq	24(%rsp), %rbx          # 8-byte Reload
	jne	.LBB39_575
# BB#576:                               # %if.end1592
	movabsq	$1, %rax
	jmp	.LBB39_577
.LBB39_575:
	movabsq	$0, %rax
.LBB39_577:                             # %if.end1592
	movq	168(%rsp), %rdx
	movl	16(%rdx), %esi
	movq	$19, %rcx
	shlq	%cl, %rax
	movabsq	$4294443007, %rcx       # imm = 0xFFF7FFFF
	andq	%rsi, %rcx
	orq	%rax, %rcx
	movl	%ecx, 16(%rdx)
	cmpq	$0, 48(%rsp)            # 8-byte Folded Reload
	movabsq	$4294967295, %r12       # imm = 0xFFFFFFFF
	je	.LBB39_579
# BB#578:                               # %if.then1605
	movq	168(%rsp), %rax
	movabsq	$274877906944, %rcx     # imm = 0x4000000000
	orq	%rcx, 48(%rax)
.LBB39_579:                             # %if.end1611
	movq	global_trees+400(%rip), %rax
	cmpq	%rax, 64(%rsp)          # 8-byte Folded Reload
	je	.LBB39_580
# BB#583:                               # %if.else1618
	testq	%r12, %rbx
	je	.LBB39_588
# BB#584:                               # %if.then1620
	movq	168(%rsp), %rax
	movq	200(%rax), %rax
	movzbl	(%rax), %ecx
	orq	$1, %rcx
	movb	%cl, (%rax)
	movq	80(%rsp), %rax          # 8-byte Reload
	testq	%r12, %rax
	je	.LBB39_585
# BB#586:                               # %if.then1627
	movq	168(%rsp), %rax
	orq	$2048, 48(%rax)         # imm = 0x800
	movq	%r13, %r12
	testq	$16, %r12
	je	.LBB39_612
# BB#587:                               # %if.then1634
	movq	$1, %rax
	movl	%eax, current_extern_inline(%rip)
	jmp	.LBB39_612
.LBB39_580:                             # %if.then1614
	testq	%r12, %rbx
	je	.LBB39_581
# BB#582:                               # %if.then1616
	movq	%rdi, %rbx
	movabsq	$.L.str319, %rdi
	xorq	%rax, %rax
	callq	warning
	movq	%rbx, %rdi
	movq	%r13, %r12
	jmp	.LBB39_612
.LBB39_119:
	movabsq	$0, %r14
	jmp	.LBB39_121
.LBB39_496:
	xorq	%r14, %r14
	xorq	%rbx, %rbx
	xorq	%r15, %r15
.LBB39_509:                             # %if.end1401
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %r15
	movabsq	$0, %rax
	movabsq	$1, %rsi
	movq	%rax, %rdx
	je	.LBB39_511
# BB#510:                               # %if.end1401
	movq	%rsi, %rdx
.LBB39_511:                             # %if.end1401
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rbx
	movq	%rax, %rdi
	movq	120(%rsp), %r15         # 8-byte Reload
	je	.LBB39_513
# BB#512:                               # %if.end1401
	movq	%rsi, %rdi
.LBB39_513:                             # %if.end1401
	movq	$2, %rcx
	shlq	%cl, %rdi
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %r14
	je	.LBB39_515
# BB#514:                               # %if.end1401
	movq	%rsi, %rax
.LBB39_515:                             # %if.end1401
	movq	184(%rsp), %r14
	addq	%rax, %rax
	orq	%rax, %rdi
	orq	%rdx, %rdi
	movq	%rdi, %rbx
.LBB39_516:                             # %if.end1435.thread
	movq	$35, %rdi
	movq	64(%rsp), %rsi          # 8-byte Reload
	movq	%r14, %rdx
	callq	build_decl
	movq	%rax, 168(%rsp)
.LBB39_528:                             # %if.end1444
	movq	184(%rsp), %rax
	movq	global_trees(%rip), %rcx
	cmpq	%rcx, %rax
	je	.LBB39_531
# BB#529:                               # %if.else1448
	movq	%rax, %rdi
	callq	simple_type_promotes_to
	testq	%rax, %rax
	jne	.LBB39_531
# BB#530:
	movq	184(%rsp), %rax
.LBB39_531:                             # %if.else1448
	movq	%rbx, %rdi
	movq	168(%rsp), %rcx
	movq	%rax, 104(%rcx)
	movq	168(%rsp), %rax
	movq	%r14, 96(%rax)
	movq	104(%rsp), %r12         # 8-byte Reload
	jmp	.LBB39_612
.LBB39_588:                             # %if.else1637
	movl	flag_inline_trees(%rip), %eax
	cmpq	$2, %rax
	jne	.LBB39_589
# BB#590:                               # %if.else1637
	movq	80(%rsp), %rax          # 8-byte Reload
	testq	%r12, %rax
	je	.LBB39_591
# BB#592:                               # %if.then1642
	movq	168(%rsp), %rax
	orq	$2048, 48(%rax)         # imm = 0x800
	movq	168(%rsp), %rax
	movq	200(%rax), %rax
	movzbl	(%rax), %ecx
	andq	$254, %rcx
	movb	%cl, (%rax)
	movq	%r13, %r12
	jmp	.LBB39_612
.LBB39_599:
	movq	%rsi, 144(%rsp)         # 8-byte Spill
.LBB39_601:                             # %if.end1687
	movq	$34, %rdi
	movq	64(%rsp), %rsi          # 8-byte Reload
	movq	%r14, %rdx
	callq	build_decl
	movq	%rax, 168(%rsp)
	movabsq	$4294967295, %r12       # imm = 0xFFFFFFFF
	movq	88(%rsp), %rcx          # 8-byte Reload
	testq	%r12, %rcx
	je	.LBB39_603
# BB#602:                               # %if.then1690
	orq	%rbx, 48(%rax)
.LBB39_603:                             # %if.end1696
	movq	24(%rsp), %rax          # 8-byte Reload
	testq	%r12, %rax
	je	.LBB39_605
# BB#604:                               # %if.then1698
	movq	168(%rsp), %rdi
	movabsq	$.L.str320, %rsi
	xorq	%rax, %rax
	callq	pedwarn_with_decl
.LBB39_605:                             # %if.end1699
	movq	168(%rsp), %rax
	movq	48(%rax), %rdx
	movq	$8, %rcx
	shlq	%cl, %r15
	andq	$-257, %rdx             # imm = 0xFFFFFFFFFFFFFEFF
	orq	%r15, %rdx
	movq	%rdx, 48(%rax)
	movq	current_binding_level(%rip), %rax
	movq	global_binding_level(%rip), %rcx
	cmpq	%rcx, %rax
	je	.LBB39_606
# BB#610:                               # %if.else1738
	movq	168(%rsp), %rax
	movl	16(%rax), %ecx
	movq	%r13, %rdx
	andq	$1, %rdx
	shlq	$18, %rdx
	movabsq	$4294705151, %rsi       # imm = 0xFFFBFFFF
	andq	%rcx, %rsi
	orq	%rdx, %rsi
	movl	%esi, 16(%rax)
	movq	168(%rsp), %rax
	movl	48(%rax), %edx
	movq	$11, %rcx
	movl	16(%rax), %esi
	shlq	%cl, %rdx
	andq	$524288, %rdx           # imm = 0x80000
	movabsq	$4294443007, %rcx       # imm = 0xFFF7FFFF
	andq	%rsi, %rcx
	orq	%rdx, %rcx
	movl	%ecx, 16(%rax)
	movq	%r13, %r12
	movq	120(%rsp), %r15         # 8-byte Reload
	jmp	.LBB39_611
.LBB39_606:                             # %if.then1710
	movq	%r13, %rdi
	testq	$33, %r13
	jne	.LBB39_607
# BB#608:                               # %if.then1710
	movabsq	$1, %rax
	jmp	.LBB39_609
.LBB39_607:
	movabsq	$0, %rax
.LBB39_609:                             # %if.then1710
	movq	120(%rsp), %r15         # 8-byte Reload
	movq	168(%rsp), %rdx
	movl	16(%rdx), %esi
	movq	$19, %rcx
	shlq	%cl, %rax
	movabsq	$4294443007, %rcx       # imm = 0xFFF7FFFF
	andq	%rsi, %rcx
	orq	%rax, %rcx
	movl	%ecx, 16(%rdx)
	movq	168(%rsp), %rax
	movq	48(%rax), %rdx
	movq	$10, %rcx
	shlq	%cl, %rdx
	movl	16(%rax), %ecx
	notq	%rdx
	andq	$262144, %rdx           # imm = 0x40000
	movabsq	$4294705151, %rsi       # imm = 0xFFFBFFFF
	andq	%rcx, %rsi
	orq	%rdx, %rsi
	movl	%esi, 16(%rax)
	movq	%rdi, %r12
.LBB39_611:                             # %if.end1765
	movq	144(%rsp), %rdi         # 8-byte Reload
.LBB39_612:                             # %if.end1765
	testq	$32, %r12
	je	.LBB39_614
# BB#613:                               # %if.then1768
	movq	168(%rsp), %rax
	orq	$1024, 48(%rax)         # imm = 0x400
.LBB39_614:                             # %if.end1773
	movq	168(%rsp), %rsi
	callq	c_apply_type_quals_to_decl
	movq	168(%rsp), %rdi
	movq	8(%rdi), %rax
	movzbl	19(%rax), %eax
	testq	$4, %rax
	je	.LBB39_616
# BB#615:                               # %if.then1781
	callq	mark_addressable
.LBB39_616:                             # %if.end1783
	leaq	168(%rsp), %rdi
	xorq	%rdx, %rdx
	movq	%r15, %rsi
	callq	decl_attributes
	movq	168(%rsp), %rax
.LBB39_617:                             # %return
	addq	$192, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB39_585:
	movq	%r13, %r12
	jmp	.LBB39_612
.LBB39_581:
	movq	%r13, %r12
	jmp	.LBB39_612
.LBB39_589:
	movq	%r13, %r12
	jmp	.LBB39_612
.LBB39_591:
	movq	%r13, %r12
	jmp	.LBB39_612
.LBB39_459:                             # %if.else1169
	movabsq	$.L.str185, %rax
	movabsq	$.L__FUNCTION__.grokdeclarator, %rcx
	movq	$4868, %rsi             # imm = 0x1304
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp329:
	.size	grokdeclarator, .Ltmp329-grokdeclarator
	.cfi_endproc

	.globl	groktypename_in_parm_context
	.align	16, 0x90
	.type	groktypename_in_parm_context,@function
groktypename_in_parm_context:           # @groktypename_in_parm_context
	.cfi_startproc
# BB#0:                                 # %entry
	movzbl	16(%rdi), %eax
	cmpq	$2, %rax
	jne	.LBB40_1
# BB#2:                                 # %if.end
	movq	24(%rdi), %rsi
	movq	32(%rdi), %rdi
	movq	$2, %rdx
	xorq	%rcx, %rcx
	jmp	grokdeclarator  # TAILCALL
.LBB40_1:                               # %return
	movq	%rdi, %rax
	retq
.Ltmp330:
	.size	groktypename_in_parm_context, .Ltmp330-groktypename_in_parm_context
	.cfi_endproc

	.globl	start_decl
	.align	16, 0x90
	.type	start_decl,@function
start_decl:                             # @start_decl
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp337:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp338:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp339:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp340:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp341:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp342:
	.cfi_def_cfa_offset 64
.Ltmp343:
	.cfi_offset %rbx, -48
.Ltmp344:
	.cfi_offset %r12, -40
.Ltmp345:
	.cfi_offset %r13, -32
.Ltmp346:
	.cfi_offset %r14, -24
.Ltmp347:
	.cfi_offset %r15, -16
	movq	%rcx, %r14
	movq	%rdx, %r12
	movq	%rsi, %r15
	movq	%rdi, %rbx
	movabsq	$.L.str169, %rdi
	movq	%r14, %rsi
	callq	lookup_attribute
	testq	%rax, %rax
	je	.LBB41_2
# BB#1:                                 # %if.then
	movq	$1, %rax
	movb	%al, deprecated_state(%rip)
.LBB41_2:                               # %if.end
	xorq	%rdx, %rdx
	xorq	%r13, %r13
	movq	%rbx, %rdi
	movq	%r15, %rsi
	movq	%r12, %rcx
	callq	grokdeclarator
	movslq	warn_main(%rip), %rdx
	movq	%rax, %rcx
	movb	%r13b, deprecated_state(%rip)
	movq	%rcx, 8(%rsp)
	testq	%rdx, %rdx
	jle	.LBB41_6
# BB#3:                                 # %land.lhs.true
	movzbl	16(%rcx), %eax
	cmpq	$30, %rax
	je	.LBB41_6
# BB#4:                                 # %land.lhs.true3
	movq	72(%rcx), %rax
	movq	global_trees+400(%rip), %rdx
	cmpq	%rdx, %rax
	jne	.LBB41_6
# BB#5:                                 # %if.then6
	movabsq	$.L.str170, %rsi
	xorq	%rax, %rax
	movq	%rcx, %rdi
	callq	warning_with_decl
.LBB41_6:                               # %if.end7
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %r12
	je	.LBB41_29
# BB#7:                                 # %if.then9
	movq	8(%rsp), %rax
	movzbl	16(%rax), %ecx
	cmpq	$30, %rcx
	je	.LBB41_15
# BB#8:                                 # %if.then9
	cmpq	$35, %rcx
	jne	.LBB41_9
# BB#16:                                # %sw.bb27
	movq	72(%rax), %rax
	movq	32(%rax), %rsi
	movabsq	$.L.str173, %rdi
	jmp	.LBB41_14
.LBB41_15:                              # %sw.bb21
	movq	72(%rax), %rax
	movq	32(%rax), %rsi
	movabsq	$.L.str172, %rdi
	jmp	.LBB41_14
.LBB41_9:                               # %if.then9
	cmpq	$33, %rcx
	jne	.LBB41_17
# BB#10:                                # %sw.bb
	movl	pedantic(%rip), %ecx
	testq	%rcx, %rcx
	jne	.LBB41_13
# BB#11:                                # %lor.lhs.false
	movq	%r15, %rdi
	callq	list_length
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$2, %rax
	jl	.LBB41_26
# BB#12:                                # %lor.lhs.false.if.then17_crit_edge
	movq	8(%rsp), %rax
.LBB41_13:                              # %if.then17
	movq	72(%rax), %rax
	movq	32(%rax), %rsi
	movabsq	$.L.str171, %rdi
.LBB41_14:                              # %if.end102
	xorq	%rax, %rax
	callq	error
.LBB41_29:                              # %if.end102
	movq	8(%rsp), %rdi
	movzbl	16(%rdi), %eax
	cmpq	$30, %rax
	jne	.LBB41_34
# BB#30:                                # %if.then108
	movq	8(%rdi), %rax
	cmpq	$0, 24(%rax)
	je	.LBB41_31
# BB#32:                                # %if.then108
	movabsq	$1, %rcx
	jmp	.LBB41_33
.LBB41_31:
	movabsq	$0, %rcx
.LBB41_33:                              # %if.then108
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	callq	gen_aux_info_record
.LBB41_34:                              # %if.end113
	movl	flag_no_common(%rip), %ecx
	movq	8(%rsp), %rax
	testq	%rcx, %rcx
	je	.LBB41_36
# BB#35:                                # %lor.lhs.false115
	movzbl	18(%rax), %ecx
	testq	$8, %rcx
	jne	.LBB41_37
.LBB41_36:                              # %if.then121
	orq	$131072, 48(%rax)       # imm = 0x20000
.LBB41_37:                              # %if.end126
	leaq	8(%rsp), %rdi
	xorq	%rdx, %rdx
	movq	%r14, %rsi
	callq	decl_attributes
	movq	current_binding_level(%rip), %rax
	movq	global_binding_level(%rip), %rcx
	cmpq	%rcx, %rax
	jne	.LBB41_39
# BB#38:                                # %if.then130
	movq	8(%rsp), %rdi
	callq	maybe_apply_pragma_weak
.LBB41_39:                              # %if.end131
	movq	8(%rsp), %rax
	movzbl	16(%rax), %ecx
	cmpq	$30, %rcx
	jne	.LBB41_44
# BB#40:                                # %land.lhs.true138
	movq	200(%rax), %rcx
	movzbl	(%rcx), %ecx
	testq	$1, %rcx
	je	.LBB41_44
# BB#41:                                # %land.lhs.true144
	movzbl	52(%rax), %ecx
	testq	$16, %rcx
	je	.LBB41_44
# BB#42:                                # %land.lhs.true151
	movq	136(%rax), %rsi
	movabsq	$.L.str177, %rdi
	callq	lookup_attribute
	testq	%rax, %rax
	je	.LBB41_44
# BB#43:                                # %if.then156
	movq	8(%rsp), %rdi
	movabsq	$.L.str178, %rsi
	xorq	%rax, %rax
	callq	warning_with_decl
.LBB41_44:                              # %if.end157
	movq	8(%rsp), %rdi
	callq	pushdecl
	movq	current_binding_level(%rip), %rcx
	movq	global_binding_level(%rip), %rdx
	movq	%rax, %rbx
	cmpq	%rdx, %rcx
	je	.LBB41_52
# BB#45:                                # %land.lhs.true161
	cmpq	$0, 144(%rbx)
	jne	.LBB41_52
# BB#46:                                # %land.lhs.true165
	cmpq	$0, 80(%rbx)
	jne	.LBB41_52
# BB#47:                                # %if.then168
	movq	8(%rbx), %rax
	movq	global_trees(%rip), %rcx
	cmpq	%rcx, %rax
	je	.LBB41_49
# BB#48:                                # %land.lhs.true173
	cmpq	$0, 32(%rax)
	jne	.LBB41_51
.LBB41_49:                              # %if.else181
	movzbl	16(%rax), %eax
	cmpq	$18, %rax
	jne	.LBB41_52
# BB#50:                                # %land.lhs.true190
	cmpq	$0, 104(%rbx)
	je	.LBB41_52
.LBB41_51:                              # %if.then195
	movq	%rbx, %rdi
	callq	expand_decl
.LBB41_52:                              # %if.end198
	movq	%rbx, %rax
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB41_17:                              # %sw.default
	movq	8(%rax), %rcx
	movq	global_trees(%rip), %rdx
	cmpq	%rdx, %rcx
	je	.LBB41_29
# BB#18:                                # %if.else
	movq	32(%rcx), %rdx
	testq	%rdx, %rdx
	je	.LBB41_22
# BB#19:                                # %if.then40
	movzbl	16(%rdx), %ecx
	cmpq	$25, %rcx
	jne	.LBB41_21
# BB#20:                                # %lor.lhs.false50
	movzbl	52(%rax), %eax
	testq	$32, %rax
	je	.LBB41_26
.LBB41_21:                              # %if.then55
	movabsq	$.L.str174, %rdi
	xorq	%rax, %rax
	callq	error
	jmp	.LBB41_29
.LBB41_22:                              # %if.else57
	movzbl	16(%rcx), %edx
	cmpq	$18, %rdx
	jne	.LBB41_23
# BB#24:                                # %if.else71
	movq	8(%rcx), %rcx
	cmpq	$0, 32(%rcx)
	je	.LBB41_25
.LBB41_26:                              # %if.then91
	movq	8(%rsp), %rax
	andq	$-257, 48(%rax)         # imm = 0xFFFFFFFFFFFFFEFF
	movq	current_binding_level(%rip), %rax
	movq	global_binding_level(%rip), %rcx
	cmpq	%rcx, %rax
	jne	.LBB41_28
# BB#27:                                # %if.then96
	movq	8(%rsp), %rax
	movl	16(%rax), %ecx
	orq	$262144, %rcx           # imm = 0x40000
	movl	%ecx, 16(%rax)
.LBB41_28:                              # %if.end100
	movq	global_trees(%rip), %rax
	movq	8(%rsp), %rcx
	movq	%rax, 104(%rcx)
	jmp	.LBB41_29
.LBB41_23:                              # %if.then65
	movq	72(%rax), %rax
	movq	32(%rax), %rsi
	movabsq	$.L.str175, %rdi
	jmp	.LBB41_14
.LBB41_25:                              # %if.then79
	movq	72(%rax), %rax
	movq	32(%rax), %rsi
	movabsq	$.L.str176, %rdi
	jmp	.LBB41_14
.Ltmp348:
	.size	start_decl, .Ltmp348-start_decl
	.cfi_endproc

	.globl	finish_decl
	.align	16, 0x90
	.type	finish_decl,@function
finish_decl:                            # @finish_decl
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp355:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp356:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp357:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp358:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp359:
	.cfi_def_cfa_offset 48
.Ltmp360:
	.cfi_offset %rbx, -48
.Ltmp361:
	.cfi_offset %r12, -40
.Ltmp362:
	.cfi_offset %r13, -32
.Ltmp363:
	.cfi_offset %r14, -24
.Ltmp364:
	.cfi_offset %r15, -16
	movq	%rdi, %r13
	movq	8(%r13), %r15
	movq	current_binding_level(%rip), %rax
	movq	global_binding_level(%rip), %rcx
	movq	40(%r13), %r12
	movq	%rsi, %rbx
	cmpq	%rcx, %rax
	jne	.LBB42_2
# BB#1:                                 # %if.then
	movq	%r13, %rdi
	movq	%rdx, %rsi
	callq	maybe_apply_renaming_pragma
	movq	%rax, %rdx
.LBB42_2:                               # %if.end
	xorq	%rax, %rax
	testq	%rdx, %rdx
	movabsq	$0, %r14
	je	.LBB42_4
# BB#3:                                 # %if.then5
	movq	40(%rdx), %r14
.LBB42_4:                               # %if.end6
	testq	%rbx, %rbx
	je	.LBB42_7
# BB#5:                                 # %land.lhs.true
	xorq	%rax, %rax
	cmpq	$0, 104(%r13)
	je	.LBB42_7
# BB#6:                                 # %select.mid
	movq	%rbx, %rax
.LBB42_7:                               # %if.end13
	movzbl	16(%r13), %ecx
	xorq	%rsi, %rsi
	cmpq	$35, %rcx
	je	.LBB42_9
# BB#8:                                 # %if.end13
	movq	%rax, %rsi
.LBB42_9:                               # %if.end13
	testq	%rsi, %rsi
	je	.LBB42_13
# BB#10:                                # %if.then20
	cmpq	$33, %rcx
	jne	.LBB42_11
# BB#12:                                # %if.else
	movq	8(%rsi), %rax
	movq	%rax, 8(%r13)
	movq	$0, 104(%r13)
	jmp	.LBB42_13
.LBB42_11:                              # %if.then27
	movq	%r13, %rdi
	callq	store_init_value
.LBB42_13:                              # %if.end35
	movzbl	16(%r15), %eax
	cmpq	$18, %rax
	jne	.LBB42_37
# BB#14:                                # %land.lhs.true42
	cmpq	$0, 24(%r15)
	jne	.LBB42_37
# BB#15:                                # %land.lhs.true46
	movl	16(%r13), %eax
	movq	%rax, %rcx
	andq	$255, %rcx
	cmpq	$33, %rcx
	je	.LBB42_37
# BB#16:                                # %if.then53
	movq	%r12, %rbx
	testq	$262144, %rax           # imm = 0x40000
	je	.LBB42_20
# BB#17:                                # %cond.true
	movl	pedantic(%rip), %ecx
	xorq	%r12, %r12
	testq	%rcx, %rcx
	je	.LBB42_23
# BB#18:                                # %land.rhs
	testq	$524288, %rax           # imm = 0x80000
	je	.LBB42_22
# BB#19:
	movabsq	$0, %r12
	jmp	.LBB42_23
.LBB42_20:                              # %cond.false
	movzbl	49(%r13), %eax
	testq	$1, %rax
	jne	.LBB42_21
.LBB42_22:                              # %cond.false
	movabsq	$1, %r12
	jmp	.LBB42_23
.LBB42_21:
	movabsq	$0, %r12
.LBB42_23:                              # %cond.end
	movq	104(%r13), %rsi
	movq	%r15, %rdi
	movq	%r12, %rdx
	callq	complete_array_type
	movq	8(%r13), %r15
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rax, %rcx
	cmpq	$2, %rcx
	jne	.LBB42_24
# BB#26:                                # %if.then81
	testq	%r12, %r12
	je	.LBB42_28
# BB#27:                                # %if.then83
	movabsq	$.L.str180, %rsi
	xorq	%rax, %rax
	movq	%r13, %rdi
	callq	error_with_decl
	movq	%rbx, %r12
	jmp	.LBB42_36
.LBB42_24:                              # %cond.end
	cmpq	$1, %rcx
	movq	%rbx, %r12
	jne	.LBB42_31
# BB#25:                                # %if.then77
	movabsq	$.L.str179, %rsi
.LBB42_35:                              # %if.end124
	xorq	%rax, %rax
	movq	%r13, %rdi
	callq	error_with_decl
	jmp	.LBB42_36
.LBB42_28:                              # %if.else84
	movl	pedantic(%rip), %eax
	testq	%rax, %rax
	movq	%rbx, %r12
	jne	.LBB42_36
# BB#29:                                # %land.lhs.true86
	movl	16(%r13), %eax
	andq	$786432, %rax           # imm = 0xC0000
	cmpq	$262144, %rax           # imm = 0x40000
	jne	.LBB42_36
# BB#30:                                # %if.then100
	orq	$256, 48(%r13)          # imm = 0x100
	jmp	.LBB42_36
.LBB42_31:                              # %if.else107
	movl	pedantic(%rip), %eax
	testq	%rax, %rax
	je	.LBB42_36
# BB#32:                                # %land.lhs.true109
	movq	24(%r15), %rax
	testq	%rax, %rax
	je	.LBB42_36
# BB#33:                                # %land.lhs.true114
	movq	112(%rax), %rdi
	callq	tree_int_cst_sgn
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	js	.LBB42_34
.LBB42_36:                              # %if.end124
	xorq	%rsi, %rsi
	movq	%r13, %rdi
	callq	layout_decl
.LBB42_37:                              # %if.end125
	movl	16(%r13), %eax
	movq	%rax, %rcx
	andq	$255, %rcx
	cmpq	$34, %rcx
	jne	.LBB42_56
# BB#38:                                # %if.then132
	cmpq	$0, 40(%r13)
	jne	.LBB42_48
# BB#39:                                # %land.lhs.true137
	movq	8(%r13), %rax
	movq	global_trees(%rip), %rcx
	cmpq	%rcx, %rax
	je	.LBB42_42
# BB#40:                                # %land.lhs.true142
	cmpq	$0, 32(%rax)
	je	.LBB42_42
# BB#41:                                # %if.end150
	xorq	%rsi, %rsi
	movq	%r13, %rdi
	callq	layout_decl
	cmpq	$0, 40(%r13)
	jne	.LBB42_48
.LBB42_42:                              # %land.lhs.true155
	movq	8(%r13), %rax
	movq	global_trees(%rip), %rcx
	cmpq	%rcx, %rax
	je	.LBB42_48
# BB#43:                                # %land.lhs.true160
	movzbl	18(%r13), %eax
	testq	$4, %rax
	je	.LBB42_46
# BB#44:                                # %cond.true167
	cmpq	$0, 104(%r13)
	jne	.LBB42_47
# BB#45:                                # %lor.lhs.false
	cmpq	$0, 80(%r13)
	jne	.LBB42_47
	jmp	.LBB42_48
.LBB42_46:                              # %cond.false175
	movzbl	49(%r13), %eax
	testq	$1, %rax
	jne	.LBB42_48
.LBB42_47:                              # %if.then183
	movabsq	$.L.str182, %rsi
	xorq	%rax, %rax
	movq	%r13, %rdi
	callq	error_with_decl
	movq	global_trees(%rip), %rax
	movq	%rax, 8(%r13)
.LBB42_48:                              # %if.end186
	movzbl	49(%r13), %eax
	testq	$1, %rax
	jne	.LBB42_50
# BB#49:                                # %lor.lhs.false194
	movzbl	18(%r13), %eax
	testq	$4, %rax
	je	.LBB42_54
.LBB42_50:                              # %land.lhs.true201
	movq	40(%r13), %rdi
	testq	%rdi, %rdi
	je	.LBB42_54
# BB#51:                                # %if.then206
	movzbl	16(%rdi), %eax
	cmpq	$25, %rax
	jne	.LBB42_53
# BB#52:                                # %if.then215
	callq	constant_expression_warning
	jmp	.LBB42_54
.LBB42_53:                              # %if.else218
	movabsq	$.L.str183, %rsi
	xorq	%rax, %rax
	movq	%r13, %rdi
	callq	error_with_decl
.LBB42_54:                              # %if.end220
	movzbl	18(%r15), %ecx
	movl	16(%r13), %eax
	testq	$1, %rcx
	je	.LBB42_56
# BB#55:                                # %if.then226
	orq	$65536, %rax            # imm = 0x10000
	movl	%eax, 16(%r13)
.LBB42_56:                              # %if.end233
	movq	%rax, %rcx
	andq	$255, %rcx
	cmpq	$30, %rcx
	jne	.LBB42_59
# BB#57:                                # %if.end233
	testq	%r14, %r14
	je	.LBB42_59
# BB#58:                                # %if.then242
	andq	$-1610612737, 48(%r13)  # imm = 0xFFFFFFFF9FFFFFFF
	movq	$0, 144(%r13)
	movq	%r14, %rdi
	callq	get_identifier
	movq	%rax, 120(%r13)
	movl	16(%r13), %eax
.LBB42_59:                              # %if.end249
	movq	%rax, %rcx
	andq	$255, %rcx
	cmpq	$30, %rcx
	je	.LBB42_61
# BB#60:                                # %if.end249
	cmpq	$34, %rcx
	jne	.LBB42_82
.LBB42_61:                              # %if.then263
	movq	%r13, %rdi
	callq	maybe_objc_check_decl
	cmpq	$0, 80(%r13)
	je	.LBB42_62
# BB#66:                                # %if.else292
	testq	%r14, %r14
	je	.LBB42_74
# BB#67:                                # %if.then294
	movq	48(%r13), %rax
	testq	$1024, %rax             # imm = 0x400
	je	.LBB42_69
# BB#68:                                # %if.then301
	movabsq	$2199023255552, %rcx    # imm = 0x20000000000
	orq	%rcx, %rax
	movq	%rax, 48(%r13)
.LBB42_69:                              # %if.end306
	movl	16(%r13), %ecx
	movq	%rcx, %rdx
	andq	$255, %rdx
	cmpq	$34, %rdx
	jne	.LBB42_73
# BB#70:                                # %land.lhs.true313
	testq	$1024, %rax             # imm = 0x400
	jne	.LBB42_73
# BB#71:                                # %land.lhs.true313
	andq	$262144, %rcx           # imm = 0x40000
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %rcx
	jne	.LBB42_73
# BB#72:                                # %if.then328
	movabsq	$.L.str184, %rsi
	xorq	%rax, %rax
	movq	%r13, %rdi
	callq	warning_with_decl
	jmp	.LBB42_74
.LBB42_62:                              # %if.then267
	movq	104(%r13), %rax
	testq	%rax, %rax
	je	.LBB42_64
# BB#63:                                # %if.then267
	movq	global_trees(%rip), %rcx
	cmpq	%rcx, %rax
	jne	.LBB42_65
.LBB42_64:                              # %if.then277
	orq	$262144, 48(%r13)       # imm = 0x40000
.LBB42_65:                              # %lor.end
	movq	$1, %rdx
	xorq	%rcx, %rcx
	movq	%r13, %rdi
	movq	%r14, %rsi
	callq	rest_of_decl_compilation
	jmp	.LBB42_76
.LBB42_73:                              # %if.else329
	movq	%r14, %rdi
	callq	get_identifier
	movq	%rax, 120(%r13)
.LBB42_74:                              # %if.end334
	movzbl	16(%r13), %eax
	cmpq	$30, %rax
	je	.LBB42_76
# BB#75:                                # %if.then341
	movq	%r13, %rdi
	callq	add_decl_stmt
.LBB42_76:                              # %if.end343
	movl	16(%r13), %eax
	cmpq	$0, 80(%r13)
	je	.LBB42_82
# BB#77:                                # %if.end343
	testq	%r12, %r12
	jne	.LBB42_82
# BB#78:                                # %if.end343
	movq	%rax, %rcx
	andq	$262144, %rcx           # imm = 0x40000
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	testq	%rdx, %rcx
	jne	.LBB42_82
# BB#79:                                # %land.lhs.true357
	movzbl	49(%r13), %ecx
	testq	$1, %rcx
	jne	.LBB42_82
# BB#80:                                # %if.then365
	movq	$6, %rcx
	movq	%rax, %rdx
	shrq	%cl, %rdx
	andq	$1024, %rdx             # imm = 0x400
	movabsq	$4294966271, %rcx       # imm = 0xFFFFFBFF
	andq	%rcx, %rax
	orq	%rdx, %rax
	movl	%eax, 16(%r13)
	cmpq	$0, 40(%r13)
	jne	.LBB42_82
# BB#81:                                # %if.then379
	movq	$0, 104(%r13)
.LBB42_82:                              # %if.end385
	andq	$255, %rax
	cmpq	$33, %rax
	jne	.LBB42_87
# BB#83:                                # %if.then392
	movq	%r13, %rdi
	callq	maybe_objc_check_decl
	cmpq	$0, 80(%r13)
	jne	.LBB42_84
# BB#85:                                # %if.then392
	movabsq	$1, %rdx
	jmp	.LBB42_86
.LBB42_84:
	movabsq	$0, %rdx
.LBB42_86:                              # %if.then392
	xorq	%rsi, %rsi
	xorq	%rcx, %rcx
	movq	%r13, %rdi
	callq	rest_of_decl_compilation
.LBB42_87:                              # %if.end397
	movq	current_binding_level(%rip), %rax
	movq	global_binding_level(%rip), %rcx
	cmpq	%rcx, %rax
	je	.LBB42_89
# BB#88:                                # %if.end402
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB42_89:                              # %if.then400
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	get_pending_sizes  # TAILCALL
.LBB42_34:                              # %if.then121
	movabsq	$.L.str181, %rsi
	jmp	.LBB42_35
.Ltmp365:
	.size	finish_decl, .Ltmp365-finish_decl
	.cfi_endproc

	.globl	maybe_build_cleanup
	.align	16, 0x90
	.type	maybe_build_cleanup,@function
maybe_build_cleanup:                    # @maybe_build_cleanup
	.cfi_startproc
# BB#0:                                 # %entry
	xorq	%rax, %rax
	retq
.Ltmp366:
	.size	maybe_build_cleanup, .Ltmp366-maybe_build_cleanup
	.cfi_endproc

	.globl	push_parm_decl
	.align	16, 0x90
	.type	push_parm_decl,@function
push_parm_decl:                         # @push_parm_decl
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp370:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp371:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp372:
	.cfi_def_cfa_offset 32
.Ltmp373:
	.cfi_offset %rbx, -24
.Ltmp374:
	.cfi_offset %r14, -16
	movq	%rdi, %rbx
	movl	immediate_size_expand(%rip), %r14d
	xorq	%rax, %rax
	movl	%eax, immediate_size_expand(%rip)
	movq	24(%rbx), %rax
	movq	24(%rax), %rsi
	movq	32(%rax), %rdi
	movq	$2, %rdx
	xorq	%rcx, %rcx
	callq	grokdeclarator
	movq	%rax, (%rsp)
	movq	32(%rbx), %rsi
	leaq	(%rsp), %rdi
	xorq	%rdx, %rdx
	callq	decl_attributes
	movq	(%rsp), %rdi
	callq	pushdecl
	movq	current_binding_level(%rip), %rcx
	movl	%r14d, immediate_size_expand(%rip)
	movq	%rax, (%rsp)
	movq	64(%rcx), %rdx
	xorq	%rdi, %rdi
	movq	%rax, %rsi
	callq	tree_cons
	movq	current_binding_level(%rip), %rcx
	movq	%rax, 64(%rcx)
	movq	(%rsp), %rdi
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	callq	finish_decl
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp375:
	.size	push_parm_decl, .Ltmp375-push_parm_decl
	.cfi_endproc

	.globl	clear_parm_order
	.align	16, 0x90
	.type	clear_parm_order,@function
clear_parm_order:                       # @clear_parm_order
	.cfi_startproc
# BB#0:                                 # %entry
	movq	current_binding_level(%rip), %rax
	movq	$0, 64(%rax)
	retq
.Ltmp376:
	.size	clear_parm_order, .Ltmp376-clear_parm_order
	.cfi_endproc

	.globl	build_compound_literal
	.align	16, 0x90
	.type	build_compound_literal,@function
build_compound_literal:                 # @build_compound_literal
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbp
.Ltmp380:
	.cfi_def_cfa_offset 16
.Ltmp381:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
.Ltmp382:
	.cfi_def_cfa_register %rbp
	pushq	%r15
	pushq	%r14
	pushq	%rbx
	pushq	%rax
.Ltmp383:
	.cfi_offset %rbx, -40
.Ltmp384:
	.cfi_offset %r14, -32
.Ltmp385:
	.cfi_offset %r15, -24
	movq	%rsi, %r15
	movq	%rdi, %r14
	movq	$34, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	callq	build_decl
	movq	%rax, %rbx
	andq	$-257, 48(%rbx)         # imm = 0xFFFFFFFFFFFFFEFF
	movl	16(%rbx), %eax
	movabsq	$4294443007, %rcx       # imm = 0xFFF7FFFF
	andq	%rax, %rcx
	movl	%ecx, 16(%rbx)
	movq	current_binding_level(%rip), %rcx
	movq	global_binding_level(%rip), %rdx
	cmpq	%rdx, %rcx
	jne	.LBB46_1
# BB#2:                                 # %entry
	movabsq	$1, %rdx
	jmp	.LBB46_3
.LBB46_1:
	movabsq	$0, %rdx
.LBB46_3:                               # %entry
	movq	$18, %rcx
	shlq	%cl, %rdx
	movabsq	$4294180863, %rcx       # imm = 0xFFF3FFFF
	andq	%rcx, %rax
	orq	%rdx, %rax
	movl	%eax, 16(%rbx)
	movq	current_function_decl(%rip), %rcx
	movq	%rcx, 80(%rbx)
	orq	$65536, %rax            # imm = 0x10000
	movl	%eax, 16(%rbx)
	movq	%r14, 8(%rbx)
	movl	16(%r14), %ecx
	andq	$4096, %rcx             # imm = 0x1000
	movabsq	$4294438911, %rdx       # imm = 0xFFF7EFFF
	andq	%rax, %rdx
	orq	%rcx, %rdx
	movl	%edx, 16(%rbx)
	movq	%rbx, %rdi
	movq	%r15, %rsi
	callq	store_init_value
	movzbl	16(%r14), %eax
	cmpq	$18, %rax
	jne	.LBB46_6
# BB#4:                                 # %land.lhs.true
	cmpq	$0, 32(%r14)
	jne	.LBB46_6
# BB#5:                                 # %if.then
	movq	104(%rbx), %rsi
	movq	$1, %rdx
	movq	%r14, %rdi
	callq	complete_array_type
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB46_11
.LBB46_6:                               # %if.end35
	movq	8(%rbx), %rax
	movq	global_trees(%rip), %r14
	cmpq	%r14, %rax
	je	.LBB46_10
# BB#7:                                 # %lor.lhs.false
	cmpq	$0, 32(%rax)
	je	.LBB46_10
# BB#8:                                 # %if.end45
	movq	$154, %rdi
	xorq	%rax, %rax
	movq	%rbx, %rsi
	callq	build_stmt
	movq	8(%rbx), %rsi
	movq	$170, %rdi
	movq	%rax, %rdx
	callq	build1
	movq	%rax, %r14
	movl	16(%r14), %eax
	orq	$256, %rax              # imm = 0x100
	movl	%eax, 16(%r14)
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	layout_decl
	movzbl	18(%rbx), %eax
	testq	$4, %rax
	je	.LBB46_10
# BB#9:                                 # %if.then60
	movq	%rsp, %r15
	addq	$-32, %r15
	movq	%r15, %rsp
	movl	var_labelno(%rip), %ecx
	movabsq	$.L.str186, %rsi
	movabsq	$.L.str187, %rdx
	xorq	%rax, %rax
	movq	%r15, %rdi
	callq	sprintf
	movl	var_labelno(%rip), %eax
	incq	%rax
	movl	%eax, var_labelno(%rip)
	movq	%r15, %rdi
	callq	get_identifier
	movq	%rax, 72(%rbx)
	orq	$71565312, 48(%rbx)     # imm = 0x4440000
	movq	%rbx, %rdi
	callq	pushdecl
	xorq	%rsi, %rsi
	movq	$1, %rdx
	xorq	%rcx, %rcx
	movq	%rbx, %rdi
	callq	rest_of_decl_compilation
.LBB46_10:                              # %return
	movq	%r14, %rax
	leaq	-24(%rbp), %rsp
	popq	%rbx
	popq	%r14
	popq	%r15
	popq	%rbp
	retq
.LBB46_11:                              # %if.then34
	movabsq	$.L.str185, %rax
	movabsq	$.L__FUNCTION__.build_compound_literal, %rcx
	movq	$3921, %rsi             # imm = 0xF51
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp386:
	.size	build_compound_literal, .Ltmp386-build_compound_literal
	.cfi_endproc

	.globl	get_parm_info
	.align	16, 0x90
	.type	get_parm_info,@function
get_parm_info:                          # @get_parm_info
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp393:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp394:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp395:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp396:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp397:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp398:
	.cfi_def_cfa_offset 64
.Ltmp399:
	.cfi_offset %rbx, -48
.Ltmp400:
	.cfi_offset %r12, -40
.Ltmp401:
	.cfi_offset %r13, -32
.Ltmp402:
	.cfi_offset %r14, -24
.Ltmp403:
	.cfi_offset %r15, -16
	movq	current_binding_level(%rip), %rax
	movq	%rdi, 8(%rsp)           # 8-byte Spill
	movq	(%rax), %r15
	movq	8(%rax), %rcx
	movq	%rcx, (%rsp)            # 8-byte Spill
	movq	64(%rax), %r13
	xorq	%r12, %r12
	movabsq	$4294967295, %r14       # imm = 0xFFFFFFFF
	testq	%r14, %rdi
	je	.LBB47_8
# BB#1:                                 # %entry
	testq	%r15, %r15
	je	.LBB47_8
# BB#2:                                 # %land.lhs.true2
	cmpq	$0, (%r15)
	jne	.LBB47_8
# BB#3:                                 # %land.lhs.true4
	movq	8(%r15), %rcx
	movzbl	16(%rcx), %ecx
	cmpq	$5, %rcx
	jne	.LBB47_8
# BB#4:                                 # %land.lhs.true8
	movzbl	17(%r15), %ecx
	testq	$24, %rcx
	jne	.LBB47_8
# BB#5:                                 # %land.lhs.true19
	movzbl	49(%r15), %ecx
	testq	$4, %rcx
	jne	.LBB47_8
# BB#6:                                 # %land.lhs.true25
	cmpq	$0, 72(%r15)
	jne	.LBB47_8
# BB#7:                                 # %if.then
	movq	$0, (%rax)
	movq	global_trees+216(%rip), %rsi
	xorq	%rdi, %rdi
	xorq	%rdx, %rdx
	callq	tree_cons
	xorq	%rdi, %rdi
	xorq	%rsi, %rsi
	jmp	.LBB47_38
.LBB47_14:                              # %for.cond.outer.backedge
                                        #   in Loop: Header=BB47_8 Depth=1
	movq	%r12, (%rbx)
	movq	%rbx, %r12
	.align	16, 0x90
.LBB47_8:                               # %for.cond
                                        # =>This Inner Loop Header: Depth=1
	movq	%r15, %rbx
	testq	%rbx, %rbx
	je	.LBB47_9
# BB#13:                                # %for.body
                                        #   in Loop: Header=BB47_8 Depth=1
	movl	16(%rbx), %eax
	movq	(%rbx), %r15
	movq	%rax, %rcx
	andq	$255, %rcx
	cmpq	$35, %rcx
	jne	.LBB47_14
# BB#15:                                # %if.else
                                        #   in Loop: Header=BB47_8 Depth=1
	testq	$16384, %rax            # imm = 0x4000
	je	.LBB47_8
# BB#16:                                # %if.then46
                                        #   in Loop: Header=BB47_8 Depth=1
	movabsq	$.L.str188, %rsi
	xorq	%rax, %rax
	movq	%rbx, %rdi
	callq	error_with_decl
	jmp	.LBB47_14
.LBB47_9:                               # %for.cond51.preheader
	xorq	%rbx, %rbx
	testq	%r13, %r13
	movabsq	$0, %rdi
	je	.LBB47_21
# BB#10:
	movq	%r13, %rax
	.align	16, 0x90
.LBB47_11:                              # %for.body53
                                        # =>This Inner Loop Header: Depth=1
	movq	(%rax), %rcx
	testq	%rcx, %rcx
	jne	.LBB47_12
# BB#17:                                # %if.else64
                                        #   in Loop: Header=BB47_11 Depth=1
	movq	32(%rax), %rcx
	movq	$0, (%rcx)
	jmp	.LBB47_18
	.align	16, 0x90
.LBB47_12:                              # %if.then57
                                        #   in Loop: Header=BB47_11 Depth=1
	movq	32(%rcx), %rcx
	movq	32(%rax), %rdx
	movq	%rcx, (%rdx)
.LBB47_18:                              # %for.inc
                                        #   in Loop: Header=BB47_11 Depth=1
	movq	(%rax), %rax
	testq	%rax, %rax
	jne	.LBB47_11
# BB#19:                                # %for.end72
	testq	%r13, %r13
	movabsq	$0, %rdi
	je	.LBB47_21
# BB#20:                                # %cond.true
	movq	32(%r13), %rdi
	callq	nreverse
	movq	%rax, %rdi
.LBB47_21:                              # %cond.end
	movq	%r12, %rsi
	callq	chainon
	movq	current_binding_level(%rip), %rcx
	movq	%rax, %r15
	testq	%r15, %r15
	movq	%r15, (%rcx)
	je	.LBB47_34
# BB#22:
	movq	%r15, %r12
	xorq	%r13, %r13
	.align	16, 0x90
.LBB47_23:                              # %for.body80
                                        # =>This Inner Loop Header: Depth=1
	movzbl	16(%r12), %eax
	cmpq	$35, %rax
	jne	.LBB47_33
# BB#24:                                # %if.then86
                                        #   in Loop: Header=BB47_23 Depth=1
	movq	8(%r12), %rsi
	movq	%rsi, 104(%r12)
	movzbl	target_flags+3(%rip), %eax
	testq	$2, %rax
	jne	.LBB47_29
# BB#25:                                # %land.lhs.true92
                                        #   in Loop: Header=BB47_23 Depth=1
	movzbl	16(%rsi), %ecx
	cmpq	$12, %rcx
	ja	.LBB47_29
# BB#26:                                # %land.lhs.true92
                                        #   in Loop: Header=BB47_23 Depth=1
	movq	$1, %rax
	shlq	%cl, %rax
	testq	$7232, %rax             # imm = 0x1C40
	je	.LBB47_29
# BB#27:                                # %land.lhs.true115
                                        #   in Loop: Header=BB47_23 Depth=1
	movq	integer_types+40(%rip), %rax
	movl	60(%rsi), %ecx
	movl	60(%rax), %edx
	andq	$511, %rcx              # imm = 0x1FF
	andq	$511, %rdx              # imm = 0x1FF
	cmpq	%rdx, %rcx
	jae	.LBB47_29
# BB#28:                                # %if.then124
                                        #   in Loop: Header=BB47_23 Depth=1
	movq	%rax, 104(%r12)
	.align	16, 0x90
.LBB47_29:                              # %if.end127
                                        #   in Loop: Header=BB47_23 Depth=1
	xorq	%rdi, %rdi
	movq	%rbx, %rdx
	callq	tree_cons
	movq	%rax, %rbx
	movq	32(%rbx), %rax
	movzbl	16(%rax), %eax
	cmpq	$5, %rax
	jne	.LBB47_33
# BB#30:                                # %if.end127
                                        #   in Loop: Header=BB47_23 Depth=1
	testq	%r14, %r13
	jne	.LBB47_33
# BB#31:                                # %land.lhs.true140
                                        #   in Loop: Header=BB47_23 Depth=1
	xorq	%r13, %r13
	cmpq	$0, 72(%r12)
	jne	.LBB47_33
# BB#32:                                # %if.then144
                                        #   in Loop: Header=BB47_23 Depth=1
	xorq	%rax, %rax
	movabsq	$.L.str189, %rdi
	callq	error
	movq	$1, %r13
	.align	16, 0x90
.LBB47_33:                              # %for.inc147
                                        #   in Loop: Header=BB47_23 Depth=1
	movq	(%r12), %r12
	testq	%r12, %r12
	jne	.LBB47_23
.LBB47_34:                              # %for.end150
	movq	8(%rsp), %rax           # 8-byte Reload
	testq	%r14, %rax
	je	.LBB47_36
# BB#35:                                # %if.then152
	movq	global_trees+216(%rip), %rsi
	xorq	%rdi, %rdi
	movq	%rbx, %rdx
	callq	tree_cons
	movq	%rax, %rdi
	jmp	.LBB47_37
.LBB47_36:                              # %if.end156
	movq	%rbx, %rdi
.LBB47_37:                              # %if.end156
	callq	nreverse
	movq	%r15, %rdi
	movq	(%rsp), %rsi            # 8-byte Reload
.LBB47_38:                              # %if.end156
	movq	%rax, %rdx
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	tree_cons  # TAILCALL
.Ltmp404:
	.size	get_parm_info, .Ltmp404-get_parm_info
	.cfi_endproc

	.globl	parmlist_tags_warning
	.align	16, 0x90
	.type	parmlist_tags_warning,@function
parmlist_tags_warning:                  # @parmlist_tags_warning
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp411:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp412:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp413:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp414:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp415:
	.cfi_def_cfa_offset 48
.Ltmp416:
	.cfi_offset %rbx, -48
.Ltmp417:
	.cfi_offset %r12, -40
.Ltmp418:
	.cfi_offset %r13, -32
.Ltmp419:
	.cfi_offset %r14, -24
.Ltmp420:
	.cfi_offset %r15, -16
	movq	current_binding_level(%rip), %rax
	movq	8(%rax), %r14
	testq	%r14, %r14
	je	.LBB48_20
# BB#1:
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	movabsq	$.L.str196, %rbx
	movq	$1, %r12
	movabsq	$.L.str190, %r13
	.align	16, 0x90
.LBB48_2:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	32(%r14), %rax
	movzbl	16(%rax), %eax
	movq	24(%r14), %rcx
	cmpq	$21, %rax
	jne	.LBB48_5
# BB#3:                                 # %land.lhs.true
                                        #   in Loop: Header=BB48_2 Depth=1
	testq	%rcx, %rcx
	jne	.LBB48_5
# BB#4:                                 # %land.lhs.true
                                        #   in Loop: Header=BB48_2 Depth=1
	movl	pedantic(%rip), %edx
	testq	%r15, %rdx
	je	.LBB48_19
	.align	16, 0x90
.LBB48_5:                               # %if.end
                                        #   in Loop: Header=BB48_2 Depth=1
	testq	%rcx, %rcx
	jne	.LBB48_6
# BB#11:                                # %if.else29
                                        #   in Loop: Header=BB48_2 Depth=1
	cmpq	$20, %rax
	jne	.LBB48_13
# BB#12:                                # %if.then31
                                        #   in Loop: Header=BB48_2 Depth=1
	xorq	%rax, %rax
	movabsq	$.L.str193, %rdi
	jmp	.LBB48_16
	.align	16, 0x90
.LBB48_6:                               # %if.then9
                                        #   in Loop: Header=BB48_2 Depth=1
	movq	32(%rcx), %rsi
	cmpq	$20, %rax
	jne	.LBB48_8
# BB#7:                                 # %if.then11
                                        #   in Loop: Header=BB48_2 Depth=1
	xorq	%rax, %rax
	movq	%r13, %rdi
	callq	warning
	jmp	.LBB48_17
.LBB48_13:                              # %if.else32
                                        #   in Loop: Header=BB48_2 Depth=1
	cmpq	$21, %rax
	jne	.LBB48_15
# BB#14:                                # %if.then34
                                        #   in Loop: Header=BB48_2 Depth=1
	xorq	%rax, %rax
	movabsq	$.L.str194, %rdi
	jmp	.LBB48_16
.LBB48_8:                               # %if.else
                                        #   in Loop: Header=BB48_2 Depth=1
	cmpq	$21, %rax
	jne	.LBB48_10
# BB#9:                                 # %if.then15
                                        #   in Loop: Header=BB48_2 Depth=1
	xorq	%rax, %rax
	movabsq	$.L.str191, %rdi
	callq	warning
	jmp	.LBB48_17
.LBB48_15:                              # %if.else35
                                        #   in Loop: Header=BB48_2 Depth=1
	xorq	%rax, %rax
	movabsq	$.L.str195, %rdi
.LBB48_16:                              # %if.end38
                                        #   in Loop: Header=BB48_2 Depth=1
	callq	warning
	jmp	.LBB48_17
.LBB48_10:                              # %if.else21
                                        #   in Loop: Header=BB48_2 Depth=1
	xorq	%rax, %rax
	movabsq	$.L.str192, %rdi
	callq	warning
	.align	16, 0x90
.LBB48_17:                              # %if.end38
                                        #   in Loop: Header=BB48_2 Depth=1
	movzbl	parmlist_tags_warning.already(%rip), %eax
	testq	%rax, %rax
	jne	.LBB48_19
# BB#18:                                # %if.then40
                                        #   in Loop: Header=BB48_2 Depth=1
	xorq	%rax, %rax
	movq	%rbx, %rdi
	callq	warning
	movb	%r12b, parmlist_tags_warning.already(%rip)
.LBB48_19:                              # %for.inc
                                        #   in Loop: Header=BB48_2 Depth=1
	movq	(%r14), %r14
	testq	%r14, %r14
	jne	.LBB48_2
.LBB48_20:                              # %for.end
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp421:
	.size	parmlist_tags_warning, .Ltmp421-parmlist_tags_warning
	.cfi_endproc

	.globl	xref_tag
	.align	16, 0x90
	.type	xref_tag,@function
xref_tag:                               # @xref_tag
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp428:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp429:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp430:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp431:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp432:
	.cfi_def_cfa_offset 48
.Ltmp433:
	.cfi_offset %rbx, -48
.Ltmp434:
	.cfi_offset %r12, -40
.Ltmp435:
	.cfi_offset %r13, -32
.Ltmp436:
	.cfi_offset %r14, -24
.Ltmp437:
	.cfi_offset %r15, -16
	movq	current_binding_level(%rip), %rcx
	movq	%rsi, %r12
	movq	%rdi, %r15
	testq	%rcx, %rcx
	je	.LBB49_17
# BB#1:
	movq	$1, %rax
.LBB49_2:                               # %for.body.us.i
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB49_4 Depth 2
	movq	8(%rcx), %rbx
	jmp	.LBB49_4
	.align	16, 0x90
.LBB49_3:                               # %for.cond1.us.i
                                        #   in Loop: Header=BB49_4 Depth=2
	movq	(%rbx), %rbx
.LBB49_4:                               # %for.cond1.us.i
                                        #   Parent Loop BB49_2 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	testq	%rbx, %rbx
	je	.LBB49_14
# BB#5:                                 # %for.body3.us.i
                                        #   in Loop: Header=BB49_4 Depth=2
	movq	24(%rbx), %rdx
	cmpq	%r12, %rdx
	jne	.LBB49_3
	jmp	.LBB49_6
	.align	16, 0x90
.LBB49_14:                              # %for.end.us.i
                                        #   in Loop: Header=BB49_2 Depth=1
	movzbl	49(%rcx), %esi
	xorq	%rdx, %rdx
	testq	%rsi, %rsi
	je	.LBB49_16
# BB#15:                                # %select.mid
                                        #   in Loop: Header=BB49_2 Depth=1
	movq	%rax, %rdx
.LBB49_16:                              # %select.end
                                        #   in Loop: Header=BB49_2 Depth=1
	movq	40(%rcx), %rcx
	testq	%rcx, %rcx
	movq	%rdx, %rax
	jne	.LBB49_2
	jmp	.LBB49_17
.LBB49_6:                               # %if.then.i
	movq	32(%rbx), %r14
	movzbl	16(%r14), %edx
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	movq	%r15, %r13
	andq	%rcx, %r13
	cmpq	%r13, %rdx
	je	.LBB49_12
# BB#7:                                 # %if.then7.i
	movq	input_filename(%rip), %rdi
	movl	lineno(%rip), %esi
	movq	%r12, pending_invalid_xref(%rip)
	movq	%rdi, pending_invalid_xref_file(%rip)
	movl	%esi, pending_invalid_xref_line(%rip)
	testq	%rcx, %rax
	je	.LBB49_11
# BB#8:                                 # %if.then9.i
	testq	%r12, %r12
	je	.LBB49_10
# BB#9:                                 # %if.then.i.i
	movq	32(%r12), %rcx
	movabsq	$.L.str155, %rdx
	xorq	%rax, %rax
	callq	error_with_file_and_line
.LBB49_10:                              # %pending_xref_error.exit.i
	movq	$0, pending_invalid_xref(%rip)
.LBB49_11:                              # %lookup_tag.exitthread-pre-split
	movq	32(%rbx), %r14
.LBB49_12:                              # %lookup_tag.exit
	testq	%r14, %r14
	je	.LBB49_17
# BB#13:                                # %land.lhs.true
	movzbl	16(%r14), %eax
	cmpq	%r13, %rax
	je	.LBB49_24
.LBB49_17:                              # %if.end
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	andq	%r15, %rbx
	movq	%r15, %rdi
	callq	make_node
	movq	%rax, %r14
	cmpq	$10, %rbx
	movabsq	$current_binding_level, %rax
	jne	.LBB49_19
# BB#18:                                # %if.then4
	movq	integer_types+48(%rip), %rcx
	movl	60(%rcx), %ecx
	movl	60(%r14), %edx
	andq	$65024, %rcx            # imm = 0xFE00
	andq	$-65025, %rdx           # imm = 0xFFFFFFFFFFFF01FF
	orq	%rcx, %rdx
	movl	%edx, 60(%r14)
	movq	integer_types+48(%rip), %rcx
	movl	64(%rcx), %ecx
	movl	%ecx, 64(%r14)
	movq	%rdx, %rcx
	andq	$2147483647, %rcx       # imm = 0x7FFFFFFF
	movl	%ecx, 60(%r14)
	movl	16(%r14), %ecx
	orq	$8192, %rcx             # imm = 0x2000
	movl	%ecx, 16(%r14)
	movq	integer_types+48(%rip), %rcx
	movl	60(%rcx), %ecx
	andq	$511, %rcx              # imm = 0x1FF
	andq	$2147483136, %rdx       # imm = 0x7FFFFE00
	orq	%rcx, %rdx
	movl	%edx, 60(%r14)
	movq	integer_types+48(%rip), %rcx
	movq	104(%rcx), %rcx
	movq	%rcx, 104(%r14)
	movq	integer_types+48(%rip), %rcx
	movq	112(%rcx), %rcx
	movq	%rcx, 112(%r14)
	.align	16, 0x90
.LBB49_19:                              # %for.cond.i
                                        # =>This Inner Loop Header: Depth=1
	movq	(%rax), %rbx
	movzbl	49(%rbx), %ecx
	leaq	40(%rbx), %rax
	testq	%rcx, %rcx
	jne	.LBB49_19
# BB#20:                                # %for.end.i
	testq	%r12, %r12
	je	.LBB49_23
# BB#21:                                # %if.then.i52
	cmpq	$0, 96(%r14)
	jne	.LBB49_23
# BB#22:                                # %if.then4.i
	movq	%r12, 96(%r14)
.LBB49_23:                              # %pushtag.exit
	movq	8(%rbx), %rdx
	movq	%r12, %rdi
	movq	%r14, %rsi
	callq	tree_cons
	movq	%rax, 8(%rbx)
	movq	$33, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	callq	build_decl
	movq	%rax, %rdi
	callq	pushdecl
	movq	%rax, (%r14)
	movq	80(%rax), %rax
	movq	%rax, 144(%r14)
.LBB49_24:                              # %return
	movq	%r14, %rax
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp438:
	.size	xref_tag, .Ltmp438-xref_tag
	.cfi_endproc

	.globl	start_struct
	.align	16, 0x90
	.type	start_struct,@function
start_struct:                           # @start_struct
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp445:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp446:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp447:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp448:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp449:
	.cfi_def_cfa_offset 48
.Ltmp450:
	.cfi_offset %rbx, -48
.Ltmp451:
	.cfi_offset %r12, -40
.Ltmp452:
	.cfi_offset %r13, -32
.Ltmp453:
	.cfi_offset %r14, -24
.Ltmp454:
	.cfi_offset %r15, -16
	movq	%rsi, %r13
	movq	%rdi, %r15
	testq	%r13, %r13
	je	.LBB50_17
# BB#1:                                 # %if.then
	movq	current_binding_level(%rip), %rax
	jmp	.LBB50_2
	.align	16, 0x90
.LBB50_16:                              # %for.inc21.i
                                        #   in Loop: Header=BB50_2 Depth=1
	movq	40(%rax), %rax
.LBB50_2:                               # %if.then
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB50_5 Depth 2
	testq	%rax, %rax
	je	.LBB50_17
# BB#3:                                 # %for.body.i
                                        #   in Loop: Header=BB50_2 Depth=1
	movq	8(%rax), %rbx
	jmp	.LBB50_5
	.align	16, 0x90
.LBB50_4:                               # %for.cond1.i
                                        #   in Loop: Header=BB50_5 Depth=2
	movq	(%rbx), %rbx
.LBB50_5:                               # %for.cond1.i
                                        #   Parent Loop BB50_2 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	testq	%rbx, %rbx
	je	.LBB50_15
# BB#6:                                 # %for.body3.i
                                        #   in Loop: Header=BB50_5 Depth=2
	movq	24(%rbx), %rcx
	cmpq	%r13, %rcx
	jne	.LBB50_4
	jmp	.LBB50_7
	.align	16, 0x90
.LBB50_15:                              # %for.end.i
                                        #   in Loop: Header=BB50_2 Depth=1
	movzbl	49(%rax), %ecx
	testq	%rcx, %rcx
	jne	.LBB50_16
	jmp	.LBB50_17
.LBB50_7:                               # %if.then.i
	movq	32(%rbx), %r14
	movzbl	16(%r14), %eax
	movabsq	$4294967295, %r12       # imm = 0xFFFFFFFF
	andq	%r15, %r12
	cmpq	%r12, %rax
	je	.LBB50_9
# BB#8:                                 # %pending_xref_error.exit.i
	movq	input_filename(%rip), %rdi
	movl	lineno(%rip), %esi
	movq	%r13, pending_invalid_xref(%rip)
	movq	%rdi, pending_invalid_xref_file(%rip)
	movl	%esi, pending_invalid_xref_line(%rip)
	movq	32(%r13), %rcx
	movabsq	$.L.str155, %rdx
	xorq	%rax, %rax
	callq	error_with_file_and_line
	movq	$0, pending_invalid_xref(%rip)
	movq	32(%rbx), %r14
.LBB50_9:                               # %if.end
	testq	%r14, %r14
	je	.LBB50_17
# BB#10:                                # %land.lhs.true
	movzbl	16(%r14), %eax
	cmpq	%r12, %rax
	jne	.LBB50_17
# BB#11:                                # %if.then3
	movl	60(%r14), %eax
	orq	$16777216, %rax         # imm = 0x1000000
	movl	%eax, 60(%r14)
	movl	flag_pack_struct(%rip), %ecx
	andq	$1, %rcx
	shlq	$20, %rcx
	movabsq	$4293918719, %rdx       # imm = 0xFFEFFFFF
	andq	%rax, %rdx
	orq	%rcx, %rdx
	movl	%edx, 60(%r14)
	cmpq	$0, 24(%r14)
	je	.LBB50_23
# BB#12:                                # %if.then12
	movq	32(%r13), %rsi
	cmpq	$21, %r12
	jne	.LBB50_24
# BB#13:                                # %if.then14
	movabsq	$.L.str197, %rdi
	jmp	.LBB50_14
.LBB50_17:                              # %if.end20
	movq	%r15, %rdi
	callq	make_node
	movq	%rax, %r14
	movabsq	$current_binding_level, %rax
	.align	16, 0x90
.LBB50_18:                              # %for.cond.i
                                        # =>This Inner Loop Header: Depth=1
	movq	(%rax), %rbx
	movzbl	49(%rbx), %ecx
	leaq	40(%rbx), %rax
	testq	%rcx, %rcx
	jne	.LBB50_18
# BB#19:                                # %for.end.i52
	testq	%r13, %r13
	je	.LBB50_22
# BB#20:                                # %if.then.i54
	cmpq	$0, 96(%r14)
	jne	.LBB50_22
# BB#21:                                # %if.then4.i
	movq	%r13, 96(%r14)
.LBB50_22:                              # %pushtag.exit
	movq	8(%rbx), %rdx
	movq	%r13, %rdi
	movq	%r14, %rsi
	callq	tree_cons
	movq	%rax, 8(%rbx)
	movq	$33, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	callq	build_decl
	movq	%rax, %rdi
	callq	pushdecl
	movq	%rax, (%r14)
	movq	80(%rax), %rax
	movq	%rax, 144(%r14)
	movl	60(%r14), %eax
	orq	$16777216, %rax         # imm = 0x1000000
	movl	%eax, 60(%r14)
	movl	flag_pack_struct(%rip), %ecx
	andq	$1, %rcx
	shlq	$20, %rcx
	movabsq	$4293918719, %rdx       # imm = 0xFFEFFFFF
	andq	%rax, %rdx
	orq	%rcx, %rdx
	movl	%edx, 60(%r14)
.LBB50_23:                              # %return
	movq	%r14, %rax
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB50_24:                              # %if.else
	movabsq	$.L.str198, %rdi
.LBB50_14:                              # %return
	xorq	%rax, %rax
	callq	error
	jmp	.LBB50_23
.Ltmp455:
	.size	start_struct, .Ltmp455-start_struct
	.cfi_endproc

	.globl	grokfield
	.align	16, 0x90
	.type	grokfield,@function
grokfield:                              # @grokfield
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp459:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp460:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp461:
	.cfi_def_cfa_offset 32
.Ltmp462:
	.cfi_offset %rbx, -24
.Ltmp463:
	.cfi_offset %r14, -16
	movq	%r8, %r14
	movq	%rcx, %rax
	movq	%rdx, %rcx
	orq	%r14, %rcx
	jne	.LBB51_5
# BB#1:                                 # %if.then
	movq	32(%rax), %rsi
	movl	16(%rsi), %ecx
	movq	%rcx, %rdi
	andq	$255, %rdi
	cmpq	$33, %rdi
	jne	.LBB51_3
# BB#2:                                 # %if.then4
	movq	8(%rsi), %rcx
	movl	16(%rcx), %ecx
.LBB51_3:                               # %if.end
	andq	$254, %rcx
	cmpq	$20, %rcx
	jne	.LBB51_4
.LBB51_5:                               # %if.end20
	testq	%r14, %r14
	je	.LBB51_6
# BB#7:                                 # %if.end20
	movabsq	$1, %rbx
	jmp	.LBB51_8
.LBB51_6:
	movabsq	$0, %rbx
.LBB51_8:                               # %if.end20
	addq	$3, %rbx
	xorq	%rcx, %rcx
	movq	%rdx, %rdi
	movq	%rax, %rsi
	movq	%rbx, %rdx
	callq	grokdeclarator
	movq	%rax, %rbx
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	movq	%rbx, %rdi
	callq	finish_decl
	movq	%r14, 104(%rbx)
	movq	%rbx, %rdi
	callq	maybe_objc_check_decl
	jmp	.LBB51_9
.LBB51_4:                               # %if.then18
	movabsq	$.L.str199, %rdi
	xorq	%rax, %rax
	xorq	%rbx, %rbx
	callq	error
.LBB51_9:                               # %return
	movq	%rbx, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp464:
	.size	grokfield, .Ltmp464-grokfield
	.cfi_endproc

	.globl	finish_struct
	.align	16, 0x90
	.type	finish_struct,@function
finish_struct:                          # @finish_struct
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp471:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp472:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp473:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp474:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp475:
	.cfi_def_cfa_offset 48
	subq	$48, %rsp
.Ltmp476:
	.cfi_def_cfa_offset 96
.Ltmp477:
	.cfi_offset %rbx, -48
.Ltmp478:
	.cfi_offset %r12, -40
.Ltmp479:
	.cfi_offset %r13, -32
.Ltmp480:
	.cfi_offset %r14, -24
.Ltmp481:
	.cfi_offset %r15, -16
	movq	global_binding_level(%rip), %r14
	movq	current_binding_level(%rip), %rcx
	movq	%rcx, 16(%rsp)          # 8-byte Spill
	movq	%rdx, %rax
	movq	%rsi, %rbx
	movq	%rdi, 40(%rsp)
	movq	%rbx, 32(%rsp)
	cmpq	%rcx, %r14
	jne	.LBB52_1
# BB#2:                                 # %entry
	movabsq	$1, %rcx
	jmp	.LBB52_3
.LBB52_1:
	movabsq	$0, %rcx
.LBB52_3:                               # %entry
	movq	%rcx, 24(%rsp)          # 8-byte Spill
	movq	$0, 32(%rdi)
	leaq	40(%rsp), %rdi
	movq	$8, %rdx
	movq	%rax, %rsi
	callq	decl_attributes
	movq	40(%rsp), %rcx
	movzbl	16(%rcx), %eax
	cmpq	$21, %rax
	jne	.LBB52_6
# BB#4:                                 # %land.lhs.true
	cmpq	$0, 96(%rcx)
	movl	pedantic(%rip), %ecx
	je	.LBB52_13
# BB#5:                                 # %land.lhs.true
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	testq	%rdx, %rcx
	je	.LBB52_7
	jmp	.LBB52_13
.LBB52_6:                               # %land.lhs.true6
	movl	pedantic(%rip), %ecx
	testq	%rcx, %rcx
	jne	.LBB52_14
.LBB52_7:                               # %if.then
	movq	current_binding_level(%rip), %rcx
	movzbl	48(%rcx), %ecx
	testq	%rcx, %rcx
	je	.LBB52_25
# BB#8:                                 # %if.then
	movl	flag_traditional(%rip), %ecx
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	testq	%rdx, %rcx
	jne	.LBB52_25
# BB#9:                                 # %if.then19
	cmpq	$21, %rax
	je	.LBB52_10
# BB#11:                                # %if.then19
	movabsq	$.L.str202, %rsi
	jmp	.LBB52_12
.LBB52_10:
	movabsq	$.L.str201, %rsi
.LBB52_12:                              # %if.then19
	movabsq	$.L.str200, %rdi
	xorq	%rax, %rax
	callq	warning
	movl	pedantic(%rip), %ecx
.LBB52_13:                              # %if.end29
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %rcx
	je	.LBB52_25
.LBB52_14:                              # %for.cond.preheader
	testq	%rbx, %rbx
	je	.LBB52_18
# BB#15:
	movq	%rbx, %rax
	.align	16, 0x90
.LBB52_17:                              # %for.body
                                        # =>This Inner Loop Header: Depth=1
	cmpq	$0, 72(%rax)
	jne	.LBB52_25
# BB#16:                                # %for.cond
                                        #   in Loop: Header=BB52_17 Depth=1
	movq	(%rax), %rax
	testq	%rax, %rax
	jne	.LBB52_17
.LBB52_18:                              # %if.then41
	movq	40(%rsp), %rax
	movzbl	16(%rax), %eax
	cmpq	$21, %rax
	je	.LBB52_19
# BB#20:                                # %if.then41
	movabsq	$.L.str204, %rsi
	jmp	.LBB52_21
.LBB52_19:
	movabsq	$.L.str201, %rsi
.LBB52_21:                              # %if.then41
	testq	%rbx, %rbx
	jne	.LBB52_22
# BB#23:                                # %if.then41
	movabsq	$.L.str206, %rdx
	jmp	.LBB52_24
.LBB52_22:
	movabsq	$.L.str205, %rdx
.LBB52_24:                              # %if.then41
	movabsq	$.L.str203, %rdi
	xorq	%rax, %rax
	callq	pedwarn
.LBB52_25:                              # %for.cond53.preheader
	movq	%r14, 8(%rsp)           # 8-byte Spill
	testq	%rbx, %rbx
	je	.LBB52_117
# BB#26:
	xorq	%r12, %r12
	movabsq	$4294967295, %r14       # imm = 0xFFFFFFFF
	movabsq	$.L.str207, %r15
	jmp	.LBB52_27
.LBB52_70:                              # %if.else338
                                        #   in Loop: Header=BB52_27 Depth=1
	movq	104(%rbx), %rdi
	movq	%r13, %rsi
	callq	compare_tree_int
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jle	.LBB52_72
# BB#71:                                # %if.then345
                                        #   in Loop: Header=BB52_27 Depth=1
	xorq	%rax, %rax
	movq	%rbx, %rdi
	movabsq	$.L.str212, %rsi
	callq	pedwarn_with_decl
	jmp	.LBB52_92
.LBB52_72:                              # %if.else346
                                        #   in Loop: Header=BB52_27 Depth=1
	movq	104(%rbx), %rdi
	callq	integer_zerop
	testq	%r14, %rax
	je	.LBB52_75
# BB#73:                                # %land.lhs.true351
                                        #   in Loop: Header=BB52_27 Depth=1
	cmpq	$0, 72(%rbx)
	jne	.LBB52_74
.LBB52_75:                              # %if.else357
                                        #   in Loop: Header=BB52_27 Depth=1
	movq	104(%rbx), %rdi
	movq	$1, %rsi
	callq	tree_low_cst
	movq	8(%rbx), %rcx
	movl	16(%rcx), %esi
	movq	%rax, %r13
	movq	%rsi, %rax
	andq	$255, %rax
	cmpq	$10, %rax
	jne	.LBB52_79
# BB#76:                                # %land.lhs.true369
                                        #   in Loop: Header=BB52_27 Depth=1
	movq	104(%rcx), %rdi
	movq	$13, %rcx
	shrq	%cl, %rsi
	andq	$1, %rsi
	callq	min_precision
	andq	%r14, %rax
	cmpq	%rax, %r13
	jb	.LBB52_78
# BB#77:                                # %lor.lhs.false383
                                        #   in Loop: Header=BB52_27 Depth=1
	movq	8(%rbx), %rax
	movl	16(%rax), %esi
	movq	112(%rax), %rdi
	movq	$13, %rcx
	shrq	%cl, %rsi
	andq	$1, %rsi
	callq	min_precision
	andq	%r14, %rax
	cmpq	%rax, %r13
	jae	.LBB52_79
.LBB52_78:                              # %if.then398
                                        #   in Loop: Header=BB52_27 Depth=1
	xorq	%rax, %rax
	movq	%rbx, %rdi
	movabsq	$.L.str214, %rsi
	callq	warning_with_decl
.LBB52_79:                              # %if.end399
                                        #   in Loop: Header=BB52_27 Depth=1
	movq	$3, %rsi
	movq	%r13, %rdi
	callq	size_int_wide
	movq	%rax, 40(%rbx)
	movabsq	$2199023259648, %rax    # imm = 0x20000001000
	orq	%rax, 48(%rbx)
	testq	%r13, %r13
	jne	.LBB52_92
# BB#80:                                # %land.lhs.true413
                                        #   in Loop: Header=BB52_27 Depth=1
	movq	40(%rsp), %rdi
	callq	*targetm+296(%rip)
	testq	%rax, %rax
	jne	.LBB52_92
# BB#81:                                # %if.then415
                                        #   in Loop: Header=BB52_27 Depth=1
	movl	target_flags(%rip), %edx
	movl	56(%rbx), %eax
	movq	%rax, %rsi
	movq	$20, %rcx
	shrq	%cl, %rdx
	andq	$32, %rdx
	addq	$32, %rdx
	andq	$16777215, %rsi         # imm = 0xFFFFFF
	movq	%rdx, %rcx
	movabsq	$4294967264, %rdi       # imm = 0xFFFFFFE0
	andq	%rdi, %rcx
	cmpq	%rcx, %rsi
	movq	%rax, %rcx
	ja	.LBB52_83
# BB#82:                                # %if.then415
                                        #   in Loop: Header=BB52_27 Depth=1
	movq	%rdx, %rcx
.LBB52_83:                              # %if.then415
                                        #   in Loop: Header=BB52_27 Depth=1
	movq	%rcx, %rdi
	andq	$16777215, %rdi         # imm = 0xFFFFFF
	movabsq	$4278190080, %rdx       # imm = 0xFF000000
	andq	%rdx, %rax
	movq	%rdi, %rdx
	orq	%rax, %rdx
	movl	%edx, 56(%rbx)
	movq	8(%rbx), %rdx
	movl	64(%rdx), %esi
	cmpq	%rsi, %rdi
	ja	.LBB52_85
# BB#84:                                # %if.then415
                                        #   in Loop: Header=BB52_27 Depth=1
	movq	%rsi, %rcx
.LBB52_85:                              # %if.then415
                                        #   in Loop: Header=BB52_27 Depth=1
	andq	$16777215, %rcx         # imm = 0xFFFFFF
	orq	%rax, %rcx
	movl	%ecx, 56(%rbx)
	movl	60(%rdx), %eax
	movq	$31, %rcx
	movq	48(%rbx), %rdx
	jmp	.LBB52_91
.LBB52_74:                              # %if.then356
                                        #   in Loop: Header=BB52_27 Depth=1
	xorq	%rax, %rax
	movq	%rbx, %rdi
	movabsq	$.L.str213, %rsi
	callq	error_with_decl
	jmp	.LBB52_92
	.align	16, 0x90
.LBB52_27:                              # %for.body55
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB52_28 Depth 2
                                        #     Child Loop BB52_40 Depth 2
	movq	40(%rsp), %rax
	movq	%rax, 80(%rbx)
	movq	40(%rsp), %rax
	movl	60(%rax), %eax
	movq	48(%rbx), %rdx
	movq	$20, %rcx
	shrq	%cl, %rax
	movq	$10, %rcx
	movq	%rdx, %rsi
	shrq	%cl, %rsi
	orq	%rax, %rsi
	andq	$1, %rsi
	shlq	$10, %rsi
	andq	$-1025, %rdx            # imm = 0xFFFFFFFFFFFFFBFF
	orq	%rsi, %rdx
	movq	%rdx, 48(%rbx)
	movzbl	17(%rbx), %eax
	testq	$16, %rax
	movq	%rbx, %rcx
	jne	.LBB52_31
	.align	16, 0x90
.LBB52_28:                              # %while.cond
                                        #   Parent Loop BB52_27 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	8(%rcx), %rcx
	movl	16(%rcx), %eax
	movq	%rax, %rdx
	andq	$255, %rdx
	cmpq	$18, %rdx
	je	.LBB52_28
# BB#29:                                # %while.cond
                                        #   in Loop: Header=BB52_27 Depth=1
	addq	$-20, %rdx
	andq	%r14, %rdx
	cmpq	$1, %rdx
	ja	.LBB52_32
# BB#30:                                # %land.lhs.true99
                                        #   in Loop: Header=BB52_27 Depth=1
	testq	$33554432, %rax         # imm = 0x2000000
	je	.LBB52_32
.LBB52_31:                              # %if.then106
                                        #   in Loop: Header=BB52_27 Depth=1
	movq	40(%rsp), %rax
	movl	16(%rax), %ecx
	orq	$33554432, %rcx         # imm = 0x2000000
	movl	%ecx, 16(%rax)
.LBB52_32:                              # %if.end113
                                        #   in Loop: Header=BB52_27 Depth=1
	movzbl	17(%rbx), %eax
	testq	$8, %rax
	je	.LBB52_34
# BB#33:                                # %if.then119
                                        #   in Loop: Header=BB52_27 Depth=1
	movq	40(%rsp), %rax
	movl	16(%rax), %ecx
	orq	$67108864, %rcx         # imm = 0x4000000
	movl	%ecx, 16(%rax)
.LBB52_34:                              # %if.end124
                                        #   in Loop: Header=BB52_27 Depth=1
	movzbl	52(%rbx), %eax
	testq	$32, %rax
	je	.LBB52_36
# BB#35:                                # %if.then131
                                        #   in Loop: Header=BB52_27 Depth=1
	movq	40(%rsp), %rax
	movl	60(%rax), %ecx
	orq	$33554432, %rcx         # imm = 0x2000000
	movl	%ecx, 60(%rax)
.LBB52_36:                              # %if.end137
                                        #   in Loop: Header=BB52_27 Depth=1
	movq	8(%rbx), %rax
	movq	40(%rsp), %rcx
	cmpq	%rcx, %rax
	jne	.LBB52_38
# BB#37:                                # %if.then142
                                        #   in Loop: Header=BB52_27 Depth=1
	movq	96(%rax), %rax
	movq	32(%rax), %rsi
	xorq	%rax, %rax
	movq	%r15, %rdi
	callq	error
.LBB52_38:                              # %if.end145
                                        #   in Loop: Header=BB52_27 Depth=1
	movq	104(%rbx), %rdi
	testq	%rdi, %rdi
	je	.LBB52_52
# BB#39:                                # %while.cond149.preheader
                                        #   in Loop: Header=BB52_27 Depth=1
	movzbl	16(%rdi), %eax
	addq	$-114, %rax
	andq	%r14, %rax
	cmpq	$2, %rax
	ja	.LBB52_45
	.align	16, 0x90
.LBB52_40:                              # %land.lhs.true176
                                        #   Parent Loop BB52_27 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	32(%rdi), %rax
	movq	global_trees(%rip), %rcx
	cmpq	%rcx, %rax
	je	.LBB52_41
# BB#42:                                # %land.rhs
                                        #   in Loop: Header=BB52_40 Depth=2
	movq	8(%rdi), %rcx
	movq	8(%rax), %rdx
	movl	60(%rcx), %ecx
	movl	60(%rdx), %edx
	xorq	%rcx, %rdx
	testq	$65024, %rdx            # imm = 0xFE00
	jne	.LBB52_45
# BB#43:                                # %while.body203
                                        #   in Loop: Header=BB52_40 Depth=2
	movq	%rax, 104(%rbx)
	movzbl	16(%rax), %ecx
	addq	$-114, %rcx
	andq	%r14, %rcx
	cmpq	$3, %rcx
	movq	%rax, %rdi
	jb	.LBB52_40
	jmp	.LBB52_44
.LBB52_41:                              #   in Loop: Header=BB52_27 Depth=1
	movq	%rdi, %rax
.LBB52_44:                              # %if.end212
                                        #   in Loop: Header=BB52_27 Depth=1
	testq	%rax, %rax
	movq	%rax, %rdi
	je	.LBB52_52
	.align	16, 0x90
.LBB52_45:                              # %if.then216
                                        #   in Loop: Header=BB52_27 Depth=1
	movzbl	16(%rdi), %eax
	cmpq	$25, %rax
	jne	.LBB52_46
# BB#47:                                # %if.end232
                                        #   in Loop: Header=BB52_27 Depth=1
	callq	constant_expression_warning
	movq	104(%rbx), %rdi
	testq	%rdi, %rdi
	je	.LBB52_52
# BB#48:                                # %land.lhs.true236
                                        #   in Loop: Header=BB52_27 Depth=1
	movq	8(%rbx), %rax
	movzbl	16(%rax), %ecx
	cmpq	$11, %rcx
	ja	.LBB52_50
# BB#49:                                # %land.lhs.true236
                                        #   in Loop: Header=BB52_27 Depth=1
	movq	$1, %rax
	shlq	%cl, %rax
	testq	$3136, %rax             # imm = 0xC40
	je	.LBB52_50
# BB#58:                                # %if.end266
                                        #   in Loop: Header=BB52_27 Depth=1
	movl	pedantic(%rip), %eax
	testq	%rax, %rax
	je	.LBB52_148
# BB#59:                                # %land.lhs.true272
                                        #   in Loop: Header=BB52_27 Depth=1
	movq	8(%rbx), %rax
	movq	128(%rax), %rdx
	movq	integer_types+40(%rip), %rcx
	cmpq	%rcx, %rdx
	je	.LBB52_148
# BB#60:                                # %land.lhs.true272
                                        #   in Loop: Header=BB52_27 Depth=1
	movq	integer_types+48(%rip), %rsi
	cmpq	%rsi, %rdx
	je	.LBB52_148
# BB#61:                                # %land.lhs.true272
                                        #   in Loop: Header=BB52_27 Depth=1
	movq	c_global_trees+152(%rip), %rsi
	cmpq	%rsi, %rdx
	je	.LBB52_148
# BB#62:                                # %land.lhs.true292
                                        #   in Loop: Header=BB52_27 Depth=1
	movzbl	16(%rax), %edx
	cmpq	$10, %rdx
	jne	.LBB52_64
# BB#63:                                # %land.lhs.true301
                                        #   in Loop: Header=BB52_27 Depth=1
	movl	60(%rax), %eax
	movl	60(%rcx), %ecx
	xorq	%rax, %rcx
	testq	$511, %rcx              # imm = 0x1FF
	je	.LBB52_65
.LBB52_64:                              # %if.then313
                                        #   in Loop: Header=BB52_27 Depth=1
	xorq	%rax, %rax
	movq	%rbx, %rdi
	movabsq	$.L.str210, %rsi
	callq	pedwarn_with_decl
	movq	104(%rbx), %rdi
.LBB52_65:                              # %if.end314
                                        #   in Loop: Header=BB52_27 Depth=1
	movq	8(%rbx), %r9
	testq	%rdi, %rdi
	je	.LBB52_53
	jmp	.LBB52_66
	.align	16, 0x90
.LBB52_46:                              # %if.else228
                                        #   in Loop: Header=BB52_27 Depth=1
	xorq	%rax, %rax
	movq	%rbx, %rdi
	movabsq	$.L.str208, %rsi
	jmp	.LBB52_51
.LBB52_50:                              # %if.then263
                                        #   in Loop: Header=BB52_27 Depth=1
	xorq	%rax, %rax
	movq	%rbx, %rdi
	movabsq	$.L.str209, %rsi
.LBB52_51:                              # %if.end314.thread1080
                                        #   in Loop: Header=BB52_27 Depth=1
	callq	error_with_decl
	movq	$0, 104(%rbx)
.LBB52_52:                              # %if.end314.thread1080
                                        #   in Loop: Header=BB52_27 Depth=1
	movq	8(%rbx), %r9
.LBB52_53:                              # %if.else494
                                        #   in Loop: Header=BB52_27 Depth=1
	movq	global_trees(%rip), %rcx
	cmpq	%rcx, %r9
	je	.LBB52_92
# BB#54:                                # %if.then499
                                        #   in Loop: Header=BB52_27 Depth=1
	movq	48(%rbx), %rdx
	movq	%rdx, %rcx
	andq	$1024, %rcx             # imm = 0x400
	movabsq	$0, %r8
	jne	.LBB52_56
# BB#55:                                # %if.then499
                                        #   in Loop: Header=BB52_27 Depth=1
	movabsq	$1, %r8
.LBB52_56:                              # %if.then499
                                        #   in Loop: Header=BB52_27 Depth=1
	testq	%rcx, %rcx
	jne	.LBB52_57
# BB#86:                                # %cond.false508
                                        #   in Loop: Header=BB52_27 Depth=1
	movl	64(%r9), %eax
	jmp	.LBB52_87
	.align	16, 0x90
.LBB52_57:                              #   in Loop: Header=BB52_27 Depth=1
	movq	$8, %rax
	xorq	%r8, %r8
.LBB52_87:                              # %cond.end513
                                        #   in Loop: Header=BB52_27 Depth=1
	movl	56(%rbx), %edi
	movq	%rdi, %rcx
	andq	$16777215, %rcx         # imm = 0xFFFFFF
	movq	%rax, %rsi
	andq	%r14, %rsi
	cmpq	%rsi, %rcx
	movq	%rdi, %rcx
	ja	.LBB52_89
# BB#88:                                # %cond.end513
                                        #   in Loop: Header=BB52_27 Depth=1
	movq	%rax, %rcx
.LBB52_89:                              # %cond.end513
                                        #   in Loop: Header=BB52_27 Depth=1
	andq	$16777215, %rcx         # imm = 0xFFFFFF
	movabsq	$4278190080, %rax       # imm = 0xFF000000
	andq	%rax, %rdi
	orq	%rcx, %rdi
	movl	%edi, 56(%rbx)
	testq	%r8, %r8
	je	.LBB52_92
# BB#90:                                # %if.then545
                                        #   in Loop: Header=BB52_27 Depth=1
	movl	60(%r9), %eax
	movq	$31, %rcx
.LBB52_91:                              # %if.end567
                                        #   in Loop: Header=BB52_27 Depth=1
	shrq	%cl, %rax
	movq	$35, %rcx
	movq	%rdx, %rsi
	shrq	%cl, %rsi
	orq	%rax, %rsi
	movq	$35, %rcx
	shlq	%cl, %rsi
	movabsq	$34359738368, %rax      # imm = 0x800000000
	andq	%rax, %rsi
	movabsq	$-34359738369, %rax     # imm = 0xFFFFFFF7FFFFFFFF
	andq	%rax, %rdx
	orq	%rsi, %rdx
	movq	%rdx, 48(%rbx)
.LBB52_92:                              # %if.end567
                                        #   in Loop: Header=BB52_27 Depth=1
	movq	$0, 104(%rbx)
	movq	8(%rbx), %rax
	movzbl	16(%rax), %ecx
	cmpq	$18, %rcx
	jne	.LBB52_103
# BB#93:                                # %land.lhs.true578
                                        #   in Loop: Header=BB52_27 Depth=1
	cmpq	$0, 32(%rax)
	jne	.LBB52_103
# BB#94:                                # %land.lhs.true585
                                        #   in Loop: Header=BB52_27 Depth=1
	movq	24(%rax), %rax
	testq	%rax, %rax
	je	.LBB52_103
# BB#95:                                # %land.lhs.true591
                                        #   in Loop: Header=BB52_27 Depth=1
	cmpq	$0, 112(%rax)
	jne	.LBB52_103
# BB#96:                                # %if.then600
                                        #   in Loop: Header=BB52_27 Depth=1
	movq	40(%rsp), %rax
	movzbl	16(%rax), %eax
	cmpq	$21, %rax
	jne	.LBB52_98
# BB#97:                                # %if.then607
                                        #   in Loop: Header=BB52_27 Depth=1
	xorq	%rax, %rax
	movq	%rbx, %rdi
	movabsq	$.L.str215, %rsi
	jmp	.LBB52_102
.LBB52_98:                              # %if.else608
                                        #   in Loop: Header=BB52_27 Depth=1
	cmpq	$0, (%rbx)
	jne	.LBB52_99
# BB#100:                               # %if.else614
                                        #   in Loop: Header=BB52_27 Depth=1
	testq	%r14, %r12
	jne	.LBB52_103
# BB#101:                               # %if.then616
                                        #   in Loop: Header=BB52_27 Depth=1
	xorq	%rax, %rax
	movq	%rbx, %rdi
	movabsq	$.L.str217, %rsi
	jmp	.LBB52_102
.LBB52_148:                             # %if.end314.thread
                                        #   in Loop: Header=BB52_27 Depth=1
	movq	8(%rbx), %r9
.LBB52_66:                              # %if.then318
                                        #   in Loop: Header=BB52_27 Depth=1
	movq	128(%r9), %rcx
	movq	c_global_trees+152(%rip), %rdx
	cmpq	%rdx, %rcx
	movq	$8, %r13
	je	.LBB52_68
# BB#67:                                # %cond.false
                                        #   in Loop: Header=BB52_27 Depth=1
	movl	60(%r9), %r13d
	andq	$511, %r13              # imm = 0x1FF
.LBB52_68:                              # %cond.end
                                        #   in Loop: Header=BB52_27 Depth=1
	callq	tree_int_cst_sgn
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jns	.LBB52_70
# BB#69:                                # %if.then337
                                        #   in Loop: Header=BB52_27 Depth=1
	xorq	%rax, %rax
	movq	%rbx, %rdi
	movabsq	$.L.str211, %rsi
	callq	error_with_decl
	jmp	.LBB52_92
.LBB52_99:                              # %if.then613
                                        #   in Loop: Header=BB52_27 Depth=1
	xorq	%rax, %rax
	movq	%rbx, %rdi
	movabsq	$.L.str216, %rsi
.LBB52_102:                             # %if.end620
                                        #   in Loop: Header=BB52_27 Depth=1
	callq	error_with_decl
	.align	16, 0x90
.LBB52_103:                             # %if.end620
                                        #   in Loop: Header=BB52_27 Depth=1
	cmpq	$0, 72(%rbx)
	je	.LBB52_105
# BB#104:                               # %select.mid
                                        #   in Loop: Header=BB52_27 Depth=1
	movq	$1, %r12
.LBB52_105:                             # %select.end
                                        #   in Loop: Header=BB52_27 Depth=1
	movq	(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB52_27
# BB#106:                               # %for.end629
	movq	32(%rsp), %rbx
	xorq	%r14, %r14
	testq	%rbx, %rbx
	je	.LBB52_118
# BB#107:
	leaq	32(%rsp), %r15
	movabsq	$.L.str218, %r14
	jmp	.LBB52_108
	.align	16, 0x90
.LBB52_111:                             #   in Loop: Header=BB52_108 Depth=1
	movq	%rdi, %rbx
.LBB52_108:                             # %land.rhs632.lr.ph
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB52_109 Depth 2
                                        #       Child Loop BB52_112 Depth 3
	movq	(%rbx), %rdi
	jmp	.LBB52_109
	.align	16, 0x90
.LBB52_116:                             # %if.then675
                                        #   in Loop: Header=BB52_109 Depth=2
	xorq	%rax, %rax
	movq	%r14, %rsi
	callq	error_with_decl
	movq	(%rbx), %rax
	movq	(%rax), %rdi
	movq	%rdi, (%rbx)
.LBB52_109:                             # %land.rhs632
                                        #   Parent Loop BB52_108 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB52_112 Depth 3
	testq	%rdi, %rdi
	je	.LBB52_117
# BB#110:                               # %for.body637
                                        #   in Loop: Header=BB52_109 Depth=2
	movq	72(%rdi), %rax
	testq	%rax, %rax
	movq	%r15, %rcx
	je	.LBB52_111
	.align	16, 0x90
.LBB52_112:                             # %while.body649
                                        #   Parent Loop BB52_108 Depth=1
                                        #     Parent Loop BB52_109 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movq	(%rcx), %rcx
	movq	72(%rcx), %rdx
	cmpq	%rax, %rdx
	je	.LBB52_114
# BB#113:                               # %while.body649
                                        #   in Loop: Header=BB52_112 Depth=3
	cmpq	%rbx, %rcx
	jne	.LBB52_112
.LBB52_114:                             # %while.end666
                                        #   in Loop: Header=BB52_109 Depth=2
	cmpq	%rax, %rdx
	je	.LBB52_116
# BB#115:                               #   in Loop: Header=BB52_108 Depth=1
	movq	%rdi, %rbx
	jmp	.LBB52_108
.LBB52_117:                             # %for.end689thread-pre-split
	movq	32(%rsp), %r14
.LBB52_118:                             # %for.end689
	movq	40(%rsp), %rax
	movq	%r14, 24(%rax)
	movq	40(%rsp), %rdi
	callq	layout_type
	testq	%r14, %r14
	je	.LBB52_125
# BB#119:
	leaq	32(%rsp), %rax
.LBB52_120:                             # %while.body694.lr.ph
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB52_121 Depth 2
	movq	%r14, %rcx
	.align	16, 0x90
.LBB52_121:                             # %while.body694
                                        #   Parent Loop BB52_120 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movzbl	16(%rcx), %edx
	cmpq	$37, %rdx
	jne	.LBB52_124
# BB#122:                               # %land.lhs.true701
                                        #   in Loop: Header=BB52_121 Depth=2
	cmpq	$0, 104(%rcx)
	je	.LBB52_124
# BB#123:                               # %if.then705
                                        #   in Loop: Header=BB52_121 Depth=2
	movq	(%rcx), %rcx
	testq	%rcx, %rcx
	movq	%rcx, (%rax)
	jne	.LBB52_121
	jmp	.LBB52_125
	.align	16, 0x90
.LBB52_124:                             # %if.else708
                                        #   in Loop: Header=BB52_120 Depth=1
	movq	(%rcx), %r14
	testq	%r14, %r14
	movq	%rcx, %rax
	jne	.LBB52_120
.LBB52_125:                             # %while.end712
	movq	32(%rsp), %rax
	movq	40(%rsp), %rcx
	movq	%rax, 24(%rcx)
	movq	40(%rsp), %rax
	movq	128(%rax), %rcx
	testq	%rcx, %rcx
	je	.LBB52_126
# BB#127:
	movabsq	$2147483648, %rdx       # imm = 0x80000000
	movq	8(%rsp), %r15           # 8-byte Reload
	.align	16, 0x90
.LBB52_128:                             # %for.body719
                                        # =>This Inner Loop Header: Depth=1
	movq	24(%rax), %rax
	movq	%rax, 24(%rcx)
	movq	40(%rsp), %rax
	movq	160(%rax), %rax
	movq	%rax, 160(%rcx)
	movq	40(%rsp), %rax
	movl	64(%rax), %esi
	movl	%esi, 64(%rcx)
	movl	60(%rax), %eax
	movl	60(%rcx), %esi
	andq	%rdx, %rax
	andq	$2147483647, %rsi       # imm = 0x7FFFFFFF
	orq	%rax, %rsi
	movl	%esi, 60(%rcx)
	movq	120(%rcx), %rcx
	movq	40(%rsp), %rax
	testq	%rcx, %rcx
	jne	.LBB52_128
	jmp	.LBB52_129
.LBB52_126:
	movq	8(%rsp), %r15           # 8-byte Reload
.LBB52_129:                             # %for.end744
	movzbl	16(%rax), %ecx
	cmpq	$21, %rcx
	jne	.LBB52_133
# BB#130:                               # %land.lhs.true751
	movl	60(%rax), %edx
	testq	$524288, %rdx           # imm = 0x80000
	je	.LBB52_133
# BB#131:                               # %land.lhs.true757
	movq	24(%rax), %rcx
	movzbl	48(%rcx), %esi
	movq	$9, %rcx
	movq	%rdx, %rdi
	shrq	%cl, %rdi
	andq	$127, %rdi
	cmpq	%rsi, %rdi
	je	.LBB52_133
# BB#132:                               # %if.then772
	movabsq	$4294443007, %rcx       # imm = 0xFFF7FFFF
	andq	%rcx, %rdx
	movl	%edx, 60(%rax)
	movabsq	$.L.str219, %rdi
	xorq	%rax, %rax
	callq	warning
.LBB52_133:                             # %if.end777
	movq	current_binding_level(%rip), %rax
	movl	56(%rax), %ecx
	testq	%rcx, %rcx
	je	.LBB52_147
# BB#134:                               # %if.then780
	movq	(%rax), %rbx
	testq	%rbx, %rbx
	je	.LBB52_147
# BB#135:
	movabsq	$4294967295, %r14       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB52_136:                             # %for.body784
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB52_140 Depth 2
	movq	8(%rbx), %rdi
	movq	40(%rsp), %rax
	movq	128(%rdi), %rcx
	movq	128(%rax), %rdx
	cmpq	%rdx, %rcx
	jne	.LBB52_138
# BB#137:                               # %land.lhs.true793
                                        #   in Loop: Header=BB52_136 Depth=1
	movzbl	16(%rbx), %ecx
	cmpq	$33, %rcx
	jne	.LBB52_143
.LBB52_138:                             # %if.else809
                                        #   in Loop: Header=BB52_136 Depth=1
	cmpq	$0, 32(%rdi)
	jne	.LBB52_146
# BB#139:                               # %land.lhs.true816
                                        #   in Loop: Header=BB52_136 Depth=1
	movzbl	16(%rdi), %ecx
	cmpq	$18, %rcx
	movq	%rdi, %rcx
	jne	.LBB52_146
	.align	16, 0x90
.LBB52_140:                             # %while.body835
                                        #   Parent Loop BB52_136 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	8(%rcx), %rcx
	movzbl	16(%rcx), %edx
	cmpq	$18, %rdx
	je	.LBB52_140
# BB#141:                               # %while.end838
                                        #   in Loop: Header=BB52_136 Depth=1
	cmpq	%rax, %rcx
	jne	.LBB52_146
# BB#142:                               # %if.then841
                                        #   in Loop: Header=BB52_136 Depth=1
	callq	layout_array_type
	movzbl	16(%rbx), %eax
	cmpq	$33, %rax
	je	.LBB52_145
	.align	16, 0x90
.LBB52_143:                             # %if.then850
                                        #   in Loop: Header=BB52_136 Depth=1
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	layout_decl
	movq	%rbx, %rdi
	callq	maybe_objc_check_decl
	xorq	%rsi, %rsi
	xorq	%rcx, %rcx
	movq	%rbx, %rdi
	movq	24(%rsp), %rdx          # 8-byte Reload
	callq	rest_of_decl_compilation
	movq	16(%rsp), %rax          # 8-byte Reload
	cmpq	%rax, %r15
	je	.LBB52_145
# BB#144:                               # %if.then852
                                        #   in Loop: Header=BB52_136 Depth=1
	movq	%rbx, %rdi
	callq	expand_decl
.LBB52_145:                             # %if.end854
                                        #   in Loop: Header=BB52_136 Depth=1
	movq	current_binding_level(%rip), %rax
	movl	56(%rax), %ecx
	decq	%rcx
	testq	%r14, %rcx
	movl	%ecx, 56(%rax)
	je	.LBB52_147
.LBB52_146:                             # %for.inc864
                                        #   in Loop: Header=BB52_136 Depth=1
	movq	(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB52_136
.LBB52_147:                             # %if.end868
	movq	40(%rsp), %rdi
	movq	24(%rsp), %rsi          # 8-byte Reload
	callq	rest_of_type_compilation
	movq	40(%rsp), %rax
	addq	$48, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp482:
	.size	finish_struct, .Ltmp482-finish_struct
	.cfi_endproc

	.align	16, 0x90
	.type	layout_array_type,@function
layout_array_type:                      # @layout_array_type
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp485:
	.cfi_def_cfa_offset 16
.Ltmp486:
	.cfi_offset %rbx, -16
	movq	%rdi, %rbx
	movq	8(%rbx), %rdi
	movzbl	16(%rdi), %eax
	cmpq	$18, %rax
	jne	.LBB53_2
# BB#1:                                 # %if.then
	callq	layout_array_type
.LBB53_2:                               # %if.end
	movq	%rbx, %rdi
	popq	%rbx
	jmp	layout_type  # TAILCALL
.Ltmp487:
	.size	layout_array_type, .Ltmp487-layout_array_type
	.cfi_endproc

	.globl	start_enum
	.align	16, 0x90
	.type	start_enum,@function
start_enum:                             # @start_enum
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp492:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp493:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp494:
	.cfi_def_cfa_offset 32
.Ltmp495:
	.cfi_offset %rbx, -32
.Ltmp496:
	.cfi_offset %r14, -24
.Ltmp497:
	.cfi_offset %r15, -16
	movq	%rdi, %r14
	testq	%r14, %r14
	je	.LBB54_11
# BB#1:                                 # %if.then
	movq	current_binding_level(%rip), %rax
	jmp	.LBB54_2
	.align	16, 0x90
.LBB54_23:                              # %for.inc21.i
                                        #   in Loop: Header=BB54_2 Depth=1
	movq	40(%rax), %rax
.LBB54_2:                               # %if.then
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB54_5 Depth 2
	testq	%rax, %rax
	je	.LBB54_11
# BB#3:                                 # %for.body.i
                                        #   in Loop: Header=BB54_2 Depth=1
	movq	8(%rax), %rbx
	jmp	.LBB54_5
	.align	16, 0x90
.LBB54_4:                               # %for.cond1.i
                                        #   in Loop: Header=BB54_5 Depth=2
	movq	(%rbx), %rbx
.LBB54_5:                               # %for.cond1.i
                                        #   Parent Loop BB54_2 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	testq	%rbx, %rbx
	je	.LBB54_22
# BB#6:                                 # %for.body3.i
                                        #   in Loop: Header=BB54_5 Depth=2
	movq	24(%rbx), %rcx
	cmpq	%r14, %rcx
	jne	.LBB54_4
	jmp	.LBB54_7
	.align	16, 0x90
.LBB54_22:                              # %for.end.i
                                        #   in Loop: Header=BB54_2 Depth=1
	movzbl	49(%rax), %ecx
	testq	%rcx, %rcx
	jne	.LBB54_23
	jmp	.LBB54_11
.LBB54_7:                               # %if.then.i
	movq	32(%rbx), %r15
	movzbl	16(%r15), %eax
	cmpq	$10, %rax
	je	.LBB54_9
# BB#8:                                 # %pending_xref_error.exit.i
	movq	input_filename(%rip), %rdi
	movl	lineno(%rip), %esi
	movq	%r14, pending_invalid_xref(%rip)
	movq	%rdi, pending_invalid_xref_file(%rip)
	movl	%esi, pending_invalid_xref_line(%rip)
	movq	32(%r14), %rcx
	movabsq	$.L.str155, %rdx
	xorq	%rax, %rax
	callq	error_with_file_and_line
	movq	$0, pending_invalid_xref(%rip)
	movq	32(%rbx), %r15
.LBB54_9:                               # %if.end
	testq	%r15, %r15
	je	.LBB54_11
# BB#10:                                # %lor.lhs.false
	movzbl	16(%r15), %eax
	cmpq	$10, %rax
	je	.LBB54_17
.LBB54_11:                              # %if.then3
	movq	$10, %rdi
	callq	make_node
	movq	%rax, %r15
	movabsq	$current_binding_level, %rax
	.align	16, 0x90
.LBB54_12:                              # %for.cond.i
                                        # =>This Inner Loop Header: Depth=1
	movq	(%rax), %rbx
	movzbl	49(%rbx), %ecx
	leaq	40(%rbx), %rax
	testq	%rcx, %rcx
	jne	.LBB54_12
# BB#13:                                # %for.end.i33
	testq	%r14, %r14
	je	.LBB54_16
# BB#14:                                # %if.then.i35
	cmpq	$0, 96(%r15)
	jne	.LBB54_16
# BB#15:                                # %if.then4.i
	movq	%r14, 96(%r15)
.LBB54_16:                              # %pushtag.exit
	movq	8(%rbx), %rdx
	movq	%r14, %rdi
	movq	%r15, %rsi
	callq	tree_cons
	movq	%rax, 8(%rbx)
	movq	$33, %rdi
	xorq	%rsi, %rsi
	movq	%r15, %rdx
	callq	build_decl
	movq	%rax, %rdi
	callq	pushdecl
	movq	%rax, (%r15)
	movq	80(%rax), %rax
	movq	%rax, 144(%r15)
.LBB54_17:                              # %if.end5
	movl	60(%r15), %eax
	orq	$16777216, %rax         # imm = 0x1000000
	movl	%eax, 60(%r15)
	cmpq	$0, 24(%r15)
	je	.LBB54_19
# BB#18:                                # %if.then10
	movq	32(%r14), %rsi
	movabsq	$.L.str220, %rdi
	xorq	%rax, %rax
	callq	error
	movq	$0, 24(%r15)
.LBB54_19:                              # %if.end13
	movq	global_trees+88(%rip), %rax
	movl	flag_short_enums(%rip), %ecx
	movq	%rax, enum_next_value(%rip)
	xorq	%rax, %rax
	movl	%eax, enum_overflow(%rip)
	testq	%rcx, %rcx
	je	.LBB54_21
# BB#20:                                # %if.then14
	movl	60(%r15), %eax
	orq	$1048576, %rax          # imm = 0x100000
	movl	%eax, 60(%r15)
.LBB54_21:                              # %if.end19
	movq	%r15, %rax
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp498:
	.size	start_enum, .Ltmp498-start_enum
	.cfi_endproc

	.globl	finish_enum
	.align	16, 0x90
	.type	finish_enum,@function
finish_enum:                            # @finish_enum
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp505:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp506:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp507:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp508:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp509:
	.cfi_def_cfa_offset 48
	subq	$32, %rsp
.Ltmp510:
	.cfi_def_cfa_offset 80
.Ltmp511:
	.cfi_offset %rbx, -48
.Ltmp512:
	.cfi_offset %r12, -40
.Ltmp513:
	.cfi_offset %r13, -32
.Ltmp514:
	.cfi_offset %r14, -24
.Ltmp515:
	.cfi_offset %r15, -16
	movq	global_binding_level(%rip), %rcx
	movq	current_binding_level(%rip), %rax
	movq	%rdx, %rbx
	movq	%rsi, %r14
	movq	%rdi, 24(%rsp)
	cmpq	%rax, %rcx
	jne	.LBB55_1
# BB#2:                                 # %entry
	movabsq	$1, %rcx
	jmp	.LBB55_3
.LBB55_1:
	movabsq	$0, %rcx
.LBB55_3:                               # %entry
	movq	%rcx, 8(%rsp)           # 8-byte Spill
	movzbl	48(%rax), %eax
	testq	%rax, %rax
	je	.LBB55_5
# BB#4:                                 # %if.then
	movabsq	$.L.str221, %rdi
	xorq	%rax, %rax
	callq	warning
.LBB55_5:                               # %if.end
	leaq	24(%rsp), %rdi
	movq	$8, %rdx
	movq	%rbx, %rsi
	callq	decl_attributes
	movq	global_trees(%rip), %rax
	cmpq	%r14, %rax
	je	.LBB55_6
# BB#7:                                 # %if.else
	movq	(%r14), %rbx
	movq	32(%r14), %r13
	testq	%rbx, %rbx
	je	.LBB55_8
# BB#9:
	movq	%r14, 16(%rsp)          # 8-byte Spill
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	movq	%r13, %r12
	.align	16, 0x90
.LBB55_10:                              # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	32(%rbx), %r14
	movq	%r12, %rdi
	movq	%r14, %rsi
	callq	tree_int_cst_lt
	testq	%r15, %rax
	je	.LBB55_12
# BB#11:                                # %for.body
                                        #   in Loop: Header=BB55_10 Depth=1
	movq	%r14, %r12
.LBB55_12:                              # %for.body
                                        #   in Loop: Header=BB55_10 Depth=1
	movq	%r14, %rdi
	movq	%r13, %rsi
	callq	tree_int_cst_lt
	testq	%r15, %rax
	je	.LBB55_14
# BB#13:                                # %for.body
                                        #   in Loop: Header=BB55_10 Depth=1
	movq	%r14, %r13
.LBB55_14:                              # %for.body
                                        #   in Loop: Header=BB55_10 Depth=1
	movq	(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB55_10
	jmp	.LBB55_15
.LBB55_6:                               # %if.then4
	movq	%r14, 16(%rsp)          # 8-byte Spill
	movq	global_trees+88(%rip), %r12
	movq	%r12, %r13
	jmp	.LBB55_15
.LBB55_8:
	movq	%r14, 16(%rsp)          # 8-byte Spill
	movq	%r13, %r12
.LBB55_15:                              # %if.end19
	movq	%r13, %rdi
	callq	tree_int_cst_sgn
	movq	%rax, %rbx
	movabsq	$2147483648, %rax       # imm = 0x80000000
	andq	%rax, %rbx
	movq	$31, %rcx
	shrq	%cl, %rbx
	xorq	$1, %rbx
	movq	%r13, %rdi
	movq	%rbx, %rsi
	callq	min_precision
	movq	%rax, %r15
	movabsq	$4294967295, %r14       # imm = 0xFFFFFFFF
	andq	%r14, %r15
	movq	%r12, %rdi
	movq	%rbx, %rsi
	callq	min_precision
	andq	%r14, %rax
	cmpq	%rax, %r15
	jbe	.LBB55_17
# BB#16:                                # %cond.true
	movq	%r13, %rdi
	jmp	.LBB55_18
.LBB55_17:                              # %cond.false
	movq	%r12, %rdi
.LBB55_18:                              # %cond.end
	movq	%rbx, %rsi
	callq	min_precision
	movq	24(%rsp), %rcx
	movzbl	62(%rcx), %ecx
	testq	$16, %rcx
	jne	.LBB55_20
# BB#19:                                # %lor.lhs.false
	movq	integer_types+40(%rip), %rcx
	movl	60(%rcx), %edx
	movq	%rdx, %r15
	andq	$511, %r15              # imm = 0x1FF
	movq	$32, %rcx
	movq	%rax, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	cmpq	%r15, %rsi
	jle	.LBB55_23
.LBB55_20:                              # %if.then36
	movq	%rax, %rdi
	movq	%rbx, %rsi
	callq	type_for_size
	testq	%rax, %rax
	jne	.LBB55_22
# BB#21:                                # %if.then40
	movabsq	$.L.str222, %rdi
	xorq	%rax, %rax
	callq	warning
	movq	integer_types+72(%rip), %rax
.LBB55_22:                              # %if.end41
	movq	integer_types+40(%rip), %rcx
	movl	60(%rax), %r15d
	movl	60(%rcx), %edx
	andq	$511, %r15              # imm = 0x1FF
.LBB55_23:                              # %if.end51
	andq	$511, %rdx              # imm = 0x1FF
	movq	%r15, %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	cmpq	%rdx, %rax
	jne	.LBB55_25
# BB#24:                                # %if.then58
	xorq	%rsi, %rsi
	movq	%r15, %rdi
	callq	type_for_size
	movq	24(%rsp), %rcx
	movq	%rax, %r14
	jmp	.LBB55_26
.LBB55_25:                              # %if.else60
	movq	24(%rsp), %rcx
	movq	%rcx, %r14
.LBB55_26:                              # %if.end61
	movq	%r13, 104(%rcx)
	movq	24(%rsp), %rax
	movq	%r12, 112(%rax)
	movq	24(%rsp), %rax
	movl	60(%rax), %ecx
	movabsq	$4294966784, %rdx       # imm = 0xFFFFFE00
	andq	%rcx, %rdx
	orq	%r15, %rdx
	movl	%edx, 60(%rax)
	movq	24(%rsp), %rax
	movl	16(%rax), %edx
	movq	$13, %rcx
	shlq	%cl, %rbx
	movabsq	$4294959103, %r15       # imm = 0xFFFFDFFF
	andq	%r15, %rdx
	orq	%rbx, %rdx
	movl	%edx, 16(%rax)
	movq	24(%rsp), %rax
	movq	$0, 32(%rax)
	movq	24(%rsp), %rdi
	callq	layout_type
	movq	global_trees(%rip), %rax
	movq	16(%rsp), %rcx          # 8-byte Reload
	cmpq	%rcx, %rax
	je	.LBB55_34
# BB#27:                                # %for.cond77.preheader
	testq	%rcx, %rcx
	je	.LBB55_33
# BB#28:
	movabsq	$34359738368, %r13      # imm = 0x800000000
	movabsq	$-34359738369, %rbx     # imm = 0xFFFFFFF7FFFFFFFF
	movq	16(%rsp), %r15          # 8-byte Reload
	.align	16, 0x90
.LBB55_29:                              # %for.body79
                                        # =>This Inner Loop Header: Depth=1
	movq	24(%r15), %r12
	movq	24(%rsp), %rax
	movq	%rax, 8(%r12)
	movq	24(%rsp), %rax
	movq	32(%rax), %rax
	movq	%rax, 40(%r12)
	movq	24(%rsp), %rax
	movq	40(%rax), %rax
	movq	%rax, 64(%r12)
	movq	24(%rsp), %rax
	movl	64(%rax), %eax
	movl	56(%r12), %ecx
	andq	$16777215, %rax         # imm = 0xFFFFFF
	movabsq	$4278190080, %rdx       # imm = 0xFF000000
	andq	%rdx, %rcx
	orq	%rax, %rcx
	movl	%ecx, 56(%r12)
	movq	24(%rsp), %rax
	movl	60(%rax), %eax
	movq	48(%r12), %rdx
	movq	$4, %rcx
	shlq	%cl, %rax
	andq	%r13, %rax
	andq	%rbx, %rdx
	orq	%rax, %rdx
	movq	%rdx, 48(%r12)
	movq	24(%rsp), %rax
	movl	60(%rax), %eax
	movq	$9, %rcx
	shrq	%cl, %rax
	andq	$127, %rax
	andq	$-256, %rdx
	orq	%rax, %rdx
	movq	%rdx, 48(%r12)
	movq	104(%r12), %rdi
	movq	%r14, %rsi
	callq	int_fits_type_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB55_30
# BB#31:                                # %if.else125
                                        #   in Loop: Header=BB55_29 Depth=1
	movq	24(%rsp), %rdi
	movq	104(%r12), %rsi
	jmp	.LBB55_32
	.align	16, 0x90
.LBB55_30:                              # %if.then119
                                        #   in Loop: Header=BB55_29 Depth=1
	movq	104(%r12), %rsi
	movq	%r14, %rdi
.LBB55_32:                              # %if.end131
                                        #   in Loop: Header=BB55_29 Depth=1
	callq	convert
	movq	%rax, 104(%r12)
	movq	72(%r12), %rcx
	movq	%rcx, 24(%r15)
	movq	%rax, 32(%r15)
	movq	(%r15), %r15
	testq	%r15, %r15
	jne	.LBB55_29
.LBB55_33:                              # %for.end142
	movq	24(%rsp), %rax
	movq	16(%rsp), %rcx          # 8-byte Reload
	movq	%rcx, 24(%rax)
	movabsq	$4294959103, %r15       # imm = 0xFFFFDFFF
.LBB55_34:                              # %if.end145
	movq	24(%rsp), %rdi
	movq	128(%rdi), %rax
	testq	%rax, %rax
	je	.LBB55_39
# BB#35:
	movabsq	$2147483648, %rbx       # imm = 0x80000000
	.align	16, 0x90
.LBB55_36:                              # %for.body149
                                        # =>This Inner Loop Header: Depth=1
	cmpq	%rdi, %rax
	je	.LBB55_38
# BB#37:                                # %if.end153
                                        #   in Loop: Header=BB55_36 Depth=1
	movq	24(%rdi), %rcx
	movq	%rcx, 24(%rax)
	movq	24(%rsp), %rcx
	movq	104(%rcx), %rcx
	movq	%rcx, 104(%rax)
	movq	24(%rsp), %rcx
	movq	112(%rcx), %rcx
	movq	%rcx, 112(%rax)
	movq	24(%rsp), %rcx
	movq	32(%rcx), %rcx
	movq	%rcx, 32(%rax)
	movq	24(%rsp), %rcx
	movq	40(%rcx), %rcx
	movq	%rcx, 40(%rax)
	movq	24(%rsp), %rcx
	movl	60(%rcx), %ecx
	movl	60(%rax), %edx
	andq	$65024, %rcx            # imm = 0xFE00
	andq	$-65025, %rdx           # imm = 0xFFFFFFFFFFFF01FF
	orq	%rcx, %rdx
	movl	%edx, 60(%rax)
	movq	24(%rsp), %rcx
	movl	60(%rcx), %ecx
	andq	$511, %rcx              # imm = 0x1FF
	andq	$-512, %rdx             # imm = 0xFFFFFFFFFFFFFE00
	orq	%rcx, %rdx
	movl	%edx, 60(%rax)
	movq	24(%rsp), %rcx
	movl	64(%rcx), %esi
	movl	%esi, 64(%rax)
	movl	60(%rcx), %ecx
	andq	%rbx, %rcx
	andq	$2147483647, %rdx       # imm = 0x7FFFFFFF
	orq	%rcx, %rdx
	movl	%edx, 60(%rax)
	movq	24(%rsp), %rcx
	movl	16(%rcx), %ecx
	movl	16(%rax), %edx
	andq	$8192, %rcx             # imm = 0x2000
	andq	%r15, %rdx
	orq	%rcx, %rdx
	movl	%edx, 16(%rax)
	movq	24(%rsp), %rdi
.LBB55_38:                              # %for.inc223
                                        #   in Loop: Header=BB55_36 Depth=1
	movq	120(%rax), %rax
	testq	%rax, %rax
	jne	.LBB55_36
.LBB55_39:                              # %for.end225
	movq	8(%rsp), %rsi           # 8-byte Reload
	callq	rest_of_type_compilation
	movq	24(%rsp), %rax
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp516:
	.size	finish_enum, .Ltmp516-finish_enum
	.cfi_endproc

	.globl	build_enumerator
	.align	16, 0x90
	.type	build_enumerator,@function
build_enumerator:                       # @build_enumerator
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp522:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp523:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp524:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp525:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp526:
	.cfi_def_cfa_offset 48
.Ltmp527:
	.cfi_offset %rbx, -40
.Ltmp528:
	.cfi_offset %r12, -32
.Ltmp529:
	.cfi_offset %r14, -24
.Ltmp530:
	.cfi_offset %r15, -16
	movq	%rdi, %r14
	testq	%rsi, %rsi
	je	.LBB56_9
# BB#1:                                 # %while.cond.preheader
	movq	global_trees(%rip), %rcx
	movabsq	$4294967295, %r8        # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB56_2:                               # %while.cond
                                        # =>This Inner Loop Header: Depth=1
	movq	%rsi, %rdi
	movzbl	16(%rdi), %eax
	leaq	-114(%rax), %rsi
	andq	%r8, %rsi
	cmpq	$2, %rsi
	ja	.LBB56_5
# BB#3:                                 # %land.lhs.true
                                        #   in Loop: Header=BB56_2 Depth=1
	movq	32(%rdi), %rsi
	cmpq	%rcx, %rsi
	je	.LBB56_6
# BB#4:                                 # %land.rhs
                                        #   in Loop: Header=BB56_2 Depth=1
	movq	8(%rdi), %rbx
	movq	8(%rsi), %rdx
	cmpq	%rdx, %rbx
	je	.LBB56_2
	jmp	.LBB56_6
.LBB56_5:                               # %if.end
	testq	%rdi, %rdi
	je	.LBB56_9
.LBB56_6:                               # %if.then25
	cmpq	$25, %rax
	jne	.LBB56_7
# BB#8:                                 # %if.end33
	callq	default_conversion
	movq	%rax, %r15
	movq	%r15, %rdi
	callq	constant_expression_warning
	testq	%r15, %r15
	jne	.LBB56_11
	jmp	.LBB56_9
.LBB56_7:                               # %if.else
	movq	32(%r14), %rsi
	movabsq	$.L.str223, %rdi
	xorq	%rax, %rax
	callq	error
.LBB56_9:                               # %if.then35
	movl	enum_overflow(%rip), %eax
	movq	enum_next_value(%rip), %r15
	testq	%rax, %rax
	je	.LBB56_11
# BB#10:                                # %if.then37
	movabsq	$.L.str224, %rdi
	xorq	%rax, %rax
	callq	error
.LBB56_11:                              # %if.end39
	movl	pedantic(%rip), %eax
	testq	%rax, %rax
	je	.LBB56_14
# BB#12:                                # %land.lhs.true41
	movq	integer_types+40(%rip), %rsi
	movq	%r15, %rdi
	callq	int_fits_type_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB56_14
# BB#13:                                # %if.then44
	movabsq	$.L.str225, %rdi
	xorq	%rax, %rax
	callq	pedwarn
	movq	integer_types+40(%rip), %rdi
	movq	%r15, %rsi
	callq	convert
	movq	%rax, %r15
.LBB56_14:                              # %if.end46
	movq	global_trees+96(%rip), %rdx
	movq	$59, %rdi
	xorq	%rcx, %rcx
	xorq	%rbx, %rbx
	movq	%r15, %rsi
	callq	build_binary_op
	movq	%rax, enum_next_value(%rip)
	movq	%rax, %rdi
	movq	%r15, %rsi
	callq	tree_int_cst_lt
	movl	%eax, enum_overflow(%rip)
	movq	8(%r15), %rax
	movq	integer_types+40(%rip), %rcx
	movl	60(%rax), %edi
	movl	60(%rcx), %esi
	movq	%rdi, %rcx
	andq	$511, %rcx              # imm = 0x1FF
	movq	%rsi, %rdx
	andq	$511, %rdx              # imm = 0x1FF
	cmpq	%rdx, %rcx
	ja	.LBB56_16
# BB#15:                                # %if.end46
	movq	%rsi, %rdi
.LBB56_16:                              # %if.end46
	movl	flag_traditional(%rip), %esi
	andq	$511, %rdi              # imm = 0x1FF
	testq	%rsi, %rsi
	jne	.LBB56_18
# BB#17:                                # %if.end46
	movabsq	$4294967295, %rsi       # imm = 0xFFFFFFFF
	andq	%rsi, %rdx
	andq	%rsi, %rcx
	cmpq	%rdx, %rcx
	jb	.LBB56_19
.LBB56_18:                              # %land.rhs78
	movzbl	17(%rax), %ebx
	andq	$32, %rbx
	movq	$5, %rcx
	shrq	%cl, %rbx
.LBB56_19:                              # %land.end83
	movq	%rbx, %rsi
	callq	type_for_size
	movq	%rax, %r12
	movq	$32, %rdi
	movq	%r14, %rsi
	movq	%r12, %rdx
	callq	build_decl
	movq	%rax, %rbx
	movq	%r12, %rdi
	movq	%r15, %rsi
	callq	convert
	movq	%rax, 104(%rbx)
	movq	%rbx, %rdi
	callq	pushdecl
	xorq	%rdx, %rdx
	movq	%rbx, %rdi
	movq	%r15, %rsi
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	jmp	tree_cons  # TAILCALL
.Ltmp531:
	.size	build_enumerator, .Ltmp531-build_enumerator
	.cfi_endproc

	.globl	start_function
	.align	16, 0x90
	.type	start_function,@function
start_function:                         # @start_function
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp538:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp539:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp540:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp541:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp542:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp543:
	.cfi_def_cfa_offset 64
.Ltmp544:
	.cfi_offset %rbx, -48
.Ltmp545:
	.cfi_offset %r12, -40
.Ltmp546:
	.cfi_offset %r13, -32
.Ltmp547:
	.cfi_offset %r14, -24
.Ltmp548:
	.cfi_offset %r15, -16
	movq	%rdx, %r14
	movq	%rdi, %rax
	movl	immediate_size_expand(%rip), %r12d
	xorq	%rbx, %rbx
	movl	%ebx, current_function_returns_value(%rip)
	movl	%ebx, current_function_returns_null(%rip)
	movl	%ebx, current_function_returns_abnormally(%rip)
	movl	%ebx, warn_about_return_type(%rip)
	movl	%ebx, current_extern_inline(%rip)
	movb	%bl, c_function_varargs(%rip)
	movq	$0, named_labels(%rip)
	movq	$0, shadowed_labels(%rip)
	movl	%ebx, immediate_size_expand(%rip)
	movq	$1, %rdx
	movq	$1, %rcx
	movq	%rsi, %rdi
	movq	%rax, %rsi
	callq	grokdeclarator
	testq	%rax, %rax
	movq	%rax, 8(%rsp)
	je	.LBB57_1
# BB#2:                                 # %if.end
	leaq	8(%rsp), %rdi
	xorq	%rdx, %rdx
	movq	%r14, %rsi
	callq	decl_attributes
	movq	current_binding_level(%rip), %rax
	movq	global_binding_level(%rip), %rcx
	cmpq	%rcx, %rax
	jne	.LBB57_4
# BB#3:                                 # %if.then3
	movq	8(%rsp), %rdi
	callq	maybe_apply_pragma_weak
.LBB57_4:                               # %if.end4
	movq	8(%rsp), %rax
	movq	200(%rax), %rcx
	movzbl	(%rcx), %ecx
	testq	$1, %rcx
	je	.LBB57_8
# BB#5:                                 # %land.lhs.true
	movzbl	52(%rax), %ecx
	testq	$16, %rcx
	je	.LBB57_8
# BB#6:                                 # %land.lhs.true10
	movq	136(%rax), %rsi
	movabsq	$.L.str177, %rdi
	callq	lookup_attribute
	testq	%rax, %rax
	je	.LBB57_8
# BB#7:                                 # %if.then15
	movq	8(%rsp), %rdi
	movabsq	$.L.str178, %rsi
	xorq	%rax, %rax
	callq	warning_with_decl
.LBB57_8:                               # %if.end16
	movq	8(%rsp), %rdi
	callq	announce_function
	movq	8(%rsp), %rax
	movq	8(%rax), %rax
	movq	8(%rax), %rax
	cmpq	$0, 32(%rax)
	jne	.LBB57_11
# BB#9:                                 # %lor.lhs.false
	movzbl	16(%rax), %eax
	cmpq	$5, %rax
	je	.LBB57_11
# BB#10:                                # %if.then29
	movabsq	$.L.str226, %rdi
	xorq	%rax, %rax
	callq	error
	movq	8(%rsp), %rax
	movq	8(%rax), %rax
	movq	global_trees+216(%rip), %rdi
	movq	24(%rax), %rsi
	callq	build_function_type
	movq	8(%rsp), %rcx
	movq	%rax, 8(%rcx)
.LBB57_11:                              # %if.end36
	movl	warn_about_return_type(%rip), %eax
	testq	%rax, %rax
	je	.LBB57_13
# BB#12:                                # %if.then38
	movabsq	$.L.str227, %rdi
	xorq	%rax, %rax
	callq	pedwarn_c99
.LBB57_13:                              # %if.end39
	movq	last_function_parms(%rip), %rax
	movq	last_function_parm_tags(%rip), %rcx
	movq	global_trees(%rip), %rdx
	movq	8(%rsp), %rsi
	movq	%rax, current_function_parms(%rip)
	movq	%rcx, current_function_parm_tags(%rip)
	movq	%rdx, 104(%rsi)
	movq	8(%rsp), %rdx
	movq	current_binding_level(%rip), %rax
	movq	global_binding_level(%rip), %rcx
	movq	72(%rdx), %rsi
	cmpq	%rcx, %rax
	movabsq	$1099511627776, %r8     # imm = 0x10000000000
	je	.LBB57_18
# BB#14:                                # %if.end.i
	cmpq	$0, 64(%rsi)
	je	.LBB57_16
.LBB57_15:                              # %if.end3.i
                                        # =>This Inner Loop Header: Depth=1
	movq	(%rax), %rax
	testq	%rax, %rax
	je	.LBB57_16
# BB#17:                                # %for.body.i
                                        #   in Loop: Header=BB57_15 Depth=1
	movq	72(%rax), %rdi
	cmpq	%rsi, %rdi
	jne	.LBB57_15
	jmp	.LBB57_19
.LBB57_1:                               # %if.then
	movl	%r12d, immediate_size_expand(%rip)
	jmp	.LBB57_114
.LBB57_18:                              # %lookup_name_current_level.exit
	movq	56(%rsi), %rax
	testq	%rax, %rax
	je	.LBB57_16
.LBB57_19:                              # %land.lhs.true44
	movq	8(%rax), %rsi
	movzbl	16(%rsi), %edi
	cmpq	$23, %rdi
	jne	.LBB57_24
# BB#20:                                # %land.lhs.true52
	movzbl	51(%rax), %edi
	testq	$96, %rdi
	jne	.LBB57_24
# BB#21:                                # %land.lhs.true59
	movq	8(%rdx), %rdi
	movq	8(%rdi), %rbx
	movq	8(%rsi), %rcx
	movq	128(%rbx), %rbx
	movq	128(%rcx), %rcx
	cmpq	%rcx, %rbx
	jne	.LBB57_24
# BB#22:                                # %land.lhs.true72
	cmpq	$0, 24(%rdi)
	jne	.LBB57_24
# BB#23:                                # %if.then78
	movq	%rsi, 8(%rdx)
	movq	24(%rax), %rcx
	movq	%rcx, current_function_prototype_file(%rip)
	movl	32(%rax), %ecx
	movl	%ecx, current_function_prototype_line(%rip)
	jmp	.LBB57_24
.LBB57_16:                              # %if.then87
	movq	80(%rsi), %rax
.LBB57_24:                              # %if.end90
	movl	warn_strict_prototypes(%rip), %ecx
	testq	%rcx, %rcx
	je	.LBB57_31
# BB#25:                                # %land.lhs.true92
	movq	8(%rsp), %rcx
	movq	8(%rcx), %rcx
	cmpq	$0, 24(%rcx)
	jne	.LBB57_31
# BB#26:                                # %land.lhs.true98
	testq	%rax, %rax
	je	.LBB57_30
# BB#27:                                # %land.lhs.true100
	movq	8(%rax), %rcx
	cmpq	$0, 24(%rcx)
	jne	.LBB57_31
# BB#28:                                # %lor.lhs.false106
	movq	48(%rax), %rdx
	testq	$1610612736, %rdx       # imm = 0x60000000
	je	.LBB57_30
# BB#29:                                # %lor.lhs.false106
	andq	%r8, %rdx
	jne	.LBB57_30
.LBB57_31:                              # %if.else
	movl	warn_missing_prototypes(%rip), %edx
	testq	%rdx, %rdx
	je	.LBB57_32
# BB#40:                                # %land.lhs.true123
	movq	8(%rsp), %rdi
	movzbl	18(%rdi), %ecx
	testq	$8, %rcx
	je	.LBB57_47
# BB#41:                                # %land.lhs.true129
	testq	%rax, %rax
	je	.LBB57_45
# BB#42:                                # %land.lhs.true131
	movq	8(%rax), %rcx
	cmpq	$0, 24(%rcx)
	jne	.LBB57_47
# BB#43:                                # %lor.lhs.false137
	movq	48(%rax), %rsi
	testq	$1610612736, %rsi       # imm = 0x60000000
	je	.LBB57_45
# BB#44:                                # %lor.lhs.false137
	andq	%r8, %rsi
	je	.LBB57_47
.LBB57_45:                              # %land.lhs.true153
	movq	72(%rdi), %rcx
	movq	global_trees+400(%rip), %rsi
	cmpq	%rsi, %rcx
	je	.LBB57_47
# BB#46:                                # %if.then157
	movabsq	$.L.str229, %rsi
	jmp	.LBB57_62
.LBB57_47:                              # %if.else158
	testq	%rax, %rax
	je	.LBB57_48
# BB#49:                                # %if.else158
	movabsq	$1, %rcx
	jmp	.LBB57_50
.LBB57_32:                              # %if.else158.thread
	testq	%rax, %rax
	je	.LBB57_33
# BB#34:                                # %if.else158.thread
	movabsq	$1, %rcx
	jmp	.LBB57_35
.LBB57_48:
	movabsq	$0, %rcx
.LBB57_50:                              # %if.else158
	testq	%rdx, %rdx
	je	.LBB57_35
# BB#51:                                # %if.else158
	testq	%rax, %rax
	je	.LBB57_35
# BB#52:                                # %land.lhs.true162
	movzbl	18(%rax), %edx
	testq	$1, %rdx
	je	.LBB57_35
# BB#53:                                # %land.lhs.true168
	movq	8(%rax), %rdx
	cmpq	$0, 24(%rdx)
	jne	.LBB57_35
# BB#54:                                # %if.then174
	movq	8(%rsp), %rdi
	movabsq	$.L.str230, %rsi
	jmp	.LBB57_62
.LBB57_33:
	movabsq	$0, %rcx
.LBB57_35:                              # %if.else175
	movl	warn_missing_declarations(%rip), %edx
	testq	%rdx, %rdx
	je	.LBB57_63
# BB#36:                                # %land.lhs.true177
	movq	8(%rsp), %rdi
	movl	16(%rdi), %esi
	testq	$524288, %rsi           # imm = 0x80000
	je	.LBB57_55
# BB#37:                                # %land.lhs.true177
	testq	%rax, %rax
	jne	.LBB57_55
# BB#38:                                # %land.lhs.true186
	movq	72(%rdi), %rsi
	movq	global_trees+400(%rip), %rbx
	cmpq	%rbx, %rsi
	je	.LBB57_55
# BB#39:                                # %if.then190
	movabsq	$.L.str231, %rsi
	jmp	.LBB57_62
.LBB57_55:                              # %if.else191
	testq	%rdx, %rdx
	je	.LBB57_56
# BB#57:                                # %if.else191
	movabsq	$1, %rdx
	jmp	.LBB57_58
.LBB57_56:
	movabsq	$0, %rdx
.LBB57_58:                              # %if.else191
	andq	%rcx, %rdx
	cmpq	$1, %rdx
	jne	.LBB57_63
# BB#59:                                # %land.lhs.true195
	movzbl	18(%rax), %ecx
	testq	$1, %rcx
	je	.LBB57_63
# BB#60:                                # %land.lhs.true202
	movq	8(%rsp), %rdi
	movq	72(%rdi), %rcx
	movq	80(%rcx), %rcx
	cmpq	%rcx, %rax
	jne	.LBB57_63
# BB#61:                                # %if.then207
	movabsq	$.L.str232, %rsi
.LBB57_62:                              # %if.end212
	xorq	%rax, %rax
	callq	warning_with_decl
	jmp	.LBB57_63
.LBB57_30:                              # %if.then121
	movabsq	$.L.str228, %rdi
	xorq	%rax, %rax
	callq	warning
.LBB57_63:                              # %if.end212
	movq	8(%rsp), %rax
	movl	current_extern_inline(%rip), %ecx
	movq	48(%rax), %rdx
	andq	$1, %rcx
	shlq	$8, %rcx
	andq	$-257, %rdx             # imm = 0xFFFFFFFFFFFFFEFF
	orq	%rcx, %rdx
	movq	%rdx, 48(%rax)
	movq	8(%rsp), %rax
	movl	16(%rax), %ecx
	orq	$262144, %rcx           # imm = 0x40000
	movl	%ecx, 16(%rax)
	cmpq	$0, current_function_decl(%rip)
	je	.LBB57_65
# BB#64:                                # %if.then221
	movq	8(%rsp), %rax
	movl	16(%rax), %ecx
	movabsq	$4294443007, %rdx       # imm = 0xFFF7FFFF
	andq	%rcx, %rdx
	movl	%edx, 16(%rax)
.LBB57_65:                              # %if.end226
	movslq	warn_main(%rip), %rax
	testq	%rax, %rax
	jle	.LBB57_93
# BB#66:                                # %land.lhs.true228
	movq	8(%rsp), %rdi
	movq	72(%rdi), %rax
	movq	global_trees+400(%rip), %rcx
	cmpq	%rcx, %rax
	jne	.LBB57_93
# BB#67:                                # %if.then232
	movq	8(%rdi), %rax
	movq	8(%rax), %rcx
	movq	128(%rcx), %rcx
	movq	integer_types+40(%rip), %rdx
	cmpq	%rdx, %rcx
	je	.LBB57_69
# BB#68:                                # %if.then240
	movabsq	$.L.str233, %rsi
	xorq	%rax, %rax
	callq	pedwarn_with_decl
	movq	8(%rsp), %rax
	movq	8(%rax), %rax
.LBB57_69:                              # %if.end241
	movq	24(%rax), %r15
	testq	%r15, %r15
	je	.LBB57_91
# BB#70:                                # %cond.true.lr.ph
	xorq	%rbx, %rbx
	movabsq	$4294967295, %r12       # imm = 0xFFFFFFFF
	movabsq	$.L.str236, %r14
	movabsq	$.L.str235, %r13
	.align	16, 0x90
.LBB57_71:                              # %cond.true
                                        # =>This Inner Loop Header: Depth=1
	movq	32(%r15), %rax
	movq	global_trees+216(%rip), %rdx
	movq	%rbx, %rcx
	cmpq	%rdx, %rax
	je	.LBB57_72
# BB#73:                                # %if.end251
                                        #   in Loop: Header=BB57_71 Depth=1
	movq	%rcx, %rdx
	andq	%r12, %rdx
	leaq	1(%rcx), %rbx
	cmpq	$2, %rdx
	je	.LBB57_82
# BB#74:                                # %if.end251
                                        #   in Loop: Header=BB57_71 Depth=1
	cmpq	$1, %rdx
	jne	.LBB57_75
# BB#78:                                # %sw.bb257
                                        #   in Loop: Header=BB57_71 Depth=1
	movzbl	16(%rax), %ecx
	cmpq	$13, %rcx
	jne	.LBB57_81
# BB#79:                                # %lor.lhs.false263
                                        #   in Loop: Header=BB57_71 Depth=1
	movq	8(%rax), %rax
	movzbl	16(%rax), %ecx
	cmpq	$13, %rcx
	jne	.LBB57_81
# BB#80:                                # %lor.lhs.false271
                                        #   in Loop: Header=BB57_71 Depth=1
	movq	8(%rax), %rax
	movq	128(%rax), %rax
	movq	integer_types(%rip), %rcx
	cmpq	%rcx, %rax
	je	.LBB57_87
	.align	16, 0x90
.LBB57_81:                              # %if.then279
                                        #   in Loop: Header=BB57_71 Depth=1
	movq	8(%rsp), %rdi
	xorq	%rax, %rax
	movq	%r13, %rsi
	jmp	.LBB57_86
	.align	16, 0x90
.LBB57_82:                              # %sw.bb281
                                        #   in Loop: Header=BB57_71 Depth=1
	movzbl	16(%rax), %ecx
	cmpq	$13, %rcx
	jne	.LBB57_85
# BB#83:                                # %lor.lhs.false287
                                        #   in Loop: Header=BB57_71 Depth=1
	movq	8(%rax), %rax
	movzbl	16(%rax), %ecx
	cmpq	$13, %rcx
	jne	.LBB57_85
# BB#84:                                # %lor.lhs.false295
                                        #   in Loop: Header=BB57_71 Depth=1
	movq	8(%rax), %rax
	movq	128(%rax), %rax
	movq	integer_types(%rip), %rcx
	cmpq	%rcx, %rax
	je	.LBB57_87
	.align	16, 0x90
.LBB57_85:                              # %if.then303
                                        #   in Loop: Header=BB57_71 Depth=1
	movq	8(%rsp), %rdi
	xorq	%rax, %rax
	movq	%r14, %rsi
	jmp	.LBB57_86
	.align	16, 0x90
.LBB57_75:                              # %if.end251
                                        #   in Loop: Header=BB57_71 Depth=1
	testq	%r12, %rcx
	jne	.LBB57_87
# BB#76:                                # %sw.bb
                                        #   in Loop: Header=BB57_71 Depth=1
	movq	128(%rax), %rax
	movq	integer_types+40(%rip), %rcx
	cmpq	%rcx, %rax
	je	.LBB57_87
# BB#77:                                # %if.then255
                                        #   in Loop: Header=BB57_71 Depth=1
	movq	8(%rsp), %rdi
	xorq	%rax, %rax
	movabsq	$.L.str234, %rsi
	.align	16, 0x90
.LBB57_86:                              # %for.inc
                                        #   in Loop: Header=BB57_71 Depth=1
	callq	pedwarn_with_decl
.LBB57_87:                              # %for.inc
                                        #   in Loop: Header=BB57_71 Depth=1
	movq	(%r15), %r15
	testq	%r15, %r15
	jne	.LBB57_71
	jmp	.LBB57_88
.LBB57_72:
	movq	%rcx, %rbx
.LBB57_88:                              # %for.end
	movq	$32, %rcx
	movq	%rbx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	movl	immediate_size_expand(%rip), %r12d
	jle	.LBB57_91
# BB#89:                                # %for.end
	movabsq	$4294967294, %rax       # imm = 0xFFFFFFFE
	andq	%rax, %rbx
	cmpq	$2, %rbx
	je	.LBB57_91
# BB#90:                                # %if.then311
	movq	8(%rsp), %rdi
	movabsq	$.L.str237, %rsi
	xorq	%rax, %rax
	callq	pedwarn_with_decl
.LBB57_91:                              # %if.end312
	movq	8(%rsp), %rdi
	movzbl	18(%rdi), %eax
	testq	$8, %rax
	jne	.LBB57_93
# BB#92:                                # %if.then319
	movabsq	$.L.str238, %rsi
	xorq	%rax, %rax
	callq	pedwarn_with_decl
.LBB57_93:                              # %if.end321
	movq	8(%rsp), %rdi
	callq	pushdecl
	movq	current_binding_level(%rip), %rcx
	movq	global_binding_level(%rip), %rdx
	movq	%rax, current_function_decl(%rip)
	cmpq	%rdx, %rcx
	jne	.LBB57_95
# BB#94:                                # %if.then.i418
	movq	$0, named_labels(%rip)
.LBB57_95:                              # %if.end.i420
	movq	free_binding_level(%rip), %rax
	testq	%rax, %rax
	je	.LBB57_97
# BB#96:                                # %if.then1.i
	movq	40(%rax), %rdx
	movq	%rdx, free_binding_level(%rip)
	jmp	.LBB57_98
.LBB57_97:                              # %if.else.i
	movq	$72, %rdi
	callq	xmalloc
	movq	current_binding_level(%rip), %rcx
.LBB57_98:                              # %if.end2.i
	movq	$0, 64(%rax)
	movq	$0, 56(%rax)
	movq	$0, 48(%rax)
	movq	$0, 40(%rax)
	movq	$0, 32(%rax)
	movq	$0, 24(%rax)
	movq	$0, 16(%rax)
	movq	$0, 8(%rax)
	movq	$0, (%rax)
	xorq	%rdx, %rdx
	testq	%rcx, %rcx
	movabsq	$0, %rdi
	movabsq	$0, %rsi
	je	.LBB57_103
# BB#99:                                # %cond.true.i
	movzbl	50(%rcx), %esi
	testq	%rsi, %rsi
	je	.LBB57_100
# BB#101:                               # %cond.true.i
	movabsq	$1, %rsi
	jmp	.LBB57_102
.LBB57_100:
	movabsq	$0, %rsi
.LBB57_102:                             # %cond.true.i
	movq	%rcx, %rdi
.LBB57_103:                             # %pushlevel.exit
	movb	%sil, 49(%rax)
	movq	%rdi, 40(%rax)
	movzbl	keep_next_level_flag(%rip), %ecx
	movq	%rax, current_binding_level(%rip)
	movb	%cl, 51(%rax)
	movb	%dl, keep_next_level_flag(%rip)
	movzbl	keep_next_if_subblocks(%rip), %ecx
	movb	%cl, 52(%rax)
	movb	%dl, keep_next_if_subblocks(%rip)
	movq	$1, %rcx
	movb	%cl, 48(%rax)
	movb	%cl, 50(%rax)
	movq	current_function_decl(%rip), %rdi
	xorq	%rsi, %rsi
	callq	make_decl_rtl
	movq	current_function_decl(%rip), %rax
	movq	8(%rax), %rax
	movq	8(%rax), %rbx
	movq	%rbx, %rdi
	callq	c_promoting_integer_type_p
	testq	%rax, %rax
	je	.LBB57_109
# BB#104:                               # %if.then328
	movzbl	17(%rbx), %eax
	testq	$32, %rax
	je	.LBB57_105
# BB#106:                               # %land.lhs.true334
	movl	flag_traditional(%rip), %eax
	testq	%rax, %rax
	jne	.LBB57_108
# BB#107:                               # %lor.lhs.false336
	movl	60(%rbx), %eax
	movq	integer_types+40(%rip), %rbx
	movl	60(%rbx), %ecx
	xorq	%rax, %rcx
	testq	$511, %rcx              # imm = 0x1FF
	jne	.LBB57_109
.LBB57_108:                             # %if.then345
	movq	integer_types+48(%rip), %rbx
	jmp	.LBB57_109
.LBB57_105:                             # %if.then328.if.else346_crit_edge
	movq	integer_types+40(%rip), %rbx
.LBB57_109:                             # %if.end348
	movq	$36, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	callq	build_decl
	movq	current_function_decl(%rip), %rcx
	movq	%rax, 96(%rcx)
	movq	current_function_decl(%rip), %rdi
	movq	120(%rdi), %rax
	testq	%rax, %rax
	jne	.LBB57_111
# BB#110:                               # %cond.false354
	callq	*lang_set_decl_assembler_name(%rip)
	movq	current_function_decl(%rip), %rdi
	movq	120(%rdi), %rax
.LBB57_111:                             # %cond.end355
	movzbl	17(%rax), %eax
	testq	$4, %rax
	je	.LBB57_113
# BB#112:                               # %if.then363
	movl	16(%rdi), %eax
	orq	$1024, %rax             # imm = 0x400
	movl	%eax, 16(%rdi)
.LBB57_113:                             # %if.end369
	movl	%r12d, immediate_size_expand(%rip)
	callq	start_fname_decls
	movq	$1, %rbx
.LBB57_114:                             # %return
	movq	%rbx, %rax
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp549:
	.size	start_function, .Ltmp549-start_function
	.cfi_endproc

	.globl	c_mark_varargs
	.align	16, 0x90
	.type	c_mark_varargs,@function
c_mark_varargs:                         # @c_mark_varargs
	.cfi_startproc
# BB#0:                                 # %entry
	movq	$1, %rax
	movb	%al, c_function_varargs(%rip)
	retq
.Ltmp550:
	.size	c_mark_varargs, .Ltmp550-c_mark_varargs
	.cfi_endproc

	.globl	store_parm_decls
	.align	16, 0x90
	.type	store_parm_decls,@function
store_parm_decls:                       # @store_parm_decls
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp557:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp558:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp559:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp560:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp561:
	.cfi_def_cfa_offset 48
	subq	$32, %rsp
.Ltmp562:
	.cfi_def_cfa_offset 80
.Ltmp563:
	.cfi_offset %rbx, -48
.Ltmp564:
	.cfi_offset %r12, -40
.Ltmp565:
	.cfi_offset %r13, -32
.Ltmp566:
	.cfi_offset %r14, -24
.Ltmp567:
	.cfi_offset %r15, -16
	movq	current_binding_level(%rip), %rax
	movq	current_function_decl(%rip), %rdi
	movq	%rdi, 24(%rsp)          # 8-byte Spill
	movq	current_function_parms(%rip), %r12
	movq	current_function_parm_tags(%rip), %rbx
	movq	(%rax), %r15
	callq	decl_function_context
	movq	%rax, 8(%rsp)           # 8-byte Spill
	movl	warn_shadow(%rip), %r13d
	xorq	%rax, %rax
	testq	%r12, %r12
	movl	%eax, warn_shadow(%rip)
	je	.LBB59_1
# BB#6:                                 # %land.lhs.true
	movzbl	16(%r12), %eax
	cmpq	$2, %rax
	jne	.LBB59_7
.LBB59_1:                               # %for.cond99.preheader
	testq	%r15, %r15
	je	.LBB59_4
# BB#2:
	movq	%r15, %rax
	.align	16, 0x90
.LBB59_3:                               # %for.body101
                                        # =>This Inner Loop Header: Depth=1
	andq	$-8388609, 48(%rax)     # imm = 0xFFFFFFFFFF7FFFFF
	movq	(%rax), %rax
	testq	%rax, %rax
	jne	.LBB59_3
.LBB59_4:                               # %for.cond109.preheader
	xorq	%rax, %rax
	testq	%r12, %r12
	je	.LBB59_5
# BB#34:
	movabsq	$.L.str242, %r13
	movq	%r12, %r14
	.align	16, 0x90
.LBB59_35:                              # %for.body111
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB59_37 Depth 2
	movq	32(%r14), %rax
	testq	%rax, %rax
	jne	.LBB59_36
# BB#111:                               # %if.then115
                                        #   in Loop: Header=BB59_35 Depth=1
	xorq	%rax, %rax
	movq	24(%rsp), %rdi          # 8-byte Reload
	movq	%r13, %rsi
	callq	error_with_decl
	movq	$0, 24(%r14)
	jmp	.LBB59_54
	.align	16, 0x90
.LBB59_36:                              # %for.cond119.preheader
                                        #   in Loop: Header=BB59_35 Depth=1
	testq	%r15, %r15
	movq	%r15, %rbx
	je	.LBB59_40
	.align	16, 0x90
.LBB59_37:                              # %for.body121
                                        #   Parent Loop BB59_35 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	72(%rbx), %rcx
	cmpq	%rax, %rcx
	jne	.LBB59_39
# BB#38:                                # %land.lhs.true127
                                        #   in Loop: Header=BB59_37 Depth=2
	movzbl	16(%rbx), %ecx
	cmpq	$35, %rcx
	je	.LBB59_42
.LBB59_39:                              # %for.inc135
                                        #   in Loop: Header=BB59_37 Depth=2
	movq	(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB59_37
	jmp	.LBB59_40
.LBB59_42:                              # %for.end138
                                        #   in Loop: Header=BB59_35 Depth=1
	testq	%rbx, %rbx
	je	.LBB59_40
# BB#43:                                # %land.lhs.true140
                                        #   in Loop: Header=BB59_35 Depth=1
	movzbl	50(%rbx), %eax
	testq	$128, %rax
	jne	.LBB59_44
# BB#45:                                # %land.lhs.true149
                                        #   in Loop: Header=BB59_35 Depth=1
	movq	8(%rbx), %rax
	movzbl	16(%rax), %eax
	cmpq	$5, %rax
	jne	.LBB59_47
# BB#46:                                # %if.then157
                                        #   in Loop: Header=BB59_35 Depth=1
	xorq	%rax, %rax
	movq	%rbx, %rdi
	movabsq	$.L.str241, %rsi
	callq	error_with_decl
	movq	integer_types+40(%rip), %rax
	movq	%rax, 8(%rbx)
	movq	integer_types+40(%rip), %rax
	movq	%rax, 104(%rbx)
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	layout_decl
.LBB59_47:                              # %if.end161
                                        #   in Loop: Header=BB59_35 Depth=1
	movl	flag_traditional(%rip), %eax
	testq	%rax, %rax
	je	.LBB59_53
# BB#48:                                # %land.lhs.true165
                                        #   in Loop: Header=BB59_35 Depth=1
	movq	8(%rbx), %rax
	movq	128(%rax), %rax
	movq	global_trees+192(%rip), %rcx
	cmpq	%rcx, %rax
	jne	.LBB59_53
# BB#49:                                # %if.then171
                                        #   in Loop: Header=BB59_35 Depth=1
	movq	global_trees+200(%rip), %rax
	movq	%rax, 8(%rbx)
	movq	global_trees+200(%rip), %rax
	movq	%rax, 104(%rbx)
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	layout_decl
	jmp	.LBB59_53
.LBB59_44:                              # %if.then146
                                        #   in Loop: Header=BB59_35 Depth=1
	xorq	%rax, %rax
	movq	%rbx, %rdi
	movabsq	$.L.str243, %rsi
	callq	error_with_decl
	.align	16, 0x90
.LBB59_40:                              # %if.then178
                                        #   in Loop: Header=BB59_35 Depth=1
	movq	32(%r14), %rsi
	movq	integer_types+40(%rip), %rdx
	movq	$35, %rdi
	callq	build_decl
	movq	%rax, %rbx
	movq	8(%rbx), %rax
	movq	%rax, 104(%rbx)
	movq	24(%rsp), %rcx          # 8-byte Reload
	movl	32(%rcx), %eax
	movl	%eax, 32(%rbx)
	movq	24(%rcx), %rax
	movq	%rax, 24(%rbx)
	movl	flag_isoc99(%rip), %eax
	testq	%rax, %rax
	jne	.LBB59_41
# BB#50:                                # %if.else194
                                        #   in Loop: Header=BB59_35 Depth=1
	movl	extra_warnings(%rip), %eax
	testq	%rax, %rax
	je	.LBB59_52
# BB#51:                                # %if.then196
                                        #   in Loop: Header=BB59_35 Depth=1
	xorq	%rax, %rax
	movq	%rbx, %rdi
	movabsq	$.L.str244, %rsi
	callq	warning_with_decl
	jmp	.LBB59_52
	.align	16, 0x90
.LBB59_41:                              # %if.then193
                                        #   in Loop: Header=BB59_35 Depth=1
	xorq	%rax, %rax
	movq	%rbx, %rdi
	movabsq	$.L.str244, %rsi
	callq	pedwarn_with_decl
.LBB59_52:                              # %if.end198
                                        #   in Loop: Header=BB59_35 Depth=1
	movq	%rbx, %rdi
	callq	pushdecl
.LBB59_53:                              # %if.end200
                                        #   in Loop: Header=BB59_35 Depth=1
	movq	%rbx, 24(%r14)
	orq	$8388608, 48(%rbx)      # imm = 0x800000
.LBB59_54:                              # %for.inc207
                                        #   in Loop: Header=BB59_35 Depth=1
	movq	(%r14), %r14
	xorq	%rax, %rax
	testq	%r14, %r14
	jne	.LBB59_35
	jmp	.LBB59_58
.LBB59_7:                               # %if.then
	movq	%rbx, 16(%rsp)          # 8-byte Spill
	testq	%r15, %r15
	je	.LBB59_17
# BB#8:                                 # %if.then4
	movabsq	$.L.str239, %rsi
	xorq	%rax, %rax
	movq	24(%rsp), %rdi          # 8-byte Reload
	callq	error_with_decl
	movq	current_binding_level(%rip), %rcx
	movq	(%rcx), %rax
	testq	%rax, %rax
	je	.LBB59_13
	.align	16, 0x90
.LBB59_9:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	72(%rax), %rcx
	testq	%rcx, %rcx
	je	.LBB59_11
# BB#10:                                # %if.then7
                                        #   in Loop: Header=BB59_9 Depth=1
	movq	$0, 64(%rcx)
.LBB59_11:                              # %for.inc
                                        #   in Loop: Header=BB59_9 Depth=1
	movq	(%rax), %rax
	testq	%rax, %rax
	jne	.LBB59_9
# BB#12:                                # %for.cond.for.end_crit_edge
	movq	current_binding_level(%rip), %rcx
.LBB59_13:                              # %for.end
	movq	16(%rcx), %rax
	testq	%rax, %rax
	je	.LBB59_16
	.align	16, 0x90
.LBB59_14:                              # %for.body13
                                        # =>This Inner Loop Header: Depth=1
	movq	24(%rax), %rcx
	movq	32(%rax), %rdx
	movq	%rdx, 64(%rcx)
	movq	(%rax), %rax
	testq	%rax, %rax
	jne	.LBB59_14
# BB#15:                                # %for.cond11.for.end19_crit_edge
	movq	current_binding_level(%rip), %rcx
.LBB59_16:                              # %for.end19
	movq	$0, (%rcx)
	movq	current_binding_level(%rip), %rax
	movq	$0, 16(%rax)
.LBB59_17:                              # %if.end22
	movq	%r12, %rdi
	callq	nreverse
	movq	%rax, %rbx
	testq	%rbx, %rbx
	je	.LBB59_32
# BB#18:
	movq	%r13, %r15
	xorq	%r14, %r14
	movabsq	$.L.str240, %r13
	jmp	.LBB59_19
.LBB59_5:
	xorq	%r12, %r12
	jmp	.LBB59_58
	.align	16, 0x90
.LBB59_57:                              # %if.then224
                                        #   in Loop: Header=BB59_58 Depth=1
	movq	16(%rsp), %rdi          # 8-byte Reload
	movq	%r13, %rsi
	callq	chainon
.LBB59_58:                              # %for.cond211.outer
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB59_55 Depth 2
	movq	%rax, 16(%rsp)          # 8-byte Spill
	movabsq	$.L.str245, %rbx
	movabsq	$.L.str246, %r14
	jmp	.LBB59_55
.LBB59_62:                              # %if.then242
                                        #   in Loop: Header=BB59_55 Depth=2
	xorq	%rax, %rax
	movq	%r13, %rdi
	movq	%r14, %rsi
	callq	error_with_decl
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	movq	%r13, %rdi
	callq	tree_cons
	movq	%r12, %rdi
	movq	%rax, %rsi
	callq	chainon
	movq	%rax, %r12
	.align	16, 0x90
.LBB59_55:                              # %for.cond211
                                        #   Parent Loop BB59_58 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	%r15, %r13
	testq	%r13, %r13
	je	.LBB59_63
# BB#56:                                # %for.body213
                                        #   in Loop: Header=BB59_55 Depth=2
	movq	(%r13), %r15
	movq	$0, (%r13)
	movzbl	16(%r13), %eax
	cmpq	$35, %rax
	jne	.LBB59_57
# BB#59:                                # %if.else226
                                        #   in Loop: Header=BB59_55 Depth=2
	movq	8(%r13), %rax
	cmpq	$0, 32(%rax)
	jne	.LBB59_61
# BB#60:                                # %if.then231
                                        #   in Loop: Header=BB59_55 Depth=2
	xorq	%rax, %rax
	movq	%r13, %rdi
	movq	%rbx, %rsi
	callq	error_with_decl
	movq	global_trees(%rip), %rax
	movq	%rax, 8(%r13)
.LBB59_61:                              # %if.end234
                                        #   in Loop: Header=BB59_55 Depth=2
	movzbl	50(%r13), %eax
	testq	$128, %rax
	jne	.LBB59_55
	jmp	.LBB59_62
.LBB59_63:                              # %for.end247
	movq	24(%rsp), %rax          # 8-byte Reload
	leaq	88(%rax), %rsi
	movq	$0, 88(%rax)
	movq	%rax, %rdx
	testq	%r12, %r12
	je	.LBB59_71
# BB#64:
	xorq	%rax, %rax
	.align	16, 0x90
.LBB59_65:                              # %for.body252
                                        # =>This Inner Loop Header: Depth=1
	movq	24(%r12), %rcx
	testq	%rcx, %rcx
	je	.LBB59_70
# BB#66:                                # %if.then256
                                        #   in Loop: Header=BB59_65 Depth=1
	testq	%rax, %rax
	jne	.LBB59_68
# BB#67:                                # %if.then258
                                        #   in Loop: Header=BB59_65 Depth=1
	movq	%rcx, (%rsi)
	jmp	.LBB59_69
	.align	16, 0x90
.LBB59_68:                              # %if.else263
                                        #   in Loop: Header=BB59_65 Depth=1
	movq	%rcx, (%rax)
.LBB59_69:                              # %if.end268
                                        #   in Loop: Header=BB59_65 Depth=1
	movq	24(%r12), %rax
	movq	$0, (%rax)
.LBB59_70:                              # %for.inc274
                                        #   in Loop: Header=BB59_65 Depth=1
	movq	(%r12), %r12
	testq	%r12, %r12
	jne	.LBB59_65
.LBB59_71:                              # %for.end277
	movq	%rdx, %r12
	movq	8(%r12), %rax
	movq	24(%rax), %rbx
	testq	%rbx, %rbx
	je	.LBB59_72
# BB#80:                                # %if.then282
	movq	%r12, %r15
	movabsq	$4294967295, %r12       # imm = 0xFFFFFFFF
	movabsq	$.L.str248, %r14
	movq	%rsi, (%rsp)            # 8-byte Spill
	movq	%rsi, %r13
	jmp	.LBB59_81
	.align	16, 0x90
.LBB59_102:                             # %for.inc410
                                        #   in Loop: Header=BB59_81 Depth=1
	movq	(%rbx), %rbx
.LBB59_81:                              # %for.cond290
                                        # =>This Inner Loop Header: Depth=1
	movq	(%r13), %r13
	testq	%r13, %r13
	jne	.LBB59_84
# BB#82:                                # %lor.rhs
                                        #   in Loop: Header=BB59_81 Depth=1
	testq	%rbx, %rbx
	je	.LBB59_88
# BB#83:                                # %land.rhs
                                        #   in Loop: Header=BB59_81 Depth=1
	movq	32(%rbx), %rax
	movq	128(%rax), %rax
	movq	global_trees+216(%rip), %rcx
	cmpq	%rcx, %rax
	je	.LBB59_88
.LBB59_84:                              # %for.body298
                                        #   in Loop: Header=BB59_81 Depth=1
	testq	%r13, %r13
	je	.LBB59_87
# BB#85:                                # %for.body298
                                        #   in Loop: Header=BB59_81 Depth=1
	testq	%rbx, %rbx
	je	.LBB59_87
# BB#86:                                # %lor.lhs.false301
                                        #   in Loop: Header=BB59_81 Depth=1
	movq	32(%rbx), %rax
	movq	128(%rax), %rsi
	movq	global_trees+216(%rip), %rax
	cmpq	%rax, %rsi
	je	.LBB59_87
# BB#89:                                # %if.end308
                                        #   in Loop: Header=BB59_81 Depth=1
	movq	104(%r13), %rax
	movq	128(%rax), %rdi
	callq	comptypes
	testq	%r12, %rax
	jne	.LBB59_102
# BB#90:                                # %if.then319
                                        #   in Loop: Header=BB59_81 Depth=1
	movq	8(%r13), %rax
	movq	32(%rbx), %rcx
	movq	128(%rax), %rdx
	movq	128(%rcx), %rcx
	cmpq	%rcx, %rdx
	jne	.LBB59_98
# BB#91:                                # %if.then329
                                        #   in Loop: Header=BB59_81 Depth=1
	movq	%rax, 104(%r13)
	movzbl	target_flags+3(%rip), %ecx
	testq	$2, %rcx
	jne	.LBB59_96
# BB#92:                                # %land.lhs.true335
                                        #   in Loop: Header=BB59_81 Depth=1
	movzbl	16(%rax), %ecx
	cmpq	$12, %rcx
	ja	.LBB59_96
# BB#93:                                # %land.lhs.true335
                                        #   in Loop: Header=BB59_81 Depth=1
	movq	$1, %rdx
	shlq	%cl, %rdx
	testq	$7232, %rdx             # imm = 0x1C40
	je	.LBB59_96
# BB#94:                                # %land.lhs.true367
                                        #   in Loop: Header=BB59_81 Depth=1
	movq	integer_types+40(%rip), %rcx
	movl	60(%rax), %eax
	movl	60(%rcx), %edx
	andq	$511, %rax              # imm = 0x1FF
	andq	$511, %rdx              # imm = 0x1FF
	cmpq	%rdx, %rax
	jae	.LBB59_96
# BB#95:                                # %if.then378
                                        #   in Loop: Header=BB59_81 Depth=1
	movq	%rcx, 104(%r13)
	.align	16, 0x90
.LBB59_96:                              # %if.end381
                                        #   in Loop: Header=BB59_81 Depth=1
	movl	pedantic(%rip), %eax
	testq	%rax, %rax
	je	.LBB59_102
# BB#97:                                # %if.then383
                                        #   in Loop: Header=BB59_81 Depth=1
	movq	72(%r13), %rax
	movq	32(%rax), %rsi
	xorq	%rax, %rax
	movabsq	$.L.str249, %rdi
	callq	pedwarn
	movq	current_function_prototype_file(%rip), %rdi
	movl	current_function_prototype_line(%rip), %esi
	xorq	%rax, %rax
	movq	%r14, %rdx
	callq	warning_with_file_and_line
	movq	(%rbx), %rbx
	jmp	.LBB59_81
	.align	16, 0x90
.LBB59_98:                              # %if.else387
                                        #   in Loop: Header=BB59_81 Depth=1
	movl	flag_traditional(%rip), %eax
	testq	%rax, %rax
	je	.LBB59_101
# BB#99:                                # %if.else387
                                        #   in Loop: Header=BB59_81 Depth=1
	movq	integer_types+40(%rip), %rax
	cmpq	%rax, %rdx
	jne	.LBB59_101
# BB#100:                               # %if.else387
                                        #   in Loop: Header=BB59_81 Depth=1
	movq	integer_types+48(%rip), %rax
	cmpq	%rax, %rcx
	je	.LBB59_102
.LBB59_101:                             # %if.then401
                                        #   in Loop: Header=BB59_81 Depth=1
	movq	72(%r13), %rax
	movq	32(%rax), %rsi
	xorq	%rax, %rax
	movabsq	$.L.str250, %rdi
	callq	error
	movq	current_function_prototype_file(%rip), %rdi
	movl	current_function_prototype_line(%rip), %esi
	xorq	%rax, %rax
	movq	%r14, %rdx
	callq	error_with_file_and_line
	jmp	.LBB59_102
.LBB59_87:                              # %if.then307
	movabsq	$.L.str247, %rdi
	xorq	%rax, %rax
	callq	error
	movq	current_function_prototype_file(%rip), %rdi
	movl	current_function_prototype_line(%rip), %esi
	movabsq	$.L.str248, %rdx
	xorq	%rax, %rax
	callq	error_with_file_and_line
.LBB59_88:                              # %for.end415
	movq	%r15, %r12
	movq	8(%r12), %rax
	movq	$0, 136(%rax)
	movl	warn_shadow(%rip), %r13d
	jmp	.LBB59_106
.LBB59_72:                              # %for.cond424.preheader
	movq	(%rsi), %rbx
	movq	%rsi, (%rsp)            # 8-byte Spill
	testq	%rbx, %rbx
	je	.LBB59_79
# BB#73:
	xorq	%r13, %r13
	xorq	%r14, %r14
	.align	16, 0x90
.LBB59_74:                              # %for.body426
                                        # =>This Inner Loop Header: Depth=1
	movq	104(%rbx), %rsi
	xorq	%rdi, %rdi
	xorq	%rdx, %rdx
	callq	tree_cons
	movq	%rax, %r12
	testq	%r13, %r13
	movq	%r12, %r15
	je	.LBB59_76
# BB#75:                                # %if.then431
                                        #   in Loop: Header=BB59_74 Depth=1
	movq	%r12, (%r13)
	movq	%r14, %r15
.LBB59_76:                              # %if.end435
                                        #   in Loop: Header=BB59_74 Depth=1
	movq	(%rbx), %rbx
	testq	%rbx, %rbx
	movq	%r12, %r13
	movq	%r15, %r14
	jne	.LBB59_74
# BB#77:                                # %for.end439
	movq	global_trees+216(%rip), %rsi
	xorq	%rdi, %rdi
	xorq	%rdx, %rdx
	callq	tree_cons
	testq	%r12, %r12
	movl	warn_shadow(%rip), %r13d
	je	.LBB59_78
# BB#103:                               # %if.then442
	movq	%rax, (%r12)
	jmp	.LBB59_104
.LBB59_112:                             # %if.else59
                                        #   in Loop: Header=BB59_19 Depth=1
	movq	$0, (%r12)
	movq	%r14, %rdi
	movq	%r12, %rsi
	callq	chainon
	movq	%rax, %r14
	jmp	.LBB59_25
	.align	16, 0x90
.LBB59_19:                              # %for.body26
                                        # =>This Inner Loop Header: Depth=1
	movq	%rbx, %r12
	movzbl	16(%r12), %eax
	movq	(%r12), %rbx
	cmpq	$35, %rax
	jne	.LBB59_112
# BB#20:                                # %if.then34
                                        #   in Loop: Header=BB59_19 Depth=1
	cmpq	$0, 72(%r12)
	jne	.LBB59_22
# BB#21:                                # %if.then38
                                        #   in Loop: Header=BB59_19 Depth=1
	xorq	%rax, %rax
	movq	%r12, %rdi
	movq	%r13, %rsi
	callq	error_with_decl
	jmp	.LBB59_24
	.align	16, 0x90
.LBB59_22:                              # %if.else
                                        #   in Loop: Header=BB59_19 Depth=1
	movq	8(%r12), %rax
	movzbl	16(%rax), %eax
	cmpq	$5, %rax
	jne	.LBB59_24
# BB#23:                                # %if.then53
                                        #   in Loop: Header=BB59_19 Depth=1
	xorq	%rax, %rax
	movq	%r12, %rdi
	movabsq	$.L.str241, %rsi
	callq	error_with_decl
	movq	global_trees(%rip), %rax
	movq	%rax, 8(%r12)
.LBB59_24:                              # %if.end57
                                        #   in Loop: Header=BB59_19 Depth=1
	movq	%r12, %rdi
	callq	pushdecl
.LBB59_25:                              # %if.end57
                                        #   in Loop: Header=BB59_19 Depth=1
	testq	%rbx, %rbx
	jne	.LBB59_19
# BB#26:                                # %for.end65
	movq	current_binding_level(%rip), %rax
	movq	(%rax), %rax
	movq	24(%rsp), %r12          # 8-byte Reload
	movq	%rax, 88(%r12)
	testq	%r14, %r14
	movq	%r15, %r13
	movq	8(%rsp), %rbx           # 8-byte Reload
	je	.LBB59_33
	.align	16, 0x90
.LBB59_27:                              # %for.body70
                                        # =>This Inner Loop Header: Depth=1
	movq	%r14, %rdi
	movq	(%rdi), %r14
	cmpq	$0, 72(%rdi)
	je	.LBB59_30
# BB#28:                                # %if.else77
                                        #   in Loop: Header=BB59_27 Depth=1
	movq	8(%rdi), %rax
	movq	128(%rax), %rax
	movq	global_trees+216(%rip), %rcx
	cmpq	%rcx, %rax
	je	.LBB59_30
# BB#29:                                # %if.else83
                                        #   in Loop: Header=BB59_27 Depth=1
	movzbl	16(%rdi), %eax
	cmpq	$35, %rax
	je	.LBB59_30
# BB#31:                                # %if.then89
                                        #   in Loop: Header=BB59_27 Depth=1
	callq	pushdecl
	.align	16, 0x90
.LBB59_30:                              # %for.cond68.backedge
                                        #   in Loop: Header=BB59_27 Depth=1
	testq	%r14, %r14
	jne	.LBB59_27
	jmp	.LBB59_33
.LBB59_32:                              # %for.end65.thread
	movq	current_binding_level(%rip), %rax
	movq	(%rax), %rax
	movq	24(%rsp), %r12          # 8-byte Reload
	movq	%rax, 88(%r12)
	movq	8(%rsp), %rbx           # 8-byte Reload
.LBB59_33:                              # %for.end95
	movq	current_binding_level(%rip), %rax
	movq	8(%rax), %rsi
	movq	16(%rsp), %rdi          # 8-byte Reload
	callq	chainon
	movq	current_binding_level(%rip), %rcx
	movq	%rax, 8(%rcx)
	movq	$1, %rcx
	jmp	.LBB59_107
.LBB59_79:                              # %for.end439.thread
	movq	global_trees+216(%rip), %rsi
	xorq	%rdi, %rdi
	xorq	%rdx, %rdx
	callq	tree_cons
	movq	%rax, %r15
	movl	warn_shadow(%rip), %r13d
	jmp	.LBB59_105
.LBB59_78:
	movq	%rax, %r15
.LBB59_104:                             # %if.end446
	movq	24(%rsp), %r12          # 8-byte Reload
.LBB59_105:                             # %if.end446
	movq	8(%r12), %rdi
	callq	build_type_copy
	movq	%rax, 8(%r12)
	movq	%r15, 136(%rax)
.LBB59_106:                             # %if.end456
	movq	8(%rsp), %rbx           # 8-byte Reload
	movq	(%rsp), %rax            # 8-byte Reload
	movq	(%rax), %rsi
	movq	16(%rsp), %rdi          # 8-byte Reload
	callq	chainon
	movq	current_binding_level(%rip), %rcx
	movq	%rax, (%rcx)
	xorq	%rcx, %rcx
.LBB59_107:                             # %if.end460
	movq	$1, %rax
	movb	%al, keep_next_if_subblocks(%rip)
	movq	$1, %rsi
	xorq	%rdx, %rdx
	xorq	%r14, %r14
	movq	%r12, %rdi
	callq	gen_aux_info_record
	movq	input_filename(%rip), %rsi
	movl	lineno(%rip), %edx
	movq	%r12, %rdi
	callq	init_function_start
	movq	current_function_decl(%rip), %rdi
	addq	$168, %rdi
	callq	begin_stmt_tree
	testq	%rbx, %rbx
	je	.LBB59_110
# BB#108:                               # %if.then463
	callq	get_pending_sizes
	movq	%rax, %rdi
	callq	nreverse
	movq	200(%r12), %rcx
	testq	%rax, %rax
	movq	%rax, 8(%rcx)
	je	.LBB59_110
	.align	16, 0x90
.LBB59_109:                             # %for.body472
                                        # =>This Inner Loop Header: Depth=1
	movq	32(%rax), %rcx
	movq	%rbx, 40(%rcx)
	movq	(%rax), %rax
	testq	%rax, %rax
	jne	.LBB59_109
.LBB59_110:                             # %if.end479
	movq	cfun(%rip), %rax
	movl	424(%rax), %ecx
	orq	$262144, %rcx           # imm = 0x40000
	movl	%ecx, 424(%rax)
	movq	cfun(%rip), %rax
	movl	%r14d, immediate_size_expand(%rip)
	movl	424(%rax), %ecx
	orq	$524288, %rcx           # imm = 0x80000
	movl	%ecx, 424(%rax)
	movl	%r13d, warn_shadow(%rip)
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp568:
	.size	store_parm_decls, .Ltmp568-store_parm_decls
	.cfi_endproc

	.globl	finish_function
	.align	16, 0x90
	.type	finish_function,@function
finish_function:                        # @finish_function
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp573:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp574:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp575:
	.cfi_def_cfa_offset 32
.Ltmp576:
	.cfi_offset %rbx, -32
.Ltmp577:
	.cfi_offset %r14, -24
.Ltmp578:
	.cfi_offset %r15, -16
	movq	current_function_decl(%rip), %rbx
	movq	%rsi, %r14
	movq	%rdi, %r15
	movq	$1, %rdi
	xorq	%rsi, %rsi
	movq	$1, %rdx
	callq	poplevel
	movq	104(%rbx), %rdi
	movq	%rbx, 48(%rdi)
	movq	96(%rbx), %rax
	movq	%rbx, 80(%rax)
	movl	flag_traditional(%rip), %eax
	testq	%rax, %rax
	je	.LBB60_3
# BB#1:                                 # %land.lhs.true
	movq	cfun(%rip), %rax
	movzbl	424(%rax), %eax
	testq	$16, %rax
	je	.LBB60_3
# BB#2:                                 # %if.then
	callq	setjmp_protect
	callq	setjmp_protect_args
.LBB60_3:                               # %if.end
	movq	72(%rbx), %rax
	movq	global_trees+400(%rip), %rcx
	cmpq	%rcx, %rax
	jne	.LBB60_10
# BB#4:                                 # %if.end
	movl	flag_hosted(%rip), %eax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB60_10
# BB#5:                                 # %if.then9
	movq	8(%rbx), %rax
	movq	8(%rax), %rax
	movq	128(%rax), %rax
	movq	integer_types+40(%rip), %rcx
	cmpq	%rcx, %rax
	je	.LBB60_8
# BB#6:                                 # %if.then14
	movl	warn_main(%rip), %eax
	testq	%rax, %rax
	jne	.LBB60_10
# BB#7:                                 # %if.then16
	movabsq	$.L.str233, %rsi
	xorq	%rax, %rax
	movq	%rbx, %rdi
	callq	pedwarn_with_decl
	jmp	.LBB60_10
.LBB60_8:                               # %if.else
	movl	flag_isoc99(%rip), %eax
	testq	%rax, %rax
	je	.LBB60_10
# BB#9:                                 # %if.then19
	movq	global_trees+88(%rip), %rdi
	callq	c_expand_return
.LBB60_10:                              # %if.end23
	callq	finish_fname_decls
	leaq	168(%rbx), %rdi
	callq	finish_stmt_tree
	movl	warn_return_type(%rip), %eax
	testq	%rax, %rax
	je	.LBB60_18
# BB#11:                                # %land.lhs.true26
	movq	8(%rbx), %rax
	movq	8(%rax), %rax
	movzbl	16(%rax), %eax
	cmpq	$5, %rax
	je	.LBB60_18
# BB#12:                                # %land.lhs.true26
	movl	current_function_returns_value(%rip), %ecx
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %rcx
	jne	.LBB60_18
# BB#13:                                # %land.lhs.true26
	movl	current_function_returns_null(%rip), %ecx
	testq	%rax, %rcx
	jne	.LBB60_18
# BB#14:                                # %land.lhs.true26
	movl	current_function_returns_abnormally(%rip), %ecx
	testq	%rax, %rcx
	jne	.LBB60_18
# BB#15:                                # %land.lhs.true41
	movq	72(%rbx), %rax
	movq	global_trees+400(%rip), %rcx
	cmpq	%rcx, %rax
	je	.LBB60_18
# BB#16:                                # %land.lhs.true45
	movq	48(%rbx), %rax
	movabsq	$274877908992, %rcx     # imm = 0x4000000800
	andq	%rax, %rcx
	cmpq	$2048, %rcx             # imm = 0x800
	jne	.LBB60_18
# BB#17:                                # %if.then58
	movabsq	$.L.str251, %rdi
	xorq	%rax, %rax
	callq	warning
.LBB60_18:                              # %if.end59
	movq	cfun(%rip), %rdi
	callq	free_after_parsing
	movq	cfun(%rip), %rdi
	callq	free_after_compilation
	movq	$0, cfun(%rip)
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %r15
	jne	.LBB60_20
# BB#19:                                # %if.then61
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	movq	%r14, %rdx
	callq	c_expand_body
	movq	$0, current_function_decl(%rip)
.LBB60_20:                              # %if.end62
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp579:
	.size	finish_function, .Ltmp579-finish_function
	.cfi_endproc

	.align	16, 0x90
	.type	c_expand_body,@function
c_expand_body:                          # @c_expand_body
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp586:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp587:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp588:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp589:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp590:
	.cfi_def_cfa_offset 48
.Ltmp591:
	.cfi_offset %rbx, -48
.Ltmp592:
	.cfi_offset %r12, -40
.Ltmp593:
	.cfi_offset %r13, -32
.Ltmp594:
	.cfi_offset %r14, -24
.Ltmp595:
	.cfi_offset %r15, -16
	movl	flag_syntax_only(%rip), %eax
	movq	%rdx, %rbx
	movq	%rsi, %r14
	movq	%rdi, %r15
	testq	%rax, %rax
	je	.LBB61_1
# BB#63:                                # %return
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB61_1:                               # %if.end
	movl	flag_inline_trees(%rip), %eax
	testq	%rax, %rax
	movq	$1, %r12
	je	.LBB61_11
# BB#2:                                 # %if.then2
	movq	$12, %rdi
	callq	timevar_push
	movq	%r15, %rdi
	callq	tree_inlinable_function_p
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	andq	%r13, %rax
	movabsq	$0, %r12
	movabsq	$1, %rdx
	movq	%r12, %rcx
	je	.LBB61_4
# BB#3:                                 # %if.then2
	movq	%rdx, %rcx
.LBB61_4:                               # %if.then2
	testq	%rax, %rax
	jne	.LBB61_6
# BB#5:                                 # %if.then2
	movq	%rdx, %r12
.LBB61_6:                               # %if.then2
	testq	%r13, %rbx
	je	.LBB61_10
# BB#7:                                 # %if.then2
	testq	%rcx, %rcx
	je	.LBB61_10
# BB#8:                                 # %land.lhs.true6
	movq	%r15, %rdi
	callq	defer_fn
	testq	%r13, %rax
	je	.LBB61_10
# BB#9:                                 # %if.then9
	movq	debug_hooks(%rip), %rax
	movq	%r15, %rdi
	callq	*136(%rax)
	movq	$12, %rdi
	jmp	.LBB61_62
.LBB61_10:                              # %if.end10
	movq	%r15, %rdi
	callq	optimize_inline_calls
	movq	$12, %rdi
	callq	timevar_pop
.LBB61_11:                              # %if.end11
	movq	$10, %rdi
	callq	timevar_push
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	testq	%r13, %r14
	je	.LBB61_13
# BB#12:                                # %if.then13
	movq	200(%r15), %rax
	movq	8(%rax), %rdi
	callq	expand_pending_sizes
	callq	push_function_context
.LBB61_13:                              # %if.end14
	movq	%r15, current_function_decl(%rip)
	movq	24(%r15), %rsi
	movq	%rsi, input_filename(%rip)
	movl	32(%r15), %edx
	movq	%r15, %rdi
	callq	init_function_start
	movq	cfun(%rip), %rax
	movl	424(%rax), %ecx
	orq	$262144, %rcx           # imm = 0x40000
	movl	%ecx, 424(%rax)
	movq	cfun(%rip), %rax
	xorq	%rcx, %rcx
	movl	%ecx, immediate_size_expand(%rip)
	movl	424(%rax), %ecx
	orq	$524288, %rcx           # imm = 0x80000
	movl	%ecx, 424(%rax)
	movzbl	c_function_varargs(%rip), %eax
	cmpq	$1, %rax
	jne	.LBB61_15
# BB#14:                                # %if.then21
	callq	mark_varargs
.LBB61_15:                              # %if.end22
	xorq	%rsi, %rsi
	movq	%r15, %rdi
	callq	expand_function_start
	movq	72(%r15), %rax
	testq	%rax, %rax
	je	.LBB61_19
# BB#16:                                # %if.end22
	movq	global_trees+400(%rip), %rcx
	cmpq	%rcx, %rax
	jne	.LBB61_19
# BB#17:                                # %land.lhs.true28
	cmpq	$0, 80(%r15)
	jne	.LBB61_19
# BB#18:                                # %if.then31
	callq	expand_main_function
.LBB61_19:                              # %if.end32
	movq	168(%r15), %rdi
	callq	expand_stmt
	testq	%r12, %r12
	je	.LBB61_21
# BB#20:                                # %if.then35
	movq	$0, 168(%r15)
.LBB61_21:                              # %if.end38
	movq	lang_expand_function_end(%rip), %rax
	movq	$1, %rcx
	movl	%ecx, immediate_size_expand(%rip)
	testq	%rax, %rax
	je	.LBB61_23
# BB#22:                                # %if.then40
	callq	*%rax
.LBB61_23:                              # %if.end41
	movq	input_filename(%rip), %rdi
	movl	lineno(%rip), %esi
	xorq	%rdx, %rdx
	callq	expand_function_end
	testq	%r13, %r14
	je	.LBB61_25
# BB#24:                                # %if.then43
	callq	ggc_push_context
	movq	%r15, %rdi
	callq	rest_of_compilation
	callq	ggc_pop_context
	jmp	.LBB61_26
.LBB61_25:                              # %if.end47.critedge
	movq	%r15, %rdi
	callq	rest_of_compilation
.LBB61_26:                              # %if.end47
	movl	extra_warnings(%rip), %eax
	testq	%rax, %rax
	je	.LBB61_30
# BB#27:                                # %if.end47
	movl	current_function_returns_value(%rip), %eax
	testq	%r13, %rax
	je	.LBB61_30
# BB#28:                                # %if.end47
	movl	current_function_returns_null(%rip), %eax
	testq	%r13, %rax
	je	.LBB61_30
# BB#29:                                # %if.then53
	movabsq	$.L.str259, %rdi
	xorq	%rax, %rax
	callq	warning
.LBB61_30:                              # %if.end54
	movl	warn_larger_than(%rip), %eax
	testq	%rax, %rax
	je	.LBB61_41
# BB#31:                                # %land.lhs.true56
	movzbl	49(%r15), %eax
	testq	$1, %rax
	jne	.LBB61_41
# BB#32:                                # %land.lhs.true61
	movq	8(%r15), %rax
	testq	%rax, %rax
	je	.LBB61_41
# BB#33:                                # %if.then63
	movq	8(%rax), %r12
	testq	%r12, %r12
	je	.LBB61_41
# BB#34:                                # %land.lhs.true69
	movq	40(%r12), %rdi
	testq	%rdi, %rdi
	je	.LBB61_41
# BB#35:                                # %land.lhs.true72
	movzbl	16(%rdi), %eax
	cmpq	$25, %rax
	jne	.LBB61_41
# BB#36:                                # %land.lhs.true79
	movq	larger_than_size(%rip), %rsi
	callq	compare_tree_int
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jle	.LBB61_41
# BB#37:                                # %if.then84
	movq	40(%r12), %rdi
	movq	32(%rdi), %r12
	movq	%r12, %rsi
	andq	%r13, %rsi
	callq	compare_tree_int
	testq	%r13, %rax
	je	.LBB61_38
# BB#39:                                # %if.else
	movq	larger_than_size(%rip), %rdx
	movabsq	$.L.str261, %rsi
	xorq	%rax, %rax
	movq	%r15, %rdi
	jmp	.LBB61_40
.LBB61_38:                              # %if.then94
	movabsq	$.L.str260, %rsi
	xorq	%rax, %rax
	movq	%r15, %rdi
	movq	%r12, %rdx
.LBB61_40:                              # %if.end97
	callq	warning_with_decl
.LBB61_41:                              # %if.end97
	cmpq	$0, 160(%r15)
	jne	.LBB61_47
# BB#42:                                # %if.end97
	testq	%r13, %r14
	jne	.LBB61_47
# BB#43:                                # %if.end97
	movl	flag_inline_trees(%rip), %eax
	testq	%r13, %rax
	jne	.LBB61_47
# BB#44:                                # %if.then105
	cmpq	$0, 104(%r15)
	je	.LBB61_46
# BB#45:                                # %if.then109
	movq	global_trees(%rip), %rax
	movq	%rax, 104(%r15)
.LBB61_46:                              # %if.end112
	movq	$0, 88(%r15)
.LBB61_47:                              # %if.end114
	movzbl	50(%r15), %eax
	testq	$16, %rax
	je	.LBB61_53
# BB#48:                                # %if.then121
	movzbl	targetm+329(%rip), %eax
	testq	%rax, %rax
	je	.LBB61_52
# BB#49:                                # %if.then123
	movq	144(%r15), %rax
	movq	targetm+152(%rip), %rbx
	testq	%rax, %rax
	jne	.LBB61_51
# BB#50:                                # %cond.false
	xorq	%rsi, %rsi
	movq	%r15, %rdi
	callq	make_decl_rtl
	movq	144(%r15), %rax
.LBB61_51:                              # %cond.end
	movq	8(%rax), %rdi
	movq	$65535, %rsi            # imm = 0xFFFF
	callq	*%rbx
	jmp	.LBB61_53
.LBB61_52:                              # %if.else130
	movq	static_ctors(%rip), %rdx
	xorq	%rdi, %rdi
	movq	%r15, %rsi
	callq	tree_cons
	movq	%rax, static_ctors(%rip)
.LBB61_53:                              # %if.end133
	movzbl	50(%r15), %eax
	testq	$32, %rax
	je	.LBB61_59
# BB#54:                                # %if.then140
	movzbl	targetm+329(%rip), %eax
	testq	%rax, %rax
	je	.LBB61_58
# BB#55:                                # %if.then142
	movq	144(%r15), %rax
	movq	targetm+160(%rip), %rbx
	testq	%rax, %rax
	jne	.LBB61_57
# BB#56:                                # %cond.false149
	xorq	%rsi, %rsi
	movq	%r15, %rdi
	callq	make_decl_rtl
	movq	144(%r15), %rax
.LBB61_57:                              # %cond.end152
	movq	8(%rax), %rdi
	movq	$65535, %rsi            # imm = 0xFFFF
	callq	*%rbx
	jmp	.LBB61_59
.LBB61_58:                              # %if.else157
	movq	static_dtors(%rip), %rdx
	xorq	%rdi, %rdi
	movq	%r15, %rsi
	callq	tree_cons
	movq	%rax, static_dtors(%rip)
.LBB61_59:                              # %if.end160
	testq	%r13, %r14
	je	.LBB61_61
# BB#60:                                # %if.then162
	callq	pop_function_context
.LBB61_61:                              # %if.end163
	movq	$10, %rdi
.LBB61_62:                              # %if.end163
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	timevar_pop  # TAILCALL
.Ltmp596:
	.size	c_expand_body, .Ltmp596-c_expand_body
	.cfi_endproc

	.globl	c_expand_deferred_function
	.align	16, 0x90
	.type	c_expand_deferred_function,@function
c_expand_deferred_function:             # @c_expand_deferred_function
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rax
.Ltmp598:
	.cfi_def_cfa_offset 16
	movzbl	49(%rdi), %eax
	testq	$8, %rax
	je	.LBB62_3
# BB#1:                                 # %land.lhs.true
	cmpq	$0, 96(%rdi)
	je	.LBB62_3
# BB#2:                                 # %if.then
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	callq	c_expand_body
	movq	$0, current_function_decl(%rip)
.LBB62_3:                               # %if.end
	popq	%rax
	retq
.Ltmp599:
	.size	c_expand_deferred_function, .Ltmp599-c_expand_deferred_function
	.cfi_endproc

	.globl	check_for_loop_decls
	.align	16, 0x90
	.type	check_for_loop_decls,@function
check_for_loop_decls:                   # @check_for_loop_decls
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp605:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp606:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp607:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp608:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp609:
	.cfi_def_cfa_offset 48
.Ltmp610:
	.cfi_offset %rbx, -40
.Ltmp611:
	.cfi_offset %r12, -32
.Ltmp612:
	.cfi_offset %r14, -24
.Ltmp613:
	.cfi_offset %r15, -16
	movl	flag_isoc99(%rip), %eax
	testq	%rax, %rax
	je	.LBB63_24
# BB#1:                                 # %if.end
	movq	current_binding_level(%rip), %rax
	movq	8(%rax), %rbx
	testq	%rbx, %rbx
	je	.LBB63_12
# BB#2:
	movabsq	$.L.str253, %r12
	movabsq	$.L.str254, %r14
	movabsq	$.L.str255, %r15
	.align	16, 0x90
.LBB63_3:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	24(%rbx), %rax
	testq	%rax, %rax
	je	.LBB63_10
# BB#4:                                 # %if.then2
                                        #   in Loop: Header=BB63_3 Depth=1
	movq	32(%rbx), %rcx
	movzbl	16(%rcx), %ecx
	cmpq	$20, %rcx
	jne	.LBB63_6
# BB#5:                                 # %if.then6
                                        #   in Loop: Header=BB63_3 Depth=1
	movq	32(%rax), %rsi
	xorq	%rax, %rax
	movq	%r12, %rdi
	jmp	.LBB63_9
	.align	16, 0x90
.LBB63_6:                               # %if.else
                                        #   in Loop: Header=BB63_3 Depth=1
	movq	32(%rax), %rsi
	xorq	%rax, %rax
	cmpq	$21, %rcx
	jne	.LBB63_8
# BB#7:                                 # %if.then10
                                        #   in Loop: Header=BB63_3 Depth=1
	movq	%r14, %rdi
	jmp	.LBB63_9
.LBB63_8:                               # %if.else16
                                        #   in Loop: Header=BB63_3 Depth=1
	movq	%r15, %rdi
.LBB63_9:                               # %for.inc
                                        #   in Loop: Header=BB63_3 Depth=1
	callq	error
.LBB63_10:                              # %for.inc
                                        #   in Loop: Header=BB63_3 Depth=1
	movq	(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB63_3
# BB#11:                                # %for.cond.for.end_crit_edge
	movq	current_binding_level(%rip), %rax
.LBB63_12:                              # %for.end
	movq	(%rax), %rbx
	testq	%rbx, %rbx
	je	.LBB63_23
# BB#13:
	movabsq	$.L.str258, %r14
	movabsq	$.L.str257, %r15
	movabsq	$.L.str256, %r12
	.align	16, 0x90
.LBB63_14:                              # %for.body29
                                        # =>This Inner Loop Header: Depth=1
	movl	16(%rbx), %eax
	movq	%rax, %rcx
	andq	$255, %rcx
	cmpq	$34, %rcx
	je	.LBB63_17
# BB#15:                                # %land.lhs.true
                                        #   in Loop: Header=BB63_14 Depth=1
	cmpq	$0, 72(%rbx)
	jne	.LBB63_16
.LBB63_17:                              # %if.else37
                                        #   in Loop: Header=BB63_14 Depth=1
	testq	$262144, %rax           # imm = 0x40000
	jne	.LBB63_18
# BB#19:                                # %if.else43
                                        #   in Loop: Header=BB63_14 Depth=1
	movzbl	49(%rbx), %eax
	testq	$1, %rax
	je	.LBB63_22
# BB#20:                                # %if.then49
                                        #   in Loop: Header=BB63_14 Depth=1
	xorq	%rax, %rax
	movq	%rbx, %rdi
	movq	%r14, %rsi
	jmp	.LBB63_21
	.align	16, 0x90
.LBB63_18:                              # %if.then42
                                        #   in Loop: Header=BB63_14 Depth=1
	xorq	%rax, %rax
	movq	%rbx, %rdi
	movq	%r15, %rsi
	jmp	.LBB63_21
	.align	16, 0x90
.LBB63_16:                              # %if.then36
                                        #   in Loop: Header=BB63_14 Depth=1
	xorq	%rax, %rax
	movq	%rbx, %rdi
	movq	%r12, %rsi
.LBB63_21:                              # %for.inc53
                                        #   in Loop: Header=BB63_14 Depth=1
	callq	error_with_decl
.LBB63_22:                              # %for.inc53
                                        #   in Loop: Header=BB63_14 Depth=1
	movq	(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB63_14
.LBB63_23:                              # %for.end56
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.LBB63_24:                              # %if.then
	movabsq	$.L.str252, %rdi
	xorq	%rax, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	jmp	error  # TAILCALL
.Ltmp614:
	.size	check_for_loop_decls, .Ltmp614-check_for_loop_decls
	.cfi_endproc

	.globl	push_c_function_context
	.align	16, 0x90
	.type	push_c_function_context,@function
push_c_function_context:                # @push_c_function_context
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp617:
	.cfi_def_cfa_offset 16
.Ltmp618:
	.cfi_offset %rbx, -16
	movq	%rdi, %rbx
	movq	$88, %rdi
	callq	xmalloc
	movq	%rax, 408(%rbx)
	movq	c_stmt_tree+24(%rip), %rcx
	movq	%rcx, 24(%rax)
	movq	c_stmt_tree+16(%rip), %rcx
	movq	%rcx, 16(%rax)
	movq	c_stmt_tree+8(%rip), %rcx
	movq	%rcx, 8(%rax)
	movq	c_stmt_tree(%rip), %rcx
	movq	%rcx, (%rax)
	movq	c_scope_stmt_stack(%rip), %rcx
	movq	%rcx, 32(%rax)
	movq	named_labels(%rip), %rcx
	movq	%rcx, 40(%rax)
	movq	shadowed_labels(%rip), %rcx
	movq	%rcx, 48(%rax)
	movl	current_function_returns_value(%rip), %ecx
	movl	%ecx, 56(%rax)
	movl	current_function_returns_null(%rip), %ecx
	movl	%ecx, 60(%rax)
	movl	current_function_returns_abnormally(%rip), %ecx
	movl	warn_about_return_type(%rip), %edx
	movl	current_extern_inline(%rip), %esi
	movq	current_binding_level(%rip), %rdi
	movl	%ecx, 64(%rax)
	movl	%edx, 68(%rax)
	movl	%esi, 72(%rax)
	movq	%rdi, 80(%rax)
	popq	%rbx
	retq
.Ltmp619:
	.size	push_c_function_context, .Ltmp619-push_c_function_context
	.cfi_endproc

	.globl	pop_c_function_context
	.align	16, 0x90
	.type	pop_c_function_context,@function
pop_c_function_context:                 # @pop_c_function_context
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp622:
	.cfi_def_cfa_offset 16
.Ltmp623:
	.cfi_offset %rbx, -16
	movq	%rdi, %rbx
	movq	shadowed_labels(%rip), %rax
	movq	408(%rbx), %rdi
	jmp	.LBB65_1
	.align	16, 0x90
.LBB65_4:                               # %for.inc
                                        #   in Loop: Header=BB65_1 Depth=1
	movq	(%rax), %rax
.LBB65_1:                               # %entry
                                        # =>This Inner Loop Header: Depth=1
	testq	%rax, %rax
	je	.LBB65_5
# BB#2:                                 # %for.body
                                        #   in Loop: Header=BB65_1 Depth=1
	movq	32(%rax), %rcx
	movq	72(%rcx), %rdx
	testq	%rdx, %rdx
	je	.LBB65_4
# BB#3:                                 # %if.then
                                        #   in Loop: Header=BB65_1 Depth=1
	movq	%rcx, 72(%rdx)
	jmp	.LBB65_4
.LBB65_5:                               # %for.end
	movq	current_function_decl(%rip), %rax
	cmpq	$0, 160(%rax)
	jne	.LBB65_8
# BB#6:                                 # %land.lhs.true
	cmpq	$0, 168(%rax)
	jne	.LBB65_8
# BB#7:                                 # %if.then12
	movq	global_trees(%rip), %rcx
	movq	%rcx, 104(%rax)
	movq	current_function_decl(%rip), %rax
	movq	$0, 88(%rax)
.LBB65_8:                               # %if.end15
	movq	24(%rdi), %rax
	movq	%rax, c_stmt_tree+24(%rip)
	movq	16(%rdi), %rax
	movq	%rax, c_stmt_tree+16(%rip)
	movq	(%rdi), %rax
	movq	8(%rdi), %rcx
	movq	%rcx, c_stmt_tree+8(%rip)
	movq	%rax, c_stmt_tree(%rip)
	movq	32(%rdi), %rax
	movq	%rax, c_scope_stmt_stack(%rip)
	movq	40(%rdi), %rax
	movq	%rax, named_labels(%rip)
	movq	48(%rdi), %rax
	movq	%rax, shadowed_labels(%rip)
	movl	56(%rdi), %eax
	movl	%eax, current_function_returns_value(%rip)
	movl	60(%rdi), %eax
	movl	%eax, current_function_returns_null(%rip)
	movl	64(%rdi), %eax
	movl	%eax, current_function_returns_abnormally(%rip)
	movl	68(%rdi), %eax
	movl	%eax, warn_about_return_type(%rip)
	movl	72(%rdi), %eax
	movl	%eax, current_extern_inline(%rip)
	movq	80(%rdi), %rax
	movq	%rax, current_binding_level(%rip)
	callq	free
	movq	$0, 408(%rbx)
	popq	%rbx
	retq
.Ltmp624:
	.size	pop_c_function_context, .Ltmp624-pop_c_function_context
	.cfi_endproc

	.globl	mark_c_function_context
	.align	16, 0x90
	.type	mark_c_function_context,@function
mark_c_function_context:                # @mark_c_function_context
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp628:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp629:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp630:
	.cfi_def_cfa_offset 32
.Ltmp631:
	.cfi_offset %rbx, -24
.Ltmp632:
	.cfi_offset %r14, -16
	movq	408(%rdi), %rbx
	testq	%rbx, %rbx
	je	.LBB66_12
# BB#1:                                 # %if.end
	movq	%rbx, %rdi
	callq	mark_c_language_function
	movq	48(%rbx), %r14
	testq	%r14, %r14
	je	.LBB66_6
# BB#2:                                 # %land.lhs.true
	movq	%r14, %rdi
	callq	ggc_set_mark
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB66_6
# BB#3:                                 # %do.body3
	movq	ggc_pending_trees(%rip), %rax
	movq	(%rax), %rsi
	movq	8(%rax), %rcx
	cmpq	%rsi, %rcx
	jb	.LBB66_5
# BB#4:                                 # %if.then5
	addq	%rsi, %rsi
	movq	%rax, %rdi
	callq	varray_grow
	movq	%rax, ggc_pending_trees(%rip)
	movq	8(%rax), %rcx
.LBB66_5:                               # %if.end8
	leaq	1(%rcx), %rdx
	movq	%rdx, 8(%rax)
	movq	%r14, 32(%rax,%rcx,8)
.LBB66_6:                               # %do.body12
	movq	40(%rbx), %r14
	testq	%r14, %r14
	je	.LBB66_11
# BB#7:                                 # %land.lhs.true15
	movq	%r14, %rdi
	callq	ggc_set_mark
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB66_11
# BB#8:                                 # %do.body19
	movq	ggc_pending_trees(%rip), %rax
	movq	(%rax), %rsi
	movq	8(%rax), %rcx
	cmpq	%rsi, %rcx
	jb	.LBB66_10
# BB#9:                                 # %if.then23
	addq	%rsi, %rsi
	movq	%rax, %rdi
	callq	varray_grow
	movq	%rax, ggc_pending_trees(%rip)
	movq	8(%rax), %rcx
.LBB66_10:                              # %if.end27
	leaq	1(%rcx), %rdx
	movq	%rdx, 8(%rax)
	movq	%r14, 32(%rax,%rcx,8)
.LBB66_11:                              # %do.end35
	addq	$80, %rbx
	movq	%rbx, %rdi
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	jmp	mark_binding_level  # TAILCALL
.LBB66_12:                              # %return
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp633:
	.size	mark_c_function_context, .Ltmp633-mark_c_function_context
	.cfi_endproc

	.globl	copy_lang_decl
	.align	16, 0x90
	.type	copy_lang_decl,@function
copy_lang_decl:                         # @copy_lang_decl
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp636:
	.cfi_def_cfa_offset 16
.Ltmp637:
	.cfi_offset %rbx, -16
	movq	%rdi, %rbx
	cmpq	$0, 200(%rbx)
	je	.LBB67_2
# BB#1:                                 # %if.end
	movq	$16, %rdi
	callq	ggc_alloc
	movq	200(%rbx), %rcx
	movq	(%rcx), %rdx
	movq	8(%rcx), %rcx
	movq	%rcx, 8(%rax)
	movq	%rdx, (%rax)
	movq	%rax, 200(%rbx)
.LBB67_2:                               # %return
	popq	%rbx
	retq
.Ltmp638:
	.size	copy_lang_decl, .Ltmp638-copy_lang_decl
	.cfi_endproc

	.globl	lang_mark_tree
	.align	16, 0x90
	.type	lang_mark_tree,@function
lang_mark_tree:                         # @lang_mark_tree
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp642:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp643:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp644:
	.cfi_def_cfa_offset 32
.Ltmp645:
	.cfi_offset %rbx, -24
.Ltmp646:
	.cfi_offset %r14, -16
	movq	%rdi, %r14
	movzbl	16(%r14), %eax
	cmpq	$1, %rax
	jne	.LBB68_27
# BB#1:                                 # %if.then
	movq	56(%r14), %rbx
	testq	%rbx, %rbx
	je	.LBB68_6
# BB#2:                                 # %land.lhs.true
	movq	%rbx, %rdi
	callq	ggc_set_mark
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB68_6
# BB#3:                                 # %do.body3
	movq	ggc_pending_trees(%rip), %rax
	movq	(%rax), %rsi
	movq	8(%rax), %rcx
	cmpq	%rsi, %rcx
	jb	.LBB68_5
# BB#4:                                 # %if.then5
	addq	%rsi, %rsi
	movq	%rax, %rdi
	callq	varray_grow
	movq	%rax, ggc_pending_trees(%rip)
	movq	8(%rax), %rcx
.LBB68_5:                               # %if.end
	leaq	1(%rcx), %rdx
	movq	%rdx, 8(%rax)
	movq	%rbx, 32(%rax,%rcx,8)
.LBB68_6:                               # %do.body11
	movq	64(%r14), %rbx
	testq	%rbx, %rbx
	je	.LBB68_11
# BB#7:                                 # %land.lhs.true14
	movq	%rbx, %rdi
	callq	ggc_set_mark
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB68_11
# BB#8:                                 # %do.body18
	movq	ggc_pending_trees(%rip), %rax
	movq	(%rax), %rsi
	movq	8(%rax), %rcx
	cmpq	%rsi, %rcx
	jb	.LBB68_10
# BB#9:                                 # %if.then22
	addq	%rsi, %rsi
	movq	%rax, %rdi
	callq	varray_grow
	movq	%rax, ggc_pending_trees(%rip)
	movq	8(%rax), %rcx
.LBB68_10:                              # %if.end26
	leaq	1(%rcx), %rdx
	movq	%rdx, 8(%rax)
	movq	%rbx, 32(%rax,%rcx,8)
.LBB68_11:                              # %do.body35
	movq	72(%r14), %rbx
	testq	%rbx, %rbx
	je	.LBB68_16
# BB#12:                                # %land.lhs.true38
	movq	%rbx, %rdi
	callq	ggc_set_mark
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB68_16
# BB#13:                                # %do.body42
	movq	ggc_pending_trees(%rip), %rax
	movq	(%rax), %rsi
	movq	8(%rax), %rcx
	cmpq	%rsi, %rcx
	jb	.LBB68_15
# BB#14:                                # %if.then46
	addq	%rsi, %rsi
	movq	%rax, %rdi
	callq	varray_grow
	movq	%rax, ggc_pending_trees(%rip)
	movq	8(%rax), %rcx
.LBB68_15:                              # %if.end50
	leaq	1(%rcx), %rdx
	movq	%rdx, 8(%rax)
	movq	%rbx, 32(%rax,%rcx,8)
.LBB68_16:                              # %do.body59
	movq	80(%r14), %rbx
	testq	%rbx, %rbx
	je	.LBB68_21
# BB#17:                                # %land.lhs.true62
	movq	%rbx, %rdi
	callq	ggc_set_mark
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB68_21
# BB#18:                                # %do.body66
	movq	ggc_pending_trees(%rip), %rax
	movq	(%rax), %rsi
	movq	8(%rax), %rcx
	cmpq	%rsi, %rcx
	jb	.LBB68_20
# BB#19:                                # %if.then70
	addq	%rsi, %rsi
	movq	%rax, %rdi
	callq	varray_grow
	movq	%rax, ggc_pending_trees(%rip)
	movq	8(%rax), %rcx
.LBB68_20:                              # %if.end74
	leaq	1(%rcx), %rdx
	movq	%rdx, 8(%rax)
	movq	%rbx, 32(%rax,%rcx,8)
.LBB68_21:                              # %do.body83
	movq	88(%r14), %rbx
	testq	%rbx, %rbx
	je	.LBB68_26
# BB#22:                                # %land.lhs.true86
	movq	%rbx, %rdi
	callq	ggc_set_mark
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB68_26
# BB#23:                                # %do.body90
	movq	ggc_pending_trees(%rip), %rax
	movq	(%rax), %rsi
	movq	8(%rax), %rcx
	cmpq	%rsi, %rcx
	jb	.LBB68_25
# BB#24:                                # %if.then94
	addq	%rsi, %rsi
	movq	%rax, %rdi
	callq	varray_grow
	movq	%rax, ggc_pending_trees(%rip)
	movq	8(%rax), %rcx
.LBB68_25:                              # %if.end98
	leaq	1(%rcx), %rdx
	movq	%rdx, 8(%rax)
	movq	%rbx, 32(%rax,%rcx,8)
.LBB68_26:                              # %do.body107
	movq	96(%r14), %rbx
	jmp	.LBB68_33
.LBB68_27:                              # %if.else
	movzbl	tree_code_type(%rax), %eax
	cmpq	$100, %rax
	jne	.LBB68_28
# BB#31:                                # %land.lhs.true160
	movq	200(%r14), %rdi
	testq	%rdi, %rdi
	je	.LBB68_38
# BB#32:                                # %if.then170
	callq	ggc_set_mark
	movq	200(%r14), %rdi
	callq	c_mark_lang_decl
	movq	200(%r14), %rax
	movq	8(%rax), %rbx
.LBB68_33:                              # %if.then170
	testq	%rbx, %rbx
	je	.LBB68_38
# BB#34:                                # %land.lhs.true182
	movq	%rbx, %rdi
	callq	ggc_set_mark
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB68_38
# BB#35:                                # %do.body186
	movq	ggc_pending_trees(%rip), %rax
	movq	(%rax), %rsi
	movq	8(%rax), %rcx
	cmpq	%rsi, %rcx
	jb	.LBB68_37
# BB#36:                                # %if.then191
	addq	%rsi, %rsi
	movq	%rax, %rdi
	callq	varray_grow
	movq	%rax, ggc_pending_trees(%rip)
	movq	8(%rax), %rcx
.LBB68_37:                              # %if.end195
	leaq	1(%rcx), %rdx
	movq	%rdx, 8(%rax)
	movq	%rbx, 32(%rax,%rcx,8)
	jmp	.LBB68_38
.LBB68_28:                              # %if.else
	cmpq	$116, %rax
	jne	.LBB68_38
# BB#29:                                # %land.lhs.true138
	movq	160(%r14), %rdi
	testq	%rdi, %rdi
	je	.LBB68_38
# BB#30:                                # %if.then146
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	jmp	ggc_set_mark  # TAILCALL
.LBB68_38:                              # %if.end206
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp647:
	.size	lang_mark_tree, .Ltmp647-lang_mark_tree
	.cfi_endproc

	.globl	stmts_are_full_exprs_p
	.align	16, 0x90
	.type	stmts_are_full_exprs_p,@function
stmts_are_full_exprs_p:                 # @stmts_are_full_exprs_p
	.cfi_startproc
# BB#0:                                 # %entry
	xorq	%rax, %rax
	retq
.Ltmp648:
	.size	stmts_are_full_exprs_p, .Ltmp648-stmts_are_full_exprs_p
	.cfi_endproc

	.globl	current_stmt_tree
	.align	16, 0x90
	.type	current_stmt_tree,@function
current_stmt_tree:                      # @current_stmt_tree
	.cfi_startproc
# BB#0:                                 # %entry
	movabsq	$c_stmt_tree, %rax
	retq
.Ltmp649:
	.size	current_stmt_tree, .Ltmp649-current_stmt_tree
	.cfi_endproc

	.globl	current_scope_stmt_stack
	.align	16, 0x90
	.type	current_scope_stmt_stack,@function
current_scope_stmt_stack:               # @current_scope_stmt_stack
	.cfi_startproc
# BB#0:                                 # %entry
	movabsq	$c_scope_stmt_stack, %rax
	retq
.Ltmp650:
	.size	current_scope_stmt_stack, .Ltmp650-current_scope_stmt_stack
	.cfi_endproc

	.globl	anon_aggr_type_p
	.align	16, 0x90
	.type	anon_aggr_type_p,@function
anon_aggr_type_p:                       # @anon_aggr_type_p
	.cfi_startproc
# BB#0:                                 # %entry
	xorq	%rax, %rax
	retq
.Ltmp651:
	.size	anon_aggr_type_p, .Ltmp651-anon_aggr_type_p
	.cfi_endproc

	.globl	extract_interface_info
	.align	16, 0x90
	.type	extract_interface_info,@function
extract_interface_info:                 # @extract_interface_info
	.cfi_startproc
# BB#0:                                 # %entry
	retq
.Ltmp652:
	.size	extract_interface_info, .Ltmp652-extract_interface_info
	.cfi_endproc

	.globl	c_begin_compound_stmt
	.align	16, 0x90
	.type	c_begin_compound_stmt,@function
c_begin_compound_stmt:                  # @c_begin_compound_stmt
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rax
.Ltmp654:
	.cfi_def_cfa_offset 16
	movq	$153, %rdi
	xorq	%rsi, %rsi
	xorq	%rax, %rax
	callq	build_stmt
	movq	%rax, %rdi
	popq	%rax
	jmp	add_stmt  # TAILCALL
.Ltmp655:
	.size	c_begin_compound_stmt, .Ltmp655-c_begin_compound_stmt
	.cfi_endproc

	.globl	c_expand_decl_stmt
	.align	16, 0x90
	.type	c_expand_decl_stmt,@function
c_expand_decl_stmt:                     # @c_expand_decl_stmt
	.cfi_startproc
# BB#0:                                 # %entry
	movq	32(%rdi), %rdi
	movzbl	16(%rdi), %eax
	cmpq	$30, %rax
	jne	.LBB75_3
# BB#1:                                 # %land.lhs.true
	movq	80(%rdi), %rax
	movq	current_function_decl(%rip), %rcx
	cmpq	%rcx, %rax
	jne	.LBB75_3
# BB#2:                                 # %land.lhs.true3
	cmpq	$0, 168(%rdi)
	je	.LBB75_3
# BB#4:                                 # %if.then
	movq	$1, %rsi
	xorq	%rdx, %rdx
	jmp	c_expand_body  # TAILCALL
.LBB75_3:                               # %if.end
	retq
.Ltmp656:
	.size	c_expand_decl_stmt, .Ltmp656-c_expand_decl_stmt
	.cfi_endproc

	.globl	identifier_global_value
	.align	16, 0x90
	.type	identifier_global_value,@function
identifier_global_value:                # @identifier_global_value
	.cfi_startproc
# BB#0:                                 # %entry
	movq	56(%rdi), %rax
	retq
.Ltmp657:
	.size	identifier_global_value, .Ltmp657-identifier_global_value
	.cfi_endproc

	.globl	record_builtin_type
	.align	16, 0x90
	.type	record_builtin_type,@function
record_builtin_type:                    # @record_builtin_type
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp660:
	.cfi_def_cfa_offset 16
.Ltmp661:
	.cfi_offset %rbx, -16
	movq	%rdx, %rbx
	testq	%rsi, %rsi
	je	.LBB77_1
# BB#2:                                 # %if.else
	movq	%rsi, %rdi
	callq	get_identifier
	movq	%rax, %rsi
	jmp	.LBB77_3
.LBB77_1:                               # %if.then
	movq	$32, %rcx
	movq	ridpointers(%rip), %rax
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	movq	(%rax,%rdi,8), %rsi
.LBB77_3:                               # %if.end
	movq	$33, %rdi
	movq	%rbx, %rdx
	callq	build_decl
	movq	%rax, %rdi
	popq	%rbx
	jmp	pushdecl  # TAILCALL
.Ltmp662:
	.size	record_builtin_type, .Ltmp662-record_builtin_type
	.cfi_endproc

	.globl	build_void_list_node
	.align	16, 0x90
	.type	build_void_list_node,@function
build_void_list_node:                   # @build_void_list_node
	.cfi_startproc
# BB#0:                                 # %entry
	movq	global_trees+216(%rip), %rsi
	xorq	%rdi, %rdi
	jmp	build_tree_list  # TAILCALL
.Ltmp663:
	.size	build_void_list_node, .Ltmp663-build_void_list_node
	.cfi_endproc

	.type	flag_isoc94,@object     # @flag_isoc94
	.bss
	.globl	flag_isoc94
	.align	4
flag_isoc94:
	.long	0                       # 0x0
	.size	flag_isoc94, 4

	.type	flag_isoc99,@object     # @flag_isoc99
	.globl	flag_isoc99
	.align	4
flag_isoc99:
	.long	0                       # 0x0
	.size	flag_isoc99, 4

	.type	flag_hosted,@object     # @flag_hosted
	.data
	.globl	flag_hosted
	.align	4
flag_hosted:
	.long	1                       # 0x1
	.size	flag_hosted, 4

	.type	flag_noniso_default_format_attributes,@object # @flag_noniso_default_format_attributes
	.globl	flag_noniso_default_format_attributes
	.align	4
flag_noniso_default_format_attributes:
	.long	1                       # 0x1
	.size	flag_noniso_default_format_attributes, 4

	.type	flag_allow_single_precision,@object # @flag_allow_single_precision
	.bss
	.globl	flag_allow_single_precision
	.align	4
flag_allow_single_precision:
	.long	0                       # 0x0
	.size	flag_allow_single_precision, 4

	.type	flag_signed_bitfields,@object # @flag_signed_bitfields
	.data
	.globl	flag_signed_bitfields
	.align	4
flag_signed_bitfields:
	.long	1                       # 0x1
	.size	flag_signed_bitfields, 4

	.type	explicit_flag_signed_bitfields,@object # @explicit_flag_signed_bitfields
	.bss
	.globl	explicit_flag_signed_bitfields
	.align	4
explicit_flag_signed_bitfields:
	.long	0                       # 0x0
	.size	explicit_flag_signed_bitfields, 4

	.type	warn_long_long,@object  # @warn_long_long
	.data
	.globl	warn_long_long
	.align	4
warn_long_long:
	.long	1                       # 0x1
	.size	warn_long_long, 4

	.type	mesg_implicit_function_declaration,@object # @mesg_implicit_function_declaration
	.globl	mesg_implicit_function_declaration
	.align	4
mesg_implicit_function_declaration:
	.long	4294967295              # 0xffffffff
	.size	mesg_implicit_function_declaration, 4

	.type	warn_redundant_decls,@object # @warn_redundant_decls
	.bss
	.globl	warn_redundant_decls
	.align	4
warn_redundant_decls:
	.long	0                       # 0x0
	.size	warn_redundant_decls, 4

	.type	warn_nested_externs,@object # @warn_nested_externs
	.globl	warn_nested_externs
	.align	4
warn_nested_externs:
	.long	0                       # 0x0
	.size	warn_nested_externs, 4

	.type	warn_char_subscripts,@object # @warn_char_subscripts
	.globl	warn_char_subscripts
	.align	4
warn_char_subscripts:
	.long	0                       # 0x0
	.size	warn_char_subscripts, 4

	.type	warn_unknown_pragmas,@object # @warn_unknown_pragmas
	.globl	warn_unknown_pragmas
	.align	4
warn_unknown_pragmas:
	.long	0                       # 0x0
	.size	warn_unknown_pragmas, 4

	.type	warn_sign_compare,@object # @warn_sign_compare
	.data
	.globl	warn_sign_compare
	.align	4
warn_sign_compare:
	.long	4294967295              # 0xffffffff
	.size	warn_sign_compare, 4

	.type	warn_float_equal,@object # @warn_float_equal
	.bss
	.globl	warn_float_equal
	.align	4
warn_float_equal:
	.long	0                       # 0x0
	.size	warn_float_equal, 4

	.type	warn_multichar,@object  # @warn_multichar
	.data
	.globl	warn_multichar
	.align	4
warn_multichar:
	.long	1                       # 0x1
	.size	warn_multichar, 4

	.type	dollars_in_ident,@object # @dollars_in_ident
	.globl	dollars_in_ident
	.align	4
dollars_in_ident:
	.long	1                       # 0x1
	.size	dollars_in_ident, 4

	.type	.L.str,@object          # @.str
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.asciz	"-ftraditional"
	.size	.L.str, 14

	.type	.L.str1,@object         # @.str1
.L.str1:
	.asciz	"-traditional"
	.size	.L.str1, 13

	.type	.L.str2,@object         # @.str2
.L.str2:
	.asciz	"-traditional is deprecated and may be removed"
	.size	.L.str2, 46

	.type	flag_traditional,@object # @flag_traditional
	.comm	flag_traditional,4,4
	.type	.L.str3,@object         # @.str3
.L.str3:
	.asciz	"-fallow-single-precision"
	.size	.L.str3, 25

	.type	.L.str4,@object         # @.str4
.L.str4:
	.asciz	"-fhosted"
	.size	.L.str4, 9

	.type	.L.str5,@object         # @.str5
.L.str5:
	.asciz	"-fno-freestanding"
	.size	.L.str5, 18

	.type	.L.str6,@object         # @.str6
.L.str6:
	.asciz	"-ffreestanding"
	.size	.L.str6, 15

	.type	.L.str7,@object         # @.str7
.L.str7:
	.asciz	"-fno-hosted"
	.size	.L.str7, 12

	.type	warn_main,@object       # @warn_main
	.comm	warn_main,4,4
	.type	.L.str8,@object         # @.str8
.L.str8:
	.asciz	"-fnotraditional"
	.size	.L.str8, 16

	.type	.L.str9,@object         # @.str9
.L.str9:
	.asciz	"-fno-traditional"
	.size	.L.str9, 17

	.type	.L.str10,@object        # @.str10
.L.str10:
	.asciz	"-std="
	.size	.L.str10, 6

	.type	.L.str11,@object        # @.str11
.L.str11:
	.asciz	"iso9899:1990"
	.size	.L.str11, 13

	.type	.L.str12,@object        # @.str12
.L.str12:
	.asciz	"c89"
	.size	.L.str12, 4

	.type	flag_no_asm,@object     # @flag_no_asm
	.comm	flag_no_asm,4,4
	.type	.L.str13,@object        # @.str13
.L.str13:
	.asciz	"iso9899:199409"
	.size	.L.str13, 15

	.type	.L.str14,@object        # @.str14
.L.str14:
	.asciz	"iso9899:199x"
	.size	.L.str14, 13

	.type	.L.str15,@object        # @.str15
.L.str15:
	.asciz	"iso9899:1999"
	.size	.L.str15, 13

	.type	.L.str16,@object        # @.str16
.L.str16:
	.asciz	"c9x"
	.size	.L.str16, 4

	.type	.L.str17,@object        # @.str17
.L.str17:
	.asciz	"c99"
	.size	.L.str17, 4

	.type	.L.str18,@object        # @.str18
.L.str18:
	.asciz	"gnu89"
	.size	.L.str18, 6

	.type	.L.str19,@object        # @.str19
.L.str19:
	.asciz	"gnu9x"
	.size	.L.str19, 6

	.type	.L.str20,@object        # @.str20
.L.str20:
	.asciz	"gnu99"
	.size	.L.str20, 6

	.type	.L.str21,@object        # @.str21
.L.str21:
	.asciz	"unknown C standard `%s'"
	.size	.L.str21, 24

	.type	.L.str22,@object        # @.str22
.L.str22:
	.asciz	"-fdollars-in-identifiers"
	.size	.L.str22, 25

	.type	.L.str23,@object        # @.str23
.L.str23:
	.asciz	"-fno-dollars-in-identifiers"
	.size	.L.str23, 28

	.type	.L.str24,@object        # @.str24
.L.str24:
	.asciz	"-fsigned-char"
	.size	.L.str24, 14

	.type	.L.str25,@object        # @.str25
.L.str25:
	.asciz	"-funsigned-char"
	.size	.L.str25, 16

	.type	.L.str26,@object        # @.str26
.L.str26:
	.asciz	"-fno-signed-char"
	.size	.L.str26, 17

	.type	.L.str27,@object        # @.str27
.L.str27:
	.asciz	"-fno-unsigned-char"
	.size	.L.str27, 19

	.type	.L.str28,@object        # @.str28
.L.str28:
	.asciz	"-fsigned-bitfields"
	.size	.L.str28, 19

	.type	.L.str29,@object        # @.str29
.L.str29:
	.asciz	"-fno-unsigned-bitfields"
	.size	.L.str29, 24

	.type	.L.str30,@object        # @.str30
.L.str30:
	.asciz	"-funsigned-bitfields"
	.size	.L.str30, 21

	.type	.L.str31,@object        # @.str31
.L.str31:
	.asciz	"-fno-signed-bitfields"
	.size	.L.str31, 22

	.type	.L.str32,@object        # @.str32
.L.str32:
	.asciz	"-fshort-enums"
	.size	.L.str32, 14

	.type	.L.str33,@object        # @.str33
.L.str33:
	.asciz	"-fno-short-enums"
	.size	.L.str33, 17

	.type	.L.str34,@object        # @.str34
.L.str34:
	.asciz	"-fshort-wchar"
	.size	.L.str34, 14

	.type	.L.str35,@object        # @.str35
.L.str35:
	.asciz	"-fno-short-wchar"
	.size	.L.str35, 17

	.type	.L.str36,@object        # @.str36
.L.str36:
	.asciz	"-fcond-mismatch"
	.size	.L.str36, 16

	.type	flag_cond_mismatch,@object # @flag_cond_mismatch
	.comm	flag_cond_mismatch,4,4
	.type	.L.str37,@object        # @.str37
.L.str37:
	.asciz	"-fno-cond-mismatch"
	.size	.L.str37, 19

	.type	.L.str38,@object        # @.str38
.L.str38:
	.asciz	"-fshort-double"
	.size	.L.str38, 15

	.type	.L.str39,@object        # @.str39
.L.str39:
	.asciz	"-fno-short-double"
	.size	.L.str39, 18

	.type	.L.str40,@object        # @.str40
.L.str40:
	.asciz	"-fasm"
	.size	.L.str40, 6

	.type	.L.str41,@object        # @.str41
.L.str41:
	.asciz	"-fno-asm"
	.size	.L.str41, 9

	.type	.L.str42,@object        # @.str42
.L.str42:
	.asciz	"-fbuiltin"
	.size	.L.str42, 10

	.type	.L.str43,@object        # @.str43
.L.str43:
	.asciz	"-fno-builtin"
	.size	.L.str43, 13

	.type	.L.str44,@object        # @.str44
.L.str44:
	.asciz	"-fno-builtin-"
	.size	.L.str44, 14

	.type	.L.str45,@object        # @.str45
.L.str45:
	.asciz	"-ansi"
	.size	.L.str45, 6

	.type	.L.str46,@object        # @.str46
.L.str46:
	.asciz	"-Werror-implicit-function-declaration"
	.size	.L.str46, 38

	.type	.L.str47,@object        # @.str47
.L.str47:
	.asciz	"-Wimplicit-function-declaration"
	.size	.L.str47, 32

	.type	.L.str48,@object        # @.str48
.L.str48:
	.asciz	"-Wno-implicit-function-declaration"
	.size	.L.str48, 35

	.type	.L.str49,@object        # @.str49
.L.str49:
	.asciz	"-Wimplicit-int"
	.size	.L.str49, 15

	.type	warn_implicit_int,@object # @warn_implicit_int
	.comm	warn_implicit_int,4,4
	.type	.L.str50,@object        # @.str50
.L.str50:
	.asciz	"-Wno-implicit-int"
	.size	.L.str50, 18

	.type	.L.str51,@object        # @.str51
.L.str51:
	.asciz	"-Wimplicit"
	.size	.L.str51, 11

	.type	.L.str52,@object        # @.str52
.L.str52:
	.asciz	"-Wno-implicit"
	.size	.L.str52, 14

	.type	.L.str53,@object        # @.str53
.L.str53:
	.asciz	"-Wlong-long"
	.size	.L.str53, 12

	.type	.L.str54,@object        # @.str54
.L.str54:
	.asciz	"-Wno-long-long"
	.size	.L.str54, 15

	.type	.L.str55,@object        # @.str55
.L.str55:
	.asciz	"-Wwrite-strings"
	.size	.L.str55, 16

	.type	flag_const_strings,@object # @flag_const_strings
	.comm	flag_const_strings,4,4
	.type	.L.str56,@object        # @.str56
.L.str56:
	.asciz	"-Wno-write-strings"
	.size	.L.str56, 19

	.type	.L.str57,@object        # @.str57
.L.str57:
	.asciz	"-Wcast-qual"
	.size	.L.str57, 12

	.type	warn_cast_qual,@object  # @warn_cast_qual
	.comm	warn_cast_qual,4,4
	.type	.L.str58,@object        # @.str58
.L.str58:
	.asciz	"-Wno-cast-qual"
	.size	.L.str58, 15

	.type	.L.str59,@object        # @.str59
.L.str59:
	.asciz	"-Wbad-function-cast"
	.size	.L.str59, 20

	.type	warn_bad_function_cast,@object # @warn_bad_function_cast
	.comm	warn_bad_function_cast,4,4
	.type	.L.str60,@object        # @.str60
.L.str60:
	.asciz	"-Wno-bad-function-cast"
	.size	.L.str60, 23

	.type	.L.str61,@object        # @.str61
.L.str61:
	.asciz	"-Wno-missing-noreturn"
	.size	.L.str61, 22

	.type	.L.str62,@object        # @.str62
.L.str62:
	.asciz	"-Wmissing-format-attribute"
	.size	.L.str62, 27

	.type	warn_missing_format_attribute,@object # @warn_missing_format_attribute
	.comm	warn_missing_format_attribute,4,4
	.type	.L.str63,@object        # @.str63
.L.str63:
	.asciz	"-Wno-missing-format-attribute"
	.size	.L.str63, 30

	.type	.L.str64,@object        # @.str64
.L.str64:
	.asciz	"-Wpointer-arith"
	.size	.L.str64, 16

	.type	warn_pointer_arith,@object # @warn_pointer_arith
	.comm	warn_pointer_arith,4,4
	.type	.L.str65,@object        # @.str65
.L.str65:
	.asciz	"-Wno-pointer-arith"
	.size	.L.str65, 19

	.type	.L.str66,@object        # @.str66
.L.str66:
	.asciz	"-Wstrict-prototypes"
	.size	.L.str66, 20

	.type	warn_strict_prototypes,@object # @warn_strict_prototypes
	.comm	warn_strict_prototypes,4,4
	.type	.L.str67,@object        # @.str67
.L.str67:
	.asciz	"-Wno-strict-prototypes"
	.size	.L.str67, 23

	.type	.L.str68,@object        # @.str68
.L.str68:
	.asciz	"-Wmissing-prototypes"
	.size	.L.str68, 21

	.type	warn_missing_prototypes,@object # @warn_missing_prototypes
	.comm	warn_missing_prototypes,4,4
	.type	.L.str69,@object        # @.str69
.L.str69:
	.asciz	"-Wno-missing-prototypes"
	.size	.L.str69, 24

	.type	.L.str70,@object        # @.str70
.L.str70:
	.asciz	"-Wmissing-declarations"
	.size	.L.str70, 23

	.type	warn_missing_declarations,@object # @warn_missing_declarations
	.comm	warn_missing_declarations,4,4
	.type	.L.str71,@object        # @.str71
.L.str71:
	.asciz	"-Wno-missing-declarations"
	.size	.L.str71, 26

	.type	.L.str72,@object        # @.str72
.L.str72:
	.asciz	"-Wredundant-decls"
	.size	.L.str72, 18

	.type	.L.str73,@object        # @.str73
.L.str73:
	.asciz	"-Wno-redundant-decls"
	.size	.L.str73, 21

	.type	.L.str74,@object        # @.str74
.L.str74:
	.asciz	"-Wnested-externs"
	.size	.L.str74, 17

	.type	.L.str75,@object        # @.str75
.L.str75:
	.asciz	"-Wno-nested-externs"
	.size	.L.str75, 20

	.type	.L.str76,@object        # @.str76
.L.str76:
	.asciz	"-Wtraditional"
	.size	.L.str76, 14

	.type	warn_traditional,@object # @warn_traditional
	.comm	warn_traditional,4,4
	.type	.L.str77,@object        # @.str77
.L.str77:
	.asciz	"-Wno-traditional"
	.size	.L.str77, 17

	.type	.L.str78,@object        # @.str78
.L.str78:
	.asciz	"-Wformat="
	.size	.L.str78, 10

	.type	.L.str79,@object        # @.str79
.L.str79:
	.asciz	"-Wformat"
	.size	.L.str79, 9

	.type	.L.str80,@object        # @.str80
.L.str80:
	.asciz	"-Wno-format"
	.size	.L.str80, 12

	.type	.L.str81,@object        # @.str81
.L.str81:
	.asciz	"-Wformat-y2k"
	.size	.L.str81, 13

	.type	.L.str82,@object        # @.str82
.L.str82:
	.asciz	"-Wno-format-y2k"
	.size	.L.str82, 16

	.type	.L.str83,@object        # @.str83
.L.str83:
	.asciz	"-Wformat-extra-args"
	.size	.L.str83, 20

	.type	.L.str84,@object        # @.str84
.L.str84:
	.asciz	"-Wno-format-extra-args"
	.size	.L.str84, 23

	.type	.L.str85,@object        # @.str85
.L.str85:
	.asciz	"-Wformat-nonliteral"
	.size	.L.str85, 20

	.type	.L.str86,@object        # @.str86
.L.str86:
	.asciz	"-Wno-format-nonliteral"
	.size	.L.str86, 23

	.type	.L.str87,@object        # @.str87
.L.str87:
	.asciz	"-Wformat-security"
	.size	.L.str87, 18

	.type	.L.str88,@object        # @.str88
.L.str88:
	.asciz	"-Wno-format-security"
	.size	.L.str88, 21

	.type	.L.str89,@object        # @.str89
.L.str89:
	.asciz	"-Wchar-subscripts"
	.size	.L.str89, 18

	.type	.L.str90,@object        # @.str90
.L.str90:
	.asciz	"-Wno-char-subscripts"
	.size	.L.str90, 21

	.type	.L.str91,@object        # @.str91
.L.str91:
	.asciz	"-Wconversion"
	.size	.L.str91, 13

	.type	warn_conversion,@object # @warn_conversion
	.comm	warn_conversion,4,4
	.type	.L.str92,@object        # @.str92
.L.str92:
	.asciz	"-Wno-conversion"
	.size	.L.str92, 16

	.type	.L.str93,@object        # @.str93
.L.str93:
	.asciz	"-Wparentheses"
	.size	.L.str93, 14

	.type	warn_parentheses,@object # @warn_parentheses
	.comm	warn_parentheses,4,4
	.type	.L.str94,@object        # @.str94
.L.str94:
	.asciz	"-Wno-parentheses"
	.size	.L.str94, 17

	.type	.L.str95,@object        # @.str95
.L.str95:
	.asciz	"-Wreturn-type"
	.size	.L.str95, 14

	.type	.L.str96,@object        # @.str96
.L.str96:
	.asciz	"-Wno-return-type"
	.size	.L.str96, 17

	.type	.L.str97,@object        # @.str97
.L.str97:
	.asciz	"-Wsequence-point"
	.size	.L.str97, 17

	.type	.L.str98,@object        # @.str98
.L.str98:
	.asciz	"-Wno-sequence-point"
	.size	.L.str98, 20

	.type	.L.str99,@object        # @.str99
.L.str99:
	.asciz	"-Wcomment"
	.size	.L.str99, 10

	.type	.L.str100,@object       # @.str100
.L.str100:
	.asciz	"-Wno-comment"
	.size	.L.str100, 13

	.type	.L.str101,@object       # @.str101
.L.str101:
	.asciz	"-Wcomments"
	.size	.L.str101, 11

	.type	.L.str102,@object       # @.str102
.L.str102:
	.asciz	"-Wno-comments"
	.size	.L.str102, 14

	.type	.L.str103,@object       # @.str103
.L.str103:
	.asciz	"-Wtrigraphs"
	.size	.L.str103, 12

	.type	.L.str104,@object       # @.str104
.L.str104:
	.asciz	"-Wno-trigraphs"
	.size	.L.str104, 15

	.type	.L.str105,@object       # @.str105
.L.str105:
	.asciz	"-Wundef"
	.size	.L.str105, 8

	.type	.L.str106,@object       # @.str106
.L.str106:
	.asciz	"-Wno-undef"
	.size	.L.str106, 11

	.type	.L.str107,@object       # @.str107
.L.str107:
	.asciz	"-Wimport"
	.size	.L.str107, 9

	.type	.L.str108,@object       # @.str108
.L.str108:
	.asciz	"-Wno-import"
	.size	.L.str108, 12

	.type	.L.str109,@object       # @.str109
.L.str109:
	.asciz	"-Wmissing-braces"
	.size	.L.str109, 17

	.type	warn_missing_braces,@object # @warn_missing_braces
	.comm	warn_missing_braces,4,4
	.type	.L.str110,@object       # @.str110
.L.str110:
	.asciz	"-Wno-missing-braces"
	.size	.L.str110, 20

	.type	.L.str111,@object       # @.str111
.L.str111:
	.asciz	"-Wmain"
	.size	.L.str111, 7

	.type	.L.str112,@object       # @.str112
.L.str112:
	.asciz	"-Wno-main"
	.size	.L.str112, 10

	.type	.L.str113,@object       # @.str113
.L.str113:
	.asciz	"-Wsign-compare"
	.size	.L.str113, 15

	.type	.L.str114,@object       # @.str114
.L.str114:
	.asciz	"-Wno-sign-compare"
	.size	.L.str114, 18

	.type	.L.str115,@object       # @.str115
.L.str115:
	.asciz	"-Wfloat-equal"
	.size	.L.str115, 14

	.type	.L.str116,@object       # @.str116
.L.str116:
	.asciz	"-Wno-float-equal"
	.size	.L.str116, 17

	.type	.L.str117,@object       # @.str117
.L.str117:
	.asciz	"-Wmultichar"
	.size	.L.str117, 12

	.type	.L.str118,@object       # @.str118
.L.str118:
	.asciz	"-Wno-multichar"
	.size	.L.str118, 15

	.type	.L.str119,@object       # @.str119
.L.str119:
	.asciz	"-Wdiv-by-zero"
	.size	.L.str119, 14

	.type	.L.str120,@object       # @.str120
.L.str120:
	.asciz	"-Wno-div-by-zero"
	.size	.L.str120, 17

	.type	.L.str121,@object       # @.str121
.L.str121:
	.asciz	"-Wunknown-pragmas"
	.size	.L.str121, 18

	.type	.L.str122,@object       # @.str122
.L.str122:
	.asciz	"-Wno-unknown-pragmas"
	.size	.L.str122, 21

	.type	.L.str123,@object       # @.str123
.L.str123:
	.asciz	"-Wall"
	.size	.L.str123, 6

	.type	.L.str124,@object       # @.str124
.L.str124:
	.asciz	"global"
	.size	.L.str124, 7

	.type	.L.str125,@object       # @.str125
.L.str125:
	.asciz	"local"
	.size	.L.str125, 6

	.type	.L.str126,@object       # @.str126
.L.str126:
	.asciz	"label"
	.size	.L.str126, 6

	.type	.L.str127,@object       # @.str127
.L.str127:
	.asciz	"implicit"
	.size	.L.str127, 9

	.type	.L.str128,@object       # @.str128
.L.str128:
	.asciz	"error locus"
	.size	.L.str128, 12

	.type	.L.str129,@object       # @.str129
.L.str129:
	.asciz	"limbo value"
	.size	.L.str129, 12

	.type	.L.str130,@object       # @.str130
.L.str130:
	.asciz	"rid "
	.size	.L.str130, 5

	.type	.L.str131,@object       # @.str131
.L.str131:
	.asciz	"%p"
	.size	.L.str131, 3

	.type	.L.str132,@object       # @.str132
.L.str132:
	.asciz	" \"%s\""
	.size	.L.str132, 6

	.type	.L.str133,@object       # @.str133
.L.str133:
	.asciz	"array `%s' assumed to have one element"
	.size	.L.str133, 39

	.type	current_binding_level,@object # @current_binding_level
	.local	current_binding_level
	.comm	current_binding_level,8,8
	.type	global_binding_level,@object # @global_binding_level
	.local	global_binding_level
	.comm	global_binding_level,8,8
	.type	keep_next_level_flag,@object # @keep_next_level_flag
	.local	keep_next_level_flag
	.comm	keep_next_level_flag,1,1
	.type	named_labels,@object    # @named_labels
	.local	named_labels
	.comm	named_labels,8,8
	.type	free_binding_level,@object # @free_binding_level
	.local	free_binding_level
	.comm	free_binding_level,8,8
	.type	keep_next_if_subblocks,@object # @keep_next_if_subblocks
	.local	keep_next_if_subblocks
	.comm	keep_next_if_subblocks,1,1
	.type	.L.str134,@object       # @.str134
.L.str134:
	.asciz	"label `%s' used but not defined"
	.size	.L.str134, 32

	.type	.L.str135,@object       # @.str135
.L.str135:
	.asciz	"label `%s' defined but not used"
	.size	.L.str135, 32

	.type	label_level_chain,@object # @label_level_chain
	.local	label_level_chain
	.comm	label_level_chain,8,8
	.type	shadowed_labels,@object # @shadowed_labels
	.local	shadowed_labels
	.comm	shadowed_labels,8,8
	.type	.L.str136,@object       # @.str136
.L.str136:
	.asciz	"nested extern declaration of `%s'"
	.size	.L.str136, 34

	.type	.L.str137,@object       # @.str137
.L.str137:
	.asciz	"`%s' used prior to declaration"
	.size	.L.str137, 31

	.type	.L.str138,@object       # @.str138
.L.str138:
	.asciz	"`%s' was declared implicitly `extern' and later `static'"
	.size	.L.str138, 57

	.type	.L.str139,@object       # @.str139
.L.str139:
	.asciz	"previous declaration of `%s'"
	.size	.L.str139, 29

	.type	.L.str140,@object       # @.str140
.L.str140:
	.asciz	"type mismatch with previous external decl"
	.size	.L.str140, 42

	.type	.L.str141,@object       # @.str141
.L.str141:
	.asciz	"previous external decl of `%s'"
	.size	.L.str141, 31

	.type	.L.str142,@object       # @.str142
.L.str142:
	.asciz	"type mismatch with previous implicit declaration"
	.size	.L.str142, 49

	.type	.L.str143,@object       # @.str143
.L.str143:
	.asciz	"previous implicit declaration of `%s'"
	.size	.L.str143, 38

	.type	.L.str144,@object       # @.str144
.L.str144:
	.asciz	"type of external `%s' is not global"
	.size	.L.str144, 36

	.type	.L.str145,@object       # @.str145
.L.str145:
	.asciz	"`%s' was previously implicitly declared to return `int'"
	.size	.L.str145, 56

	.type	.L.str146,@object       # @.str146
.L.str146:
	.asciz	"`%s' was declared `extern' and later `static'"
	.size	.L.str146, 46

	.type	.L.str147,@object       # @.str147
.L.str147:
	.asciz	"extern declaration of `%s' doesn't match global one"
	.size	.L.str147, 52

	.type	.L.str148,@object       # @.str148
.L.str148:
	.asciz	"function `%s' was previously declared within a block"
	.size	.L.str148, 53

	.type	.L.str149,@object       # @.str149
.L.str149:
	.asciz	"implicit declaration of function `%s'"
	.size	.L.str149, 38

	.type	.L.str150,@object       # @.str150
.L.str150:
	.asciz	"label %s referenced outside of any function"
	.size	.L.str150, 44

	.type	.L.str151,@object       # @.str151
.L.str151:
	.asciz	"duplicate label declaration `%s'"
	.size	.L.str151, 33

	.type	.L.str152,@object       # @.str152
.L.str152:
	.asciz	"this is a previous declaration"
	.size	.L.str152, 31

	.type	.L.str153,@object       # @.str153
.L.str153:
	.asciz	"traditional C lacks a separate namespace for labels, identifier `%s' conflicts"
	.size	.L.str153, 79

	.type	.L.str154,@object       # @.str154
.L.str154:
	.asciz	"duplicate label `%s'"
	.size	.L.str154, 21

	.type	pending_invalid_xref,@object # @pending_invalid_xref
	.comm	pending_invalid_xref,8,8
	.type	pending_invalid_xref_file,@object # @pending_invalid_xref_file
	.comm	pending_invalid_xref_file,8,8
	.type	pending_invalid_xref_line,@object # @pending_invalid_xref_line
	.comm	pending_invalid_xref_line,4,4
	.type	.L.str155,@object       # @.str155
.L.str155:
	.asciz	"`%s' defined as wrong kind of tag"
	.size	.L.str155, 34

	.type	.L.str156,@object       # @.str156
.L.str156:
	.asciz	"_Bool"
	.size	.L.str156, 6

	.type	.L.str157,@object       # @.str157
.L.str157:
	.asciz	"__g77_integer"
	.size	.L.str157, 14

	.type	.L.str158,@object       # @.str158
.L.str158:
	.asciz	"__g77_uinteger"
	.size	.L.str158, 15

	.type	.L.str159,@object       # @.str159
.L.str159:
	.asciz	"__g77_longint"
	.size	.L.str159, 14

	.type	.L.str160,@object       # @.str160
.L.str160:
	.asciz	"__g77_ulongint"
	.size	.L.str160, 15

	.type	c_stmt_tree,@object     # @c_stmt_tree
	.local	c_stmt_tree
	.comm	c_stmt_tree,32,8
	.type	c_scope_stmt_stack,@object # @c_scope_stmt_stack
	.local	c_scope_stmt_stack
	.comm	c_scope_stmt_stack,8,8
	.type	static_ctors,@object    # @static_ctors
	.comm	static_ctors,8,8
	.type	static_dtors,@object    # @static_dtors
	.comm	static_dtors,8,8
	.type	.L.str161,@object       # @.str161
.L.str161:
	.asciz	"unnamed struct/union that defines no instances"
	.size	.L.str161, 47

	.type	.L.str162,@object       # @.str162
.L.str162:
	.asciz	"useless keyword or type name in empty declaration"
	.size	.L.str162, 50

	.type	.L.str163,@object       # @.str163
.L.str163:
	.asciz	"two types specified in one empty declaration"
	.size	.L.str163, 45

	.type	.L.str164,@object       # @.str164
.L.str164:
	.asciz	"empty declaration"
	.size	.L.str164, 18

	.type	.L.str165,@object       # @.str165
.L.str165:
	.asciz	"ISO C89 does not support `static' or type qualifiers in parameter array declarators"
	.size	.L.str165, 84

	.type	.L.str166,@object       # @.str166
.L.str166:
	.asciz	"ISO C89 does not support `[*]' array declarators"
	.size	.L.str166, 49

	.type	.L.str167,@object       # @.str167
.L.str167:
	.asciz	"GCC does not yet properly implement `[*]' array declarators"
	.size	.L.str167, 60

	.type	.L.str168,@object       # @.str168
.L.str168:
	.asciz	"static or type qualifiers in abstract declarator"
	.size	.L.str168, 49

	.type	.L.str169,@object       # @.str169
.L.str169:
	.asciz	"deprecated"
	.size	.L.str169, 11

	.type	deprecated_state,@object # @deprecated_state
	.local	deprecated_state
	.comm	deprecated_state,1,1
	.type	.L.str170,@object       # @.str170
.L.str170:
	.asciz	"`%s' is usually a function"
	.size	.L.str170, 27

	.type	.L.str171,@object       # @.str171
.L.str171:
	.asciz	"typedef `%s' is initialized"
	.size	.L.str171, 28

	.type	.L.str172,@object       # @.str172
.L.str172:
	.asciz	"function `%s' is initialized like a variable"
	.size	.L.str172, 45

	.type	.L.str173,@object       # @.str173
.L.str173:
	.asciz	"parameter `%s' is initialized"
	.size	.L.str173, 30

	.type	.L.str174,@object       # @.str174
.L.str174:
	.asciz	"variable-sized object may not be initialized"
	.size	.L.str174, 45

	.type	.L.str175,@object       # @.str175
.L.str175:
	.asciz	"variable `%s' has initializer but incomplete type"
	.size	.L.str175, 50

	.type	.L.str176,@object       # @.str176
.L.str176:
	.asciz	"elements of array `%s' have incomplete type"
	.size	.L.str176, 44

	.type	.L.str177,@object       # @.str177
.L.str177:
	.asciz	"noinline"
	.size	.L.str177, 9

	.type	.L.str178,@object       # @.str178
.L.str178:
	.asciz	"inline function `%s' given attribute noinline"
	.size	.L.str178, 46

	.type	.L.str179,@object       # @.str179
.L.str179:
	.asciz	"initializer fails to determine size of `%s'"
	.size	.L.str179, 44

	.type	.L.str180,@object       # @.str180
.L.str180:
	.asciz	"array size missing in `%s'"
	.size	.L.str180, 27

	.type	.L.str181,@object       # @.str181
.L.str181:
	.asciz	"zero or negative size array `%s'"
	.size	.L.str181, 33

	.type	.L.str182,@object       # @.str182
.L.str182:
	.asciz	"storage size of `%s' isn't known"
	.size	.L.str182, 33

	.type	.L.str183,@object       # @.str183
.L.str183:
	.asciz	"storage size of `%s' isn't constant"
	.size	.L.str183, 36

	.type	.L.str184,@object       # @.str184
.L.str184:
	.asciz	"ignoring asm-specifier for non-static local variable `%s'"
	.size	.L.str184, 58

	.type	.L.str185,@object       # @.str185
.L.str185:
	.asciz	"c-decl.c"
	.size	.L.str185, 9

	.type	.L__FUNCTION__.build_compound_literal,@object # @__FUNCTION__.build_compound_literal
.L__FUNCTION__.build_compound_literal:
	.asciz	"build_compound_literal"
	.size	.L__FUNCTION__.build_compound_literal, 23

	.type	.L.str186,@object       # @.str186
.L.str186:
	.asciz	"%s.%d"
	.size	.L.str186, 6

	.type	.L.str187,@object       # @.str187
.L.str187:
	.asciz	"__compound_literal"
	.size	.L.str187, 19

	.type	.L.str188,@object       # @.str188
.L.str188:
	.asciz	"parameter `%s' has just a forward declaration"
	.size	.L.str188, 46

	.type	.L.str189,@object       # @.str189
.L.str189:
	.asciz	"`void' in parameter list must be the entire list"
	.size	.L.str189, 49

	.type	parmlist_tags_warning.already,@object # @parmlist_tags_warning.already
	.local	parmlist_tags_warning.already
	.comm	parmlist_tags_warning.already,1,1
	.type	.L.str190,@object       # @.str190
.L.str190:
	.asciz	"`struct %s' declared inside parameter list"
	.size	.L.str190, 43

	.type	.L.str191,@object       # @.str191
.L.str191:
	.asciz	"`union %s' declared inside parameter list"
	.size	.L.str191, 42

	.type	.L.str192,@object       # @.str192
.L.str192:
	.asciz	"`enum %s' declared inside parameter list"
	.size	.L.str192, 41

	.type	.L.str193,@object       # @.str193
.L.str193:
	.asciz	"anonymous struct declared inside parameter list"
	.size	.L.str193, 48

	.type	.L.str194,@object       # @.str194
.L.str194:
	.asciz	"anonymous union declared inside parameter list"
	.size	.L.str194, 47

	.type	.L.str195,@object       # @.str195
.L.str195:
	.asciz	"anonymous enum declared inside parameter list"
	.size	.L.str195, 46

	.type	.L.str196,@object       # @.str196
.L.str196:
	.asciz	"its scope is only this definition or declaration, which is probably not what you want"
	.size	.L.str196, 86

	.type	.L.str197,@object       # @.str197
.L.str197:
	.asciz	"redefinition of `union %s'"
	.size	.L.str197, 27

	.type	.L.str198,@object       # @.str198
.L.str198:
	.asciz	"redefinition of `struct %s'"
	.size	.L.str198, 28

	.type	.L.str199,@object       # @.str199
.L.str199:
	.asciz	"unnamed fields of type other than struct or union are not allowed"
	.size	.L.str199, 66

	.type	.L.str200,@object       # @.str200
.L.str200:
	.asciz	"%s defined inside parms"
	.size	.L.str200, 24

	.type	.L.str201,@object       # @.str201
.L.str201:
	.asciz	"union"
	.size	.L.str201, 6

	.type	.L.str202,@object       # @.str202
.L.str202:
	.asciz	"structure"
	.size	.L.str202, 10

	.type	.L.str203,@object       # @.str203
.L.str203:
	.asciz	"%s has no %s"
	.size	.L.str203, 13

	.type	.L.str204,@object       # @.str204
.L.str204:
	.asciz	"struct"
	.size	.L.str204, 7

	.type	.L.str205,@object       # @.str205
.L.str205:
	.asciz	"named members"
	.size	.L.str205, 14

	.type	.L.str206,@object       # @.str206
.L.str206:
	.asciz	"members"
	.size	.L.str206, 8

	.type	.L.str207,@object       # @.str207
.L.str207:
	.asciz	"nested redefinition of `%s'"
	.size	.L.str207, 28

	.type	.L.str208,@object       # @.str208
.L.str208:
	.asciz	"bit-field `%s' width not an integer constant"
	.size	.L.str208, 45

	.type	.L.str209,@object       # @.str209
.L.str209:
	.asciz	"bit-field `%s' has invalid type"
	.size	.L.str209, 32

	.type	.L.str210,@object       # @.str210
.L.str210:
	.asciz	"bit-field `%s' type invalid in ISO C"
	.size	.L.str210, 37

	.type	.L.str211,@object       # @.str211
.L.str211:
	.asciz	"negative width in bit-field `%s'"
	.size	.L.str211, 33

	.type	.L.str212,@object       # @.str212
.L.str212:
	.asciz	"width of `%s' exceeds its type"
	.size	.L.str212, 31

	.type	.L.str213,@object       # @.str213
.L.str213:
	.asciz	"zero width for bit-field `%s'"
	.size	.L.str213, 30

	.type	.L.str214,@object       # @.str214
.L.str214:
	.asciz	"`%s' is narrower than values of its type"
	.size	.L.str214, 41

	.type	.L.str215,@object       # @.str215
.L.str215:
	.asciz	"flexible array member in union"
	.size	.L.str215, 31

	.type	.L.str216,@object       # @.str216
.L.str216:
	.asciz	"flexible array member not at end of struct"
	.size	.L.str216, 43

	.type	.L.str217,@object       # @.str217
.L.str217:
	.asciz	"flexible array member in otherwise empty struct"
	.size	.L.str217, 48

	.type	.L.str218,@object       # @.str218
.L.str218:
	.asciz	"duplicate member `%s'"
	.size	.L.str218, 22

	.type	.L.str219,@object       # @.str219
.L.str219:
	.asciz	"union cannot be made transparent"
	.size	.L.str219, 33

	.type	.L.str220,@object       # @.str220
.L.str220:
	.asciz	"redeclaration of `enum %s'"
	.size	.L.str220, 27

	.type	enum_next_value,@object # @enum_next_value
	.local	enum_next_value
	.comm	enum_next_value,8,8
	.type	enum_overflow,@object   # @enum_overflow
	.local	enum_overflow
	.comm	enum_overflow,4,4
	.type	.L.str221,@object       # @.str221
.L.str221:
	.asciz	"enum defined inside parms"
	.size	.L.str221, 26

	.type	.L.str222,@object       # @.str222
.L.str222:
	.asciz	"enumeration values exceed range of largest integer"
	.size	.L.str222, 51

	.type	.L.str223,@object       # @.str223
.L.str223:
	.asciz	"enumerator value for `%s' not integer constant"
	.size	.L.str223, 47

	.type	.L.str224,@object       # @.str224
.L.str224:
	.asciz	"overflow in enumeration values"
	.size	.L.str224, 31

	.type	.L.str225,@object       # @.str225
.L.str225:
	.asciz	"ISO C restricts enumerator values to range of `int'"
	.size	.L.str225, 52

	.type	current_function_returns_value,@object # @current_function_returns_value
	.comm	current_function_returns_value,4,4
	.type	current_function_returns_null,@object # @current_function_returns_null
	.comm	current_function_returns_null,4,4
	.type	current_function_returns_abnormally,@object # @current_function_returns_abnormally
	.comm	current_function_returns_abnormally,4,4
	.type	warn_about_return_type,@object # @warn_about_return_type
	.local	warn_about_return_type
	.comm	warn_about_return_type,4,4
	.type	current_extern_inline,@object # @current_extern_inline
	.local	current_extern_inline
	.comm	current_extern_inline,4,4
	.type	c_function_varargs,@object # @c_function_varargs
	.local	c_function_varargs
	.comm	c_function_varargs,1,1
	.type	.L.str226,@object       # @.str226
.L.str226:
	.asciz	"return type is an incomplete type"
	.size	.L.str226, 34

	.type	.L.str227,@object       # @.str227
.L.str227:
	.asciz	"return type defaults to `int'"
	.size	.L.str227, 30

	.type	last_function_parms,@object # @last_function_parms
	.local	last_function_parms
	.comm	last_function_parms,8,8
	.type	current_function_parms,@object # @current_function_parms
	.local	current_function_parms
	.comm	current_function_parms,8,8
	.type	last_function_parm_tags,@object # @last_function_parm_tags
	.local	last_function_parm_tags
	.comm	last_function_parm_tags,8,8
	.type	current_function_parm_tags,@object # @current_function_parm_tags
	.local	current_function_parm_tags
	.comm	current_function_parm_tags,8,8
	.type	current_function_prototype_file,@object # @current_function_prototype_file
	.local	current_function_prototype_file
	.comm	current_function_prototype_file,8,8
	.type	current_function_prototype_line,@object # @current_function_prototype_line
	.local	current_function_prototype_line
	.comm	current_function_prototype_line,4,4
	.type	.L.str228,@object       # @.str228
.L.str228:
	.asciz	"function declaration isn't a prototype"
	.size	.L.str228, 39

	.type	.L.str229,@object       # @.str229
.L.str229:
	.asciz	"no previous prototype for `%s'"
	.size	.L.str229, 31

	.type	.L.str230,@object       # @.str230
.L.str230:
	.asciz	"`%s' was used with no prototype before its definition"
	.size	.L.str230, 54

	.type	.L.str231,@object       # @.str231
.L.str231:
	.asciz	"no previous declaration for `%s'"
	.size	.L.str231, 33

	.type	.L.str232,@object       # @.str232
.L.str232:
	.asciz	"`%s' was used with no declaration before its definition"
	.size	.L.str232, 56

	.type	.L.str233,@object       # @.str233
.L.str233:
	.asciz	"return type of `%s' is not `int'"
	.size	.L.str233, 33

	.type	.L.str234,@object       # @.str234
.L.str234:
	.asciz	"first argument of `%s' should be `int'"
	.size	.L.str234, 39

	.type	.L.str235,@object       # @.str235
.L.str235:
	.asciz	"second argument of `%s' should be `char **'"
	.size	.L.str235, 44

	.type	.L.str236,@object       # @.str236
.L.str236:
	.asciz	"third argument of `%s' should probably be `char **'"
	.size	.L.str236, 52

	.type	.L.str237,@object       # @.str237
.L.str237:
	.asciz	"`%s' takes only zero or two arguments"
	.size	.L.str237, 38

	.type	.L.str238,@object       # @.str238
.L.str238:
	.asciz	"`%s' is normally a non-static function"
	.size	.L.str238, 39

	.type	.L.str239,@object       # @.str239
.L.str239:
	.asciz	"parm types given both in parmlist and separately"
	.size	.L.str239, 49

	.type	.L.str240,@object       # @.str240
.L.str240:
	.asciz	"parameter name omitted"
	.size	.L.str240, 23

	.type	.L.str241,@object       # @.str241
.L.str241:
	.asciz	"parameter `%s' declared void"
	.size	.L.str241, 29

	.type	.L.str242,@object       # @.str242
.L.str242:
	.asciz	"parameter name missing from parameter list"
	.size	.L.str242, 43

	.type	.L.str243,@object       # @.str243
.L.str243:
	.asciz	"multiple parameters named `%s'"
	.size	.L.str243, 31

	.type	.L.str244,@object       # @.str244
.L.str244:
	.asciz	"type of `%s' defaults to `int'"
	.size	.L.str244, 31

	.type	.L.str245,@object       # @.str245
.L.str245:
	.asciz	"parameter `%s' has incomplete type"
	.size	.L.str245, 35

	.type	.L.str246,@object       # @.str246
.L.str246:
	.asciz	"declaration for parameter `%s' but no such parameter"
	.size	.L.str246, 53

	.type	.L.str247,@object       # @.str247
.L.str247:
	.asciz	"number of arguments doesn't match prototype"
	.size	.L.str247, 44

	.type	.L.str248,@object       # @.str248
.L.str248:
	.asciz	"prototype declaration"
	.size	.L.str248, 22

	.type	.L.str249,@object       # @.str249
.L.str249:
	.asciz	"promoted argument `%s' doesn't match prototype"
	.size	.L.str249, 47

	.type	.L.str250,@object       # @.str250
.L.str250:
	.asciz	"argument `%s' doesn't match prototype"
	.size	.L.str250, 38

	.type	.L.str251,@object       # @.str251
.L.str251:
	.asciz	"no return statement in function returning non-void"
	.size	.L.str251, 51

	.type	.L.str252,@object       # @.str252
.L.str252:
	.asciz	"`for' loop initial declaration used outside C99 mode"
	.size	.L.str252, 53

	.type	.L.str253,@object       # @.str253
.L.str253:
	.asciz	"`struct %s' declared in `for' loop initial declaration"
	.size	.L.str253, 55

	.type	.L.str254,@object       # @.str254
.L.str254:
	.asciz	"`union %s' declared in `for' loop initial declaration"
	.size	.L.str254, 54

	.type	.L.str255,@object       # @.str255
.L.str255:
	.asciz	"`enum %s' declared in `for' loop initial declaration"
	.size	.L.str255, 53

	.type	.L.str256,@object       # @.str256
.L.str256:
	.asciz	"declaration of non-variable `%s' in `for' loop initial declaration"
	.size	.L.str256, 67

	.type	.L.str257,@object       # @.str257
.L.str257:
	.asciz	"declaration of static variable `%s' in `for' loop initial declaration"
	.size	.L.str257, 70

	.type	.L.str258,@object       # @.str258
.L.str258:
	.asciz	"declaration of `extern' variable `%s' in `for' loop initial declaration"
	.size	.L.str258, 72

	.type	.L.str259,@object       # @.str259
.L.str259:
	.asciz	"this function may return with or without a value"
	.size	.L.str259, 49

	.type	.L.str260,@object       # @.str260
.L.str260:
	.asciz	"size of return value of `%s' is %u bytes"
	.size	.L.str260, 41

	.type	.L.str261,@object       # @.str261
.L.str261:
	.asciz	"size of return value of `%s' is larger than %d bytes"
	.size	.L.str261, 53

	.type	.L__FUNCTION__.grokdeclarator,@object # @__FUNCTION__.grokdeclarator
.L__FUNCTION__.grokdeclarator:
	.asciz	"grokdeclarator"
	.size	.L__FUNCTION__.grokdeclarator, 15

	.type	.L.str262,@object       # @.str262
.L.str262:
	.asciz	"type name"
	.size	.L.str262, 10

	.type	.L.str263,@object       # @.str263
.L.str263:
	.asciz	"`long long long' is too long for GCC"
	.size	.L.str263, 37

	.type	.L.str264,@object       # @.str264
.L.str264:
	.asciz	"ISO C89 does not support `long long'"
	.size	.L.str264, 37

	.type	.L.str265,@object       # @.str265
.L.str265:
	.asciz	"duplicate `%s'"
	.size	.L.str265, 15

	.type	.L.str266,@object       # @.str266
.L.str266:
	.asciz	"two or more data types in declaration of `%s'"
	.size	.L.str266, 46

	.type	.L.str267,@object       # @.str267
.L.str267:
	.asciz	"`%s' fails to be a typedef or built in type"
	.size	.L.str267, 44

	.type	.L.str268,@object       # @.str268
.L.str268:
	.asciz	"type defaults to `int' in declaration of `%s'"
	.size	.L.str268, 46

	.type	.L.str269,@object       # @.str269
.L.str269:
	.asciz	"both long and short specified for `%s'"
	.size	.L.str269, 39

	.type	.L.str270,@object       # @.str270
.L.str270:
	.asciz	"long or short specified with char for `%s'"
	.size	.L.str270, 43

	.type	grokdeclarator.already,@object # @grokdeclarator.already
	.local	grokdeclarator.already
	.comm	grokdeclarator.already,1,1
	.type	.L.str271,@object       # @.str271
.L.str271:
	.asciz	"long or short specified with floating type for `%s'"
	.size	.L.str271, 52

	.type	.L.str272,@object       # @.str272
.L.str272:
	.asciz	"the only valid combination is `long double'"
	.size	.L.str272, 44

	.type	.L.str273,@object       # @.str273
.L.str273:
	.asciz	"both signed and unsigned specified for `%s'"
	.size	.L.str273, 44

	.type	.L.str274,@object       # @.str274
.L.str274:
	.asciz	"long, short, signed or unsigned invalid for `%s'"
	.size	.L.str274, 49

	.type	.L.str275,@object       # @.str275
.L.str275:
	.asciz	"long, short, signed or unsigned used invalidly for `%s'"
	.size	.L.str275, 56

	.type	.L.str276,@object       # @.str276
.L.str276:
	.asciz	"complex invalid for `%s'"
	.size	.L.str276, 25

	.type	.L.str277,@object       # @.str277
.L.str277:
	.asciz	"ISO C89 does not support complex types"
	.size	.L.str277, 39

	.type	.L.str278,@object       # @.str278
.L.str278:
	.asciz	"ISO C does not support plain `complex' meaning `double complex'"
	.size	.L.str278, 64

	.type	.L.str279,@object       # @.str279
.L.str279:
	.asciz	"ISO C does not support complex integer types"
	.size	.L.str279, 45

	.type	.L.str280,@object       # @.str280
.L.str280:
	.asciz	"duplicate `const'"
	.size	.L.str280, 18

	.type	.L.str281,@object       # @.str281
.L.str281:
	.asciz	"duplicate `restrict'"
	.size	.L.str281, 21

	.type	.L.str282,@object       # @.str282
.L.str282:
	.asciz	"duplicate `volatile'"
	.size	.L.str282, 21

	.type	.L.str283,@object       # @.str283
.L.str283:
	.asciz	"multiple storage classes in declaration of `%s'"
	.size	.L.str283, 48

	.type	.L.str284,@object       # @.str284
.L.str284:
	.asciz	"function definition declared `auto'"
	.size	.L.str284, 36

	.type	.L.str285,@object       # @.str285
.L.str285:
	.asciz	"function definition declared `register'"
	.size	.L.str285, 40

	.type	.L.str286,@object       # @.str286
.L.str286:
	.asciz	"function definition declared `typedef'"
	.size	.L.str286, 39

	.type	.L.str287,@object       # @.str287
.L.str287:
	.asciz	"storage class specified for structure field `%s'"
	.size	.L.str287, 49

	.type	.L.str288,@object       # @.str288
.L.str288:
	.asciz	"storage class specified for parameter `%s'"
	.size	.L.str288, 43

	.type	.L.str289,@object       # @.str289
.L.str289:
	.asciz	"storage class specified for typename"
	.size	.L.str289, 37

	.type	.L.str290,@object       # @.str290
.L.str290:
	.asciz	"`%s' initialized and declared `extern'"
	.size	.L.str290, 39

	.type	.L.str291,@object       # @.str291
.L.str291:
	.asciz	"`%s' has both `extern' and initializer"
	.size	.L.str291, 39

	.type	.L.str292,@object       # @.str292
.L.str292:
	.asciz	"nested function `%s' declared `extern'"
	.size	.L.str292, 39

	.type	.L.str293,@object       # @.str293
.L.str293:
	.asciz	"top-level declaration of `%s' specifies `auto'"
	.size	.L.str293, 47

	.type	.L.str294,@object       # @.str294
.L.str294:
	.asciz	"static or type qualifiers in non-parameter array declarator"
	.size	.L.str294, 60

	.type	.L.str295,@object       # @.str295
.L.str295:
	.asciz	"declaration of `%s' as array of voids"
	.size	.L.str295, 38

	.type	.L.str296,@object       # @.str296
.L.str296:
	.asciz	"declaration of `%s' as array of functions"
	.size	.L.str296, 42

	.type	.L.str297,@object       # @.str297
.L.str297:
	.asciz	"size of array `%s' has non-integer type"
	.size	.L.str297, 40

	.type	.L.str298,@object       # @.str298
.L.str298:
	.asciz	"ISO C forbids zero-size array `%s'"
	.size	.L.str298, 35

	.type	.L.str299,@object       # @.str299
.L.str299:
	.asciz	"size of array `%s' is negative"
	.size	.L.str299, 31

	.type	.L.str300,@object       # @.str300
.L.str300:
	.asciz	"ISO C89 forbids array `%s' whose size can't be evaluated"
	.size	.L.str300, 57

	.type	.L.str301,@object       # @.str301
.L.str301:
	.asciz	"ISO C89 forbids variable-size array `%s'"
	.size	.L.str301, 41

	.type	.L.str302,@object       # @.str302
.L.str302:
	.asciz	"size of array `%s' is too large"
	.size	.L.str302, 32

	.type	.L.str303,@object       # @.str303
.L.str303:
	.asciz	"ISO C89 does not support flexible array members"
	.size	.L.str303, 48

	.type	.L.str304,@object       # @.str304
.L.str304:
	.asciz	"array type has incomplete element type"
	.size	.L.str304, 39

	.type	.L.str305,@object       # @.str305
.L.str305:
	.asciz	"`%s' declared as function returning a function"
	.size	.L.str305, 47

	.type	.L.str306,@object       # @.str306
.L.str306:
	.asciz	"`%s' declared as function returning an array"
	.size	.L.str306, 45

	.type	.L.str307,@object       # @.str307
.L.str307:
	.asciz	"ISO C forbids qualified void function return type"
	.size	.L.str307, 50

	.type	.L.str308,@object       # @.str308
.L.str308:
	.asciz	"type qualifiers ignored on function return type"
	.size	.L.str308, 48

	.type	.L.str309,@object       # @.str309
.L.str309:
	.asciz	"ISO C forbids qualified function types"
	.size	.L.str309, 39

	.type	.L.str310,@object       # @.str310
.L.str310:
	.asciz	"invalid type modifier within pointer declarator"
	.size	.L.str310, 48

	.type	.L.str311,@object       # @.str311
.L.str311:
	.asciz	"ISO C forbids const or volatile function types"
	.size	.L.str311, 47

	.type	.L.str312,@object       # @.str312
.L.str312:
	.asciz	"variable or field `%s' declared void"
	.size	.L.str312, 37

	.type	.L.str313,@object       # @.str313
.L.str313:
	.asciz	"attributes in parameter array declarator ignored"
	.size	.L.str313, 49

	.type	.L.str314,@object       # @.str314
.L.str314:
	.asciz	"invalid type modifier within array declarator"
	.size	.L.str314, 46

	.type	.L.str315,@object       # @.str315
.L.str315:
	.asciz	"field `%s' declared as a function"
	.size	.L.str315, 34

	.type	.L.str316,@object       # @.str316
.L.str316:
	.asciz	"field `%s' has incomplete type"
	.size	.L.str316, 31

	.type	.L.str317,@object       # @.str317
.L.str317:
	.asciz	"invalid storage class for function `%s'"
	.size	.L.str317, 40

	.type	.L.str318,@object       # @.str318
.L.str318:
	.asciz	"`noreturn' function returns non-void value"
	.size	.L.str318, 43

	.type	.L.str319,@object       # @.str319
.L.str319:
	.asciz	"cannot inline function `main'"
	.size	.L.str319, 30

	.type	.L.str320,@object       # @.str320
.L.str320:
	.asciz	"variable `%s' declared `inline'"
	.size	.L.str320, 32

	.type	.L.str321,@object       # @.str321
.L.str321:
	.asciz	"parameter names (without types) in function declaration"
	.size	.L.str321, 56

	.type	.L.str322,@object       # @.str322
.L.str322:
	.asciz	"parameter has incomplete type"
	.size	.L.str322, 30

	.type	.L.str323,@object       # @.str323
.L.str323:
	.asciz	"declaration of `%s' shadows a parameter"
	.size	.L.str323, 40

	.type	.L.str324,@object       # @.str324
.L.str324:
	.asciz	"declaration of `%s' shadows a symbol from the parameter list"
	.size	.L.str324, 61

	.type	.L.str325,@object       # @.str325
.L.str325:
	.asciz	"a parameter"
	.size	.L.str325, 12

	.type	.L.str326,@object       # @.str326
.L.str326:
	.asciz	"a previous local"
	.size	.L.str326, 17

	.type	.L.str327,@object       # @.str327
.L.str327:
	.asciz	"a global declaration"
	.size	.L.str327, 21

	.type	.L.str328,@object       # @.str328
.L.str328:
	.asciz	"function `%s' redeclared as inline"
	.size	.L.str328, 35

	.type	.L.str329,@object       # @.str329
.L.str329:
	.asciz	"previous declaration of function `%s' with attribute noinline"
	.size	.L.str329, 62

	.type	.L.str330,@object       # @.str330
.L.str330:
	.asciz	"function `%s' redeclared with attribute noinline"
	.size	.L.str330, 49

	.type	.L.str331,@object       # @.str331
.L.str331:
	.asciz	"previous declaration of function `%s' was inline"
	.size	.L.str331, 49

	.type	.L.str332,@object       # @.str332
.L.str332:
	.asciz	"shadowing built-in function `%s'"
	.size	.L.str332, 33

	.type	.L.str333,@object       # @.str333
.L.str333:
	.asciz	"shadowing library function `%s'"
	.size	.L.str333, 32

	.type	.L.str334,@object       # @.str334
.L.str334:
	.asciz	"library function `%s' declared as non-function"
	.size	.L.str334, 47

	.type	.L.str335,@object       # @.str335
.L.str335:
	.asciz	"built-in function `%s' declared as non-function"
	.size	.L.str335, 48

	.type	.L.str336,@object       # @.str336
.L.str336:
	.asciz	"`%s' redeclared as different kind of symbol"
	.size	.L.str336, 44

	.type	.L.str337,@object       # @.str337
.L.str337:
	.asciz	"conflicting types for built-in function `%s'"
	.size	.L.str337, 45

	.type	.L.str338,@object       # @.str338
.L.str338:
	.asciz	"conflicting types for `%s'"
	.size	.L.str338, 27

	.type	.L.str339,@object       # @.str339
.L.str339:
	.asciz	"a parameter list with an ellipsis can't match an empty parameter name list declaration"
	.size	.L.str339, 87

	.type	.L.str340,@object       # @.str340
.L.str340:
	.asciz	"an argument type that has a default promotion can't match an empty parameter name list declaration"
	.size	.L.str340, 99

	.type	.L.str341,@object       # @.str341
.L.str341:
	.asciz	"redefinition of `%s'"
	.size	.L.str341, 21

	.type	.L.str342,@object       # @.str342
.L.str342:
	.asciz	"redeclaration of `%s'"
	.size	.L.str342, 22

	.type	.L.str343,@object       # @.str343
.L.str343:
	.asciz	"conflicting declarations of `%s'"
	.size	.L.str343, 33

	.type	.L__FUNCTION__.duplicate_decls,@object # @__FUNCTION__.duplicate_decls
.L__FUNCTION__.duplicate_decls:
	.asciz	"duplicate_decls"
	.size	.L__FUNCTION__.duplicate_decls, 16

	.type	.L.str344,@object       # @.str344
.L.str344:
	.asciz	"`%s' previously defined here"
	.size	.L.str344, 29

	.type	.L.str345,@object       # @.str345
.L.str345:
	.asciz	"`%s' previously declared here"
	.size	.L.str345, 30

	.type	.L.str346,@object       # @.str346
.L.str346:
	.asciz	"prototype for `%s' follows"
	.size	.L.str346, 27

	.type	.L.str347,@object       # @.str347
.L.str347:
	.asciz	"non-prototype definition here"
	.size	.L.str347, 30

	.type	.L.str348,@object       # @.str348
.L.str348:
	.asciz	"prototype for `%s' follows and number of arguments doesn't match"
	.size	.L.str348, 65

	.type	.L.str349,@object       # @.str349
.L.str349:
	.asciz	"prototype for `%s' follows and argument %d doesn't match"
	.size	.L.str349, 57

	.type	.L.str350,@object       # @.str350
.L.str350:
	.asciz	"`%s' declared inline after being called"
	.size	.L.str350, 40

	.type	.L.str351,@object       # @.str351
.L.str351:
	.asciz	"`%s' declared inline after its definition"
	.size	.L.str351, 42

	.type	.L.str352,@object       # @.str352
.L.str352:
	.asciz	"static declaration for `%s' follows non-static"
	.size	.L.str352, 47

	.type	.L.str353,@object       # @.str353
.L.str353:
	.asciz	"non-static declaration for `%s' follows static"
	.size	.L.str353, 47

	.type	.L.str354,@object       # @.str354
.L.str354:
	.asciz	"const declaration for `%s' follows non-const"
	.size	.L.str354, 45

	.type	.L.str355,@object       # @.str355
.L.str355:
	.asciz	"type qualifiers for `%s' conflict with previous decl"
	.size	.L.str355, 53

	.type	.L.str356,@object       # @.str356
.L.str356:
	.asciz	"redundant redeclaration of `%s' in same scope"
	.size	.L.str356, 46


	.ident	"clang version 3.5 (trunk)"
	.section	".note.GNU-stack","",@progbits
