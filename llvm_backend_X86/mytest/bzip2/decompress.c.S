	.file	"decompress.c.bc"
	.section	.debug_info,"",@progbits
.Lsection_info:
	.section	.debug_abbrev,"",@progbits
.Lsection_abbrev:
	.section	.debug_line,"",@progbits
.Lsection_line:
	.section	.debug_loc,"",@progbits
	.section	.debug_pubnames,"",@progbits
	.section	.debug_pubtypes,"",@progbits
	.section	.debug_str,"MS",@progbits,1
.Linfo_string:
	.section	.debug_ranges,"",@progbits
.Ldebug_range:
	.section	.debug_loc,"",@progbits
.Lsection_debug_loc:
	.file	1 "decompress.c"
	.file	2 "./bzlib_private.h"
	.text
	.globl	BZ2_decompress
	.align	16, 0x90
	.type	BZ2_decompress,@function
BZ2_decompress:                         # @BZ2_decompress
.Lfunc_begin0:
	.loc	1 147 0                 # decompress.c:147:0
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbp
.Ltmp3:
	.cfi_def_cfa_offset 16
.Ltmp4:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
.Ltmp5:
	.cfi_def_cfa_register %rbp
	pushq	%r15
	pushq	%r14
	pushq	%r12
	pushq	%rbx
	subq	$384, %rsp              # imm = 0x180
.Ltmp6:
	.cfi_offset %rbx, -48
.Ltmp7:
	.cfi_offset %r12, -40
.Ltmp8:
	.cfi_offset %r14, -32
.Ltmp9:
	.cfi_offset %r15, -24
	movq	%rdi, -40(%rbp)
	.loc	1 151 0 prologue_end    # decompress.c:151:0
.Ltmp10:
	movq	(%rdi), %rax
	.loc	1 179 0                 # decompress.c:179:0
.Ltmp11:
	movq	-40(%rbp), %rcx
.Ltmp12:
	.loc	1 151 0                 # decompress.c:151:0
	movq	%rax, -64(%rbp)
	.loc	1 179 0                 # decompress.c:179:0
.Ltmp13:
	movl	8(%rcx), %eax
	cmpq	$10, %rax
	jne	.LBB0_2
# BB#1:                                 # %if.then
	.loc	1 181 0                 # decompress.c:181:0
.Ltmp14:
	movq	-40(%rbp), %rcx
	xorq	%rax, %rax
	movl	%eax, 64036(%rcx)
	.loc	1 182 0                 # decompress.c:182:0
	movq	-40(%rbp), %rcx
	movl	%eax, 64040(%rcx)
	.loc	1 183 0                 # decompress.c:183:0
	movq	-40(%rbp), %rcx
	movl	%eax, 64044(%rcx)
	.loc	1 184 0                 # decompress.c:184:0
	movq	-40(%rbp), %rcx
	movl	%eax, 64048(%rcx)
	.loc	1 185 0                 # decompress.c:185:0
	movq	-40(%rbp), %rcx
	movl	%eax, 64052(%rcx)
	.loc	1 186 0                 # decompress.c:186:0
	movq	-40(%rbp), %rcx
	movl	%eax, 64056(%rcx)
	.loc	1 187 0                 # decompress.c:187:0
	movq	-40(%rbp), %rcx
	movl	%eax, 64060(%rcx)
	.loc	1 188 0                 # decompress.c:188:0
	movq	-40(%rbp), %rcx
	movl	%eax, 64064(%rcx)
	.loc	1 189 0                 # decompress.c:189:0
	movq	-40(%rbp), %rcx
	movl	%eax, 64068(%rcx)
	.loc	1 190 0                 # decompress.c:190:0
	movq	-40(%rbp), %rcx
	movl	%eax, 64072(%rcx)
	.loc	1 191 0                 # decompress.c:191:0
	movq	-40(%rbp), %rcx
	movl	%eax, 64076(%rcx)
	.loc	1 192 0                 # decompress.c:192:0
	movq	-40(%rbp), %rcx
	movl	%eax, 64080(%rcx)
	.loc	1 193 0                 # decompress.c:193:0
	movq	-40(%rbp), %rcx
	movl	%eax, 64084(%rcx)
	.loc	1 194 0                 # decompress.c:194:0
	movq	-40(%rbp), %rcx
	movl	%eax, 64088(%rcx)
	.loc	1 195 0                 # decompress.c:195:0
	movq	-40(%rbp), %rcx
	movl	%eax, 64092(%rcx)
	.loc	1 196 0                 # decompress.c:196:0
	movq	-40(%rbp), %rcx
	movl	%eax, 64096(%rcx)
	.loc	1 197 0                 # decompress.c:197:0
	movq	-40(%rbp), %rcx
	movl	%eax, 64100(%rcx)
	.loc	1 198 0                 # decompress.c:198:0
	movq	-40(%rbp), %rcx
	movl	%eax, 64104(%rcx)
	.loc	1 199 0                 # decompress.c:199:0
	movq	-40(%rbp), %rcx
	movl	%eax, 64108(%rcx)
	.loc	1 200 0                 # decompress.c:200:0
	movq	-40(%rbp), %rcx
	movl	%eax, 64112(%rcx)
	.loc	1 201 0                 # decompress.c:201:0
	movq	-40(%rbp), %rcx
	movl	%eax, 64116(%rcx)
	.loc	1 202 0                 # decompress.c:202:0
	movq	-40(%rbp), %rax
	movq	$0, 64120(%rax)
	.loc	1 203 0                 # decompress.c:203:0
	movq	-40(%rbp), %rax
	movq	$0, 64128(%rax)
	.loc	1 204 0                 # decompress.c:204:0
	movq	-40(%rbp), %rax
	movq	$0, 64136(%rax)
.Ltmp15:
.LBB0_2:                                # %if.end
	.loc	1 208 0                 # decompress.c:208:0
	movq	-40(%rbp), %rax
	movl	64036(%rax), %eax
	.loc	1 209 0                 # decompress.c:209:0
	movq	-40(%rbp), %rcx
	.loc	1 208 0                 # decompress.c:208:0
	movl	%eax, -68(%rbp)
	.loc	1 209 0                 # decompress.c:209:0
	movl	64040(%rcx), %eax
	.loc	1 210 0                 # decompress.c:210:0
	movq	-40(%rbp), %rcx
	.loc	1 209 0                 # decompress.c:209:0
	movl	%eax, -72(%rbp)
	.loc	1 210 0                 # decompress.c:210:0
	movl	64044(%rcx), %eax
	.loc	1 211 0                 # decompress.c:211:0
	movq	-40(%rbp), %rcx
	.loc	1 210 0                 # decompress.c:210:0
	movl	%eax, -76(%rbp)
	.loc	1 211 0                 # decompress.c:211:0
	movl	64048(%rcx), %eax
	.loc	1 212 0                 # decompress.c:212:0
	movq	-40(%rbp), %rcx
	.loc	1 211 0                 # decompress.c:211:0
	movl	%eax, -80(%rbp)
	.loc	1 212 0                 # decompress.c:212:0
	movl	64052(%rcx), %eax
	.loc	1 213 0                 # decompress.c:213:0
	movq	-40(%rbp), %rcx
	.loc	1 212 0                 # decompress.c:212:0
	movl	%eax, -84(%rbp)
	.loc	1 213 0                 # decompress.c:213:0
	movl	64056(%rcx), %eax
	.loc	1 214 0                 # decompress.c:214:0
	movq	-40(%rbp), %rcx
	.loc	1 213 0                 # decompress.c:213:0
	movl	%eax, -88(%rbp)
	.loc	1 214 0                 # decompress.c:214:0
	movl	64060(%rcx), %eax
	.loc	1 215 0                 # decompress.c:215:0
	movq	-40(%rbp), %rcx
	.loc	1 214 0                 # decompress.c:214:0
	movl	%eax, -92(%rbp)
	.loc	1 215 0                 # decompress.c:215:0
	movl	64064(%rcx), %eax
	.loc	1 216 0                 # decompress.c:216:0
	movq	-40(%rbp), %rcx
	.loc	1 215 0                 # decompress.c:215:0
	movl	%eax, -96(%rbp)
	.loc	1 216 0                 # decompress.c:216:0
	movl	64068(%rcx), %eax
	.loc	1 217 0                 # decompress.c:217:0
	movq	-40(%rbp), %rcx
	.loc	1 216 0                 # decompress.c:216:0
	movl	%eax, -100(%rbp)
	.loc	1 217 0                 # decompress.c:217:0
	movl	64072(%rcx), %eax
	.loc	1 218 0                 # decompress.c:218:0
	movq	-40(%rbp), %rcx
	.loc	1 217 0                 # decompress.c:217:0
	movl	%eax, -104(%rbp)
	.loc	1 218 0                 # decompress.c:218:0
	movl	64076(%rcx), %eax
	.loc	1 219 0                 # decompress.c:219:0
	movq	-40(%rbp), %rcx
	.loc	1 218 0                 # decompress.c:218:0
	movl	%eax, -108(%rbp)
	.loc	1 219 0                 # decompress.c:219:0
	movl	64080(%rcx), %eax
	.loc	1 220 0                 # decompress.c:220:0
	movq	-40(%rbp), %rcx
	.loc	1 219 0                 # decompress.c:219:0
	movl	%eax, -112(%rbp)
	.loc	1 220 0                 # decompress.c:220:0
	movl	64084(%rcx), %eax
	.loc	1 221 0                 # decompress.c:221:0
	movq	-40(%rbp), %rcx
	.loc	1 220 0                 # decompress.c:220:0
	movl	%eax, -116(%rbp)
	.loc	1 221 0                 # decompress.c:221:0
	movl	64088(%rcx), %eax
	.loc	1 222 0                 # decompress.c:222:0
	movq	-40(%rbp), %rcx
	.loc	1 221 0                 # decompress.c:221:0
	movl	%eax, -120(%rbp)
	.loc	1 222 0                 # decompress.c:222:0
	movl	64092(%rcx), %eax
	.loc	1 223 0                 # decompress.c:223:0
	movq	-40(%rbp), %rcx
	.loc	1 222 0                 # decompress.c:222:0
	movl	%eax, -124(%rbp)
	.loc	1 223 0                 # decompress.c:223:0
	movl	64096(%rcx), %eax
	.loc	1 224 0                 # decompress.c:224:0
	movq	-40(%rbp), %rcx
	.loc	1 223 0                 # decompress.c:223:0
	movl	%eax, -128(%rbp)
	.loc	1 224 0                 # decompress.c:224:0
	movl	64100(%rcx), %eax
	.loc	1 225 0                 # decompress.c:225:0
	movq	-40(%rbp), %rcx
	.loc	1 224 0                 # decompress.c:224:0
	movl	%eax, -132(%rbp)
	.loc	1 225 0                 # decompress.c:225:0
	movl	64104(%rcx), %eax
	.loc	1 226 0                 # decompress.c:226:0
	movq	-40(%rbp), %rcx
	.loc	1 225 0                 # decompress.c:225:0
	movl	%eax, -136(%rbp)
	.loc	1 226 0                 # decompress.c:226:0
	movl	64108(%rcx), %eax
	.loc	1 227 0                 # decompress.c:227:0
	movq	-40(%rbp), %rcx
	.loc	1 226 0                 # decompress.c:226:0
	movl	%eax, -140(%rbp)
	.loc	1 227 0                 # decompress.c:227:0
	movl	64112(%rcx), %eax
	.loc	1 228 0                 # decompress.c:228:0
	movq	-40(%rbp), %rcx
	.loc	1 227 0                 # decompress.c:227:0
	movl	%eax, -144(%rbp)
	.loc	1 228 0                 # decompress.c:228:0
	movl	64116(%rcx), %eax
	.loc	1 229 0                 # decompress.c:229:0
	movq	-40(%rbp), %rcx
	.loc	1 228 0                 # decompress.c:228:0
	movl	%eax, -148(%rbp)
	.loc	1 229 0                 # decompress.c:229:0
	movq	64120(%rcx), %rax
	.loc	1 230 0                 # decompress.c:230:0
	movq	-40(%rbp), %rcx
	.loc	1 229 0                 # decompress.c:229:0
	movq	%rax, -160(%rbp)
	.loc	1 230 0                 # decompress.c:230:0
	movq	64128(%rcx), %rax
	.loc	1 231 0                 # decompress.c:231:0
	movq	-40(%rbp), %rcx
	.loc	1 230 0                 # decompress.c:230:0
	movq	%rax, -168(%rbp)
	.loc	1 231 0                 # decompress.c:231:0
	movq	64136(%rcx), %rax
	.loc	1 235 0                 # decompress.c:235:0
	movq	-40(%rbp), %rcx
	.loc	1 231 0                 # decompress.c:231:0
	movq	%rax, -176(%rbp)
	xorq	%rax, %rax
	.loc	1 233 0                 # decompress.c:233:0
	movl	%eax, -48(%rbp)
	.loc	1 235 0                 # decompress.c:235:0
	movl	8(%rcx), %eax
	addq	$-10, %rax
	movabsq	$4294967295, %r14       # imm = 0xFFFFFFFF
	andq	%r14, %rax
	cmpq	$40, %rax
	jbe	.LBB0_3
# BB#444:                               # %sw.epilog
	movq	$4001, %rdi             # imm = 0xFA1
	.loc	1 628 0                 # decompress.c:628:0
.Ltmp16:
	callq	BZ2_bz__AssertH__fail
	movq	$4002, %rdi             # imm = 0xFA2
.Ltmp17:
	.loc	1 631 0                 # decompress.c:631:0
	callq	BZ2_bz__AssertH__fail
	jmp	.LBB0_445
.Ltmp18:
.LBB0_3:                                # %if.end
	jmpq	*.LJTI0_0(,%rax,8)
.LBB0_4:                                # %sw.bb
	.loc	1 237 0                 # decompress.c:237:0
.Ltmp19:
	movq	-40(%rbp), %rax
	movq	$10, %rcx
	movl	%ecx, 8(%rax)
	jmp	.LBB0_5
.LBB0_9:                                # %if.then53
                                        #   in Loop: Header=BB0_5 Depth=1
	.loc	1 237 0                 # decompress.c:237:0
.Ltmp20:
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	movl	16(%rax), %ecx
	incq	%rcx
	movl	%ecx, 16(%rax)
.Ltmp21:
.LBB0_5:                                # %while.body
                                        # =>This Inner Loop Header: Depth=1
	.loc	1 237 0                 # decompress.c:237:0
	movq	-40(%rbp), %rax
	movslq	36(%rax), %rax
	cmpq	$7, %rax
	jg	.LBB0_10
.Ltmp22:
# BB#6:                                 # %if.end33
                                        #   in Loop: Header=BB0_5 Depth=1
	.loc	1 237 0                 # decompress.c:237:0
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	movl	8(%rax), %eax
	testq	%rax, %rax
	je	.LBB0_7
.Ltmp23:
# BB#8:                                 # %if.end38
                                        #   in Loop: Header=BB0_5 Depth=1
	.loc	1 237 0                 # decompress.c:237:0
	movq	-40(%rbp), %rax
	movq	(%rax), %rcx
	movq	(%rcx), %rcx
	movl	32(%rax), %edx
	movzbl	(%rcx), %esi
	movq	$8, %rcx
	shlq	%cl, %rdx
	orq	%rdx, %rsi
	movl	%esi, 32(%rax)
	movq	-40(%rbp), %rax
	movl	36(%rax), %ecx
	addq	$8, %rcx
	movl	%ecx, 36(%rax)
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	incq	(%rax)
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	movl	8(%rax), %ecx
	decq	%rcx
	movl	%ecx, 8(%rax)
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	movl	12(%rax), %ecx
	incq	%rcx
	movl	%ecx, 12(%rax)
	.loc	1 237 0                 # decompress.c:237:0
.Ltmp24:
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	movl	12(%rax), %eax
	testq	%rax, %rax
	jne	.LBB0_5
	jmp	.LBB0_9
.Ltmp25:
.LBB0_10:                               # %while.end
	#DEBUG_VALUE: v <- [RBP+-180]
	.loc	1 237 0                 # decompress.c:237:0
	movq	-40(%rbp), %rax
	movl	36(%rax), %ecx
	movl	32(%rax), %eax
	addq	$-8, %rcx
	andq	%r14, %rcx
	movq	-40(%rbp), %rdx
	shrq	%cl, %rax
	andq	$255, %rax
	movl	%eax, -180(%rbp)
	movl	36(%rdx), %eax
	addq	$-8, %rax
	movl	%eax, 36(%rdx)
	movzbl	-180(%rbp), %eax
	movb	%al, -41(%rbp)
.Ltmp26:
	.loc	1 238 0                 # decompress.c:238:0
	movzbl	-41(%rbp), %eax
	cmpq	$66, %rax
	jne	.LBB0_11
.Ltmp27:
.LBB0_12:                               # %sw.bb62
	.loc	1 240 0                 # decompress.c:240:0
	movq	-40(%rbp), %rax
	movq	$11, %rcx
	movl	%ecx, 8(%rax)
	jmp	.LBB0_13
.LBB0_16:                               # %if.then107
                                        #   in Loop: Header=BB0_13 Depth=1
	.loc	1 240 0                 # decompress.c:240:0
.Ltmp28:
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	movl	16(%rax), %ecx
	incq	%rcx
	movl	%ecx, 16(%rax)
.Ltmp29:
.LBB0_13:                               # %while.body64
                                        # =>This Inner Loop Header: Depth=1
	.loc	1 240 0                 # decompress.c:240:0
	movq	-40(%rbp), %rax
	movslq	36(%rax), %rax
	cmpq	$7, %rax
	jg	.LBB0_17
.Ltmp30:
# BB#14:                                # %if.end78
                                        #   in Loop: Header=BB0_13 Depth=1
	.loc	1 240 0                 # decompress.c:240:0
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	movl	8(%rax), %eax
	testq	%rax, %rax
	je	.LBB0_7
.Ltmp31:
# BB#15:                                # %if.end84
                                        #   in Loop: Header=BB0_13 Depth=1
	.loc	1 240 0                 # decompress.c:240:0
	movq	-40(%rbp), %rax
	movq	(%rax), %rcx
	movq	(%rcx), %rcx
	movl	32(%rax), %edx
	movzbl	(%rcx), %esi
	movq	$8, %rcx
	shlq	%cl, %rdx
	orq	%rdx, %rsi
	movl	%esi, 32(%rax)
	movq	-40(%rbp), %rax
	movl	36(%rax), %ecx
	addq	$8, %rcx
	movl	%ecx, 36(%rax)
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	incq	(%rax)
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	movl	8(%rax), %ecx
	decq	%rcx
	movl	%ecx, 8(%rax)
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	movl	12(%rax), %ecx
	incq	%rcx
	movl	%ecx, 12(%rax)
	.loc	1 240 0                 # decompress.c:240:0
.Ltmp32:
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	movl	12(%rax), %eax
	testq	%rax, %rax
	jne	.LBB0_13
	jmp	.LBB0_16
.Ltmp33:
.LBB0_17:                               # %while.end112
	#DEBUG_VALUE: v <- [RBP+-184]
	.loc	1 240 0                 # decompress.c:240:0
	movq	-40(%rbp), %rax
	movl	36(%rax), %ecx
	movl	32(%rax), %eax
	addq	$-8, %rcx
	andq	%r14, %rcx
	movq	-40(%rbp), %rdx
	shrq	%cl, %rax
	andq	$255, %rax
	movl	%eax, -184(%rbp)
	movl	36(%rdx), %eax
	addq	$-8, %rax
	movl	%eax, 36(%rdx)
	movzbl	-184(%rbp), %eax
	movb	%al, -41(%rbp)
.Ltmp34:
	.loc	1 241 0                 # decompress.c:241:0
	movzbl	-41(%rbp), %eax
	cmpq	$90, %rax
	jne	.LBB0_11
.Ltmp35:
.LBB0_18:                               # %sw.bb118
	.loc	1 243 0                 # decompress.c:243:0
	movq	-40(%rbp), %rax
	movq	$12, %rcx
	movl	%ecx, 8(%rax)
	jmp	.LBB0_19
.LBB0_22:                               # %if.then163
                                        #   in Loop: Header=BB0_19 Depth=1
	.loc	1 243 0                 # decompress.c:243:0
.Ltmp36:
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	movl	16(%rax), %ecx
	incq	%rcx
	movl	%ecx, 16(%rax)
.Ltmp37:
.LBB0_19:                               # %while.body120
                                        # =>This Inner Loop Header: Depth=1
	.loc	1 243 0                 # decompress.c:243:0
	movq	-40(%rbp), %rax
	movslq	36(%rax), %rax
	cmpq	$7, %rax
	jg	.LBB0_23
.Ltmp38:
# BB#20:                                # %if.end134
                                        #   in Loop: Header=BB0_19 Depth=1
	.loc	1 243 0                 # decompress.c:243:0
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	movl	8(%rax), %eax
	testq	%rax, %rax
	je	.LBB0_7
.Ltmp39:
# BB#21:                                # %if.end140
                                        #   in Loop: Header=BB0_19 Depth=1
	.loc	1 243 0                 # decompress.c:243:0
	movq	-40(%rbp), %rax
	movq	(%rax), %rcx
	movq	(%rcx), %rcx
	movl	32(%rax), %edx
	movzbl	(%rcx), %esi
	movq	$8, %rcx
	shlq	%cl, %rdx
	orq	%rdx, %rsi
	movl	%esi, 32(%rax)
	movq	-40(%rbp), %rax
	movl	36(%rax), %ecx
	addq	$8, %rcx
	movl	%ecx, 36(%rax)
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	incq	(%rax)
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	movl	8(%rax), %ecx
	decq	%rcx
	movl	%ecx, 8(%rax)
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	movl	12(%rax), %ecx
	incq	%rcx
	movl	%ecx, 12(%rax)
	.loc	1 243 0                 # decompress.c:243:0
.Ltmp40:
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	movl	12(%rax), %eax
	testq	%rax, %rax
	jne	.LBB0_19
	jmp	.LBB0_22
.Ltmp41:
.LBB0_23:                               # %while.end168
	#DEBUG_VALUE: v <- [RBP+-188]
	.loc	1 243 0                 # decompress.c:243:0
	movq	-40(%rbp), %rax
	movl	36(%rax), %ecx
	movl	32(%rax), %eax
	addq	$-8, %rcx
	andq	%r14, %rcx
	movq	-40(%rbp), %rdx
	shrq	%cl, %rax
	andq	$255, %rax
	movl	%eax, -188(%rbp)
	movl	36(%rdx), %eax
	addq	$-8, %rax
	movl	%eax, 36(%rdx)
	movzbl	-188(%rbp), %eax
	movb	%al, -41(%rbp)
.Ltmp42:
	.loc	1 244 0                 # decompress.c:244:0
	movzbl	-41(%rbp), %eax
	cmpq	$104, %rax
	jne	.LBB0_11
.Ltmp43:
.LBB0_24:                               # %sw.bb174
	.loc	1 246 0                 # decompress.c:246:0
	movq	-40(%rbp), %rax
	movq	$13, %rcx
	movl	%ecx, 8(%rax)
	jmp	.LBB0_25
.LBB0_28:                               # %if.then218
                                        #   in Loop: Header=BB0_25 Depth=1
	.loc	1 246 0                 # decompress.c:246:0
.Ltmp44:
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	movl	16(%rax), %ecx
	incq	%rcx
	movl	%ecx, 16(%rax)
.Ltmp45:
.LBB0_25:                               # %while.body176
                                        # =>This Inner Loop Header: Depth=1
	.loc	1 246 0                 # decompress.c:246:0
	movq	-40(%rbp), %rax
	movslq	36(%rax), %rax
	cmpq	$7, %rax
	jg	.LBB0_29
.Ltmp46:
# BB#26:                                # %if.end189
                                        #   in Loop: Header=BB0_25 Depth=1
	.loc	1 246 0                 # decompress.c:246:0
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	movl	8(%rax), %eax
	testq	%rax, %rax
	je	.LBB0_7
.Ltmp47:
# BB#27:                                # %if.end195
                                        #   in Loop: Header=BB0_25 Depth=1
	.loc	1 246 0                 # decompress.c:246:0
	movq	-40(%rbp), %rax
	movq	(%rax), %rcx
	movq	(%rcx), %rcx
	movl	32(%rax), %edx
	movzbl	(%rcx), %esi
	movq	$8, %rcx
	shlq	%cl, %rdx
	orq	%rdx, %rsi
	movl	%esi, 32(%rax)
	movq	-40(%rbp), %rax
	movl	36(%rax), %ecx
	addq	$8, %rcx
	movl	%ecx, 36(%rax)
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	incq	(%rax)
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	movl	8(%rax), %ecx
	decq	%rcx
	movl	%ecx, 8(%rax)
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	movl	12(%rax), %ecx
	incq	%rcx
	movl	%ecx, 12(%rax)
	.loc	1 246 0                 # decompress.c:246:0
.Ltmp48:
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	movl	12(%rax), %eax
	testq	%rax, %rax
	jne	.LBB0_25
	jmp	.LBB0_28
.Ltmp49:
.LBB0_29:                               # %while.end223
	#DEBUG_VALUE: v <- [RBP+-192]
	.loc	1 246 0                 # decompress.c:246:0
	movq	-40(%rbp), %rax
	movl	36(%rax), %ecx
	movl	32(%rax), %eax
	addq	$-8, %rcx
	andq	%r14, %rcx
	movq	-40(%rbp), %rdx
	shrq	%cl, %rax
	andq	$255, %rax
	movl	%eax, -192(%rbp)
	movl	36(%rdx), %eax
	addq	$-8, %rax
	movl	%eax, 36(%rdx)
	movl	-192(%rbp), %eax
	movq	-40(%rbp), %rcx
	movl	%eax, 40(%rcx)
.Ltmp50:
	.loc	1 247 0                 # decompress.c:247:0
	movq	-40(%rbp), %rax
	movslq	40(%rax), %rax
	cmpq	$49, %rax
	jl	.LBB0_11
.Ltmp51:
# BB#30:                                # %lor.lhs.false
	movq	-40(%rbp), %rax
	movslq	40(%rax), %rax
	cmpq	$58, %rax
	jl	.LBB0_31
.Ltmp52:
.LBB0_11:                               # %if.then60
	movabsq	$4294967291, %rax       # imm = 0xFFFFFFFB
	.loc	1 238 0                 # decompress.c:238:0
.Ltmp53:
	movl	%eax, -48(%rbp)
	jmp	.LBB0_445
.Ltmp54:
.LBB0_31:                               # %if.end231
	.loc	1 249 0                 # decompress.c:249:0
	movq	-40(%rbp), %rax
	movl	40(%rax), %ecx
	addq	$-48, %rcx
	movl	%ecx, 40(%rax)
	.loc	1 251 0                 # decompress.c:251:0
.Ltmp55:
	movq	-40(%rbp), %rax
	movzbl	44(%rax), %eax
	testq	%rax, %rax
	je	.LBB0_35
# BB#32:                                # %if.then234
	.loc	1 252 0                 # decompress.c:252:0
.Ltmp56:
	movq	-40(%rbp), %rax
	movq	-64(%rbp), %rbx
	movl	40(%rax), %eax
	movq	72(%rbx), %rdi
	imulq	$100000, %rax, %rsi     # imm = 0x186A0
	movq	$32, %rcx
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	addq	%rsi, %rsi
	movq	$1, %rdx
	callq	*56(%rbx)
	movq	-40(%rbp), %rcx
	movq	%rax, 3160(%rcx)
	.loc	1 253 0                 # decompress.c:253:0
	movq	-40(%rbp), %rax
	movq	-64(%rbp), %rbx
	movl	40(%rax), %eax
	movq	72(%rbx), %rdi
	imulq	$100000, %rax, %rsi     # imm = 0x186A0
	movq	$32, %rcx
	shlq	%cl, %rsi
	movq	$33, %rcx
	sarq	%cl, %rsi
	movq	$1, %rdx
	callq	*56(%rbx)
	movq	-40(%rbp), %rcx
	movq	%rax, 3168(%rcx)
	.loc	1 256 0                 # decompress.c:256:0
.Ltmp57:
	movq	-40(%rbp), %rax
	cmpq	$0, 3160(%rax)
	je	.LBB0_34
# BB#33:                                # %lor.lhs.false252
	movq	-40(%rbp), %rax
	cmpq	$0, 3168(%rax)
	jne	.LBB0_36
	jmp	.LBB0_34
.Ltmp58:
.LBB0_35:                               # %if.else
	.loc	1 258 0                 # decompress.c:258:0
	movq	-40(%rbp), %rax
	movq	-64(%rbp), %rbx
	movl	40(%rax), %eax
	movq	72(%rbx), %rdi
	imulq	$100000, %rax, %rsi     # imm = 0x186A0
	movq	$32, %rcx
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	movq	$2, %rcx
	shlq	%cl, %rsi
	movq	$1, %rdx
	callq	*56(%rbx)
	movq	-40(%rbp), %rcx
	movq	%rax, 3152(%rcx)
	.loc	1 259 0                 # decompress.c:259:0
.Ltmp59:
	movq	-40(%rbp), %rax
	cmpq	$0, 3152(%rax)
	je	.LBB0_34
.Ltmp60:
.LBB0_36:                               # %sw.bb272
	.loc	1 262 0                 # decompress.c:262:0
	movq	-40(%rbp), %rax
	movq	$14, %rcx
	movl	%ecx, 8(%rax)
	jmp	.LBB0_37
.LBB0_40:                               # %if.then317
                                        #   in Loop: Header=BB0_37 Depth=1
	.loc	1 262 0                 # decompress.c:262:0
.Ltmp61:
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	movl	16(%rax), %ecx
	incq	%rcx
	movl	%ecx, 16(%rax)
.Ltmp62:
.LBB0_37:                               # %while.body274
                                        # =>This Inner Loop Header: Depth=1
	.loc	1 262 0                 # decompress.c:262:0
	movq	-40(%rbp), %rax
	movslq	36(%rax), %rax
	cmpq	$7, %rax
	jg	.LBB0_41
.Ltmp63:
# BB#38:                                # %if.end288
                                        #   in Loop: Header=BB0_37 Depth=1
	.loc	1 262 0                 # decompress.c:262:0
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	movl	8(%rax), %eax
	testq	%rax, %rax
	je	.LBB0_7
.Ltmp64:
# BB#39:                                # %if.end294
                                        #   in Loop: Header=BB0_37 Depth=1
	.loc	1 262 0                 # decompress.c:262:0
	movq	-40(%rbp), %rax
	movq	(%rax), %rcx
	movq	(%rcx), %rcx
	movl	32(%rax), %edx
	movzbl	(%rcx), %esi
	movq	$8, %rcx
	shlq	%cl, %rdx
	orq	%rdx, %rsi
	movl	%esi, 32(%rax)
	movq	-40(%rbp), %rax
	movl	36(%rax), %ecx
	addq	$8, %rcx
	movl	%ecx, 36(%rax)
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	incq	(%rax)
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	movl	8(%rax), %ecx
	decq	%rcx
	movl	%ecx, 8(%rax)
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	movl	12(%rax), %ecx
	incq	%rcx
	movl	%ecx, 12(%rax)
	.loc	1 262 0                 # decompress.c:262:0
.Ltmp65:
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	movl	12(%rax), %eax
	testq	%rax, %rax
	jne	.LBB0_37
	jmp	.LBB0_40
.Ltmp66:
.LBB0_41:                               # %while.end322
	#DEBUG_VALUE: v <- [RBP+-196]
	.loc	1 262 0                 # decompress.c:262:0
	movq	-40(%rbp), %rax
	movl	36(%rax), %ecx
	movl	32(%rax), %eax
	addq	$-8, %rcx
	andq	%r14, %rcx
	movq	-40(%rbp), %rdx
	shrq	%cl, %rax
	andq	$255, %rax
	movl	%eax, -196(%rbp)
	movl	36(%rdx), %eax
	addq	$-8, %rax
	movl	%eax, 36(%rdx)
	movzbl	-196(%rbp), %eax
	cmpq	$23, %rax
	movb	%al, -41(%rbp)
	jne	.LBB0_42
.Ltmp67:
.LBB0_389:                              # %sw.bb2942
	.loc	1 604 0                 # decompress.c:604:0
	movq	-40(%rbp), %rax
	movq	$42, %rcx
	movl	%ecx, 8(%rax)
	jmp	.LBB0_390
.LBB0_393:                              # %if.then2989
                                        #   in Loop: Header=BB0_390 Depth=1
	.loc	1 604 0                 # decompress.c:604:0
.Ltmp68:
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	movl	16(%rax), %ecx
	incq	%rcx
	movl	%ecx, 16(%rax)
.Ltmp69:
.LBB0_390:                              # %while.body2945
                                        # =>This Inner Loop Header: Depth=1
	.loc	1 604 0                 # decompress.c:604:0
	movq	-40(%rbp), %rax
	movslq	36(%rax), %rax
	cmpq	$7, %rax
	jg	.LBB0_394
.Ltmp70:
# BB#391:                               # %if.end2960
                                        #   in Loop: Header=BB0_390 Depth=1
	.loc	1 604 0                 # decompress.c:604:0
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	movl	8(%rax), %eax
	testq	%rax, %rax
	je	.LBB0_7
.Ltmp71:
# BB#392:                               # %if.end2966
                                        #   in Loop: Header=BB0_390 Depth=1
	.loc	1 604 0                 # decompress.c:604:0
	movq	-40(%rbp), %rax
	movq	(%rax), %rcx
	movq	(%rcx), %rcx
	movl	32(%rax), %edx
	movzbl	(%rcx), %esi
	movq	$8, %rcx
	shlq	%cl, %rdx
	orq	%rdx, %rsi
	movl	%esi, 32(%rax)
	movq	-40(%rbp), %rax
	movl	36(%rax), %ecx
	addq	$8, %rcx
	movl	%ecx, 36(%rax)
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	incq	(%rax)
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	movl	8(%rax), %ecx
	decq	%rcx
	movl	%ecx, 8(%rax)
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	movl	12(%rax), %ecx
	incq	%rcx
	movl	%ecx, 12(%rax)
	.loc	1 604 0                 # decompress.c:604:0
.Ltmp72:
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	movl	12(%rax), %eax
	testq	%rax, %rax
	jne	.LBB0_390
	jmp	.LBB0_393
.Ltmp73:
.LBB0_394:                              # %while.end2994
	#DEBUG_VALUE: v <- [RBP+-364]
	.loc	1 604 0                 # decompress.c:604:0
	movq	-40(%rbp), %rax
	movl	36(%rax), %ecx
	movl	32(%rax), %eax
	addq	$-8, %rcx
	andq	%r14, %rcx
	movq	-40(%rbp), %rdx
	shrq	%cl, %rax
	andq	$255, %rax
	movl	%eax, -364(%rbp)
	movl	36(%rdx), %eax
	addq	$-8, %rax
	movl	%eax, 36(%rdx)
	movzbl	-364(%rbp), %eax
	movb	%al, -41(%rbp)
.Ltmp74:
	.loc	1 605 0                 # decompress.c:605:0
	movzbl	-41(%rbp), %eax
	cmpq	$114, %rax
	jne	.LBB0_43
.Ltmp75:
.LBB0_395:                              # %sw.bb3000
	.loc	1 606 0                 # decompress.c:606:0
	movq	-40(%rbp), %rax
	movq	$43, %rcx
	movl	%ecx, 8(%rax)
	jmp	.LBB0_396
.LBB0_399:                              # %if.then3047
                                        #   in Loop: Header=BB0_396 Depth=1
	.loc	1 606 0                 # decompress.c:606:0
.Ltmp76:
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	movl	16(%rax), %ecx
	incq	%rcx
	movl	%ecx, 16(%rax)
.Ltmp77:
.LBB0_396:                              # %while.body3003
                                        # =>This Inner Loop Header: Depth=1
	.loc	1 606 0                 # decompress.c:606:0
	movq	-40(%rbp), %rax
	movslq	36(%rax), %rax
	cmpq	$7, %rax
	jg	.LBB0_400
.Ltmp78:
# BB#397:                               # %if.end3018
                                        #   in Loop: Header=BB0_396 Depth=1
	.loc	1 606 0                 # decompress.c:606:0
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	movl	8(%rax), %eax
	testq	%rax, %rax
	je	.LBB0_7
.Ltmp79:
# BB#398:                               # %if.end3024
                                        #   in Loop: Header=BB0_396 Depth=1
	.loc	1 606 0                 # decompress.c:606:0
	movq	-40(%rbp), %rax
	movq	(%rax), %rcx
	movq	(%rcx), %rcx
	movl	32(%rax), %edx
	movzbl	(%rcx), %esi
	movq	$8, %rcx
	shlq	%cl, %rdx
	orq	%rdx, %rsi
	movl	%esi, 32(%rax)
	movq	-40(%rbp), %rax
	movl	36(%rax), %ecx
	addq	$8, %rcx
	movl	%ecx, 36(%rax)
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	incq	(%rax)
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	movl	8(%rax), %ecx
	decq	%rcx
	movl	%ecx, 8(%rax)
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	movl	12(%rax), %ecx
	incq	%rcx
	movl	%ecx, 12(%rax)
	.loc	1 606 0                 # decompress.c:606:0
.Ltmp80:
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	movl	12(%rax), %eax
	testq	%rax, %rax
	jne	.LBB0_396
	jmp	.LBB0_399
.Ltmp81:
.LBB0_400:                              # %while.end3052
	#DEBUG_VALUE: v <- [RBP+-368]
	.loc	1 606 0                 # decompress.c:606:0
	movq	-40(%rbp), %rax
	movl	36(%rax), %ecx
	movl	32(%rax), %eax
	addq	$-8, %rcx
	andq	%r14, %rcx
	movq	-40(%rbp), %rdx
	shrq	%cl, %rax
	andq	$255, %rax
	movl	%eax, -368(%rbp)
	movl	36(%rdx), %eax
	addq	$-8, %rax
	movl	%eax, 36(%rdx)
	movzbl	-368(%rbp), %eax
	movb	%al, -41(%rbp)
.Ltmp82:
	.loc	1 607 0                 # decompress.c:607:0
	movzbl	-41(%rbp), %eax
	cmpq	$69, %rax
	jne	.LBB0_43
.Ltmp83:
.LBB0_401:                              # %sw.bb3058
	.loc	1 608 0                 # decompress.c:608:0
	movq	-40(%rbp), %rax
	movq	$44, %rcx
	movl	%ecx, 8(%rax)
	jmp	.LBB0_402
.LBB0_405:                              # %if.then3105
                                        #   in Loop: Header=BB0_402 Depth=1
	.loc	1 608 0                 # decompress.c:608:0
.Ltmp84:
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	movl	16(%rax), %ecx
	incq	%rcx
	movl	%ecx, 16(%rax)
.Ltmp85:
.LBB0_402:                              # %while.body3061
                                        # =>This Inner Loop Header: Depth=1
	.loc	1 608 0                 # decompress.c:608:0
	movq	-40(%rbp), %rax
	movslq	36(%rax), %rax
	cmpq	$7, %rax
	jg	.LBB0_406
.Ltmp86:
# BB#403:                               # %if.end3076
                                        #   in Loop: Header=BB0_402 Depth=1
	.loc	1 608 0                 # decompress.c:608:0
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	movl	8(%rax), %eax
	testq	%rax, %rax
	je	.LBB0_7
.Ltmp87:
# BB#404:                               # %if.end3082
                                        #   in Loop: Header=BB0_402 Depth=1
	.loc	1 608 0                 # decompress.c:608:0
	movq	-40(%rbp), %rax
	movq	(%rax), %rcx
	movq	(%rcx), %rcx
	movl	32(%rax), %edx
	movzbl	(%rcx), %esi
	movq	$8, %rcx
	shlq	%cl, %rdx
	orq	%rdx, %rsi
	movl	%esi, 32(%rax)
	movq	-40(%rbp), %rax
	movl	36(%rax), %ecx
	addq	$8, %rcx
	movl	%ecx, 36(%rax)
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	incq	(%rax)
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	movl	8(%rax), %ecx
	decq	%rcx
	movl	%ecx, 8(%rax)
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	movl	12(%rax), %ecx
	incq	%rcx
	movl	%ecx, 12(%rax)
	.loc	1 608 0                 # decompress.c:608:0
.Ltmp88:
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	movl	12(%rax), %eax
	testq	%rax, %rax
	jne	.LBB0_402
	jmp	.LBB0_405
.Ltmp89:
.LBB0_406:                              # %while.end3110
	#DEBUG_VALUE: v <- [RBP+-372]
	.loc	1 608 0                 # decompress.c:608:0
	movq	-40(%rbp), %rax
	movl	36(%rax), %ecx
	movl	32(%rax), %eax
	addq	$-8, %rcx
	andq	%r14, %rcx
	movq	-40(%rbp), %rdx
	shrq	%cl, %rax
	andq	$255, %rax
	movl	%eax, -372(%rbp)
	movl	36(%rdx), %eax
	addq	$-8, %rax
	movl	%eax, 36(%rdx)
	movzbl	-372(%rbp), %eax
	movb	%al, -41(%rbp)
.Ltmp90:
	.loc	1 609 0                 # decompress.c:609:0
	movzbl	-41(%rbp), %eax
	cmpq	$56, %rax
	jne	.LBB0_43
.Ltmp91:
.LBB0_407:                              # %sw.bb3116
	.loc	1 610 0                 # decompress.c:610:0
	movq	-40(%rbp), %rax
	movq	$45, %rcx
	movl	%ecx, 8(%rax)
	jmp	.LBB0_408
.LBB0_411:                              # %if.then3163
                                        #   in Loop: Header=BB0_408 Depth=1
	.loc	1 610 0                 # decompress.c:610:0
.Ltmp92:
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	movl	16(%rax), %ecx
	incq	%rcx
	movl	%ecx, 16(%rax)
.Ltmp93:
.LBB0_408:                              # %while.body3119
                                        # =>This Inner Loop Header: Depth=1
	.loc	1 610 0                 # decompress.c:610:0
	movq	-40(%rbp), %rax
	movslq	36(%rax), %rax
	cmpq	$7, %rax
	jg	.LBB0_412
.Ltmp94:
# BB#409:                               # %if.end3134
                                        #   in Loop: Header=BB0_408 Depth=1
	.loc	1 610 0                 # decompress.c:610:0
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	movl	8(%rax), %eax
	testq	%rax, %rax
	je	.LBB0_7
.Ltmp95:
# BB#410:                               # %if.end3140
                                        #   in Loop: Header=BB0_408 Depth=1
	.loc	1 610 0                 # decompress.c:610:0
	movq	-40(%rbp), %rax
	movq	(%rax), %rcx
	movq	(%rcx), %rcx
	movl	32(%rax), %edx
	movzbl	(%rcx), %esi
	movq	$8, %rcx
	shlq	%cl, %rdx
	orq	%rdx, %rsi
	movl	%esi, 32(%rax)
	movq	-40(%rbp), %rax
	movl	36(%rax), %ecx
	addq	$8, %rcx
	movl	%ecx, 36(%rax)
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	incq	(%rax)
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	movl	8(%rax), %ecx
	decq	%rcx
	movl	%ecx, 8(%rax)
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	movl	12(%rax), %ecx
	incq	%rcx
	movl	%ecx, 12(%rax)
	.loc	1 610 0                 # decompress.c:610:0
.Ltmp96:
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	movl	12(%rax), %eax
	testq	%rax, %rax
	jne	.LBB0_408
	jmp	.LBB0_411
.Ltmp97:
.LBB0_412:                              # %while.end3168
	#DEBUG_VALUE: v <- [RBP+-376]
	.loc	1 610 0                 # decompress.c:610:0
	movq	-40(%rbp), %rax
	movl	36(%rax), %ecx
	movl	32(%rax), %eax
	addq	$-8, %rcx
	andq	%r14, %rcx
	movq	-40(%rbp), %rdx
	shrq	%cl, %rax
	andq	$255, %rax
	movl	%eax, -376(%rbp)
	movl	36(%rdx), %eax
	addq	$-8, %rax
	movl	%eax, 36(%rdx)
	movzbl	-376(%rbp), %eax
	movb	%al, -41(%rbp)
.Ltmp98:
	.loc	1 611 0                 # decompress.c:611:0
	movzbl	-41(%rbp), %eax
	cmpq	$80, %rax
	jne	.LBB0_43
.Ltmp99:
.LBB0_413:                              # %sw.bb3174
	.loc	1 612 0                 # decompress.c:612:0
	movq	-40(%rbp), %rax
	movq	$46, %rcx
	movl	%ecx, 8(%rax)
	jmp	.LBB0_414
.LBB0_417:                              # %if.then3221
                                        #   in Loop: Header=BB0_414 Depth=1
	.loc	1 612 0                 # decompress.c:612:0
.Ltmp100:
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	movl	16(%rax), %ecx
	incq	%rcx
	movl	%ecx, 16(%rax)
.Ltmp101:
.LBB0_414:                              # %while.body3177
                                        # =>This Inner Loop Header: Depth=1
	.loc	1 612 0                 # decompress.c:612:0
	movq	-40(%rbp), %rax
	movslq	36(%rax), %rax
	cmpq	$7, %rax
	jg	.LBB0_418
.Ltmp102:
# BB#415:                               # %if.end3192
                                        #   in Loop: Header=BB0_414 Depth=1
	.loc	1 612 0                 # decompress.c:612:0
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	movl	8(%rax), %eax
	testq	%rax, %rax
	je	.LBB0_7
.Ltmp103:
# BB#416:                               # %if.end3198
                                        #   in Loop: Header=BB0_414 Depth=1
	.loc	1 612 0                 # decompress.c:612:0
	movq	-40(%rbp), %rax
	movq	(%rax), %rcx
	movq	(%rcx), %rcx
	movl	32(%rax), %edx
	movzbl	(%rcx), %esi
	movq	$8, %rcx
	shlq	%cl, %rdx
	orq	%rdx, %rsi
	movl	%esi, 32(%rax)
	movq	-40(%rbp), %rax
	movl	36(%rax), %ecx
	addq	$8, %rcx
	movl	%ecx, 36(%rax)
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	incq	(%rax)
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	movl	8(%rax), %ecx
	decq	%rcx
	movl	%ecx, 8(%rax)
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	movl	12(%rax), %ecx
	incq	%rcx
	movl	%ecx, 12(%rax)
	.loc	1 612 0                 # decompress.c:612:0
.Ltmp104:
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	movl	12(%rax), %eax
	testq	%rax, %rax
	jne	.LBB0_414
	jmp	.LBB0_417
.Ltmp105:
.LBB0_418:                              # %while.end3226
	#DEBUG_VALUE: v <- [RBP+-380]
	.loc	1 612 0                 # decompress.c:612:0
	movq	-40(%rbp), %rax
	movl	36(%rax), %ecx
	movl	32(%rax), %eax
	addq	$-8, %rcx
	andq	%r14, %rcx
	movq	-40(%rbp), %rdx
	shrq	%cl, %rax
	andq	$255, %rax
	movl	%eax, -380(%rbp)
	movl	36(%rdx), %eax
	addq	$-8, %rax
	movl	%eax, 36(%rdx)
	movzbl	-380(%rbp), %eax
	movb	%al, -41(%rbp)
.Ltmp106:
	.loc	1 613 0                 # decompress.c:613:0
	movzbl	-41(%rbp), %eax
	cmpq	$144, %rax
	jne	.LBB0_43
.Ltmp107:
# BB#419:                               # %if.end3231
	.loc	1 615 0                 # decompress.c:615:0
	movq	-40(%rbp), %rax
	xorq	%rcx, %rcx
	movl	%ecx, 3180(%rax)
.LBB0_420:                              # %sw.bb3232
	.loc	1 616 0                 # decompress.c:616:0
	movq	-40(%rbp), %rax
	movq	$47, %rcx
	movl	%ecx, 8(%rax)
	jmp	.LBB0_421
.LBB0_424:                              # %if.then3279
                                        #   in Loop: Header=BB0_421 Depth=1
	.loc	1 616 0                 # decompress.c:616:0
.Ltmp108:
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	movl	16(%rax), %ecx
	incq	%rcx
	movl	%ecx, 16(%rax)
.Ltmp109:
.LBB0_421:                              # %while.body3235
                                        # =>This Inner Loop Header: Depth=1
	.loc	1 616 0                 # decompress.c:616:0
	movq	-40(%rbp), %rax
	movslq	36(%rax), %rax
	cmpq	$7, %rax
	jg	.LBB0_425
.Ltmp110:
# BB#422:                               # %if.end3250
                                        #   in Loop: Header=BB0_421 Depth=1
	.loc	1 616 0                 # decompress.c:616:0
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	movl	8(%rax), %eax
	testq	%rax, %rax
	je	.LBB0_7
.Ltmp111:
# BB#423:                               # %if.end3256
                                        #   in Loop: Header=BB0_421 Depth=1
	.loc	1 616 0                 # decompress.c:616:0
	movq	-40(%rbp), %rax
	movq	(%rax), %rcx
	movq	(%rcx), %rcx
	movl	32(%rax), %edx
	movzbl	(%rcx), %esi
	movq	$8, %rcx
	shlq	%cl, %rdx
	orq	%rdx, %rsi
	movl	%esi, 32(%rax)
	movq	-40(%rbp), %rax
	movl	36(%rax), %ecx
	addq	$8, %rcx
	movl	%ecx, 36(%rax)
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	incq	(%rax)
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	movl	8(%rax), %ecx
	decq	%rcx
	movl	%ecx, 8(%rax)
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	movl	12(%rax), %ecx
	incq	%rcx
	movl	%ecx, 12(%rax)
	.loc	1 616 0                 # decompress.c:616:0
.Ltmp112:
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	movl	12(%rax), %eax
	testq	%rax, %rax
	jne	.LBB0_421
	jmp	.LBB0_424
.Ltmp113:
.LBB0_425:                              # %while.end3284
	#DEBUG_VALUE: v <- [RBP+-384]
	.loc	1 616 0                 # decompress.c:616:0
	movq	-40(%rbp), %rax
	movl	36(%rax), %ecx
	movl	32(%rax), %eax
	addq	$-8, %rcx
	andq	%r14, %rcx
	movq	-40(%rbp), %rdx
	shrq	%cl, %rax
	andq	$255, %rax
	movl	%eax, -384(%rbp)
	movl	36(%rdx), %eax
	addq	$-8, %rax
	movl	%eax, 36(%rdx)
	movzbl	-384(%rbp), %eax
.Ltmp114:
	.loc	1 617 0                 # decompress.c:617:0
	movq	-40(%rbp), %rdx
	.loc	1 616 0                 # decompress.c:616:0
.Ltmp115:
	movb	%al, -41(%rbp)
.Ltmp116:
	.loc	1 617 0                 # decompress.c:617:0
	movl	3180(%rdx), %eax
	movzbl	-41(%rbp), %esi
	movq	$8, %rcx
	shlq	%cl, %rax
	orq	%rax, %rsi
	movl	%esi, 3180(%rdx)
.Ltmp117:
.LBB0_426:                              # %sw.bb3290
	.loc	1 618 0                 # decompress.c:618:0
	movq	-40(%rbp), %rax
	movq	$48, %rcx
	movl	%ecx, 8(%rax)
	jmp	.LBB0_427
.LBB0_430:                              # %if.then3337
                                        #   in Loop: Header=BB0_427 Depth=1
	.loc	1 618 0                 # decompress.c:618:0
.Ltmp118:
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	movl	16(%rax), %ecx
	incq	%rcx
	movl	%ecx, 16(%rax)
.Ltmp119:
.LBB0_427:                              # %while.body3293
                                        # =>This Inner Loop Header: Depth=1
	.loc	1 618 0                 # decompress.c:618:0
	movq	-40(%rbp), %rax
	movslq	36(%rax), %rax
	cmpq	$7, %rax
	jg	.LBB0_431
.Ltmp120:
# BB#428:                               # %if.end3308
                                        #   in Loop: Header=BB0_427 Depth=1
	.loc	1 618 0                 # decompress.c:618:0
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	movl	8(%rax), %eax
	testq	%rax, %rax
	je	.LBB0_7
.Ltmp121:
# BB#429:                               # %if.end3314
                                        #   in Loop: Header=BB0_427 Depth=1
	.loc	1 618 0                 # decompress.c:618:0
	movq	-40(%rbp), %rax
	movq	(%rax), %rcx
	movq	(%rcx), %rcx
	movl	32(%rax), %edx
	movzbl	(%rcx), %esi
	movq	$8, %rcx
	shlq	%cl, %rdx
	orq	%rdx, %rsi
	movl	%esi, 32(%rax)
	movq	-40(%rbp), %rax
	movl	36(%rax), %ecx
	addq	$8, %rcx
	movl	%ecx, 36(%rax)
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	incq	(%rax)
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	movl	8(%rax), %ecx
	decq	%rcx
	movl	%ecx, 8(%rax)
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	movl	12(%rax), %ecx
	incq	%rcx
	movl	%ecx, 12(%rax)
	.loc	1 618 0                 # decompress.c:618:0
.Ltmp122:
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	movl	12(%rax), %eax
	testq	%rax, %rax
	jne	.LBB0_427
	jmp	.LBB0_430
.Ltmp123:
.LBB0_431:                              # %while.end3342
	#DEBUG_VALUE: v <- [RBP+-388]
	.loc	1 618 0                 # decompress.c:618:0
	movq	-40(%rbp), %rax
	movl	36(%rax), %ecx
	movl	32(%rax), %eax
	addq	$-8, %rcx
	andq	%r14, %rcx
	movq	-40(%rbp), %rdx
	shrq	%cl, %rax
	andq	$255, %rax
	movl	%eax, -388(%rbp)
	movl	36(%rdx), %eax
	addq	$-8, %rax
	movl	%eax, 36(%rdx)
	movzbl	-388(%rbp), %eax
.Ltmp124:
	.loc	1 619 0                 # decompress.c:619:0
	movq	-40(%rbp), %rdx
	.loc	1 618 0                 # decompress.c:618:0
.Ltmp125:
	movb	%al, -41(%rbp)
.Ltmp126:
	.loc	1 619 0                 # decompress.c:619:0
	movl	3180(%rdx), %eax
	movzbl	-41(%rbp), %esi
	movq	$8, %rcx
	shlq	%cl, %rax
	orq	%rax, %rsi
	movl	%esi, 3180(%rdx)
.Ltmp127:
.LBB0_432:                              # %sw.bb3348
	.loc	1 620 0                 # decompress.c:620:0
	movq	-40(%rbp), %rax
	movq	$49, %rcx
	movl	%ecx, 8(%rax)
	jmp	.LBB0_433
.LBB0_436:                              # %if.then3395
                                        #   in Loop: Header=BB0_433 Depth=1
	.loc	1 620 0                 # decompress.c:620:0
.Ltmp128:
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	movl	16(%rax), %ecx
	incq	%rcx
	movl	%ecx, 16(%rax)
.Ltmp129:
.LBB0_433:                              # %while.body3351
                                        # =>This Inner Loop Header: Depth=1
	.loc	1 620 0                 # decompress.c:620:0
	movq	-40(%rbp), %rax
	movslq	36(%rax), %rax
	cmpq	$7, %rax
	jg	.LBB0_437
.Ltmp130:
# BB#434:                               # %if.end3366
                                        #   in Loop: Header=BB0_433 Depth=1
	.loc	1 620 0                 # decompress.c:620:0
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	movl	8(%rax), %eax
	testq	%rax, %rax
	je	.LBB0_7
.Ltmp131:
# BB#435:                               # %if.end3372
                                        #   in Loop: Header=BB0_433 Depth=1
	.loc	1 620 0                 # decompress.c:620:0
	movq	-40(%rbp), %rax
	movq	(%rax), %rcx
	movq	(%rcx), %rcx
	movl	32(%rax), %edx
	movzbl	(%rcx), %esi
	movq	$8, %rcx
	shlq	%cl, %rdx
	orq	%rdx, %rsi
	movl	%esi, 32(%rax)
	movq	-40(%rbp), %rax
	movl	36(%rax), %ecx
	addq	$8, %rcx
	movl	%ecx, 36(%rax)
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	incq	(%rax)
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	movl	8(%rax), %ecx
	decq	%rcx
	movl	%ecx, 8(%rax)
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	movl	12(%rax), %ecx
	incq	%rcx
	movl	%ecx, 12(%rax)
	.loc	1 620 0                 # decompress.c:620:0
.Ltmp132:
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	movl	12(%rax), %eax
	testq	%rax, %rax
	jne	.LBB0_433
	jmp	.LBB0_436
.Ltmp133:
.LBB0_437:                              # %while.end3400
	#DEBUG_VALUE: v <- [RBP+-392]
	.loc	1 620 0                 # decompress.c:620:0
	movq	-40(%rbp), %rax
	movl	36(%rax), %ecx
	movl	32(%rax), %eax
	addq	$-8, %rcx
	andq	%r14, %rcx
	movq	-40(%rbp), %rdx
	shrq	%cl, %rax
	andq	$255, %rax
	movl	%eax, -392(%rbp)
	movl	36(%rdx), %eax
	addq	$-8, %rax
	movl	%eax, 36(%rdx)
	movzbl	-392(%rbp), %eax
.Ltmp134:
	.loc	1 621 0                 # decompress.c:621:0
	movq	-40(%rbp), %rdx
	.loc	1 620 0                 # decompress.c:620:0
.Ltmp135:
	movb	%al, -41(%rbp)
.Ltmp136:
	.loc	1 621 0                 # decompress.c:621:0
	movl	3180(%rdx), %eax
	movzbl	-41(%rbp), %esi
	movq	$8, %rcx
	shlq	%cl, %rax
	orq	%rax, %rsi
	movl	%esi, 3180(%rdx)
.Ltmp137:
.LBB0_438:                              # %sw.bb3406
	.loc	1 622 0                 # decompress.c:622:0
	movq	-40(%rbp), %rax
	movq	$50, %rcx
	movl	%ecx, 8(%rax)
	jmp	.LBB0_439
.LBB0_442:                              # %if.then3453
                                        #   in Loop: Header=BB0_439 Depth=1
	.loc	1 622 0                 # decompress.c:622:0
.Ltmp138:
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	movl	16(%rax), %ecx
	incq	%rcx
	movl	%ecx, 16(%rax)
.Ltmp139:
.LBB0_439:                              # %while.body3409
                                        # =>This Inner Loop Header: Depth=1
	.loc	1 622 0                 # decompress.c:622:0
	movq	-40(%rbp), %rax
	movslq	36(%rax), %rax
	cmpq	$7, %rax
	jg	.LBB0_443
.Ltmp140:
# BB#440:                               # %if.end3424
                                        #   in Loop: Header=BB0_439 Depth=1
	.loc	1 622 0                 # decompress.c:622:0
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	movl	8(%rax), %eax
	testq	%rax, %rax
	je	.LBB0_7
.Ltmp141:
# BB#441:                               # %if.end3430
                                        #   in Loop: Header=BB0_439 Depth=1
	.loc	1 622 0                 # decompress.c:622:0
	movq	-40(%rbp), %rax
	movq	(%rax), %rcx
	movq	(%rcx), %rcx
	movl	32(%rax), %edx
	movzbl	(%rcx), %esi
	movq	$8, %rcx
	shlq	%cl, %rdx
	orq	%rdx, %rsi
	movl	%esi, 32(%rax)
	movq	-40(%rbp), %rax
	movl	36(%rax), %ecx
	addq	$8, %rcx
	movl	%ecx, 36(%rax)
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	incq	(%rax)
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	movl	8(%rax), %ecx
	decq	%rcx
	movl	%ecx, 8(%rax)
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	movl	12(%rax), %ecx
	incq	%rcx
	movl	%ecx, 12(%rax)
	.loc	1 622 0                 # decompress.c:622:0
.Ltmp142:
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	movl	12(%rax), %eax
	testq	%rax, %rax
	jne	.LBB0_439
	jmp	.LBB0_442
.Ltmp143:
.LBB0_443:                              # %while.end3458
	#DEBUG_VALUE: v <- [RBP+-396]
	.loc	1 622 0                 # decompress.c:622:0
	movq	-40(%rbp), %rax
	movl	36(%rax), %ecx
	movl	32(%rax), %eax
	addq	$-8, %rcx
	andq	%r14, %rcx
	movq	-40(%rbp), %rdx
	shrq	%cl, %rax
	andq	$255, %rax
	movl	%eax, -396(%rbp)
	movl	36(%rdx), %eax
	addq	$-8, %rax
	movl	%eax, 36(%rdx)
	movzbl	-396(%rbp), %eax
.Ltmp144:
	.loc	1 623 0                 # decompress.c:623:0
	movq	-40(%rbp), %rdx
	.loc	1 622 0                 # decompress.c:622:0
.Ltmp145:
	movb	%al, -41(%rbp)
.Ltmp146:
	.loc	1 623 0                 # decompress.c:623:0
	movl	3180(%rdx), %eax
	movzbl	-41(%rbp), %esi
	movq	$8, %rcx
	shlq	%cl, %rax
	orq	%rax, %rsi
	movl	%esi, 3180(%rdx)
	.loc	1 625 0                 # decompress.c:625:0
	movq	-40(%rbp), %rax
	movq	$1, %rcx
	movl	%ecx, 8(%rax)
	movq	$4, %rax
	.loc	1 626 0                 # decompress.c:626:0
.Ltmp147:
	movl	%eax, -48(%rbp)
	jmp	.LBB0_445
.Ltmp148:
.LBB0_42:                               # %if.end327
	.loc	1 265 0                 # decompress.c:265:0
	movzbl	-41(%rbp), %eax
	cmpq	$49, %rax
	jne	.LBB0_43
.Ltmp149:
.LBB0_44:                               # %sw.bb333
	.loc	1 266 0                 # decompress.c:266:0
	movq	-40(%rbp), %rax
	movq	$15, %rcx
	movl	%ecx, 8(%rax)
	jmp	.LBB0_45
.LBB0_48:                               # %if.then378
                                        #   in Loop: Header=BB0_45 Depth=1
	.loc	1 266 0                 # decompress.c:266:0
.Ltmp150:
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	movl	16(%rax), %ecx
	incq	%rcx
	movl	%ecx, 16(%rax)
.Ltmp151:
.LBB0_45:                               # %while.body335
                                        # =>This Inner Loop Header: Depth=1
	.loc	1 266 0                 # decompress.c:266:0
	movq	-40(%rbp), %rax
	movslq	36(%rax), %rax
	cmpq	$7, %rax
	jg	.LBB0_49
.Ltmp152:
# BB#46:                                # %if.end349
                                        #   in Loop: Header=BB0_45 Depth=1
	.loc	1 266 0                 # decompress.c:266:0
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	movl	8(%rax), %eax
	testq	%rax, %rax
	je	.LBB0_7
.Ltmp153:
# BB#47:                                # %if.end355
                                        #   in Loop: Header=BB0_45 Depth=1
	.loc	1 266 0                 # decompress.c:266:0
	movq	-40(%rbp), %rax
	movq	(%rax), %rcx
	movq	(%rcx), %rcx
	movl	32(%rax), %edx
	movzbl	(%rcx), %esi
	movq	$8, %rcx
	shlq	%cl, %rdx
	orq	%rdx, %rsi
	movl	%esi, 32(%rax)
	movq	-40(%rbp), %rax
	movl	36(%rax), %ecx
	addq	$8, %rcx
	movl	%ecx, 36(%rax)
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	incq	(%rax)
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	movl	8(%rax), %ecx
	decq	%rcx
	movl	%ecx, 8(%rax)
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	movl	12(%rax), %ecx
	incq	%rcx
	movl	%ecx, 12(%rax)
	.loc	1 266 0                 # decompress.c:266:0
.Ltmp154:
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	movl	12(%rax), %eax
	testq	%rax, %rax
	jne	.LBB0_45
	jmp	.LBB0_48
.Ltmp155:
.LBB0_49:                               # %while.end383
	#DEBUG_VALUE: v <- [RBP+-200]
	.loc	1 266 0                 # decompress.c:266:0
	movq	-40(%rbp), %rax
	movl	36(%rax), %ecx
	movl	32(%rax), %eax
	addq	$-8, %rcx
	andq	%r14, %rcx
	movq	-40(%rbp), %rdx
	shrq	%cl, %rax
	andq	$255, %rax
	movl	%eax, -200(%rbp)
	movl	36(%rdx), %eax
	addq	$-8, %rax
	movl	%eax, 36(%rdx)
	movzbl	-200(%rbp), %eax
	movb	%al, -41(%rbp)
.Ltmp156:
	.loc	1 267 0                 # decompress.c:267:0
	movzbl	-41(%rbp), %eax
	cmpq	$65, %rax
	jne	.LBB0_43
.Ltmp157:
.LBB0_50:                               # %sw.bb389
	.loc	1 268 0                 # decompress.c:268:0
	movq	-40(%rbp), %rax
	movq	$16, %rcx
	movl	%ecx, 8(%rax)
	jmp	.LBB0_51
.LBB0_54:                               # %if.then434
                                        #   in Loop: Header=BB0_51 Depth=1
	.loc	1 268 0                 # decompress.c:268:0
.Ltmp158:
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	movl	16(%rax), %ecx
	incq	%rcx
	movl	%ecx, 16(%rax)
.Ltmp159:
.LBB0_51:                               # %while.body391
                                        # =>This Inner Loop Header: Depth=1
	.loc	1 268 0                 # decompress.c:268:0
	movq	-40(%rbp), %rax
	movslq	36(%rax), %rax
	cmpq	$7, %rax
	jg	.LBB0_55
.Ltmp160:
# BB#52:                                # %if.end405
                                        #   in Loop: Header=BB0_51 Depth=1
	.loc	1 268 0                 # decompress.c:268:0
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	movl	8(%rax), %eax
	testq	%rax, %rax
	je	.LBB0_7
.Ltmp161:
# BB#53:                                # %if.end411
                                        #   in Loop: Header=BB0_51 Depth=1
	.loc	1 268 0                 # decompress.c:268:0
	movq	-40(%rbp), %rax
	movq	(%rax), %rcx
	movq	(%rcx), %rcx
	movl	32(%rax), %edx
	movzbl	(%rcx), %esi
	movq	$8, %rcx
	shlq	%cl, %rdx
	orq	%rdx, %rsi
	movl	%esi, 32(%rax)
	movq	-40(%rbp), %rax
	movl	36(%rax), %ecx
	addq	$8, %rcx
	movl	%ecx, 36(%rax)
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	incq	(%rax)
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	movl	8(%rax), %ecx
	decq	%rcx
	movl	%ecx, 8(%rax)
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	movl	12(%rax), %ecx
	incq	%rcx
	movl	%ecx, 12(%rax)
	.loc	1 268 0                 # decompress.c:268:0
.Ltmp162:
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	movl	12(%rax), %eax
	testq	%rax, %rax
	jne	.LBB0_51
	jmp	.LBB0_54
.Ltmp163:
.LBB0_55:                               # %while.end439
	#DEBUG_VALUE: v <- [RBP+-204]
	.loc	1 268 0                 # decompress.c:268:0
	movq	-40(%rbp), %rax
	movl	36(%rax), %ecx
	movl	32(%rax), %eax
	addq	$-8, %rcx
	andq	%r14, %rcx
	movq	-40(%rbp), %rdx
	shrq	%cl, %rax
	andq	$255, %rax
	movl	%eax, -204(%rbp)
	movl	36(%rdx), %eax
	addq	$-8, %rax
	movl	%eax, 36(%rdx)
	movzbl	-204(%rbp), %eax
	movb	%al, -41(%rbp)
.Ltmp164:
	.loc	1 269 0                 # decompress.c:269:0
	movzbl	-41(%rbp), %eax
	cmpq	$89, %rax
	jne	.LBB0_43
.Ltmp165:
.LBB0_56:                               # %sw.bb445
	.loc	1 270 0                 # decompress.c:270:0
	movq	-40(%rbp), %rax
	movq	$17, %rcx
	movl	%ecx, 8(%rax)
	jmp	.LBB0_57
.LBB0_60:                               # %if.then490
                                        #   in Loop: Header=BB0_57 Depth=1
	.loc	1 270 0                 # decompress.c:270:0
.Ltmp166:
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	movl	16(%rax), %ecx
	incq	%rcx
	movl	%ecx, 16(%rax)
.Ltmp167:
.LBB0_57:                               # %while.body447
                                        # =>This Inner Loop Header: Depth=1
	.loc	1 270 0                 # decompress.c:270:0
	movq	-40(%rbp), %rax
	movslq	36(%rax), %rax
	cmpq	$7, %rax
	jg	.LBB0_61
.Ltmp168:
# BB#58:                                # %if.end461
                                        #   in Loop: Header=BB0_57 Depth=1
	.loc	1 270 0                 # decompress.c:270:0
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	movl	8(%rax), %eax
	testq	%rax, %rax
	je	.LBB0_7
.Ltmp169:
# BB#59:                                # %if.end467
                                        #   in Loop: Header=BB0_57 Depth=1
	.loc	1 270 0                 # decompress.c:270:0
	movq	-40(%rbp), %rax
	movq	(%rax), %rcx
	movq	(%rcx), %rcx
	movl	32(%rax), %edx
	movzbl	(%rcx), %esi
	movq	$8, %rcx
	shlq	%cl, %rdx
	orq	%rdx, %rsi
	movl	%esi, 32(%rax)
	movq	-40(%rbp), %rax
	movl	36(%rax), %ecx
	addq	$8, %rcx
	movl	%ecx, 36(%rax)
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	incq	(%rax)
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	movl	8(%rax), %ecx
	decq	%rcx
	movl	%ecx, 8(%rax)
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	movl	12(%rax), %ecx
	incq	%rcx
	movl	%ecx, 12(%rax)
	.loc	1 270 0                 # decompress.c:270:0
.Ltmp170:
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	movl	12(%rax), %eax
	testq	%rax, %rax
	jne	.LBB0_57
	jmp	.LBB0_60
.Ltmp171:
.LBB0_61:                               # %while.end495
	#DEBUG_VALUE: v <- [RBP+-208]
	.loc	1 270 0                 # decompress.c:270:0
	movq	-40(%rbp), %rax
	movl	36(%rax), %ecx
	movl	32(%rax), %eax
	addq	$-8, %rcx
	andq	%r14, %rcx
	movq	-40(%rbp), %rdx
	shrq	%cl, %rax
	andq	$255, %rax
	movl	%eax, -208(%rbp)
	movl	36(%rdx), %eax
	addq	$-8, %rax
	movl	%eax, 36(%rdx)
	movzbl	-208(%rbp), %eax
	movb	%al, -41(%rbp)
.Ltmp172:
	.loc	1 271 0                 # decompress.c:271:0
	movzbl	-41(%rbp), %eax
	cmpq	$38, %rax
	jne	.LBB0_43
.Ltmp173:
.LBB0_62:                               # %sw.bb501
	.loc	1 272 0                 # decompress.c:272:0
	movq	-40(%rbp), %rax
	movq	$18, %rcx
	movl	%ecx, 8(%rax)
	jmp	.LBB0_63
.LBB0_66:                               # %if.then546
                                        #   in Loop: Header=BB0_63 Depth=1
	.loc	1 272 0                 # decompress.c:272:0
.Ltmp174:
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	movl	16(%rax), %ecx
	incq	%rcx
	movl	%ecx, 16(%rax)
.Ltmp175:
.LBB0_63:                               # %while.body503
                                        # =>This Inner Loop Header: Depth=1
	.loc	1 272 0                 # decompress.c:272:0
	movq	-40(%rbp), %rax
	movslq	36(%rax), %rax
	cmpq	$7, %rax
	jg	.LBB0_67
.Ltmp176:
# BB#64:                                # %if.end517
                                        #   in Loop: Header=BB0_63 Depth=1
	.loc	1 272 0                 # decompress.c:272:0
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	movl	8(%rax), %eax
	testq	%rax, %rax
	je	.LBB0_7
.Ltmp177:
# BB#65:                                # %if.end523
                                        #   in Loop: Header=BB0_63 Depth=1
	.loc	1 272 0                 # decompress.c:272:0
	movq	-40(%rbp), %rax
	movq	(%rax), %rcx
	movq	(%rcx), %rcx
	movl	32(%rax), %edx
	movzbl	(%rcx), %esi
	movq	$8, %rcx
	shlq	%cl, %rdx
	orq	%rdx, %rsi
	movl	%esi, 32(%rax)
	movq	-40(%rbp), %rax
	movl	36(%rax), %ecx
	addq	$8, %rcx
	movl	%ecx, 36(%rax)
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	incq	(%rax)
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	movl	8(%rax), %ecx
	decq	%rcx
	movl	%ecx, 8(%rax)
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	movl	12(%rax), %ecx
	incq	%rcx
	movl	%ecx, 12(%rax)
	.loc	1 272 0                 # decompress.c:272:0
.Ltmp178:
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	movl	12(%rax), %eax
	testq	%rax, %rax
	jne	.LBB0_63
	jmp	.LBB0_66
.Ltmp179:
.LBB0_67:                               # %while.end551
	#DEBUG_VALUE: v <- [RBP+-212]
	.loc	1 272 0                 # decompress.c:272:0
	movq	-40(%rbp), %rax
	movl	36(%rax), %ecx
	movl	32(%rax), %eax
	addq	$-8, %rcx
	andq	%r14, %rcx
	movq	-40(%rbp), %rdx
	shrq	%cl, %rax
	andq	$255, %rax
	movl	%eax, -212(%rbp)
	movl	36(%rdx), %eax
	addq	$-8, %rax
	movl	%eax, 36(%rdx)
	movzbl	-212(%rbp), %eax
	movb	%al, -41(%rbp)
.Ltmp180:
	.loc	1 273 0                 # decompress.c:273:0
	movzbl	-41(%rbp), %eax
	cmpq	$83, %rax
	jne	.LBB0_43
.Ltmp181:
.LBB0_68:                               # %sw.bb557
	.loc	1 274 0                 # decompress.c:274:0
	movq	-40(%rbp), %rax
	movq	$19, %rcx
	movl	%ecx, 8(%rax)
	jmp	.LBB0_69
.LBB0_72:                               # %if.then602
                                        #   in Loop: Header=BB0_69 Depth=1
	.loc	1 274 0                 # decompress.c:274:0
.Ltmp182:
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	movl	16(%rax), %ecx
	incq	%rcx
	movl	%ecx, 16(%rax)
.Ltmp183:
.LBB0_69:                               # %while.body559
                                        # =>This Inner Loop Header: Depth=1
	.loc	1 274 0                 # decompress.c:274:0
	movq	-40(%rbp), %rax
	movslq	36(%rax), %rax
	cmpq	$7, %rax
	jg	.LBB0_73
.Ltmp184:
# BB#70:                                # %if.end573
                                        #   in Loop: Header=BB0_69 Depth=1
	.loc	1 274 0                 # decompress.c:274:0
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	movl	8(%rax), %eax
	testq	%rax, %rax
	je	.LBB0_7
.Ltmp185:
# BB#71:                                # %if.end579
                                        #   in Loop: Header=BB0_69 Depth=1
	.loc	1 274 0                 # decompress.c:274:0
	movq	-40(%rbp), %rax
	movq	(%rax), %rcx
	movq	(%rcx), %rcx
	movl	32(%rax), %edx
	movzbl	(%rcx), %esi
	movq	$8, %rcx
	shlq	%cl, %rdx
	orq	%rdx, %rsi
	movl	%esi, 32(%rax)
	movq	-40(%rbp), %rax
	movl	36(%rax), %ecx
	addq	$8, %rcx
	movl	%ecx, 36(%rax)
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	incq	(%rax)
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	movl	8(%rax), %ecx
	decq	%rcx
	movl	%ecx, 8(%rax)
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	movl	12(%rax), %ecx
	incq	%rcx
	movl	%ecx, 12(%rax)
	.loc	1 274 0                 # decompress.c:274:0
.Ltmp186:
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	movl	12(%rax), %eax
	testq	%rax, %rax
	jne	.LBB0_69
	jmp	.LBB0_72
.Ltmp187:
.LBB0_73:                               # %while.end607
	#DEBUG_VALUE: v <- [RBP+-216]
	.loc	1 274 0                 # decompress.c:274:0
	movq	-40(%rbp), %rax
	movl	36(%rax), %ecx
	movl	32(%rax), %eax
	addq	$-8, %rcx
	andq	%r14, %rcx
	movq	-40(%rbp), %rdx
	shrq	%cl, %rax
	andq	$255, %rax
	movl	%eax, -216(%rbp)
	movl	36(%rdx), %eax
	addq	$-8, %rax
	movl	%eax, 36(%rdx)
	movzbl	-216(%rbp), %eax
	movb	%al, -41(%rbp)
.Ltmp188:
	.loc	1 275 0                 # decompress.c:275:0
	movzbl	-41(%rbp), %eax
	cmpq	$89, %rax
	jne	.LBB0_43
.Ltmp189:
# BB#74:                                # %if.end612
	.loc	1 277 0                 # decompress.c:277:0
	movq	-40(%rbp), %rax
	movl	48(%rax), %ecx
	incq	%rcx
	movl	%ecx, 48(%rax)
	.loc	1 278 0                 # decompress.c:278:0
.Ltmp190:
	movq	-40(%rbp), %rax
	movslq	52(%rax), %rax
	cmpq	$2, %rax
	jl	.LBB0_76
# BB#75:                                # %if.then616
	.loc	1 279 0                 # decompress.c:279:0
	movq	-40(%rbp), %rax
	movq	stderr(%rip), %rdi
	movl	48(%rax), %edx
	movabsq	$.L.str, %rsi
	xorq	%rax, %rax
	callq	fprintf
.Ltmp191:
.LBB0_76:                               # %if.end619
	.loc	1 281 0                 # decompress.c:281:0
	movq	-40(%rbp), %rax
	xorq	%rcx, %rcx
	movl	%ecx, 3176(%rax)
.LBB0_77:                               # %sw.bb620
	.loc	1 282 0                 # decompress.c:282:0
	movq	-40(%rbp), %rax
	movq	$20, %rcx
	movl	%ecx, 8(%rax)
	jmp	.LBB0_78
.LBB0_81:                               # %if.then665
                                        #   in Loop: Header=BB0_78 Depth=1
	.loc	1 282 0                 # decompress.c:282:0
.Ltmp192:
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	movl	16(%rax), %ecx
	incq	%rcx
	movl	%ecx, 16(%rax)
.Ltmp193:
.LBB0_78:                               # %while.body622
                                        # =>This Inner Loop Header: Depth=1
	.loc	1 282 0                 # decompress.c:282:0
	movq	-40(%rbp), %rax
	movslq	36(%rax), %rax
	cmpq	$7, %rax
	jg	.LBB0_82
.Ltmp194:
# BB#79:                                # %if.end636
                                        #   in Loop: Header=BB0_78 Depth=1
	.loc	1 282 0                 # decompress.c:282:0
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	movl	8(%rax), %eax
	testq	%rax, %rax
	je	.LBB0_7
.Ltmp195:
# BB#80:                                # %if.end642
                                        #   in Loop: Header=BB0_78 Depth=1
	.loc	1 282 0                 # decompress.c:282:0
	movq	-40(%rbp), %rax
	movq	(%rax), %rcx
	movq	(%rcx), %rcx
	movl	32(%rax), %edx
	movzbl	(%rcx), %esi
	movq	$8, %rcx
	shlq	%cl, %rdx
	orq	%rdx, %rsi
	movl	%esi, 32(%rax)
	movq	-40(%rbp), %rax
	movl	36(%rax), %ecx
	addq	$8, %rcx
	movl	%ecx, 36(%rax)
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	incq	(%rax)
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	movl	8(%rax), %ecx
	decq	%rcx
	movl	%ecx, 8(%rax)
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	movl	12(%rax), %ecx
	incq	%rcx
	movl	%ecx, 12(%rax)
	.loc	1 282 0                 # decompress.c:282:0
.Ltmp196:
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	movl	12(%rax), %eax
	testq	%rax, %rax
	jne	.LBB0_78
	jmp	.LBB0_81
.Ltmp197:
.LBB0_82:                               # %while.end670
	#DEBUG_VALUE: v <- [RBP+-220]
	.loc	1 282 0                 # decompress.c:282:0
	movq	-40(%rbp), %rax
	movl	36(%rax), %ecx
	movl	32(%rax), %eax
	addq	$-8, %rcx
	andq	%r14, %rcx
	movq	-40(%rbp), %rdx
	shrq	%cl, %rax
	andq	$255, %rax
	movl	%eax, -220(%rbp)
	movl	36(%rdx), %eax
	addq	$-8, %rax
	movl	%eax, 36(%rdx)
	movzbl	-220(%rbp), %eax
.Ltmp198:
	.loc	1 283 0                 # decompress.c:283:0
	movq	-40(%rbp), %rdx
	.loc	1 282 0                 # decompress.c:282:0
.Ltmp199:
	movb	%al, -41(%rbp)
.Ltmp200:
	.loc	1 283 0                 # decompress.c:283:0
	movl	3176(%rdx), %eax
	movzbl	-41(%rbp), %esi
	movq	$8, %rcx
	shlq	%cl, %rax
	orq	%rax, %rsi
	movl	%esi, 3176(%rdx)
.Ltmp201:
.LBB0_83:                               # %sw.bb676
	.loc	1 284 0                 # decompress.c:284:0
	movq	-40(%rbp), %rax
	movq	$21, %rcx
	movl	%ecx, 8(%rax)
	jmp	.LBB0_84
.LBB0_87:                               # %if.then721
                                        #   in Loop: Header=BB0_84 Depth=1
	.loc	1 284 0                 # decompress.c:284:0
.Ltmp202:
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	movl	16(%rax), %ecx
	incq	%rcx
	movl	%ecx, 16(%rax)
.Ltmp203:
.LBB0_84:                               # %while.body678
                                        # =>This Inner Loop Header: Depth=1
	.loc	1 284 0                 # decompress.c:284:0
	movq	-40(%rbp), %rax
	movslq	36(%rax), %rax
	cmpq	$7, %rax
	jg	.LBB0_88
.Ltmp204:
# BB#85:                                # %if.end692
                                        #   in Loop: Header=BB0_84 Depth=1
	.loc	1 284 0                 # decompress.c:284:0
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	movl	8(%rax), %eax
	testq	%rax, %rax
	je	.LBB0_7
.Ltmp205:
# BB#86:                                # %if.end698
                                        #   in Loop: Header=BB0_84 Depth=1
	.loc	1 284 0                 # decompress.c:284:0
	movq	-40(%rbp), %rax
	movq	(%rax), %rcx
	movq	(%rcx), %rcx
	movl	32(%rax), %edx
	movzbl	(%rcx), %esi
	movq	$8, %rcx
	shlq	%cl, %rdx
	orq	%rdx, %rsi
	movl	%esi, 32(%rax)
	movq	-40(%rbp), %rax
	movl	36(%rax), %ecx
	addq	$8, %rcx
	movl	%ecx, 36(%rax)
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	incq	(%rax)
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	movl	8(%rax), %ecx
	decq	%rcx
	movl	%ecx, 8(%rax)
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	movl	12(%rax), %ecx
	incq	%rcx
	movl	%ecx, 12(%rax)
	.loc	1 284 0                 # decompress.c:284:0
.Ltmp206:
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	movl	12(%rax), %eax
	testq	%rax, %rax
	jne	.LBB0_84
	jmp	.LBB0_87
.Ltmp207:
.LBB0_88:                               # %while.end726
	#DEBUG_VALUE: v <- [RBP+-224]
	.loc	1 284 0                 # decompress.c:284:0
	movq	-40(%rbp), %rax
	movl	36(%rax), %ecx
	movl	32(%rax), %eax
	addq	$-8, %rcx
	andq	%r14, %rcx
	movq	-40(%rbp), %rdx
	shrq	%cl, %rax
	andq	$255, %rax
	movl	%eax, -224(%rbp)
	movl	36(%rdx), %eax
	addq	$-8, %rax
	movl	%eax, 36(%rdx)
	movzbl	-224(%rbp), %eax
.Ltmp208:
	.loc	1 285 0                 # decompress.c:285:0
	movq	-40(%rbp), %rdx
	.loc	1 284 0                 # decompress.c:284:0
.Ltmp209:
	movb	%al, -41(%rbp)
.Ltmp210:
	.loc	1 285 0                 # decompress.c:285:0
	movl	3176(%rdx), %eax
	movzbl	-41(%rbp), %esi
	movq	$8, %rcx
	shlq	%cl, %rax
	orq	%rax, %rsi
	movl	%esi, 3176(%rdx)
.Ltmp211:
.LBB0_89:                               # %sw.bb732
	.loc	1 286 0                 # decompress.c:286:0
	movq	-40(%rbp), %rax
	movq	$22, %rcx
	movl	%ecx, 8(%rax)
	jmp	.LBB0_90
.LBB0_93:                               # %if.then777
                                        #   in Loop: Header=BB0_90 Depth=1
	.loc	1 286 0                 # decompress.c:286:0
.Ltmp212:
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	movl	16(%rax), %ecx
	incq	%rcx
	movl	%ecx, 16(%rax)
.Ltmp213:
.LBB0_90:                               # %while.body734
                                        # =>This Inner Loop Header: Depth=1
	.loc	1 286 0                 # decompress.c:286:0
	movq	-40(%rbp), %rax
	movslq	36(%rax), %rax
	cmpq	$7, %rax
	jg	.LBB0_94
.Ltmp214:
# BB#91:                                # %if.end748
                                        #   in Loop: Header=BB0_90 Depth=1
	.loc	1 286 0                 # decompress.c:286:0
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	movl	8(%rax), %eax
	testq	%rax, %rax
	je	.LBB0_7
.Ltmp215:
# BB#92:                                # %if.end754
                                        #   in Loop: Header=BB0_90 Depth=1
	.loc	1 286 0                 # decompress.c:286:0
	movq	-40(%rbp), %rax
	movq	(%rax), %rcx
	movq	(%rcx), %rcx
	movl	32(%rax), %edx
	movzbl	(%rcx), %esi
	movq	$8, %rcx
	shlq	%cl, %rdx
	orq	%rdx, %rsi
	movl	%esi, 32(%rax)
	movq	-40(%rbp), %rax
	movl	36(%rax), %ecx
	addq	$8, %rcx
	movl	%ecx, 36(%rax)
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	incq	(%rax)
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	movl	8(%rax), %ecx
	decq	%rcx
	movl	%ecx, 8(%rax)
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	movl	12(%rax), %ecx
	incq	%rcx
	movl	%ecx, 12(%rax)
	.loc	1 286 0                 # decompress.c:286:0
.Ltmp216:
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	movl	12(%rax), %eax
	testq	%rax, %rax
	jne	.LBB0_90
	jmp	.LBB0_93
.Ltmp217:
.LBB0_94:                               # %while.end782
	#DEBUG_VALUE: v <- [RBP+-228]
	.loc	1 286 0                 # decompress.c:286:0
	movq	-40(%rbp), %rax
	movl	36(%rax), %ecx
	movl	32(%rax), %eax
	addq	$-8, %rcx
	andq	%r14, %rcx
	movq	-40(%rbp), %rdx
	shrq	%cl, %rax
	andq	$255, %rax
	movl	%eax, -228(%rbp)
	movl	36(%rdx), %eax
	addq	$-8, %rax
	movl	%eax, 36(%rdx)
	movzbl	-228(%rbp), %eax
.Ltmp218:
	.loc	1 287 0                 # decompress.c:287:0
	movq	-40(%rbp), %rdx
	.loc	1 286 0                 # decompress.c:286:0
.Ltmp219:
	movb	%al, -41(%rbp)
.Ltmp220:
	.loc	1 287 0                 # decompress.c:287:0
	movl	3176(%rdx), %eax
	movzbl	-41(%rbp), %esi
	movq	$8, %rcx
	shlq	%cl, %rax
	orq	%rax, %rsi
	movl	%esi, 3176(%rdx)
.Ltmp221:
.LBB0_95:                               # %sw.bb788
	.loc	1 288 0                 # decompress.c:288:0
	movq	-40(%rbp), %rax
	movq	$23, %rcx
	movl	%ecx, 8(%rax)
	jmp	.LBB0_96
.LBB0_99:                               # %if.then833
                                        #   in Loop: Header=BB0_96 Depth=1
	.loc	1 288 0                 # decompress.c:288:0
.Ltmp222:
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	movl	16(%rax), %ecx
	incq	%rcx
	movl	%ecx, 16(%rax)
.Ltmp223:
.LBB0_96:                               # %while.body790
                                        # =>This Inner Loop Header: Depth=1
	.loc	1 288 0                 # decompress.c:288:0
	movq	-40(%rbp), %rax
	movslq	36(%rax), %rax
	cmpq	$7, %rax
	jg	.LBB0_100
.Ltmp224:
# BB#97:                                # %if.end804
                                        #   in Loop: Header=BB0_96 Depth=1
	.loc	1 288 0                 # decompress.c:288:0
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	movl	8(%rax), %eax
	testq	%rax, %rax
	je	.LBB0_7
.Ltmp225:
# BB#98:                                # %if.end810
                                        #   in Loop: Header=BB0_96 Depth=1
	.loc	1 288 0                 # decompress.c:288:0
	movq	-40(%rbp), %rax
	movq	(%rax), %rcx
	movq	(%rcx), %rcx
	movl	32(%rax), %edx
	movzbl	(%rcx), %esi
	movq	$8, %rcx
	shlq	%cl, %rdx
	orq	%rdx, %rsi
	movl	%esi, 32(%rax)
	movq	-40(%rbp), %rax
	movl	36(%rax), %ecx
	addq	$8, %rcx
	movl	%ecx, 36(%rax)
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	incq	(%rax)
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	movl	8(%rax), %ecx
	decq	%rcx
	movl	%ecx, 8(%rax)
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	movl	12(%rax), %ecx
	incq	%rcx
	movl	%ecx, 12(%rax)
	.loc	1 288 0                 # decompress.c:288:0
.Ltmp226:
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	movl	12(%rax), %eax
	testq	%rax, %rax
	jne	.LBB0_96
	jmp	.LBB0_99
.Ltmp227:
.LBB0_100:                              # %while.end838
	#DEBUG_VALUE: v <- [RBP+-232]
	.loc	1 288 0                 # decompress.c:288:0
	movq	-40(%rbp), %rax
	movl	36(%rax), %ecx
	movl	32(%rax), %eax
	addq	$-8, %rcx
	andq	%r14, %rcx
	movq	-40(%rbp), %rdx
	shrq	%cl, %rax
	andq	$255, %rax
	movl	%eax, -232(%rbp)
	movl	36(%rdx), %eax
	addq	$-8, %rax
	movl	%eax, 36(%rdx)
	movzbl	-232(%rbp), %eax
.Ltmp228:
	.loc	1 289 0                 # decompress.c:289:0
	movq	-40(%rbp), %rdx
	.loc	1 288 0                 # decompress.c:288:0
.Ltmp229:
	movb	%al, -41(%rbp)
.Ltmp230:
	.loc	1 289 0                 # decompress.c:289:0
	movl	3176(%rdx), %eax
	movzbl	-41(%rbp), %esi
	movq	$8, %rcx
	shlq	%cl, %rax
	orq	%rax, %rsi
	movl	%esi, 3176(%rdx)
.Ltmp231:
.LBB0_101:                              # %sw.bb844
	.loc	1 291 0                 # decompress.c:291:0
	movq	-40(%rbp), %rax
	movq	$24, %rcx
	movl	%ecx, 8(%rax)
	jmp	.LBB0_102
.LBB0_105:                              # %if.then889
                                        #   in Loop: Header=BB0_102 Depth=1
	.loc	1 291 0                 # decompress.c:291:0
.Ltmp232:
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	movl	16(%rax), %ecx
	incq	%rcx
	movl	%ecx, 16(%rax)
.Ltmp233:
.LBB0_102:                              # %while.body846
                                        # =>This Inner Loop Header: Depth=1
	.loc	1 291 0                 # decompress.c:291:0
	movq	-40(%rbp), %rax
	movslq	36(%rax), %rax
	testq	%rax, %rax
	jg	.LBB0_106
.Ltmp234:
# BB#103:                               # %if.end860
                                        #   in Loop: Header=BB0_102 Depth=1
	.loc	1 291 0                 # decompress.c:291:0
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	movl	8(%rax), %eax
	testq	%rax, %rax
	je	.LBB0_7
.Ltmp235:
# BB#104:                               # %if.end866
                                        #   in Loop: Header=BB0_102 Depth=1
	.loc	1 291 0                 # decompress.c:291:0
	movq	-40(%rbp), %rax
	movq	(%rax), %rcx
	movq	(%rcx), %rcx
	movl	32(%rax), %edx
	movzbl	(%rcx), %esi
	movq	$8, %rcx
	shlq	%cl, %rdx
	orq	%rdx, %rsi
	movl	%esi, 32(%rax)
	movq	-40(%rbp), %rax
	movl	36(%rax), %ecx
	addq	$8, %rcx
	movl	%ecx, 36(%rax)
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	incq	(%rax)
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	movl	8(%rax), %ecx
	decq	%rcx
	movl	%ecx, 8(%rax)
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	movl	12(%rax), %ecx
	incq	%rcx
	movl	%ecx, 12(%rax)
	.loc	1 291 0                 # decompress.c:291:0
.Ltmp236:
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	movl	12(%rax), %eax
	testq	%rax, %rax
	jne	.LBB0_102
	jmp	.LBB0_105
.Ltmp237:
.LBB0_106:                              # %while.end894
	#DEBUG_VALUE: v <- [RBP+-236]
	.loc	1 291 0                 # decompress.c:291:0
	movq	-40(%rbp), %rax
	movl	36(%rax), %ecx
	movl	32(%rax), %eax
	decq	%rcx
	andq	%r14, %rcx
	movq	-40(%rbp), %rdx
	shrq	%cl, %rax
	andq	$1, %rax
	movl	%eax, -236(%rbp)
	movl	36(%rdx), %eax
	decq	%rax
	movl	%eax, 36(%rdx)
	movzbl	-236(%rbp), %eax
	movq	-40(%rbp), %rcx
	movb	%al, 20(%rcx)
.Ltmp238:
	.loc	1 293 0                 # decompress.c:293:0
	movq	-40(%rbp), %rax
	xorq	%rcx, %rcx
	movl	%ecx, 56(%rax)
.Ltmp239:
.LBB0_107:                              # %sw.bb895
	.loc	1 294 0                 # decompress.c:294:0
	movq	-40(%rbp), %rax
	movq	$25, %rcx
	movl	%ecx, 8(%rax)
	jmp	.LBB0_108
.LBB0_111:                              # %if.then940
                                        #   in Loop: Header=BB0_108 Depth=1
	.loc	1 294 0                 # decompress.c:294:0
.Ltmp240:
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	movl	16(%rax), %ecx
	incq	%rcx
	movl	%ecx, 16(%rax)
.Ltmp241:
.LBB0_108:                              # %while.body897
                                        # =>This Inner Loop Header: Depth=1
	.loc	1 294 0                 # decompress.c:294:0
	movq	-40(%rbp), %rax
	movslq	36(%rax), %rax
	cmpq	$7, %rax
	jg	.LBB0_112
.Ltmp242:
# BB#109:                               # %if.end911
                                        #   in Loop: Header=BB0_108 Depth=1
	.loc	1 294 0                 # decompress.c:294:0
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	movl	8(%rax), %eax
	testq	%rax, %rax
	je	.LBB0_7
.Ltmp243:
# BB#110:                               # %if.end917
                                        #   in Loop: Header=BB0_108 Depth=1
	.loc	1 294 0                 # decompress.c:294:0
	movq	-40(%rbp), %rax
	movq	(%rax), %rcx
	movq	(%rcx), %rcx
	movl	32(%rax), %edx
	movzbl	(%rcx), %esi
	movq	$8, %rcx
	shlq	%cl, %rdx
	orq	%rdx, %rsi
	movl	%esi, 32(%rax)
	movq	-40(%rbp), %rax
	movl	36(%rax), %ecx
	addq	$8, %rcx
	movl	%ecx, 36(%rax)
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	incq	(%rax)
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	movl	8(%rax), %ecx
	decq	%rcx
	movl	%ecx, 8(%rax)
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	movl	12(%rax), %ecx
	incq	%rcx
	movl	%ecx, 12(%rax)
	.loc	1 294 0                 # decompress.c:294:0
.Ltmp244:
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	movl	12(%rax), %eax
	testq	%rax, %rax
	jne	.LBB0_108
	jmp	.LBB0_111
.Ltmp245:
.LBB0_112:                              # %while.end945
	#DEBUG_VALUE: v <- [RBP+-240]
	.loc	1 294 0                 # decompress.c:294:0
	movq	-40(%rbp), %rax
	movl	36(%rax), %ecx
	movl	32(%rax), %eax
	addq	$-8, %rcx
	andq	%r14, %rcx
	movq	-40(%rbp), %rdx
	shrq	%cl, %rax
	andq	$255, %rax
	movl	%eax, -240(%rbp)
	movl	36(%rdx), %eax
	addq	$-8, %rax
	movl	%eax, 36(%rdx)
	movzbl	-240(%rbp), %eax
.Ltmp246:
	.loc	1 295 0                 # decompress.c:295:0
	movq	-40(%rbp), %rdx
	.loc	1 294 0                 # decompress.c:294:0
.Ltmp247:
	movb	%al, -41(%rbp)
.Ltmp248:
	.loc	1 295 0                 # decompress.c:295:0
	movl	56(%rdx), %eax
	movzbl	-41(%rbp), %esi
	movq	$8, %rcx
	shlq	%cl, %rax
	orq	%rax, %rsi
	movl	%esi, 56(%rdx)
.Ltmp249:
.LBB0_113:                              # %sw.bb951
	.loc	1 296 0                 # decompress.c:296:0
	movq	-40(%rbp), %rax
	movq	$26, %rcx
	movl	%ecx, 8(%rax)
	jmp	.LBB0_114
.LBB0_117:                              # %if.then996
                                        #   in Loop: Header=BB0_114 Depth=1
	.loc	1 296 0                 # decompress.c:296:0
.Ltmp250:
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	movl	16(%rax), %ecx
	incq	%rcx
	movl	%ecx, 16(%rax)
.Ltmp251:
.LBB0_114:                              # %while.body953
                                        # =>This Inner Loop Header: Depth=1
	.loc	1 296 0                 # decompress.c:296:0
	movq	-40(%rbp), %rax
	movslq	36(%rax), %rax
	cmpq	$7, %rax
	jg	.LBB0_118
.Ltmp252:
# BB#115:                               # %if.end967
                                        #   in Loop: Header=BB0_114 Depth=1
	.loc	1 296 0                 # decompress.c:296:0
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	movl	8(%rax), %eax
	testq	%rax, %rax
	je	.LBB0_7
.Ltmp253:
# BB#116:                               # %if.end973
                                        #   in Loop: Header=BB0_114 Depth=1
	.loc	1 296 0                 # decompress.c:296:0
	movq	-40(%rbp), %rax
	movq	(%rax), %rcx
	movq	(%rcx), %rcx
	movl	32(%rax), %edx
	movzbl	(%rcx), %esi
	movq	$8, %rcx
	shlq	%cl, %rdx
	orq	%rdx, %rsi
	movl	%esi, 32(%rax)
	movq	-40(%rbp), %rax
	movl	36(%rax), %ecx
	addq	$8, %rcx
	movl	%ecx, 36(%rax)
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	incq	(%rax)
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	movl	8(%rax), %ecx
	decq	%rcx
	movl	%ecx, 8(%rax)
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	movl	12(%rax), %ecx
	incq	%rcx
	movl	%ecx, 12(%rax)
	.loc	1 296 0                 # decompress.c:296:0
.Ltmp254:
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	movl	12(%rax), %eax
	testq	%rax, %rax
	jne	.LBB0_114
	jmp	.LBB0_117
.Ltmp255:
.LBB0_118:                              # %while.end1001
	#DEBUG_VALUE: v <- [RBP+-244]
	.loc	1 296 0                 # decompress.c:296:0
	movq	-40(%rbp), %rax
	movl	36(%rax), %ecx
	movl	32(%rax), %eax
	addq	$-8, %rcx
	andq	%r14, %rcx
	movq	-40(%rbp), %rdx
	shrq	%cl, %rax
	andq	$255, %rax
	movl	%eax, -244(%rbp)
	movl	36(%rdx), %eax
	addq	$-8, %rax
	movl	%eax, 36(%rdx)
	movzbl	-244(%rbp), %eax
.Ltmp256:
	.loc	1 297 0                 # decompress.c:297:0
	movq	-40(%rbp), %rdx
	.loc	1 296 0                 # decompress.c:296:0
.Ltmp257:
	movb	%al, -41(%rbp)
.Ltmp258:
	.loc	1 297 0                 # decompress.c:297:0
	movl	56(%rdx), %eax
	movzbl	-41(%rbp), %esi
	movq	$8, %rcx
	shlq	%cl, %rax
	orq	%rax, %rsi
	movl	%esi, 56(%rdx)
.Ltmp259:
.LBB0_119:                              # %sw.bb1007
	.loc	1 298 0                 # decompress.c:298:0
	movq	-40(%rbp), %rax
	movq	$27, %rcx
	movl	%ecx, 8(%rax)
	jmp	.LBB0_120
.LBB0_123:                              # %if.then1052
                                        #   in Loop: Header=BB0_120 Depth=1
	.loc	1 298 0                 # decompress.c:298:0
.Ltmp260:
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	movl	16(%rax), %ecx
	incq	%rcx
	movl	%ecx, 16(%rax)
.Ltmp261:
.LBB0_120:                              # %while.body1009
                                        # =>This Inner Loop Header: Depth=1
	.loc	1 298 0                 # decompress.c:298:0
	movq	-40(%rbp), %rax
	movslq	36(%rax), %rax
	cmpq	$7, %rax
	jg	.LBB0_124
.Ltmp262:
# BB#121:                               # %if.end1023
                                        #   in Loop: Header=BB0_120 Depth=1
	.loc	1 298 0                 # decompress.c:298:0
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	movl	8(%rax), %eax
	testq	%rax, %rax
	je	.LBB0_7
.Ltmp263:
# BB#122:                               # %if.end1029
                                        #   in Loop: Header=BB0_120 Depth=1
	.loc	1 298 0                 # decompress.c:298:0
	movq	-40(%rbp), %rax
	movq	(%rax), %rcx
	movq	(%rcx), %rcx
	movl	32(%rax), %edx
	movzbl	(%rcx), %esi
	movq	$8, %rcx
	shlq	%cl, %rdx
	orq	%rdx, %rsi
	movl	%esi, 32(%rax)
	movq	-40(%rbp), %rax
	movl	36(%rax), %ecx
	addq	$8, %rcx
	movl	%ecx, 36(%rax)
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	incq	(%rax)
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	movl	8(%rax), %ecx
	decq	%rcx
	movl	%ecx, 8(%rax)
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	movl	12(%rax), %ecx
	incq	%rcx
	movl	%ecx, 12(%rax)
	.loc	1 298 0                 # decompress.c:298:0
.Ltmp264:
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	movl	12(%rax), %eax
	testq	%rax, %rax
	jne	.LBB0_120
	jmp	.LBB0_123
.Ltmp265:
.LBB0_124:                              # %while.end1057
	#DEBUG_VALUE: v <- [RBP+-248]
	.loc	1 298 0                 # decompress.c:298:0
	movq	-40(%rbp), %rax
	movl	36(%rax), %ecx
	movl	32(%rax), %eax
	addq	$-8, %rcx
	andq	%r14, %rcx
	movq	-40(%rbp), %rdx
	shrq	%cl, %rax
	andq	$255, %rax
	movl	%eax, -248(%rbp)
	movl	36(%rdx), %eax
	addq	$-8, %rax
	movl	%eax, 36(%rdx)
	movzbl	-248(%rbp), %eax
.Ltmp266:
	.loc	1 299 0                 # decompress.c:299:0
	movq	-40(%rbp), %rdx
	.loc	1 298 0                 # decompress.c:298:0
.Ltmp267:
	movb	%al, -41(%rbp)
.Ltmp268:
	.loc	1 299 0                 # decompress.c:299:0
	movl	56(%rdx), %eax
	movzbl	-41(%rbp), %esi
	movq	$8, %rcx
	shlq	%cl, %rax
	orq	%rax, %rsi
	movl	%esi, 56(%rdx)
	.loc	1 301 0                 # decompress.c:301:0
.Ltmp269:
	movq	-40(%rbp), %rax
	movslq	56(%rax), %rax
	testq	%rax, %rax
	js	.LBB0_43
.Ltmp270:
# BB#125:                               # %if.end1067
	.loc	1 303 0                 # decompress.c:303:0
	movq	-40(%rbp), %rax
	movl	40(%rax), %ecx
	movslq	56(%rax), %rax
	imulq	$100000, %rcx, %rdx     # imm = 0x186A0
	orq	$10, %rdx
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rdx, %rax
	jg	.LBB0_43
.Ltmp271:
# BB#126:                               # %if.end1075
	xorq	%rax, %rax
	jmp	.LBB0_127
.LBB0_34:                               # %if.then256
	movabsq	$4294967293, %rax       # imm = 0xFFFFFFFD
	.loc	1 256 0                 # decompress.c:256:0
.Ltmp272:
	movl	%eax, -48(%rbp)
	jmp	.LBB0_445
.Ltmp273:
.LBB0_7:                                # %if.then37
	xorq	%rax, %rax
	.loc	1 237 0                 # decompress.c:237:0
.Ltmp274:
	movl	%eax, -48(%rbp)
	jmp	.LBB0_445
.Ltmp275:
.LBB0_43:                               # %if.then331
	movabsq	$4294967292, %rax       # imm = 0xFFFFFFFC
	.loc	1 265 0                 # decompress.c:265:0
.Ltmp276:
	movl	%eax, -48(%rbp)
.Ltmp277:
.LBB0_445:                              # %save_state_and_return
	.loc	1 635 0                 # decompress.c:635:0
	movl	-68(%rbp), %eax
	movq	-40(%rbp), %rcx
	movl	%eax, 64036(%rcx)
	.loc	1 636 0                 # decompress.c:636:0
	movl	-72(%rbp), %eax
	movq	-40(%rbp), %rcx
	movl	%eax, 64040(%rcx)
	.loc	1 637 0                 # decompress.c:637:0
	movl	-76(%rbp), %eax
	movq	-40(%rbp), %rcx
	movl	%eax, 64044(%rcx)
	.loc	1 638 0                 # decompress.c:638:0
	movl	-80(%rbp), %eax
	movq	-40(%rbp), %rcx
	movl	%eax, 64048(%rcx)
	.loc	1 639 0                 # decompress.c:639:0
	movl	-84(%rbp), %eax
	movq	-40(%rbp), %rcx
	movl	%eax, 64052(%rcx)
	.loc	1 640 0                 # decompress.c:640:0
	movl	-88(%rbp), %eax
	movq	-40(%rbp), %rcx
	movl	%eax, 64056(%rcx)
	.loc	1 641 0                 # decompress.c:641:0
	movl	-92(%rbp), %eax
	movq	-40(%rbp), %rcx
	movl	%eax, 64060(%rcx)
	.loc	1 642 0                 # decompress.c:642:0
	movl	-96(%rbp), %eax
	movq	-40(%rbp), %rcx
	movl	%eax, 64064(%rcx)
	.loc	1 643 0                 # decompress.c:643:0
	movl	-100(%rbp), %eax
	movq	-40(%rbp), %rcx
	movl	%eax, 64068(%rcx)
	.loc	1 644 0                 # decompress.c:644:0
	movl	-104(%rbp), %eax
	movq	-40(%rbp), %rcx
	movl	%eax, 64072(%rcx)
	.loc	1 645 0                 # decompress.c:645:0
	movl	-108(%rbp), %eax
	movq	-40(%rbp), %rcx
	movl	%eax, 64076(%rcx)
	.loc	1 646 0                 # decompress.c:646:0
	movl	-112(%rbp), %eax
	movq	-40(%rbp), %rcx
	movl	%eax, 64080(%rcx)
	.loc	1 647 0                 # decompress.c:647:0
	movl	-116(%rbp), %eax
	movq	-40(%rbp), %rcx
	movl	%eax, 64084(%rcx)
	.loc	1 648 0                 # decompress.c:648:0
	movl	-120(%rbp), %eax
	movq	-40(%rbp), %rcx
	movl	%eax, 64088(%rcx)
	.loc	1 649 0                 # decompress.c:649:0
	movl	-124(%rbp), %eax
	movq	-40(%rbp), %rcx
	movl	%eax, 64092(%rcx)
	.loc	1 650 0                 # decompress.c:650:0
	movl	-128(%rbp), %eax
	movq	-40(%rbp), %rcx
	movl	%eax, 64096(%rcx)
	.loc	1 651 0                 # decompress.c:651:0
	movl	-132(%rbp), %eax
	movq	-40(%rbp), %rcx
	movl	%eax, 64100(%rcx)
	.loc	1 652 0                 # decompress.c:652:0
	movl	-136(%rbp), %eax
	movq	-40(%rbp), %rcx
	movl	%eax, 64104(%rcx)
	.loc	1 653 0                 # decompress.c:653:0
	movl	-140(%rbp), %eax
	movq	-40(%rbp), %rcx
	movl	%eax, 64108(%rcx)
	.loc	1 654 0                 # decompress.c:654:0
	movl	-144(%rbp), %eax
	movq	-40(%rbp), %rcx
	movl	%eax, 64112(%rcx)
	.loc	1 655 0                 # decompress.c:655:0
	movl	-148(%rbp), %eax
	movq	-40(%rbp), %rcx
	movl	%eax, 64116(%rcx)
	.loc	1 656 0                 # decompress.c:656:0
	movq	-160(%rbp), %rax
	movq	-40(%rbp), %rcx
	movq	%rax, 64120(%rcx)
	.loc	1 657 0                 # decompress.c:657:0
	movq	-168(%rbp), %rax
	movq	-40(%rbp), %rcx
	movq	%rax, 64128(%rcx)
	.loc	1 658 0                 # decompress.c:658:0
	movq	-176(%rbp), %rax
	movq	-40(%rbp), %rcx
	movq	%rax, 64136(%rcx)
	.loc	1 660 0                 # decompress.c:660:0
	movl	-48(%rbp), %eax
	addq	$384, %rsp              # imm = 0x180
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	popq	%rbp
	retq
.LBB0_127:                              # %for.cond
	.loc	1 307 0                 # decompress.c:307:0
.Ltmp278:
	movl	%eax, -68(%rbp)
	movslq	-68(%rbp), %rax
	cmpq	$15, %rax
	jg	.LBB0_137
.LBB0_128:                              # %sw.bb1078
	.loc	1 308 0                 # decompress.c:308:0
.Ltmp279:
	movq	-40(%rbp), %rax
	movq	$28, %rcx
	movl	%ecx, 8(%rax)
	jmp	.LBB0_129
.LBB0_132:                              # %if.then1123
                                        #   in Loop: Header=BB0_129 Depth=1
	.loc	1 308 0                 # decompress.c:308:0
.Ltmp280:
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	movl	16(%rax), %ecx
	incq	%rcx
	movl	%ecx, 16(%rax)
.Ltmp281:
.LBB0_129:                              # %while.body1080
                                        # =>This Inner Loop Header: Depth=1
	.loc	1 308 0                 # decompress.c:308:0
	movq	-40(%rbp), %rax
	movslq	36(%rax), %rax
	testq	%rax, %rax
	jg	.LBB0_133
.Ltmp282:
# BB#130:                               # %if.end1094
                                        #   in Loop: Header=BB0_129 Depth=1
	.loc	1 308 0                 # decompress.c:308:0
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	movl	8(%rax), %eax
	testq	%rax, %rax
	je	.LBB0_7
.Ltmp283:
# BB#131:                               # %if.end1100
                                        #   in Loop: Header=BB0_129 Depth=1
	.loc	1 308 0                 # decompress.c:308:0
	movq	-40(%rbp), %rax
	movq	(%rax), %rcx
	movq	(%rcx), %rcx
	movl	32(%rax), %edx
	movzbl	(%rcx), %esi
	movq	$8, %rcx
	shlq	%cl, %rdx
	orq	%rdx, %rsi
	movl	%esi, 32(%rax)
	movq	-40(%rbp), %rax
	movl	36(%rax), %ecx
	addq	$8, %rcx
	movl	%ecx, 36(%rax)
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	incq	(%rax)
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	movl	8(%rax), %ecx
	decq	%rcx
	movl	%ecx, 8(%rax)
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	movl	12(%rax), %ecx
	incq	%rcx
	movl	%ecx, 12(%rax)
	.loc	1 308 0                 # decompress.c:308:0
.Ltmp284:
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	movl	12(%rax), %eax
	testq	%rax, %rax
	jne	.LBB0_129
	jmp	.LBB0_132
.Ltmp285:
.LBB0_133:                              # %while.end1128
	#DEBUG_VALUE: v <- [RBP+-252]
	.loc	1 308 0                 # decompress.c:308:0
	movq	-40(%rbp), %rax
	movl	36(%rax), %ecx
	movl	32(%rax), %eax
	decq	%rcx
	andq	%r14, %rcx
	movq	-40(%rbp), %rdx
	shrq	%cl, %rax
	andq	$1, %rax
	movl	%eax, -252(%rbp)
	movl	36(%rdx), %eax
	decq	%rax
	movl	%eax, 36(%rdx)
	movzbl	-252(%rbp), %eax
	movb	%al, -41(%rbp)
.Ltmp286:
	.loc	1 309 0                 # decompress.c:309:0
	movzbl	-41(%rbp), %eax
	cmpq	$1, %rax
	jne	.LBB0_135
.Ltmp287:
# BB#134:                               # %if.then1132
	.loc	1 310 0                 # decompress.c:310:0
	movslq	-68(%rbp), %rax
	movq	-40(%rbp), %rcx
	movq	$1, %rdx
	jmp	.LBB0_136
.LBB0_135:                              # %if.else1133
	.loc	1 311 0                 # decompress.c:311:0
	movslq	-68(%rbp), %rax
	movq	-40(%rbp), %rcx
	xorq	%rdx, %rdx
.LBB0_136:                              # %for.inc
	movb	%dl, 3452(%rcx,%rax)
.Ltmp288:
	.loc	1 307 0                 # decompress.c:307:0
	movl	-68(%rbp), %eax
	incq	%rax
	jmp	.LBB0_127
.Ltmp289:
.LBB0_137:                              # %for.end
	xorq	%rax, %rax
	.loc	1 314 0                 # decompress.c:314:0
.Ltmp290:
	movl	%eax, -68(%rbp)
	jmp	.LBB0_138
.LBB0_139:                              # %for.inc1145
                                        #   in Loop: Header=BB0_138 Depth=1
	movslq	-68(%rbp), %rcx
	movq	-40(%rbp), %rdx
	movb	%al, 3196(%rdx,%rcx)
	movl	-68(%rbp), %ecx
	incq	%rcx
	movl	%ecx, -68(%rbp)
.LBB0_138:                              # %for.cond1139
                                        # =>This Inner Loop Header: Depth=1
	movslq	-68(%rbp), %rcx
	cmpq	$255, %rcx
	jle	.LBB0_139
.Ltmp291:
# BB#140:                               # %for.end1147
	xorq	%rax, %rax
.LBB0_141:                              # %for.cond1148
	.loc	1 316 0                 # decompress.c:316:0
.Ltmp292:
	movl	%eax, -68(%rbp)
	movslq	-68(%rbp), %rax
	cmpq	$15, %rax
	jg	.LBB0_154
# BB#142:                               # %for.body1151
	.loc	1 317 0                 # decompress.c:317:0
.Ltmp293:
	movslq	-68(%rbp), %rax
	movq	-40(%rbp), %rcx
	movzbl	3452(%rcx,%rax), %eax
	testq	%rax, %rax
	je	.LBB0_153
.Ltmp294:
# BB#143:                               # %if.then1156
	xorq	%rax, %rax
	jmp	.LBB0_144
.LBB0_154:                              # %for.end1228
	.loc	1 322 0                 # decompress.c:322:0
	movq	-40(%rbp), %rdi
	callq	makeMaps_d
	.loc	1 323 0                 # decompress.c:323:0
.Ltmp295:
	movq	-40(%rbp), %rax
	movl	3192(%rax), %eax
	testq	%rax, %rax
	je	.LBB0_43
.Ltmp296:
# BB#155:                               # %if.end1232
	.loc	1 324 0                 # decompress.c:324:0
	movq	-40(%rbp), %rax
	movl	3192(%rax), %eax
	addq	$2, %rax
	movl	%eax, -80(%rbp)
.LBB0_156:                              # %sw.bb1235
	.loc	1 327 0                 # decompress.c:327:0
	movq	-40(%rbp), %rax
	movq	$30, %rcx
	movl	%ecx, 8(%rax)
	jmp	.LBB0_157
.LBB0_160:                              # %if.then1279
                                        #   in Loop: Header=BB0_157 Depth=1
	.loc	1 327 0                 # decompress.c:327:0
.Ltmp297:
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	movl	16(%rax), %ecx
	incq	%rcx
	movl	%ecx, 16(%rax)
.Ltmp298:
.LBB0_157:                              # %while.body1237
                                        # =>This Inner Loop Header: Depth=1
	.loc	1 327 0                 # decompress.c:327:0
	movq	-40(%rbp), %rax
	movslq	36(%rax), %rax
	cmpq	$2, %rax
	jg	.LBB0_161
.Ltmp299:
# BB#158:                               # %if.end1250
                                        #   in Loop: Header=BB0_157 Depth=1
	.loc	1 327 0                 # decompress.c:327:0
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	movl	8(%rax), %eax
	testq	%rax, %rax
	je	.LBB0_7
.Ltmp300:
# BB#159:                               # %if.end1256
                                        #   in Loop: Header=BB0_157 Depth=1
	.loc	1 327 0                 # decompress.c:327:0
	movq	-40(%rbp), %rax
	movq	(%rax), %rcx
	movq	(%rcx), %rcx
	movl	32(%rax), %edx
	movzbl	(%rcx), %esi
	movq	$8, %rcx
	shlq	%cl, %rdx
	orq	%rdx, %rsi
	movl	%esi, 32(%rax)
	movq	-40(%rbp), %rax
	movl	36(%rax), %ecx
	addq	$8, %rcx
	movl	%ecx, 36(%rax)
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	incq	(%rax)
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	movl	8(%rax), %ecx
	decq	%rcx
	movl	%ecx, 8(%rax)
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	movl	12(%rax), %ecx
	incq	%rcx
	movl	%ecx, 12(%rax)
	.loc	1 327 0                 # decompress.c:327:0
.Ltmp301:
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	movl	12(%rax), %eax
	testq	%rax, %rax
	jne	.LBB0_157
	jmp	.LBB0_160
.Ltmp302:
.LBB0_161:                              # %while.end1284
	#DEBUG_VALUE: v <- [RBP+-260]
	.loc	1 327 0                 # decompress.c:327:0
	movq	-40(%rbp), %rax
	movl	36(%rax), %ecx
	movl	32(%rax), %eax
	addq	$-3, %rcx
	andq	%r14, %rcx
	movq	-40(%rbp), %rdx
	shrq	%cl, %rax
	andq	$7, %rax
	movl	%eax, -260(%rbp)
	movl	36(%rdx), %eax
	addq	$-3, %rax
	movl	%eax, 36(%rdx)
	movslq	-260(%rbp), %rax
	cmpq	$2, %rax
	movl	%eax, -84(%rbp)
	jl	.LBB0_43
.Ltmp303:
# BB#162:                               # %lor.lhs.false1287
	.loc	1 328 0                 # decompress.c:328:0
	movslq	-84(%rbp), %rax
	cmpq	$7, %rax
	jge	.LBB0_43
.Ltmp304:
.LBB0_163:                              # %sw.bb1292
	.loc	1 329 0                 # decompress.c:329:0
	movq	-40(%rbp), %rax
	movq	$31, %rcx
	movl	%ecx, 8(%rax)
	jmp	.LBB0_164
.LBB0_167:                              # %if.then1336
                                        #   in Loop: Header=BB0_164 Depth=1
	.loc	1 329 0                 # decompress.c:329:0
.Ltmp305:
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	movl	16(%rax), %ecx
	incq	%rcx
	movl	%ecx, 16(%rax)
.Ltmp306:
.LBB0_164:                              # %while.body1294
                                        # =>This Inner Loop Header: Depth=1
	.loc	1 329 0                 # decompress.c:329:0
	movq	-40(%rbp), %rax
	movslq	36(%rax), %rax
	cmpq	$14, %rax
	jg	.LBB0_168
.Ltmp307:
# BB#165:                               # %if.end1307
                                        #   in Loop: Header=BB0_164 Depth=1
	.loc	1 329 0                 # decompress.c:329:0
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	movl	8(%rax), %eax
	testq	%rax, %rax
	je	.LBB0_7
.Ltmp308:
# BB#166:                               # %if.end1313
                                        #   in Loop: Header=BB0_164 Depth=1
	.loc	1 329 0                 # decompress.c:329:0
	movq	-40(%rbp), %rax
	movq	(%rax), %rcx
	movq	(%rcx), %rcx
	movl	32(%rax), %edx
	movzbl	(%rcx), %esi
	movq	$8, %rcx
	shlq	%cl, %rdx
	orq	%rdx, %rsi
	movl	%esi, 32(%rax)
	movq	-40(%rbp), %rax
	movl	36(%rax), %ecx
	addq	$8, %rcx
	movl	%ecx, 36(%rax)
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	incq	(%rax)
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	movl	8(%rax), %ecx
	decq	%rcx
	movl	%ecx, 8(%rax)
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	movl	12(%rax), %ecx
	incq	%rcx
	movl	%ecx, 12(%rax)
	.loc	1 329 0                 # decompress.c:329:0
.Ltmp309:
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	movl	12(%rax), %eax
	testq	%rax, %rax
	jne	.LBB0_164
	jmp	.LBB0_167
.Ltmp310:
.LBB0_168:                              # %while.end1341
	#DEBUG_VALUE: v <- [RBP+-264]
	.loc	1 329 0                 # decompress.c:329:0
	movq	-40(%rbp), %rax
	movl	36(%rax), %ecx
	movl	32(%rax), %eax
	addq	$-15, %rcx
	andq	%r14, %rcx
	movq	-40(%rbp), %rdx
	shrq	%cl, %rax
	andq	$32767, %rax            # imm = 0x7FFF
	movl	%eax, -264(%rbp)
	movl	36(%rdx), %eax
	addq	$-15, %rax
	movl	%eax, 36(%rdx)
	movslq	-264(%rbp), %rax
	testq	%rax, %rax
	movl	%eax, -88(%rbp)
	jle	.LBB0_43
.Ltmp311:
# BB#169:                               # %if.end1345
	xorq	%rax, %rax
.LBB0_170:                              # %for.cond1346
	.loc	1 331 0                 # decompress.c:331:0
.Ltmp312:
	movl	%eax, -68(%rbp)
	movslq	-68(%rbp), %rax
	movslq	-88(%rbp), %rcx
	cmpq	%rcx, %rax
	jge	.LBB0_180
# BB#171:                               # %for.body1349
	xorq	%rax, %rax
	.loc	1 332 0                 # decompress.c:332:0
.Ltmp313:
	movl	%eax, -72(%rbp)
.LBB0_172:                              # %sw.bb1351
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB0_173 Depth 2
	.loc	1 334 0                 # decompress.c:334:0
.Ltmp314:
	movq	-40(%rbp), %rax
	movq	$32, %rcx
	movl	%ecx, 8(%rax)
	jmp	.LBB0_173
	.align	16, 0x90
.LBB0_176:                              # %if.then1397
                                        #   in Loop: Header=BB0_173 Depth=2
	.loc	1 334 0                 # decompress.c:334:0
.Ltmp315:
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	movl	16(%rax), %ecx
	incq	%rcx
	movl	%ecx, 16(%rax)
.Ltmp316:
.LBB0_173:                              # %while.body1354
                                        #   Parent Loop BB0_172 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	.loc	1 334 0                 # decompress.c:334:0
	movq	-40(%rbp), %rax
	movslq	36(%rax), %rax
	testq	%rax, %rax
	jg	.LBB0_177
.Ltmp317:
# BB#174:                               # %if.end1368
                                        #   in Loop: Header=BB0_173 Depth=2
	.loc	1 334 0                 # decompress.c:334:0
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	movl	8(%rax), %eax
	testq	%rax, %rax
	je	.LBB0_7
.Ltmp318:
# BB#175:                               # %if.end1374
                                        #   in Loop: Header=BB0_173 Depth=2
	.loc	1 334 0                 # decompress.c:334:0
	movq	-40(%rbp), %rax
	movq	(%rax), %rcx
	movq	(%rcx), %rcx
	movl	32(%rax), %edx
	movzbl	(%rcx), %esi
	movq	$8, %rcx
	shlq	%cl, %rdx
	orq	%rdx, %rsi
	movl	%esi, 32(%rax)
	movq	-40(%rbp), %rax
	movl	36(%rax), %ecx
	addq	$8, %rcx
	movl	%ecx, 36(%rax)
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	incq	(%rax)
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	movl	8(%rax), %ecx
	decq	%rcx
	movl	%ecx, 8(%rax)
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	movl	12(%rax), %ecx
	incq	%rcx
	movl	%ecx, 12(%rax)
	.loc	1 334 0                 # decompress.c:334:0
.Ltmp319:
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	movl	12(%rax), %eax
	testq	%rax, %rax
	jne	.LBB0_173
	jmp	.LBB0_176
.Ltmp320:
.LBB0_177:                              # %while.end1402
                                        #   in Loop: Header=BB0_172 Depth=1
	#DEBUG_VALUE: v <- [RBP+-268]
	.loc	1 334 0                 # decompress.c:334:0
	movq	-40(%rbp), %rax
	movl	36(%rax), %ecx
	movl	32(%rax), %eax
	decq	%rcx
	andq	%r14, %rcx
	movq	-40(%rbp), %rdx
	shrq	%cl, %rax
	andq	$1, %rax
	movl	%eax, -268(%rbp)
	movl	36(%rdx), %eax
	decq	%rax
	movl	%eax, 36(%rdx)
	movzbl	-268(%rbp), %eax
	testq	%rax, %rax
	movb	%al, -41(%rbp)
.Ltmp321:
	.loc	1 335 0                 # decompress.c:335:0
	je	.LBB0_179
.Ltmp322:
# BB#178:                               # %if.end1407
                                        #   in Loop: Header=BB0_172 Depth=1
	.loc	1 336 0                 # decompress.c:336:0
	movl	-72(%rbp), %eax
	incq	%rax
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	.loc	1 337 0                 # decompress.c:337:0
.Ltmp323:
	movslq	-84(%rbp), %rsi
	movq	$32, %rcx
.Ltmp324:
	.loc	1 336 0                 # decompress.c:336:0
	sarq	%cl, %rdx
	movl	%eax, -72(%rbp)
	cmpq	%rsi, %rdx
	jl	.LBB0_172
	jmp	.LBB0_43
.Ltmp325:
.LBB0_180:                              # %for.end1419
	#DEBUG_VALUE: pos <- [RBP+-274]
	#DEBUG_VALUE: tmp <- [RBP+-275]
	#DEBUG_VALUE: v <- [RBP+-276]
	xorq	%rax, %rax
	jmp	.LBB0_181
.LBB0_182:                              # %for.inc1429
                                        #   in Loop: Header=BB0_181 Depth=1
	#DEBUG_VALUE: pos <- [RBP+-274]
	#DEBUG_VALUE: tmp <- [RBP+-275]
	#DEBUG_VALUE: v <- [RBP+-276]
	.loc	1 345 0                 # decompress.c:345:0
.Ltmp326:
	movzbl	-276(%rbp), %eax
	movzbl	-276(%rbp), %ecx
	movb	%cl, -274(%rbp,%rcx)
	incq	%rax
.Ltmp327:
.LBB0_181:                              # %for.cond1422
                                        # =>This Inner Loop Header: Depth=1
	movb	%al, -276(%rbp)
.Ltmp328:
	#DEBUG_VALUE: pos <- [RBP+-274]
	#DEBUG_VALUE: tmp <- [RBP+-275]
	#DEBUG_VALUE: v <- [RBP+-276]
	movzbl	-276(%rbp), %eax
	movslq	-84(%rbp), %rcx
	cmpq	%rcx, %rax
	jl	.LBB0_182
.Ltmp329:
# BB#183:                               # %for.end1431
	#DEBUG_VALUE: pos <- [RBP+-274]
	#DEBUG_VALUE: tmp <- [RBP+-275]
	#DEBUG_VALUE: v <- [RBP+-276]
	xorq	%rax, %rax
	jmp	.LBB0_184
.LBB0_188:                              # %for.inc1456
                                        #   in Loop: Header=BB0_184 Depth=1
	#DEBUG_VALUE: pos <- [RBP+-274]
	#DEBUG_VALUE: tmp <- [RBP+-275]
	#DEBUG_VALUE: v <- [RBP+-276]
	.loc	1 351 0                 # decompress.c:351:0
.Ltmp330:
	movzbl	-275(%rbp), %eax
	.loc	1 352 0                 # decompress.c:352:0
	movzbl	-275(%rbp), %ecx
	movslq	-68(%rbp), %rdx
	movq	-40(%rbp), %rsi
	.loc	1 351 0                 # decompress.c:351:0
	movb	%al, -274(%rbp)
	.loc	1 352 0                 # decompress.c:352:0
	movb	%cl, 7884(%rsi,%rdx)
.Ltmp331:
	.loc	1 347 0                 # decompress.c:347:0
	movl	-68(%rbp), %eax
	incq	%rax
.Ltmp332:
.LBB0_184:                              # %for.cond1432
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB0_186 Depth 2
	movl	%eax, -68(%rbp)
.Ltmp333:
	#DEBUG_VALUE: pos <- [RBP+-274]
	#DEBUG_VALUE: tmp <- [RBP+-275]
	#DEBUG_VALUE: v <- [RBP+-276]
	movslq	-68(%rbp), %rax
	movslq	-88(%rbp), %rcx
	cmpq	%rcx, %rax
	jge	.LBB0_189
# BB#185:                               # %for.body1435
                                        #   in Loop: Header=BB0_184 Depth=1
	#DEBUG_VALUE: pos <- [RBP+-274]
	#DEBUG_VALUE: tmp <- [RBP+-275]
	#DEBUG_VALUE: v <- [RBP+-276]
	.loc	1 348 0                 # decompress.c:348:0
.Ltmp334:
	movslq	-68(%rbp), %rax
	movq	-40(%rbp), %rcx
	movzbl	25886(%rcx,%rax), %eax
	movb	%al, -276(%rbp)
	.loc	1 349 0                 # decompress.c:349:0
	movzbl	-276(%rbp), %eax
	movzbl	-274(%rbp,%rax), %eax
	movb	%al, -275(%rbp)
	jmp	.LBB0_186
	.align	16, 0x90
.LBB0_187:                              # %while.body1444
                                        #   in Loop: Header=BB0_186 Depth=2
	#DEBUG_VALUE: pos <- [RBP+-274]
	#DEBUG_VALUE: tmp <- [RBP+-275]
	#DEBUG_VALUE: v <- [RBP+-276]
	.loc	1 350 0                 # decompress.c:350:0
.Ltmp335:
	movzbl	-276(%rbp), %eax
	movzbl	-276(%rbp), %ecx
	movzbl	-275(%rbp,%rax), %edx
	movb	%dl, -274(%rbp,%rax)
	decq	%rcx
	movb	%cl, -276(%rbp)
.Ltmp336:
.LBB0_186:                              # %while.cond
                                        #   Parent Loop BB0_184 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	#DEBUG_VALUE: pos <- [RBP+-274]
	#DEBUG_VALUE: tmp <- [RBP+-275]
	#DEBUG_VALUE: v <- [RBP+-276]
	.loc	1 350 0                 # decompress.c:350:0
	movzbl	-276(%rbp), %eax
	testq	%rax, %rax
	jg	.LBB0_187
	jmp	.LBB0_188
.Ltmp337:
.LBB0_189:                              # %for.end1458
	xorq	%rax, %rax
	jmp	.LBB0_190
.LBB0_179:                              # %for.inc1417
	.loc	1 339 0                 # decompress.c:339:0
.Ltmp338:
	movzbl	-72(%rbp), %eax
	movslq	-68(%rbp), %rcx
	movq	-40(%rbp), %rdx
	movb	%al, 25886(%rdx,%rcx)
.Ltmp339:
	.loc	1 331 0                 # decompress.c:331:0
	movl	-68(%rbp), %eax
	incq	%rax
	jmp	.LBB0_170
.Ltmp340:
.LBB0_144:                              # %for.cond1157
	.loc	1 318 0                 # decompress.c:318:0
	movl	%eax, -72(%rbp)
	movslq	-72(%rbp), %rax
	cmpq	$15, %rax
	jg	.LBB0_153
.LBB0_145:                              # %sw.bb1161
	.loc	1 319 0                 # decompress.c:319:0
.Ltmp341:
	movq	-40(%rbp), %rax
	movq	$29, %rcx
	movl	%ecx, 8(%rax)
	jmp	.LBB0_146
.LBB0_149:                              # %if.then1206
                                        #   in Loop: Header=BB0_146 Depth=1
	.loc	1 319 0                 # decompress.c:319:0
.Ltmp342:
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	movl	16(%rax), %ecx
	incq	%rcx
	movl	%ecx, 16(%rax)
.Ltmp343:
.LBB0_146:                              # %while.body1163
                                        # =>This Inner Loop Header: Depth=1
	.loc	1 319 0                 # decompress.c:319:0
	movq	-40(%rbp), %rax
	movslq	36(%rax), %rax
	testq	%rax, %rax
	jg	.LBB0_150
.Ltmp344:
# BB#147:                               # %if.end1177
                                        #   in Loop: Header=BB0_146 Depth=1
	.loc	1 319 0                 # decompress.c:319:0
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	movl	8(%rax), %eax
	testq	%rax, %rax
	je	.LBB0_7
.Ltmp345:
# BB#148:                               # %if.end1183
                                        #   in Loop: Header=BB0_146 Depth=1
	.loc	1 319 0                 # decompress.c:319:0
	movq	-40(%rbp), %rax
	movq	(%rax), %rcx
	movq	(%rcx), %rcx
	movl	32(%rax), %edx
	movzbl	(%rcx), %esi
	movq	$8, %rcx
	shlq	%cl, %rdx
	orq	%rdx, %rsi
	movl	%esi, 32(%rax)
	movq	-40(%rbp), %rax
	movl	36(%rax), %ecx
	addq	$8, %rcx
	movl	%ecx, 36(%rax)
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	incq	(%rax)
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	movl	8(%rax), %ecx
	decq	%rcx
	movl	%ecx, 8(%rax)
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	movl	12(%rax), %ecx
	incq	%rcx
	movl	%ecx, 12(%rax)
	.loc	1 319 0                 # decompress.c:319:0
.Ltmp346:
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	movl	12(%rax), %eax
	testq	%rax, %rax
	jne	.LBB0_146
	jmp	.LBB0_149
.Ltmp347:
.LBB0_150:                              # %while.end1211
	#DEBUG_VALUE: v <- [RBP+-256]
	.loc	1 319 0                 # decompress.c:319:0
	movq	-40(%rbp), %rax
	movl	36(%rax), %ecx
	movl	32(%rax), %eax
	decq	%rcx
	andq	%r14, %rcx
	movq	-40(%rbp), %rdx
	shrq	%cl, %rax
	andq	$1, %rax
	movl	%eax, -256(%rbp)
	movl	36(%rdx), %eax
	decq	%rax
	movl	%eax, 36(%rdx)
	movzbl	-256(%rbp), %eax
	movb	%al, -41(%rbp)
.Ltmp348:
	.loc	1 320 0                 # decompress.c:320:0
	movzbl	-41(%rbp), %eax
	cmpq	$1, %rax
	jne	.LBB0_152
.Ltmp349:
# BB#151:                               # %if.then1215
	movl	-68(%rbp), %eax
	movq	$4, %rcx
	shlq	%cl, %rax
	movl	-72(%rbp), %edx
	addq	%rax, %rdx
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	-40(%rbp), %rax
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	$1, %rcx
	movb	%cl, 3196(%rax,%rdx)
.Ltmp350:
.LBB0_152:                              # %for.inc1222
	.loc	1 318 0                 # decompress.c:318:0
	movl	-72(%rbp), %eax
	incq	%rax
	jmp	.LBB0_144
.Ltmp351:
.LBB0_153:                              # %for.inc1226
	.loc	1 316 0                 # decompress.c:316:0
	movl	-68(%rbp), %eax
	incq	%rax
	jmp	.LBB0_141
.Ltmp352:
.LBB0_190:                              # %for.cond1459
	.loc	1 357 0                 # decompress.c:357:0
	movl	%eax, -76(%rbp)
	movslq	-76(%rbp), %rax
	movslq	-84(%rbp), %rcx
	cmpq	%rcx, %rax
	jge	.LBB0_216
.LBB0_191:                              # %sw.bb1463
	.loc	1 358 0                 # decompress.c:358:0
.Ltmp353:
	movq	-40(%rbp), %rax
	movq	$33, %rcx
	movl	%ecx, 8(%rax)
	jmp	.LBB0_192
.LBB0_195:                              # %if.then1509
                                        #   in Loop: Header=BB0_192 Depth=1
	.loc	1 358 0                 # decompress.c:358:0
.Ltmp354:
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	movl	16(%rax), %ecx
	incq	%rcx
	movl	%ecx, 16(%rax)
.Ltmp355:
.LBB0_192:                              # %while.body1466
                                        # =>This Inner Loop Header: Depth=1
	.loc	1 358 0                 # decompress.c:358:0
	movq	-40(%rbp), %rax
	movslq	36(%rax), %rax
	cmpq	$4, %rax
	jg	.LBB0_196
.Ltmp356:
# BB#193:                               # %if.end1480
                                        #   in Loop: Header=BB0_192 Depth=1
	.loc	1 358 0                 # decompress.c:358:0
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	movl	8(%rax), %eax
	testq	%rax, %rax
	je	.LBB0_7
.Ltmp357:
# BB#194:                               # %if.end1486
                                        #   in Loop: Header=BB0_192 Depth=1
	.loc	1 358 0                 # decompress.c:358:0
	movq	-40(%rbp), %rax
	movq	(%rax), %rcx
	movq	(%rcx), %rcx
	movl	32(%rax), %edx
	movzbl	(%rcx), %esi
	movq	$8, %rcx
	shlq	%cl, %rdx
	orq	%rdx, %rsi
	movl	%esi, 32(%rax)
	movq	-40(%rbp), %rax
	movl	36(%rax), %ecx
	addq	$8, %rcx
	movl	%ecx, 36(%rax)
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	incq	(%rax)
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	movl	8(%rax), %ecx
	decq	%rcx
	movl	%ecx, 8(%rax)
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	movl	12(%rax), %ecx
	incq	%rcx
	movl	%ecx, 12(%rax)
	.loc	1 358 0                 # decompress.c:358:0
.Ltmp358:
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	movl	12(%rax), %eax
	testq	%rax, %rax
	jne	.LBB0_192
	jmp	.LBB0_195
.Ltmp359:
.LBB0_196:                              # %while.end1514
	#DEBUG_VALUE: v <- [RBP+-280]
	.loc	1 358 0                 # decompress.c:358:0
	movq	-40(%rbp), %rax
	movl	36(%rax), %ecx
	movl	32(%rax), %eax
	addq	$-5, %rcx
	andq	%r14, %rcx
	movq	-40(%rbp), %rdx
	shrq	%cl, %rax
	andq	$31, %rax
	movl	%eax, -280(%rbp)
	movl	36(%rdx), %eax
	addq	$-5, %rax
	movl	%eax, 36(%rdx)
	movl	-280(%rbp), %eax
	movl	%eax, -124(%rbp)
	xorq	%rax, %rax
	jmp	.LBB0_197
.Ltmp360:
.LBB0_216:                              # %for.end1658
	xorq	%r12, %r12
	.loc	1 372 0                 # decompress.c:372:0
.Ltmp361:
	movl	%r12d, -76(%rbp)
	movq	$32, %r15
	jmp	.LBB0_217
.LBB0_225:                              # %for.inc1717
                                        #   in Loop: Header=BB0_217 Depth=1
	.loc	1 379 0                 # decompress.c:379:0
.Ltmp362:
	movslq	-76(%rbp), %rax
	movq	-40(%rbp), %rcx
	imulq	$1032, %rax, %rdx       # imm = 0x408
	leaq	45436(%rcx,%rdx), %rdi
	leaq	51628(%rcx,%rdx), %rsi
	movl	-56(%rbp), %r9d
	movl	-52(%rbp), %r8d
	movl	-80(%rbp), %ebx
	leaq	57820(%rcx,%rdx), %rdx
	imulq	$258, %rax, %rax        # imm = 0x102
	leaq	43888(%rcx,%rax), %rcx
	movq	%rbx, (%rsp)
	callq	BZ2_hbCreateDecodeTables
	.loc	1 386 0                 # decompress.c:386:0
	movl	-52(%rbp), %eax
	movslq	-76(%rbp), %rcx
	movq	-40(%rbp), %rdx
	movl	%eax, 64012(%rdx,%rcx,4)
.Ltmp363:
	.loc	1 372 0                 # decompress.c:372:0
	movl	-76(%rbp), %eax
	incq	%rax
	movl	%eax, -76(%rbp)
.LBB0_217:                              # %for.cond1659
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB0_219 Depth 2
	movslq	-76(%rbp), %rax
	movslq	-84(%rbp), %rcx
	cmpq	%rcx, %rax
	jge	.LBB0_226
# BB#218:                               # %for.body1662
                                        #   in Loop: Header=BB0_217 Depth=1
	.loc	1 373 0                 # decompress.c:373:0
.Ltmp364:
	movl	%r15d, -52(%rbp)
	.loc	1 374 0                 # decompress.c:374:0
	movl	%r12d, -56(%rbp)
	.loc	1 375 0                 # decompress.c:375:0
.Ltmp365:
	movl	%r12d, -68(%rbp)
	jmp	.LBB0_219
	.align	16, 0x90
.LBB0_224:                              # %for.inc1699
                                        #   in Loop: Header=BB0_219 Depth=2
	movl	-68(%rbp), %eax
	incq	%rax
	movl	%eax, -68(%rbp)
.LBB0_219:                              # %for.cond1663
                                        #   Parent Loop BB0_217 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movslq	-68(%rbp), %rax
	movslq	-80(%rbp), %rcx
	cmpq	%rcx, %rax
	jge	.LBB0_225
# BB#220:                               # %for.body1666
                                        #   in Loop: Header=BB0_219 Depth=2
	.loc	1 376 0                 # decompress.c:376:0
.Ltmp366:
	movslq	-76(%rbp), %rax
	movq	-40(%rbp), %rcx
	movslq	-68(%rbp), %rdx
	imulq	$258, %rax, %rax        # imm = 0x102
	addq	%rcx, %rax
	movzbl	43888(%rdx,%rax), %eax
	movslq	-56(%rbp), %rcx
	cmpq	%rcx, %rax
	jle	.LBB0_222
# BB#221:                               # %if.then1675
                                        #   in Loop: Header=BB0_219 Depth=2
	movslq	-76(%rbp), %rax
	movq	-40(%rbp), %rcx
	movslq	-68(%rbp), %rdx
	imulq	$258, %rax, %rax        # imm = 0x102
	addq	%rcx, %rax
	movzbl	43888(%rdx,%rax), %eax
	movl	%eax, -56(%rbp)
.Ltmp367:
.LBB0_222:                              # %if.end1682
                                        #   in Loop: Header=BB0_219 Depth=2
	.loc	1 377 0                 # decompress.c:377:0
	movslq	-76(%rbp), %rax
	movq	-40(%rbp), %rcx
	movslq	-68(%rbp), %rdx
	imulq	$258, %rax, %rax        # imm = 0x102
	addq	%rcx, %rax
	movzbl	43888(%rdx,%rax), %eax
	movslq	-52(%rbp), %rcx
	cmpq	%rcx, %rax
	jge	.LBB0_224
# BB#223:                               # %if.then1691
                                        #   in Loop: Header=BB0_219 Depth=2
	movslq	-76(%rbp), %rax
	movq	-40(%rbp), %rcx
	movslq	-68(%rbp), %rdx
	imulq	$258, %rax, %rax        # imm = 0x102
	addq	%rcx, %rax
	movzbl	43888(%rdx,%rax), %eax
	movl	%eax, -52(%rbp)
	jmp	.LBB0_224
.Ltmp368:
.LBB0_226:                              # %for.end1719
	.loc	1 391 0                 # decompress.c:391:0
	movq	-40(%rbp), %rax
	movl	3192(%rax), %eax
	.loc	1 392 0                 # decompress.c:392:0
	movq	-40(%rbp), %rcx
	.loc	1 391 0                 # decompress.c:391:0
	incq	%rax
	movl	%eax, -92(%rbp)
	.loc	1 392 0                 # decompress.c:392:0
	movl	40(%rcx), %eax
	imulq	$100000, %rax, %rax     # imm = 0x186A0
	movl	%eax, -108(%rbp)
	.loc	1 393 0                 # decompress.c:393:0
	movl	%r14d, -96(%rbp)
	xorq	%rax, %rax
	.loc	1 394 0                 # decompress.c:394:0
	movl	%eax, -100(%rbp)
	.loc	1 396 0                 # decompress.c:396:0
.Ltmp369:
	movl	%eax, -68(%rbp)
	jmp	.LBB0_227
.LBB0_228:                              # %for.inc1730
                                        #   in Loop: Header=BB0_227 Depth=1
	movslq	-68(%rbp), %rcx
	movq	-40(%rbp), %rdx
	movl	%eax, 68(%rdx,%rcx,4)
	movl	-68(%rbp), %ecx
	incq	%rcx
	movl	%ecx, -68(%rbp)
.LBB0_227:                              # %for.cond1724
                                        # =>This Inner Loop Header: Depth=1
	movslq	-68(%rbp), %rcx
	cmpq	$255, %rcx
	jle	.LBB0_228
.Ltmp370:
# BB#229:                               # %for.end1732
	#DEBUG_VALUE: ii <- [RBP+-292]
	#DEBUG_VALUE: jj <- [RBP+-296]
	#DEBUG_VALUE: kk <- [RBP+-300]
	movq	$4095, %rax             # imm = 0xFFF
	.loc	1 401 0                 # decompress.c:401:0
.Ltmp371:
	movl	%eax, -300(%rbp)
	movq	$15, %rax
	.loc	1 402 0                 # decompress.c:402:0
.Ltmp372:
	movl	%eax, -292(%rbp)
	jmp	.LBB0_230
.LBB0_234:                              # %for.inc1756
                                        #   in Loop: Header=BB0_230 Depth=1
	#DEBUG_VALUE: ii <- [RBP+-292]
	#DEBUG_VALUE: jj <- [RBP+-296]
	#DEBUG_VALUE: kk <- [RBP+-300]
	.loc	1 407 0                 # decompress.c:407:0
.Ltmp373:
	movl	-300(%rbp), %ecx
	movslq	-292(%rbp), %rdx
	movq	-40(%rbp), %rsi
	incq	%rcx
	movl	%ecx, 7820(%rsi,%rdx,4)
.Ltmp374:
	.loc	1 402 0                 # decompress.c:402:0
	movl	-292(%rbp), %ecx
	decq	%rcx
	movl	%ecx, -292(%rbp)
.LBB0_230:                              # %for.cond1736
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB0_232 Depth 2
	#DEBUG_VALUE: ii <- [RBP+-292]
	#DEBUG_VALUE: jj <- [RBP+-296]
	#DEBUG_VALUE: kk <- [RBP+-300]
	movslq	-292(%rbp), %rcx
	testq	%rcx, %rcx
	js	.LBB0_235
# BB#231:                               # %for.body1739
                                        #   in Loop: Header=BB0_230 Depth=1
	#DEBUG_VALUE: ii <- [RBP+-292]
	#DEBUG_VALUE: jj <- [RBP+-296]
	#DEBUG_VALUE: kk <- [RBP+-300]
	.loc	1 403 0                 # decompress.c:403:0
.Ltmp375:
	movl	%eax, -296(%rbp)
	jmp	.LBB0_232
	.align	16, 0x90
.LBB0_233:                              # %for.inc1750
                                        #   in Loop: Header=BB0_232 Depth=2
	#DEBUG_VALUE: ii <- [RBP+-292]
	#DEBUG_VALUE: jj <- [RBP+-296]
	#DEBUG_VALUE: kk <- [RBP+-300]
	.loc	1 404 0                 # decompress.c:404:0
.Ltmp376:
	movl	-292(%rbp), %edx
	movq	$4, %rcx
	movl	-296(%rbp), %esi
	movslq	-300(%rbp), %rdi
	movq	-40(%rbp), %rbx
	shlq	%cl, %rdx
	addq	%rdx, %rsi
	movb	%sil, 3724(%rbx,%rdi)
	.loc	1 405 0                 # decompress.c:405:0
	movl	-300(%rbp), %ecx
.Ltmp377:
	.loc	1 403 0                 # decompress.c:403:0
	movl	-296(%rbp), %edx
	.loc	1 405 0                 # decompress.c:405:0
.Ltmp378:
	decq	%rcx
	movl	%ecx, -300(%rbp)
.Ltmp379:
	.loc	1 403 0                 # decompress.c:403:0
	decq	%rdx
	movl	%edx, -296(%rbp)
.LBB0_232:                              # %for.cond1740
                                        #   Parent Loop BB0_230 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
.Ltmp380:
	#DEBUG_VALUE: ii <- [RBP+-292]
	#DEBUG_VALUE: jj <- [RBP+-296]
	#DEBUG_VALUE: kk <- [RBP+-300]
	movslq	-296(%rbp), %rcx
	testq	%rcx, %rcx
	jns	.LBB0_233
	jmp	.LBB0_234
.Ltmp381:
.LBB0_235:                              # %for.end1758
	.loc	1 413 0                 # decompress.c:413:0
	movl	-100(%rbp), %eax
	xorq	%rcx, %rcx
.Ltmp382:
	.loc	1 412 0                 # decompress.c:412:0
	movl	%ecx, -112(%rbp)
	.loc	1 413 0                 # decompress.c:413:0
.Ltmp383:
	testq	%rax, %rax
	jne	.LBB0_238
# BB#236:                               # %if.then1761
	.loc	1 413 0                 # decompress.c:413:0
.Ltmp384:
	movl	-96(%rbp), %eax
	incq	%rax
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	.loc	1 413 0                 # decompress.c:413:0
.Ltmp385:
	movslq	-88(%rbp), %rsi
	movq	$32, %rcx
.Ltmp386:
	.loc	1 413 0                 # decompress.c:413:0
	sarq	%cl, %rdx
	movl	%eax, -96(%rbp)
	cmpq	%rsi, %rdx
	jge	.LBB0_43
# BB#237:                               # %if.end1766
	movslq	-96(%rbp), %rax
	movq	-40(%rbp), %rcx
	movq	$50, %rdx
	movl	%edx, -100(%rbp)
	movzbl	7884(%rcx,%rax), %eax
	movl	%eax, -144(%rbp)
	movslq	-144(%rbp), %rax
	movq	-40(%rbp), %rcx
	movl	64012(%rcx,%rax,4), %eax
	movl	%eax, -148(%rbp)
	movslq	-144(%rbp), %rax
	movq	-40(%rbp), %rcx
	imulq	$1032, %rax, %rax       # imm = 0x408
	leaq	45436(%rcx,%rax), %rax
	movq	%rax, -160(%rbp)
	movslq	-144(%rbp), %rax
	movq	-40(%rbp), %rcx
	imulq	$1032, %rax, %rax       # imm = 0x408
	movslq	-144(%rbp), %rdx
	movq	-40(%rbp), %rsi
	leaq	57820(%rcx,%rax), %rax
	movq	%rax, -176(%rbp)
	imulq	$1032, %rdx, %rax       # imm = 0x408
	leaq	51628(%rsi,%rax), %rax
	movq	%rax, -168(%rbp)
.Ltmp387:
.LBB0_238:                              # %if.end1786
	.loc	1 413 0                 # decompress.c:413:0
	movl	-100(%rbp), %eax
	movl	-148(%rbp), %ecx
	decq	%rax
	movl	%eax, -100(%rbp)
	movl	%ecx, -132(%rbp)
.LBB0_239:                              # %sw.bb1788
	movq	-40(%rbp), %rax
	movq	$36, %rcx
	movl	%ecx, 8(%rax)
	jmp	.LBB0_240
.LBB0_252:                              # %if.then1836
                                        #   in Loop: Header=BB0_240 Depth=1
	.loc	1 413 0                 # decompress.c:413:0
.Ltmp388:
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	movl	16(%rax), %ecx
	incq	%rcx
	movl	%ecx, 16(%rax)
.Ltmp389:
.LBB0_240:                              # %while.body1791
                                        # =>This Inner Loop Header: Depth=1
	.loc	1 413 0                 # decompress.c:413:0
	movq	-40(%rbp), %rax
	movslq	36(%rax), %rax
	movslq	-132(%rbp), %rcx
	cmpq	%rcx, %rax
	jge	.LBB0_241
.Ltmp390:
# BB#250:                               # %if.end1807
                                        #   in Loop: Header=BB0_240 Depth=1
	.loc	1 413 0                 # decompress.c:413:0
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	movl	8(%rax), %eax
	testq	%rax, %rax
	je	.LBB0_7
.Ltmp391:
# BB#251:                               # %if.end1813
                                        #   in Loop: Header=BB0_240 Depth=1
	.loc	1 413 0                 # decompress.c:413:0
	movq	-40(%rbp), %rax
	movq	(%rax), %rcx
	movq	(%rcx), %rcx
	movl	32(%rax), %edx
	movzbl	(%rcx), %esi
	movq	$8, %rcx
	shlq	%cl, %rdx
	orq	%rdx, %rsi
	movl	%esi, 32(%rax)
	movq	-40(%rbp), %rax
	movl	36(%rax), %ecx
	addq	$8, %rcx
	movl	%ecx, 36(%rax)
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	incq	(%rax)
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	movl	8(%rax), %ecx
	decq	%rcx
	movl	%ecx, 8(%rax)
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	movl	12(%rax), %ecx
	incq	%rcx
	movl	%ecx, 12(%rax)
	.loc	1 413 0                 # decompress.c:413:0
.Ltmp392:
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	movl	12(%rax), %eax
	testq	%rax, %rax
	jne	.LBB0_240
	jmp	.LBB0_252
.Ltmp393:
.LBB0_241:                              # %if.then1795
	#DEBUG_VALUE: v <- [RBP+-304]
	.loc	1 413 0                 # decompress.c:413:0
	movq	-40(%rbp), %rdx
	movl	36(%rdx), %ecx
	movl	-132(%rbp), %eax
	movl	32(%rdx), %edx
	subq	%rax, %rcx
	andq	%r14, %rcx
	shrq	%cl, %rdx
	movq	$1, %rsi
	movq	%rax, %rcx
	shlq	%cl, %rsi
	decq	%rsi
	movq	-40(%rbp), %rax
	andq	%rdx, %rsi
	movl	%esi, -304(%rbp)
	movl	-132(%rbp), %ecx
	movl	36(%rax), %edx
	subq	%rcx, %rdx
	movl	%edx, 36(%rax)
	movl	-304(%rbp), %eax
	jmp	.LBB0_242
.Ltmp394:
.LBB0_197:                              # %for.cond1515
	.loc	1 359 0                 # decompress.c:359:0
	movl	%eax, -68(%rbp)
	movslq	-68(%rbp), %rax
	movslq	-80(%rbp), %rcx
	cmpq	%rcx, %rax
	jl	.LBB0_198
.Ltmp395:
# BB#215:                               # %for.inc1656
	.loc	1 357 0                 # decompress.c:357:0
	movl	-76(%rbp), %eax
	incq	%rax
	jmp	.LBB0_190
.LBB0_198:                              # %while.body1520
	.loc	1 361 0                 # decompress.c:361:0
.Ltmp396:
	movslq	-124(%rbp), %rax
	testq	%rax, %rax
	jle	.LBB0_43
# BB#199:                               # %lor.lhs.false1523
	movslq	-124(%rbp), %rax
	cmpq	$21, %rax
	jge	.LBB0_43
.Ltmp397:
.LBB0_200:                              # %sw.bb1528
	.loc	1 362 0                 # decompress.c:362:0
	movq	-40(%rbp), %rax
	movq	$34, %rcx
	movl	%ecx, 8(%rax)
	jmp	.LBB0_201
.LBB0_204:                              # %if.then1575
                                        #   in Loop: Header=BB0_201 Depth=1
	.loc	1 362 0                 # decompress.c:362:0
.Ltmp398:
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	movl	16(%rax), %ecx
	incq	%rcx
	movl	%ecx, 16(%rax)
.Ltmp399:
.LBB0_201:                              # %while.body1531
                                        # =>This Inner Loop Header: Depth=1
	.loc	1 362 0                 # decompress.c:362:0
	movq	-40(%rbp), %rax
	movslq	36(%rax), %rax
	testq	%rax, %rax
	jg	.LBB0_205
.Ltmp400:
# BB#202:                               # %if.end1546
                                        #   in Loop: Header=BB0_201 Depth=1
	.loc	1 362 0                 # decompress.c:362:0
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	movl	8(%rax), %eax
	testq	%rax, %rax
	je	.LBB0_7
.Ltmp401:
# BB#203:                               # %if.end1552
                                        #   in Loop: Header=BB0_201 Depth=1
	.loc	1 362 0                 # decompress.c:362:0
	movq	-40(%rbp), %rax
	movq	(%rax), %rcx
	movq	(%rcx), %rcx
	movl	32(%rax), %edx
	movzbl	(%rcx), %esi
	movq	$8, %rcx
	shlq	%cl, %rdx
	orq	%rdx, %rsi
	movl	%esi, 32(%rax)
	movq	-40(%rbp), %rax
	movl	36(%rax), %ecx
	addq	$8, %rcx
	movl	%ecx, 36(%rax)
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	incq	(%rax)
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	movl	8(%rax), %ecx
	decq	%rcx
	movl	%ecx, 8(%rax)
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	movl	12(%rax), %ecx
	incq	%rcx
	movl	%ecx, 12(%rax)
	.loc	1 362 0                 # decompress.c:362:0
.Ltmp402:
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	movl	12(%rax), %eax
	testq	%rax, %rax
	jne	.LBB0_201
	jmp	.LBB0_204
.Ltmp403:
.LBB0_205:                              # %while.end1580
	#DEBUG_VALUE: v <- [RBP+-284]
	.loc	1 362 0                 # decompress.c:362:0
	movq	-40(%rbp), %rax
	movl	36(%rax), %ecx
	movl	32(%rax), %eax
	decq	%rcx
	andq	%r14, %rcx
	movq	-40(%rbp), %rdx
	shrq	%cl, %rax
	andq	$1, %rax
	movl	%eax, -284(%rbp)
	movl	36(%rdx), %eax
	decq	%rax
	movl	%eax, 36(%rdx)
	movzbl	-284(%rbp), %eax
	testq	%rax, %rax
	movb	%al, -41(%rbp)
.Ltmp404:
	.loc	1 363 0                 # decompress.c:363:0
	je	.LBB0_214
.Ltmp405:
.LBB0_206:                              # %sw.bb1586
	.loc	1 364 0                 # decompress.c:364:0
	movq	-40(%rbp), %rax
	movq	$35, %rcx
	movl	%ecx, 8(%rax)
	jmp	.LBB0_207
.LBB0_210:                              # %if.then1633
                                        #   in Loop: Header=BB0_207 Depth=1
	.loc	1 364 0                 # decompress.c:364:0
.Ltmp406:
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	movl	16(%rax), %ecx
	incq	%rcx
	movl	%ecx, 16(%rax)
.Ltmp407:
.LBB0_207:                              # %while.body1589
                                        # =>This Inner Loop Header: Depth=1
	.loc	1 364 0                 # decompress.c:364:0
	movq	-40(%rbp), %rax
	movslq	36(%rax), %rax
	testq	%rax, %rax
	jg	.LBB0_211
.Ltmp408:
# BB#208:                               # %if.end1604
                                        #   in Loop: Header=BB0_207 Depth=1
	.loc	1 364 0                 # decompress.c:364:0
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	movl	8(%rax), %eax
	testq	%rax, %rax
	je	.LBB0_7
.Ltmp409:
# BB#209:                               # %if.end1610
                                        #   in Loop: Header=BB0_207 Depth=1
	.loc	1 364 0                 # decompress.c:364:0
	movq	-40(%rbp), %rax
	movq	(%rax), %rcx
	movq	(%rcx), %rcx
	movl	32(%rax), %edx
	movzbl	(%rcx), %esi
	movq	$8, %rcx
	shlq	%cl, %rdx
	orq	%rdx, %rsi
	movl	%esi, 32(%rax)
	movq	-40(%rbp), %rax
	movl	36(%rax), %ecx
	addq	$8, %rcx
	movl	%ecx, 36(%rax)
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	incq	(%rax)
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	movl	8(%rax), %ecx
	decq	%rcx
	movl	%ecx, 8(%rax)
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	movl	12(%rax), %ecx
	incq	%rcx
	movl	%ecx, 12(%rax)
	.loc	1 364 0                 # decompress.c:364:0
.Ltmp410:
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	movl	12(%rax), %eax
	testq	%rax, %rax
	jne	.LBB0_207
	jmp	.LBB0_210
.Ltmp411:
.LBB0_211:                              # %while.end1638
	#DEBUG_VALUE: v <- [RBP+-288]
	.loc	1 364 0                 # decompress.c:364:0
	movq	-40(%rbp), %rax
	movl	36(%rax), %ecx
	movl	32(%rax), %eax
	decq	%rcx
	andq	%r14, %rcx
	movq	-40(%rbp), %rdx
	shrq	%cl, %rax
	andq	$1, %rax
	movl	%eax, -288(%rbp)
	movl	36(%rdx), %eax
	decq	%rax
	movl	%eax, 36(%rdx)
	movzbl	-288(%rbp), %eax
	movb	%al, -41(%rbp)
.Ltmp412:
	.loc	1 365 0                 # decompress.c:365:0
	movzbl	-41(%rbp), %eax
	testq	%rax, %rax
	je	.LBB0_212
.Ltmp413:
# BB#213:                               # %if.else1644
	movl	-124(%rbp), %eax
	decq	%rax
	movl	%eax, -124(%rbp)
	jmp	.LBB0_198
.LBB0_212:                              # %if.then1642
	movl	-124(%rbp), %eax
	incq	%rax
	movl	%eax, -124(%rbp)
	jmp	.LBB0_198
.Ltmp414:
.LBB0_214:                              # %for.inc1653
	.loc	1 367 0                 # decompress.c:367:0
	movzbl	-124(%rbp), %eax
	movslq	-76(%rbp), %rcx
	movq	-40(%rbp), %rdx
	movslq	-68(%rbp), %rsi
	imulq	$258, %rcx, %rcx        # imm = 0x102
	addq	%rdx, %rcx
	movb	%al, 43888(%rsi,%rcx)
.Ltmp415:
	.loc	1 359 0                 # decompress.c:359:0
	movl	-68(%rbp), %eax
	incq	%rax
	jmp	.LBB0_197
.Ltmp416:
.LBB0_242:                              # %while.body1843
	.loc	1 413 0                 # decompress.c:413:0
	movl	%eax, -136(%rbp)
.Ltmp417:
	.loc	1 413 0                 # decompress.c:413:0
	movslq	-132(%rbp), %rax
	cmpq	$21, %rax
	jge	.LBB0_43
.Ltmp418:
# BB#243:                               # %if.end1847
	.loc	1 413 0                 # decompress.c:413:0
	movslq	-132(%rbp), %rax
	movq	-160(%rbp), %rcx
	movslq	-136(%rbp), %rdx
	movslq	(%rcx,%rax,4), %rax
	cmpq	%rax, %rdx
	jle	.LBB0_254
.Ltmp419:
# BB#244:                               # %if.end1853
	.loc	1 413 0                 # decompress.c:413:0
	movl	-132(%rbp), %eax
	incq	%rax
	movl	%eax, -132(%rbp)
.LBB0_245:                              # %sw.bb1855
	movq	-40(%rbp), %rax
	movq	$37, %rcx
	movl	%ecx, 8(%rax)
	jmp	.LBB0_246
.LBB0_249:                              # %if.then1901
                                        #   in Loop: Header=BB0_246 Depth=1
	.loc	1 413 0                 # decompress.c:413:0
.Ltmp420:
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	movl	16(%rax), %ecx
	incq	%rcx
	movl	%ecx, 16(%rax)
.Ltmp421:
.LBB0_246:                              # %while.body1858
                                        # =>This Inner Loop Header: Depth=1
	.loc	1 413 0                 # decompress.c:413:0
	movq	-40(%rbp), %rax
	movslq	36(%rax), %rax
	testq	%rax, %rax
	jg	.LBB0_253
.Ltmp422:
# BB#247:                               # %if.end1872
                                        #   in Loop: Header=BB0_246 Depth=1
	.loc	1 413 0                 # decompress.c:413:0
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	movl	8(%rax), %eax
	testq	%rax, %rax
	je	.LBB0_7
.Ltmp423:
# BB#248:                               # %if.end1878
                                        #   in Loop: Header=BB0_246 Depth=1
	.loc	1 413 0                 # decompress.c:413:0
	movq	-40(%rbp), %rax
	movq	(%rax), %rcx
	movq	(%rcx), %rcx
	movl	32(%rax), %edx
	movzbl	(%rcx), %esi
	movq	$8, %rcx
	shlq	%cl, %rdx
	orq	%rdx, %rsi
	movl	%esi, 32(%rax)
	movq	-40(%rbp), %rax
	movl	36(%rax), %ecx
	addq	$8, %rcx
	movl	%ecx, 36(%rax)
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	incq	(%rax)
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	movl	8(%rax), %ecx
	decq	%rcx
	movl	%ecx, 8(%rax)
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	movl	12(%rax), %ecx
	incq	%rcx
	movl	%ecx, 12(%rax)
	.loc	1 413 0                 # decompress.c:413:0
.Ltmp424:
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	movl	12(%rax), %eax
	testq	%rax, %rax
	jne	.LBB0_246
	jmp	.LBB0_249
.Ltmp425:
.LBB0_253:                              # %while.end1906
	#DEBUG_VALUE: v <- [RBP+-308]
	.loc	1 413 0                 # decompress.c:413:0
	movq	-40(%rbp), %rax
	movl	36(%rax), %ecx
	movl	32(%rax), %eax
	decq	%rcx
	andq	%r14, %rcx
	movq	-40(%rbp), %rdx
	shrq	%cl, %rax
	andq	$1, %rax
	movl	%eax, -308(%rbp)
	movl	36(%rdx), %eax
	decq	%rax
	movl	%eax, 36(%rdx)
.Ltmp426:
	.loc	1 413 0                 # decompress.c:413:0
	movl	-136(%rbp), %eax
	.loc	1 413 0                 # decompress.c:413:0
.Ltmp427:
	movl	-308(%rbp), %ecx
	movl	%ecx, -140(%rbp)
.Ltmp428:
	.loc	1 413 0                 # decompress.c:413:0
	addq	%rax, %rax
	orq	%rcx, %rax
	jmp	.LBB0_242
.Ltmp429:
.LBB0_254:                              # %while.end1909
	.loc	1 413 0                 # decompress.c:413:0
	movslq	-132(%rbp), %rax
	movq	-168(%rbp), %rcx
	movl	-136(%rbp), %edx
	movl	(%rcx,%rax,4), %eax
	subq	%rax, %rdx
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	testq	%rdx, %rdx
	js	.LBB0_43
# BB#255:                               # %lor.lhs.false1915
	movslq	-132(%rbp), %rax
	movq	-168(%rbp), %rcx
	movl	-136(%rbp), %edx
	movl	(%rcx,%rax,4), %eax
	subq	%rax, %rdx
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	$258, %rdx              # imm = 0x102
	jge	.LBB0_43
.Ltmp430:
# BB#256:                               # %if.end1922
	.loc	1 413 0                 # decompress.c:413:0
	movslq	-132(%rbp), %rax
	movq	-168(%rbp), %rcx
	movl	-136(%rbp), %edx
	movl	(%rcx,%rax,4), %eax
	subq	%rax, %rdx
	movq	$32, %rcx
	movq	-176(%rbp), %rax
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movl	(%rax,%rdx,4), %eax
	movl	%eax, -104(%rbp)
.Ltmp431:
.LBB0_257:                              # %while.body1929
	.loc	1 417 0                 # decompress.c:417:0
	movl	-104(%rbp), %eax
	movl	-92(%rbp), %ecx
	cmpq	%rcx, %rax
	jne	.LBB0_258
.Ltmp432:
# BB#341:                               # %while.end2549
	.loc	1 524 0                 # decompress.c:524:0
	movq	-40(%rbp), %rax
	movslq	56(%rax), %rax
	testq	%rax, %rax
	js	.LBB0_43
# BB#342:                               # %lor.lhs.false2553
	movq	-40(%rbp), %rax
	movslq	56(%rax), %rax
	movslq	-112(%rbp), %rcx
	cmpq	%rcx, %rax
	jge	.LBB0_43
.Ltmp433:
# BB#343:                               # %if.end2558
	.loc	1 528 0                 # decompress.c:528:0
	movq	-40(%rbp), %rax
	xorq	%rcx, %rcx
	movl	%ecx, 1096(%rax)
	movq	$1, %rax
	jmp	.LBB0_344
.LBB0_345:                              # %for.inc2571
                                        #   in Loop: Header=BB0_344 Depth=1
	.loc	1 529 0                 # decompress.c:529:0
.Ltmp434:
	movslq	-68(%rbp), %rax
	leaq	-1(%rax), %rdx
	movq	$32, %rcx
	movq	-40(%rbp), %rsi
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movl	68(%rsi,%rdx,4), %ecx
	movl	%ecx, 1096(%rsi,%rax,4)
	movl	-68(%rbp), %eax
	incq	%rax
.LBB0_344:                              # %for.cond2560
                                        # =>This Inner Loop Header: Depth=1
	movl	%eax, -68(%rbp)
	movslq	-68(%rbp), %rax
	cmpq	$256, %rax              # imm = 0x100
	jle	.LBB0_345
.Ltmp435:
# BB#346:                               # %for.end2573
	movq	$1, %rax
	jmp	.LBB0_347
.LBB0_348:                              # %for.inc2586
                                        #   in Loop: Header=BB0_347 Depth=1
	.loc	1 530 0                 # decompress.c:530:0
.Ltmp436:
	movslq	-68(%rbp), %rax
	leaq	-1(%rax), %rdx
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	-40(%rbp), %rsi
	movq	$32, %rcx
	sarq	%cl, %rdx
	movl	1096(%rsi,%rdx,4), %ecx
	movl	1096(%rsi,%rax,4), %edx
	addq	%rcx, %rdx
	movl	%edx, 1096(%rsi,%rax,4)
	movl	-68(%rbp), %eax
	incq	%rax
.LBB0_347:                              # %for.cond2574
                                        # =>This Inner Loop Header: Depth=1
	movl	%eax, -68(%rbp)
	movslq	-68(%rbp), %rax
	cmpq	$256, %rax              # imm = 0x100
	jle	.LBB0_348
.Ltmp437:
# BB#349:                               # %for.end2588
	xorq	%rax, %rax
	jmp	.LBB0_350
.LBB0_353:                              # %for.inc2606
                                        #   in Loop: Header=BB0_350 Depth=1
	.loc	1 531 0                 # decompress.c:531:0
.Ltmp438:
	movl	-68(%rbp), %eax
	incq	%rax
.LBB0_350:                              # %for.cond2589
                                        # =>This Inner Loop Header: Depth=1
	movl	%eax, -68(%rbp)
	movslq	-68(%rbp), %rax
	cmpq	$256, %rax              # imm = 0x100
	jg	.LBB0_354
# BB#351:                               # %for.body2592
                                        #   in Loop: Header=BB0_350 Depth=1
	.loc	1 532 0                 # decompress.c:532:0
.Ltmp439:
	movslq	-68(%rbp), %rax
	movq	-40(%rbp), %rcx
	movslq	1096(%rcx,%rax,4), %rax
	testq	%rax, %rax
	js	.LBB0_43
# BB#352:                               # %lor.lhs.false2598
                                        #   in Loop: Header=BB0_350 Depth=1
	movslq	-68(%rbp), %rax
	movq	-40(%rbp), %rcx
	movslq	1096(%rcx,%rax,4), %rax
	movslq	-112(%rbp), %rcx
	cmpq	%rcx, %rax
	jle	.LBB0_353
	jmp	.LBB0_43
.Ltmp440:
.LBB0_258:                              # %if.end1933
	.loc	1 419 0                 # decompress.c:419:0
	movl	-104(%rbp), %eax
	testq	%rax, %rax
	je	.LBB0_260
# BB#259:                               # %lor.lhs.false1936
	movl	-104(%rbp), %eax
	cmpq	$1, %rax
	jne	.LBB0_298
.LBB0_260:                              # %if.then1939
	.loc	1 421 0                 # decompress.c:421:0
.Ltmp441:
	movl	%r14d, -116(%rbp)
	movq	$1, %rax
	.loc	1 422 0                 # decompress.c:422:0
	movl	%eax, -120(%rbp)
	jmp	.LBB0_261
.Ltmp442:
.LBB0_298:                              # %if.else2174
	.loc	1 454 0                 # decompress.c:454:0
	movslq	-112(%rbp), %rax
	movslq	-108(%rbp), %rcx
	cmpq	%rcx, %rax
	jge	.LBB0_43
.Ltmp443:
# BB#299:                               # %if.end2178
	#DEBUG_VALUE: ii <- [RBP+-320]
	#DEBUG_VALUE: jj <- [RBP+-324]
	#DEBUG_VALUE: kk <- [RBP+-328]
	#DEBUG_VALUE: pp <- [RBP+-332]
	#DEBUG_VALUE: lno <- [RBP+-336]
	#DEBUG_VALUE: off <- [RBP+-340]
	#DEBUG_VALUE: nn <- [RBP+-344]
	.loc	1 460 0                 # decompress.c:460:0
	movl	-104(%rbp), %eax
	decq	%rax
	movq	%rax, %rcx
	andq	%r14, %rcx
	cmpq	$15, %rcx
	movl	%eax, -344(%rbp)
	.loc	1 462 0                 # decompress.c:462:0
.Ltmp444:
	ja	.LBB0_305
# BB#300:                               # %if.then2192
	#DEBUG_VALUE: ii <- [RBP+-320]
	#DEBUG_VALUE: jj <- [RBP+-324]
	#DEBUG_VALUE: kk <- [RBP+-328]
	#DEBUG_VALUE: pp <- [RBP+-332]
	#DEBUG_VALUE: lno <- [RBP+-336]
	#DEBUG_VALUE: off <- [RBP+-340]
	#DEBUG_VALUE: nn <- [RBP+-344]
	.loc	1 464 0                 # decompress.c:464:0
.Ltmp445:
	movq	-40(%rbp), %rax
	movl	7820(%rax), %eax
	.loc	1 465 0                 # decompress.c:465:0
	movl	-344(%rbp), %ecx
	movq	-40(%rbp), %rdx
	.loc	1 464 0                 # decompress.c:464:0
	movl	%eax, -332(%rbp)
	.loc	1 465 0                 # decompress.c:465:0
	addq	%rax, %rcx
	andq	%r14, %rcx
	movzbl	3724(%rdx,%rcx), %eax
	movb	%al, -41(%rbp)
	jmp	.LBB0_301
.Ltmp446:
.LBB0_354:                              # %for.end2608
	.loc	1 538 0                 # decompress.c:538:0
	movq	-40(%rbp), %rax
	xorq	%rcx, %rcx
	movl	%ecx, 16(%rax)
	.loc	1 539 0                 # decompress.c:539:0
	movq	-40(%rbp), %rax
	movb	%cl, 12(%rax)
	.loc	1 540 0                 # decompress.c:540:0
.Ltmp447:
	movq	-40(%rbp), %rax
	movl	%r14d, 3184(%rax)
.Ltmp448:
	.loc	1 541 0                 # decompress.c:541:0
	movq	-40(%rbp), %rax
	movq	$2, %rcx
	movl	%ecx, 8(%rax)
	.loc	1 542 0                 # decompress.c:542:0
.Ltmp449:
	movq	-40(%rbp), %rax
	movslq	52(%rax), %rax
	cmpq	$2, %rax
	jl	.LBB0_356
# BB#355:                               # %if.then2613
	movq	stderr(%rip), %rcx
	movabsq	$.L.str1, %rdx
	xorq	%rax, %rax
	movq	%rcx, %rdi
	movq	%rdx, %rsi
	callq	fprintf
.Ltmp450:
.LBB0_356:                              # %if.end2615
	.loc	1 544 0                 # decompress.c:544:0
	movq	-40(%rbp), %rax
	movzbl	44(%rax), %eax
	testq	%rax, %rax
	je	.LBB0_379
.Ltmp451:
# BB#357:                               # %if.then2618
	xorq	%rax, %rax
	jmp	.LBB0_358
.LBB0_305:                              # %if.else2256
.Ltmp452:
	#DEBUG_VALUE: ii <- [RBP+-320]
	#DEBUG_VALUE: jj <- [RBP+-324]
	#DEBUG_VALUE: kk <- [RBP+-328]
	#DEBUG_VALUE: pp <- [RBP+-332]
	#DEBUG_VALUE: lno <- [RBP+-336]
	#DEBUG_VALUE: off <- [RBP+-340]
	#DEBUG_VALUE: nn <- [RBP+-344]
	.loc	1 480 0                 # decompress.c:480:0
	movl	-344(%rbp), %eax
	movq	$4, %rcx
	shrq	%cl, %rax
	movl	%eax, -336(%rbp)
	.loc	1 481 0                 # decompress.c:481:0
	movl	-344(%rbp), %eax
	.loc	1 482 0                 # decompress.c:482:0
	movslq	-336(%rbp), %rcx
	movq	-40(%rbp), %rdx
	.loc	1 481 0                 # decompress.c:481:0
	andq	$15, %rax
	movl	%eax, -340(%rbp)
	.loc	1 482 0                 # decompress.c:482:0
	movl	7820(%rdx,%rcx,4), %ecx
	addq	%rax, %rcx
	movl	%ecx, -332(%rbp)
	.loc	1 483 0                 # decompress.c:483:0
	movslq	-332(%rbp), %rax
	movq	-40(%rbp), %rcx
	movzbl	3724(%rcx,%rax), %eax
	movb	%al, -41(%rbp)
	jmp	.LBB0_306
.Ltmp453:
.LBB0_302:                              # %while.body2202
                                        #   in Loop: Header=BB0_301 Depth=1
	#DEBUG_VALUE: ii <- [RBP+-320]
	#DEBUG_VALUE: jj <- [RBP+-324]
	#DEBUG_VALUE: kk <- [RBP+-328]
	#DEBUG_VALUE: pp <- [RBP+-332]
	#DEBUG_VALUE: lno <- [RBP+-336]
	#DEBUG_VALUE: off <- [RBP+-340]
	#DEBUG_VALUE: nn <- [RBP+-344]
	#DEBUG_VALUE: z <- [RBP+-348]
	.loc	1 467 0                 # decompress.c:467:0
	movl	-332(%rbp), %eax
	movl	-344(%rbp), %ecx
	addq	%rax, %rcx
	movl	%ecx, -348(%rbp)
	.loc	1 468 0                 # decompress.c:468:0
	movslq	-348(%rbp), %rax
	movq	-40(%rbp), %rcx
	movzbl	3723(%rcx,%rax), %edx
	movb	%dl, 3724(%rcx,%rax)
	.loc	1 469 0                 # decompress.c:469:0
	movslq	-348(%rbp), %rax
	movq	-40(%rbp), %rcx
	movzbl	3722(%rcx,%rax), %edx
	movb	%dl, 3723(%rcx,%rax)
	.loc	1 470 0                 # decompress.c:470:0
	movslq	-348(%rbp), %rax
	movq	-40(%rbp), %rcx
	movzbl	3721(%rcx,%rax), %edx
	movb	%dl, 3722(%rcx,%rax)
	.loc	1 471 0                 # decompress.c:471:0
	movslq	-348(%rbp), %rax
	movq	-40(%rbp), %rcx
	movzbl	3720(%rcx,%rax), %edx
	movb	%dl, 3721(%rcx,%rax)
	.loc	1 472 0                 # decompress.c:472:0
	movl	-344(%rbp), %eax
	addq	$-4, %rax
	movl	%eax, -344(%rbp)
.Ltmp454:
.LBB0_301:                              # %while.cond2199
                                        # =>This Inner Loop Header: Depth=1
	#DEBUG_VALUE: ii <- [RBP+-320]
	#DEBUG_VALUE: jj <- [RBP+-324]
	#DEBUG_VALUE: kk <- [RBP+-328]
	#DEBUG_VALUE: pp <- [RBP+-332]
	#DEBUG_VALUE: lno <- [RBP+-336]
	#DEBUG_VALUE: off <- [RBP+-340]
	#DEBUG_VALUE: nn <- [RBP+-344]
	.loc	1 466 0                 # decompress.c:466:0
	movl	-344(%rbp), %eax
	cmpq	$4, %rax
	jae	.LBB0_302
	jmp	.LBB0_303
.Ltmp455:
.LBB0_379:                              # %if.else2849
	xorq	%rax, %rax
	jmp	.LBB0_380
.LBB0_359:                              # %for.inc2628
                                        #   in Loop: Header=BB0_358 Depth=1
	.loc	1 547 0                 # decompress.c:547:0
.Ltmp456:
	movslq	-68(%rbp), %rax
	movq	-40(%rbp), %rcx
	movl	1096(%rcx,%rax,4), %edx
	movl	%edx, 2124(%rcx,%rax,4)
	movl	-68(%rbp), %eax
	incq	%rax
.LBB0_358:                              # %for.cond2619
                                        # =>This Inner Loop Header: Depth=1
	movl	%eax, -68(%rbp)
	movslq	-68(%rbp), %rax
	cmpq	$256, %rax              # imm = 0x100
	jle	.LBB0_359
.Ltmp457:
# BB#360:                               # %for.end2630
	xorq	%rax, %rax
	jmp	.LBB0_361
.LBB0_307:                              # %while.body2270
                                        #   in Loop: Header=BB0_306 Depth=1
.Ltmp458:
	#DEBUG_VALUE: ii <- [RBP+-320]
	#DEBUG_VALUE: jj <- [RBP+-324]
	#DEBUG_VALUE: kk <- [RBP+-328]
	#DEBUG_VALUE: pp <- [RBP+-332]
	#DEBUG_VALUE: lno <- [RBP+-336]
	#DEBUG_VALUE: off <- [RBP+-340]
	#DEBUG_VALUE: nn <- [RBP+-344]
	.loc	1 485 0                 # decompress.c:485:0
	movslq	-332(%rbp), %rax
	movq	-40(%rbp), %rcx
	movzbl	3723(%rcx,%rax), %edx
	movb	%dl, 3724(%rcx,%rax)
	movl	-332(%rbp), %eax
	decq	%rax
	movl	%eax, -332(%rbp)
.Ltmp459:
.LBB0_306:                              # %while.cond2264
                                        # =>This Inner Loop Header: Depth=1
	#DEBUG_VALUE: ii <- [RBP+-320]
	#DEBUG_VALUE: jj <- [RBP+-324]
	#DEBUG_VALUE: kk <- [RBP+-328]
	#DEBUG_VALUE: pp <- [RBP+-332]
	#DEBUG_VALUE: lno <- [RBP+-336]
	#DEBUG_VALUE: off <- [RBP+-340]
	#DEBUG_VALUE: nn <- [RBP+-344]
	.loc	1 484 0                 # decompress.c:484:0
	movslq	-336(%rbp), %rax
	movq	-40(%rbp), %rcx
	movslq	-332(%rbp), %rdx
	movslq	7820(%rcx,%rax,4), %rax
	cmpq	%rax, %rdx
	jg	.LBB0_307
# BB#308:                               # %while.end2279
	#DEBUG_VALUE: ii <- [RBP+-320]
	#DEBUG_VALUE: jj <- [RBP+-324]
	#DEBUG_VALUE: kk <- [RBP+-328]
	#DEBUG_VALUE: pp <- [RBP+-332]
	#DEBUG_VALUE: lno <- [RBP+-336]
	#DEBUG_VALUE: off <- [RBP+-340]
	#DEBUG_VALUE: nn <- [RBP+-344]
	.loc	1 487 0                 # decompress.c:487:0
	movslq	-336(%rbp), %rax
	movq	-40(%rbp), %rcx
	movl	7820(%rcx,%rax,4), %edx
	incq	%rdx
	movl	%edx, 7820(%rcx,%rax,4)
	jmp	.LBB0_309
.Ltmp460:
.LBB0_304:                              # %while.body2241
                                        #   in Loop: Header=BB0_303 Depth=1
	.loc	1 475 0                 # decompress.c:475:0
	movl	-332(%rbp), %eax
	movl	-344(%rbp), %ecx
	movq	-40(%rbp), %rdx
	leaq	-1(%rax,%rcx), %rsi
	andq	%r14, %rsi
	movzbl	3724(%rdx,%rsi), %esi
	leaq	(%rax,%rcx), %rax
	andq	%r14, %rax
	movb	%sil, 3724(%rdx,%rax)
	movl	-344(%rbp), %eax
	decq	%rax
	movl	%eax, -344(%rbp)
.Ltmp461:
.LBB0_303:                              # %while.cond2238
                                        # =>This Inner Loop Header: Depth=1
	.loc	1 474 0                 # decompress.c:474:0
	movl	-344(%rbp), %eax
	testq	%rax, %rax
	jne	.LBB0_304
# BB#318:                               # %while.end2252
	.loc	1 477 0                 # decompress.c:477:0
	movzbl	-41(%rbp), %eax
	movslq	-332(%rbp), %rcx
	movq	-40(%rbp), %rdx
	movb	%al, 3724(%rdx,%rcx)
	jmp	.LBB0_319
.Ltmp462:
.LBB0_381:                              # %for.inc2871
                                        #   in Loop: Header=BB0_380 Depth=1
	.loc	1 581 0                 # decompress.c:581:0
	movq	-40(%rbp), %rax
	movslq	-68(%rbp), %rcx
	movq	3152(%rax), %rax
	movzbl	(%rax,%rcx,4), %eax
	movb	%al, -41(%rbp)
	.loc	1 582 0                 # decompress.c:582:0
	movzbl	-41(%rbp), %eax
	movq	-40(%rbp), %rcx
	movslq	1096(%rcx,%rax,4), %rax
	movq	3152(%rcx), %rdx
	movl	-68(%rbp), %esi
	movl	(%rdx,%rax,4), %edi
	movq	$8, %rcx
	shlq	%cl, %rsi
	orq	%rsi, %rdi
	movl	%edi, (%rdx,%rax,4)
	.loc	1 583 0                 # decompress.c:583:0
	movzbl	-41(%rbp), %eax
	movq	-40(%rbp), %rcx
	movl	1096(%rcx,%rax,4), %edx
	incq	%rdx
	movl	%edx, 1096(%rcx,%rax,4)
.Ltmp463:
	.loc	1 580 0                 # decompress.c:580:0
	movl	-68(%rbp), %eax
	incq	%rax
.LBB0_380:                              # %for.cond2850
                                        # =>This Inner Loop Header: Depth=1
	movl	%eax, -68(%rbp)
	movslq	-68(%rbp), %rax
	movslq	-112(%rbp), %rcx
	cmpq	%rcx, %rax
	jl	.LBB0_381
.Ltmp464:
# BB#382:                               # %for.end2873
	.loc	1 586 0                 # decompress.c:586:0
	movq	-40(%rbp), %rax
	movslq	56(%rax), %rcx
	movq	3152(%rax), %rdx
	movl	(%rdx,%rcx,4), %edx
	movq	$8, %rcx
	shrq	%cl, %rdx
	movl	%edx, 60(%rax)
	.loc	1 587 0                 # decompress.c:587:0
	movq	-40(%rbp), %rcx
	xorq	%rax, %rax
	movl	%eax, 1092(%rcx)
	.loc	1 588 0                 # decompress.c:588:0
.Ltmp465:
	movq	-40(%rbp), %rcx
	movzbl	20(%rcx), %ecx
	testq	%rcx, %rcx
	je	.LBB0_387
# BB#383:                               # %if.then2883
	.loc	1 589 0                 # decompress.c:589:0
.Ltmp466:
	movq	-40(%rbp), %rcx
	movl	%eax, 24(%rcx)
	movq	-40(%rbp), %rcx
	movl	%eax, 28(%rcx)
	.loc	1 590 0                 # decompress.c:590:0
	movq	-40(%rbp), %rax
	movl	60(%rax), %ecx
	movq	3152(%rax), %rdx
	movl	(%rdx,%rcx,4), %ecx
	movl	%ecx, 60(%rax)
	movq	-40(%rbp), %rax
	movzbl	60(%rax), %ecx
	movl	%ecx, 64(%rax)
	movq	-40(%rbp), %rax
	movl	60(%rax), %edx
	movq	$8, %rcx
	shrq	%cl, %rdx
	movl	%edx, 60(%rax)
	movq	-40(%rbp), %rax
	movl	1092(%rax), %ecx
	incq	%rcx
	movl	%ecx, 1092(%rax)
	.loc	1 591 0                 # decompress.c:591:0
.Ltmp467:
	movq	-40(%rbp), %rax
	movl	24(%rax), %eax
	testq	%rax, %rax
	jne	.LBB0_386
# BB#384:                               # %if.then2903
	.loc	1 591 0                 # decompress.c:591:0
.Ltmp468:
	movq	-40(%rbp), %rax
	movslq	28(%rax), %rcx
	movl	BZ2_rNums(,%rcx,4), %ecx
	movl	%ecx, 24(%rax)
	movq	-40(%rbp), %rax
	movl	28(%rax), %ecx
	incq	%rcx
	movl	%ecx, 28(%rax)
	.loc	1 591 0                 # decompress.c:591:0
.Ltmp469:
	movq	-40(%rbp), %rax
	movl	28(%rax), %eax
	cmpq	$512, %rax              # imm = 0x200
	jne	.LBB0_386
# BB#385:                               # %if.then2913
	movq	-40(%rbp), %rax
	xorq	%rcx, %rcx
	movl	%ecx, 28(%rax)
.Ltmp470:
.LBB0_386:                              # %if.end2916
	.loc	1 591 0                 # decompress.c:591:0
	movq	-40(%rbp), %rax
	movl	24(%rax), %ecx
	decq	%rcx
	movl	%ecx, 24(%rax)
	movq	-40(%rbp), %rax
	movl	24(%rax), %eax
	cmpq	$1, %rax
	movq	$1, %rax
	jne	.LBB0_376
	jmp	.LBB0_377
.Ltmp471:
.LBB0_365:                              # %for.inc2690
                                        #   in Loop: Header=BB0_361 Depth=1
	.loc	1 553 0                 # decompress.c:553:0
	movzbl	-41(%rbp), %eax
	movq	-40(%rbp), %rcx
	movl	2124(%rcx,%rax,4), %edx
	incq	%rdx
	movl	%edx, 2124(%rcx,%rax,4)
.Ltmp472:
	.loc	1 550 0                 # decompress.c:550:0
	movl	-68(%rbp), %eax
	incq	%rax
.LBB0_361:                              # %for.cond2631
                                        # =>This Inner Loop Header: Depth=1
	movl	%eax, -68(%rbp)
	movslq	-68(%rbp), %rax
	movslq	-112(%rbp), %rcx
	cmpq	%rcx, %rax
	jge	.LBB0_366
# BB#362:                               # %for.body2634
                                        #   in Loop: Header=BB0_361 Depth=1
	.loc	1 551 0                 # decompress.c:551:0
.Ltmp473:
	movq	-40(%rbp), %rax
	movslq	-68(%rbp), %rcx
	movq	3160(%rax), %rax
	movzbl	(%rax,%rcx,2), %eax
	movb	%al, -41(%rbp)
	.loc	1 552 0                 # decompress.c:552:0
.Ltmp474:
	movzbl	-41(%rbp), %eax
	movq	-40(%rbp), %rcx
	movzbl	2124(%rcx,%rax,4), %eax
	movslq	-68(%rbp), %rdx
	movq	3160(%rcx), %rcx
	movw	%ax, (%rcx,%rdx,2)
	.loc	1 552 0                 # decompress.c:552:0
.Ltmp475:
	movzbl	-68(%rbp), %eax
	testq	$1, %rax
	jne	.LBB0_364
# BB#363:                               # %if.then2650
                                        #   in Loop: Header=BB0_361 Depth=1
	movq	-40(%rbp), %rax
	movslq	-68(%rbp), %rdx
	movq	3168(%rax), %rsi
	movzbl	-41(%rbp), %edi
	movq	$1, %rcx
	sarq	%cl, %rdx
	movzbl	(%rsi,%rdx), %ecx
	movzbl	2126(%rax,%rdi,4), %eax
	andq	$240, %rcx
	orq	%rcx, %rax
	movb	%al, (%rsi,%rdx)
	jmp	.LBB0_365
.LBB0_364:                              # %if.else2667
                                        #   in Loop: Header=BB0_361 Depth=1
	movq	-40(%rbp), %rax
	movslq	-68(%rbp), %rdx
	movzbl	-41(%rbp), %esi
	movq	3168(%rax), %rdi
	movq	$1, %rcx
	sarq	%cl, %rdx
	movl	2124(%rax,%rsi,4), %eax
	movzbl	(%rdi,%rdx), %esi
	andq	$15, %rsi
	movq	$12, %rcx
	shrq	%cl, %rax
	andq	$240, %rax
	orq	%rsi, %rax
	movb	%al, (%rdi,%rdx)
	jmp	.LBB0_365
.Ltmp476:
.LBB0_310:                              # %while.body2287
                                        #   in Loop: Header=BB0_309 Depth=1
	#DEBUG_VALUE: ii <- [RBP+-320]
	#DEBUG_VALUE: jj <- [RBP+-324]
	#DEBUG_VALUE: kk <- [RBP+-328]
	#DEBUG_VALUE: pp <- [RBP+-332]
	#DEBUG_VALUE: lno <- [RBP+-336]
	#DEBUG_VALUE: off <- [RBP+-340]
	#DEBUG_VALUE: nn <- [RBP+-344]
	.loc	1 489 0                 # decompress.c:489:0
	movslq	-336(%rbp), %rax
	movq	-40(%rbp), %rcx
	movl	7820(%rcx,%rax,4), %edx
	decq	%rdx
	movl	%edx, 7820(%rcx,%rax,4)
	.loc	1 490 0                 # decompress.c:490:0
	movslq	-336(%rbp), %rax
	leaq	-1(%rax), %rdx
	movq	$32, %rcx
	movq	-40(%rbp), %rsi
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movl	7820(%rsi,%rdx,4), %edx
	addq	$16, %rdx
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movzbl	3723(%rdx,%rsi), %ecx
	movslq	7820(%rsi,%rax,4), %rax
	movb	%cl, 3724(%rsi,%rax)
	.loc	1 492 0                 # decompress.c:492:0
	movl	-336(%rbp), %eax
	decq	%rax
	movl	%eax, -336(%rbp)
.Ltmp477:
.LBB0_309:                              # %while.cond2284
                                        # =>This Inner Loop Header: Depth=1
	#DEBUG_VALUE: ii <- [RBP+-320]
	#DEBUG_VALUE: jj <- [RBP+-324]
	#DEBUG_VALUE: kk <- [RBP+-328]
	#DEBUG_VALUE: pp <- [RBP+-332]
	#DEBUG_VALUE: lno <- [RBP+-336]
	#DEBUG_VALUE: off <- [RBP+-340]
	#DEBUG_VALUE: nn <- [RBP+-344]
	.loc	1 488 0                 # decompress.c:488:0
	movslq	-336(%rbp), %rax
	testq	%rax, %rax
	jg	.LBB0_310
# BB#311:                               # %while.end2308
	#DEBUG_VALUE: ii <- [RBP+-320]
	#DEBUG_VALUE: jj <- [RBP+-324]
	#DEBUG_VALUE: kk <- [RBP+-328]
	#DEBUG_VALUE: pp <- [RBP+-332]
	#DEBUG_VALUE: lno <- [RBP+-336]
	#DEBUG_VALUE: off <- [RBP+-340]
	#DEBUG_VALUE: nn <- [RBP+-344]
	.loc	1 494 0                 # decompress.c:494:0
	movq	-40(%rbp), %rax
	movl	7820(%rax), %ecx
	decq	%rcx
	movl	%ecx, 7820(%rax)
	.loc	1 495 0                 # decompress.c:495:0
	movq	-40(%rbp), %rax
	movzbl	-41(%rbp), %ecx
	movslq	7820(%rax), %rdx
	movb	%cl, 3724(%rax,%rdx)
	.loc	1 496 0                 # decompress.c:496:0
.Ltmp478:
	movq	-40(%rbp), %rax
	movl	7820(%rax), %eax
	testq	%rax, %rax
	jne	.LBB0_319
# BB#312:                               # %if.then2321
	#DEBUG_VALUE: ii <- [RBP+-320]
	#DEBUG_VALUE: jj <- [RBP+-324]
	#DEBUG_VALUE: kk <- [RBP+-328]
	#DEBUG_VALUE: pp <- [RBP+-332]
	#DEBUG_VALUE: lno <- [RBP+-336]
	#DEBUG_VALUE: off <- [RBP+-340]
	#DEBUG_VALUE: nn <- [RBP+-344]
	movq	$4095, %rax             # imm = 0xFFF
	.loc	1 497 0                 # decompress.c:497:0
.Ltmp479:
	movl	%eax, -328(%rbp)
	movq	$15, %rax
	.loc	1 498 0                 # decompress.c:498:0
.Ltmp480:
	movl	%eax, -320(%rbp)
	jmp	.LBB0_313
.Ltmp481:
.LBB0_387:                              # %if.else2925
	.loc	1 593 0                 # decompress.c:593:0
	movq	-40(%rbp), %rax
	movl	60(%rax), %ecx
	movq	3152(%rax), %rdx
	movl	(%rdx,%rcx,4), %ecx
	movl	%ecx, 60(%rax)
	movq	-40(%rbp), %rax
	movzbl	60(%rax), %ecx
	movl	%ecx, 64(%rax)
	movq	-40(%rbp), %rax
	movl	60(%rax), %edx
	movq	$8, %rcx
	shrq	%cl, %rdx
	movl	%edx, 60(%rax)
	jmp	.LBB0_388
.Ltmp482:
.LBB0_366:                              # %for.end2692
	.loc	1 557 0                 # decompress.c:557:0
	movq	-40(%rbp), %rax
	movslq	56(%rax), %rax
	.loc	1 558 0                 # decompress.c:558:0
	movq	-40(%rbp), %rcx
	.loc	1 557 0                 # decompress.c:557:0
	movl	%eax, -68(%rbp)
	.loc	1 558 0                 # decompress.c:558:0
	movq	3160(%rcx), %rdx
	movq	3168(%rcx), %rsi
	movzbl	(%rdx,%rax,2), %edx
	movq	$1, %rcx
	movq	%rax, %rdi
	sarq	%cl, %rdi
	movzbl	(%rsi,%rdi), %esi
	movq	$2, %rcx
	shlq	%cl, %rax
	andq	$4, %rax
	movq	%rax, %rcx
	shrq	%cl, %rsi
	andq	$15, %rsi
	movq	$16, %rcx
	shlq	%cl, %rsi
	orq	%rdx, %rsi
	movl	%esi, -72(%rbp)
.LBB0_367:                              # %do.body2709
                                        # =>This Inner Loop Header: Depth=1
.Ltmp483:
	#DEBUG_VALUE: tmp <- [RBP+-360]
	.loc	1 560 0                 # decompress.c:560:0
	movq	-40(%rbp), %rax
	movslq	-72(%rbp), %rdx
	movq	3160(%rax), %rcx
	movq	3168(%rax), %rax
	movzbl	(%rcx,%rdx,2), %esi
	movq	$1, %rcx
	movq	%rdx, %rdi
	sarq	%cl, %rdi
	movzbl	(%rax,%rdi), %eax
	movq	$2, %rcx
	shlq	%cl, %rdx
	andq	$4, %rdx
	movq	%rdx, %rcx
	shrq	%cl, %rax
	andq	$15, %rax
	movq	$16, %rcx
	shlq	%cl, %rax
	orq	%rsi, %rax
	.loc	1 561 0                 # decompress.c:561:0
.Ltmp484:
	movq	-40(%rbp), %rcx
.Ltmp485:
	.loc	1 560 0                 # decompress.c:560:0
	movl	%eax, -360(%rbp)
	.loc	1 561 0                 # decompress.c:561:0
.Ltmp486:
	movzbl	-68(%rbp), %eax
	movslq	-72(%rbp), %rdx
	movq	3160(%rcx), %rcx
	movw	%ax, (%rcx,%rdx,2)
	.loc	1 561 0                 # decompress.c:561:0
.Ltmp487:
	movzbl	-72(%rbp), %eax
	testq	$1, %rax
	jne	.LBB0_369
# BB#368:                               # %if.then2735
                                        #   in Loop: Header=BB0_367 Depth=1
	#DEBUG_VALUE: tmp <- [RBP+-360]
	movq	-40(%rbp), %rax
	movslq	-72(%rbp), %rdx
	movq	3168(%rax), %rax
	movq	$1, %rcx
	sarq	%cl, %rdx
	movzbl	(%rax,%rdx), %ecx
	movzbl	-66(%rbp), %esi
	andq	$240, %rcx
	orq	%rcx, %rsi
	jmp	.LBB0_370
.LBB0_369:                              # %if.else2749
                                        #   in Loop: Header=BB0_367 Depth=1
	#DEBUG_VALUE: tmp <- [RBP+-360]
	movq	-40(%rbp), %rax
	movslq	-72(%rbp), %rdx
	movq	3168(%rax), %rax
	movq	$1, %rcx
	sarq	%cl, %rdx
	movl	-68(%rbp), %esi
	movzbl	(%rax,%rdx), %edi
	andq	$15, %rdi
	movq	$12, %rcx
	shrq	%cl, %rsi
	andq	$240, %rsi
	orq	%rdi, %rsi
.Ltmp488:
.LBB0_370:                              # %do.cond2765
                                        #   in Loop: Header=BB0_367 Depth=1
	movb	%sil, (%rax,%rdx)
.Ltmp489:
	#DEBUG_VALUE: tmp <- [RBP+-360]
	.loc	1 562 0                 # decompress.c:562:0
	movl	-72(%rbp), %eax
	.loc	1 563 0                 # decompress.c:563:0
	movl	-360(%rbp), %ecx
	.loc	1 564 0                 # decompress.c:564:0
	movq	-40(%rbp), %rdx
	.loc	1 562 0                 # decompress.c:562:0
	movl	%eax, -68(%rbp)
	.loc	1 563 0                 # decompress.c:563:0
	movl	%ecx, -72(%rbp)
	.loc	1 564 0                 # decompress.c:564:0
	movl	-68(%rbp), %eax
	movl	56(%rdx), %ecx
	cmpq	%rcx, %rax
	jne	.LBB0_367
.Ltmp490:
# BB#371:                               # %do.end2769
	.loc	1 567 0                 # decompress.c:567:0
	movq	-40(%rbp), %rax
	movl	56(%rax), %ecx
	movl	%ecx, 60(%rax)
	.loc	1 568 0                 # decompress.c:568:0
	movq	-40(%rbp), %rcx
	xorq	%rax, %rax
	movl	%eax, 1092(%rcx)
	.loc	1 569 0                 # decompress.c:569:0
.Ltmp491:
	movq	-40(%rbp), %rcx
	movzbl	20(%rcx), %ecx
	testq	%rcx, %rcx
	je	.LBB0_378
# BB#372:                               # %if.then2773
	.loc	1 570 0                 # decompress.c:570:0
.Ltmp492:
	movq	-40(%rbp), %rcx
	movl	%eax, 24(%rcx)
	movq	-40(%rbp), %rcx
	movl	%eax, 28(%rcx)
	.loc	1 571 0                 # decompress.c:571:0
	movq	-40(%rbp), %rax
	movl	60(%rax), %ecx
	addq	$1096, %rax             # imm = 0x448
	movq	%rcx, %rdi
	movq	%rax, %rsi
	callq	BZ2_indexIntoF
	movq	-40(%rbp), %rcx
	movl	%eax, 64(%rcx)
	movq	-40(%rbp), %rax
	movl	60(%rax), %edx
	movq	3160(%rax), %rcx
	movq	3168(%rax), %rsi
	movzbl	(%rcx,%rdx,2), %edi
	movq	$1, %rcx
	movq	%rdx, %rbx
	shrq	%cl, %rbx
	movzbl	(%rsi,%rbx), %esi
	movq	$2, %rcx
	shlq	%cl, %rdx
	andq	$4, %rdx
	movq	%rdx, %rcx
	shrq	%cl, %rsi
	andq	$15, %rsi
	movq	$16, %rcx
	shlq	%cl, %rsi
	orq	%rdi, %rsi
	movl	%esi, 60(%rax)
	movq	-40(%rbp), %rax
	movl	1092(%rax), %ecx
	incq	%rcx
	movl	%ecx, 1092(%rax)
	.loc	1 572 0                 # decompress.c:572:0
.Ltmp493:
	movq	-40(%rbp), %rax
	movl	24(%rax), %ecx
	movq	$1, %rax
	testq	%rcx, %rcx
	jne	.LBB0_375
# BB#373:                               # %if.then2801
	.loc	1 572 0                 # decompress.c:572:0
.Ltmp494:
	movq	-40(%rbp), %rcx
	movslq	28(%rcx), %rdx
	movl	BZ2_rNums(,%rdx,4), %edx
	movl	%edx, 24(%rcx)
	movq	-40(%rbp), %rcx
	movl	28(%rcx), %edx
	incq	%rdx
	movl	%edx, 28(%rcx)
	.loc	1 572 0                 # decompress.c:572:0
.Ltmp495:
	movq	-40(%rbp), %rcx
	movl	28(%rcx), %ecx
	cmpq	$512, %rcx              # imm = 0x200
	jne	.LBB0_375
# BB#374:                               # %if.then2811
	movq	-40(%rbp), %rcx
	xorq	%rdx, %rdx
	movl	%edx, 28(%rcx)
.Ltmp496:
.LBB0_375:                              # %if.end2814
	.loc	1 572 0                 # decompress.c:572:0
	movq	-40(%rbp), %rcx
	movl	24(%rcx), %edx
	decq	%rdx
	movl	%edx, 24(%rcx)
	movq	-40(%rbp), %rcx
	movl	24(%rcx), %ecx
	cmpq	$1, %rcx
	je	.LBB0_377
.LBB0_376:                              # %select.mid
	xorq	%rax, %rax
.LBB0_377:                              # %select.end
	movq	-40(%rbp), %rcx
	movl	64(%rcx), %edx
	xorq	%rax, %rdx
	movl	%edx, 64(%rcx)
	jmp	.LBB0_7
.Ltmp497:
.LBB0_317:                              # %for.inc2348
                                        #   in Loop: Header=BB0_313 Depth=1
	#DEBUG_VALUE: ii <- [RBP+-320]
	#DEBUG_VALUE: jj <- [RBP+-324]
	#DEBUG_VALUE: kk <- [RBP+-328]
	#DEBUG_VALUE: pp <- [RBP+-332]
	#DEBUG_VALUE: lno <- [RBP+-336]
	#DEBUG_VALUE: off <- [RBP+-340]
	#DEBUG_VALUE: nn <- [RBP+-344]
	.loc	1 503 0                 # decompress.c:503:0
	movl	-328(%rbp), %ecx
	movslq	-320(%rbp), %rdx
	movq	-40(%rbp), %rsi
	incq	%rcx
	movl	%ecx, 7820(%rsi,%rdx,4)
.Ltmp498:
	.loc	1 498 0                 # decompress.c:498:0
	movl	-320(%rbp), %ecx
	decq	%rcx
	movl	%ecx, -320(%rbp)
.LBB0_313:                              # %for.cond2322
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB0_315 Depth 2
	#DEBUG_VALUE: ii <- [RBP+-320]
	#DEBUG_VALUE: jj <- [RBP+-324]
	#DEBUG_VALUE: kk <- [RBP+-328]
	#DEBUG_VALUE: pp <- [RBP+-332]
	#DEBUG_VALUE: lno <- [RBP+-336]
	#DEBUG_VALUE: off <- [RBP+-340]
	#DEBUG_VALUE: nn <- [RBP+-344]
	movslq	-320(%rbp), %rcx
	testq	%rcx, %rcx
	js	.LBB0_319
# BB#314:                               # %for.body2325
                                        #   in Loop: Header=BB0_313 Depth=1
	#DEBUG_VALUE: ii <- [RBP+-320]
	#DEBUG_VALUE: jj <- [RBP+-324]
	#DEBUG_VALUE: kk <- [RBP+-328]
	#DEBUG_VALUE: pp <- [RBP+-332]
	#DEBUG_VALUE: lno <- [RBP+-336]
	#DEBUG_VALUE: off <- [RBP+-340]
	#DEBUG_VALUE: nn <- [RBP+-344]
	.loc	1 499 0                 # decompress.c:499:0
.Ltmp499:
	movl	%eax, -324(%rbp)
	jmp	.LBB0_315
.LBB0_316:                              # %for.inc2341
                                        #   in Loop: Header=BB0_315 Depth=2
	#DEBUG_VALUE: ii <- [RBP+-320]
	#DEBUG_VALUE: jj <- [RBP+-324]
	#DEBUG_VALUE: kk <- [RBP+-328]
	#DEBUG_VALUE: pp <- [RBP+-332]
	#DEBUG_VALUE: lno <- [RBP+-336]
	#DEBUG_VALUE: off <- [RBP+-340]
	#DEBUG_VALUE: nn <- [RBP+-344]
	.loc	1 500 0                 # decompress.c:500:0
.Ltmp500:
	movslq	-320(%rbp), %rcx
	movq	-40(%rbp), %rdx
	movl	7820(%rdx,%rcx,4), %ecx
	movl	-324(%rbp), %esi
	addq	%rcx, %rsi
	movq	$32, %rcx
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	movzbl	3724(%rdx,%rsi), %ecx
	movslq	-328(%rbp), %rsi
	movb	%cl, 3724(%rdx,%rsi)
	.loc	1 501 0                 # decompress.c:501:0
	movl	-328(%rbp), %ecx
.Ltmp501:
	.loc	1 499 0                 # decompress.c:499:0
	movl	-324(%rbp), %edx
	.loc	1 501 0                 # decompress.c:501:0
.Ltmp502:
	decq	%rcx
	movl	%ecx, -328(%rbp)
.Ltmp503:
	.loc	1 499 0                 # decompress.c:499:0
	decq	%rdx
	movl	%edx, -324(%rbp)
.LBB0_315:                              # %for.cond2326
                                        #   Parent Loop BB0_313 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
.Ltmp504:
	#DEBUG_VALUE: ii <- [RBP+-320]
	#DEBUG_VALUE: jj <- [RBP+-324]
	#DEBUG_VALUE: kk <- [RBP+-328]
	#DEBUG_VALUE: pp <- [RBP+-332]
	#DEBUG_VALUE: lno <- [RBP+-336]
	#DEBUG_VALUE: off <- [RBP+-340]
	#DEBUG_VALUE: nn <- [RBP+-344]
	movslq	-324(%rbp), %rcx
	testq	%rcx, %rcx
	jns	.LBB0_316
	jmp	.LBB0_317
.Ltmp505:
.LBB0_378:                              # %if.else2821
	.loc	1 574 0                 # decompress.c:574:0
	movq	-40(%rbp), %rax
	movl	60(%rax), %ecx
	addq	$1096, %rax             # imm = 0x448
	movq	%rcx, %rdi
	movq	%rax, %rsi
	callq	BZ2_indexIntoF
	movq	-40(%rbp), %rcx
	movl	%eax, 64(%rcx)
	movq	-40(%rbp), %rax
	movl	60(%rax), %edx
	movq	3160(%rax), %rcx
	movq	3168(%rax), %rsi
	movzbl	(%rcx,%rdx,2), %edi
	movq	$1, %rcx
	movq	%rdx, %rbx
	shrq	%cl, %rbx
	movzbl	(%rsi,%rbx), %esi
	movq	$2, %rcx
	shlq	%cl, %rdx
	andq	$4, %rdx
	movq	%rdx, %rcx
	shrq	%cl, %rsi
	andq	$15, %rsi
	movq	$16, %rcx
	shlq	%cl, %rsi
	orq	%rdi, %rsi
	movl	%esi, 60(%rax)
.Ltmp506:
.LBB0_388:                              # %if.end2941
	.loc	1 593 0                 # decompress.c:593:0
	movq	-40(%rbp), %rax
	movl	1092(%rax), %ecx
	incq	%rcx
	movl	%ecx, 1092(%rax)
	jmp	.LBB0_7
.Ltmp507:
.LBB0_319:                              # %if.end2352
	.loc	1 510 0                 # decompress.c:510:0
	movzbl	-41(%rbp), %eax
	movq	-40(%rbp), %rcx
	movzbl	3468(%rcx,%rax), %eax
	movl	68(%rcx,%rax,4), %edx
	incq	%rdx
	movl	%edx, 68(%rcx,%rax,4)
	.loc	1 511 0                 # decompress.c:511:0
.Ltmp508:
	movq	-40(%rbp), %rax
	movzbl	44(%rax), %eax
	testq	%rax, %rax
	je	.LBB0_321
# BB#320:                               # %if.then2362
	.loc	1 512 0                 # decompress.c:512:0
	movzbl	-41(%rbp), %eax
	movq	-40(%rbp), %rcx
	movzbl	3468(%rcx,%rax), %eax
	movslq	-112(%rbp), %rdx
	movq	3160(%rcx), %rcx
	movw	%ax, (%rcx,%rdx,2)
	jmp	.LBB0_322
.LBB0_321:                              # %if.else2370
	.loc	1 513 0                 # decompress.c:513:0
	movzbl	-41(%rbp), %eax
	movq	-40(%rbp), %rcx
	movzbl	3468(%rcx,%rax), %eax
	movslq	-112(%rbp), %rdx
	movq	3152(%rcx), %rcx
	movl	%eax, (%rcx,%rdx,4)
.Ltmp509:
.LBB0_322:                              # %if.end2378
	.loc	1 514 0                 # decompress.c:514:0
	movl	-112(%rbp), %eax
	.loc	1 516 0                 # decompress.c:516:0
.Ltmp510:
	movl	-100(%rbp), %ecx
.Ltmp511:
	.loc	1 514 0                 # decompress.c:514:0
	incq	%rax
	movl	%eax, -112(%rbp)
	.loc	1 516 0                 # decompress.c:516:0
.Ltmp512:
	testq	%rcx, %rcx
	jne	.LBB0_325
# BB#323:                               # %if.then2382
	.loc	1 516 0                 # decompress.c:516:0
.Ltmp513:
	movl	-96(%rbp), %eax
	incq	%rax
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	.loc	1 516 0                 # decompress.c:516:0
.Ltmp514:
	movslq	-88(%rbp), %rsi
	movq	$32, %rcx
.Ltmp515:
	.loc	1 516 0                 # decompress.c:516:0
	sarq	%cl, %rdx
	movl	%eax, -96(%rbp)
	cmpq	%rsi, %rdx
	jge	.LBB0_43
# BB#324:                               # %if.end2387
	movslq	-96(%rbp), %rax
	movq	-40(%rbp), %rcx
	movq	$50, %rdx
	movl	%edx, -100(%rbp)
	movzbl	7884(%rcx,%rax), %eax
	movl	%eax, -144(%rbp)
	movslq	-144(%rbp), %rax
	movq	-40(%rbp), %rcx
	movl	64012(%rcx,%rax,4), %eax
	movl	%eax, -148(%rbp)
	movslq	-144(%rbp), %rax
	movq	-40(%rbp), %rcx
	imulq	$1032, %rax, %rax       # imm = 0x408
	leaq	45436(%rcx,%rax), %rax
	movq	%rax, -160(%rbp)
	movslq	-144(%rbp), %rax
	movq	-40(%rbp), %rcx
	imulq	$1032, %rax, %rax       # imm = 0x408
	movslq	-144(%rbp), %rdx
	movq	-40(%rbp), %rsi
	leaq	57820(%rcx,%rax), %rax
	movq	%rax, -176(%rbp)
	imulq	$1032, %rdx, %rax       # imm = 0x408
	leaq	51628(%rsi,%rax), %rax
	movq	%rax, -168(%rbp)
.Ltmp516:
.LBB0_325:                              # %if.end2407
	.loc	1 516 0                 # decompress.c:516:0
	movl	-100(%rbp), %eax
	movl	-148(%rbp), %ecx
	decq	%rax
	movl	%eax, -100(%rbp)
	movl	%ecx, -132(%rbp)
.LBB0_326:                              # %sw.bb2409
	movq	-40(%rbp), %rax
	movq	$40, %rcx
	movl	%ecx, 8(%rax)
	jmp	.LBB0_327
.LBB0_339:                              # %if.then2457
                                        #   in Loop: Header=BB0_327 Depth=1
	.loc	1 516 0                 # decompress.c:516:0
.Ltmp517:
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	movl	16(%rax), %ecx
	incq	%rcx
	movl	%ecx, 16(%rax)
.Ltmp518:
.LBB0_327:                              # %while.body2412
                                        # =>This Inner Loop Header: Depth=1
	.loc	1 516 0                 # decompress.c:516:0
	movq	-40(%rbp), %rax
	movslq	36(%rax), %rax
	movslq	-132(%rbp), %rcx
	cmpq	%rcx, %rax
	jge	.LBB0_328
.Ltmp519:
# BB#337:                               # %if.end2428
                                        #   in Loop: Header=BB0_327 Depth=1
	.loc	1 516 0                 # decompress.c:516:0
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	movl	8(%rax), %eax
	testq	%rax, %rax
	je	.LBB0_7
.Ltmp520:
# BB#338:                               # %if.end2434
                                        #   in Loop: Header=BB0_327 Depth=1
	.loc	1 516 0                 # decompress.c:516:0
	movq	-40(%rbp), %rax
	movq	(%rax), %rcx
	movq	(%rcx), %rcx
	movl	32(%rax), %edx
	movzbl	(%rcx), %esi
	movq	$8, %rcx
	shlq	%cl, %rdx
	orq	%rdx, %rsi
	movl	%esi, 32(%rax)
	movq	-40(%rbp), %rax
	movl	36(%rax), %ecx
	addq	$8, %rcx
	movl	%ecx, 36(%rax)
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	incq	(%rax)
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	movl	8(%rax), %ecx
	decq	%rcx
	movl	%ecx, 8(%rax)
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	movl	12(%rax), %ecx
	incq	%rcx
	movl	%ecx, 12(%rax)
	.loc	1 516 0                 # decompress.c:516:0
.Ltmp521:
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	movl	12(%rax), %eax
	testq	%rax, %rax
	jne	.LBB0_327
	jmp	.LBB0_339
.Ltmp522:
.LBB0_328:                              # %if.then2416
	#DEBUG_VALUE: v <- [RBP+-352]
	.loc	1 516 0                 # decompress.c:516:0
	movq	-40(%rbp), %rdx
	movl	36(%rdx), %ecx
	movl	-132(%rbp), %eax
	movl	32(%rdx), %edx
	subq	%rax, %rcx
	andq	%r14, %rcx
	shrq	%cl, %rdx
	movq	$1, %rsi
	movq	%rax, %rcx
	shlq	%cl, %rsi
	decq	%rsi
	movq	-40(%rbp), %rax
	andq	%rdx, %rsi
	movl	%esi, -352(%rbp)
	movl	-132(%rbp), %ecx
	movl	36(%rax), %edx
	subq	%rcx, %rdx
	movl	%edx, 36(%rax)
	movl	-352(%rbp), %eax
	jmp	.LBB0_329
.Ltmp523:
.LBB0_261:                              # %do.body
	.loc	1 424 0                 # decompress.c:424:0
	movl	-104(%rbp), %eax
	testq	%rax, %rax
	je	.LBB0_262
# BB#263:                               # %if.else1945
	.loc	1 425 0                 # decompress.c:425:0
.Ltmp524:
	movl	-104(%rbp), %eax
	cmpq	$1, %rax
	jne	.LBB0_265
# BB#264:                               # %if.then1948
	movl	-116(%rbp), %eax
	movl	-120(%rbp), %ecx
	leaq	(%rax,%rcx,2), %rax
	movl	%eax, -116(%rbp)
	jmp	.LBB0_265
.Ltmp525:
.LBB0_262:                              # %if.then1942
	.loc	1 424 0                 # decompress.c:424:0
	movl	-116(%rbp), %eax
	movl	-120(%rbp), %ecx
	addq	%rax, %rcx
	movl	%ecx, -116(%rbp)
.Ltmp526:
.LBB0_265:                              # %if.end1952
	.loc	1 426 0                 # decompress.c:426:0
	movl	-120(%rbp), %eax
	.loc	1 427 0                 # decompress.c:427:0
.Ltmp527:
	movl	-100(%rbp), %ecx
.Ltmp528:
	.loc	1 426 0                 # decompress.c:426:0
	addq	%rax, %rax
	movl	%eax, -120(%rbp)
	.loc	1 427 0                 # decompress.c:427:0
.Ltmp529:
	testq	%rcx, %rcx
	jne	.LBB0_268
# BB#266:                               # %if.then1956
	.loc	1 427 0                 # decompress.c:427:0
.Ltmp530:
	movl	-96(%rbp), %eax
	incq	%rax
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	.loc	1 427 0                 # decompress.c:427:0
.Ltmp531:
	movslq	-88(%rbp), %rsi
	movq	$32, %rcx
.Ltmp532:
	.loc	1 427 0                 # decompress.c:427:0
	sarq	%cl, %rdx
	movl	%eax, -96(%rbp)
	cmpq	%rsi, %rdx
	jge	.LBB0_43
# BB#267:                               # %if.end1961
	movslq	-96(%rbp), %rax
	movq	-40(%rbp), %rcx
	movq	$50, %rdx
	movl	%edx, -100(%rbp)
	movzbl	7884(%rcx,%rax), %eax
	movl	%eax, -144(%rbp)
	movslq	-144(%rbp), %rax
	movq	-40(%rbp), %rcx
	movl	64012(%rcx,%rax,4), %eax
	movl	%eax, -148(%rbp)
	movslq	-144(%rbp), %rax
	movq	-40(%rbp), %rcx
	imulq	$1032, %rax, %rax       # imm = 0x408
	leaq	45436(%rcx,%rax), %rax
	movq	%rax, -160(%rbp)
	movslq	-144(%rbp), %rax
	movq	-40(%rbp), %rcx
	imulq	$1032, %rax, %rax       # imm = 0x408
	movslq	-144(%rbp), %rdx
	movq	-40(%rbp), %rsi
	leaq	57820(%rcx,%rax), %rax
	movq	%rax, -176(%rbp)
	imulq	$1032, %rdx, %rax       # imm = 0x408
	leaq	51628(%rsi,%rax), %rax
	movq	%rax, -168(%rbp)
.Ltmp533:
.LBB0_268:                              # %if.end1981
	.loc	1 427 0                 # decompress.c:427:0
	movl	-100(%rbp), %eax
	movl	-148(%rbp), %ecx
	decq	%rax
	movl	%eax, -100(%rbp)
	movl	%ecx, -132(%rbp)
.LBB0_269:                              # %sw.bb1983
	movq	-40(%rbp), %rax
	movq	$38, %rcx
	movl	%ecx, 8(%rax)
	jmp	.LBB0_270
.LBB0_282:                              # %if.then2031
                                        #   in Loop: Header=BB0_270 Depth=1
	.loc	1 427 0                 # decompress.c:427:0
.Ltmp534:
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	movl	16(%rax), %ecx
	incq	%rcx
	movl	%ecx, 16(%rax)
.Ltmp535:
.LBB0_270:                              # %while.body1986
                                        # =>This Inner Loop Header: Depth=1
	.loc	1 427 0                 # decompress.c:427:0
	movq	-40(%rbp), %rax
	movslq	36(%rax), %rax
	movslq	-132(%rbp), %rcx
	cmpq	%rcx, %rax
	jge	.LBB0_271
.Ltmp536:
# BB#280:                               # %if.end2002
                                        #   in Loop: Header=BB0_270 Depth=1
	.loc	1 427 0                 # decompress.c:427:0
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	movl	8(%rax), %eax
	testq	%rax, %rax
	je	.LBB0_7
.Ltmp537:
# BB#281:                               # %if.end2008
                                        #   in Loop: Header=BB0_270 Depth=1
	.loc	1 427 0                 # decompress.c:427:0
	movq	-40(%rbp), %rax
	movq	(%rax), %rcx
	movq	(%rcx), %rcx
	movl	32(%rax), %edx
	movzbl	(%rcx), %esi
	movq	$8, %rcx
	shlq	%cl, %rdx
	orq	%rdx, %rsi
	movl	%esi, 32(%rax)
	movq	-40(%rbp), %rax
	movl	36(%rax), %ecx
	addq	$8, %rcx
	movl	%ecx, 36(%rax)
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	incq	(%rax)
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	movl	8(%rax), %ecx
	decq	%rcx
	movl	%ecx, 8(%rax)
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	movl	12(%rax), %ecx
	incq	%rcx
	movl	%ecx, 12(%rax)
	.loc	1 427 0                 # decompress.c:427:0
.Ltmp538:
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	movl	12(%rax), %eax
	testq	%rax, %rax
	jne	.LBB0_270
	jmp	.LBB0_282
.Ltmp539:
.LBB0_271:                              # %if.then1990
	#DEBUG_VALUE: v <- [RBP+-312]
	.loc	1 427 0                 # decompress.c:427:0
	movq	-40(%rbp), %rdx
	movl	36(%rdx), %ecx
	movl	-132(%rbp), %eax
	movl	32(%rdx), %edx
	subq	%rax, %rcx
	andq	%r14, %rcx
	shrq	%cl, %rdx
	movq	$1, %rsi
	movq	%rax, %rcx
	shlq	%cl, %rsi
	decq	%rsi
	movq	-40(%rbp), %rax
	andq	%rdx, %rsi
	movl	%esi, -312(%rbp)
	movl	-132(%rbp), %ecx
	movl	36(%rax), %edx
	subq	%rcx, %rdx
	movl	%edx, 36(%rax)
	movl	-312(%rbp), %eax
.Ltmp540:
.LBB0_272:                              # %while.body2038
	movl	%eax, -136(%rbp)
.Ltmp541:
	.loc	1 427 0                 # decompress.c:427:0
	movslq	-132(%rbp), %rax
	cmpq	$21, %rax
	jge	.LBB0_43
.Ltmp542:
# BB#273:                               # %if.end2042
	.loc	1 427 0                 # decompress.c:427:0
	movslq	-132(%rbp), %rax
	movq	-160(%rbp), %rcx
	movslq	-136(%rbp), %rdx
	movslq	(%rcx,%rax,4), %rax
	cmpq	%rax, %rdx
	jg	.LBB0_274
.Ltmp543:
# BB#284:                               # %while.end2104
	.loc	1 427 0                 # decompress.c:427:0
	movslq	-132(%rbp), %rax
	movq	-168(%rbp), %rcx
	movl	-136(%rbp), %edx
	movl	(%rcx,%rax,4), %eax
	subq	%rax, %rdx
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	testq	%rdx, %rdx
	js	.LBB0_43
# BB#285:                               # %lor.lhs.false2110
	movslq	-132(%rbp), %rax
	movq	-168(%rbp), %rcx
	movl	-136(%rbp), %edx
	movl	(%rcx,%rax,4), %eax
	subq	%rax, %rdx
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	$258, %rdx              # imm = 0x102
	jge	.LBB0_43
.Ltmp544:
# BB#286:                               # %do.cond
	.loc	1 427 0                 # decompress.c:427:0
	movslq	-132(%rbp), %rax
	movq	-168(%rbp), %rcx
	movl	-136(%rbp), %edx
	movl	(%rcx,%rax,4), %eax
	subq	%rax, %rdx
	movq	$32, %rcx
	movq	-176(%rbp), %rax
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movl	(%rax,%rdx,4), %eax
	testq	%rax, %rax
	movl	%eax, -104(%rbp)
	movq	$1, %rax
	je	.LBB0_290
.Ltmp545:
# BB#287:                               # %lor.rhs
	.loc	1 428 0                 # decompress.c:428:0
	movl	-104(%rbp), %eax
	cmpq	$1, %rax
	jne	.LBB0_288
# BB#289:                               # %lor.rhs
	movabsq	$1, %rax
	jmp	.LBB0_290
.LBB0_274:                              # %if.end2048
	.loc	1 427 0                 # decompress.c:427:0
.Ltmp546:
	movl	-132(%rbp), %eax
	incq	%rax
	movl	%eax, -132(%rbp)
.LBB0_275:                              # %sw.bb2050
	movq	-40(%rbp), %rax
	movq	$39, %rcx
	movl	%ecx, 8(%rax)
	jmp	.LBB0_276
.LBB0_279:                              # %if.then2096
                                        #   in Loop: Header=BB0_276 Depth=1
	.loc	1 427 0                 # decompress.c:427:0
.Ltmp547:
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	movl	16(%rax), %ecx
	incq	%rcx
	movl	%ecx, 16(%rax)
.Ltmp548:
.LBB0_276:                              # %while.body2053
                                        # =>This Inner Loop Header: Depth=1
	.loc	1 427 0                 # decompress.c:427:0
	movq	-40(%rbp), %rax
	movslq	36(%rax), %rax
	testq	%rax, %rax
	jg	.LBB0_283
.Ltmp549:
# BB#277:                               # %if.end2067
                                        #   in Loop: Header=BB0_276 Depth=1
	.loc	1 427 0                 # decompress.c:427:0
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	movl	8(%rax), %eax
	testq	%rax, %rax
	je	.LBB0_7
.Ltmp550:
# BB#278:                               # %if.end2073
                                        #   in Loop: Header=BB0_276 Depth=1
	.loc	1 427 0                 # decompress.c:427:0
	movq	-40(%rbp), %rax
	movq	(%rax), %rcx
	movq	(%rcx), %rcx
	movl	32(%rax), %edx
	movzbl	(%rcx), %esi
	movq	$8, %rcx
	shlq	%cl, %rdx
	orq	%rdx, %rsi
	movl	%esi, 32(%rax)
	movq	-40(%rbp), %rax
	movl	36(%rax), %ecx
	addq	$8, %rcx
	movl	%ecx, 36(%rax)
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	incq	(%rax)
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	movl	8(%rax), %ecx
	decq	%rcx
	movl	%ecx, 8(%rax)
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	movl	12(%rax), %ecx
	incq	%rcx
	movl	%ecx, 12(%rax)
	.loc	1 427 0                 # decompress.c:427:0
.Ltmp551:
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	movl	12(%rax), %eax
	testq	%rax, %rax
	jne	.LBB0_276
	jmp	.LBB0_279
.Ltmp552:
.LBB0_283:                              # %while.end2101
	#DEBUG_VALUE: v <- [RBP+-316]
	.loc	1 427 0                 # decompress.c:427:0
	movq	-40(%rbp), %rax
	movl	36(%rax), %ecx
	movl	32(%rax), %eax
	decq	%rcx
	andq	%r14, %rcx
	movq	-40(%rbp), %rdx
	shrq	%cl, %rax
	andq	$1, %rax
	movl	%eax, -316(%rbp)
	movl	36(%rdx), %eax
	decq	%rax
	movl	%eax, 36(%rdx)
.Ltmp553:
	.loc	1 427 0                 # decompress.c:427:0
	movl	-136(%rbp), %eax
	.loc	1 427 0                 # decompress.c:427:0
.Ltmp554:
	movl	-316(%rbp), %ecx
	movl	%ecx, -140(%rbp)
.Ltmp555:
	.loc	1 427 0                 # decompress.c:427:0
	addq	%rax, %rax
	orq	%rcx, %rax
	jmp	.LBB0_272
.Ltmp556:
.LBB0_288:
	.loc	1 428 0                 # decompress.c:428:0
	movabsq	$0, %rax
.LBB0_290:                              # %lor.end
	testq	%rax, %rax
	jne	.LBB0_261
.Ltmp557:
# BB#291:                               # %do.end
	.loc	1 431 0                 # decompress.c:431:0
	movl	-116(%rbp), %eax
	.loc	1 432 0                 # decompress.c:432:0
	movq	-40(%rbp), %rcx
	.loc	1 431 0                 # decompress.c:431:0
	incq	%rax
	movl	%eax, -116(%rbp)
	.loc	1 432 0                 # decompress.c:432:0
	movslq	7820(%rcx), %rax
	movzbl	3724(%rcx,%rax), %eax
	movzbl	3468(%rcx,%rax), %eax
	movb	%al, -41(%rbp)
	.loc	1 433 0                 # decompress.c:433:0
	movzbl	-41(%rbp), %eax
	movq	-40(%rbp), %rcx
	movl	-116(%rbp), %edx
	movl	68(%rcx,%rax,4), %esi
	addq	%rdx, %rsi
	movl	%esi, 68(%rcx,%rax,4)
	.loc	1 435 0                 # decompress.c:435:0
.Ltmp558:
	movq	-40(%rbp), %rax
	movzbl	44(%rax), %eax
	testq	%rax, %rax
	jne	.LBB0_292
	jmp	.LBB0_295
.LBB0_294:                              # %if.end2149
                                        #   in Loop: Header=BB0_292 Depth=1
	.loc	1 438 0                 # decompress.c:438:0
.Ltmp559:
	movq	-40(%rbp), %rax
	movzbl	-41(%rbp), %ecx
	movslq	-112(%rbp), %rdx
	movq	3160(%rax), %rax
	movw	%cx, (%rax,%rdx,2)
	.loc	1 439 0                 # decompress.c:439:0
	movl	-112(%rbp), %eax
	.loc	1 440 0                 # decompress.c:440:0
	movl	-116(%rbp), %ecx
	.loc	1 439 0                 # decompress.c:439:0
	incq	%rax
	movl	%eax, -112(%rbp)
	.loc	1 440 0                 # decompress.c:440:0
	decq	%rcx
	movl	%ecx, -116(%rbp)
.Ltmp560:
.LBB0_292:                              # %while.cond2142
                                        # =>This Inner Loop Header: Depth=1
	.loc	1 436 0                 # decompress.c:436:0
	movslq	-116(%rbp), %rax
	testq	%rax, %rax
	jle	.LBB0_257
# BB#293:                               # %while.body2145
                                        #   in Loop: Header=BB0_292 Depth=1
	.loc	1 437 0                 # decompress.c:437:0
.Ltmp561:
	movslq	-112(%rbp), %rax
	movslq	-108(%rbp), %rcx
	cmpq	%rcx, %rax
	jl	.LBB0_294
	jmp	.LBB0_43
.Ltmp562:
.LBB0_297:                              # %if.end2165
                                        #   in Loop: Header=BB0_295 Depth=1
	.loc	1 445 0                 # decompress.c:445:0
	movq	-40(%rbp), %rax
	movzbl	-41(%rbp), %ecx
	movslq	-112(%rbp), %rdx
	movq	3152(%rax), %rax
	movl	%ecx, (%rax,%rdx,4)
	.loc	1 446 0                 # decompress.c:446:0
	movl	-112(%rbp), %eax
	.loc	1 447 0                 # decompress.c:447:0
	movl	-116(%rbp), %ecx
	.loc	1 446 0                 # decompress.c:446:0
	incq	%rax
	movl	%eax, -112(%rbp)
	.loc	1 447 0                 # decompress.c:447:0
	decq	%rcx
	movl	%ecx, -116(%rbp)
.Ltmp563:
.LBB0_295:                              # %while.cond2158
                                        # =>This Inner Loop Header: Depth=1
	.loc	1 443 0                 # decompress.c:443:0
	movslq	-116(%rbp), %rax
	testq	%rax, %rax
	jle	.LBB0_257
# BB#296:                               # %while.body2161
                                        #   in Loop: Header=BB0_295 Depth=1
	.loc	1 444 0                 # decompress.c:444:0
.Ltmp564:
	movslq	-112(%rbp), %rax
	movslq	-108(%rbp), %rcx
	cmpq	%rcx, %rax
	jl	.LBB0_297
	jmp	.LBB0_43
.Ltmp565:
.LBB0_329:                              # %while.body2464
	.loc	1 516 0                 # decompress.c:516:0
	movl	%eax, -136(%rbp)
.Ltmp566:
	.loc	1 516 0                 # decompress.c:516:0
	movslq	-132(%rbp), %rax
	cmpq	$21, %rax
	jge	.LBB0_43
.Ltmp567:
# BB#330:                               # %if.end2468
	.loc	1 516 0                 # decompress.c:516:0
	movslq	-132(%rbp), %rax
	movq	-160(%rbp), %rcx
	movslq	-136(%rbp), %rdx
	movslq	(%rcx,%rax,4), %rax
	cmpq	%rax, %rdx
	jle	.LBB0_254
.Ltmp568:
# BB#331:                               # %if.end2474
	.loc	1 516 0                 # decompress.c:516:0
	movl	-132(%rbp), %eax
	incq	%rax
	movl	%eax, -132(%rbp)
.LBB0_332:                              # %sw.bb2476
	movq	-40(%rbp), %rax
	movq	$41, %rcx
	movl	%ecx, 8(%rax)
	jmp	.LBB0_333
.LBB0_336:                              # %if.then2522
                                        #   in Loop: Header=BB0_333 Depth=1
	.loc	1 516 0                 # decompress.c:516:0
.Ltmp569:
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	movl	16(%rax), %ecx
	incq	%rcx
	movl	%ecx, 16(%rax)
.Ltmp570:
.LBB0_333:                              # %while.body2479
                                        # =>This Inner Loop Header: Depth=1
	.loc	1 516 0                 # decompress.c:516:0
	movq	-40(%rbp), %rax
	movslq	36(%rax), %rax
	testq	%rax, %rax
	jg	.LBB0_340
.Ltmp571:
# BB#334:                               # %if.end2493
                                        #   in Loop: Header=BB0_333 Depth=1
	.loc	1 516 0                 # decompress.c:516:0
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	movl	8(%rax), %eax
	testq	%rax, %rax
	je	.LBB0_7
.Ltmp572:
# BB#335:                               # %if.end2499
                                        #   in Loop: Header=BB0_333 Depth=1
	.loc	1 516 0                 # decompress.c:516:0
	movq	-40(%rbp), %rax
	movq	(%rax), %rcx
	movq	(%rcx), %rcx
	movl	32(%rax), %edx
	movzbl	(%rcx), %esi
	movq	$8, %rcx
	shlq	%cl, %rdx
	orq	%rdx, %rsi
	movl	%esi, 32(%rax)
	movq	-40(%rbp), %rax
	movl	36(%rax), %ecx
	addq	$8, %rcx
	movl	%ecx, 36(%rax)
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	incq	(%rax)
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	movl	8(%rax), %ecx
	decq	%rcx
	movl	%ecx, 8(%rax)
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	movl	12(%rax), %ecx
	incq	%rcx
	movl	%ecx, 12(%rax)
	.loc	1 516 0                 # decompress.c:516:0
.Ltmp573:
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	movl	12(%rax), %eax
	testq	%rax, %rax
	jne	.LBB0_333
	jmp	.LBB0_336
.Ltmp574:
.LBB0_340:                              # %while.end2527
	#DEBUG_VALUE: v <- [RBP+-356]
	.loc	1 516 0                 # decompress.c:516:0
	movq	-40(%rbp), %rax
	movl	36(%rax), %ecx
	movl	32(%rax), %eax
	decq	%rcx
	andq	%r14, %rcx
	movq	-40(%rbp), %rdx
	shrq	%cl, %rax
	andq	$1, %rax
	movl	%eax, -356(%rbp)
	movl	36(%rdx), %eax
	decq	%rax
	movl	%eax, 36(%rdx)
.Ltmp575:
	.loc	1 516 0                 # decompress.c:516:0
	movl	-136(%rbp), %eax
	.loc	1 516 0                 # decompress.c:516:0
.Ltmp576:
	movl	-356(%rbp), %ecx
	movl	%ecx, -140(%rbp)
.Ltmp577:
	.loc	1 516 0                 # decompress.c:516:0
	addq	%rax, %rax
	orq	%rcx, %rax
	jmp	.LBB0_329
.Ltmp578:
.Ltmp579:
	.size	BZ2_decompress, .Ltmp579-BZ2_decompress
.Lfunc_end0:
	.file	3 "./bzlib.h"
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI0_0:
	.quad	.LBB0_4
	.quad	.LBB0_12
	.quad	.LBB0_18
	.quad	.LBB0_24
	.quad	.LBB0_36
	.quad	.LBB0_44
	.quad	.LBB0_50
	.quad	.LBB0_56
	.quad	.LBB0_62
	.quad	.LBB0_68
	.quad	.LBB0_77
	.quad	.LBB0_83
	.quad	.LBB0_89
	.quad	.LBB0_95
	.quad	.LBB0_101
	.quad	.LBB0_107
	.quad	.LBB0_113
	.quad	.LBB0_119
	.quad	.LBB0_128
	.quad	.LBB0_145
	.quad	.LBB0_156
	.quad	.LBB0_163
	.quad	.LBB0_172
	.quad	.LBB0_191
	.quad	.LBB0_200
	.quad	.LBB0_206
	.quad	.LBB0_239
	.quad	.LBB0_245
	.quad	.LBB0_269
	.quad	.LBB0_275
	.quad	.LBB0_326
	.quad	.LBB0_332
	.quad	.LBB0_389
	.quad	.LBB0_395
	.quad	.LBB0_401
	.quad	.LBB0_407
	.quad	.LBB0_413
	.quad	.LBB0_420
	.quad	.LBB0_426
	.quad	.LBB0_432
	.quad	.LBB0_438

	.text
	.align	16, 0x90
	.type	makeMaps_d,@function
makeMaps_d:                             # @makeMaps_d
.Lfunc_begin1:
	.loc	1 68 0                  # decompress.c:68:0
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbp
.Ltmp582:
	.cfi_def_cfa_offset 16
.Ltmp583:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
.Ltmp584:
	.cfi_def_cfa_register %rbp
	movq	%rdi, -8(%rbp)
	xorq	%rax, %rax
	.loc	1 70 0 prologue_end     # decompress.c:70:0
.Ltmp585:
	movl	%eax, 3192(%rdi)
	jmp	.LBB1_1
	.align	16, 0x90
.LBB1_4:                                # %for.inc
                                        #   in Loop: Header=BB1_1 Depth=1
	.loc	1 71 0                  # decompress.c:71:0
.Ltmp586:
	movl	-12(%rbp), %eax
	incq	%rax
.LBB1_1:                                # %for.cond
                                        # =>This Inner Loop Header: Depth=1
	movl	%eax, -12(%rbp)
	movslq	-12(%rbp), %rax
	cmpq	$255, %rax
	jg	.LBB1_5
# BB#2:                                 # %for.body
                                        #   in Loop: Header=BB1_1 Depth=1
	.loc	1 72 0                  # decompress.c:72:0
.Ltmp587:
	movslq	-12(%rbp), %rax
	movq	-8(%rbp), %rcx
	movzbl	3196(%rcx,%rax), %eax
	testq	%rax, %rax
	je	.LBB1_4
# BB#3:                                 # %if.then
                                        #   in Loop: Header=BB1_1 Depth=1
	.loc	1 73 0                  # decompress.c:73:0
.Ltmp588:
	movq	-8(%rbp), %rax
	movzbl	-12(%rbp), %ecx
	movslq	3192(%rax), %rdx
	movb	%cl, 3468(%rax,%rdx)
	.loc	1 74 0                  # decompress.c:74:0
	movq	-8(%rbp), %rax
	movl	3192(%rax), %ecx
	incq	%rcx
	movl	%ecx, 3192(%rax)
	jmp	.LBB1_4
.Ltmp589:
.LBB1_5:                                # %for.end
	.loc	1 76 0                  # decompress.c:76:0
	popq	%rbp
	retq
.Ltmp590:
.Ltmp591:
	.size	makeMaps_d, .Ltmp591-makeMaps_d
.Lfunc_end1:
	.cfi_endproc

	.type	.L.str,@object          # @.str
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.asciz	"\n    [%d: huff+mtf "
	.size	.L.str, 20

	.type	.L.str1,@object         # @.str1
.L.str1:
	.asciz	"rt+rld"
	.size	.L.str1, 7

	.text
.Ldebug_end0:
	.section	.debug_str,"MS",@progbits,1
.Linfo_string0:
	.asciz	"clang version 3.5 (trunk)"
.Linfo_string1:
	.asciz	"decompress.c"
.Linfo_string2:
	.asciz	"/var/services/homes/akale/llvm523/llvm_backend/mytest/bzip2"
.Linfo_string3:
	.asciz	"BZ2_decompress"
.Linfo_string4:
	.asciz	"int"
.Linfo_string5:
	.asciz	"Int32"
.Linfo_string6:
	.asciz	"makeMaps_d"
.Linfo_string7:
	.asciz	"s"
.Linfo_string8:
	.asciz	"strm"
.Linfo_string9:
	.asciz	"next_in"
.Linfo_string10:
	.asciz	"char"
.Linfo_string11:
	.asciz	"avail_in"
.Linfo_string12:
	.asciz	"unsigned int"
.Linfo_string13:
	.asciz	"total_in_lo32"
.Linfo_string14:
	.asciz	"total_in_hi32"
.Linfo_string15:
	.asciz	"next_out"
.Linfo_string16:
	.asciz	"avail_out"
.Linfo_string17:
	.asciz	"total_out_lo32"
.Linfo_string18:
	.asciz	"total_out_hi32"
.Linfo_string19:
	.asciz	"state"
.Linfo_string20:
	.asciz	"bzalloc"
.Linfo_string21:
	.asciz	"bzfree"
.Linfo_string22:
	.asciz	"opaque"
.Linfo_string23:
	.asciz	"bz_stream"
.Linfo_string24:
	.asciz	"state_out_ch"
.Linfo_string25:
	.asciz	"unsigned char"
.Linfo_string26:
	.asciz	"UChar"
.Linfo_string27:
	.asciz	"state_out_len"
.Linfo_string28:
	.asciz	"blockRandomised"
.Linfo_string29:
	.asciz	"Bool"
.Linfo_string30:
	.asciz	"rNToGo"
.Linfo_string31:
	.asciz	"rTPos"
.Linfo_string32:
	.asciz	"bsBuff"
.Linfo_string33:
	.asciz	"UInt32"
.Linfo_string34:
	.asciz	"bsLive"
.Linfo_string35:
	.asciz	"blockSize100k"
.Linfo_string36:
	.asciz	"smallDecompress"
.Linfo_string37:
	.asciz	"currBlockNo"
.Linfo_string38:
	.asciz	"verbosity"
.Linfo_string39:
	.asciz	"origPtr"
.Linfo_string40:
	.asciz	"tPos"
.Linfo_string41:
	.asciz	"k0"
.Linfo_string42:
	.asciz	"unzftab"
.Linfo_string43:
	.asciz	"nblock_used"
.Linfo_string44:
	.asciz	"cftab"
.Linfo_string45:
	.asciz	"cftabCopy"
.Linfo_string46:
	.asciz	"tt"
.Linfo_string47:
	.asciz	"ll16"
.Linfo_string48:
	.asciz	"unsigned short"
.Linfo_string49:
	.asciz	"UInt16"
.Linfo_string50:
	.asciz	"ll4"
.Linfo_string51:
	.asciz	"storedBlockCRC"
.Linfo_string52:
	.asciz	"storedCombinedCRC"
.Linfo_string53:
	.asciz	"calculatedBlockCRC"
.Linfo_string54:
	.asciz	"calculatedCombinedCRC"
.Linfo_string55:
	.asciz	"nInUse"
.Linfo_string56:
	.asciz	"inUse"
.Linfo_string57:
	.asciz	"inUse16"
.Linfo_string58:
	.asciz	"seqToUnseq"
.Linfo_string59:
	.asciz	"mtfa"
.Linfo_string60:
	.asciz	"mtfbase"
.Linfo_string61:
	.asciz	"selector"
.Linfo_string62:
	.asciz	"selectorMtf"
.Linfo_string63:
	.asciz	"len"
.Linfo_string64:
	.asciz	"limit"
.Linfo_string65:
	.asciz	"base"
.Linfo_string66:
	.asciz	"perm"
.Linfo_string67:
	.asciz	"minLens"
.Linfo_string68:
	.asciz	"save_i"
.Linfo_string69:
	.asciz	"save_j"
.Linfo_string70:
	.asciz	"save_t"
.Linfo_string71:
	.asciz	"save_alphaSize"
.Linfo_string72:
	.asciz	"save_nGroups"
.Linfo_string73:
	.asciz	"save_nSelectors"
.Linfo_string74:
	.asciz	"save_EOB"
.Linfo_string75:
	.asciz	"save_groupNo"
.Linfo_string76:
	.asciz	"save_groupPos"
.Linfo_string77:
	.asciz	"save_nextSym"
.Linfo_string78:
	.asciz	"save_nblockMAX"
.Linfo_string79:
	.asciz	"save_nblock"
.Linfo_string80:
	.asciz	"save_es"
.Linfo_string81:
	.asciz	"save_N"
.Linfo_string82:
	.asciz	"save_curr"
.Linfo_string83:
	.asciz	"save_zt"
.Linfo_string84:
	.asciz	"save_zn"
.Linfo_string85:
	.asciz	"save_zvec"
.Linfo_string86:
	.asciz	"save_zj"
.Linfo_string87:
	.asciz	"save_gSel"
.Linfo_string88:
	.asciz	"save_gMinlen"
.Linfo_string89:
	.asciz	"save_gLimit"
.Linfo_string90:
	.asciz	"save_gBase"
.Linfo_string91:
	.asciz	"save_gPerm"
.Linfo_string92:
	.asciz	"DState"
.Linfo_string93:
	.asciz	"uc"
.Linfo_string94:
	.asciz	"retVal"
.Linfo_string95:
	.asciz	"minLen"
.Linfo_string96:
	.asciz	"maxLen"
.Linfo_string97:
	.asciz	"i"
.Linfo_string98:
	.asciz	"j"
.Linfo_string99:
	.asciz	"t"
.Linfo_string100:
	.asciz	"alphaSize"
.Linfo_string101:
	.asciz	"nGroups"
.Linfo_string102:
	.asciz	"nSelectors"
.Linfo_string103:
	.asciz	"EOB"
.Linfo_string104:
	.asciz	"groupNo"
.Linfo_string105:
	.asciz	"groupPos"
.Linfo_string106:
	.asciz	"nextSym"
.Linfo_string107:
	.asciz	"nblockMAX"
.Linfo_string108:
	.asciz	"nblock"
.Linfo_string109:
	.asciz	"es"
.Linfo_string110:
	.asciz	"N"
.Linfo_string111:
	.asciz	"curr"
.Linfo_string112:
	.asciz	"zt"
.Linfo_string113:
	.asciz	"zn"
.Linfo_string114:
	.asciz	"zvec"
.Linfo_string115:
	.asciz	"zj"
.Linfo_string116:
	.asciz	"gSel"
.Linfo_string117:
	.asciz	"gMinlen"
.Linfo_string118:
	.asciz	"gLimit"
.Linfo_string119:
	.asciz	"gBase"
.Linfo_string120:
	.asciz	"gPerm"
.Linfo_string121:
	.asciz	"v"
.Linfo_string122:
	.asciz	"pos"
.Linfo_string123:
	.asciz	"tmp"
.Linfo_string124:
	.asciz	"ii"
.Linfo_string125:
	.asciz	"jj"
.Linfo_string126:
	.asciz	"kk"
.Linfo_string127:
	.asciz	"pp"
.Linfo_string128:
	.asciz	"lno"
.Linfo_string129:
	.asciz	"off"
.Linfo_string130:
	.asciz	"nn"
.Linfo_string131:
	.asciz	"z"
	.section	.debug_info,"",@progbits
.L.debug_info_begin0:
	.long	4632                    # Length of Unit
	.short	4                       # DWARF version number
	.long	.Lsection_abbrev        # Offset Into Abbrev. Section
	.byte	8                       # Address Size (in bytes)
	.byte	1                       # Abbrev [1] 0xb:0x1211 DW_TAG_compile_unit
	.long	.Linfo_string0          # DW_AT_producer
	.short	12                      # DW_AT_language
	.long	.Linfo_string1          # DW_AT_name
	.long	.Lsection_line          # DW_AT_stmt_list
	.long	.Linfo_string2          # DW_AT_comp_dir
	.quad	0                       # DW_AT_low_pc
	.byte	2                       # Abbrev [2] 0x26:0xbff DW_TAG_subprogram
	.long	.Linfo_string3          # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	146                     # DW_AT_decl_line
                                        # DW_AT_prototyped
	.long	3109                    # DW_AT_type
                                        # DW_AT_external
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.quad	.Lfunc_begin0           # DW_AT_low_pc
	.quad	.Lfunc_end0             # DW_AT_high_pc
	.byte	1                       # DW_AT_frame_base
	.byte	86
	.byte	3                       # Abbrev [3] 0x44:0xe DW_TAG_formal_parameter
	.long	.Linfo_string7          # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	146                     # DW_AT_decl_line
	.long	3182                    # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	88
	.byte	4                       # Abbrev [4] 0x52:0xe DW_TAG_variable
	.long	.Linfo_string93         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	148                     # DW_AT_decl_line
	.long	4389                    # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	87
	.byte	4                       # Abbrev [4] 0x60:0xe DW_TAG_variable
	.long	.Linfo_string94         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	149                     # DW_AT_decl_line
	.long	3109                    # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	80
	.byte	4                       # Abbrev [4] 0x6e:0xe DW_TAG_variable
	.long	.Linfo_string95         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	150                     # DW_AT_decl_line
	.long	3109                    # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	76
	.byte	4                       # Abbrev [4] 0x7c:0xe DW_TAG_variable
	.long	.Linfo_string96         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	150                     # DW_AT_decl_line
	.long	3109                    # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	72
	.byte	4                       # Abbrev [4] 0x8a:0xe DW_TAG_variable
	.long	.Linfo_string8          # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	151                     # DW_AT_decl_line
	.long	4149                    # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	64
	.byte	4                       # Abbrev [4] 0x98:0xf DW_TAG_variable
	.long	.Linfo_string97         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	154                     # DW_AT_decl_line
	.long	3109                    # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\274\177"
	.byte	4                       # Abbrev [4] 0xa7:0xf DW_TAG_variable
	.long	.Linfo_string98         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	155                     # DW_AT_decl_line
	.long	3109                    # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\270\177"
	.byte	4                       # Abbrev [4] 0xb6:0xf DW_TAG_variable
	.long	.Linfo_string99         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	156                     # DW_AT_decl_line
	.long	3109                    # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\264\177"
	.byte	4                       # Abbrev [4] 0xc5:0xf DW_TAG_variable
	.long	.Linfo_string100        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	157                     # DW_AT_decl_line
	.long	3109                    # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\260\177"
	.byte	4                       # Abbrev [4] 0xd4:0xf DW_TAG_variable
	.long	.Linfo_string101        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	158                     # DW_AT_decl_line
	.long	3109                    # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\254\177"
	.byte	4                       # Abbrev [4] 0xe3:0xf DW_TAG_variable
	.long	.Linfo_string102        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	159                     # DW_AT_decl_line
	.long	3109                    # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\250\177"
	.byte	4                       # Abbrev [4] 0xf2:0xf DW_TAG_variable
	.long	.Linfo_string103        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	160                     # DW_AT_decl_line
	.long	3109                    # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\244\177"
	.byte	4                       # Abbrev [4] 0x101:0xf DW_TAG_variable
	.long	.Linfo_string104        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	161                     # DW_AT_decl_line
	.long	3109                    # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\240\177"
	.byte	4                       # Abbrev [4] 0x110:0xf DW_TAG_variable
	.long	.Linfo_string105        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	162                     # DW_AT_decl_line
	.long	3109                    # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\234\177"
	.byte	4                       # Abbrev [4] 0x11f:0xf DW_TAG_variable
	.long	.Linfo_string106        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	163                     # DW_AT_decl_line
	.long	3109                    # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\230\177"
	.byte	4                       # Abbrev [4] 0x12e:0xf DW_TAG_variable
	.long	.Linfo_string107        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	164                     # DW_AT_decl_line
	.long	3109                    # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\224\177"
	.byte	4                       # Abbrev [4] 0x13d:0xf DW_TAG_variable
	.long	.Linfo_string108        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	165                     # DW_AT_decl_line
	.long	3109                    # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\220\177"
	.byte	4                       # Abbrev [4] 0x14c:0xf DW_TAG_variable
	.long	.Linfo_string109        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	166                     # DW_AT_decl_line
	.long	3109                    # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\214\177"
	.byte	4                       # Abbrev [4] 0x15b:0xf DW_TAG_variable
	.long	.Linfo_string110        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	167                     # DW_AT_decl_line
	.long	3109                    # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\210\177"
	.byte	4                       # Abbrev [4] 0x16a:0xf DW_TAG_variable
	.long	.Linfo_string111        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	168                     # DW_AT_decl_line
	.long	3109                    # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\204\177"
	.byte	4                       # Abbrev [4] 0x179:0xf DW_TAG_variable
	.long	.Linfo_string112        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	169                     # DW_AT_decl_line
	.long	3109                    # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\200\177"
	.byte	4                       # Abbrev [4] 0x188:0xf DW_TAG_variable
	.long	.Linfo_string113        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	170                     # DW_AT_decl_line
	.long	3109                    # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\374~"
	.byte	4                       # Abbrev [4] 0x197:0xf DW_TAG_variable
	.long	.Linfo_string114        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	171                     # DW_AT_decl_line
	.long	3109                    # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\370~"
	.byte	4                       # Abbrev [4] 0x1a6:0xf DW_TAG_variable
	.long	.Linfo_string115        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	172                     # DW_AT_decl_line
	.long	3109                    # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\364~"
	.byte	4                       # Abbrev [4] 0x1b5:0xf DW_TAG_variable
	.long	.Linfo_string116        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	173                     # DW_AT_decl_line
	.long	3109                    # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\360~"
	.byte	4                       # Abbrev [4] 0x1c4:0xf DW_TAG_variable
	.long	.Linfo_string117        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	174                     # DW_AT_decl_line
	.long	3109                    # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\354~"
	.byte	4                       # Abbrev [4] 0x1d3:0xf DW_TAG_variable
	.long	.Linfo_string118        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	175                     # DW_AT_decl_line
	.long	4618                    # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\340~"
	.byte	4                       # Abbrev [4] 0x1e2:0xf DW_TAG_variable
	.long	.Linfo_string119        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	176                     # DW_AT_decl_line
	.long	4618                    # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\330~"
	.byte	4                       # Abbrev [4] 0x1f1:0xf DW_TAG_variable
	.long	.Linfo_string120        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	177                     # DW_AT_decl_line
	.long	4618                    # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\320~"
	.byte	5                       # Abbrev [5] 0x200:0xa24 DW_TAG_lexical_block
	.long	.Ldebug_ranges94        # DW_AT_ranges
	.byte	5                       # Abbrev [5] 0x205:0x2d DW_TAG_lexical_block
	.long	.Ldebug_ranges1         # DW_AT_ranges
	.byte	5                       # Abbrev [5] 0x20a:0x27 DW_TAG_lexical_block
	.long	.Ldebug_ranges0         # DW_AT_ranges
	.byte	6                       # Abbrev [6] 0x20f:0x21 DW_TAG_lexical_block
	.quad	.Ltmp25                 # DW_AT_low_pc
	.quad	.Ltmp26                 # DW_AT_high_pc
	.byte	4                       # Abbrev [4] 0x220:0xf DW_TAG_variable
	.long	.Linfo_string121        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	237                     # DW_AT_decl_line
	.long	4418                    # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\314~"
	.byte	0                       # End Of Children Mark
	.byte	0                       # End Of Children Mark
	.byte	0                       # End Of Children Mark
	.byte	6                       # Abbrev [6] 0x232:0x39 DW_TAG_lexical_block
	.quad	.Ltmp28                 # DW_AT_low_pc
	.quad	.Ltmp34                 # DW_AT_high_pc
	.byte	5                       # Abbrev [5] 0x243:0x27 DW_TAG_lexical_block
	.long	.Ldebug_ranges2         # DW_AT_ranges
	.byte	6                       # Abbrev [6] 0x248:0x21 DW_TAG_lexical_block
	.quad	.Ltmp33                 # DW_AT_low_pc
	.quad	.Ltmp34                 # DW_AT_high_pc
	.byte	4                       # Abbrev [4] 0x259:0xf DW_TAG_variable
	.long	.Linfo_string121        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	240                     # DW_AT_decl_line
	.long	4418                    # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\310~"
	.byte	0                       # End Of Children Mark
	.byte	0                       # End Of Children Mark
	.byte	0                       # End Of Children Mark
	.byte	6                       # Abbrev [6] 0x26b:0x39 DW_TAG_lexical_block
	.quad	.Ltmp36                 # DW_AT_low_pc
	.quad	.Ltmp42                 # DW_AT_high_pc
	.byte	5                       # Abbrev [5] 0x27c:0x27 DW_TAG_lexical_block
	.long	.Ldebug_ranges3         # DW_AT_ranges
	.byte	6                       # Abbrev [6] 0x281:0x21 DW_TAG_lexical_block
	.quad	.Ltmp41                 # DW_AT_low_pc
	.quad	.Ltmp42                 # DW_AT_high_pc
	.byte	4                       # Abbrev [4] 0x292:0xf DW_TAG_variable
	.long	.Linfo_string121        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	243                     # DW_AT_decl_line
	.long	4418                    # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\304~"
	.byte	0                       # End Of Children Mark
	.byte	0                       # End Of Children Mark
	.byte	0                       # End Of Children Mark
	.byte	6                       # Abbrev [6] 0x2a4:0x39 DW_TAG_lexical_block
	.quad	.Ltmp44                 # DW_AT_low_pc
	.quad	.Ltmp50                 # DW_AT_high_pc
	.byte	5                       # Abbrev [5] 0x2b5:0x27 DW_TAG_lexical_block
	.long	.Ldebug_ranges4         # DW_AT_ranges
	.byte	6                       # Abbrev [6] 0x2ba:0x21 DW_TAG_lexical_block
	.quad	.Ltmp49                 # DW_AT_low_pc
	.quad	.Ltmp50                 # DW_AT_high_pc
	.byte	4                       # Abbrev [4] 0x2cb:0xf DW_TAG_variable
	.long	.Linfo_string121        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	246                     # DW_AT_decl_line
	.long	4418                    # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\300~"
	.byte	0                       # End Of Children Mark
	.byte	0                       # End Of Children Mark
	.byte	0                       # End Of Children Mark
	.byte	6                       # Abbrev [6] 0x2dd:0x3a DW_TAG_lexical_block
	.quad	.Ltmp61                 # DW_AT_low_pc
	.quad	.Ltmp67                 # DW_AT_high_pc
	.byte	5                       # Abbrev [5] 0x2ee:0x28 DW_TAG_lexical_block
	.long	.Ldebug_ranges5         # DW_AT_ranges
	.byte	6                       # Abbrev [6] 0x2f3:0x22 DW_TAG_lexical_block
	.quad	.Ltmp66                 # DW_AT_low_pc
	.quad	.Ltmp67                 # DW_AT_high_pc
	.byte	7                       # Abbrev [7] 0x304:0x10 DW_TAG_variable
	.long	.Linfo_string121        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	262                     # DW_AT_decl_line
	.long	4418                    # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\274~"
	.byte	0                       # End Of Children Mark
	.byte	0                       # End Of Children Mark
	.byte	0                       # End Of Children Mark
	.byte	6                       # Abbrev [6] 0x317:0x3a DW_TAG_lexical_block
	.quad	.Ltmp68                 # DW_AT_low_pc
	.quad	.Ltmp74                 # DW_AT_high_pc
	.byte	5                       # Abbrev [5] 0x328:0x28 DW_TAG_lexical_block
	.long	.Ldebug_ranges6         # DW_AT_ranges
	.byte	6                       # Abbrev [6] 0x32d:0x22 DW_TAG_lexical_block
	.quad	.Ltmp73                 # DW_AT_low_pc
	.quad	.Ltmp74                 # DW_AT_high_pc
	.byte	7                       # Abbrev [7] 0x33e:0x10 DW_TAG_variable
	.long	.Linfo_string121        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	604                     # DW_AT_decl_line
	.long	4418                    # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\224}"
	.byte	0                       # End Of Children Mark
	.byte	0                       # End Of Children Mark
	.byte	0                       # End Of Children Mark
	.byte	6                       # Abbrev [6] 0x351:0x3a DW_TAG_lexical_block
	.quad	.Ltmp76                 # DW_AT_low_pc
	.quad	.Ltmp82                 # DW_AT_high_pc
	.byte	5                       # Abbrev [5] 0x362:0x28 DW_TAG_lexical_block
	.long	.Ldebug_ranges7         # DW_AT_ranges
	.byte	6                       # Abbrev [6] 0x367:0x22 DW_TAG_lexical_block
	.quad	.Ltmp81                 # DW_AT_low_pc
	.quad	.Ltmp82                 # DW_AT_high_pc
	.byte	7                       # Abbrev [7] 0x378:0x10 DW_TAG_variable
	.long	.Linfo_string121        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	606                     # DW_AT_decl_line
	.long	4418                    # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\220}"
	.byte	0                       # End Of Children Mark
	.byte	0                       # End Of Children Mark
	.byte	0                       # End Of Children Mark
	.byte	6                       # Abbrev [6] 0x38b:0x3a DW_TAG_lexical_block
	.quad	.Ltmp84                 # DW_AT_low_pc
	.quad	.Ltmp90                 # DW_AT_high_pc
	.byte	5                       # Abbrev [5] 0x39c:0x28 DW_TAG_lexical_block
	.long	.Ldebug_ranges8         # DW_AT_ranges
	.byte	6                       # Abbrev [6] 0x3a1:0x22 DW_TAG_lexical_block
	.quad	.Ltmp89                 # DW_AT_low_pc
	.quad	.Ltmp90                 # DW_AT_high_pc
	.byte	7                       # Abbrev [7] 0x3b2:0x10 DW_TAG_variable
	.long	.Linfo_string121        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	608                     # DW_AT_decl_line
	.long	4418                    # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\214}"
	.byte	0                       # End Of Children Mark
	.byte	0                       # End Of Children Mark
	.byte	0                       # End Of Children Mark
	.byte	6                       # Abbrev [6] 0x3c5:0x3a DW_TAG_lexical_block
	.quad	.Ltmp92                 # DW_AT_low_pc
	.quad	.Ltmp98                 # DW_AT_high_pc
	.byte	5                       # Abbrev [5] 0x3d6:0x28 DW_TAG_lexical_block
	.long	.Ldebug_ranges9         # DW_AT_ranges
	.byte	6                       # Abbrev [6] 0x3db:0x22 DW_TAG_lexical_block
	.quad	.Ltmp97                 # DW_AT_low_pc
	.quad	.Ltmp98                 # DW_AT_high_pc
	.byte	7                       # Abbrev [7] 0x3ec:0x10 DW_TAG_variable
	.long	.Linfo_string121        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	610                     # DW_AT_decl_line
	.long	4418                    # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\210}"
	.byte	0                       # End Of Children Mark
	.byte	0                       # End Of Children Mark
	.byte	0                       # End Of Children Mark
	.byte	6                       # Abbrev [6] 0x3ff:0x3a DW_TAG_lexical_block
	.quad	.Ltmp100                # DW_AT_low_pc
	.quad	.Ltmp106                # DW_AT_high_pc
	.byte	5                       # Abbrev [5] 0x410:0x28 DW_TAG_lexical_block
	.long	.Ldebug_ranges10        # DW_AT_ranges
	.byte	6                       # Abbrev [6] 0x415:0x22 DW_TAG_lexical_block
	.quad	.Ltmp105                # DW_AT_low_pc
	.quad	.Ltmp106                # DW_AT_high_pc
	.byte	7                       # Abbrev [7] 0x426:0x10 DW_TAG_variable
	.long	.Linfo_string121        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	612                     # DW_AT_decl_line
	.long	4418                    # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\204}"
	.byte	0                       # End Of Children Mark
	.byte	0                       # End Of Children Mark
	.byte	0                       # End Of Children Mark
	.byte	5                       # Abbrev [5] 0x439:0x22 DW_TAG_lexical_block
	.long	.Ldebug_ranges13        # DW_AT_ranges
	.byte	5                       # Abbrev [5] 0x43e:0x1c DW_TAG_lexical_block
	.long	.Ldebug_ranges12        # DW_AT_ranges
	.byte	5                       # Abbrev [5] 0x443:0x16 DW_TAG_lexical_block
	.long	.Ldebug_ranges11        # DW_AT_ranges
	.byte	7                       # Abbrev [7] 0x448:0x10 DW_TAG_variable
	.long	.Linfo_string121        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	616                     # DW_AT_decl_line
	.long	4418                    # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\200}"
	.byte	0                       # End Of Children Mark
	.byte	0                       # End Of Children Mark
	.byte	0                       # End Of Children Mark
	.byte	5                       # Abbrev [5] 0x45b:0x22 DW_TAG_lexical_block
	.long	.Ldebug_ranges16        # DW_AT_ranges
	.byte	5                       # Abbrev [5] 0x460:0x1c DW_TAG_lexical_block
	.long	.Ldebug_ranges15        # DW_AT_ranges
	.byte	5                       # Abbrev [5] 0x465:0x16 DW_TAG_lexical_block
	.long	.Ldebug_ranges14        # DW_AT_ranges
	.byte	7                       # Abbrev [7] 0x46a:0x10 DW_TAG_variable
	.long	.Linfo_string121        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	618                     # DW_AT_decl_line
	.long	4418                    # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\374|"
	.byte	0                       # End Of Children Mark
	.byte	0                       # End Of Children Mark
	.byte	0                       # End Of Children Mark
	.byte	5                       # Abbrev [5] 0x47d:0x22 DW_TAG_lexical_block
	.long	.Ldebug_ranges19        # DW_AT_ranges
	.byte	5                       # Abbrev [5] 0x482:0x1c DW_TAG_lexical_block
	.long	.Ldebug_ranges18        # DW_AT_ranges
	.byte	5                       # Abbrev [5] 0x487:0x16 DW_TAG_lexical_block
	.long	.Ldebug_ranges17        # DW_AT_ranges
	.byte	7                       # Abbrev [7] 0x48c:0x10 DW_TAG_variable
	.long	.Linfo_string121        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	620                     # DW_AT_decl_line
	.long	4418                    # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\370|"
	.byte	0                       # End Of Children Mark
	.byte	0                       # End Of Children Mark
	.byte	0                       # End Of Children Mark
	.byte	5                       # Abbrev [5] 0x49f:0x22 DW_TAG_lexical_block
	.long	.Ldebug_ranges22        # DW_AT_ranges
	.byte	5                       # Abbrev [5] 0x4a4:0x1c DW_TAG_lexical_block
	.long	.Ldebug_ranges21        # DW_AT_ranges
	.byte	5                       # Abbrev [5] 0x4a9:0x16 DW_TAG_lexical_block
	.long	.Ldebug_ranges20        # DW_AT_ranges
	.byte	7                       # Abbrev [7] 0x4ae:0x10 DW_TAG_variable
	.long	.Linfo_string121        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	622                     # DW_AT_decl_line
	.long	4418                    # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\364|"
	.byte	0                       # End Of Children Mark
	.byte	0                       # End Of Children Mark
	.byte	0                       # End Of Children Mark
	.byte	6                       # Abbrev [6] 0x4c1:0x3a DW_TAG_lexical_block
	.quad	.Ltmp150                # DW_AT_low_pc
	.quad	.Ltmp156                # DW_AT_high_pc
	.byte	5                       # Abbrev [5] 0x4d2:0x28 DW_TAG_lexical_block
	.long	.Ldebug_ranges23        # DW_AT_ranges
	.byte	6                       # Abbrev [6] 0x4d7:0x22 DW_TAG_lexical_block
	.quad	.Ltmp155                # DW_AT_low_pc
	.quad	.Ltmp156                # DW_AT_high_pc
	.byte	7                       # Abbrev [7] 0x4e8:0x10 DW_TAG_variable
	.long	.Linfo_string121        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	266                     # DW_AT_decl_line
	.long	4418                    # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\270~"
	.byte	0                       # End Of Children Mark
	.byte	0                       # End Of Children Mark
	.byte	0                       # End Of Children Mark
	.byte	6                       # Abbrev [6] 0x4fb:0x3a DW_TAG_lexical_block
	.quad	.Ltmp158                # DW_AT_low_pc
	.quad	.Ltmp164                # DW_AT_high_pc
	.byte	5                       # Abbrev [5] 0x50c:0x28 DW_TAG_lexical_block
	.long	.Ldebug_ranges24        # DW_AT_ranges
	.byte	6                       # Abbrev [6] 0x511:0x22 DW_TAG_lexical_block
	.quad	.Ltmp163                # DW_AT_low_pc
	.quad	.Ltmp164                # DW_AT_high_pc
	.byte	7                       # Abbrev [7] 0x522:0x10 DW_TAG_variable
	.long	.Linfo_string121        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	268                     # DW_AT_decl_line
	.long	4418                    # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\264~"
	.byte	0                       # End Of Children Mark
	.byte	0                       # End Of Children Mark
	.byte	0                       # End Of Children Mark
	.byte	6                       # Abbrev [6] 0x535:0x3a DW_TAG_lexical_block
	.quad	.Ltmp166                # DW_AT_low_pc
	.quad	.Ltmp172                # DW_AT_high_pc
	.byte	5                       # Abbrev [5] 0x546:0x28 DW_TAG_lexical_block
	.long	.Ldebug_ranges25        # DW_AT_ranges
	.byte	6                       # Abbrev [6] 0x54b:0x22 DW_TAG_lexical_block
	.quad	.Ltmp171                # DW_AT_low_pc
	.quad	.Ltmp172                # DW_AT_high_pc
	.byte	7                       # Abbrev [7] 0x55c:0x10 DW_TAG_variable
	.long	.Linfo_string121        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	270                     # DW_AT_decl_line
	.long	4418                    # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\260~"
	.byte	0                       # End Of Children Mark
	.byte	0                       # End Of Children Mark
	.byte	0                       # End Of Children Mark
	.byte	6                       # Abbrev [6] 0x56f:0x3a DW_TAG_lexical_block
	.quad	.Ltmp174                # DW_AT_low_pc
	.quad	.Ltmp180                # DW_AT_high_pc
	.byte	5                       # Abbrev [5] 0x580:0x28 DW_TAG_lexical_block
	.long	.Ldebug_ranges26        # DW_AT_ranges
	.byte	6                       # Abbrev [6] 0x585:0x22 DW_TAG_lexical_block
	.quad	.Ltmp179                # DW_AT_low_pc
	.quad	.Ltmp180                # DW_AT_high_pc
	.byte	7                       # Abbrev [7] 0x596:0x10 DW_TAG_variable
	.long	.Linfo_string121        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	272                     # DW_AT_decl_line
	.long	4418                    # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\254~"
	.byte	0                       # End Of Children Mark
	.byte	0                       # End Of Children Mark
	.byte	0                       # End Of Children Mark
	.byte	6                       # Abbrev [6] 0x5a9:0x3a DW_TAG_lexical_block
	.quad	.Ltmp182                # DW_AT_low_pc
	.quad	.Ltmp188                # DW_AT_high_pc
	.byte	5                       # Abbrev [5] 0x5ba:0x28 DW_TAG_lexical_block
	.long	.Ldebug_ranges27        # DW_AT_ranges
	.byte	6                       # Abbrev [6] 0x5bf:0x22 DW_TAG_lexical_block
	.quad	.Ltmp187                # DW_AT_low_pc
	.quad	.Ltmp188                # DW_AT_high_pc
	.byte	7                       # Abbrev [7] 0x5d0:0x10 DW_TAG_variable
	.long	.Linfo_string121        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	274                     # DW_AT_decl_line
	.long	4418                    # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\250~"
	.byte	0                       # End Of Children Mark
	.byte	0                       # End Of Children Mark
	.byte	0                       # End Of Children Mark
	.byte	5                       # Abbrev [5] 0x5e3:0x22 DW_TAG_lexical_block
	.long	.Ldebug_ranges30        # DW_AT_ranges
	.byte	5                       # Abbrev [5] 0x5e8:0x1c DW_TAG_lexical_block
	.long	.Ldebug_ranges29        # DW_AT_ranges
	.byte	5                       # Abbrev [5] 0x5ed:0x16 DW_TAG_lexical_block
	.long	.Ldebug_ranges28        # DW_AT_ranges
	.byte	7                       # Abbrev [7] 0x5f2:0x10 DW_TAG_variable
	.long	.Linfo_string121        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	282                     # DW_AT_decl_line
	.long	4418                    # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\244~"
	.byte	0                       # End Of Children Mark
	.byte	0                       # End Of Children Mark
	.byte	0                       # End Of Children Mark
	.byte	5                       # Abbrev [5] 0x605:0x22 DW_TAG_lexical_block
	.long	.Ldebug_ranges33        # DW_AT_ranges
	.byte	5                       # Abbrev [5] 0x60a:0x1c DW_TAG_lexical_block
	.long	.Ldebug_ranges32        # DW_AT_ranges
	.byte	5                       # Abbrev [5] 0x60f:0x16 DW_TAG_lexical_block
	.long	.Ldebug_ranges31        # DW_AT_ranges
	.byte	7                       # Abbrev [7] 0x614:0x10 DW_TAG_variable
	.long	.Linfo_string121        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	284                     # DW_AT_decl_line
	.long	4418                    # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\240~"
	.byte	0                       # End Of Children Mark
	.byte	0                       # End Of Children Mark
	.byte	0                       # End Of Children Mark
	.byte	5                       # Abbrev [5] 0x627:0x22 DW_TAG_lexical_block
	.long	.Ldebug_ranges36        # DW_AT_ranges
	.byte	5                       # Abbrev [5] 0x62c:0x1c DW_TAG_lexical_block
	.long	.Ldebug_ranges35        # DW_AT_ranges
	.byte	5                       # Abbrev [5] 0x631:0x16 DW_TAG_lexical_block
	.long	.Ldebug_ranges34        # DW_AT_ranges
	.byte	7                       # Abbrev [7] 0x636:0x10 DW_TAG_variable
	.long	.Linfo_string121        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	286                     # DW_AT_decl_line
	.long	4418                    # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\234~"
	.byte	0                       # End Of Children Mark
	.byte	0                       # End Of Children Mark
	.byte	0                       # End Of Children Mark
	.byte	5                       # Abbrev [5] 0x649:0x22 DW_TAG_lexical_block
	.long	.Ldebug_ranges39        # DW_AT_ranges
	.byte	5                       # Abbrev [5] 0x64e:0x1c DW_TAG_lexical_block
	.long	.Ldebug_ranges38        # DW_AT_ranges
	.byte	5                       # Abbrev [5] 0x653:0x16 DW_TAG_lexical_block
	.long	.Ldebug_ranges37        # DW_AT_ranges
	.byte	7                       # Abbrev [7] 0x658:0x10 DW_TAG_variable
	.long	.Linfo_string121        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	288                     # DW_AT_decl_line
	.long	4418                    # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\230~"
	.byte	0                       # End Of Children Mark
	.byte	0                       # End Of Children Mark
	.byte	0                       # End Of Children Mark
	.byte	6                       # Abbrev [6] 0x66b:0x3a DW_TAG_lexical_block
	.quad	.Ltmp232                # DW_AT_low_pc
	.quad	.Ltmp238                # DW_AT_high_pc
	.byte	5                       # Abbrev [5] 0x67c:0x28 DW_TAG_lexical_block
	.long	.Ldebug_ranges40        # DW_AT_ranges
	.byte	6                       # Abbrev [6] 0x681:0x22 DW_TAG_lexical_block
	.quad	.Ltmp237                # DW_AT_low_pc
	.quad	.Ltmp238                # DW_AT_high_pc
	.byte	7                       # Abbrev [7] 0x692:0x10 DW_TAG_variable
	.long	.Linfo_string121        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	291                     # DW_AT_decl_line
	.long	4418                    # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\224~"
	.byte	0                       # End Of Children Mark
	.byte	0                       # End Of Children Mark
	.byte	0                       # End Of Children Mark
	.byte	5                       # Abbrev [5] 0x6a5:0x22 DW_TAG_lexical_block
	.long	.Ldebug_ranges43        # DW_AT_ranges
	.byte	5                       # Abbrev [5] 0x6aa:0x1c DW_TAG_lexical_block
	.long	.Ldebug_ranges42        # DW_AT_ranges
	.byte	5                       # Abbrev [5] 0x6af:0x16 DW_TAG_lexical_block
	.long	.Ldebug_ranges41        # DW_AT_ranges
	.byte	7                       # Abbrev [7] 0x6b4:0x10 DW_TAG_variable
	.long	.Linfo_string121        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	294                     # DW_AT_decl_line
	.long	4418                    # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\220~"
	.byte	0                       # End Of Children Mark
	.byte	0                       # End Of Children Mark
	.byte	0                       # End Of Children Mark
	.byte	5                       # Abbrev [5] 0x6c7:0x22 DW_TAG_lexical_block
	.long	.Ldebug_ranges46        # DW_AT_ranges
	.byte	5                       # Abbrev [5] 0x6cc:0x1c DW_TAG_lexical_block
	.long	.Ldebug_ranges45        # DW_AT_ranges
	.byte	5                       # Abbrev [5] 0x6d1:0x16 DW_TAG_lexical_block
	.long	.Ldebug_ranges44        # DW_AT_ranges
	.byte	7                       # Abbrev [7] 0x6d6:0x10 DW_TAG_variable
	.long	.Linfo_string121        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	296                     # DW_AT_decl_line
	.long	4418                    # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\214~"
	.byte	0                       # End Of Children Mark
	.byte	0                       # End Of Children Mark
	.byte	0                       # End Of Children Mark
	.byte	5                       # Abbrev [5] 0x6e9:0x22 DW_TAG_lexical_block
	.long	.Ldebug_ranges49        # DW_AT_ranges
	.byte	5                       # Abbrev [5] 0x6ee:0x1c DW_TAG_lexical_block
	.long	.Ldebug_ranges48        # DW_AT_ranges
	.byte	5                       # Abbrev [5] 0x6f3:0x16 DW_TAG_lexical_block
	.long	.Ldebug_ranges47        # DW_AT_ranges
	.byte	7                       # Abbrev [7] 0x6f8:0x10 DW_TAG_variable
	.long	.Linfo_string121        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	298                     # DW_AT_decl_line
	.long	4418                    # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\210~"
	.byte	0                       # End Of Children Mark
	.byte	0                       # End Of Children Mark
	.byte	0                       # End Of Children Mark
	.byte	6                       # Abbrev [6] 0x70b:0x5e DW_TAG_lexical_block
	.quad	.Ltmp278                # DW_AT_low_pc
	.quad	.Ltmp289                # DW_AT_high_pc
	.byte	6                       # Abbrev [6] 0x71c:0x4c DW_TAG_lexical_block
	.quad	.Ltmp279                # DW_AT_low_pc
	.quad	.Ltmp288                # DW_AT_high_pc
	.byte	6                       # Abbrev [6] 0x72d:0x3a DW_TAG_lexical_block
	.quad	.Ltmp280                # DW_AT_low_pc
	.quad	.Ltmp286                # DW_AT_high_pc
	.byte	5                       # Abbrev [5] 0x73e:0x28 DW_TAG_lexical_block
	.long	.Ldebug_ranges50        # DW_AT_ranges
	.byte	6                       # Abbrev [6] 0x743:0x22 DW_TAG_lexical_block
	.quad	.Ltmp285                # DW_AT_low_pc
	.quad	.Ltmp286                # DW_AT_high_pc
	.byte	7                       # Abbrev [7] 0x754:0x10 DW_TAG_variable
	.long	.Linfo_string121        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	308                     # DW_AT_decl_line
	.long	4418                    # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\204~"
	.byte	0                       # End Of Children Mark
	.byte	0                       # End Of Children Mark
	.byte	0                       # End Of Children Mark
	.byte	0                       # End Of Children Mark
	.byte	0                       # End Of Children Mark
	.byte	5                       # Abbrev [5] 0x769:0x6a DW_TAG_lexical_block
	.long	.Ldebug_ranges53        # DW_AT_ranges
	.byte	5                       # Abbrev [5] 0x76e:0x64 DW_TAG_lexical_block
	.long	.Ldebug_ranges52        # DW_AT_ranges
	.byte	6                       # Abbrev [6] 0x773:0x5e DW_TAG_lexical_block
	.quad	.Ltmp340                # DW_AT_low_pc
	.quad	.Ltmp351                # DW_AT_high_pc
	.byte	6                       # Abbrev [6] 0x784:0x4c DW_TAG_lexical_block
	.quad	.Ltmp341                # DW_AT_low_pc
	.quad	.Ltmp350                # DW_AT_high_pc
	.byte	6                       # Abbrev [6] 0x795:0x3a DW_TAG_lexical_block
	.quad	.Ltmp342                # DW_AT_low_pc
	.quad	.Ltmp348                # DW_AT_high_pc
	.byte	5                       # Abbrev [5] 0x7a6:0x28 DW_TAG_lexical_block
	.long	.Ldebug_ranges51        # DW_AT_ranges
	.byte	6                       # Abbrev [6] 0x7ab:0x22 DW_TAG_lexical_block
	.quad	.Ltmp347                # DW_AT_low_pc
	.quad	.Ltmp348                # DW_AT_high_pc
	.byte	7                       # Abbrev [7] 0x7bc:0x10 DW_TAG_variable
	.long	.Linfo_string121        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	319                     # DW_AT_decl_line
	.long	4418                    # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\200~"
	.byte	0                       # End Of Children Mark
	.byte	0                       # End Of Children Mark
	.byte	0                       # End Of Children Mark
	.byte	0                       # End Of Children Mark
	.byte	0                       # End Of Children Mark
	.byte	0                       # End Of Children Mark
	.byte	0                       # End Of Children Mark
	.byte	6                       # Abbrev [6] 0x7d3:0x3a DW_TAG_lexical_block
	.quad	.Ltmp297                # DW_AT_low_pc
	.quad	.Ltmp303                # DW_AT_high_pc
	.byte	5                       # Abbrev [5] 0x7e4:0x28 DW_TAG_lexical_block
	.long	.Ldebug_ranges54        # DW_AT_ranges
	.byte	6                       # Abbrev [6] 0x7e9:0x22 DW_TAG_lexical_block
	.quad	.Ltmp302                # DW_AT_low_pc
	.quad	.Ltmp303                # DW_AT_high_pc
	.byte	7                       # Abbrev [7] 0x7fa:0x10 DW_TAG_variable
	.long	.Linfo_string121        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	327                     # DW_AT_decl_line
	.long	4418                    # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\374}"
	.byte	0                       # End Of Children Mark
	.byte	0                       # End Of Children Mark
	.byte	0                       # End Of Children Mark
	.byte	6                       # Abbrev [6] 0x80d:0x3a DW_TAG_lexical_block
	.quad	.Ltmp305                # DW_AT_low_pc
	.quad	.Ltmp311                # DW_AT_high_pc
	.byte	5                       # Abbrev [5] 0x81e:0x28 DW_TAG_lexical_block
	.long	.Ldebug_ranges55        # DW_AT_ranges
	.byte	6                       # Abbrev [6] 0x823:0x22 DW_TAG_lexical_block
	.quad	.Ltmp310                # DW_AT_low_pc
	.quad	.Ltmp311                # DW_AT_high_pc
	.byte	7                       # Abbrev [7] 0x834:0x10 DW_TAG_variable
	.long	.Linfo_string121        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	329                     # DW_AT_decl_line
	.long	4418                    # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\370}"
	.byte	0                       # End Of Children Mark
	.byte	0                       # End Of Children Mark
	.byte	0                       # End Of Children Mark
	.byte	5                       # Abbrev [5] 0x847:0x58 DW_TAG_lexical_block
	.long	.Ldebug_ranges58        # DW_AT_ranges
	.byte	5                       # Abbrev [5] 0x84c:0x52 DW_TAG_lexical_block
	.long	.Ldebug_ranges57        # DW_AT_ranges
	.byte	6                       # Abbrev [6] 0x851:0x4c DW_TAG_lexical_block
	.quad	.Ltmp314                # DW_AT_low_pc
	.quad	.Ltmp325                # DW_AT_high_pc
	.byte	6                       # Abbrev [6] 0x862:0x3a DW_TAG_lexical_block
	.quad	.Ltmp315                # DW_AT_low_pc
	.quad	.Ltmp321                # DW_AT_high_pc
	.byte	5                       # Abbrev [5] 0x873:0x28 DW_TAG_lexical_block
	.long	.Ldebug_ranges56        # DW_AT_ranges
	.byte	6                       # Abbrev [6] 0x878:0x22 DW_TAG_lexical_block
	.quad	.Ltmp320                # DW_AT_low_pc
	.quad	.Ltmp321                # DW_AT_high_pc
	.byte	7                       # Abbrev [7] 0x889:0x10 DW_TAG_variable
	.long	.Linfo_string121        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	334                     # DW_AT_decl_line
	.long	4418                    # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\364}"
	.byte	0                       # End Of Children Mark
	.byte	0                       # End Of Children Mark
	.byte	0                       # End Of Children Mark
	.byte	0                       # End Of Children Mark
	.byte	0                       # End Of Children Mark
	.byte	0                       # End Of Children Mark
	.byte	6                       # Abbrev [6] 0x89f:0x42 DW_TAG_lexical_block
	.quad	.Ltmp326                # DW_AT_low_pc
	.quad	.Ltmp337                # DW_AT_high_pc
	.byte	7                       # Abbrev [7] 0x8b0:0x10 DW_TAG_variable
	.long	.Linfo_string122        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	344                     # DW_AT_decl_line
	.long	4623                    # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\356}"
	.byte	7                       # Abbrev [7] 0x8c0:0x10 DW_TAG_variable
	.long	.Linfo_string123        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	344                     # DW_AT_decl_line
	.long	4389                    # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\355}"
	.byte	7                       # Abbrev [7] 0x8d0:0x10 DW_TAG_variable
	.long	.Linfo_string121        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	344                     # DW_AT_decl_line
	.long	4389                    # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\354}"
	.byte	0                       # End Of Children Mark
	.byte	5                       # Abbrev [5] 0x8e1:0xe4 DW_TAG_lexical_block
	.long	.Ldebug_ranges64        # DW_AT_ranges
	.byte	5                       # Abbrev [5] 0x8e6:0xde DW_TAG_lexical_block
	.long	.Ldebug_ranges63        # DW_AT_ranges
	.byte	6                       # Abbrev [6] 0x8eb:0x3a DW_TAG_lexical_block
	.quad	.Ltmp354                # DW_AT_low_pc
	.quad	.Ltmp360                # DW_AT_high_pc
	.byte	5                       # Abbrev [5] 0x8fc:0x28 DW_TAG_lexical_block
	.long	.Ldebug_ranges59        # DW_AT_ranges
	.byte	6                       # Abbrev [6] 0x901:0x22 DW_TAG_lexical_block
	.quad	.Ltmp359                # DW_AT_low_pc
	.quad	.Ltmp360                # DW_AT_high_pc
	.byte	7                       # Abbrev [7] 0x912:0x10 DW_TAG_variable
	.long	.Linfo_string121        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	358                     # DW_AT_decl_line
	.long	4418                    # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\350}"
	.byte	0                       # End Of Children Mark
	.byte	0                       # End Of Children Mark
	.byte	0                       # End Of Children Mark
	.byte	5                       # Abbrev [5] 0x925:0x9e DW_TAG_lexical_block
	.long	.Ldebug_ranges62        # DW_AT_ranges
	.byte	6                       # Abbrev [6] 0x92a:0x98 DW_TAG_lexical_block
	.quad	.Ltmp396                # DW_AT_low_pc
	.quad	.Ltmp415                # DW_AT_high_pc
	.byte	6                       # Abbrev [6] 0x93b:0x86 DW_TAG_lexical_block
	.quad	.Ltmp396                # DW_AT_low_pc
	.quad	.Ltmp414                # DW_AT_high_pc
	.byte	6                       # Abbrev [6] 0x94c:0x3a DW_TAG_lexical_block
	.quad	.Ltmp398                # DW_AT_low_pc
	.quad	.Ltmp404                # DW_AT_high_pc
	.byte	5                       # Abbrev [5] 0x95d:0x28 DW_TAG_lexical_block
	.long	.Ldebug_ranges60        # DW_AT_ranges
	.byte	6                       # Abbrev [6] 0x962:0x22 DW_TAG_lexical_block
	.quad	.Ltmp403                # DW_AT_low_pc
	.quad	.Ltmp404                # DW_AT_high_pc
	.byte	7                       # Abbrev [7] 0x973:0x10 DW_TAG_variable
	.long	.Linfo_string121        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	362                     # DW_AT_decl_line
	.long	4418                    # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\344}"
	.byte	0                       # End Of Children Mark
	.byte	0                       # End Of Children Mark
	.byte	0                       # End Of Children Mark
	.byte	6                       # Abbrev [6] 0x986:0x3a DW_TAG_lexical_block
	.quad	.Ltmp406                # DW_AT_low_pc
	.quad	.Ltmp412                # DW_AT_high_pc
	.byte	5                       # Abbrev [5] 0x997:0x28 DW_TAG_lexical_block
	.long	.Ldebug_ranges61        # DW_AT_ranges
	.byte	6                       # Abbrev [6] 0x99c:0x22 DW_TAG_lexical_block
	.quad	.Ltmp411                # DW_AT_low_pc
	.quad	.Ltmp412                # DW_AT_high_pc
	.byte	7                       # Abbrev [7] 0x9ad:0x10 DW_TAG_variable
	.long	.Linfo_string121        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	364                     # DW_AT_decl_line
	.long	4418                    # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\340}"
	.byte	0                       # End Of Children Mark
	.byte	0                       # End Of Children Mark
	.byte	0                       # End Of Children Mark
	.byte	0                       # End Of Children Mark
	.byte	0                       # End Of Children Mark
	.byte	0                       # End Of Children Mark
	.byte	0                       # End Of Children Mark
	.byte	0                       # End Of Children Mark
	.byte	6                       # Abbrev [6] 0x9c5:0x42 DW_TAG_lexical_block
	.quad	.Ltmp371                # DW_AT_low_pc
	.quad	.Ltmp381                # DW_AT_high_pc
	.byte	7                       # Abbrev [7] 0x9d6:0x10 DW_TAG_variable
	.long	.Linfo_string124        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	400                     # DW_AT_decl_line
	.long	3109                    # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\334}"
	.byte	7                       # Abbrev [7] 0x9e6:0x10 DW_TAG_variable
	.long	.Linfo_string125        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	400                     # DW_AT_decl_line
	.long	3109                    # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\330}"
	.byte	7                       # Abbrev [7] 0x9f6:0x10 DW_TAG_variable
	.long	.Linfo_string126        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	400                     # DW_AT_decl_line
	.long	3109                    # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\324}"
	.byte	0                       # End Of Children Mark
	.byte	5                       # Abbrev [5] 0xa07:0x5c DW_TAG_lexical_block
	.long	.Ldebug_ranges71        # DW_AT_ranges
	.byte	5                       # Abbrev [5] 0xa0c:0x22 DW_TAG_lexical_block
	.long	.Ldebug_ranges67        # DW_AT_ranges
	.byte	5                       # Abbrev [5] 0xa11:0x1c DW_TAG_lexical_block
	.long	.Ldebug_ranges66        # DW_AT_ranges
	.byte	5                       # Abbrev [5] 0xa16:0x16 DW_TAG_lexical_block
	.long	.Ldebug_ranges65        # DW_AT_ranges
	.byte	7                       # Abbrev [7] 0xa1b:0x10 DW_TAG_variable
	.long	.Linfo_string121        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	413                     # DW_AT_decl_line
	.long	4418                    # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\320}"
	.byte	0                       # End Of Children Mark
	.byte	0                       # End Of Children Mark
	.byte	0                       # End Of Children Mark
	.byte	6                       # Abbrev [6] 0xa2e:0x34 DW_TAG_lexical_block
	.quad	.Ltmp417                # DW_AT_low_pc
	.quad	.Ltmp429                # DW_AT_high_pc
	.byte	5                       # Abbrev [5] 0xa3f:0x22 DW_TAG_lexical_block
	.long	.Ldebug_ranges70        # DW_AT_ranges
	.byte	5                       # Abbrev [5] 0xa44:0x1c DW_TAG_lexical_block
	.long	.Ldebug_ranges69        # DW_AT_ranges
	.byte	5                       # Abbrev [5] 0xa49:0x16 DW_TAG_lexical_block
	.long	.Ldebug_ranges68        # DW_AT_ranges
	.byte	7                       # Abbrev [7] 0xa4e:0x10 DW_TAG_variable
	.long	.Linfo_string121        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	413                     # DW_AT_decl_line
	.long	4418                    # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\314}"
	.byte	0                       # End Of Children Mark
	.byte	0                       # End Of Children Mark
	.byte	0                       # End Of Children Mark
	.byte	0                       # End Of Children Mark
	.byte	0                       # End Of Children Mark
	.byte	5                       # Abbrev [5] 0xa63:0x192 DW_TAG_lexical_block
	.long	.Ldebug_ranges91        # DW_AT_ranges
	.byte	5                       # Abbrev [5] 0xa68:0x18c DW_TAG_lexical_block
	.long	.Ldebug_ranges90        # DW_AT_ranges
	.byte	5                       # Abbrev [5] 0xa6d:0x80 DW_TAG_lexical_block
	.long	.Ldebug_ranges78        # DW_AT_ranges
	.byte	6                       # Abbrev [6] 0xa72:0x7a DW_TAG_lexical_block
	.quad	.Ltmp523                # DW_AT_low_pc
	.quad	.Ltmp557                # DW_AT_high_pc
	.byte	5                       # Abbrev [5] 0xa83:0x68 DW_TAG_lexical_block
	.long	.Ldebug_ranges77        # DW_AT_ranges
	.byte	6                       # Abbrev [6] 0xa88:0x3a DW_TAG_lexical_block
	.quad	.Ltmp534                # DW_AT_low_pc
	.quad	.Ltmp541                # DW_AT_high_pc
	.byte	5                       # Abbrev [5] 0xa99:0x28 DW_TAG_lexical_block
	.long	.Ldebug_ranges72        # DW_AT_ranges
	.byte	6                       # Abbrev [6] 0xa9e:0x22 DW_TAG_lexical_block
	.quad	.Ltmp539                # DW_AT_low_pc
	.quad	.Ltmp541                # DW_AT_high_pc
	.byte	7                       # Abbrev [7] 0xaaf:0x10 DW_TAG_variable
	.long	.Linfo_string121        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	427                     # DW_AT_decl_line
	.long	4418                    # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\310}"
	.byte	0                       # End Of Children Mark
	.byte	0                       # End Of Children Mark
	.byte	0                       # End Of Children Mark
	.byte	5                       # Abbrev [5] 0xac2:0x28 DW_TAG_lexical_block
	.long	.Ldebug_ranges76        # DW_AT_ranges
	.byte	5                       # Abbrev [5] 0xac7:0x22 DW_TAG_lexical_block
	.long	.Ldebug_ranges75        # DW_AT_ranges
	.byte	5                       # Abbrev [5] 0xacc:0x1c DW_TAG_lexical_block
	.long	.Ldebug_ranges74        # DW_AT_ranges
	.byte	5                       # Abbrev [5] 0xad1:0x16 DW_TAG_lexical_block
	.long	.Ldebug_ranges73        # DW_AT_ranges
	.byte	7                       # Abbrev [7] 0xad6:0x10 DW_TAG_variable
	.long	.Linfo_string121        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	427                     # DW_AT_decl_line
	.long	4418                    # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\304}"
	.byte	0                       # End Of Children Mark
	.byte	0                       # End Of Children Mark
	.byte	0                       # End Of Children Mark
	.byte	0                       # End Of Children Mark
	.byte	0                       # End Of Children Mark
	.byte	0                       # End Of Children Mark
	.byte	0                       # End Of Children Mark
	.byte	5                       # Abbrev [5] 0xaed:0x106 DW_TAG_lexical_block
	.long	.Ldebug_ranges89        # DW_AT_ranges
	.byte	5                       # Abbrev [5] 0xaf2:0xa4 DW_TAG_lexical_block
	.long	.Ldebug_ranges81        # DW_AT_ranges
	.byte	7                       # Abbrev [7] 0xaf7:0x10 DW_TAG_variable
	.long	.Linfo_string124        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	458                     # DW_AT_decl_line
	.long	3109                    # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\300}"
	.byte	7                       # Abbrev [7] 0xb07:0x10 DW_TAG_variable
	.long	.Linfo_string125        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	458                     # DW_AT_decl_line
	.long	3109                    # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\274}"
	.byte	7                       # Abbrev [7] 0xb17:0x10 DW_TAG_variable
	.long	.Linfo_string126        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	458                     # DW_AT_decl_line
	.long	3109                    # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\270}"
	.byte	7                       # Abbrev [7] 0xb27:0x10 DW_TAG_variable
	.long	.Linfo_string127        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	458                     # DW_AT_decl_line
	.long	3109                    # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\264}"
	.byte	7                       # Abbrev [7] 0xb37:0x10 DW_TAG_variable
	.long	.Linfo_string128        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	458                     # DW_AT_decl_line
	.long	3109                    # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\260}"
	.byte	7                       # Abbrev [7] 0xb47:0x10 DW_TAG_variable
	.long	.Linfo_string129        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	458                     # DW_AT_decl_line
	.long	3109                    # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\254}"
	.byte	7                       # Abbrev [7] 0xb57:0x10 DW_TAG_variable
	.long	.Linfo_string130        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	459                     # DW_AT_decl_line
	.long	4418                    # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\250}"
	.byte	5                       # Abbrev [5] 0xb67:0x2e DW_TAG_lexical_block
	.long	.Ldebug_ranges80        # DW_AT_ranges
	.byte	5                       # Abbrev [5] 0xb6c:0x28 DW_TAG_lexical_block
	.long	.Ldebug_ranges79        # DW_AT_ranges
	.byte	6                       # Abbrev [6] 0xb71:0x22 DW_TAG_lexical_block
	.quad	.Ltmp453                # DW_AT_low_pc
	.quad	.Ltmp454                # DW_AT_high_pc
	.byte	7                       # Abbrev [7] 0xb82:0x10 DW_TAG_variable
	.long	.Linfo_string131        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	467                     # DW_AT_decl_line
	.long	3109                    # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\244}"
	.byte	0                       # End Of Children Mark
	.byte	0                       # End Of Children Mark
	.byte	0                       # End Of Children Mark
	.byte	0                       # End Of Children Mark
	.byte	5                       # Abbrev [5] 0xb96:0x5c DW_TAG_lexical_block
	.long	.Ldebug_ranges88        # DW_AT_ranges
	.byte	5                       # Abbrev [5] 0xb9b:0x22 DW_TAG_lexical_block
	.long	.Ldebug_ranges84        # DW_AT_ranges
	.byte	5                       # Abbrev [5] 0xba0:0x1c DW_TAG_lexical_block
	.long	.Ldebug_ranges83        # DW_AT_ranges
	.byte	5                       # Abbrev [5] 0xba5:0x16 DW_TAG_lexical_block
	.long	.Ldebug_ranges82        # DW_AT_ranges
	.byte	7                       # Abbrev [7] 0xbaa:0x10 DW_TAG_variable
	.long	.Linfo_string121        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	516                     # DW_AT_decl_line
	.long	4418                    # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\240}"
	.byte	0                       # End Of Children Mark
	.byte	0                       # End Of Children Mark
	.byte	0                       # End Of Children Mark
	.byte	6                       # Abbrev [6] 0xbbd:0x34 DW_TAG_lexical_block
	.quad	.Ltmp566                # DW_AT_low_pc
	.quad	.Ltmp578                # DW_AT_high_pc
	.byte	5                       # Abbrev [5] 0xbce:0x22 DW_TAG_lexical_block
	.long	.Ldebug_ranges87        # DW_AT_ranges
	.byte	5                       # Abbrev [5] 0xbd3:0x1c DW_TAG_lexical_block
	.long	.Ldebug_ranges86        # DW_AT_ranges
	.byte	5                       # Abbrev [5] 0xbd8:0x16 DW_TAG_lexical_block
	.long	.Ldebug_ranges85        # DW_AT_ranges
	.byte	7                       # Abbrev [7] 0xbdd:0x10 DW_TAG_variable
	.long	.Linfo_string121        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	516                     # DW_AT_decl_line
	.long	4418                    # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\234}"
	.byte	0                       # End Of Children Mark
	.byte	0                       # End Of Children Mark
	.byte	0                       # End Of Children Mark
	.byte	0                       # End Of Children Mark
	.byte	0                       # End Of Children Mark
	.byte	0                       # End Of Children Mark
	.byte	0                       # End Of Children Mark
	.byte	0                       # End Of Children Mark
	.byte	5                       # Abbrev [5] 0xbf5:0x2e DW_TAG_lexical_block
	.long	.Ldebug_ranges93        # DW_AT_ranges
	.byte	5                       # Abbrev [5] 0xbfa:0x28 DW_TAG_lexical_block
	.long	.Ldebug_ranges92        # DW_AT_ranges
	.byte	6                       # Abbrev [6] 0xbff:0x22 DW_TAG_lexical_block
	.quad	.Ltmp483                # DW_AT_low_pc
	.quad	.Ltmp490                # DW_AT_high_pc
	.byte	7                       # Abbrev [7] 0xc10:0x10 DW_TAG_variable
	.long	.Linfo_string123        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	560                     # DW_AT_decl_line
	.long	3109                    # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\230}"
	.byte	0                       # End Of Children Mark
	.byte	0                       # End Of Children Mark
	.byte	0                       # End Of Children Mark
	.byte	0                       # End Of Children Mark
	.byte	0                       # End Of Children Mark
	.byte	8                       # Abbrev [8] 0xc25:0xb DW_TAG_typedef
	.long	3120                    # DW_AT_type
	.long	.Linfo_string5          # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.byte	84                      # DW_AT_decl_line
	.byte	9                       # Abbrev [9] 0xc30:0x7 DW_TAG_base_type
	.long	.Linfo_string4          # DW_AT_name
	.byte	5                       # DW_AT_encoding
	.byte	4                       # DW_AT_byte_size
	.byte	10                      # Abbrev [10] 0xc37:0x37 DW_TAG_subprogram
	.long	.Linfo_string6          # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	67                      # DW_AT_decl_line
                                        # DW_AT_prototyped
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.quad	.Lfunc_begin1           # DW_AT_low_pc
	.quad	.Lfunc_end1             # DW_AT_high_pc
	.byte	1                       # DW_AT_frame_base
	.byte	86
	.byte	3                       # Abbrev [3] 0xc51:0xe DW_TAG_formal_parameter
	.long	.Linfo_string7          # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	67                      # DW_AT_decl_line
	.long	3182                    # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	120
	.byte	4                       # Abbrev [4] 0xc5f:0xe DW_TAG_variable
	.long	.Linfo_string97         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	69                      # DW_AT_decl_line
	.long	3109                    # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	116
	.byte	0                       # End Of Children Mark
	.byte	11                      # Abbrev [11] 0xc6e:0x5 DW_TAG_pointer_type
	.long	3187                    # DW_AT_type
	.byte	12                      # Abbrev [12] 0xc73:0xc DW_TAG_typedef
	.long	3199                    # DW_AT_type
	.long	.Linfo_string92         # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.short	472                     # DW_AT_decl_line
	.byte	13                      # Abbrev [13] 0xc7f:0x3b6 DW_TAG_structure_type
	.short	64144                   # DW_AT_byte_size
	.byte	2                       # DW_AT_decl_file
	.short	382                     # DW_AT_decl_line
	.byte	14                      # Abbrev [14] 0xc85:0xe DW_TAG_member
	.long	.Linfo_string8          # DW_AT_name
	.long	4149                    # DW_AT_type
	.byte	2                       # DW_AT_decl_file
	.short	384                     # DW_AT_decl_line
	.byte	0                       # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	14                      # Abbrev [14] 0xc93:0xe DW_TAG_member
	.long	.Linfo_string19         # DW_AT_name
	.long	3109                    # DW_AT_type
	.byte	2                       # DW_AT_decl_file
	.short	387                     # DW_AT_decl_line
	.byte	8                       # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	14                      # Abbrev [14] 0xca1:0xe DW_TAG_member
	.long	.Linfo_string24         # DW_AT_name
	.long	4389                    # DW_AT_type
	.byte	2                       # DW_AT_decl_file
	.short	390                     # DW_AT_decl_line
	.byte	12                      # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	14                      # Abbrev [14] 0xcaf:0xe DW_TAG_member
	.long	.Linfo_string27         # DW_AT_name
	.long	3109                    # DW_AT_type
	.byte	2                       # DW_AT_decl_file
	.short	391                     # DW_AT_decl_line
	.byte	16                      # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	14                      # Abbrev [14] 0xcbd:0xe DW_TAG_member
	.long	.Linfo_string28         # DW_AT_name
	.long	4407                    # DW_AT_type
	.byte	2                       # DW_AT_decl_file
	.short	392                     # DW_AT_decl_line
	.byte	20                      # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	14                      # Abbrev [14] 0xccb:0xe DW_TAG_member
	.long	.Linfo_string30         # DW_AT_name
	.long	3109                    # DW_AT_type
	.byte	2                       # DW_AT_decl_file
	.short	393                     # DW_AT_decl_line
	.byte	24                      # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	14                      # Abbrev [14] 0xcd9:0xe DW_TAG_member
	.long	.Linfo_string31         # DW_AT_name
	.long	3109                    # DW_AT_type
	.byte	2                       # DW_AT_decl_file
	.short	393                     # DW_AT_decl_line
	.byte	28                      # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	14                      # Abbrev [14] 0xce7:0xe DW_TAG_member
	.long	.Linfo_string32         # DW_AT_name
	.long	4418                    # DW_AT_type
	.byte	2                       # DW_AT_decl_file
	.short	396                     # DW_AT_decl_line
	.byte	32                      # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	14                      # Abbrev [14] 0xcf5:0xe DW_TAG_member
	.long	.Linfo_string34         # DW_AT_name
	.long	3109                    # DW_AT_type
	.byte	2                       # DW_AT_decl_file
	.short	397                     # DW_AT_decl_line
	.byte	36                      # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	14                      # Abbrev [14] 0xd03:0xe DW_TAG_member
	.long	.Linfo_string35         # DW_AT_name
	.long	3109                    # DW_AT_type
	.byte	2                       # DW_AT_decl_file
	.short	400                     # DW_AT_decl_line
	.byte	40                      # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	14                      # Abbrev [14] 0xd11:0xe DW_TAG_member
	.long	.Linfo_string36         # DW_AT_name
	.long	4407                    # DW_AT_type
	.byte	2                       # DW_AT_decl_file
	.short	401                     # DW_AT_decl_line
	.byte	44                      # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	14                      # Abbrev [14] 0xd1f:0xe DW_TAG_member
	.long	.Linfo_string37         # DW_AT_name
	.long	3109                    # DW_AT_type
	.byte	2                       # DW_AT_decl_file
	.short	402                     # DW_AT_decl_line
	.byte	48                      # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	14                      # Abbrev [14] 0xd2d:0xe DW_TAG_member
	.long	.Linfo_string38         # DW_AT_name
	.long	3109                    # DW_AT_type
	.byte	2                       # DW_AT_decl_file
	.short	403                     # DW_AT_decl_line
	.byte	52                      # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	14                      # Abbrev [14] 0xd3b:0xe DW_TAG_member
	.long	.Linfo_string39         # DW_AT_name
	.long	3109                    # DW_AT_type
	.byte	2                       # DW_AT_decl_file
	.short	406                     # DW_AT_decl_line
	.byte	56                      # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	14                      # Abbrev [14] 0xd49:0xe DW_TAG_member
	.long	.Linfo_string40         # DW_AT_name
	.long	4418                    # DW_AT_type
	.byte	2                       # DW_AT_decl_file
	.short	407                     # DW_AT_decl_line
	.byte	60                      # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	14                      # Abbrev [14] 0xd57:0xe DW_TAG_member
	.long	.Linfo_string41         # DW_AT_name
	.long	3109                    # DW_AT_type
	.byte	2                       # DW_AT_decl_file
	.short	408                     # DW_AT_decl_line
	.byte	64                      # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	14                      # Abbrev [14] 0xd65:0xe DW_TAG_member
	.long	.Linfo_string42         # DW_AT_name
	.long	4429                    # DW_AT_type
	.byte	2                       # DW_AT_decl_file
	.short	409                     # DW_AT_decl_line
	.byte	68                      # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	15                      # Abbrev [15] 0xd73:0xf DW_TAG_member
	.long	.Linfo_string43         # DW_AT_name
	.long	3109                    # DW_AT_type
	.byte	2                       # DW_AT_decl_file
	.short	410                     # DW_AT_decl_line
	.short	1092                    # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	15                      # Abbrev [15] 0xd82:0xf DW_TAG_member
	.long	.Linfo_string44         # DW_AT_name
	.long	4448                    # DW_AT_type
	.byte	2                       # DW_AT_decl_file
	.short	411                     # DW_AT_decl_line
	.short	1096                    # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	15                      # Abbrev [15] 0xd91:0xf DW_TAG_member
	.long	.Linfo_string45         # DW_AT_name
	.long	4448                    # DW_AT_type
	.byte	2                       # DW_AT_decl_file
	.short	412                     # DW_AT_decl_line
	.short	2124                    # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	15                      # Abbrev [15] 0xda0:0xf DW_TAG_member
	.long	.Linfo_string46         # DW_AT_name
	.long	4461                    # DW_AT_type
	.byte	2                       # DW_AT_decl_file
	.short	415                     # DW_AT_decl_line
	.short	3152                    # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	15                      # Abbrev [15] 0xdaf:0xf DW_TAG_member
	.long	.Linfo_string47         # DW_AT_name
	.long	4466                    # DW_AT_type
	.byte	2                       # DW_AT_decl_file
	.short	418                     # DW_AT_decl_line
	.short	3160                    # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	15                      # Abbrev [15] 0xdbe:0xf DW_TAG_member
	.long	.Linfo_string50         # DW_AT_name
	.long	4489                    # DW_AT_type
	.byte	2                       # DW_AT_decl_file
	.short	419                     # DW_AT_decl_line
	.short	3168                    # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	15                      # Abbrev [15] 0xdcd:0xf DW_TAG_member
	.long	.Linfo_string51         # DW_AT_name
	.long	4418                    # DW_AT_type
	.byte	2                       # DW_AT_decl_file
	.short	422                     # DW_AT_decl_line
	.short	3176                    # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	15                      # Abbrev [15] 0xddc:0xf DW_TAG_member
	.long	.Linfo_string52         # DW_AT_name
	.long	4418                    # DW_AT_type
	.byte	2                       # DW_AT_decl_file
	.short	423                     # DW_AT_decl_line
	.short	3180                    # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	15                      # Abbrev [15] 0xdeb:0xf DW_TAG_member
	.long	.Linfo_string53         # DW_AT_name
	.long	4418                    # DW_AT_type
	.byte	2                       # DW_AT_decl_file
	.short	424                     # DW_AT_decl_line
	.short	3184                    # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	15                      # Abbrev [15] 0xdfa:0xf DW_TAG_member
	.long	.Linfo_string54         # DW_AT_name
	.long	4418                    # DW_AT_type
	.byte	2                       # DW_AT_decl_file
	.short	425                     # DW_AT_decl_line
	.short	3188                    # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	15                      # Abbrev [15] 0xe09:0xf DW_TAG_member
	.long	.Linfo_string55         # DW_AT_name
	.long	3109                    # DW_AT_type
	.byte	2                       # DW_AT_decl_file
	.short	428                     # DW_AT_decl_line
	.short	3192                    # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	15                      # Abbrev [15] 0xe18:0xf DW_TAG_member
	.long	.Linfo_string56         # DW_AT_name
	.long	4494                    # DW_AT_type
	.byte	2                       # DW_AT_decl_file
	.short	429                     # DW_AT_decl_line
	.short	3196                    # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	15                      # Abbrev [15] 0xe27:0xf DW_TAG_member
	.long	.Linfo_string57         # DW_AT_name
	.long	4506                    # DW_AT_type
	.byte	2                       # DW_AT_decl_file
	.short	430                     # DW_AT_decl_line
	.short	3452                    # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	15                      # Abbrev [15] 0xe36:0xf DW_TAG_member
	.long	.Linfo_string58         # DW_AT_name
	.long	4518                    # DW_AT_type
	.byte	2                       # DW_AT_decl_file
	.short	431                     # DW_AT_decl_line
	.short	3468                    # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	15                      # Abbrev [15] 0xe45:0xf DW_TAG_member
	.long	.Linfo_string59         # DW_AT_name
	.long	4530                    # DW_AT_type
	.byte	2                       # DW_AT_decl_file
	.short	434                     # DW_AT_decl_line
	.short	3724                    # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	15                      # Abbrev [15] 0xe54:0xf DW_TAG_member
	.long	.Linfo_string60         # DW_AT_name
	.long	4543                    # DW_AT_type
	.byte	2                       # DW_AT_decl_file
	.short	435                     # DW_AT_decl_line
	.short	7820                    # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	15                      # Abbrev [15] 0xe63:0xf DW_TAG_member
	.long	.Linfo_string61         # DW_AT_name
	.long	4555                    # DW_AT_type
	.byte	2                       # DW_AT_decl_file
	.short	436                     # DW_AT_decl_line
	.short	7884                    # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	15                      # Abbrev [15] 0xe72:0xf DW_TAG_member
	.long	.Linfo_string62         # DW_AT_name
	.long	4555                    # DW_AT_type
	.byte	2                       # DW_AT_decl_file
	.short	437                     # DW_AT_decl_line
	.short	25886                   # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	15                      # Abbrev [15] 0xe81:0xf DW_TAG_member
	.long	.Linfo_string63         # DW_AT_name
	.long	4568                    # DW_AT_type
	.byte	2                       # DW_AT_decl_file
	.short	438                     # DW_AT_decl_line
	.short	43888                   # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	15                      # Abbrev [15] 0xe90:0xf DW_TAG_member
	.long	.Linfo_string64         # DW_AT_name
	.long	4587                    # DW_AT_type
	.byte	2                       # DW_AT_decl_file
	.short	440                     # DW_AT_decl_line
	.short	45436                   # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	15                      # Abbrev [15] 0xe9f:0xf DW_TAG_member
	.long	.Linfo_string65         # DW_AT_name
	.long	4587                    # DW_AT_type
	.byte	2                       # DW_AT_decl_file
	.short	441                     # DW_AT_decl_line
	.short	51628                   # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	15                      # Abbrev [15] 0xeae:0xf DW_TAG_member
	.long	.Linfo_string66         # DW_AT_name
	.long	4587                    # DW_AT_type
	.byte	2                       # DW_AT_decl_file
	.short	442                     # DW_AT_decl_line
	.short	57820                   # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	15                      # Abbrev [15] 0xebd:0xf DW_TAG_member
	.long	.Linfo_string67         # DW_AT_name
	.long	4606                    # DW_AT_type
	.byte	2                       # DW_AT_decl_file
	.short	443                     # DW_AT_decl_line
	.short	64012                   # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	15                      # Abbrev [15] 0xecc:0xf DW_TAG_member
	.long	.Linfo_string68         # DW_AT_name
	.long	3109                    # DW_AT_type
	.byte	2                       # DW_AT_decl_file
	.short	446                     # DW_AT_decl_line
	.short	64036                   # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	15                      # Abbrev [15] 0xedb:0xf DW_TAG_member
	.long	.Linfo_string69         # DW_AT_name
	.long	3109                    # DW_AT_type
	.byte	2                       # DW_AT_decl_file
	.short	447                     # DW_AT_decl_line
	.short	64040                   # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	15                      # Abbrev [15] 0xeea:0xf DW_TAG_member
	.long	.Linfo_string70         # DW_AT_name
	.long	3109                    # DW_AT_type
	.byte	2                       # DW_AT_decl_file
	.short	448                     # DW_AT_decl_line
	.short	64044                   # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	15                      # Abbrev [15] 0xef9:0xf DW_TAG_member
	.long	.Linfo_string71         # DW_AT_name
	.long	3109                    # DW_AT_type
	.byte	2                       # DW_AT_decl_file
	.short	449                     # DW_AT_decl_line
	.short	64048                   # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	15                      # Abbrev [15] 0xf08:0xf DW_TAG_member
	.long	.Linfo_string72         # DW_AT_name
	.long	3109                    # DW_AT_type
	.byte	2                       # DW_AT_decl_file
	.short	450                     # DW_AT_decl_line
	.short	64052                   # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	15                      # Abbrev [15] 0xf17:0xf DW_TAG_member
	.long	.Linfo_string73         # DW_AT_name
	.long	3109                    # DW_AT_type
	.byte	2                       # DW_AT_decl_file
	.short	451                     # DW_AT_decl_line
	.short	64056                   # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	15                      # Abbrev [15] 0xf26:0xf DW_TAG_member
	.long	.Linfo_string74         # DW_AT_name
	.long	3109                    # DW_AT_type
	.byte	2                       # DW_AT_decl_file
	.short	452                     # DW_AT_decl_line
	.short	64060                   # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	15                      # Abbrev [15] 0xf35:0xf DW_TAG_member
	.long	.Linfo_string75         # DW_AT_name
	.long	3109                    # DW_AT_type
	.byte	2                       # DW_AT_decl_file
	.short	453                     # DW_AT_decl_line
	.short	64064                   # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	15                      # Abbrev [15] 0xf44:0xf DW_TAG_member
	.long	.Linfo_string76         # DW_AT_name
	.long	3109                    # DW_AT_type
	.byte	2                       # DW_AT_decl_file
	.short	454                     # DW_AT_decl_line
	.short	64068                   # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	15                      # Abbrev [15] 0xf53:0xf DW_TAG_member
	.long	.Linfo_string77         # DW_AT_name
	.long	3109                    # DW_AT_type
	.byte	2                       # DW_AT_decl_file
	.short	455                     # DW_AT_decl_line
	.short	64072                   # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	15                      # Abbrev [15] 0xf62:0xf DW_TAG_member
	.long	.Linfo_string78         # DW_AT_name
	.long	3109                    # DW_AT_type
	.byte	2                       # DW_AT_decl_file
	.short	456                     # DW_AT_decl_line
	.short	64076                   # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	15                      # Abbrev [15] 0xf71:0xf DW_TAG_member
	.long	.Linfo_string79         # DW_AT_name
	.long	3109                    # DW_AT_type
	.byte	2                       # DW_AT_decl_file
	.short	457                     # DW_AT_decl_line
	.short	64080                   # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	15                      # Abbrev [15] 0xf80:0xf DW_TAG_member
	.long	.Linfo_string80         # DW_AT_name
	.long	3109                    # DW_AT_type
	.byte	2                       # DW_AT_decl_file
	.short	458                     # DW_AT_decl_line
	.short	64084                   # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	15                      # Abbrev [15] 0xf8f:0xf DW_TAG_member
	.long	.Linfo_string81         # DW_AT_name
	.long	3109                    # DW_AT_type
	.byte	2                       # DW_AT_decl_file
	.short	459                     # DW_AT_decl_line
	.short	64088                   # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	15                      # Abbrev [15] 0xf9e:0xf DW_TAG_member
	.long	.Linfo_string82         # DW_AT_name
	.long	3109                    # DW_AT_type
	.byte	2                       # DW_AT_decl_file
	.short	460                     # DW_AT_decl_line
	.short	64092                   # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	15                      # Abbrev [15] 0xfad:0xf DW_TAG_member
	.long	.Linfo_string83         # DW_AT_name
	.long	3109                    # DW_AT_type
	.byte	2                       # DW_AT_decl_file
	.short	461                     # DW_AT_decl_line
	.short	64096                   # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	15                      # Abbrev [15] 0xfbc:0xf DW_TAG_member
	.long	.Linfo_string84         # DW_AT_name
	.long	3109                    # DW_AT_type
	.byte	2                       # DW_AT_decl_file
	.short	462                     # DW_AT_decl_line
	.short	64100                   # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	15                      # Abbrev [15] 0xfcb:0xf DW_TAG_member
	.long	.Linfo_string85         # DW_AT_name
	.long	3109                    # DW_AT_type
	.byte	2                       # DW_AT_decl_file
	.short	463                     # DW_AT_decl_line
	.short	64104                   # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	15                      # Abbrev [15] 0xfda:0xf DW_TAG_member
	.long	.Linfo_string86         # DW_AT_name
	.long	3109                    # DW_AT_type
	.byte	2                       # DW_AT_decl_file
	.short	464                     # DW_AT_decl_line
	.short	64108                   # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	15                      # Abbrev [15] 0xfe9:0xf DW_TAG_member
	.long	.Linfo_string87         # DW_AT_name
	.long	3109                    # DW_AT_type
	.byte	2                       # DW_AT_decl_file
	.short	465                     # DW_AT_decl_line
	.short	64112                   # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	15                      # Abbrev [15] 0xff8:0xf DW_TAG_member
	.long	.Linfo_string88         # DW_AT_name
	.long	3109                    # DW_AT_type
	.byte	2                       # DW_AT_decl_file
	.short	466                     # DW_AT_decl_line
	.short	64116                   # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	15                      # Abbrev [15] 0x1007:0xf DW_TAG_member
	.long	.Linfo_string89         # DW_AT_name
	.long	4618                    # DW_AT_type
	.byte	2                       # DW_AT_decl_file
	.short	467                     # DW_AT_decl_line
	.short	64120                   # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	15                      # Abbrev [15] 0x1016:0xf DW_TAG_member
	.long	.Linfo_string90         # DW_AT_name
	.long	4618                    # DW_AT_type
	.byte	2                       # DW_AT_decl_file
	.short	468                     # DW_AT_decl_line
	.short	64128                   # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	15                      # Abbrev [15] 0x1025:0xf DW_TAG_member
	.long	.Linfo_string91         # DW_AT_name
	.long	4618                    # DW_AT_type
	.byte	2                       # DW_AT_decl_file
	.short	469                     # DW_AT_decl_line
	.short	64136                   # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	0                       # End Of Children Mark
	.byte	11                      # Abbrev [11] 0x1035:0x5 DW_TAG_pointer_type
	.long	4154                    # DW_AT_type
	.byte	8                       # Abbrev [8] 0x103a:0xb DW_TAG_typedef
	.long	4165                    # DW_AT_type
	.long	.Linfo_string23         # DW_AT_name
	.byte	3                       # DW_AT_decl_file
	.byte	221                     # DW_AT_decl_line
	.byte	16                      # Abbrev [16] 0x1045:0xa1 DW_TAG_structure_type
	.byte	80                      # DW_AT_byte_size
	.byte	3                       # DW_AT_decl_file
	.byte	204                     # DW_AT_decl_line
	.byte	17                      # Abbrev [17] 0x1049:0xd DW_TAG_member
	.long	.Linfo_string9          # DW_AT_name
	.long	4326                    # DW_AT_type
	.byte	3                       # DW_AT_decl_file
	.byte	205                     # DW_AT_decl_line
	.byte	0                       # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	17                      # Abbrev [17] 0x1056:0xd DW_TAG_member
	.long	.Linfo_string11         # DW_AT_name
	.long	4338                    # DW_AT_type
	.byte	3                       # DW_AT_decl_file
	.byte	206                     # DW_AT_decl_line
	.byte	8                       # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	17                      # Abbrev [17] 0x1063:0xd DW_TAG_member
	.long	.Linfo_string13         # DW_AT_name
	.long	4338                    # DW_AT_type
	.byte	3                       # DW_AT_decl_file
	.byte	207                     # DW_AT_decl_line
	.byte	12                      # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	17                      # Abbrev [17] 0x1070:0xd DW_TAG_member
	.long	.Linfo_string14         # DW_AT_name
	.long	4338                    # DW_AT_type
	.byte	3                       # DW_AT_decl_file
	.byte	208                     # DW_AT_decl_line
	.byte	16                      # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	17                      # Abbrev [17] 0x107d:0xd DW_TAG_member
	.long	.Linfo_string15         # DW_AT_name
	.long	4326                    # DW_AT_type
	.byte	3                       # DW_AT_decl_file
	.byte	210                     # DW_AT_decl_line
	.byte	24                      # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	17                      # Abbrev [17] 0x108a:0xd DW_TAG_member
	.long	.Linfo_string16         # DW_AT_name
	.long	4338                    # DW_AT_type
	.byte	3                       # DW_AT_decl_file
	.byte	211                     # DW_AT_decl_line
	.byte	32                      # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	17                      # Abbrev [17] 0x1097:0xd DW_TAG_member
	.long	.Linfo_string17         # DW_AT_name
	.long	4338                    # DW_AT_type
	.byte	3                       # DW_AT_decl_file
	.byte	212                     # DW_AT_decl_line
	.byte	36                      # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	17                      # Abbrev [17] 0x10a4:0xd DW_TAG_member
	.long	.Linfo_string18         # DW_AT_name
	.long	4338                    # DW_AT_type
	.byte	3                       # DW_AT_decl_file
	.byte	213                     # DW_AT_decl_line
	.byte	40                      # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	17                      # Abbrev [17] 0x10b1:0xd DW_TAG_member
	.long	.Linfo_string19         # DW_AT_name
	.long	4345                    # DW_AT_type
	.byte	3                       # DW_AT_decl_file
	.byte	215                     # DW_AT_decl_line
	.byte	48                      # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	17                      # Abbrev [17] 0x10be:0xd DW_TAG_member
	.long	.Linfo_string20         # DW_AT_name
	.long	4346                    # DW_AT_type
	.byte	3                       # DW_AT_decl_file
	.byte	217                     # DW_AT_decl_line
	.byte	56                      # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	17                      # Abbrev [17] 0x10cb:0xd DW_TAG_member
	.long	.Linfo_string21         # DW_AT_name
	.long	4372                    # DW_AT_type
	.byte	3                       # DW_AT_decl_file
	.byte	218                     # DW_AT_decl_line
	.byte	64                      # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	17                      # Abbrev [17] 0x10d8:0xd DW_TAG_member
	.long	.Linfo_string22         # DW_AT_name
	.long	4345                    # DW_AT_type
	.byte	3                       # DW_AT_decl_file
	.byte	219                     # DW_AT_decl_line
	.byte	72                      # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	0                       # End Of Children Mark
	.byte	11                      # Abbrev [11] 0x10e6:0x5 DW_TAG_pointer_type
	.long	4331                    # DW_AT_type
	.byte	9                       # Abbrev [9] 0x10eb:0x7 DW_TAG_base_type
	.long	.Linfo_string10         # DW_AT_name
	.byte	6                       # DW_AT_encoding
	.byte	1                       # DW_AT_byte_size
	.byte	9                       # Abbrev [9] 0x10f2:0x7 DW_TAG_base_type
	.long	.Linfo_string12         # DW_AT_name
	.byte	7                       # DW_AT_encoding
	.byte	4                       # DW_AT_byte_size
	.byte	18                      # Abbrev [18] 0x10f9:0x1 DW_TAG_pointer_type
	.byte	11                      # Abbrev [11] 0x10fa:0x5 DW_TAG_pointer_type
	.long	4351                    # DW_AT_type
	.byte	19                      # Abbrev [19] 0x10ff:0x15 DW_TAG_subroutine_type
	.long	4345                    # DW_AT_type
                                        # DW_AT_prototyped
	.byte	20                      # Abbrev [20] 0x1104:0x5 DW_TAG_formal_parameter
	.long	4345                    # DW_AT_type
	.byte	20                      # Abbrev [20] 0x1109:0x5 DW_TAG_formal_parameter
	.long	3120                    # DW_AT_type
	.byte	20                      # Abbrev [20] 0x110e:0x5 DW_TAG_formal_parameter
	.long	3120                    # DW_AT_type
	.byte	0                       # End Of Children Mark
	.byte	11                      # Abbrev [11] 0x1114:0x5 DW_TAG_pointer_type
	.long	4377                    # DW_AT_type
	.byte	21                      # Abbrev [21] 0x1119:0xc DW_TAG_subroutine_type
                                        # DW_AT_prototyped
	.byte	20                      # Abbrev [20] 0x111a:0x5 DW_TAG_formal_parameter
	.long	4345                    # DW_AT_type
	.byte	20                      # Abbrev [20] 0x111f:0x5 DW_TAG_formal_parameter
	.long	4345                    # DW_AT_type
	.byte	0                       # End Of Children Mark
	.byte	8                       # Abbrev [8] 0x1125:0xb DW_TAG_typedef
	.long	4400                    # DW_AT_type
	.long	.Linfo_string26         # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.byte	83                      # DW_AT_decl_line
	.byte	9                       # Abbrev [9] 0x1130:0x7 DW_TAG_base_type
	.long	.Linfo_string25         # DW_AT_name
	.byte	8                       # DW_AT_encoding
	.byte	1                       # DW_AT_byte_size
	.byte	8                       # Abbrev [8] 0x1137:0xb DW_TAG_typedef
	.long	4400                    # DW_AT_type
	.long	.Linfo_string29         # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.byte	82                      # DW_AT_decl_line
	.byte	8                       # Abbrev [8] 0x1142:0xb DW_TAG_typedef
	.long	4338                    # DW_AT_type
	.long	.Linfo_string33         # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.byte	85                      # DW_AT_decl_line
	.byte	22                      # Abbrev [22] 0x114d:0xc DW_TAG_array_type
	.long	3109                    # DW_AT_type
	.byte	23                      # Abbrev [23] 0x1152:0x6 DW_TAG_subrange_type
	.long	4441                    # DW_AT_type
	.byte	255                     # DW_AT_upper_bound
	.byte	0                       # End Of Children Mark
	.byte	24                      # Abbrev [24] 0x1159:0x7 DW_TAG_base_type
	.long	.Linfo_string4          # DW_AT_name
	.byte	4                       # DW_AT_byte_size
	.byte	5                       # DW_AT_encoding
	.byte	22                      # Abbrev [22] 0x1160:0xd DW_TAG_array_type
	.long	3109                    # DW_AT_type
	.byte	25                      # Abbrev [25] 0x1165:0x7 DW_TAG_subrange_type
	.long	4441                    # DW_AT_type
	.short	256                     # DW_AT_upper_bound
	.byte	0                       # End Of Children Mark
	.byte	11                      # Abbrev [11] 0x116d:0x5 DW_TAG_pointer_type
	.long	4418                    # DW_AT_type
	.byte	11                      # Abbrev [11] 0x1172:0x5 DW_TAG_pointer_type
	.long	4471                    # DW_AT_type
	.byte	8                       # Abbrev [8] 0x1177:0xb DW_TAG_typedef
	.long	4482                    # DW_AT_type
	.long	.Linfo_string49         # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.byte	87                      # DW_AT_decl_line
	.byte	9                       # Abbrev [9] 0x1182:0x7 DW_TAG_base_type
	.long	.Linfo_string48         # DW_AT_name
	.byte	7                       # DW_AT_encoding
	.byte	2                       # DW_AT_byte_size
	.byte	11                      # Abbrev [11] 0x1189:0x5 DW_TAG_pointer_type
	.long	4389                    # DW_AT_type
	.byte	22                      # Abbrev [22] 0x118e:0xc DW_TAG_array_type
	.long	4407                    # DW_AT_type
	.byte	23                      # Abbrev [23] 0x1193:0x6 DW_TAG_subrange_type
	.long	4441                    # DW_AT_type
	.byte	255                     # DW_AT_upper_bound
	.byte	0                       # End Of Children Mark
	.byte	22                      # Abbrev [22] 0x119a:0xc DW_TAG_array_type
	.long	4407                    # DW_AT_type
	.byte	23                      # Abbrev [23] 0x119f:0x6 DW_TAG_subrange_type
	.long	4441                    # DW_AT_type
	.byte	15                      # DW_AT_upper_bound
	.byte	0                       # End Of Children Mark
	.byte	22                      # Abbrev [22] 0x11a6:0xc DW_TAG_array_type
	.long	4389                    # DW_AT_type
	.byte	23                      # Abbrev [23] 0x11ab:0x6 DW_TAG_subrange_type
	.long	4441                    # DW_AT_type
	.byte	255                     # DW_AT_upper_bound
	.byte	0                       # End Of Children Mark
	.byte	22                      # Abbrev [22] 0x11b2:0xd DW_TAG_array_type
	.long	4389                    # DW_AT_type
	.byte	25                      # Abbrev [25] 0x11b7:0x7 DW_TAG_subrange_type
	.long	4441                    # DW_AT_type
	.short	4095                    # DW_AT_upper_bound
	.byte	0                       # End Of Children Mark
	.byte	22                      # Abbrev [22] 0x11bf:0xc DW_TAG_array_type
	.long	3109                    # DW_AT_type
	.byte	23                      # Abbrev [23] 0x11c4:0x6 DW_TAG_subrange_type
	.long	4441                    # DW_AT_type
	.byte	15                      # DW_AT_upper_bound
	.byte	0                       # End Of Children Mark
	.byte	22                      # Abbrev [22] 0x11cb:0xd DW_TAG_array_type
	.long	4389                    # DW_AT_type
	.byte	25                      # Abbrev [25] 0x11d0:0x7 DW_TAG_subrange_type
	.long	4441                    # DW_AT_type
	.short	18001                   # DW_AT_upper_bound
	.byte	0                       # End Of Children Mark
	.byte	22                      # Abbrev [22] 0x11d8:0x13 DW_TAG_array_type
	.long	4389                    # DW_AT_type
	.byte	23                      # Abbrev [23] 0x11dd:0x6 DW_TAG_subrange_type
	.long	4441                    # DW_AT_type
	.byte	5                       # DW_AT_upper_bound
	.byte	25                      # Abbrev [25] 0x11e3:0x7 DW_TAG_subrange_type
	.long	4441                    # DW_AT_type
	.short	257                     # DW_AT_upper_bound
	.byte	0                       # End Of Children Mark
	.byte	22                      # Abbrev [22] 0x11eb:0x13 DW_TAG_array_type
	.long	3109                    # DW_AT_type
	.byte	23                      # Abbrev [23] 0x11f0:0x6 DW_TAG_subrange_type
	.long	4441                    # DW_AT_type
	.byte	5                       # DW_AT_upper_bound
	.byte	25                      # Abbrev [25] 0x11f6:0x7 DW_TAG_subrange_type
	.long	4441                    # DW_AT_type
	.short	257                     # DW_AT_upper_bound
	.byte	0                       # End Of Children Mark
	.byte	22                      # Abbrev [22] 0x11fe:0xc DW_TAG_array_type
	.long	3109                    # DW_AT_type
	.byte	23                      # Abbrev [23] 0x1203:0x6 DW_TAG_subrange_type
	.long	4441                    # DW_AT_type
	.byte	5                       # DW_AT_upper_bound
	.byte	0                       # End Of Children Mark
	.byte	11                      # Abbrev [11] 0x120a:0x5 DW_TAG_pointer_type
	.long	3109                    # DW_AT_type
	.byte	22                      # Abbrev [22] 0x120f:0xc DW_TAG_array_type
	.long	4389                    # DW_AT_type
	.byte	23                      # Abbrev [23] 0x1214:0x6 DW_TAG_subrange_type
	.long	4441                    # DW_AT_type
	.byte	5                       # DW_AT_upper_bound
	.byte	0                       # End Of Children Mark
	.byte	0                       # End Of Children Mark
.L.debug_info_end0:
	.section	.debug_abbrev,"",@progbits
	.byte	1                       # Abbreviation Code
	.byte	17                      # DW_TAG_compile_unit
	.byte	1                       # DW_CHILDREN_yes
	.byte	37                      # DW_AT_producer
	.byte	14                      # DW_FORM_strp
	.byte	19                      # DW_AT_language
	.byte	5                       # DW_FORM_data2
	.byte	3                       # DW_AT_name
	.byte	14                      # DW_FORM_strp
	.byte	16                      # DW_AT_stmt_list
	.byte	23                      # DW_FORM_sec_offset
	.byte	27                      # DW_AT_comp_dir
	.byte	14                      # DW_FORM_strp
	.byte	17                      # DW_AT_low_pc
	.byte	1                       # DW_FORM_addr
	.byte	0                       # EOM(1)
	.byte	0                       # EOM(2)
	.byte	2                       # Abbreviation Code
	.byte	46                      # DW_TAG_subprogram
	.byte	1                       # DW_CHILDREN_yes
	.byte	3                       # DW_AT_name
	.byte	14                      # DW_FORM_strp
	.byte	58                      # DW_AT_decl_file
	.byte	11                      # DW_FORM_data1
	.byte	59                      # DW_AT_decl_line
	.byte	11                      # DW_FORM_data1
	.byte	39                      # DW_AT_prototyped
	.byte	25                      # DW_FORM_flag_present
	.byte	73                      # DW_AT_type
	.byte	19                      # DW_FORM_ref4
	.byte	63                      # DW_AT_external
	.byte	25                      # DW_FORM_flag_present
	.byte	50                      # DW_AT_accessibility
	.byte	11                      # DW_FORM_data1
	.byte	17                      # DW_AT_low_pc
	.byte	1                       # DW_FORM_addr
	.byte	18                      # DW_AT_high_pc
	.byte	1                       # DW_FORM_addr
	.byte	64                      # DW_AT_frame_base
	.byte	24                      # DW_FORM_exprloc
	.byte	0                       # EOM(1)
	.byte	0                       # EOM(2)
	.byte	3                       # Abbreviation Code
	.byte	5                       # DW_TAG_formal_parameter
	.byte	0                       # DW_CHILDREN_no
	.byte	3                       # DW_AT_name
	.byte	14                      # DW_FORM_strp
	.byte	58                      # DW_AT_decl_file
	.byte	11                      # DW_FORM_data1
	.byte	59                      # DW_AT_decl_line
	.byte	11                      # DW_FORM_data1
	.byte	73                      # DW_AT_type
	.byte	19                      # DW_FORM_ref4
	.byte	2                       # DW_AT_location
	.byte	24                      # DW_FORM_exprloc
	.byte	0                       # EOM(1)
	.byte	0                       # EOM(2)
	.byte	4                       # Abbreviation Code
	.byte	52                      # DW_TAG_variable
	.byte	0                       # DW_CHILDREN_no
	.byte	3                       # DW_AT_name
	.byte	14                      # DW_FORM_strp
	.byte	58                      # DW_AT_decl_file
	.byte	11                      # DW_FORM_data1
	.byte	59                      # DW_AT_decl_line
	.byte	11                      # DW_FORM_data1
	.byte	73                      # DW_AT_type
	.byte	19                      # DW_FORM_ref4
	.byte	2                       # DW_AT_location
	.byte	24                      # DW_FORM_exprloc
	.byte	0                       # EOM(1)
	.byte	0                       # EOM(2)
	.byte	5                       # Abbreviation Code
	.byte	11                      # DW_TAG_lexical_block
	.byte	1                       # DW_CHILDREN_yes
	.byte	85                      # DW_AT_ranges
	.byte	23                      # DW_FORM_sec_offset
	.byte	0                       # EOM(1)
	.byte	0                       # EOM(2)
	.byte	6                       # Abbreviation Code
	.byte	11                      # DW_TAG_lexical_block
	.byte	1                       # DW_CHILDREN_yes
	.byte	17                      # DW_AT_low_pc
	.byte	1                       # DW_FORM_addr
	.byte	18                      # DW_AT_high_pc
	.byte	1                       # DW_FORM_addr
	.byte	0                       # EOM(1)
	.byte	0                       # EOM(2)
	.byte	7                       # Abbreviation Code
	.byte	52                      # DW_TAG_variable
	.byte	0                       # DW_CHILDREN_no
	.byte	3                       # DW_AT_name
	.byte	14                      # DW_FORM_strp
	.byte	58                      # DW_AT_decl_file
	.byte	11                      # DW_FORM_data1
	.byte	59                      # DW_AT_decl_line
	.byte	5                       # DW_FORM_data2
	.byte	73                      # DW_AT_type
	.byte	19                      # DW_FORM_ref4
	.byte	2                       # DW_AT_location
	.byte	24                      # DW_FORM_exprloc
	.byte	0                       # EOM(1)
	.byte	0                       # EOM(2)
	.byte	8                       # Abbreviation Code
	.byte	22                      # DW_TAG_typedef
	.byte	0                       # DW_CHILDREN_no
	.byte	73                      # DW_AT_type
	.byte	19                      # DW_FORM_ref4
	.byte	3                       # DW_AT_name
	.byte	14                      # DW_FORM_strp
	.byte	58                      # DW_AT_decl_file
	.byte	11                      # DW_FORM_data1
	.byte	59                      # DW_AT_decl_line
	.byte	11                      # DW_FORM_data1
	.byte	0                       # EOM(1)
	.byte	0                       # EOM(2)
	.byte	9                       # Abbreviation Code
	.byte	36                      # DW_TAG_base_type
	.byte	0                       # DW_CHILDREN_no
	.byte	3                       # DW_AT_name
	.byte	14                      # DW_FORM_strp
	.byte	62                      # DW_AT_encoding
	.byte	11                      # DW_FORM_data1
	.byte	11                      # DW_AT_byte_size
	.byte	11                      # DW_FORM_data1
	.byte	0                       # EOM(1)
	.byte	0                       # EOM(2)
	.byte	10                      # Abbreviation Code
	.byte	46                      # DW_TAG_subprogram
	.byte	1                       # DW_CHILDREN_yes
	.byte	3                       # DW_AT_name
	.byte	14                      # DW_FORM_strp
	.byte	58                      # DW_AT_decl_file
	.byte	11                      # DW_FORM_data1
	.byte	59                      # DW_AT_decl_line
	.byte	11                      # DW_FORM_data1
	.byte	39                      # DW_AT_prototyped
	.byte	25                      # DW_FORM_flag_present
	.byte	50                      # DW_AT_accessibility
	.byte	11                      # DW_FORM_data1
	.byte	17                      # DW_AT_low_pc
	.byte	1                       # DW_FORM_addr
	.byte	18                      # DW_AT_high_pc
	.byte	1                       # DW_FORM_addr
	.byte	64                      # DW_AT_frame_base
	.byte	24                      # DW_FORM_exprloc
	.byte	0                       # EOM(1)
	.byte	0                       # EOM(2)
	.byte	11                      # Abbreviation Code
	.byte	15                      # DW_TAG_pointer_type
	.byte	0                       # DW_CHILDREN_no
	.byte	73                      # DW_AT_type
	.byte	19                      # DW_FORM_ref4
	.byte	0                       # EOM(1)
	.byte	0                       # EOM(2)
	.byte	12                      # Abbreviation Code
	.byte	22                      # DW_TAG_typedef
	.byte	0                       # DW_CHILDREN_no
	.byte	73                      # DW_AT_type
	.byte	19                      # DW_FORM_ref4
	.byte	3                       # DW_AT_name
	.byte	14                      # DW_FORM_strp
	.byte	58                      # DW_AT_decl_file
	.byte	11                      # DW_FORM_data1
	.byte	59                      # DW_AT_decl_line
	.byte	5                       # DW_FORM_data2
	.byte	0                       # EOM(1)
	.byte	0                       # EOM(2)
	.byte	13                      # Abbreviation Code
	.byte	19                      # DW_TAG_structure_type
	.byte	1                       # DW_CHILDREN_yes
	.byte	11                      # DW_AT_byte_size
	.byte	5                       # DW_FORM_data2
	.byte	58                      # DW_AT_decl_file
	.byte	11                      # DW_FORM_data1
	.byte	59                      # DW_AT_decl_line
	.byte	5                       # DW_FORM_data2
	.byte	0                       # EOM(1)
	.byte	0                       # EOM(2)
	.byte	14                      # Abbreviation Code
	.byte	13                      # DW_TAG_member
	.byte	0                       # DW_CHILDREN_no
	.byte	3                       # DW_AT_name
	.byte	14                      # DW_FORM_strp
	.byte	73                      # DW_AT_type
	.byte	19                      # DW_FORM_ref4
	.byte	58                      # DW_AT_decl_file
	.byte	11                      # DW_FORM_data1
	.byte	59                      # DW_AT_decl_line
	.byte	5                       # DW_FORM_data2
	.byte	56                      # DW_AT_data_member_location
	.byte	11                      # DW_FORM_data1
	.byte	50                      # DW_AT_accessibility
	.byte	11                      # DW_FORM_data1
	.byte	0                       # EOM(1)
	.byte	0                       # EOM(2)
	.byte	15                      # Abbreviation Code
	.byte	13                      # DW_TAG_member
	.byte	0                       # DW_CHILDREN_no
	.byte	3                       # DW_AT_name
	.byte	14                      # DW_FORM_strp
	.byte	73                      # DW_AT_type
	.byte	19                      # DW_FORM_ref4
	.byte	58                      # DW_AT_decl_file
	.byte	11                      # DW_FORM_data1
	.byte	59                      # DW_AT_decl_line
	.byte	5                       # DW_FORM_data2
	.byte	56                      # DW_AT_data_member_location
	.byte	5                       # DW_FORM_data2
	.byte	50                      # DW_AT_accessibility
	.byte	11                      # DW_FORM_data1
	.byte	0                       # EOM(1)
	.byte	0                       # EOM(2)
	.byte	16                      # Abbreviation Code
	.byte	19                      # DW_TAG_structure_type
	.byte	1                       # DW_CHILDREN_yes
	.byte	11                      # DW_AT_byte_size
	.byte	11                      # DW_FORM_data1
	.byte	58                      # DW_AT_decl_file
	.byte	11                      # DW_FORM_data1
	.byte	59                      # DW_AT_decl_line
	.byte	11                      # DW_FORM_data1
	.byte	0                       # EOM(1)
	.byte	0                       # EOM(2)
	.byte	17                      # Abbreviation Code
	.byte	13                      # DW_TAG_member
	.byte	0                       # DW_CHILDREN_no
	.byte	3                       # DW_AT_name
	.byte	14                      # DW_FORM_strp
	.byte	73                      # DW_AT_type
	.byte	19                      # DW_FORM_ref4
	.byte	58                      # DW_AT_decl_file
	.byte	11                      # DW_FORM_data1
	.byte	59                      # DW_AT_decl_line
	.byte	11                      # DW_FORM_data1
	.byte	56                      # DW_AT_data_member_location
	.byte	11                      # DW_FORM_data1
	.byte	50                      # DW_AT_accessibility
	.byte	11                      # DW_FORM_data1
	.byte	0                       # EOM(1)
	.byte	0                       # EOM(2)
	.byte	18                      # Abbreviation Code
	.byte	15                      # DW_TAG_pointer_type
	.byte	0                       # DW_CHILDREN_no
	.byte	0                       # EOM(1)
	.byte	0                       # EOM(2)
	.byte	19                      # Abbreviation Code
	.byte	21                      # DW_TAG_subroutine_type
	.byte	1                       # DW_CHILDREN_yes
	.byte	73                      # DW_AT_type
	.byte	19                      # DW_FORM_ref4
	.byte	39                      # DW_AT_prototyped
	.byte	25                      # DW_FORM_flag_present
	.byte	0                       # EOM(1)
	.byte	0                       # EOM(2)
	.byte	20                      # Abbreviation Code
	.byte	5                       # DW_TAG_formal_parameter
	.byte	0                       # DW_CHILDREN_no
	.byte	73                      # DW_AT_type
	.byte	19                      # DW_FORM_ref4
	.byte	0                       # EOM(1)
	.byte	0                       # EOM(2)
	.byte	21                      # Abbreviation Code
	.byte	21                      # DW_TAG_subroutine_type
	.byte	1                       # DW_CHILDREN_yes
	.byte	39                      # DW_AT_prototyped
	.byte	25                      # DW_FORM_flag_present
	.byte	0                       # EOM(1)
	.byte	0                       # EOM(2)
	.byte	22                      # Abbreviation Code
	.byte	1                       # DW_TAG_array_type
	.byte	1                       # DW_CHILDREN_yes
	.byte	73                      # DW_AT_type
	.byte	19                      # DW_FORM_ref4
	.byte	0                       # EOM(1)
	.byte	0                       # EOM(2)
	.byte	23                      # Abbreviation Code
	.byte	33                      # DW_TAG_subrange_type
	.byte	0                       # DW_CHILDREN_no
	.byte	73                      # DW_AT_type
	.byte	19                      # DW_FORM_ref4
	.byte	47                      # DW_AT_upper_bound
	.byte	11                      # DW_FORM_data1
	.byte	0                       # EOM(1)
	.byte	0                       # EOM(2)
	.byte	24                      # Abbreviation Code
	.byte	36                      # DW_TAG_base_type
	.byte	0                       # DW_CHILDREN_no
	.byte	3                       # DW_AT_name
	.byte	14                      # DW_FORM_strp
	.byte	11                      # DW_AT_byte_size
	.byte	11                      # DW_FORM_data1
	.byte	62                      # DW_AT_encoding
	.byte	11                      # DW_FORM_data1
	.byte	0                       # EOM(1)
	.byte	0                       # EOM(2)
	.byte	25                      # Abbreviation Code
	.byte	33                      # DW_TAG_subrange_type
	.byte	0                       # DW_CHILDREN_no
	.byte	73                      # DW_AT_type
	.byte	19                      # DW_FORM_ref4
	.byte	47                      # DW_AT_upper_bound
	.byte	5                       # DW_FORM_data2
	.byte	0                       # EOM(1)
	.byte	0                       # EOM(2)
	.byte	0                       # EOM(3)
	.section	.debug_ranges,"",@progbits
.Lgnu_ranges0:
.Ldebug_ranges0:
	.quad	.Ltmp21
	.quad	.Ltmp22
	.quad	.Ltmp25
	.quad	.Ltmp26
	.quad	0
	.quad	0
.Ldebug_ranges1:
	.quad	.Ltmp20
	.quad	.Ltmp26
	.quad	.Ltmp274
	.quad	.Ltmp275
	.quad	0
	.quad	0
.Ldebug_ranges2:
	.quad	.Ltmp29
	.quad	.Ltmp30
	.quad	.Ltmp33
	.quad	.Ltmp34
	.quad	0
	.quad	0
.Ldebug_ranges3:
	.quad	.Ltmp37
	.quad	.Ltmp38
	.quad	.Ltmp41
	.quad	.Ltmp42
	.quad	0
	.quad	0
.Ldebug_ranges4:
	.quad	.Ltmp45
	.quad	.Ltmp46
	.quad	.Ltmp49
	.quad	.Ltmp50
	.quad	0
	.quad	0
.Ldebug_ranges5:
	.quad	.Ltmp62
	.quad	.Ltmp63
	.quad	.Ltmp66
	.quad	.Ltmp67
	.quad	0
	.quad	0
.Ldebug_ranges6:
	.quad	.Ltmp69
	.quad	.Ltmp70
	.quad	.Ltmp73
	.quad	.Ltmp74
	.quad	0
	.quad	0
.Ldebug_ranges7:
	.quad	.Ltmp77
	.quad	.Ltmp78
	.quad	.Ltmp81
	.quad	.Ltmp82
	.quad	0
	.quad	0
.Ldebug_ranges8:
	.quad	.Ltmp85
	.quad	.Ltmp86
	.quad	.Ltmp89
	.quad	.Ltmp90
	.quad	0
	.quad	0
.Ldebug_ranges9:
	.quad	.Ltmp93
	.quad	.Ltmp94
	.quad	.Ltmp97
	.quad	.Ltmp98
	.quad	0
	.quad	0
.Ldebug_ranges10:
	.quad	.Ltmp101
	.quad	.Ltmp102
	.quad	.Ltmp105
	.quad	.Ltmp106
	.quad	0
	.quad	0
.Ldebug_ranges11:
	.quad	.Ltmp113
	.quad	.Ltmp114
	.quad	.Ltmp115
	.quad	.Ltmp116
	.quad	0
	.quad	0
.Ldebug_ranges12:
	.quad	.Ltmp109
	.quad	.Ltmp110
	.quad	.Ltmp113
	.quad	.Ltmp114
	.quad	.Ltmp115
	.quad	.Ltmp116
	.quad	0
	.quad	0
.Ldebug_ranges13:
	.quad	.Ltmp108
	.quad	.Ltmp114
	.quad	.Ltmp115
	.quad	.Ltmp116
	.quad	0
	.quad	0
.Ldebug_ranges14:
	.quad	.Ltmp123
	.quad	.Ltmp124
	.quad	.Ltmp125
	.quad	.Ltmp126
	.quad	0
	.quad	0
.Ldebug_ranges15:
	.quad	.Ltmp119
	.quad	.Ltmp120
	.quad	.Ltmp123
	.quad	.Ltmp124
	.quad	.Ltmp125
	.quad	.Ltmp126
	.quad	0
	.quad	0
.Ldebug_ranges16:
	.quad	.Ltmp118
	.quad	.Ltmp124
	.quad	.Ltmp125
	.quad	.Ltmp126
	.quad	0
	.quad	0
.Ldebug_ranges17:
	.quad	.Ltmp133
	.quad	.Ltmp134
	.quad	.Ltmp135
	.quad	.Ltmp136
	.quad	0
	.quad	0
.Ldebug_ranges18:
	.quad	.Ltmp129
	.quad	.Ltmp130
	.quad	.Ltmp133
	.quad	.Ltmp134
	.quad	.Ltmp135
	.quad	.Ltmp136
	.quad	0
	.quad	0
.Ldebug_ranges19:
	.quad	.Ltmp128
	.quad	.Ltmp134
	.quad	.Ltmp135
	.quad	.Ltmp136
	.quad	0
	.quad	0
.Ldebug_ranges20:
	.quad	.Ltmp143
	.quad	.Ltmp144
	.quad	.Ltmp145
	.quad	.Ltmp146
	.quad	0
	.quad	0
.Ldebug_ranges21:
	.quad	.Ltmp139
	.quad	.Ltmp140
	.quad	.Ltmp143
	.quad	.Ltmp144
	.quad	.Ltmp145
	.quad	.Ltmp146
	.quad	0
	.quad	0
.Ldebug_ranges22:
	.quad	.Ltmp138
	.quad	.Ltmp144
	.quad	.Ltmp145
	.quad	.Ltmp146
	.quad	0
	.quad	0
.Ldebug_ranges23:
	.quad	.Ltmp151
	.quad	.Ltmp152
	.quad	.Ltmp155
	.quad	.Ltmp156
	.quad	0
	.quad	0
.Ldebug_ranges24:
	.quad	.Ltmp159
	.quad	.Ltmp160
	.quad	.Ltmp163
	.quad	.Ltmp164
	.quad	0
	.quad	0
.Ldebug_ranges25:
	.quad	.Ltmp167
	.quad	.Ltmp168
	.quad	.Ltmp171
	.quad	.Ltmp172
	.quad	0
	.quad	0
.Ldebug_ranges26:
	.quad	.Ltmp175
	.quad	.Ltmp176
	.quad	.Ltmp179
	.quad	.Ltmp180
	.quad	0
	.quad	0
.Ldebug_ranges27:
	.quad	.Ltmp183
	.quad	.Ltmp184
	.quad	.Ltmp187
	.quad	.Ltmp188
	.quad	0
	.quad	0
.Ldebug_ranges28:
	.quad	.Ltmp197
	.quad	.Ltmp198
	.quad	.Ltmp199
	.quad	.Ltmp200
	.quad	0
	.quad	0
.Ldebug_ranges29:
	.quad	.Ltmp193
	.quad	.Ltmp194
	.quad	.Ltmp197
	.quad	.Ltmp198
	.quad	.Ltmp199
	.quad	.Ltmp200
	.quad	0
	.quad	0
.Ldebug_ranges30:
	.quad	.Ltmp192
	.quad	.Ltmp198
	.quad	.Ltmp199
	.quad	.Ltmp200
	.quad	0
	.quad	0
.Ldebug_ranges31:
	.quad	.Ltmp207
	.quad	.Ltmp208
	.quad	.Ltmp209
	.quad	.Ltmp210
	.quad	0
	.quad	0
.Ldebug_ranges32:
	.quad	.Ltmp203
	.quad	.Ltmp204
	.quad	.Ltmp207
	.quad	.Ltmp208
	.quad	.Ltmp209
	.quad	.Ltmp210
	.quad	0
	.quad	0
.Ldebug_ranges33:
	.quad	.Ltmp202
	.quad	.Ltmp208
	.quad	.Ltmp209
	.quad	.Ltmp210
	.quad	0
	.quad	0
.Ldebug_ranges34:
	.quad	.Ltmp217
	.quad	.Ltmp218
	.quad	.Ltmp219
	.quad	.Ltmp220
	.quad	0
	.quad	0
.Ldebug_ranges35:
	.quad	.Ltmp213
	.quad	.Ltmp214
	.quad	.Ltmp217
	.quad	.Ltmp218
	.quad	.Ltmp219
	.quad	.Ltmp220
	.quad	0
	.quad	0
.Ldebug_ranges36:
	.quad	.Ltmp212
	.quad	.Ltmp218
	.quad	.Ltmp219
	.quad	.Ltmp220
	.quad	0
	.quad	0
.Ldebug_ranges37:
	.quad	.Ltmp227
	.quad	.Ltmp228
	.quad	.Ltmp229
	.quad	.Ltmp230
	.quad	0
	.quad	0
.Ldebug_ranges38:
	.quad	.Ltmp223
	.quad	.Ltmp224
	.quad	.Ltmp227
	.quad	.Ltmp228
	.quad	.Ltmp229
	.quad	.Ltmp230
	.quad	0
	.quad	0
.Ldebug_ranges39:
	.quad	.Ltmp222
	.quad	.Ltmp228
	.quad	.Ltmp229
	.quad	.Ltmp230
	.quad	0
	.quad	0
.Ldebug_ranges40:
	.quad	.Ltmp233
	.quad	.Ltmp234
	.quad	.Ltmp237
	.quad	.Ltmp238
	.quad	0
	.quad	0
.Ldebug_ranges41:
	.quad	.Ltmp245
	.quad	.Ltmp246
	.quad	.Ltmp247
	.quad	.Ltmp248
	.quad	0
	.quad	0
.Ldebug_ranges42:
	.quad	.Ltmp241
	.quad	.Ltmp242
	.quad	.Ltmp245
	.quad	.Ltmp246
	.quad	.Ltmp247
	.quad	.Ltmp248
	.quad	0
	.quad	0
.Ldebug_ranges43:
	.quad	.Ltmp240
	.quad	.Ltmp246
	.quad	.Ltmp247
	.quad	.Ltmp248
	.quad	0
	.quad	0
.Ldebug_ranges44:
	.quad	.Ltmp255
	.quad	.Ltmp256
	.quad	.Ltmp257
	.quad	.Ltmp258
	.quad	0
	.quad	0
.Ldebug_ranges45:
	.quad	.Ltmp251
	.quad	.Ltmp252
	.quad	.Ltmp255
	.quad	.Ltmp256
	.quad	.Ltmp257
	.quad	.Ltmp258
	.quad	0
	.quad	0
.Ldebug_ranges46:
	.quad	.Ltmp250
	.quad	.Ltmp256
	.quad	.Ltmp257
	.quad	.Ltmp258
	.quad	0
	.quad	0
.Ldebug_ranges47:
	.quad	.Ltmp265
	.quad	.Ltmp266
	.quad	.Ltmp267
	.quad	.Ltmp268
	.quad	0
	.quad	0
.Ldebug_ranges48:
	.quad	.Ltmp261
	.quad	.Ltmp262
	.quad	.Ltmp265
	.quad	.Ltmp266
	.quad	.Ltmp267
	.quad	.Ltmp268
	.quad	0
	.quad	0
.Ldebug_ranges49:
	.quad	.Ltmp260
	.quad	.Ltmp266
	.quad	.Ltmp267
	.quad	.Ltmp268
	.quad	0
	.quad	0
.Ldebug_ranges50:
	.quad	.Ltmp281
	.quad	.Ltmp282
	.quad	.Ltmp285
	.quad	.Ltmp286
	.quad	0
	.quad	0
.Ldebug_ranges51:
	.quad	.Ltmp343
	.quad	.Ltmp344
	.quad	.Ltmp347
	.quad	.Ltmp348
	.quad	0
	.quad	0
.Ldebug_ranges52:
	.quad	.Ltmp293
	.quad	.Ltmp294
	.quad	.Ltmp340
	.quad	.Ltmp351
	.quad	0
	.quad	0
.Ldebug_ranges53:
	.quad	.Ltmp292
	.quad	.Ltmp294
	.quad	.Ltmp340
	.quad	.Ltmp352
	.quad	0
	.quad	0
.Ldebug_ranges54:
	.quad	.Ltmp298
	.quad	.Ltmp299
	.quad	.Ltmp302
	.quad	.Ltmp303
	.quad	0
	.quad	0
.Ldebug_ranges55:
	.quad	.Ltmp306
	.quad	.Ltmp307
	.quad	.Ltmp310
	.quad	.Ltmp311
	.quad	0
	.quad	0
.Ldebug_ranges56:
	.quad	.Ltmp316
	.quad	.Ltmp317
	.quad	.Ltmp320
	.quad	.Ltmp321
	.quad	0
	.quad	0
.Ldebug_ranges57:
	.quad	.Ltmp313
	.quad	.Ltmp325
	.quad	.Ltmp338
	.quad	.Ltmp339
	.quad	0
	.quad	0
.Ldebug_ranges58:
	.quad	.Ltmp312
	.quad	.Ltmp325
	.quad	.Ltmp338
	.quad	.Ltmp340
	.quad	0
	.quad	0
.Ldebug_ranges59:
	.quad	.Ltmp355
	.quad	.Ltmp356
	.quad	.Ltmp359
	.quad	.Ltmp360
	.quad	0
	.quad	0
.Ldebug_ranges60:
	.quad	.Ltmp399
	.quad	.Ltmp400
	.quad	.Ltmp403
	.quad	.Ltmp404
	.quad	0
	.quad	0
.Ldebug_ranges61:
	.quad	.Ltmp407
	.quad	.Ltmp408
	.quad	.Ltmp411
	.quad	.Ltmp412
	.quad	0
	.quad	0
.Ldebug_ranges62:
	.quad	.Ltmp394
	.quad	.Ltmp395
	.quad	.Ltmp396
	.quad	.Ltmp416
	.quad	0
	.quad	0
.Ldebug_ranges63:
	.quad	.Ltmp353
	.quad	.Ltmp360
	.quad	.Ltmp394
	.quad	.Ltmp395
	.quad	.Ltmp396
	.quad	.Ltmp416
	.quad	0
	.quad	0
.Ldebug_ranges64:
	.quad	.Ltmp352
	.quad	.Ltmp360
	.quad	.Ltmp394
	.quad	.Ltmp416
	.quad	0
	.quad	0
.Ldebug_ranges65:
	.quad	.Ltmp393
	.quad	.Ltmp394
	.quad	.Ltmp416
	.quad	.Ltmp417
	.quad	0
	.quad	0
.Ldebug_ranges66:
	.quad	.Ltmp389
	.quad	.Ltmp390
	.quad	.Ltmp393
	.quad	.Ltmp394
	.quad	.Ltmp416
	.quad	.Ltmp417
	.quad	0
	.quad	0
.Ldebug_ranges67:
	.quad	.Ltmp388
	.quad	.Ltmp394
	.quad	.Ltmp416
	.quad	.Ltmp417
	.quad	0
	.quad	0
.Ldebug_ranges68:
	.quad	.Ltmp425
	.quad	.Ltmp426
	.quad	.Ltmp427
	.quad	.Ltmp428
	.quad	0
	.quad	0
.Ldebug_ranges69:
	.quad	.Ltmp421
	.quad	.Ltmp422
	.quad	.Ltmp425
	.quad	.Ltmp426
	.quad	.Ltmp427
	.quad	.Ltmp428
	.quad	0
	.quad	0
.Ldebug_ranges70:
	.quad	.Ltmp420
	.quad	.Ltmp426
	.quad	.Ltmp427
	.quad	.Ltmp428
	.quad	0
	.quad	0
.Ldebug_ranges71:
	.quad	.Ltmp381
	.quad	.Ltmp382
	.quad	.Ltmp383
	.quad	.Ltmp394
	.quad	.Ltmp416
	.quad	.Ltmp431
	.quad	0
	.quad	0
.Ldebug_ranges72:
	.quad	.Ltmp535
	.quad	.Ltmp536
	.quad	.Ltmp539
	.quad	.Ltmp541
	.quad	0
	.quad	0
.Ldebug_ranges73:
	.quad	.Ltmp552
	.quad	.Ltmp553
	.quad	.Ltmp554
	.quad	.Ltmp555
	.quad	0
	.quad	0
.Ldebug_ranges74:
	.quad	.Ltmp548
	.quad	.Ltmp549
	.quad	.Ltmp552
	.quad	.Ltmp553
	.quad	.Ltmp554
	.quad	.Ltmp555
	.quad	0
	.quad	0
.Ldebug_ranges75:
	.quad	.Ltmp547
	.quad	.Ltmp553
	.quad	.Ltmp554
	.quad	.Ltmp555
	.quad	0
	.quad	0
.Ldebug_ranges76:
	.quad	.Ltmp541
	.quad	.Ltmp543
	.quad	.Ltmp546
	.quad	.Ltmp556
	.quad	0
	.quad	0
.Ldebug_ranges77:
	.quad	.Ltmp527
	.quad	.Ltmp528
	.quad	.Ltmp529
	.quad	.Ltmp545
	.quad	.Ltmp546
	.quad	.Ltmp556
	.quad	0
	.quad	0
.Ldebug_ranges78:
	.quad	.Ltmp441
	.quad	.Ltmp442
	.quad	.Ltmp523
	.quad	.Ltmp565
	.quad	0
	.quad	0
.Ldebug_ranges79:
	.quad	.Ltmp445
	.quad	.Ltmp446
	.quad	.Ltmp453
	.quad	.Ltmp455
	.quad	.Ltmp460
	.quad	.Ltmp462
	.quad	0
	.quad	0
.Ldebug_ranges80:
	.quad	.Ltmp444
	.quad	.Ltmp446
	.quad	.Ltmp452
	.quad	.Ltmp455
	.quad	.Ltmp458
	.quad	.Ltmp462
	.quad	.Ltmp476
	.quad	.Ltmp481
	.quad	.Ltmp497
	.quad	.Ltmp505
	.quad	0
	.quad	0
.Ldebug_ranges81:
	.quad	.Ltmp443
	.quad	.Ltmp446
	.quad	.Ltmp452
	.quad	.Ltmp455
	.quad	.Ltmp458
	.quad	.Ltmp462
	.quad	.Ltmp476
	.quad	.Ltmp481
	.quad	.Ltmp497
	.quad	.Ltmp505
	.quad	0
	.quad	0
.Ldebug_ranges82:
	.quad	.Ltmp522
	.quad	.Ltmp523
	.quad	.Ltmp565
	.quad	.Ltmp566
	.quad	0
	.quad	0
.Ldebug_ranges83:
	.quad	.Ltmp518
	.quad	.Ltmp519
	.quad	.Ltmp522
	.quad	.Ltmp523
	.quad	.Ltmp565
	.quad	.Ltmp566
	.quad	0
	.quad	0
.Ldebug_ranges84:
	.quad	.Ltmp517
	.quad	.Ltmp523
	.quad	.Ltmp565
	.quad	.Ltmp566
	.quad	0
	.quad	0
.Ldebug_ranges85:
	.quad	.Ltmp574
	.quad	.Ltmp575
	.quad	.Ltmp576
	.quad	.Ltmp577
	.quad	0
	.quad	0
.Ldebug_ranges86:
	.quad	.Ltmp570
	.quad	.Ltmp571
	.quad	.Ltmp574
	.quad	.Ltmp575
	.quad	.Ltmp576
	.quad	.Ltmp577
	.quad	0
	.quad	0
.Ldebug_ranges87:
	.quad	.Ltmp569
	.quad	.Ltmp575
	.quad	.Ltmp576
	.quad	.Ltmp577
	.quad	0
	.quad	0
.Ldebug_ranges88:
	.quad	.Ltmp510
	.quad	.Ltmp511
	.quad	.Ltmp512
	.quad	.Ltmp523
	.quad	.Ltmp565
	.quad	.Ltmp578
	.quad	0
	.quad	0
.Ldebug_ranges89:
	.quad	.Ltmp442
	.quad	.Ltmp446
	.quad	.Ltmp452
	.quad	.Ltmp455
	.quad	.Ltmp458
	.quad	.Ltmp462
	.quad	.Ltmp476
	.quad	.Ltmp481
	.quad	.Ltmp497
	.quad	.Ltmp505
	.quad	.Ltmp507
	.quad	.Ltmp523
	.quad	.Ltmp565
	.quad	.Ltmp578
	.quad	0
	.quad	0
.Ldebug_ranges90:
	.quad	.Ltmp440
	.quad	.Ltmp446
	.quad	.Ltmp452
	.quad	.Ltmp455
	.quad	.Ltmp458
	.quad	.Ltmp462
	.quad	.Ltmp476
	.quad	.Ltmp481
	.quad	.Ltmp497
	.quad	.Ltmp505
	.quad	.Ltmp507
	.quad	.Ltmp578
	.quad	0
	.quad	0
.Ldebug_ranges91:
	.quad	.Ltmp431
	.quad	.Ltmp432
	.quad	.Ltmp440
	.quad	.Ltmp446
	.quad	.Ltmp452
	.quad	.Ltmp455
	.quad	.Ltmp458
	.quad	.Ltmp462
	.quad	.Ltmp476
	.quad	.Ltmp481
	.quad	.Ltmp497
	.quad	.Ltmp505
	.quad	.Ltmp507
	.quad	.Ltmp578
	.quad	0
	.quad	0
.Ldebug_ranges92:
	.quad	.Ltmp456
	.quad	.Ltmp457
	.quad	.Ltmp471
	.quad	.Ltmp476
	.quad	.Ltmp482
	.quad	.Ltmp497
	.quad	.Ltmp505
	.quad	.Ltmp506
	.quad	0
	.quad	0
.Ldebug_ranges93:
	.quad	.Ltmp450
	.quad	.Ltmp451
	.quad	.Ltmp456
	.quad	.Ltmp457
	.quad	.Ltmp462
	.quad	.Ltmp476
	.quad	.Ltmp481
	.quad	.Ltmp497
	.quad	.Ltmp505
	.quad	.Ltmp507
	.quad	0
	.quad	0
.Ldebug_ranges94:
	.quad	.Ltmp16
	.quad	.Ltmp17
	.quad	.Ltmp19
	.quad	.Ltmp277
	.quad	.Ltmp278
	.quad	.Ltmp578
	.quad	0
	.quad	0
	.section	.debug_pubnames,"",@progbits
.Lset0 = .Lpubnames_end0-.Lpubnames_begin0 # Length of Public Names Info
	.long	.Lset0
.Lpubnames_begin0:
	.short	2                       # DWARF Version
	.long	.L.debug_info_begin0    # Offset of Compilation Unit Info
.Lset1 = .L.debug_info_end0-.L.debug_info_begin0 # Compilation Unit Length
	.long	.Lset1
	.long	38                      # DIE offset
	.asciz	"BZ2_decompress"        # External Name
	.long	3127                    # DIE offset
	.asciz	"makeMaps_d"            # External Name
	.long	0                       # End Mark
.Lpubnames_end0:
	.section	.debug_pubtypes,"",@progbits
.Lset2 = .Lpubtypes_end0-.Lpubtypes_begin0 # Length of Public Types Info
	.long	.Lset2
.Lpubtypes_begin0:
	.short	2                       # DWARF Version
	.long	.L.debug_info_begin0    # Offset of Compilation Unit Info
.Lset3 = .L.debug_info_end0-.L.debug_info_begin0 # Compilation Unit Length
	.long	.Lset3
	.long	3109                    # DIE offset
	.asciz	"Int32"                 # External Name
	.long	4482                    # DIE offset
	.asciz	"unsigned short"        # External Name
	.long	4389                    # DIE offset
	.asciz	"UChar"                 # External Name
	.long	4418                    # DIE offset
	.asciz	"UInt32"                # External Name
	.long	3187                    # DIE offset
	.asciz	"DState"                # External Name
	.long	4154                    # DIE offset
	.asciz	"bz_stream"             # External Name
	.long	4338                    # DIE offset
	.asciz	"unsigned int"          # External Name
	.long	4471                    # DIE offset
	.asciz	"UInt16"                # External Name
	.long	3120                    # DIE offset
	.asciz	"int"                   # External Name
	.long	4400                    # DIE offset
	.asciz	"unsigned char"         # External Name
	.long	4407                    # DIE offset
	.asciz	"Bool"                  # External Name
	.long	4331                    # DIE offset
	.asciz	"char"                  # External Name
	.long	0                       # End Mark
.Lpubtypes_end0:

	.ident	"clang version 3.5 (trunk)"
	.section	".note.GNU-stack","",@progbits
