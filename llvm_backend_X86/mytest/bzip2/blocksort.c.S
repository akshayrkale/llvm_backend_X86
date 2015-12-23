	.file	"blocksort.c.bc"
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
	.file	1 "./bzlib_private.h"
	.file	2 "blocksort.c"
	.text
	.globl	BZ2_blockSort
	.align	16, 0x90
	.type	BZ2_blockSort,@function
BZ2_blockSort:                          # @BZ2_blockSort
.Lfunc_begin0:
	.loc	2 1079 0                # blocksort.c:1079:0
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
	pushq	%r13
	pushq	%r12
	pushq	%rbx
	subq	$88, %rsp
.Ltmp6:
	.cfi_offset %rbx, -56
.Ltmp7:
	.cfi_offset %r12, -48
.Ltmp8:
	.cfi_offset %r13, -40
.Ltmp9:
	.cfi_offset %r14, -32
.Ltmp10:
	.cfi_offset %r15, -24
	movq	%rdi, -48(%rbp)
	.loc	2 1080 0 prologue_end   # blocksort.c:1080:0
.Ltmp11:
	movq	56(%rdi), %rax
	.loc	2 1081 0                # blocksort.c:1081:0
	movq	-48(%rbp), %rcx
	.loc	2 1080 0                # blocksort.c:1080:0
	movq	%rax, -56(%rbp)
	.loc	2 1081 0                # blocksort.c:1081:0
	movq	64(%rcx), %rax
	.loc	2 1082 0                # blocksort.c:1082:0
	movq	-48(%rbp), %rcx
	.loc	2 1081 0                # blocksort.c:1081:0
	movq	%rax, -64(%rbp)
	.loc	2 1082 0                # blocksort.c:1082:0
	movq	40(%rcx), %rax
	.loc	2 1083 0                # blocksort.c:1083:0
	movq	-48(%rbp), %rcx
	.loc	2 1082 0                # blocksort.c:1082:0
	movq	%rax, -72(%rbp)
	.loc	2 1083 0                # blocksort.c:1083:0
	movl	108(%rcx), %eax
	.loc	2 1084 0                # blocksort.c:1084:0
	movq	-48(%rbp), %rcx
	.loc	2 1083 0                # blocksort.c:1083:0
	movl	%eax, -76(%rbp)
	.loc	2 1084 0                # blocksort.c:1084:0
	movl	656(%rcx), %eax
	.loc	2 1085 0                # blocksort.c:1085:0
	movq	-48(%rbp), %rcx
	.loc	2 1084 0                # blocksort.c:1084:0
	movl	%eax, -80(%rbp)
	.loc	2 1085 0                # blocksort.c:1085:0
	movl	88(%rcx), %eax
	.loc	2 1091 0                # blocksort.c:1091:0
.Ltmp12:
	movslq	-76(%rbp), %rcx
.Ltmp13:
	.loc	2 1085 0                # blocksort.c:1085:0
	movl	%eax, -84(%rbp)
	.loc	2 1091 0                # blocksort.c:1091:0
.Ltmp14:
	cmpq	$9999, %rcx             # imm = 0x270F
	jle	.LBB0_14
# BB#1:                                 # %if.else
	.loc	2 1099 0                # blocksort.c:1099:0
.Ltmp15:
	movl	-76(%rbp), %eax
	addq	$34, %rax
	movl	%eax, -108(%rbp)
	.loc	2 1100 0                # blocksort.c:1100:0
.Ltmp16:
	movzbl	-108(%rbp), %eax
	testq	$1, %rax
	je	.LBB0_3
# BB#2:                                 # %if.then5
	movl	-108(%rbp), %eax
	incq	%rax
	movl	%eax, -108(%rbp)
.Ltmp17:
.LBB0_3:                                # %if.end
	.loc	2 1101 0                # blocksort.c:1101:0
	movslq	-108(%rbp), %rax
	movq	-64(%rbp), %rcx
	.loc	2 1110 0                # blocksort.c:1110:0
.Ltmp18:
	movslq	-84(%rbp), %rdx
.Ltmp19:
	.loc	2 1101 0                # blocksort.c:1101:0
	addq	%rax, %rcx
	movq	%rcx, -96(%rbp)
	.loc	2 1110 0                # blocksort.c:1110:0
.Ltmp20:
	testq	%rdx, %rdx
	jg	.LBB0_5
# BB#4:                                 # %if.then7
	movq	$1, %rax
	movl	%eax, -84(%rbp)
.Ltmp21:
.LBB0_5:                                # %if.end8
	.loc	2 1111 0                # blocksort.c:1111:0
	movslq	-84(%rbp), %rax
	cmpq	$101, %rax
	jl	.LBB0_7
# BB#6:                                 # %if.then10
	movq	$100, %rax
	movl	%eax, -84(%rbp)
.Ltmp22:
.LBB0_7:                                # %if.end11
	.loc	2 1112 0                # blocksort.c:1112:0
	movl	-84(%rbp), %eax
	movl	-76(%rbp), %ebx
	decq	%rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movabsq	$6148914691236517206, %rcx # imm = 0x5555555555555556
	imulq	%rcx
	movq	$63, %rcx
	movq	%rdx, %rax
	shrq	%cl, %rax
	addq	%rdx, %rax
	.loc	2 1115 0                # blocksort.c:1115:0
	movq	-56(%rbp), %rdi
	movq	-64(%rbp), %rsi
	movq	-96(%rbp), %rdx
	movq	-72(%rbp), %rcx
	movl	-80(%rbp), %r9d
	movl	-76(%rbp), %r8d
	.loc	2 1112 0                # blocksort.c:1112:0
	imulq	%rbx, %rax
	movl	%eax, -104(%rbp)
	.loc	2 1113 0                # blocksort.c:1113:0
	movl	%eax, -100(%rbp)
	leaq	-100(%rbp), %rax
	.loc	2 1115 0                # blocksort.c:1115:0
	movq	%rax, (%rsp)
	callq	mainSort
	.loc	2 1116 0                # blocksort.c:1116:0
.Ltmp23:
	movslq	-80(%rbp), %rax
	cmpq	$3, %rax
	jl	.LBB0_11
# BB#8:                                 # %if.then13
	.loc	2 1117 0                # blocksort.c:1117:0
	movl	-104(%rbp), %ebx
	movl	-100(%rbp), %eax
	movq	stderr(%rip), %r14
	movl	-76(%rbp), %r15d
	subq	%rax, %rbx
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	movq	%rbx, %rdi
	andq	%r13, %rdi
	callq	int32_to_float64
	movq	%rax, %r12
	testq	%r15, %r15
	movq	$1, %rdi
	je	.LBB0_10
# BB#9:                                 # %cond.false
	movl	-76(%rbp), %edi
.LBB0_10:                               # %cond.end
	andq	%r13, %rdi
	callq	int32_to_float64
	movq	%r12, %rdi
	movq	%rax, %rsi
	callq	float64_div
	movq	%rax, %r8
	movabsq	$.L.str, %rsi
	xorq	%rax, %rax
	movq	%r14, %rdi
	movq	%rbx, %rdx
	movq	%r15, %rcx
	callq	fprintf
.Ltmp24:
.LBB0_11:                               # %if.end20
	.loc	2 1122 0                # blocksort.c:1122:0
	movslq	-100(%rbp), %rax
	testq	%rax, %rax
	jns	.LBB0_15
# BB#12:                                # %if.then23
	.loc	2 1123 0                # blocksort.c:1123:0
.Ltmp25:
	movslq	-80(%rbp), %rax
	cmpq	$2, %rax
	jl	.LBB0_14
# BB#13:                                # %if.then26
	.loc	2 1124 0                # blocksort.c:1124:0
	movq	stderr(%rip), %rdi
	movabsq	$.L.str1, %rsi
	xorq	%rax, %rax
	callq	fprintf
.Ltmp26:
.LBB0_14:                               # %if.end28
	.loc	2 1126 0                # blocksort.c:1126:0
	movq	-48(%rbp), %rax
	movq	24(%rax), %rdi
	movq	32(%rax), %rsi
	movq	-72(%rbp), %rdx
	movl	-80(%rbp), %r8d
	movl	-76(%rbp), %ecx
	callq	fallbackSort
.Ltmp27:
.LBB0_15:                               # %if.end32
	.loc	2 1130 0                # blocksort.c:1130:0
	movq	-48(%rbp), %rcx
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	movl	%eax, 48(%rcx)
	xorq	%rcx, %rcx
	jmp	.LBB0_16
	.align	16, 0x90
.LBB0_22:                               # %for.inc
                                        #   in Loop: Header=BB0_16 Depth=1
	.loc	2 1131 0                # blocksort.c:1131:0
.Ltmp28:
	movl	-108(%rbp), %ecx
	incq	%rcx
.LBB0_16:                               # %for.cond
                                        # =>This Inner Loop Header: Depth=1
	movl	%ecx, -108(%rbp)
	movq	-48(%rbp), %rcx
	movslq	-108(%rbp), %rdx
	movslq	108(%rcx), %rcx
	cmpq	%rcx, %rdx
	jge	.LBB0_19
# BB#17:                                # %for.body
                                        #   in Loop: Header=BB0_16 Depth=1
	.loc	2 1132 0                # blocksort.c:1132:0
.Ltmp29:
	movslq	-108(%rbp), %rcx
	movq	-56(%rbp), %rdx
	movl	(%rdx,%rcx,4), %ecx
	testq	%rcx, %rcx
	jne	.LBB0_22
# BB#18:                                # %if.then40
	.loc	2 1133 0                # blocksort.c:1133:0
.Ltmp30:
	movl	-108(%rbp), %ecx
	movq	-48(%rbp), %rdx
	movl	%ecx, 48(%rdx)
.Ltmp31:
.LBB0_19:                               # %for.end
	.loc	2 1135 0                # blocksort.c:1135:0
	movq	-48(%rbp), %rcx
	movl	48(%rcx), %ecx
	cmpq	%rax, %rcx
	jne	.LBB0_21
# BB#20:                                # %if.then47
	movq	$1003, %rdi             # imm = 0x3EB
	callq	BZ2_bz__AssertH__fail
.Ltmp32:
.LBB0_21:                               # %if.end48
	.loc	2 1136 0                # blocksort.c:1136:0
	addq	$88, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	popq	%rbp
	retq
.Ltmp33:
.Ltmp34:
	.size	BZ2_blockSort, .Ltmp34-BZ2_blockSort
.Lfunc_end0:
	.file	3 "./bzlib.h"
	.cfi_endproc

	.align	16, 0x90
	.type	fallbackSort,@function
fallbackSort:                           # @fallbackSort
.Lfunc_begin1:
	.loc	2 264 0                 # blocksort.c:264:0
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbp
.Ltmp38:
	.cfi_def_cfa_offset 16
.Ltmp39:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
.Ltmp40:
	.cfi_def_cfa_register %rbp
	pushq	%r15
	pushq	%r14
	pushq	%r13
	pushq	%r12
	pushq	%rbx
	subq	$2136, %rsp             # imm = 0x858
.Ltmp41:
	.cfi_offset %rbx, -56
.Ltmp42:
	.cfi_offset %r12, -48
.Ltmp43:
	.cfi_offset %r13, -40
.Ltmp44:
	.cfi_offset %r14, -32
.Ltmp45:
	.cfi_offset %r15, -24
	movq	%rdi, -48(%rbp)
	movq	%rsi, -56(%rbp)
	movl	%r8d, -72(%rbp)
	.loc	2 270 0 prologue_end    # blocksort.c:270:0
.Ltmp46:
	movq	-56(%rbp), %rax
	.loc	2 276 0                 # blocksort.c:276:0
.Ltmp47:
	movslq	-72(%rbp), %rsi
	movq	%rdx, -64(%rbp)
	movl	%ecx, -68(%rbp)
.Ltmp48:
	.loc	2 270 0                 # blocksort.c:270:0
	movq	%rax, -2176(%rbp)
	.loc	2 276 0                 # blocksort.c:276:0
.Ltmp49:
	cmpq	$4, %rsi
	jl	.LBB1_2
# BB#1:                                 # %if.then
	.loc	2 277 0                 # blocksort.c:277:0
	movq	stderr(%rip), %rdi
	movabsq	$.L.str3, %rsi
	xorq	%rax, %rax
	callq	fprintf
.Ltmp50:
.LBB1_2:                                # %if.end
	xorq	%rax, %rax
	.loc	2 278 0                 # blocksort.c:278:0
.Ltmp51:
	movl	%eax, -2136(%rbp)
	jmp	.LBB1_3
	.align	16, 0x90
.LBB1_4:                                # %for.inc
                                        #   in Loop: Header=BB1_3 Depth=1
	movl	-2136(%rbp), %ecx
	movslq	-2136(%rbp), %rdx
	movl	%eax, -1104(%rbp,%rdx,4)
	incq	%rcx
	movl	%ecx, -2136(%rbp)
.LBB1_3:                                # %for.cond
                                        # =>This Inner Loop Header: Depth=1
	movslq	-2136(%rbp), %rcx
	cmpq	$256, %rcx              # imm = 0x100
	jle	.LBB1_4
.Ltmp52:
# BB#5:                                 # %for.end
	xorq	%rax, %rax
	.loc	2 279 0                 # blocksort.c:279:0
.Ltmp53:
	movl	%eax, -2136(%rbp)
	jmp	.LBB1_6
	.align	16, 0x90
.LBB1_7:                                # %for.inc10
                                        #   in Loop: Header=BB1_6 Depth=1
	movslq	-2136(%rbp), %rax
	movq	-2176(%rbp), %rcx
	movzbl	(%rcx,%rax), %eax
	movl	-1104(%rbp,%rax,4), %ecx
	movl	-2136(%rbp), %edx
	incq	%rcx
	movl	%ecx, -1104(%rbp,%rax,4)
	incq	%rdx
	movl	%edx, -2136(%rbp)
.LBB1_6:                                # %for.cond2
                                        # =>This Inner Loop Header: Depth=1
	movslq	-2136(%rbp), %rax
	movslq	-68(%rbp), %rcx
	cmpq	%rcx, %rax
	jl	.LBB1_7
.Ltmp54:
# BB#8:                                 # %for.end12
	xorq	%rax, %rax
	.loc	2 280 0                 # blocksort.c:280:0
.Ltmp55:
	movl	%eax, -2136(%rbp)
	jmp	.LBB1_9
	.align	16, 0x90
.LBB1_10:                               # %for.inc20
                                        #   in Loop: Header=BB1_9 Depth=1
	movslq	-2136(%rbp), %rax
	movl	-2136(%rbp), %ecx
	movl	-1104(%rbp,%rax,4), %edx
	movl	%edx, -2128(%rbp,%rax,4)
	incq	%rcx
	movl	%ecx, -2136(%rbp)
.LBB1_9:                                # %for.cond13
                                        # =>This Inner Loop Header: Depth=1
	movslq	-2136(%rbp), %rax
	cmpq	$255, %rax
	jle	.LBB1_10
.Ltmp56:
# BB#11:                                # %for.end22
	movq	$1, %rax
	.loc	2 281 0                 # blocksort.c:281:0
.Ltmp57:
	movl	%eax, -2136(%rbp)
	jmp	.LBB1_12
	.align	16, 0x90
.LBB1_13:                               # %for.inc30
                                        #   in Loop: Header=BB1_12 Depth=1
	movslq	-2136(%rbp), %rax
	leaq	-1(%rax), %rdx
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movl	-1104(%rbp,%rdx,4), %ecx
	movl	-1104(%rbp,%rax,4), %edx
	movl	-2136(%rbp), %esi
	addq	%rcx, %rdx
	movl	%edx, -1104(%rbp,%rax,4)
	incq	%rsi
	movl	%esi, -2136(%rbp)
.LBB1_12:                               # %for.cond23
                                        # =>This Inner Loop Header: Depth=1
	movslq	-2136(%rbp), %rax
	cmpq	$256, %rax              # imm = 0x100
	jle	.LBB1_13
.Ltmp58:
# BB#14:                                # %for.end32
	xorq	%rax, %rax
	jmp	.LBB1_15
	.align	16, 0x90
.LBB1_16:                               # %for.inc45
                                        #   in Loop: Header=BB1_15 Depth=1
	.loc	2 284 0                 # blocksort.c:284:0
.Ltmp59:
	movslq	-2136(%rbp), %rax
	movq	-2176(%rbp), %rcx
	movzbl	(%rcx,%rax), %eax
	movl	%eax, -2140(%rbp)
	.loc	2 285 0                 # blocksort.c:285:0
	movslq	-2140(%rbp), %rax
	movl	-1104(%rbp,%rax,4), %eax
	decq	%rax
	movl	%eax, -2144(%rbp)
	.loc	2 286 0                 # blocksort.c:286:0
	movslq	-2140(%rbp), %rcx
	.loc	2 287 0                 # blocksort.c:287:0
	movl	-2136(%rbp), %edx
	movslq	-2144(%rbp), %rsi
	movq	-48(%rbp), %rdi
	.loc	2 286 0                 # blocksort.c:286:0
	movl	%eax, -1104(%rbp,%rcx,4)
	.loc	2 287 0                 # blocksort.c:287:0
	movl	%edx, (%rdi,%rsi,4)
.Ltmp60:
	.loc	2 283 0                 # blocksort.c:283:0
	movl	-2136(%rbp), %eax
	incq	%rax
.LBB1_15:                               # %for.cond33
                                        # =>This Inner Loop Header: Depth=1
	movl	%eax, -2136(%rbp)
	movslq	-2136(%rbp), %rax
	movslq	-68(%rbp), %rcx
	cmpq	%rcx, %rax
	jl	.LBB1_16
.Ltmp61:
# BB#17:                                # %for.end47
	.loc	2 290 0                 # blocksort.c:290:0
	movslq	-68(%rbp), %rax
	movq	$58, %rcx
	movq	%rax, %rdx
	shrq	%cl, %rdx
	andq	$31, %rdx
	addq	%rax, %rdx
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	$5, %rcx
	shrq	%cl, %rdx
	addq	$2, %rdx
	movl	%edx, -2168(%rbp)
	xorq	%rax, %rax
	.loc	2 291 0                 # blocksort.c:291:0
.Ltmp62:
	movl	%eax, -2136(%rbp)
	jmp	.LBB1_18
	.align	16, 0x90
.LBB1_19:                               # %for.inc55
                                        #   in Loop: Header=BB1_18 Depth=1
	movslq	-2136(%rbp), %rcx
	movq	-64(%rbp), %rdx
	movl	%eax, (%rdx,%rcx,4)
	movl	-2136(%rbp), %ecx
	incq	%rcx
	movl	%ecx, -2136(%rbp)
.LBB1_18:                               # %for.cond49
                                        # =>This Inner Loop Header: Depth=1
	movslq	-2136(%rbp), %rcx
	movslq	-2168(%rbp), %rdx
	cmpq	%rdx, %rcx
	jl	.LBB1_19
.Ltmp63:
# BB#20:                                # %for.end57
	xorq	%rax, %rax
	jmp	.LBB1_21
	.align	16, 0x90
.LBB1_22:                               # %for.inc68
                                        #   in Loop: Header=BB1_21 Depth=1
	.loc	2 292 0                 # blocksort.c:292:0
.Ltmp64:
	movslq	-2136(%rbp), %rax
	movl	-1104(%rbp,%rax,4), %eax
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	andq	$31, %rax
	movq	$1, %rsi
	movq	-64(%rbp), %rdi
	movq	%rax, %rcx
	shlq	%cl, %rsi
	movq	$37, %rcx
	sarq	%cl, %rdx
	movl	(%rdi,%rdx,4), %eax
	orq	%rsi, %rax
	movl	%eax, (%rdi,%rdx,4)
	movl	-2136(%rbp), %eax
	incq	%rax
.LBB1_21:                               # %for.cond58
                                        # =>This Inner Loop Header: Depth=1
	movl	%eax, -2136(%rbp)
	movslq	-2136(%rbp), %rax
	cmpq	$255, %rax
	jle	.LBB1_22
.Ltmp65:
# BB#23:                                # %for.end70
	xorq	%rax, %rax
	jmp	.LBB1_24
	.align	16, 0x90
.LBB1_25:                               # %for.inc96
                                        #   in Loop: Header=BB1_24 Depth=1
	.loc	2 302 0                 # blocksort.c:302:0
.Ltmp66:
	movl	-68(%rbp), %eax
	movl	-2136(%rbp), %ecx
	leaq	(%rax,%rcx,2), %rax
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	andq	$31, %rax
	movq	$1, %rsi
	movq	-64(%rbp), %rdi
	movq	%rax, %rcx
	shlq	%cl, %rsi
	movq	$37, %rcx
	sarq	%cl, %rdx
	movl	(%rdi,%rdx,4), %eax
	orq	%rsi, %rax
	movl	%eax, (%rdi,%rdx,4)
	.loc	2 303 0                 # blocksort.c:303:0
	movl	-68(%rbp), %eax
	movl	-2136(%rbp), %ecx
	leaq	1(%rax,%rcx,2), %rax
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	andq	$31, %rax
	movq	$1, %rsi
	movq	-64(%rbp), %rdi
	movq	%rax, %rcx
	shlq	%cl, %rsi
	movq	$37, %rcx
	sarq	%cl, %rdx
	movl	(%rdi,%rdx,4), %eax
	notq	%rsi
	andq	%rsi, %rax
	movl	%eax, (%rdi,%rdx,4)
.Ltmp67:
	.loc	2 301 0                 # blocksort.c:301:0
	movl	-2136(%rbp), %eax
	incq	%rax
.LBB1_24:                               # %for.cond71
                                        # =>This Inner Loop Header: Depth=1
	movl	%eax, -2136(%rbp)
	movslq	-2136(%rbp), %rax
	cmpq	$31, %rax
	jle	.LBB1_25
.Ltmp68:
# BB#26:                                # %for.end98
	movq	$1, %rax
	.loc	2 307 0                 # blocksort.c:307:0
	movl	%eax, -2132(%rbp)
	xorq	%r12, %r12
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	.loc	2 326 0                 # blocksort.c:326:0
.Ltmp69:
	movabsq	$0, %r13
	movabsq	$1, %r14
.Ltmp70:
	.loc	2 356 0                 # blocksort.c:356:0
	movabsq	$.L.str7, %r15
.Ltmp71:
	.align	16, 0x90
.LBB1_27:                               # %while.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB1_30 Depth 2
                                        #     Child Loop BB1_37 Depth 2
                                        #       Child Loop BB1_38 Depth 3
                                        #       Child Loop BB1_44 Depth 3
                                        #       Child Loop BB1_46 Depth 3
                                        #       Child Loop BB1_49 Depth 3
                                        #       Child Loop BB1_55 Depth 3
                                        #       Child Loop BB1_57 Depth 3
                                        #       Child Loop BB1_62 Depth 3
	.loc	2 310 0                 # blocksort.c:310:0
	movslq	-72(%rbp), %rax
	cmpq	$4, %rax
	jl	.LBB1_29
# BB#28:                                # %if.then101
                                        #   in Loop: Header=BB1_27 Depth=1
	.loc	2 311 0                 # blocksort.c:311:0
	movq	stderr(%rip), %rdi
	movl	-2132(%rbp), %edx
	xorq	%rax, %rax
	movabsq	$.L.str6, %rsi
	callq	fprintf
.Ltmp72:
.LBB1_29:                               # %if.end103
                                        #   in Loop: Header=BB1_27 Depth=1
	.loc	2 313 0                 # blocksort.c:313:0
	movl	%r12d, -2140(%rbp)
	.loc	2 314 0                 # blocksort.c:314:0
.Ltmp73:
	movl	%r12d, -2136(%rbp)
	jmp	.LBB1_30
	.align	16, 0x90
.LBB1_35:                               # %for.inc126
                                        #   in Loop: Header=BB1_30 Depth=2
	.loc	2 317 0                 # blocksort.c:317:0
.Ltmp74:
	movl	-2140(%rbp), %eax
	movslq	-2144(%rbp), %rcx
	movq	-56(%rbp), %rdx
	movl	%eax, (%rdx,%rcx,4)
.Ltmp75:
	.loc	2 314 0                 # blocksort.c:314:0
	movl	-2136(%rbp), %eax
	incq	%rax
	movl	%eax, -2136(%rbp)
.LBB1_30:                               # %for.cond104
                                        #   Parent Loop BB1_27 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movslq	-2136(%rbp), %rax
	movslq	-68(%rbp), %rcx
	cmpq	%rcx, %rax
	jge	.LBB1_36
# BB#31:                                # %for.body107
                                        #   in Loop: Header=BB1_30 Depth=2
	.loc	2 315 0                 # blocksort.c:315:0
.Ltmp76:
	movl	-2136(%rbp), %eax
	movq	$32, %rcx
	movq	%rax, %rdx
	movq	-64(%rbp), %rsi
	shlq	%cl, %rdx
	movq	$37, %rcx
	sarq	%cl, %rdx
	movl	(%rsi,%rdx,4), %edx
	andq	$31, %rax
	movq	$1, %rsi
	movq	%rax, %rcx
	shlq	%cl, %rsi
	andq	%rdx, %rsi
	testq	%rbx, %rsi
	je	.LBB1_33
# BB#32:                                # %if.then114
                                        #   in Loop: Header=BB1_30 Depth=2
	movl	-2136(%rbp), %eax
	movl	%eax, -2140(%rbp)
.Ltmp77:
.LBB1_33:                               # %if.end115
                                        #   in Loop: Header=BB1_30 Depth=2
	.loc	2 316 0                 # blocksort.c:316:0
	movslq	-2136(%rbp), %rax
	movq	-48(%rbp), %rcx
	movl	(%rcx,%rax,4), %eax
	movl	-2132(%rbp), %ecx
	subq	%rcx, %rax
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	testq	%rdx, %rdx
	movl	%eax, -2144(%rbp)
	jns	.LBB1_35
# BB#34:                                # %if.then121
                                        #   in Loop: Header=BB1_30 Depth=2
	.loc	2 316 0                 # blocksort.c:316:0
.Ltmp78:
	movl	-68(%rbp), %eax
	movl	-2144(%rbp), %ecx
	addq	%rax, %rcx
	movl	%ecx, -2144(%rbp)
	jmp	.LBB1_35
.Ltmp79:
	.align	16, 0x90
.LBB1_36:                               # %for.end128
                                        #   in Loop: Header=BB1_27 Depth=1
	.loc	2 320 0                 # blocksort.c:320:0
	movl	%r12d, -2164(%rbp)
	.loc	2 321 0                 # blocksort.c:321:0
	movl	%ebx, -2152(%rbp)
	jmp	.LBB1_37
	.align	16, 0x90
.LBB1_60:                               # %if.end226
                                        #   in Loop: Header=BB1_37 Depth=2
	.loc	2 342 0                 # blocksort.c:342:0
.Ltmp80:
	movslq	-2152(%rbp), %rax
	movslq	-2148(%rbp), %rcx
	cmpq	%rcx, %rax
	jle	.LBB1_37
# BB#61:                                # %if.then229
                                        #   in Loop: Header=BB1_37 Depth=2
	.loc	2 343 0                 # blocksort.c:343:0
.Ltmp81:
	movl	-2152(%rbp), %eax
	movl	-2148(%rbp), %r8d
	movl	-2164(%rbp), %r9d
	.loc	2 344 0                 # blocksort.c:344:0
	movq	-48(%rbp), %rdi
	movq	-56(%rbp), %rsi
	movl	-2152(%rbp), %ecx
	movl	-2148(%rbp), %edx
	.loc	2 343 0                 # blocksort.c:343:0
	subq	%r8, %rax
	leaq	1(%rax,%r9), %rax
	movl	%eax, -2164(%rbp)
	.loc	2 344 0                 # blocksort.c:344:0
	callq	fallbackQSort3
	.loc	2 348 0                 # blocksort.c:348:0
.Ltmp82:
	movl	-2148(%rbp), %eax
.Ltmp83:
	.loc	2 347 0                 # blocksort.c:347:0
	movl	%ebx, -2156(%rbp)
	jmp	.LBB1_62
	.align	16, 0x90
.LBB1_65:                               # %for.inc251
                                        #   in Loop: Header=BB1_62 Depth=3
	.loc	2 348 0                 # blocksort.c:348:0
.Ltmp84:
	movl	-2136(%rbp), %eax
	incq	%rax
.LBB1_62:                               # %for.cond233
                                        #   Parent Loop BB1_27 Depth=1
                                        #     Parent Loop BB1_37 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movl	%eax, -2136(%rbp)
	movslq	-2136(%rbp), %rax
	movslq	-2152(%rbp), %rcx
	cmpq	%rcx, %rax
	jg	.LBB1_37
# BB#63:                                # %for.body236
                                        #   in Loop: Header=BB1_62 Depth=3
	.loc	2 349 0                 # blocksort.c:349:0
.Ltmp85:
	movslq	-2136(%rbp), %rax
	movq	-48(%rbp), %rcx
	movl	(%rcx,%rax,4), %eax
	movq	-56(%rbp), %rcx
	movl	(%rcx,%rax,4), %eax
	.loc	2 350 0                 # blocksort.c:350:0
.Ltmp86:
	movl	-2156(%rbp), %ecx
.Ltmp87:
	.loc	2 349 0                 # blocksort.c:349:0
	movl	%eax, -2160(%rbp)
	.loc	2 350 0                 # blocksort.c:350:0
.Ltmp88:
	cmpq	%rax, %rcx
	je	.LBB1_65
# BB#64:                                # %if.then243
                                        #   in Loop: Header=BB1_62 Depth=3
	.loc	2 350 0                 # blocksort.c:350:0
.Ltmp89:
	movl	-2136(%rbp), %eax
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	andq	$31, %rax
	movq	$1, %rsi
	movq	-64(%rbp), %rdi
	movq	%rax, %rcx
	shlq	%cl, %rsi
	movq	$37, %rcx
	sarq	%cl, %rdx
	movl	(%rdi,%rdx,4), %eax
	orq	%rsi, %rax
	movl	%eax, (%rdi,%rdx,4)
	movl	-2160(%rbp), %eax
	movl	%eax, -2156(%rbp)
	jmp	.LBB1_65
.Ltmp90:
	.align	16, 0x90
.LBB1_37:                               # %while.body130
                                        #   Parent Loop BB1_27 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB1_38 Depth 3
                                        #       Child Loop BB1_44 Depth 3
                                        #       Child Loop BB1_46 Depth 3
                                        #       Child Loop BB1_49 Depth 3
                                        #       Child Loop BB1_55 Depth 3
                                        #       Child Loop BB1_57 Depth 3
                                        #       Child Loop BB1_62 Depth 3
	.loc	2 325 0                 # blocksort.c:325:0
	movl	-2152(%rbp), %eax
	jmp	.LBB1_38
	.align	16, 0x90
.LBB1_42:                               # %while.body142
                                        #   in Loop: Header=BB1_38 Depth=3
	.loc	2 326 0                 # blocksort.c:326:0
	movl	-2144(%rbp), %eax
.LBB1_38:                               # %while.cond132
                                        #   Parent Loop BB1_27 Depth=1
                                        #     Parent Loop BB1_37 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	.loc	2 325 0                 # blocksort.c:325:0
	incq	%rax
	movl	%eax, -2144(%rbp)
	.loc	2 326 0                 # blocksort.c:326:0
	movl	-2144(%rbp), %eax
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	-64(%rbp), %rsi
	movq	$37, %rcx
	sarq	%cl, %rdx
	movl	(%rsi,%rdx,4), %edx
	andq	$31, %rax
	movq	$1, %rsi
	movq	%rax, %rcx
	shlq	%cl, %rsi
	andq	%rdx, %rsi
	xorq	%rax, %rax
	testq	%rbx, %rsi
	je	.LBB1_41
# BB#39:                                # %land.rhs
                                        #   in Loop: Header=BB1_38 Depth=3
	movzbl	-2144(%rbp), %eax
	testq	$31, %rax
	movq	%r13, %rax
	je	.LBB1_41
# BB#40:                                # %land.rhs
                                        #   in Loop: Header=BB1_38 Depth=3
	movq	%r14, %rax
.LBB1_41:                               # %land.end
                                        #   in Loop: Header=BB1_38 Depth=3
	testq	%rax, %rax
	jne	.LBB1_42
# BB#43:                                # %while.end
                                        #   in Loop: Header=BB1_37 Depth=2
	.loc	2 327 0                 # blocksort.c:327:0
.Ltmp91:
	movl	-2144(%rbp), %eax
	movq	$32, %rcx
	movq	%rax, %rdx
	movq	-64(%rbp), %rsi
	shlq	%cl, %rdx
	movq	$37, %rcx
	sarq	%cl, %rdx
	movl	(%rsi,%rdx,4), %edx
	andq	$31, %rax
	movq	$1, %rsi
	movq	%rax, %rcx
	shlq	%cl, %rsi
	andq	%rdx, %rsi
	testq	%rbx, %rsi
	je	.LBB1_48
	jmp	.LBB1_44
	.align	16, 0x90
.LBB1_45:                               # %while.body158
                                        #   in Loop: Header=BB1_44 Depth=3
	.loc	2 328 0                 # blocksort.c:328:0
.Ltmp92:
	movl	-2144(%rbp), %eax
	addq	$32, %rax
	movl	%eax, -2144(%rbp)
.LBB1_44:                               # %while.cond152
                                        #   Parent Loop BB1_27 Depth=1
                                        #     Parent Loop BB1_37 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movslq	-2144(%rbp), %rax
	movq	-64(%rbp), %rdx
	movq	$5, %rcx
	sarq	%cl, %rax
	movl	(%rdx,%rax,4), %eax
	cmpq	%rbx, %rax
	je	.LBB1_45
	jmp	.LBB1_46
	.align	16, 0x90
.LBB1_47:                               # %while.body169
                                        #   in Loop: Header=BB1_46 Depth=3
	.loc	2 329 0                 # blocksort.c:329:0
	movl	-2144(%rbp), %eax
	incq	%rax
	movl	%eax, -2144(%rbp)
.LBB1_46:                               # %while.cond161
                                        #   Parent Loop BB1_27 Depth=1
                                        #     Parent Loop BB1_37 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movl	-2144(%rbp), %eax
	movq	$32, %rcx
	movq	%rax, %rdx
	movq	-64(%rbp), %rsi
	shlq	%cl, %rdx
	movq	$37, %rcx
	sarq	%cl, %rdx
	movl	(%rsi,%rdx,4), %edx
	andq	$31, %rax
	movq	$1, %rsi
	movq	%rax, %rcx
	shlq	%cl, %rsi
	andq	%rdx, %rsi
	testq	%rbx, %rsi
	jne	.LBB1_47
.Ltmp93:
.LBB1_48:                               # %if.end172
                                        #   in Loop: Header=BB1_37 Depth=2
	.loc	2 331 0                 # blocksort.c:331:0
	movl	-2144(%rbp), %eax
	decq	%rax
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	.loc	2 332 0                 # blocksort.c:332:0
.Ltmp94:
	movslq	-68(%rbp), %rsi
	movq	$32, %rcx
.Ltmp95:
	.loc	2 331 0                 # blocksort.c:331:0
	sarq	%cl, %rdx
	movl	%eax, -2148(%rbp)
	cmpq	%rsi, %rdx
	jl	.LBB1_49
	jmp	.LBB1_66
	.align	16, 0x90
.LBB1_53:                               # %while.body190
                                        #   in Loop: Header=BB1_49 Depth=3
	.loc	2 333 0                 # blocksort.c:333:0
	movl	-2144(%rbp), %eax
	incq	%rax
	movl	%eax, -2144(%rbp)
.LBB1_49:                               # %while.cond178
                                        #   Parent Loop BB1_27 Depth=1
                                        #     Parent Loop BB1_37 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movl	-2144(%rbp), %eax
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	-64(%rbp), %rsi
	movq	$37, %rcx
	sarq	%cl, %rdx
	movl	(%rsi,%rdx,4), %edx
	andq	$31, %rax
	movq	$1, %rsi
	movq	%rax, %rcx
	shlq	%cl, %rsi
	andq	%rdx, %rsi
	xorq	%rax, %rax
	testq	%rbx, %rsi
	jne	.LBB1_52
# BB#50:                                # %land.rhs186
                                        #   in Loop: Header=BB1_49 Depth=3
	movzbl	-2144(%rbp), %eax
	testq	$31, %rax
	movq	%r13, %rax
	je	.LBB1_52
# BB#51:                                # %land.rhs186
                                        #   in Loop: Header=BB1_49 Depth=3
	movq	%r14, %rax
.LBB1_52:                               # %land.end189
                                        #   in Loop: Header=BB1_49 Depth=3
	testq	%rax, %rax
	jne	.LBB1_53
# BB#54:                                # %while.end192
                                        #   in Loop: Header=BB1_37 Depth=2
	.loc	2 334 0                 # blocksort.c:334:0
.Ltmp96:
	movl	-2144(%rbp), %eax
	movq	$32, %rcx
	movq	%rax, %rdx
	movq	-64(%rbp), %rsi
	shlq	%cl, %rdx
	movq	$37, %rcx
	sarq	%cl, %rdx
	movl	(%rsi,%rdx,4), %edx
	andq	$31, %rax
	movq	$1, %rsi
	movq	%rax, %rcx
	shlq	%cl, %rsi
	andq	%rdx, %rsi
	testq	%rbx, %rsi
	jne	.LBB1_59
	jmp	.LBB1_55
	.align	16, 0x90
.LBB1_56:                               # %while.body207
                                        #   in Loop: Header=BB1_55 Depth=3
	.loc	2 335 0                 # blocksort.c:335:0
.Ltmp97:
	movl	-2144(%rbp), %eax
	addq	$32, %rax
	movl	%eax, -2144(%rbp)
.LBB1_55:                               # %while.cond201
                                        #   Parent Loop BB1_27 Depth=1
                                        #     Parent Loop BB1_37 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movslq	-2144(%rbp), %rax
	movq	-64(%rbp), %rdx
	movq	$5, %rcx
	sarq	%cl, %rax
	movl	(%rdx,%rax,4), %eax
	testq	%rax, %rax
	je	.LBB1_56
	jmp	.LBB1_57
	.align	16, 0x90
.LBB1_58:                               # %while.body218
                                        #   in Loop: Header=BB1_57 Depth=3
	.loc	2 336 0                 # blocksort.c:336:0
	movl	-2144(%rbp), %eax
	incq	%rax
	movl	%eax, -2144(%rbp)
.LBB1_57:                               # %while.cond210
                                        #   Parent Loop BB1_27 Depth=1
                                        #     Parent Loop BB1_37 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movl	-2144(%rbp), %eax
	movq	$32, %rcx
	movq	%rax, %rdx
	movq	-64(%rbp), %rsi
	shlq	%cl, %rdx
	movq	$37, %rcx
	sarq	%cl, %rdx
	movl	(%rsi,%rdx,4), %edx
	andq	$31, %rax
	movq	$1, %rsi
	movq	%rax, %rcx
	shlq	%cl, %rsi
	andq	%rdx, %rsi
	testq	%rbx, %rsi
	je	.LBB1_58
.Ltmp98:
.LBB1_59:                               # %if.end221
                                        #   in Loop: Header=BB1_37 Depth=2
	.loc	2 338 0                 # blocksort.c:338:0
	movl	-2144(%rbp), %eax
	decq	%rax
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	.loc	2 339 0                 # blocksort.c:339:0
.Ltmp99:
	movslq	-68(%rbp), %rsi
	movq	$32, %rcx
.Ltmp100:
	.loc	2 338 0                 # blocksort.c:338:0
	sarq	%cl, %rdx
	movl	%eax, -2152(%rbp)
	cmpq	%rsi, %rdx
	jl	.LBB1_60
.Ltmp101:
.LBB1_66:                               # %while.end255
                                        #   in Loop: Header=BB1_27 Depth=1
	.loc	2 355 0                 # blocksort.c:355:0
	movslq	-72(%rbp), %rax
	cmpq	$4, %rax
	jl	.LBB1_68
# BB#67:                                # %if.then258
                                        #   in Loop: Header=BB1_27 Depth=1
	.loc	2 356 0                 # blocksort.c:356:0
	movq	stderr(%rip), %rdi
	movl	-2164(%rbp), %edx
	xorq	%rax, %rax
	movq	%r15, %rsi
	callq	fprintf
.Ltmp102:
.LBB1_68:                               # %if.end260
                                        #   in Loop: Header=BB1_27 Depth=1
	.loc	2 358 0                 # blocksort.c:358:0
	movl	-2132(%rbp), %eax
	movq	$33, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	.loc	2 359 0                 # blocksort.c:359:0
.Ltmp103:
	movslq	-68(%rbp), %rsi
.Ltmp104:
	.loc	2 358 0                 # blocksort.c:358:0
	sarq	%cl, %rdx
	addq	%rax, %rax
	movl	%eax, -2132(%rbp)
	cmpq	%rsi, %rdx
	jg	.LBB1_70
# BB#69:                                # %lor.lhs.false
                                        #   in Loop: Header=BB1_27 Depth=1
	.loc	2 359 0                 # blocksort.c:359:0
.Ltmp105:
	movl	-2164(%rbp), %eax
	testq	%rax, %rax
	jne	.LBB1_27
.Ltmp106:
.LBB1_70:                               # %while.end268
	.loc	2 367 0                 # blocksort.c:367:0
	movslq	-72(%rbp), %rax
	cmpq	$4, %rax
	jl	.LBB1_72
# BB#71:                                # %if.then271
	.loc	2 368 0                 # blocksort.c:368:0
	movq	stderr(%rip), %rdi
	movabsq	$.L.str8, %rsi
	xorq	%rax, %rax
	callq	fprintf
.Ltmp107:
.LBB1_72:                               # %if.end273
	xorq	%rax, %rax
	.loc	2 369 0                 # blocksort.c:369:0
	movl	%eax, -2140(%rbp)
	jmp	.LBB1_73
	.align	16, 0x90
.LBB1_76:                               # %for.inc292
                                        #   in Loop: Header=BB1_73 Depth=1
	.loc	2 372 0                 # blocksort.c:372:0
.Ltmp108:
	movslq	-2140(%rbp), %rax
	movl	-2128(%rbp,%rax,4), %ecx
	decq	%rcx
	.loc	2 373 0                 # blocksort.c:373:0
	movslq	-2136(%rbp), %rdx
	movq	-48(%rbp), %rsi
	.loc	2 372 0                 # blocksort.c:372:0
	movl	%ecx, -2128(%rbp,%rax,4)
	.loc	2 373 0                 # blocksort.c:373:0
	movzbl	-2140(%rbp), %eax
	movl	(%rsi,%rdx,4), %ecx
	movq	-2176(%rbp), %rdx
	movb	%al, (%rdx,%rcx)
.Ltmp109:
	.loc	2 370 0                 # blocksort.c:370:0
	movl	-2136(%rbp), %eax
	incq	%rax
.LBB1_73:                               # %for.cond274
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB1_74 Depth 2
	movl	%eax, -2136(%rbp)
	movslq	-2136(%rbp), %rax
	movslq	-68(%rbp), %rcx
	cmpq	%rcx, %rax
	jl	.LBB1_74
	jmp	.LBB1_77
	.align	16, 0x90
.LBB1_75:                               # %while.body282
                                        #   in Loop: Header=BB1_74 Depth=2
	.loc	2 371 0                 # blocksort.c:371:0
.Ltmp110:
	movl	-2140(%rbp), %eax
	incq	%rax
	movl	%eax, -2140(%rbp)
.LBB1_74:                               # %while.cond
                                        #   Parent Loop BB1_73 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movslq	-2140(%rbp), %rax
	movl	-2128(%rbp,%rax,4), %eax
	testq	%rax, %rax
	je	.LBB1_75
	jmp	.LBB1_76
.Ltmp111:
.LBB1_77:                               # %for.end294
	.loc	2 375 0                 # blocksort.c:375:0
	movslq	-2140(%rbp), %rax
	cmpq	$256, %rax              # imm = 0x100
	jl	.LBB1_79
# BB#78:                                # %if.then297
	movq	$1005, %rdi             # imm = 0x3ED
	callq	BZ2_bz__AssertH__fail
.Ltmp112:
.LBB1_79:                               # %if.end298
	.loc	2 376 0                 # blocksort.c:376:0
	addq	$2136, %rsp             # imm = 0x858
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	popq	%rbp
	retq
.Ltmp113:
.Ltmp114:
	.size	fallbackSort, .Ltmp114-fallbackSort
.Lfunc_end1:
	.cfi_endproc

	.align	16, 0x90
	.type	mainSort,@function
mainSort:                               # @mainSort
.Lfunc_begin2:
	.loc	2 805 0                 # blocksort.c:805:0
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbp
.Ltmp118:
	.cfi_def_cfa_offset 16
.Ltmp119:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
.Ltmp120:
	.cfi_def_cfa_register %rbp
	pushq	%r15
	pushq	%r14
	pushq	%r13
	pushq	%r12
	pushq	%rbx
	subq	$3464, %rsp             # imm = 0xD88
.Ltmp121:
	.cfi_offset %rbx, -56
.Ltmp122:
	.cfi_offset %r12, -48
.Ltmp123:
	.cfi_offset %r13, -40
.Ltmp124:
	.cfi_offset %r14, -32
.Ltmp125:
	.cfi_offset %r15, -24
	movq	16(%rbp), %rax
	movq	%rdi, -48(%rbp)
	movq	%rsi, -56(%rbp)
	movq	%rdx, -64(%rbp)
	movl	%r9d, -80(%rbp)
	.loc	2 814 0 prologue_end    # blocksort.c:814:0
.Ltmp126:
	movslq	-80(%rbp), %rdx
	movq	%rcx, -72(%rbp)
	movl	%r8d, -76(%rbp)
	movq	%rax, -88(%rbp)
	cmpq	$4, %rdx
	jl	.LBB2_2
# BB#1:                                 # %if.then
	movq	stderr(%rip), %rdi
	movabsq	$.L.str2, %rsi
	xorq	%rax, %rax
	callq	fprintf
.Ltmp127:
.LBB2_2:                                # %if.end
	movq	$65536, %rax            # imm = 0x10000
	.loc	2 817 0                 # blocksort.c:817:0
.Ltmp128:
	movl	%eax, -92(%rbp)
	xorq	%rax, %rax
	jmp	.LBB2_3
	.align	16, 0x90
.LBB2_4:                                # %for.inc
                                        #   in Loop: Header=BB2_3 Depth=1
	movslq	-92(%rbp), %rcx
	movq	-72(%rbp), %rdx
	movl	%eax, (%rdx,%rcx,4)
	movl	-92(%rbp), %ecx
	decq	%rcx
	movl	%ecx, -92(%rbp)
.LBB2_3:                                # %for.cond
                                        # =>This Inner Loop Header: Depth=1
	movslq	-92(%rbp), %rcx
	testq	%rcx, %rcx
	jns	.LBB2_4
.Ltmp129:
# BB#5:                                 # %for.end
	.loc	2 819 0                 # blocksort.c:819:0
	movq	-56(%rbp), %rax
	movzbl	(%rax), %eax
	movq	$8, %rcx
	.loc	2 820 0                 # blocksort.c:820:0
	movl	-76(%rbp), %edx
	.loc	2 819 0                 # blocksort.c:819:0
	shlq	%cl, %rax
	movl	%eax, -96(%rbp)
	.loc	2 820 0                 # blocksort.c:820:0
	decq	%rdx
	movl	%edx, -92(%rbp)
	xorq	%rax, %rax
	jmp	.LBB2_6
	.align	16, 0x90
.LBB2_89:                               # %for.inc58
                                        #   in Loop: Header=BB2_6 Depth=1
	.loc	2 822 0                 # blocksort.c:822:0
.Ltmp130:
	movslq	-92(%rbp), %rcx
	movq	-64(%rbp), %rdx
	movw	%ax, (%rdx,%rcx,2)
	.loc	2 823 0                 # blocksort.c:823:0
	movslq	-92(%rbp), %rcx
	movq	-56(%rbp), %rdx
	movslq	-96(%rbp), %rsi
	movzbl	(%rdx,%rcx), %edx
	movq	$8, %rcx
	shrq	%cl, %rsi
	movq	$8, %rcx
	shlq	%cl, %rdx
	orq	%rsi, %rdx
	movl	%edx, -96(%rbp)
	.loc	2 824 0                 # blocksort.c:824:0
	movslq	-96(%rbp), %rcx
	movq	-72(%rbp), %rdx
	movl	(%rdx,%rcx,4), %esi
	incq	%rsi
	movl	%esi, (%rdx,%rcx,4)
	.loc	2 825 0                 # blocksort.c:825:0
	movl	-92(%rbp), %edx
	decq	%rdx
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	-64(%rbp), %rsi
	movq	$32, %rcx
	sarq	%cl, %rdx
	movw	%ax, (%rsi,%rdx,2)
	.loc	2 826 0                 # blocksort.c:826:0
	movslq	-92(%rbp), %rcx
	movq	-56(%rbp), %rdx
	movslq	-96(%rbp), %rsi
	movzbl	-1(%rcx,%rdx), %edx
	movq	$8, %rcx
	shrq	%cl, %rsi
	movq	$8, %rcx
	shlq	%cl, %rdx
	orq	%rsi, %rdx
	movl	%edx, -96(%rbp)
	.loc	2 827 0                 # blocksort.c:827:0
	movslq	-96(%rbp), %rcx
	movq	-72(%rbp), %rdx
	movl	(%rdx,%rcx,4), %esi
	incq	%rsi
	movl	%esi, (%rdx,%rcx,4)
	.loc	2 828 0                 # blocksort.c:828:0
	movl	-92(%rbp), %edx
	addq	$-2, %rdx
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	-64(%rbp), %rsi
	movq	$32, %rcx
	sarq	%cl, %rdx
	movw	%ax, (%rsi,%rdx,2)
	.loc	2 829 0                 # blocksort.c:829:0
	movslq	-92(%rbp), %rcx
	movq	-56(%rbp), %rdx
	movslq	-96(%rbp), %rsi
	movzbl	-2(%rcx,%rdx), %edx
	movq	$8, %rcx
	shrq	%cl, %rsi
	movq	$8, %rcx
	shlq	%cl, %rdx
	orq	%rsi, %rdx
	movl	%edx, -96(%rbp)
	.loc	2 830 0                 # blocksort.c:830:0
	movslq	-96(%rbp), %rcx
	movq	-72(%rbp), %rdx
	movl	(%rdx,%rcx,4), %esi
	incq	%rsi
	movl	%esi, (%rdx,%rcx,4)
	.loc	2 831 0                 # blocksort.c:831:0
	movl	-92(%rbp), %edx
	addq	$-3, %rdx
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	-64(%rbp), %rsi
	movq	$32, %rcx
	sarq	%cl, %rdx
	movw	%ax, (%rsi,%rdx,2)
	.loc	2 832 0                 # blocksort.c:832:0
	movslq	-92(%rbp), %rcx
	movq	-56(%rbp), %rdx
	movslq	-96(%rbp), %rsi
	movzbl	-3(%rcx,%rdx), %edx
	movq	$8, %rcx
	shrq	%cl, %rsi
	movq	$8, %rcx
	shlq	%cl, %rdx
	orq	%rsi, %rdx
	movl	%edx, -96(%rbp)
	.loc	2 833 0                 # blocksort.c:833:0
	movslq	-96(%rbp), %rcx
	movq	-72(%rbp), %rdx
	movl	(%rdx,%rcx,4), %esi
	incq	%rsi
	movl	%esi, (%rdx,%rcx,4)
.Ltmp131:
	.loc	2 821 0                 # blocksort.c:821:0
	movl	-92(%rbp), %ecx
	addq	$-4, %rcx
	movl	%ecx, -92(%rbp)
.LBB2_6:                                # %for.cond3
                                        # =>This Inner Loop Header: Depth=1
	movslq	-92(%rbp), %rcx
	cmpq	$3, %rcx
	jge	.LBB2_89
.Ltmp132:
# BB#7:
	xorq	%rax, %rax
	jmp	.LBB2_8
	.align	16, 0x90
.LBB2_9:                                # %for.inc77
                                        #   in Loop: Header=BB2_8 Depth=1
	.loc	2 836 0                 # blocksort.c:836:0
.Ltmp133:
	movslq	-92(%rbp), %rcx
	movq	-64(%rbp), %rdx
	movw	%ax, (%rdx,%rcx,2)
	.loc	2 837 0                 # blocksort.c:837:0
	movslq	-92(%rbp), %rcx
	movq	-56(%rbp), %rdx
	movslq	-96(%rbp), %rsi
	movzbl	(%rdx,%rcx), %edx
	movq	$8, %rcx
	shrq	%cl, %rsi
	movq	$8, %rcx
	shlq	%cl, %rdx
	orq	%rsi, %rdx
	movl	%edx, -96(%rbp)
	.loc	2 838 0                 # blocksort.c:838:0
	movslq	-96(%rbp), %rcx
	movq	-72(%rbp), %rdx
	movl	(%rdx,%rcx,4), %esi
	incq	%rsi
	movl	%esi, (%rdx,%rcx,4)
.Ltmp134:
	.loc	2 835 0                 # blocksort.c:835:0
	movl	-92(%rbp), %ecx
	decq	%rcx
	movl	%ecx, -92(%rbp)
.LBB2_8:                                # %for.cond61
                                        # =>This Inner Loop Header: Depth=1
	movslq	-92(%rbp), %rcx
	testq	%rcx, %rcx
	jns	.LBB2_9
.Ltmp135:
# BB#10:                                # %for.end79
	xorq	%rax, %rax
	.loc	2 842 0                 # blocksort.c:842:0
.Ltmp136:
	movl	%eax, -92(%rbp)
	jmp	.LBB2_11
	.align	16, 0x90
.LBB2_12:                               # %for.inc91
                                        #   in Loop: Header=BB2_11 Depth=1
	.loc	2 843 0                 # blocksort.c:843:0
.Ltmp137:
	movslq	-92(%rbp), %rcx
	movq	-56(%rbp), %rdx
	movl	-76(%rbp), %esi
	movzbl	(%rdx,%rcx), %edi
	addq	%rcx, %rsi
	movq	$32, %rcx
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	movb	%dil, (%rdx,%rsi)
	.loc	2 844 0                 # blocksort.c:844:0
	movl	-76(%rbp), %ecx
	movl	-92(%rbp), %edx
	addq	%rcx, %rdx
	movq	$32, %rcx
	movq	-64(%rbp), %rsi
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movw	%ax, (%rsi,%rdx,2)
.Ltmp138:
	.loc	2 842 0                 # blocksort.c:842:0
	movl	-92(%rbp), %ecx
	incq	%rcx
	movl	%ecx, -92(%rbp)
.LBB2_11:                               # %for.cond80
                                        # =>This Inner Loop Header: Depth=1
	movslq	-92(%rbp), %rcx
	cmpq	$33, %rcx
	jle	.LBB2_12
.Ltmp139:
# BB#13:                                # %for.end93
	.loc	2 847 0                 # blocksort.c:847:0
	movslq	-80(%rbp), %rax
	cmpq	$4, %rax
	jl	.LBB2_15
# BB#14:                                # %if.then96
	movq	stderr(%rip), %rdi
	movabsq	$.L.str3, %rsi
	xorq	%rax, %rax
	callq	fprintf
.Ltmp140:
.LBB2_15:                               # %if.end98
	movq	$1, %rax
	jmp	.LBB2_16
	.align	16, 0x90
.LBB2_17:                               # %for.inc109
                                        #   in Loop: Header=BB2_16 Depth=1
	.loc	2 850 0                 # blocksort.c:850:0
.Ltmp141:
	movslq	-92(%rbp), %rax
	leaq	-1(%rax), %rdx
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	-72(%rbp), %rsi
	movq	$32, %rcx
	sarq	%cl, %rdx
	movl	(%rsi,%rdx,4), %ecx
	movl	(%rsi,%rax,4), %edx
	addq	%rcx, %rdx
	movl	%edx, (%rsi,%rax,4)
	movl	-92(%rbp), %eax
	incq	%rax
.LBB2_16:                               # %for.cond99
                                        # =>This Inner Loop Header: Depth=1
	movl	%eax, -92(%rbp)
	movslq	-92(%rbp), %rax
	cmpq	$65536, %rax            # imm = 0x10000
	jle	.LBB2_17
.Ltmp142:
# BB#18:                                # %for.end111
	.loc	2 852 0                 # blocksort.c:852:0
	movq	-56(%rbp), %rax
	movzbl	(%rax), %eax
	.loc	2 853 0                 # blocksort.c:853:0
	movl	-76(%rbp), %edx
	movq	$8, %rcx
	.loc	2 852 0                 # blocksort.c:852:0
	shlq	%cl, %rax
	movw	%ax, -3450(%rbp)
	.loc	2 853 0                 # blocksort.c:853:0
	decq	%rdx
	movl	%edx, -92(%rbp)
	jmp	.LBB2_19
	.align	16, 0x90
.LBB2_20:                               # %for.inc187
                                        #   in Loop: Header=BB2_19 Depth=1
	.loc	2 855 0                 # blocksort.c:855:0
.Ltmp143:
	movslq	-92(%rbp), %rax
	movq	-56(%rbp), %rcx
	movzbl	-3450(%rbp), %edx
	movzbl	(%rcx,%rax), %eax
	movq	$8, %rcx
	shrq	%cl, %rdx
	movq	$8, %rcx
	shlq	%cl, %rax
	orq	%rdx, %rax
	movw	%ax, -3450(%rbp)
	.loc	2 856 0                 # blocksort.c:856:0
	movzbl	-3450(%rbp), %eax
	movq	-72(%rbp), %rcx
	movl	(%rcx,%rax,4), %eax
	.loc	2 857 0                 # blocksort.c:857:0
	movzbl	-3450(%rbp), %ecx
	movq	-72(%rbp), %rdx
	.loc	2 856 0                 # blocksort.c:856:0
	decq	%rax
	movl	%eax, -96(%rbp)
	.loc	2 857 0                 # blocksort.c:857:0
	movl	%eax, (%rdx,%rcx,4)
	.loc	2 858 0                 # blocksort.c:858:0
	movl	-92(%rbp), %eax
	movslq	-96(%rbp), %rcx
	movq	-48(%rbp), %rdx
	movl	%eax, (%rdx,%rcx,4)
	.loc	2 859 0                 # blocksort.c:859:0
	movslq	-92(%rbp), %rax
	movq	-56(%rbp), %rcx
	movzbl	-3450(%rbp), %edx
	movzbl	-1(%rax,%rcx), %eax
	movq	$8, %rcx
	shrq	%cl, %rdx
	movq	$8, %rcx
	shlq	%cl, %rax
	orq	%rdx, %rax
	movw	%ax, -3450(%rbp)
	.loc	2 860 0                 # blocksort.c:860:0
	movzbl	-3450(%rbp), %eax
	movq	-72(%rbp), %rcx
	movl	(%rcx,%rax,4), %eax
	.loc	2 861 0                 # blocksort.c:861:0
	movzbl	-3450(%rbp), %ecx
	movq	-72(%rbp), %rdx
	.loc	2 860 0                 # blocksort.c:860:0
	decq	%rax
	movl	%eax, -96(%rbp)
	.loc	2 861 0                 # blocksort.c:861:0
	movl	%eax, (%rdx,%rcx,4)
	.loc	2 862 0                 # blocksort.c:862:0
	movl	-92(%rbp), %eax
	movslq	-96(%rbp), %rcx
	movq	-48(%rbp), %rdx
	decq	%rax
	movl	%eax, (%rdx,%rcx,4)
	.loc	2 863 0                 # blocksort.c:863:0
	movslq	-92(%rbp), %rax
	movq	-56(%rbp), %rcx
	movzbl	-3450(%rbp), %edx
	movzbl	-2(%rax,%rcx), %eax
	movq	$8, %rcx
	shrq	%cl, %rdx
	movq	$8, %rcx
	shlq	%cl, %rax
	orq	%rdx, %rax
	movw	%ax, -3450(%rbp)
	.loc	2 864 0                 # blocksort.c:864:0
	movzbl	-3450(%rbp), %eax
	movq	-72(%rbp), %rcx
	movl	(%rcx,%rax,4), %eax
	.loc	2 865 0                 # blocksort.c:865:0
	movzbl	-3450(%rbp), %ecx
	movq	-72(%rbp), %rdx
	.loc	2 864 0                 # blocksort.c:864:0
	decq	%rax
	movl	%eax, -96(%rbp)
	.loc	2 865 0                 # blocksort.c:865:0
	movl	%eax, (%rdx,%rcx,4)
	.loc	2 866 0                 # blocksort.c:866:0
	movl	-92(%rbp), %eax
	movslq	-96(%rbp), %rcx
	movq	-48(%rbp), %rdx
	addq	$-2, %rax
	movl	%eax, (%rdx,%rcx,4)
	.loc	2 867 0                 # blocksort.c:867:0
	movslq	-92(%rbp), %rax
	movq	-56(%rbp), %rcx
	movzbl	-3450(%rbp), %edx
	movzbl	-3(%rax,%rcx), %eax
	movq	$8, %rcx
	shrq	%cl, %rdx
	movq	$8, %rcx
	shlq	%cl, %rax
	orq	%rdx, %rax
	movw	%ax, -3450(%rbp)
	.loc	2 868 0                 # blocksort.c:868:0
	movzbl	-3450(%rbp), %eax
	movq	-72(%rbp), %rcx
	movl	(%rcx,%rax,4), %eax
	.loc	2 869 0                 # blocksort.c:869:0
	movzbl	-3450(%rbp), %ecx
	movq	-72(%rbp), %rdx
	.loc	2 868 0                 # blocksort.c:868:0
	decq	%rax
	movl	%eax, -96(%rbp)
	.loc	2 869 0                 # blocksort.c:869:0
	movl	%eax, (%rdx,%rcx,4)
	.loc	2 870 0                 # blocksort.c:870:0
	movl	-92(%rbp), %eax
	movslq	-96(%rbp), %rcx
	movq	-48(%rbp), %rdx
	addq	$-3, %rax
	movl	%eax, (%rdx,%rcx,4)
.Ltmp144:
	.loc	2 854 0                 # blocksort.c:854:0
	movl	-92(%rbp), %eax
	addq	$-4, %rax
	movl	%eax, -92(%rbp)
.LBB2_19:                               # %for.cond117
                                        # =>This Inner Loop Header: Depth=1
	movslq	-92(%rbp), %rax
	cmpq	$3, %rax
	jge	.LBB2_20
	jmp	.LBB2_21
.Ltmp145:
	.align	16, 0x90
.LBB2_22:                               # %for.inc209
                                        #   in Loop: Header=BB2_21 Depth=1
	.loc	2 873 0                 # blocksort.c:873:0
	movslq	-92(%rbp), %rax
	movq	-56(%rbp), %rcx
	movzbl	-3450(%rbp), %edx
	movzbl	(%rcx,%rax), %eax
	movq	$8, %rcx
	shrq	%cl, %rdx
	movq	$8, %rcx
	shlq	%cl, %rax
	orq	%rdx, %rax
	movw	%ax, -3450(%rbp)
	.loc	2 874 0                 # blocksort.c:874:0
	movzbl	-3450(%rbp), %eax
	movq	-72(%rbp), %rcx
	movl	(%rcx,%rax,4), %eax
	.loc	2 875 0                 # blocksort.c:875:0
	movzbl	-3450(%rbp), %ecx
	movq	-72(%rbp), %rdx
	.loc	2 874 0                 # blocksort.c:874:0
	decq	%rax
	movl	%eax, -96(%rbp)
	.loc	2 875 0                 # blocksort.c:875:0
	movl	%eax, (%rdx,%rcx,4)
	.loc	2 876 0                 # blocksort.c:876:0
	movl	-92(%rbp), %eax
	movslq	-96(%rbp), %rcx
	movq	-48(%rbp), %rdx
	movl	%eax, (%rdx,%rcx,4)
.Ltmp146:
	.loc	2 872 0                 # blocksort.c:872:0
	movl	-92(%rbp), %eax
	decq	%rax
	movl	%eax, -92(%rbp)
.LBB2_21:                               # %for.cond190
                                        # =>This Inner Loop Header: Depth=1
	movslq	-92(%rbp), %rax
	testq	%rax, %rax
	jns	.LBB2_22
.Ltmp147:
# BB#23:                                # %for.end211
	xorq	%rax, %rax
	.loc	2 884 0                 # blocksort.c:884:0
.Ltmp148:
	movl	%eax, -92(%rbp)
	jmp	.LBB2_24
	.align	16, 0x90
.LBB2_25:                               # %for.inc220
                                        #   in Loop: Header=BB2_24 Depth=1
	.loc	2 885 0                 # blocksort.c:885:0
.Ltmp149:
	movslq	-92(%rbp), %rcx
.Ltmp150:
	.loc	2 884 0                 # blocksort.c:884:0
	movl	-92(%rbp), %edx
	.loc	2 886 0                 # blocksort.c:886:0
.Ltmp151:
	movslq	-92(%rbp), %rsi
	.loc	2 885 0                 # blocksort.c:885:0
	movb	%al, -1392(%rbp,%rcx)
	.loc	2 886 0                 # blocksort.c:886:0
	movl	%esi, -1136(%rbp,%rsi,4)
.Ltmp152:
	.loc	2 884 0                 # blocksort.c:884:0
	incq	%rdx
	movl	%edx, -92(%rbp)
.LBB2_24:                               # %for.cond212
                                        # =>This Inner Loop Header: Depth=1
	movslq	-92(%rbp), %rcx
	cmpq	$255, %rcx
	jle	.LBB2_25
.Ltmp153:
# BB#26:                                # %for.end222
	#DEBUG_VALUE: vv <- [RBP+-3456]
	#DEBUG_VALUE: h <- [RBP+-3460]
	movq	$1, %rax
	.loc	2 891 0                 # blocksort.c:891:0
.Ltmp154:
	movl	%eax, -3460(%rbp)
	.align	16, 0x90
.LBB2_27:                               # %do.cond
                                        # =>This Inner Loop Header: Depth=1
.Ltmp155:
	#DEBUG_VALUE: vv <- [RBP+-3456]
	#DEBUG_VALUE: h <- [RBP+-3460]
	.loc	2 892 0                 # blocksort.c:892:0
	movl	-3460(%rbp), %eax
	leaq	1(%rax,%rax,2), %rax
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	$257, %rdx              # imm = 0x101
	movl	%eax, -3460(%rbp)
	jl	.LBB2_27
.Ltmp156:
# BB#28:
	movabsq	$6148914691236517206, %r8 # imm = 0x5555555555555556
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB2_29:                               # %do.body226
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB2_30 Depth 2
                                        #       Child Loop BB2_32 Depth 3
	.loc	2 894 0                 # blocksort.c:894:0
.Ltmp157:
	movslq	-3460(%rbp), %rax
	imulq	%r8
	movq	$63, %rcx
	movq	%rdx, %rax
	shrq	%cl, %rax
	addq	%rdx, %rax
	movl	%eax, -3460(%rbp)
	.loc	2 895 0                 # blocksort.c:895:0
.Ltmp158:
	movl	%eax, -92(%rbp)
	jmp	.LBB2_30
	.align	16, 0x90
.LBB2_34:                               # %for.inc270
                                        #   in Loop: Header=BB2_30 Depth=2
	.loc	2 904 0                 # blocksort.c:904:0
.Ltmp159:
	movl	-3456(%rbp), %eax
.Ltmp160:
	.loc	2 895 0                 # blocksort.c:895:0
	movl	-92(%rbp), %ecx
	.loc	2 904 0                 # blocksort.c:904:0
.Ltmp161:
	movslq	-96(%rbp), %rdx
	movl	%eax, -1136(%rbp,%rdx,4)
.Ltmp162:
	.loc	2 895 0                 # blocksort.c:895:0
	incq	%rcx
	movl	%ecx, -92(%rbp)
.LBB2_30:                               # %for.cond227
                                        #   Parent Loop BB2_29 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB2_32 Depth 3
	movslq	-92(%rbp), %rax
	cmpq	$255, %rax
	jg	.LBB2_35
# BB#31:                                # %for.body230
                                        #   in Loop: Header=BB2_30 Depth=2
	.loc	2 896 0                 # blocksort.c:896:0
.Ltmp163:
	movslq	-92(%rbp), %rax
	movl	-1136(%rbp,%rax,4), %eax
	.loc	2 897 0                 # blocksort.c:897:0
	movl	-92(%rbp), %ecx
	.loc	2 896 0                 # blocksort.c:896:0
	movl	%eax, -3456(%rbp)
	.loc	2 897 0                 # blocksort.c:897:0
	movl	%ecx, -96(%rbp)
	.align	16, 0x90
.LBB2_32:                               # %while.cond
                                        #   Parent Loop BB2_29 Depth=1
                                        #     Parent Loop BB2_30 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	.loc	2 898 0                 # blocksort.c:898:0
	movl	-96(%rbp), %eax
	movl	-3460(%rbp), %ecx
	subq	%rcx, %rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movl	-1136(%rbp,%rax,4), %eax
	leaq	1(%rax), %rdx
	movq	$40, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	-72(%rbp), %rdi
	movq	$40, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	movl	-3456(%rbp), %esi
	sarq	%cl, %rax
	leaq	1(%rsi), %rbx
	movq	$40, %rcx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	movq	$40, %rcx
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	movl	(%rdi,%rdx,4), %ecx
	movl	(%rdi,%rax,4), %eax
	movl	(%rdi,%rbx,4), %edx
	movl	(%rdi,%rsi,4), %esi
	subq	%rax, %rcx
	andq	%r13, %rcx
	subq	%rsi, %rdx
	andq	%r13, %rdx
	cmpq	%rdx, %rcx
	jbe	.LBB2_34
# BB#33:                                # %while.body
                                        #   in Loop: Header=BB2_32 Depth=3
	.loc	2 899 0                 # blocksort.c:899:0
.Ltmp164:
	movslq	-96(%rbp), %rax
	movl	-3460(%rbp), %ecx
	movq	%rax, %rdx
	subq	%rcx, %rdx
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	.loc	2 900 0                 # blocksort.c:900:0
	movl	-96(%rbp), %esi
	movl	-3460(%rbp), %ecx
	.loc	2 899 0                 # blocksort.c:899:0
	movl	-1136(%rbp,%rdx,4), %edx
	movl	%edx, -1136(%rbp,%rax,4)
	.loc	2 900 0                 # blocksort.c:900:0
	subq	%rcx, %rsi
	movq	$32, %rcx
	movq	%rsi, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	.loc	2 901 0                 # blocksort.c:901:0
.Ltmp165:
	movl	-3460(%rbp), %edx
.Ltmp166:
	.loc	2 900 0                 # blocksort.c:900:0
	movl	%esi, -96(%rbp)
	.loc	2 901 0                 # blocksort.c:901:0
.Ltmp167:
	decq	%rdx
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
.Ltmp168:
	.loc	2 900 0                 # blocksort.c:900:0
	cmpq	%rdx, %rax
	jg	.LBB2_32
	jmp	.LBB2_34
.Ltmp169:
	.align	16, 0x90
.LBB2_35:                               # %do.cond273
                                        #   in Loop: Header=BB2_29 Depth=1
	.loc	2 906 0                 # blocksort.c:906:0
	movl	-3460(%rbp), %eax
	cmpq	$1, %rax
	jne	.LBB2_29
.Ltmp170:
# BB#36:                                # %do.end276
	xorq	%r15, %r15
	.loc	2 913 0                 # blocksort.c:913:0
	movl	%r15d, -3448(%rbp)
	.loc	2 915 0                 # blocksort.c:915:0
.Ltmp171:
	movl	%r15d, -92(%rbp)
	movabsq	$4292870143, %r12       # imm = 0xFFDFFFFF
	.loc	2 941 0                 # blocksort.c:941:0
.Ltmp172:
	movabsq	$.L.str4, %r14
	movq	$1, %rbx
	jmp	.LBB2_37
.Ltmp173:
	.align	16, 0x90
.LBB2_85:                               # %for.inc506
                                        #   in Loop: Header=BB2_37 Depth=1
	.loc	2 915 0                 # blocksort.c:915:0
	movl	-92(%rbp), %eax
	incq	%rax
	movl	%eax, -92(%rbp)
.LBB2_37:                               # %for.cond277
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB2_39 Depth 2
                                        #     Child Loop BB2_51 Depth 2
                                        #     Child Loop BB2_54 Depth 2
                                        #     Child Loop BB2_61 Depth 2
                                        #     Child Loop BB2_72 Depth 2
                                        #     Child Loop BB2_76 Depth 2
                                        #     Child Loop BB2_79 Depth 2
	movslq	-92(%rbp), %rax
	cmpq	$255, %rax
	jg	.LBB2_86
# BB#38:                                # %for.body280
                                        #   in Loop: Header=BB2_37 Depth=1
	.loc	2 923 0                 # blocksort.c:923:0
.Ltmp174:
	movslq	-92(%rbp), %rax
	movl	-1136(%rbp,%rax,4), %eax
	movl	%eax, -104(%rbp)
	.loc	2 933 0                 # blocksort.c:933:0
.Ltmp175:
	movl	%r15d, -96(%rbp)
	jmp	.LBB2_39
	.align	16, 0x90
.LBB2_47:                               # %for.inc326
                                        #   in Loop: Header=BB2_39 Depth=2
	movl	-96(%rbp), %eax
	incq	%rax
	movl	%eax, -96(%rbp)
.LBB2_39:                               # %for.cond283
                                        #   Parent Loop BB2_37 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movslq	-96(%rbp), %rax
	cmpq	$255, %rax
	jg	.LBB2_48
# BB#40:                                # %for.body286
                                        #   in Loop: Header=BB2_39 Depth=2
	.loc	2 934 0                 # blocksort.c:934:0
.Ltmp176:
	movl	-96(%rbp), %eax
	movl	-104(%rbp), %ecx
	cmpq	%rcx, %rax
	je	.LBB2_47
# BB#41:                                # %if.then289
                                        #   in Loop: Header=BB2_39 Depth=2
	.loc	2 935 0                 # blocksort.c:935:0
.Ltmp177:
	movl	-104(%rbp), %eax
	movl	-96(%rbp), %edx
	movq	$8, %rcx
	shlq	%cl, %rax
	addq	%rax, %rdx
	movl	%edx, -108(%rbp)
	.loc	2 936 0                 # blocksort.c:936:0
.Ltmp178:
	movslq	-108(%rbp), %rax
	movq	-72(%rbp), %rcx
	movzbl	2(%rcx,%rax,4), %eax
	testq	$32, %rax
	jne	.LBB2_46
# BB#42:                                # %if.then294
                                        #   in Loop: Header=BB2_39 Depth=2
.Ltmp179:
	#DEBUG_VALUE: lo <- [RBP+-3464]
	.loc	2 937 0                 # blocksort.c:937:0
	movslq	-108(%rbp), %rax
	movq	-72(%rbp), %rcx
	movl	(%rcx,%rax,4), %eax
	andq	%r12, %rax
	movl	%eax, -3464(%rbp)
.Ltmp180:
	#DEBUG_VALUE: hi <- [RBP+-3468]
	.loc	2 938 0                 # blocksort.c:938:0
	movl	-108(%rbp), %eax
	incq	%rax
	movq	$32, %rcx
	movq	-72(%rbp), %rdx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movl	(%rdx,%rax,4), %eax
	andq	$-2097153, %rax         # imm = 0xFFFFFFFFFFDFFFFF
	decq	%rax
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	.loc	2 939 0                 # blocksort.c:939:0
.Ltmp181:
	movslq	-3464(%rbp), %rsi
	movq	$32, %rcx
.Ltmp182:
	.loc	2 938 0                 # blocksort.c:938:0
	sarq	%cl, %rdx
	movl	%eax, -3468(%rbp)
	cmpq	%rsi, %rdx
	jle	.LBB2_46
# BB#43:                                # %if.then305
                                        #   in Loop: Header=BB2_39 Depth=2
	#DEBUG_VALUE: lo <- [RBP+-3464]
	#DEBUG_VALUE: hi <- [RBP+-3468]
	.loc	2 940 0                 # blocksort.c:940:0
.Ltmp183:
	movslq	-80(%rbp), %rax
	cmpq	$4, %rax
	jl	.LBB2_45
# BB#44:                                # %if.then308
                                        #   in Loop: Header=BB2_39 Depth=2
	#DEBUG_VALUE: lo <- [RBP+-3464]
	#DEBUG_VALUE: hi <- [RBP+-3468]
	.loc	2 941 0                 # blocksort.c:941:0
	movq	stderr(%rip), %rdi
	movl	-3468(%rbp), %r9d
	movl	-3464(%rbp), %eax
	movl	-3448(%rbp), %r8d
	movl	-96(%rbp), %ecx
	movl	-104(%rbp), %edx
	subq	%rax, %r9
	incq	%r9
	xorq	%rax, %rax
	movq	%r14, %rsi
	callq	fprintf
.Ltmp184:
.LBB2_45:                               # %if.end312
                                        #   in Loop: Header=BB2_39 Depth=2
	#DEBUG_VALUE: lo <- [RBP+-3464]
	#DEBUG_VALUE: hi <- [RBP+-3468]
	.loc	2 944 0                 # blocksort.c:944:0
	movq	-48(%rbp), %rdi
	movq	-56(%rbp), %rsi
	movq	-64(%rbp), %rdx
	movq	-88(%rbp), %rax
	movl	-3468(%rbp), %r9d
	movl	-3464(%rbp), %r8d
	movl	-76(%rbp), %ecx
	movq	%rax, 8(%rsp)
	movq	$2, (%rsp)
	callq	mainQSort3
	.loc	2 948 0                 # blocksort.c:948:0
	movl	-3468(%rbp), %eax
	movl	-3464(%rbp), %ecx
	movl	-3448(%rbp), %edx
	.loc	2 949 0                 # blocksort.c:949:0
.Ltmp185:
	movq	-88(%rbp), %rsi
.Ltmp186:
	.loc	2 948 0                 # blocksort.c:948:0
	subq	%rcx, %rax
	leaq	1(%rax,%rdx), %rax
	movl	%eax, -3448(%rbp)
	.loc	2 949 0                 # blocksort.c:949:0
.Ltmp187:
	movslq	(%rsi), %rax
	testq	%rax, %rax
	js	.LBB2_88
.Ltmp188:
.LBB2_46:                               # %if.end321
                                        #   in Loop: Header=BB2_39 Depth=2
	.loc	2 952 0                 # blocksort.c:952:0
	movslq	-108(%rbp), %rax
	movq	-72(%rbp), %rcx
	movl	(%rcx,%rax,4), %edx
	orq	$2097152, %rdx          # imm = 0x200000
	movl	%edx, (%rcx,%rax,4)
	jmp	.LBB2_47
.Ltmp189:
	.align	16, 0x90
.LBB2_48:                               # %for.end328
                                        #   in Loop: Header=BB2_37 Depth=1
	.loc	2 956 0                 # blocksort.c:956:0
	movslq	-104(%rbp), %rax
	movzbl	-1392(%rbp,%rax), %eax
	testq	%rax, %rax
	je	.LBB2_50
# BB#49:                                # %if.then332
                                        #   in Loop: Header=BB2_37 Depth=1
	movq	$1006, %rdi             # imm = 0x3EE
	callq	BZ2_bz__AssertH__fail
.Ltmp190:
.LBB2_50:                               # %if.end333
                                        #   in Loop: Header=BB2_37 Depth=1
	.loc	2 966 0                 # blocksort.c:966:0
	movl	%r15d, -96(%rbp)
	jmp	.LBB2_51
	.align	16, 0x90
.LBB2_52:                               # %for.inc354
                                        #   in Loop: Header=BB2_51 Depth=2
	.loc	2 967 0                 # blocksort.c:967:0
.Ltmp191:
	movslq	-96(%rbp), %rax
	movq	$8, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movl	-104(%rbp), %esi
	addq	%rdx, %rsi
	movq	$32, %rcx
	shlq	%cl, %rsi
	movq	$32, %rcx
	movq	-72(%rbp), %rdx
	sarq	%cl, %rsi
	movl	(%rdx,%rsi,4), %ecx
	andq	%r12, %rcx
	movl	%ecx, -2416(%rbp,%rax,4)
	.loc	2 968 0                 # blocksort.c:968:0
	movslq	-96(%rbp), %rax
	movq	$8, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movl	-104(%rbp), %ecx
	leaq	1(%rdx,%rcx), %rdx
	movq	$32, %rcx
	movq	-72(%rbp), %rsi
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movl	(%rsi,%rdx,4), %ecx
.Ltmp192:
	.loc	2 966 0                 # blocksort.c:966:0
	movl	-96(%rbp), %edx
	.loc	2 968 0                 # blocksort.c:968:0
.Ltmp193:
	andq	%r12, %rcx
	decq	%rcx
	movl	%ecx, -3440(%rbp,%rax,4)
.Ltmp194:
	.loc	2 966 0                 # blocksort.c:966:0
	incq	%rdx
	movl	%edx, -96(%rbp)
.LBB2_51:                               # %for.cond334
                                        #   Parent Loop BB2_37 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movslq	-96(%rbp), %rax
	cmpq	$255, %rax
	jle	.LBB2_52
.Ltmp195:
# BB#53:                                # %for.end356
                                        #   in Loop: Header=BB2_37 Depth=1
	.loc	2 970 0                 # blocksort.c:970:0
	movl	-104(%rbp), %eax
	movq	$40, %rcx
	movq	-72(%rbp), %rdx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movl	(%rdx,%rax,4), %eax
	andq	%r12, %rax
	jmp	.LBB2_54
	.align	16, 0x90
.LBB2_59:                               # %for.inc387
                                        #   in Loop: Header=BB2_54 Depth=2
	movl	-96(%rbp), %eax
	incq	%rax
.LBB2_54:                               # %for.cond361
                                        #   Parent Loop BB2_37 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movl	%eax, -96(%rbp)
	movslq	-104(%rbp), %rax
	movslq	-96(%rbp), %rcx
	movslq	-2416(%rbp,%rax,4), %rax
	cmpq	%rax, %rcx
	jge	.LBB2_60
# BB#55:                                # %for.body366
                                        #   in Loop: Header=BB2_54 Depth=2
	.loc	2 971 0                 # blocksort.c:971:0
.Ltmp196:
	movslq	-96(%rbp), %rax
	movq	-48(%rbp), %rcx
	movl	(%rcx,%rax,4), %eax
	decq	%rax
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	testq	%rdx, %rdx
	movl	%eax, -100(%rbp)
	jns	.LBB2_57
# BB#56:                                # %if.then372
                                        #   in Loop: Header=BB2_54 Depth=2
	.loc	2 971 0                 # blocksort.c:971:0
.Ltmp197:
	movl	-76(%rbp), %eax
	movl	-100(%rbp), %ecx
	addq	%rax, %rcx
	movl	%ecx, -100(%rbp)
.Ltmp198:
.LBB2_57:                               # %if.end374
                                        #   in Loop: Header=BB2_54 Depth=2
	.loc	2 972 0                 # blocksort.c:972:0
	movslq	-100(%rbp), %rax
	movq	-56(%rbp), %rcx
	movzbl	(%rcx,%rax), %eax
	movb	%al, -3441(%rbp)
	.loc	2 973 0                 # blocksort.c:973:0
.Ltmp199:
	movzbl	-3441(%rbp), %eax
	movzbl	-1392(%rbp,%rax), %eax
	testq	%rax, %rax
	jne	.LBB2_59
# BB#58:                                # %if.then380
                                        #   in Loop: Header=BB2_54 Depth=2
	.loc	2 974 0                 # blocksort.c:974:0
	movzbl	-3441(%rbp), %eax
	movl	-100(%rbp), %ecx
	movslq	-2416(%rbp,%rax,4), %rdx
	movq	-48(%rbp), %rsi
	leaq	1(%rdx), %rdi
	movl	%edi, -2416(%rbp,%rax,4)
	movl	%ecx, (%rsi,%rdx,4)
	jmp	.LBB2_59
.Ltmp200:
	.align	16, 0x90
.LBB2_60:                               # %for.end389
                                        #   in Loop: Header=BB2_37 Depth=1
	.loc	2 976 0                 # blocksort.c:976:0
	movl	-104(%rbp), %eax
	incq	%rax
	movq	$40, %rcx
	movq	-72(%rbp), %rdx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movl	(%rdx,%rax,4), %eax
	andq	%r12, %rax
	jmp	.LBB2_61
	.align	16, 0x90
.LBB2_66:                               # %for.inc422
                                        #   in Loop: Header=BB2_61 Depth=2
	movl	-96(%rbp), %eax
.LBB2_61:                               # %for.cond396
                                        #   Parent Loop BB2_37 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	decq	%rax
	movl	%eax, -96(%rbp)
	movslq	-104(%rbp), %rax
	movslq	-96(%rbp), %rcx
	movslq	-3440(%rbp,%rax,4), %rax
	cmpq	%rax, %rcx
	jle	.LBB2_67
# BB#62:                                # %for.body401
                                        #   in Loop: Header=BB2_61 Depth=2
	.loc	2 977 0                 # blocksort.c:977:0
.Ltmp201:
	movslq	-96(%rbp), %rax
	movq	-48(%rbp), %rcx
	movl	(%rcx,%rax,4), %eax
	decq	%rax
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	testq	%rdx, %rdx
	movl	%eax, -100(%rbp)
	jns	.LBB2_64
# BB#63:                                # %if.then407
                                        #   in Loop: Header=BB2_61 Depth=2
	.loc	2 977 0                 # blocksort.c:977:0
.Ltmp202:
	movl	-76(%rbp), %eax
	movl	-100(%rbp), %ecx
	addq	%rax, %rcx
	movl	%ecx, -100(%rbp)
.Ltmp203:
.LBB2_64:                               # %if.end409
                                        #   in Loop: Header=BB2_61 Depth=2
	.loc	2 978 0                 # blocksort.c:978:0
	movslq	-100(%rbp), %rax
	movq	-56(%rbp), %rcx
	movzbl	(%rcx,%rax), %eax
	movb	%al, -3441(%rbp)
	.loc	2 979 0                 # blocksort.c:979:0
.Ltmp204:
	movzbl	-3441(%rbp), %eax
	movzbl	-1392(%rbp,%rax), %eax
	testq	%rax, %rax
	jne	.LBB2_66
# BB#65:                                # %if.then415
                                        #   in Loop: Header=BB2_61 Depth=2
	.loc	2 980 0                 # blocksort.c:980:0
	movzbl	-3441(%rbp), %eax
	movl	-100(%rbp), %ecx
	movslq	-3440(%rbp,%rax,4), %rdx
	movq	-48(%rbp), %rsi
	leaq	-1(%rdx), %rdi
	movl	%edi, -3440(%rbp,%rax,4)
	movl	%ecx, (%rsi,%rdx,4)
	jmp	.LBB2_66
.Ltmp205:
	.align	16, 0x90
.LBB2_67:                               # %for.end424
                                        #   in Loop: Header=BB2_37 Depth=1
	.loc	2 984 0                 # blocksort.c:984:0
	movslq	-104(%rbp), %rax
	movl	-2416(%rbp,%rax,4), %ecx
	movl	-3440(%rbp,%rax,4), %eax
	decq	%rcx
	andq	%r13, %rcx
	cmpq	%rax, %rcx
	je	.LBB2_71
# BB#68:                                # %lor.lhs.false
                                        #   in Loop: Header=BB2_37 Depth=1
	movslq	-104(%rbp), %rax
	movl	-2416(%rbp,%rax,4), %eax
	testq	%rax, %rax
	jne	.LBB2_70
# BB#69:                                # %land.lhs.true
                                        #   in Loop: Header=BB2_37 Depth=1
	movslq	-104(%rbp), %rax
	movl	-76(%rbp), %ecx
	movl	-3440(%rbp,%rax,4), %eax
	decq	%rcx
	andq	%r13, %rcx
	cmpq	%rcx, %rax
	je	.LBB2_71
.LBB2_70:                               # %if.then441
                                        #   in Loop: Header=BB2_37 Depth=1
	movq	$1007, %rdi             # imm = 0x3EF
	callq	BZ2_bz__AssertH__fail
.Ltmp206:
.LBB2_71:                               # %if.end442
                                        #   in Loop: Header=BB2_37 Depth=1
	.loc	2 993 0                 # blocksort.c:993:0
	movl	%r15d, -96(%rbp)
	jmp	.LBB2_72
	.align	16, 0x90
.LBB2_73:                               # %for.inc452
                                        #   in Loop: Header=BB2_72 Depth=2
	movl	-96(%rbp), %eax
	movq	$8, %rcx
	shlq	%cl, %rax
	movl	-104(%rbp), %edx
	addq	%rax, %rdx
	movq	$32, %rcx
	movq	-72(%rbp), %rax
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movl	(%rax,%rdx,4), %ecx
	orq	$2097152, %rcx          # imm = 0x200000
	movl	%ecx, (%rax,%rdx,4)
	movl	-96(%rbp), %eax
	incq	%rax
	movl	%eax, -96(%rbp)
.LBB2_72:                               # %for.cond443
                                        #   Parent Loop BB2_37 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movslq	-96(%rbp), %rax
	cmpq	$255, %rax
	jle	.LBB2_73
.Ltmp207:
# BB#74:                                # %for.end454
                                        #   in Loop: Header=BB2_37 Depth=1
	.loc	2 1034 0                # blocksort.c:1034:0
	movslq	-104(%rbp), %rax
	.loc	2 1036 0                # blocksort.c:1036:0
.Ltmp208:
	movslq	-92(%rbp), %rcx
.Ltmp209:
	.loc	2 1034 0                # blocksort.c:1034:0
	movb	%bl, -1392(%rbp,%rax)
	.loc	2 1036 0                # blocksort.c:1036:0
.Ltmp210:
	cmpq	$254, %rcx
	jg	.LBB2_85
# BB#75:                                # %if.then459
                                        #   in Loop: Header=BB2_37 Depth=1
.Ltmp211:
	#DEBUG_VALUE: bbStart <- [RBP+-3472]
	.loc	2 1037 0                # blocksort.c:1037:0
	movl	-104(%rbp), %eax
	movq	$40, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	-72(%rbp), %rcx
	movl	(%rcx,%rax,4), %eax
	.loc	2 1038 0                # blocksort.c:1038:0
	movl	-104(%rbp), %edx
	incq	%rdx
	movq	$40, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	movq	-72(%rbp), %rsi
	sarq	%cl, %rdx
	.loc	2 1037 0                # blocksort.c:1037:0
	andq	$-2097153, %rax         # imm = 0xFFFFFFFFFFDFFFFF
	movl	%eax, -3472(%rbp)
.Ltmp212:
	#DEBUG_VALUE: bbSize <- [RBP+-3476]
	.loc	2 1038 0                # blocksort.c:1038:0
	movl	(%rsi,%rdx,4), %ecx
	andq	%r12, %rcx
	subq	%rax, %rcx
	movl	%ecx, -3476(%rbp)
.Ltmp213:
	#DEBUG_VALUE: shifts <- [RBP+-3480]
	.loc	2 1039 0                # blocksort.c:1039:0
	movl	%r15d, -3480(%rbp)
	jmp	.LBB2_76
	.align	16, 0x90
.LBB2_77:                               # %while.body474
                                        #   in Loop: Header=BB2_76 Depth=2
	#DEBUG_VALUE: bbStart <- [RBP+-3472]
	#DEBUG_VALUE: bbSize <- [RBP+-3476]
	#DEBUG_VALUE: shifts <- [RBP+-3480]
	.loc	2 1041 0                # blocksort.c:1041:0
	movl	-3480(%rbp), %eax
	incq	%rax
	movl	%eax, -3480(%rbp)
.LBB2_76:                               # %while.cond470
                                        #   Parent Loop BB2_37 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	#DEBUG_VALUE: bbStart <- [RBP+-3472]
	#DEBUG_VALUE: bbSize <- [RBP+-3476]
	#DEBUG_VALUE: shifts <- [RBP+-3480]
	movslq	-3476(%rbp), %rax
	movl	-3480(%rbp), %ecx
	sarq	%cl, %rax
	cmpq	$65535, %rax            # imm = 0xFFFF
	jge	.LBB2_77
# BB#78:                                # %while.end476
                                        #   in Loop: Header=BB2_37 Depth=1
	#DEBUG_VALUE: bbStart <- [RBP+-3472]
	#DEBUG_VALUE: bbSize <- [RBP+-3476]
	#DEBUG_VALUE: shifts <- [RBP+-3480]
	.loc	2 1043 0                # blocksort.c:1043:0
.Ltmp214:
	movl	-3476(%rbp), %eax
	jmp	.LBB2_79
	.align	16, 0x90
.LBB2_82:                               # %for.inc496
                                        #   in Loop: Header=BB2_79 Depth=2
	#DEBUG_VALUE: bbStart <- [RBP+-3472]
	#DEBUG_VALUE: bbSize <- [RBP+-3476]
	#DEBUG_VALUE: shifts <- [RBP+-3480]
	movl	-96(%rbp), %eax
.Ltmp215:
.LBB2_79:                               # %for.cond478
                                        #   Parent Loop BB2_37 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	decq	%rax
	movl	%eax, -96(%rbp)
.Ltmp216:
	#DEBUG_VALUE: bbStart <- [RBP+-3472]
	#DEBUG_VALUE: bbSize <- [RBP+-3476]
	#DEBUG_VALUE: shifts <- [RBP+-3480]
	movslq	-96(%rbp), %rax
	testq	%rax, %rax
	js	.LBB2_83
# BB#80:                                # %for.body481
                                        #   in Loop: Header=BB2_79 Depth=2
	#DEBUG_VALUE: bbStart <- [RBP+-3472]
	#DEBUG_VALUE: bbSize <- [RBP+-3476]
	#DEBUG_VALUE: shifts <- [RBP+-3480]
.Ltmp217:
	#DEBUG_VALUE: a2update <- [RBP+-3484]
	.loc	2 1044 0                # blocksort.c:1044:0
	movl	-3472(%rbp), %eax
	movl	-96(%rbp), %edx
	addq	%rax, %rdx
	movq	$32, %rcx
	movq	-48(%rbp), %rax
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movl	(%rax,%rdx,4), %eax
	movl	%eax, -3484(%rbp)
.Ltmp218:
	#DEBUG_VALUE: qVal <- [RBP+-3486]
	.loc	2 1045 0                # blocksort.c:1045:0
	movslq	-96(%rbp), %rax
	movl	-3480(%rbp), %ecx
	.loc	2 1046 0                # blocksort.c:1046:0
	movslq	-3484(%rbp), %rdx
	movq	-64(%rbp), %rsi
	.loc	2 1045 0                # blocksort.c:1045:0
	sarq	%cl, %rax
	movw	%ax, -3486(%rbp)
	.loc	2 1046 0                # blocksort.c:1046:0
	movw	%ax, (%rsi,%rdx,2)
	.loc	2 1047 0                # blocksort.c:1047:0
.Ltmp219:
	movslq	-3484(%rbp), %rax
	cmpq	$33, %rax
	jg	.LBB2_82
# BB#81:                                # %if.then491
                                        #   in Loop: Header=BB2_79 Depth=2
	#DEBUG_VALUE: bbStart <- [RBP+-3472]
	#DEBUG_VALUE: bbSize <- [RBP+-3476]
	#DEBUG_VALUE: shifts <- [RBP+-3480]
.Ltmp220:
	#DEBUG_VALUE: a2update <- [RBP+-3484]
	#DEBUG_VALUE: qVal <- [RBP+-3486]
	.loc	2 1048 0                # blocksort.c:1048:0
	movzbl	-3486(%rbp), %eax
	movl	-3484(%rbp), %ecx
	movl	-76(%rbp), %edx
	addq	%rcx, %rdx
	movq	$32, %rcx
	movq	-64(%rbp), %rsi
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movw	%ax, (%rsi,%rdx,2)
	jmp	.LBB2_82
.Ltmp221:
	.align	16, 0x90
.LBB2_83:                               # %for.end498
                                        #   in Loop: Header=BB2_37 Depth=1
	#DEBUG_VALUE: bbStart <- [RBP+-3472]
	#DEBUG_VALUE: bbSize <- [RBP+-3476]
	#DEBUG_VALUE: shifts <- [RBP+-3480]
	.loc	2 1050 0                # blocksort.c:1050:0
	movl	-3476(%rbp), %edx
	decq	%rdx
	movq	$32, %rcx
	movl	-3480(%rbp), %eax
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	%rax, %rcx
	sarq	%cl, %rdx
	cmpq	$65536, %rdx            # imm = 0x10000
	jl	.LBB2_85
# BB#84:                                # %if.then503
                                        #   in Loop: Header=BB2_37 Depth=1
.Ltmp222:
	#DEBUG_VALUE: bbStart <- [RBP+-3472]
	#DEBUG_VALUE: bbSize <- [RBP+-3476]
	#DEBUG_VALUE: shifts <- [RBP+-3480]
	movq	$1002, %rdi             # imm = 0x3EA
	callq	BZ2_bz__AssertH__fail
	jmp	.LBB2_85
.Ltmp223:
.LBB2_86:                               # %for.end508
	.loc	2 1055 0                # blocksort.c:1055:0
	movslq	-80(%rbp), %rax
	cmpq	$4, %rax
	jl	.LBB2_88
# BB#87:                                # %if.then511
	.loc	2 1056 0                # blocksort.c:1056:0
	movl	-76(%rbp), %edx
	movl	-3448(%rbp), %ecx
	movq	stderr(%rip), %rdi
	movq	%rdx, %r8
	subq	%rcx, %r8
	movabsq	$.L.str5, %rsi
	xorq	%rax, %rax
	callq	fprintf
.Ltmp224:
.LBB2_88:                               # %if.end514
	.loc	2 1058 0                # blocksort.c:1058:0
	addq	$3464, %rsp             # imm = 0xD88
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	popq	%rbp
	retq
.Ltmp225:
.Ltmp226:
	.size	mainSort, .Ltmp226-mainSort
.Lfunc_end2:
	.cfi_endproc

	.align	16, 0x90
	.type	mainQSort3,@function
mainQSort3:                             # @mainQSort3
.Lfunc_begin3:
	.loc	2 676 0                 # blocksort.c:676:0
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbp
.Ltmp230:
	.cfi_def_cfa_offset 16
.Ltmp231:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
.Ltmp232:
	.cfi_def_cfa_register %rbp
	pushq	%r14
	pushq	%rbx
	subq	$1408, %rsp             # imm = 0x580
.Ltmp233:
	.cfi_offset %rbx, -32
.Ltmp234:
	.cfi_offset %r14, -24
	movq	24(%rbp), %rax
	movq	16(%rbp), %rbx
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	%rdx, -40(%rbp)
	movl	%ecx, -44(%rbp)
	movl	%r8d, -48(%rbp)
	movl	%r9d, -52(%rbp)
	movl	%ebx, -56(%rbp)
	movq	%rax, -64(%rbp)
	xorq	%rax, %rax
	.loc	2 688 0 prologue_end    # blocksort.c:688:0
.Ltmp235:
	movl	%eax, -96(%rbp)
	.loc	2 689 0                 # blocksort.c:689:0
.Ltmp236:
	movl	-48(%rbp), %eax
	movslq	-96(%rbp), %rcx
	movl	%eax, -512(%rbp,%rcx,4)
	movl	-52(%rbp), %eax
	movslq	-96(%rbp), %rcx
	movl	%eax, -912(%rbp,%rcx,4)
	movl	-56(%rbp), %eax
	movslq	-96(%rbp), %rcx
	movl	-96(%rbp), %edx
	movl	%eax, -1312(%rbp,%rcx,4)
	incq	%rdx
	movl	%edx, -96(%rbp)
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	movabsq	$9223372036854775806, %r14 # imm = 0x7FFFFFFFFFFFFFFE
	jmp	.LBB3_1
.Ltmp237:
.LBB3_41:                               # %if.end271
                                        #   in Loop: Header=BB3_1 Depth=1
	.loc	2 760 0                 # blocksort.c:760:0
	movl	-1324(%rbp), %eax
	movslq	-96(%rbp), %rcx
	movl	%eax, -512(%rbp,%rcx,4)
	movl	-1336(%rbp), %eax
	movslq	-96(%rbp), %rcx
	movl	%eax, -912(%rbp,%rcx,4)
	movl	-1348(%rbp), %eax
	movslq	-96(%rbp), %rcx
	movl	%eax, -1312(%rbp,%rcx,4)
	movl	-96(%rbp), %eax
	incq	%rax
	movl	%eax, -96(%rbp)
.Ltmp238:
	.loc	2 761 0                 # blocksort.c:761:0
	movl	-1320(%rbp), %eax
	movslq	-96(%rbp), %rcx
	movl	%eax, -512(%rbp,%rcx,4)
	movl	-1332(%rbp), %eax
	movslq	-96(%rbp), %rcx
	movl	%eax, -912(%rbp,%rcx,4)
	movl	-1344(%rbp), %eax
	movslq	-96(%rbp), %rcx
	movl	%eax, -1312(%rbp,%rcx,4)
	movl	-96(%rbp), %eax
	incq	%rax
	movl	%eax, -96(%rbp)
.Ltmp239:
	.loc	2 762 0                 # blocksort.c:762:0
	movl	-1316(%rbp), %eax
	movslq	-96(%rbp), %rcx
	movl	%eax, -512(%rbp,%rcx,4)
	movl	-1328(%rbp), %eax
	movslq	-96(%rbp), %rcx
	movl	-1340(%rbp), %edx
	movl	-96(%rbp), %esi
	movslq	-96(%rbp), %rdi
	movl	%eax, -912(%rbp,%rcx,4)
	movl	%edx, -1312(%rbp,%rdi,4)
	incq	%rsi
	movl	%esi, -96(%rbp)
.Ltmp240:
	.align	16, 0x90
.LBB3_1:                                # %while.cond
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB3_9 Depth 2
                                        #       Child Loop BB3_14 Depth 3
                                        #     Child Loop BB3_27 Depth 2
                                        #     Child Loop BB3_33 Depth 2
	.loc	2 691 0                 # blocksort.c:691:0
	movslq	-96(%rbp), %rax
	testq	%rax, %rax
	jle	.LBB3_7
# BB#2:                                 # %while.body
                                        #   in Loop: Header=BB3_1 Depth=1
	.loc	2 693 0                 # blocksort.c:693:0
.Ltmp241:
	movslq	-96(%rbp), %rax
	cmpq	$100, %rax
	jl	.LBB3_4
# BB#3:                                 # %if.then
                                        #   in Loop: Header=BB3_1 Depth=1
	movq	$1001, %rdi             # imm = 0x3E9
	callq	BZ2_bz__AssertH__fail
.Ltmp242:
.LBB3_4:                                # %if.end
                                        #   in Loop: Header=BB3_1 Depth=1
	.loc	2 695 0                 # blocksort.c:695:0
	movl	-96(%rbp), %eax
	decq	%rax
	movl	%eax, -96(%rbp)
	movslq	-96(%rbp), %rax
	movslq	-96(%rbp), %rcx
	movl	-512(%rbp,%rax,4), %eax
	movl	%eax, -100(%rbp)
	movl	-912(%rbp,%rcx,4), %eax
	movl	%eax, -104(%rbp)
	movslq	-96(%rbp), %rax
	movl	-1312(%rbp,%rax,4), %eax
.Ltmp243:
	.loc	2 696 0                 # blocksort.c:696:0
	movl	-104(%rbp), %edx
	movl	-100(%rbp), %ecx
.Ltmp244:
	.loc	2 695 0                 # blocksort.c:695:0
	movl	%eax, -108(%rbp)
.Ltmp245:
	.loc	2 696 0                 # blocksort.c:696:0
	subq	%rcx, %rdx
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	$20, %rdx
	jl	.LBB3_6
# BB#5:                                 # %lor.lhs.false
                                        #   in Loop: Header=BB3_1 Depth=1
	movslq	-108(%rbp), %rax
	cmpq	$15, %rax
	jl	.LBB3_8
.LBB3_6:                                # %if.then14
                                        #   in Loop: Header=BB3_1 Depth=1
	.loc	2 698 0                 # blocksort.c:698:0
.Ltmp246:
	movq	-24(%rbp), %rdi
	movq	-32(%rbp), %rsi
	movq	-40(%rbp), %rdx
	movq	-64(%rbp), %rax
	movl	-104(%rbp), %r9d
	movl	-100(%rbp), %r8d
	movl	-44(%rbp), %ecx
	movl	-108(%rbp), %r10d
	movq	%rax, 8(%rsp)
	movq	%r10, (%rsp)
	callq	mainSimpleSort
	.loc	2 699 0                 # blocksort.c:699:0
.Ltmp247:
	movq	-64(%rbp), %rax
	movslq	(%rax), %rax
	testq	%rax, %rax
	jns	.LBB3_1
	jmp	.LBB3_7
.Ltmp248:
	.align	16, 0x90
.LBB3_8:                                # %if.end18
                                        #   in Loop: Header=BB3_1 Depth=1
	.loc	2 704 13                # blocksort.c:704:13
	movslq	-100(%rbp), %rax
	movq	-24(%rbp), %r9
	movl	(%r9,%rax,4), %edi
	movl	-108(%rbp), %r8d
	addq	%r8, %rdi
	andq	%rbx, %rdi
	movslq	-104(%rbp), %rcx
	movq	-32(%rbp), %rdx
	movl	(%r9,%rcx,4), %esi
	addq	%r8, %rsi
	addq	%rcx, %rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	andq	%r14, %rax
	movl	(%r9,%rax,2), %eax
	andq	%rbx, %rsi
	addq	%r8, %rax
	andq	%rbx, %rax
	movzbl	(%rdx,%rsi), %esi
	movzbl	(%rdx,%rdi), %edi
	movzbl	(%rdx,%rax), %edx
	callq	mmed3
	.loc	2 708 0                 # blocksort.c:708:0
	movl	-100(%rbp), %ecx
	.loc	2 704 13                # blocksort.c:704:13
	movl	%eax, -92(%rbp)
	.loc	2 709 0                 # blocksort.c:709:0
	movl	-104(%rbp), %eax
	.loc	2 708 0                 # blocksort.c:708:0
	movl	%ecx, -76(%rbp)
	movl	%ecx, -68(%rbp)
	.loc	2 709 0                 # blocksort.c:709:0
	movl	%eax, -80(%rbp)
	movl	%eax, -72(%rbp)
	jmp	.LBB3_9
	.align	16, 0x90
.LBB3_20:                               # %if.end105
                                        #   in Loop: Header=BB3_9 Depth=2
.Ltmp249:
	#DEBUG_VALUE: zztmp <- [RBP+-1360]
	.loc	2 733 0                 # blocksort.c:733:0
	movslq	-68(%rbp), %rax
	movq	-24(%rbp), %rcx
	movl	(%rcx,%rax,4), %eax
	movslq	-72(%rbp), %rcx
	movq	-24(%rbp), %rdx
	movl	%eax, -1360(%rbp)
	movl	(%rdx,%rcx,4), %eax
	movslq	-68(%rbp), %rcx
	movl	%eax, (%rdx,%rcx,4)
	movl	-1360(%rbp), %eax
	movslq	-72(%rbp), %rcx
	movq	-24(%rbp), %rdx
	movl	%eax, (%rdx,%rcx,4)
.Ltmp250:
	.loc	2 733 0                 # blocksort.c:733:0
	movl	-68(%rbp), %eax
	movl	-72(%rbp), %ecx
	incq	%rax
	movl	%eax, -68(%rbp)
	decq	%rcx
	movl	%ecx, -72(%rbp)
.Ltmp251:
.LBB3_9:                                # %while.body37
                                        #   Parent Loop BB3_1 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB3_14 Depth 3
	.loc	2 713 0                 # blocksort.c:713:0
	movslq	-68(%rbp), %rax
	movslq	-72(%rbp), %rcx
	cmpq	%rcx, %rax
	jg	.LBB3_14
.Ltmp252:
# BB#10:                                # %if.end41
                                        #   in Loop: Header=BB3_9 Depth=2
	.loc	2 714 0                 # blocksort.c:714:0
	movslq	-68(%rbp), %rax
	movq	-24(%rbp), %rcx
	movl	(%rcx,%rax,4), %eax
	movl	-108(%rbp), %ecx
	movq	-32(%rbp), %rdx
	addq	%rax, %rcx
	andq	%rbx, %rcx
	movzbl	(%rdx,%rcx), %eax
	movl	-92(%rbp), %ecx
	subq	%rcx, %rax
	testq	%rbx, %rax
	movl	%eax, -84(%rbp)
	.loc	2 715 0                 # blocksort.c:715:0
.Ltmp253:
	jne	.LBB3_12
# BB#11:                                # %if.then51
                                        #   in Loop: Header=BB3_9 Depth=2
.Ltmp254:
	#DEBUG_VALUE: zztmp <- [RBP+-1352]
	.loc	2 716 0                 # blocksort.c:716:0
	movslq	-68(%rbp), %rax
	movq	-24(%rbp), %rcx
	movl	(%rcx,%rax,4), %eax
	movslq	-76(%rbp), %rcx
	movq	-24(%rbp), %rdx
	movl	%eax, -1352(%rbp)
	movl	(%rdx,%rcx,4), %eax
	movslq	-68(%rbp), %rcx
	movl	%eax, (%rdx,%rcx,4)
	movl	-1352(%rbp), %eax
	movslq	-76(%rbp), %rcx
	movq	-24(%rbp), %rdx
	movl	%eax, (%rdx,%rcx,4)
.Ltmp255:
	.loc	2 717 0                 # blocksort.c:717:0
	movl	-76(%rbp), %eax
	movl	-68(%rbp), %ecx
	incq	%rax
	movl	%eax, -76(%rbp)
	incq	%rcx
	movl	%ecx, -68(%rbp)
	jmp	.LBB3_9
.Ltmp256:
	.align	16, 0x90
.LBB3_12:                               # %if.end62
                                        #   in Loop: Header=BB3_9 Depth=2
	.loc	2 719 0                 # blocksort.c:719:0
	movslq	-84(%rbp), %rax
	testq	%rax, %rax
	jg	.LBB3_14
.Ltmp257:
# BB#13:                                # %if.end66
                                        #   in Loop: Header=BB3_9 Depth=2
	.loc	2 720 0                 # blocksort.c:720:0
	movl	-68(%rbp), %eax
	incq	%rax
	movl	%eax, -68(%rbp)
	jmp	.LBB3_9
.Ltmp258:
	.align	16, 0x90
.LBB3_16:                               # %if.then83
                                        #   in Loop: Header=BB3_14 Depth=3
	#DEBUG_VALUE: zztmp <- [RBP+-1356]
	.loc	2 726 0                 # blocksort.c:726:0
	movslq	-72(%rbp), %rax
	movq	-24(%rbp), %rcx
	movl	(%rcx,%rax,4), %eax
	movslq	-80(%rbp), %rcx
	movq	-24(%rbp), %rdx
	movl	%eax, -1356(%rbp)
	movl	(%rdx,%rcx,4), %eax
	movslq	-72(%rbp), %rcx
	movl	%eax, (%rdx,%rcx,4)
	movl	-1356(%rbp), %eax
	movslq	-80(%rbp), %rcx
	movq	-24(%rbp), %rdx
	movl	%eax, (%rdx,%rcx,4)
.Ltmp259:
	.loc	2 727 0                 # blocksort.c:727:0
	movl	-80(%rbp), %eax
	movl	-72(%rbp), %ecx
	decq	%rax
	movl	%eax, -80(%rbp)
	decq	%rcx
	movl	%ecx, -72(%rbp)
.Ltmp260:
.LBB3_14:                               # %while.body69
                                        #   Parent Loop BB3_1 Depth=1
                                        #     Parent Loop BB3_9 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	.loc	2 723 0                 # blocksort.c:723:0
	movslq	-68(%rbp), %rax
	movslq	-72(%rbp), %rcx
	cmpq	%rcx, %rax
	jg	.LBB3_19
.Ltmp261:
# BB#15:                                # %if.end73
                                        #   in Loop: Header=BB3_14 Depth=3
	.loc	2 724 0                 # blocksort.c:724:0
	movslq	-72(%rbp), %rax
	movq	-24(%rbp), %rcx
	movl	(%rcx,%rax,4), %eax
	movl	-108(%rbp), %ecx
	movq	-32(%rbp), %rdx
	addq	%rax, %rcx
	andq	%rbx, %rcx
	movzbl	(%rdx,%rcx), %eax
	movl	-92(%rbp), %ecx
	subq	%rcx, %rax
	testq	%rbx, %rax
	movl	%eax, -84(%rbp)
	je	.LBB3_16
# BB#17:                                # %if.end95
                                        #   in Loop: Header=BB3_14 Depth=3
	.loc	2 729 0                 # blocksort.c:729:0
.Ltmp262:
	movslq	-84(%rbp), %rax
	testq	%rax, %rax
	js	.LBB3_19
.Ltmp263:
# BB#18:                                # %if.end99
                                        #   in Loop: Header=BB3_14 Depth=3
	.loc	2 730 0                 # blocksort.c:730:0
	movl	-72(%rbp), %eax
	decq	%rax
	movl	%eax, -72(%rbp)
	jmp	.LBB3_14
.Ltmp264:
	.align	16, 0x90
.LBB3_19:                               # %while.end101
                                        #   in Loop: Header=BB3_9 Depth=2
	.loc	2 732 0                 # blocksort.c:732:0
	movslq	-68(%rbp), %rax
	movslq	-72(%rbp), %rcx
	cmpq	%rcx, %rax
	jle	.LBB3_20
.Ltmp265:
# BB#21:                                # %while.end117
                                        #   in Loop: Header=BB3_1 Depth=1
	.loc	2 738 0                 # blocksort.c:738:0
	movslq	-80(%rbp), %rax
	movslq	-76(%rbp), %rcx
	cmpq	%rcx, %rax
	jge	.LBB3_23
# BB#22:                                # %if.then120
                                        #   in Loop: Header=BB3_1 Depth=1
	.loc	2 739 0                 # blocksort.c:739:0
.Ltmp266:
	movl	-100(%rbp), %eax
	movslq	-96(%rbp), %rcx
	movl	%eax, -512(%rbp,%rcx,4)
	movl	-104(%rbp), %eax
	movslq	-96(%rbp), %rcx
	movl	%eax, -912(%rbp,%rcx,4)
	movl	-108(%rbp), %eax
	movl	-96(%rbp), %ecx
	movslq	-96(%rbp), %rdx
	incq	%rax
	movl	%eax, -1312(%rbp,%rdx,4)
	incq	%rcx
	movl	%ecx, -96(%rbp)
	jmp	.LBB3_1
.Ltmp267:
.LBB3_23:                               # %if.end129
                                        #   in Loop: Header=BB3_1 Depth=1
	.loc	2 743 0                 # blocksort.c:743:0
	movl	-76(%rbp), %eax
	movl	-100(%rbp), %ecx
	movq	%rax, %rdx
	subq	%rcx, %rdx
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	movl	-68(%rbp), %esi
	sarq	%cl, %rdx
	subq	%rax, %rsi
	movq	$32, %rcx
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	cmpq	%rsi, %rdx
	jge	.LBB3_25
# BB#24:                                # %cond.true
                                        #   in Loop: Header=BB3_1 Depth=1
	movl	-76(%rbp), %eax
	movl	-100(%rbp), %ecx
	jmp	.LBB3_26
.LBB3_25:                               # %cond.false
                                        #   in Loop: Header=BB3_1 Depth=1
	movl	-68(%rbp), %eax
	movl	-76(%rbp), %ecx
.LBB3_26:                               # %cond.end
                                        #   in Loop: Header=BB3_1 Depth=1
	subq	%rcx, %rax
.Ltmp268:
	#DEBUG_VALUE: yyp1 <- [RBP+-1364]
	movl	%eax, -84(%rbp)
	.loc	2 743 0                 # blocksort.c:743:0
.Ltmp269:
	movl	-100(%rbp), %eax
	movl	-68(%rbp), %ecx
	movl	-84(%rbp), %edx
	movl	-84(%rbp), %esi
	movl	%eax, -1364(%rbp)
.Ltmp270:
	#DEBUG_VALUE: yyp2 <- [RBP+-1368]
	subq	%rdx, %rcx
	movl	%ecx, -1368(%rbp)
.Ltmp271:
	#DEBUG_VALUE: yyn <- [RBP+-1372]
	movl	%esi, -1372(%rbp)
	jmp	.LBB3_27
	.align	16, 0x90
.LBB3_28:                               # %while.body140
                                        #   in Loop: Header=BB3_27 Depth=2
	#DEBUG_VALUE: yyp1 <- [RBP+-1364]
	#DEBUG_VALUE: yyp2 <- [RBP+-1368]
	#DEBUG_VALUE: yyn <- [RBP+-1372]
.Ltmp272:
	#DEBUG_VALUE: zztmp <- [RBP+-1376]
	.loc	2 743 0                 # blocksort.c:743:0
	movslq	-1364(%rbp), %rax
	movq	-24(%rbp), %rcx
	movl	(%rcx,%rax,4), %eax
	movslq	-1368(%rbp), %rcx
	movq	-24(%rbp), %rdx
	movl	%eax, -1376(%rbp)
	movl	(%rdx,%rcx,4), %eax
	movslq	-1364(%rbp), %rcx
	movl	%eax, (%rdx,%rcx,4)
	movl	-1376(%rbp), %eax
	movslq	-1368(%rbp), %rcx
	movq	-24(%rbp), %rdx
	movl	%eax, (%rdx,%rcx,4)
.Ltmp273:
	.loc	2 743 0                 # blocksort.c:743:0
	movl	-1364(%rbp), %eax
	incq	%rax
	movl	-1368(%rbp), %ecx
	movl	-1372(%rbp), %edx
	movl	%eax, -1364(%rbp)
	incq	%rcx
	movl	%ecx, -1368(%rbp)
	decq	%rdx
	movl	%edx, -1372(%rbp)
.Ltmp274:
.LBB3_27:                               # %while.cond137
                                        #   Parent Loop BB3_1 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	#DEBUG_VALUE: yyp1 <- [RBP+-1364]
	#DEBUG_VALUE: yyp2 <- [RBP+-1368]
	#DEBUG_VALUE: yyn <- [RBP+-1372]
	.loc	2 743 0                 # blocksort.c:743:0
	movslq	-1372(%rbp), %rax
	testq	%rax, %rax
	jg	.LBB3_28
.Ltmp275:
# BB#29:                                # %while.end153
                                        #   in Loop: Header=BB3_1 Depth=1
	.loc	2 744 0                 # blocksort.c:744:0
	movl	-104(%rbp), %eax
	movl	-80(%rbp), %edx
	subq	%rdx, %rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	movl	-72(%rbp), %esi
	sarq	%cl, %rax
	subq	%rsi, %rdx
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rdx, %rax
	jge	.LBB3_31
# BB#30:                                # %cond.true158
                                        #   in Loop: Header=BB3_1 Depth=1
	movl	-104(%rbp), %eax
	movl	-80(%rbp), %ecx
	jmp	.LBB3_32
.LBB3_31:                               # %cond.false160
                                        #   in Loop: Header=BB3_1 Depth=1
	movl	-80(%rbp), %eax
	movl	-72(%rbp), %ecx
.LBB3_32:                               # %cond.end162
                                        #   in Loop: Header=BB3_1 Depth=1
	subq	%rcx, %rax
.Ltmp276:
	#DEBUG_VALUE: yyp1 <- [RBP+-1380]
	movl	%eax, -88(%rbp)
	.loc	2 744 0                 # blocksort.c:744:0
.Ltmp277:
	movl	-68(%rbp), %eax
	movl	-104(%rbp), %ecx
	movl	-88(%rbp), %edx
	movl	%eax, -1380(%rbp)
.Ltmp278:
	#DEBUG_VALUE: yyp2 <- [RBP+-1384]
	movl	-88(%rbp), %eax
	subq	%rdx, %rcx
	incq	%rcx
	movl	%ecx, -1384(%rbp)
.Ltmp279:
	#DEBUG_VALUE: yyn <- [RBP+-1388]
	movl	%eax, -1388(%rbp)
	jmp	.LBB3_33
	.align	16, 0x90
.LBB3_34:                               # %while.body172
                                        #   in Loop: Header=BB3_33 Depth=2
	#DEBUG_VALUE: yyp1 <- [RBP+-1380]
	#DEBUG_VALUE: yyp2 <- [RBP+-1384]
	#DEBUG_VALUE: yyn <- [RBP+-1388]
.Ltmp280:
	#DEBUG_VALUE: zztmp <- [RBP+-1392]
	.loc	2 744 0                 # blocksort.c:744:0
	movslq	-1380(%rbp), %rax
	movq	-24(%rbp), %rcx
	movl	(%rcx,%rax,4), %eax
	movslq	-1384(%rbp), %rcx
	movq	-24(%rbp), %rdx
	movl	%eax, -1392(%rbp)
	movl	(%rdx,%rcx,4), %eax
	movslq	-1380(%rbp), %rcx
	movl	%eax, (%rdx,%rcx,4)
	movl	-1392(%rbp), %eax
	movslq	-1384(%rbp), %rcx
	movq	-24(%rbp), %rdx
	movl	%eax, (%rdx,%rcx,4)
.Ltmp281:
	.loc	2 744 0                 # blocksort.c:744:0
	movl	-1380(%rbp), %eax
	incq	%rax
	movl	-1384(%rbp), %ecx
	movl	-1388(%rbp), %edx
	movl	%eax, -1380(%rbp)
	incq	%rcx
	movl	%ecx, -1384(%rbp)
	decq	%rdx
	movl	%edx, -1388(%rbp)
.Ltmp282:
.LBB3_33:                               # %while.cond169
                                        #   Parent Loop BB3_1 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	#DEBUG_VALUE: yyp1 <- [RBP+-1380]
	#DEBUG_VALUE: yyp2 <- [RBP+-1384]
	#DEBUG_VALUE: yyn <- [RBP+-1388]
	.loc	2 744 0                 # blocksort.c:744:0
	movslq	-1388(%rbp), %rax
	testq	%rax, %rax
	jg	.LBB3_34
.Ltmp283:
# BB#35:                                # %while.end185
                                        #   in Loop: Header=BB3_1 Depth=1
	.loc	2 746 0                 # blocksort.c:746:0
	movl	-100(%rbp), %eax
	movl	-68(%rbp), %ecx
	addq	%rax, %rcx
	movl	-76(%rbp), %eax
	subq	%rax, %rcx
	decq	%rcx
	movl	%ecx, -84(%rbp)
	.loc	2 747 0                 # blocksort.c:747:0
	movl	-104(%rbp), %eax
	movl	-80(%rbp), %ecx
	movl	-72(%rbp), %edx
	subq	%rdx, %rcx
	negq	%rcx
	leaq	1(%rax,%rcx), %rax
	movl	%eax, -88(%rbp)
	.loc	2 749 0                 # blocksort.c:749:0
	movl	-100(%rbp), %eax
	movl	%eax, -1324(%rbp)
	movl	-84(%rbp), %eax
	movl	%eax, -1336(%rbp)
	movl	-108(%rbp), %eax
	movl	%eax, -1348(%rbp)
	.loc	2 750 0                 # blocksort.c:750:0
	movl	-88(%rbp), %eax
	movl	%eax, -1320(%rbp)
	movl	-104(%rbp), %eax
	movl	%eax, -1332(%rbp)
	movl	-108(%rbp), %eax
	movl	%eax, -1344(%rbp)
	.loc	2 751 0                 # blocksort.c:751:0
	movl	-84(%rbp), %eax
	incq	%rax
	movl	%eax, -1316(%rbp)
	movl	-88(%rbp), %eax
	decq	%rax
	movl	%eax, -1328(%rbp)
	movl	-108(%rbp), %eax
	incq	%rax
	movl	%eax, -1340(%rbp)
	.loc	2 753 0                 # blocksort.c:753:0
.Ltmp284:
	movl	-1336(%rbp), %eax
	movl	-1324(%rbp), %ecx
	subq	%rcx, %rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	movl	-1332(%rbp), %edx
	movl	-1320(%rbp), %esi
	sarq	%cl, %rax
	subq	%rsi, %rdx
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rdx, %rax
	jge	.LBB3_37
# BB#36:                                # %if.then212
                                        #   in Loop: Header=BB3_1 Depth=1
.Ltmp285:
	#DEBUG_VALUE: tz <- [RBP+-1396]
	.loc	2 753 0                 # blocksort.c:753:0
	movl	-1324(%rbp), %eax
	movl	%eax, -1396(%rbp)
	movl	-1320(%rbp), %eax
	movl	%eax, -1324(%rbp)
	movl	-1396(%rbp), %eax
	movl	%eax, -1320(%rbp)
	movl	-1336(%rbp), %eax
	movl	%eax, -1396(%rbp)
	movl	-1348(%rbp), %eax
	movl	-1332(%rbp), %ecx
	movl	-1396(%rbp), %edx
	movl	%eax, -1396(%rbp)
	movl	-1344(%rbp), %eax
	movl	-1396(%rbp), %esi
	movl	%ecx, -1336(%rbp)
	movl	%edx, -1332(%rbp)
	movl	%eax, -1348(%rbp)
	movl	%esi, -1344(%rbp)
.Ltmp286:
.LBB3_37:                               # %if.end225
                                        #   in Loop: Header=BB3_1 Depth=1
	.loc	2 754 0                 # blocksort.c:754:0
	movl	-1332(%rbp), %eax
	movl	-1320(%rbp), %ecx
	subq	%rcx, %rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	movl	-1328(%rbp), %edx
	movl	-1316(%rbp), %esi
	sarq	%cl, %rax
	subq	%rsi, %rdx
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rdx, %rax
	jge	.LBB3_39
# BB#38:                                # %if.then234
                                        #   in Loop: Header=BB3_1 Depth=1
.Ltmp287:
	#DEBUG_VALUE: tz <- [RBP+-1400]
	.loc	2 754 0                 # blocksort.c:754:0
	movl	-1320(%rbp), %eax
	movl	%eax, -1400(%rbp)
	movl	-1316(%rbp), %eax
	movl	%eax, -1320(%rbp)
	movl	-1400(%rbp), %eax
	movl	%eax, -1316(%rbp)
	movl	-1332(%rbp), %eax
	movl	%eax, -1400(%rbp)
	movl	-1344(%rbp), %eax
	movl	-1328(%rbp), %ecx
	movl	-1400(%rbp), %edx
	movl	%eax, -1400(%rbp)
	movl	-1340(%rbp), %eax
	movl	-1400(%rbp), %esi
	movl	%ecx, -1332(%rbp)
	movl	%edx, -1328(%rbp)
	movl	%eax, -1344(%rbp)
	movl	%esi, -1340(%rbp)
.Ltmp288:
.LBB3_39:                               # %if.end248
                                        #   in Loop: Header=BB3_1 Depth=1
	.loc	2 755 0                 # blocksort.c:755:0
	movl	-1336(%rbp), %eax
	movl	-1324(%rbp), %ecx
	subq	%rcx, %rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	movl	-1332(%rbp), %edx
	movl	-1320(%rbp), %esi
	sarq	%cl, %rax
	subq	%rsi, %rdx
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rdx, %rax
	jge	.LBB3_41
# BB#40:                                # %if.then257
                                        #   in Loop: Header=BB3_1 Depth=1
.Ltmp289:
	#DEBUG_VALUE: tz <- [RBP+-1404]
	.loc	2 755 0                 # blocksort.c:755:0
	movl	-1324(%rbp), %eax
	movl	%eax, -1404(%rbp)
	movl	-1320(%rbp), %eax
	movl	%eax, -1324(%rbp)
	movl	-1404(%rbp), %eax
	movl	%eax, -1320(%rbp)
	movl	-1336(%rbp), %eax
	movl	%eax, -1404(%rbp)
	movl	-1348(%rbp), %eax
	movl	-1332(%rbp), %ecx
	movl	-1404(%rbp), %edx
	movl	%eax, -1404(%rbp)
	movl	-1344(%rbp), %eax
	movl	-1404(%rbp), %esi
	movl	%ecx, -1336(%rbp)
	movl	%edx, -1332(%rbp)
	movl	%eax, -1348(%rbp)
	movl	%esi, -1344(%rbp)
	jmp	.LBB3_41
.Ltmp290:
.LBB3_7:                                # %while.end302
	.loc	2 764 0                 # blocksort.c:764:0
	addq	$1408, %rsp             # imm = 0x580
	popq	%rbx
	popq	%r14
	popq	%rbp
	retq
.Ltmp291:
.Ltmp292:
	.size	mainQSort3, .Ltmp292-mainQSort3
.Lfunc_end3:
	.cfi_endproc

	.align	16, 0x90
	.type	mainSimpleSort,@function
mainSimpleSort:                         # @mainSimpleSort
.Lfunc_begin4:
	.loc	2 540 0                 # blocksort.c:540:0
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbp
.Ltmp295:
	.cfi_def_cfa_offset 16
.Ltmp296:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
.Ltmp297:
	.cfi_def_cfa_register %rbp
	subq	$80, %rsp
	movq	24(%rbp), %r10
	movq	16(%rbp), %rax
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	%rdx, -24(%rbp)
	movl	%ecx, -28(%rbp)
	movl	%r8d, -32(%rbp)
	movl	%r9d, -36(%rbp)
	.loc	2 544 0 prologue_end    # blocksort.c:544:0
.Ltmp298:
	movl	-36(%rbp), %edx
	movl	-32(%rbp), %ecx
	movl	%eax, -40(%rbp)
	movq	%r10, -48(%rbp)
	subq	%rcx, %rdx
	incq	%rdx
	movq	$32, %rcx
	movq	%rdx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$2, %rax
	movl	%edx, -64(%rbp)
	jl	.LBB4_19
# BB#1:                                 # %if.end
	xorq	%rax, %rax
	jmp	.LBB4_2
	.align	16, 0x90
.LBB4_3:                                # %while.body
                                        #   in Loop: Header=BB4_2 Depth=1
	.loc	2 548 0                 # blocksort.c:548:0
	movl	-68(%rbp), %eax
	incq	%rax
.LBB4_2:                                # %while.cond
                                        # =>This Inner Loop Header: Depth=1
	.loc	2 547 0                 # blocksort.c:547:0
	movl	%eax, -68(%rbp)
	.loc	2 548 0                 # blocksort.c:548:0
	movslq	-68(%rbp), %rax
	movslq	incs(,%rax,4), %rax
	movslq	-64(%rbp), %rcx
	cmpq	%rcx, %rax
	jl	.LBB4_3
.LBB4_4:                                # %while.end
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB4_6 Depth 2
                                        #       Child Loop BB4_8 Depth 3
                                        #       Child Loop BB4_12 Depth 3
                                        #       Child Loop BB4_16 Depth 3
	.loc	2 549 0                 # blocksort.c:549:0
	movl	-68(%rbp), %eax
	decq	%rax
	movl	%eax, -68(%rbp)
	.loc	2 551 0                 # blocksort.c:551:0
.Ltmp299:
	movslq	-68(%rbp), %rax
	testq	%rax, %rax
	js	.LBB4_19
# BB#5:                                 # %for.body
                                        #   in Loop: Header=BB4_4 Depth=1
	.loc	2 552 0                 # blocksort.c:552:0
.Ltmp300:
	movslq	-68(%rbp), %rax
	movl	incs(,%rax,4), %eax
	.loc	2 554 0                 # blocksort.c:554:0
	movl	-32(%rbp), %ecx
	.loc	2 552 0                 # blocksort.c:552:0
	movl	%eax, -60(%rbp)
	.loc	2 554 0                 # blocksort.c:554:0
	addq	%rax, %rcx
	movl	%ecx, -52(%rbp)
	.align	16, 0x90
.LBB4_6:                                # %while.body7
                                        #   Parent Loop BB4_4 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB4_8 Depth 3
                                        #       Child Loop BB4_12 Depth 3
                                        #       Child Loop BB4_16 Depth 3
	.loc	2 558 0                 # blocksort.c:558:0
.Ltmp301:
	movslq	-52(%rbp), %rax
	movslq	-36(%rbp), %rcx
	cmpq	%rcx, %rax
	jg	.LBB4_4
.Ltmp302:
# BB#7:                                 # %if.end10
                                        #   in Loop: Header=BB4_6 Depth=2
	.loc	2 559 0                 # blocksort.c:559:0
	movslq	-52(%rbp), %rax
	movq	-8(%rbp), %rcx
	movl	(%rcx,%rax,4), %eax
	.loc	2 560 0                 # blocksort.c:560:0
	movl	-52(%rbp), %ecx
	.loc	2 559 0                 # blocksort.c:559:0
	movl	%eax, -72(%rbp)
	.loc	2 560 0                 # blocksort.c:560:0
	movl	%ecx, -56(%rbp)
	.align	16, 0x90
.LBB4_8:                                # %while.cond13
                                        #   Parent Loop BB4_4 Depth=1
                                        #     Parent Loop BB4_6 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	.loc	2 561 18                # blocksort.c:561:18
	movl	-56(%rbp), %eax
	movl	-60(%rbp), %ecx
	subq	%rcx, %rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	-8(%rbp), %rcx
	movl	(%rcx,%rax,4), %edi
	movl	-40(%rbp), %eax
	movl	-72(%rbp), %esi
	movq	-16(%rbp), %rdx
	movq	-24(%rbp), %rcx
	movq	-48(%rbp), %r9
	movl	-28(%rbp), %r8d
	addq	%rax, %rdi
	addq	%rax, %rsi
	callq	mainGtU
	testq	%rax, %rax
	je	.LBB4_10
# BB#9:                                 # %while.body19
                                        #   in Loop: Header=BB4_8 Depth=3
	.loc	2 564 0                 # blocksort.c:564:0
.Ltmp303:
	movslq	-56(%rbp), %rax
	movl	-60(%rbp), %ecx
	movq	%rax, %rdx
	subq	%rcx, %rdx
	movq	$32, %rcx
	movq	-8(%rbp), %rsi
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movl	(%rsi,%rdx,4), %ecx
	movl	%ecx, (%rsi,%rax,4)
	.loc	2 565 0                 # blocksort.c:565:0
	movl	-56(%rbp), %eax
	movl	-60(%rbp), %ecx
	subq	%rcx, %rax
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	.loc	2 566 0                 # blocksort.c:566:0
.Ltmp304:
	movl	-32(%rbp), %ecx
	movl	-60(%rbp), %esi
.Ltmp305:
	.loc	2 565 0                 # blocksort.c:565:0
	movl	%eax, -56(%rbp)
	.loc	2 566 0                 # blocksort.c:566:0
.Ltmp306:
	leaq	-1(%rcx,%rsi), %rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
.Ltmp307:
	.loc	2 565 0                 # blocksort.c:565:0
	cmpq	%rax, %rdx
	jg	.LBB4_8
.Ltmp308:
.LBB4_10:                               # %while.end31
                                        #   in Loop: Header=BB4_6 Depth=2
	.loc	2 568 0                 # blocksort.c:568:0
	movl	-72(%rbp), %eax
	movslq	-56(%rbp), %rcx
	movq	-8(%rbp), %rdx
	movl	%eax, (%rdx,%rcx,4)
	.loc	2 569 0                 # blocksort.c:569:0
	movl	-52(%rbp), %eax
	incq	%rax
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	.loc	2 572 0                 # blocksort.c:572:0
.Ltmp309:
	movslq	-36(%rbp), %rsi
	movq	$32, %rcx
.Ltmp310:
	.loc	2 569 0                 # blocksort.c:569:0
	sarq	%cl, %rdx
	movl	%eax, -52(%rbp)
	cmpq	%rsi, %rdx
	jg	.LBB4_4
# BB#11:                                # %if.end37
                                        #   in Loop: Header=BB4_6 Depth=2
	.loc	2 573 0                 # blocksort.c:573:0
	movslq	-52(%rbp), %rax
	movq	-8(%rbp), %rcx
	movl	(%rcx,%rax,4), %eax
	.loc	2 574 0                 # blocksort.c:574:0
	movl	-52(%rbp), %ecx
	.loc	2 573 0                 # blocksort.c:573:0
	movl	%eax, -72(%rbp)
	.loc	2 574 0                 # blocksort.c:574:0
	movl	%ecx, -56(%rbp)
	.align	16, 0x90
.LBB4_12:                               # %while.cond40
                                        #   Parent Loop BB4_4 Depth=1
                                        #     Parent Loop BB4_6 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	.loc	2 575 18                # blocksort.c:575:18
	movl	-56(%rbp), %eax
	movl	-60(%rbp), %ecx
	subq	%rcx, %rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	-8(%rbp), %rcx
	movl	(%rcx,%rax,4), %edi
	movl	-40(%rbp), %eax
	movl	-72(%rbp), %esi
	movq	-16(%rbp), %rdx
	movq	-24(%rbp), %rcx
	movq	-48(%rbp), %r9
	movl	-28(%rbp), %r8d
	addq	%rax, %rdi
	addq	%rax, %rsi
	callq	mainGtU
	testq	%rax, %rax
	je	.LBB4_14
# BB#13:                                # %while.body48
                                        #   in Loop: Header=BB4_12 Depth=3
	.loc	2 578 0                 # blocksort.c:578:0
.Ltmp311:
	movslq	-56(%rbp), %rax
	movl	-60(%rbp), %ecx
	movq	%rax, %rdx
	subq	%rcx, %rdx
	movq	$32, %rcx
	movq	-8(%rbp), %rsi
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movl	(%rsi,%rdx,4), %ecx
	movl	%ecx, (%rsi,%rax,4)
	.loc	2 579 0                 # blocksort.c:579:0
	movl	-56(%rbp), %eax
	movl	-60(%rbp), %ecx
	subq	%rcx, %rax
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	.loc	2 580 0                 # blocksort.c:580:0
.Ltmp312:
	movl	-32(%rbp), %ecx
	movl	-60(%rbp), %esi
.Ltmp313:
	.loc	2 579 0                 # blocksort.c:579:0
	movl	%eax, -56(%rbp)
	.loc	2 580 0                 # blocksort.c:580:0
.Ltmp314:
	leaq	-1(%rcx,%rsi), %rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
.Ltmp315:
	.loc	2 579 0                 # blocksort.c:579:0
	cmpq	%rax, %rdx
	jg	.LBB4_12
.Ltmp316:
.LBB4_14:                               # %while.end60
                                        #   in Loop: Header=BB4_6 Depth=2
	.loc	2 582 0                 # blocksort.c:582:0
	movl	-72(%rbp), %eax
	movslq	-56(%rbp), %rcx
	movq	-8(%rbp), %rdx
	movl	%eax, (%rdx,%rcx,4)
	.loc	2 583 0                 # blocksort.c:583:0
	movl	-52(%rbp), %eax
	incq	%rax
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	.loc	2 586 0                 # blocksort.c:586:0
.Ltmp317:
	movslq	-36(%rbp), %rsi
	movq	$32, %rcx
.Ltmp318:
	.loc	2 583 0                 # blocksort.c:583:0
	sarq	%cl, %rdx
	movl	%eax, -52(%rbp)
	cmpq	%rsi, %rdx
	jg	.LBB4_4
# BB#15:                                # %if.end66
                                        #   in Loop: Header=BB4_6 Depth=2
	.loc	2 587 0                 # blocksort.c:587:0
	movslq	-52(%rbp), %rax
	movq	-8(%rbp), %rcx
	movl	(%rcx,%rax,4), %eax
	.loc	2 588 0                 # blocksort.c:588:0
	movl	-52(%rbp), %ecx
	.loc	2 587 0                 # blocksort.c:587:0
	movl	%eax, -72(%rbp)
	.loc	2 588 0                 # blocksort.c:588:0
	movl	%ecx, -56(%rbp)
	.align	16, 0x90
.LBB4_16:                               # %while.cond69
                                        #   Parent Loop BB4_4 Depth=1
                                        #     Parent Loop BB4_6 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	.loc	2 589 18                # blocksort.c:589:18
	movl	-56(%rbp), %eax
	movl	-60(%rbp), %ecx
	subq	%rcx, %rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	-8(%rbp), %rcx
	movl	(%rcx,%rax,4), %edi
	movl	-40(%rbp), %eax
	movl	-72(%rbp), %esi
	movq	-16(%rbp), %rdx
	movq	-24(%rbp), %rcx
	movq	-48(%rbp), %r9
	movl	-28(%rbp), %r8d
	addq	%rax, %rdi
	addq	%rax, %rsi
	callq	mainGtU
	testq	%rax, %rax
	je	.LBB4_18
# BB#17:                                # %while.body77
                                        #   in Loop: Header=BB4_16 Depth=3
	.loc	2 592 0                 # blocksort.c:592:0
.Ltmp319:
	movslq	-56(%rbp), %rax
	movl	-60(%rbp), %ecx
	movq	%rax, %rdx
	subq	%rcx, %rdx
	movq	$32, %rcx
	movq	-8(%rbp), %rsi
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movl	(%rsi,%rdx,4), %ecx
	movl	%ecx, (%rsi,%rax,4)
	.loc	2 593 0                 # blocksort.c:593:0
	movl	-56(%rbp), %eax
	movl	-60(%rbp), %ecx
	subq	%rcx, %rax
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	.loc	2 594 0                 # blocksort.c:594:0
.Ltmp320:
	movl	-32(%rbp), %ecx
	movl	-60(%rbp), %esi
.Ltmp321:
	.loc	2 593 0                 # blocksort.c:593:0
	movl	%eax, -56(%rbp)
	.loc	2 594 0                 # blocksort.c:594:0
.Ltmp322:
	leaq	-1(%rcx,%rsi), %rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
.Ltmp323:
	.loc	2 593 0                 # blocksort.c:593:0
	cmpq	%rax, %rdx
	jg	.LBB4_16
.Ltmp324:
.LBB4_18:                               # %while.end89
                                        #   in Loop: Header=BB4_6 Depth=2
	.loc	2 596 0                 # blocksort.c:596:0
	movl	-72(%rbp), %eax
	movslq	-56(%rbp), %rcx
	movq	-8(%rbp), %rdx
	movl	%eax, (%rdx,%rcx,4)
	.loc	2 597 0                 # blocksort.c:597:0
	movl	-52(%rbp), %eax
	.loc	2 599 0                 # blocksort.c:599:0
.Ltmp325:
	movq	-48(%rbp), %rcx
.Ltmp326:
	.loc	2 597 0                 # blocksort.c:597:0
	incq	%rax
	movl	%eax, -52(%rbp)
	.loc	2 599 0                 # blocksort.c:599:0
.Ltmp327:
	movslq	(%rcx), %rax
	testq	%rax, %rax
	jns	.LBB4_6
.Ltmp328:
.LBB4_19:                               # %for.end
	.loc	2 602 0                 # blocksort.c:602:0
	addq	$80, %rsp
	popq	%rbp
	retq
.Ltmp329:
.Ltmp330:
	.size	mainSimpleSort, .Ltmp330-mainSimpleSort
.Lfunc_end4:
	.cfi_endproc

	.align	16, 0x90
	.type	mmed3,@function
mmed3:                                  # @mmed3
.Lfunc_begin5:
	.loc	2 631 0                 # blocksort.c:631:0
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbp
.Ltmp333:
	.cfi_def_cfa_offset 16
.Ltmp334:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
.Ltmp335:
	.cfi_def_cfa_register %rbp
	movb	%dil, -1(%rbp)
	movb	%sil, -2(%rbp)
	.loc	2 633 0 prologue_end    # blocksort.c:633:0
.Ltmp336:
	movzbl	-1(%rbp), %eax
	movzbl	-2(%rbp), %ecx
	movb	%dl, -3(%rbp)
	cmpq	%rcx, %rax
	jle	.LBB5_2
# BB#1:                                 # %if.then
	.loc	2 633 0                 # blocksort.c:633:0
.Ltmp337:
	movzbl	-1(%rbp), %eax
	movb	%al, -4(%rbp)
	movzbl	-2(%rbp), %eax
	movzbl	-4(%rbp), %ecx
	movb	%al, -1(%rbp)
	movb	%cl, -2(%rbp)
.Ltmp338:
.LBB5_2:                                # %if.end
	.loc	2 634 0                 # blocksort.c:634:0
	movzbl	-2(%rbp), %eax
	movzbl	-3(%rbp), %ecx
	cmpq	%rcx, %rax
	jle	.LBB5_5
# BB#3:                                 # %if.then7
	.loc	2 635 0                 # blocksort.c:635:0
.Ltmp339:
	movzbl	-3(%rbp), %eax
	movb	%al, -2(%rbp)
	.loc	2 636 0                 # blocksort.c:636:0
.Ltmp340:
	movzbl	-1(%rbp), %eax
	movzbl	-2(%rbp), %ecx
	cmpq	%rcx, %rax
	jle	.LBB5_5
# BB#4:                                 # %if.then12
	movzbl	-1(%rbp), %eax
	movb	%al, -2(%rbp)
.Ltmp341:
.LBB5_5:                                # %if.end14
	.loc	2 638 0                 # blocksort.c:638:0
	movzbl	-2(%rbp), %eax
	popq	%rbp
	retq
.Ltmp342:
.Ltmp343:
	.size	mmed3, .Ltmp343-mmed3
.Lfunc_end5:
	.cfi_endproc

	.align	16, 0x90
	.type	mainGtU,@function
mainGtU:                                # @mainGtU
.Lfunc_begin6:
	.loc	2 400 0                 # blocksort.c:400:0
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbp
.Ltmp346:
	.cfi_def_cfa_offset 16
.Ltmp347:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
.Ltmp348:
	.cfi_def_cfa_register %rbp
	movl	%edi, -8(%rbp)
	movl	%esi, -12(%rbp)
	movq	%rdx, -24(%rbp)
	movq	%rcx, -32(%rbp)
	.loc	2 407 0 prologue_end    # blocksort.c:407:0
.Ltmp349:
	movl	-8(%rbp), %eax
	movq	-24(%rbp), %rcx
	movl	%r8d, -36(%rbp)
	movq	%r9, -48(%rbp)
	movzbl	(%rcx,%rax), %eax
	movl	-12(%rbp), %ecx
	movq	-24(%rbp), %rdx
	movb	%al, -53(%rbp)
	movzbl	(%rdx,%rcx), %eax
	movb	%al, -54(%rbp)
	.loc	2 408 0                 # blocksort.c:408:0
.Ltmp350:
	movzbl	-53(%rbp), %eax
	movzbl	-54(%rbp), %ecx
	cmpq	%rcx, %rax
	je	.LBB6_4
# BB#1:                                 # %if.then
	movzbl	-53(%rbp), %eax
	movzbl	-54(%rbp), %ecx
	cmpq	%rcx, %rax
	jle	.LBB6_2
# BB#3:                                 # %if.then
	movabsq	$1, %rax
	jmp	.LBB6_119
.Ltmp351:
.LBB6_4:                                # %if.end
	.loc	2 409 0                 # blocksort.c:409:0
	movl	-8(%rbp), %eax
	incq	%rax
	movl	%eax, -8(%rbp)
	movl	-12(%rbp), %eax
	.loc	2 411 0                 # blocksort.c:411:0
	movl	-8(%rbp), %ecx
	movq	-24(%rbp), %rdx
	.loc	2 409 0                 # blocksort.c:409:0
	incq	%rax
	movl	%eax, -12(%rbp)
	.loc	2 411 0                 # blocksort.c:411:0
	movzbl	(%rdx,%rcx), %eax
	movl	-12(%rbp), %ecx
	movq	-24(%rbp), %rdx
	movb	%al, -53(%rbp)
	movzbl	(%rdx,%rcx), %eax
	movb	%al, -54(%rbp)
	.loc	2 412 0                 # blocksort.c:412:0
.Ltmp352:
	movzbl	-53(%rbp), %eax
	movzbl	-54(%rbp), %ecx
	cmpq	%rcx, %rax
	je	.LBB6_8
# BB#5:                                 # %if.then19
	movzbl	-53(%rbp), %eax
	movzbl	-54(%rbp), %ecx
	cmpq	%rcx, %rax
	jle	.LBB6_6
# BB#7:                                 # %if.then19
	movabsq	$1, %rax
	jmp	.LBB6_119
.Ltmp353:
.LBB6_2:
	.loc	2 408 0                 # blocksort.c:408:0
	movabsq	$0, %rax
	jmp	.LBB6_119
.Ltmp354:
.LBB6_8:                                # %if.end25
	.loc	2 413 0                 # blocksort.c:413:0
	movl	-8(%rbp), %eax
	incq	%rax
	movl	%eax, -8(%rbp)
	movl	-12(%rbp), %eax
	.loc	2 415 0                 # blocksort.c:415:0
	movl	-8(%rbp), %ecx
	movq	-24(%rbp), %rdx
	.loc	2 413 0                 # blocksort.c:413:0
	incq	%rax
	movl	%eax, -12(%rbp)
	.loc	2 415 0                 # blocksort.c:415:0
	movzbl	(%rdx,%rcx), %eax
	movl	-12(%rbp), %ecx
	movq	-24(%rbp), %rdx
	movb	%al, -53(%rbp)
	movzbl	(%rdx,%rcx), %eax
	movb	%al, -54(%rbp)
	.loc	2 416 0                 # blocksort.c:416:0
.Ltmp355:
	movzbl	-53(%rbp), %eax
	movzbl	-54(%rbp), %ecx
	cmpq	%rcx, %rax
	je	.LBB6_12
# BB#9:                                 # %if.then36
	movzbl	-53(%rbp), %eax
	movzbl	-54(%rbp), %ecx
	cmpq	%rcx, %rax
	jle	.LBB6_10
# BB#11:                                # %if.then36
	movabsq	$1, %rax
	jmp	.LBB6_119
.Ltmp356:
.LBB6_6:
	.loc	2 412 0                 # blocksort.c:412:0
	movabsq	$0, %rax
	jmp	.LBB6_119
.Ltmp357:
.LBB6_12:                               # %if.end42
	.loc	2 417 0                 # blocksort.c:417:0
	movl	-8(%rbp), %eax
	incq	%rax
	movl	%eax, -8(%rbp)
	movl	-12(%rbp), %eax
	.loc	2 419 0                 # blocksort.c:419:0
	movl	-8(%rbp), %ecx
	movq	-24(%rbp), %rdx
	.loc	2 417 0                 # blocksort.c:417:0
	incq	%rax
	movl	%eax, -12(%rbp)
	.loc	2 419 0                 # blocksort.c:419:0
	movzbl	(%rdx,%rcx), %eax
	movl	-12(%rbp), %ecx
	movq	-24(%rbp), %rdx
	movb	%al, -53(%rbp)
	movzbl	(%rdx,%rcx), %eax
	movb	%al, -54(%rbp)
	.loc	2 420 0                 # blocksort.c:420:0
.Ltmp358:
	movzbl	-53(%rbp), %eax
	movzbl	-54(%rbp), %ecx
	cmpq	%rcx, %rax
	je	.LBB6_16
# BB#13:                                # %if.then53
	movzbl	-53(%rbp), %eax
	movzbl	-54(%rbp), %ecx
	cmpq	%rcx, %rax
	jle	.LBB6_14
# BB#15:                                # %if.then53
	movabsq	$1, %rax
	jmp	.LBB6_119
.Ltmp359:
.LBB6_10:
	.loc	2 416 0                 # blocksort.c:416:0
	movabsq	$0, %rax
	jmp	.LBB6_119
.Ltmp360:
.LBB6_16:                               # %if.end59
	.loc	2 421 0                 # blocksort.c:421:0
	movl	-8(%rbp), %eax
	incq	%rax
	movl	%eax, -8(%rbp)
	movl	-12(%rbp), %eax
	.loc	2 423 0                 # blocksort.c:423:0
	movl	-8(%rbp), %ecx
	movq	-24(%rbp), %rdx
	.loc	2 421 0                 # blocksort.c:421:0
	incq	%rax
	movl	%eax, -12(%rbp)
	.loc	2 423 0                 # blocksort.c:423:0
	movzbl	(%rdx,%rcx), %eax
	movl	-12(%rbp), %ecx
	movq	-24(%rbp), %rdx
	movb	%al, -53(%rbp)
	movzbl	(%rdx,%rcx), %eax
	movb	%al, -54(%rbp)
	.loc	2 424 0                 # blocksort.c:424:0
.Ltmp361:
	movzbl	-53(%rbp), %eax
	movzbl	-54(%rbp), %ecx
	cmpq	%rcx, %rax
	je	.LBB6_20
# BB#17:                                # %if.then70
	movzbl	-53(%rbp), %eax
	movzbl	-54(%rbp), %ecx
	cmpq	%rcx, %rax
	jle	.LBB6_18
# BB#19:                                # %if.then70
	movabsq	$1, %rax
	jmp	.LBB6_119
.Ltmp362:
.LBB6_14:
	.loc	2 420 0                 # blocksort.c:420:0
	movabsq	$0, %rax
	jmp	.LBB6_119
.Ltmp363:
.LBB6_20:                               # %if.end76
	.loc	2 425 0                 # blocksort.c:425:0
	movl	-8(%rbp), %eax
	incq	%rax
	movl	%eax, -8(%rbp)
	movl	-12(%rbp), %eax
	.loc	2 427 0                 # blocksort.c:427:0
	movl	-8(%rbp), %ecx
	movq	-24(%rbp), %rdx
	.loc	2 425 0                 # blocksort.c:425:0
	incq	%rax
	movl	%eax, -12(%rbp)
	.loc	2 427 0                 # blocksort.c:427:0
	movzbl	(%rdx,%rcx), %eax
	movl	-12(%rbp), %ecx
	movq	-24(%rbp), %rdx
	movb	%al, -53(%rbp)
	movzbl	(%rdx,%rcx), %eax
	movb	%al, -54(%rbp)
	.loc	2 428 0                 # blocksort.c:428:0
.Ltmp364:
	movzbl	-53(%rbp), %eax
	movzbl	-54(%rbp), %ecx
	cmpq	%rcx, %rax
	je	.LBB6_24
# BB#21:                                # %if.then87
	movzbl	-53(%rbp), %eax
	movzbl	-54(%rbp), %ecx
	cmpq	%rcx, %rax
	jle	.LBB6_22
# BB#23:                                # %if.then87
	movabsq	$1, %rax
	jmp	.LBB6_119
.Ltmp365:
.LBB6_18:
	.loc	2 424 0                 # blocksort.c:424:0
	movabsq	$0, %rax
	jmp	.LBB6_119
.Ltmp366:
.LBB6_24:                               # %if.end93
	.loc	2 429 0                 # blocksort.c:429:0
	movl	-8(%rbp), %eax
	incq	%rax
	movl	%eax, -8(%rbp)
	movl	-12(%rbp), %eax
	.loc	2 431 0                 # blocksort.c:431:0
	movl	-8(%rbp), %ecx
	movq	-24(%rbp), %rdx
	.loc	2 429 0                 # blocksort.c:429:0
	incq	%rax
	movl	%eax, -12(%rbp)
	.loc	2 431 0                 # blocksort.c:431:0
	movzbl	(%rdx,%rcx), %eax
	movl	-12(%rbp), %ecx
	movq	-24(%rbp), %rdx
	movb	%al, -53(%rbp)
	movzbl	(%rdx,%rcx), %eax
	movb	%al, -54(%rbp)
	.loc	2 432 0                 # blocksort.c:432:0
.Ltmp367:
	movzbl	-53(%rbp), %eax
	movzbl	-54(%rbp), %ecx
	cmpq	%rcx, %rax
	je	.LBB6_28
# BB#25:                                # %if.then104
	movzbl	-53(%rbp), %eax
	movzbl	-54(%rbp), %ecx
	cmpq	%rcx, %rax
	jle	.LBB6_26
# BB#27:                                # %if.then104
	movabsq	$1, %rax
	jmp	.LBB6_119
.Ltmp368:
.LBB6_22:
	.loc	2 428 0                 # blocksort.c:428:0
	movabsq	$0, %rax
	jmp	.LBB6_119
.Ltmp369:
.LBB6_28:                               # %if.end110
	.loc	2 433 0                 # blocksort.c:433:0
	movl	-8(%rbp), %eax
	incq	%rax
	movl	%eax, -8(%rbp)
	movl	-12(%rbp), %eax
	.loc	2 435 0                 # blocksort.c:435:0
	movl	-8(%rbp), %ecx
	movq	-24(%rbp), %rdx
	.loc	2 433 0                 # blocksort.c:433:0
	incq	%rax
	movl	%eax, -12(%rbp)
	.loc	2 435 0                 # blocksort.c:435:0
	movzbl	(%rdx,%rcx), %eax
	movl	-12(%rbp), %ecx
	movq	-24(%rbp), %rdx
	movb	%al, -53(%rbp)
	movzbl	(%rdx,%rcx), %eax
	movb	%al, -54(%rbp)
	.loc	2 436 0                 # blocksort.c:436:0
.Ltmp370:
	movzbl	-53(%rbp), %eax
	movzbl	-54(%rbp), %ecx
	cmpq	%rcx, %rax
	je	.LBB6_32
# BB#29:                                # %if.then121
	movzbl	-53(%rbp), %eax
	movzbl	-54(%rbp), %ecx
	cmpq	%rcx, %rax
	jle	.LBB6_30
# BB#31:                                # %if.then121
	movabsq	$1, %rax
	jmp	.LBB6_119
.Ltmp371:
.LBB6_26:
	.loc	2 432 0                 # blocksort.c:432:0
	movabsq	$0, %rax
	jmp	.LBB6_119
.Ltmp372:
.LBB6_32:                               # %if.end127
	.loc	2 437 0                 # blocksort.c:437:0
	movl	-8(%rbp), %eax
	incq	%rax
	movl	%eax, -8(%rbp)
	movl	-12(%rbp), %eax
	.loc	2 439 0                 # blocksort.c:439:0
	movl	-8(%rbp), %ecx
	movq	-24(%rbp), %rdx
	.loc	2 437 0                 # blocksort.c:437:0
	incq	%rax
	movl	%eax, -12(%rbp)
	.loc	2 439 0                 # blocksort.c:439:0
	movzbl	(%rdx,%rcx), %eax
	movl	-12(%rbp), %ecx
	movq	-24(%rbp), %rdx
	movb	%al, -53(%rbp)
	movzbl	(%rdx,%rcx), %eax
	movb	%al, -54(%rbp)
	.loc	2 440 0                 # blocksort.c:440:0
.Ltmp373:
	movzbl	-53(%rbp), %eax
	movzbl	-54(%rbp), %ecx
	cmpq	%rcx, %rax
	je	.LBB6_36
# BB#33:                                # %if.then138
	movzbl	-53(%rbp), %eax
	movzbl	-54(%rbp), %ecx
	cmpq	%rcx, %rax
	jle	.LBB6_34
# BB#35:                                # %if.then138
	movabsq	$1, %rax
	jmp	.LBB6_119
.Ltmp374:
.LBB6_30:
	.loc	2 436 0                 # blocksort.c:436:0
	movabsq	$0, %rax
	jmp	.LBB6_119
.Ltmp375:
.LBB6_36:                               # %if.end144
	.loc	2 441 0                 # blocksort.c:441:0
	movl	-8(%rbp), %eax
	incq	%rax
	movl	%eax, -8(%rbp)
	movl	-12(%rbp), %eax
	.loc	2 443 0                 # blocksort.c:443:0
	movl	-8(%rbp), %ecx
	movq	-24(%rbp), %rdx
	.loc	2 441 0                 # blocksort.c:441:0
	incq	%rax
	movl	%eax, -12(%rbp)
	.loc	2 443 0                 # blocksort.c:443:0
	movzbl	(%rdx,%rcx), %eax
	movl	-12(%rbp), %ecx
	movq	-24(%rbp), %rdx
	movb	%al, -53(%rbp)
	movzbl	(%rdx,%rcx), %eax
	movb	%al, -54(%rbp)
	.loc	2 444 0                 # blocksort.c:444:0
.Ltmp376:
	movzbl	-53(%rbp), %eax
	movzbl	-54(%rbp), %ecx
	cmpq	%rcx, %rax
	je	.LBB6_40
# BB#37:                                # %if.then155
	movzbl	-53(%rbp), %eax
	movzbl	-54(%rbp), %ecx
	cmpq	%rcx, %rax
	jle	.LBB6_38
# BB#39:                                # %if.then155
	movabsq	$1, %rax
	jmp	.LBB6_119
.Ltmp377:
.LBB6_34:
	.loc	2 440 0                 # blocksort.c:440:0
	movabsq	$0, %rax
	jmp	.LBB6_119
.Ltmp378:
.LBB6_40:                               # %if.end161
	.loc	2 445 0                 # blocksort.c:445:0
	movl	-8(%rbp), %eax
	incq	%rax
	movl	%eax, -8(%rbp)
	movl	-12(%rbp), %eax
	.loc	2 447 0                 # blocksort.c:447:0
	movl	-8(%rbp), %ecx
	movq	-24(%rbp), %rdx
	.loc	2 445 0                 # blocksort.c:445:0
	incq	%rax
	movl	%eax, -12(%rbp)
	.loc	2 447 0                 # blocksort.c:447:0
	movzbl	(%rdx,%rcx), %eax
	movl	-12(%rbp), %ecx
	movq	-24(%rbp), %rdx
	movb	%al, -53(%rbp)
	movzbl	(%rdx,%rcx), %eax
	movb	%al, -54(%rbp)
	.loc	2 448 0                 # blocksort.c:448:0
.Ltmp379:
	movzbl	-53(%rbp), %eax
	movzbl	-54(%rbp), %ecx
	cmpq	%rcx, %rax
	je	.LBB6_44
# BB#41:                                # %if.then172
	movzbl	-53(%rbp), %eax
	movzbl	-54(%rbp), %ecx
	cmpq	%rcx, %rax
	jle	.LBB6_42
# BB#43:                                # %if.then172
	movabsq	$1, %rax
	jmp	.LBB6_119
.Ltmp380:
.LBB6_38:
	.loc	2 444 0                 # blocksort.c:444:0
	movabsq	$0, %rax
	jmp	.LBB6_119
.Ltmp381:
.LBB6_44:                               # %if.end178
	.loc	2 449 0                 # blocksort.c:449:0
	movl	-8(%rbp), %eax
	incq	%rax
	movl	%eax, -8(%rbp)
	movl	-12(%rbp), %eax
	.loc	2 451 0                 # blocksort.c:451:0
	movl	-8(%rbp), %ecx
	movq	-24(%rbp), %rdx
	.loc	2 449 0                 # blocksort.c:449:0
	incq	%rax
	movl	%eax, -12(%rbp)
	.loc	2 451 0                 # blocksort.c:451:0
	movzbl	(%rdx,%rcx), %eax
	movl	-12(%rbp), %ecx
	movq	-24(%rbp), %rdx
	movb	%al, -53(%rbp)
	movzbl	(%rdx,%rcx), %eax
	movb	%al, -54(%rbp)
	.loc	2 452 0                 # blocksort.c:452:0
.Ltmp382:
	movzbl	-53(%rbp), %eax
	movzbl	-54(%rbp), %ecx
	cmpq	%rcx, %rax
	je	.LBB6_48
# BB#45:                                # %if.then189
	movzbl	-53(%rbp), %eax
	movzbl	-54(%rbp), %ecx
	cmpq	%rcx, %rax
	jle	.LBB6_46
# BB#47:                                # %if.then189
	movabsq	$1, %rax
	jmp	.LBB6_119
.Ltmp383:
.LBB6_42:
	.loc	2 448 0                 # blocksort.c:448:0
	movabsq	$0, %rax
	jmp	.LBB6_119
.Ltmp384:
.LBB6_48:                               # %if.end195
	.loc	2 453 0                 # blocksort.c:453:0
	movl	-8(%rbp), %eax
	incq	%rax
	movl	-12(%rbp), %ecx
	.loc	2 455 0                 # blocksort.c:455:0
	movl	-36(%rbp), %edx
	.loc	2 453 0                 # blocksort.c:453:0
	movl	%eax, -8(%rbp)
	incq	%rcx
	movl	%ecx, -12(%rbp)
	.loc	2 455 0                 # blocksort.c:455:0
	addq	$8, %rdx
	movl	%edx, -52(%rbp)
.LBB6_49:                               # %do.body
                                        # =>This Inner Loop Header: Depth=1
	.loc	2 459 0                 # blocksort.c:459:0
.Ltmp385:
	movl	-8(%rbp), %eax
	movq	-24(%rbp), %rcx
	movzbl	(%rcx,%rax), %eax
	movl	-12(%rbp), %ecx
	movq	-24(%rbp), %rdx
	movb	%al, -53(%rbp)
	movzbl	(%rdx,%rcx), %eax
	movb	%al, -54(%rbp)
	.loc	2 460 0                 # blocksort.c:460:0
.Ltmp386:
	movzbl	-53(%rbp), %eax
	movzbl	-54(%rbp), %ecx
	cmpq	%rcx, %rax
	jne	.LBB6_50
.Ltmp387:
# BB#53:                                # %if.end212
                                        #   in Loop: Header=BB6_49 Depth=1
	.loc	2 461 0                 # blocksort.c:461:0
	movl	-8(%rbp), %eax
	movq	-32(%rbp), %rcx
	movzbl	(%rcx,%rax,2), %eax
	movl	-12(%rbp), %ecx
	movq	-32(%rbp), %rdx
	movw	%ax, -56(%rbp)
	movzbl	(%rdx,%rcx,2), %eax
	movw	%ax, -58(%rbp)
	.loc	2 462 0                 # blocksort.c:462:0
.Ltmp388:
	movzbl	-56(%rbp), %eax
	movzbl	-58(%rbp), %ecx
	cmpq	%rcx, %rax
	jne	.LBB6_54
.Ltmp389:
# BB#57:                                # %if.end227
                                        #   in Loop: Header=BB6_49 Depth=1
	.loc	2 463 0                 # blocksort.c:463:0
	movl	-8(%rbp), %eax
	incq	%rax
	movl	%eax, -8(%rbp)
	movl	-12(%rbp), %eax
	.loc	2 465 0                 # blocksort.c:465:0
	movl	-8(%rbp), %ecx
	movq	-24(%rbp), %rdx
	.loc	2 463 0                 # blocksort.c:463:0
	incq	%rax
	movl	%eax, -12(%rbp)
	.loc	2 465 0                 # blocksort.c:465:0
	movzbl	(%rdx,%rcx), %eax
	movl	-12(%rbp), %ecx
	movq	-24(%rbp), %rdx
	movb	%al, -53(%rbp)
	movzbl	(%rdx,%rcx), %eax
	movb	%al, -54(%rbp)
	.loc	2 466 0                 # blocksort.c:466:0
.Ltmp390:
	movzbl	-53(%rbp), %eax
	movzbl	-54(%rbp), %ecx
	cmpq	%rcx, %rax
	jne	.LBB6_58
.Ltmp391:
# BB#61:                                # %if.end244
                                        #   in Loop: Header=BB6_49 Depth=1
	.loc	2 467 0                 # blocksort.c:467:0
	movl	-8(%rbp), %eax
	movq	-32(%rbp), %rcx
	movzbl	(%rcx,%rax,2), %eax
	movl	-12(%rbp), %ecx
	movq	-32(%rbp), %rdx
	movw	%ax, -56(%rbp)
	movzbl	(%rdx,%rcx,2), %eax
	movw	%ax, -58(%rbp)
	.loc	2 468 0                 # blocksort.c:468:0
.Ltmp392:
	movzbl	-56(%rbp), %eax
	movzbl	-58(%rbp), %ecx
	cmpq	%rcx, %rax
	jne	.LBB6_62
.Ltmp393:
# BB#65:                                # %if.end259
                                        #   in Loop: Header=BB6_49 Depth=1
	.loc	2 469 0                 # blocksort.c:469:0
	movl	-8(%rbp), %eax
	incq	%rax
	movl	%eax, -8(%rbp)
	movl	-12(%rbp), %eax
	.loc	2 471 0                 # blocksort.c:471:0
	movl	-8(%rbp), %ecx
	movq	-24(%rbp), %rdx
	.loc	2 469 0                 # blocksort.c:469:0
	incq	%rax
	movl	%eax, -12(%rbp)
	.loc	2 471 0                 # blocksort.c:471:0
	movzbl	(%rdx,%rcx), %eax
	movl	-12(%rbp), %ecx
	movq	-24(%rbp), %rdx
	movb	%al, -53(%rbp)
	movzbl	(%rdx,%rcx), %eax
	movb	%al, -54(%rbp)
	.loc	2 472 0                 # blocksort.c:472:0
.Ltmp394:
	movzbl	-53(%rbp), %eax
	movzbl	-54(%rbp), %ecx
	cmpq	%rcx, %rax
	jne	.LBB6_66
.Ltmp395:
# BB#69:                                # %if.end276
                                        #   in Loop: Header=BB6_49 Depth=1
	.loc	2 473 0                 # blocksort.c:473:0
	movl	-8(%rbp), %eax
	movq	-32(%rbp), %rcx
	movzbl	(%rcx,%rax,2), %eax
	movl	-12(%rbp), %ecx
	movq	-32(%rbp), %rdx
	movw	%ax, -56(%rbp)
	movzbl	(%rdx,%rcx,2), %eax
	movw	%ax, -58(%rbp)
	.loc	2 474 0                 # blocksort.c:474:0
.Ltmp396:
	movzbl	-56(%rbp), %eax
	movzbl	-58(%rbp), %ecx
	cmpq	%rcx, %rax
	jne	.LBB6_70
.Ltmp397:
# BB#73:                                # %if.end291
                                        #   in Loop: Header=BB6_49 Depth=1
	.loc	2 475 0                 # blocksort.c:475:0
	movl	-8(%rbp), %eax
	incq	%rax
	movl	%eax, -8(%rbp)
	movl	-12(%rbp), %eax
	.loc	2 477 0                 # blocksort.c:477:0
	movl	-8(%rbp), %ecx
	movq	-24(%rbp), %rdx
	.loc	2 475 0                 # blocksort.c:475:0
	incq	%rax
	movl	%eax, -12(%rbp)
	.loc	2 477 0                 # blocksort.c:477:0
	movzbl	(%rdx,%rcx), %eax
	movl	-12(%rbp), %ecx
	movq	-24(%rbp), %rdx
	movb	%al, -53(%rbp)
	movzbl	(%rdx,%rcx), %eax
	movb	%al, -54(%rbp)
	.loc	2 478 0                 # blocksort.c:478:0
.Ltmp398:
	movzbl	-53(%rbp), %eax
	movzbl	-54(%rbp), %ecx
	cmpq	%rcx, %rax
	jne	.LBB6_74
.Ltmp399:
# BB#77:                                # %if.end308
                                        #   in Loop: Header=BB6_49 Depth=1
	.loc	2 479 0                 # blocksort.c:479:0
	movl	-8(%rbp), %eax
	movq	-32(%rbp), %rcx
	movzbl	(%rcx,%rax,2), %eax
	movl	-12(%rbp), %ecx
	movq	-32(%rbp), %rdx
	movw	%ax, -56(%rbp)
	movzbl	(%rdx,%rcx,2), %eax
	movw	%ax, -58(%rbp)
	.loc	2 480 0                 # blocksort.c:480:0
.Ltmp400:
	movzbl	-56(%rbp), %eax
	movzbl	-58(%rbp), %ecx
	cmpq	%rcx, %rax
	jne	.LBB6_78
.Ltmp401:
# BB#81:                                # %if.end323
                                        #   in Loop: Header=BB6_49 Depth=1
	.loc	2 481 0                 # blocksort.c:481:0
	movl	-8(%rbp), %eax
	incq	%rax
	movl	%eax, -8(%rbp)
	movl	-12(%rbp), %eax
	.loc	2 483 0                 # blocksort.c:483:0
	movl	-8(%rbp), %ecx
	movq	-24(%rbp), %rdx
	.loc	2 481 0                 # blocksort.c:481:0
	incq	%rax
	movl	%eax, -12(%rbp)
	.loc	2 483 0                 # blocksort.c:483:0
	movzbl	(%rdx,%rcx), %eax
	movl	-12(%rbp), %ecx
	movq	-24(%rbp), %rdx
	movb	%al, -53(%rbp)
	movzbl	(%rdx,%rcx), %eax
	movb	%al, -54(%rbp)
	.loc	2 484 0                 # blocksort.c:484:0
.Ltmp402:
	movzbl	-53(%rbp), %eax
	movzbl	-54(%rbp), %ecx
	cmpq	%rcx, %rax
	jne	.LBB6_82
.Ltmp403:
# BB#85:                                # %if.end340
                                        #   in Loop: Header=BB6_49 Depth=1
	.loc	2 485 0                 # blocksort.c:485:0
	movl	-8(%rbp), %eax
	movq	-32(%rbp), %rcx
	movzbl	(%rcx,%rax,2), %eax
	movl	-12(%rbp), %ecx
	movq	-32(%rbp), %rdx
	movw	%ax, -56(%rbp)
	movzbl	(%rdx,%rcx,2), %eax
	movw	%ax, -58(%rbp)
	.loc	2 486 0                 # blocksort.c:486:0
.Ltmp404:
	movzbl	-56(%rbp), %eax
	movzbl	-58(%rbp), %ecx
	cmpq	%rcx, %rax
	jne	.LBB6_86
.Ltmp405:
# BB#89:                                # %if.end355
                                        #   in Loop: Header=BB6_49 Depth=1
	.loc	2 487 0                 # blocksort.c:487:0
	movl	-8(%rbp), %eax
	incq	%rax
	movl	%eax, -8(%rbp)
	movl	-12(%rbp), %eax
	.loc	2 489 0                 # blocksort.c:489:0
	movl	-8(%rbp), %ecx
	movq	-24(%rbp), %rdx
	.loc	2 487 0                 # blocksort.c:487:0
	incq	%rax
	movl	%eax, -12(%rbp)
	.loc	2 489 0                 # blocksort.c:489:0
	movzbl	(%rdx,%rcx), %eax
	movl	-12(%rbp), %ecx
	movq	-24(%rbp), %rdx
	movb	%al, -53(%rbp)
	movzbl	(%rdx,%rcx), %eax
	movb	%al, -54(%rbp)
	.loc	2 490 0                 # blocksort.c:490:0
.Ltmp406:
	movzbl	-53(%rbp), %eax
	movzbl	-54(%rbp), %ecx
	cmpq	%rcx, %rax
	jne	.LBB6_90
.Ltmp407:
# BB#93:                                # %if.end372
                                        #   in Loop: Header=BB6_49 Depth=1
	.loc	2 491 0                 # blocksort.c:491:0
	movl	-8(%rbp), %eax
	movq	-32(%rbp), %rcx
	movzbl	(%rcx,%rax,2), %eax
	movl	-12(%rbp), %ecx
	movq	-32(%rbp), %rdx
	movw	%ax, -56(%rbp)
	movzbl	(%rdx,%rcx,2), %eax
	movw	%ax, -58(%rbp)
	.loc	2 492 0                 # blocksort.c:492:0
.Ltmp408:
	movzbl	-56(%rbp), %eax
	movzbl	-58(%rbp), %ecx
	cmpq	%rcx, %rax
	jne	.LBB6_94
.Ltmp409:
# BB#97:                                # %if.end387
                                        #   in Loop: Header=BB6_49 Depth=1
	.loc	2 493 0                 # blocksort.c:493:0
	movl	-8(%rbp), %eax
	incq	%rax
	movl	%eax, -8(%rbp)
	movl	-12(%rbp), %eax
	.loc	2 495 0                 # blocksort.c:495:0
	movl	-8(%rbp), %ecx
	movq	-24(%rbp), %rdx
	.loc	2 493 0                 # blocksort.c:493:0
	incq	%rax
	movl	%eax, -12(%rbp)
	.loc	2 495 0                 # blocksort.c:495:0
	movzbl	(%rdx,%rcx), %eax
	movl	-12(%rbp), %ecx
	movq	-24(%rbp), %rdx
	movb	%al, -53(%rbp)
	movzbl	(%rdx,%rcx), %eax
	movb	%al, -54(%rbp)
	.loc	2 496 0                 # blocksort.c:496:0
.Ltmp410:
	movzbl	-53(%rbp), %eax
	movzbl	-54(%rbp), %ecx
	cmpq	%rcx, %rax
	jne	.LBB6_98
.Ltmp411:
# BB#101:                               # %if.end404
                                        #   in Loop: Header=BB6_49 Depth=1
	.loc	2 497 0                 # blocksort.c:497:0
	movl	-8(%rbp), %eax
	movq	-32(%rbp), %rcx
	movzbl	(%rcx,%rax,2), %eax
	movl	-12(%rbp), %ecx
	movq	-32(%rbp), %rdx
	movw	%ax, -56(%rbp)
	movzbl	(%rdx,%rcx,2), %eax
	movw	%ax, -58(%rbp)
	.loc	2 498 0                 # blocksort.c:498:0
.Ltmp412:
	movzbl	-56(%rbp), %eax
	movzbl	-58(%rbp), %ecx
	cmpq	%rcx, %rax
	jne	.LBB6_102
.Ltmp413:
# BB#105:                               # %if.end419
                                        #   in Loop: Header=BB6_49 Depth=1
	.loc	2 499 0                 # blocksort.c:499:0
	movl	-8(%rbp), %eax
	incq	%rax
	movl	%eax, -8(%rbp)
	movl	-12(%rbp), %eax
	.loc	2 501 0                 # blocksort.c:501:0
	movl	-8(%rbp), %ecx
	movq	-24(%rbp), %rdx
	.loc	2 499 0                 # blocksort.c:499:0
	incq	%rax
	movl	%eax, -12(%rbp)
	.loc	2 501 0                 # blocksort.c:501:0
	movzbl	(%rdx,%rcx), %eax
	movl	-12(%rbp), %ecx
	movq	-24(%rbp), %rdx
	movb	%al, -53(%rbp)
	movzbl	(%rdx,%rcx), %eax
	movb	%al, -54(%rbp)
	.loc	2 502 0                 # blocksort.c:502:0
.Ltmp414:
	movzbl	-53(%rbp), %eax
	movzbl	-54(%rbp), %ecx
	cmpq	%rcx, %rax
	jne	.LBB6_106
.Ltmp415:
# BB#109:                               # %if.end436
                                        #   in Loop: Header=BB6_49 Depth=1
	.loc	2 503 0                 # blocksort.c:503:0
	movl	-8(%rbp), %eax
	movq	-32(%rbp), %rcx
	movzbl	(%rcx,%rax,2), %eax
	movl	-12(%rbp), %ecx
	movq	-32(%rbp), %rdx
	movw	%ax, -56(%rbp)
	movzbl	(%rdx,%rcx,2), %eax
	movw	%ax, -58(%rbp)
	.loc	2 504 0                 # blocksort.c:504:0
.Ltmp416:
	movzbl	-56(%rbp), %eax
	movzbl	-58(%rbp), %ecx
	cmpq	%rcx, %rax
	jne	.LBB6_110
.Ltmp417:
# BB#113:                               # %if.end451
                                        #   in Loop: Header=BB6_49 Depth=1
	.loc	2 505 0                 # blocksort.c:505:0
	movl	-8(%rbp), %eax
	incq	%rax
	movl	%eax, -8(%rbp)
	movl	-12(%rbp), %eax
	.loc	2 507 0                 # blocksort.c:507:0
.Ltmp418:
	movl	-8(%rbp), %ecx
	movl	-36(%rbp), %edx
.Ltmp419:
	.loc	2 505 0                 # blocksort.c:505:0
	incq	%rax
	movl	%eax, -12(%rbp)
	.loc	2 507 0                 # blocksort.c:507:0
.Ltmp420:
	cmpq	%rdx, %rcx
	jb	.LBB6_115
# BB#114:                               # %if.then456
                                        #   in Loop: Header=BB6_49 Depth=1
	movl	-36(%rbp), %eax
	movl	-8(%rbp), %ecx
	subq	%rax, %rcx
	movl	%ecx, -8(%rbp)
.Ltmp421:
.LBB6_115:                              # %if.end457
                                        #   in Loop: Header=BB6_49 Depth=1
	.loc	2 508 0                 # blocksort.c:508:0
	movl	-12(%rbp), %eax
	movl	-36(%rbp), %ecx
	cmpq	%rcx, %rax
	jb	.LBB6_117
# BB#116:                               # %if.then460
                                        #   in Loop: Header=BB6_49 Depth=1
	movl	-36(%rbp), %eax
	movl	-12(%rbp), %ecx
	subq	%rax, %rcx
	movl	%ecx, -12(%rbp)
.Ltmp422:
.LBB6_117:                              # %if.end462
                                        #   in Loop: Header=BB6_49 Depth=1
	.loc	2 510 0                 # blocksort.c:510:0
	movl	-52(%rbp), %eax
	.loc	2 511 0                 # blocksort.c:511:0
	movq	-48(%rbp), %rcx
	.loc	2 510 0                 # blocksort.c:510:0
	addq	$-8, %rax
	movl	%eax, -52(%rbp)
	.loc	2 511 0                 # blocksort.c:511:0
	movl	(%rcx), %eax
	decq	%rax
	movl	%eax, (%rcx)
	.loc	2 512 0                 # blocksort.c:512:0
	movslq	-52(%rbp), %rax
	testq	%rax, %rax
	jns	.LBB6_49
.Ltmp423:
# BB#118:                               # %do.end
	xorq	%rax, %rax
	jmp	.LBB6_119
.LBB6_46:
	.loc	2 452 0                 # blocksort.c:452:0
.Ltmp424:
	movabsq	$0, %rax
	jmp	.LBB6_119
.Ltmp425:
.LBB6_50:                               # %if.then206
	.loc	2 460 0                 # blocksort.c:460:0
	movzbl	-53(%rbp), %eax
	movzbl	-54(%rbp), %ecx
	cmpq	%rcx, %rax
	jle	.LBB6_51
# BB#52:                                # %if.then206
	movabsq	$1, %rax
	jmp	.LBB6_119
.Ltmp426:
.LBB6_54:                               # %if.then221
	.loc	2 462 0                 # blocksort.c:462:0
	movzbl	-56(%rbp), %eax
	movzbl	-58(%rbp), %ecx
	cmpq	%rcx, %rax
	jle	.LBB6_55
# BB#56:                                # %if.then221
	movabsq	$1, %rax
	jmp	.LBB6_119
.Ltmp427:
.LBB6_58:                               # %if.then238
	.loc	2 466 0                 # blocksort.c:466:0
	movzbl	-53(%rbp), %eax
	movzbl	-54(%rbp), %ecx
	cmpq	%rcx, %rax
	jle	.LBB6_59
# BB#60:                                # %if.then238
	movabsq	$1, %rax
	jmp	.LBB6_119
.Ltmp428:
.LBB6_62:                               # %if.then253
	.loc	2 468 0                 # blocksort.c:468:0
	movzbl	-56(%rbp), %eax
	movzbl	-58(%rbp), %ecx
	cmpq	%rcx, %rax
	jle	.LBB6_63
# BB#64:                                # %if.then253
	movabsq	$1, %rax
	jmp	.LBB6_119
.Ltmp429:
.LBB6_66:                               # %if.then270
	.loc	2 472 0                 # blocksort.c:472:0
	movzbl	-53(%rbp), %eax
	movzbl	-54(%rbp), %ecx
	cmpq	%rcx, %rax
	jle	.LBB6_67
# BB#68:                                # %if.then270
	movabsq	$1, %rax
	jmp	.LBB6_119
.Ltmp430:
.LBB6_70:                               # %if.then285
	.loc	2 474 0                 # blocksort.c:474:0
	movzbl	-56(%rbp), %eax
	movzbl	-58(%rbp), %ecx
	cmpq	%rcx, %rax
	jle	.LBB6_71
# BB#72:                                # %if.then285
	movabsq	$1, %rax
	jmp	.LBB6_119
.Ltmp431:
.LBB6_74:                               # %if.then302
	.loc	2 478 0                 # blocksort.c:478:0
	movzbl	-53(%rbp), %eax
	movzbl	-54(%rbp), %ecx
	cmpq	%rcx, %rax
	jle	.LBB6_75
# BB#76:                                # %if.then302
	movabsq	$1, %rax
	jmp	.LBB6_119
.Ltmp432:
.LBB6_78:                               # %if.then317
	.loc	2 480 0                 # blocksort.c:480:0
	movzbl	-56(%rbp), %eax
	movzbl	-58(%rbp), %ecx
	cmpq	%rcx, %rax
	jle	.LBB6_79
# BB#80:                                # %if.then317
	movabsq	$1, %rax
	jmp	.LBB6_119
.Ltmp433:
.LBB6_82:                               # %if.then334
	.loc	2 484 0                 # blocksort.c:484:0
	movzbl	-53(%rbp), %eax
	movzbl	-54(%rbp), %ecx
	cmpq	%rcx, %rax
	jle	.LBB6_83
# BB#84:                                # %if.then334
	movabsq	$1, %rax
	jmp	.LBB6_119
.Ltmp434:
.LBB6_86:                               # %if.then349
	.loc	2 486 0                 # blocksort.c:486:0
	movzbl	-56(%rbp), %eax
	movzbl	-58(%rbp), %ecx
	cmpq	%rcx, %rax
	jle	.LBB6_87
# BB#88:                                # %if.then349
	movabsq	$1, %rax
	jmp	.LBB6_119
.Ltmp435:
.LBB6_90:                               # %if.then366
	.loc	2 490 0                 # blocksort.c:490:0
	movzbl	-53(%rbp), %eax
	movzbl	-54(%rbp), %ecx
	cmpq	%rcx, %rax
	jle	.LBB6_91
# BB#92:                                # %if.then366
	movabsq	$1, %rax
	jmp	.LBB6_119
.Ltmp436:
.LBB6_94:                               # %if.then381
	.loc	2 492 0                 # blocksort.c:492:0
	movzbl	-56(%rbp), %eax
	movzbl	-58(%rbp), %ecx
	cmpq	%rcx, %rax
	jle	.LBB6_95
# BB#96:                                # %if.then381
	movabsq	$1, %rax
	jmp	.LBB6_119
.Ltmp437:
.LBB6_98:                               # %if.then398
	.loc	2 496 0                 # blocksort.c:496:0
	movzbl	-53(%rbp), %eax
	movzbl	-54(%rbp), %ecx
	cmpq	%rcx, %rax
	jle	.LBB6_99
# BB#100:                               # %if.then398
	movabsq	$1, %rax
	jmp	.LBB6_119
.Ltmp438:
.LBB6_102:                              # %if.then413
	.loc	2 498 0                 # blocksort.c:498:0
	movzbl	-56(%rbp), %eax
	movzbl	-58(%rbp), %ecx
	cmpq	%rcx, %rax
	jle	.LBB6_103
# BB#104:                               # %if.then413
	movabsq	$1, %rax
	jmp	.LBB6_119
.Ltmp439:
.LBB6_106:                              # %if.then430
	.loc	2 502 0                 # blocksort.c:502:0
	movzbl	-53(%rbp), %eax
	movzbl	-54(%rbp), %ecx
	cmpq	%rcx, %rax
	jle	.LBB6_107
# BB#108:                               # %if.then430
	movabsq	$1, %rax
	jmp	.LBB6_119
.Ltmp440:
.LBB6_110:                              # %if.then445
	.loc	2 504 0                 # blocksort.c:504:0
	movzbl	-56(%rbp), %eax
	movzbl	-58(%rbp), %ecx
	cmpq	%rcx, %rax
	jle	.LBB6_111
# BB#112:                               # %if.then445
	movabsq	$1, %rax
	jmp	.LBB6_119
.Ltmp441:
.LBB6_51:
	.loc	2 460 0                 # blocksort.c:460:0
	movabsq	$0, %rax
	jmp	.LBB6_119
.Ltmp442:
.LBB6_55:
	.loc	2 462 0                 # blocksort.c:462:0
	movabsq	$0, %rax
	jmp	.LBB6_119
.Ltmp443:
.LBB6_59:
	.loc	2 466 0                 # blocksort.c:466:0
	movabsq	$0, %rax
	jmp	.LBB6_119
.Ltmp444:
.LBB6_63:
	.loc	2 468 0                 # blocksort.c:468:0
	movabsq	$0, %rax
	jmp	.LBB6_119
.Ltmp445:
.LBB6_67:
	.loc	2 472 0                 # blocksort.c:472:0
	movabsq	$0, %rax
	jmp	.LBB6_119
.Ltmp446:
.LBB6_71:
	.loc	2 474 0                 # blocksort.c:474:0
	movabsq	$0, %rax
	jmp	.LBB6_119
.Ltmp447:
.LBB6_75:
	.loc	2 478 0                 # blocksort.c:478:0
	movabsq	$0, %rax
	jmp	.LBB6_119
.Ltmp448:
.LBB6_79:
	.loc	2 480 0                 # blocksort.c:480:0
	movabsq	$0, %rax
	jmp	.LBB6_119
.Ltmp449:
.LBB6_83:
	.loc	2 484 0                 # blocksort.c:484:0
	movabsq	$0, %rax
	jmp	.LBB6_119
.Ltmp450:
.LBB6_87:
	.loc	2 486 0                 # blocksort.c:486:0
	movabsq	$0, %rax
	jmp	.LBB6_119
.Ltmp451:
.LBB6_91:
	.loc	2 490 0                 # blocksort.c:490:0
	movabsq	$0, %rax
	jmp	.LBB6_119
.Ltmp452:
.LBB6_95:
	.loc	2 492 0                 # blocksort.c:492:0
	movabsq	$0, %rax
	jmp	.LBB6_119
.Ltmp453:
.LBB6_99:
	.loc	2 496 0                 # blocksort.c:496:0
	movabsq	$0, %rax
	jmp	.LBB6_119
.Ltmp454:
.LBB6_103:
	.loc	2 498 0                 # blocksort.c:498:0
	movabsq	$0, %rax
	jmp	.LBB6_119
.Ltmp455:
.LBB6_107:
	.loc	2 502 0                 # blocksort.c:502:0
	movabsq	$0, %rax
	jmp	.LBB6_119
.Ltmp456:
.LBB6_111:
	.loc	2 504 0                 # blocksort.c:504:0
	movabsq	$0, %rax
.Ltmp457:
.LBB6_119:                              # %return
	.loc	2 515 0                 # blocksort.c:515:0
	movb	%al, -1(%rbp)
	.loc	2 516 0                 # blocksort.c:516:0
	movzbl	-1(%rbp), %eax
	popq	%rbp
	retq
.Ltmp458:
.Ltmp459:
	.size	mainGtU, .Ltmp459-mainGtU
.Lfunc_end6:
	.cfi_endproc

	.align	16, 0x90
	.type	fallbackQSort3,@function
fallbackQSort3:                         # @fallbackQSort3
.Lfunc_begin7:
	.loc	2 144 0                 # blocksort.c:144:0
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbp
.Ltmp463:
	.cfi_def_cfa_offset 16
.Ltmp464:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
.Ltmp465:
	.cfi_def_cfa_register %rbp
	pushq	%r15
	pushq	%r14
	pushq	%rbx
	subq	$920, %rsp              # imm = 0x398
.Ltmp466:
	.cfi_offset %rbx, -40
.Ltmp467:
	.cfi_offset %r14, -32
.Ltmp468:
	.cfi_offset %r15, -24
	movq	%rdi, -32(%rbp)
	movq	%rsi, -40(%rbp)
	movl	%edx, -44(%rbp)
	movl	%ecx, -48(%rbp)
	xorq	%rax, %rax
	.loc	2 151 0 prologue_end    # blocksort.c:151:0
.Ltmp469:
	movl	%eax, -92(%rbp)
	.loc	2 153 0                 # blocksort.c:153:0
	movl	%eax, -76(%rbp)
	.loc	2 154 0                 # blocksort.c:154:0
.Ltmp470:
	movl	-44(%rbp), %eax
	movslq	-76(%rbp), %rcx
	movl	%eax, -496(%rbp,%rcx,4)
	movl	-48(%rbp), %eax
	movslq	-76(%rbp), %rcx
	movl	%eax, -896(%rbp,%rcx,4)
	movl	-76(%rbp), %eax
	incq	%rax
	movl	%eax, -76(%rbp)
	movabsq	$-6148914691236517205, %r14 # imm = 0xAAAAAAAAAAAAAAAB
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	movabsq	$9223372036854775806, %r15 # imm = 0x7FFFFFFFFFFFFFFE
	jmp	.LBB7_1
.Ltmp471:
	.align	16, 0x90
.LBB7_5:                                # %if.then9
                                        #   in Loop: Header=BB7_1 Depth=1
	.loc	2 162 0                 # blocksort.c:162:0
	movq	-32(%rbp), %rdi
	movq	-40(%rbp), %rsi
	movl	-84(%rbp), %ecx
	movl	-80(%rbp), %edx
	callq	fallbackSimpleSort
.Ltmp472:
.LBB7_1:                                # %while.cond
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB7_13 Depth 2
                                        #       Child Loop BB7_14 Depth 3
                                        #       Child Loop BB7_19 Depth 3
                                        #     Child Loop BB7_31 Depth 2
                                        #     Child Loop BB7_37 Depth 2
	.loc	2 156 0                 # blocksort.c:156:0
	movslq	-76(%rbp), %rax
	testq	%rax, %rax
	jle	.LBB7_43
# BB#2:                                 # %while.body
                                        #   in Loop: Header=BB7_1 Depth=1
	.loc	2 158 0                 # blocksort.c:158:0
.Ltmp473:
	movslq	-76(%rbp), %rax
	cmpq	$100, %rax
	jl	.LBB7_4
# BB#3:                                 # %if.then
                                        #   in Loop: Header=BB7_1 Depth=1
	movq	$1004, %rdi             # imm = 0x3EC
	callq	BZ2_bz__AssertH__fail
.Ltmp474:
.LBB7_4:                                # %if.end
                                        #   in Loop: Header=BB7_1 Depth=1
	.loc	2 160 0                 # blocksort.c:160:0
	movl	-76(%rbp), %eax
	decq	%rax
	movl	%eax, -76(%rbp)
	movslq	-76(%rbp), %rax
	movslq	-76(%rbp), %rcx
	movl	-496(%rbp,%rax,4), %eax
	movl	-896(%rbp,%rcx,4), %edx
	movl	%eax, -80(%rbp)
.Ltmp475:
	.loc	2 161 0                 # blocksort.c:161:0
	movl	-80(%rbp), %eax
.Ltmp476:
	.loc	2 160 0                 # blocksort.c:160:0
	movl	%edx, -84(%rbp)
.Ltmp477:
	.loc	2 161 0                 # blocksort.c:161:0
	subq	%rax, %rdx
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	$9, %rdx
	jle	.LBB7_5
.Ltmp478:
# BB#6:                                 # %if.end10
                                        #   in Loop: Header=BB7_1 Depth=1
	.loc	2 173 0                 # blocksort.c:173:0
	movl	-92(%rbp), %eax
	imulq	$7621, %rax, %rsi       # imm = 0x1DC5
	incq	%rsi
	andq	$32767, %rsi            # imm = 0x7FFF
	movl	%esi, -92(%rbp)
	.loc	2 174 0                 # blocksort.c:174:0
	movq	%rsi, %rax
	mulq	%r14
	movq	$1, %rcx
	shrq	%cl, %rdx
	leaq	(%rdx,%rdx,2), %rax
	subq	%rax, %rsi
	testq	%rbx, %rsi
	movl	%esi, -96(%rbp)
	.loc	2 175 0                 # blocksort.c:175:0
.Ltmp479:
	jne	.LBB7_8
# BB#7:                                 # %if.then13
                                        #   in Loop: Header=BB7_1 Depth=1
	movslq	-80(%rbp), %rax
	jmp	.LBB7_11
	.align	16, 0x90
.LBB7_8:                                # %if.else
                                        #   in Loop: Header=BB7_1 Depth=1
	.loc	2 176 0                 # blocksort.c:176:0
.Ltmp480:
	movl	-96(%rbp), %eax
	cmpq	$1, %rax
	jne	.LBB7_10
# BB#9:                                 # %if.then19
                                        #   in Loop: Header=BB7_1 Depth=1
	movl	-80(%rbp), %eax
	movl	-84(%rbp), %edx
	addq	%rax, %rdx
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	movq	-32(%rbp), %rax
	sarq	%cl, %rdx
	andq	%r15, %rdx
	movl	(%rax,%rdx,2), %eax
	jmp	.LBB7_12
.LBB7_10:                               # %if.else25
                                        #   in Loop: Header=BB7_1 Depth=1
	.loc	2 177 0                 # blocksort.c:177:0
	movslq	-84(%rbp), %rax
.LBB7_11:                               # %if.end31
                                        #   in Loop: Header=BB7_1 Depth=1
	movq	-32(%rbp), %rcx
	movl	(%rcx,%rax,4), %eax
.LBB7_12:                               # %if.end31
                                        #   in Loop: Header=BB7_1 Depth=1
	movq	-40(%rbp), %rcx
	movl	(%rcx,%rax,4), %eax
	movl	%eax, -88(%rbp)
.Ltmp481:
	.loc	2 179 0                 # blocksort.c:179:0
	movl	-80(%rbp), %eax
	.loc	2 180 0                 # blocksort.c:180:0
	movl	-84(%rbp), %ecx
	.loc	2 179 0                 # blocksort.c:179:0
	movl	%eax, -60(%rbp)
	movl	%eax, -52(%rbp)
	.loc	2 180 0                 # blocksort.c:180:0
	movl	%ecx, -64(%rbp)
	jmp	.LBB7_13
	.align	16, 0x90
.LBB7_25:                               # %if.end92
                                        #   in Loop: Header=BB7_13 Depth=2
.Ltmp482:
	#DEBUG_VALUE: zztmp <- [RBP+-908]
	.loc	2 206 0                 # blocksort.c:206:0
	movslq	-52(%rbp), %rax
	movq	-32(%rbp), %rcx
	movl	(%rcx,%rax,4), %eax
	movslq	-56(%rbp), %rcx
	movq	-32(%rbp), %rdx
	movl	%eax, -908(%rbp)
	movl	(%rdx,%rcx,4), %eax
	movslq	-52(%rbp), %rcx
	movl	%eax, (%rdx,%rcx,4)
	movl	-908(%rbp), %eax
	movslq	-56(%rbp), %rcx
	movq	-32(%rbp), %rdx
	movl	%eax, (%rdx,%rcx,4)
.Ltmp483:
	.loc	2 206 0                 # blocksort.c:206:0
	movl	-52(%rbp), %eax
	movl	-56(%rbp), %ecx
	incq	%rax
	movl	%eax, -52(%rbp)
	decq	%rcx
.Ltmp484:
.LBB7_13:                               # %while.body35
                                        #   Parent Loop BB7_1 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB7_14 Depth 3
                                        #       Child Loop BB7_19 Depth 3
	.loc	2 180 0                 # blocksort.c:180:0
	movl	%ecx, -56(%rbp)
	jmp	.LBB7_14
	.align	16, 0x90
.LBB7_16:                               # %if.then45
                                        #   in Loop: Header=BB7_14 Depth=3
.Ltmp485:
	#DEBUG_VALUE: zztmp <- [RBP+-900]
	.loc	2 187 0                 # blocksort.c:187:0
	movslq	-52(%rbp), %rax
	movq	-32(%rbp), %rcx
	movl	(%rcx,%rax,4), %eax
	movslq	-60(%rbp), %rcx
	movq	-32(%rbp), %rdx
	movl	%eax, -900(%rbp)
	movl	(%rdx,%rcx,4), %eax
	movslq	-52(%rbp), %rcx
	movl	%eax, (%rdx,%rcx,4)
	movl	-900(%rbp), %eax
	movslq	-60(%rbp), %rcx
	movq	-32(%rbp), %rdx
	movl	%eax, (%rdx,%rcx,4)
.Ltmp486:
	.loc	2 188 0                 # blocksort.c:188:0
	movl	-60(%rbp), %eax
	movl	-52(%rbp), %ecx
	incq	%rax
	movl	%eax, -60(%rbp)
	incq	%rcx
	movl	%ecx, -52(%rbp)
.Ltmp487:
.LBB7_14:                               # %while.body35
                                        #   Parent Loop BB7_1 Depth=1
                                        #     Parent Loop BB7_13 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	.loc	2 184 0                 # blocksort.c:184:0
	movslq	-52(%rbp), %rax
	movslq	-56(%rbp), %rcx
	cmpq	%rcx, %rax
	jg	.LBB7_19
.Ltmp488:
# BB#15:                                # %if.end38
                                        #   in Loop: Header=BB7_14 Depth=3
	.loc	2 185 0                 # blocksort.c:185:0
	movslq	-52(%rbp), %rax
	movq	-32(%rbp), %rcx
	movl	(%rcx,%rax,4), %eax
	movq	-40(%rbp), %rcx
	movl	(%rcx,%rax,4), %eax
	movl	-88(%rbp), %ecx
	subq	%rcx, %rax
	testq	%rbx, %rax
	movl	%eax, -68(%rbp)
	je	.LBB7_16
# BB#17:                                # %if.end56
                                        #   in Loop: Header=BB7_14 Depth=3
	.loc	2 191 0                 # blocksort.c:191:0
.Ltmp489:
	movslq	-68(%rbp), %rax
	testq	%rax, %rax
	jg	.LBB7_19
.Ltmp490:
# BB#18:                                # %if.end59
                                        #   in Loop: Header=BB7_14 Depth=3
	.loc	2 192 0                 # blocksort.c:192:0
	movl	-52(%rbp), %eax
	incq	%rax
	movl	%eax, -52(%rbp)
	jmp	.LBB7_14
.Ltmp491:
	.align	16, 0x90
.LBB7_21:                               # %if.then72
                                        #   in Loop: Header=BB7_19 Depth=3
	#DEBUG_VALUE: zztmp <- [RBP+-904]
	.loc	2 198 0                 # blocksort.c:198:0
	movslq	-56(%rbp), %rax
	movq	-32(%rbp), %rcx
	movl	(%rcx,%rax,4), %eax
	movslq	-64(%rbp), %rcx
	movq	-32(%rbp), %rdx
	movl	%eax, -904(%rbp)
	movl	(%rdx,%rcx,4), %eax
	movslq	-56(%rbp), %rcx
	movl	%eax, (%rdx,%rcx,4)
	movl	-904(%rbp), %eax
	movslq	-64(%rbp), %rcx
	movq	-32(%rbp), %rdx
	movl	%eax, (%rdx,%rcx,4)
.Ltmp492:
	.loc	2 199 0                 # blocksort.c:199:0
	movl	-64(%rbp), %eax
	movl	-56(%rbp), %ecx
	decq	%rax
	movl	%eax, -64(%rbp)
	decq	%rcx
	movl	%ecx, -56(%rbp)
.Ltmp493:
.LBB7_19:                               # %while.body62
                                        #   Parent Loop BB7_1 Depth=1
                                        #     Parent Loop BB7_13 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	.loc	2 195 0                 # blocksort.c:195:0
	movslq	-52(%rbp), %rax
	movslq	-56(%rbp), %rcx
	cmpq	%rcx, %rax
	jg	.LBB7_24
.Ltmp494:
# BB#20:                                # %if.end65
                                        #   in Loop: Header=BB7_19 Depth=3
	.loc	2 196 0                 # blocksort.c:196:0
	movslq	-56(%rbp), %rax
	movq	-32(%rbp), %rcx
	movl	(%rcx,%rax,4), %eax
	movq	-40(%rbp), %rcx
	movl	(%rcx,%rax,4), %eax
	movl	-88(%rbp), %ecx
	subq	%rcx, %rax
	testq	%rbx, %rax
	movl	%eax, -68(%rbp)
	je	.LBB7_21
# BB#22:                                # %if.end84
                                        #   in Loop: Header=BB7_19 Depth=3
	.loc	2 202 0                 # blocksort.c:202:0
.Ltmp495:
	movslq	-68(%rbp), %rax
	testq	%rax, %rax
	js	.LBB7_24
.Ltmp496:
# BB#23:                                # %if.end87
                                        #   in Loop: Header=BB7_19 Depth=3
	.loc	2 203 0                 # blocksort.c:203:0
	movl	-56(%rbp), %eax
	decq	%rax
	movl	%eax, -56(%rbp)
	jmp	.LBB7_19
.Ltmp497:
	.align	16, 0x90
.LBB7_24:                               # %while.end89
                                        #   in Loop: Header=BB7_13 Depth=2
	.loc	2 205 0                 # blocksort.c:205:0
	movslq	-52(%rbp), %rax
	movslq	-56(%rbp), %rcx
	cmpq	%rcx, %rax
	jle	.LBB7_25
.Ltmp498:
# BB#26:                                # %while.end104
                                        #   in Loop: Header=BB7_1 Depth=1
	.loc	2 211 0                 # blocksort.c:211:0
	movslq	-64(%rbp), %rax
	movslq	-60(%rbp), %rcx
	cmpq	%rcx, %rax
	jl	.LBB7_1
.Ltmp499:
# BB#27:                                # %if.end107
                                        #   in Loop: Header=BB7_1 Depth=1
	.loc	2 213 0                 # blocksort.c:213:0
	movl	-60(%rbp), %eax
	movl	-80(%rbp), %ecx
	movq	%rax, %rdx
	subq	%rcx, %rdx
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	movl	-52(%rbp), %esi
	sarq	%cl, %rdx
	subq	%rax, %rsi
	movq	$32, %rcx
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	cmpq	%rsi, %rdx
	jge	.LBB7_29
# BB#28:                                # %cond.true
                                        #   in Loop: Header=BB7_1 Depth=1
	movl	-60(%rbp), %eax
	movl	-80(%rbp), %ecx
	jmp	.LBB7_30
.LBB7_29:                               # %cond.false
                                        #   in Loop: Header=BB7_1 Depth=1
	movl	-52(%rbp), %eax
	movl	-60(%rbp), %ecx
.LBB7_30:                               # %cond.end
                                        #   in Loop: Header=BB7_1 Depth=1
	subq	%rcx, %rax
.Ltmp500:
	#DEBUG_VALUE: yyp1 <- [RBP+-912]
	movl	%eax, -68(%rbp)
	.loc	2 213 0                 # blocksort.c:213:0
.Ltmp501:
	movl	-80(%rbp), %eax
	movl	-52(%rbp), %ecx
	movl	-68(%rbp), %edx
	movl	-68(%rbp), %esi
	movl	%eax, -912(%rbp)
.Ltmp502:
	#DEBUG_VALUE: yyp2 <- [RBP+-916]
	subq	%rdx, %rcx
	movl	%ecx, -916(%rbp)
.Ltmp503:
	#DEBUG_VALUE: yyn <- [RBP+-920]
	movl	%esi, -920(%rbp)
	jmp	.LBB7_31
	.align	16, 0x90
.LBB7_32:                               # %while.body116
                                        #   in Loop: Header=BB7_31 Depth=2
	#DEBUG_VALUE: yyp1 <- [RBP+-912]
	#DEBUG_VALUE: yyp2 <- [RBP+-916]
	#DEBUG_VALUE: yyn <- [RBP+-920]
.Ltmp504:
	#DEBUG_VALUE: zztmp <- [RBP+-924]
	.loc	2 213 0                 # blocksort.c:213:0
	movslq	-912(%rbp), %rax
	movq	-32(%rbp), %rcx
	movl	(%rcx,%rax,4), %eax
	movslq	-916(%rbp), %rcx
	movq	-32(%rbp), %rdx
	movl	%eax, -924(%rbp)
	movl	(%rdx,%rcx,4), %eax
	movslq	-912(%rbp), %rcx
	movl	%eax, (%rdx,%rcx,4)
	movl	-924(%rbp), %eax
	movslq	-916(%rbp), %rcx
	movq	-32(%rbp), %rdx
	movl	%eax, (%rdx,%rcx,4)
.Ltmp505:
	.loc	2 213 0                 # blocksort.c:213:0
	movl	-912(%rbp), %eax
	incq	%rax
	movl	-916(%rbp), %ecx
	movl	-920(%rbp), %edx
	movl	%eax, -912(%rbp)
	incq	%rcx
	movl	%ecx, -916(%rbp)
	decq	%rdx
	movl	%edx, -920(%rbp)
.Ltmp506:
.LBB7_31:                               # %while.cond114
                                        #   Parent Loop BB7_1 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	#DEBUG_VALUE: yyp1 <- [RBP+-912]
	#DEBUG_VALUE: yyp2 <- [RBP+-916]
	#DEBUG_VALUE: yyn <- [RBP+-920]
	.loc	2 213 0                 # blocksort.c:213:0
	movslq	-920(%rbp), %rax
	testq	%rax, %rax
	jg	.LBB7_32
.Ltmp507:
# BB#33:                                # %while.end129
                                        #   in Loop: Header=BB7_1 Depth=1
	.loc	2 214 0                 # blocksort.c:214:0
	movl	-84(%rbp), %eax
	movl	-64(%rbp), %edx
	subq	%rdx, %rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	movl	-56(%rbp), %esi
	sarq	%cl, %rax
	subq	%rsi, %rdx
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rdx, %rax
	jge	.LBB7_35
# BB#34:                                # %cond.true133
                                        #   in Loop: Header=BB7_1 Depth=1
	movl	-84(%rbp), %eax
	movl	-64(%rbp), %ecx
	jmp	.LBB7_36
.LBB7_35:                               # %cond.false135
                                        #   in Loop: Header=BB7_1 Depth=1
	movl	-64(%rbp), %eax
	movl	-56(%rbp), %ecx
.LBB7_36:                               # %cond.end137
                                        #   in Loop: Header=BB7_1 Depth=1
	subq	%rcx, %rax
.Ltmp508:
	#DEBUG_VALUE: yyp1 <- [RBP+-928]
	movl	%eax, -72(%rbp)
	.loc	2 214 0                 # blocksort.c:214:0
.Ltmp509:
	movl	-52(%rbp), %eax
	movl	-84(%rbp), %ecx
	movl	-72(%rbp), %edx
	movl	%eax, -928(%rbp)
.Ltmp510:
	#DEBUG_VALUE: yyp2 <- [RBP+-932]
	movl	-72(%rbp), %eax
	subq	%rdx, %rcx
	incq	%rcx
	movl	%ecx, -932(%rbp)
.Ltmp511:
	#DEBUG_VALUE: yyn <- [RBP+-936]
	movl	%eax, -936(%rbp)
	jmp	.LBB7_37
	.align	16, 0x90
.LBB7_38:                               # %while.body146
                                        #   in Loop: Header=BB7_37 Depth=2
	#DEBUG_VALUE: yyp1 <- [RBP+-928]
	#DEBUG_VALUE: yyp2 <- [RBP+-932]
	#DEBUG_VALUE: yyn <- [RBP+-936]
.Ltmp512:
	#DEBUG_VALUE: zztmp <- [RBP+-940]
	.loc	2 214 0                 # blocksort.c:214:0
	movslq	-928(%rbp), %rax
	movq	-32(%rbp), %rcx
	movl	(%rcx,%rax,4), %eax
	movslq	-932(%rbp), %rcx
	movq	-32(%rbp), %rdx
	movl	%eax, -940(%rbp)
	movl	(%rdx,%rcx,4), %eax
	movslq	-928(%rbp), %rcx
	movl	%eax, (%rdx,%rcx,4)
	movl	-940(%rbp), %eax
	movslq	-932(%rbp), %rcx
	movq	-32(%rbp), %rdx
	movl	%eax, (%rdx,%rcx,4)
.Ltmp513:
	.loc	2 214 0                 # blocksort.c:214:0
	movl	-928(%rbp), %eax
	incq	%rax
	movl	-932(%rbp), %ecx
	movl	-936(%rbp), %edx
	movl	%eax, -928(%rbp)
	incq	%rcx
	movl	%ecx, -932(%rbp)
	decq	%rdx
	movl	%edx, -936(%rbp)
.Ltmp514:
.LBB7_37:                               # %while.cond144
                                        #   Parent Loop BB7_1 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	#DEBUG_VALUE: yyp1 <- [RBP+-928]
	#DEBUG_VALUE: yyp2 <- [RBP+-932]
	#DEBUG_VALUE: yyn <- [RBP+-936]
	.loc	2 214 0                 # blocksort.c:214:0
	movslq	-936(%rbp), %rax
	testq	%rax, %rax
	jg	.LBB7_38
.Ltmp515:
# BB#39:                                # %while.end159
                                        #   in Loop: Header=BB7_1 Depth=1
	.loc	2 216 0                 # blocksort.c:216:0
	movl	-80(%rbp), %eax
	movl	-52(%rbp), %ecx
	addq	%rax, %rcx
	movl	-60(%rbp), %eax
	subq	%rax, %rcx
	decq	%rcx
	movl	%ecx, -68(%rbp)
	.loc	2 217 0                 # blocksort.c:217:0
	movl	-84(%rbp), %eax
	movl	-64(%rbp), %ecx
	movl	-56(%rbp), %edx
	subq	%rdx, %rcx
	negq	%rcx
	.loc	2 219 0                 # blocksort.c:219:0
.Ltmp516:
	movl	-68(%rbp), %edx
	movl	-80(%rbp), %esi
.Ltmp517:
	.loc	2 217 0                 # blocksort.c:217:0
	leaq	1(%rax,%rcx), %rax
	.loc	2 219 0                 # blocksort.c:219:0
.Ltmp518:
	subq	%rsi, %rdx
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movl	-84(%rbp), %esi
.Ltmp519:
	.loc	2 217 0                 # blocksort.c:217:0
	movl	%eax, -72(%rbp)
	.loc	2 219 0                 # blocksort.c:219:0
.Ltmp520:
	subq	%rax, %rsi
	movq	$32, %rcx
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	cmpq	%rsi, %rdx
	jle	.LBB7_42
# BB#40:                                # %if.then169
                                        #   in Loop: Header=BB7_1 Depth=1
	.loc	2 220 0                 # blocksort.c:220:0
.Ltmp521:
	movl	-80(%rbp), %eax
	movslq	-76(%rbp), %rcx
	movl	%eax, -496(%rbp,%rcx,4)
	movl	-68(%rbp), %eax
	movslq	-76(%rbp), %rcx
	movl	%eax, -896(%rbp,%rcx,4)
	movl	-76(%rbp), %eax
	incq	%rax
	movl	%eax, -76(%rbp)
.Ltmp522:
	.loc	2 221 0                 # blocksort.c:221:0
	movl	-72(%rbp), %eax
	movslq	-76(%rbp), %rcx
	movl	-84(%rbp), %edx
	jmp	.LBB7_41
.Ltmp523:
.LBB7_42:                               # %if.else180
                                        #   in Loop: Header=BB7_1 Depth=1
	.loc	2 223 0                 # blocksort.c:223:0
	movl	-72(%rbp), %eax
	movslq	-76(%rbp), %rcx
	movl	%eax, -496(%rbp,%rcx,4)
	movl	-84(%rbp), %eax
	movslq	-76(%rbp), %rcx
	movl	%eax, -896(%rbp,%rcx,4)
	movl	-76(%rbp), %eax
	incq	%rax
	movl	%eax, -76(%rbp)
.Ltmp524:
	.loc	2 224 0                 # blocksort.c:224:0
	movl	-80(%rbp), %eax
	movslq	-76(%rbp), %rcx
	movl	-68(%rbp), %edx
.Ltmp525:
.LBB7_41:                               # %while.cond
                                        #   in Loop: Header=BB7_1 Depth=1
	.loc	2 221 0                 # blocksort.c:221:0
	movl	-76(%rbp), %esi
	movslq	-76(%rbp), %rdi
	movl	%eax, -496(%rbp,%rcx,4)
	movl	%edx, -896(%rbp,%rdi,4)
	incq	%rsi
	movl	%esi, -76(%rbp)
	jmp	.LBB7_1
.Ltmp526:
.LBB7_43:                               # %while.end192
	.loc	2 227 0                 # blocksort.c:227:0
	addq	$920, %rsp              # imm = 0x398
	popq	%rbx
	popq	%r14
	popq	%r15
	popq	%rbp
	retq
.Ltmp527:
.Ltmp528:
	.size	fallbackQSort3, .Ltmp528-fallbackQSort3
.Lfunc_end7:
	.cfi_endproc

	.align	16, 0x90
	.type	fallbackSimpleSort,@function
fallbackSimpleSort:                     # @fallbackSimpleSort
.Lfunc_begin8:
	.loc	2 83 0                  # blocksort.c:83:0
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbp
.Ltmp531:
	.cfi_def_cfa_offset 16
.Ltmp532:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
.Ltmp533:
	.cfi_def_cfa_register %rbp
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	movl	%edx, -20(%rbp)
	.loc	2 87 0 prologue_end     # blocksort.c:87:0
.Ltmp534:
	movl	-20(%rbp), %edx
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movl	%ecx, -24(%rbp)
	cmpq	%rax, %rdx
	je	.LBB8_18
.Ltmp535:
# BB#1:                                 # %if.end
	.loc	2 89 0                  # blocksort.c:89:0
	movl	-24(%rbp), %eax
	movl	-20(%rbp), %ecx
	subq	%rcx, %rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$4, %rax
	jl	.LBB8_10
# BB#2:                                 # %if.then3
	.loc	2 90 0                  # blocksort.c:90:0
.Ltmp536:
	movl	-24(%rbp), %eax
	addq	$-4, %rax
	jmp	.LBB8_3
	.align	16, 0x90
.LBB8_9:                                # %for.inc25
                                        #   in Loop: Header=BB8_3 Depth=1
	.loc	2 95 0                  # blocksort.c:95:0
.Ltmp537:
	movl	-36(%rbp), %eax
	movl	-32(%rbp), %edx
	addq	$-4, %rdx
	movq	$32, %rcx
	movq	-8(%rbp), %rsi
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movl	%eax, (%rsi,%rdx,4)
.Ltmp538:
	.loc	2 90 0                  # blocksort.c:90:0
	movl	-28(%rbp), %eax
	decq	%rax
.LBB8_3:                                # %for.cond
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB8_5 Depth 2
	movl	%eax, -28(%rbp)
	movslq	-28(%rbp), %rax
	movslq	-20(%rbp), %rcx
	cmpq	%rcx, %rax
	jl	.LBB8_10
# BB#4:                                 # %for.body
                                        #   in Loop: Header=BB8_3 Depth=1
	.loc	2 91 0                  # blocksort.c:91:0
.Ltmp539:
	movslq	-28(%rbp), %rax
	movq	-8(%rbp), %rcx
	movl	(%rcx,%rax,4), %eax
	movl	%eax, -36(%rbp)
	.loc	2 92 0                  # blocksort.c:92:0
	movslq	-36(%rbp), %rax
	movq	-16(%rbp), %rcx
	.loc	2 93 0                  # blocksort.c:93:0
.Ltmp540:
	movl	-28(%rbp), %edx
.Ltmp541:
	.loc	2 92 0                  # blocksort.c:92:0
	movl	(%rcx,%rax,4), %eax
	movl	%eax, -40(%rbp)
	.loc	2 93 0                  # blocksort.c:93:0
.Ltmp542:
	addq	$4, %rdx
	movl	%edx, -32(%rbp)
	jmp	.LBB8_5
	.align	16, 0x90
.LBB8_8:                                # %for.inc
                                        #   in Loop: Header=BB8_5 Depth=2
	.loc	2 94 0                  # blocksort.c:94:0
	movslq	-32(%rbp), %rax
	movq	-8(%rbp), %rdx
	movl	(%rdx,%rax,4), %esi
	addq	$-4, %rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movl	%esi, (%rdx,%rax,4)
	.loc	2 93 0                  # blocksort.c:93:0
	movl	-32(%rbp), %eax
	addq	$4, %rax
	movl	%eax, -32(%rbp)
.LBB8_5:                                # %for.cond8
                                        #   Parent Loop BB8_3 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movslq	-32(%rbp), %rcx
	movslq	-24(%rbp), %rdx
	xorq	%rax, %rax
	cmpq	%rdx, %rcx
	jg	.LBB8_7
# BB#6:                                 # %land.rhs
                                        #   in Loop: Header=BB8_5 Depth=2
	movslq	-32(%rbp), %rax
	movq	-8(%rbp), %rcx
	movl	(%rcx,%rax,4), %eax
	movq	-16(%rbp), %rcx
	movl	-40(%rbp), %edx
	movl	(%rcx,%rax,4), %eax
	cmpq	%rdx, %rax
	sbbq	%rax, %rax
	andq	$1, %rax
.Ltmp543:
.LBB8_7:                                # %land.end
                                        #   in Loop: Header=BB8_5 Depth=2
	testq	%rax, %rax
	jne	.LBB8_8
	jmp	.LBB8_9
.LBB8_10:                               # %if.end27
	.loc	2 99 0                  # blocksort.c:99:0
.Ltmp544:
	movl	-24(%rbp), %eax
	jmp	.LBB8_11
	.align	16, 0x90
.LBB8_17:                               # %for.inc57
                                        #   in Loop: Header=BB8_11 Depth=1
	.loc	2 104 0                 # blocksort.c:104:0
.Ltmp545:
	movl	-36(%rbp), %eax
	movl	-32(%rbp), %edx
	decq	%rdx
	movq	$32, %rcx
	movq	-8(%rbp), %rsi
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movl	%eax, (%rsi,%rdx,4)
.Ltmp546:
	.loc	2 99 0                  # blocksort.c:99:0
	movl	-28(%rbp), %eax
.LBB8_11:                               # %for.cond29
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB8_13 Depth 2
	decq	%rax
	movl	%eax, -28(%rbp)
	movslq	-28(%rbp), %rax
	movslq	-20(%rbp), %rcx
	cmpq	%rcx, %rax
	jl	.LBB8_18
# BB#12:                                # %for.body31
                                        #   in Loop: Header=BB8_11 Depth=1
	.loc	2 100 0                 # blocksort.c:100:0
.Ltmp547:
	movslq	-28(%rbp), %rax
	movq	-8(%rbp), %rcx
	movl	(%rcx,%rax,4), %eax
	movl	%eax, -36(%rbp)
	.loc	2 101 0                 # blocksort.c:101:0
	movslq	-36(%rbp), %rax
	movq	-16(%rbp), %rcx
	.loc	2 102 0                 # blocksort.c:102:0
.Ltmp548:
	movl	-28(%rbp), %edx
.Ltmp549:
	.loc	2 101 0                 # blocksort.c:101:0
	movl	(%rcx,%rax,4), %eax
	movl	%eax, -40(%rbp)
	.loc	2 102 0                 # blocksort.c:102:0
.Ltmp550:
	incq	%rdx
	movl	%edx, -32(%rbp)
	jmp	.LBB8_13
	.align	16, 0x90
.LBB8_16:                               # %for.inc52
                                        #   in Loop: Header=BB8_13 Depth=2
	.loc	2 103 0                 # blocksort.c:103:0
	movslq	-32(%rbp), %rax
	movq	-8(%rbp), %rdx
	movl	(%rdx,%rax,4), %esi
	decq	%rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movl	%esi, (%rdx,%rax,4)
	.loc	2 102 0                 # blocksort.c:102:0
	movl	-32(%rbp), %eax
	incq	%rax
	movl	%eax, -32(%rbp)
.LBB8_13:                               # %for.cond37
                                        #   Parent Loop BB8_11 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movslq	-32(%rbp), %rcx
	movslq	-24(%rbp), %rdx
	xorq	%rax, %rax
	cmpq	%rdx, %rcx
	jg	.LBB8_15
# BB#14:                                # %land.rhs39
                                        #   in Loop: Header=BB8_13 Depth=2
	movslq	-32(%rbp), %rax
	movq	-8(%rbp), %rcx
	movl	(%rcx,%rax,4), %eax
	movq	-16(%rbp), %rcx
	movl	-40(%rbp), %edx
	movl	(%rcx,%rax,4), %eax
	cmpq	%rdx, %rax
	sbbq	%rax, %rax
	andq	$1, %rax
.Ltmp551:
.LBB8_15:                               # %land.end45
                                        #   in Loop: Header=BB8_13 Depth=2
	testq	%rax, %rax
	jne	.LBB8_16
	jmp	.LBB8_17
.LBB8_18:                               # %for.end59
	.loc	2 106 0                 # blocksort.c:106:0
	popq	%rbp
	retq
.Ltmp552:
.Ltmp553:
	.size	fallbackSimpleSort, .Ltmp553-fallbackSimpleSort
.Lfunc_end8:
	.cfi_endproc

	.type	.L.str,@object          # @.str
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.asciz	"      %d work, %d block, ratio %5.2f\n"
	.size	.L.str, 38

	.type	.L.str1,@object         # @.str1
.L.str1:
	.asciz	"    too repetitive; using fallback sorting algorithm\n"
	.size	.L.str1, 54

	.type	.L.str2,@object         # @.str2
.L.str2:
	.asciz	"        main sort initialise ...\n"
	.size	.L.str2, 34

	.type	.L.str3,@object         # @.str3
.L.str3:
	.asciz	"        bucket sorting ...\n"
	.size	.L.str3, 28

	.type	.L.str4,@object         # @.str4
.L.str4:
	.asciz	"        qsort [0x%x, 0x%x]   done %d   this %d\n"
	.size	.L.str4, 48

	.type	.L.str5,@object         # @.str5
.L.str5:
	.asciz	"        %d pointers, %d sorted, %d scanned\n"
	.size	.L.str5, 44

	.type	incs,@object            # @incs
	.data
	.align	16
incs:
	.long	1                       # 0x1
	.long	4                       # 0x4
	.long	13                      # 0xd
	.long	40                      # 0x28
	.long	121                     # 0x79
	.long	364                     # 0x16c
	.long	1093                    # 0x445
	.long	3280                    # 0xcd0
	.long	9841                    # 0x2671
	.long	29524                   # 0x7354
	.long	88573                   # 0x159fd
	.long	265720                  # 0x40df8
	.long	797161                  # 0xc29e9
	.long	2391484                 # 0x247dbc
	.size	incs, 56

	.type	.L.str6,@object         # @.str6
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str6:
	.asciz	"        depth %6d has "
	.size	.L.str6, 23

	.type	.L.str7,@object         # @.str7
.L.str7:
	.asciz	"%6d unresolved strings\n"
	.size	.L.str7, 24

	.type	.L.str8,@object         # @.str8
.L.str8:
	.asciz	"        reconstructing block ...\n"
	.size	.L.str8, 34

	.data
.Ldebug_end0:
	.text
.Ldebug_end1:
	.section	.debug_str,"MS",@progbits,1
.Linfo_string0:
	.asciz	"clang version 3.5 (trunk)"
.Linfo_string1:
	.asciz	"blocksort.c"
.Linfo_string2:
	.asciz	"/var/services/homes/akale/llvm523/llvm_backend/mytest/bzip2"
.Linfo_string3:
	.asciz	"incs"
.Linfo_string4:
	.asciz	"int"
.Linfo_string5:
	.asciz	"Int32"
.Linfo_string6:
	.asciz	"BZ2_blockSort"
.Linfo_string7:
	.asciz	"mainSort"
.Linfo_string8:
	.asciz	"mainQSort3"
.Linfo_string9:
	.asciz	"mmed3"
.Linfo_string10:
	.asciz	"unsigned char"
.Linfo_string11:
	.asciz	"UChar"
.Linfo_string12:
	.asciz	"mainSimpleSort"
.Linfo_string13:
	.asciz	"mainGtU"
.Linfo_string14:
	.asciz	"Bool"
.Linfo_string15:
	.asciz	"fallbackSort"
.Linfo_string16:
	.asciz	"fallbackQSort3"
.Linfo_string17:
	.asciz	"fallbackSimpleSort"
.Linfo_string18:
	.asciz	"s"
.Linfo_string19:
	.asciz	"strm"
.Linfo_string20:
	.asciz	"next_in"
.Linfo_string21:
	.asciz	"char"
.Linfo_string22:
	.asciz	"avail_in"
.Linfo_string23:
	.asciz	"unsigned int"
.Linfo_string24:
	.asciz	"total_in_lo32"
.Linfo_string25:
	.asciz	"total_in_hi32"
.Linfo_string26:
	.asciz	"next_out"
.Linfo_string27:
	.asciz	"avail_out"
.Linfo_string28:
	.asciz	"total_out_lo32"
.Linfo_string29:
	.asciz	"total_out_hi32"
.Linfo_string30:
	.asciz	"state"
.Linfo_string31:
	.asciz	"bzalloc"
.Linfo_string32:
	.asciz	"bzfree"
.Linfo_string33:
	.asciz	"opaque"
.Linfo_string34:
	.asciz	"bz_stream"
.Linfo_string35:
	.asciz	"mode"
.Linfo_string36:
	.asciz	"avail_in_expect"
.Linfo_string37:
	.asciz	"UInt32"
.Linfo_string38:
	.asciz	"arr1"
.Linfo_string39:
	.asciz	"arr2"
.Linfo_string40:
	.asciz	"ftab"
.Linfo_string41:
	.asciz	"origPtr"
.Linfo_string42:
	.asciz	"ptr"
.Linfo_string43:
	.asciz	"block"
.Linfo_string44:
	.asciz	"mtfv"
.Linfo_string45:
	.asciz	"unsigned short"
.Linfo_string46:
	.asciz	"UInt16"
.Linfo_string47:
	.asciz	"zbits"
.Linfo_string48:
	.asciz	"workFactor"
.Linfo_string49:
	.asciz	"state_in_ch"
.Linfo_string50:
	.asciz	"state_in_len"
.Linfo_string51:
	.asciz	"rNToGo"
.Linfo_string52:
	.asciz	"rTPos"
.Linfo_string53:
	.asciz	"nblock"
.Linfo_string54:
	.asciz	"nblockMAX"
.Linfo_string55:
	.asciz	"numZ"
.Linfo_string56:
	.asciz	"state_out_pos"
.Linfo_string57:
	.asciz	"nInUse"
.Linfo_string58:
	.asciz	"inUse"
.Linfo_string59:
	.asciz	"unseqToSeq"
.Linfo_string60:
	.asciz	"bsBuff"
.Linfo_string61:
	.asciz	"bsLive"
.Linfo_string62:
	.asciz	"blockCRC"
.Linfo_string63:
	.asciz	"combinedCRC"
.Linfo_string64:
	.asciz	"verbosity"
.Linfo_string65:
	.asciz	"blockNo"
.Linfo_string66:
	.asciz	"blockSize100k"
.Linfo_string67:
	.asciz	"nMTF"
.Linfo_string68:
	.asciz	"mtfFreq"
.Linfo_string69:
	.asciz	"selector"
.Linfo_string70:
	.asciz	"selectorMtf"
.Linfo_string71:
	.asciz	"len"
.Linfo_string72:
	.asciz	"code"
.Linfo_string73:
	.asciz	"rfreq"
.Linfo_string74:
	.asciz	"len_pack"
.Linfo_string75:
	.asciz	"EState"
.Linfo_string76:
	.asciz	"verb"
.Linfo_string77:
	.asciz	"wfact"
.Linfo_string78:
	.asciz	"quadrant"
.Linfo_string79:
	.asciz	"budget"
.Linfo_string80:
	.asciz	"budgetInit"
.Linfo_string81:
	.asciz	"i"
.Linfo_string82:
	.asciz	"fmap"
.Linfo_string83:
	.asciz	"eclass"
.Linfo_string84:
	.asciz	"bhtab"
.Linfo_string85:
	.asciz	"ftabCopy"
.Linfo_string86:
	.asciz	"H"
.Linfo_string87:
	.asciz	"j"
.Linfo_string88:
	.asciz	"k"
.Linfo_string89:
	.asciz	"l"
.Linfo_string90:
	.asciz	"r"
.Linfo_string91:
	.asciz	"cc"
.Linfo_string92:
	.asciz	"cc1"
.Linfo_string93:
	.asciz	"nNotDone"
.Linfo_string94:
	.asciz	"nBhtab"
.Linfo_string95:
	.asciz	"eclass8"
.Linfo_string96:
	.asciz	"ss"
.Linfo_string97:
	.asciz	"sb"
.Linfo_string98:
	.asciz	"runningOrder"
.Linfo_string99:
	.asciz	"bigDone"
.Linfo_string100:
	.asciz	"copyStart"
.Linfo_string101:
	.asciz	"copyEnd"
.Linfo_string102:
	.asciz	"c1"
.Linfo_string103:
	.asciz	"numQSorted"
.Linfo_string104:
	.asciz	"vv"
.Linfo_string105:
	.asciz	"h"
.Linfo_string106:
	.asciz	"lo"
.Linfo_string107:
	.asciz	"hi"
.Linfo_string108:
	.asciz	"bbStart"
.Linfo_string109:
	.asciz	"bbSize"
.Linfo_string110:
	.asciz	"shifts"
.Linfo_string111:
	.asciz	"a2update"
.Linfo_string112:
	.asciz	"qVal"
.Linfo_string113:
	.asciz	"loSt"
.Linfo_string114:
	.asciz	"hiSt"
.Linfo_string115:
	.asciz	"dSt"
.Linfo_string116:
	.asciz	"unLo"
.Linfo_string117:
	.asciz	"unHi"
.Linfo_string118:
	.asciz	"ltLo"
.Linfo_string119:
	.asciz	"gtHi"
.Linfo_string120:
	.asciz	"n"
.Linfo_string121:
	.asciz	"m"
.Linfo_string122:
	.asciz	"med"
.Linfo_string123:
	.asciz	"sp"
.Linfo_string124:
	.asciz	"d"
.Linfo_string125:
	.asciz	"stackLo"
.Linfo_string126:
	.asciz	"stackHi"
.Linfo_string127:
	.asciz	"stackD"
.Linfo_string128:
	.asciz	"nextLo"
.Linfo_string129:
	.asciz	"nextHi"
.Linfo_string130:
	.asciz	"nextD"
.Linfo_string131:
	.asciz	"zztmp"
.Linfo_string132:
	.asciz	"yyp1"
.Linfo_string133:
	.asciz	"yyp2"
.Linfo_string134:
	.asciz	"yyn"
.Linfo_string135:
	.asciz	"tz"
.Linfo_string136:
	.asciz	"bigN"
.Linfo_string137:
	.asciz	"hp"
.Linfo_string138:
	.asciz	"v"
.Linfo_string139:
	.asciz	"a"
.Linfo_string140:
	.asciz	"b"
.Linfo_string141:
	.asciz	"c"
.Linfo_string142:
	.asciz	"t"
.Linfo_string143:
	.asciz	"i1"
.Linfo_string144:
	.asciz	"i2"
.Linfo_string145:
	.asciz	"c2"
.Linfo_string146:
	.asciz	"s1"
.Linfo_string147:
	.asciz	"s2"
.Linfo_string148:
	.asciz	"r3"
.Linfo_string149:
	.asciz	"tmp"
.Linfo_string150:
	.asciz	"ec_tmp"
	.section	.debug_info,"",@progbits
.L.debug_info_begin0:
	.long	4716                    # Length of Unit
	.short	4                       # DWARF version number
	.long	.Lsection_abbrev        # Offset Into Abbrev. Section
	.byte	8                       # Address Size (in bytes)
	.byte	1                       # Abbrev [1] 0xb:0x1265 DW_TAG_compile_unit
	.long	.Linfo_string0          # DW_AT_producer
	.short	12                      # DW_AT_language
	.long	.Linfo_string1          # DW_AT_name
	.long	.Lsection_line          # DW_AT_stmt_list
	.long	.Linfo_string2          # DW_AT_comp_dir
	.quad	0                       # DW_AT_low_pc
	.byte	2                       # Abbrev [2] 0x26:0x16 DW_TAG_variable
	.long	.Linfo_string3          # DW_AT_name
	.long	60                      # DW_AT_type
	.byte	2                       # DW_AT_decl_file
	.short	527                     # DW_AT_decl_line
	.byte	9                       # DW_AT_location
	.byte	3
	.quad	incs
	.byte	3                       # Abbrev [3] 0x3c:0xc DW_TAG_array_type
	.long	72                      # DW_AT_type
	.byte	4                       # Abbrev [4] 0x41:0x6 DW_TAG_subrange_type
	.long	90                      # DW_AT_type
	.byte	13                      # DW_AT_upper_bound
	.byte	0                       # End Of Children Mark
	.byte	5                       # Abbrev [5] 0x48:0xb DW_TAG_typedef
	.long	83                      # DW_AT_type
	.long	.Linfo_string5          # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	84                      # DW_AT_decl_line
	.byte	6                       # Abbrev [6] 0x53:0x7 DW_TAG_base_type
	.long	.Linfo_string4          # DW_AT_name
	.byte	5                       # DW_AT_encoding
	.byte	4                       # DW_AT_byte_size
	.byte	7                       # Abbrev [7] 0x5a:0x7 DW_TAG_base_type
	.long	.Linfo_string4          # DW_AT_name
	.byte	4                       # DW_AT_byte_size
	.byte	5                       # DW_AT_encoding
	.byte	8                       # Abbrev [8] 0x61:0xc9 DW_TAG_subprogram
	.long	.Linfo_string6          # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.short	1078                    # DW_AT_decl_line
                                        # DW_AT_prototyped
                                        # DW_AT_external
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.quad	.Lfunc_begin0           # DW_AT_low_pc
	.quad	.Lfunc_end0             # DW_AT_high_pc
	.byte	1                       # DW_AT_frame_base
	.byte	86
	.byte	9                       # Abbrev [9] 0x7c:0xf DW_TAG_formal_parameter
	.long	.Linfo_string18         # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.short	1078                    # DW_AT_decl_line
	.long	3701                    # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	80
	.byte	10                      # Abbrev [10] 0x8b:0xf DW_TAG_variable
	.long	.Linfo_string42         # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.short	1080                    # DW_AT_decl_line
	.long	4525                    # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	72
	.byte	10                      # Abbrev [10] 0x9a:0xf DW_TAG_variable
	.long	.Linfo_string43         # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.short	1081                    # DW_AT_decl_line
	.long	4530                    # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	64
	.byte	10                      # Abbrev [10] 0xa9:0x10 DW_TAG_variable
	.long	.Linfo_string40         # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.short	1082                    # DW_AT_decl_line
	.long	4525                    # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\270\177"
	.byte	10                      # Abbrev [10] 0xb9:0x10 DW_TAG_variable
	.long	.Linfo_string53         # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.short	1083                    # DW_AT_decl_line
	.long	72                      # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\264\177"
	.byte	10                      # Abbrev [10] 0xc9:0x10 DW_TAG_variable
	.long	.Linfo_string76         # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.short	1084                    # DW_AT_decl_line
	.long	72                      # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\260\177"
	.byte	10                      # Abbrev [10] 0xd9:0x10 DW_TAG_variable
	.long	.Linfo_string77         # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.short	1085                    # DW_AT_decl_line
	.long	72                      # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\254\177"
	.byte	10                      # Abbrev [10] 0xe9:0x10 DW_TAG_variable
	.long	.Linfo_string78         # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.short	1086                    # DW_AT_decl_line
	.long	4535                    # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\240\177"
	.byte	10                      # Abbrev [10] 0xf9:0x10 DW_TAG_variable
	.long	.Linfo_string79         # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.short	1087                    # DW_AT_decl_line
	.long	72                      # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\234\177"
	.byte	10                      # Abbrev [10] 0x109:0x10 DW_TAG_variable
	.long	.Linfo_string80         # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.short	1088                    # DW_AT_decl_line
	.long	72                      # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\230\177"
	.byte	10                      # Abbrev [10] 0x119:0x10 DW_TAG_variable
	.long	.Linfo_string81         # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.short	1089                    # DW_AT_decl_line
	.long	72                      # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\224\177"
	.byte	0                       # End Of Children Mark
	.byte	11                      # Abbrev [11] 0x12a:0x263 DW_TAG_subprogram
	.long	.Linfo_string7          # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.short	798                     # DW_AT_decl_line
                                        # DW_AT_prototyped
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.quad	.Lfunc_begin2           # DW_AT_low_pc
	.quad	.Lfunc_end2             # DW_AT_high_pc
	.byte	1                       # DW_AT_frame_base
	.byte	86
	.byte	9                       # Abbrev [9] 0x145:0xf DW_TAG_formal_parameter
	.long	.Linfo_string42         # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.short	798                     # DW_AT_decl_line
	.long	4525                    # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	80
	.byte	9                       # Abbrev [9] 0x154:0xf DW_TAG_formal_parameter
	.long	.Linfo_string43         # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.short	799                     # DW_AT_decl_line
	.long	4530                    # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	72
	.byte	9                       # Abbrev [9] 0x163:0xf DW_TAG_formal_parameter
	.long	.Linfo_string78         # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.short	800                     # DW_AT_decl_line
	.long	4535                    # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	64
	.byte	9                       # Abbrev [9] 0x172:0x10 DW_TAG_formal_parameter
	.long	.Linfo_string40         # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.short	801                     # DW_AT_decl_line
	.long	4525                    # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\270\177"
	.byte	9                       # Abbrev [9] 0x182:0x10 DW_TAG_formal_parameter
	.long	.Linfo_string53         # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.short	802                     # DW_AT_decl_line
	.long	72                      # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\264\177"
	.byte	9                       # Abbrev [9] 0x192:0x10 DW_TAG_formal_parameter
	.long	.Linfo_string76         # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.short	803                     # DW_AT_decl_line
	.long	72                      # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\260\177"
	.byte	9                       # Abbrev [9] 0x1a2:0x10 DW_TAG_formal_parameter
	.long	.Linfo_string79         # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.short	804                     # DW_AT_decl_line
	.long	4690                    # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\250\177"
	.byte	10                      # Abbrev [10] 0x1b2:0x10 DW_TAG_variable
	.long	.Linfo_string81         # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.short	806                     # DW_AT_decl_line
	.long	72                      # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\244\177"
	.byte	10                      # Abbrev [10] 0x1c2:0x10 DW_TAG_variable
	.long	.Linfo_string87         # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.short	806                     # DW_AT_decl_line
	.long	72                      # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\240\177"
	.byte	10                      # Abbrev [10] 0x1d2:0x10 DW_TAG_variable
	.long	.Linfo_string88         # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.short	806                     # DW_AT_decl_line
	.long	72                      # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\234\177"
	.byte	10                      # Abbrev [10] 0x1e2:0x10 DW_TAG_variable
	.long	.Linfo_string96         # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.short	806                     # DW_AT_decl_line
	.long	72                      # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\230\177"
	.byte	10                      # Abbrev [10] 0x1f2:0x10 DW_TAG_variable
	.long	.Linfo_string97         # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.short	806                     # DW_AT_decl_line
	.long	72                      # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\224\177"
	.byte	10                      # Abbrev [10] 0x202:0x10 DW_TAG_variable
	.long	.Linfo_string98         # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.short	807                     # DW_AT_decl_line
	.long	4678                    # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\220w"
	.byte	10                      # Abbrev [10] 0x212:0x10 DW_TAG_variable
	.long	.Linfo_string99         # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.short	808                     # DW_AT_decl_line
	.long	4558                    # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\220u"
	.byte	10                      # Abbrev [10] 0x222:0x10 DW_TAG_variable
	.long	.Linfo_string100        # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.short	809                     # DW_AT_decl_line
	.long	4678                    # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\220m"
	.byte	10                      # Abbrev [10] 0x232:0x10 DW_TAG_variable
	.long	.Linfo_string101        # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.short	810                     # DW_AT_decl_line
	.long	4678                    # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\220e"
	.byte	10                      # Abbrev [10] 0x242:0x10 DW_TAG_variable
	.long	.Linfo_string102        # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.short	811                     # DW_AT_decl_line
	.long	2047                    # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\217e"
	.byte	10                      # Abbrev [10] 0x252:0x10 DW_TAG_variable
	.long	.Linfo_string103        # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.short	812                     # DW_AT_decl_line
	.long	72                      # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\210e"
	.byte	10                      # Abbrev [10] 0x262:0x10 DW_TAG_variable
	.long	.Linfo_string18         # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.short	813                     # DW_AT_decl_line
	.long	4540                    # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\206e"
	.byte	12                      # Abbrev [12] 0x272:0x32 DW_TAG_lexical_block
	.quad	.Ltmp154                # DW_AT_low_pc
	.quad	.Ltmp170                # DW_AT_high_pc
	.byte	10                      # Abbrev [10] 0x283:0x10 DW_TAG_variable
	.long	.Linfo_string104        # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.short	890                     # DW_AT_decl_line
	.long	72                      # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\200e"
	.byte	10                      # Abbrev [10] 0x293:0x10 DW_TAG_variable
	.long	.Linfo_string105        # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.short	891                     # DW_AT_decl_line
	.long	72                      # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\374d"
	.byte	0                       # End Of Children Mark
	.byte	12                      # Abbrev [12] 0x2a4:0xe8 DW_TAG_lexical_block
	.quad	.Ltmp171                # DW_AT_low_pc
	.quad	.Ltmp223                # DW_AT_high_pc
	.byte	13                      # Abbrev [13] 0x2b5:0xd6 DW_TAG_lexical_block
	.long	.Ldebug_ranges7         # DW_AT_ranges
	.byte	13                      # Abbrev [13] 0x2ba:0x44 DW_TAG_lexical_block
	.long	.Ldebug_ranges5         # DW_AT_ranges
	.byte	13                      # Abbrev [13] 0x2bf:0x3e DW_TAG_lexical_block
	.long	.Ldebug_ranges4         # DW_AT_ranges
	.byte	13                      # Abbrev [13] 0x2c4:0x38 DW_TAG_lexical_block
	.long	.Ldebug_ranges3         # DW_AT_ranges
	.byte	13                      # Abbrev [13] 0x2c9:0x32 DW_TAG_lexical_block
	.long	.Ldebug_ranges2         # DW_AT_ranges
	.byte	13                      # Abbrev [13] 0x2ce:0x2c DW_TAG_lexical_block
	.long	.Ldebug_ranges1         # DW_AT_ranges
	.byte	13                      # Abbrev [13] 0x2d3:0x26 DW_TAG_lexical_block
	.long	.Ldebug_ranges0         # DW_AT_ranges
	.byte	10                      # Abbrev [10] 0x2d8:0x10 DW_TAG_variable
	.long	.Linfo_string106        # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.short	937                     # DW_AT_decl_line
	.long	72                      # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\370d"
	.byte	10                      # Abbrev [10] 0x2e8:0x10 DW_TAG_variable
	.long	.Linfo_string107        # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.short	938                     # DW_AT_decl_line
	.long	72                      # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\364d"
	.byte	0                       # End Of Children Mark
	.byte	0                       # End Of Children Mark
	.byte	0                       # End Of Children Mark
	.byte	0                       # End Of Children Mark
	.byte	0                       # End Of Children Mark
	.byte	0                       # End Of Children Mark
	.byte	13                      # Abbrev [13] 0x2fe:0x8c DW_TAG_lexical_block
	.long	.Ldebug_ranges6         # DW_AT_ranges
	.byte	12                      # Abbrev [12] 0x303:0x86 DW_TAG_lexical_block
	.quad	.Ltmp211                # DW_AT_low_pc
	.quad	.Ltmp223                # DW_AT_high_pc
	.byte	10                      # Abbrev [10] 0x314:0x10 DW_TAG_variable
	.long	.Linfo_string108        # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.short	1037                    # DW_AT_decl_line
	.long	72                      # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\360d"
	.byte	10                      # Abbrev [10] 0x324:0x10 DW_TAG_variable
	.long	.Linfo_string109        # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.short	1038                    # DW_AT_decl_line
	.long	72                      # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\354d"
	.byte	10                      # Abbrev [10] 0x334:0x10 DW_TAG_variable
	.long	.Linfo_string110        # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.short	1039                    # DW_AT_decl_line
	.long	72                      # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\350d"
	.byte	12                      # Abbrev [12] 0x344:0x44 DW_TAG_lexical_block
	.quad	.Ltmp214                # DW_AT_low_pc
	.quad	.Ltmp221                # DW_AT_high_pc
	.byte	12                      # Abbrev [12] 0x355:0x32 DW_TAG_lexical_block
	.quad	.Ltmp217                # DW_AT_low_pc
	.quad	.Ltmp221                # DW_AT_high_pc
	.byte	10                      # Abbrev [10] 0x366:0x10 DW_TAG_variable
	.long	.Linfo_string111        # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.short	1044                    # DW_AT_decl_line
	.long	72                      # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\344d"
	.byte	10                      # Abbrev [10] 0x376:0x10 DW_TAG_variable
	.long	.Linfo_string112        # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.short	1045                    # DW_AT_decl_line
	.long	4540                    # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\342d"
	.byte	0                       # End Of Children Mark
	.byte	0                       # End Of Children Mark
	.byte	0                       # End Of Children Mark
	.byte	0                       # End Of Children Mark
	.byte	0                       # End Of Children Mark
	.byte	0                       # End Of Children Mark
	.byte	0                       # End Of Children Mark
	.byte	11                      # Abbrev [11] 0x38d:0x416 DW_TAG_subprogram
	.long	.Linfo_string8          # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.short	668                     # DW_AT_decl_line
                                        # DW_AT_prototyped
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.quad	.Lfunc_begin3           # DW_AT_low_pc
	.quad	.Lfunc_end3             # DW_AT_high_pc
	.byte	1                       # DW_AT_frame_base
	.byte	86
	.byte	9                       # Abbrev [9] 0x3a8:0xf DW_TAG_formal_parameter
	.long	.Linfo_string42         # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.short	668                     # DW_AT_decl_line
	.long	4525                    # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	104
	.byte	9                       # Abbrev [9] 0x3b7:0xf DW_TAG_formal_parameter
	.long	.Linfo_string43         # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.short	669                     # DW_AT_decl_line
	.long	4530                    # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	96
	.byte	9                       # Abbrev [9] 0x3c6:0xf DW_TAG_formal_parameter
	.long	.Linfo_string78         # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.short	670                     # DW_AT_decl_line
	.long	4535                    # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	88
	.byte	9                       # Abbrev [9] 0x3d5:0xf DW_TAG_formal_parameter
	.long	.Linfo_string53         # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.short	671                     # DW_AT_decl_line
	.long	72                      # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	84
	.byte	9                       # Abbrev [9] 0x3e4:0xf DW_TAG_formal_parameter
	.long	.Linfo_string113        # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.short	672                     # DW_AT_decl_line
	.long	72                      # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	80
	.byte	9                       # Abbrev [9] 0x3f3:0xf DW_TAG_formal_parameter
	.long	.Linfo_string114        # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.short	673                     # DW_AT_decl_line
	.long	72                      # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	76
	.byte	9                       # Abbrev [9] 0x402:0xf DW_TAG_formal_parameter
	.long	.Linfo_string115        # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.short	674                     # DW_AT_decl_line
	.long	72                      # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	72
	.byte	9                       # Abbrev [9] 0x411:0xf DW_TAG_formal_parameter
	.long	.Linfo_string79         # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.short	675                     # DW_AT_decl_line
	.long	4690                    # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	64
	.byte	10                      # Abbrev [10] 0x420:0x10 DW_TAG_variable
	.long	.Linfo_string116        # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.short	677                     # DW_AT_decl_line
	.long	72                      # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\274\177"
	.byte	10                      # Abbrev [10] 0x430:0x10 DW_TAG_variable
	.long	.Linfo_string117        # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.short	677                     # DW_AT_decl_line
	.long	72                      # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\270\177"
	.byte	10                      # Abbrev [10] 0x440:0x10 DW_TAG_variable
	.long	.Linfo_string118        # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.short	677                     # DW_AT_decl_line
	.long	72                      # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\264\177"
	.byte	10                      # Abbrev [10] 0x450:0x10 DW_TAG_variable
	.long	.Linfo_string119        # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.short	677                     # DW_AT_decl_line
	.long	72                      # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\260\177"
	.byte	10                      # Abbrev [10] 0x460:0x10 DW_TAG_variable
	.long	.Linfo_string120        # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.short	677                     # DW_AT_decl_line
	.long	72                      # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\254\177"
	.byte	10                      # Abbrev [10] 0x470:0x10 DW_TAG_variable
	.long	.Linfo_string121        # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.short	677                     # DW_AT_decl_line
	.long	72                      # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\250\177"
	.byte	10                      # Abbrev [10] 0x480:0x10 DW_TAG_variable
	.long	.Linfo_string122        # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.short	677                     # DW_AT_decl_line
	.long	72                      # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\244\177"
	.byte	10                      # Abbrev [10] 0x490:0x10 DW_TAG_variable
	.long	.Linfo_string123        # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.short	678                     # DW_AT_decl_line
	.long	72                      # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\240\177"
	.byte	10                      # Abbrev [10] 0x4a0:0x10 DW_TAG_variable
	.long	.Linfo_string106        # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.short	678                     # DW_AT_decl_line
	.long	72                      # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\234\177"
	.byte	10                      # Abbrev [10] 0x4b0:0x10 DW_TAG_variable
	.long	.Linfo_string107        # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.short	678                     # DW_AT_decl_line
	.long	72                      # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\230\177"
	.byte	10                      # Abbrev [10] 0x4c0:0x10 DW_TAG_variable
	.long	.Linfo_string124        # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.short	678                     # DW_AT_decl_line
	.long	72                      # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\224\177"
	.byte	10                      # Abbrev [10] 0x4d0:0x10 DW_TAG_variable
	.long	.Linfo_string125        # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.short	680                     # DW_AT_decl_line
	.long	4695                    # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\200|"
	.byte	10                      # Abbrev [10] 0x4e0:0x10 DW_TAG_variable
	.long	.Linfo_string126        # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.short	681                     # DW_AT_decl_line
	.long	4695                    # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\360x"
	.byte	10                      # Abbrev [10] 0x4f0:0x10 DW_TAG_variable
	.long	.Linfo_string127        # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.short	682                     # DW_AT_decl_line
	.long	4695                    # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\340u"
	.byte	10                      # Abbrev [10] 0x500:0x10 DW_TAG_variable
	.long	.Linfo_string128        # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.short	684                     # DW_AT_decl_line
	.long	4707                    # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\324u"
	.byte	10                      # Abbrev [10] 0x510:0x10 DW_TAG_variable
	.long	.Linfo_string129        # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.short	685                     # DW_AT_decl_line
	.long	4707                    # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\310u"
	.byte	10                      # Abbrev [10] 0x520:0x10 DW_TAG_variable
	.long	.Linfo_string130        # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.short	686                     # DW_AT_decl_line
	.long	4707                    # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\274u"
	.byte	13                      # Abbrev [13] 0x530:0x272 DW_TAG_lexical_block
	.long	.Ldebug_ranges8         # DW_AT_ranges
	.byte	12                      # Abbrev [12] 0x535:0xe4 DW_TAG_lexical_block
	.quad	.Ltmp249                # DW_AT_low_pc
	.quad	.Ltmp265                # DW_AT_high_pc
	.byte	12                      # Abbrev [12] 0x546:0x22 DW_TAG_lexical_block
	.quad	.Ltmp249                # DW_AT_low_pc
	.quad	.Ltmp250                # DW_AT_high_pc
	.byte	10                      # Abbrev [10] 0x557:0x10 DW_TAG_variable
	.long	.Linfo_string131        # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.short	733                     # DW_AT_decl_line
	.long	72                      # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\260u"
	.byte	0                       # End Of Children Mark
	.byte	12                      # Abbrev [12] 0x568:0x58 DW_TAG_lexical_block
	.quad	.Ltmp251                # DW_AT_low_pc
	.quad	.Ltmp258                # DW_AT_high_pc
	.byte	12                      # Abbrev [12] 0x579:0x46 DW_TAG_lexical_block
	.quad	.Ltmp253                # DW_AT_low_pc
	.quad	.Ltmp256                # DW_AT_high_pc
	.byte	12                      # Abbrev [12] 0x58a:0x34 DW_TAG_lexical_block
	.quad	.Ltmp254                # DW_AT_low_pc
	.quad	.Ltmp256                # DW_AT_high_pc
	.byte	12                      # Abbrev [12] 0x59b:0x22 DW_TAG_lexical_block
	.quad	.Ltmp254                # DW_AT_low_pc
	.quad	.Ltmp255                # DW_AT_high_pc
	.byte	10                      # Abbrev [10] 0x5ac:0x10 DW_TAG_variable
	.long	.Linfo_string131        # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.short	716                     # DW_AT_decl_line
	.long	72                      # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\270u"
	.byte	0                       # End Of Children Mark
	.byte	0                       # End Of Children Mark
	.byte	0                       # End Of Children Mark
	.byte	0                       # End Of Children Mark
	.byte	12                      # Abbrev [12] 0x5c0:0x58 DW_TAG_lexical_block
	.quad	.Ltmp258                # DW_AT_low_pc
	.quad	.Ltmp264                # DW_AT_high_pc
	.byte	12                      # Abbrev [12] 0x5d1:0x46 DW_TAG_lexical_block
	.quad	.Ltmp258                # DW_AT_low_pc
	.quad	.Ltmp260                # DW_AT_high_pc
	.byte	12                      # Abbrev [12] 0x5e2:0x34 DW_TAG_lexical_block
	.quad	.Ltmp258                # DW_AT_low_pc
	.quad	.Ltmp260                # DW_AT_high_pc
	.byte	12                      # Abbrev [12] 0x5f3:0x22 DW_TAG_lexical_block
	.quad	.Ltmp258                # DW_AT_low_pc
	.quad	.Ltmp259                # DW_AT_high_pc
	.byte	10                      # Abbrev [10] 0x604:0x10 DW_TAG_variable
	.long	.Linfo_string131        # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.short	726                     # DW_AT_decl_line
	.long	72                      # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\264u"
	.byte	0                       # End Of Children Mark
	.byte	0                       # End Of Children Mark
	.byte	0                       # End Of Children Mark
	.byte	0                       # End Of Children Mark
	.byte	0                       # End Of Children Mark
	.byte	12                      # Abbrev [12] 0x619:0x76 DW_TAG_lexical_block
	.quad	.Ltmp269                # DW_AT_low_pc
	.quad	.Ltmp275                # DW_AT_high_pc
	.byte	10                      # Abbrev [10] 0x62a:0x10 DW_TAG_variable
	.long	.Linfo_string132        # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.short	743                     # DW_AT_decl_line
	.long	72                      # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\254u"
	.byte	10                      # Abbrev [10] 0x63a:0x10 DW_TAG_variable
	.long	.Linfo_string133        # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.short	743                     # DW_AT_decl_line
	.long	72                      # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\250u"
	.byte	10                      # Abbrev [10] 0x64a:0x10 DW_TAG_variable
	.long	.Linfo_string134        # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.short	743                     # DW_AT_decl_line
	.long	72                      # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\244u"
	.byte	12                      # Abbrev [12] 0x65a:0x34 DW_TAG_lexical_block
	.quad	.Ltmp272                # DW_AT_low_pc
	.quad	.Ltmp274                # DW_AT_high_pc
	.byte	12                      # Abbrev [12] 0x66b:0x22 DW_TAG_lexical_block
	.quad	.Ltmp272                # DW_AT_low_pc
	.quad	.Ltmp273                # DW_AT_high_pc
	.byte	10                      # Abbrev [10] 0x67c:0x10 DW_TAG_variable
	.long	.Linfo_string131        # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.short	743                     # DW_AT_decl_line
	.long	72                      # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\240u"
	.byte	0                       # End Of Children Mark
	.byte	0                       # End Of Children Mark
	.byte	0                       # End Of Children Mark
	.byte	12                      # Abbrev [12] 0x68f:0x76 DW_TAG_lexical_block
	.quad	.Ltmp277                # DW_AT_low_pc
	.quad	.Ltmp283                # DW_AT_high_pc
	.byte	10                      # Abbrev [10] 0x6a0:0x10 DW_TAG_variable
	.long	.Linfo_string132        # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.short	744                     # DW_AT_decl_line
	.long	72                      # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\234u"
	.byte	10                      # Abbrev [10] 0x6b0:0x10 DW_TAG_variable
	.long	.Linfo_string133        # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.short	744                     # DW_AT_decl_line
	.long	72                      # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\230u"
	.byte	10                      # Abbrev [10] 0x6c0:0x10 DW_TAG_variable
	.long	.Linfo_string134        # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.short	744                     # DW_AT_decl_line
	.long	72                      # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\224u"
	.byte	12                      # Abbrev [12] 0x6d0:0x34 DW_TAG_lexical_block
	.quad	.Ltmp280                # DW_AT_low_pc
	.quad	.Ltmp282                # DW_AT_high_pc
	.byte	12                      # Abbrev [12] 0x6e1:0x22 DW_TAG_lexical_block
	.quad	.Ltmp280                # DW_AT_low_pc
	.quad	.Ltmp281                # DW_AT_high_pc
	.byte	10                      # Abbrev [10] 0x6f2:0x10 DW_TAG_variable
	.long	.Linfo_string131        # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.short	744                     # DW_AT_decl_line
	.long	72                      # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\220u"
	.byte	0                       # End Of Children Mark
	.byte	0                       # End Of Children Mark
	.byte	0                       # End Of Children Mark
	.byte	12                      # Abbrev [12] 0x705:0x34 DW_TAG_lexical_block
	.quad	.Ltmp284                # DW_AT_low_pc
	.quad	.Ltmp286                # DW_AT_high_pc
	.byte	12                      # Abbrev [12] 0x716:0x22 DW_TAG_lexical_block
	.quad	.Ltmp285                # DW_AT_low_pc
	.quad	.Ltmp286                # DW_AT_high_pc
	.byte	10                      # Abbrev [10] 0x727:0x10 DW_TAG_variable
	.long	.Linfo_string135        # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.short	753                     # DW_AT_decl_line
	.long	72                      # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\214u"
	.byte	0                       # End Of Children Mark
	.byte	0                       # End Of Children Mark
	.byte	12                      # Abbrev [12] 0x739:0x34 DW_TAG_lexical_block
	.quad	.Ltmp286                # DW_AT_low_pc
	.quad	.Ltmp288                # DW_AT_high_pc
	.byte	12                      # Abbrev [12] 0x74a:0x22 DW_TAG_lexical_block
	.quad	.Ltmp287                # DW_AT_low_pc
	.quad	.Ltmp288                # DW_AT_high_pc
	.byte	10                      # Abbrev [10] 0x75b:0x10 DW_TAG_variable
	.long	.Linfo_string135        # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.short	754                     # DW_AT_decl_line
	.long	72                      # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\210u"
	.byte	0                       # End Of Children Mark
	.byte	0                       # End Of Children Mark
	.byte	12                      # Abbrev [12] 0x76d:0x34 DW_TAG_lexical_block
	.quad	.Ltmp288                # DW_AT_low_pc
	.quad	.Ltmp290                # DW_AT_high_pc
	.byte	12                      # Abbrev [12] 0x77e:0x22 DW_TAG_lexical_block
	.quad	.Ltmp289                # DW_AT_low_pc
	.quad	.Ltmp290                # DW_AT_high_pc
	.byte	10                      # Abbrev [10] 0x78f:0x10 DW_TAG_variable
	.long	.Linfo_string135        # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.short	755                     # DW_AT_decl_line
	.long	72                      # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\204u"
	.byte	0                       # End Of Children Mark
	.byte	0                       # End Of Children Mark
	.byte	0                       # End Of Children Mark
	.byte	0                       # End Of Children Mark
	.byte	14                      # Abbrev [14] 0x7a3:0x5c DW_TAG_subprogram
	.long	.Linfo_string9          # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.short	630                     # DW_AT_decl_line
                                        # DW_AT_prototyped
	.long	2047                    # DW_AT_type
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.quad	.Lfunc_begin5           # DW_AT_low_pc
	.quad	.Lfunc_end5             # DW_AT_high_pc
	.byte	1                       # DW_AT_frame_base
	.byte	86
	.byte	9                       # Abbrev [9] 0x7c2:0xf DW_TAG_formal_parameter
	.long	.Linfo_string139        # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.short	630                     # DW_AT_decl_line
	.long	2047                    # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	127
	.byte	9                       # Abbrev [9] 0x7d1:0xf DW_TAG_formal_parameter
	.long	.Linfo_string140        # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.short	630                     # DW_AT_decl_line
	.long	2047                    # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	126
	.byte	9                       # Abbrev [9] 0x7e0:0xf DW_TAG_formal_parameter
	.long	.Linfo_string141        # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.short	630                     # DW_AT_decl_line
	.long	2047                    # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	125
	.byte	10                      # Abbrev [10] 0x7ef:0xf DW_TAG_variable
	.long	.Linfo_string142        # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.short	632                     # DW_AT_decl_line
	.long	2047                    # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	124
	.byte	0                       # End Of Children Mark
	.byte	5                       # Abbrev [5] 0x7ff:0xb DW_TAG_typedef
	.long	2058                    # DW_AT_type
	.long	.Linfo_string11         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	83                      # DW_AT_decl_line
	.byte	6                       # Abbrev [6] 0x80a:0x7 DW_TAG_base_type
	.long	.Linfo_string10         # DW_AT_name
	.byte	8                       # DW_AT_encoding
	.byte	1                       # DW_AT_byte_size
	.byte	11                      # Abbrev [11] 0x811:0xf0 DW_TAG_subprogram
	.long	.Linfo_string12         # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.short	532                     # DW_AT_decl_line
                                        # DW_AT_prototyped
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.quad	.Lfunc_begin4           # DW_AT_low_pc
	.quad	.Lfunc_end4             # DW_AT_high_pc
	.byte	1                       # DW_AT_frame_base
	.byte	86
	.byte	9                       # Abbrev [9] 0x82c:0xf DW_TAG_formal_parameter
	.long	.Linfo_string42         # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.short	532                     # DW_AT_decl_line
	.long	4525                    # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	120
	.byte	9                       # Abbrev [9] 0x83b:0xf DW_TAG_formal_parameter
	.long	.Linfo_string43         # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.short	533                     # DW_AT_decl_line
	.long	4530                    # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	112
	.byte	9                       # Abbrev [9] 0x84a:0xf DW_TAG_formal_parameter
	.long	.Linfo_string78         # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.short	534                     # DW_AT_decl_line
	.long	4535                    # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	104
	.byte	9                       # Abbrev [9] 0x859:0xf DW_TAG_formal_parameter
	.long	.Linfo_string53         # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.short	535                     # DW_AT_decl_line
	.long	72                      # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	100
	.byte	9                       # Abbrev [9] 0x868:0xf DW_TAG_formal_parameter
	.long	.Linfo_string106        # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.short	536                     # DW_AT_decl_line
	.long	72                      # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	96
	.byte	9                       # Abbrev [9] 0x877:0xf DW_TAG_formal_parameter
	.long	.Linfo_string107        # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.short	537                     # DW_AT_decl_line
	.long	72                      # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	92
	.byte	9                       # Abbrev [9] 0x886:0xf DW_TAG_formal_parameter
	.long	.Linfo_string124        # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.short	538                     # DW_AT_decl_line
	.long	72                      # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	88
	.byte	9                       # Abbrev [9] 0x895:0xf DW_TAG_formal_parameter
	.long	.Linfo_string79         # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.short	539                     # DW_AT_decl_line
	.long	4690                    # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	80
	.byte	10                      # Abbrev [10] 0x8a4:0xf DW_TAG_variable
	.long	.Linfo_string81         # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.short	541                     # DW_AT_decl_line
	.long	72                      # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	76
	.byte	10                      # Abbrev [10] 0x8b3:0xf DW_TAG_variable
	.long	.Linfo_string87         # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.short	541                     # DW_AT_decl_line
	.long	72                      # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	72
	.byte	10                      # Abbrev [10] 0x8c2:0xf DW_TAG_variable
	.long	.Linfo_string105        # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.short	541                     # DW_AT_decl_line
	.long	72                      # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	68
	.byte	10                      # Abbrev [10] 0x8d1:0xf DW_TAG_variable
	.long	.Linfo_string136        # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.short	541                     # DW_AT_decl_line
	.long	72                      # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	64
	.byte	10                      # Abbrev [10] 0x8e0:0x10 DW_TAG_variable
	.long	.Linfo_string137        # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.short	541                     # DW_AT_decl_line
	.long	72                      # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\274\177"
	.byte	10                      # Abbrev [10] 0x8f0:0x10 DW_TAG_variable
	.long	.Linfo_string138        # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.short	542                     # DW_AT_decl_line
	.long	4514                    # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\270\177"
	.byte	0                       # End Of Children Mark
	.byte	14                      # Abbrev [14] 0x901:0xc5 DW_TAG_subprogram
	.long	.Linfo_string13         # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.short	394                     # DW_AT_decl_line
                                        # DW_AT_prototyped
	.long	2502                    # DW_AT_type
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.quad	.Lfunc_begin6           # DW_AT_low_pc
	.quad	.Lfunc_end6             # DW_AT_high_pc
	.byte	1                       # DW_AT_frame_base
	.byte	86
	.byte	9                       # Abbrev [9] 0x920:0xf DW_TAG_formal_parameter
	.long	.Linfo_string143        # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.short	394                     # DW_AT_decl_line
	.long	4514                    # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	120
	.byte	9                       # Abbrev [9] 0x92f:0xf DW_TAG_formal_parameter
	.long	.Linfo_string144        # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.short	395                     # DW_AT_decl_line
	.long	4514                    # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	116
	.byte	9                       # Abbrev [9] 0x93e:0xf DW_TAG_formal_parameter
	.long	.Linfo_string43         # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.short	396                     # DW_AT_decl_line
	.long	4530                    # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	104
	.byte	9                       # Abbrev [9] 0x94d:0xf DW_TAG_formal_parameter
	.long	.Linfo_string78         # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.short	397                     # DW_AT_decl_line
	.long	4535                    # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	96
	.byte	9                       # Abbrev [9] 0x95c:0xf DW_TAG_formal_parameter
	.long	.Linfo_string53         # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.short	398                     # DW_AT_decl_line
	.long	4514                    # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	92
	.byte	9                       # Abbrev [9] 0x96b:0xf DW_TAG_formal_parameter
	.long	.Linfo_string79         # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.short	399                     # DW_AT_decl_line
	.long	4690                    # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	80
	.byte	10                      # Abbrev [10] 0x97a:0xf DW_TAG_variable
	.long	.Linfo_string88         # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.short	401                     # DW_AT_decl_line
	.long	72                      # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	76
	.byte	10                      # Abbrev [10] 0x989:0xf DW_TAG_variable
	.long	.Linfo_string102        # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.short	402                     # DW_AT_decl_line
	.long	2047                    # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	75
	.byte	10                      # Abbrev [10] 0x998:0xf DW_TAG_variable
	.long	.Linfo_string145        # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.short	402                     # DW_AT_decl_line
	.long	2047                    # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	74
	.byte	10                      # Abbrev [10] 0x9a7:0xf DW_TAG_variable
	.long	.Linfo_string146        # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.short	403                     # DW_AT_decl_line
	.long	4540                    # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	72
	.byte	10                      # Abbrev [10] 0x9b6:0xf DW_TAG_variable
	.long	.Linfo_string147        # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.short	403                     # DW_AT_decl_line
	.long	4540                    # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	70
	.byte	0                       # End Of Children Mark
	.byte	5                       # Abbrev [5] 0x9c6:0xb DW_TAG_typedef
	.long	2058                    # DW_AT_type
	.long	.Linfo_string14         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	82                      # DW_AT_decl_line
	.byte	11                      # Abbrev [11] 0x9d1:0x139 DW_TAG_subprogram
	.long	.Linfo_string15         # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.short	259                     # DW_AT_decl_line
                                        # DW_AT_prototyped
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.quad	.Lfunc_begin1           # DW_AT_low_pc
	.quad	.Lfunc_end1             # DW_AT_high_pc
	.byte	1                       # DW_AT_frame_base
	.byte	86
	.byte	9                       # Abbrev [9] 0x9ec:0xf DW_TAG_formal_parameter
	.long	.Linfo_string82         # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.short	259                     # DW_AT_decl_line
	.long	4525                    # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	80
	.byte	9                       # Abbrev [9] 0x9fb:0xf DW_TAG_formal_parameter
	.long	.Linfo_string83         # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.short	260                     # DW_AT_decl_line
	.long	4525                    # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	72
	.byte	9                       # Abbrev [9] 0xa0a:0xf DW_TAG_formal_parameter
	.long	.Linfo_string84         # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.short	261                     # DW_AT_decl_line
	.long	4525                    # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	64
	.byte	9                       # Abbrev [9] 0xa19:0x10 DW_TAG_formal_parameter
	.long	.Linfo_string53         # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.short	262                     # DW_AT_decl_line
	.long	72                      # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\274\177"
	.byte	9                       # Abbrev [9] 0xa29:0x10 DW_TAG_formal_parameter
	.long	.Linfo_string76         # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.short	263                     # DW_AT_decl_line
	.long	72                      # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\270\177"
	.byte	10                      # Abbrev [10] 0xa39:0x10 DW_TAG_variable
	.long	.Linfo_string40         # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.short	265                     # DW_AT_decl_line
	.long	4665                    # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\260w"
	.byte	10                      # Abbrev [10] 0xa49:0x10 DW_TAG_variable
	.long	.Linfo_string85         # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.short	266                     # DW_AT_decl_line
	.long	4678                    # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\260o"
	.byte	10                      # Abbrev [10] 0xa59:0x10 DW_TAG_variable
	.long	.Linfo_string86         # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.short	267                     # DW_AT_decl_line
	.long	72                      # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\254o"
	.byte	10                      # Abbrev [10] 0xa69:0x10 DW_TAG_variable
	.long	.Linfo_string81         # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.short	267                     # DW_AT_decl_line
	.long	72                      # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\250o"
	.byte	10                      # Abbrev [10] 0xa79:0x10 DW_TAG_variable
	.long	.Linfo_string87         # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.short	267                     # DW_AT_decl_line
	.long	72                      # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\244o"
	.byte	10                      # Abbrev [10] 0xa89:0x10 DW_TAG_variable
	.long	.Linfo_string88         # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.short	267                     # DW_AT_decl_line
	.long	72                      # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\240o"
	.byte	10                      # Abbrev [10] 0xa99:0x10 DW_TAG_variable
	.long	.Linfo_string89         # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.short	267                     # DW_AT_decl_line
	.long	72                      # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\234o"
	.byte	10                      # Abbrev [10] 0xaa9:0x10 DW_TAG_variable
	.long	.Linfo_string90         # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.short	267                     # DW_AT_decl_line
	.long	72                      # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\230o"
	.byte	10                      # Abbrev [10] 0xab9:0x10 DW_TAG_variable
	.long	.Linfo_string91         # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.short	267                     # DW_AT_decl_line
	.long	72                      # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\224o"
	.byte	10                      # Abbrev [10] 0xac9:0x10 DW_TAG_variable
	.long	.Linfo_string92         # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.short	267                     # DW_AT_decl_line
	.long	72                      # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\220o"
	.byte	10                      # Abbrev [10] 0xad9:0x10 DW_TAG_variable
	.long	.Linfo_string93         # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.short	268                     # DW_AT_decl_line
	.long	72                      # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\214o"
	.byte	10                      # Abbrev [10] 0xae9:0x10 DW_TAG_variable
	.long	.Linfo_string94         # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.short	269                     # DW_AT_decl_line
	.long	72                      # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\210o"
	.byte	10                      # Abbrev [10] 0xaf9:0x10 DW_TAG_variable
	.long	.Linfo_string95         # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.short	270                     # DW_AT_decl_line
	.long	4530                    # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\200o"
	.byte	0                       # End Of Children Mark
	.byte	15                      # Abbrev [15] 0xb0a:0x2e0 DW_TAG_subprogram
	.long	.Linfo_string16         # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.byte	140                     # DW_AT_decl_line
                                        # DW_AT_prototyped
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.quad	.Lfunc_begin7           # DW_AT_low_pc
	.quad	.Lfunc_end7             # DW_AT_high_pc
	.byte	1                       # DW_AT_frame_base
	.byte	86
	.byte	16                      # Abbrev [16] 0xb24:0xe DW_TAG_formal_parameter
	.long	.Linfo_string82         # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.byte	140                     # DW_AT_decl_line
	.long	4525                    # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	96
	.byte	16                      # Abbrev [16] 0xb32:0xe DW_TAG_formal_parameter
	.long	.Linfo_string83         # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.byte	141                     # DW_AT_decl_line
	.long	4525                    # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	88
	.byte	16                      # Abbrev [16] 0xb40:0xe DW_TAG_formal_parameter
	.long	.Linfo_string113        # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.byte	142                     # DW_AT_decl_line
	.long	72                      # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	84
	.byte	16                      # Abbrev [16] 0xb4e:0xe DW_TAG_formal_parameter
	.long	.Linfo_string114        # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.byte	143                     # DW_AT_decl_line
	.long	72                      # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	80
	.byte	17                      # Abbrev [17] 0xb5c:0xe DW_TAG_variable
	.long	.Linfo_string116        # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.byte	145                     # DW_AT_decl_line
	.long	72                      # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	76
	.byte	17                      # Abbrev [17] 0xb6a:0xe DW_TAG_variable
	.long	.Linfo_string117        # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.byte	145                     # DW_AT_decl_line
	.long	72                      # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	72
	.byte	17                      # Abbrev [17] 0xb78:0xe DW_TAG_variable
	.long	.Linfo_string118        # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.byte	145                     # DW_AT_decl_line
	.long	72                      # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	68
	.byte	17                      # Abbrev [17] 0xb86:0xe DW_TAG_variable
	.long	.Linfo_string119        # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.byte	145                     # DW_AT_decl_line
	.long	72                      # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	64
	.byte	17                      # Abbrev [17] 0xb94:0xf DW_TAG_variable
	.long	.Linfo_string120        # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.byte	145                     # DW_AT_decl_line
	.long	72                      # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\274\177"
	.byte	17                      # Abbrev [17] 0xba3:0xf DW_TAG_variable
	.long	.Linfo_string121        # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.byte	145                     # DW_AT_decl_line
	.long	72                      # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\270\177"
	.byte	17                      # Abbrev [17] 0xbb2:0xf DW_TAG_variable
	.long	.Linfo_string123        # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.byte	146                     # DW_AT_decl_line
	.long	72                      # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\264\177"
	.byte	17                      # Abbrev [17] 0xbc1:0xf DW_TAG_variable
	.long	.Linfo_string106        # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.byte	146                     # DW_AT_decl_line
	.long	72                      # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\260\177"
	.byte	17                      # Abbrev [17] 0xbd0:0xf DW_TAG_variable
	.long	.Linfo_string107        # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.byte	146                     # DW_AT_decl_line
	.long	72                      # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\254\177"
	.byte	17                      # Abbrev [17] 0xbdf:0xf DW_TAG_variable
	.long	.Linfo_string122        # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.byte	147                     # DW_AT_decl_line
	.long	4514                    # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\250\177"
	.byte	17                      # Abbrev [17] 0xbee:0xf DW_TAG_variable
	.long	.Linfo_string90         # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.byte	147                     # DW_AT_decl_line
	.long	4514                    # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\244\177"
	.byte	17                      # Abbrev [17] 0xbfd:0xf DW_TAG_variable
	.long	.Linfo_string148        # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.byte	147                     # DW_AT_decl_line
	.long	4514                    # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\240\177"
	.byte	17                      # Abbrev [17] 0xc0c:0xf DW_TAG_variable
	.long	.Linfo_string125        # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.byte	148                     # DW_AT_decl_line
	.long	4695                    # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\220|"
	.byte	17                      # Abbrev [17] 0xc1b:0xf DW_TAG_variable
	.long	.Linfo_string126        # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.byte	149                     # DW_AT_decl_line
	.long	4695                    # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\200y"
	.byte	13                      # Abbrev [13] 0xc2a:0x1bf DW_TAG_lexical_block
	.long	.Ldebug_ranges10        # DW_AT_ranges
	.byte	13                      # Abbrev [13] 0xc2f:0xd5 DW_TAG_lexical_block
	.long	.Ldebug_ranges9         # DW_AT_ranges
	.byte	12                      # Abbrev [12] 0xc34:0x21 DW_TAG_lexical_block
	.quad	.Ltmp482                # DW_AT_low_pc
	.quad	.Ltmp483                # DW_AT_high_pc
	.byte	17                      # Abbrev [17] 0xc45:0xf DW_TAG_variable
	.long	.Linfo_string131        # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.byte	206                     # DW_AT_decl_line
	.long	72                      # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\364x"
	.byte	0                       # End Of Children Mark
	.byte	12                      # Abbrev [12] 0xc55:0x57 DW_TAG_lexical_block
	.quad	.Ltmp485                # DW_AT_low_pc
	.quad	.Ltmp491                # DW_AT_high_pc
	.byte	12                      # Abbrev [12] 0xc66:0x45 DW_TAG_lexical_block
	.quad	.Ltmp485                # DW_AT_low_pc
	.quad	.Ltmp487                # DW_AT_high_pc
	.byte	12                      # Abbrev [12] 0xc77:0x33 DW_TAG_lexical_block
	.quad	.Ltmp485                # DW_AT_low_pc
	.quad	.Ltmp487                # DW_AT_high_pc
	.byte	12                      # Abbrev [12] 0xc88:0x21 DW_TAG_lexical_block
	.quad	.Ltmp485                # DW_AT_low_pc
	.quad	.Ltmp486                # DW_AT_high_pc
	.byte	17                      # Abbrev [17] 0xc99:0xf DW_TAG_variable
	.long	.Linfo_string131        # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.byte	187                     # DW_AT_decl_line
	.long	72                      # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\374x"
	.byte	0                       # End Of Children Mark
	.byte	0                       # End Of Children Mark
	.byte	0                       # End Of Children Mark
	.byte	0                       # End Of Children Mark
	.byte	12                      # Abbrev [12] 0xcac:0x57 DW_TAG_lexical_block
	.quad	.Ltmp491                # DW_AT_low_pc
	.quad	.Ltmp497                # DW_AT_high_pc
	.byte	12                      # Abbrev [12] 0xcbd:0x45 DW_TAG_lexical_block
	.quad	.Ltmp491                # DW_AT_low_pc
	.quad	.Ltmp493                # DW_AT_high_pc
	.byte	12                      # Abbrev [12] 0xcce:0x33 DW_TAG_lexical_block
	.quad	.Ltmp491                # DW_AT_low_pc
	.quad	.Ltmp493                # DW_AT_high_pc
	.byte	12                      # Abbrev [12] 0xcdf:0x21 DW_TAG_lexical_block
	.quad	.Ltmp491                # DW_AT_low_pc
	.quad	.Ltmp492                # DW_AT_high_pc
	.byte	17                      # Abbrev [17] 0xcf0:0xf DW_TAG_variable
	.long	.Linfo_string131        # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.byte	198                     # DW_AT_decl_line
	.long	72                      # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\370x"
	.byte	0                       # End Of Children Mark
	.byte	0                       # End Of Children Mark
	.byte	0                       # End Of Children Mark
	.byte	0                       # End Of Children Mark
	.byte	0                       # End Of Children Mark
	.byte	12                      # Abbrev [12] 0xd04:0x72 DW_TAG_lexical_block
	.quad	.Ltmp501                # DW_AT_low_pc
	.quad	.Ltmp507                # DW_AT_high_pc
	.byte	17                      # Abbrev [17] 0xd15:0xf DW_TAG_variable
	.long	.Linfo_string132        # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.byte	213                     # DW_AT_decl_line
	.long	72                      # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\360x"
	.byte	17                      # Abbrev [17] 0xd24:0xf DW_TAG_variable
	.long	.Linfo_string133        # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.byte	213                     # DW_AT_decl_line
	.long	72                      # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\354x"
	.byte	17                      # Abbrev [17] 0xd33:0xf DW_TAG_variable
	.long	.Linfo_string134        # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.byte	213                     # DW_AT_decl_line
	.long	72                      # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\350x"
	.byte	12                      # Abbrev [12] 0xd42:0x33 DW_TAG_lexical_block
	.quad	.Ltmp504                # DW_AT_low_pc
	.quad	.Ltmp506                # DW_AT_high_pc
	.byte	12                      # Abbrev [12] 0xd53:0x21 DW_TAG_lexical_block
	.quad	.Ltmp504                # DW_AT_low_pc
	.quad	.Ltmp505                # DW_AT_high_pc
	.byte	17                      # Abbrev [17] 0xd64:0xf DW_TAG_variable
	.long	.Linfo_string131        # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.byte	213                     # DW_AT_decl_line
	.long	72                      # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\344x"
	.byte	0                       # End Of Children Mark
	.byte	0                       # End Of Children Mark
	.byte	0                       # End Of Children Mark
	.byte	12                      # Abbrev [12] 0xd76:0x72 DW_TAG_lexical_block
	.quad	.Ltmp509                # DW_AT_low_pc
	.quad	.Ltmp515                # DW_AT_high_pc
	.byte	17                      # Abbrev [17] 0xd87:0xf DW_TAG_variable
	.long	.Linfo_string132        # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.byte	214                     # DW_AT_decl_line
	.long	72                      # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\340x"
	.byte	17                      # Abbrev [17] 0xd96:0xf DW_TAG_variable
	.long	.Linfo_string133        # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.byte	214                     # DW_AT_decl_line
	.long	72                      # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\334x"
	.byte	17                      # Abbrev [17] 0xda5:0xf DW_TAG_variable
	.long	.Linfo_string134        # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.byte	214                     # DW_AT_decl_line
	.long	72                      # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\330x"
	.byte	12                      # Abbrev [12] 0xdb4:0x33 DW_TAG_lexical_block
	.quad	.Ltmp512                # DW_AT_low_pc
	.quad	.Ltmp514                # DW_AT_high_pc
	.byte	12                      # Abbrev [12] 0xdc5:0x21 DW_TAG_lexical_block
	.quad	.Ltmp512                # DW_AT_low_pc
	.quad	.Ltmp513                # DW_AT_high_pc
	.byte	17                      # Abbrev [17] 0xdd6:0xf DW_TAG_variable
	.long	.Linfo_string131        # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.byte	214                     # DW_AT_decl_line
	.long	72                      # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\324x"
	.byte	0                       # End Of Children Mark
	.byte	0                       # End Of Children Mark
	.byte	0                       # End Of Children Mark
	.byte	0                       # End Of Children Mark
	.byte	0                       # End Of Children Mark
	.byte	15                      # Abbrev [15] 0xdea:0x8b DW_TAG_subprogram
	.long	.Linfo_string17         # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.byte	79                      # DW_AT_decl_line
                                        # DW_AT_prototyped
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.quad	.Lfunc_begin8           # DW_AT_low_pc
	.quad	.Lfunc_end8             # DW_AT_high_pc
	.byte	1                       # DW_AT_frame_base
	.byte	86
	.byte	16                      # Abbrev [16] 0xe04:0xe DW_TAG_formal_parameter
	.long	.Linfo_string82         # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.byte	79                      # DW_AT_decl_line
	.long	4525                    # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	120
	.byte	16                      # Abbrev [16] 0xe12:0xe DW_TAG_formal_parameter
	.long	.Linfo_string83         # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.byte	80                      # DW_AT_decl_line
	.long	4525                    # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	112
	.byte	16                      # Abbrev [16] 0xe20:0xe DW_TAG_formal_parameter
	.long	.Linfo_string106        # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.byte	81                      # DW_AT_decl_line
	.long	72                      # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	108
	.byte	16                      # Abbrev [16] 0xe2e:0xe DW_TAG_formal_parameter
	.long	.Linfo_string107        # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.byte	82                      # DW_AT_decl_line
	.long	72                      # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	104
	.byte	17                      # Abbrev [17] 0xe3c:0xe DW_TAG_variable
	.long	.Linfo_string81         # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.byte	84                      # DW_AT_decl_line
	.long	72                      # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	100
	.byte	17                      # Abbrev [17] 0xe4a:0xe DW_TAG_variable
	.long	.Linfo_string87         # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.byte	84                      # DW_AT_decl_line
	.long	72                      # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	96
	.byte	17                      # Abbrev [17] 0xe58:0xe DW_TAG_variable
	.long	.Linfo_string149        # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.byte	84                      # DW_AT_decl_line
	.long	72                      # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	92
	.byte	17                      # Abbrev [17] 0xe66:0xe DW_TAG_variable
	.long	.Linfo_string150        # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.byte	85                      # DW_AT_decl_line
	.long	4514                    # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	88
	.byte	0                       # End Of Children Mark
	.byte	18                      # Abbrev [18] 0xe75:0x5 DW_TAG_pointer_type
	.long	3706                    # DW_AT_type
	.byte	19                      # Abbrev [19] 0xe7a:0xc DW_TAG_typedef
	.long	3718                    # DW_AT_type
	.long	.Linfo_string75         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	300                     # DW_AT_decl_line
	.byte	20                      # Abbrev [20] 0xe86:0x22c DW_TAG_structure_type
	.short	55768                   # DW_AT_byte_size
	.byte	1                       # DW_AT_decl_file
	.byte	231                     # DW_AT_decl_line
	.byte	21                      # Abbrev [21] 0xe8b:0xd DW_TAG_member
	.long	.Linfo_string19         # DW_AT_name
	.long	4274                    # DW_AT_type
	.byte	1                       # DW_AT_decl_file
	.byte	233                     # DW_AT_decl_line
	.byte	0                       # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	21                      # Abbrev [21] 0xe98:0xd DW_TAG_member
	.long	.Linfo_string35         # DW_AT_name
	.long	72                      # DW_AT_type
	.byte	1                       # DW_AT_decl_file
	.byte	237                     # DW_AT_decl_line
	.byte	8                       # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	21                      # Abbrev [21] 0xea5:0xd DW_TAG_member
	.long	.Linfo_string30         # DW_AT_name
	.long	72                      # DW_AT_type
	.byte	1                       # DW_AT_decl_file
	.byte	238                     # DW_AT_decl_line
	.byte	12                      # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	21                      # Abbrev [21] 0xeb2:0xd DW_TAG_member
	.long	.Linfo_string36         # DW_AT_name
	.long	4514                    # DW_AT_type
	.byte	1                       # DW_AT_decl_file
	.byte	241                     # DW_AT_decl_line
	.byte	16                      # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	21                      # Abbrev [21] 0xebf:0xd DW_TAG_member
	.long	.Linfo_string38         # DW_AT_name
	.long	4525                    # DW_AT_type
	.byte	1                       # DW_AT_decl_file
	.byte	244                     # DW_AT_decl_line
	.byte	24                      # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	21                      # Abbrev [21] 0xecc:0xd DW_TAG_member
	.long	.Linfo_string39         # DW_AT_name
	.long	4525                    # DW_AT_type
	.byte	1                       # DW_AT_decl_file
	.byte	245                     # DW_AT_decl_line
	.byte	32                      # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	21                      # Abbrev [21] 0xed9:0xd DW_TAG_member
	.long	.Linfo_string40         # DW_AT_name
	.long	4525                    # DW_AT_type
	.byte	1                       # DW_AT_decl_file
	.byte	246                     # DW_AT_decl_line
	.byte	40                      # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	21                      # Abbrev [21] 0xee6:0xd DW_TAG_member
	.long	.Linfo_string41         # DW_AT_name
	.long	72                      # DW_AT_type
	.byte	1                       # DW_AT_decl_file
	.byte	247                     # DW_AT_decl_line
	.byte	48                      # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	21                      # Abbrev [21] 0xef3:0xd DW_TAG_member
	.long	.Linfo_string42         # DW_AT_name
	.long	4525                    # DW_AT_type
	.byte	1                       # DW_AT_decl_file
	.byte	250                     # DW_AT_decl_line
	.byte	56                      # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	21                      # Abbrev [21] 0xf00:0xd DW_TAG_member
	.long	.Linfo_string43         # DW_AT_name
	.long	4530                    # DW_AT_type
	.byte	1                       # DW_AT_decl_file
	.byte	251                     # DW_AT_decl_line
	.byte	64                      # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	21                      # Abbrev [21] 0xf0d:0xd DW_TAG_member
	.long	.Linfo_string44         # DW_AT_name
	.long	4535                    # DW_AT_type
	.byte	1                       # DW_AT_decl_file
	.byte	252                     # DW_AT_decl_line
	.byte	72                      # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	21                      # Abbrev [21] 0xf1a:0xd DW_TAG_member
	.long	.Linfo_string47         # DW_AT_name
	.long	4530                    # DW_AT_type
	.byte	1                       # DW_AT_decl_file
	.byte	253                     # DW_AT_decl_line
	.byte	80                      # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	22                      # Abbrev [22] 0xf27:0xe DW_TAG_member
	.long	.Linfo_string48         # DW_AT_name
	.long	72                      # DW_AT_type
	.byte	1                       # DW_AT_decl_file
	.short	256                     # DW_AT_decl_line
	.byte	88                      # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	22                      # Abbrev [22] 0xf35:0xe DW_TAG_member
	.long	.Linfo_string49         # DW_AT_name
	.long	4514                    # DW_AT_type
	.byte	1                       # DW_AT_decl_file
	.short	259                     # DW_AT_decl_line
	.byte	92                      # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	22                      # Abbrev [22] 0xf43:0xe DW_TAG_member
	.long	.Linfo_string50         # DW_AT_name
	.long	72                      # DW_AT_type
	.byte	1                       # DW_AT_decl_file
	.short	260                     # DW_AT_decl_line
	.byte	96                      # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	22                      # Abbrev [22] 0xf51:0xe DW_TAG_member
	.long	.Linfo_string51         # DW_AT_name
	.long	72                      # DW_AT_type
	.byte	1                       # DW_AT_decl_file
	.short	261                     # DW_AT_decl_line
	.byte	100                     # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	22                      # Abbrev [22] 0xf5f:0xe DW_TAG_member
	.long	.Linfo_string52         # DW_AT_name
	.long	72                      # DW_AT_type
	.byte	1                       # DW_AT_decl_file
	.short	261                     # DW_AT_decl_line
	.byte	104                     # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	22                      # Abbrev [22] 0xf6d:0xe DW_TAG_member
	.long	.Linfo_string53         # DW_AT_name
	.long	72                      # DW_AT_type
	.byte	1                       # DW_AT_decl_file
	.short	264                     # DW_AT_decl_line
	.byte	108                     # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	22                      # Abbrev [22] 0xf7b:0xe DW_TAG_member
	.long	.Linfo_string54         # DW_AT_name
	.long	72                      # DW_AT_type
	.byte	1                       # DW_AT_decl_file
	.short	265                     # DW_AT_decl_line
	.byte	112                     # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	22                      # Abbrev [22] 0xf89:0xe DW_TAG_member
	.long	.Linfo_string55         # DW_AT_name
	.long	72                      # DW_AT_type
	.byte	1                       # DW_AT_decl_file
	.short	266                     # DW_AT_decl_line
	.byte	116                     # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	22                      # Abbrev [22] 0xf97:0xe DW_TAG_member
	.long	.Linfo_string56         # DW_AT_name
	.long	72                      # DW_AT_type
	.byte	1                       # DW_AT_decl_file
	.short	267                     # DW_AT_decl_line
	.byte	120                     # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	22                      # Abbrev [22] 0xfa5:0xe DW_TAG_member
	.long	.Linfo_string57         # DW_AT_name
	.long	72                      # DW_AT_type
	.byte	1                       # DW_AT_decl_file
	.short	270                     # DW_AT_decl_line
	.byte	124                     # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	22                      # Abbrev [22] 0xfb3:0xe DW_TAG_member
	.long	.Linfo_string58         # DW_AT_name
	.long	4558                    # DW_AT_type
	.byte	1                       # DW_AT_decl_file
	.short	271                     # DW_AT_decl_line
	.byte	128                     # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	23                      # Abbrev [23] 0xfc1:0xf DW_TAG_member
	.long	.Linfo_string59         # DW_AT_name
	.long	4570                    # DW_AT_type
	.byte	1                       # DW_AT_decl_file
	.short	272                     # DW_AT_decl_line
	.short	384                     # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	23                      # Abbrev [23] 0xfd0:0xf DW_TAG_member
	.long	.Linfo_string60         # DW_AT_name
	.long	4514                    # DW_AT_type
	.byte	1                       # DW_AT_decl_file
	.short	275                     # DW_AT_decl_line
	.short	640                     # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	23                      # Abbrev [23] 0xfdf:0xf DW_TAG_member
	.long	.Linfo_string61         # DW_AT_name
	.long	72                      # DW_AT_type
	.byte	1                       # DW_AT_decl_file
	.short	276                     # DW_AT_decl_line
	.short	644                     # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	23                      # Abbrev [23] 0xfee:0xf DW_TAG_member
	.long	.Linfo_string62         # DW_AT_name
	.long	4514                    # DW_AT_type
	.byte	1                       # DW_AT_decl_file
	.short	279                     # DW_AT_decl_line
	.short	648                     # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	23                      # Abbrev [23] 0xffd:0xf DW_TAG_member
	.long	.Linfo_string63         # DW_AT_name
	.long	4514                    # DW_AT_type
	.byte	1                       # DW_AT_decl_file
	.short	280                     # DW_AT_decl_line
	.short	652                     # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	23                      # Abbrev [23] 0x100c:0xf DW_TAG_member
	.long	.Linfo_string64         # DW_AT_name
	.long	72                      # DW_AT_type
	.byte	1                       # DW_AT_decl_file
	.short	283                     # DW_AT_decl_line
	.short	656                     # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	23                      # Abbrev [23] 0x101b:0xf DW_TAG_member
	.long	.Linfo_string65         # DW_AT_name
	.long	72                      # DW_AT_type
	.byte	1                       # DW_AT_decl_file
	.short	284                     # DW_AT_decl_line
	.short	660                     # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	23                      # Abbrev [23] 0x102a:0xf DW_TAG_member
	.long	.Linfo_string66         # DW_AT_name
	.long	72                      # DW_AT_type
	.byte	1                       # DW_AT_decl_file
	.short	285                     # DW_AT_decl_line
	.short	664                     # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	23                      # Abbrev [23] 0x1039:0xf DW_TAG_member
	.long	.Linfo_string67         # DW_AT_name
	.long	72                      # DW_AT_type
	.byte	1                       # DW_AT_decl_file
	.short	288                     # DW_AT_decl_line
	.short	668                     # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	23                      # Abbrev [23] 0x1048:0xf DW_TAG_member
	.long	.Linfo_string68         # DW_AT_name
	.long	4582                    # DW_AT_type
	.byte	1                       # DW_AT_decl_file
	.short	289                     # DW_AT_decl_line
	.short	672                     # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	23                      # Abbrev [23] 0x1057:0xf DW_TAG_member
	.long	.Linfo_string69         # DW_AT_name
	.long	4595                    # DW_AT_type
	.byte	1                       # DW_AT_decl_file
	.short	290                     # DW_AT_decl_line
	.short	1704                    # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	23                      # Abbrev [23] 0x1066:0xf DW_TAG_member
	.long	.Linfo_string70         # DW_AT_name
	.long	4595                    # DW_AT_type
	.byte	1                       # DW_AT_decl_file
	.short	291                     # DW_AT_decl_line
	.short	19706                   # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	23                      # Abbrev [23] 0x1075:0xf DW_TAG_member
	.long	.Linfo_string71         # DW_AT_name
	.long	4608                    # DW_AT_type
	.byte	1                       # DW_AT_decl_file
	.short	293                     # DW_AT_decl_line
	.short	37708                   # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	23                      # Abbrev [23] 0x1084:0xf DW_TAG_member
	.long	.Linfo_string72         # DW_AT_name
	.long	4627                    # DW_AT_type
	.byte	1                       # DW_AT_decl_file
	.short	294                     # DW_AT_decl_line
	.short	39256                   # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	23                      # Abbrev [23] 0x1093:0xf DW_TAG_member
	.long	.Linfo_string73         # DW_AT_name
	.long	4627                    # DW_AT_type
	.byte	1                       # DW_AT_decl_file
	.short	295                     # DW_AT_decl_line
	.short	45448                   # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	23                      # Abbrev [23] 0x10a2:0xf DW_TAG_member
	.long	.Linfo_string74         # DW_AT_name
	.long	4646                    # DW_AT_type
	.byte	1                       # DW_AT_decl_file
	.short	297                     # DW_AT_decl_line
	.short	51640                   # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	0                       # End Of Children Mark
	.byte	18                      # Abbrev [18] 0x10b2:0x5 DW_TAG_pointer_type
	.long	4279                    # DW_AT_type
	.byte	5                       # Abbrev [5] 0x10b7:0xb DW_TAG_typedef
	.long	4290                    # DW_AT_type
	.long	.Linfo_string34         # DW_AT_name
	.byte	3                       # DW_AT_decl_file
	.byte	221                     # DW_AT_decl_line
	.byte	24                      # Abbrev [24] 0x10c2:0xa1 DW_TAG_structure_type
	.byte	80                      # DW_AT_byte_size
	.byte	3                       # DW_AT_decl_file
	.byte	204                     # DW_AT_decl_line
	.byte	21                      # Abbrev [21] 0x10c6:0xd DW_TAG_member
	.long	.Linfo_string20         # DW_AT_name
	.long	4451                    # DW_AT_type
	.byte	3                       # DW_AT_decl_file
	.byte	205                     # DW_AT_decl_line
	.byte	0                       # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	21                      # Abbrev [21] 0x10d3:0xd DW_TAG_member
	.long	.Linfo_string22         # DW_AT_name
	.long	4463                    # DW_AT_type
	.byte	3                       # DW_AT_decl_file
	.byte	206                     # DW_AT_decl_line
	.byte	8                       # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	21                      # Abbrev [21] 0x10e0:0xd DW_TAG_member
	.long	.Linfo_string24         # DW_AT_name
	.long	4463                    # DW_AT_type
	.byte	3                       # DW_AT_decl_file
	.byte	207                     # DW_AT_decl_line
	.byte	12                      # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	21                      # Abbrev [21] 0x10ed:0xd DW_TAG_member
	.long	.Linfo_string25         # DW_AT_name
	.long	4463                    # DW_AT_type
	.byte	3                       # DW_AT_decl_file
	.byte	208                     # DW_AT_decl_line
	.byte	16                      # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	21                      # Abbrev [21] 0x10fa:0xd DW_TAG_member
	.long	.Linfo_string26         # DW_AT_name
	.long	4451                    # DW_AT_type
	.byte	3                       # DW_AT_decl_file
	.byte	210                     # DW_AT_decl_line
	.byte	24                      # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	21                      # Abbrev [21] 0x1107:0xd DW_TAG_member
	.long	.Linfo_string27         # DW_AT_name
	.long	4463                    # DW_AT_type
	.byte	3                       # DW_AT_decl_file
	.byte	211                     # DW_AT_decl_line
	.byte	32                      # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	21                      # Abbrev [21] 0x1114:0xd DW_TAG_member
	.long	.Linfo_string28         # DW_AT_name
	.long	4463                    # DW_AT_type
	.byte	3                       # DW_AT_decl_file
	.byte	212                     # DW_AT_decl_line
	.byte	36                      # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	21                      # Abbrev [21] 0x1121:0xd DW_TAG_member
	.long	.Linfo_string29         # DW_AT_name
	.long	4463                    # DW_AT_type
	.byte	3                       # DW_AT_decl_file
	.byte	213                     # DW_AT_decl_line
	.byte	40                      # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	21                      # Abbrev [21] 0x112e:0xd DW_TAG_member
	.long	.Linfo_string30         # DW_AT_name
	.long	4470                    # DW_AT_type
	.byte	3                       # DW_AT_decl_file
	.byte	215                     # DW_AT_decl_line
	.byte	48                      # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	21                      # Abbrev [21] 0x113b:0xd DW_TAG_member
	.long	.Linfo_string31         # DW_AT_name
	.long	4471                    # DW_AT_type
	.byte	3                       # DW_AT_decl_file
	.byte	217                     # DW_AT_decl_line
	.byte	56                      # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	21                      # Abbrev [21] 0x1148:0xd DW_TAG_member
	.long	.Linfo_string32         # DW_AT_name
	.long	4497                    # DW_AT_type
	.byte	3                       # DW_AT_decl_file
	.byte	218                     # DW_AT_decl_line
	.byte	64                      # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	21                      # Abbrev [21] 0x1155:0xd DW_TAG_member
	.long	.Linfo_string33         # DW_AT_name
	.long	4470                    # DW_AT_type
	.byte	3                       # DW_AT_decl_file
	.byte	219                     # DW_AT_decl_line
	.byte	72                      # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	0                       # End Of Children Mark
	.byte	18                      # Abbrev [18] 0x1163:0x5 DW_TAG_pointer_type
	.long	4456                    # DW_AT_type
	.byte	6                       # Abbrev [6] 0x1168:0x7 DW_TAG_base_type
	.long	.Linfo_string21         # DW_AT_name
	.byte	6                       # DW_AT_encoding
	.byte	1                       # DW_AT_byte_size
	.byte	6                       # Abbrev [6] 0x116f:0x7 DW_TAG_base_type
	.long	.Linfo_string23         # DW_AT_name
	.byte	7                       # DW_AT_encoding
	.byte	4                       # DW_AT_byte_size
	.byte	25                      # Abbrev [25] 0x1176:0x1 DW_TAG_pointer_type
	.byte	18                      # Abbrev [18] 0x1177:0x5 DW_TAG_pointer_type
	.long	4476                    # DW_AT_type
	.byte	26                      # Abbrev [26] 0x117c:0x15 DW_TAG_subroutine_type
	.long	4470                    # DW_AT_type
                                        # DW_AT_prototyped
	.byte	27                      # Abbrev [27] 0x1181:0x5 DW_TAG_formal_parameter
	.long	4470                    # DW_AT_type
	.byte	27                      # Abbrev [27] 0x1186:0x5 DW_TAG_formal_parameter
	.long	83                      # DW_AT_type
	.byte	27                      # Abbrev [27] 0x118b:0x5 DW_TAG_formal_parameter
	.long	83                      # DW_AT_type
	.byte	0                       # End Of Children Mark
	.byte	18                      # Abbrev [18] 0x1191:0x5 DW_TAG_pointer_type
	.long	4502                    # DW_AT_type
	.byte	28                      # Abbrev [28] 0x1196:0xc DW_TAG_subroutine_type
                                        # DW_AT_prototyped
	.byte	27                      # Abbrev [27] 0x1197:0x5 DW_TAG_formal_parameter
	.long	4470                    # DW_AT_type
	.byte	27                      # Abbrev [27] 0x119c:0x5 DW_TAG_formal_parameter
	.long	4470                    # DW_AT_type
	.byte	0                       # End Of Children Mark
	.byte	5                       # Abbrev [5] 0x11a2:0xb DW_TAG_typedef
	.long	4463                    # DW_AT_type
	.long	.Linfo_string37         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	85                      # DW_AT_decl_line
	.byte	18                      # Abbrev [18] 0x11ad:0x5 DW_TAG_pointer_type
	.long	4514                    # DW_AT_type
	.byte	18                      # Abbrev [18] 0x11b2:0x5 DW_TAG_pointer_type
	.long	2047                    # DW_AT_type
	.byte	18                      # Abbrev [18] 0x11b7:0x5 DW_TAG_pointer_type
	.long	4540                    # DW_AT_type
	.byte	5                       # Abbrev [5] 0x11bc:0xb DW_TAG_typedef
	.long	4551                    # DW_AT_type
	.long	.Linfo_string46         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	87                      # DW_AT_decl_line
	.byte	6                       # Abbrev [6] 0x11c7:0x7 DW_TAG_base_type
	.long	.Linfo_string45         # DW_AT_name
	.byte	7                       # DW_AT_encoding
	.byte	2                       # DW_AT_byte_size
	.byte	3                       # Abbrev [3] 0x11ce:0xc DW_TAG_array_type
	.long	2502                    # DW_AT_type
	.byte	4                       # Abbrev [4] 0x11d3:0x6 DW_TAG_subrange_type
	.long	90                      # DW_AT_type
	.byte	255                     # DW_AT_upper_bound
	.byte	0                       # End Of Children Mark
	.byte	3                       # Abbrev [3] 0x11da:0xc DW_TAG_array_type
	.long	2047                    # DW_AT_type
	.byte	4                       # Abbrev [4] 0x11df:0x6 DW_TAG_subrange_type
	.long	90                      # DW_AT_type
	.byte	255                     # DW_AT_upper_bound
	.byte	0                       # End Of Children Mark
	.byte	3                       # Abbrev [3] 0x11e6:0xd DW_TAG_array_type
	.long	72                      # DW_AT_type
	.byte	29                      # Abbrev [29] 0x11eb:0x7 DW_TAG_subrange_type
	.long	90                      # DW_AT_type
	.short	257                     # DW_AT_upper_bound
	.byte	0                       # End Of Children Mark
	.byte	3                       # Abbrev [3] 0x11f3:0xd DW_TAG_array_type
	.long	2047                    # DW_AT_type
	.byte	29                      # Abbrev [29] 0x11f8:0x7 DW_TAG_subrange_type
	.long	90                      # DW_AT_type
	.short	18001                   # DW_AT_upper_bound
	.byte	0                       # End Of Children Mark
	.byte	3                       # Abbrev [3] 0x1200:0x13 DW_TAG_array_type
	.long	2047                    # DW_AT_type
	.byte	4                       # Abbrev [4] 0x1205:0x6 DW_TAG_subrange_type
	.long	90                      # DW_AT_type
	.byte	5                       # DW_AT_upper_bound
	.byte	29                      # Abbrev [29] 0x120b:0x7 DW_TAG_subrange_type
	.long	90                      # DW_AT_type
	.short	257                     # DW_AT_upper_bound
	.byte	0                       # End Of Children Mark
	.byte	3                       # Abbrev [3] 0x1213:0x13 DW_TAG_array_type
	.long	72                      # DW_AT_type
	.byte	4                       # Abbrev [4] 0x1218:0x6 DW_TAG_subrange_type
	.long	90                      # DW_AT_type
	.byte	5                       # DW_AT_upper_bound
	.byte	29                      # Abbrev [29] 0x121e:0x7 DW_TAG_subrange_type
	.long	90                      # DW_AT_type
	.short	257                     # DW_AT_upper_bound
	.byte	0                       # End Of Children Mark
	.byte	3                       # Abbrev [3] 0x1226:0x13 DW_TAG_array_type
	.long	4514                    # DW_AT_type
	.byte	29                      # Abbrev [29] 0x122b:0x7 DW_TAG_subrange_type
	.long	90                      # DW_AT_type
	.short	257                     # DW_AT_upper_bound
	.byte	4                       # Abbrev [4] 0x1232:0x6 DW_TAG_subrange_type
	.long	90                      # DW_AT_type
	.byte	3                       # DW_AT_upper_bound
	.byte	0                       # End Of Children Mark
	.byte	3                       # Abbrev [3] 0x1239:0xd DW_TAG_array_type
	.long	72                      # DW_AT_type
	.byte	29                      # Abbrev [29] 0x123e:0x7 DW_TAG_subrange_type
	.long	90                      # DW_AT_type
	.short	256                     # DW_AT_upper_bound
	.byte	0                       # End Of Children Mark
	.byte	3                       # Abbrev [3] 0x1246:0xc DW_TAG_array_type
	.long	72                      # DW_AT_type
	.byte	4                       # Abbrev [4] 0x124b:0x6 DW_TAG_subrange_type
	.long	90                      # DW_AT_type
	.byte	255                     # DW_AT_upper_bound
	.byte	0                       # End Of Children Mark
	.byte	18                      # Abbrev [18] 0x1252:0x5 DW_TAG_pointer_type
	.long	72                      # DW_AT_type
	.byte	3                       # Abbrev [3] 0x1257:0xc DW_TAG_array_type
	.long	72                      # DW_AT_type
	.byte	4                       # Abbrev [4] 0x125c:0x6 DW_TAG_subrange_type
	.long	90                      # DW_AT_type
	.byte	99                      # DW_AT_upper_bound
	.byte	0                       # End Of Children Mark
	.byte	3                       # Abbrev [3] 0x1263:0xc DW_TAG_array_type
	.long	72                      # DW_AT_type
	.byte	4                       # Abbrev [4] 0x1268:0x6 DW_TAG_subrange_type
	.long	90                      # DW_AT_type
	.byte	2                       # DW_AT_upper_bound
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
	.byte	52                      # DW_TAG_variable
	.byte	0                       # DW_CHILDREN_no
	.byte	3                       # DW_AT_name
	.byte	14                      # DW_FORM_strp
	.byte	73                      # DW_AT_type
	.byte	19                      # DW_FORM_ref4
	.byte	58                      # DW_AT_decl_file
	.byte	11                      # DW_FORM_data1
	.byte	59                      # DW_AT_decl_line
	.byte	5                       # DW_FORM_data2
	.byte	2                       # DW_AT_location
	.byte	24                      # DW_FORM_exprloc
	.byte	0                       # EOM(1)
	.byte	0                       # EOM(2)
	.byte	3                       # Abbreviation Code
	.byte	1                       # DW_TAG_array_type
	.byte	1                       # DW_CHILDREN_yes
	.byte	73                      # DW_AT_type
	.byte	19                      # DW_FORM_ref4
	.byte	0                       # EOM(1)
	.byte	0                       # EOM(2)
	.byte	4                       # Abbreviation Code
	.byte	33                      # DW_TAG_subrange_type
	.byte	0                       # DW_CHILDREN_no
	.byte	73                      # DW_AT_type
	.byte	19                      # DW_FORM_ref4
	.byte	47                      # DW_AT_upper_bound
	.byte	11                      # DW_FORM_data1
	.byte	0                       # EOM(1)
	.byte	0                       # EOM(2)
	.byte	5                       # Abbreviation Code
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
	.byte	6                       # Abbreviation Code
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
	.byte	7                       # Abbreviation Code
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
	.byte	8                       # Abbreviation Code
	.byte	46                      # DW_TAG_subprogram
	.byte	1                       # DW_CHILDREN_yes
	.byte	3                       # DW_AT_name
	.byte	14                      # DW_FORM_strp
	.byte	58                      # DW_AT_decl_file
	.byte	11                      # DW_FORM_data1
	.byte	59                      # DW_AT_decl_line
	.byte	5                       # DW_FORM_data2
	.byte	39                      # DW_AT_prototyped
	.byte	25                      # DW_FORM_flag_present
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
	.byte	9                       # Abbreviation Code
	.byte	5                       # DW_TAG_formal_parameter
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
	.byte	10                      # Abbreviation Code
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
	.byte	11                      # Abbreviation Code
	.byte	46                      # DW_TAG_subprogram
	.byte	1                       # DW_CHILDREN_yes
	.byte	3                       # DW_AT_name
	.byte	14                      # DW_FORM_strp
	.byte	58                      # DW_AT_decl_file
	.byte	11                      # DW_FORM_data1
	.byte	59                      # DW_AT_decl_line
	.byte	5                       # DW_FORM_data2
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
	.byte	12                      # Abbreviation Code
	.byte	11                      # DW_TAG_lexical_block
	.byte	1                       # DW_CHILDREN_yes
	.byte	17                      # DW_AT_low_pc
	.byte	1                       # DW_FORM_addr
	.byte	18                      # DW_AT_high_pc
	.byte	1                       # DW_FORM_addr
	.byte	0                       # EOM(1)
	.byte	0                       # EOM(2)
	.byte	13                      # Abbreviation Code
	.byte	11                      # DW_TAG_lexical_block
	.byte	1                       # DW_CHILDREN_yes
	.byte	85                      # DW_AT_ranges
	.byte	23                      # DW_FORM_sec_offset
	.byte	0                       # EOM(1)
	.byte	0                       # EOM(2)
	.byte	14                      # Abbreviation Code
	.byte	46                      # DW_TAG_subprogram
	.byte	1                       # DW_CHILDREN_yes
	.byte	3                       # DW_AT_name
	.byte	14                      # DW_FORM_strp
	.byte	58                      # DW_AT_decl_file
	.byte	11                      # DW_FORM_data1
	.byte	59                      # DW_AT_decl_line
	.byte	5                       # DW_FORM_data2
	.byte	39                      # DW_AT_prototyped
	.byte	25                      # DW_FORM_flag_present
	.byte	73                      # DW_AT_type
	.byte	19                      # DW_FORM_ref4
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
	.byte	15                      # Abbreviation Code
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
	.byte	16                      # Abbreviation Code
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
	.byte	17                      # Abbreviation Code
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
	.byte	18                      # Abbreviation Code
	.byte	15                      # DW_TAG_pointer_type
	.byte	0                       # DW_CHILDREN_no
	.byte	73                      # DW_AT_type
	.byte	19                      # DW_FORM_ref4
	.byte	0                       # EOM(1)
	.byte	0                       # EOM(2)
	.byte	19                      # Abbreviation Code
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
	.byte	20                      # Abbreviation Code
	.byte	19                      # DW_TAG_structure_type
	.byte	1                       # DW_CHILDREN_yes
	.byte	11                      # DW_AT_byte_size
	.byte	5                       # DW_FORM_data2
	.byte	58                      # DW_AT_decl_file
	.byte	11                      # DW_FORM_data1
	.byte	59                      # DW_AT_decl_line
	.byte	11                      # DW_FORM_data1
	.byte	0                       # EOM(1)
	.byte	0                       # EOM(2)
	.byte	21                      # Abbreviation Code
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
	.byte	22                      # Abbreviation Code
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
	.byte	23                      # Abbreviation Code
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
	.byte	24                      # Abbreviation Code
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
	.byte	25                      # Abbreviation Code
	.byte	15                      # DW_TAG_pointer_type
	.byte	0                       # DW_CHILDREN_no
	.byte	0                       # EOM(1)
	.byte	0                       # EOM(2)
	.byte	26                      # Abbreviation Code
	.byte	21                      # DW_TAG_subroutine_type
	.byte	1                       # DW_CHILDREN_yes
	.byte	73                      # DW_AT_type
	.byte	19                      # DW_FORM_ref4
	.byte	39                      # DW_AT_prototyped
	.byte	25                      # DW_FORM_flag_present
	.byte	0                       # EOM(1)
	.byte	0                       # EOM(2)
	.byte	27                      # Abbreviation Code
	.byte	5                       # DW_TAG_formal_parameter
	.byte	0                       # DW_CHILDREN_no
	.byte	73                      # DW_AT_type
	.byte	19                      # DW_FORM_ref4
	.byte	0                       # EOM(1)
	.byte	0                       # EOM(2)
	.byte	28                      # Abbreviation Code
	.byte	21                      # DW_TAG_subroutine_type
	.byte	1                       # DW_CHILDREN_yes
	.byte	39                      # DW_AT_prototyped
	.byte	25                      # DW_FORM_flag_present
	.byte	0                       # EOM(1)
	.byte	0                       # EOM(2)
	.byte	29                      # Abbreviation Code
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
	.quad	.Ltmp172
	.quad	.Ltmp173
	.quad	.Ltmp179
	.quad	.Ltmp188
	.quad	0
	.quad	0
.Ldebug_ranges1:
	.quad	.Ltmp172
	.quad	.Ltmp173
	.quad	.Ltmp178
	.quad	.Ltmp188
	.quad	0
	.quad	0
.Ldebug_ranges2:
	.quad	.Ltmp172
	.quad	.Ltmp173
	.quad	.Ltmp177
	.quad	.Ltmp189
	.quad	0
	.quad	0
.Ldebug_ranges3:
	.quad	.Ltmp172
	.quad	.Ltmp173
	.quad	.Ltmp176
	.quad	.Ltmp189
	.quad	0
	.quad	0
.Ldebug_ranges4:
	.quad	.Ltmp172
	.quad	.Ltmp173
	.quad	.Ltmp176
	.quad	.Ltmp189
	.quad	0
	.quad	0
.Ldebug_ranges5:
	.quad	.Ltmp172
	.quad	.Ltmp173
	.quad	.Ltmp175
	.quad	.Ltmp189
	.quad	0
	.quad	0
.Ldebug_ranges6:
	.quad	.Ltmp208
	.quad	.Ltmp209
	.quad	.Ltmp210
	.quad	.Ltmp223
	.quad	0
	.quad	0
.Ldebug_ranges7:
	.quad	.Ltmp172
	.quad	.Ltmp173
	.quad	.Ltmp174
	.quad	.Ltmp223
	.quad	0
	.quad	0
.Ldebug_ranges8:
	.quad	.Ltmp237
	.quad	.Ltmp240
	.quad	.Ltmp241
	.quad	.Ltmp290
	.quad	0
	.quad	0
.Ldebug_ranges9:
	.quad	.Ltmp482
	.quad	.Ltmp484
	.quad	.Ltmp485
	.quad	.Ltmp498
	.quad	0
	.quad	0
.Ldebug_ranges10:
	.quad	.Ltmp471
	.quad	.Ltmp472
	.quad	.Ltmp473
	.quad	.Ltmp526
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
	.long	97                      # DIE offset
	.asciz	"BZ2_blockSort"         # External Name
	.long	909                     # DIE offset
	.asciz	"mainQSort3"            # External Name
	.long	3562                    # DIE offset
	.asciz	"fallbackSimpleSort"    # External Name
	.long	2305                    # DIE offset
	.asciz	"mainGtU"               # External Name
	.long	1955                    # DIE offset
	.asciz	"mmed3"                 # External Name
	.long	2065                    # DIE offset
	.asciz	"mainSimpleSort"        # External Name
	.long	2513                    # DIE offset
	.asciz	"fallbackSort"          # External Name
	.long	2826                    # DIE offset
	.asciz	"fallbackQSort3"        # External Name
	.long	298                     # DIE offset
	.asciz	"mainSort"              # External Name
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
	.long	72                      # DIE offset
	.asciz	"Int32"                 # External Name
	.long	4551                    # DIE offset
	.asciz	"unsigned short"        # External Name
	.long	2047                    # DIE offset
	.asciz	"UChar"                 # External Name
	.long	4514                    # DIE offset
	.asciz	"UInt32"                # External Name
	.long	3706                    # DIE offset
	.asciz	"EState"                # External Name
	.long	4279                    # DIE offset
	.asciz	"bz_stream"             # External Name
	.long	4463                    # DIE offset
	.asciz	"unsigned int"          # External Name
	.long	4540                    # DIE offset
	.asciz	"UInt16"                # External Name
	.long	83                      # DIE offset
	.asciz	"int"                   # External Name
	.long	2058                    # DIE offset
	.asciz	"unsigned char"         # External Name
	.long	2502                    # DIE offset
	.asciz	"Bool"                  # External Name
	.long	4456                    # DIE offset
	.asciz	"char"                  # External Name
	.long	0                       # End Mark
.Lpubtypes_end0:

	.ident	"clang version 3.5 (trunk)"
	.section	".note.GNU-stack","",@progbits
