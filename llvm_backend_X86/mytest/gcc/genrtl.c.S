	.file	"genrtl.c.bc"
	.text
	.globl	gen_rtx_fmt_s
	.align	16, 0x90
	.type	gen_rtx_fmt_s,@function
gen_rtx_fmt_s:                          # @gen_rtx_fmt_s
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp4:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp5:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp6:
	.cfi_def_cfa_offset 32
.Ltmp7:
	.cfi_offset %rbx, -32
.Ltmp8:
	.cfi_offset %r14, -24
.Ltmp9:
	.cfi_offset %r15, -16
	movq	%rdx, %r14
	movq	%rsi, %rbx
	movq	%rdi, %r15
	movq	$16, %rdi
	callq	ggc_alloc
	movq	$0, (%rax)
	andq	$65535, %r15            # imm = 0xFFFF
	movq	$16, %rcx
	shlq	%cl, %rbx
	andq	$16711680, %rbx         # imm = 0xFF0000
	orq	%r15, %rbx
	movl	%ebx, (%rax)
	movq	%r14, 8(%rax)
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp10:
	.size	gen_rtx_fmt_s, .Ltmp10-gen_rtx_fmt_s
	.cfi_endproc

	.globl	gen_rtx_fmt_ee
	.align	16, 0x90
	.type	gen_rtx_fmt_ee,@function
gen_rtx_fmt_ee:                         # @gen_rtx_fmt_ee
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp16:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp17:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp18:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp19:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp20:
	.cfi_def_cfa_offset 48
.Ltmp21:
	.cfi_offset %rbx, -40
.Ltmp22:
	.cfi_offset %r12, -32
.Ltmp23:
	.cfi_offset %r14, -24
.Ltmp24:
	.cfi_offset %r15, -16
	movq	%rcx, %r14
	movq	%rdx, %r15
	movq	%rsi, %rbx
	movq	%rdi, %r12
	movq	$24, %rdi
	callq	ggc_alloc
	movq	$0, (%rax)
	andq	$65535, %r12            # imm = 0xFFFF
	movq	$16, %rcx
	shlq	%cl, %rbx
	andq	$16711680, %rbx         # imm = 0xFF0000
	orq	%r12, %rbx
	movl	%ebx, (%rax)
	movq	%r15, 8(%rax)
	movq	%r14, 16(%rax)
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.Ltmp25:
	.size	gen_rtx_fmt_ee, .Ltmp25-gen_rtx_fmt_ee
	.cfi_endproc

	.globl	gen_rtx_fmt_ue
	.align	16, 0x90
	.type	gen_rtx_fmt_ue,@function
gen_rtx_fmt_ue:                         # @gen_rtx_fmt_ue
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp31:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp32:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp33:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp34:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp35:
	.cfi_def_cfa_offset 48
.Ltmp36:
	.cfi_offset %rbx, -40
.Ltmp37:
	.cfi_offset %r12, -32
.Ltmp38:
	.cfi_offset %r14, -24
.Ltmp39:
	.cfi_offset %r15, -16
	movq	%rcx, %r14
	movq	%rdx, %r15
	movq	%rsi, %rbx
	movq	%rdi, %r12
	movq	$24, %rdi
	callq	ggc_alloc
	movq	$0, (%rax)
	andq	$65535, %r12            # imm = 0xFFFF
	movq	$16, %rcx
	shlq	%cl, %rbx
	andq	$16711680, %rbx         # imm = 0xFF0000
	orq	%r12, %rbx
	movl	%ebx, (%rax)
	movq	%r15, 8(%rax)
	movq	%r14, 16(%rax)
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.Ltmp40:
	.size	gen_rtx_fmt_ue, .Ltmp40-gen_rtx_fmt_ue
	.cfi_endproc

	.globl	gen_rtx_fmt_iss
	.align	16, 0x90
	.type	gen_rtx_fmt_iss,@function
gen_rtx_fmt_iss:                        # @gen_rtx_fmt_iss
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
.Ltmp52:
	.cfi_offset %rbx, -48
.Ltmp53:
	.cfi_offset %r12, -40
.Ltmp54:
	.cfi_offset %r13, -32
.Ltmp55:
	.cfi_offset %r14, -24
.Ltmp56:
	.cfi_offset %r15, -16
	movq	%r8, %r14
	movq	%rcx, %r15
	movq	%rdx, %r12
	movq	%rsi, %rbx
	movq	%rdi, %r13
	movq	$32, %rdi
	callq	ggc_alloc
	movq	$0, (%rax)
	andq	$65535, %r13            # imm = 0xFFFF
	movq	$16, %rcx
	shlq	%cl, %rbx
	andq	$16711680, %rbx         # imm = 0xFF0000
	orq	%r13, %rbx
	movl	%ebx, (%rax)
	movl	%r12d, 8(%rax)
	movq	%r15, 16(%rax)
	movq	%r14, 24(%rax)
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp57:
	.size	gen_rtx_fmt_iss, .Ltmp57-gen_rtx_fmt_iss
	.cfi_endproc

	.globl	gen_rtx_fmt_is
	.align	16, 0x90
	.type	gen_rtx_fmt_is,@function
gen_rtx_fmt_is:                         # @gen_rtx_fmt_is
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp63:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp64:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp65:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp66:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp67:
	.cfi_def_cfa_offset 48
.Ltmp68:
	.cfi_offset %rbx, -40
.Ltmp69:
	.cfi_offset %r12, -32
.Ltmp70:
	.cfi_offset %r14, -24
.Ltmp71:
	.cfi_offset %r15, -16
	movq	%rcx, %r14
	movq	%rdx, %r15
	movq	%rsi, %rbx
	movq	%rdi, %r12
	movq	$24, %rdi
	callq	ggc_alloc
	movq	$0, (%rax)
	andq	$65535, %r12            # imm = 0xFFFF
	movq	$16, %rcx
	shlq	%cl, %rbx
	andq	$16711680, %rbx         # imm = 0xFF0000
	orq	%r12, %rbx
	movl	%ebx, (%rax)
	movl	%r15d, 8(%rax)
	movq	%r14, 16(%rax)
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.Ltmp72:
	.size	gen_rtx_fmt_is, .Ltmp72-gen_rtx_fmt_is
	.cfi_endproc

	.globl	gen_rtx_fmt_i
	.align	16, 0x90
	.type	gen_rtx_fmt_i,@function
gen_rtx_fmt_i:                          # @gen_rtx_fmt_i
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp77:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp78:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp79:
	.cfi_def_cfa_offset 32
.Ltmp80:
	.cfi_offset %rbx, -32
.Ltmp81:
	.cfi_offset %r14, -24
.Ltmp82:
	.cfi_offset %r15, -16
	movq	%rdx, %r14
	movq	%rsi, %rbx
	movq	%rdi, %r15
	movq	$16, %rdi
	callq	ggc_alloc
	movq	$0, (%rax)
	andq	$65535, %r15            # imm = 0xFFFF
	movq	$16, %rcx
	shlq	%cl, %rbx
	andq	$16711680, %rbx         # imm = 0xFF0000
	orq	%r15, %rbx
	movl	%ebx, (%rax)
	movl	%r14d, 8(%rax)
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp83:
	.size	gen_rtx_fmt_i, .Ltmp83-gen_rtx_fmt_i
	.cfi_endproc

	.globl	gen_rtx_fmt_isE
	.align	16, 0x90
	.type	gen_rtx_fmt_isE,@function
gen_rtx_fmt_isE:                        # @gen_rtx_fmt_isE
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
.Ltmp95:
	.cfi_offset %rbx, -48
.Ltmp96:
	.cfi_offset %r12, -40
.Ltmp97:
	.cfi_offset %r13, -32
.Ltmp98:
	.cfi_offset %r14, -24
.Ltmp99:
	.cfi_offset %r15, -16
	movq	%r8, %r14
	movq	%rcx, %r15
	movq	%rdx, %r12
	movq	%rsi, %rbx
	movq	%rdi, %r13
	movq	$32, %rdi
	callq	ggc_alloc
	movq	$0, (%rax)
	andq	$65535, %r13            # imm = 0xFFFF
	movq	$16, %rcx
	shlq	%cl, %rbx
	andq	$16711680, %rbx         # imm = 0xFF0000
	orq	%r13, %rbx
	movl	%ebx, (%rax)
	movl	%r12d, 8(%rax)
	movq	%r15, 16(%rax)
	movq	%r14, 24(%rax)
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp100:
	.size	gen_rtx_fmt_isE, .Ltmp100-gen_rtx_fmt_isE
	.cfi_endproc

	.globl	gen_rtx_fmt_iE
	.align	16, 0x90
	.type	gen_rtx_fmt_iE,@function
gen_rtx_fmt_iE:                         # @gen_rtx_fmt_iE
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp106:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp107:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp108:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp109:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp110:
	.cfi_def_cfa_offset 48
.Ltmp111:
	.cfi_offset %rbx, -40
.Ltmp112:
	.cfi_offset %r12, -32
.Ltmp113:
	.cfi_offset %r14, -24
.Ltmp114:
	.cfi_offset %r15, -16
	movq	%rcx, %r14
	movq	%rdx, %r15
	movq	%rsi, %rbx
	movq	%rdi, %r12
	movq	$24, %rdi
	callq	ggc_alloc
	movq	$0, (%rax)
	andq	$65535, %r12            # imm = 0xFFFF
	movq	$16, %rcx
	shlq	%cl, %rbx
	andq	$16711680, %rbx         # imm = 0xFF0000
	orq	%r12, %rbx
	movl	%ebx, (%rax)
	movl	%r15d, 8(%rax)
	movq	%r14, 16(%rax)
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.Ltmp115:
	.size	gen_rtx_fmt_iE, .Ltmp115-gen_rtx_fmt_iE
	.cfi_endproc

	.globl	gen_rtx_fmt_Ess
	.align	16, 0x90
	.type	gen_rtx_fmt_Ess,@function
gen_rtx_fmt_Ess:                        # @gen_rtx_fmt_Ess
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp122:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp123:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp124:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp125:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp126:
	.cfi_def_cfa_offset 48
.Ltmp127:
	.cfi_offset %rbx, -48
.Ltmp128:
	.cfi_offset %r12, -40
.Ltmp129:
	.cfi_offset %r13, -32
.Ltmp130:
	.cfi_offset %r14, -24
.Ltmp131:
	.cfi_offset %r15, -16
	movq	%r8, %r14
	movq	%rcx, %r15
	movq	%rdx, %r12
	movq	%rsi, %rbx
	movq	%rdi, %r13
	movq	$32, %rdi
	callq	ggc_alloc
	movq	$0, (%rax)
	andq	$65535, %r13            # imm = 0xFFFF
	movq	$16, %rcx
	shlq	%cl, %rbx
	andq	$16711680, %rbx         # imm = 0xFF0000
	orq	%r13, %rbx
	movl	%ebx, (%rax)
	movq	%r12, 8(%rax)
	movq	%r15, 16(%rax)
	movq	%r14, 24(%rax)
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp132:
	.size	gen_rtx_fmt_Ess, .Ltmp132-gen_rtx_fmt_Ess
	.cfi_endproc

	.globl	gen_rtx_fmt_sEss
	.align	16, 0x90
	.type	gen_rtx_fmt_sEss,@function
gen_rtx_fmt_sEss:                       # @gen_rtx_fmt_sEss
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp139:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp140:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp141:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp142:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp143:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp144:
	.cfi_def_cfa_offset 64
.Ltmp145:
	.cfi_offset %rbx, -48
.Ltmp146:
	.cfi_offset %r12, -40
.Ltmp147:
	.cfi_offset %r13, -32
.Ltmp148:
	.cfi_offset %r14, -24
.Ltmp149:
	.cfi_offset %r15, -16
	movq	%r9, 8(%rsp)            # 8-byte Spill
	movq	%r8, %r15
	movq	%rcx, %r12
	movq	%rdx, %r13
	movq	%rsi, %rbx
	movq	%rdi, %r14
	movq	$40, %rdi
	callq	ggc_alloc
	movq	$0, (%rax)
	andq	$65535, %r14            # imm = 0xFFFF
	movq	$16, %rcx
	shlq	%cl, %rbx
	andq	$16711680, %rbx         # imm = 0xFF0000
	orq	%r14, %rbx
	movl	%ebx, (%rax)
	movq	%r13, 8(%rax)
	movq	%r12, 16(%rax)
	movq	%r15, 24(%rax)
	movq	8(%rsp), %rcx           # 8-byte Reload
	movq	%rcx, 32(%rax)
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp150:
	.size	gen_rtx_fmt_sEss, .Ltmp150-gen_rtx_fmt_sEss
	.cfi_endproc

	.globl	gen_rtx_fmt_eE
	.align	16, 0x90
	.type	gen_rtx_fmt_eE,@function
gen_rtx_fmt_eE:                         # @gen_rtx_fmt_eE
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp156:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp157:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp158:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp159:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp160:
	.cfi_def_cfa_offset 48
.Ltmp161:
	.cfi_offset %rbx, -40
.Ltmp162:
	.cfi_offset %r12, -32
.Ltmp163:
	.cfi_offset %r14, -24
.Ltmp164:
	.cfi_offset %r15, -16
	movq	%rcx, %r14
	movq	%rdx, %r15
	movq	%rsi, %rbx
	movq	%rdi, %r12
	movq	$24, %rdi
	callq	ggc_alloc
	movq	$0, (%rax)
	andq	$65535, %r12            # imm = 0xFFFF
	movq	$16, %rcx
	shlq	%cl, %rbx
	andq	$16711680, %rbx         # imm = 0xFF0000
	orq	%r12, %rbx
	movl	%ebx, (%rax)
	movq	%r15, 8(%rax)
	movq	%r14, 16(%rax)
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.Ltmp165:
	.size	gen_rtx_fmt_eE, .Ltmp165-gen_rtx_fmt_eE
	.cfi_endproc

	.globl	gen_rtx_fmt_E
	.align	16, 0x90
	.type	gen_rtx_fmt_E,@function
gen_rtx_fmt_E:                          # @gen_rtx_fmt_E
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp170:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp171:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp172:
	.cfi_def_cfa_offset 32
.Ltmp173:
	.cfi_offset %rbx, -32
.Ltmp174:
	.cfi_offset %r14, -24
.Ltmp175:
	.cfi_offset %r15, -16
	movq	%rdx, %r14
	movq	%rsi, %rbx
	movq	%rdi, %r15
	movq	$16, %rdi
	callq	ggc_alloc
	movq	$0, (%rax)
	andq	$65535, %r15            # imm = 0xFFFF
	movq	$16, %rcx
	shlq	%cl, %rbx
	andq	$16711680, %rbx         # imm = 0xFF0000
	orq	%r15, %rbx
	movl	%ebx, (%rax)
	movq	%r14, 8(%rax)
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp176:
	.size	gen_rtx_fmt_E, .Ltmp176-gen_rtx_fmt_E
	.cfi_endproc

	.globl	gen_rtx_fmt_e
	.align	16, 0x90
	.type	gen_rtx_fmt_e,@function
gen_rtx_fmt_e:                          # @gen_rtx_fmt_e
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp181:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp182:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp183:
	.cfi_def_cfa_offset 32
.Ltmp184:
	.cfi_offset %rbx, -32
.Ltmp185:
	.cfi_offset %r14, -24
.Ltmp186:
	.cfi_offset %r15, -16
	movq	%rdx, %r14
	movq	%rsi, %rbx
	movq	%rdi, %r15
	movq	$16, %rdi
	callq	ggc_alloc
	movq	$0, (%rax)
	andq	$65535, %r15            # imm = 0xFFFF
	movq	$16, %rcx
	shlq	%cl, %rbx
	andq	$16711680, %rbx         # imm = 0xFF0000
	orq	%r15, %rbx
	movl	%ebx, (%rax)
	movq	%r14, 8(%rax)
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp187:
	.size	gen_rtx_fmt_e, .Ltmp187-gen_rtx_fmt_e
	.cfi_endproc

	.globl	gen_rtx_fmt_sse
	.align	16, 0x90
	.type	gen_rtx_fmt_sse,@function
gen_rtx_fmt_sse:                        # @gen_rtx_fmt_sse
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp194:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp195:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp196:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp197:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp198:
	.cfi_def_cfa_offset 48
.Ltmp199:
	.cfi_offset %rbx, -48
.Ltmp200:
	.cfi_offset %r12, -40
.Ltmp201:
	.cfi_offset %r13, -32
.Ltmp202:
	.cfi_offset %r14, -24
.Ltmp203:
	.cfi_offset %r15, -16
	movq	%r8, %r14
	movq	%rcx, %r15
	movq	%rdx, %r12
	movq	%rsi, %rbx
	movq	%rdi, %r13
	movq	$32, %rdi
	callq	ggc_alloc
	movq	$0, (%rax)
	andq	$65535, %r13            # imm = 0xFFFF
	movq	$16, %rcx
	shlq	%cl, %rbx
	andq	$16711680, %rbx         # imm = 0xFF0000
	orq	%r13, %rbx
	movl	%ebx, (%rax)
	movq	%r12, 8(%rax)
	movq	%r15, 16(%rax)
	movq	%r14, 24(%rax)
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp204:
	.size	gen_rtx_fmt_sse, .Ltmp204-gen_rtx_fmt_sse
	.cfi_endproc

	.globl	gen_rtx_fmt_ss
	.align	16, 0x90
	.type	gen_rtx_fmt_ss,@function
gen_rtx_fmt_ss:                         # @gen_rtx_fmt_ss
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp210:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp211:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp212:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp213:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp214:
	.cfi_def_cfa_offset 48
.Ltmp215:
	.cfi_offset %rbx, -40
.Ltmp216:
	.cfi_offset %r12, -32
.Ltmp217:
	.cfi_offset %r14, -24
.Ltmp218:
	.cfi_offset %r15, -16
	movq	%rcx, %r14
	movq	%rdx, %r15
	movq	%rsi, %rbx
	movq	%rdi, %r12
	movq	$24, %rdi
	callq	ggc_alloc
	movq	$0, (%rax)
	andq	$65535, %r12            # imm = 0xFFFF
	movq	$16, %rcx
	shlq	%cl, %rbx
	andq	$16711680, %rbx         # imm = 0xFF0000
	orq	%r12, %rbx
	movl	%ebx, (%rax)
	movq	%r15, 8(%rax)
	movq	%r14, 16(%rax)
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.Ltmp219:
	.size	gen_rtx_fmt_ss, .Ltmp219-gen_rtx_fmt_ss
	.cfi_endproc

	.globl	gen_rtx_fmt_sE
	.align	16, 0x90
	.type	gen_rtx_fmt_sE,@function
gen_rtx_fmt_sE:                         # @gen_rtx_fmt_sE
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp225:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp226:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp227:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp228:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp229:
	.cfi_def_cfa_offset 48
.Ltmp230:
	.cfi_offset %rbx, -40
.Ltmp231:
	.cfi_offset %r12, -32
.Ltmp232:
	.cfi_offset %r14, -24
.Ltmp233:
	.cfi_offset %r15, -16
	movq	%rcx, %r14
	movq	%rdx, %r15
	movq	%rsi, %rbx
	movq	%rdi, %r12
	movq	$24, %rdi
	callq	ggc_alloc
	movq	$0, (%rax)
	andq	$65535, %r12            # imm = 0xFFFF
	movq	$16, %rcx
	shlq	%cl, %rbx
	andq	$16711680, %rbx         # imm = 0xFF0000
	orq	%r12, %rbx
	movl	%ebx, (%rax)
	movq	%r15, 8(%rax)
	movq	%r14, 16(%rax)
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.Ltmp234:
	.size	gen_rtx_fmt_sE, .Ltmp234-gen_rtx_fmt_sE
	.cfi_endproc

	.globl	gen_rtx_fmt_iuueiee
	.align	16, 0x90
	.type	gen_rtx_fmt_iuueiee,@function
gen_rtx_fmt_iuueiee:                    # @gen_rtx_fmt_iuueiee
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp241:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp242:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp243:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp244:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp245:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp246:
	.cfi_def_cfa_offset 64
.Ltmp247:
	.cfi_offset %rbx, -48
.Ltmp248:
	.cfi_offset %r12, -40
.Ltmp249:
	.cfi_offset %r13, -32
.Ltmp250:
	.cfi_offset %r14, -24
.Ltmp251:
	.cfi_offset %r15, -16
	movq	%r9, 8(%rsp)            # 8-byte Spill
	movq	%r8, %r15
	movq	%rcx, %r12
	movq	%rdx, %r13
	movq	%rsi, %rbx
	movq	%rdi, %r14
	movq	$64, %rdi
	callq	ggc_alloc
	movq	$0, (%rax)
	andq	$65535, %r14            # imm = 0xFFFF
	movq	$16, %rcx
	shlq	%cl, %rbx
	andq	$16711680, %rbx         # imm = 0xFF0000
	orq	%r14, %rbx
	movl	%ebx, (%rax)
	movl	%r13d, 8(%rax)
	movq	%r12, 16(%rax)
	movq	%r15, 24(%rax)
	movq	8(%rsp), %rcx           # 8-byte Reload
	movq	%rcx, 32(%rax)
	movq	64(%rsp), %rcx
	movl	%ecx, 40(%rax)
	movq	72(%rsp), %rcx
	movq	%rcx, 48(%rax)
	movq	80(%rsp), %rcx
	movq	%rcx, 56(%rax)
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp252:
	.size	gen_rtx_fmt_iuueiee, .Ltmp252-gen_rtx_fmt_iuueiee
	.cfi_endproc

	.globl	gen_rtx_fmt_iuueiee0
	.align	16, 0x90
	.type	gen_rtx_fmt_iuueiee0,@function
gen_rtx_fmt_iuueiee0:                   # @gen_rtx_fmt_iuueiee0
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp259:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp260:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp261:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp262:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp263:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp264:
	.cfi_def_cfa_offset 64
.Ltmp265:
	.cfi_offset %rbx, -48
.Ltmp266:
	.cfi_offset %r12, -40
.Ltmp267:
	.cfi_offset %r13, -32
.Ltmp268:
	.cfi_offset %r14, -24
.Ltmp269:
	.cfi_offset %r15, -16
	movq	%r9, 8(%rsp)            # 8-byte Spill
	movq	%r8, %r15
	movq	%rcx, %r12
	movq	%rdx, %r13
	movq	%rsi, %rbx
	movq	%rdi, %r14
	movq	$72, %rdi
	callq	ggc_alloc
	movq	$0, (%rax)
	andq	$65535, %r14            # imm = 0xFFFF
	movq	$16, %rcx
	shlq	%cl, %rbx
	andq	$16711680, %rbx         # imm = 0xFF0000
	orq	%r14, %rbx
	movl	%ebx, (%rax)
	movl	%r13d, 8(%rax)
	movq	%r12, 16(%rax)
	movq	%r15, 24(%rax)
	movq	8(%rsp), %rcx           # 8-byte Reload
	movq	%rcx, 32(%rax)
	movq	64(%rsp), %rcx
	movl	%ecx, 40(%rax)
	movq	72(%rsp), %rcx
	movq	%rcx, 48(%rax)
	movq	80(%rsp), %rcx
	movq	%rcx, 56(%rax)
	movq	$0, 64(%rax)
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp270:
	.size	gen_rtx_fmt_iuueiee0, .Ltmp270-gen_rtx_fmt_iuueiee0
	.cfi_endproc

	.globl	gen_rtx_fmt_iuueieee
	.align	16, 0x90
	.type	gen_rtx_fmt_iuueieee,@function
gen_rtx_fmt_iuueieee:                   # @gen_rtx_fmt_iuueieee
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp277:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp278:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp279:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp280:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp281:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp282:
	.cfi_def_cfa_offset 64
.Ltmp283:
	.cfi_offset %rbx, -48
.Ltmp284:
	.cfi_offset %r12, -40
.Ltmp285:
	.cfi_offset %r13, -32
.Ltmp286:
	.cfi_offset %r14, -24
.Ltmp287:
	.cfi_offset %r15, -16
	movq	%r9, 8(%rsp)            # 8-byte Spill
	movq	%r8, %r15
	movq	%rcx, %r12
	movq	%rdx, %r13
	movq	%rsi, %rbx
	movq	%rdi, %r14
	movq	$72, %rdi
	callq	ggc_alloc
	movq	$0, (%rax)
	andq	$65535, %r14            # imm = 0xFFFF
	movq	$16, %rcx
	shlq	%cl, %rbx
	andq	$16711680, %rbx         # imm = 0xFF0000
	orq	%r14, %rbx
	movl	%ebx, (%rax)
	movl	%r13d, 8(%rax)
	movq	%r12, 16(%rax)
	movq	%r15, 24(%rax)
	movq	8(%rsp), %rcx           # 8-byte Reload
	movq	%rcx, 32(%rax)
	movq	64(%rsp), %rcx
	movl	%ecx, 40(%rax)
	movq	72(%rsp), %rcx
	movq	%rcx, 48(%rax)
	movq	80(%rsp), %rcx
	movq	%rcx, 56(%rax)
	movq	88(%rsp), %rcx
	movq	%rcx, 64(%rax)
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp288:
	.size	gen_rtx_fmt_iuueieee, .Ltmp288-gen_rtx_fmt_iuueieee
	.cfi_endproc

	.globl	gen_rtx_fmt_iuu
	.align	16, 0x90
	.type	gen_rtx_fmt_iuu,@function
gen_rtx_fmt_iuu:                        # @gen_rtx_fmt_iuu
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp295:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp296:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp297:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp298:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp299:
	.cfi_def_cfa_offset 48
.Ltmp300:
	.cfi_offset %rbx, -48
.Ltmp301:
	.cfi_offset %r12, -40
.Ltmp302:
	.cfi_offset %r13, -32
.Ltmp303:
	.cfi_offset %r14, -24
.Ltmp304:
	.cfi_offset %r15, -16
	movq	%r8, %r14
	movq	%rcx, %r15
	movq	%rdx, %r12
	movq	%rsi, %rbx
	movq	%rdi, %r13
	movq	$32, %rdi
	callq	ggc_alloc
	movq	$0, (%rax)
	andq	$65535, %r13            # imm = 0xFFFF
	movq	$16, %rcx
	shlq	%cl, %rbx
	andq	$16711680, %rbx         # imm = 0xFF0000
	orq	%r13, %rbx
	movl	%ebx, (%rax)
	movl	%r12d, 8(%rax)
	movq	%r15, 16(%rax)
	movq	%r14, 24(%rax)
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp305:
	.size	gen_rtx_fmt_iuu, .Ltmp305-gen_rtx_fmt_iuu
	.cfi_endproc

	.globl	gen_rtx_fmt_iuu00iss
	.align	16, 0x90
	.type	gen_rtx_fmt_iuu00iss,@function
gen_rtx_fmt_iuu00iss:                   # @gen_rtx_fmt_iuu00iss
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp312:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp313:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp314:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp315:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp316:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp317:
	.cfi_def_cfa_offset 64
.Ltmp318:
	.cfi_offset %rbx, -48
.Ltmp319:
	.cfi_offset %r12, -40
.Ltmp320:
	.cfi_offset %r13, -32
.Ltmp321:
	.cfi_offset %r14, -24
.Ltmp322:
	.cfi_offset %r15, -16
	movq	%r9, 8(%rsp)            # 8-byte Spill
	movq	%r8, %r15
	movq	%rcx, %r12
	movq	%rdx, %r13
	movq	%rsi, %rbx
	movq	%rdi, %r14
	movq	$72, %rdi
	callq	ggc_alloc
	movq	$0, (%rax)
	andq	$65535, %r14            # imm = 0xFFFF
	movq	$16, %rcx
	shlq	%cl, %rbx
	andq	$16711680, %rbx         # imm = 0xFF0000
	orq	%r14, %rbx
	movl	%ebx, (%rax)
	movl	%r13d, 8(%rax)
	movq	%r12, 16(%rax)
	movq	%r15, 24(%rax)
	movq	$0, 40(%rax)
	movq	$0, 32(%rax)
	movq	8(%rsp), %rcx           # 8-byte Reload
	movl	%ecx, 48(%rax)
	movq	64(%rsp), %rcx
	movq	%rcx, 56(%rax)
	movq	72(%rsp), %rcx
	movq	%rcx, 64(%rax)
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp323:
	.size	gen_rtx_fmt_iuu00iss, .Ltmp323-gen_rtx_fmt_iuu00iss
	.cfi_endproc

	.globl	gen_rtx_fmt_ssiEEsi
	.align	16, 0x90
	.type	gen_rtx_fmt_ssiEEsi,@function
gen_rtx_fmt_ssiEEsi:                    # @gen_rtx_fmt_ssiEEsi
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp330:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp331:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp332:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp333:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp334:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp335:
	.cfi_def_cfa_offset 64
.Ltmp336:
	.cfi_offset %rbx, -48
.Ltmp337:
	.cfi_offset %r12, -40
.Ltmp338:
	.cfi_offset %r13, -32
.Ltmp339:
	.cfi_offset %r14, -24
.Ltmp340:
	.cfi_offset %r15, -16
	movq	%r9, 8(%rsp)            # 8-byte Spill
	movq	%r8, %r15
	movq	%rcx, %r12
	movq	%rdx, %r13
	movq	%rsi, %rbx
	movq	%rdi, %r14
	movq	$64, %rdi
	callq	ggc_alloc
	movq	$0, (%rax)
	andq	$65535, %r14            # imm = 0xFFFF
	movq	$16, %rcx
	shlq	%cl, %rbx
	andq	$16711680, %rbx         # imm = 0xFF0000
	orq	%r14, %rbx
	movl	%ebx, (%rax)
	movq	%r13, 8(%rax)
	movq	%r12, 16(%rax)
	movl	%r15d, 24(%rax)
	movq	8(%rsp), %rcx           # 8-byte Reload
	movq	%rcx, 32(%rax)
	movq	64(%rsp), %rcx
	movq	%rcx, 40(%rax)
	movq	72(%rsp), %rcx
	movq	%rcx, 48(%rax)
	movq	80(%rsp), %rcx
	movl	%ecx, 56(%rax)
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp341:
	.size	gen_rtx_fmt_ssiEEsi, .Ltmp341-gen_rtx_fmt_ssiEEsi
	.cfi_endproc

	.globl	gen_rtx_fmt_Ei
	.align	16, 0x90
	.type	gen_rtx_fmt_Ei,@function
gen_rtx_fmt_Ei:                         # @gen_rtx_fmt_Ei
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp347:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp348:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp349:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp350:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp351:
	.cfi_def_cfa_offset 48
.Ltmp352:
	.cfi_offset %rbx, -40
.Ltmp353:
	.cfi_offset %r12, -32
.Ltmp354:
	.cfi_offset %r14, -24
.Ltmp355:
	.cfi_offset %r15, -16
	movq	%rcx, %r14
	movq	%rdx, %r15
	movq	%rsi, %rbx
	movq	%rdi, %r12
	movq	$24, %rdi
	callq	ggc_alloc
	movq	$0, (%rax)
	andq	$65535, %r12            # imm = 0xFFFF
	movq	$16, %rcx
	shlq	%cl, %rbx
	andq	$16711680, %rbx         # imm = 0xFF0000
	orq	%r12, %rbx
	movl	%ebx, (%rax)
	movq	%r15, 8(%rax)
	movl	%r14d, 16(%rax)
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.Ltmp356:
	.size	gen_rtx_fmt_Ei, .Ltmp356-gen_rtx_fmt_Ei
	.cfi_endproc

	.globl	gen_rtx_fmt_eEee0
	.align	16, 0x90
	.type	gen_rtx_fmt_eEee0,@function
gen_rtx_fmt_eEee0:                      # @gen_rtx_fmt_eEee0
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp363:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp364:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp365:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp366:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp367:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp368:
	.cfi_def_cfa_offset 64
.Ltmp369:
	.cfi_offset %rbx, -48
.Ltmp370:
	.cfi_offset %r12, -40
.Ltmp371:
	.cfi_offset %r13, -32
.Ltmp372:
	.cfi_offset %r14, -24
.Ltmp373:
	.cfi_offset %r15, -16
	movq	%r9, 8(%rsp)            # 8-byte Spill
	movq	%r8, %r15
	movq	%rcx, %r12
	movq	%rdx, %r13
	movq	%rsi, %rbx
	movq	%rdi, %r14
	movq	$48, %rdi
	callq	ggc_alloc
	movq	$0, (%rax)
	andq	$65535, %r14            # imm = 0xFFFF
	movq	$16, %rcx
	shlq	%cl, %rbx
	andq	$16711680, %rbx         # imm = 0xFF0000
	orq	%r14, %rbx
	movl	%ebx, (%rax)
	movq	%r13, 8(%rax)
	movq	%r12, 16(%rax)
	movq	%r15, 24(%rax)
	movq	8(%rsp), %rcx           # 8-byte Reload
	movq	%rcx, 32(%rax)
	movq	$0, 40(%rax)
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp374:
	.size	gen_rtx_fmt_eEee0, .Ltmp374-gen_rtx_fmt_eEee0
	.cfi_endproc

	.globl	gen_rtx_fmt_eee
	.align	16, 0x90
	.type	gen_rtx_fmt_eee,@function
gen_rtx_fmt_eee:                        # @gen_rtx_fmt_eee
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp381:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp382:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp383:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp384:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp385:
	.cfi_def_cfa_offset 48
.Ltmp386:
	.cfi_offset %rbx, -48
.Ltmp387:
	.cfi_offset %r12, -40
.Ltmp388:
	.cfi_offset %r13, -32
.Ltmp389:
	.cfi_offset %r14, -24
.Ltmp390:
	.cfi_offset %r15, -16
	movq	%r8, %r14
	movq	%rcx, %r15
	movq	%rdx, %r12
	movq	%rsi, %rbx
	movq	%rdi, %r13
	movq	$32, %rdi
	callq	ggc_alloc
	movq	$0, (%rax)
	andq	$65535, %r13            # imm = 0xFFFF
	movq	$16, %rcx
	shlq	%cl, %rbx
	andq	$16711680, %rbx         # imm = 0xFF0000
	orq	%r13, %rbx
	movl	%ebx, (%rax)
	movq	%r12, 8(%rax)
	movq	%r15, 16(%rax)
	movq	%r14, 24(%rax)
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp391:
	.size	gen_rtx_fmt_eee, .Ltmp391-gen_rtx_fmt_eee
	.cfi_endproc

	.globl	gen_rtx_fmt_
	.align	16, 0x90
	.type	gen_rtx_fmt_,@function
gen_rtx_fmt_:                           # @gen_rtx_fmt_
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp395:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp396:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp397:
	.cfi_def_cfa_offset 32
.Ltmp398:
	.cfi_offset %rbx, -24
.Ltmp399:
	.cfi_offset %r14, -16
	movq	%rsi, %rbx
	movq	%rdi, %r14
	movq	$8, %rdi
	callq	ggc_alloc
	movq	$0, (%rax)
	andq	$65535, %r14            # imm = 0xFFFF
	movq	$16, %rcx
	shlq	%cl, %rbx
	andq	$16711680, %rbx         # imm = 0xFF0000
	orq	%r14, %rbx
	movl	%ebx, (%rax)
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp400:
	.size	gen_rtx_fmt_, .Ltmp400-gen_rtx_fmt_
	.cfi_endproc

	.globl	gen_rtx_fmt_w
	.align	16, 0x90
	.type	gen_rtx_fmt_w,@function
gen_rtx_fmt_w:                          # @gen_rtx_fmt_w
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp405:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp406:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp407:
	.cfi_def_cfa_offset 32
.Ltmp408:
	.cfi_offset %rbx, -32
.Ltmp409:
	.cfi_offset %r14, -24
.Ltmp410:
	.cfi_offset %r15, -16
	movq	%rdx, %r14
	movq	%rsi, %rbx
	movq	%rdi, %r15
	movq	$16, %rdi
	callq	ggc_alloc
	movq	$0, (%rax)
	andq	$65535, %r15            # imm = 0xFFFF
	movq	$16, %rcx
	shlq	%cl, %rbx
	andq	$16711680, %rbx         # imm = 0xFF0000
	orq	%r15, %rbx
	movl	%ebx, (%rax)
	movq	%r14, 8(%rax)
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp411:
	.size	gen_rtx_fmt_w, .Ltmp411-gen_rtx_fmt_w
	.cfi_endproc

	.globl	gen_rtx_fmt_0www
	.align	16, 0x90
	.type	gen_rtx_fmt_0www,@function
gen_rtx_fmt_0www:                       # @gen_rtx_fmt_0www
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp418:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp419:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp420:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp421:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp422:
	.cfi_def_cfa_offset 48
.Ltmp423:
	.cfi_offset %rbx, -48
.Ltmp424:
	.cfi_offset %r12, -40
.Ltmp425:
	.cfi_offset %r13, -32
.Ltmp426:
	.cfi_offset %r14, -24
.Ltmp427:
	.cfi_offset %r15, -16
	movq	%r8, %r14
	movq	%rcx, %r15
	movq	%rdx, %r12
	movq	%rsi, %rbx
	movq	%rdi, %r13
	movq	$40, %rdi
	callq	ggc_alloc
	movq	$0, (%rax)
	andq	$65535, %r13            # imm = 0xFFFF
	movq	$16, %rcx
	shlq	%cl, %rbx
	andq	$16711680, %rbx         # imm = 0xFF0000
	orq	%r13, %rbx
	movl	%ebx, (%rax)
	movq	$0, 8(%rax)
	movq	%r12, 16(%rax)
	movq	%r15, 24(%rax)
	movq	%r14, 32(%rax)
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp428:
	.size	gen_rtx_fmt_0www, .Ltmp428-gen_rtx_fmt_0www
	.cfi_endproc

	.globl	gen_rtx_fmt_0
	.align	16, 0x90
	.type	gen_rtx_fmt_0,@function
gen_rtx_fmt_0:                          # @gen_rtx_fmt_0
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp432:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp433:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp434:
	.cfi_def_cfa_offset 32
.Ltmp435:
	.cfi_offset %rbx, -24
.Ltmp436:
	.cfi_offset %r14, -16
	movq	%rsi, %rbx
	movq	%rdi, %r14
	movq	$16, %rdi
	callq	ggc_alloc
	movq	$0, (%rax)
	andq	$65535, %r14            # imm = 0xFFFF
	movq	$16, %rcx
	shlq	%cl, %rbx
	andq	$16711680, %rbx         # imm = 0xFF0000
	orq	%r14, %rbx
	movl	%ebx, (%rax)
	movq	$0, 8(%rax)
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp437:
	.size	gen_rtx_fmt_0, .Ltmp437-gen_rtx_fmt_0
	.cfi_endproc

	.globl	gen_rtx_fmt_i0
	.align	16, 0x90
	.type	gen_rtx_fmt_i0,@function
gen_rtx_fmt_i0:                         # @gen_rtx_fmt_i0
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp442:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp443:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp444:
	.cfi_def_cfa_offset 32
.Ltmp445:
	.cfi_offset %rbx, -32
.Ltmp446:
	.cfi_offset %r14, -24
.Ltmp447:
	.cfi_offset %r15, -16
	movq	%rdx, %r14
	movq	%rsi, %rbx
	movq	%rdi, %r15
	movq	$24, %rdi
	callq	ggc_alloc
	movq	$0, (%rax)
	andq	$65535, %r15            # imm = 0xFFFF
	movq	$16, %rcx
	shlq	%cl, %rbx
	andq	$16711680, %rbx         # imm = 0xFF0000
	orq	%r15, %rbx
	movl	%ebx, (%rax)
	movl	%r14d, 8(%rax)
	movq	$0, 16(%rax)
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp448:
	.size	gen_rtx_fmt_i0, .Ltmp448-gen_rtx_fmt_i0
	.cfi_endproc

	.globl	gen_rtx_fmt_ei
	.align	16, 0x90
	.type	gen_rtx_fmt_ei,@function
gen_rtx_fmt_ei:                         # @gen_rtx_fmt_ei
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp454:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp455:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp456:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp457:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp458:
	.cfi_def_cfa_offset 48
.Ltmp459:
	.cfi_offset %rbx, -40
.Ltmp460:
	.cfi_offset %r12, -32
.Ltmp461:
	.cfi_offset %r14, -24
.Ltmp462:
	.cfi_offset %r15, -16
	movq	%rcx, %r14
	movq	%rdx, %r15
	movq	%rsi, %rbx
	movq	%rdi, %r12
	movq	$24, %rdi
	callq	ggc_alloc
	movq	$0, (%rax)
	andq	$65535, %r12            # imm = 0xFFFF
	movq	$16, %rcx
	shlq	%cl, %rbx
	andq	$16711680, %rbx         # imm = 0xFF0000
	orq	%r12, %rbx
	movl	%ebx, (%rax)
	movq	%r15, 8(%rax)
	movl	%r14d, 16(%rax)
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.Ltmp463:
	.size	gen_rtx_fmt_ei, .Ltmp463-gen_rtx_fmt_ei
	.cfi_endproc

	.globl	gen_rtx_fmt_e0
	.align	16, 0x90
	.type	gen_rtx_fmt_e0,@function
gen_rtx_fmt_e0:                         # @gen_rtx_fmt_e0
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp468:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp469:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp470:
	.cfi_def_cfa_offset 32
.Ltmp471:
	.cfi_offset %rbx, -32
.Ltmp472:
	.cfi_offset %r14, -24
.Ltmp473:
	.cfi_offset %r15, -16
	movq	%rdx, %r14
	movq	%rsi, %rbx
	movq	%rdi, %r15
	movq	$24, %rdi
	callq	ggc_alloc
	movq	$0, (%rax)
	andq	$65535, %r15            # imm = 0xFFFF
	movq	$16, %rcx
	shlq	%cl, %rbx
	andq	$16711680, %rbx         # imm = 0xFF0000
	orq	%r15, %rbx
	movl	%ebx, (%rax)
	movq	%r14, 8(%rax)
	movq	$0, 16(%rax)
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp474:
	.size	gen_rtx_fmt_e0, .Ltmp474-gen_rtx_fmt_e0
	.cfi_endproc

	.globl	gen_rtx_fmt_u00
	.align	16, 0x90
	.type	gen_rtx_fmt_u00,@function
gen_rtx_fmt_u00:                        # @gen_rtx_fmt_u00
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp479:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp480:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp481:
	.cfi_def_cfa_offset 32
.Ltmp482:
	.cfi_offset %rbx, -32
.Ltmp483:
	.cfi_offset %r14, -24
.Ltmp484:
	.cfi_offset %r15, -16
	movq	%rdx, %r14
	movq	%rsi, %rbx
	movq	%rdi, %r15
	movq	$32, %rdi
	callq	ggc_alloc
	movq	$0, (%rax)
	andq	$65535, %r15            # imm = 0xFFFF
	movq	$16, %rcx
	shlq	%cl, %rbx
	andq	$16711680, %rbx         # imm = 0xFF0000
	orq	%r15, %rbx
	movl	%ebx, (%rax)
	movq	%r14, 8(%rax)
	movq	$0, 24(%rax)
	movq	$0, 16(%rax)
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp485:
	.size	gen_rtx_fmt_u00, .Ltmp485-gen_rtx_fmt_u00
	.cfi_endproc

	.globl	gen_rtx_fmt_eit
	.align	16, 0x90
	.type	gen_rtx_fmt_eit,@function
gen_rtx_fmt_eit:                        # @gen_rtx_fmt_eit
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp492:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp493:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp494:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp495:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp496:
	.cfi_def_cfa_offset 48
.Ltmp497:
	.cfi_offset %rbx, -48
.Ltmp498:
	.cfi_offset %r12, -40
.Ltmp499:
	.cfi_offset %r13, -32
.Ltmp500:
	.cfi_offset %r14, -24
.Ltmp501:
	.cfi_offset %r15, -16
	movq	%r8, %r14
	movq	%rcx, %r15
	movq	%rdx, %r12
	movq	%rsi, %rbx
	movq	%rdi, %r13
	movq	$32, %rdi
	callq	ggc_alloc
	movq	$0, (%rax)
	andq	$65535, %r13            # imm = 0xFFFF
	movq	$16, %rcx
	shlq	%cl, %rbx
	andq	$16711680, %rbx         # imm = 0xFF0000
	orq	%r13, %rbx
	movl	%ebx, (%rax)
	movq	%r12, 8(%rax)
	movl	%r15d, 16(%rax)
	movq	%r14, 24(%rax)
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp502:
	.size	gen_rtx_fmt_eit, .Ltmp502-gen_rtx_fmt_eit
	.cfi_endproc

	.globl	gen_rtx_fmt_eeeee
	.align	16, 0x90
	.type	gen_rtx_fmt_eeeee,@function
gen_rtx_fmt_eeeee:                      # @gen_rtx_fmt_eeeee
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp509:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp510:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp511:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp512:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp513:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp514:
	.cfi_def_cfa_offset 64
.Ltmp515:
	.cfi_offset %rbx, -48
.Ltmp516:
	.cfi_offset %r12, -40
.Ltmp517:
	.cfi_offset %r13, -32
.Ltmp518:
	.cfi_offset %r14, -24
.Ltmp519:
	.cfi_offset %r15, -16
	movq	%r9, 8(%rsp)            # 8-byte Spill
	movq	%r8, %r15
	movq	%rcx, %r12
	movq	%rdx, %r13
	movq	%rsi, %rbx
	movq	%rdi, %r14
	movq	$48, %rdi
	callq	ggc_alloc
	movq	$0, (%rax)
	andq	$65535, %r14            # imm = 0xFFFF
	movq	$16, %rcx
	shlq	%cl, %rbx
	andq	$16711680, %rbx         # imm = 0xFF0000
	orq	%r14, %rbx
	movl	%ebx, (%rax)
	movq	%r13, 8(%rax)
	movq	%r12, 16(%rax)
	movq	%r15, 24(%rax)
	movq	8(%rsp), %rcx           # 8-byte Reload
	movq	%rcx, 32(%rax)
	movq	64(%rsp), %rcx
	movq	%rcx, 40(%rax)
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp520:
	.size	gen_rtx_fmt_eeeee, .Ltmp520-gen_rtx_fmt_eeeee
	.cfi_endproc

	.globl	gen_rtx_fmt_Ee
	.align	16, 0x90
	.type	gen_rtx_fmt_Ee,@function
gen_rtx_fmt_Ee:                         # @gen_rtx_fmt_Ee
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp526:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp527:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp528:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp529:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp530:
	.cfi_def_cfa_offset 48
.Ltmp531:
	.cfi_offset %rbx, -40
.Ltmp532:
	.cfi_offset %r12, -32
.Ltmp533:
	.cfi_offset %r14, -24
.Ltmp534:
	.cfi_offset %r15, -16
	movq	%rcx, %r14
	movq	%rdx, %r15
	movq	%rsi, %rbx
	movq	%rdi, %r12
	movq	$24, %rdi
	callq	ggc_alloc
	movq	$0, (%rax)
	andq	$65535, %r12            # imm = 0xFFFF
	movq	$16, %rcx
	shlq	%cl, %rbx
	andq	$16711680, %rbx         # imm = 0xFF0000
	orq	%r12, %rbx
	movl	%ebx, (%rax)
	movq	%r15, 8(%rax)
	movq	%r14, 16(%rax)
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.Ltmp535:
	.size	gen_rtx_fmt_Ee, .Ltmp535-gen_rtx_fmt_Ee
	.cfi_endproc

	.globl	gen_rtx_fmt_uuEiiiiiibbii
	.align	16, 0x90
	.type	gen_rtx_fmt_uuEiiiiiibbii,@function
gen_rtx_fmt_uuEiiiiiibbii:              # @gen_rtx_fmt_uuEiiiiiibbii
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp542:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp543:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp544:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp545:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp546:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp547:
	.cfi_def_cfa_offset 64
.Ltmp548:
	.cfi_offset %rbx, -48
.Ltmp549:
	.cfi_offset %r12, -40
.Ltmp550:
	.cfi_offset %r13, -32
.Ltmp551:
	.cfi_offset %r14, -24
.Ltmp552:
	.cfi_offset %r15, -16
	movq	%r9, 8(%rsp)            # 8-byte Spill
	movq	%r8, %r15
	movq	%rcx, %r12
	movq	%rdx, %r13
	movq	%rsi, %rbx
	movq	%rdi, %r14
	movq	$112, %rdi
	callq	ggc_alloc
	movq	$16, %rcx
	shlq	%cl, %rbx
	andq	$65535, %r14            # imm = 0xFFFF
	andq	$16711680, %rbx         # imm = 0xFF0000
	orq	%r14, %rbx
	movq	$0, (%rax)
	movl	%ebx, (%rax)
	movq	%r13, 8(%rax)
	movq	%r12, 16(%rax)
	movq	%r15, 24(%rax)
	movq	8(%rsp), %rcx           # 8-byte Reload
	movl	%ecx, 32(%rax)
	movq	64(%rsp), %rcx
	movl	%ecx, 40(%rax)
	movq	72(%rsp), %rcx
	movl	%ecx, 48(%rax)
	movq	80(%rsp), %rcx
	movl	%ecx, 56(%rax)
	movq	88(%rsp), %rcx
	movl	%ecx, 64(%rax)
	movq	96(%rsp), %rcx
	movl	%ecx, 72(%rax)
	movq	104(%rsp), %rcx
	movq	%rcx, 80(%rax)
	movq	112(%rsp), %rcx
	movq	%rcx, 88(%rax)
	movq	120(%rsp), %rcx
	movl	%ecx, 96(%rax)
	movq	128(%rsp), %rcx
	movl	%ecx, 104(%rax)
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp553:
	.size	gen_rtx_fmt_uuEiiiiiibbii, .Ltmp553-gen_rtx_fmt_uuEiiiiiibbii
	.cfi_endproc

	.globl	gen_rtx_fmt_iiiiiiiitt
	.align	16, 0x90
	.type	gen_rtx_fmt_iiiiiiiitt,@function
gen_rtx_fmt_iiiiiiiitt:                 # @gen_rtx_fmt_iiiiiiiitt
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp560:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp561:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp562:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp563:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp564:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp565:
	.cfi_def_cfa_offset 64
.Ltmp566:
	.cfi_offset %rbx, -48
.Ltmp567:
	.cfi_offset %r12, -40
.Ltmp568:
	.cfi_offset %r13, -32
.Ltmp569:
	.cfi_offset %r14, -24
.Ltmp570:
	.cfi_offset %r15, -16
	movq	%r9, 8(%rsp)            # 8-byte Spill
	movq	%r8, %r15
	movq	%rcx, %r12
	movq	%rdx, %r13
	movq	%rsi, %rbx
	movq	%rdi, %r14
	movq	$88, %rdi
	callq	ggc_alloc
	movq	$0, (%rax)
	andq	$65535, %r14            # imm = 0xFFFF
	movq	$16, %rcx
	shlq	%cl, %rbx
	andq	$16711680, %rbx         # imm = 0xFF0000
	orq	%r14, %rbx
	movl	%ebx, (%rax)
	movl	%r13d, 8(%rax)
	movl	%r12d, 16(%rax)
	movl	%r15d, 24(%rax)
	movq	8(%rsp), %rcx           # 8-byte Reload
	movl	%ecx, 32(%rax)
	movq	64(%rsp), %rcx
	movl	%ecx, 40(%rax)
	movq	72(%rsp), %rcx
	movl	%ecx, 48(%rax)
	movq	80(%rsp), %rcx
	movl	%ecx, 56(%rax)
	movq	88(%rsp), %rcx
	movl	%ecx, 64(%rax)
	movq	96(%rsp), %rcx
	movq	%rcx, 72(%rax)
	movq	104(%rsp), %rcx
	movq	%rcx, 80(%rax)
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp571:
	.size	gen_rtx_fmt_iiiiiiiitt, .Ltmp571-gen_rtx_fmt_iiiiiiiitt
	.cfi_endproc

	.globl	gen_rtx_fmt_eti
	.align	16, 0x90
	.type	gen_rtx_fmt_eti,@function
gen_rtx_fmt_eti:                        # @gen_rtx_fmt_eti
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp578:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp579:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp580:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp581:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp582:
	.cfi_def_cfa_offset 48
.Ltmp583:
	.cfi_offset %rbx, -48
.Ltmp584:
	.cfi_offset %r12, -40
.Ltmp585:
	.cfi_offset %r13, -32
.Ltmp586:
	.cfi_offset %r14, -24
.Ltmp587:
	.cfi_offset %r15, -16
	movq	%r8, %r14
	movq	%rcx, %r15
	movq	%rdx, %r12
	movq	%rsi, %rbx
	movq	%rdi, %r13
	movq	$32, %rdi
	callq	ggc_alloc
	movq	$0, (%rax)
	andq	$65535, %r13            # imm = 0xFFFF
	movq	$16, %rcx
	shlq	%cl, %rbx
	andq	$16711680, %rbx         # imm = 0xFF0000
	orq	%r13, %rbx
	movl	%ebx, (%rax)
	movq	%r12, 8(%rax)
	movq	%r15, 16(%rax)
	movl	%r14d, 24(%rax)
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp588:
	.size	gen_rtx_fmt_eti, .Ltmp588-gen_rtx_fmt_eti
	.cfi_endproc

	.globl	gen_rtx_fmt_bi
	.align	16, 0x90
	.type	gen_rtx_fmt_bi,@function
gen_rtx_fmt_bi:                         # @gen_rtx_fmt_bi
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp594:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp595:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp596:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp597:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp598:
	.cfi_def_cfa_offset 48
.Ltmp599:
	.cfi_offset %rbx, -40
.Ltmp600:
	.cfi_offset %r12, -32
.Ltmp601:
	.cfi_offset %r14, -24
.Ltmp602:
	.cfi_offset %r15, -16
	movq	%rcx, %r14
	movq	%rdx, %r15
	movq	%rsi, %rbx
	movq	%rdi, %r12
	movq	$24, %rdi
	callq	ggc_alloc
	movq	$0, (%rax)
	andq	$65535, %r12            # imm = 0xFFFF
	movq	$16, %rcx
	shlq	%cl, %rbx
	andq	$16711680, %rbx         # imm = 0xFF0000
	orq	%r12, %rbx
	movl	%ebx, (%rax)
	movq	%r15, 8(%rax)
	movl	%r14d, 16(%rax)
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.Ltmp603:
	.size	gen_rtx_fmt_bi, .Ltmp603-gen_rtx_fmt_bi
	.cfi_endproc

	.globl	gen_rtx_fmt_uuuu
	.align	16, 0x90
	.type	gen_rtx_fmt_uuuu,@function
gen_rtx_fmt_uuuu:                       # @gen_rtx_fmt_uuuu
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp610:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp611:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp612:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp613:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp614:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp615:
	.cfi_def_cfa_offset 64
.Ltmp616:
	.cfi_offset %rbx, -48
.Ltmp617:
	.cfi_offset %r12, -40
.Ltmp618:
	.cfi_offset %r13, -32
.Ltmp619:
	.cfi_offset %r14, -24
.Ltmp620:
	.cfi_offset %r15, -16
	movq	%r9, 8(%rsp)            # 8-byte Spill
	movq	%r8, %r15
	movq	%rcx, %r12
	movq	%rdx, %r13
	movq	%rsi, %rbx
	movq	%rdi, %r14
	movq	$40, %rdi
	callq	ggc_alloc
	movq	$0, (%rax)
	andq	$65535, %r14            # imm = 0xFFFF
	movq	$16, %rcx
	shlq	%cl, %rbx
	andq	$16711680, %rbx         # imm = 0xFF0000
	orq	%r14, %rbx
	movl	%ebx, (%rax)
	movq	%r13, 8(%rax)
	movq	%r12, 16(%rax)
	movq	%r15, 24(%rax)
	movq	8(%rsp), %rcx           # 8-byte Reload
	movq	%rcx, 32(%rax)
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp621:
	.size	gen_rtx_fmt_uuuu, .Ltmp621-gen_rtx_fmt_uuuu
	.cfi_endproc


	.ident	"clang version 3.5 (trunk)"
	.section	".note.GNU-stack","",@progbits
