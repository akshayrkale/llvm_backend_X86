	.file	"insn-emit.c.bc"
	.text
	.globl	gen_cmpdi_ccno_1_rex64
	.align	16, 0x90
	.type	gen_cmpdi_ccno_1_rex64,@function
gen_cmpdi_ccno_1_rex64:                 # @gen_cmpdi_ccno_1_rex64
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
	movq	%rsi, %r14
	movq	%rdi, %rbx
	xorq	%rdi, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	%rax, %r15
	movq	$74, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%r14, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r15, %rdx
	movq	%rax, %rcx
	popq	%rbx
	popq	%r14
	popq	%r15
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp10:
	.size	gen_cmpdi_ccno_1_rex64, .Ltmp10-gen_cmpdi_ccno_1_rex64
	.cfi_endproc

	.globl	gen_cmpdi_1_insn_rex64
	.align	16, 0x90
	.type	gen_cmpdi_1_insn_rex64,@function
gen_cmpdi_1_insn_rex64:                 # @gen_cmpdi_1_insn_rex64
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp15:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp16:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp17:
	.cfi_def_cfa_offset 32
.Ltmp18:
	.cfi_offset %rbx, -32
.Ltmp19:
	.cfi_offset %r14, -24
.Ltmp20:
	.cfi_offset %r15, -16
	movq	%rsi, %r14
	movq	%rdi, %rbx
	xorq	%rdi, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	%rax, %r15
	movq	$74, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%r14, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r15, %rdx
	movq	%rax, %rcx
	popq	%rbx
	popq	%r14
	popq	%r15
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp21:
	.size	gen_cmpdi_1_insn_rex64, .Ltmp21-gen_cmpdi_1_insn_rex64
	.cfi_endproc

	.globl	gen_cmpqi_ext_3_insn
	.align	16, 0x90
	.type	gen_cmpqi_ext_3_insn,@function
gen_cmpqi_ext_3_insn:                   # @gen_cmpqi_ext_3_insn
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp27:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp28:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp29:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp30:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp31:
	.cfi_def_cfa_offset 48
.Ltmp32:
	.cfi_offset %rbx, -40
.Ltmp33:
	.cfi_offset %r12, -32
.Ltmp34:
	.cfi_offset %r14, -24
.Ltmp35:
	.cfi_offset %r15, -16
	movq	%rsi, %r14
	movq	%rdi, %r12
	xorq	%rdi, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	%rax, %r15
	xorq	%rdi, %rdi
	movq	$8, %rsi
	callq	gen_rtx_CONST_INT
	movq	%rax, %rbx
	xorq	%rdi, %rdi
	movq	$8, %rsi
	callq	gen_rtx_CONST_INT
	movq	$133, %rdi
	movq	$4, %rsi
	movq	%r12, %rdx
	movq	%rbx, %rcx
	movq	%rax, %r8
	callq	gen_rtx_fmt_eee
	movq	$2, %rdi
	xorq	%rdx, %rdx
	movq	%rax, %rsi
	callq	gen_rtx_SUBREG
	movq	$74, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	movq	%r14, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r15, %rdx
	movq	%rax, %rcx
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp36:
	.size	gen_cmpqi_ext_3_insn, .Ltmp36-gen_cmpqi_ext_3_insn
	.cfi_endproc

	.globl	gen_cmpqi_ext_3_insn_rex64
	.align	16, 0x90
	.type	gen_cmpqi_ext_3_insn_rex64,@function
gen_cmpqi_ext_3_insn_rex64:             # @gen_cmpqi_ext_3_insn_rex64
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
	movq	%rsi, %r14
	movq	%rdi, %r12
	xorq	%rdi, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	%rax, %r15
	xorq	%rdi, %rdi
	movq	$8, %rsi
	callq	gen_rtx_CONST_INT
	movq	%rax, %rbx
	xorq	%rdi, %rdi
	movq	$8, %rsi
	callq	gen_rtx_CONST_INT
	movq	$133, %rdi
	movq	$4, %rsi
	movq	%r12, %rdx
	movq	%rbx, %rcx
	movq	%rax, %r8
	callq	gen_rtx_fmt_eee
	movq	$2, %rdi
	xorq	%rdx, %rdx
	movq	%rax, %rsi
	callq	gen_rtx_SUBREG
	movq	$74, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	movq	%r14, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r15, %rdx
	movq	%rax, %rcx
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp51:
	.size	gen_cmpqi_ext_3_insn_rex64, .Ltmp51-gen_cmpqi_ext_3_insn_rex64
	.cfi_endproc

	.globl	gen_x86_fnstsw_1
	.align	16, 0x90
	.type	gen_x86_fnstsw_1,@function
gen_x86_fnstsw_1:                       # @gen_x86_fnstsw_1
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp54:
	.cfi_def_cfa_offset 16
.Ltmp55:
	.cfi_offset %rbx, -16
	movq	%rdi, %rbx
	xorq	%rdi, %rdi
	movq	$18, %rsi
	callq	gen_rtx_REG
	movq	%rax, %rcx
	movq	$1, %rdi
	xorq	%rax, %rax
	movq	%rcx, %rsi
	callq	gen_rtvec
	movq	$42, %rdi
	movq	$3, %rsi
	movq	$9, %rcx
	movq	%rax, %rdx
	callq	gen_rtx_fmt_Ei
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	popq	%rbx
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp56:
	.size	gen_x86_fnstsw_1, .Ltmp56-gen_x86_fnstsw_1
	.cfi_endproc

	.globl	gen_x86_sahf_1
	.align	16, 0x90
	.type	gen_x86_sahf_1,@function
gen_x86_sahf_1:                         # @gen_x86_sahf_1
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
	movq	$52, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	%rax, %r14
	movq	$1, %rdi
	xorq	%rax, %rax
	movq	%rbx, %rsi
	callq	gen_rtvec
	movq	$42, %rdi
	movq	$52, %rsi
	movq	$10, %rcx
	movq	%rax, %rdx
	callq	gen_rtx_fmt_Ei
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp65:
	.size	gen_x86_sahf_1, .Ltmp65-gen_x86_sahf_1
	.cfi_endproc

	.globl	gen_popsi1
	.align	16, 0x90
	.type	gen_popsi1,@function
gen_popsi1:                             # @gen_popsi1
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp70:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp71:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp72:
	.cfi_def_cfa_offset 32
.Ltmp73:
	.cfi_offset %rbx, -32
.Ltmp74:
	.cfi_offset %r14, -24
.Ltmp75:
	.cfi_offset %r15, -16
	movq	%rdi, %rbx
	movq	$4, %rdi
	movq	$7, %rsi
	callq	gen_rtx_REG
	movq	$4, %rdi
	movq	%rax, %rsi
	callq	gen_rtx_MEM
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %r14
	movq	$4, %rdi
	movq	$7, %rsi
	callq	gen_rtx_REG
	movq	%rax, %r15
	movq	$4, %rdi
	movq	$7, %rsi
	callq	gen_rtx_REG
	movq	%rax, %rbx
	xorq	%rdi, %rdi
	movq	$4, %rsi
	callq	gen_rtx_CONST_INT
	movq	$75, %rdi
	movq	$4, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r15, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rcx
	movq	$2, %rdi
	xorq	%rax, %rax
	movq	%r14, %rsi
	movq	%rcx, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	popq	%rbx
	popq	%r14
	popq	%r15
	jmp	gen_rtx_fmt_E  # TAILCALL
.Ltmp76:
	.size	gen_popsi1, .Ltmp76-gen_popsi1
	.cfi_endproc

	.globl	gen_movsi_insv_1
	.align	16, 0x90
	.type	gen_movsi_insv_1,@function
gen_movsi_insv_1:                       # @gen_movsi_insv_1
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp81:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp82:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp83:
	.cfi_def_cfa_offset 32
.Ltmp84:
	.cfi_offset %rbx, -32
.Ltmp85:
	.cfi_offset %r14, -24
.Ltmp86:
	.cfi_offset %r15, -16
	movq	%rsi, %r14
	movq	%rdi, %r15
	xorq	%rdi, %rdi
	movq	$8, %rsi
	callq	gen_rtx_CONST_INT
	movq	%rax, %rbx
	xorq	%rdi, %rdi
	movq	$8, %rsi
	callq	gen_rtx_CONST_INT
	movq	$133, %rdi
	movq	$4, %rsi
	movq	%r15, %rdx
	movq	%rbx, %rcx
	movq	%rax, %r8
	callq	gen_rtx_fmt_eee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	movq	%r14, %rcx
	popq	%rbx
	popq	%r14
	popq	%r15
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp87:
	.size	gen_movsi_insv_1, .Ltmp87-gen_movsi_insv_1
	.cfi_endproc

	.globl	gen_pushdi2_rex64
	.align	16, 0x90
	.type	gen_pushdi2_rex64,@function
gen_pushdi2_rex64:                      # @gen_pushdi2_rex64
	.cfi_startproc
# BB#0:                                 # %entry
	movq	%rsi, %rax
	movq	%rdi, %rcx
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rcx, %rdx
	movq	%rax, %rcx
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp88:
	.size	gen_pushdi2_rex64, .Ltmp88-gen_pushdi2_rex64
	.cfi_endproc

	.globl	gen_popdi1
	.align	16, 0x90
	.type	gen_popdi1,@function
gen_popdi1:                             # @gen_popdi1
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp93:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp94:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp95:
	.cfi_def_cfa_offset 32
.Ltmp96:
	.cfi_offset %rbx, -32
.Ltmp97:
	.cfi_offset %r14, -24
.Ltmp98:
	.cfi_offset %r15, -16
	movq	%rdi, %rbx
	movq	$5, %rdi
	movq	$7, %rsi
	callq	gen_rtx_REG
	movq	$5, %rdi
	movq	%rax, %rsi
	callq	gen_rtx_MEM
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %r14
	movq	$5, %rdi
	movq	$7, %rsi
	callq	gen_rtx_REG
	movq	%rax, %r15
	movq	$5, %rdi
	movq	$7, %rsi
	callq	gen_rtx_REG
	movq	%rax, %rbx
	xorq	%rdi, %rdi
	movq	$8, %rsi
	callq	gen_rtx_CONST_INT
	movq	$75, %rdi
	movq	$5, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r15, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rcx
	movq	$2, %rdi
	xorq	%rax, %rax
	movq	%r14, %rsi
	movq	%rcx, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	popq	%rbx
	popq	%r14
	popq	%r15
	jmp	gen_rtx_fmt_E  # TAILCALL
.Ltmp99:
	.size	gen_popdi1, .Ltmp99-gen_popdi1
	.cfi_endproc

	.globl	gen_swapxf
	.align	16, 0x90
	.type	gen_swapxf,@function
gen_swapxf:                             # @gen_swapxf
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp104:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp105:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp106:
	.cfi_def_cfa_offset 32
.Ltmp107:
	.cfi_offset %rbx, -32
.Ltmp108:
	.cfi_offset %r14, -24
.Ltmp109:
	.cfi_offset %r15, -16
	movq	%rsi, %rbx
	movq	%rdi, %r14
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	movq	%rbx, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %r15
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%r14, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rcx
	movq	$2, %rdi
	xorq	%rax, %rax
	movq	%r15, %rsi
	movq	%rcx, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	popq	%rbx
	popq	%r14
	popq	%r15
	jmp	gen_rtx_fmt_E  # TAILCALL
.Ltmp110:
	.size	gen_swapxf, .Ltmp110-gen_swapxf
	.cfi_endproc

	.globl	gen_swaptf
	.align	16, 0x90
	.type	gen_swaptf,@function
gen_swaptf:                             # @gen_swaptf
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp115:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp116:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp117:
	.cfi_def_cfa_offset 32
.Ltmp118:
	.cfi_offset %rbx, -32
.Ltmp119:
	.cfi_offset %r14, -24
.Ltmp120:
	.cfi_offset %r15, -16
	movq	%rsi, %rbx
	movq	%rdi, %r14
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	movq	%rbx, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %r15
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%r14, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rcx
	movq	$2, %rdi
	xorq	%rax, %rax
	movq	%r15, %rsi
	movq	%rcx, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	popq	%rbx
	popq	%r14
	popq	%r15
	jmp	gen_rtx_fmt_E  # TAILCALL
.Ltmp121:
	.size	gen_swaptf, .Ltmp121-gen_swaptf
	.cfi_endproc

	.globl	gen_zero_extendhisi2_and
	.align	16, 0x90
	.type	gen_zero_extendhisi2_and,@function
gen_zero_extendhisi2_and:               # @gen_zero_extendhisi2_and
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp124:
	.cfi_def_cfa_offset 16
.Ltmp125:
	.cfi_offset %rbx, -16
	movq	%rsi, %rax
	movq	%rdi, %rbx
	movq	$121, %rdi
	movq	$4, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rbx
	movq	$52, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rcx
	movq	$2, %rdi
	xorq	%rax, %rax
	movq	%rbx, %rsi
	movq	%rcx, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	popq	%rbx
	jmp	gen_rtx_fmt_E  # TAILCALL
.Ltmp126:
	.size	gen_zero_extendhisi2_and, .Ltmp126-gen_zero_extendhisi2_and
	.cfi_endproc

	.globl	gen_zero_extendsidi2_32
	.align	16, 0x90
	.type	gen_zero_extendsidi2_32,@function
gen_zero_extendsidi2_32:                # @gen_zero_extendsidi2_32
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp129:
	.cfi_def_cfa_offset 16
.Ltmp130:
	.cfi_offset %rbx, -16
	movq	%rsi, %rax
	movq	%rdi, %rbx
	movq	$121, %rdi
	movq	$5, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rbx
	movq	$52, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rcx
	movq	$2, %rdi
	xorq	%rax, %rax
	movq	%rbx, %rsi
	movq	%rcx, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	popq	%rbx
	jmp	gen_rtx_fmt_E  # TAILCALL
.Ltmp131:
	.size	gen_zero_extendsidi2_32, .Ltmp131-gen_zero_extendsidi2_32
	.cfi_endproc

	.globl	gen_zero_extendsidi2_rex64
	.align	16, 0x90
	.type	gen_zero_extendsidi2_rex64,@function
gen_zero_extendsidi2_rex64:             # @gen_zero_extendsidi2_rex64
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp134:
	.cfi_def_cfa_offset 16
.Ltmp135:
	.cfi_offset %rbx, -16
	movq	%rsi, %rax
	movq	%rdi, %rbx
	movq	$121, %rdi
	movq	$5, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	popq	%rbx
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp136:
	.size	gen_zero_extendsidi2_rex64, .Ltmp136-gen_zero_extendsidi2_rex64
	.cfi_endproc

	.globl	gen_zero_extendhidi2
	.align	16, 0x90
	.type	gen_zero_extendhidi2,@function
gen_zero_extendhidi2:                   # @gen_zero_extendhidi2
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp139:
	.cfi_def_cfa_offset 16
.Ltmp140:
	.cfi_offset %rbx, -16
	movq	%rsi, %rax
	movq	%rdi, %rbx
	movq	$121, %rdi
	movq	$5, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	popq	%rbx
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp141:
	.size	gen_zero_extendhidi2, .Ltmp141-gen_zero_extendhidi2
	.cfi_endproc

	.globl	gen_zero_extendqidi2
	.align	16, 0x90
	.type	gen_zero_extendqidi2,@function
gen_zero_extendqidi2:                   # @gen_zero_extendqidi2
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp144:
	.cfi_def_cfa_offset 16
.Ltmp145:
	.cfi_offset %rbx, -16
	movq	%rsi, %rax
	movq	%rdi, %rbx
	movq	$121, %rdi
	movq	$5, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	popq	%rbx
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp146:
	.size	gen_zero_extendqidi2, .Ltmp146-gen_zero_extendqidi2
	.cfi_endproc

	.globl	gen_extendsidi2_rex64
	.align	16, 0x90
	.type	gen_extendsidi2_rex64,@function
gen_extendsidi2_rex64:                  # @gen_extendsidi2_rex64
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp149:
	.cfi_def_cfa_offset 16
.Ltmp150:
	.cfi_offset %rbx, -16
	movq	%rsi, %rax
	movq	%rdi, %rbx
	movq	$120, %rdi
	movq	$5, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	popq	%rbx
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp151:
	.size	gen_extendsidi2_rex64, .Ltmp151-gen_extendsidi2_rex64
	.cfi_endproc

	.globl	gen_extendhidi2
	.align	16, 0x90
	.type	gen_extendhidi2,@function
gen_extendhidi2:                        # @gen_extendhidi2
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp154:
	.cfi_def_cfa_offset 16
.Ltmp155:
	.cfi_offset %rbx, -16
	movq	%rsi, %rax
	movq	%rdi, %rbx
	movq	$120, %rdi
	movq	$5, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	popq	%rbx
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp156:
	.size	gen_extendhidi2, .Ltmp156-gen_extendhidi2
	.cfi_endproc

	.globl	gen_extendqidi2
	.align	16, 0x90
	.type	gen_extendqidi2,@function
gen_extendqidi2:                        # @gen_extendqidi2
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp159:
	.cfi_def_cfa_offset 16
.Ltmp160:
	.cfi_offset %rbx, -16
	movq	%rsi, %rax
	movq	%rdi, %rbx
	movq	$120, %rdi
	movq	$5, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	popq	%rbx
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp161:
	.size	gen_extendqidi2, .Ltmp161-gen_extendqidi2
	.cfi_endproc

	.globl	gen_extendhisi2
	.align	16, 0x90
	.type	gen_extendhisi2,@function
gen_extendhisi2:                        # @gen_extendhisi2
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp164:
	.cfi_def_cfa_offset 16
.Ltmp165:
	.cfi_offset %rbx, -16
	movq	%rsi, %rax
	movq	%rdi, %rbx
	movq	$120, %rdi
	movq	$4, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	popq	%rbx
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp166:
	.size	gen_extendhisi2, .Ltmp166-gen_extendhisi2
	.cfi_endproc

	.globl	gen_extendqihi2
	.align	16, 0x90
	.type	gen_extendqihi2,@function
gen_extendqihi2:                        # @gen_extendqihi2
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp169:
	.cfi_def_cfa_offset 16
.Ltmp170:
	.cfi_offset %rbx, -16
	movq	%rsi, %rax
	movq	%rdi, %rbx
	movq	$120, %rdi
	movq	$3, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	popq	%rbx
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp171:
	.size	gen_extendqihi2, .Ltmp171-gen_extendqihi2
	.cfi_endproc

	.globl	gen_extendqisi2
	.align	16, 0x90
	.type	gen_extendqisi2,@function
gen_extendqisi2:                        # @gen_extendqisi2
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp174:
	.cfi_def_cfa_offset 16
.Ltmp175:
	.cfi_offset %rbx, -16
	movq	%rsi, %rax
	movq	%rdi, %rbx
	movq	$120, %rdi
	movq	$4, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	popq	%rbx
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp176:
	.size	gen_extendqisi2, .Ltmp176-gen_extendqisi2
	.cfi_endproc

	.globl	gen_truncdfsf2_3
	.align	16, 0x90
	.type	gen_truncdfsf2_3,@function
gen_truncdfsf2_3:                       # @gen_truncdfsf2_3
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp179:
	.cfi_def_cfa_offset 16
.Ltmp180:
	.cfi_offset %rbx, -16
	movq	%rsi, %rax
	movq	%rdi, %rbx
	movq	$124, %rdi
	movq	$15, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	popq	%rbx
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp181:
	.size	gen_truncdfsf2_3, .Ltmp181-gen_truncdfsf2_3
	.cfi_endproc

	.globl	gen_truncdfsf2_sse_only
	.align	16, 0x90
	.type	gen_truncdfsf2_sse_only,@function
gen_truncdfsf2_sse_only:                # @gen_truncdfsf2_sse_only
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp184:
	.cfi_def_cfa_offset 16
.Ltmp185:
	.cfi_offset %rbx, -16
	movq	%rsi, %rax
	movq	%rdi, %rbx
	movq	$124, %rdi
	movq	$15, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	popq	%rbx
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp186:
	.size	gen_truncdfsf2_sse_only, .Ltmp186-gen_truncdfsf2_sse_only
	.cfi_endproc

	.globl	gen_fix_truncdi_nomemory
	.align	16, 0x90
	.type	gen_fix_truncdi_nomemory,@function
gen_fix_truncdi_nomemory:               # @gen_fix_truncdi_nomemory
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp193:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp194:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp195:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp196:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp197:
	.cfi_def_cfa_offset 48
.Ltmp198:
	.cfi_offset %rbx, -48
.Ltmp199:
	.cfi_offset %r12, -40
.Ltmp200:
	.cfi_offset %r13, -32
.Ltmp201:
	.cfi_offset %r14, -24
.Ltmp202:
	.cfi_offset %r15, -16
	movq	%r8, %r15
	movq	%rcx, %r13
	movq	%rdx, %r12
	movq	%rsi, %rax
	movq	%rdi, %rbx
	movq	$126, %rdi
	movq	$5, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %r14
	movq	$48, %rdi
	xorq	%rsi, %rsi
	movq	%r12, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %r12
	movq	$48, %rdi
	xorq	%rsi, %rsi
	movq	%r13, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %r13
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%r15, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %r15
	movq	$62, %rdi
	movq	$16, %rsi
	callq	gen_rtx_fmt_0
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rbx
	movq	$5, %rdi
	xorq	%rax, %rax
	movq	%r14, %rsi
	movq	%r12, %rdx
	movq	%r13, %rcx
	movq	%r15, %r8
	movq	%rbx, %r9
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	gen_rtx_fmt_E  # TAILCALL
.Ltmp203:
	.size	gen_fix_truncdi_nomemory, .Ltmp203-gen_fix_truncdi_nomemory
	.cfi_endproc

	.globl	gen_fix_truncdi_memory
	.align	16, 0x90
	.type	gen_fix_truncdi_memory,@function
gen_fix_truncdi_memory:                 # @gen_fix_truncdi_memory
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp209:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp210:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp211:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp212:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp213:
	.cfi_def_cfa_offset 48
.Ltmp214:
	.cfi_offset %rbx, -40
.Ltmp215:
	.cfi_offset %r12, -32
.Ltmp216:
	.cfi_offset %r14, -24
.Ltmp217:
	.cfi_offset %r15, -16
	movq	%rcx, %r15
	movq	%rdx, %r12
	movq	%rsi, %rax
	movq	%rdi, %rbx
	movq	$126, %rdi
	movq	$5, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %r14
	movq	$48, %rdi
	xorq	%rsi, %rsi
	movq	%r12, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %r12
	movq	$48, %rdi
	xorq	%rsi, %rsi
	movq	%r15, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %r15
	movq	$62, %rdi
	movq	$16, %rsi
	callq	gen_rtx_fmt_0
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rbx
	movq	$4, %rdi
	xorq	%rax, %rax
	movq	%r14, %rsi
	movq	%r12, %rdx
	movq	%r15, %rcx
	movq	%rbx, %r8
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	jmp	gen_rtx_fmt_E  # TAILCALL
.Ltmp218:
	.size	gen_fix_truncdi_memory, .Ltmp218-gen_fix_truncdi_memory
	.cfi_endproc

	.globl	gen_fix_truncsfdi_sse
	.align	16, 0x90
	.type	gen_fix_truncsfdi_sse,@function
gen_fix_truncsfdi_sse:                  # @gen_fix_truncsfdi_sse
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp221:
	.cfi_def_cfa_offset 16
.Ltmp222:
	.cfi_offset %rbx, -16
	movq	%rsi, %rax
	movq	%rdi, %rbx
	movq	$126, %rdi
	movq	$5, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	popq	%rbx
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp223:
	.size	gen_fix_truncsfdi_sse, .Ltmp223-gen_fix_truncsfdi_sse
	.cfi_endproc

	.globl	gen_fix_truncdfdi_sse
	.align	16, 0x90
	.type	gen_fix_truncdfdi_sse,@function
gen_fix_truncdfdi_sse:                  # @gen_fix_truncdfdi_sse
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp226:
	.cfi_def_cfa_offset 16
.Ltmp227:
	.cfi_offset %rbx, -16
	movq	%rsi, %rax
	movq	%rdi, %rbx
	movq	$126, %rdi
	movq	$5, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	popq	%rbx
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp228:
	.size	gen_fix_truncdfdi_sse, .Ltmp228-gen_fix_truncdfdi_sse
	.cfi_endproc

	.globl	gen_fix_truncsi_nomemory
	.align	16, 0x90
	.type	gen_fix_truncsi_nomemory,@function
gen_fix_truncsi_nomemory:               # @gen_fix_truncsi_nomemory
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp235:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp236:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp237:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp238:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp239:
	.cfi_def_cfa_offset 48
.Ltmp240:
	.cfi_offset %rbx, -48
.Ltmp241:
	.cfi_offset %r12, -40
.Ltmp242:
	.cfi_offset %r13, -32
.Ltmp243:
	.cfi_offset %r14, -24
.Ltmp244:
	.cfi_offset %r15, -16
	movq	%r8, %r14
	movq	%rcx, %r15
	movq	%rdx, %r12
	movq	%rsi, %rax
	movq	%rdi, %rbx
	movq	$126, %rdi
	movq	$4, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %r13
	movq	$48, %rdi
	xorq	%rsi, %rsi
	movq	%r12, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %r12
	movq	$48, %rdi
	xorq	%rsi, %rsi
	movq	%r15, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %r15
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rbx
	movq	$4, %rdi
	xorq	%rax, %rax
	movq	%r13, %rsi
	movq	%r12, %rdx
	movq	%r15, %rcx
	movq	%rbx, %r8
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	gen_rtx_fmt_E  # TAILCALL
.Ltmp245:
	.size	gen_fix_truncsi_nomemory, .Ltmp245-gen_fix_truncsi_nomemory
	.cfi_endproc

	.globl	gen_fix_truncsi_memory
	.align	16, 0x90
	.type	gen_fix_truncsi_memory,@function
gen_fix_truncsi_memory:                 # @gen_fix_truncsi_memory
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp251:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp252:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp253:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp254:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp255:
	.cfi_def_cfa_offset 48
.Ltmp256:
	.cfi_offset %rbx, -40
.Ltmp257:
	.cfi_offset %r12, -32
.Ltmp258:
	.cfi_offset %r14, -24
.Ltmp259:
	.cfi_offset %r15, -16
	movq	%rcx, %r14
	movq	%rdx, %r15
	movq	%rsi, %rax
	movq	%rdi, %rbx
	movq	$126, %rdi
	movq	$4, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %r12
	movq	$48, %rdi
	xorq	%rsi, %rsi
	movq	%r15, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rbx
	movq	$48, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rcx
	movq	$3, %rdi
	xorq	%rax, %rax
	movq	%r12, %rsi
	movq	%rbx, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	jmp	gen_rtx_fmt_E  # TAILCALL
.Ltmp260:
	.size	gen_fix_truncsi_memory, .Ltmp260-gen_fix_truncsi_memory
	.cfi_endproc

	.globl	gen_fix_truncsfsi_sse
	.align	16, 0x90
	.type	gen_fix_truncsfsi_sse,@function
gen_fix_truncsfsi_sse:                  # @gen_fix_truncsfsi_sse
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp263:
	.cfi_def_cfa_offset 16
.Ltmp264:
	.cfi_offset %rbx, -16
	movq	%rsi, %rax
	movq	%rdi, %rbx
	movq	$126, %rdi
	movq	$4, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	popq	%rbx
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp265:
	.size	gen_fix_truncsfsi_sse, .Ltmp265-gen_fix_truncsfsi_sse
	.cfi_endproc

	.globl	gen_fix_truncdfsi_sse
	.align	16, 0x90
	.type	gen_fix_truncdfsi_sse,@function
gen_fix_truncdfsi_sse:                  # @gen_fix_truncdfsi_sse
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp268:
	.cfi_def_cfa_offset 16
.Ltmp269:
	.cfi_offset %rbx, -16
	movq	%rsi, %rax
	movq	%rdi, %rbx
	movq	$126, %rdi
	movq	$4, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	popq	%rbx
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp270:
	.size	gen_fix_truncdfsi_sse, .Ltmp270-gen_fix_truncdfsi_sse
	.cfi_endproc

	.globl	gen_fix_trunchi_nomemory
	.align	16, 0x90
	.type	gen_fix_trunchi_nomemory,@function
gen_fix_trunchi_nomemory:               # @gen_fix_trunchi_nomemory
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
.Ltmp282:
	.cfi_offset %rbx, -48
.Ltmp283:
	.cfi_offset %r12, -40
.Ltmp284:
	.cfi_offset %r13, -32
.Ltmp285:
	.cfi_offset %r14, -24
.Ltmp286:
	.cfi_offset %r15, -16
	movq	%r8, %r14
	movq	%rcx, %r15
	movq	%rdx, %r12
	movq	%rsi, %rax
	movq	%rdi, %rbx
	movq	$126, %rdi
	movq	$3, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %r13
	movq	$48, %rdi
	xorq	%rsi, %rsi
	movq	%r12, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %r12
	movq	$48, %rdi
	xorq	%rsi, %rsi
	movq	%r15, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %r15
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rbx
	movq	$4, %rdi
	xorq	%rax, %rax
	movq	%r13, %rsi
	movq	%r12, %rdx
	movq	%r15, %rcx
	movq	%rbx, %r8
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	gen_rtx_fmt_E  # TAILCALL
.Ltmp287:
	.size	gen_fix_trunchi_nomemory, .Ltmp287-gen_fix_trunchi_nomemory
	.cfi_endproc

	.globl	gen_fix_trunchi_memory
	.align	16, 0x90
	.type	gen_fix_trunchi_memory,@function
gen_fix_trunchi_memory:                 # @gen_fix_trunchi_memory
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp293:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp294:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp295:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp296:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp297:
	.cfi_def_cfa_offset 48
.Ltmp298:
	.cfi_offset %rbx, -40
.Ltmp299:
	.cfi_offset %r12, -32
.Ltmp300:
	.cfi_offset %r14, -24
.Ltmp301:
	.cfi_offset %r15, -16
	movq	%rcx, %r14
	movq	%rdx, %r15
	movq	%rsi, %rax
	movq	%rdi, %rbx
	movq	$126, %rdi
	movq	$3, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %r12
	movq	$48, %rdi
	xorq	%rsi, %rsi
	movq	%r15, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rbx
	movq	$48, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rcx
	movq	$3, %rdi
	xorq	%rax, %rax
	movq	%r12, %rsi
	movq	%rbx, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	jmp	gen_rtx_fmt_E  # TAILCALL
.Ltmp302:
	.size	gen_fix_trunchi_memory, .Ltmp302-gen_fix_trunchi_memory
	.cfi_endproc

	.globl	gen_x86_fnstcw_1
	.align	16, 0x90
	.type	gen_x86_fnstcw_1,@function
gen_x86_fnstcw_1:                       # @gen_x86_fnstcw_1
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp305:
	.cfi_def_cfa_offset 16
.Ltmp306:
	.cfi_offset %rbx, -16
	movq	%rdi, %rbx
	movq	$3, %rdi
	movq	$18, %rsi
	callq	gen_rtx_REG
	movq	%rax, %rcx
	movq	$1, %rdi
	xorq	%rax, %rax
	movq	%rcx, %rsi
	callq	gen_rtvec
	movq	$42, %rdi
	movq	$3, %rsi
	movq	$11, %rcx
	movq	%rax, %rdx
	callq	gen_rtx_fmt_Ei
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	popq	%rbx
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp307:
	.size	gen_x86_fnstcw_1, .Ltmp307-gen_x86_fnstcw_1
	.cfi_endproc

	.globl	gen_x86_fldcw_1
	.align	16, 0x90
	.type	gen_x86_fldcw_1,@function
gen_x86_fldcw_1:                        # @gen_x86_fldcw_1
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp311:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp312:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp313:
	.cfi_def_cfa_offset 32
.Ltmp314:
	.cfi_offset %rbx, -24
.Ltmp315:
	.cfi_offset %r14, -16
	movq	%rdi, %rbx
	movq	$3, %rdi
	movq	$18, %rsi
	callq	gen_rtx_REG
	movq	%rax, %r14
	movq	$1, %rdi
	xorq	%rax, %rax
	movq	%rbx, %rsi
	callq	gen_rtvec
	movq	$42, %rdi
	movq	$3, %rsi
	movq	$12, %rcx
	movq	%rax, %rdx
	callq	gen_rtx_fmt_Ei
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp316:
	.size	gen_x86_fldcw_1, .Ltmp316-gen_x86_fldcw_1
	.cfi_endproc

	.globl	gen_floathisf2
	.align	16, 0x90
	.type	gen_floathisf2,@function
gen_floathisf2:                         # @gen_floathisf2
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp319:
	.cfi_def_cfa_offset 16
.Ltmp320:
	.cfi_offset %rbx, -16
	movq	%rsi, %rax
	movq	%rdi, %rbx
	movq	$125, %rdi
	movq	$15, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	popq	%rbx
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp321:
	.size	gen_floathisf2, .Ltmp321-gen_floathisf2
	.cfi_endproc

	.globl	gen_floathidf2
	.align	16, 0x90
	.type	gen_floathidf2,@function
gen_floathidf2:                         # @gen_floathidf2
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp324:
	.cfi_def_cfa_offset 16
.Ltmp325:
	.cfi_offset %rbx, -16
	movq	%rsi, %rax
	movq	%rdi, %rbx
	movq	$125, %rdi
	movq	$16, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	popq	%rbx
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp326:
	.size	gen_floathidf2, .Ltmp326-gen_floathidf2
	.cfi_endproc

	.globl	gen_floathixf2
	.align	16, 0x90
	.type	gen_floathixf2,@function
gen_floathixf2:                         # @gen_floathixf2
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp329:
	.cfi_def_cfa_offset 16
.Ltmp330:
	.cfi_offset %rbx, -16
	movq	%rsi, %rax
	movq	%rdi, %rbx
	movq	$125, %rdi
	movq	$17, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	popq	%rbx
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp331:
	.size	gen_floathixf2, .Ltmp331-gen_floathixf2
	.cfi_endproc

	.globl	gen_floathitf2
	.align	16, 0x90
	.type	gen_floathitf2,@function
gen_floathitf2:                         # @gen_floathitf2
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp334:
	.cfi_def_cfa_offset 16
.Ltmp335:
	.cfi_offset %rbx, -16
	movq	%rsi, %rax
	movq	%rdi, %rbx
	movq	$125, %rdi
	movq	$18, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	popq	%rbx
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp336:
	.size	gen_floathitf2, .Ltmp336-gen_floathitf2
	.cfi_endproc

	.globl	gen_floatsixf2
	.align	16, 0x90
	.type	gen_floatsixf2,@function
gen_floatsixf2:                         # @gen_floatsixf2
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp339:
	.cfi_def_cfa_offset 16
.Ltmp340:
	.cfi_offset %rbx, -16
	movq	%rsi, %rax
	movq	%rdi, %rbx
	movq	$125, %rdi
	movq	$17, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	popq	%rbx
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp341:
	.size	gen_floatsixf2, .Ltmp341-gen_floatsixf2
	.cfi_endproc

	.globl	gen_floatsitf2
	.align	16, 0x90
	.type	gen_floatsitf2,@function
gen_floatsitf2:                         # @gen_floatsitf2
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp344:
	.cfi_def_cfa_offset 16
.Ltmp345:
	.cfi_offset %rbx, -16
	movq	%rsi, %rax
	movq	%rdi, %rbx
	movq	$125, %rdi
	movq	$18, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	popq	%rbx
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp346:
	.size	gen_floatsitf2, .Ltmp346-gen_floatsitf2
	.cfi_endproc

	.globl	gen_floatdixf2
	.align	16, 0x90
	.type	gen_floatdixf2,@function
gen_floatdixf2:                         # @gen_floatdixf2
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp349:
	.cfi_def_cfa_offset 16
.Ltmp350:
	.cfi_offset %rbx, -16
	movq	%rsi, %rax
	movq	%rdi, %rbx
	movq	$125, %rdi
	movq	$17, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	popq	%rbx
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp351:
	.size	gen_floatdixf2, .Ltmp351-gen_floatdixf2
	.cfi_endproc

	.globl	gen_floatditf2
	.align	16, 0x90
	.type	gen_floatditf2,@function
gen_floatditf2:                         # @gen_floatditf2
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp354:
	.cfi_def_cfa_offset 16
.Ltmp355:
	.cfi_offset %rbx, -16
	movq	%rsi, %rax
	movq	%rdi, %rbx
	movq	$125, %rdi
	movq	$18, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	popq	%rbx
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp356:
	.size	gen_floatditf2, .Ltmp356-gen_floatditf2
	.cfi_endproc

	.globl	gen_addqi3_cc
	.align	16, 0x90
	.type	gen_addqi3_cc,@function
gen_addqi3_cc:                          # @gen_addqi3_cc
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp362:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp363:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp364:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp365:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp366:
	.cfi_def_cfa_offset 48
.Ltmp367:
	.cfi_offset %rbx, -40
.Ltmp368:
	.cfi_offset %r12, -32
.Ltmp369:
	.cfi_offset %r14, -24
.Ltmp370:
	.cfi_offset %r15, -16
	movq	%rdx, %r15
	movq	%rsi, %rbx
	movq	%rdi, %r14
	movq	$52, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	%rax, %r12
	movq	$2, %rdi
	xorq	%rax, %rax
	movq	%rbx, %rsi
	movq	%r15, %rdx
	callq	gen_rtvec
	movq	$42, %rdi
	movq	$52, %rsi
	movq	$12, %rcx
	movq	%rax, %rdx
	callq	gen_rtx_fmt_Ei
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r12, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %r12
	movq	$75, %rdi
	movq	$2, %rsi
	movq	%rbx, %rdx
	movq	%r15, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rcx
	movq	$2, %rdi
	xorq	%rax, %rax
	movq	%r12, %rsi
	movq	%rcx, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	jmp	gen_rtx_fmt_E  # TAILCALL
.Ltmp371:
	.size	gen_addqi3_cc, .Ltmp371-gen_addqi3_cc
	.cfi_endproc

	.globl	gen_addsi_1_zext
	.align	16, 0x90
	.type	gen_addsi_1_zext,@function
gen_addsi_1_zext:                       # @gen_addsi_1_zext
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp374:
	.cfi_def_cfa_offset 16
.Ltmp375:
	.cfi_offset %rbx, -16
	movq	%rdx, %rax
	movq	%rsi, %rcx
	movq	%rdi, %rbx
	movq	$75, %rdi
	movq	$4, %rsi
	movq	%rcx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$121, %rdi
	movq	$5, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rbx
	movq	$52, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rcx
	movq	$2, %rdi
	xorq	%rax, %rax
	movq	%rbx, %rsi
	movq	%rcx, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	popq	%rbx
	jmp	gen_rtx_fmt_E  # TAILCALL
.Ltmp376:
	.size	gen_addsi_1_zext, .Ltmp376-gen_addsi_1_zext
	.cfi_endproc

	.globl	gen_addqi_ext_1
	.align	16, 0x90
	.type	gen_addqi_ext_1,@function
gen_addqi_ext_1:                        # @gen_addqi_ext_1
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp382:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp383:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp384:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp385:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp386:
	.cfi_def_cfa_offset 48
.Ltmp387:
	.cfi_offset %rbx, -40
.Ltmp388:
	.cfi_offset %r12, -32
.Ltmp389:
	.cfi_offset %r14, -24
.Ltmp390:
	.cfi_offset %r15, -16
	movq	%rdx, %r14
	movq	%rsi, %r15
	movq	%rdi, %r12
	xorq	%rdi, %rdi
	movq	$8, %rsi
	callq	gen_rtx_CONST_INT
	movq	%rax, %rbx
	xorq	%rdi, %rdi
	movq	$8, %rsi
	callq	gen_rtx_CONST_INT
	movq	$133, %rdi
	movq	$4, %rsi
	movq	%r12, %rdx
	movq	%rbx, %rcx
	movq	%rax, %r8
	callq	gen_rtx_fmt_eee
	movq	%rax, %r12
	xorq	%rdi, %rdi
	movq	$8, %rsi
	callq	gen_rtx_CONST_INT
	movq	%rax, %rbx
	xorq	%rdi, %rdi
	movq	$8, %rsi
	callq	gen_rtx_CONST_INT
	movq	$133, %rdi
	movq	$4, %rsi
	movq	%r15, %rdx
	movq	%rbx, %rcx
	movq	%rax, %r8
	callq	gen_rtx_fmt_eee
	movq	$75, %rdi
	movq	$4, %rsi
	movq	%rax, %rdx
	movq	%r14, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r12, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rbx
	movq	$52, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rcx
	movq	$2, %rdi
	xorq	%rax, %rax
	movq	%rbx, %rsi
	movq	%rcx, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	jmp	gen_rtx_fmt_E  # TAILCALL
.Ltmp391:
	.size	gen_addqi_ext_1, .Ltmp391-gen_addqi_ext_1
	.cfi_endproc

	.globl	gen_subdi3_carry_rex64
	.align	16, 0x90
	.type	gen_subdi3_carry_rex64,@function
gen_subdi3_carry_rex64:                 # @gen_subdi3_carry_rex64
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp396:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp397:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp398:
	.cfi_def_cfa_offset 32
.Ltmp399:
	.cfi_offset %rbx, -32
.Ltmp400:
	.cfi_offset %r14, -24
.Ltmp401:
	.cfi_offset %r15, -16
	movq	%rdx, %rbx
	movq	%rsi, %r15
	movq	%rdi, %r14
	movq	$52, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	const_int_rtx+512(%rip), %rcx
	movq	$111, %rdi
	movq	$5, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_ee
	movq	$75, %rdi
	movq	$5, %rsi
	movq	%rax, %rdx
	movq	%rbx, %rcx
	callq	gen_rtx_fmt_ee
	movq	$76, %rdi
	movq	$5, %rsi
	movq	%r15, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rbx
	movq	$52, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rcx
	movq	$2, %rdi
	xorq	%rax, %rax
	movq	%rbx, %rsi
	movq	%rcx, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	popq	%rbx
	popq	%r14
	popq	%r15
	jmp	gen_rtx_fmt_E  # TAILCALL
.Ltmp402:
	.size	gen_subdi3_carry_rex64, .Ltmp402-gen_subdi3_carry_rex64
	.cfi_endproc

	.globl	gen_subsi3_carry
	.align	16, 0x90
	.type	gen_subsi3_carry,@function
gen_subsi3_carry:                       # @gen_subsi3_carry
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp407:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp408:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp409:
	.cfi_def_cfa_offset 32
.Ltmp410:
	.cfi_offset %rbx, -32
.Ltmp411:
	.cfi_offset %r14, -24
.Ltmp412:
	.cfi_offset %r15, -16
	movq	%rdx, %rbx
	movq	%rsi, %r15
	movq	%rdi, %r14
	movq	$52, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	const_int_rtx+512(%rip), %rcx
	movq	$111, %rdi
	movq	$4, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_ee
	movq	$75, %rdi
	movq	$4, %rsi
	movq	%rax, %rdx
	movq	%rbx, %rcx
	callq	gen_rtx_fmt_ee
	movq	$76, %rdi
	movq	$4, %rsi
	movq	%r15, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rbx
	movq	$52, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rcx
	movq	$2, %rdi
	xorq	%rax, %rax
	movq	%rbx, %rsi
	movq	%rcx, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	popq	%rbx
	popq	%r14
	popq	%r15
	jmp	gen_rtx_fmt_E  # TAILCALL
.Ltmp413:
	.size	gen_subsi3_carry, .Ltmp413-gen_subsi3_carry
	.cfi_endproc

	.globl	gen_subsi3_carry_zext
	.align	16, 0x90
	.type	gen_subsi3_carry_zext,@function
gen_subsi3_carry_zext:                  # @gen_subsi3_carry_zext
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp418:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp419:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp420:
	.cfi_def_cfa_offset 32
.Ltmp421:
	.cfi_offset %rbx, -32
.Ltmp422:
	.cfi_offset %r14, -24
.Ltmp423:
	.cfi_offset %r15, -16
	movq	%rdx, %rbx
	movq	%rsi, %r15
	movq	%rdi, %r14
	movq	$52, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	const_int_rtx+512(%rip), %rcx
	movq	$111, %rdi
	movq	$4, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_ee
	movq	$75, %rdi
	movq	$4, %rsi
	movq	%rax, %rdx
	movq	%rbx, %rcx
	callq	gen_rtx_fmt_ee
	movq	$76, %rdi
	movq	$4, %rsi
	movq	%r15, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$121, %rdi
	movq	$5, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rbx
	movq	$52, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rcx
	movq	$2, %rdi
	xorq	%rax, %rax
	movq	%rbx, %rsi
	movq	%rcx, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	popq	%rbx
	popq	%r14
	popq	%r15
	jmp	gen_rtx_fmt_E  # TAILCALL
.Ltmp424:
	.size	gen_subsi3_carry_zext, .Ltmp424-gen_subsi3_carry_zext
	.cfi_endproc

	.globl	gen_divqi3
	.align	16, 0x90
	.type	gen_divqi3,@function
gen_divqi3:                             # @gen_divqi3
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp427:
	.cfi_def_cfa_offset 16
.Ltmp428:
	.cfi_offset %rbx, -16
	movq	%rdx, %rax
	movq	%rsi, %rcx
	movq	%rdi, %rbx
	movq	$79, %rdi
	movq	$2, %rsi
	movq	%rcx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rbx
	movq	$52, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rcx
	movq	$2, %rdi
	xorq	%rax, %rax
	movq	%rbx, %rsi
	movq	%rcx, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	popq	%rbx
	jmp	gen_rtx_fmt_E  # TAILCALL
.Ltmp429:
	.size	gen_divqi3, .Ltmp429-gen_divqi3
	.cfi_endproc

	.globl	gen_udivqi3
	.align	16, 0x90
	.type	gen_udivqi3,@function
gen_udivqi3:                            # @gen_udivqi3
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp432:
	.cfi_def_cfa_offset 16
.Ltmp433:
	.cfi_offset %rbx, -16
	movq	%rdx, %rax
	movq	%rsi, %rcx
	movq	%rdi, %rbx
	movq	$81, %rdi
	movq	$2, %rsi
	movq	%rcx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rbx
	movq	$52, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rcx
	movq	$2, %rdi
	xorq	%rax, %rax
	movq	%rbx, %rsi
	movq	%rcx, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	popq	%rbx
	jmp	gen_rtx_fmt_E  # TAILCALL
.Ltmp434:
	.size	gen_udivqi3, .Ltmp434-gen_udivqi3
	.cfi_endproc

	.globl	gen_divmodhi4
	.align	16, 0x90
	.type	gen_divmodhi4,@function
gen_divmodhi4:                          # @gen_divmodhi4
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp440:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp441:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp442:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp443:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp444:
	.cfi_def_cfa_offset 48
.Ltmp445:
	.cfi_offset %rbx, -40
.Ltmp446:
	.cfi_offset %r12, -32
.Ltmp447:
	.cfi_offset %r14, -24
.Ltmp448:
	.cfi_offset %r15, -16
	movq	%rcx, %r15
	movq	%rdx, %r12
	movq	%rsi, %rbx
	movq	%rdi, %r14
	movq	$79, %rdi
	movq	$3, %rsi
	movq	%rbx, %rdx
	movq	%r12, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %r14
	movq	$80, %rdi
	movq	$3, %rsi
	movq	%rbx, %rdx
	movq	%r12, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r15, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rbx
	movq	$52, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rcx
	movq	$3, %rdi
	xorq	%rax, %rax
	movq	%r14, %rsi
	movq	%rbx, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	jmp	gen_rtx_fmt_E  # TAILCALL
.Ltmp449:
	.size	gen_divmodhi4, .Ltmp449-gen_divmodhi4
	.cfi_endproc

	.globl	gen_udivmoddi4
	.align	16, 0x90
	.type	gen_udivmoddi4,@function
gen_udivmoddi4:                         # @gen_udivmoddi4
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp455:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp456:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp457:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp458:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp459:
	.cfi_def_cfa_offset 48
.Ltmp460:
	.cfi_offset %rbx, -40
.Ltmp461:
	.cfi_offset %r12, -32
.Ltmp462:
	.cfi_offset %r14, -24
.Ltmp463:
	.cfi_offset %r15, -16
	movq	%rcx, %r15
	movq	%rdx, %r12
	movq	%rsi, %rbx
	movq	%rdi, %r14
	movq	$81, %rdi
	movq	$5, %rsi
	movq	%rbx, %rdx
	movq	%r12, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %r14
	movq	$82, %rdi
	movq	$5, %rsi
	movq	%rbx, %rdx
	movq	%r12, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r15, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rbx
	movq	$52, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rcx
	movq	$3, %rdi
	xorq	%rax, %rax
	movq	%r14, %rsi
	movq	%rbx, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	jmp	gen_rtx_fmt_E  # TAILCALL
.Ltmp464:
	.size	gen_udivmoddi4, .Ltmp464-gen_udivmoddi4
	.cfi_endproc

	.globl	gen_udivmodsi4
	.align	16, 0x90
	.type	gen_udivmodsi4,@function
gen_udivmodsi4:                         # @gen_udivmodsi4
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp470:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp471:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp472:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp473:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp474:
	.cfi_def_cfa_offset 48
.Ltmp475:
	.cfi_offset %rbx, -40
.Ltmp476:
	.cfi_offset %r12, -32
.Ltmp477:
	.cfi_offset %r14, -24
.Ltmp478:
	.cfi_offset %r15, -16
	movq	%rcx, %r15
	movq	%rdx, %r12
	movq	%rsi, %rbx
	movq	%rdi, %r14
	movq	$81, %rdi
	movq	$4, %rsi
	movq	%rbx, %rdx
	movq	%r12, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %r14
	movq	$82, %rdi
	movq	$4, %rsi
	movq	%rbx, %rdx
	movq	%r12, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r15, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rbx
	movq	$52, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rcx
	movq	$3, %rdi
	xorq	%rax, %rax
	movq	%r14, %rsi
	movq	%rbx, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	jmp	gen_rtx_fmt_E  # TAILCALL
.Ltmp479:
	.size	gen_udivmodsi4, .Ltmp479-gen_udivmodsi4
	.cfi_endproc

	.globl	gen_testsi_1
	.align	16, 0x90
	.type	gen_testsi_1,@function
gen_testsi_1:                           # @gen_testsi_1
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp484:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp485:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp486:
	.cfi_def_cfa_offset 32
.Ltmp487:
	.cfi_offset %rbx, -32
.Ltmp488:
	.cfi_offset %r14, -24
.Ltmp489:
	.cfi_offset %r15, -16
	movq	%rsi, %r14
	movq	%rdi, %rbx
	xorq	%rdi, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	%rax, %r15
	movq	$83, %rdi
	movq	$4, %rsi
	movq	%rbx, %rdx
	movq	%r14, %rcx
	callq	gen_rtx_fmt_ee
	movq	const_int_rtx+512(%rip), %rcx
	movq	$74, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r15, %rdx
	movq	%rax, %rcx
	popq	%rbx
	popq	%r14
	popq	%r15
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp490:
	.size	gen_testsi_1, .Ltmp490-gen_testsi_1
	.cfi_endproc

	.globl	gen_andqi_ext_0
	.align	16, 0x90
	.type	gen_andqi_ext_0,@function
gen_andqi_ext_0:                        # @gen_andqi_ext_0
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp496:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp497:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp498:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp499:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp500:
	.cfi_def_cfa_offset 48
.Ltmp501:
	.cfi_offset %rbx, -40
.Ltmp502:
	.cfi_offset %r12, -32
.Ltmp503:
	.cfi_offset %r14, -24
.Ltmp504:
	.cfi_offset %r15, -16
	movq	%rdx, %r14
	movq	%rsi, %r15
	movq	%rdi, %r12
	xorq	%rdi, %rdi
	movq	$8, %rsi
	callq	gen_rtx_CONST_INT
	movq	%rax, %rbx
	xorq	%rdi, %rdi
	movq	$8, %rsi
	callq	gen_rtx_CONST_INT
	movq	$133, %rdi
	movq	$4, %rsi
	movq	%r12, %rdx
	movq	%rbx, %rcx
	movq	%rax, %r8
	callq	gen_rtx_fmt_eee
	movq	%rax, %r12
	xorq	%rdi, %rdi
	movq	$8, %rsi
	callq	gen_rtx_CONST_INT
	movq	%rax, %rbx
	xorq	%rdi, %rdi
	movq	$8, %rsi
	callq	gen_rtx_CONST_INT
	movq	$133, %rdi
	movq	$4, %rsi
	movq	%r15, %rdx
	movq	%rbx, %rcx
	movq	%rax, %r8
	callq	gen_rtx_fmt_eee
	movq	$83, %rdi
	movq	$4, %rsi
	movq	%rax, %rdx
	movq	%r14, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r12, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rbx
	movq	$52, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rcx
	movq	$2, %rdi
	xorq	%rax, %rax
	movq	%rbx, %rsi
	movq	%rcx, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	jmp	gen_rtx_fmt_E  # TAILCALL
.Ltmp505:
	.size	gen_andqi_ext_0, .Ltmp505-gen_andqi_ext_0
	.cfi_endproc

	.globl	gen_negsf2_memory
	.align	16, 0x90
	.type	gen_negsf2_memory,@function
gen_negsf2_memory:                      # @gen_negsf2_memory
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp508:
	.cfi_def_cfa_offset 16
.Ltmp509:
	.cfi_offset %rbx, -16
	movq	%rsi, %rax
	movq	%rdi, %rbx
	movq	$77, %rdi
	movq	$15, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rbx
	movq	$52, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rcx
	movq	$2, %rdi
	xorq	%rax, %rax
	movq	%rbx, %rsi
	movq	%rcx, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	popq	%rbx
	jmp	gen_rtx_fmt_E  # TAILCALL
.Ltmp510:
	.size	gen_negsf2_memory, .Ltmp510-gen_negsf2_memory
	.cfi_endproc

	.globl	gen_negsf2_ifs
	.align	16, 0x90
	.type	gen_negsf2_ifs,@function
gen_negsf2_ifs:                         # @gen_negsf2_ifs
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp515:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp516:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp517:
	.cfi_def_cfa_offset 32
.Ltmp518:
	.cfi_offset %rbx, -32
.Ltmp519:
	.cfi_offset %r14, -24
.Ltmp520:
	.cfi_offset %r15, -16
	movq	%rdx, %r14
	movq	%rsi, %rax
	movq	%rdi, %rbx
	movq	$77, %rdi
	movq	$15, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %r15
	movq	$48, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rbx
	movq	$52, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rcx
	movq	$3, %rdi
	xorq	%rax, %rax
	movq	%r15, %rsi
	movq	%rbx, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	popq	%rbx
	popq	%r14
	popq	%r15
	jmp	gen_rtx_fmt_E  # TAILCALL
.Ltmp521:
	.size	gen_negsf2_ifs, .Ltmp521-gen_negsf2_ifs
	.cfi_endproc

	.globl	gen_negdf2_memory
	.align	16, 0x90
	.type	gen_negdf2_memory,@function
gen_negdf2_memory:                      # @gen_negdf2_memory
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp524:
	.cfi_def_cfa_offset 16
.Ltmp525:
	.cfi_offset %rbx, -16
	movq	%rsi, %rax
	movq	%rdi, %rbx
	movq	$77, %rdi
	movq	$16, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rbx
	movq	$52, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rcx
	movq	$2, %rdi
	xorq	%rax, %rax
	movq	%rbx, %rsi
	movq	%rcx, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	popq	%rbx
	jmp	gen_rtx_fmt_E  # TAILCALL
.Ltmp526:
	.size	gen_negdf2_memory, .Ltmp526-gen_negdf2_memory
	.cfi_endproc

	.globl	gen_negdf2_ifs
	.align	16, 0x90
	.type	gen_negdf2_ifs,@function
gen_negdf2_ifs:                         # @gen_negdf2_ifs
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp531:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp532:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp533:
	.cfi_def_cfa_offset 32
.Ltmp534:
	.cfi_offset %rbx, -32
.Ltmp535:
	.cfi_offset %r14, -24
.Ltmp536:
	.cfi_offset %r15, -16
	movq	%rdx, %r14
	movq	%rsi, %rax
	movq	%rdi, %rbx
	movq	$77, %rdi
	movq	$16, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %r15
	movq	$48, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rbx
	movq	$52, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rcx
	movq	$3, %rdi
	xorq	%rax, %rax
	movq	%r15, %rsi
	movq	%rbx, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	popq	%rbx
	popq	%r14
	popq	%r15
	jmp	gen_rtx_fmt_E  # TAILCALL
.Ltmp537:
	.size	gen_negdf2_ifs, .Ltmp537-gen_negdf2_ifs
	.cfi_endproc

	.globl	gen_abssf2_memory
	.align	16, 0x90
	.type	gen_abssf2_memory,@function
gen_abssf2_memory:                      # @gen_abssf2_memory
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp540:
	.cfi_def_cfa_offset 16
.Ltmp541:
	.cfi_offset %rbx, -16
	movq	%rsi, %rax
	movq	%rdi, %rbx
	movq	$129, %rdi
	movq	$15, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rbx
	movq	$52, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rcx
	movq	$2, %rdi
	xorq	%rax, %rax
	movq	%rbx, %rsi
	movq	%rcx, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	popq	%rbx
	jmp	gen_rtx_fmt_E  # TAILCALL
.Ltmp542:
	.size	gen_abssf2_memory, .Ltmp542-gen_abssf2_memory
	.cfi_endproc

	.globl	gen_abssf2_ifs
	.align	16, 0x90
	.type	gen_abssf2_ifs,@function
gen_abssf2_ifs:                         # @gen_abssf2_ifs
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp547:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp548:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp549:
	.cfi_def_cfa_offset 32
.Ltmp550:
	.cfi_offset %rbx, -32
.Ltmp551:
	.cfi_offset %r14, -24
.Ltmp552:
	.cfi_offset %r15, -16
	movq	%rdx, %r14
	movq	%rsi, %rax
	movq	%rdi, %rbx
	movq	$129, %rdi
	movq	$15, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %r15
	movq	$48, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rbx
	movq	$52, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rcx
	movq	$3, %rdi
	xorq	%rax, %rax
	movq	%r15, %rsi
	movq	%rbx, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	popq	%rbx
	popq	%r14
	popq	%r15
	jmp	gen_rtx_fmt_E  # TAILCALL
.Ltmp553:
	.size	gen_abssf2_ifs, .Ltmp553-gen_abssf2_ifs
	.cfi_endproc

	.globl	gen_absdf2_memory
	.align	16, 0x90
	.type	gen_absdf2_memory,@function
gen_absdf2_memory:                      # @gen_absdf2_memory
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp556:
	.cfi_def_cfa_offset 16
.Ltmp557:
	.cfi_offset %rbx, -16
	movq	%rsi, %rax
	movq	%rdi, %rbx
	movq	$129, %rdi
	movq	$16, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rbx
	movq	$52, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rcx
	movq	$2, %rdi
	xorq	%rax, %rax
	movq	%rbx, %rsi
	movq	%rcx, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	popq	%rbx
	jmp	gen_rtx_fmt_E  # TAILCALL
.Ltmp558:
	.size	gen_absdf2_memory, .Ltmp558-gen_absdf2_memory
	.cfi_endproc

	.globl	gen_absdf2_ifs
	.align	16, 0x90
	.type	gen_absdf2_ifs,@function
gen_absdf2_ifs:                         # @gen_absdf2_ifs
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp563:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp564:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp565:
	.cfi_def_cfa_offset 32
.Ltmp566:
	.cfi_offset %rbx, -32
.Ltmp567:
	.cfi_offset %r14, -24
.Ltmp568:
	.cfi_offset %r15, -16
	movq	%rdx, %r14
	movq	%rsi, %rax
	movq	%rdi, %rbx
	movq	$129, %rdi
	movq	$16, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %r15
	movq	$48, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rbx
	movq	$52, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rcx
	movq	$3, %rdi
	xorq	%rax, %rax
	movq	%r15, %rsi
	movq	%rbx, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	popq	%rbx
	popq	%r14
	popq	%r15
	jmp	gen_rtx_fmt_E  # TAILCALL
.Ltmp569:
	.size	gen_absdf2_ifs, .Ltmp569-gen_absdf2_ifs
	.cfi_endproc

	.globl	gen_ashldi3_1
	.align	16, 0x90
	.type	gen_ashldi3_1,@function
gen_ashldi3_1:                          # @gen_ashldi3_1
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp573:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp574:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp575:
	.cfi_def_cfa_offset 32
.Ltmp576:
	.cfi_offset %rbx, -24
.Ltmp577:
	.cfi_offset %r14, -16
	movq	%rdx, %rax
	movq	%rsi, %rcx
	movq	%rdi, %rbx
	movq	$87, %rdi
	movq	$5, %rsi
	movq	%rcx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %r14
	movq	$62, %rdi
	movq	$4, %rsi
	callq	gen_rtx_fmt_0
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rbx
	movq	$52, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rcx
	movq	$3, %rdi
	xorq	%rax, %rax
	movq	%r14, %rsi
	movq	%rbx, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	jmp	gen_rtx_fmt_E  # TAILCALL
.Ltmp578:
	.size	gen_ashldi3_1, .Ltmp578-gen_ashldi3_1
	.cfi_endproc

	.globl	gen_x86_shld_1
	.align	16, 0x90
	.type	gen_x86_shld_1,@function
gen_x86_shld_1:                         # @gen_x86_shld_1
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp584:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp585:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp586:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp587:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp588:
	.cfi_def_cfa_offset 48
.Ltmp589:
	.cfi_offset %rbx, -40
.Ltmp590:
	.cfi_offset %r12, -32
.Ltmp591:
	.cfi_offset %r14, -24
.Ltmp592:
	.cfi_offset %r15, -16
	movq	%rdx, %rbx
	movq	%rsi, %r15
	movq	%rdi, %r14
	movq	$87, %rdi
	movq	$4, %rsi
	movq	%r14, %rdx
	movq	%rbx, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %r12
	xorq	%rdi, %rdi
	movq	$32, %rsi
	callq	gen_rtx_CONST_INT
	movq	$76, %rdi
	movq	$2, %rsi
	movq	%rax, %rdx
	movq	%rbx, %rcx
	callq	gen_rtx_fmt_ee
	movq	$90, %rdi
	movq	$4, %rsi
	movq	%r15, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$84, %rdi
	movq	$4, %rsi
	movq	%r12, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rbx
	movq	$52, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rcx
	movq	$2, %rdi
	xorq	%rax, %rax
	movq	%rbx, %rsi
	movq	%rcx, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	jmp	gen_rtx_fmt_E  # TAILCALL
.Ltmp593:
	.size	gen_x86_shld_1, .Ltmp593-gen_x86_shld_1
	.cfi_endproc

	.globl	gen_ashrdi3_63_rex64
	.align	16, 0x90
	.type	gen_ashrdi3_63_rex64,@function
gen_ashrdi3_63_rex64:                   # @gen_ashrdi3_63_rex64
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp596:
	.cfi_def_cfa_offset 16
.Ltmp597:
	.cfi_offset %rbx, -16
	movq	%rdx, %rax
	movq	%rsi, %rcx
	movq	%rdi, %rbx
	movq	$89, %rdi
	movq	$5, %rsi
	movq	%rcx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rbx
	movq	$52, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rcx
	movq	$2, %rdi
	xorq	%rax, %rax
	movq	%rbx, %rsi
	movq	%rcx, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	popq	%rbx
	jmp	gen_rtx_fmt_E  # TAILCALL
.Ltmp598:
	.size	gen_ashrdi3_63_rex64, .Ltmp598-gen_ashrdi3_63_rex64
	.cfi_endproc

	.globl	gen_ashrdi3_1
	.align	16, 0x90
	.type	gen_ashrdi3_1,@function
gen_ashrdi3_1:                          # @gen_ashrdi3_1
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp602:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp603:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp604:
	.cfi_def_cfa_offset 32
.Ltmp605:
	.cfi_offset %rbx, -24
.Ltmp606:
	.cfi_offset %r14, -16
	movq	%rdx, %rax
	movq	%rsi, %rcx
	movq	%rdi, %rbx
	movq	$89, %rdi
	movq	$5, %rsi
	movq	%rcx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %r14
	movq	$62, %rdi
	movq	$4, %rsi
	callq	gen_rtx_fmt_0
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rbx
	movq	$52, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rcx
	movq	$3, %rdi
	xorq	%rax, %rax
	movq	%r14, %rsi
	movq	%rbx, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	jmp	gen_rtx_fmt_E  # TAILCALL
.Ltmp607:
	.size	gen_ashrdi3_1, .Ltmp607-gen_ashrdi3_1
	.cfi_endproc

	.globl	gen_x86_shrd_1
	.align	16, 0x90
	.type	gen_x86_shrd_1,@function
gen_x86_shrd_1:                         # @gen_x86_shrd_1
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp613:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp614:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp615:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp616:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp617:
	.cfi_def_cfa_offset 48
.Ltmp618:
	.cfi_offset %rbx, -40
.Ltmp619:
	.cfi_offset %r12, -32
.Ltmp620:
	.cfi_offset %r14, -24
.Ltmp621:
	.cfi_offset %r15, -16
	movq	%rdx, %rbx
	movq	%rsi, %r15
	movq	%rdi, %r14
	movq	$89, %rdi
	movq	$4, %rsi
	movq	%r14, %rdx
	movq	%rbx, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %r12
	xorq	%rdi, %rdi
	movq	$32, %rsi
	callq	gen_rtx_CONST_INT
	movq	$76, %rdi
	movq	$2, %rsi
	movq	%rax, %rdx
	movq	%rbx, %rcx
	callq	gen_rtx_fmt_ee
	movq	$87, %rdi
	movq	$4, %rsi
	movq	%r15, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$84, %rdi
	movq	$4, %rsi
	movq	%r12, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rbx
	movq	$52, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rcx
	movq	$2, %rdi
	xorq	%rax, %rax
	movq	%rbx, %rsi
	movq	%rcx, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	jmp	gen_rtx_fmt_E  # TAILCALL
.Ltmp622:
	.size	gen_x86_shrd_1, .Ltmp622-gen_x86_shrd_1
	.cfi_endproc

	.globl	gen_ashrsi3_31
	.align	16, 0x90
	.type	gen_ashrsi3_31,@function
gen_ashrsi3_31:                         # @gen_ashrsi3_31
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp625:
	.cfi_def_cfa_offset 16
.Ltmp626:
	.cfi_offset %rbx, -16
	movq	%rdx, %rax
	movq	%rsi, %rcx
	movq	%rdi, %rbx
	movq	$89, %rdi
	movq	$4, %rsi
	movq	%rcx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rbx
	movq	$52, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rcx
	movq	$2, %rdi
	xorq	%rax, %rax
	movq	%rbx, %rsi
	movq	%rcx, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	popq	%rbx
	jmp	gen_rtx_fmt_E  # TAILCALL
.Ltmp627:
	.size	gen_ashrsi3_31, .Ltmp627-gen_ashrsi3_31
	.cfi_endproc

	.globl	gen_lshrdi3_1
	.align	16, 0x90
	.type	gen_lshrdi3_1,@function
gen_lshrdi3_1:                          # @gen_lshrdi3_1
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp631:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp632:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp633:
	.cfi_def_cfa_offset 32
.Ltmp634:
	.cfi_offset %rbx, -24
.Ltmp635:
	.cfi_offset %r14, -16
	movq	%rdx, %rax
	movq	%rsi, %rcx
	movq	%rdi, %rbx
	movq	$90, %rdi
	movq	$5, %rsi
	movq	%rcx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %r14
	movq	$62, %rdi
	movq	$4, %rsi
	callq	gen_rtx_fmt_0
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rbx
	movq	$52, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rcx
	movq	$3, %rdi
	xorq	%rax, %rax
	movq	%r14, %rsi
	movq	%rbx, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	jmp	gen_rtx_fmt_E  # TAILCALL
.Ltmp636:
	.size	gen_lshrdi3_1, .Ltmp636-gen_lshrdi3_1
	.cfi_endproc

	.globl	gen_setcc_2
	.align	16, 0x90
	.type	gen_setcc_2,@function
gen_setcc_2:                            # @gen_setcc_2
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp640:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp641:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp642:
	.cfi_def_cfa_offset 32
.Ltmp643:
	.cfi_offset %rbx, -24
.Ltmp644:
	.cfi_offset %r14, -16
	movq	%rsi, %rbx
	movq	%rdi, %rax
	movq	$64, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movzbl	(%rbx), %ebx
	movq	%rax, %r14
	xorq	%rdi, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	const_int_rtx+512(%rip), %rcx
	movq	%rax, %rdx
	movq	$2, %rsi
	xorq	%rax, %rax
	movq	%rbx, %rdi
	callq	gen_rtx
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp645:
	.size	gen_setcc_2, .Ltmp645-gen_setcc_2
	.cfi_endproc

	.globl	gen_jump
	.align	16, 0x90
	.type	gen_jump,@function
gen_jump:                               # @gen_jump
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp648:
	.cfi_def_cfa_offset 16
.Ltmp649:
	.cfi_offset %rbx, -16
	movq	global_rtl(%rip), %rbx
	movq	%rdi, %rax
	movq	$67, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_u00
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	popq	%rbx
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp650:
	.size	gen_jump, .Ltmp650-gen_jump
	.cfi_endproc

	.globl	gen_doloop_end_internal
	.align	16, 0x90
	.type	gen_doloop_end_internal,@function
gen_doloop_end_internal:                # @gen_doloop_end_internal
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp657:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp658:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp659:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp660:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp661:
	.cfi_def_cfa_offset 48
.Ltmp662:
	.cfi_offset %rbx, -48
.Ltmp663:
	.cfi_offset %r12, -40
.Ltmp664:
	.cfi_offset %r13, -32
.Ltmp665:
	.cfi_offset %r14, -24
.Ltmp666:
	.cfi_offset %r15, -16
	movq	global_rtl(%rip), %r14
	movq	const_int_rtx+520(%rip), %rcx
	movq	%rdx, %r15
	movq	%rsi, %r12
	movq	%rdi, %rbx
	movq	$102, %rdi
	xorq	%rsi, %rsi
	movq	%r12, %rdx
	callq	gen_rtx_fmt_ee
	movq	%rax, %r13
	movq	$67, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	callq	gen_rtx_fmt_u00
	movq	global_rtl(%rip), %r8
	movq	$72, %rdi
	xorq	%rsi, %rsi
	movq	%r13, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_eee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	const_int_rtx+504(%rip), %rcx
	movq	%rax, %r14
	movq	$75, %rdi
	movq	$4, %rsi
	movq	%r12, %rdx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r15, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %r15
	movq	$62, %rdi
	movq	$4, %rsi
	callq	gen_rtx_fmt_0
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %r12
	movq	$52, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rbx
	movq	$4, %rdi
	xorq	%rax, %rax
	movq	%r14, %rsi
	movq	%r15, %rdx
	movq	%r12, %rcx
	movq	%rbx, %r8
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	gen_rtx_fmt_E  # TAILCALL
.Ltmp667:
	.size	gen_doloop_end_internal, .Ltmp667-gen_doloop_end_internal
	.cfi_endproc

	.globl	gen_blockage
	.align	16, 0x90
	.type	gen_blockage,@function
gen_blockage:                           # @gen_blockage
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rax
.Ltmp669:
	.cfi_def_cfa_offset 16
	movq	const_int_rtx+512(%rip), %rsi
	movq	$1, %rdi
	xorq	%rax, %rax
	callq	gen_rtvec
	movq	$43, %rdi
	xorq	%rsi, %rsi
	xorq	%rcx, %rcx
	movq	%rax, %rdx
	popq	%rax
	jmp	gen_rtx_fmt_Ei  # TAILCALL
.Ltmp670:
	.size	gen_blockage, .Ltmp670-gen_blockage
	.cfi_endproc

	.globl	gen_return_internal
	.align	16, 0x90
	.type	gen_return_internal,@function
gen_return_internal:                    # @gen_return_internal
	.cfi_startproc
# BB#0:                                 # %entry
	movq	$51, %rdi
	xorq	%rsi, %rsi
	jmp	gen_rtx_fmt_  # TAILCALL
.Ltmp671:
	.size	gen_return_internal, .Ltmp671-gen_return_internal
	.cfi_endproc

	.globl	gen_return_pop_internal
	.align	16, 0x90
	.type	gen_return_pop_internal,@function
gen_return_pop_internal:                # @gen_return_pop_internal
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp675:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp676:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp677:
	.cfi_def_cfa_offset 32
.Ltmp678:
	.cfi_offset %rbx, -24
.Ltmp679:
	.cfi_offset %r14, -16
	movq	%rdi, %rbx
	movq	$51, %rdi
	xorq	%rsi, %rsi
	callq	gen_rtx_fmt_
	movq	%rax, %r14
	movq	$48, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rcx
	movq	$2, %rdi
	xorq	%rax, %rax
	movq	%r14, %rsi
	movq	%rcx, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	jmp	gen_rtx_fmt_E  # TAILCALL
.Ltmp680:
	.size	gen_return_pop_internal, .Ltmp680-gen_return_pop_internal
	.cfi_endproc

	.globl	gen_return_indirect_internal
	.align	16, 0x90
	.type	gen_return_indirect_internal,@function
gen_return_indirect_internal:           # @gen_return_indirect_internal
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp684:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp685:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp686:
	.cfi_def_cfa_offset 32
.Ltmp687:
	.cfi_offset %rbx, -24
.Ltmp688:
	.cfi_offset %r14, -16
	movq	%rdi, %rbx
	movq	$51, %rdi
	xorq	%rsi, %rsi
	callq	gen_rtx_fmt_
	movq	%rax, %r14
	movq	$48, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rcx
	movq	$2, %rdi
	xorq	%rax, %rax
	movq	%r14, %rsi
	movq	%rcx, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	jmp	gen_rtx_fmt_E  # TAILCALL
.Ltmp689:
	.size	gen_return_indirect_internal, .Ltmp689-gen_return_indirect_internal
	.cfi_endproc

	.globl	gen_nop
	.align	16, 0x90
	.type	gen_nop,@function
gen_nop:                                # @gen_nop
	.cfi_startproc
# BB#0:                                 # %entry
	movq	const_int_rtx+512(%rip), %rax
	retq
.Ltmp690:
	.size	gen_nop, .Ltmp690-gen_nop
	.cfi_endproc

	.globl	gen_prologue_set_got
	.align	16, 0x90
	.type	gen_prologue_set_got,@function
gen_prologue_set_got:                   # @gen_prologue_set_got
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp694:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp695:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp696:
	.cfi_def_cfa_offset 32
.Ltmp697:
	.cfi_offset %rbx, -24
.Ltmp698:
	.cfi_offset %r14, -16
	movq	global_rtl(%rip), %rax
	movq	%rdx, %rcx
	movq	%rsi, %rbx
	movq	%rdi, %r14
	movq	$76, %rdi
	movq	$4, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_ee
	movq	$75, %rdi
	movq	$4, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$75, %rdi
	movq	$4, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rcx
	movq	$1, %rdi
	xorq	%rax, %rax
	movq	%rcx, %rsi
	callq	gen_rtvec
	movq	$43, %rdi
	movq	$4, %rsi
	movq	$1, %rcx
	movq	%rax, %rdx
	callq	gen_rtx_fmt_Ei
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rbx
	movq	$52, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rcx
	movq	$2, %rdi
	xorq	%rax, %rax
	movq	%rbx, %rsi
	movq	%rcx, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	jmp	gen_rtx_fmt_E  # TAILCALL
.Ltmp699:
	.size	gen_prologue_set_got, .Ltmp699-gen_prologue_set_got
	.cfi_endproc

	.globl	gen_prologue_get_pc
	.align	16, 0x90
	.type	gen_prologue_get_pc,@function
gen_prologue_get_pc:                    # @gen_prologue_get_pc
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp702:
	.cfi_def_cfa_offset 16
.Ltmp703:
	.cfi_offset %rbx, -16
	movq	global_rtl(%rip), %rdx
	movq	%rsi, %rax
	movq	%rdi, %rbx
	movq	$75, %rdi
	movq	$4, %rsi
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rcx
	movq	$1, %rdi
	xorq	%rax, %rax
	movq	%rcx, %rsi
	callq	gen_rtvec
	movq	$43, %rdi
	movq	$4, %rsi
	movq	$2, %rcx
	movq	%rax, %rdx
	callq	gen_rtx_fmt_Ei
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	popq	%rbx
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp704:
	.size	gen_prologue_get_pc, .Ltmp704-gen_prologue_get_pc
	.cfi_endproc

	.globl	gen_eh_return_si
	.align	16, 0x90
	.type	gen_eh_return_si,@function
gen_eh_return_si:                       # @gen_eh_return_si
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rax
.Ltmp706:
	.cfi_def_cfa_offset 16
	movq	%rdi, %rcx
	movq	$1, %rdi
	xorq	%rax, %rax
	movq	%rcx, %rsi
	callq	gen_rtvec
	movq	$43, %rdi
	xorq	%rsi, %rsi
	movq	$13, %rcx
	movq	%rax, %rdx
	popq	%rax
	jmp	gen_rtx_fmt_Ei  # TAILCALL
.Ltmp707:
	.size	gen_eh_return_si, .Ltmp707-gen_eh_return_si
	.cfi_endproc

	.globl	gen_eh_return_di
	.align	16, 0x90
	.type	gen_eh_return_di,@function
gen_eh_return_di:                       # @gen_eh_return_di
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rax
.Ltmp709:
	.cfi_def_cfa_offset 16
	movq	%rdi, %rcx
	movq	$1, %rdi
	xorq	%rax, %rax
	movq	%rcx, %rsi
	callq	gen_rtvec
	movq	$43, %rdi
	xorq	%rsi, %rsi
	movq	$13, %rcx
	movq	%rax, %rdx
	popq	%rax
	jmp	gen_rtx_fmt_Ei  # TAILCALL
.Ltmp710:
	.size	gen_eh_return_di, .Ltmp710-gen_eh_return_di
	.cfi_endproc

	.globl	gen_leave
	.align	16, 0x90
	.type	gen_leave,@function
gen_leave:                              # @gen_leave
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp714:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp715:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp716:
	.cfi_def_cfa_offset 32
.Ltmp717:
	.cfi_offset %rbx, -24
.Ltmp718:
	.cfi_offset %r14, -16
	movq	$4, %rdi
	movq	$7, %rsi
	callq	gen_rtx_REG
	movq	%rax, %r14
	movq	$4, %rdi
	movq	$6, %rsi
	callq	gen_rtx_REG
	movq	%rax, %rbx
	xorq	%rdi, %rdi
	movq	$4, %rsi
	callq	gen_rtx_CONST_INT
	movq	$75, %rdi
	movq	$4, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %r14
	movq	$4, %rdi
	movq	$6, %rsi
	callq	gen_rtx_REG
	movq	%rax, %rbx
	movq	$4, %rdi
	movq	$6, %rsi
	callq	gen_rtx_REG
	movq	$4, %rdi
	movq	%rax, %rsi
	callq	gen_rtx_MEM
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rbx
	movq	$62, %rdi
	xorq	%rsi, %rsi
	callq	gen_rtx_fmt_0
	movq	$51, %rdi
	movq	%rax, %rsi
	callq	gen_rtx_MEM
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rcx
	movq	$3, %rdi
	xorq	%rax, %rax
	movq	%r14, %rsi
	movq	%rbx, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	jmp	gen_rtx_fmt_E  # TAILCALL
.Ltmp719:
	.size	gen_leave, .Ltmp719-gen_leave
	.cfi_endproc

	.globl	gen_leave_rex64
	.align	16, 0x90
	.type	gen_leave_rex64,@function
gen_leave_rex64:                        # @gen_leave_rex64
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp723:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp724:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp725:
	.cfi_def_cfa_offset 32
.Ltmp726:
	.cfi_offset %rbx, -24
.Ltmp727:
	.cfi_offset %r14, -16
	movq	$5, %rdi
	movq	$7, %rsi
	callq	gen_rtx_REG
	movq	%rax, %r14
	movq	$5, %rdi
	movq	$6, %rsi
	callq	gen_rtx_REG
	movq	%rax, %rbx
	xorq	%rdi, %rdi
	movq	$8, %rsi
	callq	gen_rtx_CONST_INT
	movq	$75, %rdi
	movq	$5, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %r14
	movq	$5, %rdi
	movq	$6, %rsi
	callq	gen_rtx_REG
	movq	%rax, %rbx
	movq	$5, %rdi
	movq	$6, %rsi
	callq	gen_rtx_REG
	movq	$5, %rdi
	movq	%rax, %rsi
	callq	gen_rtx_MEM
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rbx
	movq	$62, %rdi
	xorq	%rsi, %rsi
	callq	gen_rtx_fmt_0
	movq	$51, %rdi
	movq	%rax, %rsi
	callq	gen_rtx_MEM
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rcx
	movq	$3, %rdi
	xorq	%rax, %rax
	movq	%r14, %rsi
	movq	%rbx, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	jmp	gen_rtx_fmt_E  # TAILCALL
.Ltmp728:
	.size	gen_leave_rex64, .Ltmp728-gen_leave_rex64
	.cfi_endproc

	.globl	gen_ffssi_1
	.align	16, 0x90
	.type	gen_ffssi_1,@function
gen_ffssi_1:                            # @gen_ffssi_1
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp733:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp734:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp735:
	.cfi_def_cfa_offset 32
.Ltmp736:
	.cfi_offset %rbx, -32
.Ltmp737:
	.cfi_offset %r14, -24
.Ltmp738:
	.cfi_offset %r15, -16
	movq	%rsi, %r15
	movq	%rdi, %r14
	movq	$56, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	const_int_rtx+512(%rip), %rcx
	movq	%rax, %rbx
	movq	$74, %rdi
	movq	$56, %rsi
	movq	%r15, %rdx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rbx
	movq	$1, %rdi
	xorq	%rax, %rax
	movq	%r15, %rsi
	callq	gen_rtvec
	movq	$42, %rdi
	movq	$4, %rsi
	movq	$5, %rcx
	movq	%rax, %rdx
	callq	gen_rtx_fmt_Ei
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rcx
	movq	$2, %rdi
	xorq	%rax, %rax
	movq	%rbx, %rsi
	movq	%rcx, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	popq	%rbx
	popq	%r14
	popq	%r15
	jmp	gen_rtx_fmt_E  # TAILCALL
.Ltmp739:
	.size	gen_ffssi_1, .Ltmp739-gen_ffssi_1
	.cfi_endproc

	.globl	gen_sqrtsf2_1
	.align	16, 0x90
	.type	gen_sqrtsf2_1,@function
gen_sqrtsf2_1:                          # @gen_sqrtsf2_1
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp742:
	.cfi_def_cfa_offset 16
.Ltmp743:
	.cfi_offset %rbx, -16
	movq	%rsi, %rax
	movq	%rdi, %rbx
	movq	$130, %rdi
	movq	$15, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	popq	%rbx
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp744:
	.size	gen_sqrtsf2_1, .Ltmp744-gen_sqrtsf2_1
	.cfi_endproc

	.globl	gen_sqrtsf2_1_sse_only
	.align	16, 0x90
	.type	gen_sqrtsf2_1_sse_only,@function
gen_sqrtsf2_1_sse_only:                 # @gen_sqrtsf2_1_sse_only
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp747:
	.cfi_def_cfa_offset 16
.Ltmp748:
	.cfi_offset %rbx, -16
	movq	%rsi, %rax
	movq	%rdi, %rbx
	movq	$130, %rdi
	movq	$15, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	popq	%rbx
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp749:
	.size	gen_sqrtsf2_1_sse_only, .Ltmp749-gen_sqrtsf2_1_sse_only
	.cfi_endproc

	.globl	gen_sqrtsf2_i387
	.align	16, 0x90
	.type	gen_sqrtsf2_i387,@function
gen_sqrtsf2_i387:                       # @gen_sqrtsf2_i387
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp752:
	.cfi_def_cfa_offset 16
.Ltmp753:
	.cfi_offset %rbx, -16
	movq	%rsi, %rax
	movq	%rdi, %rbx
	movq	$130, %rdi
	movq	$15, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	popq	%rbx
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp754:
	.size	gen_sqrtsf2_i387, .Ltmp754-gen_sqrtsf2_i387
	.cfi_endproc

	.globl	gen_sqrtdf2_1
	.align	16, 0x90
	.type	gen_sqrtdf2_1,@function
gen_sqrtdf2_1:                          # @gen_sqrtdf2_1
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp757:
	.cfi_def_cfa_offset 16
.Ltmp758:
	.cfi_offset %rbx, -16
	movq	%rsi, %rax
	movq	%rdi, %rbx
	movq	$130, %rdi
	movq	$16, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	popq	%rbx
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp759:
	.size	gen_sqrtdf2_1, .Ltmp759-gen_sqrtdf2_1
	.cfi_endproc

	.globl	gen_sqrtdf2_1_sse_only
	.align	16, 0x90
	.type	gen_sqrtdf2_1_sse_only,@function
gen_sqrtdf2_1_sse_only:                 # @gen_sqrtdf2_1_sse_only
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp762:
	.cfi_def_cfa_offset 16
.Ltmp763:
	.cfi_offset %rbx, -16
	movq	%rsi, %rax
	movq	%rdi, %rbx
	movq	$130, %rdi
	movq	$16, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	popq	%rbx
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp764:
	.size	gen_sqrtdf2_1_sse_only, .Ltmp764-gen_sqrtdf2_1_sse_only
	.cfi_endproc

	.globl	gen_sqrtdf2_i387
	.align	16, 0x90
	.type	gen_sqrtdf2_i387,@function
gen_sqrtdf2_i387:                       # @gen_sqrtdf2_i387
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp767:
	.cfi_def_cfa_offset 16
.Ltmp768:
	.cfi_offset %rbx, -16
	movq	%rsi, %rax
	movq	%rdi, %rbx
	movq	$130, %rdi
	movq	$16, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	popq	%rbx
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp769:
	.size	gen_sqrtdf2_i387, .Ltmp769-gen_sqrtdf2_i387
	.cfi_endproc

	.globl	gen_sqrtxf2
	.align	16, 0x90
	.type	gen_sqrtxf2,@function
gen_sqrtxf2:                            # @gen_sqrtxf2
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp772:
	.cfi_def_cfa_offset 16
.Ltmp773:
	.cfi_offset %rbx, -16
	movq	%rsi, %rax
	movq	%rdi, %rbx
	movq	$130, %rdi
	movq	$17, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	popq	%rbx
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp774:
	.size	gen_sqrtxf2, .Ltmp774-gen_sqrtxf2
	.cfi_endproc

	.globl	gen_sqrttf2
	.align	16, 0x90
	.type	gen_sqrttf2,@function
gen_sqrttf2:                            # @gen_sqrttf2
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp777:
	.cfi_def_cfa_offset 16
.Ltmp778:
	.cfi_offset %rbx, -16
	movq	%rsi, %rax
	movq	%rdi, %rbx
	movq	$130, %rdi
	movq	$18, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	popq	%rbx
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp779:
	.size	gen_sqrttf2, .Ltmp779-gen_sqrttf2
	.cfi_endproc

	.globl	gen_sindf2
	.align	16, 0x90
	.type	gen_sindf2,@function
gen_sindf2:                             # @gen_sindf2
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp782:
	.cfi_def_cfa_offset 16
.Ltmp783:
	.cfi_offset %rbx, -16
	movq	%rdi, %rbx
	movq	$1, %rdi
	xorq	%rax, %rax
	callq	gen_rtvec
	movq	$42, %rdi
	movq	$16, %rsi
	movq	$1, %rcx
	movq	%rax, %rdx
	callq	gen_rtx_fmt_Ei
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	popq	%rbx
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp784:
	.size	gen_sindf2, .Ltmp784-gen_sindf2
	.cfi_endproc

	.globl	gen_sinsf2
	.align	16, 0x90
	.type	gen_sinsf2,@function
gen_sinsf2:                             # @gen_sinsf2
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp787:
	.cfi_def_cfa_offset 16
.Ltmp788:
	.cfi_offset %rbx, -16
	movq	%rdi, %rbx
	movq	$1, %rdi
	xorq	%rax, %rax
	callq	gen_rtvec
	movq	$42, %rdi
	movq	$15, %rsi
	movq	$1, %rcx
	movq	%rax, %rdx
	callq	gen_rtx_fmt_Ei
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	popq	%rbx
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp789:
	.size	gen_sinsf2, .Ltmp789-gen_sinsf2
	.cfi_endproc

	.globl	gen_sinxf2
	.align	16, 0x90
	.type	gen_sinxf2,@function
gen_sinxf2:                             # @gen_sinxf2
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp792:
	.cfi_def_cfa_offset 16
.Ltmp793:
	.cfi_offset %rbx, -16
	movq	%rdi, %rbx
	movq	$1, %rdi
	xorq	%rax, %rax
	callq	gen_rtvec
	movq	$42, %rdi
	movq	$17, %rsi
	movq	$1, %rcx
	movq	%rax, %rdx
	callq	gen_rtx_fmt_Ei
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	popq	%rbx
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp794:
	.size	gen_sinxf2, .Ltmp794-gen_sinxf2
	.cfi_endproc

	.globl	gen_sintf2
	.align	16, 0x90
	.type	gen_sintf2,@function
gen_sintf2:                             # @gen_sintf2
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp797:
	.cfi_def_cfa_offset 16
.Ltmp798:
	.cfi_offset %rbx, -16
	movq	%rdi, %rbx
	movq	$1, %rdi
	xorq	%rax, %rax
	callq	gen_rtvec
	movq	$42, %rdi
	movq	$18, %rsi
	movq	$1, %rcx
	movq	%rax, %rdx
	callq	gen_rtx_fmt_Ei
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	popq	%rbx
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp799:
	.size	gen_sintf2, .Ltmp799-gen_sintf2
	.cfi_endproc

	.globl	gen_cosdf2
	.align	16, 0x90
	.type	gen_cosdf2,@function
gen_cosdf2:                             # @gen_cosdf2
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp802:
	.cfi_def_cfa_offset 16
.Ltmp803:
	.cfi_offset %rbx, -16
	movq	%rdi, %rbx
	movq	$1, %rdi
	xorq	%rax, %rax
	callq	gen_rtvec
	movq	$42, %rdi
	movq	$16, %rsi
	movq	$2, %rcx
	movq	%rax, %rdx
	callq	gen_rtx_fmt_Ei
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	popq	%rbx
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp804:
	.size	gen_cosdf2, .Ltmp804-gen_cosdf2
	.cfi_endproc

	.globl	gen_cossf2
	.align	16, 0x90
	.type	gen_cossf2,@function
gen_cossf2:                             # @gen_cossf2
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp807:
	.cfi_def_cfa_offset 16
.Ltmp808:
	.cfi_offset %rbx, -16
	movq	%rdi, %rbx
	movq	$1, %rdi
	xorq	%rax, %rax
	callq	gen_rtvec
	movq	$42, %rdi
	movq	$15, %rsi
	movq	$2, %rcx
	movq	%rax, %rdx
	callq	gen_rtx_fmt_Ei
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	popq	%rbx
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp809:
	.size	gen_cossf2, .Ltmp809-gen_cossf2
	.cfi_endproc

	.globl	gen_cosxf2
	.align	16, 0x90
	.type	gen_cosxf2,@function
gen_cosxf2:                             # @gen_cosxf2
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp812:
	.cfi_def_cfa_offset 16
.Ltmp813:
	.cfi_offset %rbx, -16
	movq	%rdi, %rbx
	movq	$1, %rdi
	xorq	%rax, %rax
	callq	gen_rtvec
	movq	$42, %rdi
	movq	$17, %rsi
	movq	$2, %rcx
	movq	%rax, %rdx
	callq	gen_rtx_fmt_Ei
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	popq	%rbx
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp814:
	.size	gen_cosxf2, .Ltmp814-gen_cosxf2
	.cfi_endproc

	.globl	gen_costf2
	.align	16, 0x90
	.type	gen_costf2,@function
gen_costf2:                             # @gen_costf2
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp817:
	.cfi_def_cfa_offset 16
.Ltmp818:
	.cfi_offset %rbx, -16
	movq	%rdi, %rbx
	movq	$1, %rdi
	xorq	%rax, %rax
	callq	gen_rtvec
	movq	$42, %rdi
	movq	$18, %rsi
	movq	$2, %rcx
	movq	%rax, %rdx
	callq	gen_rtx_fmt_Ei
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	popq	%rbx
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp819:
	.size	gen_costf2, .Ltmp819-gen_costf2
	.cfi_endproc

	.globl	gen_cld
	.align	16, 0x90
	.type	gen_cld,@function
gen_cld:                                # @gen_cld
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rax
.Ltmp821:
	.cfi_def_cfa_offset 16
	movq	$4, %rdi
	movq	$19, %rsi
	callq	gen_rtx_REG
	movq	const_int_rtx+512(%rip), %rcx
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	popq	%rax
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp822:
	.size	gen_cld, .Ltmp822-gen_cld
	.cfi_endproc

	.globl	gen_strmovdi_rex_1
	.align	16, 0x90
	.type	gen_strmovdi_rex_1,@function
gen_strmovdi_rex_1:                     # @gen_strmovdi_rex_1
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp829:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp830:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp831:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp832:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp833:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp834:
	.cfi_def_cfa_offset 64
.Ltmp835:
	.cfi_offset %rbx, -48
.Ltmp836:
	.cfi_offset %r12, -40
.Ltmp837:
	.cfi_offset %r13, -32
.Ltmp838:
	.cfi_offset %r14, -24
.Ltmp839:
	.cfi_offset %r15, -16
	movq	%rcx, %r13
	movq	%rdx, %r14
	movq	%rsi, %r15
	movq	%rdi, %r12
	movq	$5, %rdi
	movq	%r14, %rsi
	callq	gen_rtx_MEM
	movq	%rax, %rbx
	movq	$5, %rdi
	movq	%r13, %rsi
	callq	gen_rtx_MEM
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, 8(%rsp)           # 8-byte Spill
	xorq	%rdi, %rdi
	movq	$8, %rsi
	callq	gen_rtx_CONST_INT
	movq	$75, %rdi
	movq	$5, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r12, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %r12
	xorq	%rdi, %rdi
	movq	$8, %rsi
	callq	gen_rtx_CONST_INT
	movq	$75, %rdi
	movq	$5, %rsi
	movq	%r13, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r15, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %r14
	movq	$4, %rdi
	movq	$19, %rsi
	callq	gen_rtx_REG
	movq	$48, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rbx
	movq	$4, %rdi
	xorq	%rax, %rax
	movq	8(%rsp), %rsi           # 8-byte Reload
	movq	%r12, %rdx
	movq	%r14, %rcx
	movq	%rbx, %r8
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	gen_rtx_fmt_E  # TAILCALL
.Ltmp840:
	.size	gen_strmovdi_rex_1, .Ltmp840-gen_strmovdi_rex_1
	.cfi_endproc

	.globl	gen_strmovsi_1
	.align	16, 0x90
	.type	gen_strmovsi_1,@function
gen_strmovsi_1:                         # @gen_strmovsi_1
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp847:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp848:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp849:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp850:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp851:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp852:
	.cfi_def_cfa_offset 64
.Ltmp853:
	.cfi_offset %rbx, -48
.Ltmp854:
	.cfi_offset %r12, -40
.Ltmp855:
	.cfi_offset %r13, -32
.Ltmp856:
	.cfi_offset %r14, -24
.Ltmp857:
	.cfi_offset %r15, -16
	movq	%rcx, %r13
	movq	%rdx, %r14
	movq	%rsi, %r15
	movq	%rdi, %r12
	movq	$4, %rdi
	movq	%r14, %rsi
	callq	gen_rtx_MEM
	movq	%rax, %rbx
	movq	$4, %rdi
	movq	%r13, %rsi
	callq	gen_rtx_MEM
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, 8(%rsp)           # 8-byte Spill
	xorq	%rdi, %rdi
	movq	$4, %rsi
	callq	gen_rtx_CONST_INT
	movq	$75, %rdi
	movq	$4, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r12, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %r12
	xorq	%rdi, %rdi
	movq	$4, %rsi
	callq	gen_rtx_CONST_INT
	movq	$75, %rdi
	movq	$4, %rsi
	movq	%r13, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r15, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %r14
	movq	$4, %rdi
	movq	$19, %rsi
	callq	gen_rtx_REG
	movq	$48, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rbx
	movq	$4, %rdi
	xorq	%rax, %rax
	movq	8(%rsp), %rsi           # 8-byte Reload
	movq	%r12, %rdx
	movq	%r14, %rcx
	movq	%rbx, %r8
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	gen_rtx_fmt_E  # TAILCALL
.Ltmp858:
	.size	gen_strmovsi_1, .Ltmp858-gen_strmovsi_1
	.cfi_endproc

	.globl	gen_strmovsi_rex_1
	.align	16, 0x90
	.type	gen_strmovsi_rex_1,@function
gen_strmovsi_rex_1:                     # @gen_strmovsi_rex_1
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp865:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp866:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp867:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp868:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp869:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp870:
	.cfi_def_cfa_offset 64
.Ltmp871:
	.cfi_offset %rbx, -48
.Ltmp872:
	.cfi_offset %r12, -40
.Ltmp873:
	.cfi_offset %r13, -32
.Ltmp874:
	.cfi_offset %r14, -24
.Ltmp875:
	.cfi_offset %r15, -16
	movq	%rcx, %r13
	movq	%rdx, %r14
	movq	%rsi, %r15
	movq	%rdi, %r12
	movq	$4, %rdi
	movq	%r14, %rsi
	callq	gen_rtx_MEM
	movq	%rax, %rbx
	movq	$4, %rdi
	movq	%r13, %rsi
	callq	gen_rtx_MEM
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, 8(%rsp)           # 8-byte Spill
	xorq	%rdi, %rdi
	movq	$4, %rsi
	callq	gen_rtx_CONST_INT
	movq	$75, %rdi
	movq	$5, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r12, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %r12
	xorq	%rdi, %rdi
	movq	$4, %rsi
	callq	gen_rtx_CONST_INT
	movq	$75, %rdi
	movq	$5, %rsi
	movq	%r13, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r15, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %r14
	movq	$4, %rdi
	movq	$19, %rsi
	callq	gen_rtx_REG
	movq	$48, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rbx
	movq	$4, %rdi
	xorq	%rax, %rax
	movq	8(%rsp), %rsi           # 8-byte Reload
	movq	%r12, %rdx
	movq	%r14, %rcx
	movq	%rbx, %r8
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	gen_rtx_fmt_E  # TAILCALL
.Ltmp876:
	.size	gen_strmovsi_rex_1, .Ltmp876-gen_strmovsi_rex_1
	.cfi_endproc

	.globl	gen_strmovhi_1
	.align	16, 0x90
	.type	gen_strmovhi_1,@function
gen_strmovhi_1:                         # @gen_strmovhi_1
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp883:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp884:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp885:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp886:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp887:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp888:
	.cfi_def_cfa_offset 64
.Ltmp889:
	.cfi_offset %rbx, -48
.Ltmp890:
	.cfi_offset %r12, -40
.Ltmp891:
	.cfi_offset %r13, -32
.Ltmp892:
	.cfi_offset %r14, -24
.Ltmp893:
	.cfi_offset %r15, -16
	movq	%rcx, %r13
	movq	%rdx, %r14
	movq	%rsi, %r15
	movq	%rdi, %r12
	movq	$3, %rdi
	movq	%r14, %rsi
	callq	gen_rtx_MEM
	movq	%rax, %rbx
	movq	$3, %rdi
	movq	%r13, %rsi
	callq	gen_rtx_MEM
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, 8(%rsp)           # 8-byte Spill
	xorq	%rdi, %rdi
	movq	$2, %rsi
	callq	gen_rtx_CONST_INT
	movq	$75, %rdi
	movq	$4, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r12, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %r12
	xorq	%rdi, %rdi
	movq	$2, %rsi
	callq	gen_rtx_CONST_INT
	movq	$75, %rdi
	movq	$4, %rsi
	movq	%r13, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r15, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %r14
	movq	$4, %rdi
	movq	$19, %rsi
	callq	gen_rtx_REG
	movq	$48, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rbx
	movq	$4, %rdi
	xorq	%rax, %rax
	movq	8(%rsp), %rsi           # 8-byte Reload
	movq	%r12, %rdx
	movq	%r14, %rcx
	movq	%rbx, %r8
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	gen_rtx_fmt_E  # TAILCALL
.Ltmp894:
	.size	gen_strmovhi_1, .Ltmp894-gen_strmovhi_1
	.cfi_endproc

	.globl	gen_strmovhi_rex_1
	.align	16, 0x90
	.type	gen_strmovhi_rex_1,@function
gen_strmovhi_rex_1:                     # @gen_strmovhi_rex_1
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp901:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp902:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp903:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp904:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp905:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp906:
	.cfi_def_cfa_offset 64
.Ltmp907:
	.cfi_offset %rbx, -48
.Ltmp908:
	.cfi_offset %r12, -40
.Ltmp909:
	.cfi_offset %r13, -32
.Ltmp910:
	.cfi_offset %r14, -24
.Ltmp911:
	.cfi_offset %r15, -16
	movq	%rcx, %r13
	movq	%rdx, %r14
	movq	%rsi, %r15
	movq	%rdi, %r12
	movq	$3, %rdi
	movq	%r14, %rsi
	callq	gen_rtx_MEM
	movq	%rax, %rbx
	movq	$3, %rdi
	movq	%r13, %rsi
	callq	gen_rtx_MEM
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, 8(%rsp)           # 8-byte Spill
	xorq	%rdi, %rdi
	movq	$2, %rsi
	callq	gen_rtx_CONST_INT
	movq	$75, %rdi
	movq	$5, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r12, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %r12
	xorq	%rdi, %rdi
	movq	$2, %rsi
	callq	gen_rtx_CONST_INT
	movq	$75, %rdi
	movq	$5, %rsi
	movq	%r13, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r15, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %r14
	movq	$4, %rdi
	movq	$19, %rsi
	callq	gen_rtx_REG
	movq	$48, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rbx
	movq	$4, %rdi
	xorq	%rax, %rax
	movq	8(%rsp), %rsi           # 8-byte Reload
	movq	%r12, %rdx
	movq	%r14, %rcx
	movq	%rbx, %r8
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	gen_rtx_fmt_E  # TAILCALL
.Ltmp912:
	.size	gen_strmovhi_rex_1, .Ltmp912-gen_strmovhi_rex_1
	.cfi_endproc

	.globl	gen_strmovqi_1
	.align	16, 0x90
	.type	gen_strmovqi_1,@function
gen_strmovqi_1:                         # @gen_strmovqi_1
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp919:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp920:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp921:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp922:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp923:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp924:
	.cfi_def_cfa_offset 64
.Ltmp925:
	.cfi_offset %rbx, -48
.Ltmp926:
	.cfi_offset %r12, -40
.Ltmp927:
	.cfi_offset %r13, -32
.Ltmp928:
	.cfi_offset %r14, -24
.Ltmp929:
	.cfi_offset %r15, -16
	movq	%rcx, %r13
	movq	%rdx, %r14
	movq	%rsi, %r15
	movq	%rdi, %r12
	movq	$2, %rdi
	movq	%r14, %rsi
	callq	gen_rtx_MEM
	movq	%rax, %rbx
	movq	$2, %rdi
	movq	%r13, %rsi
	callq	gen_rtx_MEM
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	const_int_rtx+520(%rip), %rcx
	movq	%rax, 8(%rsp)           # 8-byte Spill
	movq	$75, %rdi
	movq	$4, %rsi
	movq	%r14, %rdx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r12, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	const_int_rtx+520(%rip), %rcx
	movq	%rax, %r12
	movq	$75, %rdi
	movq	$4, %rsi
	movq	%r13, %rdx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r15, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %r14
	movq	$4, %rdi
	movq	$19, %rsi
	callq	gen_rtx_REG
	movq	$48, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rbx
	movq	$4, %rdi
	xorq	%rax, %rax
	movq	8(%rsp), %rsi           # 8-byte Reload
	movq	%r12, %rdx
	movq	%r14, %rcx
	movq	%rbx, %r8
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	gen_rtx_fmt_E  # TAILCALL
.Ltmp930:
	.size	gen_strmovqi_1, .Ltmp930-gen_strmovqi_1
	.cfi_endproc

	.globl	gen_strmovqi_rex_1
	.align	16, 0x90
	.type	gen_strmovqi_rex_1,@function
gen_strmovqi_rex_1:                     # @gen_strmovqi_rex_1
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp937:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp938:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp939:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp940:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp941:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp942:
	.cfi_def_cfa_offset 64
.Ltmp943:
	.cfi_offset %rbx, -48
.Ltmp944:
	.cfi_offset %r12, -40
.Ltmp945:
	.cfi_offset %r13, -32
.Ltmp946:
	.cfi_offset %r14, -24
.Ltmp947:
	.cfi_offset %r15, -16
	movq	%rcx, %r13
	movq	%rdx, %r14
	movq	%rsi, %r15
	movq	%rdi, %r12
	movq	$2, %rdi
	movq	%r14, %rsi
	callq	gen_rtx_MEM
	movq	%rax, %rbx
	movq	$2, %rdi
	movq	%r13, %rsi
	callq	gen_rtx_MEM
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	const_int_rtx+520(%rip), %rcx
	movq	%rax, 8(%rsp)           # 8-byte Spill
	movq	$75, %rdi
	movq	$5, %rsi
	movq	%r14, %rdx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r12, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	const_int_rtx+520(%rip), %rcx
	movq	%rax, %r12
	movq	$75, %rdi
	movq	$5, %rsi
	movq	%r13, %rdx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r15, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %r14
	movq	$4, %rdi
	movq	$19, %rsi
	callq	gen_rtx_REG
	movq	$48, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rbx
	movq	$4, %rdi
	xorq	%rax, %rax
	movq	8(%rsp), %rsi           # 8-byte Reload
	movq	%r12, %rdx
	movq	%r14, %rcx
	movq	%rbx, %r8
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	gen_rtx_fmt_E  # TAILCALL
.Ltmp948:
	.size	gen_strmovqi_rex_1, .Ltmp948-gen_strmovqi_rex_1
	.cfi_endproc

	.globl	gen_rep_movdi_rex64
	.align	16, 0x90
	.type	gen_rep_movdi_rex64,@function
gen_rep_movdi_rex64:                    # @gen_rep_movdi_rex64
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp955:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp956:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp957:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp958:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp959:
	.cfi_def_cfa_offset 48
	subq	$32, %rsp
.Ltmp960:
	.cfi_def_cfa_offset 80
.Ltmp961:
	.cfi_offset %rbx, -48
.Ltmp962:
	.cfi_offset %r12, -40
.Ltmp963:
	.cfi_offset %r13, -32
.Ltmp964:
	.cfi_offset %r14, -24
.Ltmp965:
	.cfi_offset %r15, -16
	movq	const_int_rtx+512(%rip), %rax
	movq	%r9, %r14
	movq	%r8, %r12
	movq	%rcx, %r13
	movq	%rsi, %rbx
	movq	%rdi, %r15
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, 24(%rsp)          # 8-byte Spill
	xorq	%rdi, %rdi
	movq	$3, %rsi
	callq	gen_rtx_CONST_INT
	movq	$87, %rdi
	movq	$5, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$75, %rdi
	movq	$5, %rsi
	movq	%rax, %rdx
	movq	%r13, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r15, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, 16(%rsp)          # 8-byte Spill
	xorq	%rdi, %rdi
	movq	$3, %rsi
	callq	gen_rtx_CONST_INT
	movq	$87, %rdi
	movq	$5, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$75, %rdi
	movq	$5, %rsi
	movq	%rax, %rdx
	movq	%r12, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %r15
	movq	$51, %rdi
	movq	%r13, %rsi
	callq	gen_rtx_MEM
	movq	%rax, %rbx
	movq	$51, %rdi
	movq	%r12, %rsi
	callq	gen_rtx_MEM
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %r12
	movq	$48, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rbx
	movq	$4, %rdi
	movq	$19, %rsi
	callq	gen_rtx_REG
	movq	$48, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rcx
	movq	$6, %rdi
	xorq	%rax, %rax
	movq	%rcx, (%rsp)
	movq	24(%rsp), %rsi          # 8-byte Reload
	movq	16(%rsp), %rdx          # 8-byte Reload
	movq	%r15, %rcx
	movq	%r12, %r8
	movq	%rbx, %r9
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	gen_rtx_fmt_E  # TAILCALL
.Ltmp966:
	.size	gen_rep_movdi_rex64, .Ltmp966-gen_rep_movdi_rex64
	.cfi_endproc

	.globl	gen_rep_movsi
	.align	16, 0x90
	.type	gen_rep_movsi,@function
gen_rep_movsi:                          # @gen_rep_movsi
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp973:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp974:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp975:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp976:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp977:
	.cfi_def_cfa_offset 48
	subq	$32, %rsp
.Ltmp978:
	.cfi_def_cfa_offset 80
.Ltmp979:
	.cfi_offset %rbx, -48
.Ltmp980:
	.cfi_offset %r12, -40
.Ltmp981:
	.cfi_offset %r13, -32
.Ltmp982:
	.cfi_offset %r14, -24
.Ltmp983:
	.cfi_offset %r15, -16
	movq	const_int_rtx+512(%rip), %rax
	movq	%r9, %r14
	movq	%r8, %r12
	movq	%rcx, %r13
	movq	%rsi, %rbx
	movq	%rdi, %r15
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, 24(%rsp)          # 8-byte Spill
	xorq	%rdi, %rdi
	movq	$2, %rsi
	callq	gen_rtx_CONST_INT
	movq	$87, %rdi
	movq	$4, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$75, %rdi
	movq	$4, %rsi
	movq	%rax, %rdx
	movq	%r13, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r15, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, 16(%rsp)          # 8-byte Spill
	xorq	%rdi, %rdi
	movq	$2, %rsi
	callq	gen_rtx_CONST_INT
	movq	$87, %rdi
	movq	$4, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$75, %rdi
	movq	$4, %rsi
	movq	%rax, %rdx
	movq	%r12, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %r15
	movq	$51, %rdi
	movq	%r13, %rsi
	callq	gen_rtx_MEM
	movq	%rax, %rbx
	movq	$51, %rdi
	movq	%r12, %rsi
	callq	gen_rtx_MEM
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %r12
	movq	$48, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rbx
	movq	$4, %rdi
	movq	$19, %rsi
	callq	gen_rtx_REG
	movq	$48, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rcx
	movq	$6, %rdi
	xorq	%rax, %rax
	movq	%rcx, (%rsp)
	movq	24(%rsp), %rsi          # 8-byte Reload
	movq	16(%rsp), %rdx          # 8-byte Reload
	movq	%r15, %rcx
	movq	%r12, %r8
	movq	%rbx, %r9
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	gen_rtx_fmt_E  # TAILCALL
.Ltmp984:
	.size	gen_rep_movsi, .Ltmp984-gen_rep_movsi
	.cfi_endproc

	.globl	gen_rep_movsi_rex64
	.align	16, 0x90
	.type	gen_rep_movsi_rex64,@function
gen_rep_movsi_rex64:                    # @gen_rep_movsi_rex64
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp991:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp992:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp993:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp994:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp995:
	.cfi_def_cfa_offset 48
	subq	$32, %rsp
.Ltmp996:
	.cfi_def_cfa_offset 80
.Ltmp997:
	.cfi_offset %rbx, -48
.Ltmp998:
	.cfi_offset %r12, -40
.Ltmp999:
	.cfi_offset %r13, -32
.Ltmp1000:
	.cfi_offset %r14, -24
.Ltmp1001:
	.cfi_offset %r15, -16
	movq	const_int_rtx+512(%rip), %rax
	movq	%r9, %r14
	movq	%r8, %r12
	movq	%rcx, %r13
	movq	%rsi, %rbx
	movq	%rdi, %r15
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, 24(%rsp)          # 8-byte Spill
	xorq	%rdi, %rdi
	movq	$2, %rsi
	callq	gen_rtx_CONST_INT
	movq	$87, %rdi
	movq	$5, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$75, %rdi
	movq	$5, %rsi
	movq	%rax, %rdx
	movq	%r13, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r15, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, 16(%rsp)          # 8-byte Spill
	xorq	%rdi, %rdi
	movq	$2, %rsi
	callq	gen_rtx_CONST_INT
	movq	$87, %rdi
	movq	$5, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$75, %rdi
	movq	$5, %rsi
	movq	%rax, %rdx
	movq	%r12, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %r15
	movq	$51, %rdi
	movq	%r13, %rsi
	callq	gen_rtx_MEM
	movq	%rax, %rbx
	movq	$51, %rdi
	movq	%r12, %rsi
	callq	gen_rtx_MEM
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %r12
	movq	$48, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rbx
	movq	$4, %rdi
	movq	$19, %rsi
	callq	gen_rtx_REG
	movq	$48, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rcx
	movq	$6, %rdi
	xorq	%rax, %rax
	movq	%rcx, (%rsp)
	movq	24(%rsp), %rsi          # 8-byte Reload
	movq	16(%rsp), %rdx          # 8-byte Reload
	movq	%r15, %rcx
	movq	%r12, %r8
	movq	%rbx, %r9
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	gen_rtx_fmt_E  # TAILCALL
.Ltmp1002:
	.size	gen_rep_movsi_rex64, .Ltmp1002-gen_rep_movsi_rex64
	.cfi_endproc

	.globl	gen_rep_movqi
	.align	16, 0x90
	.type	gen_rep_movqi,@function
gen_rep_movqi:                          # @gen_rep_movqi
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp1009:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp1010:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp1011:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp1012:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp1013:
	.cfi_def_cfa_offset 48
	subq	$32, %rsp
.Ltmp1014:
	.cfi_def_cfa_offset 80
.Ltmp1015:
	.cfi_offset %rbx, -48
.Ltmp1016:
	.cfi_offset %r12, -40
.Ltmp1017:
	.cfi_offset %r13, -32
.Ltmp1018:
	.cfi_offset %r14, -24
.Ltmp1019:
	.cfi_offset %r15, -16
	movq	const_int_rtx+512(%rip), %rax
	movq	%r9, %r15
	movq	%r8, %r13
	movq	%rcx, %r14
	movq	%rsi, %r12
	movq	%rdi, %rbx
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, 24(%rsp)          # 8-byte Spill
	movq	$75, %rdi
	movq	$4, %rsi
	movq	%r14, %rdx
	movq	%r15, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, 16(%rsp)          # 8-byte Spill
	movq	$75, %rdi
	movq	$4, %rsi
	movq	%r13, %rdx
	movq	%r15, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r12, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %r12
	movq	$51, %rdi
	movq	%r14, %rsi
	callq	gen_rtx_MEM
	movq	%rax, %rbx
	movq	$51, %rdi
	movq	%r13, %rsi
	callq	gen_rtx_MEM
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %r14
	movq	$48, %rdi
	xorq	%rsi, %rsi
	movq	%r15, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rbx
	movq	$4, %rdi
	movq	$19, %rsi
	callq	gen_rtx_REG
	movq	$48, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rcx
	movq	$6, %rdi
	xorq	%rax, %rax
	movq	%rcx, (%rsp)
	movq	24(%rsp), %rsi          # 8-byte Reload
	movq	16(%rsp), %rdx          # 8-byte Reload
	movq	%r12, %rcx
	movq	%r14, %r8
	movq	%rbx, %r9
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	gen_rtx_fmt_E  # TAILCALL
.Ltmp1020:
	.size	gen_rep_movqi, .Ltmp1020-gen_rep_movqi
	.cfi_endproc

	.globl	gen_rep_movqi_rex64
	.align	16, 0x90
	.type	gen_rep_movqi_rex64,@function
gen_rep_movqi_rex64:                    # @gen_rep_movqi_rex64
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp1027:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp1028:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp1029:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp1030:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp1031:
	.cfi_def_cfa_offset 48
	subq	$32, %rsp
.Ltmp1032:
	.cfi_def_cfa_offset 80
.Ltmp1033:
	.cfi_offset %rbx, -48
.Ltmp1034:
	.cfi_offset %r12, -40
.Ltmp1035:
	.cfi_offset %r13, -32
.Ltmp1036:
	.cfi_offset %r14, -24
.Ltmp1037:
	.cfi_offset %r15, -16
	movq	const_int_rtx+512(%rip), %rax
	movq	%r9, %r15
	movq	%r8, %r13
	movq	%rcx, %r14
	movq	%rsi, %r12
	movq	%rdi, %rbx
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, 24(%rsp)          # 8-byte Spill
	movq	$75, %rdi
	movq	$5, %rsi
	movq	%r14, %rdx
	movq	%r15, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, 16(%rsp)          # 8-byte Spill
	movq	$75, %rdi
	movq	$5, %rsi
	movq	%r13, %rdx
	movq	%r15, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r12, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %r12
	movq	$51, %rdi
	movq	%r14, %rsi
	callq	gen_rtx_MEM
	movq	%rax, %rbx
	movq	$51, %rdi
	movq	%r13, %rsi
	callq	gen_rtx_MEM
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %r14
	movq	$48, %rdi
	xorq	%rsi, %rsi
	movq	%r15, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rbx
	movq	$4, %rdi
	movq	$19, %rsi
	callq	gen_rtx_REG
	movq	$48, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rcx
	movq	$6, %rdi
	xorq	%rax, %rax
	movq	%rcx, (%rsp)
	movq	24(%rsp), %rsi          # 8-byte Reload
	movq	16(%rsp), %rdx          # 8-byte Reload
	movq	%r12, %rcx
	movq	%r14, %r8
	movq	%rbx, %r9
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	gen_rtx_fmt_E  # TAILCALL
.Ltmp1038:
	.size	gen_rep_movqi_rex64, .Ltmp1038-gen_rep_movqi_rex64
	.cfi_endproc

	.globl	gen_strsetdi_rex_1
	.align	16, 0x90
	.type	gen_strsetdi_rex_1,@function
gen_strsetdi_rex_1:                     # @gen_strsetdi_rex_1
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp1043:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp1044:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp1045:
	.cfi_def_cfa_offset 32
.Ltmp1046:
	.cfi_offset %rbx, -32
.Ltmp1047:
	.cfi_offset %r14, -24
.Ltmp1048:
	.cfi_offset %r15, -16
	movq	%rdx, %r14
	movq	%rsi, %rbx
	movq	%rdi, %r15
	movq	$4, %rdi
                                        # kill: RSI<def> RBX<kill>
	callq	gen_rtx_MEM
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	movq	%r14, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %r14
	xorq	%rdi, %rdi
	movq	$8, %rsi
	callq	gen_rtx_CONST_INT
	movq	$75, %rdi
	movq	$5, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r15, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rbx
	movq	$4, %rdi
	movq	$19, %rsi
	callq	gen_rtx_REG
	movq	$48, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rcx
	movq	$3, %rdi
	xorq	%rax, %rax
	movq	%r14, %rsi
	movq	%rbx, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	popq	%rbx
	popq	%r14
	popq	%r15
	jmp	gen_rtx_fmt_E  # TAILCALL
.Ltmp1049:
	.size	gen_strsetdi_rex_1, .Ltmp1049-gen_strsetdi_rex_1
	.cfi_endproc

	.globl	gen_strsetsi_1
	.align	16, 0x90
	.type	gen_strsetsi_1,@function
gen_strsetsi_1:                         # @gen_strsetsi_1
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp1054:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp1055:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp1056:
	.cfi_def_cfa_offset 32
.Ltmp1057:
	.cfi_offset %rbx, -32
.Ltmp1058:
	.cfi_offset %r14, -24
.Ltmp1059:
	.cfi_offset %r15, -16
	movq	%rdx, %r14
	movq	%rsi, %rbx
	movq	%rdi, %r15
	movq	$4, %rdi
                                        # kill: RSI<def> RBX<kill>
	callq	gen_rtx_MEM
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	movq	%r14, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %r14
	xorq	%rdi, %rdi
	movq	$4, %rsi
	callq	gen_rtx_CONST_INT
	movq	$75, %rdi
	movq	$4, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r15, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rbx
	movq	$4, %rdi
	movq	$19, %rsi
	callq	gen_rtx_REG
	movq	$48, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rcx
	movq	$3, %rdi
	xorq	%rax, %rax
	movq	%r14, %rsi
	movq	%rbx, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	popq	%rbx
	popq	%r14
	popq	%r15
	jmp	gen_rtx_fmt_E  # TAILCALL
.Ltmp1060:
	.size	gen_strsetsi_1, .Ltmp1060-gen_strsetsi_1
	.cfi_endproc

	.globl	gen_strsetsi_rex_1
	.align	16, 0x90
	.type	gen_strsetsi_rex_1,@function
gen_strsetsi_rex_1:                     # @gen_strsetsi_rex_1
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp1065:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp1066:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp1067:
	.cfi_def_cfa_offset 32
.Ltmp1068:
	.cfi_offset %rbx, -32
.Ltmp1069:
	.cfi_offset %r14, -24
.Ltmp1070:
	.cfi_offset %r15, -16
	movq	%rdx, %r14
	movq	%rsi, %rbx
	movq	%rdi, %r15
	movq	$4, %rdi
                                        # kill: RSI<def> RBX<kill>
	callq	gen_rtx_MEM
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	movq	%r14, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %r14
	xorq	%rdi, %rdi
	movq	$4, %rsi
	callq	gen_rtx_CONST_INT
	movq	$75, %rdi
	movq	$5, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r15, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rbx
	movq	$4, %rdi
	movq	$19, %rsi
	callq	gen_rtx_REG
	movq	$48, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rcx
	movq	$3, %rdi
	xorq	%rax, %rax
	movq	%r14, %rsi
	movq	%rbx, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	popq	%rbx
	popq	%r14
	popq	%r15
	jmp	gen_rtx_fmt_E  # TAILCALL
.Ltmp1071:
	.size	gen_strsetsi_rex_1, .Ltmp1071-gen_strsetsi_rex_1
	.cfi_endproc

	.globl	gen_strsethi_1
	.align	16, 0x90
	.type	gen_strsethi_1,@function
gen_strsethi_1:                         # @gen_strsethi_1
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp1076:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp1077:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp1078:
	.cfi_def_cfa_offset 32
.Ltmp1079:
	.cfi_offset %rbx, -32
.Ltmp1080:
	.cfi_offset %r14, -24
.Ltmp1081:
	.cfi_offset %r15, -16
	movq	%rdx, %r14
	movq	%rsi, %rbx
	movq	%rdi, %r15
	movq	$3, %rdi
                                        # kill: RSI<def> RBX<kill>
	callq	gen_rtx_MEM
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	movq	%r14, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %r14
	xorq	%rdi, %rdi
	movq	$2, %rsi
	callq	gen_rtx_CONST_INT
	movq	$75, %rdi
	movq	$4, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r15, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rbx
	movq	$4, %rdi
	movq	$19, %rsi
	callq	gen_rtx_REG
	movq	$48, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rcx
	movq	$3, %rdi
	xorq	%rax, %rax
	movq	%r14, %rsi
	movq	%rbx, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	popq	%rbx
	popq	%r14
	popq	%r15
	jmp	gen_rtx_fmt_E  # TAILCALL
.Ltmp1082:
	.size	gen_strsethi_1, .Ltmp1082-gen_strsethi_1
	.cfi_endproc

	.globl	gen_strsethi_rex_1
	.align	16, 0x90
	.type	gen_strsethi_rex_1,@function
gen_strsethi_rex_1:                     # @gen_strsethi_rex_1
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp1087:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp1088:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp1089:
	.cfi_def_cfa_offset 32
.Ltmp1090:
	.cfi_offset %rbx, -32
.Ltmp1091:
	.cfi_offset %r14, -24
.Ltmp1092:
	.cfi_offset %r15, -16
	movq	%rdx, %r14
	movq	%rsi, %rbx
	movq	%rdi, %r15
	movq	$3, %rdi
                                        # kill: RSI<def> RBX<kill>
	callq	gen_rtx_MEM
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	movq	%r14, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %r14
	xorq	%rdi, %rdi
	movq	$2, %rsi
	callq	gen_rtx_CONST_INT
	movq	$75, %rdi
	movq	$5, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r15, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rbx
	movq	$4, %rdi
	movq	$19, %rsi
	callq	gen_rtx_REG
	movq	$48, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rcx
	movq	$3, %rdi
	xorq	%rax, %rax
	movq	%r14, %rsi
	movq	%rbx, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	popq	%rbx
	popq	%r14
	popq	%r15
	jmp	gen_rtx_fmt_E  # TAILCALL
.Ltmp1093:
	.size	gen_strsethi_rex_1, .Ltmp1093-gen_strsethi_rex_1
	.cfi_endproc

	.globl	gen_strsetqi_1
	.align	16, 0x90
	.type	gen_strsetqi_1,@function
gen_strsetqi_1:                         # @gen_strsetqi_1
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp1098:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp1099:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp1100:
	.cfi_def_cfa_offset 32
.Ltmp1101:
	.cfi_offset %rbx, -32
.Ltmp1102:
	.cfi_offset %r14, -24
.Ltmp1103:
	.cfi_offset %r15, -16
	movq	%rdx, %r14
	movq	%rsi, %rbx
	movq	%rdi, %r15
	movq	$2, %rdi
                                        # kill: RSI<def> RBX<kill>
	callq	gen_rtx_MEM
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	movq	%r14, %rcx
	callq	gen_rtx_fmt_ee
	movq	const_int_rtx+520(%rip), %rcx
	movq	%rax, %r14
	movq	$75, %rdi
	movq	$4, %rsi
	movq	%rbx, %rdx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r15, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rbx
	movq	$4, %rdi
	movq	$19, %rsi
	callq	gen_rtx_REG
	movq	$48, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rcx
	movq	$3, %rdi
	xorq	%rax, %rax
	movq	%r14, %rsi
	movq	%rbx, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	popq	%rbx
	popq	%r14
	popq	%r15
	jmp	gen_rtx_fmt_E  # TAILCALL
.Ltmp1104:
	.size	gen_strsetqi_1, .Ltmp1104-gen_strsetqi_1
	.cfi_endproc

	.globl	gen_strsetqi_rex_1
	.align	16, 0x90
	.type	gen_strsetqi_rex_1,@function
gen_strsetqi_rex_1:                     # @gen_strsetqi_rex_1
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp1109:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp1110:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp1111:
	.cfi_def_cfa_offset 32
.Ltmp1112:
	.cfi_offset %rbx, -32
.Ltmp1113:
	.cfi_offset %r14, -24
.Ltmp1114:
	.cfi_offset %r15, -16
	movq	%rdx, %r14
	movq	%rsi, %rbx
	movq	%rdi, %r15
	movq	$2, %rdi
                                        # kill: RSI<def> RBX<kill>
	callq	gen_rtx_MEM
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	movq	%r14, %rcx
	callq	gen_rtx_fmt_ee
	movq	const_int_rtx+520(%rip), %rcx
	movq	%rax, %r14
	movq	$75, %rdi
	movq	$5, %rsi
	movq	%rbx, %rdx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r15, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rbx
	movq	$4, %rdi
	movq	$19, %rsi
	callq	gen_rtx_REG
	movq	$48, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rcx
	movq	$3, %rdi
	xorq	%rax, %rax
	movq	%r14, %rsi
	movq	%rbx, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	popq	%rbx
	popq	%r14
	popq	%r15
	jmp	gen_rtx_fmt_E  # TAILCALL
.Ltmp1115:
	.size	gen_strsetqi_rex_1, .Ltmp1115-gen_strsetqi_rex_1
	.cfi_endproc

	.globl	gen_rep_stosdi_rex64
	.align	16, 0x90
	.type	gen_rep_stosdi_rex64,@function
gen_rep_stosdi_rex64:                   # @gen_rep_stosdi_rex64
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp1122:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp1123:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp1124:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp1125:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp1126:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp1127:
	.cfi_def_cfa_offset 64
.Ltmp1128:
	.cfi_offset %rbx, -48
.Ltmp1129:
	.cfi_offset %r12, -40
.Ltmp1130:
	.cfi_offset %r13, -32
.Ltmp1131:
	.cfi_offset %r14, -24
.Ltmp1132:
	.cfi_offset %r15, -16
	movq	const_int_rtx+512(%rip), %rax
	movq	%r8, %r13
	movq	%rcx, %rbx
	movq	%rdx, %r15
	movq	%rsi, %rcx
	movq	%rdi, %r12
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rcx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, 8(%rsp)           # 8-byte Spill
	xorq	%rdi, %rdi
	movq	$3, %rsi
	callq	gen_rtx_CONST_INT
	movq	$87, %rdi
	movq	$5, %rsi
	movq	%r13, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$75, %rdi
	movq	$5, %rsi
	movq	%rax, %rdx
	movq	%rbx, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r12, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %r12
	movq	$51, %rdi
	movq	%rbx, %rsi
	callq	gen_rtx_MEM
	movq	const_int_rtx+512(%rip), %rcx
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_ee
	movq	%rax, %r14
	movq	$48, %rdi
	xorq	%rsi, %rsi
	movq	%r15, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %r15
	movq	$48, %rdi
	xorq	%rsi, %rsi
	movq	%r13, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rbx
	movq	$4, %rdi
	movq	$19, %rsi
	callq	gen_rtx_REG
	movq	$48, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rcx
	movq	$6, %rdi
	xorq	%rax, %rax
	movq	%rcx, (%rsp)
	movq	8(%rsp), %rsi           # 8-byte Reload
	movq	%r12, %rdx
	movq	%r14, %rcx
	movq	%r15, %r8
	movq	%rbx, %r9
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	gen_rtx_fmt_E  # TAILCALL
.Ltmp1133:
	.size	gen_rep_stosdi_rex64, .Ltmp1133-gen_rep_stosdi_rex64
	.cfi_endproc

	.globl	gen_rep_stossi
	.align	16, 0x90
	.type	gen_rep_stossi,@function
gen_rep_stossi:                         # @gen_rep_stossi
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp1140:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp1141:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp1142:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp1143:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp1144:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp1145:
	.cfi_def_cfa_offset 64
.Ltmp1146:
	.cfi_offset %rbx, -48
.Ltmp1147:
	.cfi_offset %r12, -40
.Ltmp1148:
	.cfi_offset %r13, -32
.Ltmp1149:
	.cfi_offset %r14, -24
.Ltmp1150:
	.cfi_offset %r15, -16
	movq	const_int_rtx+512(%rip), %rax
	movq	%r8, %r13
	movq	%rcx, %rbx
	movq	%rdx, %r15
	movq	%rsi, %rcx
	movq	%rdi, %r12
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rcx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, 8(%rsp)           # 8-byte Spill
	xorq	%rdi, %rdi
	movq	$2, %rsi
	callq	gen_rtx_CONST_INT
	movq	$87, %rdi
	movq	$4, %rsi
	movq	%r13, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$75, %rdi
	movq	$4, %rsi
	movq	%rax, %rdx
	movq	%rbx, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r12, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %r12
	movq	$51, %rdi
	movq	%rbx, %rsi
	callq	gen_rtx_MEM
	movq	const_int_rtx+512(%rip), %rcx
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_ee
	movq	%rax, %r14
	movq	$48, %rdi
	xorq	%rsi, %rsi
	movq	%r15, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %r15
	movq	$48, %rdi
	xorq	%rsi, %rsi
	movq	%r13, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rbx
	movq	$4, %rdi
	movq	$19, %rsi
	callq	gen_rtx_REG
	movq	$48, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rcx
	movq	$6, %rdi
	xorq	%rax, %rax
	movq	%rcx, (%rsp)
	movq	8(%rsp), %rsi           # 8-byte Reload
	movq	%r12, %rdx
	movq	%r14, %rcx
	movq	%r15, %r8
	movq	%rbx, %r9
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	gen_rtx_fmt_E  # TAILCALL
.Ltmp1151:
	.size	gen_rep_stossi, .Ltmp1151-gen_rep_stossi
	.cfi_endproc

	.globl	gen_rep_stossi_rex64
	.align	16, 0x90
	.type	gen_rep_stossi_rex64,@function
gen_rep_stossi_rex64:                   # @gen_rep_stossi_rex64
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp1158:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp1159:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp1160:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp1161:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp1162:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp1163:
	.cfi_def_cfa_offset 64
.Ltmp1164:
	.cfi_offset %rbx, -48
.Ltmp1165:
	.cfi_offset %r12, -40
.Ltmp1166:
	.cfi_offset %r13, -32
.Ltmp1167:
	.cfi_offset %r14, -24
.Ltmp1168:
	.cfi_offset %r15, -16
	movq	const_int_rtx+512(%rip), %rax
	movq	%r8, %r13
	movq	%rcx, %rbx
	movq	%rdx, %r15
	movq	%rsi, %rcx
	movq	%rdi, %r12
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rcx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, 8(%rsp)           # 8-byte Spill
	xorq	%rdi, %rdi
	movq	$2, %rsi
	callq	gen_rtx_CONST_INT
	movq	$87, %rdi
	movq	$5, %rsi
	movq	%r13, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$75, %rdi
	movq	$5, %rsi
	movq	%rax, %rdx
	movq	%rbx, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r12, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %r12
	movq	$51, %rdi
	movq	%rbx, %rsi
	callq	gen_rtx_MEM
	movq	const_int_rtx+512(%rip), %rcx
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_ee
	movq	%rax, %r14
	movq	$48, %rdi
	xorq	%rsi, %rsi
	movq	%r15, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %r15
	movq	$48, %rdi
	xorq	%rsi, %rsi
	movq	%r13, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rbx
	movq	$4, %rdi
	movq	$19, %rsi
	callq	gen_rtx_REG
	movq	$48, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rcx
	movq	$6, %rdi
	xorq	%rax, %rax
	movq	%rcx, (%rsp)
	movq	8(%rsp), %rsi           # 8-byte Reload
	movq	%r12, %rdx
	movq	%r14, %rcx
	movq	%r15, %r8
	movq	%rbx, %r9
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	gen_rtx_fmt_E  # TAILCALL
.Ltmp1169:
	.size	gen_rep_stossi_rex64, .Ltmp1169-gen_rep_stossi_rex64
	.cfi_endproc

	.globl	gen_rep_stosqi
	.align	16, 0x90
	.type	gen_rep_stosqi,@function
gen_rep_stosqi:                         # @gen_rep_stosqi
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp1176:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp1177:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp1178:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp1179:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp1180:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp1181:
	.cfi_def_cfa_offset 64
.Ltmp1182:
	.cfi_offset %rbx, -48
.Ltmp1183:
	.cfi_offset %r12, -40
.Ltmp1184:
	.cfi_offset %r13, -32
.Ltmp1185:
	.cfi_offset %r14, -24
.Ltmp1186:
	.cfi_offset %r15, -16
	movq	const_int_rtx+512(%rip), %rax
	movq	%r8, %r14
	movq	%rcx, %rbx
	movq	%rdx, %r12
	movq	%rsi, %rcx
	movq	%rdi, %r15
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rcx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, 8(%rsp)           # 8-byte Spill
	movq	$75, %rdi
	movq	$4, %rsi
	movq	%rbx, %rdx
	movq	%r14, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r15, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %r15
	movq	$51, %rdi
	movq	%rbx, %rsi
	callq	gen_rtx_MEM
	movq	const_int_rtx+512(%rip), %rcx
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_ee
	movq	%rax, %r13
	movq	$48, %rdi
	xorq	%rsi, %rsi
	movq	%r12, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %r12
	movq	$48, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rbx
	movq	$4, %rdi
	movq	$19, %rsi
	callq	gen_rtx_REG
	movq	$48, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rcx
	movq	$6, %rdi
	xorq	%rax, %rax
	movq	%rcx, (%rsp)
	movq	8(%rsp), %rsi           # 8-byte Reload
	movq	%r15, %rdx
	movq	%r13, %rcx
	movq	%r12, %r8
	movq	%rbx, %r9
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	gen_rtx_fmt_E  # TAILCALL
.Ltmp1187:
	.size	gen_rep_stosqi, .Ltmp1187-gen_rep_stosqi
	.cfi_endproc

	.globl	gen_rep_stosqi_rex64
	.align	16, 0x90
	.type	gen_rep_stosqi_rex64,@function
gen_rep_stosqi_rex64:                   # @gen_rep_stosqi_rex64
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp1194:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp1195:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp1196:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp1197:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp1198:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp1199:
	.cfi_def_cfa_offset 64
.Ltmp1200:
	.cfi_offset %rbx, -48
.Ltmp1201:
	.cfi_offset %r12, -40
.Ltmp1202:
	.cfi_offset %r13, -32
.Ltmp1203:
	.cfi_offset %r14, -24
.Ltmp1204:
	.cfi_offset %r15, -16
	movq	const_int_rtx+512(%rip), %rax
	movq	%r8, %r14
	movq	%rcx, %rbx
	movq	%rdx, %r12
	movq	%rsi, %rcx
	movq	%rdi, %r15
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rcx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, 8(%rsp)           # 8-byte Spill
	movq	$75, %rdi
	movq	$5, %rsi
	movq	%rbx, %rdx
	movq	%r14, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r15, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %r15
	movq	$51, %rdi
	movq	%rbx, %rsi
	callq	gen_rtx_MEM
	movq	const_int_rtx+512(%rip), %rcx
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_ee
	movq	%rax, %r13
	movq	$48, %rdi
	xorq	%rsi, %rsi
	movq	%r12, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %r12
	movq	$48, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rbx
	movq	$5, %rdi
	movq	$19, %rsi
	callq	gen_rtx_REG
	movq	$48, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rcx
	movq	$6, %rdi
	xorq	%rax, %rax
	movq	%rcx, (%rsp)
	movq	8(%rsp), %rsi           # 8-byte Reload
	movq	%r15, %rdx
	movq	%r13, %rcx
	movq	%r12, %r8
	movq	%rbx, %r9
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	gen_rtx_fmt_E  # TAILCALL
.Ltmp1205:
	.size	gen_rep_stosqi_rex64, .Ltmp1205-gen_rep_stosqi_rex64
	.cfi_endproc

	.globl	gen_cmpstrqi_nz_1
	.align	16, 0x90
	.type	gen_cmpstrqi_nz_1,@function
gen_cmpstrqi_nz_1:                      # @gen_cmpstrqi_nz_1
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp1212:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp1213:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp1214:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp1215:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp1216:
	.cfi_def_cfa_offset 48
	subq	$48, %rsp
.Ltmp1217:
	.cfi_def_cfa_offset 96
.Ltmp1218:
	.cfi_offset %rbx, -48
.Ltmp1219:
	.cfi_offset %r12, -40
.Ltmp1220:
	.cfi_offset %r13, -32
.Ltmp1221:
	.cfi_offset %r14, -24
.Ltmp1222:
	.cfi_offset %r15, -16
	movq	96(%rsp), %r12
	movq	%r9, %r13
	movq	%r8, %rbx
	movq	%rcx, %r14
	movq	%rdx, 40(%rsp)          # 8-byte Spill
	movq	%rsi, 32(%rsp)          # 8-byte Spill
	movq	%rdi, 24(%rsp)          # 8-byte Spill
	movq	$52, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	%rax, %r15
	movq	$51, %rdi
	movq	%rbx, %rsi
	callq	gen_rtx_MEM
	movq	%rax, %rbx
	movq	$51, %rdi
	movq	%r13, %rsi
	callq	gen_rtx_MEM
	movq	$74, %rdi
	movq	$52, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r15, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, 16(%rsp)          # 8-byte Spill
	movq	$48, %rdi
	xorq	%rsi, %rsi
	movq	%r12, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %r15
	movq	$48, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %r14
	movq	$4, %rdi
	movq	$19, %rsi
	callq	gen_rtx_REG
	movq	$48, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %r12
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	24(%rsp), %rdx          # 8-byte Reload
	callq	gen_rtx_fmt_e
	movq	%rax, %rbx
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	32(%rsp), %rdx          # 8-byte Reload
	callq	gen_rtx_fmt_e
	movq	%rax, %r13
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	40(%rsp), %rdx          # 8-byte Reload
	callq	gen_rtx_fmt_e
	movq	%rax, 8(%rsp)
	movq	$7, %rdi
	xorq	%rax, %rax
	movq	%r13, (%rsp)
	movq	16(%rsp), %rsi          # 8-byte Reload
	movq	%r15, %rdx
	movq	%r14, %rcx
	movq	%r12, %r8
	movq	%rbx, %r9
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	addq	$48, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	gen_rtx_fmt_E  # TAILCALL
.Ltmp1223:
	.size	gen_cmpstrqi_nz_1, .Ltmp1223-gen_cmpstrqi_nz_1
	.cfi_endproc

	.globl	gen_cmpstrqi_nz_rex_1
	.align	16, 0x90
	.type	gen_cmpstrqi_nz_rex_1,@function
gen_cmpstrqi_nz_rex_1:                  # @gen_cmpstrqi_nz_rex_1
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp1230:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp1231:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp1232:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp1233:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp1234:
	.cfi_def_cfa_offset 48
	subq	$48, %rsp
.Ltmp1235:
	.cfi_def_cfa_offset 96
.Ltmp1236:
	.cfi_offset %rbx, -48
.Ltmp1237:
	.cfi_offset %r12, -40
.Ltmp1238:
	.cfi_offset %r13, -32
.Ltmp1239:
	.cfi_offset %r14, -24
.Ltmp1240:
	.cfi_offset %r15, -16
	movq	96(%rsp), %r12
	movq	%r9, %r13
	movq	%r8, %rbx
	movq	%rcx, %r14
	movq	%rdx, 40(%rsp)          # 8-byte Spill
	movq	%rsi, 32(%rsp)          # 8-byte Spill
	movq	%rdi, 24(%rsp)          # 8-byte Spill
	movq	$52, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	%rax, %r15
	movq	$51, %rdi
	movq	%rbx, %rsi
	callq	gen_rtx_MEM
	movq	%rax, %rbx
	movq	$51, %rdi
	movq	%r13, %rsi
	callq	gen_rtx_MEM
	movq	$74, %rdi
	movq	$52, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r15, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, 16(%rsp)          # 8-byte Spill
	movq	$48, %rdi
	xorq	%rsi, %rsi
	movq	%r12, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %r15
	movq	$48, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %r14
	movq	$4, %rdi
	movq	$19, %rsi
	callq	gen_rtx_REG
	movq	$48, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %r12
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	24(%rsp), %rdx          # 8-byte Reload
	callq	gen_rtx_fmt_e
	movq	%rax, %rbx
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	32(%rsp), %rdx          # 8-byte Reload
	callq	gen_rtx_fmt_e
	movq	%rax, %r13
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	40(%rsp), %rdx          # 8-byte Reload
	callq	gen_rtx_fmt_e
	movq	%rax, 8(%rsp)
	movq	$7, %rdi
	xorq	%rax, %rax
	movq	%r13, (%rsp)
	movq	16(%rsp), %rsi          # 8-byte Reload
	movq	%r15, %rdx
	movq	%r14, %rcx
	movq	%r12, %r8
	movq	%rbx, %r9
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	addq	$48, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	gen_rtx_fmt_E  # TAILCALL
.Ltmp1241:
	.size	gen_cmpstrqi_nz_rex_1, .Ltmp1241-gen_cmpstrqi_nz_rex_1
	.cfi_endproc

	.globl	gen_cmpstrqi_1
	.align	16, 0x90
	.type	gen_cmpstrqi_1,@function
gen_cmpstrqi_1:                         # @gen_cmpstrqi_1
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp1248:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp1249:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp1250:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp1251:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp1252:
	.cfi_def_cfa_offset 48
	subq	$64, %rsp
.Ltmp1253:
	.cfi_def_cfa_offset 112
.Ltmp1254:
	.cfi_offset %rbx, -48
.Ltmp1255:
	.cfi_offset %r12, -40
.Ltmp1256:
	.cfi_offset %r13, -32
.Ltmp1257:
	.cfi_offset %r14, -24
.Ltmp1258:
	.cfi_offset %r15, -16
	movq	112(%rsp), %r15
	movq	%r9, %r14
	movq	%r8, %r12
	movq	%rcx, 24(%rsp)          # 8-byte Spill
	movq	%rdx, 56(%rsp)          # 8-byte Spill
	movq	%rsi, 48(%rsp)          # 8-byte Spill
	movq	%rdi, 40(%rsp)          # 8-byte Spill
	movq	$52, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	const_int_rtx+512(%rip), %rcx
	movq	%rax, %r13
	movq	$102, %rdi
	xorq	%rsi, %rsi
	movq	%r15, %rdx
	callq	gen_rtx_fmt_ee
	movq	%rax, %r15
	movq	$51, %rdi
	movq	%r12, %rsi
	callq	gen_rtx_MEM
	movq	%rax, %rbx
	movq	$51, %rdi
	movq	%r14, %rsi
	callq	gen_rtx_MEM
	movq	$74, %rdi
	movq	$52, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	const_int_rtx+512(%rip), %r8
	movq	$72, %rdi
	movq	$52, %rsi
	movq	%r15, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_eee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r13, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, 32(%rsp)          # 8-byte Spill
	movq	$48, %rdi
	xorq	%rsi, %rsi
	movq	24(%rsp), %rdx          # 8-byte Reload
	callq	gen_rtx_fmt_e
	movq	%rax, %r12
	movq	$52, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	$48, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %r14
	movq	$4, %rdi
	movq	$19, %rsi
	callq	gen_rtx_REG
	movq	$48, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %r15
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	40(%rsp), %rdx          # 8-byte Reload
	callq	gen_rtx_fmt_e
	movq	%rax, %rbx
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	48(%rsp), %rdx          # 8-byte Reload
	callq	gen_rtx_fmt_e
	movq	%rax, %r13
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	56(%rsp), %rdx          # 8-byte Reload
	callq	gen_rtx_fmt_e
	movq	%rax, 8(%rsp)
	movq	$7, %rdi
	xorq	%rax, %rax
	movq	%r13, (%rsp)
	movq	32(%rsp), %rsi          # 8-byte Reload
	movq	%r12, %rdx
	movq	%r14, %rcx
	movq	%r15, %r8
	movq	%rbx, %r9
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	addq	$64, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	gen_rtx_fmt_E  # TAILCALL
.Ltmp1259:
	.size	gen_cmpstrqi_1, .Ltmp1259-gen_cmpstrqi_1
	.cfi_endproc

	.globl	gen_cmpstrqi_rex_1
	.align	16, 0x90
	.type	gen_cmpstrqi_rex_1,@function
gen_cmpstrqi_rex_1:                     # @gen_cmpstrqi_rex_1
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp1266:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp1267:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp1268:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp1269:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp1270:
	.cfi_def_cfa_offset 48
	subq	$64, %rsp
.Ltmp1271:
	.cfi_def_cfa_offset 112
.Ltmp1272:
	.cfi_offset %rbx, -48
.Ltmp1273:
	.cfi_offset %r12, -40
.Ltmp1274:
	.cfi_offset %r13, -32
.Ltmp1275:
	.cfi_offset %r14, -24
.Ltmp1276:
	.cfi_offset %r15, -16
	movq	112(%rsp), %r15
	movq	%r9, %r14
	movq	%r8, %r12
	movq	%rcx, 24(%rsp)          # 8-byte Spill
	movq	%rdx, 56(%rsp)          # 8-byte Spill
	movq	%rsi, 48(%rsp)          # 8-byte Spill
	movq	%rdi, 40(%rsp)          # 8-byte Spill
	movq	$52, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	const_int_rtx+512(%rip), %rcx
	movq	%rax, %r13
	movq	$102, %rdi
	xorq	%rsi, %rsi
	movq	%r15, %rdx
	callq	gen_rtx_fmt_ee
	movq	%rax, %r15
	movq	$51, %rdi
	movq	%r12, %rsi
	callq	gen_rtx_MEM
	movq	%rax, %rbx
	movq	$51, %rdi
	movq	%r14, %rsi
	callq	gen_rtx_MEM
	movq	$74, %rdi
	movq	$52, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	const_int_rtx+512(%rip), %r8
	movq	$72, %rdi
	movq	$52, %rsi
	movq	%r15, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_eee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r13, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, 32(%rsp)          # 8-byte Spill
	movq	$48, %rdi
	xorq	%rsi, %rsi
	movq	24(%rsp), %rdx          # 8-byte Reload
	callq	gen_rtx_fmt_e
	movq	%rax, %r12
	movq	$52, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	$48, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %r14
	movq	$4, %rdi
	movq	$19, %rsi
	callq	gen_rtx_REG
	movq	$48, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %r15
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	40(%rsp), %rdx          # 8-byte Reload
	callq	gen_rtx_fmt_e
	movq	%rax, %rbx
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	48(%rsp), %rdx          # 8-byte Reload
	callq	gen_rtx_fmt_e
	movq	%rax, %r13
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	56(%rsp), %rdx          # 8-byte Reload
	callq	gen_rtx_fmt_e
	movq	%rax, 8(%rsp)
	movq	$7, %rdi
	xorq	%rax, %rax
	movq	%r13, (%rsp)
	movq	32(%rsp), %rsi          # 8-byte Reload
	movq	%r12, %rdx
	movq	%r14, %rcx
	movq	%r15, %r8
	movq	%rbx, %r9
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	addq	$64, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	gen_rtx_fmt_E  # TAILCALL
.Ltmp1277:
	.size	gen_cmpstrqi_rex_1, .Ltmp1277-gen_cmpstrqi_rex_1
	.cfi_endproc

	.globl	gen_strlenqi_1
	.align	16, 0x90
	.type	gen_strlenqi_1,@function
gen_strlenqi_1:                         # @gen_strlenqi_1
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp1284:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp1285:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp1286:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp1287:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp1288:
	.cfi_def_cfa_offset 48
.Ltmp1289:
	.cfi_offset %rbx, -48
.Ltmp1290:
	.cfi_offset %r12, -40
.Ltmp1291:
	.cfi_offset %r13, -32
.Ltmp1292:
	.cfi_offset %r14, -24
.Ltmp1293:
	.cfi_offset %r15, -16
	movq	%r8, %r15
	movq	%rcx, %r12
	movq	%rdx, %rbx
	movq	%rsi, %r14
	movq	%rdi, %r13
	movq	$51, %rdi
	movq	%r9, %rsi
	callq	gen_rtx_MEM
	movq	%rax, %rcx
	movq	$4, %rdi
	xorq	%rax, %rax
	movq	%rcx, %rsi
	movq	%rbx, %rdx
	movq	%r12, %rcx
	movq	%r15, %r8
	callq	gen_rtvec
	movq	$42, %rdi
	movq	$4, %rsi
	xorq	%rcx, %rcx
	movq	%rax, %rdx
	callq	gen_rtx_fmt_Ei
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r13, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %r15
	movq	$4, %rdi
	movq	$19, %rsi
	callq	gen_rtx_REG
	movq	$48, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %r12
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %r14
	movq	$52, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rbx
	movq	$4, %rdi
	xorq	%rax, %rax
	movq	%r15, %rsi
	movq	%r12, %rdx
	movq	%r14, %rcx
	movq	%rbx, %r8
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	gen_rtx_fmt_E  # TAILCALL
.Ltmp1294:
	.size	gen_strlenqi_1, .Ltmp1294-gen_strlenqi_1
	.cfi_endproc

	.globl	gen_strlenqi_rex_1
	.align	16, 0x90
	.type	gen_strlenqi_rex_1,@function
gen_strlenqi_rex_1:                     # @gen_strlenqi_rex_1
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp1301:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp1302:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp1303:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp1304:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp1305:
	.cfi_def_cfa_offset 48
.Ltmp1306:
	.cfi_offset %rbx, -48
.Ltmp1307:
	.cfi_offset %r12, -40
.Ltmp1308:
	.cfi_offset %r13, -32
.Ltmp1309:
	.cfi_offset %r14, -24
.Ltmp1310:
	.cfi_offset %r15, -16
	movq	%r8, %r15
	movq	%rcx, %r12
	movq	%rdx, %rbx
	movq	%rsi, %r14
	movq	%rdi, %r13
	movq	$51, %rdi
	movq	%r9, %rsi
	callq	gen_rtx_MEM
	movq	%rax, %rcx
	movq	$4, %rdi
	xorq	%rax, %rax
	movq	%rcx, %rsi
	movq	%rbx, %rdx
	movq	%r12, %rcx
	movq	%r15, %r8
	callq	gen_rtvec
	movq	$42, %rdi
	movq	$5, %rsi
	xorq	%rcx, %rcx
	movq	%rax, %rdx
	callq	gen_rtx_fmt_Ei
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r13, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %r15
	movq	$4, %rdi
	movq	$19, %rsi
	callq	gen_rtx_REG
	movq	$48, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %r12
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %r14
	movq	$52, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rbx
	movq	$4, %rdi
	xorq	%rax, %rax
	movq	%r15, %rsi
	movq	%r12, %rdx
	movq	%r14, %rcx
	movq	%rbx, %r8
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	gen_rtx_fmt_E  # TAILCALL
.Ltmp1311:
	.size	gen_strlenqi_rex_1, .Ltmp1311-gen_strlenqi_rex_1
	.cfi_endproc

	.globl	gen_x86_movdicc_0_m1_rex64
	.align	16, 0x90
	.type	gen_x86_movdicc_0_m1_rex64,@function
gen_x86_movdicc_0_m1_rex64:             # @gen_x86_movdicc_0_m1_rex64
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp1314:
	.cfi_def_cfa_offset 16
.Ltmp1315:
	.cfi_offset %rbx, -16
	movq	%rdi, %rbx
	movq	$52, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	const_int_rtx+512(%rip), %rcx
	movq	$111, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_ee
	movq	const_int_rtx+504(%rip), %rcx
	movq	const_int_rtx+512(%rip), %r8
	movq	$72, %rdi
	movq	$5, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_eee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rbx
	movq	$52, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rcx
	movq	$2, %rdi
	xorq	%rax, %rax
	movq	%rbx, %rsi
	movq	%rcx, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	popq	%rbx
	jmp	gen_rtx_fmt_E  # TAILCALL
.Ltmp1316:
	.size	gen_x86_movdicc_0_m1_rex64, .Ltmp1316-gen_x86_movdicc_0_m1_rex64
	.cfi_endproc

	.globl	gen_x86_movsicc_0_m1
	.align	16, 0x90
	.type	gen_x86_movsicc_0_m1,@function
gen_x86_movsicc_0_m1:                   # @gen_x86_movsicc_0_m1
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp1319:
	.cfi_def_cfa_offset 16
.Ltmp1320:
	.cfi_offset %rbx, -16
	movq	%rdi, %rbx
	movq	$52, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	const_int_rtx+512(%rip), %rcx
	movq	$111, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_ee
	movq	const_int_rtx+504(%rip), %rcx
	movq	const_int_rtx+512(%rip), %r8
	movq	$72, %rdi
	movq	$4, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_eee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rbx
	movq	$52, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rcx
	movq	$2, %rdi
	xorq	%rax, %rax
	movq	%rbx, %rsi
	movq	%rcx, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	popq	%rbx
	jmp	gen_rtx_fmt_E  # TAILCALL
.Ltmp1321:
	.size	gen_x86_movsicc_0_m1, .Ltmp1321-gen_x86_movsicc_0_m1
	.cfi_endproc

	.globl	gen_pro_epilogue_adjust_stack_rex64
	.align	16, 0x90
	.type	gen_pro_epilogue_adjust_stack_rex64,@function
gen_pro_epilogue_adjust_stack_rex64:    # @gen_pro_epilogue_adjust_stack_rex64
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp1325:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp1326:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp1327:
	.cfi_def_cfa_offset 32
.Ltmp1328:
	.cfi_offset %rbx, -24
.Ltmp1329:
	.cfi_offset %r14, -16
	movq	%rdx, %rax
	movq	%rsi, %rcx
	movq	%rdi, %rbx
	movq	$75, %rdi
	movq	$5, %rsi
	movq	%rcx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %r14
	movq	$52, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rbx
	movq	$62, %rdi
	xorq	%rsi, %rsi
	callq	gen_rtx_fmt_0
	movq	$51, %rdi
	movq	%rax, %rsi
	callq	gen_rtx_MEM
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rcx
	movq	$3, %rdi
	xorq	%rax, %rax
	movq	%r14, %rsi
	movq	%rbx, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	jmp	gen_rtx_fmt_E  # TAILCALL
.Ltmp1330:
	.size	gen_pro_epilogue_adjust_stack_rex64, .Ltmp1330-gen_pro_epilogue_adjust_stack_rex64
	.cfi_endproc

	.globl	gen_sse_movsfcc
	.align	16, 0x90
	.type	gen_sse_movsfcc,@function
gen_sse_movsfcc:                        # @gen_sse_movsfcc
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp1336:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp1337:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp1338:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp1339:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp1340:
	.cfi_def_cfa_offset 48
.Ltmp1341:
	.cfi_offset %rbx, -40
.Ltmp1342:
	.cfi_offset %r12, -32
.Ltmp1343:
	.cfi_offset %r14, -24
.Ltmp1344:
	.cfi_offset %r15, -16
	movzbl	(%rsi), %ebx
	movq	%rcx, %r14
	movq	%rdx, %r15
	movq	%rdi, %r12
	xorq	%rsi, %rsi
	xorq	%rax, %rax
	movq	%rbx, %rdi
	movq	%r8, %rdx
	movq	%r9, %rcx
	callq	gen_rtx
	movq	$72, %rdi
	movq	$15, %rsi
	movq	%rax, %rdx
	movq	%r15, %rcx
	movq	%r14, %r8
	callq	gen_rtx_fmt_eee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r12, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %r14
	movq	$62, %rdi
	movq	$15, %rsi
	callq	gen_rtx_fmt_0
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rbx
	movq	$52, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rcx
	movq	$3, %rdi
	xorq	%rax, %rax
	movq	%r14, %rsi
	movq	%rbx, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	jmp	gen_rtx_fmt_E  # TAILCALL
.Ltmp1345:
	.size	gen_sse_movsfcc, .Ltmp1345-gen_sse_movsfcc
	.cfi_endproc

	.globl	gen_sse_movsfcc_eq
	.align	16, 0x90
	.type	gen_sse_movsfcc_eq,@function
gen_sse_movsfcc_eq:                     # @gen_sse_movsfcc_eq
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp1350:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp1351:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp1352:
	.cfi_def_cfa_offset 32
.Ltmp1353:
	.cfi_offset %rbx, -32
.Ltmp1354:
	.cfi_offset %r14, -24
.Ltmp1355:
	.cfi_offset %r15, -16
	movq	%rdx, %r14
	movq	%rsi, %rbx
	movq	%rdi, %r15
	movq	$103, %rdi
	xorq	%rsi, %rsi
	movq	%rcx, %rdx
	movq	%r8, %rcx
	callq	gen_rtx_fmt_ee
	movq	$72, %rdi
	movq	$15, %rsi
	movq	%rax, %rdx
	movq	%rbx, %rcx
	movq	%r14, %r8
	callq	gen_rtx_fmt_eee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r15, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %r14
	movq	$62, %rdi
	movq	$15, %rsi
	callq	gen_rtx_fmt_0
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rbx
	movq	$52, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rcx
	movq	$3, %rdi
	xorq	%rax, %rax
	movq	%r14, %rsi
	movq	%rbx, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	popq	%rbx
	popq	%r14
	popq	%r15
	jmp	gen_rtx_fmt_E  # TAILCALL
.Ltmp1356:
	.size	gen_sse_movsfcc_eq, .Ltmp1356-gen_sse_movsfcc_eq
	.cfi_endproc

	.globl	gen_sse_movdfcc
	.align	16, 0x90
	.type	gen_sse_movdfcc,@function
gen_sse_movdfcc:                        # @gen_sse_movdfcc
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp1362:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp1363:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp1364:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp1365:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp1366:
	.cfi_def_cfa_offset 48
.Ltmp1367:
	.cfi_offset %rbx, -40
.Ltmp1368:
	.cfi_offset %r12, -32
.Ltmp1369:
	.cfi_offset %r14, -24
.Ltmp1370:
	.cfi_offset %r15, -16
	movzbl	(%rsi), %ebx
	movq	%rcx, %r14
	movq	%rdx, %r15
	movq	%rdi, %r12
	xorq	%rsi, %rsi
	xorq	%rax, %rax
	movq	%rbx, %rdi
	movq	%r8, %rdx
	movq	%r9, %rcx
	callq	gen_rtx
	movq	$72, %rdi
	movq	$16, %rsi
	movq	%rax, %rdx
	movq	%r15, %rcx
	movq	%r14, %r8
	callq	gen_rtx_fmt_eee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r12, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %r14
	movq	$62, %rdi
	movq	$16, %rsi
	callq	gen_rtx_fmt_0
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rbx
	movq	$52, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rcx
	movq	$3, %rdi
	xorq	%rax, %rax
	movq	%r14, %rsi
	movq	%rbx, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	jmp	gen_rtx_fmt_E  # TAILCALL
.Ltmp1371:
	.size	gen_sse_movdfcc, .Ltmp1371-gen_sse_movdfcc
	.cfi_endproc

	.globl	gen_sse_movdfcc_eq
	.align	16, 0x90
	.type	gen_sse_movdfcc_eq,@function
gen_sse_movdfcc_eq:                     # @gen_sse_movdfcc_eq
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp1376:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp1377:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp1378:
	.cfi_def_cfa_offset 32
.Ltmp1379:
	.cfi_offset %rbx, -32
.Ltmp1380:
	.cfi_offset %r14, -24
.Ltmp1381:
	.cfi_offset %r15, -16
	movq	%rdx, %r14
	movq	%rsi, %rbx
	movq	%rdi, %r15
	movq	$103, %rdi
	xorq	%rsi, %rsi
	movq	%rcx, %rdx
	movq	%r8, %rcx
	callq	gen_rtx_fmt_ee
	movq	$72, %rdi
	movq	$16, %rsi
	movq	%rax, %rdx
	movq	%rbx, %rcx
	movq	%r14, %r8
	callq	gen_rtx_fmt_eee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r15, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %r14
	movq	$62, %rdi
	movq	$16, %rsi
	callq	gen_rtx_fmt_0
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rbx
	movq	$52, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rcx
	movq	$3, %rdi
	xorq	%rax, %rax
	movq	%r14, %rsi
	movq	%rbx, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	popq	%rbx
	popq	%r14
	popq	%r15
	jmp	gen_rtx_fmt_E  # TAILCALL
.Ltmp1382:
	.size	gen_sse_movdfcc_eq, .Ltmp1382-gen_sse_movdfcc_eq
	.cfi_endproc

	.globl	gen_allocate_stack_worker_1
	.align	16, 0x90
	.type	gen_allocate_stack_worker_1,@function
gen_allocate_stack_worker_1:            # @gen_allocate_stack_worker_1
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp1388:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp1389:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp1390:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp1391:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp1392:
	.cfi_def_cfa_offset 48
.Ltmp1393:
	.cfi_offset %rbx, -40
.Ltmp1394:
	.cfi_offset %r12, -32
.Ltmp1395:
	.cfi_offset %r14, -24
.Ltmp1396:
	.cfi_offset %r15, -16
	movq	%rdi, %rbx
	movq	$1, %rdi
	xorq	%rax, %rax
	movq	%rbx, %rsi
	callq	gen_rtvec
	movq	$42, %rdi
	movq	$4, %rsi
	movq	$3, %rcx
	movq	%rax, %rdx
	callq	gen_rtx_fmt_Ei
	movq	%rax, %r14
	movq	$4, %rdi
	movq	$7, %rsi
	callq	gen_rtx_REG
	movq	%rax, %r15
	movq	$4, %rdi
	movq	$7, %rsi
	callq	gen_rtx_REG
	movq	$76, %rdi
	movq	$4, %rsi
	movq	%rax, %rdx
	movq	%rbx, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r15, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %r15
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %r12
	movq	$52, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rbx
	movq	$4, %rdi
	xorq	%rax, %rax
	movq	%r14, %rsi
	movq	%r15, %rdx
	movq	%r12, %rcx
	movq	%rbx, %r8
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	jmp	gen_rtx_fmt_E  # TAILCALL
.Ltmp1397:
	.size	gen_allocate_stack_worker_1, .Ltmp1397-gen_allocate_stack_worker_1
	.cfi_endproc

	.globl	gen_allocate_stack_worker_rex64
	.align	16, 0x90
	.type	gen_allocate_stack_worker_rex64,@function
gen_allocate_stack_worker_rex64:        # @gen_allocate_stack_worker_rex64
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp1403:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp1404:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp1405:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp1406:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp1407:
	.cfi_def_cfa_offset 48
.Ltmp1408:
	.cfi_offset %rbx, -40
.Ltmp1409:
	.cfi_offset %r12, -32
.Ltmp1410:
	.cfi_offset %r14, -24
.Ltmp1411:
	.cfi_offset %r15, -16
	movq	%rdi, %rbx
	movq	$1, %rdi
	xorq	%rax, %rax
	movq	%rbx, %rsi
	callq	gen_rtvec
	movq	$42, %rdi
	movq	$5, %rsi
	movq	$3, %rcx
	movq	%rax, %rdx
	callq	gen_rtx_fmt_Ei
	movq	%rax, %r14
	movq	$5, %rdi
	movq	$7, %rsi
	callq	gen_rtx_REG
	movq	%rax, %r15
	movq	$5, %rdi
	movq	$7, %rsi
	callq	gen_rtx_REG
	movq	$76, %rdi
	movq	$5, %rsi
	movq	%rax, %rdx
	movq	%rbx, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r15, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %r15
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %r12
	movq	$52, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rbx
	movq	$4, %rdi
	xorq	%rax, %rax
	movq	%r14, %rsi
	movq	%r15, %rdx
	movq	%r12, %rcx
	movq	%rbx, %r8
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	jmp	gen_rtx_fmt_E  # TAILCALL
.Ltmp1412:
	.size	gen_allocate_stack_worker_rex64, .Ltmp1412-gen_allocate_stack_worker_rex64
	.cfi_endproc

	.globl	gen_trap
	.align	16, 0x90
	.type	gen_trap,@function
gen_trap:                               # @gen_trap
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp1415:
	.cfi_def_cfa_offset 16
.Ltmp1416:
	.cfi_offset %rbx, -16
	movq	const_int_rtx+520(%rip), %rbx
	xorq	%rdi, %rdi
	movq	$5, %rsi
	callq	gen_rtx_CONST_INT
	movq	$52, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	popq	%rbx
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp1417:
	.size	gen_trap, .Ltmp1417-gen_trap
	.cfi_endproc

	.globl	gen_movv4sf_internal
	.align	16, 0x90
	.type	gen_movv4sf_internal,@function
gen_movv4sf_internal:                   # @gen_movv4sf_internal
	.cfi_startproc
# BB#0:                                 # %entry
	movq	%rsi, %rax
	movq	%rdi, %rcx
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rcx, %rdx
	movq	%rax, %rcx
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp1418:
	.size	gen_movv4sf_internal, .Ltmp1418-gen_movv4sf_internal
	.cfi_endproc

	.globl	gen_movv4si_internal
	.align	16, 0x90
	.type	gen_movv4si_internal,@function
gen_movv4si_internal:                   # @gen_movv4si_internal
	.cfi_startproc
# BB#0:                                 # %entry
	movq	%rsi, %rax
	movq	%rdi, %rcx
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rcx, %rdx
	movq	%rax, %rcx
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp1419:
	.size	gen_movv4si_internal, .Ltmp1419-gen_movv4si_internal
	.cfi_endproc

	.globl	gen_movv8qi_internal
	.align	16, 0x90
	.type	gen_movv8qi_internal,@function
gen_movv8qi_internal:                   # @gen_movv8qi_internal
	.cfi_startproc
# BB#0:                                 # %entry
	movq	%rsi, %rax
	movq	%rdi, %rcx
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rcx, %rdx
	movq	%rax, %rcx
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp1420:
	.size	gen_movv8qi_internal, .Ltmp1420-gen_movv8qi_internal
	.cfi_endproc

	.globl	gen_movv4hi_internal
	.align	16, 0x90
	.type	gen_movv4hi_internal,@function
gen_movv4hi_internal:                   # @gen_movv4hi_internal
	.cfi_startproc
# BB#0:                                 # %entry
	movq	%rsi, %rax
	movq	%rdi, %rcx
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rcx, %rdx
	movq	%rax, %rcx
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp1421:
	.size	gen_movv4hi_internal, .Ltmp1421-gen_movv4hi_internal
	.cfi_endproc

	.globl	gen_movv2si_internal
	.align	16, 0x90
	.type	gen_movv2si_internal,@function
gen_movv2si_internal:                   # @gen_movv2si_internal
	.cfi_startproc
# BB#0:                                 # %entry
	movq	%rsi, %rax
	movq	%rdi, %rcx
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rcx, %rdx
	movq	%rax, %rcx
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp1422:
	.size	gen_movv2si_internal, .Ltmp1422-gen_movv2si_internal
	.cfi_endproc

	.globl	gen_movv2sf_internal
	.align	16, 0x90
	.type	gen_movv2sf_internal,@function
gen_movv2sf_internal:                   # @gen_movv2sf_internal
	.cfi_startproc
# BB#0:                                 # %entry
	movq	%rsi, %rax
	movq	%rdi, %rcx
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rcx, %rdx
	movq	%rax, %rcx
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp1423:
	.size	gen_movv2sf_internal, .Ltmp1423-gen_movv2sf_internal
	.cfi_endproc

	.globl	gen_movti_internal
	.align	16, 0x90
	.type	gen_movti_internal,@function
gen_movti_internal:                     # @gen_movti_internal
	.cfi_startproc
# BB#0:                                 # %entry
	movq	%rsi, %rax
	movq	%rdi, %rcx
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rcx, %rdx
	movq	%rax, %rcx
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp1424:
	.size	gen_movti_internal, .Ltmp1424-gen_movti_internal
	.cfi_endproc

	.globl	gen_sse_movaps
	.align	16, 0x90
	.type	gen_sse_movaps,@function
gen_sse_movaps:                         # @gen_sse_movaps
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp1427:
	.cfi_def_cfa_offset 16
.Ltmp1428:
	.cfi_offset %rbx, -16
	movq	%rdi, %rbx
	movq	$1, %rdi
	xorq	%rax, %rax
	callq	gen_rtvec
	movq	$42, %rdi
	movq	$46, %rsi
	movq	$38, %rcx
	movq	%rax, %rdx
	callq	gen_rtx_fmt_Ei
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	popq	%rbx
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp1429:
	.size	gen_sse_movaps, .Ltmp1429-gen_sse_movaps
	.cfi_endproc

	.globl	gen_sse_movups
	.align	16, 0x90
	.type	gen_sse_movups,@function
gen_sse_movups:                         # @gen_sse_movups
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp1432:
	.cfi_def_cfa_offset 16
.Ltmp1433:
	.cfi_offset %rbx, -16
	movq	%rdi, %rbx
	movq	$1, %rdi
	xorq	%rax, %rax
	callq	gen_rtvec
	movq	$42, %rdi
	movq	$46, %rsi
	movq	$39, %rcx
	movq	%rax, %rdx
	callq	gen_rtx_fmt_Ei
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	popq	%rbx
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp1434:
	.size	gen_sse_movups, .Ltmp1434-gen_sse_movups
	.cfi_endproc

	.globl	gen_sse_movmskps
	.align	16, 0x90
	.type	gen_sse_movmskps,@function
gen_sse_movmskps:                       # @gen_sse_movmskps
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp1437:
	.cfi_def_cfa_offset 16
.Ltmp1438:
	.cfi_offset %rbx, -16
	movq	%rdi, %rbx
	movq	$1, %rdi
	xorq	%rax, %rax
	callq	gen_rtvec
	movq	$42, %rdi
	movq	$4, %rsi
	movq	$33, %rcx
	movq	%rax, %rdx
	callq	gen_rtx_fmt_Ei
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	popq	%rbx
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp1439:
	.size	gen_sse_movmskps, .Ltmp1439-gen_sse_movmskps
	.cfi_endproc

	.globl	gen_mmx_pmovmskb
	.align	16, 0x90
	.type	gen_mmx_pmovmskb,@function
gen_mmx_pmovmskb:                       # @gen_mmx_pmovmskb
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp1442:
	.cfi_def_cfa_offset 16
.Ltmp1443:
	.cfi_offset %rbx, -16
	movq	%rdi, %rbx
	movq	$1, %rdi
	xorq	%rax, %rax
	callq	gen_rtvec
	movq	$42, %rdi
	movq	$4, %rsi
	movq	$33, %rcx
	movq	%rax, %rdx
	callq	gen_rtx_fmt_Ei
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	popq	%rbx
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp1444:
	.size	gen_mmx_pmovmskb, .Ltmp1444-gen_mmx_pmovmskb
	.cfi_endproc

	.globl	gen_mmx_maskmovq
	.align	16, 0x90
	.type	gen_mmx_maskmovq,@function
gen_mmx_maskmovq:                       # @gen_mmx_maskmovq
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp1449:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp1450:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp1451:
	.cfi_def_cfa_offset 32
.Ltmp1452:
	.cfi_offset %rbx, -32
.Ltmp1453:
	.cfi_offset %r14, -24
.Ltmp1454:
	.cfi_offset %r15, -16
	movq	%rdx, %r14
	movq	%rsi, %rbx
	movq	%rdi, %rax
	movq	$39, %rdi
	movq	%rax, %rsi
	callq	gen_rtx_MEM
	movq	%rax, %r15
	movq	$2, %rdi
	xorq	%rax, %rax
	movq	%rbx, %rsi
	movq	%r14, %rdx
	callq	gen_rtvec
	movq	$42, %rdi
	movq	$39, %rsi
	movq	$32, %rcx
	movq	%rax, %rdx
	callq	gen_rtx_fmt_Ei
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r15, %rdx
	movq	%rax, %rcx
	popq	%rbx
	popq	%r14
	popq	%r15
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp1455:
	.size	gen_mmx_maskmovq, .Ltmp1455-gen_mmx_maskmovq
	.cfi_endproc

	.globl	gen_mmx_maskmovq_rex
	.align	16, 0x90
	.type	gen_mmx_maskmovq_rex,@function
gen_mmx_maskmovq_rex:                   # @gen_mmx_maskmovq_rex
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp1460:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp1461:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp1462:
	.cfi_def_cfa_offset 32
.Ltmp1463:
	.cfi_offset %rbx, -32
.Ltmp1464:
	.cfi_offset %r14, -24
.Ltmp1465:
	.cfi_offset %r15, -16
	movq	%rdx, %r14
	movq	%rsi, %rbx
	movq	%rdi, %rax
	movq	$39, %rdi
	movq	%rax, %rsi
	callq	gen_rtx_MEM
	movq	%rax, %r15
	movq	$2, %rdi
	xorq	%rax, %rax
	movq	%rbx, %rsi
	movq	%r14, %rdx
	callq	gen_rtvec
	movq	$42, %rdi
	movq	$39, %rsi
	movq	$32, %rcx
	movq	%rax, %rdx
	callq	gen_rtx_fmt_Ei
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r15, %rdx
	movq	%rax, %rcx
	popq	%rbx
	popq	%r14
	popq	%r15
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp1466:
	.size	gen_mmx_maskmovq_rex, .Ltmp1466-gen_mmx_maskmovq_rex
	.cfi_endproc

	.globl	gen_sse_movntv4sf
	.align	16, 0x90
	.type	gen_sse_movntv4sf,@function
gen_sse_movntv4sf:                      # @gen_sse_movntv4sf
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp1469:
	.cfi_def_cfa_offset 16
.Ltmp1470:
	.cfi_offset %rbx, -16
	movq	%rdi, %rbx
	movq	$1, %rdi
	xorq	%rax, %rax
	callq	gen_rtvec
	movq	$42, %rdi
	movq	$46, %rsi
	movq	$34, %rcx
	movq	%rax, %rdx
	callq	gen_rtx_fmt_Ei
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	popq	%rbx
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp1471:
	.size	gen_sse_movntv4sf, .Ltmp1471-gen_sse_movntv4sf
	.cfi_endproc

	.globl	gen_sse_movntdi
	.align	16, 0x90
	.type	gen_sse_movntdi,@function
gen_sse_movntdi:                        # @gen_sse_movntdi
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp1474:
	.cfi_def_cfa_offset 16
.Ltmp1475:
	.cfi_offset %rbx, -16
	movq	%rdi, %rbx
	movq	$1, %rdi
	xorq	%rax, %rax
	callq	gen_rtvec
	movq	$42, %rdi
	movq	$5, %rsi
	movq	$34, %rcx
	movq	%rax, %rdx
	callq	gen_rtx_fmt_Ei
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	popq	%rbx
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp1476:
	.size	gen_sse_movntdi, .Ltmp1476-gen_sse_movntdi
	.cfi_endproc

	.globl	gen_sse_movhlps
	.align	16, 0x90
	.type	gen_sse_movhlps,@function
gen_sse_movhlps:                        # @gen_sse_movhlps
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp1482:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp1483:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp1484:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp1485:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp1486:
	.cfi_def_cfa_offset 48
.Ltmp1487:
	.cfi_offset %rbx, -40
.Ltmp1488:
	.cfi_offset %r12, -32
.Ltmp1489:
	.cfi_offset %r14, -24
.Ltmp1490:
	.cfi_offset %r15, -16
	movq	%rdx, %r12
	movq	%rsi, %r15
	movq	%rdi, %r14
	xorq	%rdi, %rdi
	movq	$2, %rsi
	callq	gen_rtx_CONST_INT
	movq	%rax, %rbx
	xorq	%rdi, %rdi
	movq	$3, %rsi
	callq	gen_rtx_CONST_INT
	movq	const_int_rtx+512(%rip), %rcx
	movq	const_int_rtx+520(%rip), %r8
	movq	%rax, %rdx
	movq	$4, %rdi
	xorq	%rax, %rax
	movq	%rbx, %rsi
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_E
	movq	$143, %rdi
	movq	$46, %rsi
	movq	%r12, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rbx
	xorq	%rdi, %rdi
	movq	$3, %rsi
	callq	gen_rtx_CONST_INT
	movq	$142, %rdi
	movq	$46, %rsi
	movq	%r15, %rdx
	movq	%rbx, %rcx
	movq	%rax, %r8
	callq	gen_rtx_fmt_eee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp1491:
	.size	gen_sse_movhlps, .Ltmp1491-gen_sse_movhlps
	.cfi_endproc

	.globl	gen_sse_movlhps
	.align	16, 0x90
	.type	gen_sse_movlhps,@function
gen_sse_movlhps:                        # @gen_sse_movlhps
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp1497:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp1498:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp1499:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp1500:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp1501:
	.cfi_def_cfa_offset 48
.Ltmp1502:
	.cfi_offset %rbx, -40
.Ltmp1503:
	.cfi_offset %r12, -32
.Ltmp1504:
	.cfi_offset %r14, -24
.Ltmp1505:
	.cfi_offset %r15, -16
	movq	%rdx, %r12
	movq	%rsi, %r15
	movq	%rdi, %r14
	xorq	%rdi, %rdi
	movq	$2, %rsi
	callq	gen_rtx_CONST_INT
	movq	%rax, %rbx
	xorq	%rdi, %rdi
	movq	$3, %rsi
	callq	gen_rtx_CONST_INT
	movq	const_int_rtx+512(%rip), %rcx
	movq	const_int_rtx+520(%rip), %r8
	movq	%rax, %rdx
	movq	$4, %rdi
	xorq	%rax, %rax
	movq	%rbx, %rsi
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_E
	movq	$143, %rdi
	movq	$46, %rsi
	movq	%r12, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rbx
	xorq	%rdi, %rdi
	movq	$12, %rsi
	callq	gen_rtx_CONST_INT
	movq	$142, %rdi
	movq	$46, %rsi
	movq	%r15, %rdx
	movq	%rbx, %rcx
	movq	%rax, %r8
	callq	gen_rtx_fmt_eee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp1506:
	.size	gen_sse_movlhps, .Ltmp1506-gen_sse_movlhps
	.cfi_endproc

	.globl	gen_sse_movhps
	.align	16, 0x90
	.type	gen_sse_movhps,@function
gen_sse_movhps:                         # @gen_sse_movhps
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp1511:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp1512:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp1513:
	.cfi_def_cfa_offset 32
.Ltmp1514:
	.cfi_offset %rbx, -32
.Ltmp1515:
	.cfi_offset %r14, -24
.Ltmp1516:
	.cfi_offset %r15, -16
	movq	%rdx, %r14
	movq	%rsi, %rbx
	movq	%rdi, %r15
	xorq	%rdi, %rdi
	movq	$12, %rsi
	callq	gen_rtx_CONST_INT
	movq	$142, %rdi
	movq	$46, %rsi
	movq	%rbx, %rdx
	movq	%r14, %rcx
	movq	%rax, %r8
	callq	gen_rtx_fmt_eee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r15, %rdx
	movq	%rax, %rcx
	popq	%rbx
	popq	%r14
	popq	%r15
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp1517:
	.size	gen_sse_movhps, .Ltmp1517-gen_sse_movhps
	.cfi_endproc

	.globl	gen_sse_movlps
	.align	16, 0x90
	.type	gen_sse_movlps,@function
gen_sse_movlps:                         # @gen_sse_movlps
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp1522:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp1523:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp1524:
	.cfi_def_cfa_offset 32
.Ltmp1525:
	.cfi_offset %rbx, -32
.Ltmp1526:
	.cfi_offset %r14, -24
.Ltmp1527:
	.cfi_offset %r15, -16
	movq	%rdx, %r14
	movq	%rsi, %rbx
	movq	%rdi, %r15
	xorq	%rdi, %rdi
	movq	$3, %rsi
	callq	gen_rtx_CONST_INT
	movq	$142, %rdi
	movq	$46, %rsi
	movq	%rbx, %rdx
	movq	%r14, %rcx
	movq	%rax, %r8
	callq	gen_rtx_fmt_eee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r15, %rdx
	movq	%rax, %rcx
	popq	%rbx
	popq	%r14
	popq	%r15
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp1528:
	.size	gen_sse_movlps, .Ltmp1528-gen_sse_movlps
	.cfi_endproc

	.globl	gen_sse_loadss
	.align	16, 0x90
	.type	gen_sse_loadss,@function
gen_sse_loadss:                         # @gen_sse_loadss
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp1532:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp1533:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp1534:
	.cfi_def_cfa_offset 32
.Ltmp1535:
	.cfi_offset %rbx, -24
.Ltmp1536:
	.cfi_offset %r14, -16
	movq	const_int_rtx+512(%rip), %rdx
	movq	%rsi, %rbx
	movq	%rdi, %r14
	movq	$125, %rdi
	movq	$15, %rsi
	callq	gen_rtx_fmt_e
	movq	$145, %rdi
	movq	$46, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	const_int_rtx+520(%rip), %r8
	movq	$142, %rdi
	movq	$46, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_eee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp1537:
	.size	gen_sse_loadss, .Ltmp1537-gen_sse_loadss
	.cfi_endproc

	.globl	gen_sse_movss
	.align	16, 0x90
	.type	gen_sse_movss,@function
gen_sse_movss:                          # @gen_sse_movss
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp1540:
	.cfi_def_cfa_offset 16
.Ltmp1541:
	.cfi_offset %rbx, -16
	movq	const_int_rtx+520(%rip), %r8
	movq	%rdx, %rax
	movq	%rsi, %rcx
	movq	%rdi, %rbx
	movq	$142, %rdi
	movq	$46, %rsi
	movq	%rcx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_eee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	popq	%rbx
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp1542:
	.size	gen_sse_movss, .Ltmp1542-gen_sse_movss
	.cfi_endproc

	.globl	gen_sse_storess
	.align	16, 0x90
	.type	gen_sse_storess,@function
gen_sse_storess:                        # @gen_sse_storess
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp1546:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp1547:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp1548:
	.cfi_def_cfa_offset 32
.Ltmp1549:
	.cfi_offset %rbx, -24
.Ltmp1550:
	.cfi_offset %r14, -16
	movq	const_int_rtx+512(%rip), %rcx
	movq	%rsi, %rbx
	movq	%rdi, %r14
	movq	$1, %rdi
	xorq	%rax, %rax
	movq	%rcx, %rsi
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_E
	movq	$143, %rdi
	movq	$15, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp1551:
	.size	gen_sse_storess, .Ltmp1551-gen_sse_storess
	.cfi_endproc

	.globl	gen_sse_shufps
	.align	16, 0x90
	.type	gen_sse_shufps,@function
gen_sse_shufps:                         # @gen_sse_shufps
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp1554:
	.cfi_def_cfa_offset 16
.Ltmp1555:
	.cfi_offset %rbx, -16
	movq	%rdi, %rbx
	movq	$3, %rdi
	xorq	%rax, %rax
	callq	gen_rtvec
	movq	$42, %rdi
	movq	$46, %rsi
	movq	$41, %rcx
	movq	%rax, %rdx
	callq	gen_rtx_fmt_Ei
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	popq	%rbx
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp1556:
	.size	gen_sse_shufps, .Ltmp1556-gen_sse_shufps
	.cfi_endproc

	.globl	gen_addv4sf3
	.align	16, 0x90
	.type	gen_addv4sf3,@function
gen_addv4sf3:                           # @gen_addv4sf3
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp1559:
	.cfi_def_cfa_offset 16
.Ltmp1560:
	.cfi_offset %rbx, -16
	movq	%rdx, %rax
	movq	%rsi, %rcx
	movq	%rdi, %rbx
	movq	$75, %rdi
	movq	$46, %rsi
	movq	%rcx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	popq	%rbx
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp1561:
	.size	gen_addv4sf3, .Ltmp1561-gen_addv4sf3
	.cfi_endproc

	.globl	gen_vmaddv4sf3
	.align	16, 0x90
	.type	gen_vmaddv4sf3,@function
gen_vmaddv4sf3:                         # @gen_vmaddv4sf3
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp1565:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp1566:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp1567:
	.cfi_def_cfa_offset 32
.Ltmp1568:
	.cfi_offset %rbx, -24
.Ltmp1569:
	.cfi_offset %r14, -16
	movq	%rdx, %rax
	movq	%rsi, %rbx
	movq	%rdi, %r14
	movq	$75, %rdi
	movq	$46, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	const_int_rtx+520(%rip), %r8
	movq	$142, %rdi
	movq	$46, %rsi
	movq	%rax, %rdx
	movq	%rbx, %rcx
	callq	gen_rtx_fmt_eee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp1570:
	.size	gen_vmaddv4sf3, .Ltmp1570-gen_vmaddv4sf3
	.cfi_endproc

	.globl	gen_subv4sf3
	.align	16, 0x90
	.type	gen_subv4sf3,@function
gen_subv4sf3:                           # @gen_subv4sf3
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp1573:
	.cfi_def_cfa_offset 16
.Ltmp1574:
	.cfi_offset %rbx, -16
	movq	%rdx, %rax
	movq	%rsi, %rcx
	movq	%rdi, %rbx
	movq	$76, %rdi
	movq	$46, %rsi
	movq	%rcx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	popq	%rbx
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp1575:
	.size	gen_subv4sf3, .Ltmp1575-gen_subv4sf3
	.cfi_endproc

	.globl	gen_vmsubv4sf3
	.align	16, 0x90
	.type	gen_vmsubv4sf3,@function
gen_vmsubv4sf3:                         # @gen_vmsubv4sf3
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp1579:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp1580:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp1581:
	.cfi_def_cfa_offset 32
.Ltmp1582:
	.cfi_offset %rbx, -24
.Ltmp1583:
	.cfi_offset %r14, -16
	movq	%rdx, %rax
	movq	%rsi, %rbx
	movq	%rdi, %r14
	movq	$76, %rdi
	movq	$46, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	const_int_rtx+520(%rip), %r8
	movq	$142, %rdi
	movq	$46, %rsi
	movq	%rax, %rdx
	movq	%rbx, %rcx
	callq	gen_rtx_fmt_eee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp1584:
	.size	gen_vmsubv4sf3, .Ltmp1584-gen_vmsubv4sf3
	.cfi_endproc

	.globl	gen_mulv4sf3
	.align	16, 0x90
	.type	gen_mulv4sf3,@function
gen_mulv4sf3:                           # @gen_mulv4sf3
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp1587:
	.cfi_def_cfa_offset 16
.Ltmp1588:
	.cfi_offset %rbx, -16
	movq	%rdx, %rax
	movq	%rsi, %rcx
	movq	%rdi, %rbx
	movq	$78, %rdi
	movq	$46, %rsi
	movq	%rcx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	popq	%rbx
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp1589:
	.size	gen_mulv4sf3, .Ltmp1589-gen_mulv4sf3
	.cfi_endproc

	.globl	gen_vmmulv4sf3
	.align	16, 0x90
	.type	gen_vmmulv4sf3,@function
gen_vmmulv4sf3:                         # @gen_vmmulv4sf3
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp1593:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp1594:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp1595:
	.cfi_def_cfa_offset 32
.Ltmp1596:
	.cfi_offset %rbx, -24
.Ltmp1597:
	.cfi_offset %r14, -16
	movq	%rdx, %rax
	movq	%rsi, %rbx
	movq	%rdi, %r14
	movq	$78, %rdi
	movq	$46, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	const_int_rtx+520(%rip), %r8
	movq	$142, %rdi
	movq	$46, %rsi
	movq	%rax, %rdx
	movq	%rbx, %rcx
	callq	gen_rtx_fmt_eee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp1598:
	.size	gen_vmmulv4sf3, .Ltmp1598-gen_vmmulv4sf3
	.cfi_endproc

	.globl	gen_divv4sf3
	.align	16, 0x90
	.type	gen_divv4sf3,@function
gen_divv4sf3:                           # @gen_divv4sf3
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp1601:
	.cfi_def_cfa_offset 16
.Ltmp1602:
	.cfi_offset %rbx, -16
	movq	%rdx, %rax
	movq	%rsi, %rcx
	movq	%rdi, %rbx
	movq	$79, %rdi
	movq	$46, %rsi
	movq	%rcx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	popq	%rbx
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp1603:
	.size	gen_divv4sf3, .Ltmp1603-gen_divv4sf3
	.cfi_endproc

	.globl	gen_vmdivv4sf3
	.align	16, 0x90
	.type	gen_vmdivv4sf3,@function
gen_vmdivv4sf3:                         # @gen_vmdivv4sf3
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp1607:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp1608:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp1609:
	.cfi_def_cfa_offset 32
.Ltmp1610:
	.cfi_offset %rbx, -24
.Ltmp1611:
	.cfi_offset %r14, -16
	movq	%rdx, %rax
	movq	%rsi, %rbx
	movq	%rdi, %r14
	movq	$79, %rdi
	movq	$46, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	const_int_rtx+520(%rip), %r8
	movq	$142, %rdi
	movq	$46, %rsi
	movq	%rax, %rdx
	movq	%rbx, %rcx
	callq	gen_rtx_fmt_eee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp1612:
	.size	gen_vmdivv4sf3, .Ltmp1612-gen_vmdivv4sf3
	.cfi_endproc

	.globl	gen_rcpv4sf2
	.align	16, 0x90
	.type	gen_rcpv4sf2,@function
gen_rcpv4sf2:                           # @gen_rcpv4sf2
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp1615:
	.cfi_def_cfa_offset 16
.Ltmp1616:
	.cfi_offset %rbx, -16
	movq	%rdi, %rbx
	movq	$1, %rdi
	xorq	%rax, %rax
	callq	gen_rtvec
	movq	$42, %rdi
	movq	$46, %rsi
	movq	$42, %rcx
	movq	%rax, %rdx
	callq	gen_rtx_fmt_Ei
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	popq	%rbx
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp1617:
	.size	gen_rcpv4sf2, .Ltmp1617-gen_rcpv4sf2
	.cfi_endproc

	.globl	gen_vmrcpv4sf2
	.align	16, 0x90
	.type	gen_vmrcpv4sf2,@function
gen_vmrcpv4sf2:                         # @gen_vmrcpv4sf2
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp1621:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp1622:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp1623:
	.cfi_def_cfa_offset 32
.Ltmp1624:
	.cfi_offset %rbx, -24
.Ltmp1625:
	.cfi_offset %r14, -16
	movq	%rdx, %rbx
	movq	%rdi, %r14
	movq	$1, %rdi
	xorq	%rax, %rax
	callq	gen_rtvec
	movq	$42, %rdi
	movq	$46, %rsi
	movq	$42, %rcx
	movq	%rax, %rdx
	callq	gen_rtx_fmt_Ei
	movq	const_int_rtx+520(%rip), %r8
	movq	$142, %rdi
	movq	$46, %rsi
	movq	%rax, %rdx
	movq	%rbx, %rcx
	callq	gen_rtx_fmt_eee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp1626:
	.size	gen_vmrcpv4sf2, .Ltmp1626-gen_vmrcpv4sf2
	.cfi_endproc

	.globl	gen_rsqrtv4sf2
	.align	16, 0x90
	.type	gen_rsqrtv4sf2,@function
gen_rsqrtv4sf2:                         # @gen_rsqrtv4sf2
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp1629:
	.cfi_def_cfa_offset 16
.Ltmp1630:
	.cfi_offset %rbx, -16
	movq	%rdi, %rbx
	movq	$1, %rdi
	xorq	%rax, %rax
	callq	gen_rtvec
	movq	$42, %rdi
	movq	$46, %rsi
	movq	$43, %rcx
	movq	%rax, %rdx
	callq	gen_rtx_fmt_Ei
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	popq	%rbx
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp1631:
	.size	gen_rsqrtv4sf2, .Ltmp1631-gen_rsqrtv4sf2
	.cfi_endproc

	.globl	gen_vmrsqrtv4sf2
	.align	16, 0x90
	.type	gen_vmrsqrtv4sf2,@function
gen_vmrsqrtv4sf2:                       # @gen_vmrsqrtv4sf2
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp1635:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp1636:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp1637:
	.cfi_def_cfa_offset 32
.Ltmp1638:
	.cfi_offset %rbx, -24
.Ltmp1639:
	.cfi_offset %r14, -16
	movq	%rdx, %rbx
	movq	%rdi, %r14
	movq	$1, %rdi
	xorq	%rax, %rax
	callq	gen_rtvec
	movq	$42, %rdi
	movq	$46, %rsi
	movq	$43, %rcx
	movq	%rax, %rdx
	callq	gen_rtx_fmt_Ei
	movq	const_int_rtx+520(%rip), %r8
	movq	$142, %rdi
	movq	$46, %rsi
	movq	%rax, %rdx
	movq	%rbx, %rcx
	callq	gen_rtx_fmt_eee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp1640:
	.size	gen_vmrsqrtv4sf2, .Ltmp1640-gen_vmrsqrtv4sf2
	.cfi_endproc

	.globl	gen_sqrtv4sf2
	.align	16, 0x90
	.type	gen_sqrtv4sf2,@function
gen_sqrtv4sf2:                          # @gen_sqrtv4sf2
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp1643:
	.cfi_def_cfa_offset 16
.Ltmp1644:
	.cfi_offset %rbx, -16
	movq	%rsi, %rax
	movq	%rdi, %rbx
	movq	$130, %rdi
	movq	$46, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	popq	%rbx
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp1645:
	.size	gen_sqrtv4sf2, .Ltmp1645-gen_sqrtv4sf2
	.cfi_endproc

	.globl	gen_vmsqrtv4sf2
	.align	16, 0x90
	.type	gen_vmsqrtv4sf2,@function
gen_vmsqrtv4sf2:                        # @gen_vmsqrtv4sf2
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp1649:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp1650:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp1651:
	.cfi_def_cfa_offset 32
.Ltmp1652:
	.cfi_offset %rbx, -24
.Ltmp1653:
	.cfi_offset %r14, -16
	movq	%rdx, %rbx
	movq	%rsi, %rax
	movq	%rdi, %r14
	movq	$130, %rdi
	movq	$46, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	const_int_rtx+520(%rip), %r8
	movq	$142, %rdi
	movq	$46, %rsi
	movq	%rax, %rdx
	movq	%rbx, %rcx
	callq	gen_rtx_fmt_eee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp1654:
	.size	gen_vmsqrtv4sf2, .Ltmp1654-gen_vmsqrtv4sf2
	.cfi_endproc

	.globl	gen_sse_andti3
	.align	16, 0x90
	.type	gen_sse_andti3,@function
gen_sse_andti3:                         # @gen_sse_andti3
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp1657:
	.cfi_def_cfa_offset 16
.Ltmp1658:
	.cfi_offset %rbx, -16
	movq	%rdx, %rax
	movq	%rsi, %rcx
	movq	%rdi, %rbx
	movq	$83, %rdi
	movq	$6, %rsi
	movq	%rcx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	popq	%rbx
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp1659:
	.size	gen_sse_andti3, .Ltmp1659-gen_sse_andti3
	.cfi_endproc

	.globl	gen_sse_nandti3
	.align	16, 0x90
	.type	gen_sse_nandti3,@function
gen_sse_nandti3:                        # @gen_sse_nandti3
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp1663:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp1664:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp1665:
	.cfi_def_cfa_offset 32
.Ltmp1666:
	.cfi_offset %rbx, -24
.Ltmp1667:
	.cfi_offset %r14, -16
	movq	%rdx, %rbx
	movq	%rsi, %rax
	movq	%rdi, %r14
	movq	$86, %rdi
	movq	$6, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	$83, %rdi
	movq	$6, %rsi
	movq	%rax, %rdx
	movq	%rbx, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp1668:
	.size	gen_sse_nandti3, .Ltmp1668-gen_sse_nandti3
	.cfi_endproc

	.globl	gen_sse_iorti3
	.align	16, 0x90
	.type	gen_sse_iorti3,@function
gen_sse_iorti3:                         # @gen_sse_iorti3
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp1671:
	.cfi_def_cfa_offset 16
.Ltmp1672:
	.cfi_offset %rbx, -16
	movq	%rdx, %rax
	movq	%rsi, %rcx
	movq	%rdi, %rbx
	movq	$84, %rdi
	movq	$6, %rsi
	movq	%rcx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	popq	%rbx
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp1673:
	.size	gen_sse_iorti3, .Ltmp1673-gen_sse_iorti3
	.cfi_endproc

	.globl	gen_sse_xorti3
	.align	16, 0x90
	.type	gen_sse_xorti3,@function
gen_sse_xorti3:                         # @gen_sse_xorti3
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp1676:
	.cfi_def_cfa_offset 16
.Ltmp1677:
	.cfi_offset %rbx, -16
	movq	%rdx, %rax
	movq	%rsi, %rcx
	movq	%rdi, %rbx
	movq	$85, %rdi
	movq	$6, %rsi
	movq	%rcx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	popq	%rbx
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp1678:
	.size	gen_sse_xorti3, .Ltmp1678-gen_sse_xorti3
	.cfi_endproc

	.globl	gen_sse_clrv4sf
	.align	16, 0x90
	.type	gen_sse_clrv4sf,@function
gen_sse_clrv4sf:                        # @gen_sse_clrv4sf
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp1681:
	.cfi_def_cfa_offset 16
.Ltmp1682:
	.cfi_offset %rbx, -16
	movq	const_int_rtx+512(%rip), %rsi
	movq	%rdi, %rbx
	movq	$1, %rdi
	xorq	%rax, %rax
	callq	gen_rtvec
	movq	$42, %rdi
	movq	$46, %rsi
	movq	$45, %rcx
	movq	%rax, %rdx
	callq	gen_rtx_fmt_Ei
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	popq	%rbx
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp1683:
	.size	gen_sse_clrv4sf, .Ltmp1683-gen_sse_clrv4sf
	.cfi_endproc

	.globl	gen_maskcmpv4sf3
	.align	16, 0x90
	.type	gen_maskcmpv4sf3,@function
gen_maskcmpv4sf3:                       # @gen_maskcmpv4sf3
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp1686:
	.cfi_def_cfa_offset 16
.Ltmp1687:
	.cfi_offset %rbx, -16
	movzbl	(%rcx), %ecx
	movq	%rdx, %r8
	movq	%rsi, %rdx
	movq	%rdi, %rbx
	movq	$37, %rsi
	xorq	%rax, %rax
	movq	%rcx, %rdi
	movq	%r8, %rcx
	callq	gen_rtx
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	popq	%rbx
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp1688:
	.size	gen_maskcmpv4sf3, .Ltmp1688-gen_maskcmpv4sf3
	.cfi_endproc

	.globl	gen_maskncmpv4sf3
	.align	16, 0x90
	.type	gen_maskncmpv4sf3,@function
gen_maskncmpv4sf3:                      # @gen_maskncmpv4sf3
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp1691:
	.cfi_def_cfa_offset 16
.Ltmp1692:
	.cfi_offset %rbx, -16
	movzbl	(%rcx), %ecx
	movq	%rdx, %r8
	movq	%rsi, %rdx
	movq	%rdi, %rbx
	movq	$37, %rsi
	xorq	%rax, %rax
	movq	%rcx, %rdi
	movq	%r8, %rcx
	callq	gen_rtx
	movq	$86, %rdi
	movq	$37, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	popq	%rbx
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp1693:
	.size	gen_maskncmpv4sf3, .Ltmp1693-gen_maskncmpv4sf3
	.cfi_endproc

	.globl	gen_vmmaskcmpv4sf3
	.align	16, 0x90
	.type	gen_vmmaskcmpv4sf3,@function
gen_vmmaskcmpv4sf3:                     # @gen_vmmaskcmpv4sf3
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp1697:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp1698:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp1699:
	.cfi_def_cfa_offset 32
.Ltmp1700:
	.cfi_offset %rbx, -24
.Ltmp1701:
	.cfi_offset %r14, -16
	movzbl	(%rcx), %ecx
	movq	%rdx, %r8
	movq	%rsi, %rbx
	movq	%rdi, %r14
	movq	$37, %rsi
	xorq	%rax, %rax
	movq	%rcx, %rdi
	movq	%rbx, %rdx
	movq	%r8, %rcx
	callq	gen_rtx
	movq	const_int_rtx+520(%rip), %r8
	movq	$142, %rdi
	movq	$37, %rsi
	movq	%rax, %rdx
	movq	%rbx, %rcx
	callq	gen_rtx_fmt_eee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp1702:
	.size	gen_vmmaskcmpv4sf3, .Ltmp1702-gen_vmmaskcmpv4sf3
	.cfi_endproc

	.globl	gen_vmmaskncmpv4sf3
	.align	16, 0x90
	.type	gen_vmmaskncmpv4sf3,@function
gen_vmmaskncmpv4sf3:                    # @gen_vmmaskncmpv4sf3
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp1707:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp1708:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp1709:
	.cfi_def_cfa_offset 32
.Ltmp1710:
	.cfi_offset %rbx, -32
.Ltmp1711:
	.cfi_offset %r14, -24
.Ltmp1712:
	.cfi_offset %r15, -16
	movzbl	(%rcx), %ecx
	movq	%rdx, %r8
	movq	%rsi, %rbx
	movq	%rdi, %r14
	movq	$37, %rsi
	xorq	%rax, %rax
	movq	%rcx, %rdi
	movq	%rbx, %rdx
	movq	%r8, %rcx
	callq	gen_rtx
	movq	$86, %rdi
	movq	$37, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %r15
	movq	$37, %rdi
	xorq	%rdx, %rdx
	movq	%rbx, %rsi
	callq	gen_rtx_SUBREG
	movq	const_int_rtx+520(%rip), %r8
	movq	$142, %rdi
	movq	$37, %rsi
	movq	%r15, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_eee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	popq	%rbx
	popq	%r14
	popq	%r15
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp1713:
	.size	gen_vmmaskncmpv4sf3, .Ltmp1713-gen_vmmaskncmpv4sf3
	.cfi_endproc

	.globl	gen_sse_comi
	.align	16, 0x90
	.type	gen_sse_comi,@function
gen_sse_comi:                           # @gen_sse_comi
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp1720:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp1721:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp1722:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp1723:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp1724:
	.cfi_def_cfa_offset 48
.Ltmp1725:
	.cfi_offset %rbx, -48
.Ltmp1726:
	.cfi_offset %r12, -40
.Ltmp1727:
	.cfi_offset %r13, -32
.Ltmp1728:
	.cfi_offset %r14, -24
.Ltmp1729:
	.cfi_offset %r15, -16
	movq	%rdx, %rbx
	movq	%rsi, %r15
	movq	%rdi, %r12
	movq	$57, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	const_int_rtx+512(%rip), %rsi
	movzbl	(%rbx), %r13d
	movq	%rax, %r14
	movq	$1, %rdi
	xorq	%rax, %rax
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_E
	movq	$143, %rdi
	movq	$15, %rsi
	movq	%r12, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	const_int_rtx+512(%rip), %rsi
	movq	%rax, %rbx
	movq	$1, %rdi
	xorq	%rax, %rax
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_E
	movq	$143, %rdi
	movq	$15, %rsi
	movq	%r15, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rcx
	movq	$57, %rsi
	xorq	%rax, %rax
	movq	%r13, %rdi
	movq	%rbx, %rdx
	callq	gen_rtx
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp1730:
	.size	gen_sse_comi, .Ltmp1730-gen_sse_comi
	.cfi_endproc

	.globl	gen_sse_ucomi
	.align	16, 0x90
	.type	gen_sse_ucomi,@function
gen_sse_ucomi:                          # @gen_sse_ucomi
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp1737:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp1738:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp1739:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp1740:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp1741:
	.cfi_def_cfa_offset 48
.Ltmp1742:
	.cfi_offset %rbx, -48
.Ltmp1743:
	.cfi_offset %r12, -40
.Ltmp1744:
	.cfi_offset %r13, -32
.Ltmp1745:
	.cfi_offset %r14, -24
.Ltmp1746:
	.cfi_offset %r15, -16
	movq	%rdx, %rbx
	movq	%rsi, %r15
	movq	%rdi, %r12
	movq	$58, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	const_int_rtx+512(%rip), %rsi
	movzbl	(%rbx), %r13d
	movq	%rax, %r14
	movq	$1, %rdi
	xorq	%rax, %rax
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_E
	movq	$143, %rdi
	movq	$15, %rsi
	movq	%r12, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	const_int_rtx+512(%rip), %rsi
	movq	%rax, %rbx
	movq	$1, %rdi
	xorq	%rax, %rax
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_E
	movq	$143, %rdi
	movq	$15, %rsi
	movq	%r15, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rcx
	movq	$58, %rsi
	xorq	%rax, %rax
	movq	%r13, %rdi
	movq	%rbx, %rdx
	callq	gen_rtx
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp1747:
	.size	gen_sse_ucomi, .Ltmp1747-gen_sse_ucomi
	.cfi_endproc

	.globl	gen_sse_unpckhps
	.align	16, 0x90
	.type	gen_sse_unpckhps,@function
gen_sse_unpckhps:                       # @gen_sse_unpckhps
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp1754:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp1755:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp1756:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp1757:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp1758:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp1759:
	.cfi_def_cfa_offset 64
.Ltmp1760:
	.cfi_offset %rbx, -48
.Ltmp1761:
	.cfi_offset %r12, -40
.Ltmp1762:
	.cfi_offset %r13, -32
.Ltmp1763:
	.cfi_offset %r14, -24
.Ltmp1764:
	.cfi_offset %r15, -16
	movq	%rdx, (%rsp)            # 8-byte Spill
	movq	%rsi, %r12
	movq	%rdi, 8(%rsp)           # 8-byte Spill
	xorq	%rdi, %rdi
	movq	$2, %rsi
	callq	gen_rtx_CONST_INT
	movq	const_int_rtx+512(%rip), %r13
	movq	%rax, %rbx
	xorq	%rdi, %rdi
	movq	$3, %rsi
	callq	gen_rtx_CONST_INT
	movq	const_int_rtx+520(%rip), %r8
	movq	%rax, %rcx
	movq	$4, %rdi
	xorq	%rax, %rax
	movq	%rbx, %rsi
	movq	%r13, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_E
	movq	$143, %rdi
	movq	$46, %rsi
	movq	%r12, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	const_int_rtx+512(%rip), %r13
	movq	%rax, %r12
	xorq	%rdi, %rdi
	movq	$2, %rsi
	callq	gen_rtx_CONST_INT
	movq	const_int_rtx+520(%rip), %r15
	movq	%rax, %r14
	xorq	%rdi, %rdi
	movq	$3, %rsi
	callq	gen_rtx_CONST_INT
	movq	%rax, %rbx
	movq	$4, %rdi
	xorq	%rax, %rax
	movq	%r13, %rsi
	movq	%r14, %rdx
	movq	%r15, %rcx
	movq	%rbx, %r8
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_E
	movq	$143, %rdi
	movq	$46, %rsi
	movq	(%rsp), %rdx            # 8-byte Reload
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rbx
	xorq	%rdi, %rdi
	movq	$5, %rsi
	callq	gen_rtx_CONST_INT
	movq	$142, %rdi
	movq	$46, %rsi
	movq	%r12, %rdx
	movq	%rbx, %rcx
	movq	%rax, %r8
	callq	gen_rtx_fmt_eee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	8(%rsp), %rdx           # 8-byte Reload
	movq	%rax, %rcx
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp1765:
	.size	gen_sse_unpckhps, .Ltmp1765-gen_sse_unpckhps
	.cfi_endproc

	.globl	gen_sse_unpcklps
	.align	16, 0x90
	.type	gen_sse_unpcklps,@function
gen_sse_unpcklps:                       # @gen_sse_unpcklps
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp1772:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp1773:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp1774:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp1775:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp1776:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp1777:
	.cfi_def_cfa_offset 64
.Ltmp1778:
	.cfi_offset %rbx, -48
.Ltmp1779:
	.cfi_offset %r12, -40
.Ltmp1780:
	.cfi_offset %r13, -32
.Ltmp1781:
	.cfi_offset %r14, -24
.Ltmp1782:
	.cfi_offset %r15, -16
	movq	const_int_rtx+512(%rip), %r13
	movq	%rdx, (%rsp)            # 8-byte Spill
	movq	%rsi, %r12
	movq	%rdi, 8(%rsp)           # 8-byte Spill
	xorq	%rdi, %rdi
	movq	$2, %rsi
	callq	gen_rtx_CONST_INT
	movq	const_int_rtx+520(%rip), %r15
	movq	%rax, %r14
	xorq	%rdi, %rdi
	movq	$3, %rsi
	callq	gen_rtx_CONST_INT
	movq	%rax, %rbx
	movq	$4, %rdi
	xorq	%rax, %rax
	movq	%r13, %rsi
	movq	%r14, %rdx
	movq	%r15, %rcx
	movq	%rbx, %r8
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_E
	movq	$143, %rdi
	movq	$46, %rsi
	movq	%r12, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %r14
	xorq	%rdi, %rdi
	movq	$2, %rsi
	callq	gen_rtx_CONST_INT
	movq	const_int_rtx+512(%rip), %r15
	movq	%rax, %rbx
	xorq	%rdi, %rdi
	movq	$3, %rsi
	callq	gen_rtx_CONST_INT
	movq	const_int_rtx+520(%rip), %r8
	movq	%rax, %rcx
	movq	$4, %rdi
	xorq	%rax, %rax
	movq	%rbx, %rsi
	movq	%r15, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_E
	movq	$143, %rdi
	movq	$46, %rsi
	movq	(%rsp), %rdx            # 8-byte Reload
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rbx
	xorq	%rdi, %rdi
	movq	$5, %rsi
	callq	gen_rtx_CONST_INT
	movq	$142, %rdi
	movq	$46, %rsi
	movq	%r14, %rdx
	movq	%rbx, %rcx
	movq	%rax, %r8
	callq	gen_rtx_fmt_eee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	8(%rsp), %rdx           # 8-byte Reload
	movq	%rax, %rcx
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp1783:
	.size	gen_sse_unpcklps, .Ltmp1783-gen_sse_unpcklps
	.cfi_endproc

	.globl	gen_smaxv4sf3
	.align	16, 0x90
	.type	gen_smaxv4sf3,@function
gen_smaxv4sf3:                          # @gen_smaxv4sf3
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp1786:
	.cfi_def_cfa_offset 16
.Ltmp1787:
	.cfi_offset %rbx, -16
	movq	%rdx, %rax
	movq	%rsi, %rcx
	movq	%rdi, %rbx
	movq	$93, %rdi
	movq	$46, %rsi
	movq	%rcx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	popq	%rbx
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp1788:
	.size	gen_smaxv4sf3, .Ltmp1788-gen_smaxv4sf3
	.cfi_endproc

	.globl	gen_vmsmaxv4sf3
	.align	16, 0x90
	.type	gen_vmsmaxv4sf3,@function
gen_vmsmaxv4sf3:                        # @gen_vmsmaxv4sf3
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp1792:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp1793:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp1794:
	.cfi_def_cfa_offset 32
.Ltmp1795:
	.cfi_offset %rbx, -24
.Ltmp1796:
	.cfi_offset %r14, -16
	movq	%rdx, %rax
	movq	%rsi, %rbx
	movq	%rdi, %r14
	movq	$93, %rdi
	movq	$46, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	const_int_rtx+520(%rip), %r8
	movq	$142, %rdi
	movq	$46, %rsi
	movq	%rax, %rdx
	movq	%rbx, %rcx
	callq	gen_rtx_fmt_eee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp1797:
	.size	gen_vmsmaxv4sf3, .Ltmp1797-gen_vmsmaxv4sf3
	.cfi_endproc

	.globl	gen_sminv4sf3
	.align	16, 0x90
	.type	gen_sminv4sf3,@function
gen_sminv4sf3:                          # @gen_sminv4sf3
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp1800:
	.cfi_def_cfa_offset 16
.Ltmp1801:
	.cfi_offset %rbx, -16
	movq	%rdx, %rax
	movq	%rsi, %rcx
	movq	%rdi, %rbx
	movq	$92, %rdi
	movq	$46, %rsi
	movq	%rcx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	popq	%rbx
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp1802:
	.size	gen_sminv4sf3, .Ltmp1802-gen_sminv4sf3
	.cfi_endproc

	.globl	gen_vmsminv4sf3
	.align	16, 0x90
	.type	gen_vmsminv4sf3,@function
gen_vmsminv4sf3:                        # @gen_vmsminv4sf3
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp1806:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp1807:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp1808:
	.cfi_def_cfa_offset 32
.Ltmp1809:
	.cfi_offset %rbx, -24
.Ltmp1810:
	.cfi_offset %r14, -16
	movq	%rdx, %rax
	movq	%rsi, %rbx
	movq	%rdi, %r14
	movq	$92, %rdi
	movq	$46, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	const_int_rtx+520(%rip), %r8
	movq	$142, %rdi
	movq	$46, %rsi
	movq	%rax, %rdx
	movq	%rbx, %rcx
	callq	gen_rtx_fmt_eee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp1811:
	.size	gen_vmsminv4sf3, .Ltmp1811-gen_vmsminv4sf3
	.cfi_endproc

	.globl	gen_cvtpi2ps
	.align	16, 0x90
	.type	gen_cvtpi2ps,@function
gen_cvtpi2ps:                           # @gen_cvtpi2ps
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp1816:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp1817:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp1818:
	.cfi_def_cfa_offset 32
.Ltmp1819:
	.cfi_offset %rbx, -32
.Ltmp1820:
	.cfi_offset %r14, -24
.Ltmp1821:
	.cfi_offset %r15, -16
	movq	%rsi, %r15
	movq	%rdi, %r14
	movq	$125, %rdi
	movq	$44, %rsi
	callq	gen_rtx_fmt_e
	movq	$145, %rdi
	movq	$46, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rbx
	xorq	%rdi, %rdi
	movq	$12, %rsi
	callq	gen_rtx_CONST_INT
	movq	$142, %rdi
	movq	$46, %rsi
	movq	%r15, %rdx
	movq	%rbx, %rcx
	movq	%rax, %r8
	callq	gen_rtx_fmt_eee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	popq	%rbx
	popq	%r14
	popq	%r15
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp1822:
	.size	gen_cvtpi2ps, .Ltmp1822-gen_cvtpi2ps
	.cfi_endproc

	.globl	gen_cvtps2pi
	.align	16, 0x90
	.type	gen_cvtps2pi,@function
gen_cvtps2pi:                           # @gen_cvtps2pi
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp1826:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp1827:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp1828:
	.cfi_def_cfa_offset 32
.Ltmp1829:
	.cfi_offset %rbx, -24
.Ltmp1830:
	.cfi_offset %r14, -16
	movq	%rsi, %rax
	movq	%rdi, %r14
	movq	$126, %rdi
	movq	$37, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	const_int_rtx+512(%rip), %rsi
	movq	const_int_rtx+520(%rip), %rdx
	movq	%rax, %rbx
	movq	$2, %rdi
	xorq	%rax, %rax
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_E
	movq	$143, %rdi
	movq	$33, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp1831:
	.size	gen_cvtps2pi, .Ltmp1831-gen_cvtps2pi
	.cfi_endproc

	.globl	gen_cvttps2pi
	.align	16, 0x90
	.type	gen_cvttps2pi,@function
gen_cvttps2pi:                          # @gen_cvttps2pi
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp1835:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp1836:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp1837:
	.cfi_def_cfa_offset 32
.Ltmp1838:
	.cfi_offset %rbx, -24
.Ltmp1839:
	.cfi_offset %r14, -16
	movq	%rdi, %r14
	movq	$1, %rdi
	xorq	%rax, %rax
	callq	gen_rtvec
	movq	$42, %rdi
	movq	$37, %rsi
	movq	$30, %rcx
	movq	%rax, %rdx
	callq	gen_rtx_fmt_Ei
	movq	const_int_rtx+512(%rip), %rsi
	movq	const_int_rtx+520(%rip), %rdx
	movq	%rax, %rbx
	movq	$2, %rdi
	xorq	%rax, %rax
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_E
	movq	$143, %rdi
	movq	$33, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp1840:
	.size	gen_cvttps2pi, .Ltmp1840-gen_cvttps2pi
	.cfi_endproc

	.globl	gen_cvtsi2ss
	.align	16, 0x90
	.type	gen_cvtsi2ss,@function
gen_cvtsi2ss:                           # @gen_cvtsi2ss
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp1845:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp1846:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp1847:
	.cfi_def_cfa_offset 32
.Ltmp1848:
	.cfi_offset %rbx, -32
.Ltmp1849:
	.cfi_offset %r14, -24
.Ltmp1850:
	.cfi_offset %r15, -16
	movq	%rsi, %r15
	movq	%rdi, %r14
	movq	$125, %rdi
	movq	$15, %rsi
	callq	gen_rtx_fmt_e
	movq	$145, %rdi
	movq	$46, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rbx
	xorq	%rdi, %rdi
	movq	$14, %rsi
	callq	gen_rtx_CONST_INT
	movq	$142, %rdi
	movq	$46, %rsi
	movq	%r15, %rdx
	movq	%rbx, %rcx
	movq	%rax, %r8
	callq	gen_rtx_fmt_eee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	popq	%rbx
	popq	%r14
	popq	%r15
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp1851:
	.size	gen_cvtsi2ss, .Ltmp1851-gen_cvtsi2ss
	.cfi_endproc

	.globl	gen_cvtss2si
	.align	16, 0x90
	.type	gen_cvtss2si,@function
gen_cvtss2si:                           # @gen_cvtss2si
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp1855:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp1856:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp1857:
	.cfi_def_cfa_offset 32
.Ltmp1858:
	.cfi_offset %rbx, -24
.Ltmp1859:
	.cfi_offset %r14, -16
	movq	%rsi, %rax
	movq	%rdi, %r14
	movq	$126, %rdi
	movq	$37, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	const_int_rtx+512(%rip), %rsi
	movq	%rax, %rbx
	movq	$1, %rdi
	xorq	%rax, %rax
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_E
	movq	$143, %rdi
	movq	$4, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp1860:
	.size	gen_cvtss2si, .Ltmp1860-gen_cvtss2si
	.cfi_endproc

	.globl	gen_cvttss2si
	.align	16, 0x90
	.type	gen_cvttss2si,@function
gen_cvttss2si:                          # @gen_cvttss2si
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp1864:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp1865:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp1866:
	.cfi_def_cfa_offset 32
.Ltmp1867:
	.cfi_offset %rbx, -24
.Ltmp1868:
	.cfi_offset %r14, -16
	movq	%rdi, %r14
	movq	$1, %rdi
	xorq	%rax, %rax
	callq	gen_rtvec
	movq	$42, %rdi
	movq	$37, %rsi
	movq	$30, %rcx
	movq	%rax, %rdx
	callq	gen_rtx_fmt_Ei
	movq	const_int_rtx+512(%rip), %rsi
	movq	%rax, %rbx
	movq	$1, %rdi
	xorq	%rax, %rax
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_E
	movq	$143, %rdi
	movq	$4, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp1869:
	.size	gen_cvttss2si, .Ltmp1869-gen_cvttss2si
	.cfi_endproc

	.globl	gen_addv8qi3
	.align	16, 0x90
	.type	gen_addv8qi3,@function
gen_addv8qi3:                           # @gen_addv8qi3
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp1872:
	.cfi_def_cfa_offset 16
.Ltmp1873:
	.cfi_offset %rbx, -16
	movq	%rdx, %rax
	movq	%rsi, %rcx
	movq	%rdi, %rbx
	movq	$75, %rdi
	movq	$39, %rsi
	movq	%rcx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	popq	%rbx
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp1874:
	.size	gen_addv8qi3, .Ltmp1874-gen_addv8qi3
	.cfi_endproc

	.globl	gen_addv4hi3
	.align	16, 0x90
	.type	gen_addv4hi3,@function
gen_addv4hi3:                           # @gen_addv4hi3
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp1877:
	.cfi_def_cfa_offset 16
.Ltmp1878:
	.cfi_offset %rbx, -16
	movq	%rdx, %rax
	movq	%rsi, %rcx
	movq	%rdi, %rbx
	movq	$75, %rdi
	movq	$36, %rsi
	movq	%rcx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	popq	%rbx
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp1879:
	.size	gen_addv4hi3, .Ltmp1879-gen_addv4hi3
	.cfi_endproc

	.globl	gen_addv2si3
	.align	16, 0x90
	.type	gen_addv2si3,@function
gen_addv2si3:                           # @gen_addv2si3
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp1882:
	.cfi_def_cfa_offset 16
.Ltmp1883:
	.cfi_offset %rbx, -16
	movq	%rdx, %rax
	movq	%rsi, %rcx
	movq	%rdi, %rbx
	movq	$75, %rdi
	movq	$33, %rsi
	movq	%rcx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	popq	%rbx
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp1884:
	.size	gen_addv2si3, .Ltmp1884-gen_addv2si3
	.cfi_endproc

	.globl	gen_ssaddv8qi3
	.align	16, 0x90
	.type	gen_ssaddv8qi3,@function
gen_ssaddv8qi3:                         # @gen_ssaddv8qi3
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp1887:
	.cfi_def_cfa_offset 16
.Ltmp1888:
	.cfi_offset %rbx, -16
	movq	%rdx, %rax
	movq	%rsi, %rcx
	movq	%rdi, %rbx
	movq	$146, %rdi
	movq	$39, %rsi
	movq	%rcx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	popq	%rbx
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp1889:
	.size	gen_ssaddv8qi3, .Ltmp1889-gen_ssaddv8qi3
	.cfi_endproc

	.globl	gen_ssaddv4hi3
	.align	16, 0x90
	.type	gen_ssaddv4hi3,@function
gen_ssaddv4hi3:                         # @gen_ssaddv4hi3
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp1892:
	.cfi_def_cfa_offset 16
.Ltmp1893:
	.cfi_offset %rbx, -16
	movq	%rdx, %rax
	movq	%rsi, %rcx
	movq	%rdi, %rbx
	movq	$146, %rdi
	movq	$36, %rsi
	movq	%rcx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	popq	%rbx
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp1894:
	.size	gen_ssaddv4hi3, .Ltmp1894-gen_ssaddv4hi3
	.cfi_endproc

	.globl	gen_usaddv8qi3
	.align	16, 0x90
	.type	gen_usaddv8qi3,@function
gen_usaddv8qi3:                         # @gen_usaddv8qi3
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp1897:
	.cfi_def_cfa_offset 16
.Ltmp1898:
	.cfi_offset %rbx, -16
	movq	%rdx, %rax
	movq	%rsi, %rcx
	movq	%rdi, %rbx
	movq	$147, %rdi
	movq	$39, %rsi
	movq	%rcx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	popq	%rbx
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp1899:
	.size	gen_usaddv8qi3, .Ltmp1899-gen_usaddv8qi3
	.cfi_endproc

	.globl	gen_usaddv4hi3
	.align	16, 0x90
	.type	gen_usaddv4hi3,@function
gen_usaddv4hi3:                         # @gen_usaddv4hi3
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp1902:
	.cfi_def_cfa_offset 16
.Ltmp1903:
	.cfi_offset %rbx, -16
	movq	%rdx, %rax
	movq	%rsi, %rcx
	movq	%rdi, %rbx
	movq	$147, %rdi
	movq	$36, %rsi
	movq	%rcx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	popq	%rbx
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp1904:
	.size	gen_usaddv4hi3, .Ltmp1904-gen_usaddv4hi3
	.cfi_endproc

	.globl	gen_subv8qi3
	.align	16, 0x90
	.type	gen_subv8qi3,@function
gen_subv8qi3:                           # @gen_subv8qi3
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp1907:
	.cfi_def_cfa_offset 16
.Ltmp1908:
	.cfi_offset %rbx, -16
	movq	%rdx, %rax
	movq	%rsi, %rcx
	movq	%rdi, %rbx
	movq	$76, %rdi
	movq	$39, %rsi
	movq	%rcx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	popq	%rbx
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp1909:
	.size	gen_subv8qi3, .Ltmp1909-gen_subv8qi3
	.cfi_endproc

	.globl	gen_subv4hi3
	.align	16, 0x90
	.type	gen_subv4hi3,@function
gen_subv4hi3:                           # @gen_subv4hi3
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp1912:
	.cfi_def_cfa_offset 16
.Ltmp1913:
	.cfi_offset %rbx, -16
	movq	%rdx, %rax
	movq	%rsi, %rcx
	movq	%rdi, %rbx
	movq	$76, %rdi
	movq	$36, %rsi
	movq	%rcx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	popq	%rbx
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp1914:
	.size	gen_subv4hi3, .Ltmp1914-gen_subv4hi3
	.cfi_endproc

	.globl	gen_subv2si3
	.align	16, 0x90
	.type	gen_subv2si3,@function
gen_subv2si3:                           # @gen_subv2si3
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp1917:
	.cfi_def_cfa_offset 16
.Ltmp1918:
	.cfi_offset %rbx, -16
	movq	%rdx, %rax
	movq	%rsi, %rcx
	movq	%rdi, %rbx
	movq	$76, %rdi
	movq	$33, %rsi
	movq	%rcx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	popq	%rbx
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp1919:
	.size	gen_subv2si3, .Ltmp1919-gen_subv2si3
	.cfi_endproc

	.globl	gen_sssubv8qi3
	.align	16, 0x90
	.type	gen_sssubv8qi3,@function
gen_sssubv8qi3:                         # @gen_sssubv8qi3
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp1922:
	.cfi_def_cfa_offset 16
.Ltmp1923:
	.cfi_offset %rbx, -16
	movq	%rdx, %rax
	movq	%rsi, %rcx
	movq	%rdi, %rbx
	movq	$148, %rdi
	movq	$39, %rsi
	movq	%rcx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	popq	%rbx
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp1924:
	.size	gen_sssubv8qi3, .Ltmp1924-gen_sssubv8qi3
	.cfi_endproc

	.globl	gen_sssubv4hi3
	.align	16, 0x90
	.type	gen_sssubv4hi3,@function
gen_sssubv4hi3:                         # @gen_sssubv4hi3
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp1927:
	.cfi_def_cfa_offset 16
.Ltmp1928:
	.cfi_offset %rbx, -16
	movq	%rdx, %rax
	movq	%rsi, %rcx
	movq	%rdi, %rbx
	movq	$148, %rdi
	movq	$36, %rsi
	movq	%rcx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	popq	%rbx
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp1929:
	.size	gen_sssubv4hi3, .Ltmp1929-gen_sssubv4hi3
	.cfi_endproc

	.globl	gen_ussubv8qi3
	.align	16, 0x90
	.type	gen_ussubv8qi3,@function
gen_ussubv8qi3:                         # @gen_ussubv8qi3
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp1932:
	.cfi_def_cfa_offset 16
.Ltmp1933:
	.cfi_offset %rbx, -16
	movq	%rdx, %rax
	movq	%rsi, %rcx
	movq	%rdi, %rbx
	movq	$149, %rdi
	movq	$39, %rsi
	movq	%rcx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	popq	%rbx
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp1934:
	.size	gen_ussubv8qi3, .Ltmp1934-gen_ussubv8qi3
	.cfi_endproc

	.globl	gen_ussubv4hi3
	.align	16, 0x90
	.type	gen_ussubv4hi3,@function
gen_ussubv4hi3:                         # @gen_ussubv4hi3
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp1937:
	.cfi_def_cfa_offset 16
.Ltmp1938:
	.cfi_offset %rbx, -16
	movq	%rdx, %rax
	movq	%rsi, %rcx
	movq	%rdi, %rbx
	movq	$149, %rdi
	movq	$36, %rsi
	movq	%rcx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	popq	%rbx
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp1939:
	.size	gen_ussubv4hi3, .Ltmp1939-gen_ussubv4hi3
	.cfi_endproc

	.globl	gen_mulv4hi3
	.align	16, 0x90
	.type	gen_mulv4hi3,@function
gen_mulv4hi3:                           # @gen_mulv4hi3
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp1942:
	.cfi_def_cfa_offset 16
.Ltmp1943:
	.cfi_offset %rbx, -16
	movq	%rdx, %rax
	movq	%rsi, %rcx
	movq	%rdi, %rbx
	movq	$78, %rdi
	movq	$36, %rsi
	movq	%rcx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	popq	%rbx
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp1944:
	.size	gen_mulv4hi3, .Ltmp1944-gen_mulv4hi3
	.cfi_endproc

	.globl	gen_smulv4hi3_highpart
	.align	16, 0x90
	.type	gen_smulv4hi3_highpart,@function
gen_smulv4hi3_highpart:                 # @gen_smulv4hi3_highpart
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp1949:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp1950:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp1951:
	.cfi_def_cfa_offset 32
.Ltmp1952:
	.cfi_offset %rbx, -32
.Ltmp1953:
	.cfi_offset %r14, -24
.Ltmp1954:
	.cfi_offset %r15, -16
	movq	%rdx, %r15
	movq	%rsi, %rax
	movq	%rdi, %r14
	movq	$120, %rdi
	movq	$37, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rbx
	movq	$120, %rdi
	movq	$37, %rsi
	movq	%r15, %rdx
	callq	gen_rtx_fmt_e
	movq	$78, %rdi
	movq	$37, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rbx
	xorq	%rdi, %rdi
	movq	$16, %rsi
	callq	gen_rtx_CONST_INT
	movq	$90, %rdi
	movq	$37, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$122, %rdi
	movq	$36, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	popq	%rbx
	popq	%r14
	popq	%r15
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp1955:
	.size	gen_smulv4hi3_highpart, .Ltmp1955-gen_smulv4hi3_highpart
	.cfi_endproc

	.globl	gen_umulv4hi3_highpart
	.align	16, 0x90
	.type	gen_umulv4hi3_highpart,@function
gen_umulv4hi3_highpart:                 # @gen_umulv4hi3_highpart
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp1960:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp1961:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp1962:
	.cfi_def_cfa_offset 32
.Ltmp1963:
	.cfi_offset %rbx, -32
.Ltmp1964:
	.cfi_offset %r14, -24
.Ltmp1965:
	.cfi_offset %r15, -16
	movq	%rdx, %r15
	movq	%rsi, %rax
	movq	%rdi, %r14
	movq	$121, %rdi
	movq	$37, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rbx
	movq	$121, %rdi
	movq	$37, %rsi
	movq	%r15, %rdx
	callq	gen_rtx_fmt_e
	movq	$78, %rdi
	movq	$37, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rbx
	xorq	%rdi, %rdi
	movq	$16, %rsi
	callq	gen_rtx_CONST_INT
	movq	$90, %rdi
	movq	$37, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$122, %rdi
	movq	$36, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	popq	%rbx
	popq	%r14
	popq	%r15
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp1966:
	.size	gen_umulv4hi3_highpart, .Ltmp1966-gen_umulv4hi3_highpart
	.cfi_endproc

	.globl	gen_mmx_pmaddwd
	.align	16, 0x90
	.type	gen_mmx_pmaddwd,@function
gen_mmx_pmaddwd:                        # @gen_mmx_pmaddwd
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp1973:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp1974:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp1975:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp1976:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp1977:
	.cfi_def_cfa_offset 48
.Ltmp1978:
	.cfi_offset %rbx, -48
.Ltmp1979:
	.cfi_offset %r12, -40
.Ltmp1980:
	.cfi_offset %r13, -32
.Ltmp1981:
	.cfi_offset %r14, -24
.Ltmp1982:
	.cfi_offset %r15, -16
	movq	const_int_rtx+512(%rip), %rbx
	movq	%rdx, %r15
	movq	%rsi, %r13
	movq	%rdi, %r14
	xorq	%rdi, %rdi
	movq	$2, %rsi
	callq	gen_rtx_CONST_INT
	movq	%rax, %rcx
	movq	$2, %rdi
	xorq	%rax, %rax
	movq	%rbx, %rsi
	movq	%rcx, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_E
	movq	$143, %rdi
	movq	$32, %rsi
	movq	%r13, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$120, %rdi
	movq	$33, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	const_int_rtx+512(%rip), %rbx
	movq	%rax, %r12
	xorq	%rdi, %rdi
	movq	$2, %rsi
	callq	gen_rtx_CONST_INT
	movq	%rax, %rcx
	movq	$2, %rdi
	xorq	%rax, %rax
	movq	%rbx, %rsi
	movq	%rcx, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_E
	movq	$143, %rdi
	movq	$32, %rsi
	movq	%r15, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$120, %rdi
	movq	$33, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	$78, %rdi
	movq	$33, %rsi
	movq	%r12, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	const_int_rtx+520(%rip), %rbx
	movq	%rax, %r12
	xorq	%rdi, %rdi
	movq	$3, %rsi
	callq	gen_rtx_CONST_INT
	movq	%rax, %rcx
	movq	$2, %rdi
	xorq	%rax, %rax
	movq	%rbx, %rsi
	movq	%rcx, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_E
	movq	$143, %rdi
	movq	$32, %rsi
	movq	%r13, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$120, %rdi
	movq	$33, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	const_int_rtx+520(%rip), %rbx
	movq	%rax, %r13
	xorq	%rdi, %rdi
	movq	$3, %rsi
	callq	gen_rtx_CONST_INT
	movq	%rax, %rcx
	movq	$2, %rdi
	xorq	%rax, %rax
	movq	%rbx, %rsi
	movq	%rcx, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_E
	movq	$143, %rdi
	movq	$32, %rsi
	movq	%r15, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$120, %rdi
	movq	$33, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	$78, %rdi
	movq	$33, %rsi
	movq	%r13, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$75, %rdi
	movq	$33, %rsi
	movq	%r12, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp1983:
	.size	gen_mmx_pmaddwd, .Ltmp1983-gen_mmx_pmaddwd
	.cfi_endproc

	.globl	gen_mmx_iordi3
	.align	16, 0x90
	.type	gen_mmx_iordi3,@function
gen_mmx_iordi3:                         # @gen_mmx_iordi3
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp1986:
	.cfi_def_cfa_offset 16
.Ltmp1987:
	.cfi_offset %rbx, -16
	movq	%rdx, %rax
	movq	%rsi, %rcx
	movq	%rdi, %rbx
	movq	$84, %rdi
	movq	$5, %rsi
	movq	%rcx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rcx
	movq	$1, %rdi
	xorq	%rax, %rax
	movq	%rcx, %rsi
	callq	gen_rtvec
	movq	$42, %rdi
	movq	$5, %rsi
	movq	$45, %rcx
	movq	%rax, %rdx
	callq	gen_rtx_fmt_Ei
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	popq	%rbx
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp1988:
	.size	gen_mmx_iordi3, .Ltmp1988-gen_mmx_iordi3
	.cfi_endproc

	.globl	gen_mmx_xordi3
	.align	16, 0x90
	.type	gen_mmx_xordi3,@function
gen_mmx_xordi3:                         # @gen_mmx_xordi3
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp1991:
	.cfi_def_cfa_offset 16
.Ltmp1992:
	.cfi_offset %rbx, -16
	movq	%rdx, %rax
	movq	%rsi, %rcx
	movq	%rdi, %rbx
	movq	$85, %rdi
	movq	$5, %rsi
	movq	%rcx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rcx
	movq	$1, %rdi
	xorq	%rax, %rax
	movq	%rcx, %rsi
	callq	gen_rtvec
	movq	$42, %rdi
	movq	$5, %rsi
	movq	$45, %rcx
	movq	%rax, %rdx
	callq	gen_rtx_fmt_Ei
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	popq	%rbx
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp1993:
	.size	gen_mmx_xordi3, .Ltmp1993-gen_mmx_xordi3
	.cfi_endproc

	.globl	gen_mmx_clrdi
	.align	16, 0x90
	.type	gen_mmx_clrdi,@function
gen_mmx_clrdi:                          # @gen_mmx_clrdi
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp1996:
	.cfi_def_cfa_offset 16
.Ltmp1997:
	.cfi_offset %rbx, -16
	movq	const_int_rtx+512(%rip), %rsi
	movq	%rdi, %rbx
	movq	$1, %rdi
	xorq	%rax, %rax
	callq	gen_rtvec
	movq	$42, %rdi
	movq	$5, %rsi
	movq	$45, %rcx
	movq	%rax, %rdx
	callq	gen_rtx_fmt_Ei
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	popq	%rbx
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp1998:
	.size	gen_mmx_clrdi, .Ltmp1998-gen_mmx_clrdi
	.cfi_endproc

	.globl	gen_mmx_anddi3
	.align	16, 0x90
	.type	gen_mmx_anddi3,@function
gen_mmx_anddi3:                         # @gen_mmx_anddi3
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp2001:
	.cfi_def_cfa_offset 16
.Ltmp2002:
	.cfi_offset %rbx, -16
	movq	%rdx, %rax
	movq	%rsi, %rcx
	movq	%rdi, %rbx
	movq	$83, %rdi
	movq	$5, %rsi
	movq	%rcx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rcx
	movq	$1, %rdi
	xorq	%rax, %rax
	movq	%rcx, %rsi
	callq	gen_rtvec
	movq	$42, %rdi
	movq	$5, %rsi
	movq	$45, %rcx
	movq	%rax, %rdx
	callq	gen_rtx_fmt_Ei
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	popq	%rbx
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp2003:
	.size	gen_mmx_anddi3, .Ltmp2003-gen_mmx_anddi3
	.cfi_endproc

	.globl	gen_mmx_nanddi3
	.align	16, 0x90
	.type	gen_mmx_nanddi3,@function
gen_mmx_nanddi3:                        # @gen_mmx_nanddi3
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp2007:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp2008:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp2009:
	.cfi_def_cfa_offset 32
.Ltmp2010:
	.cfi_offset %rbx, -24
.Ltmp2011:
	.cfi_offset %r14, -16
	movq	%rdx, %rbx
	movq	%rsi, %rax
	movq	%rdi, %r14
	movq	$86, %rdi
	movq	$5, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	$83, %rdi
	movq	$5, %rsi
	movq	%rax, %rdx
	movq	%rbx, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rcx
	movq	$1, %rdi
	xorq	%rax, %rax
	movq	%rcx, %rsi
	callq	gen_rtvec
	movq	$42, %rdi
	movq	$5, %rsi
	movq	$45, %rcx
	movq	%rax, %rdx
	callq	gen_rtx_fmt_Ei
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp2012:
	.size	gen_mmx_nanddi3, .Ltmp2012-gen_mmx_nanddi3
	.cfi_endproc

	.globl	gen_mmx_uavgv8qi3
	.align	16, 0x90
	.type	gen_mmx_uavgv8qi3,@function
gen_mmx_uavgv8qi3:                      # @gen_mmx_uavgv8qi3
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp2016:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp2017:
	.cfi_def_cfa_offset 24
	subq	$24, %rsp
.Ltmp2018:
	.cfi_def_cfa_offset 48
.Ltmp2019:
	.cfi_offset %rbx, -24
.Ltmp2020:
	.cfi_offset %r14, -16
	movq	%rdx, %rax
	movq	%rsi, %rcx
	movq	%rdi, %r14
	movq	$75, %rdi
	movq	$39, %rsi
	movq	%rcx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	const_int_rtx+520(%rip), %rsi
	movq	%rax, %rbx
	movq	%rsi, 16(%rsp)
	movq	%rsi, 8(%rsp)
	movq	$8, %rdi
	xorq	%rax, %rax
	movq	%rsi, (%rsp)
	movq	%rsi, %rdx
	movq	%rsi, %rcx
	movq	%rsi, %r8
	movq	%rsi, %r9
	callq	gen_rtvec
	movq	$56, %rdi
	movq	$39, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_E
	movq	$75, %rdi
	movq	$39, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	const_int_rtx+520(%rip), %rcx
	movq	$89, %rdi
	movq	$39, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	addq	$24, %rsp
	popq	%rbx
	popq	%r14
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp2021:
	.size	gen_mmx_uavgv8qi3, .Ltmp2021-gen_mmx_uavgv8qi3
	.cfi_endproc

	.globl	gen_mmx_uavgv4hi3
	.align	16, 0x90
	.type	gen_mmx_uavgv4hi3,@function
gen_mmx_uavgv4hi3:                      # @gen_mmx_uavgv4hi3
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp2025:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp2026:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp2027:
	.cfi_def_cfa_offset 32
.Ltmp2028:
	.cfi_offset %rbx, -24
.Ltmp2029:
	.cfi_offset %r14, -16
	movq	%rdx, %rax
	movq	%rsi, %rcx
	movq	%rdi, %r14
	movq	$75, %rdi
	movq	$36, %rsi
	movq	%rcx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	const_int_rtx+520(%rip), %rsi
	movq	%rax, %rbx
	movq	$4, %rdi
	xorq	%rax, %rax
	movq	%rsi, %rdx
	movq	%rsi, %rcx
	movq	%rsi, %r8
	callq	gen_rtvec
	movq	$56, %rdi
	movq	$36, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_E
	movq	$75, %rdi
	movq	$36, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	const_int_rtx+520(%rip), %rcx
	movq	$89, %rdi
	movq	$36, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp2030:
	.size	gen_mmx_uavgv4hi3, .Ltmp2030-gen_mmx_uavgv4hi3
	.cfi_endproc

	.globl	gen_mmx_psadbw
	.align	16, 0x90
	.type	gen_mmx_psadbw,@function
gen_mmx_psadbw:                         # @gen_mmx_psadbw
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp2033:
	.cfi_def_cfa_offset 16
.Ltmp2034:
	.cfi_offset %rbx, -16
	movq	%rdx, %rax
	movq	%rsi, %rcx
	movq	%rdi, %rbx
	movq	$76, %rdi
	movq	$39, %rsi
	movq	%rcx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$129, %rdi
	movq	$39, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	popq	%rbx
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp2035:
	.size	gen_mmx_psadbw, .Ltmp2035-gen_mmx_psadbw
	.cfi_endproc

	.globl	gen_mmx_pinsrw
	.align	16, 0x90
	.type	gen_mmx_pinsrw,@function
gen_mmx_pinsrw:                         # @gen_mmx_pinsrw
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp2040:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp2041:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp2042:
	.cfi_def_cfa_offset 32
.Ltmp2043:
	.cfi_offset %rbx, -32
.Ltmp2044:
	.cfi_offset %r14, -24
.Ltmp2045:
	.cfi_offset %r15, -16
	movq	%rcx, %r14
	movq	%rsi, %rbx
	movq	%rdi, %r15
	movq	$122, %rdi
	movq	$3, %rsi
	callq	gen_rtx_fmt_e
	movq	$145, %rdi
	movq	$36, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	$142, %rdi
	movq	$36, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	movq	%r14, %r8
	callq	gen_rtx_fmt_eee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r15, %rdx
	movq	%rax, %rcx
	popq	%rbx
	popq	%r14
	popq	%r15
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp2046:
	.size	gen_mmx_pinsrw, .Ltmp2046-gen_mmx_pinsrw
	.cfi_endproc

	.globl	gen_mmx_pextrw
	.align	16, 0x90
	.type	gen_mmx_pextrw,@function
gen_mmx_pextrw:                         # @gen_mmx_pextrw
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp2050:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp2051:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp2052:
	.cfi_def_cfa_offset 32
.Ltmp2053:
	.cfi_offset %rbx, -24
.Ltmp2054:
	.cfi_offset %r14, -16
	movq	%rsi, %rbx
	movq	%rdi, %r14
	movq	$1, %rdi
	xorq	%rax, %rax
	movq	%rdx, %rsi
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_E
	movq	$143, %rdi
	movq	$3, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$121, %rdi
	movq	$4, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp2055:
	.size	gen_mmx_pextrw, .Ltmp2055-gen_mmx_pextrw
	.cfi_endproc

	.globl	gen_mmx_pshufw
	.align	16, 0x90
	.type	gen_mmx_pshufw,@function
gen_mmx_pshufw:                         # @gen_mmx_pshufw
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp2058:
	.cfi_def_cfa_offset 16
.Ltmp2059:
	.cfi_offset %rbx, -16
	movq	%rdi, %rbx
	movq	$2, %rdi
	xorq	%rax, %rax
	callq	gen_rtvec
	movq	$42, %rdi
	movq	$36, %rsi
	movq	$41, %rcx
	movq	%rax, %rdx
	callq	gen_rtx_fmt_Ei
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	popq	%rbx
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp2060:
	.size	gen_mmx_pshufw, .Ltmp2060-gen_mmx_pshufw
	.cfi_endproc

	.globl	gen_eqv8qi3
	.align	16, 0x90
	.type	gen_eqv8qi3,@function
gen_eqv8qi3:                            # @gen_eqv8qi3
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp2063:
	.cfi_def_cfa_offset 16
.Ltmp2064:
	.cfi_offset %rbx, -16
	movq	%rdx, %rax
	movq	%rsi, %rcx
	movq	%rdi, %rbx
	movq	$103, %rdi
	movq	$39, %rsi
	movq	%rcx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	popq	%rbx
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp2065:
	.size	gen_eqv8qi3, .Ltmp2065-gen_eqv8qi3
	.cfi_endproc

	.globl	gen_eqv4hi3
	.align	16, 0x90
	.type	gen_eqv4hi3,@function
gen_eqv4hi3:                            # @gen_eqv4hi3
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp2068:
	.cfi_def_cfa_offset 16
.Ltmp2069:
	.cfi_offset %rbx, -16
	movq	%rdx, %rax
	movq	%rsi, %rcx
	movq	%rdi, %rbx
	movq	$103, %rdi
	movq	$36, %rsi
	movq	%rcx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	popq	%rbx
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp2070:
	.size	gen_eqv4hi3, .Ltmp2070-gen_eqv4hi3
	.cfi_endproc

	.globl	gen_eqv2si3
	.align	16, 0x90
	.type	gen_eqv2si3,@function
gen_eqv2si3:                            # @gen_eqv2si3
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp2073:
	.cfi_def_cfa_offset 16
.Ltmp2074:
	.cfi_offset %rbx, -16
	movq	%rdx, %rax
	movq	%rsi, %rcx
	movq	%rdi, %rbx
	movq	$103, %rdi
	movq	$33, %rsi
	movq	%rcx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	popq	%rbx
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp2075:
	.size	gen_eqv2si3, .Ltmp2075-gen_eqv2si3
	.cfi_endproc

	.globl	gen_gtv8qi3
	.align	16, 0x90
	.type	gen_gtv8qi3,@function
gen_gtv8qi3:                            # @gen_gtv8qi3
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp2078:
	.cfi_def_cfa_offset 16
.Ltmp2079:
	.cfi_offset %rbx, -16
	movq	%rdx, %rax
	movq	%rsi, %rcx
	movq	%rdi, %rbx
	movq	$105, %rdi
	movq	$39, %rsi
	movq	%rcx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	popq	%rbx
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp2080:
	.size	gen_gtv8qi3, .Ltmp2080-gen_gtv8qi3
	.cfi_endproc

	.globl	gen_gtv4hi3
	.align	16, 0x90
	.type	gen_gtv4hi3,@function
gen_gtv4hi3:                            # @gen_gtv4hi3
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp2083:
	.cfi_def_cfa_offset 16
.Ltmp2084:
	.cfi_offset %rbx, -16
	movq	%rdx, %rax
	movq	%rsi, %rcx
	movq	%rdi, %rbx
	movq	$105, %rdi
	movq	$36, %rsi
	movq	%rcx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	popq	%rbx
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp2085:
	.size	gen_gtv4hi3, .Ltmp2085-gen_gtv4hi3
	.cfi_endproc

	.globl	gen_gtv2si3
	.align	16, 0x90
	.type	gen_gtv2si3,@function
gen_gtv2si3:                            # @gen_gtv2si3
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp2088:
	.cfi_def_cfa_offset 16
.Ltmp2089:
	.cfi_offset %rbx, -16
	movq	%rdx, %rax
	movq	%rsi, %rcx
	movq	%rdi, %rbx
	movq	$105, %rdi
	movq	$33, %rsi
	movq	%rcx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	popq	%rbx
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp2090:
	.size	gen_gtv2si3, .Ltmp2090-gen_gtv2si3
	.cfi_endproc

	.globl	gen_umaxv8qi3
	.align	16, 0x90
	.type	gen_umaxv8qi3,@function
gen_umaxv8qi3:                          # @gen_umaxv8qi3
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp2093:
	.cfi_def_cfa_offset 16
.Ltmp2094:
	.cfi_offset %rbx, -16
	movq	%rdx, %rax
	movq	%rsi, %rcx
	movq	%rdi, %rbx
	movq	$95, %rdi
	movq	$39, %rsi
	movq	%rcx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	popq	%rbx
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp2095:
	.size	gen_umaxv8qi3, .Ltmp2095-gen_umaxv8qi3
	.cfi_endproc

	.globl	gen_smaxv4hi3
	.align	16, 0x90
	.type	gen_smaxv4hi3,@function
gen_smaxv4hi3:                          # @gen_smaxv4hi3
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp2098:
	.cfi_def_cfa_offset 16
.Ltmp2099:
	.cfi_offset %rbx, -16
	movq	%rdx, %rax
	movq	%rsi, %rcx
	movq	%rdi, %rbx
	movq	$93, %rdi
	movq	$36, %rsi
	movq	%rcx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	popq	%rbx
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp2100:
	.size	gen_smaxv4hi3, .Ltmp2100-gen_smaxv4hi3
	.cfi_endproc

	.globl	gen_uminv8qi3
	.align	16, 0x90
	.type	gen_uminv8qi3,@function
gen_uminv8qi3:                          # @gen_uminv8qi3
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp2103:
	.cfi_def_cfa_offset 16
.Ltmp2104:
	.cfi_offset %rbx, -16
	movq	%rdx, %rax
	movq	%rsi, %rcx
	movq	%rdi, %rbx
	movq	$94, %rdi
	movq	$39, %rsi
	movq	%rcx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	popq	%rbx
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp2105:
	.size	gen_uminv8qi3, .Ltmp2105-gen_uminv8qi3
	.cfi_endproc

	.globl	gen_sminv4hi3
	.align	16, 0x90
	.type	gen_sminv4hi3,@function
gen_sminv4hi3:                          # @gen_sminv4hi3
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp2108:
	.cfi_def_cfa_offset 16
.Ltmp2109:
	.cfi_offset %rbx, -16
	movq	%rdx, %rax
	movq	%rsi, %rcx
	movq	%rdi, %rbx
	movq	$92, %rdi
	movq	$36, %rsi
	movq	%rcx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	popq	%rbx
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp2110:
	.size	gen_sminv4hi3, .Ltmp2110-gen_sminv4hi3
	.cfi_endproc

	.globl	gen_ashrv4hi3
	.align	16, 0x90
	.type	gen_ashrv4hi3,@function
gen_ashrv4hi3:                          # @gen_ashrv4hi3
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp2113:
	.cfi_def_cfa_offset 16
.Ltmp2114:
	.cfi_offset %rbx, -16
	movq	%rdx, %rax
	movq	%rsi, %rcx
	movq	%rdi, %rbx
	movq	$89, %rdi
	movq	$36, %rsi
	movq	%rcx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	popq	%rbx
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp2115:
	.size	gen_ashrv4hi3, .Ltmp2115-gen_ashrv4hi3
	.cfi_endproc

	.globl	gen_ashrv2si3
	.align	16, 0x90
	.type	gen_ashrv2si3,@function
gen_ashrv2si3:                          # @gen_ashrv2si3
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp2118:
	.cfi_def_cfa_offset 16
.Ltmp2119:
	.cfi_offset %rbx, -16
	movq	%rdx, %rax
	movq	%rsi, %rcx
	movq	%rdi, %rbx
	movq	$89, %rdi
	movq	$33, %rsi
	movq	%rcx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	popq	%rbx
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp2120:
	.size	gen_ashrv2si3, .Ltmp2120-gen_ashrv2si3
	.cfi_endproc

	.globl	gen_lshrv4hi3
	.align	16, 0x90
	.type	gen_lshrv4hi3,@function
gen_lshrv4hi3:                          # @gen_lshrv4hi3
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp2123:
	.cfi_def_cfa_offset 16
.Ltmp2124:
	.cfi_offset %rbx, -16
	movq	%rdx, %rax
	movq	%rsi, %rcx
	movq	%rdi, %rbx
	movq	$90, %rdi
	movq	$36, %rsi
	movq	%rcx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	popq	%rbx
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp2125:
	.size	gen_lshrv4hi3, .Ltmp2125-gen_lshrv4hi3
	.cfi_endproc

	.globl	gen_lshrv2si3
	.align	16, 0x90
	.type	gen_lshrv2si3,@function
gen_lshrv2si3:                          # @gen_lshrv2si3
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp2128:
	.cfi_def_cfa_offset 16
.Ltmp2129:
	.cfi_offset %rbx, -16
	movq	%rdx, %rax
	movq	%rsi, %rcx
	movq	%rdi, %rbx
	movq	$90, %rdi
	movq	$33, %rsi
	movq	%rcx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	popq	%rbx
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp2130:
	.size	gen_lshrv2si3, .Ltmp2130-gen_lshrv2si3
	.cfi_endproc

	.globl	gen_mmx_lshrdi3
	.align	16, 0x90
	.type	gen_mmx_lshrdi3,@function
gen_mmx_lshrdi3:                        # @gen_mmx_lshrdi3
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp2133:
	.cfi_def_cfa_offset 16
.Ltmp2134:
	.cfi_offset %rbx, -16
	movq	%rdx, %rax
	movq	%rsi, %rcx
	movq	%rdi, %rbx
	movq	$90, %rdi
	movq	$5, %rsi
	movq	%rcx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rcx
	movq	$1, %rdi
	xorq	%rax, %rax
	movq	%rcx, %rsi
	callq	gen_rtvec
	movq	$42, %rdi
	movq	$5, %rsi
	movq	$45, %rcx
	movq	%rax, %rdx
	callq	gen_rtx_fmt_Ei
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	popq	%rbx
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp2135:
	.size	gen_mmx_lshrdi3, .Ltmp2135-gen_mmx_lshrdi3
	.cfi_endproc

	.globl	gen_ashlv4hi3
	.align	16, 0x90
	.type	gen_ashlv4hi3,@function
gen_ashlv4hi3:                          # @gen_ashlv4hi3
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp2138:
	.cfi_def_cfa_offset 16
.Ltmp2139:
	.cfi_offset %rbx, -16
	movq	%rdx, %rax
	movq	%rsi, %rcx
	movq	%rdi, %rbx
	movq	$87, %rdi
	movq	$36, %rsi
	movq	%rcx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	popq	%rbx
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp2140:
	.size	gen_ashlv4hi3, .Ltmp2140-gen_ashlv4hi3
	.cfi_endproc

	.globl	gen_ashlv2si3
	.align	16, 0x90
	.type	gen_ashlv2si3,@function
gen_ashlv2si3:                          # @gen_ashlv2si3
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp2143:
	.cfi_def_cfa_offset 16
.Ltmp2144:
	.cfi_offset %rbx, -16
	movq	%rdx, %rax
	movq	%rsi, %rcx
	movq	%rdi, %rbx
	movq	$87, %rdi
	movq	$33, %rsi
	movq	%rcx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	popq	%rbx
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp2145:
	.size	gen_ashlv2si3, .Ltmp2145-gen_ashlv2si3
	.cfi_endproc

	.globl	gen_mmx_ashldi3
	.align	16, 0x90
	.type	gen_mmx_ashldi3,@function
gen_mmx_ashldi3:                        # @gen_mmx_ashldi3
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp2148:
	.cfi_def_cfa_offset 16
.Ltmp2149:
	.cfi_offset %rbx, -16
	movq	%rdx, %rax
	movq	%rsi, %rcx
	movq	%rdi, %rbx
	movq	$87, %rdi
	movq	$5, %rsi
	movq	%rcx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rcx
	movq	$1, %rdi
	xorq	%rax, %rax
	movq	%rcx, %rsi
	callq	gen_rtvec
	movq	$42, %rdi
	movq	$5, %rsi
	movq	$45, %rcx
	movq	%rax, %rdx
	callq	gen_rtx_fmt_Ei
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	popq	%rbx
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp2150:
	.size	gen_mmx_ashldi3, .Ltmp2150-gen_mmx_ashldi3
	.cfi_endproc

	.globl	gen_mmx_packsswb
	.align	16, 0x90
	.type	gen_mmx_packsswb,@function
gen_mmx_packsswb:                       # @gen_mmx_packsswb
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp2155:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp2156:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp2157:
	.cfi_def_cfa_offset 32
.Ltmp2158:
	.cfi_offset %rbx, -32
.Ltmp2159:
	.cfi_offset %r14, -24
.Ltmp2160:
	.cfi_offset %r15, -16
	movq	%rdx, %r15
	movq	%rsi, %rax
	movq	%rdi, %r14
	movq	$150, %rdi
	movq	$35, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rbx
	movq	$150, %rdi
	movq	$35, %rsi
	movq	%r15, %rdx
	callq	gen_rtx_fmt_e
	movq	$144, %rdi
	movq	$39, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	popq	%rbx
	popq	%r14
	popq	%r15
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp2161:
	.size	gen_mmx_packsswb, .Ltmp2161-gen_mmx_packsswb
	.cfi_endproc

	.globl	gen_mmx_packssdw
	.align	16, 0x90
	.type	gen_mmx_packssdw,@function
gen_mmx_packssdw:                       # @gen_mmx_packssdw
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp2166:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp2167:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp2168:
	.cfi_def_cfa_offset 32
.Ltmp2169:
	.cfi_offset %rbx, -32
.Ltmp2170:
	.cfi_offset %r14, -24
.Ltmp2171:
	.cfi_offset %r15, -16
	movq	%rdx, %r15
	movq	%rsi, %rax
	movq	%rdi, %r14
	movq	$150, %rdi
	movq	$32, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rbx
	movq	$150, %rdi
	movq	$32, %rsi
	movq	%r15, %rdx
	callq	gen_rtx_fmt_e
	movq	$144, %rdi
	movq	$36, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	popq	%rbx
	popq	%r14
	popq	%r15
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp2172:
	.size	gen_mmx_packssdw, .Ltmp2172-gen_mmx_packssdw
	.cfi_endproc

	.globl	gen_mmx_packuswb
	.align	16, 0x90
	.type	gen_mmx_packuswb,@function
gen_mmx_packuswb:                       # @gen_mmx_packuswb
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp2177:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp2178:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp2179:
	.cfi_def_cfa_offset 32
.Ltmp2180:
	.cfi_offset %rbx, -32
.Ltmp2181:
	.cfi_offset %r14, -24
.Ltmp2182:
	.cfi_offset %r15, -16
	movq	%rdx, %r15
	movq	%rsi, %rax
	movq	%rdi, %r14
	movq	$151, %rdi
	movq	$35, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rbx
	movq	$151, %rdi
	movq	$35, %rsi
	movq	%r15, %rdx
	callq	gen_rtx_fmt_e
	movq	$144, %rdi
	movq	$39, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	popq	%rbx
	popq	%r14
	popq	%r15
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp2183:
	.size	gen_mmx_packuswb, .Ltmp2183-gen_mmx_packuswb
	.cfi_endproc

	.globl	gen_mmx_punpckhbw
	.align	16, 0x90
	.type	gen_mmx_punpckhbw,@function
gen_mmx_punpckhbw:                      # @gen_mmx_punpckhbw
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp2190:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp2191:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp2192:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp2193:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp2194:
	.cfi_def_cfa_offset 48
	subq	$64, %rsp
.Ltmp2195:
	.cfi_def_cfa_offset 112
.Ltmp2196:
	.cfi_offset %rbx, -48
.Ltmp2197:
	.cfi_offset %r12, -40
.Ltmp2198:
	.cfi_offset %r13, -32
.Ltmp2199:
	.cfi_offset %r14, -24
.Ltmp2200:
	.cfi_offset %r15, -16
	movq	%rdx, 48(%rsp)          # 8-byte Spill
	movq	%rsi, 40(%rsp)          # 8-byte Spill
	movq	%rdi, 56(%rsp)          # 8-byte Spill
	xorq	%rdi, %rdi
	movq	$4, %rsi
	callq	gen_rtx_CONST_INT
	movq	const_int_rtx+512(%rip), %rcx
	movq	%rcx, 32(%rsp)          # 8-byte Spill
	movq	%rax, 24(%rsp)          # 8-byte Spill
	xorq	%rdi, %rdi
	movq	$5, %rsi
	callq	gen_rtx_CONST_INT
	movq	const_int_rtx+520(%rip), %r14
	movq	%rax, %r15
	xorq	%rdi, %rdi
	movq	$6, %rsi
	callq	gen_rtx_CONST_INT
	movq	%rax, %r12
	xorq	%rdi, %rdi
	movq	$2, %rsi
	callq	gen_rtx_CONST_INT
	movq	%rax, %r13
	xorq	%rdi, %rdi
	movq	$7, %rsi
	callq	gen_rtx_CONST_INT
	movq	%rax, %rbx
	xorq	%rdi, %rdi
	movq	$3, %rsi
	callq	gen_rtx_CONST_INT
	movq	%rax, 16(%rsp)
	movq	%rbx, 8(%rsp)
	movq	$8, %rdi
	xorq	%rax, %rax
	movq	%r13, (%rsp)
	movq	24(%rsp), %rsi          # 8-byte Reload
	movq	32(%rsp), %rdx          # 8-byte Reload
	movq	%r15, %rcx
	movq	%r14, %r8
	movq	%r12, %r9
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_E
	movq	$143, %rdi
	movq	$39, %rsi
	movq	40(%rsp), %rdx          # 8-byte Reload
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	const_int_rtx+512(%rip), %rcx
	movq	%rcx, 32(%rsp)          # 8-byte Spill
	movq	%rax, 40(%rsp)          # 8-byte Spill
	xorq	%rdi, %rdi
	movq	$4, %rsi
	callq	gen_rtx_CONST_INT
	movq	const_int_rtx+520(%rip), %rcx
	movq	%rcx, 24(%rsp)          # 8-byte Spill
	movq	%rax, %r15
	xorq	%rdi, %rdi
	movq	$5, %rsi
	callq	gen_rtx_CONST_INT
	movq	%rax, %rbx
	xorq	%rdi, %rdi
	movq	$2, %rsi
	callq	gen_rtx_CONST_INT
	movq	%rax, %r12
	xorq	%rdi, %rdi
	movq	$6, %rsi
	callq	gen_rtx_CONST_INT
	movq	%rax, %r13
	xorq	%rdi, %rdi
	movq	$3, %rsi
	callq	gen_rtx_CONST_INT
	movq	%rax, %r14
	xorq	%rdi, %rdi
	movq	$7, %rsi
	callq	gen_rtx_CONST_INT
	movq	%rax, 16(%rsp)
	movq	%r14, 8(%rsp)
	movq	$8, %rdi
	xorq	%rax, %rax
	movq	%r13, (%rsp)
	movq	32(%rsp), %rsi          # 8-byte Reload
	movq	%r15, %rdx
	movq	24(%rsp), %rcx          # 8-byte Reload
	movq	%rbx, %r8
	movq	%r12, %r9
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_E
	movq	$143, %rdi
	movq	$39, %rsi
	movq	48(%rsp), %rdx          # 8-byte Reload
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rbx
	xorq	%rdi, %rdi
	movq	$85, %rsi
	callq	gen_rtx_CONST_INT
	movq	$142, %rdi
	movq	$39, %rsi
	movq	40(%rsp), %rdx          # 8-byte Reload
	movq	%rbx, %rcx
	movq	%rax, %r8
	callq	gen_rtx_fmt_eee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	56(%rsp), %rdx          # 8-byte Reload
	movq	%rax, %rcx
	addq	$64, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp2201:
	.size	gen_mmx_punpckhbw, .Ltmp2201-gen_mmx_punpckhbw
	.cfi_endproc

	.globl	gen_mmx_punpckhwd
	.align	16, 0x90
	.type	gen_mmx_punpckhwd,@function
gen_mmx_punpckhwd:                      # @gen_mmx_punpckhwd
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp2208:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp2209:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp2210:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp2211:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp2212:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp2213:
	.cfi_def_cfa_offset 64
.Ltmp2214:
	.cfi_offset %rbx, -48
.Ltmp2215:
	.cfi_offset %r12, -40
.Ltmp2216:
	.cfi_offset %r13, -32
.Ltmp2217:
	.cfi_offset %r14, -24
.Ltmp2218:
	.cfi_offset %r15, -16
	movq	const_int_rtx+512(%rip), %r13
	movq	%rdx, (%rsp)            # 8-byte Spill
	movq	%rsi, %r12
	movq	%rdi, 8(%rsp)           # 8-byte Spill
	xorq	%rdi, %rdi
	movq	$2, %rsi
	callq	gen_rtx_CONST_INT
	movq	const_int_rtx+520(%rip), %r15
	movq	%rax, %r14
	xorq	%rdi, %rdi
	movq	$3, %rsi
	callq	gen_rtx_CONST_INT
	movq	%rax, %rbx
	movq	$4, %rdi
	xorq	%rax, %rax
	movq	%r13, %rsi
	movq	%r14, %rdx
	movq	%r15, %rcx
	movq	%rbx, %r8
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_E
	movq	$143, %rdi
	movq	$36, %rsi
	movq	%r12, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %r14
	xorq	%rdi, %rdi
	movq	$2, %rsi
	callq	gen_rtx_CONST_INT
	movq	const_int_rtx+512(%rip), %r15
	movq	%rax, %rbx
	xorq	%rdi, %rdi
	movq	$3, %rsi
	callq	gen_rtx_CONST_INT
	movq	const_int_rtx+520(%rip), %r8
	movq	%rax, %rcx
	movq	$4, %rdi
	xorq	%rax, %rax
	movq	%rbx, %rsi
	movq	%r15, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_E
	movq	$143, %rdi
	movq	$36, %rsi
	movq	(%rsp), %rdx            # 8-byte Reload
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rbx
	xorq	%rdi, %rdi
	movq	$5, %rsi
	callq	gen_rtx_CONST_INT
	movq	$142, %rdi
	movq	$36, %rsi
	movq	%r14, %rdx
	movq	%rbx, %rcx
	movq	%rax, %r8
	callq	gen_rtx_fmt_eee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	8(%rsp), %rdx           # 8-byte Reload
	movq	%rax, %rcx
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp2219:
	.size	gen_mmx_punpckhwd, .Ltmp2219-gen_mmx_punpckhwd
	.cfi_endproc

	.globl	gen_mmx_punpckhdq
	.align	16, 0x90
	.type	gen_mmx_punpckhdq,@function
gen_mmx_punpckhdq:                      # @gen_mmx_punpckhdq
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp2225:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp2226:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp2227:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp2228:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp2229:
	.cfi_def_cfa_offset 48
.Ltmp2230:
	.cfi_offset %rbx, -40
.Ltmp2231:
	.cfi_offset %r12, -32
.Ltmp2232:
	.cfi_offset %r14, -24
.Ltmp2233:
	.cfi_offset %r15, -16
	movq	const_int_rtx+512(%rip), %rcx
	movq	const_int_rtx+520(%rip), %rbx
	movq	%rdx, %r15
	movq	%rsi, %r12
	movq	%rdi, %r14
	movq	$2, %rdi
	xorq	%rax, %rax
	movq	%rcx, %rsi
	movq	%rbx, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_E
	movq	$143, %rdi
	movq	$33, %rsi
	movq	%r12, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	const_int_rtx+520(%rip), %rsi
	movq	const_int_rtx+512(%rip), %rdx
	movq	%rax, %rbx
	movq	$2, %rdi
	xorq	%rax, %rax
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_E
	movq	$143, %rdi
	movq	$33, %rsi
	movq	%r15, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	const_int_rtx+520(%rip), %r8
	movq	$142, %rdi
	movq	$33, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_eee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp2234:
	.size	gen_mmx_punpckhdq, .Ltmp2234-gen_mmx_punpckhdq
	.cfi_endproc

	.globl	gen_mmx_punpcklbw
	.align	16, 0x90
	.type	gen_mmx_punpcklbw,@function
gen_mmx_punpcklbw:                      # @gen_mmx_punpcklbw
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp2241:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp2242:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp2243:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp2244:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp2245:
	.cfi_def_cfa_offset 48
	subq	$64, %rsp
.Ltmp2246:
	.cfi_def_cfa_offset 112
.Ltmp2247:
	.cfi_offset %rbx, -48
.Ltmp2248:
	.cfi_offset %r12, -40
.Ltmp2249:
	.cfi_offset %r13, -32
.Ltmp2250:
	.cfi_offset %r14, -24
.Ltmp2251:
	.cfi_offset %r15, -16
	movq	const_int_rtx+512(%rip), %rax
	movq	%rax, 32(%rsp)          # 8-byte Spill
	movq	%rdx, 48(%rsp)          # 8-byte Spill
	movq	%rsi, 40(%rsp)          # 8-byte Spill
	movq	%rdi, 56(%rsp)          # 8-byte Spill
	xorq	%rdi, %rdi
	movq	$4, %rsi
	callq	gen_rtx_CONST_INT
	movq	const_int_rtx+520(%rip), %rcx
	movq	%rcx, 24(%rsp)          # 8-byte Spill
	movq	%rax, %r14
	xorq	%rdi, %rdi
	movq	$5, %rsi
	callq	gen_rtx_CONST_INT
	movq	%rax, %r15
	xorq	%rdi, %rdi
	movq	$2, %rsi
	callq	gen_rtx_CONST_INT
	movq	%rax, %r12
	xorq	%rdi, %rdi
	movq	$6, %rsi
	callq	gen_rtx_CONST_INT
	movq	%rax, %r13
	xorq	%rdi, %rdi
	movq	$3, %rsi
	callq	gen_rtx_CONST_INT
	movq	%rax, %rbx
	xorq	%rdi, %rdi
	movq	$7, %rsi
	callq	gen_rtx_CONST_INT
	movq	%rax, 16(%rsp)
	movq	%rbx, 8(%rsp)
	movq	$8, %rdi
	xorq	%rax, %rax
	movq	%r13, (%rsp)
	movq	32(%rsp), %rsi          # 8-byte Reload
	movq	%r14, %rdx
	movq	24(%rsp), %rcx          # 8-byte Reload
	movq	%r15, %r8
	movq	%r12, %r9
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_E
	movq	$143, %rdi
	movq	$39, %rsi
	movq	40(%rsp), %rdx          # 8-byte Reload
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, 40(%rsp)          # 8-byte Spill
	xorq	%rdi, %rdi
	movq	$4, %rsi
	callq	gen_rtx_CONST_INT
	movq	const_int_rtx+512(%rip), %rcx
	movq	%rcx, 32(%rsp)          # 8-byte Spill
	movq	%rax, 24(%rsp)          # 8-byte Spill
	xorq	%rdi, %rdi
	movq	$5, %rsi
	callq	gen_rtx_CONST_INT
	movq	const_int_rtx+520(%rip), %r13
	movq	%rax, %rbx
	xorq	%rdi, %rdi
	movq	$6, %rsi
	callq	gen_rtx_CONST_INT
	movq	%rax, %r14
	xorq	%rdi, %rdi
	movq	$2, %rsi
	callq	gen_rtx_CONST_INT
	movq	%rax, %r15
	xorq	%rdi, %rdi
	movq	$7, %rsi
	callq	gen_rtx_CONST_INT
	movq	%rax, %r12
	xorq	%rdi, %rdi
	movq	$3, %rsi
	callq	gen_rtx_CONST_INT
	movq	%rax, 16(%rsp)
	movq	%r12, 8(%rsp)
	movq	$8, %rdi
	xorq	%rax, %rax
	movq	%r15, (%rsp)
	movq	24(%rsp), %rsi          # 8-byte Reload
	movq	32(%rsp), %rdx          # 8-byte Reload
	movq	%rbx, %rcx
	movq	%r13, %r8
	movq	%r14, %r9
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_E
	movq	$143, %rdi
	movq	$39, %rsi
	movq	48(%rsp), %rdx          # 8-byte Reload
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rbx
	xorq	%rdi, %rdi
	movq	$85, %rsi
	callq	gen_rtx_CONST_INT
	movq	$142, %rdi
	movq	$39, %rsi
	movq	40(%rsp), %rdx          # 8-byte Reload
	movq	%rbx, %rcx
	movq	%rax, %r8
	callq	gen_rtx_fmt_eee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	56(%rsp), %rdx          # 8-byte Reload
	movq	%rax, %rcx
	addq	$64, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp2252:
	.size	gen_mmx_punpcklbw, .Ltmp2252-gen_mmx_punpcklbw
	.cfi_endproc

	.globl	gen_mmx_punpcklwd
	.align	16, 0x90
	.type	gen_mmx_punpcklwd,@function
gen_mmx_punpcklwd:                      # @gen_mmx_punpcklwd
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp2259:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp2260:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp2261:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp2262:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp2263:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp2264:
	.cfi_def_cfa_offset 64
.Ltmp2265:
	.cfi_offset %rbx, -48
.Ltmp2266:
	.cfi_offset %r12, -40
.Ltmp2267:
	.cfi_offset %r13, -32
.Ltmp2268:
	.cfi_offset %r14, -24
.Ltmp2269:
	.cfi_offset %r15, -16
	movq	%rdx, (%rsp)            # 8-byte Spill
	movq	%rsi, %r12
	movq	%rdi, 8(%rsp)           # 8-byte Spill
	xorq	%rdi, %rdi
	movq	$2, %rsi
	callq	gen_rtx_CONST_INT
	movq	const_int_rtx+512(%rip), %r13
	movq	%rax, %rbx
	xorq	%rdi, %rdi
	movq	$3, %rsi
	callq	gen_rtx_CONST_INT
	movq	const_int_rtx+520(%rip), %r8
	movq	%rax, %rcx
	movq	$4, %rdi
	xorq	%rax, %rax
	movq	%rbx, %rsi
	movq	%r13, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_E
	movq	$143, %rdi
	movq	$36, %rsi
	movq	%r12, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	const_int_rtx+512(%rip), %r13
	movq	%rax, %r12
	xorq	%rdi, %rdi
	movq	$2, %rsi
	callq	gen_rtx_CONST_INT
	movq	const_int_rtx+520(%rip), %r15
	movq	%rax, %r14
	xorq	%rdi, %rdi
	movq	$3, %rsi
	callq	gen_rtx_CONST_INT
	movq	%rax, %rbx
	movq	$4, %rdi
	xorq	%rax, %rax
	movq	%r13, %rsi
	movq	%r14, %rdx
	movq	%r15, %rcx
	movq	%rbx, %r8
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_E
	movq	$143, %rdi
	movq	$36, %rsi
	movq	(%rsp), %rdx            # 8-byte Reload
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rbx
	xorq	%rdi, %rdi
	movq	$5, %rsi
	callq	gen_rtx_CONST_INT
	movq	$142, %rdi
	movq	$36, %rsi
	movq	%r12, %rdx
	movq	%rbx, %rcx
	movq	%rax, %r8
	callq	gen_rtx_fmt_eee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	8(%rsp), %rdx           # 8-byte Reload
	movq	%rax, %rcx
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp2270:
	.size	gen_mmx_punpcklwd, .Ltmp2270-gen_mmx_punpcklwd
	.cfi_endproc

	.globl	gen_mmx_punpckldq
	.align	16, 0x90
	.type	gen_mmx_punpckldq,@function
gen_mmx_punpckldq:                      # @gen_mmx_punpckldq
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp2276:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp2277:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp2278:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp2279:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp2280:
	.cfi_def_cfa_offset 48
.Ltmp2281:
	.cfi_offset %rbx, -40
.Ltmp2282:
	.cfi_offset %r12, -32
.Ltmp2283:
	.cfi_offset %r14, -24
.Ltmp2284:
	.cfi_offset %r15, -16
	movq	const_int_rtx+520(%rip), %rcx
	movq	const_int_rtx+512(%rip), %rbx
	movq	%rdx, %r15
	movq	%rsi, %r12
	movq	%rdi, %r14
	movq	$2, %rdi
	xorq	%rax, %rax
	movq	%rcx, %rsi
	movq	%rbx, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_E
	movq	$143, %rdi
	movq	$33, %rsi
	movq	%r12, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	const_int_rtx+512(%rip), %rsi
	movq	const_int_rtx+520(%rip), %rdx
	movq	%rax, %rbx
	movq	$2, %rdi
	xorq	%rax, %rax
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_E
	movq	$143, %rdi
	movq	$33, %rsi
	movq	%r15, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	const_int_rtx+520(%rip), %r8
	movq	$142, %rdi
	movq	$33, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_eee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp2285:
	.size	gen_mmx_punpckldq, .Ltmp2285-gen_mmx_punpckldq
	.cfi_endproc

	.globl	gen_emms
	.align	16, 0x90
	.type	gen_emms,@function
gen_emms:                               # @gen_emms
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp2292:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp2293:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp2294:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp2295:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp2296:
	.cfi_def_cfa_offset 48
	subq	$192, %rsp
.Ltmp2297:
	.cfi_def_cfa_offset 240
.Ltmp2298:
	.cfi_offset %rbx, -48
.Ltmp2299:
	.cfi_offset %r12, -40
.Ltmp2300:
	.cfi_offset %r13, -32
.Ltmp2301:
	.cfi_offset %r14, -24
.Ltmp2302:
	.cfi_offset %r15, -16
	movq	const_int_rtx+512(%rip), %rsi
	movq	$1, %rdi
	xorq	%rax, %rax
	callq	gen_rtvec
	movq	$43, %rdi
	xorq	%rsi, %rsi
	movq	$31, %rcx
	movq	%rax, %rdx
	callq	gen_rtx_fmt_Ei
	movq	%rax, 184(%rsp)         # 8-byte Spill
	movq	$17, %rdi
	movq	$8, %rsi
	callq	gen_rtx_REG
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, 176(%rsp)         # 8-byte Spill
	movq	$17, %rdi
	movq	$9, %rsi
	callq	gen_rtx_REG
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, 168(%rsp)         # 8-byte Spill
	movq	$17, %rdi
	movq	$10, %rsi
	callq	gen_rtx_REG
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, 160(%rsp)         # 8-byte Spill
	movq	$17, %rdi
	movq	$11, %rsi
	callq	gen_rtx_REG
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, 152(%rsp)         # 8-byte Spill
	movq	$17, %rdi
	movq	$12, %rsi
	callq	gen_rtx_REG
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, 144(%rsp)         # 8-byte Spill
	movq	$17, %rdi
	movq	$13, %rsi
	callq	gen_rtx_REG
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, 136(%rsp)         # 8-byte Spill
	movq	$17, %rdi
	movq	$14, %rsi
	callq	gen_rtx_REG
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, 128(%rsp)         # 8-byte Spill
	movq	$17, %rdi
	movq	$15, %rsi
	callq	gen_rtx_REG
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, 120(%rsp)         # 8-byte Spill
	movq	$5, %rdi
	movq	$29, %rsi
	callq	gen_rtx_REG
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, 112(%rsp)         # 8-byte Spill
	movq	$5, %rdi
	movq	$30, %rsi
	callq	gen_rtx_REG
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, 104(%rsp)         # 8-byte Spill
	movq	$5, %rdi
	movq	$31, %rsi
	callq	gen_rtx_REG
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %r15
	movq	$5, %rdi
	movq	$32, %rsi
	callq	gen_rtx_REG
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %r12
	movq	$5, %rdi
	movq	$33, %rsi
	callq	gen_rtx_REG
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %r13
	movq	$5, %rdi
	movq	$34, %rsi
	callq	gen_rtx_REG
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rbx
	movq	$5, %rdi
	movq	$35, %rsi
	callq	gen_rtx_REG
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %r14
	movq	$5, %rdi
	movq	$36, %rsi
	callq	gen_rtx_REG
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, 88(%rsp)
	movq	%r14, 80(%rsp)
	movq	%rbx, 72(%rsp)
	movq	%r13, 64(%rsp)
	movq	%r12, 56(%rsp)
	movq	%r15, 48(%rsp)
	movq	104(%rsp), %rax         # 8-byte Reload
	movq	%rax, 40(%rsp)
	movq	112(%rsp), %rax         # 8-byte Reload
	movq	%rax, 32(%rsp)
	movq	120(%rsp), %rax         # 8-byte Reload
	movq	%rax, 24(%rsp)
	movq	128(%rsp), %rax         # 8-byte Reload
	movq	%rax, 16(%rsp)
	movq	136(%rsp), %rax         # 8-byte Reload
	movq	%rax, 8(%rsp)
	movq	$17, %rdi
	xorq	%rax, %rax
	movq	144(%rsp), %rcx         # 8-byte Reload
	movq	%rcx, (%rsp)
	movq	184(%rsp), %rsi         # 8-byte Reload
	movq	176(%rsp), %rdx         # 8-byte Reload
	movq	168(%rsp), %rcx         # 8-byte Reload
	movq	160(%rsp), %r8          # 8-byte Reload
	movq	152(%rsp), %r9          # 8-byte Reload
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	addq	$192, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	gen_rtx_fmt_E  # TAILCALL
.Ltmp2303:
	.size	gen_emms, .Ltmp2303-gen_emms
	.cfi_endproc

	.globl	gen_ldmxcsr
	.align	16, 0x90
	.type	gen_ldmxcsr,@function
gen_ldmxcsr:                            # @gen_ldmxcsr
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rax
.Ltmp2305:
	.cfi_def_cfa_offset 16
	movq	%rdi, %rcx
	movq	$1, %rdi
	xorq	%rax, %rax
	movq	%rcx, %rsi
	callq	gen_rtvec
	movq	$43, %rdi
	xorq	%rsi, %rsi
	movq	$37, %rcx
	movq	%rax, %rdx
	popq	%rax
	jmp	gen_rtx_fmt_Ei  # TAILCALL
.Ltmp2306:
	.size	gen_ldmxcsr, .Ltmp2306-gen_ldmxcsr
	.cfi_endproc

	.globl	gen_stmxcsr
	.align	16, 0x90
	.type	gen_stmxcsr,@function
gen_stmxcsr:                            # @gen_stmxcsr
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp2309:
	.cfi_def_cfa_offset 16
.Ltmp2310:
	.cfi_offset %rbx, -16
	movq	const_int_rtx+512(%rip), %rsi
	movq	%rdi, %rbx
	movq	$1, %rdi
	xorq	%rax, %rax
	callq	gen_rtvec
	movq	$43, %rdi
	movq	$4, %rsi
	movq	$40, %rcx
	movq	%rax, %rdx
	callq	gen_rtx_fmt_Ei
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	popq	%rbx
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp2311:
	.size	gen_stmxcsr, .Ltmp2311-gen_stmxcsr
	.cfi_endproc

	.globl	gen_addv2sf3
	.align	16, 0x90
	.type	gen_addv2sf3,@function
gen_addv2sf3:                           # @gen_addv2sf3
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp2314:
	.cfi_def_cfa_offset 16
.Ltmp2315:
	.cfi_offset %rbx, -16
	movq	%rdx, %rax
	movq	%rsi, %rcx
	movq	%rdi, %rbx
	movq	$75, %rdi
	movq	$44, %rsi
	movq	%rcx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	popq	%rbx
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp2316:
	.size	gen_addv2sf3, .Ltmp2316-gen_addv2sf3
	.cfi_endproc

	.globl	gen_subv2sf3
	.align	16, 0x90
	.type	gen_subv2sf3,@function
gen_subv2sf3:                           # @gen_subv2sf3
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp2319:
	.cfi_def_cfa_offset 16
.Ltmp2320:
	.cfi_offset %rbx, -16
	movq	%rdx, %rax
	movq	%rsi, %rcx
	movq	%rdi, %rbx
	movq	$76, %rdi
	movq	$44, %rsi
	movq	%rcx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	popq	%rbx
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp2321:
	.size	gen_subv2sf3, .Ltmp2321-gen_subv2sf3
	.cfi_endproc

	.globl	gen_subrv2sf3
	.align	16, 0x90
	.type	gen_subrv2sf3,@function
gen_subrv2sf3:                          # @gen_subrv2sf3
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp2324:
	.cfi_def_cfa_offset 16
.Ltmp2325:
	.cfi_offset %rbx, -16
	movq	%rsi, %rax
	movq	%rdi, %rbx
	movq	$76, %rdi
	movq	$44, %rsi
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	popq	%rbx
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp2326:
	.size	gen_subrv2sf3, .Ltmp2326-gen_subrv2sf3
	.cfi_endproc

	.globl	gen_gtv2sf3
	.align	16, 0x90
	.type	gen_gtv2sf3,@function
gen_gtv2sf3:                            # @gen_gtv2sf3
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp2329:
	.cfi_def_cfa_offset 16
.Ltmp2330:
	.cfi_offset %rbx, -16
	movq	%rdx, %rax
	movq	%rsi, %rcx
	movq	%rdi, %rbx
	movq	$105, %rdi
	movq	$33, %rsi
	movq	%rcx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	popq	%rbx
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp2331:
	.size	gen_gtv2sf3, .Ltmp2331-gen_gtv2sf3
	.cfi_endproc

	.globl	gen_gev2sf3
	.align	16, 0x90
	.type	gen_gev2sf3,@function
gen_gev2sf3:                            # @gen_gev2sf3
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp2334:
	.cfi_def_cfa_offset 16
.Ltmp2335:
	.cfi_offset %rbx, -16
	movq	%rdx, %rax
	movq	%rsi, %rcx
	movq	%rdi, %rbx
	movq	$104, %rdi
	movq	$33, %rsi
	movq	%rcx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	popq	%rbx
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp2336:
	.size	gen_gev2sf3, .Ltmp2336-gen_gev2sf3
	.cfi_endproc

	.globl	gen_eqv2sf3
	.align	16, 0x90
	.type	gen_eqv2sf3,@function
gen_eqv2sf3:                            # @gen_eqv2sf3
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp2339:
	.cfi_def_cfa_offset 16
.Ltmp2340:
	.cfi_offset %rbx, -16
	movq	%rdx, %rax
	movq	%rsi, %rcx
	movq	%rdi, %rbx
	movq	$103, %rdi
	movq	$33, %rsi
	movq	%rcx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	popq	%rbx
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp2341:
	.size	gen_eqv2sf3, .Ltmp2341-gen_eqv2sf3
	.cfi_endproc

	.globl	gen_pfmaxv2sf3
	.align	16, 0x90
	.type	gen_pfmaxv2sf3,@function
gen_pfmaxv2sf3:                         # @gen_pfmaxv2sf3
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp2344:
	.cfi_def_cfa_offset 16
.Ltmp2345:
	.cfi_offset %rbx, -16
	movq	%rdx, %rax
	movq	%rsi, %rcx
	movq	%rdi, %rbx
	movq	$93, %rdi
	movq	$44, %rsi
	movq	%rcx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	popq	%rbx
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp2346:
	.size	gen_pfmaxv2sf3, .Ltmp2346-gen_pfmaxv2sf3
	.cfi_endproc

	.globl	gen_pfminv2sf3
	.align	16, 0x90
	.type	gen_pfminv2sf3,@function
gen_pfminv2sf3:                         # @gen_pfminv2sf3
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp2349:
	.cfi_def_cfa_offset 16
.Ltmp2350:
	.cfi_offset %rbx, -16
	movq	%rdx, %rax
	movq	%rsi, %rcx
	movq	%rdi, %rbx
	movq	$92, %rdi
	movq	$44, %rsi
	movq	%rcx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	popq	%rbx
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp2351:
	.size	gen_pfminv2sf3, .Ltmp2351-gen_pfminv2sf3
	.cfi_endproc

	.globl	gen_mulv2sf3
	.align	16, 0x90
	.type	gen_mulv2sf3,@function
gen_mulv2sf3:                           # @gen_mulv2sf3
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp2354:
	.cfi_def_cfa_offset 16
.Ltmp2355:
	.cfi_offset %rbx, -16
	movq	%rdx, %rax
	movq	%rsi, %rcx
	movq	%rdi, %rbx
	movq	$78, %rdi
	movq	$44, %rsi
	movq	%rcx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	popq	%rbx
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp2356:
	.size	gen_mulv2sf3, .Ltmp2356-gen_mulv2sf3
	.cfi_endproc

	.globl	gen_femms
	.align	16, 0x90
	.type	gen_femms,@function
gen_femms:                              # @gen_femms
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp2363:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp2364:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp2365:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp2366:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp2367:
	.cfi_def_cfa_offset 48
	subq	$192, %rsp
.Ltmp2368:
	.cfi_def_cfa_offset 240
.Ltmp2369:
	.cfi_offset %rbx, -48
.Ltmp2370:
	.cfi_offset %r12, -40
.Ltmp2371:
	.cfi_offset %r13, -32
.Ltmp2372:
	.cfi_offset %r14, -24
.Ltmp2373:
	.cfi_offset %r15, -16
	movq	const_int_rtx+512(%rip), %rsi
	movq	$1, %rdi
	xorq	%rax, %rax
	callq	gen_rtvec
	movq	$43, %rdi
	xorq	%rsi, %rsi
	movq	$46, %rcx
	movq	%rax, %rdx
	callq	gen_rtx_fmt_Ei
	movq	%rax, 184(%rsp)         # 8-byte Spill
	movq	$17, %rdi
	movq	$8, %rsi
	callq	gen_rtx_REG
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, 176(%rsp)         # 8-byte Spill
	movq	$17, %rdi
	movq	$9, %rsi
	callq	gen_rtx_REG
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, 168(%rsp)         # 8-byte Spill
	movq	$17, %rdi
	movq	$10, %rsi
	callq	gen_rtx_REG
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, 160(%rsp)         # 8-byte Spill
	movq	$17, %rdi
	movq	$11, %rsi
	callq	gen_rtx_REG
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, 152(%rsp)         # 8-byte Spill
	movq	$17, %rdi
	movq	$12, %rsi
	callq	gen_rtx_REG
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, 144(%rsp)         # 8-byte Spill
	movq	$17, %rdi
	movq	$13, %rsi
	callq	gen_rtx_REG
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, 136(%rsp)         # 8-byte Spill
	movq	$17, %rdi
	movq	$14, %rsi
	callq	gen_rtx_REG
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, 128(%rsp)         # 8-byte Spill
	movq	$17, %rdi
	movq	$15, %rsi
	callq	gen_rtx_REG
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, 120(%rsp)         # 8-byte Spill
	movq	$5, %rdi
	movq	$29, %rsi
	callq	gen_rtx_REG
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, 112(%rsp)         # 8-byte Spill
	movq	$5, %rdi
	movq	$30, %rsi
	callq	gen_rtx_REG
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, 104(%rsp)         # 8-byte Spill
	movq	$5, %rdi
	movq	$31, %rsi
	callq	gen_rtx_REG
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %r15
	movq	$5, %rdi
	movq	$32, %rsi
	callq	gen_rtx_REG
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %r12
	movq	$5, %rdi
	movq	$33, %rsi
	callq	gen_rtx_REG
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %r13
	movq	$5, %rdi
	movq	$34, %rsi
	callq	gen_rtx_REG
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rbx
	movq	$5, %rdi
	movq	$35, %rsi
	callq	gen_rtx_REG
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %r14
	movq	$5, %rdi
	movq	$36, %rsi
	callq	gen_rtx_REG
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, 88(%rsp)
	movq	%r14, 80(%rsp)
	movq	%rbx, 72(%rsp)
	movq	%r13, 64(%rsp)
	movq	%r12, 56(%rsp)
	movq	%r15, 48(%rsp)
	movq	104(%rsp), %rax         # 8-byte Reload
	movq	%rax, 40(%rsp)
	movq	112(%rsp), %rax         # 8-byte Reload
	movq	%rax, 32(%rsp)
	movq	120(%rsp), %rax         # 8-byte Reload
	movq	%rax, 24(%rsp)
	movq	128(%rsp), %rax         # 8-byte Reload
	movq	%rax, 16(%rsp)
	movq	136(%rsp), %rax         # 8-byte Reload
	movq	%rax, 8(%rsp)
	movq	$17, %rdi
	xorq	%rax, %rax
	movq	144(%rsp), %rcx         # 8-byte Reload
	movq	%rcx, (%rsp)
	movq	184(%rsp), %rsi         # 8-byte Reload
	movq	176(%rsp), %rdx         # 8-byte Reload
	movq	168(%rsp), %rcx         # 8-byte Reload
	movq	160(%rsp), %r8          # 8-byte Reload
	movq	152(%rsp), %r9          # 8-byte Reload
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	addq	$192, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	gen_rtx_fmt_E  # TAILCALL
.Ltmp2374:
	.size	gen_femms, .Ltmp2374-gen_femms
	.cfi_endproc

	.globl	gen_pf2id
	.align	16, 0x90
	.type	gen_pf2id,@function
gen_pf2id:                              # @gen_pf2id
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp2377:
	.cfi_def_cfa_offset 16
.Ltmp2378:
	.cfi_offset %rbx, -16
	movq	%rsi, %rax
	movq	%rdi, %rbx
	movq	$126, %rdi
	movq	$33, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	popq	%rbx
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp2379:
	.size	gen_pf2id, .Ltmp2379-gen_pf2id
	.cfi_endproc

	.globl	gen_pf2iw
	.align	16, 0x90
	.type	gen_pf2iw,@function
gen_pf2iw:                              # @gen_pf2iw
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp2382:
	.cfi_def_cfa_offset 16
.Ltmp2383:
	.cfi_offset %rbx, -16
	movq	%rsi, %rax
	movq	%rdi, %rbx
	movq	$126, %rdi
	movq	$33, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	$150, %rdi
	movq	$32, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	$120, %rdi
	movq	$33, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	popq	%rbx
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp2384:
	.size	gen_pf2iw, .Ltmp2384-gen_pf2iw
	.cfi_endproc

	.globl	gen_pfacc
	.align	16, 0x90
	.type	gen_pfacc,@function
gen_pfacc:                              # @gen_pfacc
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp2390:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp2391:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp2392:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp2393:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp2394:
	.cfi_def_cfa_offset 48
.Ltmp2395:
	.cfi_offset %rbx, -40
.Ltmp2396:
	.cfi_offset %r12, -32
.Ltmp2397:
	.cfi_offset %r14, -24
.Ltmp2398:
	.cfi_offset %r15, -16
	movq	const_int_rtx+512(%rip), %rcx
	movq	%rdx, %r15
	movq	%rsi, %rbx
	movq	%rdi, %r14
	movq	$1, %rdi
	xorq	%rax, %rax
	movq	%rcx, %rsi
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_E
	movq	$143, %rdi
	movq	$15, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	const_int_rtx+520(%rip), %rsi
	movq	%rax, %r12
	movq	$1, %rdi
	xorq	%rax, %rax
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_E
	movq	$143, %rdi
	movq	$15, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$75, %rdi
	movq	$15, %rsi
	movq	%r12, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	const_int_rtx+512(%rip), %rsi
	movq	%rax, %r12
	movq	$1, %rdi
	xorq	%rax, %rax
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_E
	movq	$143, %rdi
	movq	$15, %rsi
	movq	%r15, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	const_int_rtx+520(%rip), %rsi
	movq	%rax, %rbx
	movq	$1, %rdi
	xorq	%rax, %rax
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_E
	movq	$143, %rdi
	movq	$15, %rsi
	movq	%r15, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$75, %rdi
	movq	$15, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$144, %rdi
	movq	$44, %rsi
	movq	%r12, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp2399:
	.size	gen_pfacc, .Ltmp2399-gen_pfacc
	.cfi_endproc

	.globl	gen_pfnacc
	.align	16, 0x90
	.type	gen_pfnacc,@function
gen_pfnacc:                             # @gen_pfnacc
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp2405:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp2406:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp2407:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp2408:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp2409:
	.cfi_def_cfa_offset 48
.Ltmp2410:
	.cfi_offset %rbx, -40
.Ltmp2411:
	.cfi_offset %r12, -32
.Ltmp2412:
	.cfi_offset %r14, -24
.Ltmp2413:
	.cfi_offset %r15, -16
	movq	const_int_rtx+512(%rip), %rcx
	movq	%rdx, %r15
	movq	%rsi, %rbx
	movq	%rdi, %r14
	movq	$1, %rdi
	xorq	%rax, %rax
	movq	%rcx, %rsi
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_E
	movq	$143, %rdi
	movq	$15, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	const_int_rtx+520(%rip), %rsi
	movq	%rax, %r12
	movq	$1, %rdi
	xorq	%rax, %rax
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_E
	movq	$143, %rdi
	movq	$15, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$76, %rdi
	movq	$15, %rsi
	movq	%r12, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	const_int_rtx+512(%rip), %rsi
	movq	%rax, %r12
	movq	$1, %rdi
	xorq	%rax, %rax
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_E
	movq	$143, %rdi
	movq	$15, %rsi
	movq	%r15, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	const_int_rtx+520(%rip), %rsi
	movq	%rax, %rbx
	movq	$1, %rdi
	xorq	%rax, %rax
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_E
	movq	$143, %rdi
	movq	$15, %rsi
	movq	%r15, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$76, %rdi
	movq	$15, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$144, %rdi
	movq	$44, %rsi
	movq	%r12, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp2414:
	.size	gen_pfnacc, .Ltmp2414-gen_pfnacc
	.cfi_endproc

	.globl	gen_pfpnacc
	.align	16, 0x90
	.type	gen_pfpnacc,@function
gen_pfpnacc:                            # @gen_pfpnacc
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp2420:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp2421:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp2422:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp2423:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp2424:
	.cfi_def_cfa_offset 48
.Ltmp2425:
	.cfi_offset %rbx, -40
.Ltmp2426:
	.cfi_offset %r12, -32
.Ltmp2427:
	.cfi_offset %r14, -24
.Ltmp2428:
	.cfi_offset %r15, -16
	movq	const_int_rtx+512(%rip), %rcx
	movq	%rdx, %r12
	movq	%rsi, %rbx
	movq	%rdi, %r14
	movq	$1, %rdi
	xorq	%rax, %rax
	movq	%rcx, %rsi
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_E
	movq	$143, %rdi
	movq	$15, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	const_int_rtx+520(%rip), %rsi
	movq	%rax, %r15
	movq	$1, %rdi
	xorq	%rax, %rax
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_E
	movq	$143, %rdi
	movq	$15, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$76, %rdi
	movq	$15, %rsi
	movq	%r15, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	const_int_rtx+512(%rip), %rsi
	movq	%rax, %r15
	movq	$1, %rdi
	xorq	%rax, %rax
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_E
	movq	$143, %rdi
	movq	$15, %rsi
	movq	%r12, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	const_int_rtx+520(%rip), %rsi
	movq	%rax, %rbx
	movq	$1, %rdi
	xorq	%rax, %rax
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_E
	movq	$143, %rdi
	movq	$15, %rsi
	movq	%r12, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$75, %rdi
	movq	$15, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$144, %rdi
	movq	$44, %rsi
	movq	%r15, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp2429:
	.size	gen_pfpnacc, .Ltmp2429-gen_pfpnacc
	.cfi_endproc

	.globl	gen_pi2fw
	.align	16, 0x90
	.type	gen_pi2fw,@function
gen_pi2fw:                              # @gen_pi2fw
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp2434:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp2435:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp2436:
	.cfi_def_cfa_offset 32
.Ltmp2437:
	.cfi_offset %rbx, -32
.Ltmp2438:
	.cfi_offset %r14, -24
.Ltmp2439:
	.cfi_offset %r15, -16
	movq	const_int_rtx+512(%rip), %rcx
	movq	%rsi, %rbx
	movq	%rdi, %r14
	movq	$1, %rdi
	xorq	%rax, %rax
	movq	%rcx, %rsi
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_E
	movq	$143, %rdi
	movq	$4, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$122, %rdi
	movq	$3, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	$120, %rdi
	movq	$4, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	const_int_rtx+520(%rip), %rsi
	movq	%rax, %r15
	movq	$1, %rdi
	xorq	%rax, %rax
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_E
	movq	$143, %rdi
	movq	$4, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$122, %rdi
	movq	$3, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	$120, %rdi
	movq	$4, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	$144, %rdi
	movq	$33, %rsi
	movq	%r15, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$125, %rdi
	movq	$44, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	popq	%rbx
	popq	%r14
	popq	%r15
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp2440:
	.size	gen_pi2fw, .Ltmp2440-gen_pi2fw
	.cfi_endproc

	.globl	gen_floatv2si2
	.align	16, 0x90
	.type	gen_floatv2si2,@function
gen_floatv2si2:                         # @gen_floatv2si2
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp2443:
	.cfi_def_cfa_offset 16
.Ltmp2444:
	.cfi_offset %rbx, -16
	movq	%rsi, %rax
	movq	%rdi, %rbx
	movq	$125, %rdi
	movq	$44, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	popq	%rbx
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp2445:
	.size	gen_floatv2si2, .Ltmp2445-gen_floatv2si2
	.cfi_endproc

	.globl	gen_pavgusb
	.align	16, 0x90
	.type	gen_pavgusb,@function
gen_pavgusb:                            # @gen_pavgusb
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp2448:
	.cfi_def_cfa_offset 16
.Ltmp2449:
	.cfi_offset %rbx, -16
	movq	%rdi, %rbx
	movq	$2, %rdi
	xorq	%rax, %rax
	callq	gen_rtvec
	movq	$42, %rdi
	movq	$39, %rsi
	movq	$49, %rcx
	movq	%rax, %rdx
	callq	gen_rtx_fmt_Ei
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	popq	%rbx
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp2450:
	.size	gen_pavgusb, .Ltmp2450-gen_pavgusb
	.cfi_endproc

	.globl	gen_pfrcpv2sf2
	.align	16, 0x90
	.type	gen_pfrcpv2sf2,@function
gen_pfrcpv2sf2:                         # @gen_pfrcpv2sf2
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp2453:
	.cfi_def_cfa_offset 16
.Ltmp2454:
	.cfi_offset %rbx, -16
	movq	%rdi, %rbx
	movq	$1, %rdi
	xorq	%rax, %rax
	callq	gen_rtvec
	movq	$42, %rdi
	movq	$44, %rsi
	movq	$50, %rcx
	movq	%rax, %rdx
	callq	gen_rtx_fmt_Ei
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	popq	%rbx
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp2455:
	.size	gen_pfrcpv2sf2, .Ltmp2455-gen_pfrcpv2sf2
	.cfi_endproc

	.globl	gen_pfrcpit1v2sf3
	.align	16, 0x90
	.type	gen_pfrcpit1v2sf3,@function
gen_pfrcpit1v2sf3:                      # @gen_pfrcpit1v2sf3
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp2458:
	.cfi_def_cfa_offset 16
.Ltmp2459:
	.cfi_offset %rbx, -16
	movq	%rdi, %rbx
	movq	$2, %rdi
	xorq	%rax, %rax
	callq	gen_rtvec
	movq	$42, %rdi
	movq	$44, %rsi
	movq	$51, %rcx
	movq	%rax, %rdx
	callq	gen_rtx_fmt_Ei
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	popq	%rbx
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp2460:
	.size	gen_pfrcpit1v2sf3, .Ltmp2460-gen_pfrcpit1v2sf3
	.cfi_endproc

	.globl	gen_pfrcpit2v2sf3
	.align	16, 0x90
	.type	gen_pfrcpit2v2sf3,@function
gen_pfrcpit2v2sf3:                      # @gen_pfrcpit2v2sf3
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp2463:
	.cfi_def_cfa_offset 16
.Ltmp2464:
	.cfi_offset %rbx, -16
	movq	%rdi, %rbx
	movq	$2, %rdi
	xorq	%rax, %rax
	callq	gen_rtvec
	movq	$42, %rdi
	movq	$44, %rsi
	movq	$52, %rcx
	movq	%rax, %rdx
	callq	gen_rtx_fmt_Ei
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	popq	%rbx
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp2465:
	.size	gen_pfrcpit2v2sf3, .Ltmp2465-gen_pfrcpit2v2sf3
	.cfi_endproc

	.globl	gen_pfrsqrtv2sf2
	.align	16, 0x90
	.type	gen_pfrsqrtv2sf2,@function
gen_pfrsqrtv2sf2:                       # @gen_pfrsqrtv2sf2
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp2468:
	.cfi_def_cfa_offset 16
.Ltmp2469:
	.cfi_offset %rbx, -16
	movq	%rdi, %rbx
	movq	$1, %rdi
	xorq	%rax, %rax
	callq	gen_rtvec
	movq	$42, %rdi
	movq	$44, %rsi
	movq	$53, %rcx
	movq	%rax, %rdx
	callq	gen_rtx_fmt_Ei
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	popq	%rbx
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp2470:
	.size	gen_pfrsqrtv2sf2, .Ltmp2470-gen_pfrsqrtv2sf2
	.cfi_endproc

	.globl	gen_pfrsqit1v2sf3
	.align	16, 0x90
	.type	gen_pfrsqit1v2sf3,@function
gen_pfrsqit1v2sf3:                      # @gen_pfrsqit1v2sf3
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp2473:
	.cfi_def_cfa_offset 16
.Ltmp2474:
	.cfi_offset %rbx, -16
	movq	%rdi, %rbx
	movq	$2, %rdi
	xorq	%rax, %rax
	callq	gen_rtvec
	movq	$42, %rdi
	movq	$44, %rsi
	movq	$54, %rcx
	movq	%rax, %rdx
	callq	gen_rtx_fmt_Ei
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	popq	%rbx
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp2475:
	.size	gen_pfrsqit1v2sf3, .Ltmp2475-gen_pfrsqit1v2sf3
	.cfi_endproc

	.globl	gen_pmulhrwv4hi3
	.align	16, 0x90
	.type	gen_pmulhrwv4hi3,@function
gen_pmulhrwv4hi3:                       # @gen_pmulhrwv4hi3
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp2482:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp2483:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp2484:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp2485:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp2486:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp2487:
	.cfi_def_cfa_offset 64
.Ltmp2488:
	.cfi_offset %rbx, -48
.Ltmp2489:
	.cfi_offset %r12, -40
.Ltmp2490:
	.cfi_offset %r13, -32
.Ltmp2491:
	.cfi_offset %r14, -24
.Ltmp2492:
	.cfi_offset %r15, -16
	movq	%rdx, %r15
	movq	%rsi, %rax
	movq	%rdi, 8(%rsp)           # 8-byte Spill
	movq	$120, %rdi
	movq	$37, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rbx
	movq	$120, %rdi
	movq	$37, %rsi
	movq	%r15, %rdx
	callq	gen_rtx_fmt_e
	movq	$78, %rdi
	movq	$37, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %r15
	xorq	%rdi, %rdi
	movq	$32768, %rsi            # imm = 0x8000
	callq	gen_rtx_CONST_INT
	movq	%rax, %r12
	xorq	%rdi, %rdi
	movq	$32768, %rsi            # imm = 0x8000
	callq	gen_rtx_CONST_INT
	movq	%rax, %r13
	xorq	%rdi, %rdi
	movq	$32768, %rsi            # imm = 0x8000
	callq	gen_rtx_CONST_INT
	movq	%rax, %r14
	xorq	%rdi, %rdi
	movq	$32768, %rsi            # imm = 0x8000
	callq	gen_rtx_CONST_INT
	movq	%rax, %rbx
	movq	$4, %rdi
	xorq	%rax, %rax
	movq	%r12, %rsi
	movq	%r13, %rdx
	movq	%r14, %rcx
	movq	%rbx, %r8
	callq	gen_rtvec
	movq	$56, %rdi
	movq	$37, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_E
	movq	$75, %rdi
	movq	$37, %rsi
	movq	%r15, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rbx
	xorq	%rdi, %rdi
	movq	$16, %rsi
	callq	gen_rtx_CONST_INT
	movq	$90, %rdi
	movq	$37, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$122, %rdi
	movq	$36, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	8(%rsp), %rdx           # 8-byte Reload
	movq	%rax, %rcx
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp2493:
	.size	gen_pmulhrwv4hi3, .Ltmp2493-gen_pmulhrwv4hi3
	.cfi_endproc

	.globl	gen_pswapdv2si2
	.align	16, 0x90
	.type	gen_pswapdv2si2,@function
gen_pswapdv2si2:                        # @gen_pswapdv2si2
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp2497:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp2498:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp2499:
	.cfi_def_cfa_offset 32
.Ltmp2500:
	.cfi_offset %rbx, -24
.Ltmp2501:
	.cfi_offset %r14, -16
	movq	const_int_rtx+520(%rip), %rcx
	movq	const_int_rtx+512(%rip), %rdx
	movq	%rsi, %rbx
	movq	%rdi, %r14
	movq	$2, %rdi
	xorq	%rax, %rax
	movq	%rcx, %rsi
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_E
	movq	$143, %rdi
	movq	$33, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp2502:
	.size	gen_pswapdv2si2, .Ltmp2502-gen_pswapdv2si2
	.cfi_endproc

	.globl	gen_pswapdv2sf2
	.align	16, 0x90
	.type	gen_pswapdv2sf2,@function
gen_pswapdv2sf2:                        # @gen_pswapdv2sf2
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp2506:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp2507:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp2508:
	.cfi_def_cfa_offset 32
.Ltmp2509:
	.cfi_offset %rbx, -24
.Ltmp2510:
	.cfi_offset %r14, -16
	movq	const_int_rtx+520(%rip), %rcx
	movq	const_int_rtx+512(%rip), %rdx
	movq	%rsi, %rbx
	movq	%rdi, %r14
	movq	$2, %rdi
	xorq	%rax, %rax
	movq	%rcx, %rsi
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_E
	movq	$143, %rdi
	movq	$44, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp2511:
	.size	gen_pswapdv2sf2, .Ltmp2511-gen_pswapdv2sf2
	.cfi_endproc

	.globl	gen_cmpdi
	.align	16, 0x90
	.type	gen_cmpdi,@function
gen_cmpdi:                              # @gen_cmpdi
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp2515:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp2516:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp2517:
	.cfi_def_cfa_offset 32
.Ltmp2518:
	.cfi_offset %rbx, -24
.Ltmp2519:
	.cfi_offset %r14, -16
	movq	%rsi, %r14
	movq	%rdi, %rbx
	callq	start_sequence
	movzbl	(%rbx), %eax
	cmpq	$66, %rax
	jne	.LBB290_3
# BB#1:                                 # %land.lhs.true
	movzbl	(%r14), %eax
	cmpq	$66, %rax
	jne	.LBB290_3
# BB#2:                                 # %if.then
	movq	$5, %rdi
	movq	%rbx, %rsi
	callq	force_reg
	movq	%rax, %rbx
.LBB290_3:                              # %if.end
	movq	%rbx, ix86_compare_op0(%rip)
	movq	%r14, ix86_compare_op1(%rip)
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp2520:
	.size	gen_cmpdi, .Ltmp2520-gen_cmpdi
	.cfi_endproc

	.globl	gen_cmpsi
	.align	16, 0x90
	.type	gen_cmpsi,@function
gen_cmpsi:                              # @gen_cmpsi
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp2524:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp2525:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp2526:
	.cfi_def_cfa_offset 32
.Ltmp2527:
	.cfi_offset %rbx, -24
.Ltmp2528:
	.cfi_offset %r14, -16
	movq	%rsi, %r14
	movq	%rdi, %rbx
	callq	start_sequence
	movzbl	(%rbx), %eax
	cmpq	$66, %rax
	jne	.LBB291_3
# BB#1:                                 # %land.lhs.true
	movzbl	(%r14), %eax
	cmpq	$66, %rax
	jne	.LBB291_3
# BB#2:                                 # %if.then
	movq	$4, %rdi
	movq	%rbx, %rsi
	callq	force_reg
	movq	%rax, %rbx
.LBB291_3:                              # %if.end
	movq	%rbx, ix86_compare_op0(%rip)
	movq	%r14, ix86_compare_op1(%rip)
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp2529:
	.size	gen_cmpsi, .Ltmp2529-gen_cmpsi
	.cfi_endproc

	.globl	gen_cmphi
	.align	16, 0x90
	.type	gen_cmphi,@function
gen_cmphi:                              # @gen_cmphi
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp2533:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp2534:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp2535:
	.cfi_def_cfa_offset 32
.Ltmp2536:
	.cfi_offset %rbx, -24
.Ltmp2537:
	.cfi_offset %r14, -16
	movq	%rsi, %r14
	movq	%rdi, %rbx
	callq	start_sequence
	movzbl	(%rbx), %eax
	cmpq	$66, %rax
	jne	.LBB292_3
# BB#1:                                 # %land.lhs.true
	movzbl	(%r14), %eax
	cmpq	$66, %rax
	jne	.LBB292_3
# BB#2:                                 # %if.then
	movq	$3, %rdi
	movq	%rbx, %rsi
	callq	force_reg
	movq	%rax, %rbx
.LBB292_3:                              # %if.end
	movq	%rbx, ix86_compare_op0(%rip)
	movq	%r14, ix86_compare_op1(%rip)
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp2538:
	.size	gen_cmphi, .Ltmp2538-gen_cmphi
	.cfi_endproc

	.globl	gen_cmpqi
	.align	16, 0x90
	.type	gen_cmpqi,@function
gen_cmpqi:                              # @gen_cmpqi
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp2542:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp2543:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp2544:
	.cfi_def_cfa_offset 32
.Ltmp2545:
	.cfi_offset %rbx, -24
.Ltmp2546:
	.cfi_offset %r14, -16
	movq	%rsi, %r14
	movq	%rdi, %rbx
	callq	start_sequence
	movzbl	(%rbx), %eax
	cmpq	$66, %rax
	jne	.LBB293_3
# BB#1:                                 # %land.lhs.true
	movzbl	(%r14), %eax
	cmpq	$66, %rax
	jne	.LBB293_3
# BB#2:                                 # %if.then
	movq	$2, %rdi
	movq	%rbx, %rsi
	callq	force_reg
	movq	%rax, %rbx
.LBB293_3:                              # %if.end
	movq	%rbx, ix86_compare_op0(%rip)
	movq	%r14, ix86_compare_op1(%rip)
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp2547:
	.size	gen_cmpqi, .Ltmp2547-gen_cmpqi
	.cfi_endproc

	.globl	gen_cmpdi_1_rex64
	.align	16, 0x90
	.type	gen_cmpdi_1_rex64,@function
gen_cmpdi_1_rex64:                      # @gen_cmpdi_1_rex64
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp2552:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp2553:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp2554:
	.cfi_def_cfa_offset 32
.Ltmp2555:
	.cfi_offset %rbx, -32
.Ltmp2556:
	.cfi_offset %r14, -24
.Ltmp2557:
	.cfi_offset %r15, -16
	movq	%rsi, %r14
	movq	%rdi, %rbx
	movq	$52, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	%rax, %r15
	movq	$74, %rdi
	movq	$52, %rsi
	movq	%rbx, %rdx
	movq	%r14, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r15, %rdx
	movq	%rax, %rcx
	popq	%rbx
	popq	%r14
	popq	%r15
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp2558:
	.size	gen_cmpdi_1_rex64, .Ltmp2558-gen_cmpdi_1_rex64
	.cfi_endproc

	.globl	gen_cmpsi_1
	.align	16, 0x90
	.type	gen_cmpsi_1,@function
gen_cmpsi_1:                            # @gen_cmpsi_1
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp2563:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp2564:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp2565:
	.cfi_def_cfa_offset 32
.Ltmp2566:
	.cfi_offset %rbx, -32
.Ltmp2567:
	.cfi_offset %r14, -24
.Ltmp2568:
	.cfi_offset %r15, -16
	movq	%rsi, %r14
	movq	%rdi, %rbx
	movq	$52, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	%rax, %r15
	movq	$74, %rdi
	movq	$52, %rsi
	movq	%rbx, %rdx
	movq	%r14, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r15, %rdx
	movq	%rax, %rcx
	popq	%rbx
	popq	%r14
	popq	%r15
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp2569:
	.size	gen_cmpsi_1, .Ltmp2569-gen_cmpsi_1
	.cfi_endproc

	.globl	gen_cmpqi_ext_3
	.align	16, 0x90
	.type	gen_cmpqi_ext_3,@function
gen_cmpqi_ext_3:                        # @gen_cmpqi_ext_3
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp2575:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp2576:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp2577:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp2578:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp2579:
	.cfi_def_cfa_offset 48
.Ltmp2580:
	.cfi_offset %rbx, -40
.Ltmp2581:
	.cfi_offset %r12, -32
.Ltmp2582:
	.cfi_offset %r14, -24
.Ltmp2583:
	.cfi_offset %r15, -16
	movq	%rsi, %r14
	movq	%rdi, %r12
	movq	$52, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	%rax, %r15
	xorq	%rdi, %rdi
	movq	$8, %rsi
	callq	gen_rtx_CONST_INT
	movq	%rax, %rbx
	xorq	%rdi, %rdi
	movq	$8, %rsi
	callq	gen_rtx_CONST_INT
	movq	$133, %rdi
	movq	$4, %rsi
	movq	%r12, %rdx
	movq	%rbx, %rcx
	movq	%rax, %r8
	callq	gen_rtx_fmt_eee
	movq	$2, %rdi
	xorq	%rdx, %rdx
	movq	%rax, %rsi
	callq	gen_rtx_SUBREG
	movq	$74, %rdi
	movq	$52, %rsi
	movq	%rax, %rdx
	movq	%r14, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r15, %rdx
	movq	%rax, %rcx
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp2584:
	.size	gen_cmpqi_ext_3, .Ltmp2584-gen_cmpqi_ext_3
	.cfi_endproc

	.globl	gen_cmpxf
	.align	16, 0x90
	.type	gen_cmpxf,@function
gen_cmpxf:                              # @gen_cmpxf
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp2588:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp2589:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp2590:
	.cfi_def_cfa_offset 32
.Ltmp2591:
	.cfi_offset %rbx, -24
.Ltmp2592:
	.cfi_offset %r14, -16
	movq	%rsi, %r14
	movq	%rdi, %rbx
	callq	start_sequence
	movq	%rbx, ix86_compare_op0(%rip)
	movq	%r14, ix86_compare_op1(%rip)
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp2593:
	.size	gen_cmpxf, .Ltmp2593-gen_cmpxf
	.cfi_endproc

	.globl	gen_cmptf
	.align	16, 0x90
	.type	gen_cmptf,@function
gen_cmptf:                              # @gen_cmptf
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp2597:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp2598:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp2599:
	.cfi_def_cfa_offset 32
.Ltmp2600:
	.cfi_offset %rbx, -24
.Ltmp2601:
	.cfi_offset %r14, -16
	movq	%rsi, %r14
	movq	%rdi, %rbx
	callq	start_sequence
	movq	%rbx, ix86_compare_op0(%rip)
	movq	%r14, ix86_compare_op1(%rip)
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp2602:
	.size	gen_cmptf, .Ltmp2602-gen_cmptf
	.cfi_endproc

	.globl	gen_cmpdf
	.align	16, 0x90
	.type	gen_cmpdf,@function
gen_cmpdf:                              # @gen_cmpdf
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp2606:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp2607:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp2608:
	.cfi_def_cfa_offset 32
.Ltmp2609:
	.cfi_offset %rbx, -24
.Ltmp2610:
	.cfi_offset %r14, -16
	movq	%rsi, %r14
	movq	%rdi, %rbx
	callq	start_sequence
	movq	%rbx, ix86_compare_op0(%rip)
	movq	%r14, ix86_compare_op1(%rip)
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp2611:
	.size	gen_cmpdf, .Ltmp2611-gen_cmpdf
	.cfi_endproc

	.globl	gen_cmpsf
	.align	16, 0x90
	.type	gen_cmpsf,@function
gen_cmpsf:                              # @gen_cmpsf
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp2615:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp2616:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp2617:
	.cfi_def_cfa_offset 32
.Ltmp2618:
	.cfi_offset %rbx, -24
.Ltmp2619:
	.cfi_offset %r14, -16
	movq	%rsi, %r14
	movq	%rdi, %rbx
	callq	start_sequence
	movq	%rbx, ix86_compare_op0(%rip)
	movq	%r14, ix86_compare_op1(%rip)
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp2620:
	.size	gen_cmpsf, .Ltmp2620-gen_cmpsf
	.cfi_endproc

	.globl	gen_split_845
	.align	16, 0x90
	.type	gen_split_845,@function
gen_split_845:                          # @gen_split_845
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp2626:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp2627:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp2628:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp2629:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp2630:
	.cfi_def_cfa_offset 48
.Ltmp2631:
	.cfi_offset %rbx, -40
.Ltmp2632:
	.cfi_offset %r12, -32
.Ltmp2633:
	.cfi_offset %r14, -24
.Ltmp2634:
	.cfi_offset %r15, -16
	movq	%rdi, %rbx
	callq	start_sequence
	movl	target_flags(%rip), %edi
	movq	$25, %rcx
	movq	global_rtl+16(%rip), %rsi
	shrq	%cl, %rdi
	andq	$1, %rdi
	orq	$4, %rdi
	callq	gen_rtx_MEM
	movq	%rax, 16(%rbx)
	movq	(%rbx), %rcx
	movzbl	2(%rcx), %esi
	movq	$125, %rdi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %r12
	movq	%r12, 16(%rbx)
	movq	(%rbx), %r15
	movq	8(%rbx), %r14
	movq	$4, %rdi
	movq	$7, %rsi
	callq	gen_rtx_REG
	movq	$96, %rdi
	movq	$4, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	$4, %rdi
	movq	%rax, %rsi
	callq	gen_rtx_MEM
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	movq	%r14, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	movq	$57, %rdi
	movq	$18, %rsi
	callq	gen_rtx_REG
	movq	%rax, %rbx
	movq	$74, %rdi
	movq	$57, %rsi
	movq	%r15, %rdx
	movq	%r12, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	movq	%r14, %rdi
	callq	copy_rtx
	movq	%rax, %rbx
	movq	$4, %rdi
	movq	$7, %rsi
	callq	gen_rtx_REG
	movq	$4, %rdi
	movq	%rax, %rsi
	callq	gen_rtx_MEM
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %r14
	movq	$4, %rdi
	movq	$7, %rsi
	callq	gen_rtx_REG
	movq	%rax, %r15
	movq	$4, %rdi
	movq	$7, %rsi
	callq	gen_rtx_REG
	movq	%rax, %rbx
	xorq	%rdi, %rdi
	movq	$4, %rsi
	callq	gen_rtx_CONST_INT
	movq	$75, %rdi
	movq	$4, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r15, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rcx
	movq	$2, %rdi
	xorq	%rax, %rax
	movq	%r14, %rsi
	movq	%rcx, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_E
	movq	%rax, %rdi
	callq	emit
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.Ltmp2635:
	.size	gen_split_845, .Ltmp2635-gen_split_845
	.cfi_endproc

	.globl	gen_movsi
	.align	16, 0x90
	.type	gen_movsi,@function
gen_movsi:                              # @gen_movsi
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp2639:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp2640:
	.cfi_def_cfa_offset 24
	subq	$24, %rsp
.Ltmp2641:
	.cfi_def_cfa_offset 48
.Ltmp2642:
	.cfi_offset %rbx, -24
.Ltmp2643:
	.cfi_offset %r14, -16
	movq	%rsi, %r14
	movq	%rdi, %rbx
	callq	start_sequence
	movq	%rbx, (%rsp)
	movq	%r14, 8(%rsp)
	leaq	(%rsp), %rsi
	movq	$4, %rdi
	callq	ix86_expand_move
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	addq	$24, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp2644:
	.size	gen_movsi, .Ltmp2644-gen_movsi
	.cfi_endproc

	.globl	gen_movhi
	.align	16, 0x90
	.type	gen_movhi,@function
gen_movhi:                              # @gen_movhi
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp2648:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp2649:
	.cfi_def_cfa_offset 24
	subq	$24, %rsp
.Ltmp2650:
	.cfi_def_cfa_offset 48
.Ltmp2651:
	.cfi_offset %rbx, -24
.Ltmp2652:
	.cfi_offset %r14, -16
	movq	%rsi, %r14
	movq	%rdi, %rbx
	callq	start_sequence
	movq	%rbx, (%rsp)
	movq	%r14, 8(%rsp)
	leaq	(%rsp), %rsi
	movq	$3, %rdi
	callq	ix86_expand_move
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	addq	$24, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp2653:
	.size	gen_movhi, .Ltmp2653-gen_movhi
	.cfi_endproc

	.globl	gen_movstricthi
	.align	16, 0x90
	.type	gen_movstricthi,@function
gen_movstricthi:                        # @gen_movstricthi
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp2657:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp2658:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp2659:
	.cfi_def_cfa_offset 32
.Ltmp2660:
	.cfi_offset %rbx, -24
.Ltmp2661:
	.cfi_offset %r14, -16
	movq	%rsi, %r14
	movq	%rdi, %rbx
	callq	start_sequence
	movzbl	(%rbx), %eax
	cmpq	$66, %rax
	jne	.LBB304_3
# BB#1:                                 # %land.lhs.true
	movzbl	(%r14), %eax
	cmpq	$66, %rax
	jne	.LBB304_3
# BB#2:                                 # %if.then
	movq	$3, %rdi
	movq	%r14, %rsi
	callq	force_reg
	movq	%rax, %r14
.LBB304_3:                              # %if.end
	movq	$64, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	callq	gen_rtx_fmt_e
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	movq	%r14, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp2662:
	.size	gen_movstricthi, .Ltmp2662-gen_movstricthi
	.cfi_endproc

	.globl	gen_movqi
	.align	16, 0x90
	.type	gen_movqi,@function
gen_movqi:                              # @gen_movqi
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp2666:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp2667:
	.cfi_def_cfa_offset 24
	subq	$24, %rsp
.Ltmp2668:
	.cfi_def_cfa_offset 48
.Ltmp2669:
	.cfi_offset %rbx, -24
.Ltmp2670:
	.cfi_offset %r14, -16
	movq	%rsi, %r14
	movq	%rdi, %rbx
	callq	start_sequence
	movq	%rbx, (%rsp)
	movq	%r14, 8(%rsp)
	leaq	(%rsp), %rsi
	movq	$2, %rdi
	callq	ix86_expand_move
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	addq	$24, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp2671:
	.size	gen_movqi, .Ltmp2671-gen_movqi
	.cfi_endproc

	.globl	gen_reload_outqi
	.align	16, 0x90
	.type	gen_reload_outqi,@function
gen_reload_outqi:                       # @gen_reload_outqi
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp2677:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp2678:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp2679:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp2680:
	.cfi_def_cfa_offset 40
	subq	$24, %rsp
.Ltmp2681:
	.cfi_def_cfa_offset 64
.Ltmp2682:
	.cfi_offset %rbx, -40
.Ltmp2683:
	.cfi_offset %r12, -32
.Ltmp2684:
	.cfi_offset %r14, -24
.Ltmp2685:
	.cfi_offset %r15, -16
	movq	%rdx, %r15
	movq	%rsi, %rbx
	movq	%rdi, %r14
	callq	start_sequence
	movq	%r15, %rdi
	movq	%r14, %rsi
	callq	reg_overlap_mentioned_p
	movabsq	$4294967295, %r12       # imm = 0xFFFFFFFF
	testq	%r12, %rax
	jne	.LBB306_5
# BB#1:                                 # %if.end
	movq	$2, %rsi
	movq	%rbx, %rdi
	callq	q_regs_operand
	testq	%r12, %rax
	je	.LBB306_3
# BB#2:                                 # %if.end.if.end11_crit_edge
	leaq	(%rsp), %r12
	jmp	.LBB306_4
.LBB306_3:                              # %if.then8
	callq	start_sequence
	leaq	(%rsp), %r12
	movq	%r15, (%rsp)
	movq	%rbx, 8(%rsp)
	movq	$2, %rdi
	movq	%r12, %rsi
	callq	ix86_expand_move
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rdi
	callq	emit_insn
	movq	%r15, %rbx
.LBB306_4:                              # %if.end11
	callq	start_sequence
	movq	%r14, (%rsp)
	movq	%rbx, 8(%rsp)
	movq	$2, %rdi
	movq	%r12, %rsi
	callq	ix86_expand_move
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rdi
	callq	emit_insn
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	addq	$24, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.LBB306_5:                              # %if.then
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.gen_reload_outqi, %rcx
	movq	$5519, %rsi             # imm = 0x158F
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp2686:
	.size	gen_reload_outqi, .Ltmp2686-gen_reload_outqi
	.cfi_endproc

	.globl	gen_movstrictqi
	.align	16, 0x90
	.type	gen_movstrictqi,@function
gen_movstrictqi:                        # @gen_movstrictqi
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp2690:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp2691:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp2692:
	.cfi_def_cfa_offset 32
.Ltmp2693:
	.cfi_offset %rbx, -24
.Ltmp2694:
	.cfi_offset %r14, -16
	movq	%rsi, %r14
	movq	%rdi, %rbx
	callq	start_sequence
	movzbl	(%rbx), %eax
	cmpq	$66, %rax
	jne	.LBB307_3
# BB#1:                                 # %land.lhs.true
	movzbl	(%r14), %eax
	cmpq	$66, %rax
	jne	.LBB307_3
# BB#2:                                 # %if.then
	movq	$2, %rdi
	movq	%r14, %rsi
	callq	force_reg
	movq	%rax, %r14
.LBB307_3:                              # %if.end
	movq	$64, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	callq	gen_rtx_fmt_e
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	movq	%r14, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp2695:
	.size	gen_movstrictqi, .Ltmp2695-gen_movstrictqi
	.cfi_endproc

	.globl	gen_movdi
	.align	16, 0x90
	.type	gen_movdi,@function
gen_movdi:                              # @gen_movdi
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp2699:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp2700:
	.cfi_def_cfa_offset 24
	subq	$24, %rsp
.Ltmp2701:
	.cfi_def_cfa_offset 48
.Ltmp2702:
	.cfi_offset %rbx, -24
.Ltmp2703:
	.cfi_offset %r14, -16
	movq	%rsi, %r14
	movq	%rdi, %rbx
	callq	start_sequence
	movq	%rbx, (%rsp)
	movq	%r14, 8(%rsp)
	leaq	(%rsp), %rsi
	movq	$5, %rdi
	callq	ix86_expand_move
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	addq	$24, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp2704:
	.size	gen_movdi, .Ltmp2704-gen_movdi
	.cfi_endproc

	.globl	gen_peephole2_853
	.align	16, 0x90
	.type	gen_peephole2_853,@function
gen_peephole2_853:                      # @gen_peephole2_853
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp2708:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp2709:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp2710:
	.cfi_def_cfa_offset 32
.Ltmp2711:
	.cfi_offset %rbx, -24
.Ltmp2712:
	.cfi_offset %r14, -16
	movq	%rsi, %rbx
	movq	$0, (%rsp)
	movabsq	$.L.str1, %rdx
	leaq	(%rsp), %r8
	xorq	%rdi, %rdi
	xorq	%rsi, %rsi
	movq	$5, %rcx
	xorq	%r14, %r14
	callq	peep2_find_free_register
	testq	%rax, %rax
	movq	%rax, 16(%rbx)
	je	.LBB309_2
# BB#1:                                 # %if.end
	callq	start_sequence
	movq	(%rbx), %r14
	movq	8(%rbx), %rcx
	movq	16(%rbx), %rbx
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	movq	%rbx, %rdi
	callq	copy_rtx
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	callq	gen_sequence
	movq	%rax, %r14
	callq	end_sequence
.LBB309_2:                              # %return
	movq	%r14, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp2713:
	.size	gen_peephole2_853, .Ltmp2713-gen_peephole2_853
	.cfi_endproc

	.globl	gen_peephole2_854
	.align	16, 0x90
	.type	gen_peephole2_854,@function
gen_peephole2_854:                      # @gen_peephole2_854
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp2717:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp2718:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp2719:
	.cfi_def_cfa_offset 32
.Ltmp2720:
	.cfi_offset %rbx, -24
.Ltmp2721:
	.cfi_offset %r14, -16
	movq	%rsi, %rbx
	callq	start_sequence
	leaq	8(%rbx), %rdi
	leaq	16(%rbx), %rdx
	leaq	24(%rbx), %rcx
	movq	$1, %rsi
	callq	split_di
	movq	16(%rbx), %rsi
	movq	$5, %rdi
	callq	gen_lowpart
	movq	%rax, 8(%rbx)
	movq	global_rtl+16(%rip), %r14
	xorq	%rdi, %rdi
	movq	$4, %rsi
	callq	gen_rtx_CONST_INT
	movq	$75, %rdi
	movq	$5, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$4, %rdi
	movq	%rax, %rsi
	callq	gen_rtx_MEM
	movq	%rax, %r14
	movq	%r14, 16(%rbx)
	movq	(%rbx), %rdx
	movq	8(%rbx), %rcx
	movq	24(%rbx), %rbx
	movq	$47, %rdi
	xorq	%rsi, %rsi
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	movq	%rbx, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp2722:
	.size	gen_peephole2_854, .Ltmp2722-gen_peephole2_854
	.cfi_endproc

	.globl	gen_split_855
	.align	16, 0x90
	.type	gen_split_855,@function
gen_split_855:                          # @gen_split_855
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp2726:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp2727:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp2728:
	.cfi_def_cfa_offset 32
.Ltmp2729:
	.cfi_offset %rbx, -24
.Ltmp2730:
	.cfi_offset %r14, -16
	movq	%rdi, %rbx
	callq	start_sequence
	leaq	8(%rbx), %rdi
	leaq	16(%rbx), %rdx
	leaq	24(%rbx), %rcx
	movq	$1, %rsi
	callq	split_di
	movq	16(%rbx), %rsi
	movq	$5, %rdi
	callq	gen_lowpart
	movq	%rax, 8(%rbx)
	movq	global_rtl+16(%rip), %r14
	xorq	%rdi, %rdi
	movq	$4, %rsi
	callq	gen_rtx_CONST_INT
	movq	$75, %rdi
	movq	$5, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$4, %rdi
	movq	%rax, %rsi
	callq	gen_rtx_MEM
	movq	%rax, %r14
	movq	%r14, 16(%rbx)
	movq	(%rbx), %rdx
	movq	8(%rbx), %rcx
	movq	24(%rbx), %rbx
	movq	$47, %rdi
	xorq	%rsi, %rsi
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	movq	%rbx, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp2731:
	.size	gen_split_855, .Ltmp2731-gen_split_855
	.cfi_endproc

	.globl	gen_split_856
	.align	16, 0x90
	.type	gen_split_856,@function
gen_split_856:                          # @gen_split_856
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp2734:
	.cfi_def_cfa_offset 16
.Ltmp2735:
	.cfi_offset %rbx, -16
	movq	%rdi, %rbx
	callq	start_sequence
	movq	%rbx, %rdi
	callq	ix86_split_long_move
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	popq	%rbx
	retq
.Ltmp2736:
	.size	gen_split_856, .Ltmp2736-gen_split_856
	.cfi_endproc

	.globl	gen_split_857
	.align	16, 0x90
	.type	gen_split_857,@function
gen_split_857:                          # @gen_split_857
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp2739:
	.cfi_def_cfa_offset 16
.Ltmp2740:
	.cfi_offset %rbx, -16
	movq	%rdi, %rbx
	callq	start_sequence
	movq	%rbx, %rdi
	callq	ix86_split_long_move
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	popq	%rbx
	retq
.Ltmp2741:
	.size	gen_split_857, .Ltmp2741-gen_split_857
	.cfi_endproc

	.globl	gen_peephole2_858
	.align	16, 0x90
	.type	gen_peephole2_858,@function
gen_peephole2_858:                      # @gen_peephole2_858
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp2745:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp2746:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp2747:
	.cfi_def_cfa_offset 32
.Ltmp2748:
	.cfi_offset %rbx, -24
.Ltmp2749:
	.cfi_offset %r14, -16
	movq	%rsi, %rbx
	movq	$0, (%rsp)
	movabsq	$.L.str1, %rdx
	leaq	(%rsp), %r8
	xorq	%rdi, %rdi
	xorq	%rsi, %rsi
	movq	$5, %rcx
	xorq	%r14, %r14
	callq	peep2_find_free_register
	testq	%rax, %rax
	movq	%rax, 16(%rbx)
	je	.LBB314_2
# BB#1:                                 # %if.end
	callq	start_sequence
	movq	(%rbx), %r14
	movq	8(%rbx), %rcx
	movq	16(%rbx), %rbx
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	movq	%rbx, %rdi
	callq	copy_rtx
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	callq	gen_sequence
	movq	%rax, %r14
	callq	end_sequence
.LBB314_2:                              # %return
	movq	%r14, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp2750:
	.size	gen_peephole2_858, .Ltmp2750-gen_peephole2_858
	.cfi_endproc

	.globl	gen_peephole2_859
	.align	16, 0x90
	.type	gen_peephole2_859,@function
gen_peephole2_859:                      # @gen_peephole2_859
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp2754:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp2755:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp2756:
	.cfi_def_cfa_offset 32
.Ltmp2757:
	.cfi_offset %rbx, -24
.Ltmp2758:
	.cfi_offset %r14, -16
	movq	%rsi, %rbx
	callq	start_sequence
	leaq	16(%rbx), %rdx
	leaq	32(%rbx), %rcx
	movq	$2, %rsi
	movq	%rbx, %rdi
	callq	split_di
	movq	16(%rbx), %rdx
	movq	24(%rbx), %rcx
	movq	32(%rbx), %r14
	movq	40(%rbx), %rbx
	movq	$47, %rdi
	xorq	%rsi, %rsi
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	movq	%rbx, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp2759:
	.size	gen_peephole2_859, .Ltmp2759-gen_peephole2_859
	.cfi_endproc

	.globl	gen_split_860
	.align	16, 0x90
	.type	gen_split_860,@function
gen_split_860:                          # @gen_split_860
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp2763:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp2764:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp2765:
	.cfi_def_cfa_offset 32
.Ltmp2766:
	.cfi_offset %rbx, -24
.Ltmp2767:
	.cfi_offset %r14, -16
	movq	%rdi, %rbx
	callq	start_sequence
	leaq	16(%rbx), %rdx
	leaq	32(%rbx), %rcx
	movq	$2, %rsi
	movq	%rbx, %rdi
	callq	split_di
	movq	16(%rbx), %rdx
	movq	24(%rbx), %rcx
	movq	32(%rbx), %r14
	movq	40(%rbx), %rbx
	movq	$47, %rdi
	xorq	%rsi, %rsi
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	movq	%rbx, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp2768:
	.size	gen_split_860, .Ltmp2768-gen_split_860
	.cfi_endproc

	.globl	gen_movsf
	.align	16, 0x90
	.type	gen_movsf,@function
gen_movsf:                              # @gen_movsf
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp2772:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp2773:
	.cfi_def_cfa_offset 24
	subq	$24, %rsp
.Ltmp2774:
	.cfi_def_cfa_offset 48
.Ltmp2775:
	.cfi_offset %rbx, -24
.Ltmp2776:
	.cfi_offset %r14, -16
	movq	%rsi, %r14
	movq	%rdi, %rbx
	callq	start_sequence
	movq	%rbx, (%rsp)
	movq	%r14, 8(%rsp)
	leaq	(%rsp), %rsi
	movq	$15, %rdi
	callq	ix86_expand_move
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	addq	$24, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp2777:
	.size	gen_movsf, .Ltmp2777-gen_movsf
	.cfi_endproc

	.globl	gen_split_862
	.align	16, 0x90
	.type	gen_split_862,@function
gen_split_862:                          # @gen_split_862
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp2780:
	.cfi_def_cfa_offset 16
.Ltmp2781:
	.cfi_offset %rbx, -16
	movq	%rdi, %rbx
	callq	start_sequence
	movq	8(%rbx), %rax
	movq	8(%rax), %rdi
	callq	get_pool_constant
	movq	%rax, 8(%rbx)
	movq	(%rbx), %rdx
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	popq	%rbx
	retq
.Ltmp2782:
	.size	gen_split_862, .Ltmp2782-gen_split_862
	.cfi_endproc

	.globl	gen_split_863
	.align	16, 0x90
	.type	gen_split_863,@function
gen_split_863:                          # @gen_split_863
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp2787:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp2788:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp2789:
	.cfi_def_cfa_offset 32
.Ltmp2790:
	.cfi_offset %rbx, -32
.Ltmp2791:
	.cfi_offset %r14, -24
.Ltmp2792:
	.cfi_offset %r15, -16
	movq	%rdi, %rbx
	callq	start_sequence
	movq	8(%rbx), %r14
	movq	$4, %rdi
	movq	$7, %rsi
	callq	gen_rtx_REG
	movq	%rax, %r15
	movq	$4, %rdi
	movq	$7, %rsi
	callq	gen_rtx_REG
	movq	%rax, %rbx
	xorq	%rdi, %rdi
	movq	$-4, %rsi
	callq	gen_rtx_CONST_INT
	movq	$75, %rdi
	movq	$4, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r15, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	movq	$4, %rdi
	movq	$7, %rsi
	callq	gen_rtx_REG
	movq	$15, %rdi
	movq	%rax, %rsi
	callq	gen_rtx_MEM
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	movq	%r14, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp2793:
	.size	gen_split_863, .Ltmp2793-gen_split_863
	.cfi_endproc

	.globl	gen_split_864
	.align	16, 0x90
	.type	gen_split_864,@function
gen_split_864:                          # @gen_split_864
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp2798:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp2799:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp2800:
	.cfi_def_cfa_offset 32
.Ltmp2801:
	.cfi_offset %rbx, -32
.Ltmp2802:
	.cfi_offset %r14, -24
.Ltmp2803:
	.cfi_offset %r15, -16
	movq	%rdi, %rbx
	callq	start_sequence
	movq	8(%rbx), %r14
	movq	$5, %rdi
	movq	$7, %rsi
	callq	gen_rtx_REG
	movq	%rax, %r15
	movq	$5, %rdi
	movq	$7, %rsi
	callq	gen_rtx_REG
	movq	%rax, %rbx
	xorq	%rdi, %rdi
	movq	$-8, %rsi
	callq	gen_rtx_CONST_INT
	movq	$75, %rdi
	movq	$5, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r15, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	movq	$5, %rdi
	movq	$7, %rsi
	callq	gen_rtx_REG
	movq	$15, %rdi
	movq	%rax, %rsi
	callq	gen_rtx_MEM
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	movq	%r14, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp2804:
	.size	gen_split_864, .Ltmp2804-gen_split_864
	.cfi_endproc

	.globl	gen_movdf
	.align	16, 0x90
	.type	gen_movdf,@function
gen_movdf:                              # @gen_movdf
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp2808:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp2809:
	.cfi_def_cfa_offset 24
	subq	$24, %rsp
.Ltmp2810:
	.cfi_def_cfa_offset 48
.Ltmp2811:
	.cfi_offset %rbx, -24
.Ltmp2812:
	.cfi_offset %r14, -16
	movq	%rsi, %r14
	movq	%rdi, %rbx
	callq	start_sequence
	movq	%rbx, (%rsp)
	movq	%r14, 8(%rsp)
	leaq	(%rsp), %rsi
	movq	$16, %rdi
	callq	ix86_expand_move
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	addq	$24, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp2813:
	.size	gen_movdf, .Ltmp2813-gen_movdf
	.cfi_endproc

	.globl	gen_split_866
	.align	16, 0x90
	.type	gen_split_866,@function
gen_split_866:                          # @gen_split_866
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp2818:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp2819:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp2820:
	.cfi_def_cfa_offset 32
.Ltmp2821:
	.cfi_offset %rbx, -32
.Ltmp2822:
	.cfi_offset %r14, -24
.Ltmp2823:
	.cfi_offset %r15, -16
	movq	%rdi, %rbx
	callq	start_sequence
	movq	8(%rbx), %r14
	movq	$4, %rdi
	movq	$7, %rsi
	callq	gen_rtx_REG
	movq	%rax, %r15
	movq	$4, %rdi
	movq	$7, %rsi
	callq	gen_rtx_REG
	movq	%rax, %rbx
	xorq	%rdi, %rdi
	movq	$-8, %rsi
	callq	gen_rtx_CONST_INT
	movq	$75, %rdi
	movq	$4, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r15, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	movq	$4, %rdi
	movq	$7, %rsi
	callq	gen_rtx_REG
	movq	$16, %rdi
	movq	%rax, %rsi
	callq	gen_rtx_MEM
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	movq	%r14, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp2824:
	.size	gen_split_866, .Ltmp2824-gen_split_866
	.cfi_endproc

	.globl	gen_split_867
	.align	16, 0x90
	.type	gen_split_867,@function
gen_split_867:                          # @gen_split_867
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp2829:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp2830:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp2831:
	.cfi_def_cfa_offset 32
.Ltmp2832:
	.cfi_offset %rbx, -32
.Ltmp2833:
	.cfi_offset %r14, -24
.Ltmp2834:
	.cfi_offset %r15, -16
	movq	%rdi, %rbx
	callq	start_sequence
	movq	8(%rbx), %r14
	movq	$5, %rdi
	movq	$7, %rsi
	callq	gen_rtx_REG
	movq	%rax, %r15
	movq	$5, %rdi
	movq	$7, %rsi
	callq	gen_rtx_REG
	movq	%rax, %rbx
	xorq	%rdi, %rdi
	movq	$-8, %rsi
	callq	gen_rtx_CONST_INT
	movq	$75, %rdi
	movq	$5, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r15, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	movq	$5, %rdi
	movq	$7, %rsi
	callq	gen_rtx_REG
	movq	$16, %rdi
	movq	%rax, %rsi
	callq	gen_rtx_MEM
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	movq	%r14, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp2835:
	.size	gen_split_867, .Ltmp2835-gen_split_867
	.cfi_endproc

	.globl	gen_split_868
	.align	16, 0x90
	.type	gen_split_868,@function
gen_split_868:                          # @gen_split_868
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp2838:
	.cfi_def_cfa_offset 16
.Ltmp2839:
	.cfi_offset %rbx, -16
	movq	%rdi, %rbx
	callq	start_sequence
	movq	%rbx, %rdi
	callq	ix86_split_long_move
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	popq	%rbx
	retq
.Ltmp2840:
	.size	gen_split_868, .Ltmp2840-gen_split_868
	.cfi_endproc

	.globl	gen_split_869
	.align	16, 0x90
	.type	gen_split_869,@function
gen_split_869:                          # @gen_split_869
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp2843:
	.cfi_def_cfa_offset 16
.Ltmp2844:
	.cfi_offset %rbx, -16
	movq	%rdi, %rbx
	callq	start_sequence
	movq	%rbx, %rdi
	callq	ix86_split_long_move
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	popq	%rbx
	retq
.Ltmp2845:
	.size	gen_split_869, .Ltmp2845-gen_split_869
	.cfi_endproc

	.globl	gen_movxf
	.align	16, 0x90
	.type	gen_movxf,@function
gen_movxf:                              # @gen_movxf
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp2849:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp2850:
	.cfi_def_cfa_offset 24
	subq	$24, %rsp
.Ltmp2851:
	.cfi_def_cfa_offset 48
.Ltmp2852:
	.cfi_offset %rbx, -24
.Ltmp2853:
	.cfi_offset %r14, -16
	movq	%rsi, %r14
	movq	%rdi, %rbx
	callq	start_sequence
	movq	%rbx, (%rsp)
	movq	%r14, 8(%rsp)
	leaq	(%rsp), %rsi
	movq	$17, %rdi
	callq	ix86_expand_move
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	addq	$24, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp2854:
	.size	gen_movxf, .Ltmp2854-gen_movxf
	.cfi_endproc

	.globl	gen_movtf
	.align	16, 0x90
	.type	gen_movtf,@function
gen_movtf:                              # @gen_movtf
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp2858:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp2859:
	.cfi_def_cfa_offset 24
	subq	$24, %rsp
.Ltmp2860:
	.cfi_def_cfa_offset 48
.Ltmp2861:
	.cfi_offset %rbx, -24
.Ltmp2862:
	.cfi_offset %r14, -16
	movq	%rsi, %r14
	movq	%rdi, %rbx
	callq	start_sequence
	movq	%rbx, (%rsp)
	movq	%r14, 8(%rsp)
	leaq	(%rsp), %rsi
	movq	$18, %rdi
	callq	ix86_expand_move
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	addq	$24, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp2863:
	.size	gen_movtf, .Ltmp2863-gen_movtf
	.cfi_endproc

	.globl	gen_split_872
	.align	16, 0x90
	.type	gen_split_872,@function
gen_split_872:                          # @gen_split_872
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp2866:
	.cfi_def_cfa_offset 16
.Ltmp2867:
	.cfi_offset %rbx, -16
	movq	%rdi, %rbx
	callq	start_sequence
	movq	%rbx, %rdi
	callq	ix86_split_long_move
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	popq	%rbx
	retq
.Ltmp2868:
	.size	gen_split_872, .Ltmp2868-gen_split_872
	.cfi_endproc

	.globl	gen_split_873
	.align	16, 0x90
	.type	gen_split_873,@function
gen_split_873:                          # @gen_split_873
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp2873:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp2874:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp2875:
	.cfi_def_cfa_offset 32
.Ltmp2876:
	.cfi_offset %rbx, -32
.Ltmp2877:
	.cfi_offset %r14, -24
.Ltmp2878:
	.cfi_offset %r15, -16
	movq	%rdi, %rbx
	callq	start_sequence
	movq	8(%rbx), %r14
	movq	$4, %rdi
	movq	$7, %rsi
	callq	gen_rtx_REG
	movq	%rax, %r15
	movq	$4, %rdi
	movq	$7, %rsi
	callq	gen_rtx_REG
	movq	%rax, %rbx
	xorq	%rdi, %rdi
	movq	$-12, %rsi
	callq	gen_rtx_CONST_INT
	movq	$75, %rdi
	movq	$4, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r15, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	movq	$4, %rdi
	movq	$7, %rsi
	callq	gen_rtx_REG
	movq	$17, %rdi
	movq	%rax, %rsi
	callq	gen_rtx_MEM
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	movq	%r14, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp2879:
	.size	gen_split_873, .Ltmp2879-gen_split_873
	.cfi_endproc

	.globl	gen_split_874
	.align	16, 0x90
	.type	gen_split_874,@function
gen_split_874:                          # @gen_split_874
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp2884:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp2885:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp2886:
	.cfi_def_cfa_offset 32
.Ltmp2887:
	.cfi_offset %rbx, -32
.Ltmp2888:
	.cfi_offset %r14, -24
.Ltmp2889:
	.cfi_offset %r15, -16
	movq	%rdi, %rbx
	callq	start_sequence
	movq	8(%rbx), %r14
	movq	$4, %rdi
	movq	$7, %rsi
	callq	gen_rtx_REG
	movq	%rax, %r15
	movq	$4, %rdi
	movq	$7, %rsi
	callq	gen_rtx_REG
	movq	%rax, %rbx
	xorq	%rdi, %rdi
	movq	$-16, %rsi
	callq	gen_rtx_CONST_INT
	movq	$75, %rdi
	movq	$4, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r15, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	movq	$4, %rdi
	movq	$7, %rsi
	callq	gen_rtx_REG
	movq	$18, %rdi
	movq	%rax, %rsi
	callq	gen_rtx_MEM
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	movq	%r14, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp2890:
	.size	gen_split_874, .Ltmp2890-gen_split_874
	.cfi_endproc

	.globl	gen_split_875
	.align	16, 0x90
	.type	gen_split_875,@function
gen_split_875:                          # @gen_split_875
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp2895:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp2896:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp2897:
	.cfi_def_cfa_offset 32
.Ltmp2898:
	.cfi_offset %rbx, -32
.Ltmp2899:
	.cfi_offset %r14, -24
.Ltmp2900:
	.cfi_offset %r15, -16
	movq	%rdi, %rbx
	callq	start_sequence
	movq	8(%rbx), %r14
	movq	$5, %rdi
	movq	$7, %rsi
	callq	gen_rtx_REG
	movq	%rax, %r15
	movq	$5, %rdi
	movq	$7, %rsi
	callq	gen_rtx_REG
	movq	%rax, %rbx
	xorq	%rdi, %rdi
	movq	$-16, %rsi
	callq	gen_rtx_CONST_INT
	movq	$75, %rdi
	movq	$5, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r15, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	movq	$5, %rdi
	movq	$7, %rsi
	callq	gen_rtx_REG
	movq	$18, %rdi
	movq	%rax, %rsi
	callq	gen_rtx_MEM
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	movq	%r14, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp2901:
	.size	gen_split_875, .Ltmp2901-gen_split_875
	.cfi_endproc

	.globl	gen_split_876
	.align	16, 0x90
	.type	gen_split_876,@function
gen_split_876:                          # @gen_split_876
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp2904:
	.cfi_def_cfa_offset 16
.Ltmp2905:
	.cfi_offset %rbx, -16
	movq	%rdi, %rbx
	callq	start_sequence
	movq	%rbx, %rdi
	callq	ix86_split_long_move
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	popq	%rbx
	retq
.Ltmp2906:
	.size	gen_split_876, .Ltmp2906-gen_split_876
	.cfi_endproc

	.globl	gen_split_877
	.align	16, 0x90
	.type	gen_split_877,@function
gen_split_877:                          # @gen_split_877
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp2909:
	.cfi_def_cfa_offset 16
.Ltmp2910:
	.cfi_offset %rbx, -16
	movq	%rdi, %rbx
	callq	start_sequence
	movq	8(%rbx), %rax
	movq	8(%rax), %rdi
	callq	get_pool_constant
	movq	%rax, 8(%rbx)
	movq	(%rbx), %rdx
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	popq	%rbx
	retq
.Ltmp2911:
	.size	gen_split_877, .Ltmp2911-gen_split_877
	.cfi_endproc

	.globl	gen_zero_extendhisi2
	.align	16, 0x90
	.type	gen_zero_extendhisi2,@function
gen_zero_extendhisi2:                   # @gen_zero_extendhisi2
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp2915:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp2916:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp2917:
	.cfi_def_cfa_offset 32
.Ltmp2918:
	.cfi_offset %rbx, -24
.Ltmp2919:
	.cfi_offset %r14, -16
	movq	%rsi, %rbx
	movq	%rdi, %r14
	callq	start_sequence
	movl	ix86_cpu(%rip), %ecx
	movl	x86_zero_extend_with_and(%rip), %eax
	movq	$1, %rdx
	shlq	%cl, %rdx
	andq	%rax, %rdx
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %rdx
	je	.LBB334_3
# BB#1:                                 # %entry
	movl	optimize_size(%rip), %ecx
	testq	%rax, %rcx
	jne	.LBB334_3
# BB#2:                                 # %if.then
	movq	$3, %rdi
	movq	%rbx, %rsi
	callq	force_reg
	movq	$121, %rdi
	movq	$4, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rbx
	movq	$52, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rcx
	movq	$2, %rdi
	xorq	%rax, %rax
	movq	%rbx, %rsi
	movq	%rcx, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_E
	jmp	.LBB334_4
.LBB334_3:                              # %if.end
	movq	$121, %rdi
	movq	$4, %rsi
	movq	%rbx, %rdx
	callq	gen_rtx_fmt_e
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
.LBB334_4:                              # %if.end
	movq	%rax, %rdi
	callq	emit_insn
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp2920:
	.size	gen_zero_extendhisi2, .Ltmp2920-gen_zero_extendhisi2
	.cfi_endproc

	.globl	gen_split_879
	.align	16, 0x90
	.type	gen_split_879,@function
gen_split_879:                          # @gen_split_879
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp2924:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp2925:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp2926:
	.cfi_def_cfa_offset 32
.Ltmp2927:
	.cfi_offset %rbx, -24
.Ltmp2928:
	.cfi_offset %r14, -16
	movq	%rdi, %rbx
	callq	start_sequence
	movq	(%rbx), %rbx
	movq	%rbx, %rdi
	callq	copy_rtx
	movq	%rax, %r14
	xorq	%rdi, %rdi
	movq	$65535, %rsi            # imm = 0xFFFF
	callq	gen_rtx_CONST_INT
	movq	$83, %rdi
	movq	$4, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rbx
	movq	$52, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rcx
	movq	$2, %rdi
	xorq	%rax, %rax
	movq	%rbx, %rsi
	movq	%rcx, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_E
	movq	%rax, %rdi
	callq	emit
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp2929:
	.size	gen_split_879, .Ltmp2929-gen_split_879
	.cfi_endproc

	.globl	gen_zero_extendqihi2
	.align	16, 0x90
	.type	gen_zero_extendqihi2,@function
gen_zero_extendqihi2:                   # @gen_zero_extendqihi2
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp2932:
	.cfi_def_cfa_offset 16
.Ltmp2933:
	.cfi_offset %rbx, -16
	movq	%rsi, %rax
	movq	%rdi, %rbx
	movq	$121, %rdi
	movq	$3, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rbx
	movq	$52, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rcx
	movq	$2, %rdi
	xorq	%rax, %rax
	movq	%rbx, %rsi
	movq	%rcx, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	popq	%rbx
	jmp	gen_rtx_fmt_E  # TAILCALL
.Ltmp2934:
	.size	gen_zero_extendqihi2, .Ltmp2934-gen_zero_extendqihi2
	.cfi_endproc

	.globl	gen_split_881
	.align	16, 0x90
	.type	gen_split_881,@function
gen_split_881:                          # @gen_split_881
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp2938:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp2939:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp2940:
	.cfi_def_cfa_offset 32
.Ltmp2941:
	.cfi_offset %rbx, -24
.Ltmp2942:
	.cfi_offset %r14, -16
	movq	%rdi, %rbx
	callq	start_sequence
	movq	(%rbx), %r14
	movq	8(%rbx), %rdx
	movq	$121, %rdi
	movq	$3, %rsi
	callq	gen_rtx_fmt_e
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp2943:
	.size	gen_split_881, .Ltmp2943-gen_split_881
	.cfi_endproc

	.globl	gen_split_882
	.align	16, 0x90
	.type	gen_split_882,@function
gen_split_882:                          # @gen_split_882
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp2947:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp2948:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp2949:
	.cfi_def_cfa_offset 32
.Ltmp2950:
	.cfi_offset %rbx, -24
.Ltmp2951:
	.cfi_offset %r14, -16
	movq	%rdi, %rbx
	callq	start_sequence
	movq	(%rbx), %rsi
	movq	$2, %rdi
	callq	gen_lowpart
	movq	%rax, %r14
	movq	%r14, 16(%rbx)
	movq	(%rbx), %rdx
	movq	8(%rbx), %rbx
	movq	const_int_rtx+512(%rip), %rcx
	movq	$47, %rdi
	xorq	%rsi, %rsi
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	movq	$64, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	callq	gen_rtx_fmt_e
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	movq	%rbx, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp2952:
	.size	gen_split_882, .Ltmp2952-gen_split_882
	.cfi_endproc

	.globl	gen_split_883
	.align	16, 0x90
	.type	gen_split_883,@function
gen_split_883:                          # @gen_split_883
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp2956:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp2957:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp2958:
	.cfi_def_cfa_offset 32
.Ltmp2959:
	.cfi_offset %rbx, -24
.Ltmp2960:
	.cfi_offset %r14, -16
	movq	%rdi, %rbx
	callq	start_sequence
	movq	(%rbx), %rbx
	movq	%rbx, %rdi
	callq	copy_rtx
	movq	%rax, %r14
	xorq	%rdi, %rdi
	movq	$255, %rsi
	callq	gen_rtx_CONST_INT
	movq	$83, %rdi
	movq	$3, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rbx
	movq	$52, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rcx
	movq	$2, %rdi
	xorq	%rax, %rax
	movq	%rbx, %rsi
	movq	%rcx, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_E
	movq	%rax, %rdi
	callq	emit
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp2961:
	.size	gen_split_883, .Ltmp2961-gen_split_883
	.cfi_endproc

	.globl	gen_zero_extendqisi2
	.align	16, 0x90
	.type	gen_zero_extendqisi2,@function
gen_zero_extendqisi2:                   # @gen_zero_extendqisi2
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp2964:
	.cfi_def_cfa_offset 16
.Ltmp2965:
	.cfi_offset %rbx, -16
	movq	%rsi, %rax
	movq	%rdi, %rbx
	movq	$121, %rdi
	movq	$4, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rbx
	movq	$52, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rcx
	movq	$2, %rdi
	xorq	%rax, %rax
	movq	%rbx, %rsi
	movq	%rcx, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	popq	%rbx
	jmp	gen_rtx_fmt_E  # TAILCALL
.Ltmp2966:
	.size	gen_zero_extendqisi2, .Ltmp2966-gen_zero_extendqisi2
	.cfi_endproc

	.globl	gen_split_885
	.align	16, 0x90
	.type	gen_split_885,@function
gen_split_885:                          # @gen_split_885
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp2970:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp2971:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp2972:
	.cfi_def_cfa_offset 32
.Ltmp2973:
	.cfi_offset %rbx, -24
.Ltmp2974:
	.cfi_offset %r14, -16
	movq	%rdi, %rbx
	callq	start_sequence
	movq	(%rbx), %r14
	movq	8(%rbx), %rdx
	movq	$121, %rdi
	movq	$4, %rsi
	callq	gen_rtx_fmt_e
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp2975:
	.size	gen_split_885, .Ltmp2975-gen_split_885
	.cfi_endproc

	.globl	gen_split_886
	.align	16, 0x90
	.type	gen_split_886,@function
gen_split_886:                          # @gen_split_886
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp2979:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp2980:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp2981:
	.cfi_def_cfa_offset 32
.Ltmp2982:
	.cfi_offset %rbx, -24
.Ltmp2983:
	.cfi_offset %r14, -16
	movq	%rdi, %rbx
	callq	start_sequence
	movq	(%rbx), %rsi
	movq	$2, %rdi
	callq	gen_lowpart
	movq	%rax, %r14
	movq	%r14, 16(%rbx)
	movq	(%rbx), %rdx
	movq	8(%rbx), %rbx
	movq	const_int_rtx+512(%rip), %rcx
	movq	$47, %rdi
	xorq	%rsi, %rsi
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	movq	$64, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	callq	gen_rtx_fmt_e
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	movq	%rbx, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp2984:
	.size	gen_split_886, .Ltmp2984-gen_split_886
	.cfi_endproc

	.globl	gen_split_887
	.align	16, 0x90
	.type	gen_split_887,@function
gen_split_887:                          # @gen_split_887
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp2988:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp2989:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp2990:
	.cfi_def_cfa_offset 32
.Ltmp2991:
	.cfi_offset %rbx, -24
.Ltmp2992:
	.cfi_offset %r14, -16
	movq	%rdi, %rbx
	callq	start_sequence
	movq	(%rbx), %rbx
	movq	%rbx, %rdi
	callq	copy_rtx
	movq	%rax, %r14
	xorq	%rdi, %rdi
	movq	$255, %rsi
	callq	gen_rtx_CONST_INT
	movq	$83, %rdi
	movq	$4, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rbx
	movq	$52, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rcx
	movq	$2, %rdi
	xorq	%rax, %rax
	movq	%rbx, %rsi
	movq	%rcx, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_E
	movq	%rax, %rdi
	callq	emit
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp2993:
	.size	gen_split_887, .Ltmp2993-gen_split_887
	.cfi_endproc

	.globl	gen_zero_extendsidi2
	.align	16, 0x90
	.type	gen_zero_extendsidi2,@function
gen_zero_extendsidi2:                   # @gen_zero_extendsidi2
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp2998:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp2999:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp3000:
	.cfi_def_cfa_offset 32
.Ltmp3001:
	.cfi_offset %rbx, -32
.Ltmp3002:
	.cfi_offset %r14, -24
.Ltmp3003:
	.cfi_offset %r15, -16
	movq	%rsi, %rbx
	movq	%rdi, %r14
	callq	start_sequence
	movl	target_flags(%rip), %r15d
	movq	$121, %rdi
	movq	$5, %rsi
	movq	%rbx, %rdx
	callq	gen_rtx_fmt_e
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rbx
	testq	$33554432, %r15         # imm = 0x2000000
	je	.LBB344_1
# BB#2:                                 # %if.end
	movq	%rbx, %rdi
	jmp	.LBB344_3
.LBB344_1:                              # %if.then
	movq	$52, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rcx
	movq	$2, %rdi
	xorq	%rax, %rax
	movq	%rbx, %rsi
	movq	%rcx, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_E
	movq	%rax, %rdi
.LBB344_3:                              # %if.end
	callq	emit_insn
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp3004:
	.size	gen_zero_extendsidi2, .Ltmp3004-gen_zero_extendsidi2
	.cfi_endproc

	.globl	gen_split_889
	.align	16, 0x90
	.type	gen_split_889,@function
gen_split_889:                          # @gen_split_889
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp3007:
	.cfi_def_cfa_offset 16
.Ltmp3008:
	.cfi_offset %rbx, -16
	movq	%rdi, %rbx
	callq	start_sequence
	leaq	24(%rbx), %rdx
	leaq	32(%rbx), %rcx
	movq	$1, %rsi
	movq	%rbx, %rdi
	callq	split_di
	movq	32(%rbx), %rdx
	movq	const_int_rtx+512(%rip), %rcx
	movq	$47, %rdi
	xorq	%rsi, %rsi
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	popq	%rbx
	retq
.Ltmp3009:
	.size	gen_split_889, .Ltmp3009-gen_split_889
	.cfi_endproc

	.globl	gen_split_890
	.align	16, 0x90
	.type	gen_split_890,@function
gen_split_890:                          # @gen_split_890
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp3012:
	.cfi_def_cfa_offset 16
.Ltmp3013:
	.cfi_offset %rbx, -16
	movq	%rdi, %rbx
	callq	start_sequence
	leaq	24(%rbx), %rdx
	leaq	32(%rbx), %rcx
	movq	$1, %rsi
	movq	%rbx, %rdi
	callq	split_di
	movq	32(%rbx), %rdx
	movq	const_int_rtx+512(%rip), %rcx
	movq	$47, %rdi
	xorq	%rsi, %rsi
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	popq	%rbx
	retq
.Ltmp3014:
	.size	gen_split_890, .Ltmp3014-gen_split_890
	.cfi_endproc

	.globl	gen_split_891
	.align	16, 0x90
	.type	gen_split_891,@function
gen_split_891:                          # @gen_split_891
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp3017:
	.cfi_def_cfa_offset 16
.Ltmp3018:
	.cfi_offset %rbx, -16
	movq	%rdi, %rbx
	callq	start_sequence
	leaq	24(%rbx), %rdx
	leaq	32(%rbx), %rcx
	movq	$1, %rsi
	movq	%rbx, %rdi
	callq	split_di
	movq	8(%rbx), %rcx
	movq	24(%rbx), %rdx
	movq	32(%rbx), %rbx
	movq	$47, %rdi
	xorq	%rsi, %rsi
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	movq	const_int_rtx+512(%rip), %rcx
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	popq	%rbx
	retq
.Ltmp3019:
	.size	gen_split_891, .Ltmp3019-gen_split_891
	.cfi_endproc

	.globl	gen_extendsidi2
	.align	16, 0x90
	.type	gen_extendsidi2,@function
gen_extendsidi2:                        # @gen_extendsidi2
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp3024:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp3025:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp3026:
	.cfi_def_cfa_offset 32
.Ltmp3027:
	.cfi_offset %rbx, -32
.Ltmp3028:
	.cfi_offset %r14, -24
.Ltmp3029:
	.cfi_offset %r15, -16
	movq	%rsi, %rbx
	movq	%rdi, %r14
	callq	start_sequence
	movl	target_flags(%rip), %r15d
	movq	$120, %rdi
	movq	$5, %rsi
	movq	%rbx, %rdx
	callq	gen_rtx_fmt_e
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rbx
	testq	$33554432, %r15         # imm = 0x2000000
	je	.LBB348_3
# BB#1:                                 # %if.then
	movq	%rbx, %rdi
	callq	emit_insn
	jmp	.LBB348_2
.LBB348_3:                              # %if.end
	movq	$52, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %r14
	movq	$62, %rdi
	movq	$4, %rsi
	callq	gen_rtx_fmt_0
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rcx
	movq	$3, %rdi
	xorq	%rax, %rax
	movq	%rbx, %rsi
	movq	%r14, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_E
	movq	%rax, %rdi
	callq	emit
.LBB348_2:                              # %if.then
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp3030:
	.size	gen_extendsidi2, .Ltmp3030-gen_extendsidi2
	.cfi_endproc

	.globl	gen_split_893
	.align	16, 0x90
	.type	gen_split_893,@function
gen_split_893:                          # @gen_split_893
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp3036:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp3037:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp3038:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp3039:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp3040:
	.cfi_def_cfa_offset 48
.Ltmp3041:
	.cfi_offset %rbx, -40
.Ltmp3042:
	.cfi_offset %r12, -32
.Ltmp3043:
	.cfi_offset %r14, -24
.Ltmp3044:
	.cfi_offset %r15, -16
	movq	%rdi, %rbx
	callq	start_sequence
	leaq	24(%rbx), %rdx
	leaq	32(%rbx), %rcx
	movq	$1, %rsi
	movq	%rbx, %rdi
	callq	split_di
	movq	8(%rbx), %r15
	movq	24(%rbx), %rdx
	movq	32(%rbx), %r14
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r15, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	movq	%r15, %rdi
	callq	copy_rtx
	movq	%rax, %r12
	movq	%r15, %rdi
	callq	copy_rtx
	movq	%rax, %rbx
	xorq	%rdi, %rdi
	movq	$31, %rsi
	callq	gen_rtx_CONST_INT
	movq	$89, %rdi
	movq	$4, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r12, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rbx
	movq	$52, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rcx
	movq	$2, %rdi
	xorq	%rax, %rax
	movq	%rbx, %rsi
	movq	%rcx, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_E
	movq	%rax, %rdi
	callq	emit
	movq	%r15, %rdi
	callq	copy_rtx
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.Ltmp3045:
	.size	gen_split_893, .Ltmp3045-gen_split_893
	.cfi_endproc

	.globl	gen_split_894
	.align	16, 0x90
	.type	gen_split_894,@function
gen_split_894:                          # @gen_split_894
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp3050:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp3051:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp3052:
	.cfi_def_cfa_offset 32
.Ltmp3053:
	.cfi_offset %rbx, -32
.Ltmp3054:
	.cfi_offset %r14, -24
.Ltmp3055:
	.cfi_offset %r15, -16
	movq	%rdi, %rbx
	callq	start_sequence
	leaq	24(%rbx), %rdx
	leaq	32(%rbx), %rcx
	movq	$1, %rsi
	movq	%rbx, %rdi
	callq	split_di
	movq	8(%rbx), %rsi
	movq	24(%rbx), %rdi
	callq	emit_move_insn
	movq	8(%rbx), %rdi
	callq	true_regnum
	movabsq	$4294967295, %r14       # imm = 0xFFFFFFFF
	testq	%r14, %rax
	jne	.LBB350_5
# BB#1:                                 # %land.lhs.true
	movq	16(%rbx), %rdi
	callq	true_regnum
	andq	%r14, %rax
	cmpq	$1, %rax
	jne	.LBB350_5
# BB#2:                                 # %land.lhs.true10
	movl	optimize_size(%rip), %eax
	testq	%rax, %rax
	jne	.LBB350_4
# BB#3:                                 # %lor.lhs.false
	movl	ix86_cpu(%rip), %ecx
	movl	x86_use_cltd(%rip), %eax
	movq	$1, %rdx
	shlq	%cl, %rdx
	andq	%rax, %rdx
	testq	%r14, %rdx
	je	.LBB350_5
.LBB350_4:                              # %if.then
	movq	8(%rbx), %r14
	movq	16(%rbx), %r15
	xorq	%rdi, %rdi
	movq	$31, %rsi
	callq	gen_rtx_CONST_INT
	movq	$89, %rdi
	movq	$4, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r15, %rdx
	jmp	.LBB350_6
.LBB350_5:                              # %if.else
	movq	8(%rbx), %rsi
	movq	16(%rbx), %rdi
	callq	emit_move_insn
	movq	16(%rbx), %r14
	xorq	%rdi, %rdi
	movq	$31, %rsi
	callq	gen_rtx_CONST_INT
	movq	$89, %rdi
	movq	$4, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
.LBB350_6:                              # %if.end
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %r14
	movq	$52, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rcx
	movq	$2, %rdi
	xorq	%rax, %rax
	movq	%r14, %rsi
	movq	%rcx, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_E
	movq	%rax, %rdi
	callq	emit_insn
	movq	16(%rbx), %rsi
	movq	32(%rbx), %rdi
	callq	emit_move_insn
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp3056:
	.size	gen_split_894, .Ltmp3056-gen_split_894
	.cfi_endproc

	.globl	gen_split_895
	.align	16, 0x90
	.type	gen_split_895,@function
gen_split_895:                          # @gen_split_895
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp3063:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp3064:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp3065:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp3066:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp3067:
	.cfi_def_cfa_offset 48
.Ltmp3068:
	.cfi_offset %rbx, -48
.Ltmp3069:
	.cfi_offset %r12, -40
.Ltmp3070:
	.cfi_offset %r13, -32
.Ltmp3071:
	.cfi_offset %r14, -24
.Ltmp3072:
	.cfi_offset %r15, -16
	movq	%rdi, %r12
	callq	start_sequence
	leaq	24(%r12), %r15
	leaq	32(%r12), %r14
	movq	$1, %rsi
	movq	%r12, %rdi
	movq	%r15, %rdx
	movq	%r14, %rcx
	callq	split_di
	movq	24(%r12), %rdi
	callq	true_regnum
	movq	8(%r12), %rdi
	movq	%rax, %rbx
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	andq	%r13, %rbx
	callq	true_regnum
	andq	%r13, %rax
	cmpq	%rax, %rbx
	je	.LBB351_2
# BB#1:                                 # %if.then
	movq	8(%r12), %rsi
	movq	24(%r12), %rdi
	callq	emit_move_insn
.LBB351_2:                              # %if.end
	movq	(%r15), %rdi
	callq	true_regnum
	testq	%r13, %rax
	jne	.LBB351_6
# BB#3:                                 # %land.lhs.true
	movl	optimize_size(%rip), %eax
	testq	%rax, %rax
	jne	.LBB351_5
# BB#4:                                 # %lor.lhs.false
	movl	ix86_cpu(%rip), %ecx
	movl	x86_use_cltd(%rip), %eax
	movq	$1, %rdx
	shlq	%cl, %rdx
	andq	%rax, %rdx
	testq	%r13, %rdx
	je	.LBB351_6
.LBB351_5:                              # %if.then13
	movq	(%r14), %r14
	movq	(%r15), %rbx
	xorq	%rdi, %rdi
	movq	$31, %rsi
	callq	gen_rtx_CONST_INT
	movq	$89, %rdi
	movq	$4, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	jmp	.LBB351_9
.LBB351_6:                              # %if.end20
	movq	32(%r12), %rdi
	callq	true_regnum
	movq	8(%r12), %rdi
	movq	%rax, %rbx
	andq	%r13, %rbx
	callq	true_regnum
	andq	%r13, %rax
	cmpq	%rax, %rbx
	je	.LBB351_8
# BB#7:                                 # %if.then26
	movq	8(%r12), %rsi
	movq	32(%r12), %rdi
	callq	emit_move_insn
.LBB351_8:                              # %if.end30
	movq	(%r14), %rbx
	xorq	%rdi, %rdi
	movq	$31, %rsi
	callq	gen_rtx_CONST_INT
	movq	$89, %rdi
	movq	$4, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
.LBB351_9:                              # %if.end30
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rbx
	movq	$52, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rcx
	movq	$2, %rdi
	xorq	%rax, %rax
	movq	%rbx, %rsi
	movq	%rcx, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_E
	movq	%rax, %rdi
	callq	emit_insn
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp3073:
	.size	gen_split_895, .Ltmp3073-gen_split_895
	.cfi_endproc

	.globl	gen_split_896
	.align	16, 0x90
	.type	gen_split_896,@function
gen_split_896:                          # @gen_split_896
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp3078:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp3079:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp3080:
	.cfi_def_cfa_offset 32
.Ltmp3081:
	.cfi_offset %rbx, -32
.Ltmp3082:
	.cfi_offset %r14, -24
.Ltmp3083:
	.cfi_offset %r15, -16
	movq	%rdi, %rbx
	callq	start_sequence
	movq	8(%rbx), %r14
	movq	$4, %rdi
	movq	$7, %rsi
	callq	gen_rtx_REG
	movq	%rax, %r15
	movq	$4, %rdi
	movq	$7, %rsi
	callq	gen_rtx_REG
	movq	%rax, %rbx
	xorq	%rdi, %rdi
	movq	$-8, %rsi
	callq	gen_rtx_CONST_INT
	movq	$75, %rdi
	movq	$4, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r15, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	movq	$4, %rdi
	movq	$7, %rsi
	callq	gen_rtx_REG
	movq	$16, %rdi
	movq	%rax, %rsi
	callq	gen_rtx_MEM
	movq	%rax, %rbx
	movq	$123, %rdi
	movq	$16, %rsi
	movq	%r14, %rdx
	callq	gen_rtx_fmt_e
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp3084:
	.size	gen_split_896, .Ltmp3084-gen_split_896
	.cfi_endproc

	.globl	gen_split_897
	.align	16, 0x90
	.type	gen_split_897,@function
gen_split_897:                          # @gen_split_897
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp3089:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp3090:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp3091:
	.cfi_def_cfa_offset 32
.Ltmp3092:
	.cfi_offset %rbx, -32
.Ltmp3093:
	.cfi_offset %r14, -24
.Ltmp3094:
	.cfi_offset %r15, -16
	movq	%rdi, %rbx
	callq	start_sequence
	movq	8(%rbx), %r14
	movq	$5, %rdi
	movq	$7, %rsi
	callq	gen_rtx_REG
	movq	%rax, %r15
	movq	$5, %rdi
	movq	$7, %rsi
	callq	gen_rtx_REG
	movq	%rax, %rbx
	xorq	%rdi, %rdi
	movq	$-8, %rsi
	callq	gen_rtx_CONST_INT
	movq	$75, %rdi
	movq	$5, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r15, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	movq	$5, %rdi
	movq	$7, %rsi
	callq	gen_rtx_REG
	movq	$16, %rdi
	movq	%rax, %rsi
	callq	gen_rtx_MEM
	movq	%rax, %rbx
	movq	$123, %rdi
	movq	$16, %rsi
	movq	%r14, %rdx
	callq	gen_rtx_fmt_e
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp3095:
	.size	gen_split_897, .Ltmp3095-gen_split_897
	.cfi_endproc

	.globl	gen_split_898
	.align	16, 0x90
	.type	gen_split_898,@function
gen_split_898:                          # @gen_split_898
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp3100:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp3101:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp3102:
	.cfi_def_cfa_offset 32
.Ltmp3103:
	.cfi_offset %rbx, -32
.Ltmp3104:
	.cfi_offset %r14, -24
.Ltmp3105:
	.cfi_offset %r15, -16
	movq	%rdi, %rbx
	callq	start_sequence
	movq	8(%rbx), %r14
	movq	$4, %rdi
	movq	$7, %rsi
	callq	gen_rtx_REG
	movq	%rax, %r15
	movq	$4, %rdi
	movq	$7, %rsi
	callq	gen_rtx_REG
	movq	%rax, %rbx
	xorq	%rdi, %rdi
	movq	$-12, %rsi
	callq	gen_rtx_CONST_INT
	movq	$75, %rdi
	movq	$4, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r15, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	movq	$4, %rdi
	movq	$7, %rsi
	callq	gen_rtx_REG
	movq	$17, %rdi
	movq	%rax, %rsi
	callq	gen_rtx_MEM
	movq	%rax, %rbx
	movq	$123, %rdi
	movq	$17, %rsi
	movq	%r14, %rdx
	callq	gen_rtx_fmt_e
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp3106:
	.size	gen_split_898, .Ltmp3106-gen_split_898
	.cfi_endproc

	.globl	gen_split_899
	.align	16, 0x90
	.type	gen_split_899,@function
gen_split_899:                          # @gen_split_899
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp3111:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp3112:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp3113:
	.cfi_def_cfa_offset 32
.Ltmp3114:
	.cfi_offset %rbx, -32
.Ltmp3115:
	.cfi_offset %r14, -24
.Ltmp3116:
	.cfi_offset %r15, -16
	movq	%rdi, %rbx
	callq	start_sequence
	movq	8(%rbx), %r14
	movq	$4, %rdi
	movq	$7, %rsi
	callq	gen_rtx_REG
	movq	%rax, %r15
	movq	$4, %rdi
	movq	$7, %rsi
	callq	gen_rtx_REG
	movq	%rax, %rbx
	xorq	%rdi, %rdi
	movq	$-16, %rsi
	callq	gen_rtx_CONST_INT
	movq	$75, %rdi
	movq	$4, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r15, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	movq	$4, %rdi
	movq	$7, %rsi
	callq	gen_rtx_REG
	movq	$18, %rdi
	movq	%rax, %rsi
	callq	gen_rtx_MEM
	movq	%rax, %rbx
	movq	$123, %rdi
	movq	$18, %rsi
	movq	%r14, %rdx
	callq	gen_rtx_fmt_e
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp3117:
	.size	gen_split_899, .Ltmp3117-gen_split_899
	.cfi_endproc

	.globl	gen_split_900
	.align	16, 0x90
	.type	gen_split_900,@function
gen_split_900:                          # @gen_split_900
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp3122:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp3123:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp3124:
	.cfi_def_cfa_offset 32
.Ltmp3125:
	.cfi_offset %rbx, -32
.Ltmp3126:
	.cfi_offset %r14, -24
.Ltmp3127:
	.cfi_offset %r15, -16
	movq	%rdi, %rbx
	callq	start_sequence
	movq	8(%rbx), %r14
	movq	$5, %rdi
	movq	$7, %rsi
	callq	gen_rtx_REG
	movq	%rax, %r15
	movq	$5, %rdi
	movq	$7, %rsi
	callq	gen_rtx_REG
	movq	%rax, %rbx
	xorq	%rdi, %rdi
	movq	$-16, %rsi
	callq	gen_rtx_CONST_INT
	movq	$75, %rdi
	movq	$5, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r15, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	movq	$5, %rdi
	movq	$7, %rsi
	callq	gen_rtx_REG
	movq	$16, %rdi
	movq	%rax, %rsi
	callq	gen_rtx_MEM
	movq	%rax, %rbx
	movq	$123, %rdi
	movq	$18, %rsi
	movq	%r14, %rdx
	callq	gen_rtx_fmt_e
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp3128:
	.size	gen_split_900, .Ltmp3128-gen_split_900
	.cfi_endproc

	.globl	gen_split_901
	.align	16, 0x90
	.type	gen_split_901,@function
gen_split_901:                          # @gen_split_901
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp3133:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp3134:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp3135:
	.cfi_def_cfa_offset 32
.Ltmp3136:
	.cfi_offset %rbx, -32
.Ltmp3137:
	.cfi_offset %r14, -24
.Ltmp3138:
	.cfi_offset %r15, -16
	movq	%rdi, %rbx
	callq	start_sequence
	movq	8(%rbx), %r14
	movq	$4, %rdi
	movq	$7, %rsi
	callq	gen_rtx_REG
	movq	%rax, %r15
	movq	$4, %rdi
	movq	$7, %rsi
	callq	gen_rtx_REG
	movq	%rax, %rbx
	xorq	%rdi, %rdi
	movq	$-12, %rsi
	callq	gen_rtx_CONST_INT
	movq	$75, %rdi
	movq	$4, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r15, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	movq	$4, %rdi
	movq	$7, %rsi
	callq	gen_rtx_REG
	movq	$16, %rdi
	movq	%rax, %rsi
	callq	gen_rtx_MEM
	movq	%rax, %rbx
	movq	$123, %rdi
	movq	$17, %rsi
	movq	%r14, %rdx
	callq	gen_rtx_fmt_e
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp3139:
	.size	gen_split_901, .Ltmp3139-gen_split_901
	.cfi_endproc

	.globl	gen_split_902
	.align	16, 0x90
	.type	gen_split_902,@function
gen_split_902:                          # @gen_split_902
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp3144:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp3145:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp3146:
	.cfi_def_cfa_offset 32
.Ltmp3147:
	.cfi_offset %rbx, -32
.Ltmp3148:
	.cfi_offset %r14, -24
.Ltmp3149:
	.cfi_offset %r15, -16
	movq	%rdi, %rbx
	callq	start_sequence
	movq	8(%rbx), %r14
	movq	$4, %rdi
	movq	$7, %rsi
	callq	gen_rtx_REG
	movq	%rax, %r15
	movq	$4, %rdi
	movq	$7, %rsi
	callq	gen_rtx_REG
	movq	%rax, %rbx
	xorq	%rdi, %rdi
	movq	$-16, %rsi
	callq	gen_rtx_CONST_INT
	movq	$75, %rdi
	movq	$4, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r15, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	movq	$4, %rdi
	movq	$7, %rsi
	callq	gen_rtx_REG
	movq	$18, %rdi
	movq	%rax, %rsi
	callq	gen_rtx_MEM
	movq	%rax, %rbx
	movq	$123, %rdi
	movq	$17, %rsi
	movq	%r14, %rdx
	callq	gen_rtx_fmt_e
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp3150:
	.size	gen_split_902, .Ltmp3150-gen_split_902
	.cfi_endproc

	.globl	gen_split_903
	.align	16, 0x90
	.type	gen_split_903,@function
gen_split_903:                          # @gen_split_903
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp3155:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp3156:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp3157:
	.cfi_def_cfa_offset 32
.Ltmp3158:
	.cfi_offset %rbx, -32
.Ltmp3159:
	.cfi_offset %r14, -24
.Ltmp3160:
	.cfi_offset %r15, -16
	movq	%rdi, %rbx
	callq	start_sequence
	movq	8(%rbx), %r14
	movq	$5, %rdi
	movq	$7, %rsi
	callq	gen_rtx_REG
	movq	%rax, %r15
	movq	$5, %rdi
	movq	$7, %rsi
	callq	gen_rtx_REG
	movq	%rax, %rbx
	xorq	%rdi, %rdi
	movq	$-16, %rsi
	callq	gen_rtx_CONST_INT
	movq	$75, %rdi
	movq	$5, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r15, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	movq	$5, %rdi
	movq	$7, %rsi
	callq	gen_rtx_REG
	movq	$18, %rdi
	movq	%rax, %rsi
	callq	gen_rtx_MEM
	movq	%rax, %rbx
	movq	$123, %rdi
	movq	$18, %rsi
	movq	%r14, %rdx
	callq	gen_rtx_fmt_e
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp3161:
	.size	gen_split_903, .Ltmp3161-gen_split_903
	.cfi_endproc

	.globl	gen_extendsfdf2
	.align	16, 0x90
	.type	gen_extendsfdf2,@function
gen_extendsfdf2:                        # @gen_extendsfdf2
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp3165:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp3166:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp3167:
	.cfi_def_cfa_offset 32
.Ltmp3168:
	.cfi_offset %rbx, -24
.Ltmp3169:
	.cfi_offset %r14, -16
	movq	%rsi, %rbx
	movq	%rdi, %r14
	callq	start_sequence
	movzbl	(%r14), %eax
	cmpq	$66, %rax
	jne	.LBB360_3
# BB#1:                                 # %land.lhs.true
	movzbl	(%rbx), %eax
	cmpq	$66, %rax
	jne	.LBB360_3
# BB#2:                                 # %if.then
	movq	$15, %rdi
	movq	%rbx, %rsi
	callq	force_reg
	movq	%rax, %rbx
.LBB360_3:                              # %if.end
	movq	$123, %rdi
	movq	$16, %rsi
	movq	%rbx, %rdx
	callq	gen_rtx_fmt_e
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp3170:
	.size	gen_extendsfdf2, .Ltmp3170-gen_extendsfdf2
	.cfi_endproc

	.globl	gen_extendsfxf2
	.align	16, 0x90
	.type	gen_extendsfxf2,@function
gen_extendsfxf2:                        # @gen_extendsfxf2
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp3174:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp3175:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp3176:
	.cfi_def_cfa_offset 32
.Ltmp3177:
	.cfi_offset %rbx, -24
.Ltmp3178:
	.cfi_offset %r14, -16
	movq	%rsi, %rbx
	movq	%rdi, %r14
	callq	start_sequence
	movzbl	(%r14), %eax
	cmpq	$66, %rax
	jne	.LBB361_3
# BB#1:                                 # %land.lhs.true
	movzbl	(%rbx), %eax
	cmpq	$66, %rax
	jne	.LBB361_3
# BB#2:                                 # %if.then
	movq	$15, %rdi
	movq	%rbx, %rsi
	callq	force_reg
	movq	%rax, %rbx
.LBB361_3:                              # %if.end
	movq	$123, %rdi
	movq	$17, %rsi
	movq	%rbx, %rdx
	callq	gen_rtx_fmt_e
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp3179:
	.size	gen_extendsfxf2, .Ltmp3179-gen_extendsfxf2
	.cfi_endproc

	.globl	gen_extendsftf2
	.align	16, 0x90
	.type	gen_extendsftf2,@function
gen_extendsftf2:                        # @gen_extendsftf2
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp3183:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp3184:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp3185:
	.cfi_def_cfa_offset 32
.Ltmp3186:
	.cfi_offset %rbx, -24
.Ltmp3187:
	.cfi_offset %r14, -16
	movq	%rsi, %rbx
	movq	%rdi, %r14
	callq	start_sequence
	movzbl	(%r14), %eax
	cmpq	$66, %rax
	jne	.LBB362_3
# BB#1:                                 # %land.lhs.true
	movzbl	(%rbx), %eax
	cmpq	$66, %rax
	jne	.LBB362_3
# BB#2:                                 # %if.then
	movq	$15, %rdi
	movq	%rbx, %rsi
	callq	force_reg
	movq	%rax, %rbx
.LBB362_3:                              # %if.end
	movq	$123, %rdi
	movq	$18, %rsi
	movq	%rbx, %rdx
	callq	gen_rtx_fmt_e
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp3188:
	.size	gen_extendsftf2, .Ltmp3188-gen_extendsftf2
	.cfi_endproc

	.globl	gen_extenddfxf2
	.align	16, 0x90
	.type	gen_extenddfxf2,@function
gen_extenddfxf2:                        # @gen_extenddfxf2
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp3192:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp3193:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp3194:
	.cfi_def_cfa_offset 32
.Ltmp3195:
	.cfi_offset %rbx, -24
.Ltmp3196:
	.cfi_offset %r14, -16
	movq	%rsi, %rbx
	movq	%rdi, %r14
	callq	start_sequence
	movzbl	(%r14), %eax
	cmpq	$66, %rax
	jne	.LBB363_3
# BB#1:                                 # %land.lhs.true
	movzbl	(%rbx), %eax
	cmpq	$66, %rax
	jne	.LBB363_3
# BB#2:                                 # %if.then
	movq	$16, %rdi
	movq	%rbx, %rsi
	callq	force_reg
	movq	%rax, %rbx
.LBB363_3:                              # %if.end
	movq	$123, %rdi
	movq	$17, %rsi
	movq	%rbx, %rdx
	callq	gen_rtx_fmt_e
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp3197:
	.size	gen_extenddfxf2, .Ltmp3197-gen_extenddfxf2
	.cfi_endproc

	.globl	gen_extenddftf2
	.align	16, 0x90
	.type	gen_extenddftf2,@function
gen_extenddftf2:                        # @gen_extenddftf2
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp3201:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp3202:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp3203:
	.cfi_def_cfa_offset 32
.Ltmp3204:
	.cfi_offset %rbx, -24
.Ltmp3205:
	.cfi_offset %r14, -16
	movq	%rsi, %rbx
	movq	%rdi, %r14
	callq	start_sequence
	movzbl	(%r14), %eax
	cmpq	$66, %rax
	jne	.LBB364_3
# BB#1:                                 # %land.lhs.true
	movzbl	(%rbx), %eax
	cmpq	$66, %rax
	jne	.LBB364_3
# BB#2:                                 # %if.then
	movq	$16, %rdi
	movq	%rbx, %rsi
	callq	force_reg
	movq	%rax, %rbx
.LBB364_3:                              # %if.end
	movq	$123, %rdi
	movq	$18, %rsi
	movq	%rbx, %rdx
	callq	gen_rtx_fmt_e
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp3206:
	.size	gen_extenddftf2, .Ltmp3206-gen_extenddftf2
	.cfi_endproc

	.globl	gen_truncdfsf2
	.align	16, 0x90
	.type	gen_truncdfsf2,@function
gen_truncdfsf2:                         # @gen_truncdfsf2
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp3211:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp3212:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp3213:
	.cfi_def_cfa_offset 32
.Ltmp3214:
	.cfi_offset %rbx, -32
.Ltmp3215:
	.cfi_offset %r14, -24
.Ltmp3216:
	.cfi_offset %r15, -16
	movq	%rsi, %rbx
	movq	%rdi, %r14
	callq	start_sequence
	movzbl	target_flags(%rip), %eax
	testq	$1, %rax
	je	.LBB365_2
# BB#1:                                 # %if.then
	movq	$15, %rdi
	xorq	%rsi, %rsi
	callq	assign_386_stack_local
	movq	%rax, %r15
	movq	$124, %rdi
	movq	$15, %rsi
	movq	%rbx, %rdx
	callq	gen_rtx_fmt_e
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rbx
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%r15, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rcx
	movq	$2, %rdi
	xorq	%rax, %rax
	movq	%rbx, %rsi
	movq	%rcx, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_E
	movq	%rax, %rdi
	callq	emit
	jmp	.LBB365_3
.LBB365_2:                              # %if.else
	movq	$124, %rdi
	movq	$15, %rsi
	movq	%rbx, %rdx
	callq	gen_rtx_fmt_e
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
.LBB365_3:                              # %if.else
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp3217:
	.size	gen_truncdfsf2, .Ltmp3217-gen_truncdfsf2
	.cfi_endproc

	.globl	gen_split_910
	.align	16, 0x90
	.type	gen_split_910,@function
gen_split_910:                          # @gen_split_910
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp3221:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp3222:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp3223:
	.cfi_def_cfa_offset 32
.Ltmp3224:
	.cfi_offset %rbx, -24
.Ltmp3225:
	.cfi_offset %r14, -16
	movq	%rdi, %rbx
	callq	start_sequence
	movq	(%rbx), %r14
	movq	8(%rbx), %rdx
	movq	$124, %rdi
	movq	$15, %rsi
	callq	gen_rtx_fmt_e
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp3226:
	.size	gen_split_910, .Ltmp3226-gen_split_910
	.cfi_endproc

	.globl	gen_split_911
	.align	16, 0x90
	.type	gen_split_911,@function
gen_split_911:                          # @gen_split_911
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp3230:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp3231:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp3232:
	.cfi_def_cfa_offset 32
.Ltmp3233:
	.cfi_offset %rbx, -24
.Ltmp3234:
	.cfi_offset %r14, -16
	movq	%rdi, %rbx
	callq	start_sequence
	movq	(%rbx), %r14
	movq	8(%rbx), %rdx
	movq	$124, %rdi
	movq	$15, %rsi
	callq	gen_rtx_fmt_e
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp3235:
	.size	gen_split_911, .Ltmp3235-gen_split_911
	.cfi_endproc

	.globl	gen_split_912
	.align	16, 0x90
	.type	gen_split_912,@function
gen_split_912:                          # @gen_split_912
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp3239:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp3240:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp3241:
	.cfi_def_cfa_offset 32
.Ltmp3242:
	.cfi_offset %rbx, -24
.Ltmp3243:
	.cfi_offset %r14, -16
	movq	%rdi, %rbx
	callq	start_sequence
	movq	(%rbx), %r14
	movq	8(%rbx), %rdx
	movq	16(%rbx), %rbx
	movq	$124, %rdi
	movq	$15, %rsi
	callq	gen_rtx_fmt_e
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	movq	%rbx, %rdi
	callq	copy_rtx
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp3244:
	.size	gen_split_912, .Ltmp3244-gen_split_912
	.cfi_endproc

	.globl	gen_truncxfsf2
	.align	16, 0x90
	.type	gen_truncxfsf2,@function
gen_truncxfsf2:                         # @gen_truncxfsf2
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp3249:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp3250:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp3251:
	.cfi_def_cfa_offset 32
.Ltmp3252:
	.cfi_offset %rbx, -32
.Ltmp3253:
	.cfi_offset %r14, -24
.Ltmp3254:
	.cfi_offset %r15, -16
	movq	%rsi, %rbx
	movq	%rdi, %r14
	callq	start_sequence
	movq	$15, %rdi
	xorq	%rsi, %rsi
	callq	assign_386_stack_local
	movq	%rax, %r15
	movq	$124, %rdi
	movq	$15, %rsi
	movq	%rbx, %rdx
	callq	gen_rtx_fmt_e
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rbx
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%r15, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rcx
	movq	$2, %rdi
	xorq	%rax, %rax
	movq	%rbx, %rsi
	movq	%rcx, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_E
	movq	%rax, %rdi
	callq	emit
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp3255:
	.size	gen_truncxfsf2, .Ltmp3255-gen_truncxfsf2
	.cfi_endproc

	.globl	gen_split_914
	.align	16, 0x90
	.type	gen_split_914,@function
gen_split_914:                          # @gen_split_914
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp3259:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp3260:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp3261:
	.cfi_def_cfa_offset 32
.Ltmp3262:
	.cfi_offset %rbx, -24
.Ltmp3263:
	.cfi_offset %r14, -16
	movq	%rdi, %rbx
	callq	start_sequence
	movq	(%rbx), %r14
	movq	8(%rbx), %rdx
	movq	$124, %rdi
	movq	$15, %rsi
	callq	gen_rtx_fmt_e
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp3264:
	.size	gen_split_914, .Ltmp3264-gen_split_914
	.cfi_endproc

	.globl	gen_split_915
	.align	16, 0x90
	.type	gen_split_915,@function
gen_split_915:                          # @gen_split_915
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp3268:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp3269:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp3270:
	.cfi_def_cfa_offset 32
.Ltmp3271:
	.cfi_offset %rbx, -24
.Ltmp3272:
	.cfi_offset %r14, -16
	movq	%rdi, %rbx
	callq	start_sequence
	movq	(%rbx), %r14
	movq	8(%rbx), %rdx
	movq	16(%rbx), %rbx
	movq	$124, %rdi
	movq	$15, %rsi
	callq	gen_rtx_fmt_e
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	movq	%rbx, %rdi
	callq	copy_rtx
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp3273:
	.size	gen_split_915, .Ltmp3273-gen_split_915
	.cfi_endproc

	.globl	gen_trunctfsf2
	.align	16, 0x90
	.type	gen_trunctfsf2,@function
gen_trunctfsf2:                         # @gen_trunctfsf2
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp3278:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp3279:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp3280:
	.cfi_def_cfa_offset 32
.Ltmp3281:
	.cfi_offset %rbx, -32
.Ltmp3282:
	.cfi_offset %r14, -24
.Ltmp3283:
	.cfi_offset %r15, -16
	movq	%rsi, %rbx
	movq	%rdi, %r14
	callq	start_sequence
	movq	$15, %rdi
	xorq	%rsi, %rsi
	callq	assign_386_stack_local
	movq	%rax, %r15
	movq	$124, %rdi
	movq	$15, %rsi
	movq	%rbx, %rdx
	callq	gen_rtx_fmt_e
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rbx
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%r15, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rcx
	movq	$2, %rdi
	xorq	%rax, %rax
	movq	%rbx, %rsi
	movq	%rcx, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_E
	movq	%rax, %rdi
	callq	emit
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp3284:
	.size	gen_trunctfsf2, .Ltmp3284-gen_trunctfsf2
	.cfi_endproc

	.globl	gen_split_917
	.align	16, 0x90
	.type	gen_split_917,@function
gen_split_917:                          # @gen_split_917
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp3288:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp3289:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp3290:
	.cfi_def_cfa_offset 32
.Ltmp3291:
	.cfi_offset %rbx, -24
.Ltmp3292:
	.cfi_offset %r14, -16
	movq	%rdi, %rbx
	callq	start_sequence
	movq	(%rbx), %r14
	movq	8(%rbx), %rdx
	movq	$124, %rdi
	movq	$15, %rsi
	callq	gen_rtx_fmt_e
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp3293:
	.size	gen_split_917, .Ltmp3293-gen_split_917
	.cfi_endproc

	.globl	gen_split_918
	.align	16, 0x90
	.type	gen_split_918,@function
gen_split_918:                          # @gen_split_918
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp3297:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp3298:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp3299:
	.cfi_def_cfa_offset 32
.Ltmp3300:
	.cfi_offset %rbx, -24
.Ltmp3301:
	.cfi_offset %r14, -16
	movq	%rdi, %rbx
	callq	start_sequence
	movq	(%rbx), %r14
	movq	8(%rbx), %rdx
	movq	16(%rbx), %rbx
	movq	$124, %rdi
	movq	$15, %rsi
	callq	gen_rtx_fmt_e
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	movq	%rbx, %rdi
	callq	copy_rtx
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp3302:
	.size	gen_split_918, .Ltmp3302-gen_split_918
	.cfi_endproc

	.globl	gen_truncxfdf2
	.align	16, 0x90
	.type	gen_truncxfdf2,@function
gen_truncxfdf2:                         # @gen_truncxfdf2
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp3307:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp3308:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp3309:
	.cfi_def_cfa_offset 32
.Ltmp3310:
	.cfi_offset %rbx, -32
.Ltmp3311:
	.cfi_offset %r14, -24
.Ltmp3312:
	.cfi_offset %r15, -16
	movq	%rsi, %rbx
	movq	%rdi, %r14
	callq	start_sequence
	movq	$16, %rdi
	xorq	%rsi, %rsi
	callq	assign_386_stack_local
	movq	%rax, %r15
	movq	$124, %rdi
	movq	$16, %rsi
	movq	%rbx, %rdx
	callq	gen_rtx_fmt_e
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rbx
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%r15, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rcx
	movq	$2, %rdi
	xorq	%rax, %rax
	movq	%rbx, %rsi
	movq	%rcx, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_E
	movq	%rax, %rdi
	callq	emit
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp3313:
	.size	gen_truncxfdf2, .Ltmp3313-gen_truncxfdf2
	.cfi_endproc

	.globl	gen_split_920
	.align	16, 0x90
	.type	gen_split_920,@function
gen_split_920:                          # @gen_split_920
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp3317:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp3318:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp3319:
	.cfi_def_cfa_offset 32
.Ltmp3320:
	.cfi_offset %rbx, -24
.Ltmp3321:
	.cfi_offset %r14, -16
	movq	%rdi, %rbx
	callq	start_sequence
	movq	(%rbx), %r14
	movq	8(%rbx), %rdx
	movq	$124, %rdi
	movq	$16, %rsi
	callq	gen_rtx_fmt_e
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp3322:
	.size	gen_split_920, .Ltmp3322-gen_split_920
	.cfi_endproc

	.globl	gen_split_921
	.align	16, 0x90
	.type	gen_split_921,@function
gen_split_921:                          # @gen_split_921
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp3326:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp3327:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp3328:
	.cfi_def_cfa_offset 32
.Ltmp3329:
	.cfi_offset %rbx, -24
.Ltmp3330:
	.cfi_offset %r14, -16
	movq	%rdi, %rbx
	callq	start_sequence
	movq	(%rbx), %r14
	movq	8(%rbx), %rdx
	movq	16(%rbx), %rbx
	movq	$124, %rdi
	movq	$16, %rsi
	callq	gen_rtx_fmt_e
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	movq	%rbx, %rdi
	callq	copy_rtx
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp3331:
	.size	gen_split_921, .Ltmp3331-gen_split_921
	.cfi_endproc

	.globl	gen_trunctfdf2
	.align	16, 0x90
	.type	gen_trunctfdf2,@function
gen_trunctfdf2:                         # @gen_trunctfdf2
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp3336:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp3337:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp3338:
	.cfi_def_cfa_offset 32
.Ltmp3339:
	.cfi_offset %rbx, -32
.Ltmp3340:
	.cfi_offset %r14, -24
.Ltmp3341:
	.cfi_offset %r15, -16
	movq	%rsi, %rbx
	movq	%rdi, %r14
	callq	start_sequence
	movq	$16, %rdi
	xorq	%rsi, %rsi
	callq	assign_386_stack_local
	movq	%rax, %r15
	movq	$124, %rdi
	movq	$16, %rsi
	movq	%rbx, %rdx
	callq	gen_rtx_fmt_e
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rbx
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%r15, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rcx
	movq	$2, %rdi
	xorq	%rax, %rax
	movq	%rbx, %rsi
	movq	%rcx, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_E
	movq	%rax, %rdi
	callq	emit
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp3342:
	.size	gen_trunctfdf2, .Ltmp3342-gen_trunctfdf2
	.cfi_endproc

	.globl	gen_split_923
	.align	16, 0x90
	.type	gen_split_923,@function
gen_split_923:                          # @gen_split_923
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp3346:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp3347:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp3348:
	.cfi_def_cfa_offset 32
.Ltmp3349:
	.cfi_offset %rbx, -24
.Ltmp3350:
	.cfi_offset %r14, -16
	movq	%rdi, %rbx
	callq	start_sequence
	movq	(%rbx), %r14
	movq	8(%rbx), %rdx
	movq	$124, %rdi
	movq	$16, %rsi
	callq	gen_rtx_fmt_e
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp3351:
	.size	gen_split_923, .Ltmp3351-gen_split_923
	.cfi_endproc

	.globl	gen_split_924
	.align	16, 0x90
	.type	gen_split_924,@function
gen_split_924:                          # @gen_split_924
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp3355:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp3356:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp3357:
	.cfi_def_cfa_offset 32
.Ltmp3358:
	.cfi_offset %rbx, -24
.Ltmp3359:
	.cfi_offset %r14, -16
	movq	%rdi, %rbx
	callq	start_sequence
	movq	(%rbx), %r14
	movq	8(%rbx), %rdx
	movq	16(%rbx), %rbx
	movq	$124, %rdi
	movq	$16, %rsi
	callq	gen_rtx_fmt_e
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	movq	%rbx, %rdi
	callq	copy_rtx
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp3360:
	.size	gen_split_924, .Ltmp3360-gen_split_924
	.cfi_endproc

	.globl	gen_fix_truncxfdi2
	.align	16, 0x90
	.type	gen_fix_truncxfdi2,@function
gen_fix_truncxfdi2:                     # @gen_fix_truncxfdi2
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp3363:
	.cfi_def_cfa_offset 16
.Ltmp3364:
	.cfi_offset %rbx, -16
	movq	%rsi, %rax
	movq	%rdi, %rbx
	movq	$126, %rdi
	movq	$5, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	popq	%rbx
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp3365:
	.size	gen_fix_truncxfdi2, .Ltmp3365-gen_fix_truncxfdi2
	.cfi_endproc

	.globl	gen_fix_trunctfdi2
	.align	16, 0x90
	.type	gen_fix_trunctfdi2,@function
gen_fix_trunctfdi2:                     # @gen_fix_trunctfdi2
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp3368:
	.cfi_def_cfa_offset 16
.Ltmp3369:
	.cfi_offset %rbx, -16
	movq	%rsi, %rax
	movq	%rdi, %rbx
	movq	$126, %rdi
	movq	$5, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	popq	%rbx
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp3370:
	.size	gen_fix_trunctfdi2, .Ltmp3370-gen_fix_trunctfdi2
	.cfi_endproc

	.globl	gen_fix_truncdfdi2
	.align	16, 0x90
	.type	gen_fix_truncdfdi2,@function
gen_fix_truncdfdi2:                     # @gen_fix_truncdfdi2
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp3375:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp3376:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp3377:
	.cfi_def_cfa_offset 32
.Ltmp3378:
	.cfi_offset %rbx, -32
.Ltmp3379:
	.cfi_offset %r14, -24
.Ltmp3380:
	.cfi_offset %r15, -16
	movq	%rsi, %r14
	movq	%rdi, %rbx
	callq	start_sequence
	movl	target_flags(%rip), %eax
	andq	$33816576, %rax         # imm = 0x2040000
	cmpq	$33816576, %rax         # imm = 0x2040000
	jne	.LBB383_6
# BB#1:                                 # %if.then
	movzbl	(%rbx), %eax
	cmpq	$61, %rax
	movq	%rbx, %r15
	je	.LBB383_3
# BB#2:                                 # %cond.false
	movq	$5, %rdi
	callq	gen_reg_rtx
	movq	%rax, %r15
.LBB383_3:                              # %cond.end
	movq	$126, %rdi
	movq	$5, %rsi
	movq	%r14, %rdx
	callq	gen_rtx_fmt_e
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r15, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	cmpq	%rbx, %r15
	je	.LBB383_5
# BB#4:                                 # %if.then11
	movq	%rbx, %rdi
	movq	%r15, %rsi
	callq	emit_move_insn
	jmp	.LBB383_5
.LBB383_6:                              # %if.end15
	movq	$126, %rdi
	movq	$5, %rsi
	movq	%r14, %rdx
	callq	gen_rtx_fmt_e
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
.LBB383_5:                              # %if.end
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp3381:
	.size	gen_fix_truncdfdi2, .Ltmp3381-gen_fix_truncdfdi2
	.cfi_endproc

	.globl	gen_fix_truncsfdi2
	.align	16, 0x90
	.type	gen_fix_truncsfdi2,@function
gen_fix_truncsfdi2:                     # @gen_fix_truncsfdi2
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp3386:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp3387:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp3388:
	.cfi_def_cfa_offset 32
.Ltmp3389:
	.cfi_offset %rbx, -32
.Ltmp3390:
	.cfi_offset %r14, -24
.Ltmp3391:
	.cfi_offset %r15, -16
	movq	%rsi, %r14
	movq	%rdi, %r15
	callq	start_sequence
	movl	target_flags(%rip), %eax
	testq	$327680, %rax           # imm = 0x50000
	je	.LBB384_7
# BB#1:                                 # %entry
	andq	$33554432, %rax         # imm = 0x2000000
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB384_7
# BB#2:                                 # %if.then
	movzbl	(%r15), %eax
	cmpq	$61, %rax
	movq	%r15, %rbx
	je	.LBB384_4
# BB#3:                                 # %cond.false
	movq	$5, %rdi
	callq	gen_reg_rtx
	movq	%rax, %rbx
.LBB384_4:                              # %cond.end
	movq	$126, %rdi
	movq	$5, %rsi
	movq	%r14, %rdx
	callq	gen_rtx_fmt_e
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	cmpq	%r15, %rbx
	je	.LBB384_6
# BB#5:                                 # %if.then11
	movq	%r15, %rdi
	movq	%rbx, %rsi
	callq	emit_move_insn
	jmp	.LBB384_6
.LBB384_7:                              # %if.end15
	movq	$126, %rdi
	movq	$5, %rsi
	movq	%r14, %rdx
	callq	gen_rtx_fmt_e
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r15, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
.LBB384_6:                              # %if.end
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp3392:
	.size	gen_fix_truncsfdi2, .Ltmp3392-gen_fix_truncsfdi2
	.cfi_endproc

	.globl	gen_split_929
	.align	16, 0x90
	.type	gen_split_929,@function
gen_split_929:                          # @gen_split_929
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp3399:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp3400:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp3401:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp3402:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp3403:
	.cfi_def_cfa_offset 48
.Ltmp3404:
	.cfi_offset %rbx, -48
.Ltmp3405:
	.cfi_offset %r12, -40
.Ltmp3406:
	.cfi_offset %r13, -32
.Ltmp3407:
	.cfi_offset %r14, -24
.Ltmp3408:
	.cfi_offset %r15, -16
	movq	%rdi, %rbx
	callq	start_sequence
	movq	$3, %rdi
	movq	$1, %rsi
	callq	assign_386_stack_local
	movq	%rax, 16(%rbx)
	movq	$3, %rdi
	movq	$2, %rsi
	callq	assign_386_stack_local
	movq	%rax, 24(%rbx)
	movq	(%rbx), %rdi
	xorq	%rsi, %rsi
	callq	memory_operand
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB385_2
# BB#1:                                 # %if.then
	movq	(%rbx), %r14
	movq	8(%rbx), %rdx
	movq	16(%rbx), %r15
	movq	24(%rbx), %rbx
	movq	$126, %rdi
	movq	$5, %rsi
	callq	gen_rtx_fmt_e
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %r14
	movq	$48, %rdi
	xorq	%rsi, %rsi
	movq	%r15, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %r15
	movq	$48, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %r12
	movq	$62, %rdi
	movq	$16, %rsi
	callq	gen_rtx_fmt_0
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rbx
	movq	$4, %rdi
	xorq	%rax, %rax
	movq	%r14, %rsi
	movq	%r15, %rdx
	movq	%r12, %rcx
	movq	%rbx, %r8
	callq	gen_rtvec
	jmp	.LBB385_3
.LBB385_2:                              # %if.else
	movq	$5, %rdi
	xorq	%rsi, %rsi
	callq	assign_386_stack_local
	movq	%rax, %r14
	movq	%r14, 32(%rbx)
	movq	(%rbx), %r15
	movq	8(%rbx), %rdx
	movq	16(%rbx), %r12
	movq	24(%rbx), %rbx
	movq	$126, %rdi
	movq	$5, %rsi
	callq	gen_rtx_fmt_e
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r15, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %r15
	movq	$48, %rdi
	xorq	%rsi, %rsi
	movq	%r12, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %r12
	movq	$48, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %r13
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %r14
	movq	$62, %rdi
	movq	$16, %rsi
	callq	gen_rtx_fmt_0
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rbx
	movq	$5, %rdi
	xorq	%rax, %rax
	movq	%r15, %rsi
	movq	%r12, %rdx
	movq	%r13, %rcx
	movq	%r14, %r8
	movq	%rbx, %r9
	callq	gen_rtvec
.LBB385_3:                              # %if.end
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_E
	movq	%rax, %rdi
	callq	emit_insn
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp3409:
	.size	gen_split_929, .Ltmp3409-gen_split_929
	.cfi_endproc

	.globl	gen_split_930
	.align	16, 0x90
	.type	gen_split_930,@function
gen_split_930:                          # @gen_split_930
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp3416:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp3417:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp3418:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp3419:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp3420:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp3421:
	.cfi_def_cfa_offset 64
.Ltmp3422:
	.cfi_offset %rbx, -48
.Ltmp3423:
	.cfi_offset %r12, -40
.Ltmp3424:
	.cfi_offset %r13, -32
.Ltmp3425:
	.cfi_offset %r14, -24
.Ltmp3426:
	.cfi_offset %r15, -16
	movq	%rdi, %rbx
	callq	start_sequence
	movq	(%rbx), %rax
	movq	%rax, 8(%rsp)           # 8-byte Spill
	movq	8(%rbx), %rdx
	movq	16(%rbx), %r13
	movq	24(%rbx), %r14
	movq	32(%rbx), %r12
	movq	40(%rbx), %rbx
	movq	$126, %rdi
	movq	$5, %rsi
	callq	gen_rtx_fmt_e
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r12, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %r15
	movq	$48, %rdi
	xorq	%rsi, %rsi
	movq	%r13, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %r13
	movq	$48, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %r14
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rbx
	movq	$4, %rdi
	xorq	%rax, %rax
	movq	%r15, %rsi
	movq	%r13, %rdx
	movq	%r14, %rcx
	movq	%rbx, %r8
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_E
	movq	%rax, %rdi
	callq	emit
	movq	%r12, %rdi
	callq	copy_rtx
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	8(%rsp), %rdx           # 8-byte Reload
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp3427:
	.size	gen_split_930, .Ltmp3427-gen_split_930
	.cfi_endproc

	.globl	gen_split_931
	.align	16, 0x90
	.type	gen_split_931,@function
gen_split_931:                          # @gen_split_931
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp3434:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp3435:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp3436:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp3437:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp3438:
	.cfi_def_cfa_offset 48
.Ltmp3439:
	.cfi_offset %rbx, -48
.Ltmp3440:
	.cfi_offset %r12, -40
.Ltmp3441:
	.cfi_offset %r13, -32
.Ltmp3442:
	.cfi_offset %r14, -24
.Ltmp3443:
	.cfi_offset %r15, -16
	movq	%rdi, %rbx
	callq	start_sequence
	movq	(%rbx), %r14
	movq	8(%rbx), %rdx
	movq	16(%rbx), %r15
	movq	24(%rbx), %r12
	movq	40(%rbx), %r13
	movq	$126, %rdi
	movq	$5, %rsi
	callq	gen_rtx_fmt_e
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %r14
	movq	$48, %rdi
	xorq	%rsi, %rsi
	movq	%r15, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %r15
	movq	$48, %rdi
	xorq	%rsi, %rsi
	movq	%r12, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %r12
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%r13, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rbx
	movq	$4, %rdi
	xorq	%rax, %rax
	movq	%r14, %rsi
	movq	%r15, %rdx
	movq	%r12, %rcx
	movq	%rbx, %r8
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_E
	movq	%rax, %rdi
	callq	emit
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp3444:
	.size	gen_split_931, .Ltmp3444-gen_split_931
	.cfi_endproc

	.globl	gen_fix_truncxfsi2
	.align	16, 0x90
	.type	gen_fix_truncxfsi2,@function
gen_fix_truncxfsi2:                     # @gen_fix_truncxfsi2
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp3447:
	.cfi_def_cfa_offset 16
.Ltmp3448:
	.cfi_offset %rbx, -16
	movq	%rsi, %rax
	movq	%rdi, %rbx
	movq	$126, %rdi
	movq	$4, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	popq	%rbx
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp3449:
	.size	gen_fix_truncxfsi2, .Ltmp3449-gen_fix_truncxfsi2
	.cfi_endproc

	.globl	gen_fix_trunctfsi2
	.align	16, 0x90
	.type	gen_fix_trunctfsi2,@function
gen_fix_trunctfsi2:                     # @gen_fix_trunctfsi2
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp3452:
	.cfi_def_cfa_offset 16
.Ltmp3453:
	.cfi_offset %rbx, -16
	movq	%rsi, %rax
	movq	%rdi, %rbx
	movq	$126, %rdi
	movq	$4, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	popq	%rbx
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp3454:
	.size	gen_fix_trunctfsi2, .Ltmp3454-gen_fix_trunctfsi2
	.cfi_endproc

	.globl	gen_fix_truncdfsi2
	.align	16, 0x90
	.type	gen_fix_truncdfsi2,@function
gen_fix_truncdfsi2:                     # @gen_fix_truncdfsi2
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp3459:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp3460:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp3461:
	.cfi_def_cfa_offset 32
.Ltmp3462:
	.cfi_offset %rbx, -32
.Ltmp3463:
	.cfi_offset %r14, -24
.Ltmp3464:
	.cfi_offset %r15, -16
	movq	%rsi, %r14
	movq	%rdi, %r15
	callq	start_sequence
	movzbl	target_flags+2(%rip), %eax
	testq	$4, %rax
	je	.LBB390_6
# BB#1:                                 # %if.then
	movzbl	(%r15), %eax
	cmpq	$61, %rax
	movq	%r15, %rbx
	je	.LBB390_3
# BB#2:                                 # %cond.false
	movq	$4, %rdi
	callq	gen_reg_rtx
	movq	%rax, %rbx
.LBB390_3:                              # %cond.end
	movq	$126, %rdi
	movq	$4, %rsi
	movq	%r14, %rdx
	callq	gen_rtx_fmt_e
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	cmpq	%r15, %rbx
	je	.LBB390_5
# BB#4:                                 # %if.then10
	movq	%r15, %rdi
	movq	%rbx, %rsi
	callq	emit_move_insn
	jmp	.LBB390_5
.LBB390_6:                              # %if.end14
	movq	$126, %rdi
	movq	$4, %rsi
	movq	%r14, %rdx
	callq	gen_rtx_fmt_e
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r15, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
.LBB390_5:                              # %if.end
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp3465:
	.size	gen_fix_truncdfsi2, .Ltmp3465-gen_fix_truncdfsi2
	.cfi_endproc

	.globl	gen_fix_truncsfsi2
	.align	16, 0x90
	.type	gen_fix_truncsfsi2,@function
gen_fix_truncsfsi2:                     # @gen_fix_truncsfsi2
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp3470:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp3471:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp3472:
	.cfi_def_cfa_offset 32
.Ltmp3473:
	.cfi_offset %rbx, -32
.Ltmp3474:
	.cfi_offset %r14, -24
.Ltmp3475:
	.cfi_offset %r15, -16
	movq	%rsi, %r14
	movq	%rdi, %r15
	callq	start_sequence
	movzbl	target_flags+2(%rip), %eax
	testq	$5, %rax
	je	.LBB391_6
# BB#1:                                 # %if.then
	movzbl	(%r15), %eax
	cmpq	$61, %rax
	movq	%r15, %rbx
	je	.LBB391_3
# BB#2:                                 # %cond.false
	movq	$4, %rdi
	callq	gen_reg_rtx
	movq	%rax, %rbx
.LBB391_3:                              # %cond.end
	movq	$126, %rdi
	movq	$4, %rsi
	movq	%r14, %rdx
	callq	gen_rtx_fmt_e
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	cmpq	%r15, %rbx
	je	.LBB391_5
# BB#4:                                 # %if.then10
	movq	%r15, %rdi
	movq	%rbx, %rsi
	callq	emit_move_insn
	jmp	.LBB391_5
.LBB391_6:                              # %if.end14
	movq	$126, %rdi
	movq	$4, %rsi
	movq	%r14, %rdx
	callq	gen_rtx_fmt_e
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r15, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
.LBB391_5:                              # %if.end
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp3476:
	.size	gen_fix_truncsfsi2, .Ltmp3476-gen_fix_truncsfsi2
	.cfi_endproc

	.globl	gen_split_936
	.align	16, 0x90
	.type	gen_split_936,@function
gen_split_936:                          # @gen_split_936
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp3483:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp3484:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp3485:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp3486:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp3487:
	.cfi_def_cfa_offset 48
.Ltmp3488:
	.cfi_offset %rbx, -48
.Ltmp3489:
	.cfi_offset %r12, -40
.Ltmp3490:
	.cfi_offset %r13, -32
.Ltmp3491:
	.cfi_offset %r14, -24
.Ltmp3492:
	.cfi_offset %r15, -16
	movq	%rdi, %rbx
	callq	start_sequence
	movq	$3, %rdi
	movq	$1, %rsi
	callq	assign_386_stack_local
	movq	%rax, 16(%rbx)
	movq	$3, %rdi
	movq	$2, %rsi
	callq	assign_386_stack_local
	movq	%rax, 24(%rbx)
	movq	(%rbx), %rdi
	xorq	%rsi, %rsi
	callq	memory_operand
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB392_2
# BB#1:                                 # %if.then
	movq	(%rbx), %r14
	movq	8(%rbx), %rdx
	movq	16(%rbx), %r15
	movq	24(%rbx), %r12
	movq	$126, %rdi
	movq	$4, %rsi
	callq	gen_rtx_fmt_e
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %r14
	movq	$48, %rdi
	xorq	%rsi, %rsi
	movq	%r15, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rbx
	movq	$48, %rdi
	xorq	%rsi, %rsi
	movq	%r12, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rcx
	movq	$3, %rdi
	xorq	%rax, %rax
	movq	%r14, %rsi
	movq	%rbx, %rdx
	callq	gen_rtvec
	jmp	.LBB392_3
.LBB392_2:                              # %if.else
	movq	$4, %rdi
	xorq	%rsi, %rsi
	callq	assign_386_stack_local
	movq	%rax, %r14
	movq	%r14, 32(%rbx)
	movq	(%rbx), %r15
	movq	8(%rbx), %rdx
	movq	16(%rbx), %r12
	movq	24(%rbx), %r13
	movq	$126, %rdi
	movq	$4, %rsi
	callq	gen_rtx_fmt_e
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r15, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %r15
	movq	$48, %rdi
	xorq	%rsi, %rsi
	movq	%r12, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %r12
	movq	$48, %rdi
	xorq	%rsi, %rsi
	movq	%r13, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %r13
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rbx
	movq	$4, %rdi
	xorq	%rax, %rax
	movq	%r15, %rsi
	movq	%r12, %rdx
	movq	%r13, %rcx
	movq	%rbx, %r8
	callq	gen_rtvec
.LBB392_3:                              # %if.end
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_E
	movq	%rax, %rdi
	callq	emit_insn
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp3493:
	.size	gen_split_936, .Ltmp3493-gen_split_936
	.cfi_endproc

	.globl	gen_split_937
	.align	16, 0x90
	.type	gen_split_937,@function
gen_split_937:                          # @gen_split_937
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp3500:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp3501:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp3502:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp3503:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp3504:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp3505:
	.cfi_def_cfa_offset 64
.Ltmp3506:
	.cfi_offset %rbx, -48
.Ltmp3507:
	.cfi_offset %r12, -40
.Ltmp3508:
	.cfi_offset %r13, -32
.Ltmp3509:
	.cfi_offset %r14, -24
.Ltmp3510:
	.cfi_offset %r15, -16
	movq	%rdi, %rbx
	callq	start_sequence
	movq	(%rbx), %rax
	movq	%rax, 8(%rsp)           # 8-byte Spill
	movq	8(%rbx), %rdx
	movq	16(%rbx), %r15
	movq	24(%rbx), %r12
	movq	32(%rbx), %r13
	movq	$126, %rdi
	movq	$4, %rsi
	callq	gen_rtx_fmt_e
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r13, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %r14
	movq	$48, %rdi
	xorq	%rsi, %rsi
	movq	%r15, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rbx
	movq	$48, %rdi
	xorq	%rsi, %rsi
	movq	%r12, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rcx
	movq	$3, %rdi
	xorq	%rax, %rax
	movq	%r14, %rsi
	movq	%rbx, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_E
	movq	%rax, %rdi
	callq	emit
	movq	%r13, %rdi
	callq	copy_rtx
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	8(%rsp), %rdx           # 8-byte Reload
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp3511:
	.size	gen_split_937, .Ltmp3511-gen_split_937
	.cfi_endproc

	.globl	gen_split_938
	.align	16, 0x90
	.type	gen_split_938,@function
gen_split_938:                          # @gen_split_938
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp3517:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp3518:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp3519:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp3520:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp3521:
	.cfi_def_cfa_offset 48
.Ltmp3522:
	.cfi_offset %rbx, -40
.Ltmp3523:
	.cfi_offset %r12, -32
.Ltmp3524:
	.cfi_offset %r14, -24
.Ltmp3525:
	.cfi_offset %r15, -16
	movq	%rdi, %rbx
	callq	start_sequence
	movq	(%rbx), %r14
	movq	8(%rbx), %rdx
	movq	16(%rbx), %r15
	movq	24(%rbx), %r12
	movq	$126, %rdi
	movq	$4, %rsi
	callq	gen_rtx_fmt_e
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %r14
	movq	$48, %rdi
	xorq	%rsi, %rsi
	movq	%r15, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rbx
	movq	$48, %rdi
	xorq	%rsi, %rsi
	movq	%r12, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rcx
	movq	$3, %rdi
	xorq	%rax, %rax
	movq	%r14, %rsi
	movq	%rbx, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_E
	movq	%rax, %rdi
	callq	emit
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.Ltmp3526:
	.size	gen_split_938, .Ltmp3526-gen_split_938
	.cfi_endproc

	.globl	gen_fix_truncxfhi2
	.align	16, 0x90
	.type	gen_fix_truncxfhi2,@function
gen_fix_truncxfhi2:                     # @gen_fix_truncxfhi2
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp3529:
	.cfi_def_cfa_offset 16
.Ltmp3530:
	.cfi_offset %rbx, -16
	movq	%rsi, %rax
	movq	%rdi, %rbx
	movq	$126, %rdi
	movq	$3, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	popq	%rbx
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp3531:
	.size	gen_fix_truncxfhi2, .Ltmp3531-gen_fix_truncxfhi2
	.cfi_endproc

	.globl	gen_fix_trunctfhi2
	.align	16, 0x90
	.type	gen_fix_trunctfhi2,@function
gen_fix_trunctfhi2:                     # @gen_fix_trunctfhi2
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp3534:
	.cfi_def_cfa_offset 16
.Ltmp3535:
	.cfi_offset %rbx, -16
	movq	%rsi, %rax
	movq	%rdi, %rbx
	movq	$126, %rdi
	movq	$3, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	popq	%rbx
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp3536:
	.size	gen_fix_trunctfhi2, .Ltmp3536-gen_fix_trunctfhi2
	.cfi_endproc

	.globl	gen_fix_truncdfhi2
	.align	16, 0x90
	.type	gen_fix_truncdfhi2,@function
gen_fix_truncdfhi2:                     # @gen_fix_truncdfhi2
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp3539:
	.cfi_def_cfa_offset 16
.Ltmp3540:
	.cfi_offset %rbx, -16
	movq	%rsi, %rax
	movq	%rdi, %rbx
	movq	$126, %rdi
	movq	$3, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	popq	%rbx
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp3541:
	.size	gen_fix_truncdfhi2, .Ltmp3541-gen_fix_truncdfhi2
	.cfi_endproc

	.globl	gen_fix_truncsfhi2
	.align	16, 0x90
	.type	gen_fix_truncsfhi2,@function
gen_fix_truncsfhi2:                     # @gen_fix_truncsfhi2
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp3544:
	.cfi_def_cfa_offset 16
.Ltmp3545:
	.cfi_offset %rbx, -16
	movq	%rsi, %rax
	movq	%rdi, %rbx
	movq	$126, %rdi
	movq	$3, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	popq	%rbx
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp3546:
	.size	gen_fix_truncsfhi2, .Ltmp3546-gen_fix_truncsfhi2
	.cfi_endproc

	.globl	gen_split_943
	.align	16, 0x90
	.type	gen_split_943,@function
gen_split_943:                          # @gen_split_943
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp3553:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp3554:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp3555:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp3556:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp3557:
	.cfi_def_cfa_offset 48
.Ltmp3558:
	.cfi_offset %rbx, -48
.Ltmp3559:
	.cfi_offset %r12, -40
.Ltmp3560:
	.cfi_offset %r13, -32
.Ltmp3561:
	.cfi_offset %r14, -24
.Ltmp3562:
	.cfi_offset %r15, -16
	movq	%rdi, %rbx
	callq	start_sequence
	movq	$3, %rdi
	movq	$1, %rsi
	callq	assign_386_stack_local
	movq	%rax, 16(%rbx)
	movq	$3, %rdi
	movq	$2, %rsi
	callq	assign_386_stack_local
	movq	%rax, 24(%rbx)
	movq	(%rbx), %rdi
	xorq	%rsi, %rsi
	callq	memory_operand
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB399_2
# BB#1:                                 # %if.then
	movq	(%rbx), %r14
	movq	8(%rbx), %rdx
	movq	16(%rbx), %r15
	movq	24(%rbx), %r12
	movq	$126, %rdi
	movq	$3, %rsi
	callq	gen_rtx_fmt_e
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %r14
	movq	$48, %rdi
	xorq	%rsi, %rsi
	movq	%r15, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rbx
	movq	$48, %rdi
	xorq	%rsi, %rsi
	movq	%r12, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rcx
	movq	$3, %rdi
	xorq	%rax, %rax
	movq	%r14, %rsi
	movq	%rbx, %rdx
	callq	gen_rtvec
	jmp	.LBB399_3
.LBB399_2:                              # %if.else
	movq	$3, %rdi
	xorq	%rsi, %rsi
	callq	assign_386_stack_local
	movq	%rax, %r14
	movq	%r14, 32(%rbx)
	movq	(%rbx), %r15
	movq	8(%rbx), %rdx
	movq	16(%rbx), %r12
	movq	24(%rbx), %r13
	movq	$126, %rdi
	movq	$3, %rsi
	callq	gen_rtx_fmt_e
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r15, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %r15
	movq	$48, %rdi
	xorq	%rsi, %rsi
	movq	%r12, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %r12
	movq	$48, %rdi
	xorq	%rsi, %rsi
	movq	%r13, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %r13
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rbx
	movq	$4, %rdi
	xorq	%rax, %rax
	movq	%r15, %rsi
	movq	%r12, %rdx
	movq	%r13, %rcx
	movq	%rbx, %r8
	callq	gen_rtvec
.LBB399_3:                              # %if.end
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_E
	movq	%rax, %rdi
	callq	emit_insn
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp3563:
	.size	gen_split_943, .Ltmp3563-gen_split_943
	.cfi_endproc

	.globl	gen_split_944
	.align	16, 0x90
	.type	gen_split_944,@function
gen_split_944:                          # @gen_split_944
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp3569:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp3570:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp3571:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp3572:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp3573:
	.cfi_def_cfa_offset 48
.Ltmp3574:
	.cfi_offset %rbx, -40
.Ltmp3575:
	.cfi_offset %r12, -32
.Ltmp3576:
	.cfi_offset %r14, -24
.Ltmp3577:
	.cfi_offset %r15, -16
	movq	%rdi, %rbx
	callq	start_sequence
	movq	(%rbx), %r14
	movq	8(%rbx), %rdx
	movq	16(%rbx), %r15
	movq	24(%rbx), %r12
	movq	$126, %rdi
	movq	$3, %rsi
	callq	gen_rtx_fmt_e
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %r14
	movq	$48, %rdi
	xorq	%rsi, %rsi
	movq	%r15, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rbx
	movq	$48, %rdi
	xorq	%rsi, %rsi
	movq	%r12, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rcx
	movq	$3, %rdi
	xorq	%rax, %rax
	movq	%r14, %rsi
	movq	%rbx, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_E
	movq	%rax, %rdi
	callq	emit
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.Ltmp3578:
	.size	gen_split_944, .Ltmp3578-gen_split_944
	.cfi_endproc

	.globl	gen_split_945
	.align	16, 0x90
	.type	gen_split_945,@function
gen_split_945:                          # @gen_split_945
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp3585:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp3586:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp3587:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp3588:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp3589:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp3590:
	.cfi_def_cfa_offset 64
.Ltmp3591:
	.cfi_offset %rbx, -48
.Ltmp3592:
	.cfi_offset %r12, -40
.Ltmp3593:
	.cfi_offset %r13, -32
.Ltmp3594:
	.cfi_offset %r14, -24
.Ltmp3595:
	.cfi_offset %r15, -16
	movq	%rdi, %rbx
	callq	start_sequence
	movq	(%rbx), %rax
	movq	%rax, 8(%rsp)           # 8-byte Spill
	movq	8(%rbx), %rdx
	movq	16(%rbx), %r12
	movq	24(%rbx), %r13
	movq	32(%rbx), %r14
	movq	$126, %rdi
	movq	$3, %rsi
	callq	gen_rtx_fmt_e
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %r15
	movq	$48, %rdi
	xorq	%rsi, %rsi
	movq	%r12, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %r12
	movq	$48, %rdi
	xorq	%rsi, %rsi
	movq	%r13, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %r13
	movq	%r14, %rdi
	callq	copy_rtx
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rbx
	movq	$4, %rdi
	xorq	%rax, %rax
	movq	%r15, %rsi
	movq	%r12, %rdx
	movq	%r13, %rcx
	movq	%rbx, %r8
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_E
	movq	%rax, %rdi
	callq	emit
	movq	%r14, %rdi
	callq	copy_rtx
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	8(%rsp), %rdx           # 8-byte Reload
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp3596:
	.size	gen_split_945, .Ltmp3596-gen_split_945
	.cfi_endproc

	.globl	gen_floatsisf2
	.align	16, 0x90
	.type	gen_floatsisf2,@function
gen_floatsisf2:                         # @gen_floatsisf2
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp3599:
	.cfi_def_cfa_offset 16
.Ltmp3600:
	.cfi_offset %rbx, -16
	movq	%rsi, %rax
	movq	%rdi, %rbx
	movq	$125, %rdi
	movq	$15, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	popq	%rbx
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp3601:
	.size	gen_floatsisf2, .Ltmp3601-gen_floatsisf2
	.cfi_endproc

	.globl	gen_floatdisf2
	.align	16, 0x90
	.type	gen_floatdisf2,@function
gen_floatdisf2:                         # @gen_floatdisf2
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp3604:
	.cfi_def_cfa_offset 16
.Ltmp3605:
	.cfi_offset %rbx, -16
	movq	%rsi, %rax
	movq	%rdi, %rbx
	movq	$125, %rdi
	movq	$15, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	popq	%rbx
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp3606:
	.size	gen_floatdisf2, .Ltmp3606-gen_floatdisf2
	.cfi_endproc

	.globl	gen_floatsidf2
	.align	16, 0x90
	.type	gen_floatsidf2,@function
gen_floatsidf2:                         # @gen_floatsidf2
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp3609:
	.cfi_def_cfa_offset 16
.Ltmp3610:
	.cfi_offset %rbx, -16
	movq	%rsi, %rax
	movq	%rdi, %rbx
	movq	$125, %rdi
	movq	$16, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	popq	%rbx
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp3611:
	.size	gen_floatsidf2, .Ltmp3611-gen_floatsidf2
	.cfi_endproc

	.globl	gen_floatdidf2
	.align	16, 0x90
	.type	gen_floatdidf2,@function
gen_floatdidf2:                         # @gen_floatdidf2
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp3614:
	.cfi_def_cfa_offset 16
.Ltmp3615:
	.cfi_offset %rbx, -16
	movq	%rsi, %rax
	movq	%rdi, %rbx
	movq	$125, %rdi
	movq	$16, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	popq	%rbx
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp3616:
	.size	gen_floatdidf2, .Ltmp3616-gen_floatdidf2
	.cfi_endproc

	.globl	gen_split_950
	.align	16, 0x90
	.type	gen_split_950,@function
gen_split_950:                          # @gen_split_950
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp3619:
	.cfi_def_cfa_offset 16
.Ltmp3620:
	.cfi_offset %rbx, -16
	movq	%rdi, %rbx
	callq	start_sequence
	movq	8(%rbx), %rsi
	movzbl	2(%rsi), %edi
	callq	ix86_force_to_memory
	movq	%rax, 16(%rbx)
	movq	(%rbx), %rcx
	movzbl	2(%rcx), %esi
	movq	$125, %rdi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, 16(%rbx)
	movq	(%rbx), %rdx
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	movq	8(%rbx), %rax
	movzbl	2(%rax), %edi
	callq	ix86_free_from_memory
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	popq	%rbx
	retq
.Ltmp3621:
	.size	gen_split_950, .Ltmp3621-gen_split_950
	.cfi_endproc

	.globl	gen_adddi3
	.align	16, 0x90
	.type	gen_adddi3,@function
gen_adddi3:                             # @gen_adddi3
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp3626:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp3627:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp3628:
	.cfi_def_cfa_offset 32
	subq	$32, %rsp
.Ltmp3629:
	.cfi_def_cfa_offset 64
.Ltmp3630:
	.cfi_offset %rbx, -32
.Ltmp3631:
	.cfi_offset %r14, -24
.Ltmp3632:
	.cfi_offset %r15, -16
	movq	%rdx, %r14
	movq	%rsi, %r15
	movq	%rdi, %rbx
	callq	start_sequence
	movq	%rbx, (%rsp)
	movq	%r15, 8(%rsp)
	movq	%r14, 16(%rsp)
	leaq	(%rsp), %rdx
	movq	$75, %rdi
	movq	$5, %rsi
	callq	ix86_expand_binary_operator
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	addq	$32, %rsp
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp3633:
	.size	gen_adddi3, .Ltmp3633-gen_adddi3
	.cfi_endproc

	.globl	gen_split_952
	.align	16, 0x90
	.type	gen_split_952,@function
gen_split_952:                          # @gen_split_952
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp3640:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp3641:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp3642:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp3643:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp3644:
	.cfi_def_cfa_offset 48
	subq	$32, %rsp
.Ltmp3645:
	.cfi_def_cfa_offset 80
.Ltmp3646:
	.cfi_offset %rbx, -48
.Ltmp3647:
	.cfi_offset %r12, -40
.Ltmp3648:
	.cfi_offset %r13, -32
.Ltmp3649:
	.cfi_offset %r14, -24
.Ltmp3650:
	.cfi_offset %r15, -16
	movq	%rdi, %rbx
	callq	start_sequence
	leaq	24(%rbx), %rcx
	movq	$1, %rsi
	movq	%rbx, %rdi
	movq	%rbx, %rdx
	callq	split_di
	leaq	8(%rbx), %rdi
	leaq	32(%rbx), %rcx
	movq	$1, %rsi
	movq	%rdi, %rdx
	callq	split_di
	leaq	16(%rbx), %rdi
	leaq	40(%rbx), %rcx
	movq	$1, %rsi
	movq	%rdi, %rdx
	callq	split_di
	movq	(%rbx), %r13
	movq	8(%rbx), %r14
	movq	16(%rbx), %r15
	movq	24(%rbx), %rax
	movq	%rax, 24(%rsp)          # 8-byte Spill
	movq	32(%rbx), %rax
	movq	%rax, 8(%rsp)           # 8-byte Spill
	movq	40(%rbx), %rax
	movq	%rax, 16(%rsp)          # 8-byte Spill
	movq	$52, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	%rax, %rbx
	movq	$2, %rdi
	xorq	%rax, %rax
	movq	%r14, %rsi
	movq	%r15, %rdx
	callq	gen_rtvec
	movq	$42, %rdi
	movq	$52, %rsi
	movq	$12, %rcx
	movq	%rax, %rdx
	callq	gen_rtx_fmt_Ei
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %r12
	movq	%r14, %rdi
	callq	copy_rtx
	movq	%rax, %rbx
	movq	%r15, %rdi
	callq	copy_rtx
	movq	$75, %rdi
	movq	$4, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r13, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rcx
	movq	$2, %rdi
	xorq	%rax, %rax
	movq	%r12, %rsi
	movq	%rcx, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_E
	movq	%rax, %rdi
	callq	emit
	movq	$52, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	const_int_rtx+512(%rip), %rcx
	movq	$111, %rdi
	movq	$4, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_ee
	movq	$75, %rdi
	movq	$4, %rsi
	movq	%rax, %rdx
	movq	8(%rsp), %rcx           # 8-byte Reload
	callq	gen_rtx_fmt_ee
	movq	$75, %rdi
	movq	$4, %rsi
	movq	%rax, %rdx
	movq	16(%rsp), %rcx          # 8-byte Reload
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	24(%rsp), %rdx          # 8-byte Reload
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rbx
	movq	$52, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rcx
	movq	$2, %rdi
	xorq	%rax, %rax
	movq	%rbx, %rsi
	movq	%rcx, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_E
	movq	%rax, %rdi
	callq	emit
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp3651:
	.size	gen_split_952, .Ltmp3651-gen_split_952
	.cfi_endproc

	.globl	gen_addsi3
	.align	16, 0x90
	.type	gen_addsi3,@function
gen_addsi3:                             # @gen_addsi3
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp3656:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp3657:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp3658:
	.cfi_def_cfa_offset 32
	subq	$32, %rsp
.Ltmp3659:
	.cfi_def_cfa_offset 64
.Ltmp3660:
	.cfi_offset %rbx, -32
.Ltmp3661:
	.cfi_offset %r14, -24
.Ltmp3662:
	.cfi_offset %r15, -16
	movq	%rdx, %r14
	movq	%rsi, %r15
	movq	%rdi, %rbx
	callq	start_sequence
	movq	%rbx, (%rsp)
	movq	%r15, 8(%rsp)
	movq	%r14, 16(%rsp)
	leaq	(%rsp), %rdx
	movq	$75, %rdi
	movq	$4, %rsi
	callq	ix86_expand_binary_operator
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	addq	$32, %rsp
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp3663:
	.size	gen_addsi3, .Ltmp3663-gen_addsi3
	.cfi_endproc

	.globl	gen_split_954
	.align	16, 0x90
	.type	gen_split_954,@function
gen_split_954:                          # @gen_split_954
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp3667:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp3668:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp3669:
	.cfi_def_cfa_offset 32
.Ltmp3670:
	.cfi_offset %rbx, -24
.Ltmp3671:
	.cfi_offset %r14, -16
	movq	%rdi, %r14
	callq	start_sequence
	movq	(%r14), %rsi
	movq	$4, %rdi
	callq	gen_lowpart
	movq	%rax, (%r14)
	movl	target_flags(%rip), %edi
	movq	$25, %rcx
	movq	8(%r14), %rsi
	shrq	%cl, %rdi
	andq	$1, %rdi
	orq	$4, %rdi
	callq	gen_lowpart
	movq	%rax, 8(%r14)
	movl	target_flags(%rip), %edi
	movq	$25, %rcx
	movq	16(%r14), %rsi
	shrq	%cl, %rdi
	andq	$1, %rdi
	orq	$4, %rdi
	callq	gen_lowpart
	movq	%rax, 16(%r14)
	movl	target_flags(%rip), %edi
	movq	$25, %rcx
	movq	24(%r14), %rsi
	shrq	%cl, %rdi
	andq	$1, %rdi
	orq	$4, %rdi
	callq	gen_lowpart
	movq	%rax, 24(%r14)
	movl	target_flags(%rip), %ebx
	movq	$25, %rcx
	movq	8(%r14), %rdx
	movq	16(%r14), %rax
	shrq	%cl, %rbx
	andq	$1, %rbx
	orq	$4, %rbx
	movq	$75, %rdi
	movq	%rbx, %rsi
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	24(%r14), %rcx
	movq	$75, %rdi
	movq	%rbx, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_ee
	movzbl	target_flags+3(%rip), %ecx
	testq	$2, %rcx
	je	.LBB410_2
# BB#1:                                 # %if.then
	movq	$4, %rdi
	xorq	%rdx, %rdx
	movq	%rax, %rsi
	callq	gen_rtx_SUBREG
.LBB410_2:                              # %if.end
	movq	(%r14), %rdx
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp3672:
	.size	gen_split_954, .Ltmp3672-gen_split_954
	.cfi_endproc

	.globl	gen_split_955
	.align	16, 0x90
	.type	gen_split_955,@function
gen_split_955:                          # @gen_split_955
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp3677:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp3678:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp3679:
	.cfi_def_cfa_offset 32
.Ltmp3680:
	.cfi_offset %rbx, -32
.Ltmp3681:
	.cfi_offset %r14, -24
.Ltmp3682:
	.cfi_offset %r15, -16
	movq	%rdi, %rbx
	callq	start_sequence
	movl	target_flags(%rip), %edi
	movq	$25, %rcx
	movq	8(%rbx), %rsi
	shrq	%cl, %rdi
	andq	$1, %rdi
	orq	$4, %rdi
	callq	gen_lowpart
	movq	%rax, 8(%rbx)
	movl	target_flags(%rip), %edi
	movq	$25, %rcx
	movq	16(%rbx), %rsi
	shrq	%cl, %rdi
	andq	$1, %rdi
	orq	$4, %rdi
	callq	gen_lowpart
	movq	%rax, 16(%rbx)
	movl	target_flags(%rip), %edi
	movq	$25, %rcx
	movq	24(%rbx), %rsi
	shrq	%cl, %rdi
	andq	$1, %rdi
	orq	$4, %rdi
	callq	gen_lowpart
	movq	%rax, %r15
	movq	%r15, 24(%rbx)
	movq	(%rbx), %r14
	movq	8(%rbx), %rdx
	movq	16(%rbx), %rcx
	movq	$75, %rdi
	movq	$5, %rsi
	callq	gen_rtx_fmt_ee
	movq	$75, %rdi
	movq	$5, %rsi
	movq	%rax, %rdx
	movq	%r15, %rcx
	callq	gen_rtx_fmt_ee
	movq	$4, %rdi
	xorq	%rdx, %rdx
	movq	%rax, %rsi
	callq	gen_rtx_SUBREG
	movq	$121, %rdi
	movq	$5, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp3683:
	.size	gen_split_955, .Ltmp3683-gen_split_955
	.cfi_endproc

	.globl	gen_split_956
	.align	16, 0x90
	.type	gen_split_956,@function
gen_split_956:                          # @gen_split_956
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp3687:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp3688:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp3689:
	.cfi_def_cfa_offset 32
.Ltmp3690:
	.cfi_offset %rbx, -24
.Ltmp3691:
	.cfi_offset %r14, -16
	movq	%rdi, %r14
	callq	start_sequence
	movq	(%r14), %rsi
	movq	$4, %rdi
	callq	gen_lowpart
	movq	%rax, (%r14)
	movl	target_flags(%rip), %edi
	movq	$25, %rcx
	movq	8(%r14), %rsi
	shrq	%cl, %rdi
	andq	$1, %rdi
	orq	$4, %rdi
	callq	gen_lowpart
	movq	%rax, 8(%r14)
	movl	target_flags(%rip), %edi
	movq	$25, %rcx
	movq	24(%r14), %rsi
	shrq	%cl, %rdi
	andq	$1, %rdi
	orq	$4, %rdi
	callq	gen_lowpart
	movq	%rax, 24(%r14)
	movl	target_flags(%rip), %ebx
	movq	$25, %rcx
	movq	8(%r14), %rdx
	movq	16(%r14), %rax
	shrq	%cl, %rbx
	andq	$1, %rbx
	orq	$4, %rbx
	movq	$78, %rdi
	movq	%rbx, %rsi
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	24(%r14), %rcx
	movq	$75, %rdi
	movq	%rbx, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_ee
	movzbl	target_flags+3(%rip), %ecx
	testq	$2, %rcx
	je	.LBB412_2
# BB#1:                                 # %if.then
	movq	$4, %rdi
	xorq	%rdx, %rdx
	movq	%rax, %rsi
	callq	gen_rtx_SUBREG
.LBB412_2:                              # %if.end
	movq	(%r14), %rdx
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp3692:
	.size	gen_split_956, .Ltmp3692-gen_split_956
	.cfi_endproc

	.globl	gen_split_957
	.align	16, 0x90
	.type	gen_split_957,@function
gen_split_957:                          # @gen_split_957
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp3697:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp3698:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp3699:
	.cfi_def_cfa_offset 32
.Ltmp3700:
	.cfi_offset %rbx, -32
.Ltmp3701:
	.cfi_offset %r14, -24
.Ltmp3702:
	.cfi_offset %r15, -16
	movq	%rdi, %rbx
	callq	start_sequence
	movl	target_flags(%rip), %edi
	movq	$25, %rcx
	movq	8(%rbx), %rsi
	shrq	%cl, %rdi
	andq	$1, %rdi
	orq	$4, %rdi
	callq	gen_lowpart
	movq	%rax, 8(%rbx)
	movl	target_flags(%rip), %edi
	movq	$25, %rcx
	movq	24(%rbx), %rsi
	shrq	%cl, %rdi
	andq	$1, %rdi
	orq	$4, %rdi
	callq	gen_lowpart
	movq	%rax, %r15
	movq	%r15, 24(%rbx)
	movq	(%rbx), %r14
	movq	8(%rbx), %rdx
	movq	16(%rbx), %rcx
	movq	$78, %rdi
	movq	$5, %rsi
	callq	gen_rtx_fmt_ee
	movq	$75, %rdi
	movq	$5, %rsi
	movq	%rax, %rdx
	movq	%r15, %rcx
	callq	gen_rtx_fmt_ee
	movq	$4, %rdi
	xorq	%rdx, %rdx
	movq	%rax, %rsi
	callq	gen_rtx_SUBREG
	movq	$121, %rdi
	movq	$5, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp3703:
	.size	gen_split_957, .Ltmp3703-gen_split_957
	.cfi_endproc

	.globl	gen_split_958
	.align	16, 0x90
	.type	gen_split_958,@function
gen_split_958:                          # @gen_split_958
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp3707:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp3708:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp3709:
	.cfi_def_cfa_offset 32
.Ltmp3710:
	.cfi_offset %rbx, -24
.Ltmp3711:
	.cfi_offset %r14, -16
	movq	%rdi, %r14
	callq	start_sequence
	movq	(%r14), %rsi
	movq	$4, %rdi
	callq	gen_lowpart
	movq	%rax, (%r14)
	movl	target_flags(%rip), %edi
	movq	$25, %rcx
	movq	8(%r14), %rsi
	shrq	%cl, %rdi
	andq	$1, %rdi
	orq	$4, %rdi
	callq	gen_lowpart
	movq	%rax, 8(%r14)
	movl	target_flags(%rip), %edi
	movq	$25, %rcx
	movq	24(%r14), %rsi
	shrq	%cl, %rdi
	andq	$1, %rdi
	orq	$4, %rdi
	callq	gen_lowpart
	movq	%rax, 24(%r14)
	movl	target_flags(%rip), %edi
	movq	$25, %rcx
	movq	32(%r14), %rsi
	shrq	%cl, %rdi
	andq	$1, %rdi
	orq	$4, %rdi
	callq	gen_lowpart
	movq	%rax, 32(%r14)
	movl	target_flags(%rip), %ebx
	movq	$25, %rcx
	movq	8(%r14), %rdx
	movq	16(%r14), %rax
	shrq	%cl, %rbx
	andq	$1, %rbx
	orq	$4, %rbx
	movq	$78, %rdi
	movq	%rbx, %rsi
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	24(%r14), %rcx
	movq	$75, %rdi
	movq	%rbx, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_ee
	movq	32(%r14), %rcx
	movq	$75, %rdi
	movq	%rbx, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_ee
	movzbl	target_flags+3(%rip), %ecx
	testq	$2, %rcx
	je	.LBB414_2
# BB#1:                                 # %if.then
	movq	$4, %rdi
	xorq	%rdx, %rdx
	movq	%rax, %rsi
	callq	gen_rtx_SUBREG
.LBB414_2:                              # %if.end
	movq	(%r14), %rdx
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp3712:
	.size	gen_split_958, .Ltmp3712-gen_split_958
	.cfi_endproc

	.globl	gen_split_959
	.align	16, 0x90
	.type	gen_split_959,@function
gen_split_959:                          # @gen_split_959
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp3717:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp3718:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp3719:
	.cfi_def_cfa_offset 32
.Ltmp3720:
	.cfi_offset %rbx, -32
.Ltmp3721:
	.cfi_offset %r14, -24
.Ltmp3722:
	.cfi_offset %r15, -16
	movq	%rdi, %rbx
	callq	start_sequence
	movl	target_flags(%rip), %edi
	movq	$25, %rcx
	movq	8(%rbx), %rsi
	shrq	%cl, %rdi
	andq	$1, %rdi
	orq	$4, %rdi
	callq	gen_lowpart
	movq	%rax, 8(%rbx)
	movl	target_flags(%rip), %edi
	movq	$25, %rcx
	movq	24(%rbx), %rsi
	shrq	%cl, %rdi
	andq	$1, %rdi
	orq	$4, %rdi
	callq	gen_lowpart
	movq	%rax, 24(%rbx)
	movl	target_flags(%rip), %edi
	movq	$25, %rcx
	movq	32(%rbx), %rsi
	shrq	%cl, %rdi
	andq	$1, %rdi
	orq	$4, %rdi
	callq	gen_lowpart
	movq	%rax, %r15
	movq	%r15, 32(%rbx)
	movq	(%rbx), %r14
	movq	8(%rbx), %rdx
	movq	16(%rbx), %rcx
	movq	24(%rbx), %rbx
	movq	$78, %rdi
	movq	$5, %rsi
	callq	gen_rtx_fmt_ee
	movq	$75, %rdi
	movq	$5, %rsi
	movq	%rax, %rdx
	movq	%rbx, %rcx
	callq	gen_rtx_fmt_ee
	movq	$75, %rdi
	movq	$5, %rsi
	movq	%rax, %rdx
	movq	%r15, %rcx
	callq	gen_rtx_fmt_ee
	movq	$4, %rdi
	xorq	%rdx, %rdx
	movq	%rax, %rsi
	callq	gen_rtx_SUBREG
	movq	$121, %rdi
	movq	$5, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp3723:
	.size	gen_split_959, .Ltmp3723-gen_split_959
	.cfi_endproc

	.globl	gen_split_960
	.align	16, 0x90
	.type	gen_split_960,@function
gen_split_960:                          # @gen_split_960
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp3727:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp3728:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp3729:
	.cfi_def_cfa_offset 32
.Ltmp3730:
	.cfi_offset %rbx, -24
.Ltmp3731:
	.cfi_offset %r14, -16
	movq	%rdi, %rbx
	callq	start_sequence
	movq	(%rbx), %r14
	movq	8(%rbx), %rdx
	movq	16(%rbx), %rcx
	movq	$75, %rdi
	movq	$5, %rsi
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp3732:
	.size	gen_split_960, .Ltmp3732-gen_split_960
	.cfi_endproc

	.globl	gen_split_961
	.align	16, 0x90
	.type	gen_split_961,@function
gen_split_961:                          # @gen_split_961
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp3735:
	.cfi_def_cfa_offset 16
.Ltmp3736:
	.cfi_offset %rbx, -16
	movq	%rdi, %rbx
	callq	start_sequence
	movq	(%rbx), %rsi
	movl	target_flags(%rip), %edi
	movzbl	2(%rsi), %eax
	movq	$25, %rcx
	shrq	%cl, %rdi
	andq	$1, %rdi
	orq	$4, %rdi
	cmpq	%rdi, %rax
	je	.LBB417_2
# BB#1:                                 # %if.then
	movq	8(%rbx), %rsi
	callq	gen_lowpart
	movq	%rax, 8(%rbx)
	movl	target_flags(%rip), %edi
	movq	$25, %rcx
	movq	16(%rbx), %rsi
	shrq	%cl, %rdi
	andq	$1, %rdi
	orq	$4, %rdi
	callq	gen_lowpart
	movq	%rax, 16(%rbx)
	movq	(%rbx), %rsi
.LBB417_2:                              # %if.end
	movq	$4, %rdi
	callq	gen_lowpart
	movq	%rax, (%rbx)
	movl	target_flags(%rip), %esi
	movq	$25, %rcx
	movq	8(%rbx), %rdx
	movq	16(%rbx), %rax
	shrq	%cl, %rsi
	andq	$1, %rsi
	orq	$4, %rsi
	movq	$75, %rdi
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movzbl	target_flags+3(%rip), %ecx
	testq	$2, %rcx
	je	.LBB417_4
# BB#3:                                 # %if.then25
	movq	$4, %rdi
	xorq	%rdx, %rdx
	movq	%rax, %rsi
	callq	gen_rtx_SUBREG
.LBB417_4:                              # %if.end27
	movq	(%rbx), %rdx
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	popq	%rbx
	retq
.Ltmp3737:
	.size	gen_split_961, .Ltmp3737-gen_split_961
	.cfi_endproc

	.globl	gen_split_962
	.align	16, 0x90
	.type	gen_split_962,@function
gen_split_962:                          # @gen_split_962
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp3741:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp3742:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp3743:
	.cfi_def_cfa_offset 32
.Ltmp3744:
	.cfi_offset %rbx, -24
.Ltmp3745:
	.cfi_offset %r14, -16
	movq	%rdi, %rbx
	callq	start_sequence
	movl	target_flags(%rip), %edi
	movq	$25, %rcx
	movq	8(%rbx), %rsi
	shrq	%cl, %rdi
	andq	$1, %rdi
	orq	$4, %rdi
	callq	gen_lowpart
	movq	%rax, 8(%rbx)
	movl	target_flags(%rip), %edi
	movq	$25, %rcx
	movq	16(%rbx), %rsi
	shrq	%cl, %rdi
	andq	$1, %rdi
	orq	$4, %rdi
	callq	gen_lowpart
	movq	%rax, 16(%rbx)
	movq	(%rbx), %r14
	movq	8(%rbx), %rdx
	movq	$75, %rdi
	movq	$5, %rsi
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$4, %rdi
	xorq	%rdx, %rdx
	movq	%rax, %rsi
	callq	gen_rtx_SUBREG
	movq	$121, %rdi
	movq	$5, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp3746:
	.size	gen_split_962, .Ltmp3746-gen_split_962
	.cfi_endproc

	.globl	gen_addhi3
	.align	16, 0x90
	.type	gen_addhi3,@function
gen_addhi3:                             # @gen_addhi3
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp3751:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp3752:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp3753:
	.cfi_def_cfa_offset 32
	subq	$32, %rsp
.Ltmp3754:
	.cfi_def_cfa_offset 64
.Ltmp3755:
	.cfi_offset %rbx, -32
.Ltmp3756:
	.cfi_offset %r14, -24
.Ltmp3757:
	.cfi_offset %r15, -16
	movq	%rdx, %r14
	movq	%rsi, %r15
	movq	%rdi, %rbx
	callq	start_sequence
	movq	%rbx, (%rsp)
	movq	%r15, 8(%rsp)
	movq	%r14, 16(%rsp)
	leaq	(%rsp), %rdx
	movq	$75, %rdi
	movq	$3, %rsi
	callq	ix86_expand_binary_operator
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	addq	$32, %rsp
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp3758:
	.size	gen_addhi3, .Ltmp3758-gen_addhi3
	.cfi_endproc

	.globl	gen_addqi3
	.align	16, 0x90
	.type	gen_addqi3,@function
gen_addqi3:                             # @gen_addqi3
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp3763:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp3764:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp3765:
	.cfi_def_cfa_offset 32
	subq	$32, %rsp
.Ltmp3766:
	.cfi_def_cfa_offset 64
.Ltmp3767:
	.cfi_offset %rbx, -32
.Ltmp3768:
	.cfi_offset %r14, -24
.Ltmp3769:
	.cfi_offset %r15, -16
	movq	%rdx, %r14
	movq	%rsi, %r15
	movq	%rdi, %rbx
	callq	start_sequence
	movq	%rbx, (%rsp)
	movq	%r15, 8(%rsp)
	movq	%r14, 16(%rsp)
	leaq	(%rsp), %rdx
	movq	$75, %rdi
	movq	$2, %rsi
	callq	ix86_expand_binary_operator
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	addq	$32, %rsp
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp3770:
	.size	gen_addqi3, .Ltmp3770-gen_addqi3
	.cfi_endproc

	.globl	gen_addxf3
	.align	16, 0x90
	.type	gen_addxf3,@function
gen_addxf3:                             # @gen_addxf3
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp3773:
	.cfi_def_cfa_offset 16
.Ltmp3774:
	.cfi_offset %rbx, -16
	movq	%rdx, %rax
	movq	%rsi, %rcx
	movq	%rdi, %rbx
	movq	$75, %rdi
	movq	$17, %rsi
	movq	%rcx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	popq	%rbx
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp3775:
	.size	gen_addxf3, .Ltmp3775-gen_addxf3
	.cfi_endproc

	.globl	gen_addtf3
	.align	16, 0x90
	.type	gen_addtf3,@function
gen_addtf3:                             # @gen_addtf3
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp3778:
	.cfi_def_cfa_offset 16
.Ltmp3779:
	.cfi_offset %rbx, -16
	movq	%rdx, %rax
	movq	%rsi, %rcx
	movq	%rdi, %rbx
	movq	$75, %rdi
	movq	$18, %rsi
	movq	%rcx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	popq	%rbx
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp3780:
	.size	gen_addtf3, .Ltmp3780-gen_addtf3
	.cfi_endproc

	.globl	gen_adddf3
	.align	16, 0x90
	.type	gen_adddf3,@function
gen_adddf3:                             # @gen_adddf3
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp3783:
	.cfi_def_cfa_offset 16
.Ltmp3784:
	.cfi_offset %rbx, -16
	movq	%rdx, %rax
	movq	%rsi, %rcx
	movq	%rdi, %rbx
	movq	$75, %rdi
	movq	$16, %rsi
	movq	%rcx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	popq	%rbx
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp3785:
	.size	gen_adddf3, .Ltmp3785-gen_adddf3
	.cfi_endproc

	.globl	gen_addsf3
	.align	16, 0x90
	.type	gen_addsf3,@function
gen_addsf3:                             # @gen_addsf3
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp3788:
	.cfi_def_cfa_offset 16
.Ltmp3789:
	.cfi_offset %rbx, -16
	movq	%rdx, %rax
	movq	%rsi, %rcx
	movq	%rdi, %rbx
	movq	$75, %rdi
	movq	$15, %rsi
	movq	%rcx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	popq	%rbx
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp3790:
	.size	gen_addsf3, .Ltmp3790-gen_addsf3
	.cfi_endproc

	.globl	gen_subdi3
	.align	16, 0x90
	.type	gen_subdi3,@function
gen_subdi3:                             # @gen_subdi3
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp3795:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp3796:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp3797:
	.cfi_def_cfa_offset 32
	subq	$32, %rsp
.Ltmp3798:
	.cfi_def_cfa_offset 64
.Ltmp3799:
	.cfi_offset %rbx, -32
.Ltmp3800:
	.cfi_offset %r14, -24
.Ltmp3801:
	.cfi_offset %r15, -16
	movq	%rdx, %r14
	movq	%rsi, %r15
	movq	%rdi, %rbx
	callq	start_sequence
	movq	%rbx, (%rsp)
	movq	%r15, 8(%rsp)
	movq	%r14, 16(%rsp)
	leaq	(%rsp), %rdx
	movq	$76, %rdi
	movq	$5, %rsi
	callq	ix86_expand_binary_operator
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	addq	$32, %rsp
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp3802:
	.size	gen_subdi3, .Ltmp3802-gen_subdi3
	.cfi_endproc

	.globl	gen_split_970
	.align	16, 0x90
	.type	gen_split_970,@function
gen_split_970:                          # @gen_split_970
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp3809:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp3810:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp3811:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp3812:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp3813:
	.cfi_def_cfa_offset 48
	subq	$32, %rsp
.Ltmp3814:
	.cfi_def_cfa_offset 80
.Ltmp3815:
	.cfi_offset %rbx, -48
.Ltmp3816:
	.cfi_offset %r12, -40
.Ltmp3817:
	.cfi_offset %r13, -32
.Ltmp3818:
	.cfi_offset %r14, -24
.Ltmp3819:
	.cfi_offset %r15, -16
	movq	%rdi, %rbx
	callq	start_sequence
	leaq	24(%rbx), %rcx
	movq	$1, %rsi
	movq	%rbx, %rdi
	movq	%rbx, %rdx
	callq	split_di
	leaq	8(%rbx), %rdi
	leaq	32(%rbx), %rcx
	movq	$1, %rsi
	movq	%rdi, %rdx
	callq	split_di
	leaq	16(%rbx), %rdi
	leaq	40(%rbx), %rcx
	movq	$1, %rsi
	movq	%rdi, %rdx
	callq	split_di
	movq	(%rbx), %r13
	movq	8(%rbx), %r15
	movq	16(%rbx), %r14
	movq	24(%rbx), %rax
	movq	%rax, 24(%rsp)          # 8-byte Spill
	movq	32(%rbx), %rax
	movq	%rax, 16(%rsp)          # 8-byte Spill
	movq	40(%rbx), %rax
	movq	%rax, 8(%rsp)           # 8-byte Spill
	movq	$52, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	%rax, %rbx
	movq	$74, %rdi
	movq	$52, %rsi
	movq	%r15, %rdx
	movq	%r14, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %r12
	movq	%r15, %rdi
	callq	copy_rtx
	movq	%rax, %rbx
	movq	%r14, %rdi
	callq	copy_rtx
	movq	$76, %rdi
	movq	$4, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r13, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rcx
	movq	$2, %rdi
	xorq	%rax, %rax
	movq	%r12, %rsi
	movq	%rcx, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_E
	movq	%rax, %rdi
	callq	emit
	movq	$52, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	const_int_rtx+512(%rip), %rcx
	movq	$111, %rdi
	movq	$4, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_ee
	movq	$75, %rdi
	movq	$4, %rsi
	movq	%rax, %rdx
	movq	8(%rsp), %rcx           # 8-byte Reload
	callq	gen_rtx_fmt_ee
	movq	$76, %rdi
	movq	$4, %rsi
	movq	16(%rsp), %rdx          # 8-byte Reload
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	24(%rsp), %rdx          # 8-byte Reload
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rbx
	movq	$52, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rcx
	movq	$2, %rdi
	xorq	%rax, %rax
	movq	%rbx, %rsi
	movq	%rcx, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_E
	movq	%rax, %rdi
	callq	emit
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp3820:
	.size	gen_split_970, .Ltmp3820-gen_split_970
	.cfi_endproc

	.globl	gen_subsi3
	.align	16, 0x90
	.type	gen_subsi3,@function
gen_subsi3:                             # @gen_subsi3
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp3825:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp3826:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp3827:
	.cfi_def_cfa_offset 32
	subq	$32, %rsp
.Ltmp3828:
	.cfi_def_cfa_offset 64
.Ltmp3829:
	.cfi_offset %rbx, -32
.Ltmp3830:
	.cfi_offset %r14, -24
.Ltmp3831:
	.cfi_offset %r15, -16
	movq	%rdx, %r14
	movq	%rsi, %r15
	movq	%rdi, %rbx
	callq	start_sequence
	movq	%rbx, (%rsp)
	movq	%r15, 8(%rsp)
	movq	%r14, 16(%rsp)
	leaq	(%rsp), %rdx
	movq	$76, %rdi
	movq	$4, %rsi
	callq	ix86_expand_binary_operator
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	addq	$32, %rsp
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp3832:
	.size	gen_subsi3, .Ltmp3832-gen_subsi3
	.cfi_endproc

	.globl	gen_subhi3
	.align	16, 0x90
	.type	gen_subhi3,@function
gen_subhi3:                             # @gen_subhi3
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp3837:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp3838:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp3839:
	.cfi_def_cfa_offset 32
	subq	$32, %rsp
.Ltmp3840:
	.cfi_def_cfa_offset 64
.Ltmp3841:
	.cfi_offset %rbx, -32
.Ltmp3842:
	.cfi_offset %r14, -24
.Ltmp3843:
	.cfi_offset %r15, -16
	movq	%rdx, %r14
	movq	%rsi, %r15
	movq	%rdi, %rbx
	callq	start_sequence
	movq	%rbx, (%rsp)
	movq	%r15, 8(%rsp)
	movq	%r14, 16(%rsp)
	leaq	(%rsp), %rdx
	movq	$76, %rdi
	movq	$3, %rsi
	callq	ix86_expand_binary_operator
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	addq	$32, %rsp
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp3844:
	.size	gen_subhi3, .Ltmp3844-gen_subhi3
	.cfi_endproc

	.globl	gen_subqi3
	.align	16, 0x90
	.type	gen_subqi3,@function
gen_subqi3:                             # @gen_subqi3
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp3849:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp3850:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp3851:
	.cfi_def_cfa_offset 32
	subq	$32, %rsp
.Ltmp3852:
	.cfi_def_cfa_offset 64
.Ltmp3853:
	.cfi_offset %rbx, -32
.Ltmp3854:
	.cfi_offset %r14, -24
.Ltmp3855:
	.cfi_offset %r15, -16
	movq	%rdx, %r14
	movq	%rsi, %r15
	movq	%rdi, %rbx
	callq	start_sequence
	movq	%rbx, (%rsp)
	movq	%r15, 8(%rsp)
	movq	%r14, 16(%rsp)
	leaq	(%rsp), %rdx
	movq	$76, %rdi
	movq	$2, %rsi
	callq	ix86_expand_binary_operator
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	addq	$32, %rsp
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp3856:
	.size	gen_subqi3, .Ltmp3856-gen_subqi3
	.cfi_endproc

	.globl	gen_subxf3
	.align	16, 0x90
	.type	gen_subxf3,@function
gen_subxf3:                             # @gen_subxf3
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp3859:
	.cfi_def_cfa_offset 16
.Ltmp3860:
	.cfi_offset %rbx, -16
	movq	%rdx, %rax
	movq	%rsi, %rcx
	movq	%rdi, %rbx
	movq	$76, %rdi
	movq	$17, %rsi
	movq	%rcx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	popq	%rbx
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp3861:
	.size	gen_subxf3, .Ltmp3861-gen_subxf3
	.cfi_endproc

	.globl	gen_subtf3
	.align	16, 0x90
	.type	gen_subtf3,@function
gen_subtf3:                             # @gen_subtf3
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp3864:
	.cfi_def_cfa_offset 16
.Ltmp3865:
	.cfi_offset %rbx, -16
	movq	%rdx, %rax
	movq	%rsi, %rcx
	movq	%rdi, %rbx
	movq	$76, %rdi
	movq	$18, %rsi
	movq	%rcx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	popq	%rbx
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp3866:
	.size	gen_subtf3, .Ltmp3866-gen_subtf3
	.cfi_endproc

	.globl	gen_subdf3
	.align	16, 0x90
	.type	gen_subdf3,@function
gen_subdf3:                             # @gen_subdf3
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp3869:
	.cfi_def_cfa_offset 16
.Ltmp3870:
	.cfi_offset %rbx, -16
	movq	%rdx, %rax
	movq	%rsi, %rcx
	movq	%rdi, %rbx
	movq	$76, %rdi
	movq	$16, %rsi
	movq	%rcx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	popq	%rbx
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp3871:
	.size	gen_subdf3, .Ltmp3871-gen_subdf3
	.cfi_endproc

	.globl	gen_subsf3
	.align	16, 0x90
	.type	gen_subsf3,@function
gen_subsf3:                             # @gen_subsf3
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp3874:
	.cfi_def_cfa_offset 16
.Ltmp3875:
	.cfi_offset %rbx, -16
	movq	%rdx, %rax
	movq	%rsi, %rcx
	movq	%rdi, %rbx
	movq	$76, %rdi
	movq	$15, %rsi
	movq	%rcx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	popq	%rbx
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp3876:
	.size	gen_subsf3, .Ltmp3876-gen_subsf3
	.cfi_endproc

	.globl	gen_muldi3
	.align	16, 0x90
	.type	gen_muldi3,@function
gen_muldi3:                             # @gen_muldi3
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp3879:
	.cfi_def_cfa_offset 16
.Ltmp3880:
	.cfi_offset %rbx, -16
	movq	%rdx, %rax
	movq	%rsi, %rcx
	movq	%rdi, %rbx
	movq	$78, %rdi
	movq	$5, %rsi
	movq	%rcx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rbx
	movq	$52, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rcx
	movq	$2, %rdi
	xorq	%rax, %rax
	movq	%rbx, %rsi
	movq	%rcx, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	popq	%rbx
	jmp	gen_rtx_fmt_E  # TAILCALL
.Ltmp3881:
	.size	gen_muldi3, .Ltmp3881-gen_muldi3
	.cfi_endproc

	.globl	gen_mulsi3
	.align	16, 0x90
	.type	gen_mulsi3,@function
gen_mulsi3:                             # @gen_mulsi3
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp3884:
	.cfi_def_cfa_offset 16
.Ltmp3885:
	.cfi_offset %rbx, -16
	movq	%rdx, %rax
	movq	%rsi, %rcx
	movq	%rdi, %rbx
	movq	$78, %rdi
	movq	$4, %rsi
	movq	%rcx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rbx
	movq	$52, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rcx
	movq	$2, %rdi
	xorq	%rax, %rax
	movq	%rbx, %rsi
	movq	%rcx, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	popq	%rbx
	jmp	gen_rtx_fmt_E  # TAILCALL
.Ltmp3886:
	.size	gen_mulsi3, .Ltmp3886-gen_mulsi3
	.cfi_endproc

	.globl	gen_mulhi3
	.align	16, 0x90
	.type	gen_mulhi3,@function
gen_mulhi3:                             # @gen_mulhi3
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp3889:
	.cfi_def_cfa_offset 16
.Ltmp3890:
	.cfi_offset %rbx, -16
	movq	%rdx, %rax
	movq	%rsi, %rcx
	movq	%rdi, %rbx
	movq	$78, %rdi
	movq	$3, %rsi
	movq	%rcx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rbx
	movq	$52, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rcx
	movq	$2, %rdi
	xorq	%rax, %rax
	movq	%rbx, %rsi
	movq	%rcx, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	popq	%rbx
	jmp	gen_rtx_fmt_E  # TAILCALL
.Ltmp3891:
	.size	gen_mulhi3, .Ltmp3891-gen_mulhi3
	.cfi_endproc

	.globl	gen_mulqi3
	.align	16, 0x90
	.type	gen_mulqi3,@function
gen_mulqi3:                             # @gen_mulqi3
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp3894:
	.cfi_def_cfa_offset 16
.Ltmp3895:
	.cfi_offset %rbx, -16
	movq	%rdx, %rax
	movq	%rsi, %rcx
	movq	%rdi, %rbx
	movq	$78, %rdi
	movq	$2, %rsi
	movq	%rcx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rbx
	movq	$52, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rcx
	movq	$2, %rdi
	xorq	%rax, %rax
	movq	%rbx, %rsi
	movq	%rcx, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	popq	%rbx
	jmp	gen_rtx_fmt_E  # TAILCALL
.Ltmp3896:
	.size	gen_mulqi3, .Ltmp3896-gen_mulqi3
	.cfi_endproc

	.globl	gen_umulqihi3
	.align	16, 0x90
	.type	gen_umulqihi3,@function
gen_umulqihi3:                          # @gen_umulqihi3
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp3901:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp3902:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp3903:
	.cfi_def_cfa_offset 32
.Ltmp3904:
	.cfi_offset %rbx, -32
.Ltmp3905:
	.cfi_offset %r14, -24
.Ltmp3906:
	.cfi_offset %r15, -16
	movq	%rdx, %r15
	movq	%rsi, %rax
	movq	%rdi, %r14
	movq	$121, %rdi
	movq	$3, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rbx
	movq	$121, %rdi
	movq	$3, %rsi
	movq	%r15, %rdx
	callq	gen_rtx_fmt_e
	movq	$78, %rdi
	movq	$3, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rbx
	movq	$52, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rcx
	movq	$2, %rdi
	xorq	%rax, %rax
	movq	%rbx, %rsi
	movq	%rcx, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	popq	%rbx
	popq	%r14
	popq	%r15
	jmp	gen_rtx_fmt_E  # TAILCALL
.Ltmp3907:
	.size	gen_umulqihi3, .Ltmp3907-gen_umulqihi3
	.cfi_endproc

	.globl	gen_mulqihi3
	.align	16, 0x90
	.type	gen_mulqihi3,@function
gen_mulqihi3:                           # @gen_mulqihi3
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp3912:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp3913:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp3914:
	.cfi_def_cfa_offset 32
.Ltmp3915:
	.cfi_offset %rbx, -32
.Ltmp3916:
	.cfi_offset %r14, -24
.Ltmp3917:
	.cfi_offset %r15, -16
	movq	%rdx, %r15
	movq	%rsi, %rax
	movq	%rdi, %r14
	movq	$120, %rdi
	movq	$3, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rbx
	movq	$120, %rdi
	movq	$3, %rsi
	movq	%r15, %rdx
	callq	gen_rtx_fmt_e
	movq	$78, %rdi
	movq	$3, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rbx
	movq	$52, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rcx
	movq	$2, %rdi
	xorq	%rax, %rax
	movq	%rbx, %rsi
	movq	%rcx, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	popq	%rbx
	popq	%r14
	popq	%r15
	jmp	gen_rtx_fmt_E  # TAILCALL
.Ltmp3918:
	.size	gen_mulqihi3, .Ltmp3918-gen_mulqihi3
	.cfi_endproc

	.globl	gen_umulditi3
	.align	16, 0x90
	.type	gen_umulditi3,@function
gen_umulditi3:                          # @gen_umulditi3
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp3923:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp3924:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp3925:
	.cfi_def_cfa_offset 32
.Ltmp3926:
	.cfi_offset %rbx, -32
.Ltmp3927:
	.cfi_offset %r14, -24
.Ltmp3928:
	.cfi_offset %r15, -16
	movq	%rdx, %r15
	movq	%rsi, %rax
	movq	%rdi, %r14
	movq	$121, %rdi
	movq	$6, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rbx
	movq	$121, %rdi
	movq	$6, %rsi
	movq	%r15, %rdx
	callq	gen_rtx_fmt_e
	movq	$78, %rdi
	movq	$6, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rbx
	movq	$52, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rcx
	movq	$2, %rdi
	xorq	%rax, %rax
	movq	%rbx, %rsi
	movq	%rcx, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	popq	%rbx
	popq	%r14
	popq	%r15
	jmp	gen_rtx_fmt_E  # TAILCALL
.Ltmp3929:
	.size	gen_umulditi3, .Ltmp3929-gen_umulditi3
	.cfi_endproc

	.globl	gen_umulsidi3
	.align	16, 0x90
	.type	gen_umulsidi3,@function
gen_umulsidi3:                          # @gen_umulsidi3
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp3934:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp3935:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp3936:
	.cfi_def_cfa_offset 32
.Ltmp3937:
	.cfi_offset %rbx, -32
.Ltmp3938:
	.cfi_offset %r14, -24
.Ltmp3939:
	.cfi_offset %r15, -16
	movq	%rdx, %r15
	movq	%rsi, %rax
	movq	%rdi, %r14
	movq	$121, %rdi
	movq	$5, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rbx
	movq	$121, %rdi
	movq	$5, %rsi
	movq	%r15, %rdx
	callq	gen_rtx_fmt_e
	movq	$78, %rdi
	movq	$5, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rbx
	movq	$52, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rcx
	movq	$2, %rdi
	xorq	%rax, %rax
	movq	%rbx, %rsi
	movq	%rcx, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	popq	%rbx
	popq	%r14
	popq	%r15
	jmp	gen_rtx_fmt_E  # TAILCALL
.Ltmp3940:
	.size	gen_umulsidi3, .Ltmp3940-gen_umulsidi3
	.cfi_endproc

	.globl	gen_mulditi3
	.align	16, 0x90
	.type	gen_mulditi3,@function
gen_mulditi3:                           # @gen_mulditi3
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp3945:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp3946:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp3947:
	.cfi_def_cfa_offset 32
.Ltmp3948:
	.cfi_offset %rbx, -32
.Ltmp3949:
	.cfi_offset %r14, -24
.Ltmp3950:
	.cfi_offset %r15, -16
	movq	%rdx, %r15
	movq	%rsi, %rax
	movq	%rdi, %r14
	movq	$120, %rdi
	movq	$6, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rbx
	movq	$120, %rdi
	movq	$6, %rsi
	movq	%r15, %rdx
	callq	gen_rtx_fmt_e
	movq	$78, %rdi
	movq	$6, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rbx
	movq	$52, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rcx
	movq	$2, %rdi
	xorq	%rax, %rax
	movq	%rbx, %rsi
	movq	%rcx, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	popq	%rbx
	popq	%r14
	popq	%r15
	jmp	gen_rtx_fmt_E  # TAILCALL
.Ltmp3951:
	.size	gen_mulditi3, .Ltmp3951-gen_mulditi3
	.cfi_endproc

	.globl	gen_mulsidi3
	.align	16, 0x90
	.type	gen_mulsidi3,@function
gen_mulsidi3:                           # @gen_mulsidi3
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp3956:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp3957:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp3958:
	.cfi_def_cfa_offset 32
.Ltmp3959:
	.cfi_offset %rbx, -32
.Ltmp3960:
	.cfi_offset %r14, -24
.Ltmp3961:
	.cfi_offset %r15, -16
	movq	%rdx, %r15
	movq	%rsi, %rax
	movq	%rdi, %r14
	movq	$120, %rdi
	movq	$5, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rbx
	movq	$120, %rdi
	movq	$5, %rsi
	movq	%r15, %rdx
	callq	gen_rtx_fmt_e
	movq	$78, %rdi
	movq	$5, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rbx
	movq	$52, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rcx
	movq	$2, %rdi
	xorq	%rax, %rax
	movq	%rbx, %rsi
	movq	%rcx, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	popq	%rbx
	popq	%r14
	popq	%r15
	jmp	gen_rtx_fmt_E  # TAILCALL
.Ltmp3962:
	.size	gen_mulsidi3, .Ltmp3962-gen_mulsidi3
	.cfi_endproc

	.globl	gen_umuldi3_highpart
	.align	16, 0x90
	.type	gen_umuldi3_highpart,@function
gen_umuldi3_highpart:                   # @gen_umuldi3_highpart
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp3967:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp3968:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp3969:
	.cfi_def_cfa_offset 32
.Ltmp3970:
	.cfi_offset %rbx, -32
.Ltmp3971:
	.cfi_offset %r14, -24
.Ltmp3972:
	.cfi_offset %r15, -16
	movq	%rdx, %r15
	movq	%rsi, %rax
	movq	%rdi, %r14
	movq	$121, %rdi
	movq	$6, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rbx
	movq	$121, %rdi
	movq	$6, %rsi
	movq	%r15, %rdx
	callq	gen_rtx_fmt_e
	movq	$78, %rdi
	movq	$6, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rbx
	xorq	%rdi, %rdi
	movq	$64, %rsi
	callq	gen_rtx_CONST_INT
	movq	$90, %rdi
	movq	$6, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$122, %rdi
	movq	$5, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %r14
	movq	$62, %rdi
	movq	$5, %rsi
	callq	gen_rtx_fmt_0
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rbx
	movq	$52, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rcx
	movq	$3, %rdi
	xorq	%rax, %rax
	movq	%r14, %rsi
	movq	%rbx, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	popq	%rbx
	popq	%r14
	popq	%r15
	jmp	gen_rtx_fmt_E  # TAILCALL
.Ltmp3973:
	.size	gen_umuldi3_highpart, .Ltmp3973-gen_umuldi3_highpart
	.cfi_endproc

	.globl	gen_umulsi3_highpart
	.align	16, 0x90
	.type	gen_umulsi3_highpart,@function
gen_umulsi3_highpart:                   # @gen_umulsi3_highpart
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp3978:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp3979:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp3980:
	.cfi_def_cfa_offset 32
.Ltmp3981:
	.cfi_offset %rbx, -32
.Ltmp3982:
	.cfi_offset %r14, -24
.Ltmp3983:
	.cfi_offset %r15, -16
	movq	%rdx, %r15
	movq	%rsi, %rax
	movq	%rdi, %r14
	movq	$121, %rdi
	movq	$5, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rbx
	movq	$121, %rdi
	movq	$5, %rsi
	movq	%r15, %rdx
	callq	gen_rtx_fmt_e
	movq	$78, %rdi
	movq	$5, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rbx
	xorq	%rdi, %rdi
	movq	$32, %rsi
	callq	gen_rtx_CONST_INT
	movq	$90, %rdi
	movq	$5, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$122, %rdi
	movq	$4, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %r14
	movq	$62, %rdi
	movq	$4, %rsi
	callq	gen_rtx_fmt_0
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rbx
	movq	$52, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rcx
	movq	$3, %rdi
	xorq	%rax, %rax
	movq	%r14, %rsi
	movq	%rbx, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	popq	%rbx
	popq	%r14
	popq	%r15
	jmp	gen_rtx_fmt_E  # TAILCALL
.Ltmp3984:
	.size	gen_umulsi3_highpart, .Ltmp3984-gen_umulsi3_highpart
	.cfi_endproc

	.globl	gen_smuldi3_highpart
	.align	16, 0x90
	.type	gen_smuldi3_highpart,@function
gen_smuldi3_highpart:                   # @gen_smuldi3_highpart
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp3989:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp3990:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp3991:
	.cfi_def_cfa_offset 32
.Ltmp3992:
	.cfi_offset %rbx, -32
.Ltmp3993:
	.cfi_offset %r14, -24
.Ltmp3994:
	.cfi_offset %r15, -16
	movq	%rdx, %r15
	movq	%rsi, %rax
	movq	%rdi, %r14
	movq	$120, %rdi
	movq	$6, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rbx
	movq	$120, %rdi
	movq	$6, %rsi
	movq	%r15, %rdx
	callq	gen_rtx_fmt_e
	movq	$78, %rdi
	movq	$6, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rbx
	xorq	%rdi, %rdi
	movq	$64, %rsi
	callq	gen_rtx_CONST_INT
	movq	$90, %rdi
	movq	$6, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$122, %rdi
	movq	$5, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %r14
	movq	$62, %rdi
	movq	$5, %rsi
	callq	gen_rtx_fmt_0
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rbx
	movq	$52, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rcx
	movq	$3, %rdi
	xorq	%rax, %rax
	movq	%r14, %rsi
	movq	%rbx, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	popq	%rbx
	popq	%r14
	popq	%r15
	jmp	gen_rtx_fmt_E  # TAILCALL
.Ltmp3995:
	.size	gen_smuldi3_highpart, .Ltmp3995-gen_smuldi3_highpart
	.cfi_endproc

	.globl	gen_smulsi3_highpart
	.align	16, 0x90
	.type	gen_smulsi3_highpart,@function
gen_smulsi3_highpart:                   # @gen_smulsi3_highpart
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp4000:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp4001:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp4002:
	.cfi_def_cfa_offset 32
.Ltmp4003:
	.cfi_offset %rbx, -32
.Ltmp4004:
	.cfi_offset %r14, -24
.Ltmp4005:
	.cfi_offset %r15, -16
	movq	%rdx, %r15
	movq	%rsi, %rax
	movq	%rdi, %r14
	movq	$120, %rdi
	movq	$5, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rbx
	movq	$120, %rdi
	movq	$5, %rsi
	movq	%r15, %rdx
	callq	gen_rtx_fmt_e
	movq	$78, %rdi
	movq	$5, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rbx
	xorq	%rdi, %rdi
	movq	$32, %rsi
	callq	gen_rtx_CONST_INT
	movq	$90, %rdi
	movq	$5, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$122, %rdi
	movq	$4, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %r14
	movq	$62, %rdi
	movq	$4, %rsi
	callq	gen_rtx_fmt_0
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rbx
	movq	$52, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rcx
	movq	$3, %rdi
	xorq	%rax, %rax
	movq	%r14, %rsi
	movq	%rbx, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	popq	%rbx
	popq	%r14
	popq	%r15
	jmp	gen_rtx_fmt_E  # TAILCALL
.Ltmp4006:
	.size	gen_smulsi3_highpart, .Ltmp4006-gen_smulsi3_highpart
	.cfi_endproc

	.globl	gen_mulxf3
	.align	16, 0x90
	.type	gen_mulxf3,@function
gen_mulxf3:                             # @gen_mulxf3
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp4009:
	.cfi_def_cfa_offset 16
.Ltmp4010:
	.cfi_offset %rbx, -16
	movq	%rdx, %rax
	movq	%rsi, %rcx
	movq	%rdi, %rbx
	movq	$78, %rdi
	movq	$17, %rsi
	movq	%rcx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	popq	%rbx
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp4011:
	.size	gen_mulxf3, .Ltmp4011-gen_mulxf3
	.cfi_endproc

	.globl	gen_multf3
	.align	16, 0x90
	.type	gen_multf3,@function
gen_multf3:                             # @gen_multf3
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp4014:
	.cfi_def_cfa_offset 16
.Ltmp4015:
	.cfi_offset %rbx, -16
	movq	%rdx, %rax
	movq	%rsi, %rcx
	movq	%rdi, %rbx
	movq	$78, %rdi
	movq	$18, %rsi
	movq	%rcx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	popq	%rbx
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp4016:
	.size	gen_multf3, .Ltmp4016-gen_multf3
	.cfi_endproc

	.globl	gen_muldf3
	.align	16, 0x90
	.type	gen_muldf3,@function
gen_muldf3:                             # @gen_muldf3
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp4019:
	.cfi_def_cfa_offset 16
.Ltmp4020:
	.cfi_offset %rbx, -16
	movq	%rdx, %rax
	movq	%rsi, %rcx
	movq	%rdi, %rbx
	movq	$78, %rdi
	movq	$16, %rsi
	movq	%rcx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	popq	%rbx
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp4021:
	.size	gen_muldf3, .Ltmp4021-gen_muldf3
	.cfi_endproc

	.globl	gen_mulsf3
	.align	16, 0x90
	.type	gen_mulsf3,@function
gen_mulsf3:                             # @gen_mulsf3
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp4024:
	.cfi_def_cfa_offset 16
.Ltmp4025:
	.cfi_offset %rbx, -16
	movq	%rdx, %rax
	movq	%rsi, %rcx
	movq	%rdi, %rbx
	movq	$78, %rdi
	movq	$15, %rsi
	movq	%rcx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	popq	%rbx
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp4026:
	.size	gen_mulsf3, .Ltmp4026-gen_mulsf3
	.cfi_endproc

	.globl	gen_divxf3
	.align	16, 0x90
	.type	gen_divxf3,@function
gen_divxf3:                             # @gen_divxf3
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp4029:
	.cfi_def_cfa_offset 16
.Ltmp4030:
	.cfi_offset %rbx, -16
	movq	%rdx, %rax
	movq	%rsi, %rcx
	movq	%rdi, %rbx
	movq	$79, %rdi
	movq	$17, %rsi
	movq	%rcx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	popq	%rbx
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp4031:
	.size	gen_divxf3, .Ltmp4031-gen_divxf3
	.cfi_endproc

	.globl	gen_divtf3
	.align	16, 0x90
	.type	gen_divtf3,@function
gen_divtf3:                             # @gen_divtf3
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp4034:
	.cfi_def_cfa_offset 16
.Ltmp4035:
	.cfi_offset %rbx, -16
	movq	%rdx, %rax
	movq	%rsi, %rcx
	movq	%rdi, %rbx
	movq	$79, %rdi
	movq	$18, %rsi
	movq	%rcx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	popq	%rbx
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp4036:
	.size	gen_divtf3, .Ltmp4036-gen_divtf3
	.cfi_endproc

	.globl	gen_divdf3
	.align	16, 0x90
	.type	gen_divdf3,@function
gen_divdf3:                             # @gen_divdf3
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp4039:
	.cfi_def_cfa_offset 16
.Ltmp4040:
	.cfi_offset %rbx, -16
	movq	%rdx, %rax
	movq	%rsi, %rcx
	movq	%rdi, %rbx
	movq	$79, %rdi
	movq	$16, %rsi
	movq	%rcx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	popq	%rbx
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp4041:
	.size	gen_divdf3, .Ltmp4041-gen_divdf3
	.cfi_endproc

	.globl	gen_divsf3
	.align	16, 0x90
	.type	gen_divsf3,@function
gen_divsf3:                             # @gen_divsf3
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp4044:
	.cfi_def_cfa_offset 16
.Ltmp4045:
	.cfi_offset %rbx, -16
	movq	%rdx, %rax
	movq	%rsi, %rcx
	movq	%rdi, %rbx
	movq	$79, %rdi
	movq	$15, %rsi
	movq	%rcx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	popq	%rbx
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp4046:
	.size	gen_divsf3, .Ltmp4046-gen_divsf3
	.cfi_endproc

	.globl	gen_divmoddi4
	.align	16, 0x90
	.type	gen_divmoddi4,@function
gen_divmoddi4:                          # @gen_divmoddi4
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp4052:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp4053:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp4054:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp4055:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp4056:
	.cfi_def_cfa_offset 48
.Ltmp4057:
	.cfi_offset %rbx, -40
.Ltmp4058:
	.cfi_offset %r12, -32
.Ltmp4059:
	.cfi_offset %r14, -24
.Ltmp4060:
	.cfi_offset %r15, -16
	movq	%rcx, %r15
	movq	%rdx, %r12
	movq	%rsi, %rbx
	movq	%rdi, %r14
	movq	$79, %rdi
	movq	$5, %rsi
	movq	%rbx, %rdx
	movq	%r12, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %r14
	movq	$80, %rdi
	movq	$5, %rsi
	movq	%rbx, %rdx
	movq	%r12, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r15, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rbx
	movq	$52, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rcx
	movq	$3, %rdi
	xorq	%rax, %rax
	movq	%r14, %rsi
	movq	%rbx, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	jmp	gen_rtx_fmt_E  # TAILCALL
.Ltmp4061:
	.size	gen_divmoddi4, .Ltmp4061-gen_divmoddi4
	.cfi_endproc

	.globl	gen_split_1001
	.align	16, 0x90
	.type	gen_split_1001,@function
gen_split_1001:                         # @gen_split_1001
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp4068:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp4069:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp4070:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp4071:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp4072:
	.cfi_def_cfa_offset 48
.Ltmp4073:
	.cfi_offset %rbx, -48
.Ltmp4074:
	.cfi_offset %r12, -40
.Ltmp4075:
	.cfi_offset %r13, -32
.Ltmp4076:
	.cfi_offset %r14, -24
.Ltmp4077:
	.cfi_offset %r15, -16
	movq	%rdi, %r14
	callq	start_sequence
	movl	x86_use_cltd(%rip), %eax
	movl	ix86_cpu(%rip), %ecx
	movq	$1, %rdx
	movl	optimize_size(%rip), %ebx
	movq	8(%r14), %rdi
	shlq	%cl, %rdx
	andq	%rax, %rdx
	orq	%rdx, %rbx
	callq	true_regnum
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rbx
	je	.LBB457_1
# BB#5:                                 # %if.else12
	testq	%rcx, %rax
	jne	.LBB457_8
# BB#6:                                 # %if.end17
	movq	8(%r14), %r13
	movq	%r13, 32(%r14)
	movq	24(%r14), %r12
	jmp	.LBB457_7
.LBB457_1:                              # %if.then
	testq	%rcx, %rax
	je	.LBB457_3
# BB#2:                                 # %if.then3
	movq	(%r14), %rdi
	movq	8(%r14), %rsi
	jmp	.LBB457_4
.LBB457_3:                              # %if.else
	movq	8(%r14), %rsi
	movq	24(%r14), %rdi
.LBB457_4:                              # %if.end
	callq	emit_move_insn
	movq	24(%r14), %r13
	movq	%r13, 32(%r14)
	movq	%r13, %r12
.LBB457_7:                              # %if.end20
	movq	(%r14), %r15
	movq	16(%r14), %r14
	xorq	%rdi, %rdi
	movq	$63, %rsi
	callq	gen_rtx_CONST_INT
	movq	$89, %rdi
	movq	$5, %rsi
	movq	%r13, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r12, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rbx
	movq	$52, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rcx
	movq	$2, %rdi
	xorq	%rax, %rax
	movq	%rbx, %rsi
	movq	%rcx, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_E
	movq	%rax, %rdi
	callq	emit
	movq	$5, %rdi
	xorq	%rsi, %rsi
	callq	gen_rtx_REG
	movq	$79, %rdi
	movq	$5, %rsi
	movq	%rax, %rdx
	movq	%r14, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r15, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %r15
	movq	%r12, %rdi
	callq	copy_rtx
	movq	%rax, %r13
	movq	$5, %rdi
	xorq	%rsi, %rsi
	callq	gen_rtx_REG
	movq	%rax, %rbx
	movq	%r14, %rdi
	callq	copy_rtx
	movq	$80, %rdi
	movq	$5, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r13, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %r14
	movq	%r12, %rdi
	callq	copy_rtx
	movq	$48, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %r12
	movq	$52, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rbx
	movq	$4, %rdi
	xorq	%rax, %rax
	movq	%r15, %rsi
	movq	%r14, %rdx
	movq	%r12, %rcx
	movq	%rbx, %r8
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_E
	movq	%rax, %rdi
	callq	emit
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB457_8:                              # %if.then16
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.gen_split_1001, %rcx
	movq	$9237, %rsi             # imm = 0x2415
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp4078:
	.size	gen_split_1001, .Ltmp4078-gen_split_1001
	.cfi_endproc

	.globl	gen_divmodsi4
	.align	16, 0x90
	.type	gen_divmodsi4,@function
gen_divmodsi4:                          # @gen_divmodsi4
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp4084:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp4085:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp4086:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp4087:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp4088:
	.cfi_def_cfa_offset 48
.Ltmp4089:
	.cfi_offset %rbx, -40
.Ltmp4090:
	.cfi_offset %r12, -32
.Ltmp4091:
	.cfi_offset %r14, -24
.Ltmp4092:
	.cfi_offset %r15, -16
	movq	%rcx, %r15
	movq	%rdx, %r12
	movq	%rsi, %rbx
	movq	%rdi, %r14
	movq	$79, %rdi
	movq	$4, %rsi
	movq	%rbx, %rdx
	movq	%r12, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %r14
	movq	$80, %rdi
	movq	$4, %rsi
	movq	%rbx, %rdx
	movq	%r12, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r15, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rbx
	movq	$52, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rcx
	movq	$3, %rdi
	xorq	%rax, %rax
	movq	%r14, %rsi
	movq	%rbx, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	jmp	gen_rtx_fmt_E  # TAILCALL
.Ltmp4093:
	.size	gen_divmodsi4, .Ltmp4093-gen_divmodsi4
	.cfi_endproc

	.globl	gen_split_1003
	.align	16, 0x90
	.type	gen_split_1003,@function
gen_split_1003:                         # @gen_split_1003
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp4100:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp4101:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp4102:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp4103:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp4104:
	.cfi_def_cfa_offset 48
.Ltmp4105:
	.cfi_offset %rbx, -48
.Ltmp4106:
	.cfi_offset %r12, -40
.Ltmp4107:
	.cfi_offset %r13, -32
.Ltmp4108:
	.cfi_offset %r14, -24
.Ltmp4109:
	.cfi_offset %r15, -16
	movq	%rdi, %r14
	callq	start_sequence
	movl	x86_use_cltd(%rip), %eax
	movl	ix86_cpu(%rip), %ecx
	movq	$1, %rdx
	movl	optimize_size(%rip), %ebx
	movq	8(%r14), %rdi
	shlq	%cl, %rdx
	andq	%rax, %rdx
	orq	%rdx, %rbx
	callq	true_regnum
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rbx
	je	.LBB459_1
# BB#5:                                 # %if.else12
	testq	%rcx, %rax
	jne	.LBB459_8
# BB#6:                                 # %if.end17
	movq	8(%r14), %r13
	movq	%r13, 32(%r14)
	movq	24(%r14), %r12
	jmp	.LBB459_7
.LBB459_1:                              # %if.then
	testq	%rcx, %rax
	je	.LBB459_3
# BB#2:                                 # %if.then3
	movq	(%r14), %rdi
	movq	8(%r14), %rsi
	jmp	.LBB459_4
.LBB459_3:                              # %if.else
	movq	8(%r14), %rsi
	movq	24(%r14), %rdi
.LBB459_4:                              # %if.end
	callq	emit_move_insn
	movq	24(%r14), %r13
	movq	%r13, 32(%r14)
	movq	%r13, %r12
.LBB459_7:                              # %if.end20
	movq	(%r14), %r15
	movq	16(%r14), %r14
	xorq	%rdi, %rdi
	movq	$31, %rsi
	callq	gen_rtx_CONST_INT
	movq	$89, %rdi
	movq	$4, %rsi
	movq	%r13, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r12, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rbx
	movq	$52, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rcx
	movq	$2, %rdi
	xorq	%rax, %rax
	movq	%rbx, %rsi
	movq	%rcx, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_E
	movq	%rax, %rdi
	callq	emit
	movq	$4, %rdi
	xorq	%rsi, %rsi
	callq	gen_rtx_REG
	movq	$79, %rdi
	movq	$4, %rsi
	movq	%rax, %rdx
	movq	%r14, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r15, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %r15
	movq	%r12, %rdi
	callq	copy_rtx
	movq	%rax, %r13
	movq	$4, %rdi
	xorq	%rsi, %rsi
	callq	gen_rtx_REG
	movq	%rax, %rbx
	movq	%r14, %rdi
	callq	copy_rtx
	movq	$80, %rdi
	movq	$4, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r13, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %r14
	movq	%r12, %rdi
	callq	copy_rtx
	movq	$48, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %r12
	movq	$52, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rbx
	movq	$4, %rdi
	xorq	%rax, %rax
	movq	%r15, %rsi
	movq	%r14, %rdx
	movq	%r12, %rcx
	movq	%rbx, %r8
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_E
	movq	%rax, %rdi
	callq	emit
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB459_8:                              # %if.then16
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.gen_split_1003, %rcx
	movq	$9329, %rsi             # imm = 0x2471
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp4110:
	.size	gen_split_1003, .Ltmp4110-gen_split_1003
	.cfi_endproc

	.globl	gen_split_1004
	.align	16, 0x90
	.type	gen_split_1004,@function
gen_split_1004:                         # @gen_split_1004
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp4117:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp4118:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp4119:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp4120:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp4121:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp4122:
	.cfi_def_cfa_offset 64
.Ltmp4123:
	.cfi_offset %rbx, -48
.Ltmp4124:
	.cfi_offset %r12, -40
.Ltmp4125:
	.cfi_offset %r13, -32
.Ltmp4126:
	.cfi_offset %r14, -24
.Ltmp4127:
	.cfi_offset %r15, -16
	movq	%rdi, %rbx
	callq	start_sequence
	movq	(%rbx), %r14
	movq	8(%rbx), %r13
	movq	16(%rbx), %r15
	movq	24(%rbx), %r12
	movq	const_int_rtx+512(%rip), %rcx
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r12, %rdx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	movq	$81, %rdi
	movq	$5, %rsi
	movq	%r13, %rdx
	movq	%r15, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, 8(%rsp)           # 8-byte Spill
	movq	%r12, %rdi
	callq	copy_rtx
	movq	%rax, %r14
	movq	%r13, %rdi
	callq	copy_rtx
	movq	%rax, %rbx
	movq	%r15, %rdi
	callq	copy_rtx
	movq	$82, %rdi
	movq	$5, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %r14
	movq	%r12, %rdi
	callq	copy_rtx
	movq	$48, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %r15
	movq	$52, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rbx
	movq	$4, %rdi
	xorq	%rax, %rax
	movq	8(%rsp), %rsi           # 8-byte Reload
	movq	%r14, %rdx
	movq	%r15, %rcx
	movq	%rbx, %r8
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_E
	movq	%rax, %rdi
	callq	emit
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp4128:
	.size	gen_split_1004, .Ltmp4128-gen_split_1004
	.cfi_endproc

	.globl	gen_split_1005
	.align	16, 0x90
	.type	gen_split_1005,@function
gen_split_1005:                         # @gen_split_1005
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp4135:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp4136:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp4137:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp4138:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp4139:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp4140:
	.cfi_def_cfa_offset 64
.Ltmp4141:
	.cfi_offset %rbx, -48
.Ltmp4142:
	.cfi_offset %r12, -40
.Ltmp4143:
	.cfi_offset %r13, -32
.Ltmp4144:
	.cfi_offset %r14, -24
.Ltmp4145:
	.cfi_offset %r15, -16
	movq	%rdi, %rbx
	callq	start_sequence
	movq	(%rbx), %r14
	movq	8(%rbx), %r13
	movq	16(%rbx), %r15
	movq	24(%rbx), %r12
	movq	const_int_rtx+512(%rip), %rcx
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r12, %rdx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	movq	$81, %rdi
	movq	$4, %rsi
	movq	%r13, %rdx
	movq	%r15, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, 8(%rsp)           # 8-byte Spill
	movq	%r12, %rdi
	callq	copy_rtx
	movq	%rax, %r14
	movq	%r13, %rdi
	callq	copy_rtx
	movq	%rax, %rbx
	movq	%r15, %rdi
	callq	copy_rtx
	movq	$82, %rdi
	movq	$4, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %r14
	movq	%r12, %rdi
	callq	copy_rtx
	movq	$48, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %r15
	movq	$52, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rbx
	movq	$4, %rdi
	xorq	%rax, %rax
	movq	8(%rsp), %rsi           # 8-byte Reload
	movq	%r14, %rdx
	movq	%r15, %rcx
	movq	%rbx, %r8
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_E
	movq	%rax, %rdi
	callq	emit
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp4146:
	.size	gen_split_1005, .Ltmp4146-gen_split_1005
	.cfi_endproc

	.globl	gen_udivmodhi4
	.align	16, 0x90
	.type	gen_udivmodhi4,@function
gen_udivmodhi4:                         # @gen_udivmodhi4
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp4153:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp4154:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp4155:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp4156:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp4157:
	.cfi_def_cfa_offset 48
.Ltmp4158:
	.cfi_offset %rbx, -48
.Ltmp4159:
	.cfi_offset %r12, -40
.Ltmp4160:
	.cfi_offset %r13, -32
.Ltmp4161:
	.cfi_offset %r14, -24
.Ltmp4162:
	.cfi_offset %r15, -16
	movq	%rcx, %r14
	movq	%rdx, %r13
	movq	%rsi, %rbx
	movq	%rdi, %r15
	callq	start_sequence
	movq	$3, %rdi
	callq	gen_reg_rtx
	movq	const_int_rtx+512(%rip), %rcx
	movq	%rax, %r12
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r12, %rdx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	movq	$81, %rdi
	movq	$3, %rsi
	movq	%rbx, %rdx
	movq	%r13, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r15, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %r15
	movq	$82, %rdi
	movq	$3, %rsi
	movq	%rbx, %rdx
	movq	%r13, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %r14
	movq	$48, %rdi
	xorq	%rsi, %rsi
	movq	%r12, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %r12
	movq	$52, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rbx
	movq	$4, %rdi
	xorq	%rax, %rax
	movq	%r15, %rsi
	movq	%r14, %rdx
	movq	%r12, %rcx
	movq	%rbx, %r8
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_E
	movq	%rax, %rdi
	callq	emit
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp4163:
	.size	gen_udivmodhi4, .Ltmp4163-gen_udivmodhi4
	.cfi_endproc

	.globl	gen_testsi_ccno_1
	.align	16, 0x90
	.type	gen_testsi_ccno_1,@function
gen_testsi_ccno_1:                      # @gen_testsi_ccno_1
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp4168:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp4169:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp4170:
	.cfi_def_cfa_offset 32
.Ltmp4171:
	.cfi_offset %rbx, -32
.Ltmp4172:
	.cfi_offset %r14, -24
.Ltmp4173:
	.cfi_offset %r15, -16
	movq	%rsi, %r14
	movq	%rdi, %rbx
	movq	$55, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	%rax, %r15
	movq	$83, %rdi
	movq	$4, %rsi
	movq	%rbx, %rdx
	movq	%r14, %rcx
	callq	gen_rtx_fmt_ee
	movq	const_int_rtx+512(%rip), %rcx
	movq	$74, %rdi
	movq	$55, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r15, %rdx
	movq	%rax, %rcx
	popq	%rbx
	popq	%r14
	popq	%r15
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp4174:
	.size	gen_testsi_ccno_1, .Ltmp4174-gen_testsi_ccno_1
	.cfi_endproc

	.globl	gen_testqi_ccz_1
	.align	16, 0x90
	.type	gen_testqi_ccz_1,@function
gen_testqi_ccz_1:                       # @gen_testqi_ccz_1
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp4179:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp4180:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp4181:
	.cfi_def_cfa_offset 32
.Ltmp4182:
	.cfi_offset %rbx, -32
.Ltmp4183:
	.cfi_offset %r14, -24
.Ltmp4184:
	.cfi_offset %r15, -16
	movq	%rsi, %r14
	movq	%rdi, %rbx
	movq	$56, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	%rax, %r15
	movq	$83, %rdi
	movq	$2, %rsi
	movq	%rbx, %rdx
	movq	%r14, %rcx
	callq	gen_rtx_fmt_ee
	movq	const_int_rtx+512(%rip), %rcx
	movq	$74, %rdi
	movq	$56, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r15, %rdx
	movq	%rax, %rcx
	popq	%rbx
	popq	%r14
	popq	%r15
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp4185:
	.size	gen_testqi_ccz_1, .Ltmp4185-gen_testqi_ccz_1
	.cfi_endproc

	.globl	gen_testqi_ext_ccno_0
	.align	16, 0x90
	.type	gen_testqi_ext_ccno_0,@function
gen_testqi_ext_ccno_0:                  # @gen_testqi_ext_ccno_0
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp4191:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp4192:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp4193:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp4194:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp4195:
	.cfi_def_cfa_offset 48
.Ltmp4196:
	.cfi_offset %rbx, -40
.Ltmp4197:
	.cfi_offset %r12, -32
.Ltmp4198:
	.cfi_offset %r14, -24
.Ltmp4199:
	.cfi_offset %r15, -16
	movq	%rsi, %r15
	movq	%rdi, %r12
	movq	$55, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	%rax, %r14
	xorq	%rdi, %rdi
	movq	$8, %rsi
	callq	gen_rtx_CONST_INT
	movq	%rax, %rbx
	xorq	%rdi, %rdi
	movq	$8, %rsi
	callq	gen_rtx_CONST_INT
	movq	$133, %rdi
	movq	$4, %rsi
	movq	%r12, %rdx
	movq	%rbx, %rcx
	movq	%rax, %r8
	callq	gen_rtx_fmt_eee
	movq	$83, %rdi
	movq	$4, %rsi
	movq	%rax, %rdx
	movq	%r15, %rcx
	callq	gen_rtx_fmt_ee
	movq	const_int_rtx+512(%rip), %rcx
	movq	$74, %rdi
	movq	$55, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp4200:
	.size	gen_testqi_ext_ccno_0, .Ltmp4200-gen_testqi_ext_ccno_0
	.cfi_endproc

	.globl	gen_split_1010
	.align	16, 0x90
	.type	gen_split_1010,@function
gen_split_1010:                         # @gen_split_1010
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp4207:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp4208:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp4209:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp4210:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp4211:
	.cfi_def_cfa_offset 48
.Ltmp4212:
	.cfi_offset %rbx, -48
.Ltmp4213:
	.cfi_offset %r12, -40
.Ltmp4214:
	.cfi_offset %r13, -32
.Ltmp4215:
	.cfi_offset %r14, -24
.Ltmp4216:
	.cfi_offset %r15, -16
	movq	%rdi, %r15
	callq	start_sequence
	movq	(%r15), %rsi
	movq	8(%r15), %rax
	movq	16(%r15), %rcx
	movl	(%rsi), %edx
	movq	8(%rax), %r12
	movq	8(%rcx), %r14
	movq	$16, %rcx
	movq	%rdx, %rax
	shrq	%cl, %rax
	andq	$255, %rax
	movq	%rdx, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$63, %rcx
	jne	.LBB466_1
# BB#5:                                 # %land.lhs.true
	movq	8(%rsi), %r13
	movzbl	2(%r13), %ebx
	movzbl	mode_bitsize(%rax,%rax), %edx
	movzbl	mode_bitsize(%rbx,%rbx), %ecx
	cmpq	%rcx, %rdx
	jbe	.LBB466_8
# BB#6:                                 # %land.lhs.true36
	leaq	(%r14,%r12), %rdx
	cmpq	%rcx, %rdx
	jg	.LBB466_8
# BB#7:                                 # %if.then43
	movq	%r13, (%r15)
	movq	%rdx, %r12
	jmp	.LBB466_12
.LBB466_1:                              # %entry
	cmpq	$66, %rcx
	jne	.LBB466_8
# BB#2:                                 # %if.then
	addq	%r14, %r12
	testq	$134217728, %rdx        # imm = 0x8000000
	jne	.LBB466_3
# BB#4:                                 # %if.then14
	movq	$1, %rsi
	movq	%r12, %rdi
	callq	smallest_mode_for_size
	movq	(%r15), %rdi
	movq	%rax, %rbx
	xorq	%rdx, %rdx
	movq	$1, %rcx
	movq	$1, %r8
	movq	%rbx, %rsi
	callq	adjust_address_1
	jmp	.LBB466_11
.LBB466_8:                              # %if.else49
	addq	%r14, %r12
	cmpq	$3, %rax
	jne	.LBB466_3
# BB#9:                                 # %if.else49
	cmpq	$8, %r12
	jg	.LBB466_3
# BB#10:                                # %if.then56
	movq	$2, %rdi
	movq	$2, %rbx
	callq	gen_lowpart
.LBB466_11:                             # %if.end62
	movq	%rax, %r13
	movq	%r13, (%r15)
	jmp	.LBB466_12
.LBB466_3:
	movq	%rsi, %r13
	movq	%rax, %rbx
.LBB466_12:                             # %if.end62
	movq	$1, %rax
	movq	%r12, %rcx
	shlq	%cl, %rax
	decq	%rax
	movq	$-1, %rdi
	movq	%r14, %rcx
	shlq	%cl, %rdi
	andq	%rax, %rdi
	movq	%rbx, %rsi
	callq	trunc_int_for_mode
	xorq	%rdi, %rdi
	movq	%rax, %rsi
	callq	gen_rtx_CONST_INT
	movq	$83, %rdi
	movq	%rbx, %rsi
	movq	%r13, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rbx
	movq	%rbx, 24(%r15)
	movq	$55, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	const_int_rtx+512(%rip), %rcx
	movq	%rax, %r14
	movq	$74, %rdi
	movq	$55, %rsi
	movq	%rbx, %rdx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp4217:
	.size	gen_split_1010, .Ltmp4217-gen_split_1010
	.cfi_endproc

	.globl	gen_anddi3
	.align	16, 0x90
	.type	gen_anddi3,@function
gen_anddi3:                             # @gen_anddi3
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp4222:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp4223:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp4224:
	.cfi_def_cfa_offset 32
	subq	$32, %rsp
.Ltmp4225:
	.cfi_def_cfa_offset 64
.Ltmp4226:
	.cfi_offset %rbx, -32
.Ltmp4227:
	.cfi_offset %r14, -24
.Ltmp4228:
	.cfi_offset %r15, -16
	movq	%rdx, %r14
	movq	%rsi, %r15
	movq	%rdi, %rbx
	callq	start_sequence
	movq	%rbx, (%rsp)
	movq	%r15, 8(%rsp)
	movq	%r14, 16(%rsp)
	leaq	(%rsp), %rdx
	movq	$83, %rdi
	movq	$5, %rsi
	callq	ix86_expand_binary_operator
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	addq	$32, %rsp
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp4229:
	.size	gen_anddi3, .Ltmp4229-gen_anddi3
	.cfi_endproc

	.globl	gen_andsi3
	.align	16, 0x90
	.type	gen_andsi3,@function
gen_andsi3:                             # @gen_andsi3
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp4234:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp4235:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp4236:
	.cfi_def_cfa_offset 32
	subq	$32, %rsp
.Ltmp4237:
	.cfi_def_cfa_offset 64
.Ltmp4238:
	.cfi_offset %rbx, -32
.Ltmp4239:
	.cfi_offset %r14, -24
.Ltmp4240:
	.cfi_offset %r15, -16
	movq	%rdx, %r14
	movq	%rsi, %r15
	movq	%rdi, %rbx
	callq	start_sequence
	movq	%rbx, (%rsp)
	movq	%r15, 8(%rsp)
	movq	%r14, 16(%rsp)
	leaq	(%rsp), %rdx
	movq	$83, %rdi
	movq	$4, %rsi
	callq	ix86_expand_binary_operator
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	addq	$32, %rsp
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp4241:
	.size	gen_andsi3, .Ltmp4241-gen_andsi3
	.cfi_endproc

	.globl	gen_split_1013
	.align	16, 0x90
	.type	gen_split_1013,@function
gen_split_1013:                         # @gen_split_1013
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp4244:
	.cfi_def_cfa_offset 16
.Ltmp4245:
	.cfi_offset %rbx, -16
	movq	%rdi, %rbx
	callq	start_sequence
	movq	(%rbx), %rsi
	movq	$3, %rdi
	callq	gen_lowpart
	movq	%rax, 8(%rbx)
	movq	$64, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	const_int_rtx+512(%rip), %rcx
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	popq	%rbx
	retq
.Ltmp4246:
	.size	gen_split_1013, .Ltmp4246-gen_split_1013
	.cfi_endproc

	.globl	gen_split_1014
	.align	16, 0x90
	.type	gen_split_1014,@function
gen_split_1014:                         # @gen_split_1014
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp4249:
	.cfi_def_cfa_offset 16
.Ltmp4250:
	.cfi_offset %rbx, -16
	movq	%rdi, %rbx
	callq	start_sequence
	movq	(%rbx), %rsi
	movq	$2, %rdi
	callq	gen_lowpart
	movq	%rax, 8(%rbx)
	movq	$64, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	const_int_rtx+512(%rip), %rcx
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	popq	%rbx
	retq
.Ltmp4251:
	.size	gen_split_1014, .Ltmp4251-gen_split_1014
	.cfi_endproc

	.globl	gen_split_1015
	.align	16, 0x90
	.type	gen_split_1015,@function
gen_split_1015:                         # @gen_split_1015
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp4257:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp4258:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp4259:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp4260:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp4261:
	.cfi_def_cfa_offset 48
.Ltmp4262:
	.cfi_offset %rbx, -40
.Ltmp4263:
	.cfi_offset %r12, -32
.Ltmp4264:
	.cfi_offset %r14, -24
.Ltmp4265:
	.cfi_offset %r15, -16
	movq	%rdi, %rbx
	callq	start_sequence
	movq	(%rbx), %rsi
	movq	$4, %rdi
	callq	gen_lowpart
	movq	%rax, %r15
	movq	%r15, (%rbx)
	xorq	%rdi, %rdi
	movq	$8, %rsi
	callq	gen_rtx_CONST_INT
	movq	%rax, %rbx
	xorq	%rdi, %rdi
	movq	$8, %rsi
	callq	gen_rtx_CONST_INT
	movq	$133, %rdi
	movq	$4, %rsi
	movq	%r15, %rdx
	movq	%rbx, %rcx
	movq	%rax, %r8
	callq	gen_rtx_fmt_eee
	movq	%rax, %r14
	movq	%r15, %rdi
	callq	copy_rtx
	movq	%rax, %r12
	xorq	%rdi, %rdi
	movq	$8, %rsi
	callq	gen_rtx_CONST_INT
	movq	%rax, %rbx
	xorq	%rdi, %rdi
	movq	$8, %rsi
	callq	gen_rtx_CONST_INT
	movq	$133, %rdi
	movq	$4, %rsi
	movq	%r12, %rdx
	movq	%rbx, %rcx
	movq	%rax, %r8
	callq	gen_rtx_fmt_eee
	movq	%rax, %r12
	movq	%r15, %rdi
	callq	copy_rtx
	movq	%rax, %r15
	xorq	%rdi, %rdi
	movq	$8, %rsi
	callq	gen_rtx_CONST_INT
	movq	%rax, %rbx
	xorq	%rdi, %rdi
	movq	$8, %rsi
	callq	gen_rtx_CONST_INT
	movq	$133, %rdi
	movq	$4, %rsi
	movq	%r15, %rdx
	movq	%rbx, %rcx
	movq	%rax, %r8
	callq	gen_rtx_fmt_eee
	movq	$85, %rdi
	movq	$4, %rsi
	movq	%r12, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rbx
	movq	$52, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rcx
	movq	$2, %rdi
	xorq	%rax, %rax
	movq	%rbx, %rsi
	movq	%rcx, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_E
	movq	%rax, %rdi
	callq	emit
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.Ltmp4266:
	.size	gen_split_1015, .Ltmp4266-gen_split_1015
	.cfi_endproc

	.globl	gen_andhi3
	.align	16, 0x90
	.type	gen_andhi3,@function
gen_andhi3:                             # @gen_andhi3
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp4271:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp4272:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp4273:
	.cfi_def_cfa_offset 32
	subq	$32, %rsp
.Ltmp4274:
	.cfi_def_cfa_offset 64
.Ltmp4275:
	.cfi_offset %rbx, -32
.Ltmp4276:
	.cfi_offset %r14, -24
.Ltmp4277:
	.cfi_offset %r15, -16
	movq	%rdx, %r14
	movq	%rsi, %r15
	movq	%rdi, %rbx
	callq	start_sequence
	movq	%rbx, (%rsp)
	movq	%r15, 8(%rsp)
	movq	%r14, 16(%rsp)
	leaq	(%rsp), %rdx
	movq	$83, %rdi
	movq	$3, %rsi
	callq	ix86_expand_binary_operator
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	addq	$32, %rsp
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp4278:
	.size	gen_andhi3, .Ltmp4278-gen_andhi3
	.cfi_endproc

	.globl	gen_andqi3
	.align	16, 0x90
	.type	gen_andqi3,@function
gen_andqi3:                             # @gen_andqi3
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp4283:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp4284:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp4285:
	.cfi_def_cfa_offset 32
	subq	$32, %rsp
.Ltmp4286:
	.cfi_def_cfa_offset 64
.Ltmp4287:
	.cfi_offset %rbx, -32
.Ltmp4288:
	.cfi_offset %r14, -24
.Ltmp4289:
	.cfi_offset %r15, -16
	movq	%rdx, %r14
	movq	%rsi, %r15
	movq	%rdi, %rbx
	callq	start_sequence
	movq	%rbx, (%rsp)
	movq	%r15, 8(%rsp)
	movq	%r14, 16(%rsp)
	leaq	(%rsp), %rdx
	movq	$83, %rdi
	movq	$2, %rsi
	callq	ix86_expand_binary_operator
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	addq	$32, %rsp
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp4290:
	.size	gen_andqi3, .Ltmp4290-gen_andqi3
	.cfi_endproc

	.globl	gen_iordi3
	.align	16, 0x90
	.type	gen_iordi3,@function
gen_iordi3:                             # @gen_iordi3
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp4295:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp4296:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp4297:
	.cfi_def_cfa_offset 32
	subq	$32, %rsp
.Ltmp4298:
	.cfi_def_cfa_offset 64
.Ltmp4299:
	.cfi_offset %rbx, -32
.Ltmp4300:
	.cfi_offset %r14, -24
.Ltmp4301:
	.cfi_offset %r15, -16
	movq	%rdx, %r14
	movq	%rsi, %r15
	movq	%rdi, %rbx
	callq	start_sequence
	movq	%rbx, (%rsp)
	movq	%r15, 8(%rsp)
	movq	%r14, 16(%rsp)
	leaq	(%rsp), %rdx
	movq	$84, %rdi
	movq	$5, %rsi
	callq	ix86_expand_binary_operator
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	addq	$32, %rsp
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp4302:
	.size	gen_iordi3, .Ltmp4302-gen_iordi3
	.cfi_endproc

	.globl	gen_iorsi3
	.align	16, 0x90
	.type	gen_iorsi3,@function
gen_iorsi3:                             # @gen_iorsi3
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp4307:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp4308:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp4309:
	.cfi_def_cfa_offset 32
	subq	$32, %rsp
.Ltmp4310:
	.cfi_def_cfa_offset 64
.Ltmp4311:
	.cfi_offset %rbx, -32
.Ltmp4312:
	.cfi_offset %r14, -24
.Ltmp4313:
	.cfi_offset %r15, -16
	movq	%rdx, %r14
	movq	%rsi, %r15
	movq	%rdi, %rbx
	callq	start_sequence
	movq	%rbx, (%rsp)
	movq	%r15, 8(%rsp)
	movq	%r14, 16(%rsp)
	leaq	(%rsp), %rdx
	movq	$84, %rdi
	movq	$4, %rsi
	callq	ix86_expand_binary_operator
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	addq	$32, %rsp
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp4314:
	.size	gen_iorsi3, .Ltmp4314-gen_iorsi3
	.cfi_endproc

	.globl	gen_iorhi3
	.align	16, 0x90
	.type	gen_iorhi3,@function
gen_iorhi3:                             # @gen_iorhi3
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp4319:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp4320:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp4321:
	.cfi_def_cfa_offset 32
	subq	$32, %rsp
.Ltmp4322:
	.cfi_def_cfa_offset 64
.Ltmp4323:
	.cfi_offset %rbx, -32
.Ltmp4324:
	.cfi_offset %r14, -24
.Ltmp4325:
	.cfi_offset %r15, -16
	movq	%rdx, %r14
	movq	%rsi, %r15
	movq	%rdi, %rbx
	callq	start_sequence
	movq	%rbx, (%rsp)
	movq	%r15, 8(%rsp)
	movq	%r14, 16(%rsp)
	leaq	(%rsp), %rdx
	movq	$84, %rdi
	movq	$3, %rsi
	callq	ix86_expand_binary_operator
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	addq	$32, %rsp
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp4326:
	.size	gen_iorhi3, .Ltmp4326-gen_iorhi3
	.cfi_endproc

	.globl	gen_iorqi3
	.align	16, 0x90
	.type	gen_iorqi3,@function
gen_iorqi3:                             # @gen_iorqi3
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp4331:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp4332:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp4333:
	.cfi_def_cfa_offset 32
	subq	$32, %rsp
.Ltmp4334:
	.cfi_def_cfa_offset 64
.Ltmp4335:
	.cfi_offset %rbx, -32
.Ltmp4336:
	.cfi_offset %r14, -24
.Ltmp4337:
	.cfi_offset %r15, -16
	movq	%rdx, %r14
	movq	%rsi, %r15
	movq	%rdi, %rbx
	callq	start_sequence
	movq	%rbx, (%rsp)
	movq	%r15, 8(%rsp)
	movq	%r14, 16(%rsp)
	leaq	(%rsp), %rdx
	movq	$84, %rdi
	movq	$2, %rsi
	callq	ix86_expand_binary_operator
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	addq	$32, %rsp
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp4338:
	.size	gen_iorqi3, .Ltmp4338-gen_iorqi3
	.cfi_endproc

	.globl	gen_xordi3
	.align	16, 0x90
	.type	gen_xordi3,@function
gen_xordi3:                             # @gen_xordi3
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp4343:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp4344:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp4345:
	.cfi_def_cfa_offset 32
	subq	$32, %rsp
.Ltmp4346:
	.cfi_def_cfa_offset 64
.Ltmp4347:
	.cfi_offset %rbx, -32
.Ltmp4348:
	.cfi_offset %r14, -24
.Ltmp4349:
	.cfi_offset %r15, -16
	movq	%rdx, %r14
	movq	%rsi, %r15
	movq	%rdi, %rbx
	callq	start_sequence
	movq	%rbx, (%rsp)
	movq	%r15, 8(%rsp)
	movq	%r14, 16(%rsp)
	leaq	(%rsp), %rdx
	movq	$85, %rdi
	movq	$5, %rsi
	callq	ix86_expand_binary_operator
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	addq	$32, %rsp
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp4350:
	.size	gen_xordi3, .Ltmp4350-gen_xordi3
	.cfi_endproc

	.globl	gen_xorsi3
	.align	16, 0x90
	.type	gen_xorsi3,@function
gen_xorsi3:                             # @gen_xorsi3
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp4355:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp4356:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp4357:
	.cfi_def_cfa_offset 32
	subq	$32, %rsp
.Ltmp4358:
	.cfi_def_cfa_offset 64
.Ltmp4359:
	.cfi_offset %rbx, -32
.Ltmp4360:
	.cfi_offset %r14, -24
.Ltmp4361:
	.cfi_offset %r15, -16
	movq	%rdx, %r14
	movq	%rsi, %r15
	movq	%rdi, %rbx
	callq	start_sequence
	movq	%rbx, (%rsp)
	movq	%r15, 8(%rsp)
	movq	%r14, 16(%rsp)
	leaq	(%rsp), %rdx
	movq	$85, %rdi
	movq	$4, %rsi
	callq	ix86_expand_binary_operator
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	addq	$32, %rsp
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp4362:
	.size	gen_xorsi3, .Ltmp4362-gen_xorsi3
	.cfi_endproc

	.globl	gen_xorhi3
	.align	16, 0x90
	.type	gen_xorhi3,@function
gen_xorhi3:                             # @gen_xorhi3
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp4367:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp4368:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp4369:
	.cfi_def_cfa_offset 32
	subq	$32, %rsp
.Ltmp4370:
	.cfi_def_cfa_offset 64
.Ltmp4371:
	.cfi_offset %rbx, -32
.Ltmp4372:
	.cfi_offset %r14, -24
.Ltmp4373:
	.cfi_offset %r15, -16
	movq	%rdx, %r14
	movq	%rsi, %r15
	movq	%rdi, %rbx
	callq	start_sequence
	movq	%rbx, (%rsp)
	movq	%r15, 8(%rsp)
	movq	%r14, 16(%rsp)
	leaq	(%rsp), %rdx
	movq	$85, %rdi
	movq	$3, %rsi
	callq	ix86_expand_binary_operator
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	addq	$32, %rsp
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp4374:
	.size	gen_xorhi3, .Ltmp4374-gen_xorhi3
	.cfi_endproc

	.globl	gen_xorqi3
	.align	16, 0x90
	.type	gen_xorqi3,@function
gen_xorqi3:                             # @gen_xorqi3
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp4379:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp4380:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp4381:
	.cfi_def_cfa_offset 32
	subq	$32, %rsp
.Ltmp4382:
	.cfi_def_cfa_offset 64
.Ltmp4383:
	.cfi_offset %rbx, -32
.Ltmp4384:
	.cfi_offset %r14, -24
.Ltmp4385:
	.cfi_offset %r15, -16
	movq	%rdx, %r14
	movq	%rsi, %r15
	movq	%rdi, %rbx
	callq	start_sequence
	movq	%rbx, (%rsp)
	movq	%r15, 8(%rsp)
	movq	%r14, 16(%rsp)
	leaq	(%rsp), %rdx
	movq	$85, %rdi
	movq	$2, %rsi
	callq	ix86_expand_binary_operator
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	addq	$32, %rsp
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp4386:
	.size	gen_xorqi3, .Ltmp4386-gen_xorqi3
	.cfi_endproc

	.globl	gen_xorqi_cc_ext_1
	.align	16, 0x90
	.type	gen_xorqi_cc_ext_1,@function
gen_xorqi_cc_ext_1:                     # @gen_xorqi_cc_ext_1
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp4393:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp4394:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp4395:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp4396:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp4397:
	.cfi_def_cfa_offset 48
.Ltmp4398:
	.cfi_offset %rbx, -48
.Ltmp4399:
	.cfi_offset %r12, -40
.Ltmp4400:
	.cfi_offset %r13, -32
.Ltmp4401:
	.cfi_offset %r14, -24
.Ltmp4402:
	.cfi_offset %r15, -16
	movq	%rdx, %r14
	movq	%rsi, %r13
	movq	%rdi, %r15
	movq	$55, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	%rax, %r12
	xorq	%rdi, %rdi
	movq	$8, %rsi
	callq	gen_rtx_CONST_INT
	movq	%rax, %rbx
	xorq	%rdi, %rdi
	movq	$8, %rsi
	callq	gen_rtx_CONST_INT
	movq	$133, %rdi
	movq	$4, %rsi
	movq	%r13, %rdx
	movq	%rbx, %rcx
	movq	%rax, %r8
	callq	gen_rtx_fmt_eee
	movq	$85, %rdi
	movq	$4, %rsi
	movq	%rax, %rdx
	movq	%r14, %rcx
	callq	gen_rtx_fmt_ee
	movq	const_int_rtx+512(%rip), %rcx
	movq	$74, %rdi
	movq	$55, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r12, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %r12
	xorq	%rdi, %rdi
	movq	$8, %rsi
	callq	gen_rtx_CONST_INT
	movq	%rax, %rbx
	xorq	%rdi, %rdi
	movq	$8, %rsi
	callq	gen_rtx_CONST_INT
	movq	$133, %rdi
	movq	$4, %rsi
	movq	%r15, %rdx
	movq	%rbx, %rcx
	movq	%rax, %r8
	callq	gen_rtx_fmt_eee
	movq	%rax, %r15
	xorq	%rdi, %rdi
	movq	$8, %rsi
	callq	gen_rtx_CONST_INT
	movq	%rax, %rbx
	xorq	%rdi, %rdi
	movq	$8, %rsi
	callq	gen_rtx_CONST_INT
	movq	$133, %rdi
	movq	$4, %rsi
	movq	%r13, %rdx
	movq	%rbx, %rcx
	movq	%rax, %r8
	callq	gen_rtx_fmt_eee
	movq	$85, %rdi
	movq	$4, %rsi
	movq	%rax, %rdx
	movq	%r14, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r15, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rcx
	movq	$2, %rdi
	xorq	%rax, %rax
	movq	%r12, %rsi
	movq	%rcx, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	gen_rtx_fmt_E  # TAILCALL
.Ltmp4403:
	.size	gen_xorqi_cc_ext_1, .Ltmp4403-gen_xorqi_cc_ext_1
	.cfi_endproc

	.globl	gen_negdi2
	.align	16, 0x90
	.type	gen_negdi2,@function
gen_negdi2:                             # @gen_negdi2
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp4407:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp4408:
	.cfi_def_cfa_offset 24
	subq	$24, %rsp
.Ltmp4409:
	.cfi_def_cfa_offset 48
.Ltmp4410:
	.cfi_offset %rbx, -24
.Ltmp4411:
	.cfi_offset %r14, -16
	movq	%rsi, %r14
	movq	%rdi, %rbx
	callq	start_sequence
	movq	%rbx, (%rsp)
	movq	%r14, 8(%rsp)
	leaq	(%rsp), %rdx
	movq	$77, %rdi
	movq	$5, %rsi
	callq	ix86_expand_unary_operator
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	addq	$24, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp4412:
	.size	gen_negdi2, .Ltmp4412-gen_negdi2
	.cfi_endproc

	.globl	gen_split_1028
	.align	16, 0x90
	.type	gen_split_1028,@function
gen_split_1028:                         # @gen_split_1028
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp4419:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp4420:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp4421:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp4422:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp4423:
	.cfi_def_cfa_offset 48
.Ltmp4424:
	.cfi_offset %rbx, -48
.Ltmp4425:
	.cfi_offset %r12, -40
.Ltmp4426:
	.cfi_offset %r13, -32
.Ltmp4427:
	.cfi_offset %r14, -24
.Ltmp4428:
	.cfi_offset %r15, -16
	movq	%rdi, %rbx
	callq	start_sequence
	leaq	8(%rbx), %r14
	leaq	16(%rbx), %rdx
	leaq	24(%rbx), %rcx
	movq	$1, %rsi
	movq	%r14, %rdi
	callq	split_di
	movq	$1, %rsi
	movq	%rbx, %rdi
	movq	%rbx, %rdx
	movq	%r14, %rcx
	callq	split_di
	movq	(%rbx), %r12
	movq	8(%rbx), %r15
	movq	16(%rbx), %r13
	movq	24(%rbx), %r14
	movq	$56, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	%rax, %rbx
	movq	$77, %rdi
	movq	$4, %rsi
	movq	%r13, %rdx
	callq	gen_rtx_fmt_e
	movq	const_int_rtx+512(%rip), %rcx
	movq	$74, %rdi
	movq	$56, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rbx
	movq	%r13, %rdi
	callq	copy_rtx
	movq	$77, %rdi
	movq	$4, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r12, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rcx
	movq	$2, %rdi
	xorq	%rax, %rax
	movq	%rbx, %rsi
	movq	%rcx, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_E
	movq	%rax, %rdi
	callq	emit
	movq	$52, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	const_int_rtx+512(%rip), %rcx
	movq	$111, %rdi
	movq	$4, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_ee
	movq	$75, %rdi
	movq	$4, %rsi
	movq	%rax, %rdx
	movq	%r14, %rcx
	callq	gen_rtx_fmt_ee
	movq	const_int_rtx+512(%rip), %rcx
	movq	$75, %rdi
	movq	$4, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r15, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rbx
	movq	$52, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rcx
	movq	$2, %rdi
	xorq	%rax, %rax
	movq	%rbx, %rsi
	movq	%rcx, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_E
	movq	%rax, %rdi
	callq	emit
	movq	%r15, %rdi
	callq	copy_rtx
	movq	%rax, %rbx
	movq	%r15, %rdi
	callq	copy_rtx
	movq	$77, %rdi
	movq	$4, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rbx
	movq	$52, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rcx
	movq	$2, %rdi
	xorq	%rax, %rax
	movq	%rbx, %rsi
	movq	%rcx, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_E
	movq	%rax, %rdi
	callq	emit
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp4429:
	.size	gen_split_1028, .Ltmp4429-gen_split_1028
	.cfi_endproc

	.globl	gen_negsi2
	.align	16, 0x90
	.type	gen_negsi2,@function
gen_negsi2:                             # @gen_negsi2
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp4433:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp4434:
	.cfi_def_cfa_offset 24
	subq	$24, %rsp
.Ltmp4435:
	.cfi_def_cfa_offset 48
.Ltmp4436:
	.cfi_offset %rbx, -24
.Ltmp4437:
	.cfi_offset %r14, -16
	movq	%rsi, %r14
	movq	%rdi, %rbx
	callq	start_sequence
	movq	%rbx, (%rsp)
	movq	%r14, 8(%rsp)
	leaq	(%rsp), %rdx
	movq	$77, %rdi
	movq	$4, %rsi
	callq	ix86_expand_unary_operator
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	addq	$24, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp4438:
	.size	gen_negsi2, .Ltmp4438-gen_negsi2
	.cfi_endproc

	.globl	gen_neghi2
	.align	16, 0x90
	.type	gen_neghi2,@function
gen_neghi2:                             # @gen_neghi2
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp4442:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp4443:
	.cfi_def_cfa_offset 24
	subq	$24, %rsp
.Ltmp4444:
	.cfi_def_cfa_offset 48
.Ltmp4445:
	.cfi_offset %rbx, -24
.Ltmp4446:
	.cfi_offset %r14, -16
	movq	%rsi, %r14
	movq	%rdi, %rbx
	callq	start_sequence
	movq	%rbx, (%rsp)
	movq	%r14, 8(%rsp)
	leaq	(%rsp), %rdx
	movq	$77, %rdi
	movq	$3, %rsi
	callq	ix86_expand_unary_operator
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	addq	$24, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp4447:
	.size	gen_neghi2, .Ltmp4447-gen_neghi2
	.cfi_endproc

	.globl	gen_negqi2
	.align	16, 0x90
	.type	gen_negqi2,@function
gen_negqi2:                             # @gen_negqi2
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp4451:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp4452:
	.cfi_def_cfa_offset 24
	subq	$24, %rsp
.Ltmp4453:
	.cfi_def_cfa_offset 48
.Ltmp4454:
	.cfi_offset %rbx, -24
.Ltmp4455:
	.cfi_offset %r14, -16
	movq	%rsi, %r14
	movq	%rdi, %rbx
	callq	start_sequence
	movq	%rbx, (%rsp)
	movq	%r14, 8(%rsp)
	leaq	(%rsp), %rdx
	movq	$77, %rdi
	movq	$2, %rsi
	callq	ix86_expand_unary_operator
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	addq	$24, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp4456:
	.size	gen_negqi2, .Ltmp4456-gen_negqi2
	.cfi_endproc

	.globl	gen_negsf2
	.align	16, 0x90
	.type	gen_negsf2,@function
gen_negsf2:                             # @gen_negsf2
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp4462:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp4463:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp4464:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp4465:
	.cfi_def_cfa_offset 40
	subq	$24, %rsp
.Ltmp4466:
	.cfi_def_cfa_offset 64
.Ltmp4467:
	.cfi_offset %rbx, -40
.Ltmp4468:
	.cfi_offset %r12, -32
.Ltmp4469:
	.cfi_offset %r14, -24
.Ltmp4470:
	.cfi_offset %r15, -16
	movq	%rsi, %r14
	movq	%rdi, %rbx
	callq	start_sequence
	movzbl	target_flags+2(%rip), %eax
	movq	%rbx, (%rsp)
	movq	%r14, 8(%rsp)
	testq	$5, %rax
	je	.LBB488_6
# BB#1:                                 # %if.then
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	memory_operand
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	testq	%rbx, %rax
	je	.LBB488_4
# BB#2:                                 # %land.lhs.true
	movq	(%rsp), %rdi
	movq	8(%rsp), %rsi
	callq	rtx_equal_p
	testq	%rbx, %rax
	je	.LBB488_4
# BB#3:                                 # %if.then7
	movq	(%rsp), %rbx
	movq	8(%rsp), %rdx
	movq	$77, %rdi
	movq	$15, %rsi
	callq	gen_rtx_fmt_e
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rbx
	movq	$52, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rcx
	movq	$2, %rdi
	xorq	%rax, %rax
	movq	%rbx, %rsi
	movq	%rcx, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_E
	movq	%rax, %rdi
	callq	emit_insn
	jmp	.LBB488_7
.LBB488_4:                              # %if.else
	movq	$15, %rdi
	callq	gen_reg_rtx
	movq	(%rsp), %r14
	movq	8(%rsp), %rsi
	movq	%rax, %r15
	movq	$15, %rdi
	callq	force_reg
	movq	%rax, 8(%rsp)
	movq	(%rsp), %rsi
	movq	$15, %rdi
	callq	force_reg
	movq	%rax, (%rsp)
	movabsq	$2147483648, %rdi       # imm = 0x80000000
	movq	$4, %rsi
	callq	trunc_int_for_mode
	xorq	%rdi, %rdi
	movq	%rax, %rsi
	callq	gen_rtx_CONST_INT
	movq	$15, %rdi
	movq	%rax, %rsi
	callq	gen_lowpart
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	emit_move_insn
	movq	(%rsp), %rbx
	movq	8(%rsp), %rdx
	movq	$77, %rdi
	movq	$15, %rsi
	callq	gen_rtx_fmt_e
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %r12
	movq	$48, %rdi
	xorq	%rsi, %rsi
	movq	%r15, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rbx
	movq	$52, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rcx
	movq	$3, %rdi
	xorq	%rax, %rax
	movq	%r12, %rsi
	movq	%rbx, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_E
	movq	%rax, %rdi
	callq	emit_insn
	movq	(%rsp), %rsi
	cmpq	%rsi, %r14
	je	.LBB488_7
# BB#5:                                 # %if.then30
	movq	%r14, %rdi
	callq	emit_move_insn
	jmp	.LBB488_7
.LBB488_6:                              # %if.end35
	leaq	(%rsp), %rdx
	movq	$77, %rdi
	movq	$15, %rsi
	callq	ix86_expand_unary_operator
.LBB488_7:                              # %return
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	addq	$24, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.Ltmp4471:
	.size	gen_negsf2, .Ltmp4471-gen_negsf2
	.cfi_endproc

	.globl	gen_split_1033
	.align	16, 0x90
	.type	gen_split_1033,@function
gen_split_1033:                         # @gen_split_1033
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp4475:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp4476:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp4477:
	.cfi_def_cfa_offset 32
.Ltmp4478:
	.cfi_offset %rbx, -24
.Ltmp4479:
	.cfi_offset %r14, -16
	movq	%rdi, %rbx
	callq	start_sequence
	movq	(%rbx), %r14
	movq	8(%rbx), %rdx
	movq	$77, %rdi
	movq	$15, %rsi
	callq	gen_rtx_fmt_e
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rbx
	movq	$52, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rcx
	movq	$2, %rdi
	xorq	%rax, %rax
	movq	%rbx, %rsi
	movq	%rcx, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_E
	movq	%rax, %rdi
	callq	emit
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp4480:
	.size	gen_split_1033, .Ltmp4480-gen_split_1033
	.cfi_endproc

	.globl	gen_split_1034
	.align	16, 0x90
	.type	gen_split_1034,@function
gen_split_1034:                         # @gen_split_1034
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp4484:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp4485:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp4486:
	.cfi_def_cfa_offset 32
.Ltmp4487:
	.cfi_offset %rbx, -24
.Ltmp4488:
	.cfi_offset %r14, -16
	movq	%rdi, %rbx
	callq	start_sequence
	movq	(%rbx), %r14
	movq	8(%rbx), %rdx
	movq	$77, %rdi
	movq	$15, %rsi
	callq	gen_rtx_fmt_e
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rbx
	movq	$52, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rcx
	movq	$2, %rdi
	xorq	%rax, %rax
	movq	%rbx, %rsi
	movq	%rcx, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_E
	movq	%rax, %rdi
	callq	emit
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp4489:
	.size	gen_split_1034, .Ltmp4489-gen_split_1034
	.cfi_endproc

	.globl	gen_split_1035
	.align	16, 0x90
	.type	gen_split_1035,@function
gen_split_1035:                         # @gen_split_1035
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp4495:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp4496:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp4497:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp4498:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp4499:
	.cfi_def_cfa_offset 48
.Ltmp4500:
	.cfi_offset %rbx, -40
.Ltmp4501:
	.cfi_offset %r12, -32
.Ltmp4502:
	.cfi_offset %r14, -24
.Ltmp4503:
	.cfi_offset %r15, -16
	movq	%rdi, %rbx
	callq	start_sequence
	movq	(%rbx), %rdi
	movq	16(%rbx), %rsi
	callq	operands_match_p
	movq	8(%rbx), %rcx
	movq	16(%rbx), %r15
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	testq	%rdx, %rax
	je	.LBB491_1
# BB#2:                                 # %if.then
	movq	%r15, 8(%rbx)
	movq	%rcx, 16(%rbx)
	movq	%rcx, %r14
	jmp	.LBB491_3
.LBB491_1:
	movq	%r15, %r14
	movq	%rcx, %r15
.LBB491_3:                              # %if.end
	movq	(%rbx), %rsi
	movq	$6, %rdi
	xorq	%rdx, %rdx
	callq	gen_rtx_SUBREG
	movq	%rax, %r12
	movq	$6, %rdi
	xorq	%rdx, %rdx
	movq	%r15, %rsi
	callq	gen_rtx_SUBREG
	movq	%rax, %rbx
	movq	$6, %rdi
	xorq	%rdx, %rdx
	movq	%r14, %rsi
	callq	gen_rtx_SUBREG
	movq	$85, %rdi
	movq	$6, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r12, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.Ltmp4504:
	.size	gen_split_1035, .Ltmp4504-gen_split_1035
	.cfi_endproc

	.globl	gen_split_1036
	.align	16, 0x90
	.type	gen_split_1036,@function
gen_split_1036:                         # @gen_split_1036
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp4508:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp4509:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp4510:
	.cfi_def_cfa_offset 32
.Ltmp4511:
	.cfi_offset %rbx, -24
.Ltmp4512:
	.cfi_offset %r14, -16
	movq	%rdi, %rbx
	callq	start_sequence
	movq	(%rbx), %r14
	movq	8(%rbx), %rdx
	movq	$77, %rdi
	movq	$15, %rsi
	callq	gen_rtx_fmt_e
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp4513:
	.size	gen_split_1036, .Ltmp4513-gen_split_1036
	.cfi_endproc

	.globl	gen_split_1037
	.align	16, 0x90
	.type	gen_split_1037,@function
gen_split_1037:                         # @gen_split_1037
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp4517:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp4518:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp4519:
	.cfi_def_cfa_offset 32
.Ltmp4520:
	.cfi_offset %rbx, -24
.Ltmp4521:
	.cfi_offset %r14, -16
	movq	%rdi, %rbx
	callq	start_sequence
	movabsq	$2147483648, %rdi       # imm = 0x80000000
	movq	$4, %rsi
	callq	trunc_int_for_mode
	xorq	%rdi, %rdi
	movq	%rax, %rsi
	callq	gen_rtx_CONST_INT
	movq	%rax, 8(%rbx)
	movq	(%rbx), %rax
	movl	8(%rax), %esi
	movq	$4, %rdi
	callq	gen_rtx_REG
	movq	%rax, %r14
	movq	%r14, (%rbx)
	movq	8(%rbx), %rbx
	movq	%r14, %rdi
	callq	copy_rtx
	movq	$85, %rdi
	movq	$4, %rsi
	movq	%rax, %rdx
	movq	%rbx, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rbx
	movq	$52, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rcx
	movq	$2, %rdi
	xorq	%rax, %rax
	movq	%rbx, %rsi
	movq	%rcx, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_E
	movq	%rax, %rdi
	callq	emit
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp4522:
	.size	gen_split_1037, .Ltmp4522-gen_split_1037
	.cfi_endproc

	.globl	gen_split_1038
	.align	16, 0x90
	.type	gen_split_1038,@function
gen_split_1038:                         # @gen_split_1038
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp4526:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp4527:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp4528:
	.cfi_def_cfa_offset 32
.Ltmp4529:
	.cfi_offset %rbx, -24
.Ltmp4530:
	.cfi_offset %r14, -16
	movq	%rdi, %rbx
	callq	start_sequence
	movq	8(%rbx), %rax
	movzbl	2(%rax), %eax
	movzbl	mode_size(%rax), %eax
	movq	(%rbx), %rdi
	cmpq	$11, %rax
	movq	$9, %rdx
	ja	.LBB494_2
# BB#1:                                 # %entry
	decq	%rax
	movq	%rax, %rdx
.LBB494_2:                              # %entry
	movq	$2, %rsi
	movq	$1, %rcx
	movq	$1, %r8
	callq	adjust_address_1
	movq	%rax, (%rbx)
	movq	$128, %rdi
	movq	$2, %rsi
	callq	trunc_int_for_mode
	xorq	%rdi, %rdi
	movq	%rax, %rsi
	callq	gen_rtx_CONST_INT
	movq	%rax, %r14
	movq	%r14, 8(%rbx)
	movq	(%rbx), %rbx
	movq	%rbx, %rdi
	callq	copy_rtx
	movq	$85, %rdi
	movq	$2, %rsi
	movq	%rax, %rdx
	movq	%r14, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rbx
	movq	$52, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rcx
	movq	$2, %rdi
	xorq	%rax, %rax
	movq	%rbx, %rsi
	movq	%rcx, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_E
	movq	%rax, %rdi
	callq	emit
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp4531:
	.size	gen_split_1038, .Ltmp4531-gen_split_1038
	.cfi_endproc

	.globl	gen_negdf2
	.align	16, 0x90
	.type	gen_negdf2,@function
gen_negdf2:                             # @gen_negdf2
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp4537:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp4538:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp4539:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp4540:
	.cfi_def_cfa_offset 40
	subq	$24, %rsp
.Ltmp4541:
	.cfi_def_cfa_offset 64
.Ltmp4542:
	.cfi_offset %rbx, -40
.Ltmp4543:
	.cfi_offset %r12, -32
.Ltmp4544:
	.cfi_offset %r14, -24
.Ltmp4545:
	.cfi_offset %r15, -16
	movq	%rsi, %r14
	movq	%rdi, %rbx
	callq	start_sequence
	movzbl	target_flags+2(%rip), %eax
	movq	%rbx, (%rsp)
	movq	%r14, 8(%rsp)
	testq	$4, %rax
	je	.LBB495_6
# BB#1:                                 # %if.then
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	memory_operand
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	testq	%rbx, %rax
	je	.LBB495_4
# BB#2:                                 # %land.lhs.true
	movq	(%rsp), %rdi
	movq	8(%rsp), %rsi
	callq	rtx_equal_p
	testq	%rbx, %rax
	je	.LBB495_4
# BB#3:                                 # %if.then7
	movq	(%rsp), %rbx
	movq	8(%rsp), %rdx
	movq	$77, %rdi
	movq	$16, %rsi
	callq	gen_rtx_fmt_e
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rbx
	movq	$52, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rcx
	movq	$2, %rdi
	xorq	%rax, %rax
	movq	%rbx, %rsi
	movq	%rcx, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_E
	movq	%rax, %rdi
	callq	emit_insn
	jmp	.LBB495_7
.LBB495_4:                              # %if.else
	movq	$16, %rdi
	callq	gen_reg_rtx
	movq	%rax, %r15
	movabsq	$-9223372036854775808, %rdi # imm = 0x8000000000000000
	movq	$5, %rsi
	callq	trunc_int_for_mode
	xorq	%rdi, %rdi
	movq	%rax, %rsi
	callq	gen_rtx_CONST_INT
	movq	(%rsp), %r14
	movq	8(%rsp), %rsi
	movq	%rax, %rbx
	movq	$16, %rdi
	callq	force_reg
	movq	%rax, 8(%rsp)
	movq	(%rsp), %rsi
	movq	$16, %rdi
	callq	force_reg
	movq	%rax, (%rsp)
	movq	$16, %rdi
	movq	%rbx, %rsi
	callq	gen_lowpart
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	emit_move_insn
	movq	(%rsp), %rbx
	movq	8(%rsp), %rdx
	movq	$77, %rdi
	movq	$16, %rsi
	callq	gen_rtx_fmt_e
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %r12
	movq	$48, %rdi
	xorq	%rsi, %rsi
	movq	%r15, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rbx
	movq	$52, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rcx
	movq	$3, %rdi
	xorq	%rax, %rax
	movq	%r12, %rsi
	movq	%rbx, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_E
	movq	%rax, %rdi
	callq	emit_insn
	movq	(%rsp), %rsi
	cmpq	%rsi, %r14
	je	.LBB495_7
# BB#5:                                 # %if.then30
	movq	%r14, %rdi
	callq	emit_move_insn
	jmp	.LBB495_7
.LBB495_6:                              # %if.end35
	leaq	(%rsp), %rdx
	movq	$77, %rdi
	movq	$16, %rsi
	callq	ix86_expand_unary_operator
.LBB495_7:                              # %return
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	addq	$24, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.Ltmp4546:
	.size	gen_negdf2, .Ltmp4546-gen_negdf2
	.cfi_endproc

	.globl	gen_split_1040
	.align	16, 0x90
	.type	gen_split_1040,@function
gen_split_1040:                         # @gen_split_1040
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp4550:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp4551:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp4552:
	.cfi_def_cfa_offset 32
.Ltmp4553:
	.cfi_offset %rbx, -24
.Ltmp4554:
	.cfi_offset %r14, -16
	movq	%rdi, %rbx
	callq	start_sequence
	movq	(%rbx), %r14
	movq	8(%rbx), %rdx
	movq	$77, %rdi
	movq	$16, %rsi
	callq	gen_rtx_fmt_e
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rbx
	movq	$52, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rcx
	movq	$2, %rdi
	xorq	%rax, %rax
	movq	%rbx, %rsi
	movq	%rcx, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_E
	movq	%rax, %rdi
	callq	emit
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp4555:
	.size	gen_split_1040, .Ltmp4555-gen_split_1040
	.cfi_endproc

	.globl	gen_split_1041
	.align	16, 0x90
	.type	gen_split_1041,@function
gen_split_1041:                         # @gen_split_1041
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp4559:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp4560:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp4561:
	.cfi_def_cfa_offset 32
.Ltmp4562:
	.cfi_offset %rbx, -24
.Ltmp4563:
	.cfi_offset %r14, -16
	movq	%rdi, %rbx
	callq	start_sequence
	movq	(%rbx), %r14
	movq	8(%rbx), %rdx
	movq	$77, %rdi
	movq	$16, %rsi
	callq	gen_rtx_fmt_e
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rbx
	movq	$52, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rcx
	movq	$2, %rdi
	xorq	%rax, %rax
	movq	%rbx, %rsi
	movq	%rcx, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_E
	movq	%rax, %rdi
	callq	emit
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp4564:
	.size	gen_split_1041, .Ltmp4564-gen_split_1041
	.cfi_endproc

	.globl	gen_split_1042
	.align	16, 0x90
	.type	gen_split_1042,@function
gen_split_1042:                         # @gen_split_1042
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp4568:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp4569:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp4570:
	.cfi_def_cfa_offset 32
.Ltmp4571:
	.cfi_offset %rbx, -24
.Ltmp4572:
	.cfi_offset %r14, -16
	movq	%rdi, %rbx
	callq	start_sequence
	movq	(%rbx), %rsi
	movq	$5, %rdi
	callq	gen_lowpart
	movq	%rax, (%rbx)
	movq	8(%rbx), %rsi
	movq	$5, %rdi
	callq	gen_lowpart
	movq	%rax, 8(%rbx)
	movq	16(%rbx), %rsi
	movq	$5, %rdi
	callq	gen_lowpart
	movq	%rax, 16(%rbx)
	movq	(%rbx), %r14
	movq	8(%rbx), %rdx
	movq	$85, %rdi
	movq	$5, %rsi
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rbx
	movq	$52, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rcx
	movq	$2, %rdi
	xorq	%rax, %rax
	movq	%rbx, %rsi
	movq	%rcx, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_E
	movq	%rax, %rdi
	callq	emit
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp4573:
	.size	gen_split_1042, .Ltmp4573-gen_split_1042
	.cfi_endproc

	.globl	gen_split_1043
	.align	16, 0x90
	.type	gen_split_1043,@function
gen_split_1043:                         # @gen_split_1043
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp4579:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp4580:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp4581:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp4582:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp4583:
	.cfi_def_cfa_offset 48
.Ltmp4584:
	.cfi_offset %rbx, -40
.Ltmp4585:
	.cfi_offset %r12, -32
.Ltmp4586:
	.cfi_offset %r14, -24
.Ltmp4587:
	.cfi_offset %r15, -16
	movq	%rdi, %rbx
	callq	start_sequence
	movq	(%rbx), %rdi
	movq	16(%rbx), %rsi
	callq	operands_match_p
	movq	8(%rbx), %rcx
	movq	16(%rbx), %r15
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	testq	%rdx, %rax
	je	.LBB499_1
# BB#2:                                 # %if.then
	movq	%r15, 8(%rbx)
	movq	%rcx, 16(%rbx)
	movq	%rcx, %r14
	jmp	.LBB499_3
.LBB499_1:
	movq	%r15, %r14
	movq	%rcx, %r15
.LBB499_3:                              # %if.end
	movq	(%rbx), %rsi
	movq	$6, %rdi
	xorq	%rdx, %rdx
	callq	gen_rtx_SUBREG
	movq	%rax, %r12
	movq	$6, %rdi
	xorq	%rdx, %rdx
	movq	%r15, %rsi
	callq	gen_rtx_SUBREG
	movq	%rax, %rbx
	movq	$6, %rdi
	xorq	%rdx, %rdx
	movq	%r14, %rsi
	callq	gen_rtx_SUBREG
	movq	$85, %rdi
	movq	$6, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r12, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.Ltmp4588:
	.size	gen_split_1043, .Ltmp4588-gen_split_1043
	.cfi_endproc

	.globl	gen_split_1044
	.align	16, 0x90
	.type	gen_split_1044,@function
gen_split_1044:                         # @gen_split_1044
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp4592:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp4593:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp4594:
	.cfi_def_cfa_offset 32
.Ltmp4595:
	.cfi_offset %rbx, -24
.Ltmp4596:
	.cfi_offset %r14, -16
	movq	%rdi, %rbx
	callq	start_sequence
	movq	(%rbx), %r14
	movq	8(%rbx), %rdx
	movq	$77, %rdi
	movq	$16, %rsi
	callq	gen_rtx_fmt_e
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp4597:
	.size	gen_split_1044, .Ltmp4597-gen_split_1044
	.cfi_endproc

	.globl	gen_split_1045
	.align	16, 0x90
	.type	gen_split_1045,@function
gen_split_1045:                         # @gen_split_1045
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp4601:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp4602:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp4603:
	.cfi_def_cfa_offset 32
.Ltmp4604:
	.cfi_offset %rbx, -24
.Ltmp4605:
	.cfi_offset %r14, -16
	movq	%rdi, %rbx
	callq	start_sequence
	movabsq	$2147483648, %rdi       # imm = 0x80000000
	movq	$4, %rsi
	callq	trunc_int_for_mode
	xorq	%rdi, %rdi
	movq	%rax, %rsi
	callq	gen_rtx_CONST_INT
	movq	%rax, 32(%rbx)
	leaq	16(%rbx), %rdx
	leaq	24(%rbx), %rcx
	movq	$1, %rsi
	movq	%rbx, %rdi
	callq	split_di
	movq	24(%rbx), %r14
	movq	32(%rbx), %rbx
	movq	%r14, %rdi
	callq	copy_rtx
	movq	$85, %rdi
	movq	$4, %rsi
	movq	%rax, %rdx
	movq	%rbx, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rbx
	movq	$52, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rcx
	movq	$2, %rdi
	xorq	%rax, %rax
	movq	%rbx, %rsi
	movq	%rcx, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_E
	movq	%rax, %rdi
	callq	emit
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp4606:
	.size	gen_split_1045, .Ltmp4606-gen_split_1045
	.cfi_endproc

	.globl	gen_negxf2
	.align	16, 0x90
	.type	gen_negxf2,@function
gen_negxf2:                             # @gen_negxf2
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp4610:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp4611:
	.cfi_def_cfa_offset 24
	subq	$24, %rsp
.Ltmp4612:
	.cfi_def_cfa_offset 48
.Ltmp4613:
	.cfi_offset %rbx, -24
.Ltmp4614:
	.cfi_offset %r14, -16
	movq	%rsi, %r14
	movq	%rdi, %rbx
	callq	start_sequence
	movq	%rbx, (%rsp)
	movq	%r14, 8(%rsp)
	leaq	(%rsp), %rdx
	movq	$77, %rdi
	movq	$17, %rsi
	callq	ix86_expand_unary_operator
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	addq	$24, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp4615:
	.size	gen_negxf2, .Ltmp4615-gen_negxf2
	.cfi_endproc

	.globl	gen_negtf2
	.align	16, 0x90
	.type	gen_negtf2,@function
gen_negtf2:                             # @gen_negtf2
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp4619:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp4620:
	.cfi_def_cfa_offset 24
	subq	$24, %rsp
.Ltmp4621:
	.cfi_def_cfa_offset 48
.Ltmp4622:
	.cfi_offset %rbx, -24
.Ltmp4623:
	.cfi_offset %r14, -16
	movq	%rsi, %r14
	movq	%rdi, %rbx
	callq	start_sequence
	movq	%rbx, (%rsp)
	movq	%r14, 8(%rsp)
	leaq	(%rsp), %rdx
	movq	$77, %rdi
	movq	$18, %rsi
	callq	ix86_expand_unary_operator
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	addq	$24, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp4624:
	.size	gen_negtf2, .Ltmp4624-gen_negtf2
	.cfi_endproc

	.globl	gen_split_1048
	.align	16, 0x90
	.type	gen_split_1048,@function
gen_split_1048:                         # @gen_split_1048
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp4628:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp4629:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp4630:
	.cfi_def_cfa_offset 32
.Ltmp4631:
	.cfi_offset %rbx, -24
.Ltmp4632:
	.cfi_offset %r14, -16
	movq	%rdi, %rbx
	callq	start_sequence
	movq	(%rbx), %r14
	movq	8(%rbx), %rdx
	movq	$77, %rdi
	movq	$17, %rsi
	callq	gen_rtx_fmt_e
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp4633:
	.size	gen_split_1048, .Ltmp4633-gen_split_1048
	.cfi_endproc

	.globl	gen_split_1049
	.align	16, 0x90
	.type	gen_split_1049,@function
gen_split_1049:                         # @gen_split_1049
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp4637:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp4638:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp4639:
	.cfi_def_cfa_offset 32
.Ltmp4640:
	.cfi_offset %rbx, -24
.Ltmp4641:
	.cfi_offset %r14, -16
	movq	%rdi, %r14
	callq	start_sequence
	xorq	%rdi, %rdi
	movq	$32768, %rsi            # imm = 0x8000
	callq	gen_rtx_CONST_INT
	movq	%rax, 8(%r14)
	movq	(%r14), %rdi
	callq	true_regnum
	movl	target_flags(%rip), %edx
	movq	$25, %rcx
	shrq	%cl, %rdx
	andq	$1, %rdx
	negq	%rdx
	leaq	2(%rax,%rdx), %rsi
	movq	$4, %rdi
	callq	gen_rtx_REG
	movq	%rax, %rbx
	movq	%rbx, (%r14)
	movq	8(%r14), %r14
	movq	%rbx, %rdi
	callq	copy_rtx
	movq	$85, %rdi
	movq	$4, %rsi
	movq	%rax, %rdx
	movq	%r14, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rbx
	movq	$52, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rcx
	movq	$2, %rdi
	xorq	%rax, %rax
	movq	%rbx, %rsi
	movq	%rcx, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_E
	movq	%rax, %rdi
	callq	emit
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp4642:
	.size	gen_split_1049, .Ltmp4642-gen_split_1049
	.cfi_endproc

	.globl	gen_split_1050
	.align	16, 0x90
	.type	gen_split_1050,@function
gen_split_1050:                         # @gen_split_1050
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp4646:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp4647:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp4648:
	.cfi_def_cfa_offset 32
.Ltmp4649:
	.cfi_offset %rbx, -24
.Ltmp4650:
	.cfi_offset %r14, -16
	movq	%rdi, %rbx
	callq	start_sequence
	movq	(%rbx), %r14
	movq	8(%rbx), %rdx
	movq	$77, %rdi
	movq	$18, %rsi
	callq	gen_rtx_fmt_e
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp4651:
	.size	gen_split_1050, .Ltmp4651-gen_split_1050
	.cfi_endproc

	.globl	gen_split_1051
	.align	16, 0x90
	.type	gen_split_1051,@function
gen_split_1051:                         # @gen_split_1051
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp4655:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp4656:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp4657:
	.cfi_def_cfa_offset 32
.Ltmp4658:
	.cfi_offset %rbx, -24
.Ltmp4659:
	.cfi_offset %r14, -16
	movq	%rdi, %r14
	callq	start_sequence
	xorq	%rdi, %rdi
	movq	$32768, %rsi            # imm = 0x8000
	callq	gen_rtx_CONST_INT
	movq	%rax, 8(%r14)
	movq	(%r14), %rdi
	callq	true_regnum
	movl	target_flags(%rip), %edx
	movq	$25, %rcx
	shrq	%cl, %rdx
	andq	$1, %rdx
	negq	%rdx
	leaq	2(%rax,%rdx), %rsi
	movq	$4, %rdi
	callq	gen_rtx_REG
	movq	%rax, %rbx
	movq	%rbx, (%r14)
	movq	8(%r14), %r14
	movq	%rbx, %rdi
	callq	copy_rtx
	movq	$85, %rdi
	movq	$4, %rsi
	movq	%rax, %rdx
	movq	%r14, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rbx
	movq	$52, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rcx
	movq	$2, %rdi
	xorq	%rax, %rax
	movq	%rbx, %rsi
	movq	%rcx, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_E
	movq	%rax, %rdi
	callq	emit
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp4660:
	.size	gen_split_1051, .Ltmp4660-gen_split_1051
	.cfi_endproc

	.globl	gen_abssf2
	.align	16, 0x90
	.type	gen_abssf2,@function
gen_abssf2:                             # @gen_abssf2
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp4666:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp4667:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp4668:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp4669:
	.cfi_def_cfa_offset 40
	subq	$24, %rsp
.Ltmp4670:
	.cfi_def_cfa_offset 64
.Ltmp4671:
	.cfi_offset %rbx, -40
.Ltmp4672:
	.cfi_offset %r12, -32
.Ltmp4673:
	.cfi_offset %r14, -24
.Ltmp4674:
	.cfi_offset %r15, -16
	movq	%rsi, %r14
	movq	%rdi, %rbx
	callq	start_sequence
	movzbl	target_flags+2(%rip), %eax
	movq	%rbx, (%rsp)
	movq	%r14, 8(%rsp)
	testq	$5, %rax
	je	.LBB508_6
# BB#1:                                 # %if.then
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	memory_operand
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	testq	%rbx, %rax
	je	.LBB508_4
# BB#2:                                 # %land.lhs.true
	movq	(%rsp), %rdi
	movq	8(%rsp), %rsi
	callq	rtx_equal_p
	testq	%rbx, %rax
	je	.LBB508_4
# BB#3:                                 # %if.then7
	movq	(%rsp), %rbx
	movq	8(%rsp), %rdx
	movq	$129, %rdi
	movq	$15, %rsi
	callq	gen_rtx_fmt_e
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rbx
	movq	$52, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rcx
	movq	$2, %rdi
	xorq	%rax, %rax
	movq	%rbx, %rsi
	movq	%rcx, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_E
	movq	%rax, %rdi
	callq	emit_insn
	jmp	.LBB508_7
.LBB508_4:                              # %if.else
	movq	$15, %rdi
	callq	gen_reg_rtx
	movq	(%rsp), %r14
	movq	8(%rsp), %rsi
	movq	%rax, %r15
	movq	$15, %rdi
	callq	force_reg
	movq	%rax, 8(%rsp)
	movq	(%rsp), %rsi
	movq	$15, %rdi
	callq	force_reg
	movq	%rax, (%rsp)
	movabsq	$2147483648, %rdi       # imm = 0x80000000
	movq	$4, %rsi
	callq	trunc_int_for_mode
	xorq	%rdi, %rdi
	movq	%rax, %rsi
	callq	gen_rtx_CONST_INT
	movq	$15, %rdi
	movq	%rax, %rsi
	callq	gen_lowpart
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	emit_move_insn
	movq	(%rsp), %rbx
	movq	8(%rsp), %rdx
	movq	$129, %rdi
	movq	$15, %rsi
	callq	gen_rtx_fmt_e
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %r12
	movq	$48, %rdi
	xorq	%rsi, %rsi
	movq	%r15, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rbx
	movq	$52, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rcx
	movq	$3, %rdi
	xorq	%rax, %rax
	movq	%r12, %rsi
	movq	%rbx, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_E
	movq	%rax, %rdi
	callq	emit_insn
	movq	(%rsp), %rsi
	cmpq	%rsi, %r14
	je	.LBB508_7
# BB#5:                                 # %if.then30
	movq	%r14, %rdi
	callq	emit_move_insn
	jmp	.LBB508_7
.LBB508_6:                              # %if.end35
	leaq	(%rsp), %rdx
	movq	$129, %rdi
	movq	$15, %rsi
	callq	ix86_expand_unary_operator
.LBB508_7:                              # %return
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	addq	$24, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.Ltmp4675:
	.size	gen_abssf2, .Ltmp4675-gen_abssf2
	.cfi_endproc

	.globl	gen_split_1053
	.align	16, 0x90
	.type	gen_split_1053,@function
gen_split_1053:                         # @gen_split_1053
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp4679:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp4680:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp4681:
	.cfi_def_cfa_offset 32
.Ltmp4682:
	.cfi_offset %rbx, -24
.Ltmp4683:
	.cfi_offset %r14, -16
	movq	%rdi, %rbx
	callq	start_sequence
	movq	(%rbx), %r14
	movq	8(%rbx), %rdx
	movq	$129, %rdi
	movq	$15, %rsi
	callq	gen_rtx_fmt_e
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rbx
	movq	$52, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rcx
	movq	$2, %rdi
	xorq	%rax, %rax
	movq	%rbx, %rsi
	movq	%rcx, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_E
	movq	%rax, %rdi
	callq	emit
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp4684:
	.size	gen_split_1053, .Ltmp4684-gen_split_1053
	.cfi_endproc

	.globl	gen_split_1054
	.align	16, 0x90
	.type	gen_split_1054,@function
gen_split_1054:                         # @gen_split_1054
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp4688:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp4689:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp4690:
	.cfi_def_cfa_offset 32
.Ltmp4691:
	.cfi_offset %rbx, -24
.Ltmp4692:
	.cfi_offset %r14, -16
	movq	%rdi, %rbx
	callq	start_sequence
	movq	(%rbx), %r14
	movq	8(%rbx), %rdx
	movq	$129, %rdi
	movq	$15, %rsi
	callq	gen_rtx_fmt_e
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rbx
	movq	$52, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rcx
	movq	$2, %rdi
	xorq	%rax, %rax
	movq	%rbx, %rsi
	movq	%rcx, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_E
	movq	%rax, %rdi
	callq	emit
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp4693:
	.size	gen_split_1054, .Ltmp4693-gen_split_1054
	.cfi_endproc

	.globl	gen_split_1055
	.align	16, 0x90
	.type	gen_split_1055,@function
gen_split_1055:                         # @gen_split_1055
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp4698:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp4699:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp4700:
	.cfi_def_cfa_offset 32
.Ltmp4701:
	.cfi_offset %rbx, -32
.Ltmp4702:
	.cfi_offset %r14, -24
.Ltmp4703:
	.cfi_offset %r15, -16
	movq	%rdi, %rbx
	callq	start_sequence
	movq	(%rbx), %rsi
	movq	8(%rbx), %r14
	movq	16(%rbx), %rbx
	movq	$6, %rdi
	xorq	%rdx, %rdx
	callq	gen_rtx_SUBREG
	movq	%rax, %r15
	movq	$6, %rdi
	xorq	%rdx, %rdx
	movq	%rbx, %rsi
	callq	gen_rtx_SUBREG
	movq	$86, %rdi
	movq	$6, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rbx
	movq	$6, %rdi
	xorq	%rdx, %rdx
	movq	%r14, %rsi
	callq	gen_rtx_SUBREG
	movq	$83, %rdi
	movq	$6, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r15, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp4704:
	.size	gen_split_1055, .Ltmp4704-gen_split_1055
	.cfi_endproc

	.globl	gen_split_1056
	.align	16, 0x90
	.type	gen_split_1056,@function
gen_split_1056:                         # @gen_split_1056
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp4708:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp4709:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp4710:
	.cfi_def_cfa_offset 32
.Ltmp4711:
	.cfi_offset %rbx, -24
.Ltmp4712:
	.cfi_offset %r14, -16
	movq	%rdi, %rbx
	callq	start_sequence
	movq	(%rbx), %r14
	movq	8(%rbx), %rdx
	movq	$129, %rdi
	movq	$15, %rsi
	callq	gen_rtx_fmt_e
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp4713:
	.size	gen_split_1056, .Ltmp4713-gen_split_1056
	.cfi_endproc

	.globl	gen_split_1057
	.align	16, 0x90
	.type	gen_split_1057,@function
gen_split_1057:                         # @gen_split_1057
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp4717:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp4718:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp4719:
	.cfi_def_cfa_offset 32
.Ltmp4720:
	.cfi_offset %rbx, -24
.Ltmp4721:
	.cfi_offset %r14, -16
	movq	%rdi, %rbx
	callq	start_sequence
	movq	$2147483647, %rdi       # imm = 0x7FFFFFFF
	movq	$4, %rsi
	callq	trunc_int_for_mode
	xorq	%rdi, %rdi
	movq	%rax, %rsi
	callq	gen_rtx_CONST_INT
	movq	%rax, 8(%rbx)
	movq	(%rbx), %rax
	movl	8(%rax), %esi
	movq	$4, %rdi
	callq	gen_rtx_REG
	movq	%rax, %r14
	movq	%r14, (%rbx)
	movq	8(%rbx), %rbx
	movq	%r14, %rdi
	callq	copy_rtx
	movq	$83, %rdi
	movq	$4, %rsi
	movq	%rax, %rdx
	movq	%rbx, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rbx
	movq	$52, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rcx
	movq	$2, %rdi
	xorq	%rax, %rax
	movq	%rbx, %rsi
	movq	%rcx, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_E
	movq	%rax, %rdi
	callq	emit
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp4722:
	.size	gen_split_1057, .Ltmp4722-gen_split_1057
	.cfi_endproc

	.globl	gen_split_1058
	.align	16, 0x90
	.type	gen_split_1058,@function
gen_split_1058:                         # @gen_split_1058
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp4726:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp4727:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp4728:
	.cfi_def_cfa_offset 32
.Ltmp4729:
	.cfi_offset %rbx, -24
.Ltmp4730:
	.cfi_offset %r14, -16
	movq	%rdi, %rbx
	callq	start_sequence
	movq	8(%rbx), %rax
	movzbl	2(%rax), %eax
	movzbl	mode_size(%rax), %eax
	movq	(%rbx), %rdi
	cmpq	$11, %rax
	movq	$9, %rdx
	ja	.LBB514_2
# BB#1:                                 # %entry
	decq	%rax
	movq	%rax, %rdx
.LBB514_2:                              # %entry
	movq	$2, %rsi
	movq	$1, %rcx
	movq	$1, %r8
	callq	adjust_address_1
	movq	%rax, (%rbx)
	movq	$-129, %rdi
	movq	$2, %rsi
	callq	trunc_int_for_mode
	xorq	%rdi, %rdi
	movq	%rax, %rsi
	callq	gen_rtx_CONST_INT
	movq	%rax, %r14
	movq	%r14, 8(%rbx)
	movq	(%rbx), %rbx
	movq	%rbx, %rdi
	callq	copy_rtx
	movq	$83, %rdi
	movq	$2, %rsi
	movq	%rax, %rdx
	movq	%r14, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rbx
	movq	$52, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rcx
	movq	$2, %rdi
	xorq	%rax, %rax
	movq	%rbx, %rsi
	movq	%rcx, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_E
	movq	%rax, %rdi
	callq	emit
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp4731:
	.size	gen_split_1058, .Ltmp4731-gen_split_1058
	.cfi_endproc

	.globl	gen_absdf2
	.align	16, 0x90
	.type	gen_absdf2,@function
gen_absdf2:                             # @gen_absdf2
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp4737:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp4738:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp4739:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp4740:
	.cfi_def_cfa_offset 40
	subq	$24, %rsp
.Ltmp4741:
	.cfi_def_cfa_offset 64
.Ltmp4742:
	.cfi_offset %rbx, -40
.Ltmp4743:
	.cfi_offset %r12, -32
.Ltmp4744:
	.cfi_offset %r14, -24
.Ltmp4745:
	.cfi_offset %r15, -16
	movq	%rsi, %r14
	movq	%rdi, %rbx
	callq	start_sequence
	movzbl	target_flags+2(%rip), %eax
	movq	%rbx, (%rsp)
	movq	%r14, 8(%rsp)
	testq	$4, %rax
	je	.LBB515_6
# BB#1:                                 # %if.then
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	memory_operand
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	testq	%rbx, %rax
	je	.LBB515_4
# BB#2:                                 # %land.lhs.true
	movq	(%rsp), %rdi
	movq	8(%rsp), %rsi
	callq	rtx_equal_p
	testq	%rbx, %rax
	je	.LBB515_4
# BB#3:                                 # %if.then7
	movq	(%rsp), %rbx
	movq	8(%rsp), %rdx
	movq	$129, %rdi
	movq	$16, %rsi
	callq	gen_rtx_fmt_e
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rbx
	movq	$52, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rcx
	movq	$2, %rdi
	xorq	%rax, %rax
	movq	%rbx, %rsi
	movq	%rcx, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_E
	movq	%rax, %rdi
	callq	emit_insn
	jmp	.LBB515_7
.LBB515_4:                              # %if.else
	movq	$16, %rdi
	callq	gen_reg_rtx
	movq	%rax, %r15
	movabsq	$-9223372036854775808, %rdi # imm = 0x8000000000000000
	movq	$5, %rsi
	callq	trunc_int_for_mode
	xorq	%rdi, %rdi
	movq	%rax, %rsi
	callq	gen_rtx_CONST_INT
	movq	(%rsp), %r14
	movq	8(%rsp), %rsi
	movq	%rax, %rbx
	movq	$16, %rdi
	callq	force_reg
	movq	%rax, 8(%rsp)
	movq	(%rsp), %rsi
	movq	$16, %rdi
	callq	force_reg
	movq	%rax, (%rsp)
	movq	$16, %rdi
	movq	%rbx, %rsi
	callq	gen_lowpart
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	emit_move_insn
	movq	(%rsp), %rbx
	movq	8(%rsp), %rdx
	movq	$129, %rdi
	movq	$16, %rsi
	callq	gen_rtx_fmt_e
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %r12
	movq	$48, %rdi
	xorq	%rsi, %rsi
	movq	%r15, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rbx
	movq	$52, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rcx
	movq	$3, %rdi
	xorq	%rax, %rax
	movq	%r12, %rsi
	movq	%rbx, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_E
	movq	%rax, %rdi
	callq	emit_insn
	movq	(%rsp), %rsi
	cmpq	%rsi, %r14
	je	.LBB515_7
# BB#5:                                 # %if.then30
	movq	%r14, %rdi
	callq	emit_move_insn
	jmp	.LBB515_7
.LBB515_6:                              # %if.end35
	leaq	(%rsp), %rdx
	movq	$129, %rdi
	movq	$16, %rsi
	callq	ix86_expand_unary_operator
.LBB515_7:                              # %return
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	addq	$24, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.Ltmp4746:
	.size	gen_absdf2, .Ltmp4746-gen_absdf2
	.cfi_endproc

	.globl	gen_split_1060
	.align	16, 0x90
	.type	gen_split_1060,@function
gen_split_1060:                         # @gen_split_1060
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp4750:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp4751:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp4752:
	.cfi_def_cfa_offset 32
.Ltmp4753:
	.cfi_offset %rbx, -24
.Ltmp4754:
	.cfi_offset %r14, -16
	movq	%rdi, %rbx
	callq	start_sequence
	movq	(%rbx), %r14
	movq	8(%rbx), %rdx
	movq	$129, %rdi
	movq	$16, %rsi
	callq	gen_rtx_fmt_e
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rbx
	movq	$52, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rcx
	movq	$2, %rdi
	xorq	%rax, %rax
	movq	%rbx, %rsi
	movq	%rcx, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_E
	movq	%rax, %rdi
	callq	emit
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp4755:
	.size	gen_split_1060, .Ltmp4755-gen_split_1060
	.cfi_endproc

	.globl	gen_split_1061
	.align	16, 0x90
	.type	gen_split_1061,@function
gen_split_1061:                         # @gen_split_1061
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp4759:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp4760:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp4761:
	.cfi_def_cfa_offset 32
.Ltmp4762:
	.cfi_offset %rbx, -24
.Ltmp4763:
	.cfi_offset %r14, -16
	movq	%rdi, %rbx
	callq	start_sequence
	movq	(%rbx), %r14
	movq	8(%rbx), %rdx
	movq	$129, %rdi
	movq	$16, %rsi
	callq	gen_rtx_fmt_e
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rbx
	movq	$52, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rcx
	movq	$2, %rdi
	xorq	%rax, %rax
	movq	%rbx, %rsi
	movq	%rcx, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_E
	movq	%rax, %rdi
	callq	emit
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp4764:
	.size	gen_split_1061, .Ltmp4764-gen_split_1061
	.cfi_endproc

	.globl	gen_split_1062
	.align	16, 0x90
	.type	gen_split_1062,@function
gen_split_1062:                         # @gen_split_1062
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp4769:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp4770:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp4771:
	.cfi_def_cfa_offset 32
.Ltmp4772:
	.cfi_offset %rbx, -32
.Ltmp4773:
	.cfi_offset %r14, -24
.Ltmp4774:
	.cfi_offset %r15, -16
	movq	%rdi, %rbx
	callq	start_sequence
	movq	(%rbx), %rsi
	movq	8(%rbx), %r14
	movq	16(%rbx), %rbx
	movq	$6, %rdi
	xorq	%rdx, %rdx
	callq	gen_rtx_SUBREG
	movq	%rax, %r15
	movq	$6, %rdi
	xorq	%rdx, %rdx
	movq	%rbx, %rsi
	callq	gen_rtx_SUBREG
	movq	$86, %rdi
	movq	$6, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rbx
	movq	$6, %rdi
	xorq	%rdx, %rdx
	movq	%r14, %rsi
	callq	gen_rtx_SUBREG
	movq	$83, %rdi
	movq	$6, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r15, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp4775:
	.size	gen_split_1062, .Ltmp4775-gen_split_1062
	.cfi_endproc

	.globl	gen_split_1063
	.align	16, 0x90
	.type	gen_split_1063,@function
gen_split_1063:                         # @gen_split_1063
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp4779:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp4780:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp4781:
	.cfi_def_cfa_offset 32
.Ltmp4782:
	.cfi_offset %rbx, -24
.Ltmp4783:
	.cfi_offset %r14, -16
	movq	%rdi, %rbx
	callq	start_sequence
	movq	(%rbx), %r14
	movq	8(%rbx), %rdx
	movq	$129, %rdi
	movq	$16, %rsi
	callq	gen_rtx_fmt_e
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp4784:
	.size	gen_split_1063, .Ltmp4784-gen_split_1063
	.cfi_endproc

	.globl	gen_split_1064
	.align	16, 0x90
	.type	gen_split_1064,@function
gen_split_1064:                         # @gen_split_1064
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp4788:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp4789:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp4790:
	.cfi_def_cfa_offset 32
.Ltmp4791:
	.cfi_offset %rbx, -24
.Ltmp4792:
	.cfi_offset %r14, -16
	movq	%rdi, %rbx
	callq	start_sequence
	movq	$2147483647, %rdi       # imm = 0x7FFFFFFF
	movq	$4, %rsi
	callq	trunc_int_for_mode
	xorq	%rdi, %rdi
	movq	%rax, %rsi
	callq	gen_rtx_CONST_INT
	movq	%rax, 32(%rbx)
	leaq	16(%rbx), %rdx
	leaq	24(%rbx), %rcx
	movq	$1, %rsi
	movq	%rbx, %rdi
	callq	split_di
	movq	24(%rbx), %r14
	movq	32(%rbx), %rbx
	movq	%r14, %rdi
	callq	copy_rtx
	movq	$83, %rdi
	movq	$4, %rsi
	movq	%rax, %rdx
	movq	%rbx, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rbx
	movq	$52, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rcx
	movq	$2, %rdi
	xorq	%rax, %rax
	movq	%rbx, %rsi
	movq	%rcx, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_E
	movq	%rax, %rdi
	callq	emit
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp4793:
	.size	gen_split_1064, .Ltmp4793-gen_split_1064
	.cfi_endproc

	.globl	gen_absxf2
	.align	16, 0x90
	.type	gen_absxf2,@function
gen_absxf2:                             # @gen_absxf2
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp4797:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp4798:
	.cfi_def_cfa_offset 24
	subq	$24, %rsp
.Ltmp4799:
	.cfi_def_cfa_offset 48
.Ltmp4800:
	.cfi_offset %rbx, -24
.Ltmp4801:
	.cfi_offset %r14, -16
	movq	%rsi, %r14
	movq	%rdi, %rbx
	callq	start_sequence
	movq	%rbx, (%rsp)
	movq	%r14, 8(%rsp)
	leaq	(%rsp), %rdx
	movq	$129, %rdi
	movq	$17, %rsi
	callq	ix86_expand_unary_operator
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	addq	$24, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp4802:
	.size	gen_absxf2, .Ltmp4802-gen_absxf2
	.cfi_endproc

	.globl	gen_abstf2
	.align	16, 0x90
	.type	gen_abstf2,@function
gen_abstf2:                             # @gen_abstf2
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp4806:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp4807:
	.cfi_def_cfa_offset 24
	subq	$24, %rsp
.Ltmp4808:
	.cfi_def_cfa_offset 48
.Ltmp4809:
	.cfi_offset %rbx, -24
.Ltmp4810:
	.cfi_offset %r14, -16
	movq	%rsi, %r14
	movq	%rdi, %rbx
	callq	start_sequence
	movq	%rbx, (%rsp)
	movq	%r14, 8(%rsp)
	leaq	(%rsp), %rdx
	movq	$129, %rdi
	movq	$18, %rsi
	callq	ix86_expand_unary_operator
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	addq	$24, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp4811:
	.size	gen_abstf2, .Ltmp4811-gen_abstf2
	.cfi_endproc

	.globl	gen_split_1067
	.align	16, 0x90
	.type	gen_split_1067,@function
gen_split_1067:                         # @gen_split_1067
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp4815:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp4816:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp4817:
	.cfi_def_cfa_offset 32
.Ltmp4818:
	.cfi_offset %rbx, -24
.Ltmp4819:
	.cfi_offset %r14, -16
	movq	%rdi, %rbx
	callq	start_sequence
	movq	(%rbx), %r14
	movq	8(%rbx), %rdx
	movq	$129, %rdi
	movq	$17, %rsi
	callq	gen_rtx_fmt_e
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp4820:
	.size	gen_split_1067, .Ltmp4820-gen_split_1067
	.cfi_endproc

	.globl	gen_split_1068
	.align	16, 0x90
	.type	gen_split_1068,@function
gen_split_1068:                         # @gen_split_1068
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp4824:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp4825:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp4826:
	.cfi_def_cfa_offset 32
.Ltmp4827:
	.cfi_offset %rbx, -24
.Ltmp4828:
	.cfi_offset %r14, -16
	movq	%rdi, %r14
	callq	start_sequence
	xorq	%rdi, %rdi
	movq	$-32769, %rsi           # imm = 0xFFFFFFFFFFFF7FFF
	callq	gen_rtx_CONST_INT
	movq	%rax, 8(%r14)
	movq	(%r14), %rdi
	callq	true_regnum
	movl	target_flags(%rip), %edx
	movq	$25, %rcx
	shrq	%cl, %rdx
	andq	$1, %rdx
	negq	%rdx
	leaq	2(%rax,%rdx), %rsi
	movq	$4, %rdi
	callq	gen_rtx_REG
	movq	%rax, %rbx
	movq	%rbx, (%r14)
	movq	8(%r14), %r14
	movq	%rbx, %rdi
	callq	copy_rtx
	movq	$83, %rdi
	movq	$4, %rsi
	movq	%rax, %rdx
	movq	%r14, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rbx
	movq	$52, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rcx
	movq	$2, %rdi
	xorq	%rax, %rax
	movq	%rbx, %rsi
	movq	%rcx, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_E
	movq	%rax, %rdi
	callq	emit
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp4829:
	.size	gen_split_1068, .Ltmp4829-gen_split_1068
	.cfi_endproc

	.globl	gen_split_1069
	.align	16, 0x90
	.type	gen_split_1069,@function
gen_split_1069:                         # @gen_split_1069
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp4833:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp4834:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp4835:
	.cfi_def_cfa_offset 32
.Ltmp4836:
	.cfi_offset %rbx, -24
.Ltmp4837:
	.cfi_offset %r14, -16
	movq	%rdi, %rbx
	callq	start_sequence
	movq	(%rbx), %r14
	movq	8(%rbx), %rdx
	movq	$129, %rdi
	movq	$18, %rsi
	callq	gen_rtx_fmt_e
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp4838:
	.size	gen_split_1069, .Ltmp4838-gen_split_1069
	.cfi_endproc

	.globl	gen_split_1070
	.align	16, 0x90
	.type	gen_split_1070,@function
gen_split_1070:                         # @gen_split_1070
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp4842:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp4843:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp4844:
	.cfi_def_cfa_offset 32
.Ltmp4845:
	.cfi_offset %rbx, -24
.Ltmp4846:
	.cfi_offset %r14, -16
	movq	%rdi, %r14
	callq	start_sequence
	xorq	%rdi, %rdi
	movq	$-32769, %rsi           # imm = 0xFFFFFFFFFFFF7FFF
	callq	gen_rtx_CONST_INT
	movq	%rax, 8(%r14)
	movq	(%r14), %rdi
	callq	true_regnum
	movl	target_flags(%rip), %edx
	movq	$25, %rcx
	shrq	%cl, %rdx
	andq	$1, %rdx
	negq	%rdx
	leaq	2(%rax,%rdx), %rsi
	movq	$4, %rdi
	callq	gen_rtx_REG
	movq	%rax, %rbx
	movq	%rbx, (%r14)
	movq	8(%r14), %r14
	movq	%rbx, %rdi
	callq	copy_rtx
	movq	$83, %rdi
	movq	$4, %rsi
	movq	%rax, %rdx
	movq	%r14, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rbx
	movq	$52, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rcx
	movq	$2, %rdi
	xorq	%rax, %rax
	movq	%rbx, %rsi
	movq	%rcx, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_E
	movq	%rax, %rdi
	callq	emit
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp4847:
	.size	gen_split_1070, .Ltmp4847-gen_split_1070
	.cfi_endproc

	.globl	gen_one_cmpldi2
	.align	16, 0x90
	.type	gen_one_cmpldi2,@function
gen_one_cmpldi2:                        # @gen_one_cmpldi2
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp4851:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp4852:
	.cfi_def_cfa_offset 24
	subq	$24, %rsp
.Ltmp4853:
	.cfi_def_cfa_offset 48
.Ltmp4854:
	.cfi_offset %rbx, -24
.Ltmp4855:
	.cfi_offset %r14, -16
	movq	%rsi, %r14
	movq	%rdi, %rbx
	callq	start_sequence
	movq	%rbx, (%rsp)
	movq	%r14, 8(%rsp)
	leaq	(%rsp), %rdx
	movq	$86, %rdi
	movq	$5, %rsi
	callq	ix86_expand_unary_operator
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	addq	$24, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp4856:
	.size	gen_one_cmpldi2, .Ltmp4856-gen_one_cmpldi2
	.cfi_endproc

	.globl	gen_split_1072
	.align	16, 0x90
	.type	gen_split_1072,@function
gen_split_1072:                         # @gen_split_1072
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp4861:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp4862:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp4863:
	.cfi_def_cfa_offset 32
.Ltmp4864:
	.cfi_offset %rbx, -32
.Ltmp4865:
	.cfi_offset %r14, -24
.Ltmp4866:
	.cfi_offset %r15, -16
	movq	%rdi, %rbx
	callq	start_sequence
	movq	(%rbx), %r14
	movq	8(%rbx), %rbx
	movq	$55, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	const_int_rtx+504(%rip), %rcx
	movq	%rax, %r15
	movq	$85, %rdi
	movq	$5, %rsi
	movq	%rbx, %rdx
	callq	gen_rtx_fmt_ee
	movq	const_int_rtx+512(%rip), %rcx
	movq	$74, %rdi
	movq	$55, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r15, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %r15
	movq	%rbx, %rdi
	callq	copy_rtx
	movq	const_int_rtx+504(%rip), %rcx
	movq	$85, %rdi
	movq	$5, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rcx
	movq	$2, %rdi
	xorq	%rax, %rax
	movq	%r15, %rsi
	movq	%rcx, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_E
	movq	%rax, %rdi
	callq	emit
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp4867:
	.size	gen_split_1072, .Ltmp4867-gen_split_1072
	.cfi_endproc

	.globl	gen_one_cmplsi2
	.align	16, 0x90
	.type	gen_one_cmplsi2,@function
gen_one_cmplsi2:                        # @gen_one_cmplsi2
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp4871:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp4872:
	.cfi_def_cfa_offset 24
	subq	$24, %rsp
.Ltmp4873:
	.cfi_def_cfa_offset 48
.Ltmp4874:
	.cfi_offset %rbx, -24
.Ltmp4875:
	.cfi_offset %r14, -16
	movq	%rsi, %r14
	movq	%rdi, %rbx
	callq	start_sequence
	movq	%rbx, (%rsp)
	movq	%r14, 8(%rsp)
	leaq	(%rsp), %rdx
	movq	$86, %rdi
	movq	$4, %rsi
	callq	ix86_expand_unary_operator
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	addq	$24, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp4876:
	.size	gen_one_cmplsi2, .Ltmp4876-gen_one_cmplsi2
	.cfi_endproc

	.globl	gen_split_1074
	.align	16, 0x90
	.type	gen_split_1074,@function
gen_split_1074:                         # @gen_split_1074
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp4881:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp4882:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp4883:
	.cfi_def_cfa_offset 32
.Ltmp4884:
	.cfi_offset %rbx, -32
.Ltmp4885:
	.cfi_offset %r14, -24
.Ltmp4886:
	.cfi_offset %r15, -16
	movq	%rdi, %rbx
	callq	start_sequence
	movq	(%rbx), %r14
	movq	8(%rbx), %rbx
	movq	$55, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	const_int_rtx+504(%rip), %rcx
	movq	%rax, %r15
	movq	$85, %rdi
	movq	$4, %rsi
	movq	%rbx, %rdx
	callq	gen_rtx_fmt_ee
	movq	const_int_rtx+512(%rip), %rcx
	movq	$74, %rdi
	movq	$55, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r15, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %r15
	movq	%rbx, %rdi
	callq	copy_rtx
	movq	const_int_rtx+504(%rip), %rcx
	movq	$85, %rdi
	movq	$4, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rcx
	movq	$2, %rdi
	xorq	%rax, %rax
	movq	%r15, %rsi
	movq	%rcx, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_E
	movq	%rax, %rdi
	callq	emit
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp4887:
	.size	gen_split_1074, .Ltmp4887-gen_split_1074
	.cfi_endproc

	.globl	gen_split_1075
	.align	16, 0x90
	.type	gen_split_1075,@function
gen_split_1075:                         # @gen_split_1075
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp4892:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp4893:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp4894:
	.cfi_def_cfa_offset 32
.Ltmp4895:
	.cfi_offset %rbx, -32
.Ltmp4896:
	.cfi_offset %r14, -24
.Ltmp4897:
	.cfi_offset %r15, -16
	movq	%rdi, %rbx
	callq	start_sequence
	movq	(%rbx), %r14
	movq	8(%rbx), %rbx
	movq	$55, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	const_int_rtx+504(%rip), %rcx
	movq	%rax, %r15
	movq	$85, %rdi
	movq	$4, %rsi
	movq	%rbx, %rdx
	callq	gen_rtx_fmt_ee
	movq	const_int_rtx+512(%rip), %rcx
	movq	$74, %rdi
	movq	$55, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r15, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %r15
	movq	%rbx, %rdi
	callq	copy_rtx
	movq	const_int_rtx+504(%rip), %rcx
	movq	$85, %rdi
	movq	$4, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_ee
	movq	$121, %rdi
	movq	$5, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rcx
	movq	$2, %rdi
	xorq	%rax, %rax
	movq	%r15, %rsi
	movq	%rcx, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_E
	movq	%rax, %rdi
	callq	emit
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp4898:
	.size	gen_split_1075, .Ltmp4898-gen_split_1075
	.cfi_endproc

	.globl	gen_one_cmplhi2
	.align	16, 0x90
	.type	gen_one_cmplhi2,@function
gen_one_cmplhi2:                        # @gen_one_cmplhi2
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp4902:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp4903:
	.cfi_def_cfa_offset 24
	subq	$24, %rsp
.Ltmp4904:
	.cfi_def_cfa_offset 48
.Ltmp4905:
	.cfi_offset %rbx, -24
.Ltmp4906:
	.cfi_offset %r14, -16
	movq	%rsi, %r14
	movq	%rdi, %rbx
	callq	start_sequence
	movq	%rbx, (%rsp)
	movq	%r14, 8(%rsp)
	leaq	(%rsp), %rdx
	movq	$86, %rdi
	movq	$3, %rsi
	callq	ix86_expand_unary_operator
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	addq	$24, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp4907:
	.size	gen_one_cmplhi2, .Ltmp4907-gen_one_cmplhi2
	.cfi_endproc

	.globl	gen_split_1077
	.align	16, 0x90
	.type	gen_split_1077,@function
gen_split_1077:                         # @gen_split_1077
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp4912:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp4913:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp4914:
	.cfi_def_cfa_offset 32
.Ltmp4915:
	.cfi_offset %rbx, -32
.Ltmp4916:
	.cfi_offset %r14, -24
.Ltmp4917:
	.cfi_offset %r15, -16
	movq	%rdi, %rbx
	callq	start_sequence
	movq	(%rbx), %r14
	movq	8(%rbx), %rbx
	movq	$55, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	const_int_rtx+504(%rip), %rcx
	movq	%rax, %r15
	movq	$85, %rdi
	movq	$3, %rsi
	movq	%rbx, %rdx
	callq	gen_rtx_fmt_ee
	movq	const_int_rtx+512(%rip), %rcx
	movq	$74, %rdi
	movq	$55, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r15, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %r15
	movq	%rbx, %rdi
	callq	copy_rtx
	movq	const_int_rtx+504(%rip), %rcx
	movq	$85, %rdi
	movq	$3, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rcx
	movq	$2, %rdi
	xorq	%rax, %rax
	movq	%r15, %rsi
	movq	%rcx, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_E
	movq	%rax, %rdi
	callq	emit
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp4918:
	.size	gen_split_1077, .Ltmp4918-gen_split_1077
	.cfi_endproc

	.globl	gen_one_cmplqi2
	.align	16, 0x90
	.type	gen_one_cmplqi2,@function
gen_one_cmplqi2:                        # @gen_one_cmplqi2
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp4922:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp4923:
	.cfi_def_cfa_offset 24
	subq	$24, %rsp
.Ltmp4924:
	.cfi_def_cfa_offset 48
.Ltmp4925:
	.cfi_offset %rbx, -24
.Ltmp4926:
	.cfi_offset %r14, -16
	movq	%rsi, %r14
	movq	%rdi, %rbx
	callq	start_sequence
	movq	%rbx, (%rsp)
	movq	%r14, 8(%rsp)
	leaq	(%rsp), %rdx
	movq	$86, %rdi
	movq	$2, %rsi
	callq	ix86_expand_unary_operator
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	addq	$24, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp4927:
	.size	gen_one_cmplqi2, .Ltmp4927-gen_one_cmplqi2
	.cfi_endproc

	.globl	gen_split_1079
	.align	16, 0x90
	.type	gen_split_1079,@function
gen_split_1079:                         # @gen_split_1079
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp4932:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp4933:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp4934:
	.cfi_def_cfa_offset 32
.Ltmp4935:
	.cfi_offset %rbx, -32
.Ltmp4936:
	.cfi_offset %r14, -24
.Ltmp4937:
	.cfi_offset %r15, -16
	movq	%rdi, %rbx
	callq	start_sequence
	movq	(%rbx), %r14
	movq	8(%rbx), %rbx
	movq	$55, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	const_int_rtx+504(%rip), %rcx
	movq	%rax, %r15
	movq	$85, %rdi
	movq	$2, %rsi
	movq	%rbx, %rdx
	callq	gen_rtx_fmt_ee
	movq	const_int_rtx+512(%rip), %rcx
	movq	$74, %rdi
	movq	$55, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r15, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %r15
	movq	%rbx, %rdi
	callq	copy_rtx
	movq	const_int_rtx+504(%rip), %rcx
	movq	$85, %rdi
	movq	$2, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rcx
	movq	$2, %rdi
	xorq	%rax, %rax
	movq	%r15, %rsi
	movq	%rcx, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_E
	movq	%rax, %rdi
	callq	emit
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp4938:
	.size	gen_split_1079, .Ltmp4938-gen_split_1079
	.cfi_endproc

	.globl	gen_ashldi3
	.align	16, 0x90
	.type	gen_ashldi3,@function
gen_ashldi3:                            # @gen_ashldi3
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp4943:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp4944:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp4945:
	.cfi_def_cfa_offset 32
	subq	$32, %rsp
.Ltmp4946:
	.cfi_def_cfa_offset 64
.Ltmp4947:
	.cfi_offset %rbx, -32
.Ltmp4948:
	.cfi_offset %r14, -24
.Ltmp4949:
	.cfi_offset %r15, -16
	movq	%rdx, %r14
	movq	%rsi, %r15
	movq	%rdi, %rbx
	callq	start_sequence
	movl	target_flags(%rip), %eax
	movq	%rbx, (%rsp)
	movq	%r15, 8(%rsp)
	movq	%r14, 16(%rsp)
	testq	$33554432, %rax         # imm = 0x2000000
	jne	.LBB536_4
# BB#1:                                 # %land.lhs.true
	movl	x86_cmove(%rip), %edx
	movl	ix86_arch(%rip), %ecx
	movq	$1, %rsi
	shlq	%cl, %rsi
	andq	%rdx, %rsi
	andq	$327680, %rax           # imm = 0x50000
	orq	%rsi, %rax
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	testq	%rbx, %rax
	je	.LBB536_4
# BB#2:                                 # %land.lhs.true6
	movq	$2, %rsi
	movq	%r14, %rdi
	callq	immediate_operand
	testq	%rbx, %rax
	je	.LBB536_3
.LBB536_4:                              # %if.end
	leaq	(%rsp), %rdx
	movq	$87, %rdi
	movq	$5, %rsi
	callq	ix86_expand_binary_operator
.LBB536_5:                              # %return
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	addq	$32, %rsp
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.LBB536_3:                              # %if.then
	movq	(%rsp), %rbx
	movq	8(%rsp), %rdx
	movq	16(%rsp), %rcx
	movq	$87, %rdi
	movq	$5, %rsi
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %r14
	movq	$62, %rdi
	movq	$4, %rsi
	callq	gen_rtx_fmt_0
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rbx
	movq	$52, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rcx
	movq	$3, %rdi
	xorq	%rax, %rax
	movq	%r14, %rsi
	movq	%rbx, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_E
	movq	%rax, %rdi
	callq	emit_insn
	jmp	.LBB536_5
.Ltmp4950:
	.size	gen_ashldi3, .Ltmp4950-gen_ashldi3
	.cfi_endproc

	.globl	gen_split_1081
	.align	16, 0x90
	.type	gen_split_1081,@function
gen_split_1081:                         # @gen_split_1081
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp4954:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp4955:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp4956:
	.cfi_def_cfa_offset 32
.Ltmp4957:
	.cfi_offset %rbx, -24
.Ltmp4958:
	.cfi_offset %r14, -16
	movq	%rdi, %rbx
	callq	start_sequence
	movq	16(%rbx), %rax
	movl	8(%rax), %ecx
	movq	$1, %rdi
	shlq	%cl, %rdi
	movq	$32, %rcx
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	movq	$5, %rsi
	callq	trunc_int_for_mode
	xorq	%rdi, %rdi
	movq	%rax, %rsi
	callq	gen_rtx_CONST_INT
	movq	%rax, 16(%rbx)
	movq	(%rbx), %r14
	movq	8(%rbx), %rdx
	movq	$78, %rdi
	movq	$5, %rsi
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp4959:
	.size	gen_split_1081, .Ltmp4959-gen_split_1081
	.cfi_endproc

	.globl	gen_split_1082
	.align	16, 0x90
	.type	gen_split_1082,@function
gen_split_1082:                         # @gen_split_1082
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp4962:
	.cfi_def_cfa_offset 16
.Ltmp4963:
	.cfi_offset %rbx, -16
	movq	%rdi, %rbx
	callq	start_sequence
	movq	24(%rbx), %rsi
	movq	%rbx, %rdi
	callq	ix86_split_ashldi
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	popq	%rbx
	retq
.Ltmp4964:
	.size	gen_split_1082, .Ltmp4964-gen_split_1082
	.cfi_endproc

	.globl	gen_split_1083
	.align	16, 0x90
	.type	gen_split_1083,@function
gen_split_1083:                         # @gen_split_1083
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp4967:
	.cfi_def_cfa_offset 16
.Ltmp4968:
	.cfi_offset %rbx, -16
	movq	%rdi, %rbx
	callq	start_sequence
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	ix86_split_ashldi
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	popq	%rbx
	retq
.Ltmp4969:
	.size	gen_split_1083, .Ltmp4969-gen_split_1083
	.cfi_endproc

	.globl	gen_x86_shift_adj_1
	.align	16, 0x90
	.type	gen_x86_shift_adj_1,@function
gen_x86_shift_adj_1:                    # @gen_x86_shift_adj_1
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp4976:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp4977:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp4978:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp4979:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp4980:
	.cfi_def_cfa_offset 48
.Ltmp4981:
	.cfi_offset %rbx, -48
.Ltmp4982:
	.cfi_offset %r12, -40
.Ltmp4983:
	.cfi_offset %r13, -32
.Ltmp4984:
	.cfi_offset %r14, -24
.Ltmp4985:
	.cfi_offset %r15, -16
	movq	%rcx, %r14
	movq	%rdx, %rbx
	movq	%rsi, %r15
	movq	%rdi, %r12
	callq	start_sequence
	movq	$56, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	%rax, %r13
	xorq	%rdi, %rdi
	movq	$32, %rsi
	callq	gen_rtx_CONST_INT
	movq	$83, %rdi
	movq	$2, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	const_int_rtx+512(%rip), %rcx
	movq	$74, %rdi
	movq	$56, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r13, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	movq	$56, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	const_int_rtx+512(%rip), %rcx
	movq	$102, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_ee
	movq	$72, %rdi
	movq	$4, %rsi
	movq	%rax, %rdx
	movq	%r15, %rcx
	movq	%r12, %r8
	callq	gen_rtx_fmt_eee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r12, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	movq	$56, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	const_int_rtx+512(%rip), %rcx
	movq	$102, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_ee
	movq	$72, %rdi
	movq	$4, %rsi
	movq	%rax, %rdx
	movq	%r14, %rcx
	movq	%r15, %r8
	callq	gen_rtx_fmt_eee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r15, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp4986:
	.size	gen_x86_shift_adj_1, .Ltmp4986-gen_x86_shift_adj_1
	.cfi_endproc

	.globl	gen_x86_shift_adj_2
	.align	16, 0x90
	.type	gen_x86_shift_adj_2,@function
gen_x86_shift_adj_2:                    # @gen_x86_shift_adj_2
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp4993:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp4994:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp4995:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp4996:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp4997:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp4998:
	.cfi_def_cfa_offset 64
.Ltmp4999:
	.cfi_offset %rbx, -48
.Ltmp5000:
	.cfi_offset %r12, -40
.Ltmp5001:
	.cfi_offset %r13, -32
.Ltmp5002:
	.cfi_offset %r14, -24
.Ltmp5003:
	.cfi_offset %r15, -16
	movq	%rdx, %r13
	movq	%rsi, %r14
	movq	%rdi, 8(%rsp)           # 8-byte Spill
	callq	start_sequence
	callq	gen_label_rtx
	movq	%rax, %r12
	xorq	%rdi, %rdi
	movq	$32, %rsi
	callq	gen_rtx_CONST_INT
	movq	%rax, %rbx
	movq	$56, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	%rax, %r15
	movq	$83, %rdi
	movq	$2, %rsi
	movq	%r13, %rdx
	movq	%rbx, %rcx
	callq	gen_rtx_fmt_ee
	movq	const_int_rtx+512(%rip), %rcx
	movq	$74, %rdi
	movq	$56, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r15, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	movq	$56, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	const_int_rtx+512(%rip), %rcx
	movq	$103, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rbx
	movq	$67, %rdi
	xorq	%rsi, %rsi
	movq	%r12, %rdx
	callq	gen_rtx_fmt_u00
	movq	global_rtl(%rip), %r8
	movq	$72, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_eee
	movq	global_rtl(%rip), %rdx
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_jump_insn
	movq	%r12, 64(%rax)
	movq	8(%rsp), %rdi           # 8-byte Reload
	movq	%r14, %rsi
	callq	emit_move_insn
	movq	const_int_rtx+512(%rip), %rsi
	movq	%r14, %rdi
	callq	emit_move_insn
	movq	%r12, %rdi
	callq	emit_label
	movq	$1, %rax
	movl	%eax, 32(%r12)
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp5004:
	.size	gen_x86_shift_adj_2, .Ltmp5004-gen_x86_shift_adj_2
	.cfi_endproc

	.globl	gen_ashlsi3
	.align	16, 0x90
	.type	gen_ashlsi3,@function
gen_ashlsi3:                            # @gen_ashlsi3
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp5009:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp5010:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp5011:
	.cfi_def_cfa_offset 32
	subq	$32, %rsp
.Ltmp5012:
	.cfi_def_cfa_offset 64
.Ltmp5013:
	.cfi_offset %rbx, -32
.Ltmp5014:
	.cfi_offset %r14, -24
.Ltmp5015:
	.cfi_offset %r15, -16
	movq	%rdx, %r14
	movq	%rsi, %r15
	movq	%rdi, %rbx
	callq	start_sequence
	movq	%rbx, (%rsp)
	movq	%r15, 8(%rsp)
	movq	%r14, 16(%rsp)
	leaq	(%rsp), %rdx
	movq	$87, %rdi
	movq	$4, %rsi
	callq	ix86_expand_binary_operator
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	addq	$32, %rsp
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp5016:
	.size	gen_ashlsi3, .Ltmp5016-gen_ashlsi3
	.cfi_endproc

	.globl	gen_split_1087
	.align	16, 0x90
	.type	gen_split_1087,@function
gen_split_1087:                         # @gen_split_1087
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp5019:
	.cfi_def_cfa_offset 16
.Ltmp5020:
	.cfi_offset %rbx, -16
	movq	%rdi, %rbx
	callq	start_sequence
	movq	(%rbx), %rsi
	movq	$4, %rdi
	callq	gen_lowpart
	movq	%rax, (%rbx)
	movl	target_flags(%rip), %edi
	movq	$25, %rcx
	movq	8(%rbx), %rsi
	shrq	%cl, %rdi
	andq	$1, %rdi
	orq	$4, %rdi
	callq	gen_lowpart
	movq	%rax, 8(%rbx)
	movq	16(%rbx), %rax
	movl	8(%rax), %ecx
	movq	$1, %rdi
	shlq	%cl, %rdi
	movq	$32, %rcx
	shlq	%cl, %rdi
	movl	target_flags(%rip), %esi
	movq	$32, %rcx
	sarq	%cl, %rdi
	movq	$25, %rcx
	shrq	%cl, %rsi
	andq	$1, %rsi
	orq	$4, %rsi
	callq	trunc_int_for_mode
	xorq	%rdi, %rdi
	movq	%rax, %rsi
	callq	gen_rtx_CONST_INT
	movq	%rax, 16(%rbx)
	movl	target_flags(%rip), %esi
	movq	$25, %rcx
	movq	8(%rbx), %rdx
	shrq	%cl, %rsi
	andq	$1, %rsi
	orq	$4, %rsi
	movq	$78, %rdi
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movzbl	target_flags+3(%rip), %ecx
	testq	$2, %rcx
	je	.LBB543_2
# BB#1:                                 # %if.then
	movq	$4, %rdi
	xorq	%rdx, %rdx
	movq	%rax, %rsi
	callq	gen_rtx_SUBREG
.LBB543_2:                              # %if.end
	movq	(%rbx), %rdx
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	popq	%rbx
	retq
.Ltmp5021:
	.size	gen_split_1087, .Ltmp5021-gen_split_1087
	.cfi_endproc

	.globl	gen_split_1088
	.align	16, 0x90
	.type	gen_split_1088,@function
gen_split_1088:                         # @gen_split_1088
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp5025:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp5026:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp5027:
	.cfi_def_cfa_offset 32
.Ltmp5028:
	.cfi_offset %rbx, -24
.Ltmp5029:
	.cfi_offset %r14, -16
	movq	%rdi, %rbx
	callq	start_sequence
	movl	target_flags(%rip), %edi
	movq	$25, %rcx
	movq	8(%rbx), %rsi
	shrq	%cl, %rdi
	andq	$1, %rdi
	orq	$4, %rdi
	callq	gen_lowpart
	movq	%rax, 8(%rbx)
	movq	16(%rbx), %rax
	movl	8(%rax), %ecx
	movq	$1, %rdi
	shlq	%cl, %rdi
	movq	$32, %rcx
	shlq	%cl, %rdi
	movl	target_flags(%rip), %esi
	movq	$32, %rcx
	sarq	%cl, %rdi
	movq	$25, %rcx
	shrq	%cl, %rsi
	andq	$1, %rsi
	orq	$4, %rsi
	callq	trunc_int_for_mode
	xorq	%rdi, %rdi
	movq	%rax, %rsi
	callq	gen_rtx_CONST_INT
	movq	%rax, 16(%rbx)
	movq	(%rbx), %r14
	movq	8(%rbx), %rdx
	movq	$78, %rdi
	movq	$4, %rsi
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$4, %rdi
	xorq	%rdx, %rdx
	movq	%rax, %rsi
	callq	gen_rtx_SUBREG
	movq	$121, %rdi
	movq	$5, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp5030:
	.size	gen_split_1088, .Ltmp5030-gen_split_1088
	.cfi_endproc

	.globl	gen_ashlhi3
	.align	16, 0x90
	.type	gen_ashlhi3,@function
gen_ashlhi3:                            # @gen_ashlhi3
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp5035:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp5036:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp5037:
	.cfi_def_cfa_offset 32
	subq	$32, %rsp
.Ltmp5038:
	.cfi_def_cfa_offset 64
.Ltmp5039:
	.cfi_offset %rbx, -32
.Ltmp5040:
	.cfi_offset %r14, -24
.Ltmp5041:
	.cfi_offset %r15, -16
	movq	%rdx, %r14
	movq	%rsi, %r15
	movq	%rdi, %rbx
	callq	start_sequence
	movq	%rbx, (%rsp)
	movq	%r15, 8(%rsp)
	movq	%r14, 16(%rsp)
	leaq	(%rsp), %rdx
	movq	$87, %rdi
	movq	$3, %rsi
	callq	ix86_expand_binary_operator
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	addq	$32, %rsp
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp5042:
	.size	gen_ashlhi3, .Ltmp5042-gen_ashlhi3
	.cfi_endproc

	.globl	gen_ashlqi3
	.align	16, 0x90
	.type	gen_ashlqi3,@function
gen_ashlqi3:                            # @gen_ashlqi3
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp5047:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp5048:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp5049:
	.cfi_def_cfa_offset 32
	subq	$32, %rsp
.Ltmp5050:
	.cfi_def_cfa_offset 64
.Ltmp5051:
	.cfi_offset %rbx, -32
.Ltmp5052:
	.cfi_offset %r14, -24
.Ltmp5053:
	.cfi_offset %r15, -16
	movq	%rdx, %r14
	movq	%rsi, %r15
	movq	%rdi, %rbx
	callq	start_sequence
	movq	%rbx, (%rsp)
	movq	%r15, 8(%rsp)
	movq	%r14, 16(%rsp)
	leaq	(%rsp), %rdx
	movq	$87, %rdi
	movq	$2, %rsi
	callq	ix86_expand_binary_operator
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	addq	$32, %rsp
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp5054:
	.size	gen_ashlqi3, .Ltmp5054-gen_ashlqi3
	.cfi_endproc

	.globl	gen_ashrdi3
	.align	16, 0x90
	.type	gen_ashrdi3,@function
gen_ashrdi3:                            # @gen_ashrdi3
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp5059:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp5060:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp5061:
	.cfi_def_cfa_offset 32
	subq	$32, %rsp
.Ltmp5062:
	.cfi_def_cfa_offset 64
.Ltmp5063:
	.cfi_offset %rbx, -32
.Ltmp5064:
	.cfi_offset %r14, -24
.Ltmp5065:
	.cfi_offset %r15, -16
	movq	%rdx, %r14
	movq	%rsi, %r15
	movq	%rdi, %rbx
	callq	start_sequence
	movl	target_flags(%rip), %eax
	movq	%rbx, (%rsp)
	movq	%r15, 8(%rsp)
	movq	%r14, 16(%rsp)
	testq	$33554432, %rax         # imm = 0x2000000
	jne	.LBB547_4
# BB#1:                                 # %land.lhs.true
	movl	x86_cmove(%rip), %edx
	movl	ix86_arch(%rip), %ecx
	movq	$1, %rsi
	shlq	%cl, %rsi
	andq	%rdx, %rsi
	andq	$327680, %rax           # imm = 0x50000
	orq	%rsi, %rax
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	testq	%rbx, %rax
	je	.LBB547_4
# BB#2:                                 # %land.lhs.true6
	movq	$2, %rsi
	movq	%r14, %rdi
	callq	immediate_operand
	testq	%rbx, %rax
	je	.LBB547_3
.LBB547_4:                              # %if.end
	leaq	(%rsp), %rdx
	movq	$89, %rdi
	movq	$5, %rsi
	callq	ix86_expand_binary_operator
.LBB547_5:                              # %return
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	addq	$32, %rsp
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.LBB547_3:                              # %if.then
	movq	(%rsp), %rbx
	movq	8(%rsp), %rdx
	movq	16(%rsp), %rcx
	movq	$89, %rdi
	movq	$5, %rsi
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %r14
	movq	$62, %rdi
	movq	$4, %rsi
	callq	gen_rtx_fmt_0
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rbx
	movq	$52, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rcx
	movq	$3, %rdi
	xorq	%rax, %rax
	movq	%r14, %rsi
	movq	%rbx, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_E
	movq	%rax, %rdi
	callq	emit_insn
	jmp	.LBB547_5
.Ltmp5066:
	.size	gen_ashrdi3, .Ltmp5066-gen_ashrdi3
	.cfi_endproc

	.globl	gen_split_1092
	.align	16, 0x90
	.type	gen_split_1092,@function
gen_split_1092:                         # @gen_split_1092
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp5069:
	.cfi_def_cfa_offset 16
.Ltmp5070:
	.cfi_offset %rbx, -16
	movq	%rdi, %rbx
	callq	start_sequence
	movq	24(%rbx), %rsi
	movq	%rbx, %rdi
	callq	ix86_split_ashrdi
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	popq	%rbx
	retq
.Ltmp5071:
	.size	gen_split_1092, .Ltmp5071-gen_split_1092
	.cfi_endproc

	.globl	gen_split_1093
	.align	16, 0x90
	.type	gen_split_1093,@function
gen_split_1093:                         # @gen_split_1093
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp5074:
	.cfi_def_cfa_offset 16
.Ltmp5075:
	.cfi_offset %rbx, -16
	movq	%rdi, %rbx
	callq	start_sequence
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	ix86_split_ashrdi
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	popq	%rbx
	retq
.Ltmp5076:
	.size	gen_split_1093, .Ltmp5076-gen_split_1093
	.cfi_endproc

	.globl	gen_x86_shift_adj_3
	.align	16, 0x90
	.type	gen_x86_shift_adj_3,@function
gen_x86_shift_adj_3:                    # @gen_x86_shift_adj_3
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp5083:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp5084:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp5085:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp5086:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp5087:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp5088:
	.cfi_def_cfa_offset 64
.Ltmp5089:
	.cfi_offset %rbx, -48
.Ltmp5090:
	.cfi_offset %r12, -40
.Ltmp5091:
	.cfi_offset %r13, -32
.Ltmp5092:
	.cfi_offset %r14, -24
.Ltmp5093:
	.cfi_offset %r15, -16
	movq	%rdx, %r13
	movq	%rsi, %r14
	movq	%rdi, 8(%rsp)           # 8-byte Spill
	callq	start_sequence
	callq	gen_label_rtx
	movq	%rax, %r12
	xorq	%rdi, %rdi
	movq	$32, %rsi
	callq	gen_rtx_CONST_INT
	movq	%rax, %rbx
	movq	$56, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	%rax, %r15
	movq	$83, %rdi
	movq	$2, %rsi
	movq	%r13, %rdx
	movq	%rbx, %rcx
	callq	gen_rtx_fmt_ee
	movq	const_int_rtx+512(%rip), %rcx
	movq	$74, %rdi
	movq	$56, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r15, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	movq	$56, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	const_int_rtx+512(%rip), %rcx
	movq	$103, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rbx
	movq	$67, %rdi
	xorq	%rsi, %rsi
	movq	%r12, %rdx
	callq	gen_rtx_fmt_u00
	movq	global_rtl(%rip), %r8
	movq	$72, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_eee
	movq	global_rtl(%rip), %rdx
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_jump_insn
	movq	%r12, 64(%rax)
	movq	8(%rsp), %rdi           # 8-byte Reload
	movq	%r14, %rsi
	callq	emit_move_insn
	xorq	%rdi, %rdi
	movq	$31, %rsi
	callq	gen_rtx_CONST_INT
	movq	$89, %rdi
	movq	$4, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rbx
	movq	$52, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rcx
	movq	$2, %rdi
	xorq	%rax, %rax
	movq	%rbx, %rsi
	movq	%rcx, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_E
	movq	%rax, %rdi
	callq	emit_insn
	movq	%r12, %rdi
	callq	emit_label
	movq	$1, %rax
	movl	%eax, 32(%r12)
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp5094:
	.size	gen_x86_shift_adj_3, .Ltmp5094-gen_x86_shift_adj_3
	.cfi_endproc

	.globl	gen_ashrsi3
	.align	16, 0x90
	.type	gen_ashrsi3,@function
gen_ashrsi3:                            # @gen_ashrsi3
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp5099:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp5100:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp5101:
	.cfi_def_cfa_offset 32
	subq	$32, %rsp
.Ltmp5102:
	.cfi_def_cfa_offset 64
.Ltmp5103:
	.cfi_offset %rbx, -32
.Ltmp5104:
	.cfi_offset %r14, -24
.Ltmp5105:
	.cfi_offset %r15, -16
	movq	%rdx, %r14
	movq	%rsi, %r15
	movq	%rdi, %rbx
	callq	start_sequence
	movq	%rbx, (%rsp)
	movq	%r15, 8(%rsp)
	movq	%r14, 16(%rsp)
	leaq	(%rsp), %rdx
	movq	$89, %rdi
	movq	$4, %rsi
	callq	ix86_expand_binary_operator
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	addq	$32, %rsp
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp5106:
	.size	gen_ashrsi3, .Ltmp5106-gen_ashrsi3
	.cfi_endproc

	.globl	gen_ashrhi3
	.align	16, 0x90
	.type	gen_ashrhi3,@function
gen_ashrhi3:                            # @gen_ashrhi3
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp5111:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp5112:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp5113:
	.cfi_def_cfa_offset 32
	subq	$32, %rsp
.Ltmp5114:
	.cfi_def_cfa_offset 64
.Ltmp5115:
	.cfi_offset %rbx, -32
.Ltmp5116:
	.cfi_offset %r14, -24
.Ltmp5117:
	.cfi_offset %r15, -16
	movq	%rdx, %r14
	movq	%rsi, %r15
	movq	%rdi, %rbx
	callq	start_sequence
	movq	%rbx, (%rsp)
	movq	%r15, 8(%rsp)
	movq	%r14, 16(%rsp)
	leaq	(%rsp), %rdx
	movq	$89, %rdi
	movq	$3, %rsi
	callq	ix86_expand_binary_operator
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	addq	$32, %rsp
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp5118:
	.size	gen_ashrhi3, .Ltmp5118-gen_ashrhi3
	.cfi_endproc

	.globl	gen_ashrqi3
	.align	16, 0x90
	.type	gen_ashrqi3,@function
gen_ashrqi3:                            # @gen_ashrqi3
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp5123:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp5124:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp5125:
	.cfi_def_cfa_offset 32
	subq	$32, %rsp
.Ltmp5126:
	.cfi_def_cfa_offset 64
.Ltmp5127:
	.cfi_offset %rbx, -32
.Ltmp5128:
	.cfi_offset %r14, -24
.Ltmp5129:
	.cfi_offset %r15, -16
	movq	%rdx, %r14
	movq	%rsi, %r15
	movq	%rdi, %rbx
	callq	start_sequence
	movq	%rbx, (%rsp)
	movq	%r15, 8(%rsp)
	movq	%r14, 16(%rsp)
	leaq	(%rsp), %rdx
	movq	$89, %rdi
	movq	$2, %rsi
	callq	ix86_expand_binary_operator
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	addq	$32, %rsp
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp5130:
	.size	gen_ashrqi3, .Ltmp5130-gen_ashrqi3
	.cfi_endproc

	.globl	gen_lshrdi3
	.align	16, 0x90
	.type	gen_lshrdi3,@function
gen_lshrdi3:                            # @gen_lshrdi3
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp5135:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp5136:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp5137:
	.cfi_def_cfa_offset 32
	subq	$32, %rsp
.Ltmp5138:
	.cfi_def_cfa_offset 64
.Ltmp5139:
	.cfi_offset %rbx, -32
.Ltmp5140:
	.cfi_offset %r14, -24
.Ltmp5141:
	.cfi_offset %r15, -16
	movq	%rdx, %r14
	movq	%rsi, %r15
	movq	%rdi, %rbx
	callq	start_sequence
	movl	target_flags(%rip), %eax
	movq	%rbx, (%rsp)
	movq	%r15, 8(%rsp)
	movq	%r14, 16(%rsp)
	testq	$33554432, %rax         # imm = 0x2000000
	jne	.LBB554_4
# BB#1:                                 # %land.lhs.true
	movl	x86_cmove(%rip), %edx
	movl	ix86_arch(%rip), %ecx
	movq	$1, %rsi
	shlq	%cl, %rsi
	andq	%rdx, %rsi
	andq	$327680, %rax           # imm = 0x50000
	orq	%rsi, %rax
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	testq	%rbx, %rax
	je	.LBB554_4
# BB#2:                                 # %land.lhs.true6
	movq	$2, %rsi
	movq	%r14, %rdi
	callq	immediate_operand
	testq	%rbx, %rax
	je	.LBB554_3
.LBB554_4:                              # %if.end
	leaq	(%rsp), %rdx
	movq	$90, %rdi
	movq	$5, %rsi
	callq	ix86_expand_binary_operator
.LBB554_5:                              # %return
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	addq	$32, %rsp
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.LBB554_3:                              # %if.then
	movq	(%rsp), %rbx
	movq	8(%rsp), %rdx
	movq	16(%rsp), %rcx
	movq	$90, %rdi
	movq	$5, %rsi
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %r14
	movq	$62, %rdi
	movq	$4, %rsi
	callq	gen_rtx_fmt_0
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rbx
	movq	$52, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rcx
	movq	$3, %rdi
	xorq	%rax, %rax
	movq	%r14, %rsi
	movq	%rbx, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_E
	movq	%rax, %rdi
	callq	emit_insn
	jmp	.LBB554_5
.Ltmp5142:
	.size	gen_lshrdi3, .Ltmp5142-gen_lshrdi3
	.cfi_endproc

	.globl	gen_split_1099
	.align	16, 0x90
	.type	gen_split_1099,@function
gen_split_1099:                         # @gen_split_1099
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp5145:
	.cfi_def_cfa_offset 16
.Ltmp5146:
	.cfi_offset %rbx, -16
	movq	%rdi, %rbx
	callq	start_sequence
	movq	24(%rbx), %rsi
	movq	%rbx, %rdi
	callq	ix86_split_lshrdi
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	popq	%rbx
	retq
.Ltmp5147:
	.size	gen_split_1099, .Ltmp5147-gen_split_1099
	.cfi_endproc

	.globl	gen_split_1100
	.align	16, 0x90
	.type	gen_split_1100,@function
gen_split_1100:                         # @gen_split_1100
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp5150:
	.cfi_def_cfa_offset 16
.Ltmp5151:
	.cfi_offset %rbx, -16
	movq	%rdi, %rbx
	callq	start_sequence
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	ix86_split_lshrdi
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	popq	%rbx
	retq
.Ltmp5152:
	.size	gen_split_1100, .Ltmp5152-gen_split_1100
	.cfi_endproc

	.globl	gen_lshrsi3
	.align	16, 0x90
	.type	gen_lshrsi3,@function
gen_lshrsi3:                            # @gen_lshrsi3
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp5157:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp5158:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp5159:
	.cfi_def_cfa_offset 32
	subq	$32, %rsp
.Ltmp5160:
	.cfi_def_cfa_offset 64
.Ltmp5161:
	.cfi_offset %rbx, -32
.Ltmp5162:
	.cfi_offset %r14, -24
.Ltmp5163:
	.cfi_offset %r15, -16
	movq	%rdx, %r14
	movq	%rsi, %r15
	movq	%rdi, %rbx
	callq	start_sequence
	movq	%rbx, (%rsp)
	movq	%r15, 8(%rsp)
	movq	%r14, 16(%rsp)
	leaq	(%rsp), %rdx
	movq	$90, %rdi
	movq	$4, %rsi
	callq	ix86_expand_binary_operator
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	addq	$32, %rsp
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp5164:
	.size	gen_lshrsi3, .Ltmp5164-gen_lshrsi3
	.cfi_endproc

	.globl	gen_lshrhi3
	.align	16, 0x90
	.type	gen_lshrhi3,@function
gen_lshrhi3:                            # @gen_lshrhi3
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp5169:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp5170:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp5171:
	.cfi_def_cfa_offset 32
	subq	$32, %rsp
.Ltmp5172:
	.cfi_def_cfa_offset 64
.Ltmp5173:
	.cfi_offset %rbx, -32
.Ltmp5174:
	.cfi_offset %r14, -24
.Ltmp5175:
	.cfi_offset %r15, -16
	movq	%rdx, %r14
	movq	%rsi, %r15
	movq	%rdi, %rbx
	callq	start_sequence
	movq	%rbx, (%rsp)
	movq	%r15, 8(%rsp)
	movq	%r14, 16(%rsp)
	leaq	(%rsp), %rdx
	movq	$90, %rdi
	movq	$3, %rsi
	callq	ix86_expand_binary_operator
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	addq	$32, %rsp
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp5176:
	.size	gen_lshrhi3, .Ltmp5176-gen_lshrhi3
	.cfi_endproc

	.globl	gen_lshrqi3
	.align	16, 0x90
	.type	gen_lshrqi3,@function
gen_lshrqi3:                            # @gen_lshrqi3
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp5181:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp5182:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp5183:
	.cfi_def_cfa_offset 32
	subq	$32, %rsp
.Ltmp5184:
	.cfi_def_cfa_offset 64
.Ltmp5185:
	.cfi_offset %rbx, -32
.Ltmp5186:
	.cfi_offset %r14, -24
.Ltmp5187:
	.cfi_offset %r15, -16
	movq	%rdx, %r14
	movq	%rsi, %r15
	movq	%rdi, %rbx
	callq	start_sequence
	movq	%rbx, (%rsp)
	movq	%r15, 8(%rsp)
	movq	%r14, 16(%rsp)
	leaq	(%rsp), %rdx
	movq	$90, %rdi
	movq	$2, %rsi
	callq	ix86_expand_binary_operator
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	addq	$32, %rsp
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp5188:
	.size	gen_lshrqi3, .Ltmp5188-gen_lshrqi3
	.cfi_endproc

	.globl	gen_rotldi3
	.align	16, 0x90
	.type	gen_rotldi3,@function
gen_rotldi3:                            # @gen_rotldi3
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp5193:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp5194:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp5195:
	.cfi_def_cfa_offset 32
	subq	$32, %rsp
.Ltmp5196:
	.cfi_def_cfa_offset 64
.Ltmp5197:
	.cfi_offset %rbx, -32
.Ltmp5198:
	.cfi_offset %r14, -24
.Ltmp5199:
	.cfi_offset %r15, -16
	movq	%rdx, %r14
	movq	%rsi, %r15
	movq	%rdi, %rbx
	callq	start_sequence
	movq	%rbx, (%rsp)
	movq	%r15, 8(%rsp)
	movq	%r14, 16(%rsp)
	leaq	(%rsp), %rdx
	movq	$88, %rdi
	movq	$5, %rsi
	callq	ix86_expand_binary_operator
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	addq	$32, %rsp
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp5200:
	.size	gen_rotldi3, .Ltmp5200-gen_rotldi3
	.cfi_endproc

	.globl	gen_rotlsi3
	.align	16, 0x90
	.type	gen_rotlsi3,@function
gen_rotlsi3:                            # @gen_rotlsi3
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp5205:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp5206:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp5207:
	.cfi_def_cfa_offset 32
	subq	$32, %rsp
.Ltmp5208:
	.cfi_def_cfa_offset 64
.Ltmp5209:
	.cfi_offset %rbx, -32
.Ltmp5210:
	.cfi_offset %r14, -24
.Ltmp5211:
	.cfi_offset %r15, -16
	movq	%rdx, %r14
	movq	%rsi, %r15
	movq	%rdi, %rbx
	callq	start_sequence
	movq	%rbx, (%rsp)
	movq	%r15, 8(%rsp)
	movq	%r14, 16(%rsp)
	leaq	(%rsp), %rdx
	movq	$88, %rdi
	movq	$4, %rsi
	callq	ix86_expand_binary_operator
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	addq	$32, %rsp
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp5212:
	.size	gen_rotlsi3, .Ltmp5212-gen_rotlsi3
	.cfi_endproc

	.globl	gen_rotlhi3
	.align	16, 0x90
	.type	gen_rotlhi3,@function
gen_rotlhi3:                            # @gen_rotlhi3
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp5217:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp5218:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp5219:
	.cfi_def_cfa_offset 32
	subq	$32, %rsp
.Ltmp5220:
	.cfi_def_cfa_offset 64
.Ltmp5221:
	.cfi_offset %rbx, -32
.Ltmp5222:
	.cfi_offset %r14, -24
.Ltmp5223:
	.cfi_offset %r15, -16
	movq	%rdx, %r14
	movq	%rsi, %r15
	movq	%rdi, %rbx
	callq	start_sequence
	movq	%rbx, (%rsp)
	movq	%r15, 8(%rsp)
	movq	%r14, 16(%rsp)
	leaq	(%rsp), %rdx
	movq	$88, %rdi
	movq	$3, %rsi
	callq	ix86_expand_binary_operator
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	addq	$32, %rsp
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp5224:
	.size	gen_rotlhi3, .Ltmp5224-gen_rotlhi3
	.cfi_endproc

	.globl	gen_rotlqi3
	.align	16, 0x90
	.type	gen_rotlqi3,@function
gen_rotlqi3:                            # @gen_rotlqi3
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp5229:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp5230:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp5231:
	.cfi_def_cfa_offset 32
	subq	$32, %rsp
.Ltmp5232:
	.cfi_def_cfa_offset 64
.Ltmp5233:
	.cfi_offset %rbx, -32
.Ltmp5234:
	.cfi_offset %r14, -24
.Ltmp5235:
	.cfi_offset %r15, -16
	movq	%rdx, %r14
	movq	%rsi, %r15
	movq	%rdi, %rbx
	callq	start_sequence
	movq	%rbx, (%rsp)
	movq	%r15, 8(%rsp)
	movq	%r14, 16(%rsp)
	leaq	(%rsp), %rdx
	movq	$88, %rdi
	movq	$2, %rsi
	callq	ix86_expand_binary_operator
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	addq	$32, %rsp
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp5236:
	.size	gen_rotlqi3, .Ltmp5236-gen_rotlqi3
	.cfi_endproc

	.globl	gen_rotrdi3
	.align	16, 0x90
	.type	gen_rotrdi3,@function
gen_rotrdi3:                            # @gen_rotrdi3
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp5241:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp5242:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp5243:
	.cfi_def_cfa_offset 32
	subq	$32, %rsp
.Ltmp5244:
	.cfi_def_cfa_offset 64
.Ltmp5245:
	.cfi_offset %rbx, -32
.Ltmp5246:
	.cfi_offset %r14, -24
.Ltmp5247:
	.cfi_offset %r15, -16
	movq	%rdx, %r14
	movq	%rsi, %r15
	movq	%rdi, %rbx
	callq	start_sequence
	movq	%rbx, (%rsp)
	movq	%r15, 8(%rsp)
	movq	%r14, 16(%rsp)
	leaq	(%rsp), %rdx
	movq	$91, %rdi
	movq	$5, %rsi
	callq	ix86_expand_binary_operator
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	addq	$32, %rsp
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp5248:
	.size	gen_rotrdi3, .Ltmp5248-gen_rotrdi3
	.cfi_endproc

	.globl	gen_rotrsi3
	.align	16, 0x90
	.type	gen_rotrsi3,@function
gen_rotrsi3:                            # @gen_rotrsi3
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp5253:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp5254:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp5255:
	.cfi_def_cfa_offset 32
	subq	$32, %rsp
.Ltmp5256:
	.cfi_def_cfa_offset 64
.Ltmp5257:
	.cfi_offset %rbx, -32
.Ltmp5258:
	.cfi_offset %r14, -24
.Ltmp5259:
	.cfi_offset %r15, -16
	movq	%rdx, %r14
	movq	%rsi, %r15
	movq	%rdi, %rbx
	callq	start_sequence
	movq	%rbx, (%rsp)
	movq	%r15, 8(%rsp)
	movq	%r14, 16(%rsp)
	leaq	(%rsp), %rdx
	movq	$91, %rdi
	movq	$4, %rsi
	callq	ix86_expand_binary_operator
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	addq	$32, %rsp
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp5260:
	.size	gen_rotrsi3, .Ltmp5260-gen_rotrsi3
	.cfi_endproc

	.globl	gen_rotrhi3
	.align	16, 0x90
	.type	gen_rotrhi3,@function
gen_rotrhi3:                            # @gen_rotrhi3
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp5265:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp5266:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp5267:
	.cfi_def_cfa_offset 32
	subq	$32, %rsp
.Ltmp5268:
	.cfi_def_cfa_offset 64
.Ltmp5269:
	.cfi_offset %rbx, -32
.Ltmp5270:
	.cfi_offset %r14, -24
.Ltmp5271:
	.cfi_offset %r15, -16
	movq	%rdx, %r14
	movq	%rsi, %r15
	movq	%rdi, %rbx
	callq	start_sequence
	movq	%rbx, (%rsp)
	movq	%r15, 8(%rsp)
	movq	%r14, 16(%rsp)
	leaq	(%rsp), %rdx
	movq	$91, %rdi
	movq	$3, %rsi
	callq	ix86_expand_binary_operator
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	addq	$32, %rsp
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp5272:
	.size	gen_rotrhi3, .Ltmp5272-gen_rotrhi3
	.cfi_endproc

	.globl	gen_rotrqi3
	.align	16, 0x90
	.type	gen_rotrqi3,@function
gen_rotrqi3:                            # @gen_rotrqi3
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp5277:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp5278:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp5279:
	.cfi_def_cfa_offset 32
	subq	$32, %rsp
.Ltmp5280:
	.cfi_def_cfa_offset 64
.Ltmp5281:
	.cfi_offset %rbx, -32
.Ltmp5282:
	.cfi_offset %r14, -24
.Ltmp5283:
	.cfi_offset %r15, -16
	movq	%rdx, %r14
	movq	%rsi, %r15
	movq	%rdi, %rbx
	callq	start_sequence
	movq	%rbx, (%rsp)
	movq	%r15, 8(%rsp)
	movq	%r14, 16(%rsp)
	leaq	(%rsp), %rdx
	movq	$91, %rdi
	movq	$2, %rsi
	callq	ix86_expand_binary_operator
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	addq	$32, %rsp
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp5284:
	.size	gen_rotrqi3, .Ltmp5284-gen_rotrqi3
	.cfi_endproc

	.globl	gen_extv
	.align	16, 0x90
	.type	gen_extv,@function
gen_extv:                               # @gen_extv
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp5290:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp5291:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp5292:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp5293:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp5294:
	.cfi_def_cfa_offset 48
.Ltmp5295:
	.cfi_offset %rbx, -40
.Ltmp5296:
	.cfi_offset %r12, -32
.Ltmp5297:
	.cfi_offset %r14, -24
.Ltmp5298:
	.cfi_offset %r15, -16
	movq	%rcx, %r15
	movq	%rdx, %rbx
	movq	%rsi, %r12
	movq	%rdi, %r14
	callq	start_sequence
	movq	8(%rbx), %rax
	cmpq	$8, %rax
	jne	.LBB568_3
# BB#1:                                 # %lor.lhs.false
	movq	8(%r15), %rax
	cmpq	$8, %rax
	jne	.LBB568_3
# BB#2:                                 # %if.end
	xorq	%rsi, %rsi
	movq	%r12, %rdi
	callq	register_operand
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB568_3
# BB#5:                                 # %if.end13
	movq	$132, %rdi
	movq	$4, %rsi
	movq	%r12, %rdx
	movq	%rbx, %rcx
	movq	%r15, %r8
	callq	gen_rtx_fmt_eee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	jmp	.LBB568_4
.LBB568_3:                              # %if.then12
	callq	end_sequence
	xorq	%rax, %rax
.LBB568_4:                              # %return
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.Ltmp5299:
	.size	gen_extv, .Ltmp5299-gen_extv
	.cfi_endproc

	.globl	gen_extzv
	.align	16, 0x90
	.type	gen_extzv,@function
gen_extzv:                              # @gen_extzv
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp5305:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp5306:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp5307:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp5308:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp5309:
	.cfi_def_cfa_offset 48
.Ltmp5310:
	.cfi_offset %rbx, -40
.Ltmp5311:
	.cfi_offset %r12, -32
.Ltmp5312:
	.cfi_offset %r14, -24
.Ltmp5313:
	.cfi_offset %r15, -16
	movq	%rcx, %r15
	movq	%rdx, %rbx
	movq	%rsi, %r12
	movq	%rdi, %r14
	callq	start_sequence
	movq	8(%rbx), %rax
	cmpq	$8, %rax
	jne	.LBB569_3
# BB#1:                                 # %lor.lhs.false
	movq	8(%r15), %rax
	cmpq	$8, %rax
	jne	.LBB569_3
# BB#2:                                 # %if.end
	xorq	%rsi, %rsi
	movq	%r12, %rdi
	callq	register_operand
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB569_3
# BB#5:                                 # %if.end13
	movq	$133, %rdi
	movq	$4, %rsi
	movq	%r12, %rdx
	movq	%rbx, %rcx
	movq	%r15, %r8
	callq	gen_rtx_fmt_eee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	jmp	.LBB569_4
.LBB569_3:                              # %if.then12
	callq	end_sequence
	xorq	%rax, %rax
.LBB569_4:                              # %return
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.Ltmp5314:
	.size	gen_extzv, .Ltmp5314-gen_extzv
	.cfi_endproc

	.globl	gen_insv
	.align	16, 0x90
	.type	gen_insv,@function
gen_insv:                               # @gen_insv
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp5320:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp5321:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp5322:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp5323:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp5324:
	.cfi_def_cfa_offset 48
.Ltmp5325:
	.cfi_offset %rbx, -40
.Ltmp5326:
	.cfi_offset %r12, -32
.Ltmp5327:
	.cfi_offset %r14, -24
.Ltmp5328:
	.cfi_offset %r15, -16
	movq	%rcx, %r14
	movq	%rdx, %r15
	movq	%rsi, %rbx
	movq	%rdi, %r12
	callq	start_sequence
	movq	8(%rbx), %rax
	cmpq	$8, %rax
	jne	.LBB570_3
# BB#1:                                 # %lor.lhs.false
	movq	8(%r15), %rax
	cmpq	$8, %rax
	jne	.LBB570_3
# BB#2:                                 # %if.end
	xorq	%rsi, %rsi
	movq	%r12, %rdi
	callq	register_operand
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB570_3
# BB#5:                                 # %if.end13
	movq	$133, %rdi
	movq	$4, %rsi
	movq	%r12, %rdx
	movq	%rbx, %rcx
	movq	%r15, %r8
	callq	gen_rtx_fmt_eee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	movq	%r14, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	jmp	.LBB570_4
.LBB570_3:                              # %if.then12
	callq	end_sequence
	xorq	%rax, %rax
.LBB570_4:                              # %return
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.Ltmp5329:
	.size	gen_insv, .Ltmp5329-gen_insv
	.cfi_endproc

	.globl	gen_seq
	.align	16, 0x90
	.type	gen_seq,@function
gen_seq:                                # @gen_seq
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp5332:
	.cfi_def_cfa_offset 16
.Ltmp5333:
	.cfi_offset %rbx, -16
	movq	%rdi, %rbx
	callq	start_sequence
	movq	$103, %rdi
	movq	%rbx, %rsi
	callq	ix86_expand_setcc
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB571_2
# BB#1:                                 # %if.then
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	popq	%rbx
	retq
.LBB571_2:                              # %return
	callq	end_sequence
	xorq	%rax, %rax
	popq	%rbx
	retq
.Ltmp5334:
	.size	gen_seq, .Ltmp5334-gen_seq
	.cfi_endproc

	.globl	gen_sne
	.align	16, 0x90
	.type	gen_sne,@function
gen_sne:                                # @gen_sne
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp5337:
	.cfi_def_cfa_offset 16
.Ltmp5338:
	.cfi_offset %rbx, -16
	movq	%rdi, %rbx
	callq	start_sequence
	movq	$102, %rdi
	movq	%rbx, %rsi
	callq	ix86_expand_setcc
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB572_2
# BB#1:                                 # %if.then
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	popq	%rbx
	retq
.LBB572_2:                              # %return
	callq	end_sequence
	xorq	%rax, %rax
	popq	%rbx
	retq
.Ltmp5339:
	.size	gen_sne, .Ltmp5339-gen_sne
	.cfi_endproc

	.globl	gen_sgt
	.align	16, 0x90
	.type	gen_sgt,@function
gen_sgt:                                # @gen_sgt
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp5342:
	.cfi_def_cfa_offset 16
.Ltmp5343:
	.cfi_offset %rbx, -16
	movq	%rdi, %rbx
	callq	start_sequence
	movq	$105, %rdi
	movq	%rbx, %rsi
	callq	ix86_expand_setcc
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB573_2
# BB#1:                                 # %if.then
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	popq	%rbx
	retq
.LBB573_2:                              # %return
	callq	end_sequence
	xorq	%rax, %rax
	popq	%rbx
	retq
.Ltmp5344:
	.size	gen_sgt, .Ltmp5344-gen_sgt
	.cfi_endproc

	.globl	gen_sgtu
	.align	16, 0x90
	.type	gen_sgtu,@function
gen_sgtu:                               # @gen_sgtu
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp5347:
	.cfi_def_cfa_offset 16
.Ltmp5348:
	.cfi_offset %rbx, -16
	movq	%rdi, %rbx
	callq	start_sequence
	movq	$109, %rdi
	movq	%rbx, %rsi
	callq	ix86_expand_setcc
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB574_2
# BB#1:                                 # %if.then
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	popq	%rbx
	retq
.LBB574_2:                              # %return
	callq	end_sequence
	xorq	%rax, %rax
	popq	%rbx
	retq
.Ltmp5349:
	.size	gen_sgtu, .Ltmp5349-gen_sgtu
	.cfi_endproc

	.globl	gen_slt
	.align	16, 0x90
	.type	gen_slt,@function
gen_slt:                                # @gen_slt
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp5352:
	.cfi_def_cfa_offset 16
.Ltmp5353:
	.cfi_offset %rbx, -16
	movq	%rdi, %rbx
	callq	start_sequence
	movq	$107, %rdi
	movq	%rbx, %rsi
	callq	ix86_expand_setcc
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB575_2
# BB#1:                                 # %if.then
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	popq	%rbx
	retq
.LBB575_2:                              # %return
	callq	end_sequence
	xorq	%rax, %rax
	popq	%rbx
	retq
.Ltmp5354:
	.size	gen_slt, .Ltmp5354-gen_slt
	.cfi_endproc

	.globl	gen_sltu
	.align	16, 0x90
	.type	gen_sltu,@function
gen_sltu:                               # @gen_sltu
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp5357:
	.cfi_def_cfa_offset 16
.Ltmp5358:
	.cfi_offset %rbx, -16
	movq	%rdi, %rbx
	callq	start_sequence
	movq	$111, %rdi
	movq	%rbx, %rsi
	callq	ix86_expand_setcc
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB576_2
# BB#1:                                 # %if.then
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	popq	%rbx
	retq
.LBB576_2:                              # %return
	callq	end_sequence
	xorq	%rax, %rax
	popq	%rbx
	retq
.Ltmp5359:
	.size	gen_sltu, .Ltmp5359-gen_sltu
	.cfi_endproc

	.globl	gen_sge
	.align	16, 0x90
	.type	gen_sge,@function
gen_sge:                                # @gen_sge
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp5362:
	.cfi_def_cfa_offset 16
.Ltmp5363:
	.cfi_offset %rbx, -16
	movq	%rdi, %rbx
	callq	start_sequence
	movq	$104, %rdi
	movq	%rbx, %rsi
	callq	ix86_expand_setcc
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB577_2
# BB#1:                                 # %if.then
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	popq	%rbx
	retq
.LBB577_2:                              # %return
	callq	end_sequence
	xorq	%rax, %rax
	popq	%rbx
	retq
.Ltmp5364:
	.size	gen_sge, .Ltmp5364-gen_sge
	.cfi_endproc

	.globl	gen_sgeu
	.align	16, 0x90
	.type	gen_sgeu,@function
gen_sgeu:                               # @gen_sgeu
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp5367:
	.cfi_def_cfa_offset 16
.Ltmp5368:
	.cfi_offset %rbx, -16
	movq	%rdi, %rbx
	callq	start_sequence
	movq	$108, %rdi
	movq	%rbx, %rsi
	callq	ix86_expand_setcc
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB578_2
# BB#1:                                 # %if.then
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	popq	%rbx
	retq
.LBB578_2:                              # %return
	callq	end_sequence
	xorq	%rax, %rax
	popq	%rbx
	retq
.Ltmp5369:
	.size	gen_sgeu, .Ltmp5369-gen_sgeu
	.cfi_endproc

	.globl	gen_sle
	.align	16, 0x90
	.type	gen_sle,@function
gen_sle:                                # @gen_sle
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp5372:
	.cfi_def_cfa_offset 16
.Ltmp5373:
	.cfi_offset %rbx, -16
	movq	%rdi, %rbx
	callq	start_sequence
	movq	$106, %rdi
	movq	%rbx, %rsi
	callq	ix86_expand_setcc
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB579_2
# BB#1:                                 # %if.then
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	popq	%rbx
	retq
.LBB579_2:                              # %return
	callq	end_sequence
	xorq	%rax, %rax
	popq	%rbx
	retq
.Ltmp5374:
	.size	gen_sle, .Ltmp5374-gen_sle
	.cfi_endproc

	.globl	gen_sleu
	.align	16, 0x90
	.type	gen_sleu,@function
gen_sleu:                               # @gen_sleu
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp5377:
	.cfi_def_cfa_offset 16
.Ltmp5378:
	.cfi_offset %rbx, -16
	movq	%rdi, %rbx
	callq	start_sequence
	movq	$110, %rdi
	movq	%rbx, %rsi
	callq	ix86_expand_setcc
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB580_2
# BB#1:                                 # %if.then
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	popq	%rbx
	retq
.LBB580_2:                              # %return
	callq	end_sequence
	xorq	%rax, %rax
	popq	%rbx
	retq
.Ltmp5379:
	.size	gen_sleu, .Ltmp5379-gen_sleu
	.cfi_endproc

	.globl	gen_sunordered
	.align	16, 0x90
	.type	gen_sunordered,@function
gen_sunordered:                         # @gen_sunordered
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp5382:
	.cfi_def_cfa_offset 16
.Ltmp5383:
	.cfi_offset %rbx, -16
	movq	%rdi, %rbx
	callq	start_sequence
	movq	$112, %rdi
	movq	%rbx, %rsi
	callq	ix86_expand_setcc
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB581_2
# BB#1:                                 # %if.then
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	popq	%rbx
	retq
.LBB581_2:                              # %return
	callq	end_sequence
	xorq	%rax, %rax
	popq	%rbx
	retq
.Ltmp5384:
	.size	gen_sunordered, .Ltmp5384-gen_sunordered
	.cfi_endproc

	.globl	gen_sordered
	.align	16, 0x90
	.type	gen_sordered,@function
gen_sordered:                           # @gen_sordered
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp5387:
	.cfi_def_cfa_offset 16
.Ltmp5388:
	.cfi_offset %rbx, -16
	movq	%rdi, %rbx
	callq	start_sequence
	movq	$113, %rdi
	movq	%rbx, %rsi
	callq	ix86_expand_setcc
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB582_2
# BB#1:                                 # %if.then
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	popq	%rbx
	retq
.LBB582_2:                              # %return
	callq	end_sequence
	xorq	%rax, %rax
	popq	%rbx
	retq
.Ltmp5389:
	.size	gen_sordered, .Ltmp5389-gen_sordered
	.cfi_endproc

	.globl	gen_suneq
	.align	16, 0x90
	.type	gen_suneq,@function
gen_suneq:                              # @gen_suneq
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp5392:
	.cfi_def_cfa_offset 16
.Ltmp5393:
	.cfi_offset %rbx, -16
	movq	%rdi, %rbx
	callq	start_sequence
	movq	$114, %rdi
	movq	%rbx, %rsi
	callq	ix86_expand_setcc
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB583_2
# BB#1:                                 # %if.then
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	popq	%rbx
	retq
.LBB583_2:                              # %return
	callq	end_sequence
	xorq	%rax, %rax
	popq	%rbx
	retq
.Ltmp5394:
	.size	gen_suneq, .Ltmp5394-gen_suneq
	.cfi_endproc

	.globl	gen_sunge
	.align	16, 0x90
	.type	gen_sunge,@function
gen_sunge:                              # @gen_sunge
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp5397:
	.cfi_def_cfa_offset 16
.Ltmp5398:
	.cfi_offset %rbx, -16
	movq	%rdi, %rbx
	callq	start_sequence
	movq	$115, %rdi
	movq	%rbx, %rsi
	callq	ix86_expand_setcc
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB584_2
# BB#1:                                 # %if.then
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	popq	%rbx
	retq
.LBB584_2:                              # %return
	callq	end_sequence
	xorq	%rax, %rax
	popq	%rbx
	retq
.Ltmp5399:
	.size	gen_sunge, .Ltmp5399-gen_sunge
	.cfi_endproc

	.globl	gen_sungt
	.align	16, 0x90
	.type	gen_sungt,@function
gen_sungt:                              # @gen_sungt
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp5402:
	.cfi_def_cfa_offset 16
.Ltmp5403:
	.cfi_offset %rbx, -16
	movq	%rdi, %rbx
	callq	start_sequence
	movq	$116, %rdi
	movq	%rbx, %rsi
	callq	ix86_expand_setcc
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB585_2
# BB#1:                                 # %if.then
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	popq	%rbx
	retq
.LBB585_2:                              # %return
	callq	end_sequence
	xorq	%rax, %rax
	popq	%rbx
	retq
.Ltmp5404:
	.size	gen_sungt, .Ltmp5404-gen_sungt
	.cfi_endproc

	.globl	gen_sunle
	.align	16, 0x90
	.type	gen_sunle,@function
gen_sunle:                              # @gen_sunle
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp5407:
	.cfi_def_cfa_offset 16
.Ltmp5408:
	.cfi_offset %rbx, -16
	movq	%rdi, %rbx
	callq	start_sequence
	movq	$117, %rdi
	movq	%rbx, %rsi
	callq	ix86_expand_setcc
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB586_2
# BB#1:                                 # %if.then
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	popq	%rbx
	retq
.LBB586_2:                              # %return
	callq	end_sequence
	xorq	%rax, %rax
	popq	%rbx
	retq
.Ltmp5409:
	.size	gen_sunle, .Ltmp5409-gen_sunle
	.cfi_endproc

	.globl	gen_sunlt
	.align	16, 0x90
	.type	gen_sunlt,@function
gen_sunlt:                              # @gen_sunlt
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp5412:
	.cfi_def_cfa_offset 16
.Ltmp5413:
	.cfi_offset %rbx, -16
	movq	%rdi, %rbx
	callq	start_sequence
	movq	$118, %rdi
	movq	%rbx, %rsi
	callq	ix86_expand_setcc
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB587_2
# BB#1:                                 # %if.then
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	popq	%rbx
	retq
.LBB587_2:                              # %return
	callq	end_sequence
	xorq	%rax, %rax
	popq	%rbx
	retq
.Ltmp5414:
	.size	gen_sunlt, .Ltmp5414-gen_sunlt
	.cfi_endproc

	.globl	gen_sltgt
	.align	16, 0x90
	.type	gen_sltgt,@function
gen_sltgt:                              # @gen_sltgt
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp5417:
	.cfi_def_cfa_offset 16
.Ltmp5418:
	.cfi_offset %rbx, -16
	movq	%rdi, %rbx
	callq	start_sequence
	movq	$119, %rdi
	movq	%rbx, %rsi
	callq	ix86_expand_setcc
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB588_2
# BB#1:                                 # %if.then
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	popq	%rbx
	retq
.LBB588_2:                              # %return
	callq	end_sequence
	xorq	%rax, %rax
	popq	%rbx
	retq
.Ltmp5419:
	.size	gen_sltgt, .Ltmp5419-gen_sltgt
	.cfi_endproc

	.globl	gen_split_1133
	.align	16, 0x90
	.type	gen_split_1133,@function
gen_split_1133:                         # @gen_split_1133
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp5422:
	.cfi_def_cfa_offset 16
.Ltmp5423:
	.cfi_offset %rbx, -16
	movq	%rdi, %rbx
	callq	start_sequence
	movq	8(%rbx), %rax
	movq	$2, %rcx
	movb	%cl, 2(%rax)
	movq	(%rbx), %rdx
	movq	8(%rbx), %rcx
	movq	$47, %rdi
	xorq	%rsi, %rsi
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	popq	%rbx
	retq
.Ltmp5424:
	.size	gen_split_1133, .Ltmp5424-gen_split_1133
	.cfi_endproc

	.globl	gen_split_1134
	.align	16, 0x90
	.type	gen_split_1134,@function
gen_split_1134:                         # @gen_split_1134
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp5427:
	.cfi_def_cfa_offset 16
.Ltmp5428:
	.cfi_offset %rbx, -16
	movq	%rdi, %rbx
	callq	start_sequence
	movq	8(%rbx), %rax
	movq	$2, %rcx
	movb	%cl, 2(%rax)
	movq	(%rbx), %rdx
	movq	8(%rbx), %rcx
	movq	$47, %rdi
	xorq	%rsi, %rsi
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	popq	%rbx
	retq
.Ltmp5429:
	.size	gen_split_1134, .Ltmp5429-gen_split_1134
	.cfi_endproc

	.globl	gen_split_1135
	.align	16, 0x90
	.type	gen_split_1135,@function
gen_split_1135:                         # @gen_split_1135
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp5434:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp5435:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp5436:
	.cfi_def_cfa_offset 32
.Ltmp5437:
	.cfi_offset %rbx, -32
.Ltmp5438:
	.cfi_offset %r14, -24
.Ltmp5439:
	.cfi_offset %r15, -16
	movq	%rdi, %r14
	callq	start_sequence
	movq	8(%r14), %rdi
	callq	copy_rtx
	movq	%rax, %rbx
	movq	%rbx, 8(%r14)
	movzbl	(%rbx), %edi
	movq	$2, %rax
	movb	%al, 2(%rbx)
	movq	8(%rbx), %rax
	movzbl	2(%rax), %eax
	addq	$-57, %rax
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	andq	%r15, %rax
	cmpq	$2, %rax
	jae	.LBB591_1
# BB#2:                                 # %cond.false
	callq	reverse_condition_maybe_unordered
	jmp	.LBB591_3
.LBB591_1:                              # %cond.true
	callq	reverse_condition
.LBB591_3:                              # %cond.end
	movw	%ax, (%rbx)
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	ix86_comparison_operator
	testq	%r15, %rax
	je	.LBB591_5
# BB#4:                                 # %if.end
	movq	(%r14), %rdx
	movq	8(%r14), %rcx
	movq	$47, %rdi
	xorq	%rsi, %rsi
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	jmp	.LBB591_6
.LBB591_5:                              # %return
	callq	end_sequence
	xorq	%rax, %rax
.LBB591_6:                              # %return
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp5440:
	.size	gen_split_1135, .Ltmp5440-gen_split_1135
	.cfi_endproc

	.globl	gen_split_1136
	.align	16, 0x90
	.type	gen_split_1136,@function
gen_split_1136:                         # @gen_split_1136
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp5445:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp5446:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp5447:
	.cfi_def_cfa_offset 32
.Ltmp5448:
	.cfi_offset %rbx, -32
.Ltmp5449:
	.cfi_offset %r14, -24
.Ltmp5450:
	.cfi_offset %r15, -16
	movq	%rdi, %r14
	callq	start_sequence
	movq	8(%r14), %rdi
	callq	copy_rtx
	movq	%rax, %rbx
	movq	%rbx, 8(%r14)
	movzbl	(%rbx), %edi
	movq	$2, %rax
	movb	%al, 2(%rbx)
	movq	8(%rbx), %rax
	movzbl	2(%rax), %eax
	addq	$-57, %rax
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	andq	%r15, %rax
	cmpq	$2, %rax
	jae	.LBB592_1
# BB#2:                                 # %cond.false
	callq	reverse_condition_maybe_unordered
	jmp	.LBB592_3
.LBB592_1:                              # %cond.true
	callq	reverse_condition
.LBB592_3:                              # %cond.end
	movw	%ax, (%rbx)
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	ix86_comparison_operator
	testq	%r15, %rax
	je	.LBB592_5
# BB#4:                                 # %if.end
	movq	(%r14), %rdx
	movq	8(%r14), %rcx
	movq	$47, %rdi
	xorq	%rsi, %rsi
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	jmp	.LBB592_6
.LBB592_5:                              # %return
	callq	end_sequence
	xorq	%rax, %rax
.LBB592_6:                              # %return
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp5451:
	.size	gen_split_1136, .Ltmp5451-gen_split_1136
	.cfi_endproc

	.globl	gen_beq
	.align	16, 0x90
	.type	gen_beq,@function
gen_beq:                                # @gen_beq
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp5454:
	.cfi_def_cfa_offset 16
.Ltmp5455:
	.cfi_offset %rbx, -16
	movq	%rdi, %rbx
	callq	start_sequence
	movq	$103, %rdi
	movq	%rbx, %rsi
	callq	ix86_expand_branch
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	popq	%rbx
	retq
.Ltmp5456:
	.size	gen_beq, .Ltmp5456-gen_beq
	.cfi_endproc

	.globl	gen_bne
	.align	16, 0x90
	.type	gen_bne,@function
gen_bne:                                # @gen_bne
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp5459:
	.cfi_def_cfa_offset 16
.Ltmp5460:
	.cfi_offset %rbx, -16
	movq	%rdi, %rbx
	callq	start_sequence
	movq	$102, %rdi
	movq	%rbx, %rsi
	callq	ix86_expand_branch
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	popq	%rbx
	retq
.Ltmp5461:
	.size	gen_bne, .Ltmp5461-gen_bne
	.cfi_endproc

	.globl	gen_bgt
	.align	16, 0x90
	.type	gen_bgt,@function
gen_bgt:                                # @gen_bgt
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp5464:
	.cfi_def_cfa_offset 16
.Ltmp5465:
	.cfi_offset %rbx, -16
	movq	%rdi, %rbx
	callq	start_sequence
	movq	$105, %rdi
	movq	%rbx, %rsi
	callq	ix86_expand_branch
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	popq	%rbx
	retq
.Ltmp5466:
	.size	gen_bgt, .Ltmp5466-gen_bgt
	.cfi_endproc

	.globl	gen_bgtu
	.align	16, 0x90
	.type	gen_bgtu,@function
gen_bgtu:                               # @gen_bgtu
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp5469:
	.cfi_def_cfa_offset 16
.Ltmp5470:
	.cfi_offset %rbx, -16
	movq	%rdi, %rbx
	callq	start_sequence
	movq	$109, %rdi
	movq	%rbx, %rsi
	callq	ix86_expand_branch
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	popq	%rbx
	retq
.Ltmp5471:
	.size	gen_bgtu, .Ltmp5471-gen_bgtu
	.cfi_endproc

	.globl	gen_blt
	.align	16, 0x90
	.type	gen_blt,@function
gen_blt:                                # @gen_blt
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp5474:
	.cfi_def_cfa_offset 16
.Ltmp5475:
	.cfi_offset %rbx, -16
	movq	%rdi, %rbx
	callq	start_sequence
	movq	$107, %rdi
	movq	%rbx, %rsi
	callq	ix86_expand_branch
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	popq	%rbx
	retq
.Ltmp5476:
	.size	gen_blt, .Ltmp5476-gen_blt
	.cfi_endproc

	.globl	gen_bltu
	.align	16, 0x90
	.type	gen_bltu,@function
gen_bltu:                               # @gen_bltu
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp5479:
	.cfi_def_cfa_offset 16
.Ltmp5480:
	.cfi_offset %rbx, -16
	movq	%rdi, %rbx
	callq	start_sequence
	movq	$111, %rdi
	movq	%rbx, %rsi
	callq	ix86_expand_branch
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	popq	%rbx
	retq
.Ltmp5481:
	.size	gen_bltu, .Ltmp5481-gen_bltu
	.cfi_endproc

	.globl	gen_bge
	.align	16, 0x90
	.type	gen_bge,@function
gen_bge:                                # @gen_bge
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp5484:
	.cfi_def_cfa_offset 16
.Ltmp5485:
	.cfi_offset %rbx, -16
	movq	%rdi, %rbx
	callq	start_sequence
	movq	$104, %rdi
	movq	%rbx, %rsi
	callq	ix86_expand_branch
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	popq	%rbx
	retq
.Ltmp5486:
	.size	gen_bge, .Ltmp5486-gen_bge
	.cfi_endproc

	.globl	gen_bgeu
	.align	16, 0x90
	.type	gen_bgeu,@function
gen_bgeu:                               # @gen_bgeu
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp5489:
	.cfi_def_cfa_offset 16
.Ltmp5490:
	.cfi_offset %rbx, -16
	movq	%rdi, %rbx
	callq	start_sequence
	movq	$108, %rdi
	movq	%rbx, %rsi
	callq	ix86_expand_branch
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	popq	%rbx
	retq
.Ltmp5491:
	.size	gen_bgeu, .Ltmp5491-gen_bgeu
	.cfi_endproc

	.globl	gen_ble
	.align	16, 0x90
	.type	gen_ble,@function
gen_ble:                                # @gen_ble
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp5494:
	.cfi_def_cfa_offset 16
.Ltmp5495:
	.cfi_offset %rbx, -16
	movq	%rdi, %rbx
	callq	start_sequence
	movq	$106, %rdi
	movq	%rbx, %rsi
	callq	ix86_expand_branch
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	popq	%rbx
	retq
.Ltmp5496:
	.size	gen_ble, .Ltmp5496-gen_ble
	.cfi_endproc

	.globl	gen_bleu
	.align	16, 0x90
	.type	gen_bleu,@function
gen_bleu:                               # @gen_bleu
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp5499:
	.cfi_def_cfa_offset 16
.Ltmp5500:
	.cfi_offset %rbx, -16
	movq	%rdi, %rbx
	callq	start_sequence
	movq	$110, %rdi
	movq	%rbx, %rsi
	callq	ix86_expand_branch
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	popq	%rbx
	retq
.Ltmp5501:
	.size	gen_bleu, .Ltmp5501-gen_bleu
	.cfi_endproc

	.globl	gen_bunordered
	.align	16, 0x90
	.type	gen_bunordered,@function
gen_bunordered:                         # @gen_bunordered
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp5504:
	.cfi_def_cfa_offset 16
.Ltmp5505:
	.cfi_offset %rbx, -16
	movq	%rdi, %rbx
	callq	start_sequence
	movq	$112, %rdi
	movq	%rbx, %rsi
	callq	ix86_expand_branch
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	popq	%rbx
	retq
.Ltmp5506:
	.size	gen_bunordered, .Ltmp5506-gen_bunordered
	.cfi_endproc

	.globl	gen_bordered
	.align	16, 0x90
	.type	gen_bordered,@function
gen_bordered:                           # @gen_bordered
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp5509:
	.cfi_def_cfa_offset 16
.Ltmp5510:
	.cfi_offset %rbx, -16
	movq	%rdi, %rbx
	callq	start_sequence
	movq	$113, %rdi
	movq	%rbx, %rsi
	callq	ix86_expand_branch
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	popq	%rbx
	retq
.Ltmp5511:
	.size	gen_bordered, .Ltmp5511-gen_bordered
	.cfi_endproc

	.globl	gen_buneq
	.align	16, 0x90
	.type	gen_buneq,@function
gen_buneq:                              # @gen_buneq
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp5514:
	.cfi_def_cfa_offset 16
.Ltmp5515:
	.cfi_offset %rbx, -16
	movq	%rdi, %rbx
	callq	start_sequence
	movq	$114, %rdi
	movq	%rbx, %rsi
	callq	ix86_expand_branch
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	popq	%rbx
	retq
.Ltmp5516:
	.size	gen_buneq, .Ltmp5516-gen_buneq
	.cfi_endproc

	.globl	gen_bunge
	.align	16, 0x90
	.type	gen_bunge,@function
gen_bunge:                              # @gen_bunge
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp5519:
	.cfi_def_cfa_offset 16
.Ltmp5520:
	.cfi_offset %rbx, -16
	movq	%rdi, %rbx
	callq	start_sequence
	movq	$115, %rdi
	movq	%rbx, %rsi
	callq	ix86_expand_branch
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	popq	%rbx
	retq
.Ltmp5521:
	.size	gen_bunge, .Ltmp5521-gen_bunge
	.cfi_endproc

	.globl	gen_bungt
	.align	16, 0x90
	.type	gen_bungt,@function
gen_bungt:                              # @gen_bungt
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp5524:
	.cfi_def_cfa_offset 16
.Ltmp5525:
	.cfi_offset %rbx, -16
	movq	%rdi, %rbx
	callq	start_sequence
	movq	$116, %rdi
	movq	%rbx, %rsi
	callq	ix86_expand_branch
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	popq	%rbx
	retq
.Ltmp5526:
	.size	gen_bungt, .Ltmp5526-gen_bungt
	.cfi_endproc

	.globl	gen_bunle
	.align	16, 0x90
	.type	gen_bunle,@function
gen_bunle:                              # @gen_bunle
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp5529:
	.cfi_def_cfa_offset 16
.Ltmp5530:
	.cfi_offset %rbx, -16
	movq	%rdi, %rbx
	callq	start_sequence
	movq	$117, %rdi
	movq	%rbx, %rsi
	callq	ix86_expand_branch
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	popq	%rbx
	retq
.Ltmp5531:
	.size	gen_bunle, .Ltmp5531-gen_bunle
	.cfi_endproc

	.globl	gen_bunlt
	.align	16, 0x90
	.type	gen_bunlt,@function
gen_bunlt:                              # @gen_bunlt
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp5534:
	.cfi_def_cfa_offset 16
.Ltmp5535:
	.cfi_offset %rbx, -16
	movq	%rdi, %rbx
	callq	start_sequence
	movq	$118, %rdi
	movq	%rbx, %rsi
	callq	ix86_expand_branch
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	popq	%rbx
	retq
.Ltmp5536:
	.size	gen_bunlt, .Ltmp5536-gen_bunlt
	.cfi_endproc

	.globl	gen_bltgt
	.align	16, 0x90
	.type	gen_bltgt,@function
gen_bltgt:                              # @gen_bltgt
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp5539:
	.cfi_def_cfa_offset 16
.Ltmp5540:
	.cfi_offset %rbx, -16
	movq	%rdi, %rbx
	callq	start_sequence
	movq	$119, %rdi
	movq	%rbx, %rsi
	callq	ix86_expand_branch
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	popq	%rbx
	retq
.Ltmp5541:
	.size	gen_bltgt, .Ltmp5541-gen_bltgt
	.cfi_endproc

	.globl	gen_split_1155
	.align	16, 0x90
	.type	gen_split_1155,@function
gen_split_1155:                         # @gen_split_1155
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp5545:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp5546:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp5547:
	.cfi_def_cfa_offset 32
.Ltmp5548:
	.cfi_offset %rbx, -24
.Ltmp5549:
	.cfi_offset %r14, -16
	movq	%rdi, %rbx
	callq	start_sequence
	movq	(%rbx), %rax
	movl	(%rax), %ecx
	movabsq	$4278255615, %rdx       # imm = 0xFF00FFFF
	andq	%rcx, %rdx
	movl	%edx, (%rax)
	movq	(%rbx), %r14
	movq	8(%rbx), %rdx
	movq	global_rtl(%rip), %rbx
	movq	$67, %rdi
	xorq	%rsi, %rsi
	callq	gen_rtx_fmt_u00
	movq	global_rtl(%rip), %r8
	movq	$72, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_eee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_jump_insn
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp5550:
	.size	gen_split_1155, .Ltmp5550-gen_split_1155
	.cfi_endproc

	.globl	gen_split_1156
	.align	16, 0x90
	.type	gen_split_1156,@function
gen_split_1156:                         # @gen_split_1156
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp5555:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp5556:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp5557:
	.cfi_def_cfa_offset 32
.Ltmp5558:
	.cfi_offset %rbx, -32
.Ltmp5559:
	.cfi_offset %r14, -24
.Ltmp5560:
	.cfi_offset %r15, -16
	movq	%rdi, %r14
	callq	start_sequence
	movq	(%r14), %rdi
	callq	copy_rtx
	movq	%rax, %rbx
	movq	%rbx, (%r14)
	movl	(%rbx), %edi
	movabsq	$4278255615, %rax       # imm = 0xFF00FFFF
	andq	%rdi, %rax
	movl	%eax, (%rbx)
	movq	8(%rbx), %rax
	movzbl	2(%rax), %eax
	addq	$-57, %rax
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	andq	%r15, %rax
	andq	$65535, %rdi            # imm = 0xFFFF
	cmpq	$2, %rax
	jae	.LBB612_1
# BB#2:                                 # %cond.false
	callq	reverse_condition_maybe_unordered
	jmp	.LBB612_3
.LBB612_1:                              # %cond.true
	callq	reverse_condition
.LBB612_3:                              # %cond.end
	movw	%ax, (%rbx)
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	ix86_comparison_operator
	testq	%r15, %rax
	je	.LBB612_5
# BB#4:                                 # %if.end
	movq	(%r14), %rbx
	movq	8(%r14), %rdx
	movq	global_rtl(%rip), %r14
	movq	$67, %rdi
	xorq	%rsi, %rsi
	callq	gen_rtx_fmt_u00
	movq	global_rtl(%rip), %r8
	movq	$72, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_eee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_jump_insn
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	jmp	.LBB612_6
.LBB612_5:                              # %return
	callq	end_sequence
	xorq	%rax, %rax
.LBB612_6:                              # %return
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp5561:
	.size	gen_split_1156, .Ltmp5561-gen_split_1156
	.cfi_endproc

	.globl	gen_split_1157
	.align	16, 0x90
	.type	gen_split_1157,@function
gen_split_1157:                         # @gen_split_1157
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp5564:
	.cfi_def_cfa_offset 16
.Ltmp5565:
	.cfi_offset %rbx, -16
	movq	%rdi, %rbx
	callq	start_sequence
	movq	(%rbx), %rax
	movq	8(%rbx), %rsi
	movq	16(%rbx), %rdx
	movq	24(%rbx), %rcx
	movq	32(%rbx), %r8
	movzbl	(%rax), %edi
	xorq	%r9, %r9
	callq	ix86_split_fp_branch
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	popq	%rbx
	retq
.Ltmp5566:
	.size	gen_split_1157, .Ltmp5566-gen_split_1157
	.cfi_endproc

	.globl	gen_split_1158
	.align	16, 0x90
	.type	gen_split_1158,@function
gen_split_1158:                         # @gen_split_1158
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp5569:
	.cfi_def_cfa_offset 16
.Ltmp5570:
	.cfi_offset %rbx, -16
	movq	%rdi, %rbx
	callq	start_sequence
	movq	(%rbx), %rax
	movq	8(%rbx), %rsi
	movq	16(%rbx), %rdx
	movq	24(%rbx), %rcx
	movq	32(%rbx), %r8
	movq	40(%rbx), %r9
	movzbl	(%rax), %edi
	callq	ix86_split_fp_branch
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	popq	%rbx
	retq
.Ltmp5571:
	.size	gen_split_1158, .Ltmp5571-gen_split_1158
	.cfi_endproc

	.globl	gen_indirect_jump
	.align	16, 0x90
	.type	gen_indirect_jump,@function
gen_indirect_jump:                      # @gen_indirect_jump
	.cfi_startproc
# BB#0:                                 # %entry
	movq	global_rtl(%rip), %rdx
	movq	%rdi, %rax
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rcx
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp5572:
	.size	gen_indirect_jump, .Ltmp5572-gen_indirect_jump
	.cfi_endproc

	.globl	gen_tablejump
	.align	16, 0x90
	.type	gen_tablejump,@function
gen_tablejump:                          # @gen_tablejump
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp5577:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp5578:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp5579:
	.cfi_def_cfa_offset 32
	subq	$16, %rsp
.Ltmp5580:
	.cfi_def_cfa_offset 48
.Ltmp5581:
	.cfi_offset %rbx, -32
.Ltmp5582:
	.cfi_offset %r14, -24
.Ltmp5583:
	.cfi_offset %r15, -16
	movq	%rsi, %r14
	movq	%rdi, %r15
	callq	start_sequence
	movl	flag_pic(%rip), %eax
	testq	%rax, %rax
	je	.LBB616_4
# BB#1:                                 # %if.then
	movl	target_flags(%rip), %eax
	movq	%rax, %rbx
	andq	$33554432, %rbx         # imm = 0x2000000
	movq	$25, %rcx
	shrq	%cl, %rbx
	orq	$4, %rbx
	andq	$33554432, %rax         # imm = 0x2000000
	je	.LBB616_3
# BB#2:                                 # %if.then3
	movq	$67, %rdi
	movq	%rbx, %rsi
	movq	%r14, %rdx
	callq	gen_rtx_fmt_u00
	movq	$75, %rsi
	xorq	%r8, %r8
	xorq	%r9, %r9
	movq	$0, (%rsp)
	movq	%rbx, %rdi
	movq	%r15, %rdx
	movq	%rax, %rcx
	callq	expand_simple_binop
	movq	%rax, %r15
	jmp	.LBB616_4
.LBB616_3:                              # %if.else
	movq	pic_offset_table_rtx(%rip), %rdx
	movq	$76, %rsi
	xorq	%r8, %r8
	movq	$1, %r9
	movq	$0, (%rsp)
	movq	%rbx, %rdi
	movq	%r15, %rcx
	callq	expand_simple_binop
	movq	cfun(%rip), %rcx
	movl	424(%rcx), %edx
	movq	%rax, %r15
	orq	$2097152, %rdx          # imm = 0x200000
	movl	%edx, 424(%rcx)
.LBB616_4:                              # %if.end19
	movq	global_rtl(%rip), %rdx
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r15, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rbx
	movq	$67, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	callq	gen_rtx_fmt_u00
	movq	$48, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rcx
	movq	$2, %rdi
	xorq	%rax, %rax
	movq	%rbx, %rsi
	movq	%rcx, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_E
	movq	%rax, %rdi
	callq	emit_jump_insn
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	addq	$16, %rsp
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp5584:
	.size	gen_tablejump, .Ltmp5584-gen_tablejump
	.cfi_endproc

	.globl	gen_doloop_end
	.align	16, 0x90
	.type	gen_doloop_end,@function
gen_doloop_end:                         # @gen_doloop_end
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp5589:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp5590:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp5591:
	.cfi_def_cfa_offset 32
.Ltmp5592:
	.cfi_offset %rbx, -32
.Ltmp5593:
	.cfi_offset %r14, -24
.Ltmp5594:
	.cfi_offset %r15, -16
	movq	%r8, %r14
	movq	%rcx, %rbx
	movq	%rdi, %r15
	callq	start_sequence
	movq	8(%rbx), %rax
	cmpq	$2, %rax
	jge	.LBB617_2
# BB#1:                                 # %if.end
	movl	(%r15), %eax
	andq	$16711680, %rax         # imm = 0xFF0000
	cmpq	$262144, %rax           # imm = 0x40000
	jne	.LBB617_2
# BB#4:                                 # %if.end10
	movq	%r14, %rdi
	movq	%r15, %rsi
	movq	%r15, %rdx
	callq	gen_doloop_end_internal
	movq	%rax, %rdi
	callq	emit_jump_insn
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	jmp	.LBB617_3
.LBB617_2:                              # %if.then9
	callq	end_sequence
	xorq	%rax, %rax
.LBB617_3:                              # %cleanup
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp5595:
	.size	gen_doloop_end, .Ltmp5595-gen_doloop_end
	.cfi_endproc

	.globl	gen_split_1162
	.align	16, 0x90
	.type	gen_split_1162,@function
gen_split_1162:                         # @gen_split_1162
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp5601:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp5602:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp5603:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp5604:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp5605:
	.cfi_def_cfa_offset 48
.Ltmp5606:
	.cfi_offset %rbx, -40
.Ltmp5607:
	.cfi_offset %r12, -32
.Ltmp5608:
	.cfi_offset %r14, -24
.Ltmp5609:
	.cfi_offset %r15, -16
	movq	%rdi, %rbx
	callq	start_sequence
	movq	(%rbx), %r14
	movq	8(%rbx), %rbx
	movq	$56, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	const_int_rtx+504(%rip), %rcx
	movq	%rax, %r15
	movq	$75, %rdi
	movq	$4, %rsi
	movq	%rbx, %rdx
	callq	gen_rtx_fmt_ee
	movq	const_int_rtx+512(%rip), %rcx
	movq	$74, %rdi
	movq	$56, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r15, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %r15
	movq	%rbx, %rdi
	callq	copy_rtx
	movq	%rax, %r12
	movq	%rbx, %rdi
	callq	copy_rtx
	movq	const_int_rtx+504(%rip), %rcx
	movq	$75, %rdi
	movq	$4, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r12, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rcx
	movq	$2, %rdi
	xorq	%rax, %rax
	movq	%r15, %rsi
	movq	%rcx, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_E
	movq	%rax, %rdi
	callq	emit
	movq	global_rtl(%rip), %rbx
	movq	$56, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	const_int_rtx+512(%rip), %rcx
	movq	$102, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_ee
	movq	global_rtl(%rip), %r8
	movq	$72, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	movq	%r14, %rcx
	callq	gen_rtx_fmt_eee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_jump_insn
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.Ltmp5610:
	.size	gen_split_1162, .Ltmp5610-gen_split_1162
	.cfi_endproc

	.globl	gen_split_1163
	.align	16, 0x90
	.type	gen_split_1163,@function
gen_split_1163:                         # @gen_split_1163
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp5617:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp5618:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp5619:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp5620:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp5621:
	.cfi_def_cfa_offset 48
.Ltmp5622:
	.cfi_offset %rbx, -48
.Ltmp5623:
	.cfi_offset %r12, -40
.Ltmp5624:
	.cfi_offset %r13, -32
.Ltmp5625:
	.cfi_offset %r14, -24
.Ltmp5626:
	.cfi_offset %r15, -16
	movq	%rdi, %rbx
	callq	start_sequence
	movq	(%rbx), %r14
	movq	8(%rbx), %rcx
	movq	16(%rbx), %r15
	movq	24(%rbx), %r13
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r13, %rdx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	movq	$56, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	%rax, %r12
	movq	%r13, %rdi
	callq	copy_rtx
	movq	const_int_rtx+504(%rip), %rcx
	movq	$75, %rdi
	movq	$4, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_ee
	movq	const_int_rtx+512(%rip), %rcx
	movq	$74, %rdi
	movq	$56, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r12, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %r12
	movq	%r13, %rdi
	callq	copy_rtx
	movq	%rax, %rbx
	movq	%r13, %rdi
	callq	copy_rtx
	movq	const_int_rtx+504(%rip), %rcx
	movq	$75, %rdi
	movq	$4, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rcx
	movq	$2, %rdi
	xorq	%rax, %rax
	movq	%r12, %rsi
	movq	%rcx, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_E
	movq	%rax, %rdi
	callq	emit
	movq	%r13, %rdi
	callq	copy_rtx
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r15, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	movq	global_rtl(%rip), %rbx
	movq	$56, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	const_int_rtx+512(%rip), %rcx
	movq	$102, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_ee
	movq	global_rtl(%rip), %r8
	movq	$72, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	movq	%r14, %rcx
	callq	gen_rtx_fmt_eee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_jump_insn
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp5627:
	.size	gen_split_1163, .Ltmp5627-gen_split_1163
	.cfi_endproc

	.globl	gen_peephole2_1164
	.align	16, 0x90
	.type	gen_peephole2_1164,@function
gen_peephole2_1164:                     # @gen_peephole2_1164
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp5631:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp5632:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp5633:
	.cfi_def_cfa_offset 32
.Ltmp5634:
	.cfi_offset %rbx, -24
.Ltmp5635:
	.cfi_offset %r14, -16
	movq	%rsi, %rbx
	callq	start_sequence
	movq	(%rbx), %rax
	movzbl	2(%rax), %edi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	%rax, 32(%rbx)
	movq	24(%rbx), %rax
	movl	8(%rax), %esi
	movq	$2, %rdi
	callq	gen_rtx_REG
	movq	%rax, 40(%rbx)
	movq	24(%rbx), %rdi
	callq	ix86_expand_clear
	movq	(%rbx), %rcx
	movq	16(%rbx), %r14
	movq	32(%rbx), %rdx
	movq	40(%rbx), %rbx
	movq	$47, %rdi
	xorq	%rsi, %rsi
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	movq	$64, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	callq	gen_rtx_fmt_e
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	movq	%r14, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp5636:
	.size	gen_peephole2_1164, .Ltmp5636-gen_peephole2_1164
	.cfi_endproc

	.globl	gen_peephole2_1165
	.align	16, 0x90
	.type	gen_peephole2_1165,@function
gen_peephole2_1165:                     # @gen_peephole2_1165
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp5640:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp5641:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp5642:
	.cfi_def_cfa_offset 32
.Ltmp5643:
	.cfi_offset %rbx, -24
.Ltmp5644:
	.cfi_offset %r14, -16
	movq	%rsi, %rbx
	callq	start_sequence
	movq	(%rbx), %rax
	movzbl	2(%rax), %edi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	%rax, 32(%rbx)
	movq	24(%rbx), %rax
	movl	8(%rax), %esi
	movq	$2, %rdi
	callq	gen_rtx_REG
	movq	%rax, 40(%rbx)
	movq	24(%rbx), %rdi
	callq	ix86_expand_clear
	movq	(%rbx), %rcx
	movq	16(%rbx), %r14
	movq	32(%rbx), %rdx
	movq	40(%rbx), %rbx
	movq	$47, %rdi
	xorq	%rsi, %rsi
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	movq	$64, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	callq	gen_rtx_fmt_e
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	movq	%r14, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp5645:
	.size	gen_peephole2_1165, .Ltmp5645-gen_peephole2_1165
	.cfi_endproc

	.globl	gen_call_pop
	.align	16, 0x90
	.type	gen_call_pop,@function
gen_call_pop:                           # @gen_call_pop
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp5650:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp5651:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp5652:
	.cfi_def_cfa_offset 32
.Ltmp5653:
	.cfi_offset %rbx, -32
.Ltmp5654:
	.cfi_offset %r14, -24
.Ltmp5655:
	.cfi_offset %r15, -16
	movq	%rcx, %r15
	movq	%rsi, %r14
	movq	%rdi, %rbx
	callq	start_sequence
	movq	const_int_rtx+512(%rip), %rax
	cmpq	%r15, %rax
	je	.LBB622_1
# BB#3:                                 # %if.end
	movl	flag_pic(%rip), %eax
	testq	%rax, %rax
	je	.LBB622_6
# BB#4:                                 # %land.lhs.true
	movq	8(%rbx), %rax
	movl	(%rax), %eax
	andq	$134283263, %rax        # imm = 0x800FFFF
	cmpq	$68, %rax
	jne	.LBB622_6
# BB#5:                                 # %if.then20
	movq	cfun(%rip), %rax
	movl	424(%rax), %ecx
	orq	$2097152, %rcx          # imm = 0x200000
	movl	%ecx, 424(%rax)
.LBB622_6:                              # %if.end23
	movl	target_flags(%rip), %esi
	movq	8(%rbx), %rdi
	movq	$25, %rcx
	shrq	%cl, %rsi
	andq	$1, %rsi
	orq	$4, %rsi
	callq	call_insn_operand
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB622_8
# BB#7:                                 # %if.then31
	movl	target_flags(%rip), %edi
	movq	$25, %rcx
	movq	8(%rbx), %rsi
	shrq	%cl, %rdi
	andq	$1, %rdi
	orq	$4, %rdi
	callq	copy_to_mode_reg
	movq	%rax, 8(%rbx)
.LBB622_8:                              # %if.end44
	movzbl	target_flags+3(%rip), %eax
	testq	$2, %rax
	jne	.LBB622_10
# BB#9:                                 # %if.end48
	movq	$50, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%r14, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %r14
	movq	$4, %rdi
	movq	$7, %rsi
	callq	gen_rtx_REG
	movq	%rax, %rbx
	movq	$4, %rdi
	movq	$7, %rsi
	callq	gen_rtx_REG
	movq	$75, %rdi
	movq	$4, %rsi
	movq	%rax, %rdx
	movq	%r15, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rcx
	movq	$2, %rdi
	xorq	%rax, %rax
	movq	%r14, %rsi
	movq	%rcx, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_E
	movq	%rax, %rdi
	callq	emit_call_insn
	jmp	.LBB622_2
.LBB622_1:                              # %if.then
	movq	const_int_rtx+504(%rip), %rdx
	movq	%rbx, %rdi
	movq	%r14, %rsi
	callq	gen_call
	movq	%rax, %rdi
	callq	emit_insn
.LBB622_2:                              # %if.then
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.LBB622_10:                             # %if.then47
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.gen_call_pop, %rcx
	movq	$14203, %rsi            # imm = 0x377B
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp5656:
	.size	gen_call_pop, .Ltmp5656-gen_call_pop
	.cfi_endproc

	.globl	gen_call
	.align	16, 0x90
	.type	gen_call,@function
gen_call:                               # @gen_call
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp5662:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp5663:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp5664:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp5665:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp5666:
	.cfi_def_cfa_offset 48
.Ltmp5667:
	.cfi_offset %rbx, -40
.Ltmp5668:
	.cfi_offset %r12, -32
.Ltmp5669:
	.cfi_offset %r14, -24
.Ltmp5670:
	.cfi_offset %r15, -16
	movq	%rdx, %r15
	movq	%rsi, %r14
	movq	%rdi, %rbx
	callq	start_sequence
	movl	flag_pic(%rip), %eax
	testq	%rax, %rax
	je	.LBB623_3
# BB#1:                                 # %land.lhs.true
	movq	8(%rbx), %rax
	movl	(%rax), %eax
	andq	$134283263, %rax        # imm = 0x800FFFF
	cmpq	$68, %rax
	jne	.LBB623_3
# BB#2:                                 # %if.then
	movq	cfun(%rip), %rax
	movl	424(%rax), %ecx
	orq	$2097152, %rcx          # imm = 0x200000
	movl	%ecx, 424(%rax)
.LBB623_3:                              # %if.end
	movl	target_flags(%rip), %esi
	movq	8(%rbx), %rdi
	movq	$25, %rcx
	shrq	%cl, %rsi
	andq	$1, %rsi
	orq	$4, %rsi
	callq	call_insn_operand
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB623_5
# BB#4:                                 # %if.then21
	movl	target_flags(%rip), %edi
	movq	$25, %rcx
	movq	8(%rbx), %rsi
	shrq	%cl, %rdi
	andq	$1, %rdi
	orq	$4, %rdi
	callq	copy_to_mode_reg
	movq	%rax, 8(%rbx)
.LBB623_5:                              # %if.end34
	movzbl	target_flags+3(%rip), %eax
	testq	$2, %rax
	je	.LBB623_8
# BB#6:                                 # %land.lhs.true37
	cmpq	$0, 8(%r15)
	js	.LBB623_8
# BB#7:                                 # %if.then42
	movq	$2, %rdi
	xorq	%rsi, %rsi
	callq	gen_rtx_REG
	movq	%rax, %r12
	movq	%r12, %rdi
	movq	%r15, %rsi
	callq	emit_move_insn
	movq	$50, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%r14, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_call_insn
	leaq	64(%rax), %rdi
	movq	%r12, %rsi
	callq	use_reg
	jmp	.LBB623_9
.LBB623_8:                              # %if.end54
	movq	$50, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%r14, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_call_insn
.LBB623_9:                              # %if.end54
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.Ltmp5671:
	.size	gen_call, .Ltmp5671-gen_call
	.cfi_endproc

	.globl	gen_call_exp
	.align	16, 0x90
	.type	gen_call_exp,@function
gen_call_exp:                           # @gen_call_exp
	.cfi_startproc
# BB#0:                                 # %entry
	movq	%rsi, %rax
	movq	%rdi, %rcx
	movq	$50, %rdi
	xorq	%rsi, %rsi
	movq	%rcx, %rdx
	movq	%rax, %rcx
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp5672:
	.size	gen_call_exp, .Ltmp5672-gen_call_exp
	.cfi_endproc

	.globl	gen_call_value_pop
	.align	16, 0x90
	.type	gen_call_value_pop,@function
gen_call_value_pop:                     # @gen_call_value_pop
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp5678:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp5679:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp5680:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp5681:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp5682:
	.cfi_def_cfa_offset 48
.Ltmp5683:
	.cfi_offset %rbx, -40
.Ltmp5684:
	.cfi_offset %r12, -32
.Ltmp5685:
	.cfi_offset %r14, -24
.Ltmp5686:
	.cfi_offset %r15, -16
	movq	%r8, %r12
	movq	%rdx, %r15
	movq	%rsi, %rbx
	movq	%rdi, %r14
	callq	start_sequence
	movq	const_int_rtx+512(%rip), %rax
	cmpq	%r12, %rax
	je	.LBB625_7
# BB#1:                                 # %if.end
	movl	flag_pic(%rip), %eax
	testq	%rax, %rax
	je	.LBB625_4
# BB#2:                                 # %land.lhs.true
	movq	8(%rbx), %rax
	movl	(%rax), %eax
	andq	$134283263, %rax        # imm = 0x800FFFF
	cmpq	$68, %rax
	jne	.LBB625_4
# BB#3:                                 # %if.then22
	movq	cfun(%rip), %rax
	movl	424(%rax), %ecx
	orq	$2097152, %rcx          # imm = 0x200000
	movl	%ecx, 424(%rax)
.LBB625_4:                              # %if.end25
	movl	target_flags(%rip), %esi
	movq	8(%rbx), %rdi
	movq	$25, %rcx
	shrq	%cl, %rsi
	andq	$1, %rsi
	orq	$4, %rsi
	callq	call_insn_operand
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB625_6
# BB#5:                                 # %if.then33
	movl	target_flags(%rip), %edi
	movq	$25, %rcx
	movq	8(%rbx), %rsi
	shrq	%cl, %rdi
	andq	$1, %rdi
	orq	$4, %rdi
	callq	copy_to_mode_reg
	movq	%rax, 8(%rbx)
.LBB625_6:                              # %cleanup.cont
	movq	$50, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%r15, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %r14
	movq	$4, %rdi
	movq	$7, %rsi
	callq	gen_rtx_REG
	movq	%rax, %rbx
	movq	$4, %rdi
	movq	$7, %rsi
	callq	gen_rtx_REG
	movq	$75, %rdi
	movq	$4, %rsi
	movq	%rax, %rdx
	movq	%r12, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rcx
	movq	$2, %rdi
	xorq	%rax, %rax
	movq	%r14, %rsi
	movq	%rcx, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_E
	movq	%rax, %rdi
	callq	emit_call_insn
	jmp	.LBB625_8
.LBB625_7:                              # %cleanup
	movq	const_int_rtx+504(%rip), %rcx
	movq	%r14, %rdi
	movq	%rbx, %rsi
	movq	%r15, %rdx
	callq	gen_call_value
	movq	%rax, %rdi
	callq	emit_insn
.LBB625_8:                              # %cleanup
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.Ltmp5687:
	.size	gen_call_value_pop, .Ltmp5687-gen_call_value_pop
	.cfi_endproc

	.globl	gen_call_value
	.align	16, 0x90
	.type	gen_call_value,@function
gen_call_value:                         # @gen_call_value
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp5694:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp5695:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp5696:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp5697:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp5698:
	.cfi_def_cfa_offset 48
.Ltmp5699:
	.cfi_offset %rbx, -48
.Ltmp5700:
	.cfi_offset %r12, -40
.Ltmp5701:
	.cfi_offset %r13, -32
.Ltmp5702:
	.cfi_offset %r14, -24
.Ltmp5703:
	.cfi_offset %r15, -16
	movq	%rcx, %r12
	movq	%rdx, %r15
	movq	%rsi, %rbx
	movq	%rdi, %r14
	callq	start_sequence
	movl	flag_pic(%rip), %eax
	testq	%rax, %rax
	je	.LBB626_3
# BB#1:                                 # %land.lhs.true
	movq	8(%rbx), %rax
	movl	(%rax), %eax
	andq	$134283263, %rax        # imm = 0x800FFFF
	cmpq	$68, %rax
	jne	.LBB626_3
# BB#2:                                 # %if.then
	movq	cfun(%rip), %rax
	movl	424(%rax), %ecx
	orq	$2097152, %rcx          # imm = 0x200000
	movl	%ecx, 424(%rax)
.LBB626_3:                              # %if.end
	movl	target_flags(%rip), %esi
	movq	8(%rbx), %rdi
	movq	$25, %rcx
	shrq	%cl, %rsi
	andq	$1, %rsi
	orq	$4, %rsi
	callq	call_insn_operand
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB626_5
# BB#4:                                 # %if.then22
	movl	target_flags(%rip), %edi
	movq	$25, %rcx
	movq	8(%rbx), %rsi
	shrq	%cl, %rdi
	andq	$1, %rdi
	orq	$4, %rdi
	callq	copy_to_mode_reg
	movq	%rax, 8(%rbx)
.LBB626_5:                              # %if.end35
	movzbl	target_flags+3(%rip), %eax
	testq	$2, %rax
	je	.LBB626_8
# BB#6:                                 # %land.lhs.true38
	cmpq	$0, 8(%r12)
	js	.LBB626_8
# BB#7:                                 # %if.then43
	movq	$2, %rdi
	xorq	%rsi, %rsi
	callq	gen_rtx_REG
	movq	%rax, %r13
	movq	%r13, %rdi
	movq	%r12, %rsi
	callq	emit_move_insn
	movq	$50, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%r15, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_call_insn
	leaq	64(%rax), %rdi
	movq	%r13, %rsi
	callq	use_reg
	jmp	.LBB626_9
.LBB626_8:                              # %if.end56
	movq	$50, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%r15, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_call_insn
.LBB626_9:                              # %if.end56
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp5704:
	.size	gen_call_value, .Ltmp5704-gen_call_value
	.cfi_endproc

	.globl	gen_call_value_exp
	.align	16, 0x90
	.type	gen_call_value_exp,@function
gen_call_value_exp:                     # @gen_call_value_exp
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp5707:
	.cfi_def_cfa_offset 16
.Ltmp5708:
	.cfi_offset %rbx, -16
	movq	%rdx, %rax
	movq	%rsi, %rcx
	movq	%rdi, %rbx
	movq	$50, %rdi
	xorq	%rsi, %rsi
	movq	%rcx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	popq	%rbx
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp5709:
	.size	gen_call_value_exp, .Ltmp5709-gen_call_value_exp
	.cfi_endproc

	.globl	gen_untyped_call
	.align	16, 0x90
	.type	gen_untyped_call,@function
gen_untyped_call:                       # @gen_untyped_call
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp5716:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp5717:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp5718:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp5719:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp5720:
	.cfi_def_cfa_offset 48
.Ltmp5721:
	.cfi_offset %rbx, -48
.Ltmp5722:
	.cfi_offset %r12, -40
.Ltmp5723:
	.cfi_offset %r13, -32
.Ltmp5724:
	.cfi_offset %r14, -24
.Ltmp5725:
	.cfi_offset %r15, -16
	movq	%rdx, %r13
	movq	%rdi, %r14
	callq	start_sequence
	movl	target_flags(%rip), %esi
	testq	$32, %rsi
	je	.LBB628_2
# BB#1:                                 # %cond.true
	movq	$23, %rdi
	movq	$8, %rsi
	callq	gen_rtx_REG
	movl	target_flags(%rip), %esi
	movq	%rax, %r15
	movq	const_int_rtx+512(%rip), %r12
	movq	$22, %rcx
	shrq	%cl, %rsi
	andq	$8, %rsi
	decq	%rsi
	xorq	%rdi, %rdi
	callq	gen_rtx_CONST_INT
	movq	%r15, %rdi
	movq	%r14, %rsi
	movq	%r12, %rdx
	movq	%rax, %rcx
	callq	gen_call_value
	jmp	.LBB628_3
.LBB628_2:                              # %cond.false
	movq	const_int_rtx+512(%rip), %r15
	movq	$22, %rcx
	shrq	%cl, %rsi
	andq	$8, %rsi
	decq	%rsi
	xorq	%rdi, %rdi
	callq	gen_rtx_CONST_INT
	movq	%r14, %rdi
	movq	%r15, %rsi
	movq	%rax, %rdx
	callq	gen_call
.LBB628_3:                              # %cond.end
	movq	%rax, %rdi
	callq	emit_call_insn
	movq	8(%r13), %rax
	movslq	(%rax), %rcx
	testq	%rcx, %rcx
	jle	.LBB628_6
# BB#4:
	movq	$1, %rbx
	.align	16, 0x90
.LBB628_5:                              # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	(%rax,%rbx,8), %rax
	movq	8(%rax), %rdi
	movq	16(%rax), %rsi
	callq	emit_move_insn
	movq	8(%r13), %rax
	movslq	(%rax), %rdx
	movq	$32, %rcx
	movq	%rbx, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	incq	%rbx
	cmpq	%rdx, %rsi
	jl	.LBB628_5
.LBB628_6:                              # %for.end
	movq	const_int_rtx+512(%rip), %rsi
	movq	$1, %rdi
	xorq	%rax, %rax
	callq	gen_rtvec
	movq	$43, %rdi
	xorq	%rsi, %rsi
	xorq	%rcx, %rcx
	movq	%rax, %rdx
	callq	gen_rtx_fmt_Ei
	movq	%rax, %rdi
	callq	emit_insn
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp5726:
	.size	gen_untyped_call, .Ltmp5726-gen_untyped_call
	.cfi_endproc

	.globl	gen_return
	.align	16, 0x90
	.type	gen_return,@function
gen_return:                             # @gen_return
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp5730:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp5731:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp5732:
	.cfi_def_cfa_offset 32
.Ltmp5733:
	.cfi_offset %rbx, -24
.Ltmp5734:
	.cfi_offset %r14, -16
	callq	start_sequence
	movq	cfun(%rip), %rax
	movslq	64(%rax), %rsi
	testq	%rsi, %rsi
	je	.LBB629_2
# BB#1:                                 # %if.then
	xorq	%rdi, %rdi
	callq	gen_rtx_CONST_INT
	movq	%rax, %rbx
	movq	$51, %rdi
	xorq	%rsi, %rsi
	callq	gen_rtx_fmt_
	movq	%rax, %r14
	movq	$48, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rcx
	movq	$2, %rdi
	xorq	%rax, %rax
	movq	%r14, %rsi
	movq	%rcx, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_E
	jmp	.LBB629_3
.LBB629_2:                              # %if.end
	movq	$51, %rdi
	xorq	%rsi, %rsi
	callq	gen_rtx_fmt_
.LBB629_3:                              # %if.end
	movq	%rax, %rdi
	callq	emit_jump_insn
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp5735:
	.size	gen_return, .Ltmp5735-gen_return
	.cfi_endproc

	.globl	gen_prologue
	.align	16, 0x90
	.type	gen_prologue,@function
gen_prologue:                           # @gen_prologue
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp5738:
	.cfi_def_cfa_offset 16
.Ltmp5739:
	.cfi_offset %rbx, -16
	callq	start_sequence
	callq	ix86_expand_prologue
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	popq	%rbx
	retq
.Ltmp5740:
	.size	gen_prologue, .Ltmp5740-gen_prologue
	.cfi_endproc

	.globl	gen_epilogue
	.align	16, 0x90
	.type	gen_epilogue,@function
gen_epilogue:                           # @gen_epilogue
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp5743:
	.cfi_def_cfa_offset 16
.Ltmp5744:
	.cfi_offset %rbx, -16
	callq	start_sequence
	movq	$1, %rdi
	callq	ix86_expand_epilogue
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	popq	%rbx
	retq
.Ltmp5745:
	.size	gen_epilogue, .Ltmp5745-gen_epilogue
	.cfi_endproc

	.globl	gen_sibcall_epilogue
	.align	16, 0x90
	.type	gen_sibcall_epilogue,@function
gen_sibcall_epilogue:                   # @gen_sibcall_epilogue
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp5748:
	.cfi_def_cfa_offset 16
.Ltmp5749:
	.cfi_offset %rbx, -16
	callq	start_sequence
	xorq	%rdi, %rdi
	callq	ix86_expand_epilogue
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	popq	%rbx
	retq
.Ltmp5750:
	.size	gen_sibcall_epilogue, .Ltmp5750-gen_sibcall_epilogue
	.cfi_endproc

	.globl	gen_eh_return
	.align	16, 0x90
	.type	gen_eh_return,@function
gen_eh_return:                          # @gen_eh_return
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp5754:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp5755:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp5756:
	.cfi_def_cfa_offset 32
.Ltmp5757:
	.cfi_offset %rbx, -24
.Ltmp5758:
	.cfi_offset %r14, -16
	movq	%rsi, %r14
	movq	%rdi, %rbx
	callq	start_sequence
	movl	target_flags(%rip), %esi
	movq	$25, %rcx
	movq	global_rtl+40(%rip), %rdx
	shrq	%cl, %rsi
	andq	$1, %rsi
	orq	$4, %rsi
	movq	$75, %rdi
	movq	%rbx, %rcx
	callq	gen_rtx_fmt_ee
	movl	target_flags(%rip), %edx
	movq	$23, %rcx
	shrq	%cl, %rdx
	andq	$4, %rdx
	movq	$-4, %rsi
	subq	%rdx, %rsi
	movq	%rax, %rdi
	callq	plus_constant_wide
	movl	target_flags(%rip), %edi
	movq	$25, %rcx
	shrq	%cl, %rdi
	andq	$1, %rdi
	orq	$4, %rdi
	movq	%rax, %rsi
	callq	gen_rtx_MEM
	movq	%rax, %rdi
	movq	%r14, %rsi
	callq	emit_move_insn
	movq	$1, %rdi
	xorq	%rax, %rax
	movq	%rbx, %rsi
	callq	gen_rtvec
	movq	$43, %rdi
	xorq	%rsi, %rsi
	movq	$13, %rcx
	movq	%rax, %rdx
	callq	gen_rtx_fmt_Ei
	movq	%rax, %rdi
	callq	emit_insn
	callq	emit_barrier
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp5759:
	.size	gen_eh_return, .Ltmp5759-gen_eh_return
	.cfi_endproc

	.globl	gen_split_1178
	.align	16, 0x90
	.type	gen_split_1178,@function
gen_split_1178:                         # @gen_split_1178
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp5762:
	.cfi_def_cfa_offset 16
.Ltmp5763:
	.cfi_offset %rbx, -16
	callq	start_sequence
	movq	$2, %rdi
	callq	ix86_expand_epilogue
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	popq	%rbx
	retq
.Ltmp5764:
	.size	gen_split_1178, .Ltmp5764-gen_split_1178
	.cfi_endproc

	.globl	gen_split_1179
	.align	16, 0x90
	.type	gen_split_1179,@function
gen_split_1179:                         # @gen_split_1179
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp5767:
	.cfi_def_cfa_offset 16
.Ltmp5768:
	.cfi_offset %rbx, -16
	callq	start_sequence
	movq	$2, %rdi
	callq	ix86_expand_epilogue
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	popq	%rbx
	retq
.Ltmp5769:
	.size	gen_split_1179, .Ltmp5769-gen_split_1179
	.cfi_endproc

	.globl	gen_ffssi2
	.align	16, 0x90
	.type	gen_ffssi2,@function
gen_ffssi2:                             # @gen_ffssi2
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp5776:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp5777:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp5778:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp5779:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp5780:
	.cfi_def_cfa_offset 48
	subq	$80, %rsp
.Ltmp5781:
	.cfi_def_cfa_offset 128
.Ltmp5782:
	.cfi_offset %rbx, -48
.Ltmp5783:
	.cfi_offset %r12, -40
.Ltmp5784:
	.cfi_offset %r13, -32
.Ltmp5785:
	.cfi_offset %r14, -24
.Ltmp5786:
	.cfi_offset %r15, -16
	movq	%rsi, %r13
	movq	%rdi, %r14
	callq	start_sequence
	movq	$4, %rdi
	callq	gen_reg_rtx
	movq	%rax, %r12
	movq	$4, %rdi
	callq	gen_reg_rtx
	movq	%rax, %r15
	movl	ix86_arch(%rip), %ecx
	movl	x86_cmove(%rip), %eax
	movq	$1, %rdx
	shlq	%cl, %rdx
	andq	%rax, %rdx
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rdx
	jne	.LBB636_2
# BB#1:                                 # %lor.lhs.false
	movl	target_flags(%rip), %eax
	testq	$327680, %rax           # imm = 0x50000
	je	.LBB636_3
.LBB636_2:                              # %if.then
	movq	const_int_rtx+504(%rip), %rsi
	movq	%r15, %rdi
	callq	emit_move_insn
	movq	$56, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	const_int_rtx+512(%rip), %rcx
	movq	%rax, %rbx
	movq	$74, %rdi
	movq	$56, %rsi
	movq	%r13, %rdx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rbx
	movq	$1, %rdi
	xorq	%rax, %rax
	movq	%r13, %rsi
	callq	gen_rtvec
	movq	$42, %rdi
	movq	$4, %rsi
	movq	$5, %rcx
	movq	%rax, %rdx
	callq	gen_rtx_fmt_Ei
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r12, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rcx
	movq	$2, %rdi
	xorq	%rax, %rax
	movq	%rbx, %rsi
	movq	%rcx, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_E
	movq	%rax, %rdi
	callq	emit_insn
	movq	$56, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	const_int_rtx+512(%rip), %rcx
	movq	$103, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_ee
	movq	$72, %rdi
	movq	$4, %rsi
	movq	%rax, %rdx
	movq	%r15, %rcx
	movq	%r12, %r8
	callq	gen_rtx_fmt_eee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r12, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	movq	const_int_rtx+520(%rip), %rbx
	callq	start_sequence
	movq	%r12, 48(%rsp)
	movq	%r12, 56(%rsp)
	movq	%rbx, 64(%rsp)
	leaq	48(%rsp), %rdx
	movq	$75, %rdi
	movq	$4, %rsi
.LBB636_8:                              # %if.end74
	callq	ix86_expand_binary_operator
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rdi
	callq	emit_insn
.LBB636_9:                              # %if.end74
	movq	%r14, %rdi
	movq	%r12, %rsi
	callq	emit_move_insn
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	addq	$80, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB636_3:                              # %if.else
	movl	ix86_cpu(%rip), %edx
	cmpq	$2, %rdx
	jne	.LBB636_7
# BB#4:                                 # %if.else
	movl	optimize_size(%rip), %edx
	testq	%rcx, %rdx
	jne	.LBB636_7
# BB#5:                                 # %if.else
	andq	$1, %rax
	je	.LBB636_7
# BB#6:                                 # %if.then23
	callq	gen_label_rtx
	movq	%rax, %rbx
	movq	%rbx, 16(%rsp)          # 8-byte Spill
	movq	$5, %rdi
	xorq	%rsi, %rsi
	callq	assign_386_stack_local
	movq	%rax, 24(%rsp)
	movq	$16, %rdi
	callq	gen_reg_rtx
	movq	%rax, 8(%rsp)           # 8-byte Spill
	leaq	24(%rsp), %rdi
	leaq	40(%rsp), %rdx
	leaq	32(%rsp), %rcx
	movq	$1, %rsi
	callq	split_di
	movq	const_int_rtx+512(%rip), %rsi
	movq	%r12, %rdi
	callq	emit_move_insn
	movq	const_int_rtx+512(%rip), %rsi
	movq	$103, %rdx
	xorq	%rcx, %rcx
	movq	$4, %r8
	movq	$1, %r9
	movq	%rbx, (%rsp)
	movq	%r13, %rdi
	callq	emit_cmp_and_jump_insns
	movq	32(%rsp), %rdi
	movq	%r12, %rsi
	callq	emit_move_insn
	callq	start_sequence
	movq	%r12, 48(%rsp)
	movq	%r12, 56(%rsp)
	movq	%r13, 64(%rsp)
	leaq	48(%rsp), %rdx
	movq	$76, %rdi
	movq	$4, %rsi
	movq	%rdx, %rbx
	callq	ix86_expand_binary_operator
	callq	gen_sequence
	movq	%rax, %r15
	callq	end_sequence
	movq	%r15, %rdi
	callq	emit_insn
	callq	start_sequence
	movq	%r12, 48(%rsp)
	movq	%r12, 56(%rsp)
	movq	%r13, 64(%rsp)
	movq	$83, %rdi
	movq	$4, %rsi
	movq	%rbx, %rdx
	movq	%rbx, %r15
	callq	ix86_expand_binary_operator
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rdi
	callq	emit_insn
	movq	40(%rsp), %rdi
	movq	%r12, %rsi
	callq	emit_move_insn
	movq	24(%rsp), %rdx
	movq	$125, %rdi
	movq	$16, %rsi
	callq	gen_rtx_fmt_e
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	8(%rsp), %rbx           # 8-byte Reload
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	movq	24(%rsp), %rax
	movq	8(%rax), %rsi
	movq	$16, %rdi
	callq	gen_rtx_MEM
	movq	%rax, %rdi
	movq	%rbx, %rsi
	callq	emit_move_insn
	movq	32(%rsp), %rsi
	movq	%r12, %rdi
	callq	emit_move_insn
	xorq	%rdi, %rdi
	movq	$20, %rsi
	callq	gen_rtx_CONST_INT
	movq	%rax, %rbx
	callq	start_sequence
	movq	%r12, 48(%rsp)
	movq	%r12, 56(%rsp)
	movq	%rbx, 64(%rsp)
	movq	$90, %rdi
	movq	$4, %rsi
	movq	%r15, %rdx
	callq	ix86_expand_binary_operator
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rdi
	callq	emit_insn
	xorq	%rdi, %rdi
	movq	$1022, %rsi             # imm = 0x3FE
	callq	gen_rtx_CONST_INT
	movq	%rax, %rbx
	callq	start_sequence
	movq	%r12, 48(%rsp)
	movq	%r12, 56(%rsp)
	movq	%rbx, 64(%rsp)
	movq	$76, %rdi
	movq	$4, %rsi
	movq	%r15, %rdx
	callq	ix86_expand_binary_operator
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rdi
	callq	emit_insn
	movq	16(%rsp), %rbx          # 8-byte Reload
	movq	%rbx, %rdi
	callq	emit_label
	movq	$1, %rax
	movl	%eax, 32(%rbx)
	jmp	.LBB636_9
.LBB636_7:                              # %if.else56
	movq	const_int_rtx+512(%rip), %rsi
	movq	%r15, %rdi
	callq	emit_move_insn
	movq	$56, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	const_int_rtx+512(%rip), %rcx
	movq	%rax, %rbx
	movq	$74, %rdi
	movq	$56, %rsi
	movq	%r13, %rdx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rbx
	movq	$1, %rdi
	xorq	%rax, %rax
	movq	%r13, %rsi
	callq	gen_rtvec
	movq	$42, %rdi
	movq	$4, %rsi
	movq	$5, %rcx
	movq	%rax, %rdx
	callq	gen_rtx_fmt_Ei
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r12, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rcx
	movq	$2, %rdi
	xorq	%rax, %rax
	movq	%rbx, %rsi
	movq	%rcx, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_E
	movq	%rax, %rdi
	callq	emit_insn
	movq	$2, %rdi
	movq	%r15, %rsi
	callq	gen_lowpart
	movq	$64, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rbx
	movq	$56, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	const_int_rtx+512(%rip), %rcx
	movq	$103, %rdi
	movq	$2, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	callq	start_sequence
	movq	%r15, 48(%rsp)
	movq	%r15, 56(%rsp)
	leaq	48(%rsp), %rdx
	movq	$77, %rdi
	movq	$4, %rsi
	callq	ix86_expand_unary_operator
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rdi
	callq	emit_insn
	callq	start_sequence
	movq	%r12, 48(%rsp)
	movq	%r12, 56(%rsp)
	movq	%r15, 64(%rsp)
	leaq	48(%rsp), %r15
	movq	$84, %rdi
	movq	$4, %rsi
	movq	%r15, %rdx
	callq	ix86_expand_binary_operator
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rdi
	callq	emit_insn
	movq	const_int_rtx+520(%rip), %rbx
	callq	start_sequence
	movq	%r12, 48(%rsp)
	movq	%r12, 56(%rsp)
	movq	%rbx, 64(%rsp)
	movq	$75, %rdi
	movq	$4, %rsi
	movq	%r15, %rdx
	jmp	.LBB636_8
.Ltmp5787:
	.size	gen_ffssi2, .Ltmp5787-gen_ffssi2
	.cfi_endproc

	.globl	gen_split_1181
	.align	16, 0x90
	.type	gen_split_1181,@function
gen_split_1181:                         # @gen_split_1181
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp5791:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp5792:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp5793:
	.cfi_def_cfa_offset 32
.Ltmp5794:
	.cfi_offset %rbx, -24
.Ltmp5795:
	.cfi_offset %r14, -16
	movq	%rdi, %rbx
	callq	start_sequence
	movq	8(%rbx), %rsi
	movzbl	2(%rsi), %edi
	callq	ix86_force_to_memory
	movq	%rax, 32(%rbx)
	movq	(%rbx), %rcx
	movzbl	2(%rcx), %esi
	movq	$125, %rdi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, 32(%rbx)
	movq	24(%rbx), %rcx
	movl	(%rcx), %esi
	movq	%rsi, %rdi
	movq	$16, %rcx
	movq	(%rbx), %r14
	movq	16(%rbx), %r8
	shrq	%cl, %rsi
	andq	$65535, %rdi            # imm = 0xFFFF
	andq	$255, %rsi
	movq	%rax, %rdx
	movq	%r8, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	movq	8(%rbx), %rax
	movzbl	2(%rax), %edi
	callq	ix86_free_from_memory
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp5796:
	.size	gen_split_1181, .Ltmp5796-gen_split_1181
	.cfi_endproc

	.globl	gen_split_1182
	.align	16, 0x90
	.type	gen_split_1182,@function
gen_split_1182:                         # @gen_split_1182
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp5800:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp5801:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp5802:
	.cfi_def_cfa_offset 32
.Ltmp5803:
	.cfi_offset %rbx, -24
.Ltmp5804:
	.cfi_offset %r14, -16
	movq	%rdi, %rbx
	callq	start_sequence
	movq	16(%rbx), %rsi
	movzbl	2(%rsi), %edi
	callq	ix86_force_to_memory
	movq	%rax, 32(%rbx)
	movq	(%rbx), %rcx
	movzbl	2(%rcx), %esi
	movq	$125, %rdi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, 32(%rbx)
	movq	24(%rbx), %rcx
	movl	(%rcx), %esi
	movq	%rsi, %rdi
	movq	$16, %rcx
	movq	(%rbx), %r14
	movq	8(%rbx), %rdx
	shrq	%cl, %rsi
	andq	$65535, %rdi            # imm = 0xFFFF
	andq	$255, %rsi
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	movq	16(%rbx), %rax
	movzbl	2(%rax), %edi
	callq	ix86_free_from_memory
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp5805:
	.size	gen_split_1182, .Ltmp5805-gen_split_1182
	.cfi_endproc

	.globl	gen_sqrtsf2
	.align	16, 0x90
	.type	gen_sqrtsf2,@function
gen_sqrtsf2:                            # @gen_sqrtsf2
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp5809:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp5810:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp5811:
	.cfi_def_cfa_offset 32
.Ltmp5812:
	.cfi_offset %rbx, -24
.Ltmp5813:
	.cfi_offset %r14, -16
	movq	%rsi, %rbx
	movq	%rdi, %r14
	callq	start_sequence
	movzbl	ix86_fpmath(%rip), %eax
	testq	$2, %rax
	jne	.LBB639_2
# BB#1:                                 # %if.then
	movq	$15, %rdi
	movq	%rbx, %rsi
	callq	force_reg
	movq	%rax, %rbx
.LBB639_2:                              # %if.end
	movq	$130, %rdi
	movq	$15, %rsi
	movq	%rbx, %rdx
	callq	gen_rtx_fmt_e
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp5814:
	.size	gen_sqrtsf2, .Ltmp5814-gen_sqrtsf2
	.cfi_endproc

	.globl	gen_sqrtdf2
	.align	16, 0x90
	.type	gen_sqrtdf2,@function
gen_sqrtdf2:                            # @gen_sqrtdf2
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp5818:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp5819:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp5820:
	.cfi_def_cfa_offset 32
.Ltmp5821:
	.cfi_offset %rbx, -24
.Ltmp5822:
	.cfi_offset %r14, -16
	movq	%rsi, %rbx
	movq	%rdi, %r14
	callq	start_sequence
	movzbl	target_flags+2(%rip), %eax
	testq	$4, %rax
	je	.LBB640_2
# BB#1:                                 # %lor.lhs.false
	movzbl	ix86_fpmath(%rip), %eax
	testq	$2, %rax
	jne	.LBB640_3
.LBB640_2:                              # %if.then
	movq	$16, %rdi
	movq	%rbx, %rsi
	callq	force_reg
	movq	%rax, %rbx
.LBB640_3:                              # %if.end
	movq	$130, %rdi
	movq	$16, %rsi
	movq	%rbx, %rdx
	callq	gen_rtx_fmt_e
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp5823:
	.size	gen_sqrtdf2, .Ltmp5823-gen_sqrtdf2
	.cfi_endproc

	.globl	gen_movstrsi
	.align	16, 0x90
	.type	gen_movstrsi,@function
gen_movstrsi:                           # @gen_movstrsi
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp5829:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp5830:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp5831:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp5832:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp5833:
	.cfi_def_cfa_offset 48
.Ltmp5834:
	.cfi_offset %rbx, -40
.Ltmp5835:
	.cfi_offset %r12, -32
.Ltmp5836:
	.cfi_offset %r14, -24
.Ltmp5837:
	.cfi_offset %r15, -16
	movq	%rcx, %r14
	movq	%rdx, %r15
	movq	%rsi, %r12
	movq	%rdi, %rbx
	callq	start_sequence
	movq	%rbx, %rdi
	movq	%r12, %rsi
	movq	%r15, %rdx
	movq	%r14, %rcx
	callq	ix86_expand_movstr
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB641_2
# BB#1:                                 # %if.then
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	jmp	.LBB641_3
.LBB641_2:                              # %return
	callq	end_sequence
	xorq	%rax, %rax
.LBB641_3:                              # %return
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.Ltmp5838:
	.size	gen_movstrsi, .Ltmp5838-gen_movstrsi
	.cfi_endproc

	.globl	gen_movstrdi
	.align	16, 0x90
	.type	gen_movstrdi,@function
gen_movstrdi:                           # @gen_movstrdi
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp5844:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp5845:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp5846:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp5847:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp5848:
	.cfi_def_cfa_offset 48
.Ltmp5849:
	.cfi_offset %rbx, -40
.Ltmp5850:
	.cfi_offset %r12, -32
.Ltmp5851:
	.cfi_offset %r14, -24
.Ltmp5852:
	.cfi_offset %r15, -16
	movq	%rcx, %r14
	movq	%rdx, %r15
	movq	%rsi, %r12
	movq	%rdi, %rbx
	callq	start_sequence
	movq	%rbx, %rdi
	movq	%r12, %rsi
	movq	%r15, %rdx
	movq	%r14, %rcx
	callq	ix86_expand_movstr
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB642_2
# BB#1:                                 # %if.then
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	jmp	.LBB642_3
.LBB642_2:                              # %return
	callq	end_sequence
	xorq	%rax, %rax
.LBB642_3:                              # %return
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.Ltmp5853:
	.size	gen_movstrdi, .Ltmp5853-gen_movstrdi
	.cfi_endproc

	.globl	gen_strmovdi_rex64
	.align	16, 0x90
	.type	gen_strmovdi_rex64,@function
gen_strmovdi_rex64:                     # @gen_strmovdi_rex64
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp5858:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp5859:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp5860:
	.cfi_def_cfa_offset 32
.Ltmp5861:
	.cfi_offset %rbx, -32
.Ltmp5862:
	.cfi_offset %r14, -24
.Ltmp5863:
	.cfi_offset %r15, -16
	movq	%rsi, %r14
	movq	%rdi, %rbx
	callq	start_sequence
	movl	x86_single_stringop(%rip), %eax
	movl	ix86_cpu(%rip), %ecx
	movq	$1, %rdx
	movl	optimize_size(%rip), %esi
	shlq	%cl, %rdx
	andq	%rax, %rdx
	orq	%rdx, %rsi
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %rsi
	je	.LBB643_3
# BB#1:                                 # %if.then
	movq	%rbx, %rdi
	movq	%r14, %rsi
	movq	%rbx, %rdx
	movq	%r14, %rcx
	callq	gen_strmovdi_rex_1
	movq	%rax, %rdi
	callq	emit_insn
	jmp	.LBB643_2
.LBB643_3:                              # %if.else
	movq	$5, %rdi
	callq	gen_reg_rtx
	movq	%rax, %r15
	movq	$5, %rdi
	movq	%r14, %rsi
	callq	gen_rtx_MEM
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r15, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	movq	$5, %rdi
	movq	%rbx, %rsi
	callq	gen_rtx_MEM
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	movq	%r15, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	xorq	%rdi, %rdi
	movq	$8, %rsi
	callq	gen_rtx_CONST_INT
	movq	$75, %rdi
	movq	$5, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rbx
	movq	$52, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rcx
	movq	$2, %rdi
	xorq	%rax, %rax
	movq	%rbx, %rsi
	movq	%rcx, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_E
	movq	%rax, %rdi
	callq	emit
	xorq	%rdi, %rdi
	movq	$8, %rsi
	callq	gen_rtx_CONST_INT
	movq	$75, %rdi
	movq	$5, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rbx
	movq	$52, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rcx
	movq	$2, %rdi
	xorq	%rax, %rax
	movq	%rbx, %rsi
	movq	%rcx, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_E
	movq	%rax, %rdi
	callq	emit
.LBB643_2:                              # %if.then
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp5864:
	.size	gen_strmovdi_rex64, .Ltmp5864-gen_strmovdi_rex64
	.cfi_endproc

	.globl	gen_strmovsi
	.align	16, 0x90
	.type	gen_strmovsi,@function
gen_strmovsi:                           # @gen_strmovsi
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp5869:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp5870:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp5871:
	.cfi_def_cfa_offset 32
.Ltmp5872:
	.cfi_offset %rbx, -32
.Ltmp5873:
	.cfi_offset %r14, -24
.Ltmp5874:
	.cfi_offset %r15, -16
	movq	%rsi, %r14
	movq	%rdi, %rbx
	callq	start_sequence
	movzbl	target_flags+3(%rip), %eax
	testq	$2, %rax
	je	.LBB644_4
# BB#1:                                 # %if.then
	movq	%rbx, %rdi
	movq	%r14, %rsi
	callq	gen_strmovsi_rex64
	jmp	.LBB644_2
.LBB644_4:                              # %if.end
	movl	x86_single_stringop(%rip), %eax
	movl	ix86_cpu(%rip), %ecx
	movq	$1, %rdx
	movl	optimize_size(%rip), %esi
	shlq	%cl, %rdx
	andq	%rax, %rdx
	orq	%rdx, %rsi
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %rsi
	je	.LBB644_6
# BB#5:                                 # %if.then9
	movq	%rbx, %rdi
	movq	%r14, %rsi
	movq	%rbx, %rdx
	movq	%r14, %rcx
	callq	gen_strmovsi_1
.LBB644_2:                              # %if.then
	movq	%rax, %rdi
	callq	emit_insn
.LBB644_3:                              # %if.then
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.LBB644_6:                              # %if.else
	movq	$4, %rdi
	callq	gen_reg_rtx
	movq	%rax, %r15
	movq	$4, %rdi
	movq	%r14, %rsi
	callq	gen_rtx_MEM
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r15, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	movq	$4, %rdi
	movq	%rbx, %rsi
	callq	gen_rtx_MEM
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	movq	%r15, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	xorq	%rdi, %rdi
	movq	$4, %rsi
	callq	gen_rtx_CONST_INT
	movq	$75, %rdi
	movq	$4, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rbx
	movq	$52, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rcx
	movq	$2, %rdi
	xorq	%rax, %rax
	movq	%rbx, %rsi
	movq	%rcx, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_E
	movq	%rax, %rdi
	callq	emit
	xorq	%rdi, %rdi
	movq	$4, %rsi
	callq	gen_rtx_CONST_INT
	movq	$75, %rdi
	movq	$4, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rbx
	movq	$52, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rcx
	movq	$2, %rdi
	xorq	%rax, %rax
	movq	%rbx, %rsi
	movq	%rcx, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_E
	movq	%rax, %rdi
	callq	emit
	jmp	.LBB644_3
.Ltmp5875:
	.size	gen_strmovsi, .Ltmp5875-gen_strmovsi
	.cfi_endproc

	.globl	gen_strmovsi_rex64
	.align	16, 0x90
	.type	gen_strmovsi_rex64,@function
gen_strmovsi_rex64:                     # @gen_strmovsi_rex64
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp5880:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp5881:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp5882:
	.cfi_def_cfa_offset 32
.Ltmp5883:
	.cfi_offset %rbx, -32
.Ltmp5884:
	.cfi_offset %r14, -24
.Ltmp5885:
	.cfi_offset %r15, -16
	movq	%rsi, %r14
	movq	%rdi, %rbx
	callq	start_sequence
	movl	x86_single_stringop(%rip), %eax
	movl	ix86_cpu(%rip), %ecx
	movq	$1, %rdx
	movl	optimize_size(%rip), %esi
	shlq	%cl, %rdx
	andq	%rax, %rdx
	orq	%rdx, %rsi
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %rsi
	je	.LBB645_3
# BB#1:                                 # %if.then
	movq	%rbx, %rdi
	movq	%r14, %rsi
	movq	%rbx, %rdx
	movq	%r14, %rcx
	callq	gen_strmovsi_rex_1
	movq	%rax, %rdi
	callq	emit_insn
	jmp	.LBB645_2
.LBB645_3:                              # %if.else
	movq	$4, %rdi
	callq	gen_reg_rtx
	movq	%rax, %r15
	movq	$4, %rdi
	movq	%r14, %rsi
	callq	gen_rtx_MEM
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r15, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	movq	$4, %rdi
	movq	%rbx, %rsi
	callq	gen_rtx_MEM
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	movq	%r15, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	xorq	%rdi, %rdi
	movq	$4, %rsi
	callq	gen_rtx_CONST_INT
	movq	$75, %rdi
	movq	$5, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rbx
	movq	$52, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rcx
	movq	$2, %rdi
	xorq	%rax, %rax
	movq	%rbx, %rsi
	movq	%rcx, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_E
	movq	%rax, %rdi
	callq	emit
	xorq	%rdi, %rdi
	movq	$4, %rsi
	callq	gen_rtx_CONST_INT
	movq	$75, %rdi
	movq	$5, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rbx
	movq	$52, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rcx
	movq	$2, %rdi
	xorq	%rax, %rax
	movq	%rbx, %rsi
	movq	%rcx, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_E
	movq	%rax, %rdi
	callq	emit
.LBB645_2:                              # %if.then
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp5886:
	.size	gen_strmovsi_rex64, .Ltmp5886-gen_strmovsi_rex64
	.cfi_endproc

	.globl	gen_strmovhi
	.align	16, 0x90
	.type	gen_strmovhi,@function
gen_strmovhi:                           # @gen_strmovhi
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp5891:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp5892:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp5893:
	.cfi_def_cfa_offset 32
.Ltmp5894:
	.cfi_offset %rbx, -32
.Ltmp5895:
	.cfi_offset %r14, -24
.Ltmp5896:
	.cfi_offset %r15, -16
	movq	%rsi, %r14
	movq	%rdi, %rbx
	callq	start_sequence
	movzbl	target_flags+3(%rip), %eax
	testq	$2, %rax
	je	.LBB646_4
# BB#1:                                 # %if.then
	movq	%rbx, %rdi
	movq	%r14, %rsi
	callq	gen_strmovhi_rex64
	jmp	.LBB646_2
.LBB646_4:                              # %if.end
	movl	x86_single_stringop(%rip), %eax
	movl	ix86_cpu(%rip), %ecx
	movq	$1, %rdx
	movl	optimize_size(%rip), %esi
	shlq	%cl, %rdx
	andq	%rax, %rdx
	orq	%rdx, %rsi
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %rsi
	je	.LBB646_6
# BB#5:                                 # %if.then9
	movq	%rbx, %rdi
	movq	%r14, %rsi
	movq	%rbx, %rdx
	movq	%r14, %rcx
	callq	gen_strmovhi_1
.LBB646_2:                              # %if.then
	movq	%rax, %rdi
	callq	emit_insn
.LBB646_3:                              # %if.then
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.LBB646_6:                              # %if.else
	movq	$3, %rdi
	callq	gen_reg_rtx
	movq	%rax, %r15
	movq	$3, %rdi
	movq	%r14, %rsi
	callq	gen_rtx_MEM
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r15, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	movq	$3, %rdi
	movq	%rbx, %rsi
	callq	gen_rtx_MEM
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	movq	%r15, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	xorq	%rdi, %rdi
	movq	$2, %rsi
	callq	gen_rtx_CONST_INT
	movq	$75, %rdi
	movq	$4, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rbx
	movq	$52, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rcx
	movq	$2, %rdi
	xorq	%rax, %rax
	movq	%rbx, %rsi
	movq	%rcx, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_E
	movq	%rax, %rdi
	callq	emit
	xorq	%rdi, %rdi
	movq	$2, %rsi
	callq	gen_rtx_CONST_INT
	movq	$75, %rdi
	movq	$4, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rbx
	movq	$52, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rcx
	movq	$2, %rdi
	xorq	%rax, %rax
	movq	%rbx, %rsi
	movq	%rcx, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_E
	movq	%rax, %rdi
	callq	emit
	jmp	.LBB646_3
.Ltmp5897:
	.size	gen_strmovhi, .Ltmp5897-gen_strmovhi
	.cfi_endproc

	.globl	gen_strmovhi_rex64
	.align	16, 0x90
	.type	gen_strmovhi_rex64,@function
gen_strmovhi_rex64:                     # @gen_strmovhi_rex64
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp5902:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp5903:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp5904:
	.cfi_def_cfa_offset 32
.Ltmp5905:
	.cfi_offset %rbx, -32
.Ltmp5906:
	.cfi_offset %r14, -24
.Ltmp5907:
	.cfi_offset %r15, -16
	movq	%rsi, %r14
	movq	%rdi, %rbx
	callq	start_sequence
	movl	x86_single_stringop(%rip), %eax
	movl	ix86_cpu(%rip), %ecx
	movq	$1, %rdx
	movl	optimize_size(%rip), %esi
	shlq	%cl, %rdx
	andq	%rax, %rdx
	orq	%rdx, %rsi
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %rsi
	je	.LBB647_3
# BB#1:                                 # %if.then
	movq	%rbx, %rdi
	movq	%r14, %rsi
	movq	%rbx, %rdx
	movq	%r14, %rcx
	callq	gen_strmovhi_rex_1
	movq	%rax, %rdi
	callq	emit_insn
	jmp	.LBB647_2
.LBB647_3:                              # %if.else
	movq	$3, %rdi
	callq	gen_reg_rtx
	movq	%rax, %r15
	movq	$3, %rdi
	movq	%r14, %rsi
	callq	gen_rtx_MEM
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r15, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	movq	$3, %rdi
	movq	%rbx, %rsi
	callq	gen_rtx_MEM
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	movq	%r15, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	xorq	%rdi, %rdi
	movq	$2, %rsi
	callq	gen_rtx_CONST_INT
	movq	$75, %rdi
	movq	$5, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rbx
	movq	$52, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rcx
	movq	$2, %rdi
	xorq	%rax, %rax
	movq	%rbx, %rsi
	movq	%rcx, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_E
	movq	%rax, %rdi
	callq	emit
	xorq	%rdi, %rdi
	movq	$2, %rsi
	callq	gen_rtx_CONST_INT
	movq	$75, %rdi
	movq	$5, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rbx
	movq	$52, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rcx
	movq	$2, %rdi
	xorq	%rax, %rax
	movq	%rbx, %rsi
	movq	%rcx, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_E
	movq	%rax, %rdi
	callq	emit
.LBB647_2:                              # %if.then
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp5908:
	.size	gen_strmovhi_rex64, .Ltmp5908-gen_strmovhi_rex64
	.cfi_endproc

	.globl	gen_strmovqi
	.align	16, 0x90
	.type	gen_strmovqi,@function
gen_strmovqi:                           # @gen_strmovqi
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp5913:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp5914:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp5915:
	.cfi_def_cfa_offset 32
.Ltmp5916:
	.cfi_offset %rbx, -32
.Ltmp5917:
	.cfi_offset %r14, -24
.Ltmp5918:
	.cfi_offset %r15, -16
	movq	%rsi, %r14
	movq	%rdi, %rbx
	callq	start_sequence
	movzbl	target_flags+3(%rip), %eax
	testq	$2, %rax
	je	.LBB648_4
# BB#1:                                 # %if.then
	movq	%rbx, %rdi
	movq	%r14, %rsi
	callq	gen_strmovqi_rex64
	jmp	.LBB648_2
.LBB648_4:                              # %if.end
	movl	x86_single_stringop(%rip), %eax
	movl	ix86_cpu(%rip), %ecx
	movq	$1, %rdx
	movl	optimize_size(%rip), %esi
	shlq	%cl, %rdx
	andq	%rax, %rdx
	orq	%rdx, %rsi
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %rsi
	je	.LBB648_6
# BB#5:                                 # %if.then9
	movq	%rbx, %rdi
	movq	%r14, %rsi
	movq	%rbx, %rdx
	movq	%r14, %rcx
	callq	gen_strmovqi_1
.LBB648_2:                              # %if.then
	movq	%rax, %rdi
	callq	emit_insn
.LBB648_3:                              # %if.then
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.LBB648_6:                              # %if.else
	movq	$2, %rdi
	callq	gen_reg_rtx
	movq	%rax, %r15
	movq	$2, %rdi
	movq	%r14, %rsi
	callq	gen_rtx_MEM
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r15, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	movq	$2, %rdi
	movq	%rbx, %rsi
	callq	gen_rtx_MEM
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	movq	%r15, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	movq	const_int_rtx+520(%rip), %rcx
	movq	$75, %rdi
	movq	$4, %rsi
	movq	%rbx, %rdx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rbx
	movq	$52, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rcx
	movq	$2, %rdi
	xorq	%rax, %rax
	movq	%rbx, %rsi
	movq	%rcx, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_E
	movq	%rax, %rdi
	callq	emit
	movq	const_int_rtx+520(%rip), %rcx
	movq	$75, %rdi
	movq	$4, %rsi
	movq	%r14, %rdx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rbx
	movq	$52, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rcx
	movq	$2, %rdi
	xorq	%rax, %rax
	movq	%rbx, %rsi
	movq	%rcx, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_E
	movq	%rax, %rdi
	callq	emit
	jmp	.LBB648_3
.Ltmp5919:
	.size	gen_strmovqi, .Ltmp5919-gen_strmovqi
	.cfi_endproc

	.globl	gen_strmovqi_rex64
	.align	16, 0x90
	.type	gen_strmovqi_rex64,@function
gen_strmovqi_rex64:                     # @gen_strmovqi_rex64
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp5924:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp5925:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp5926:
	.cfi_def_cfa_offset 32
.Ltmp5927:
	.cfi_offset %rbx, -32
.Ltmp5928:
	.cfi_offset %r14, -24
.Ltmp5929:
	.cfi_offset %r15, -16
	movq	%rsi, %r14
	movq	%rdi, %rbx
	callq	start_sequence
	movl	x86_single_stringop(%rip), %eax
	movl	ix86_cpu(%rip), %ecx
	movq	$1, %rdx
	movl	optimize_size(%rip), %esi
	shlq	%cl, %rdx
	andq	%rax, %rdx
	orq	%rdx, %rsi
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %rsi
	je	.LBB649_3
# BB#1:                                 # %if.then
	movq	%rbx, %rdi
	movq	%r14, %rsi
	movq	%rbx, %rdx
	movq	%r14, %rcx
	callq	gen_strmovqi_rex_1
	movq	%rax, %rdi
	callq	emit_insn
	jmp	.LBB649_2
.LBB649_3:                              # %if.else
	movq	$2, %rdi
	callq	gen_reg_rtx
	movq	%rax, %r15
	movq	$2, %rdi
	movq	%r14, %rsi
	callq	gen_rtx_MEM
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r15, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	movq	$2, %rdi
	movq	%rbx, %rsi
	callq	gen_rtx_MEM
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	movq	%r15, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	movq	const_int_rtx+520(%rip), %rcx
	movq	$75, %rdi
	movq	$5, %rsi
	movq	%rbx, %rdx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rbx
	movq	$52, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rcx
	movq	$2, %rdi
	xorq	%rax, %rax
	movq	%rbx, %rsi
	movq	%rcx, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_E
	movq	%rax, %rdi
	callq	emit
	movq	const_int_rtx+520(%rip), %rcx
	movq	$75, %rdi
	movq	$5, %rsi
	movq	%r14, %rdx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rbx
	movq	$52, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rcx
	movq	$2, %rdi
	xorq	%rax, %rax
	movq	%rbx, %rsi
	movq	%rcx, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_E
	movq	%rax, %rdi
	callq	emit
.LBB649_2:                              # %if.then
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp5930:
	.size	gen_strmovqi_rex64, .Ltmp5930-gen_strmovqi_rex64
	.cfi_endproc

	.globl	gen_clrstrsi
	.align	16, 0x90
	.type	gen_clrstrsi,@function
gen_clrstrsi:                           # @gen_clrstrsi
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp5935:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp5936:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp5937:
	.cfi_def_cfa_offset 32
.Ltmp5938:
	.cfi_offset %rbx, -32
.Ltmp5939:
	.cfi_offset %r14, -24
.Ltmp5940:
	.cfi_offset %r15, -16
	movq	%rdx, %r14
	movq	%rsi, %r15
	movq	%rdi, %rbx
	callq	start_sequence
	movq	%rbx, %rdi
	movq	%r15, %rsi
	movq	%r14, %rdx
	callq	ix86_expand_clrstr
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB650_2
# BB#1:                                 # %if.then
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	jmp	.LBB650_3
.LBB650_2:                              # %return
	callq	end_sequence
	xorq	%rax, %rax
.LBB650_3:                              # %return
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp5941:
	.size	gen_clrstrsi, .Ltmp5941-gen_clrstrsi
	.cfi_endproc

	.globl	gen_clrstrdi
	.align	16, 0x90
	.type	gen_clrstrdi,@function
gen_clrstrdi:                           # @gen_clrstrdi
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp5946:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp5947:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp5948:
	.cfi_def_cfa_offset 32
.Ltmp5949:
	.cfi_offset %rbx, -32
.Ltmp5950:
	.cfi_offset %r14, -24
.Ltmp5951:
	.cfi_offset %r15, -16
	movq	%rdx, %r14
	movq	%rsi, %r15
	movq	%rdi, %rbx
	callq	start_sequence
	movq	%rbx, %rdi
	movq	%r15, %rsi
	movq	%r14, %rdx
	callq	ix86_expand_clrstr
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB651_2
# BB#1:                                 # %if.then
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	jmp	.LBB651_3
.LBB651_2:                              # %return
	callq	end_sequence
	xorq	%rax, %rax
.LBB651_3:                              # %return
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp5952:
	.size	gen_clrstrdi, .Ltmp5952-gen_clrstrdi
	.cfi_endproc

	.globl	gen_strsetdi_rex64
	.align	16, 0x90
	.type	gen_strsetdi_rex64,@function
gen_strsetdi_rex64:                     # @gen_strsetdi_rex64
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp5956:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp5957:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp5958:
	.cfi_def_cfa_offset 32
.Ltmp5959:
	.cfi_offset %rbx, -24
.Ltmp5960:
	.cfi_offset %r14, -16
	movq	%rsi, %r14
	movq	%rdi, %rbx
	callq	start_sequence
	movl	x86_single_stringop(%rip), %eax
	movl	ix86_cpu(%rip), %ecx
	movq	$1, %rdx
	movl	optimize_size(%rip), %esi
	shlq	%cl, %rdx
	andq	%rax, %rdx
	orq	%rdx, %rsi
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %rsi
	je	.LBB652_3
# BB#1:                                 # %if.then
	movq	$4, %rdi
	movq	%rbx, %rsi
	callq	gen_rtx_MEM
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	movq	%r14, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %r14
	xorq	%rdi, %rdi
	movq	$8, %rsi
	callq	gen_rtx_CONST_INT
	movq	$75, %rdi
	movq	$5, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rbx
	movq	$4, %rdi
	movq	$19, %rsi
	callq	gen_rtx_REG
	movq	$48, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rcx
	movq	$3, %rdi
	xorq	%rax, %rax
	movq	%r14, %rsi
	movq	%rbx, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_E
	movq	%rax, %rdi
	callq	emit_insn
	jmp	.LBB652_2
.LBB652_3:                              # %if.end
	movq	$5, %rdi
	movq	%rbx, %rsi
	callq	gen_rtx_MEM
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	movq	%r14, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	xorq	%rdi, %rdi
	movq	$8, %rsi
	callq	gen_rtx_CONST_INT
	movq	$75, %rdi
	movq	$5, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rbx
	movq	$52, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rcx
	movq	$2, %rdi
	xorq	%rax, %rax
	movq	%rbx, %rsi
	movq	%rcx, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_E
	movq	%rax, %rdi
	callq	emit
.LBB652_2:                              # %if.then
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp5961:
	.size	gen_strsetdi_rex64, .Ltmp5961-gen_strsetdi_rex64
	.cfi_endproc

	.globl	gen_strsetsi
	.align	16, 0x90
	.type	gen_strsetsi,@function
gen_strsetsi:                           # @gen_strsetsi
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp5966:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp5967:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp5968:
	.cfi_def_cfa_offset 32
.Ltmp5969:
	.cfi_offset %rbx, -32
.Ltmp5970:
	.cfi_offset %r14, -24
.Ltmp5971:
	.cfi_offset %r15, -16
	movq	%rsi, %r14
	movq	%rdi, %r15
	callq	start_sequence
	movzbl	target_flags+3(%rip), %eax
	testq	$2, %rax
	je	.LBB653_4
# BB#1:                                 # %if.then
	movq	%r15, %rdi
	movq	%r14, %rsi
	callq	gen_strsetsi_rex64
	jmp	.LBB653_2
.LBB653_4:                              # %if.else
	movl	ix86_cpu(%rip), %ecx
	movl	x86_single_stringop(%rip), %eax
	movl	optimize_size(%rip), %ebx
	movq	$1, %rdx
	shlq	%cl, %rdx
	andq	%rax, %rdx
	orq	%rdx, %rbx
	movq	$4, %rdi
	movq	%r15, %rsi
	callq	gen_rtx_MEM
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	movq	%r14, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %r14
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %rbx
	je	.LBB653_6
# BB#5:                                 # %if.then9
	xorq	%rdi, %rdi
	movq	$4, %rsi
	callq	gen_rtx_CONST_INT
	movq	$75, %rdi
	movq	$4, %rsi
	movq	%r15, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r15, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rbx
	movq	$4, %rdi
	movq	$19, %rsi
	callq	gen_rtx_REG
	movq	$48, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rcx
	movq	$3, %rdi
	xorq	%rax, %rax
	movq	%r14, %rsi
	movq	%rbx, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_E
.LBB653_2:                              # %if.then
	movq	%rax, %rdi
	callq	emit_insn
.LBB653_3:                              # %if.then
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.LBB653_6:                              # %if.end16
	movq	%r14, %rdi
	callq	emit_insn
	xorq	%rdi, %rdi
	movq	$4, %rsi
	callq	gen_rtx_CONST_INT
	movq	$75, %rdi
	movq	$4, %rsi
	movq	%r15, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r15, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rbx
	movq	$52, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rcx
	movq	$2, %rdi
	xorq	%rax, %rax
	movq	%rbx, %rsi
	movq	%rcx, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_E
	movq	%rax, %rdi
	callq	emit
	jmp	.LBB653_3
.Ltmp5972:
	.size	gen_strsetsi, .Ltmp5972-gen_strsetsi
	.cfi_endproc

	.globl	gen_strsetsi_rex64
	.align	16, 0x90
	.type	gen_strsetsi_rex64,@function
gen_strsetsi_rex64:                     # @gen_strsetsi_rex64
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp5977:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp5978:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp5979:
	.cfi_def_cfa_offset 32
.Ltmp5980:
	.cfi_offset %rbx, -32
.Ltmp5981:
	.cfi_offset %r14, -24
.Ltmp5982:
	.cfi_offset %r15, -16
	movq	%rsi, %r14
	movq	%rdi, %r15
	callq	start_sequence
	movl	ix86_cpu(%rip), %ecx
	movl	x86_single_stringop(%rip), %eax
	movl	optimize_size(%rip), %ebx
	movq	$1, %rdx
	shlq	%cl, %rdx
	andq	%rax, %rdx
	orq	%rdx, %rbx
	movq	$4, %rdi
	movq	%r15, %rsi
	callq	gen_rtx_MEM
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	movq	%r14, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %r14
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %rbx
	je	.LBB654_3
# BB#1:                                 # %if.then
	xorq	%rdi, %rdi
	movq	$4, %rsi
	callq	gen_rtx_CONST_INT
	movq	$75, %rdi
	movq	$5, %rsi
	movq	%r15, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r15, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rbx
	movq	$4, %rdi
	movq	$19, %rsi
	callq	gen_rtx_REG
	movq	$48, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rcx
	movq	$3, %rdi
	xorq	%rax, %rax
	movq	%r14, %rsi
	movq	%rbx, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_E
	movq	%rax, %rdi
	callq	emit_insn
	jmp	.LBB654_2
.LBB654_3:                              # %if.end
	movq	%r14, %rdi
	callq	emit_insn
	xorq	%rdi, %rdi
	movq	$4, %rsi
	callq	gen_rtx_CONST_INT
	movq	$75, %rdi
	movq	$5, %rsi
	movq	%r15, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r15, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rbx
	movq	$52, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rcx
	movq	$2, %rdi
	xorq	%rax, %rax
	movq	%rbx, %rsi
	movq	%rcx, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_E
	movq	%rax, %rdi
	callq	emit
.LBB654_2:                              # %if.then
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp5983:
	.size	gen_strsetsi_rex64, .Ltmp5983-gen_strsetsi_rex64
	.cfi_endproc

	.globl	gen_strsethi
	.align	16, 0x90
	.type	gen_strsethi,@function
gen_strsethi:                           # @gen_strsethi
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp5988:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp5989:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp5990:
	.cfi_def_cfa_offset 32
.Ltmp5991:
	.cfi_offset %rbx, -32
.Ltmp5992:
	.cfi_offset %r14, -24
.Ltmp5993:
	.cfi_offset %r15, -16
	movq	%rsi, %r14
	movq	%rdi, %r15
	callq	start_sequence
	movzbl	target_flags+3(%rip), %eax
	testq	$2, %rax
	je	.LBB655_4
# BB#1:                                 # %if.then
	movq	%r15, %rdi
	movq	%r14, %rsi
	callq	gen_strsethi_rex64
	jmp	.LBB655_2
.LBB655_4:                              # %if.else
	movl	ix86_cpu(%rip), %ecx
	movl	x86_single_stringop(%rip), %eax
	movl	optimize_size(%rip), %ebx
	movq	$1, %rdx
	shlq	%cl, %rdx
	andq	%rax, %rdx
	orq	%rdx, %rbx
	movq	$3, %rdi
	movq	%r15, %rsi
	callq	gen_rtx_MEM
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	movq	%r14, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %r14
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %rbx
	je	.LBB655_6
# BB#5:                                 # %if.then9
	xorq	%rdi, %rdi
	movq	$2, %rsi
	callq	gen_rtx_CONST_INT
	movq	$75, %rdi
	movq	$4, %rsi
	movq	%r15, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r15, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rbx
	movq	$4, %rdi
	movq	$19, %rsi
	callq	gen_rtx_REG
	movq	$48, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rcx
	movq	$3, %rdi
	xorq	%rax, %rax
	movq	%r14, %rsi
	movq	%rbx, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_E
.LBB655_2:                              # %if.then
	movq	%rax, %rdi
	callq	emit_insn
.LBB655_3:                              # %if.then
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.LBB655_6:                              # %if.end16
	movq	%r14, %rdi
	callq	emit_insn
	xorq	%rdi, %rdi
	movq	$2, %rsi
	callq	gen_rtx_CONST_INT
	movq	$75, %rdi
	movq	$4, %rsi
	movq	%r15, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r15, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rbx
	movq	$52, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rcx
	movq	$2, %rdi
	xorq	%rax, %rax
	movq	%rbx, %rsi
	movq	%rcx, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_E
	movq	%rax, %rdi
	callq	emit
	jmp	.LBB655_3
.Ltmp5994:
	.size	gen_strsethi, .Ltmp5994-gen_strsethi
	.cfi_endproc

	.globl	gen_strsethi_rex64
	.align	16, 0x90
	.type	gen_strsethi_rex64,@function
gen_strsethi_rex64:                     # @gen_strsethi_rex64
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp5999:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp6000:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp6001:
	.cfi_def_cfa_offset 32
.Ltmp6002:
	.cfi_offset %rbx, -32
.Ltmp6003:
	.cfi_offset %r14, -24
.Ltmp6004:
	.cfi_offset %r15, -16
	movq	%rsi, %r14
	movq	%rdi, %r15
	callq	start_sequence
	movl	ix86_cpu(%rip), %ecx
	movl	x86_single_stringop(%rip), %eax
	movl	optimize_size(%rip), %ebx
	movq	$1, %rdx
	shlq	%cl, %rdx
	andq	%rax, %rdx
	orq	%rdx, %rbx
	movq	$3, %rdi
	movq	%r15, %rsi
	callq	gen_rtx_MEM
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	movq	%r14, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %r14
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %rbx
	je	.LBB656_3
# BB#1:                                 # %if.then
	xorq	%rdi, %rdi
	movq	$2, %rsi
	callq	gen_rtx_CONST_INT
	movq	$75, %rdi
	movq	$5, %rsi
	movq	%r15, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r15, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rbx
	movq	$4, %rdi
	movq	$19, %rsi
	callq	gen_rtx_REG
	movq	$48, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rcx
	movq	$3, %rdi
	xorq	%rax, %rax
	movq	%r14, %rsi
	movq	%rbx, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_E
	movq	%rax, %rdi
	callq	emit_insn
	jmp	.LBB656_2
.LBB656_3:                              # %if.end
	movq	%r14, %rdi
	callq	emit_insn
	xorq	%rdi, %rdi
	movq	$2, %rsi
	callq	gen_rtx_CONST_INT
	movq	$75, %rdi
	movq	$5, %rsi
	movq	%r15, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r15, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rbx
	movq	$52, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rcx
	movq	$2, %rdi
	xorq	%rax, %rax
	movq	%rbx, %rsi
	movq	%rcx, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_E
	movq	%rax, %rdi
	callq	emit
.LBB656_2:                              # %if.then
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp6005:
	.size	gen_strsethi_rex64, .Ltmp6005-gen_strsethi_rex64
	.cfi_endproc

	.globl	gen_strsetqi
	.align	16, 0x90
	.type	gen_strsetqi,@function
gen_strsetqi:                           # @gen_strsetqi
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp6010:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp6011:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp6012:
	.cfi_def_cfa_offset 32
.Ltmp6013:
	.cfi_offset %rbx, -32
.Ltmp6014:
	.cfi_offset %r14, -24
.Ltmp6015:
	.cfi_offset %r15, -16
	movq	%rsi, %r14
	movq	%rdi, %r15
	callq	start_sequence
	movzbl	target_flags+3(%rip), %eax
	testq	$2, %rax
	je	.LBB657_4
# BB#1:                                 # %if.then
	movq	%r15, %rdi
	movq	%r14, %rsi
	callq	gen_strsetqi_rex64
	jmp	.LBB657_2
.LBB657_4:                              # %if.else
	movl	ix86_cpu(%rip), %ecx
	movl	x86_single_stringop(%rip), %eax
	movl	optimize_size(%rip), %ebx
	movq	$1, %rdx
	shlq	%cl, %rdx
	andq	%rax, %rdx
	orq	%rdx, %rbx
	movq	$2, %rdi
	movq	%r15, %rsi
	callq	gen_rtx_MEM
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	movq	%r14, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %r14
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %rbx
	je	.LBB657_6
# BB#5:                                 # %if.then9
	movq	const_int_rtx+520(%rip), %rcx
	movq	$75, %rdi
	movq	$4, %rsi
	movq	%r15, %rdx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r15, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rbx
	movq	$4, %rdi
	movq	$19, %rsi
	callq	gen_rtx_REG
	movq	$48, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rcx
	movq	$3, %rdi
	xorq	%rax, %rax
	movq	%r14, %rsi
	movq	%rbx, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_E
.LBB657_2:                              # %if.then
	movq	%rax, %rdi
	callq	emit_insn
.LBB657_3:                              # %if.then
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.LBB657_6:                              # %if.end16
	movq	%r14, %rdi
	callq	emit_insn
	movq	const_int_rtx+520(%rip), %rcx
	movq	$75, %rdi
	movq	$4, %rsi
	movq	%r15, %rdx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r15, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rbx
	movq	$52, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rcx
	movq	$2, %rdi
	xorq	%rax, %rax
	movq	%rbx, %rsi
	movq	%rcx, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_E
	movq	%rax, %rdi
	callq	emit
	jmp	.LBB657_3
.Ltmp6016:
	.size	gen_strsetqi, .Ltmp6016-gen_strsetqi
	.cfi_endproc

	.globl	gen_strsetqi_rex64
	.align	16, 0x90
	.type	gen_strsetqi_rex64,@function
gen_strsetqi_rex64:                     # @gen_strsetqi_rex64
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp6021:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp6022:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp6023:
	.cfi_def_cfa_offset 32
.Ltmp6024:
	.cfi_offset %rbx, -32
.Ltmp6025:
	.cfi_offset %r14, -24
.Ltmp6026:
	.cfi_offset %r15, -16
	movq	%rsi, %r14
	movq	%rdi, %r15
	callq	start_sequence
	movl	ix86_cpu(%rip), %ecx
	movl	x86_single_stringop(%rip), %eax
	movl	optimize_size(%rip), %ebx
	movq	$1, %rdx
	shlq	%cl, %rdx
	andq	%rax, %rdx
	orq	%rdx, %rbx
	movq	$2, %rdi
	movq	%r15, %rsi
	callq	gen_rtx_MEM
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	movq	%r14, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %r14
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %rbx
	je	.LBB658_3
# BB#1:                                 # %if.then
	movq	const_int_rtx+520(%rip), %rcx
	movq	$75, %rdi
	movq	$5, %rsi
	movq	%r15, %rdx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r15, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rbx
	movq	$4, %rdi
	movq	$19, %rsi
	callq	gen_rtx_REG
	movq	$48, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rcx
	movq	$3, %rdi
	xorq	%rax, %rax
	movq	%r14, %rsi
	movq	%rbx, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_E
	movq	%rax, %rdi
	callq	emit_insn
	jmp	.LBB658_2
.LBB658_3:                              # %if.end
	movq	%r14, %rdi
	callq	emit_insn
	movq	const_int_rtx+520(%rip), %rcx
	movq	$75, %rdi
	movq	$5, %rsi
	movq	%r15, %rdx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r15, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rbx
	movq	$52, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rcx
	movq	$2, %rdi
	xorq	%rax, %rax
	movq	%rbx, %rsi
	movq	%rcx, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_E
	movq	%rax, %rdi
	callq	emit
.LBB658_2:                              # %if.then
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp6027:
	.size	gen_strsetqi_rex64, .Ltmp6027-gen_strsetqi_rex64
	.cfi_endproc

	.globl	gen_cmpstrsi
	.align	16, 0x90
	.type	gen_cmpstrsi,@function
gen_cmpstrsi:                           # @gen_cmpstrsi
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp6034:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp6035:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp6036:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp6037:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp6038:
	.cfi_def_cfa_offset 48
	subq	$32, %rsp
.Ltmp6039:
	.cfi_def_cfa_offset 80
.Ltmp6040:
	.cfi_offset %rbx, -48
.Ltmp6041:
	.cfi_offset %r12, -40
.Ltmp6042:
	.cfi_offset %r13, -32
.Ltmp6043:
	.cfi_offset %r14, -24
.Ltmp6044:
	.cfi_offset %r15, -16
	movq	%r8, 16(%rsp)           # 8-byte Spill
	movq	%rcx, %r14
	movq	%rdx, %r15
	movq	%rsi, %r12
	movq	%rdi, %rbx
	movq	%rbx, 24(%rsp)          # 8-byte Spill
	callq	start_sequence
	movzbl	(%rbx), %eax
	cmpq	$61, %rax
	movq	%rbx, %r13
	je	.LBB659_2
# BB#1:                                 # %if.then
	movq	$4, %rdi
	callq	gen_reg_rtx
	movq	%rax, %r13
.LBB659_2:                              # %if.end
	movl	target_flags(%rip), %edi
	movq	$25, %rcx
	movq	8(%r12), %rsi
	shrq	%cl, %rdi
	andq	$1, %rdi
	orq	$4, %rdi
	callq	copy_to_mode_reg
	movl	target_flags(%rip), %edi
	movq	%rax, %rbx
	movq	$25, %rcx
	movq	8(%r15), %rsi
	shrq	%cl, %rdi
	andq	$1, %rdi
	orq	$4, %rdi
	callq	copy_to_mode_reg
	movq	%rax, %r12
	movq	%r14, %rdi
	callq	ix86_zero_extend_to_Pmode
	movq	%rax, %r15
	movq	$4, %rdi
	movq	$19, %rsi
	callq	gen_rtx_REG
	movq	const_int_rtx+512(%rip), %rcx
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	movzbl	(%r14), %eax
	cmpq	$54, %rax
	jne	.LBB659_8
# BB#3:                                 # %if.then25
	cmpq	$0, 8(%r14)
	je	.LBB659_4
# BB#5:                                 # %if.end33
	movzbl	target_flags+3(%rip), %eax
	testq	$2, %rax
	je	.LBB659_7
# BB#6:                                 # %if.then36
	movq	%r15, (%rsp)
	movq	%rbx, %rdi
	movq	%r12, %rsi
	movq	%r15, %rdx
	movq	16(%rsp), %rcx          # 8-byte Reload
	movq	%rbx, %r8
	movq	%r12, %r9
	callq	gen_cmpstrqi_nz_rex_1
	jmp	.LBB659_11
.LBB659_8:                              # %if.else42
	movq	%rbx, %r14
	movq	$52, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	%rax, %rbx
	movq	$74, %rdi
	movq	$52, %rsi
	movq	%r15, %rdx
	movq	%r15, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	movl	target_flags(%rip), %eax
	testq	$33554432, %rax         # imm = 0x2000000
	je	.LBB659_10
# BB#9:                                 # %if.then45
	movq	%r15, (%rsp)
	movq	%r14, %rdi
	movq	%r12, %rsi
	movq	%r15, %rdx
	movq	16(%rsp), %rcx          # 8-byte Reload
	movq	%rdi, %r8
	movq	%r12, %r9
	callq	gen_cmpstrqi_rex_1
	jmp	.LBB659_11
.LBB659_4:                              # %if.then29
	movq	const_int_rtx+512(%rip), %rsi
	movq	24(%rsp), %rdi          # 8-byte Reload
	jmp	.LBB659_13
.LBB659_10:                             # %if.else50
	movq	%r15, (%rsp)
	movq	%r14, %rdi
	movq	%r12, %rsi
	movq	%r15, %rdx
	movq	16(%rsp), %rcx          # 8-byte Reload
	movq	%rdi, %r8
	movq	%r12, %r9
	callq	gen_cmpstrqi_1
	jmp	.LBB659_11
.LBB659_7:                              # %if.else
	movq	%r15, (%rsp)
	movq	%rbx, %rdi
	movq	%r12, %rsi
	movq	%r15, %rdx
	movq	16(%rsp), %rcx          # 8-byte Reload
	movq	%rbx, %r8
	movq	%r12, %r9
	callq	gen_cmpstrqi_nz_1
.LBB659_11:                             # %if.end56
	movq	%rax, %rdi
	callq	emit_insn
	movq	$2, %rdi
	movq	%r13, %rsi
	callq	gen_lowpart
	movq	%rax, %rbx
	movq	%rbx, %rdi
	callq	gen_cmpintqi
	movq	%rax, %rdi
	callq	emit_insn
	movq	$120, %rdi
	movq	$4, %rsi
	movq	%rbx, %rdx
	callq	gen_rtx_fmt_e
	movq	%r13, %rdi
	movq	%rax, %rsi
	callq	emit_move_insn
	movq	24(%rsp), %rdi          # 8-byte Reload
	cmpq	%rdi, %r13
	je	.LBB659_14
# BB#12:                                # %if.then64
	movq	%r13, %rsi
.LBB659_13:                             # %if.end67
	callq	emit_move_insn
.LBB659_14:                             # %if.end67
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp6045:
	.size	gen_cmpstrsi, .Ltmp6045-gen_cmpstrsi
	.cfi_endproc

	.globl	gen_cmpintqi
	.align	16, 0x90
	.type	gen_cmpintqi,@function
gen_cmpintqi:                           # @gen_cmpintqi
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp6050:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp6051:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp6052:
	.cfi_def_cfa_offset 32
.Ltmp6053:
	.cfi_offset %rbx, -32
.Ltmp6054:
	.cfi_offset %r14, -24
.Ltmp6055:
	.cfi_offset %r15, -16
	movq	%rdi, %r14
	callq	start_sequence
	movq	$2, %rdi
	callq	gen_reg_rtx
	movq	%rax, %r15
	movq	$2, %rdi
	callq	gen_reg_rtx
	movq	%rax, %rbx
	movq	$52, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	const_int_rtx+512(%rip), %rcx
	movq	$109, %rdi
	movq	$2, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r15, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	movq	$52, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	const_int_rtx+512(%rip), %rcx
	movq	$111, %rdi
	movq	$2, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	movq	$76, %rdi
	movq	$2, %rsi
	movq	%r15, %rdx
	movq	%rbx, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rbx
	movq	$52, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rcx
	movq	$2, %rdi
	xorq	%rax, %rax
	movq	%rbx, %rsi
	movq	%rcx, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_E
	movq	%rax, %rdi
	callq	emit
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp6056:
	.size	gen_cmpintqi, .Ltmp6056-gen_cmpintqi
	.cfi_endproc

	.globl	gen_strlensi
	.align	16, 0x90
	.type	gen_strlensi,@function
gen_strlensi:                           # @gen_strlensi
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp6062:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp6063:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp6064:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp6065:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp6066:
	.cfi_def_cfa_offset 48
.Ltmp6067:
	.cfi_offset %rbx, -40
.Ltmp6068:
	.cfi_offset %r12, -32
.Ltmp6069:
	.cfi_offset %r14, -24
.Ltmp6070:
	.cfi_offset %r15, -16
	movq	%rcx, %r14
	movq	%rdx, %r15
	movq	%rsi, %r12
	movq	%rdi, %rbx
	callq	start_sequence
	movq	%rbx, %rdi
	movq	%r12, %rsi
	movq	%r15, %rdx
	movq	%r14, %rcx
	callq	ix86_expand_strlen
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB661_2
# BB#1:                                 # %if.then
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	jmp	.LBB661_3
.LBB661_2:                              # %return
	callq	end_sequence
	xorq	%rax, %rax
.LBB661_3:                              # %return
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.Ltmp6071:
	.size	gen_strlensi, .Ltmp6071-gen_strlensi
	.cfi_endproc

	.globl	gen_strlendi
	.align	16, 0x90
	.type	gen_strlendi,@function
gen_strlendi:                           # @gen_strlendi
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp6077:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp6078:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp6079:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp6080:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp6081:
	.cfi_def_cfa_offset 48
.Ltmp6082:
	.cfi_offset %rbx, -40
.Ltmp6083:
	.cfi_offset %r12, -32
.Ltmp6084:
	.cfi_offset %r14, -24
.Ltmp6085:
	.cfi_offset %r15, -16
	movq	%rcx, %r14
	movq	%rdx, %r15
	movq	%rsi, %r12
	movq	%rdi, %rbx
	callq	start_sequence
	movq	%rbx, %rdi
	movq	%r12, %rsi
	movq	%r15, %rdx
	movq	%r14, %rcx
	callq	ix86_expand_strlen
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB662_2
# BB#1:                                 # %if.then
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	jmp	.LBB662_3
.LBB662_2:                              # %return
	callq	end_sequence
	xorq	%rax, %rax
.LBB662_3:                              # %return
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.Ltmp6086:
	.size	gen_strlendi, .Ltmp6086-gen_strlendi
	.cfi_endproc

	.globl	gen_peephole2_1207
	.align	16, 0x90
	.type	gen_peephole2_1207,@function
gen_peephole2_1207:                     # @gen_peephole2_1207
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp6093:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp6094:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp6095:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp6096:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp6097:
	.cfi_def_cfa_offset 48
	subq	$64, %rsp
.Ltmp6098:
	.cfi_def_cfa_offset 112
.Ltmp6099:
	.cfi_offset %rbx, -48
.Ltmp6100:
	.cfi_offset %r12, -40
.Ltmp6101:
	.cfi_offset %r13, -32
.Ltmp6102:
	.cfi_offset %r14, -24
.Ltmp6103:
	.cfi_offset %r15, -16
	movq	%rsi, %rbx
	callq	start_sequence
	movq	(%rbx), %rax
	movq	%rax, 40(%rsp)          # 8-byte Spill
	movq	8(%rbx), %rax
	movq	%rax, 48(%rsp)          # 8-byte Spill
	movq	16(%rbx), %rax
	movq	%rax, 56(%rsp)          # 8-byte Spill
	movq	24(%rbx), %rax
	movq	%rax, 24(%rsp)          # 8-byte Spill
	movq	32(%rbx), %r15
	movq	40(%rbx), %r13
	movq	48(%rbx), %r14
	movq	$52, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	%rax, %r12
	movq	$51, %rdi
	movq	%r15, %rsi
	callq	gen_rtx_MEM
	movq	%rax, %rbx
	movq	$51, %rdi
	movq	%r13, %rsi
	callq	gen_rtx_MEM
	movq	$74, %rdi
	movq	$52, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r12, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, 32(%rsp)          # 8-byte Spill
	movq	$48, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %r15
	movq	$48, %rdi
	xorq	%rsi, %rsi
	movq	24(%rsp), %rdx          # 8-byte Reload
	callq	gen_rtx_fmt_e
	movq	%rax, %r14
	movq	$4, %rdi
	movq	$19, %rsi
	callq	gen_rtx_REG
	movq	$48, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %r12
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	40(%rsp), %rdx          # 8-byte Reload
	callq	gen_rtx_fmt_e
	movq	%rax, %rbx
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	48(%rsp), %rdx          # 8-byte Reload
	callq	gen_rtx_fmt_e
	movq	%rax, %r13
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	56(%rsp), %rdx          # 8-byte Reload
	callq	gen_rtx_fmt_e
	movq	%rax, 8(%rsp)
	movq	$7, %rdi
	xorq	%rax, %rax
	movq	%r13, (%rsp)
	movq	32(%rsp), %rsi          # 8-byte Reload
	movq	%r15, %rdx
	movq	%r14, %rcx
	movq	%r12, %r8
	movq	%rbx, %r9
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_E
	movq	%rax, %rdi
	callq	emit
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	addq	$64, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp6104:
	.size	gen_peephole2_1207, .Ltmp6104-gen_peephole2_1207
	.cfi_endproc

	.globl	gen_peephole2_1208
	.align	16, 0x90
	.type	gen_peephole2_1208,@function
gen_peephole2_1208:                     # @gen_peephole2_1208
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp6111:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp6112:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp6113:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp6114:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp6115:
	.cfi_def_cfa_offset 48
	subq	$64, %rsp
.Ltmp6116:
	.cfi_def_cfa_offset 112
.Ltmp6117:
	.cfi_offset %rbx, -48
.Ltmp6118:
	.cfi_offset %r12, -40
.Ltmp6119:
	.cfi_offset %r13, -32
.Ltmp6120:
	.cfi_offset %r14, -24
.Ltmp6121:
	.cfi_offset %r15, -16
	movq	%rsi, %rbx
	callq	start_sequence
	movq	(%rbx), %rax
	movq	%rax, 40(%rsp)          # 8-byte Spill
	movq	8(%rbx), %rax
	movq	%rax, 48(%rsp)          # 8-byte Spill
	movq	16(%rbx), %rax
	movq	%rax, 56(%rsp)          # 8-byte Spill
	movq	24(%rbx), %rax
	movq	%rax, 24(%rsp)          # 8-byte Spill
	movq	32(%rbx), %r15
	movq	40(%rbx), %r13
	movq	48(%rbx), %r12
	movq	$52, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	const_int_rtx+512(%rip), %rcx
	movq	%rax, %r14
	movq	$102, %rdi
	xorq	%rsi, %rsi
	movq	%r12, %rdx
	callq	gen_rtx_fmt_ee
	movq	%rax, %r12
	movq	$51, %rdi
	movq	%r15, %rsi
	callq	gen_rtx_MEM
	movq	%rax, %rbx
	movq	$51, %rdi
	movq	%r13, %rsi
	callq	gen_rtx_MEM
	movq	$74, %rdi
	movq	$52, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	const_int_rtx+512(%rip), %r8
	movq	$72, %rdi
	movq	$52, %rsi
	movq	%r12, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_eee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, 32(%rsp)          # 8-byte Spill
	movq	$48, %rdi
	xorq	%rsi, %rsi
	movq	24(%rsp), %rdx          # 8-byte Reload
	callq	gen_rtx_fmt_e
	movq	%rax, %r12
	movq	$52, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	$48, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %r14
	movq	$4, %rdi
	movq	$19, %rsi
	callq	gen_rtx_REG
	movq	$48, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %r15
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	40(%rsp), %rdx          # 8-byte Reload
	callq	gen_rtx_fmt_e
	movq	%rax, %rbx
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	48(%rsp), %rdx          # 8-byte Reload
	callq	gen_rtx_fmt_e
	movq	%rax, %r13
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	56(%rsp), %rdx          # 8-byte Reload
	callq	gen_rtx_fmt_e
	movq	%rax, 8(%rsp)
	movq	$7, %rdi
	xorq	%rax, %rax
	movq	%r13, (%rsp)
	movq	32(%rsp), %rsi          # 8-byte Reload
	movq	%r12, %rdx
	movq	%r14, %rcx
	movq	%r15, %r8
	movq	%rbx, %r9
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_E
	movq	%rax, %rdi
	callq	emit
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	addq	$64, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp6122:
	.size	gen_peephole2_1208, .Ltmp6122-gen_peephole2_1208
	.cfi_endproc

	.globl	gen_movdicc
	.align	16, 0x90
	.type	gen_movdicc,@function
gen_movdicc:                            # @gen_movdicc
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp6128:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp6129:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp6130:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp6131:
	.cfi_def_cfa_offset 40
	subq	$40, %rsp
.Ltmp6132:
	.cfi_def_cfa_offset 80
.Ltmp6133:
	.cfi_offset %rbx, -40
.Ltmp6134:
	.cfi_offset %r12, -32
.Ltmp6135:
	.cfi_offset %r14, -24
.Ltmp6136:
	.cfi_offset %r15, -16
	movq	%rcx, %r14
	movq	%rdx, %r15
	movq	%rsi, %r12
	movq	%rdi, %rbx
	callq	start_sequence
	movq	%rbx, (%rsp)
	movq	%r12, 8(%rsp)
	movq	%r15, 16(%rsp)
	movq	%r14, 24(%rsp)
	leaq	(%rsp), %rdi
	callq	ix86_expand_int_movcc
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB665_1
# BB#2:                                 # %if.end
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	jmp	.LBB665_3
.LBB665_1:                              # %if.then
	callq	end_sequence
	xorq	%rbx, %rbx
.LBB665_3:                              # %return
	movq	%rbx, %rax
	addq	$40, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.Ltmp6137:
	.size	gen_movdicc, .Ltmp6137-gen_movdicc
	.cfi_endproc

	.globl	gen_movsicc
	.align	16, 0x90
	.type	gen_movsicc,@function
gen_movsicc:                            # @gen_movsicc
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp6143:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp6144:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp6145:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp6146:
	.cfi_def_cfa_offset 40
	subq	$40, %rsp
.Ltmp6147:
	.cfi_def_cfa_offset 80
.Ltmp6148:
	.cfi_offset %rbx, -40
.Ltmp6149:
	.cfi_offset %r12, -32
.Ltmp6150:
	.cfi_offset %r14, -24
.Ltmp6151:
	.cfi_offset %r15, -16
	movq	%rcx, %r14
	movq	%rdx, %r15
	movq	%rsi, %r12
	movq	%rdi, %rbx
	callq	start_sequence
	movq	%rbx, (%rsp)
	movq	%r12, 8(%rsp)
	movq	%r15, 16(%rsp)
	movq	%r14, 24(%rsp)
	leaq	(%rsp), %rdi
	callq	ix86_expand_int_movcc
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB666_1
# BB#2:                                 # %if.end
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	jmp	.LBB666_3
.LBB666_1:                              # %if.then
	callq	end_sequence
	xorq	%rbx, %rbx
.LBB666_3:                              # %return
	movq	%rbx, %rax
	addq	$40, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.Ltmp6152:
	.size	gen_movsicc, .Ltmp6152-gen_movsicc
	.cfi_endproc

	.globl	gen_movhicc
	.align	16, 0x90
	.type	gen_movhicc,@function
gen_movhicc:                            # @gen_movhicc
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp6158:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp6159:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp6160:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp6161:
	.cfi_def_cfa_offset 40
	subq	$40, %rsp
.Ltmp6162:
	.cfi_def_cfa_offset 80
.Ltmp6163:
	.cfi_offset %rbx, -40
.Ltmp6164:
	.cfi_offset %r12, -32
.Ltmp6165:
	.cfi_offset %r14, -24
.Ltmp6166:
	.cfi_offset %r15, -16
	movq	%rcx, %r14
	movq	%rdx, %r15
	movq	%rsi, %r12
	movq	%rdi, %rbx
	callq	start_sequence
	movq	%rbx, (%rsp)
	movq	%r12, 8(%rsp)
	movq	%r15, 16(%rsp)
	movq	%r14, 24(%rsp)
	leaq	(%rsp), %rdi
	callq	ix86_expand_int_movcc
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB667_1
# BB#2:                                 # %if.end
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	jmp	.LBB667_3
.LBB667_1:                              # %if.then
	callq	end_sequence
	xorq	%rbx, %rbx
.LBB667_3:                              # %return
	movq	%rbx, %rax
	addq	$40, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.Ltmp6167:
	.size	gen_movhicc, .Ltmp6167-gen_movhicc
	.cfi_endproc

	.globl	gen_movsfcc
	.align	16, 0x90
	.type	gen_movsfcc,@function
gen_movsfcc:                            # @gen_movsfcc
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp6173:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp6174:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp6175:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp6176:
	.cfi_def_cfa_offset 40
	subq	$40, %rsp
.Ltmp6177:
	.cfi_def_cfa_offset 80
.Ltmp6178:
	.cfi_offset %rbx, -40
.Ltmp6179:
	.cfi_offset %r12, -32
.Ltmp6180:
	.cfi_offset %r14, -24
.Ltmp6181:
	.cfi_offset %r15, -16
	movq	%rcx, %r14
	movq	%rdx, %r15
	movq	%rsi, %r12
	movq	%rdi, %rbx
	callq	start_sequence
	movq	%rbx, (%rsp)
	movq	%r12, 8(%rsp)
	movq	%r15, 16(%rsp)
	movq	%r14, 24(%rsp)
	leaq	(%rsp), %rdi
	callq	ix86_expand_fp_movcc
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB668_1
# BB#2:                                 # %if.end
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	jmp	.LBB668_3
.LBB668_1:                              # %if.then
	callq	end_sequence
	xorq	%rbx, %rbx
.LBB668_3:                              # %return
	movq	%rbx, %rax
	addq	$40, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.Ltmp6182:
	.size	gen_movsfcc, .Ltmp6182-gen_movsfcc
	.cfi_endproc

	.globl	gen_movdfcc
	.align	16, 0x90
	.type	gen_movdfcc,@function
gen_movdfcc:                            # @gen_movdfcc
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp6188:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp6189:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp6190:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp6191:
	.cfi_def_cfa_offset 40
	subq	$40, %rsp
.Ltmp6192:
	.cfi_def_cfa_offset 80
.Ltmp6193:
	.cfi_offset %rbx, -40
.Ltmp6194:
	.cfi_offset %r12, -32
.Ltmp6195:
	.cfi_offset %r14, -24
.Ltmp6196:
	.cfi_offset %r15, -16
	movq	%rcx, %r14
	movq	%rdx, %r15
	movq	%rsi, %r12
	movq	%rdi, %rbx
	callq	start_sequence
	movq	%rbx, (%rsp)
	movq	%r12, 8(%rsp)
	movq	%r15, 16(%rsp)
	movq	%r14, 24(%rsp)
	leaq	(%rsp), %rdi
	callq	ix86_expand_fp_movcc
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB669_1
# BB#2:                                 # %if.end
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	jmp	.LBB669_3
.LBB669_1:                              # %if.then
	callq	end_sequence
	xorq	%rbx, %rbx
.LBB669_3:                              # %return
	movq	%rbx, %rax
	addq	$40, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.Ltmp6197:
	.size	gen_movdfcc, .Ltmp6197-gen_movdfcc
	.cfi_endproc

	.globl	gen_split_1214
	.align	16, 0x90
	.type	gen_split_1214,@function
gen_split_1214:                         # @gen_split_1214
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp6204:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp6205:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp6206:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp6207:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp6208:
	.cfi_def_cfa_offset 48
	subq	$32, %rsp
.Ltmp6209:
	.cfi_def_cfa_offset 80
.Ltmp6210:
	.cfi_offset %rbx, -48
.Ltmp6211:
	.cfi_offset %r12, -40
.Ltmp6212:
	.cfi_offset %r13, -32
.Ltmp6213:
	.cfi_offset %r14, -24
.Ltmp6214:
	.cfi_offset %r15, -16
	movq	%rdi, %rbx
	callq	start_sequence
	leaq	16(%rbx), %r14
	leaq	40(%rbx), %rdx
	leaq	48(%rbx), %rcx
	movq	$1, %rsi
	movq	%r14, %rdi
	callq	split_di
	leaq	24(%rbx), %r15
	leaq	56(%rbx), %rdx
	leaq	64(%rbx), %rcx
	movq	$1, %rsi
	movq	%r15, %rdi
	callq	split_di
	movq	$1, %rsi
	movq	%rbx, %rdi
	movq	%r14, %rdx
	movq	%r15, %rcx
	callq	split_di
	movq	8(%rbx), %r13
	movq	16(%rbx), %rax
	movq	%rax, (%rsp)            # 8-byte Spill
	movq	24(%rbx), %rax
	movq	%rax, 24(%rsp)          # 8-byte Spill
	movq	32(%rbx), %r14
	movq	40(%rbx), %r15
	movq	48(%rbx), %rax
	movq	%rax, 16(%rsp)          # 8-byte Spill
	movq	56(%rbx), %r12
	movl	(%r13), %esi
	movq	64(%rbx), %rax
	movq	%rax, 8(%rsp)           # 8-byte Spill
	movq	%rsi, %rdi
	movq	$16, %rcx
	movq	const_int_rtx+512(%rip), %rbx
	shrq	%cl, %rsi
	andq	$65535, %rdi            # imm = 0xFFFF
	andq	$255, %rsi
	xorq	%rax, %rax
	movq	%r14, %rdx
	movq	%rbx, %rcx
	callq	gen_rtx
	movq	$72, %rdi
	movq	$4, %rsi
	movq	%rax, %rdx
	movq	%r15, %rcx
	movq	%r12, %r8
	callq	gen_rtx_fmt_eee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	(%rsp), %rdx            # 8-byte Reload
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	movl	(%r13), %ebx
	movq	%rbx, %r15
	andq	$65535, %r15            # imm = 0xFFFF
	movq	$16, %rcx
	shrq	%cl, %rbx
	andq	$255, %rbx
	movq	%r14, %rdi
	callq	copy_rtx
	movq	const_int_rtx+512(%rip), %rcx
	movq	%rax, %rdx
	xorq	%rax, %rax
	movq	%r15, %rdi
	movq	%rbx, %rsi
	callq	gen_rtx
	movq	$72, %rdi
	movq	$4, %rsi
	movq	%rax, %rdx
	movq	16(%rsp), %rcx          # 8-byte Reload
	movq	8(%rsp), %r8            # 8-byte Reload
	callq	gen_rtx_fmt_eee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	24(%rsp), %rdx          # 8-byte Reload
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp6215:
	.size	gen_split_1214, .Ltmp6215-gen_split_1214
	.cfi_endproc

	.globl	gen_movxfcc
	.align	16, 0x90
	.type	gen_movxfcc,@function
gen_movxfcc:                            # @gen_movxfcc
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp6221:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp6222:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp6223:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp6224:
	.cfi_def_cfa_offset 40
	subq	$40, %rsp
.Ltmp6225:
	.cfi_def_cfa_offset 80
.Ltmp6226:
	.cfi_offset %rbx, -40
.Ltmp6227:
	.cfi_offset %r12, -32
.Ltmp6228:
	.cfi_offset %r14, -24
.Ltmp6229:
	.cfi_offset %r15, -16
	movq	%rcx, %r14
	movq	%rdx, %r15
	movq	%rsi, %r12
	movq	%rdi, %rbx
	callq	start_sequence
	movq	%rbx, (%rsp)
	movq	%r12, 8(%rsp)
	movq	%r15, 16(%rsp)
	movq	%r14, 24(%rsp)
	leaq	(%rsp), %rdi
	callq	ix86_expand_fp_movcc
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB671_1
# BB#2:                                 # %if.end
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	jmp	.LBB671_3
.LBB671_1:                              # %if.then
	callq	end_sequence
	xorq	%rbx, %rbx
.LBB671_3:                              # %return
	movq	%rbx, %rax
	addq	$40, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.Ltmp6230:
	.size	gen_movxfcc, .Ltmp6230-gen_movxfcc
	.cfi_endproc

	.globl	gen_movtfcc
	.align	16, 0x90
	.type	gen_movtfcc,@function
gen_movtfcc:                            # @gen_movtfcc
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp6236:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp6237:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp6238:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp6239:
	.cfi_def_cfa_offset 40
	subq	$40, %rsp
.Ltmp6240:
	.cfi_def_cfa_offset 80
.Ltmp6241:
	.cfi_offset %rbx, -40
.Ltmp6242:
	.cfi_offset %r12, -32
.Ltmp6243:
	.cfi_offset %r14, -24
.Ltmp6244:
	.cfi_offset %r15, -16
	movq	%rcx, %r14
	movq	%rdx, %r15
	movq	%rsi, %r12
	movq	%rdi, %rbx
	callq	start_sequence
	movq	%rbx, (%rsp)
	movq	%r12, 8(%rsp)
	movq	%r15, 16(%rsp)
	movq	%r14, 24(%rsp)
	leaq	(%rsp), %rdi
	callq	ix86_expand_fp_movcc
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB672_1
# BB#2:                                 # %if.end
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	jmp	.LBB672_3
.LBB672_1:                              # %if.then
	callq	end_sequence
	xorq	%rbx, %rbx
.LBB672_3:                              # %return
	movq	%rbx, %rax
	addq	$40, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.Ltmp6245:
	.size	gen_movtfcc, .Ltmp6245-gen_movtfcc
	.cfi_endproc

	.globl	gen_minsf3
	.align	16, 0x90
	.type	gen_minsf3,@function
gen_minsf3:                             # @gen_minsf3
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp6250:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp6251:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp6252:
	.cfi_def_cfa_offset 32
.Ltmp6253:
	.cfi_offset %rbx, -32
.Ltmp6254:
	.cfi_offset %r14, -24
.Ltmp6255:
	.cfi_offset %r15, -16
	movq	%rdx, %r14
	movq	%rsi, %rbx
	movq	%rdi, %r15
	movq	$107, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%r14, %rcx
	callq	gen_rtx_fmt_ee
	movq	$72, %rdi
	movq	$15, %rsi
	movq	%rax, %rdx
	movq	%rbx, %rcx
	movq	%r14, %r8
	callq	gen_rtx_fmt_eee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r15, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rbx
	movq	$52, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rcx
	movq	$2, %rdi
	xorq	%rax, %rax
	movq	%rbx, %rsi
	movq	%rcx, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	popq	%rbx
	popq	%r14
	popq	%r15
	jmp	gen_rtx_fmt_E  # TAILCALL
.Ltmp6256:
	.size	gen_minsf3, .Ltmp6256-gen_minsf3
	.cfi_endproc

	.globl	gen_split_1218
	.align	16, 0x90
	.type	gen_split_1218,@function
gen_split_1218:                         # @gen_split_1218
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp6262:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp6263:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp6264:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp6265:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp6266:
	.cfi_def_cfa_offset 48
.Ltmp6267:
	.cfi_offset %rbx, -40
.Ltmp6268:
	.cfi_offset %r12, -32
.Ltmp6269:
	.cfi_offset %r14, -24
.Ltmp6270:
	.cfi_offset %r15, -16
	movq	%rdi, %rbx
	callq	start_sequence
	movq	(%rbx), %r14
	movq	8(%rbx), %r15
	movq	16(%rbx), %rbx
	movq	$107, %rdi
	xorq	%rsi, %rsi
	movq	%r15, %rdx
	movq	%rbx, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %r12
	movq	%r15, %rdi
	callq	copy_rtx
	movq	%rax, %r15
	movq	%rbx, %rdi
	callq	copy_rtx
	movq	$72, %rdi
	movq	$15, %rsi
	movq	%r12, %rdx
	movq	%r15, %rcx
	movq	%rax, %r8
	callq	gen_rtx_fmt_eee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.Ltmp6271:
	.size	gen_split_1218, .Ltmp6271-gen_split_1218
	.cfi_endproc

	.globl	gen_split_1219
	.align	16, 0x90
	.type	gen_split_1219,@function
gen_split_1219:                         # @gen_split_1219
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp6278:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp6279:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp6280:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp6281:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp6282:
	.cfi_def_cfa_offset 48
.Ltmp6283:
	.cfi_offset %rbx, -48
.Ltmp6284:
	.cfi_offset %r12, -40
.Ltmp6285:
	.cfi_offset %r13, -32
.Ltmp6286:
	.cfi_offset %r14, -24
.Ltmp6287:
	.cfi_offset %r15, -16
	movq	%rdi, %rbx
	callq	start_sequence
	movq	(%rbx), %r14
	movq	8(%rbx), %r12
	movq	16(%rbx), %r15
	movq	$57, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	%rax, %rbx
	movq	$74, %rdi
	movq	$57, %rsi
	movq	%r15, %rdx
	movq	%r12, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	movq	$57, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	const_int_rtx+512(%rip), %rcx
	movq	$104, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_ee
	movq	%rax, %r13
	movq	%r12, %rdi
	callq	copy_rtx
	movq	%rax, %rbx
	movq	%r15, %rdi
	callq	copy_rtx
	movq	$72, %rdi
	movq	$15, %rsi
	movq	%r13, %rdx
	movq	%rbx, %rcx
	movq	%rax, %r8
	callq	gen_rtx_fmt_eee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp6288:
	.size	gen_split_1219, .Ltmp6288-gen_split_1219
	.cfi_endproc

	.globl	gen_mindf3
	.align	16, 0x90
	.type	gen_mindf3,@function
gen_mindf3:                             # @gen_mindf3
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp6293:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp6294:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp6295:
	.cfi_def_cfa_offset 32
.Ltmp6296:
	.cfi_offset %rbx, -32
.Ltmp6297:
	.cfi_offset %r14, -24
.Ltmp6298:
	.cfi_offset %r15, -16
	movq	%rdx, %r14
	movq	%rsi, %rbx
	movq	%rdi, %r15
	callq	start_sequence
	movq	$107, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%r14, %rcx
	callq	gen_rtx_fmt_ee
	movq	$72, %rdi
	movq	$16, %rsi
	movq	%rax, %rdx
	movq	%rbx, %rcx
	movq	%r14, %r8
	callq	gen_rtx_fmt_eee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r15, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rbx
	movq	$52, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rcx
	movq	$2, %rdi
	xorq	%rax, %rax
	movq	%rbx, %rsi
	movq	%rcx, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_E
	movq	%rax, %rdi
	callq	emit
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp6299:
	.size	gen_mindf3, .Ltmp6299-gen_mindf3
	.cfi_endproc

	.globl	gen_split_1221
	.align	16, 0x90
	.type	gen_split_1221,@function
gen_split_1221:                         # @gen_split_1221
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp6305:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp6306:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp6307:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp6308:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp6309:
	.cfi_def_cfa_offset 48
.Ltmp6310:
	.cfi_offset %rbx, -40
.Ltmp6311:
	.cfi_offset %r12, -32
.Ltmp6312:
	.cfi_offset %r14, -24
.Ltmp6313:
	.cfi_offset %r15, -16
	movq	%rdi, %rbx
	callq	start_sequence
	movq	(%rbx), %r14
	movq	8(%rbx), %r15
	movq	16(%rbx), %rbx
	movq	$107, %rdi
	xorq	%rsi, %rsi
	movq	%r15, %rdx
	movq	%rbx, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %r12
	movq	%r15, %rdi
	callq	copy_rtx
	movq	%rax, %r15
	movq	%rbx, %rdi
	callq	copy_rtx
	movq	$72, %rdi
	movq	$16, %rsi
	movq	%r12, %rdx
	movq	%r15, %rcx
	movq	%rax, %r8
	callq	gen_rtx_fmt_eee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.Ltmp6314:
	.size	gen_split_1221, .Ltmp6314-gen_split_1221
	.cfi_endproc

	.globl	gen_split_1222
	.align	16, 0x90
	.type	gen_split_1222,@function
gen_split_1222:                         # @gen_split_1222
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp6321:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp6322:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp6323:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp6324:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp6325:
	.cfi_def_cfa_offset 48
.Ltmp6326:
	.cfi_offset %rbx, -48
.Ltmp6327:
	.cfi_offset %r12, -40
.Ltmp6328:
	.cfi_offset %r13, -32
.Ltmp6329:
	.cfi_offset %r14, -24
.Ltmp6330:
	.cfi_offset %r15, -16
	movq	%rdi, %rbx
	callq	start_sequence
	movq	(%rbx), %r14
	movq	8(%rbx), %r15
	movq	16(%rbx), %r13
	movq	$57, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	%rax, %r12
	movq	%r13, %rdi
	callq	copy_rtx
	movq	$74, %rdi
	movq	$57, %rsi
	movq	%r13, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r12, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	movq	$57, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	const_int_rtx+512(%rip), %rcx
	movq	$104, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rbx
	movq	%r13, %rdi
	callq	copy_rtx
	movq	$72, %rdi
	movq	$16, %rsi
	movq	%rbx, %rdx
	movq	%r15, %rcx
	movq	%rax, %r8
	callq	gen_rtx_fmt_eee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp6331:
	.size	gen_split_1222, .Ltmp6331-gen_split_1222
	.cfi_endproc

	.globl	gen_maxsf3
	.align	16, 0x90
	.type	gen_maxsf3,@function
gen_maxsf3:                             # @gen_maxsf3
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp6336:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp6337:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp6338:
	.cfi_def_cfa_offset 32
.Ltmp6339:
	.cfi_offset %rbx, -32
.Ltmp6340:
	.cfi_offset %r14, -24
.Ltmp6341:
	.cfi_offset %r15, -16
	movq	%rdx, %r14
	movq	%rsi, %rbx
	movq	%rdi, %r15
	callq	start_sequence
	movq	$105, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%r14, %rcx
	callq	gen_rtx_fmt_ee
	movq	$72, %rdi
	movq	$15, %rsi
	movq	%rax, %rdx
	movq	%rbx, %rcx
	movq	%r14, %r8
	callq	gen_rtx_fmt_eee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r15, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rbx
	movq	$52, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rcx
	movq	$2, %rdi
	xorq	%rax, %rax
	movq	%rbx, %rsi
	movq	%rcx, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_E
	movq	%rax, %rdi
	callq	emit
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp6342:
	.size	gen_maxsf3, .Ltmp6342-gen_maxsf3
	.cfi_endproc

	.globl	gen_split_1224
	.align	16, 0x90
	.type	gen_split_1224,@function
gen_split_1224:                         # @gen_split_1224
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp6348:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp6349:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp6350:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp6351:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp6352:
	.cfi_def_cfa_offset 48
.Ltmp6353:
	.cfi_offset %rbx, -40
.Ltmp6354:
	.cfi_offset %r12, -32
.Ltmp6355:
	.cfi_offset %r14, -24
.Ltmp6356:
	.cfi_offset %r15, -16
	movq	%rdi, %rbx
	callq	start_sequence
	movq	(%rbx), %r14
	movq	8(%rbx), %r15
	movq	16(%rbx), %rbx
	movq	$105, %rdi
	xorq	%rsi, %rsi
	movq	%r15, %rdx
	movq	%rbx, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %r12
	movq	%r15, %rdi
	callq	copy_rtx
	movq	%rax, %r15
	movq	%rbx, %rdi
	callq	copy_rtx
	movq	$72, %rdi
	movq	$15, %rsi
	movq	%r12, %rdx
	movq	%r15, %rcx
	movq	%rax, %r8
	callq	gen_rtx_fmt_eee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.Ltmp6357:
	.size	gen_split_1224, .Ltmp6357-gen_split_1224
	.cfi_endproc

	.globl	gen_split_1225
	.align	16, 0x90
	.type	gen_split_1225,@function
gen_split_1225:                         # @gen_split_1225
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp6364:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp6365:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp6366:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp6367:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp6368:
	.cfi_def_cfa_offset 48
.Ltmp6369:
	.cfi_offset %rbx, -48
.Ltmp6370:
	.cfi_offset %r12, -40
.Ltmp6371:
	.cfi_offset %r13, -32
.Ltmp6372:
	.cfi_offset %r14, -24
.Ltmp6373:
	.cfi_offset %r15, -16
	movq	%rdi, %rbx
	callq	start_sequence
	movq	(%rbx), %r14
	movq	8(%rbx), %r12
	movq	16(%rbx), %r15
	movq	$57, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	%rax, %rbx
	movq	$74, %rdi
	movq	$57, %rsi
	movq	%r12, %rdx
	movq	%r15, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	movq	$57, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	const_int_rtx+512(%rip), %rcx
	movq	$105, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_ee
	movq	%rax, %r13
	movq	%r12, %rdi
	callq	copy_rtx
	movq	%rax, %rbx
	movq	%r15, %rdi
	callq	copy_rtx
	movq	$72, %rdi
	movq	$15, %rsi
	movq	%r13, %rdx
	movq	%rbx, %rcx
	movq	%rax, %r8
	callq	gen_rtx_fmt_eee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp6374:
	.size	gen_split_1225, .Ltmp6374-gen_split_1225
	.cfi_endproc

	.globl	gen_maxdf3
	.align	16, 0x90
	.type	gen_maxdf3,@function
gen_maxdf3:                             # @gen_maxdf3
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp6379:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp6380:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp6381:
	.cfi_def_cfa_offset 32
.Ltmp6382:
	.cfi_offset %rbx, -32
.Ltmp6383:
	.cfi_offset %r14, -24
.Ltmp6384:
	.cfi_offset %r15, -16
	movq	%rdx, %r14
	movq	%rsi, %rbx
	movq	%rdi, %r15
	callq	start_sequence
	movq	$105, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%r14, %rcx
	callq	gen_rtx_fmt_ee
	movq	$72, %rdi
	movq	$16, %rsi
	movq	%rax, %rdx
	movq	%rbx, %rcx
	movq	%r14, %r8
	callq	gen_rtx_fmt_eee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r15, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rbx
	movq	$52, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rcx
	movq	$2, %rdi
	xorq	%rax, %rax
	movq	%rbx, %rsi
	movq	%rcx, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_E
	movq	%rax, %rdi
	callq	emit
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp6385:
	.size	gen_maxdf3, .Ltmp6385-gen_maxdf3
	.cfi_endproc

	.globl	gen_split_1227
	.align	16, 0x90
	.type	gen_split_1227,@function
gen_split_1227:                         # @gen_split_1227
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp6391:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp6392:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp6393:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp6394:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp6395:
	.cfi_def_cfa_offset 48
.Ltmp6396:
	.cfi_offset %rbx, -40
.Ltmp6397:
	.cfi_offset %r12, -32
.Ltmp6398:
	.cfi_offset %r14, -24
.Ltmp6399:
	.cfi_offset %r15, -16
	movq	%rdi, %rbx
	callq	start_sequence
	movq	(%rbx), %r14
	movq	8(%rbx), %r15
	movq	16(%rbx), %rbx
	movq	$105, %rdi
	xorq	%rsi, %rsi
	movq	%r15, %rdx
	movq	%rbx, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %r12
	movq	%r15, %rdi
	callq	copy_rtx
	movq	%rax, %r15
	movq	%rbx, %rdi
	callq	copy_rtx
	movq	$72, %rdi
	movq	$16, %rsi
	movq	%r12, %rdx
	movq	%r15, %rcx
	movq	%rax, %r8
	callq	gen_rtx_fmt_eee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.Ltmp6400:
	.size	gen_split_1227, .Ltmp6400-gen_split_1227
	.cfi_endproc

	.globl	gen_split_1228
	.align	16, 0x90
	.type	gen_split_1228,@function
gen_split_1228:                         # @gen_split_1228
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp6407:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp6408:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp6409:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp6410:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp6411:
	.cfi_def_cfa_offset 48
.Ltmp6412:
	.cfi_offset %rbx, -48
.Ltmp6413:
	.cfi_offset %r12, -40
.Ltmp6414:
	.cfi_offset %r13, -32
.Ltmp6415:
	.cfi_offset %r14, -24
.Ltmp6416:
	.cfi_offset %r15, -16
	movq	%rdi, %rbx
	callq	start_sequence
	movq	(%rbx), %r14
	movq	8(%rbx), %r12
	movq	16(%rbx), %r15
	movq	$57, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	%rax, %rbx
	movq	$74, %rdi
	movq	$57, %rsi
	movq	%r12, %rdx
	movq	%r15, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	movq	$57, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	const_int_rtx+512(%rip), %rcx
	movq	$105, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_ee
	movq	%rax, %r13
	movq	%r12, %rdi
	callq	copy_rtx
	movq	%rax, %rbx
	movq	%r15, %rdi
	callq	copy_rtx
	movq	$72, %rdi
	movq	$16, %rsi
	movq	%r13, %rdx
	movq	%rbx, %rcx
	movq	%rax, %r8
	callq	gen_rtx_fmt_eee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp6417:
	.size	gen_split_1228, .Ltmp6417-gen_split_1228
	.cfi_endproc

	.globl	gen_pro_epilogue_adjust_stack
	.align	16, 0x90
	.type	gen_pro_epilogue_adjust_stack,@function
gen_pro_epilogue_adjust_stack:          # @gen_pro_epilogue_adjust_stack
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp6422:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp6423:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp6424:
	.cfi_def_cfa_offset 32
.Ltmp6425:
	.cfi_offset %rbx, -32
.Ltmp6426:
	.cfi_offset %r14, -24
.Ltmp6427:
	.cfi_offset %r15, -16
	movq	%rdx, %r15
	movq	%rsi, %rbx
	movq	%rdi, %r14
	callq	start_sequence
	movzbl	target_flags+3(%rip), %eax
	movq	$75, %rdi
	testq	$2, %rax
	je	.LBB685_3
# BB#1:                                 # %if.then
	movq	$5, %rsi
	movq	%rbx, %rdx
	movq	%r15, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %r14
	movq	$52, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rbx
	movq	$62, %rdi
	xorq	%rsi, %rsi
	callq	gen_rtx_fmt_0
	movq	$51, %rdi
	movq	%rax, %rsi
	callq	gen_rtx_MEM
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rcx
	movq	$3, %rdi
	xorq	%rax, %rax
	movq	%r14, %rsi
	movq	%rbx, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_E
	movq	%rax, %rdi
	callq	emit_insn
	jmp	.LBB685_2
.LBB685_3:                              # %if.end
	movq	$4, %rsi
	movq	%rbx, %rdx
	movq	%r15, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %r14
	movq	$52, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rbx
	movq	$62, %rdi
	xorq	%rsi, %rsi
	callq	gen_rtx_fmt_0
	movq	$51, %rdi
	movq	%rax, %rsi
	callq	gen_rtx_MEM
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rcx
	movq	$3, %rdi
	xorq	%rax, %rax
	movq	%r14, %rsi
	movq	%rbx, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_E
	movq	%rax, %rdi
	callq	emit
.LBB685_2:                              # %if.then
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp6428:
	.size	gen_pro_epilogue_adjust_stack, .Ltmp6428-gen_pro_epilogue_adjust_stack
	.cfi_endproc

	.globl	gen_split_1230
	.align	16, 0x90
	.type	gen_split_1230,@function
gen_split_1230:                         # @gen_split_1230
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp6431:
	.cfi_def_cfa_offset 16
.Ltmp6432:
	.cfi_offset %rbx, -16
	movq	%rdi, %rbx
	callq	start_sequence
	movq	40(%rbx), %rax
	movq	%rax, ix86_compare_op0(%rip)
	movq	32(%rbx), %rax
	movq	%rax, ix86_compare_op1(%rip)
	movq	8(%rbx), %rax
	movzbl	(%rax), %edi
	callq	swap_condition
	movq	32(%rbx), %rcx
	movq	40(%rbx), %rdx
	xorq	%rsi, %rsi
	movq	%rax, %rdi
	callq	gen_rtx_fmt_ee
	movq	%rax, 8(%rbx)
	movq	%rbx, %rdi
	callq	ix86_expand_fp_movcc
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	popq	%rbx
	retq
.Ltmp6433:
	.size	gen_split_1230, .Ltmp6433-gen_split_1230
	.cfi_endproc

	.globl	gen_split_1231
	.align	16, 0x90
	.type	gen_split_1231,@function
gen_split_1231:                         # @gen_split_1231
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp6440:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp6441:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp6442:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp6443:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp6444:
	.cfi_def_cfa_offset 48
	subq	$32, %rsp
.Ltmp6445:
	.cfi_def_cfa_offset 80
.Ltmp6446:
	.cfi_offset %rbx, -48
.Ltmp6447:
	.cfi_offset %r12, -40
.Ltmp6448:
	.cfi_offset %r13, -32
.Ltmp6449:
	.cfi_offset %r14, -24
.Ltmp6450:
	.cfi_offset %r15, -16
	movq	%rdi, %r15
	callq	start_sequence
	movq	16(%r15), %rdi
	movq	24(%r15), %rsi
	callq	operands_match_p
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	testq	%rbx, %rax
	jne	.LBB687_5
# BB#1:                                 # %if.end
	movq	(%r15), %rax
	movl	(%rax), %eax
	movq	8(%r15), %rdx
	movq	$16, %rcx
	shrq	%cl, %rax
	movb	%al, 2(%rdx)
	movq	(%r15), %rdi
	movq	32(%r15), %rsi
	callq	operands_match_p
	testq	%rbx, %rax
	je	.LBB687_3
# BB#2:                                 # %if.then10
	movq	32(%r15), %r13
	movq	%r13, 16(%rsp)          # 8-byte Spill
	movq	%r13, 48(%r15)
	movq	16(%r15), %r12
	movq	%r12, 24(%rsp)          # 8-byte Spill
	movq	%r12, 56(%r15)
	jmp	.LBB687_4
.LBB687_3:                              # %if.else
	movq	16(%r15), %r12
	movq	%r12, 16(%rsp)          # 8-byte Spill
	movq	%r12, 48(%r15)
	movq	32(%r15), %r13
	movq	%r13, 24(%rsp)          # 8-byte Spill
	movq	%r13, 56(%r15)
.LBB687_4:                              # %if.end19
	movq	8(%r15), %rax
	movq	(%r15), %rcx
	movq	%rcx, 8(%rsp)           # 8-byte Spill
	movq	24(%r15), %rcx
	movq	%rcx, (%rsp)            # 8-byte Spill
	movl	(%rax), %r14d
	movq	40(%r15), %r15
	movq	%r14, %rbx
	andq	$65535, %rbx            # imm = 0xFFFF
	movq	$16, %rcx
	shrq	%cl, %r14
	andq	$255, %r14
	movq	%r13, %rdi
	callq	copy_rtx
	movq	%rax, %rcx
	xorq	%rax, %rax
	movq	%rbx, %rdi
	movq	%r14, %rsi
	movq	%rcx, %rdx
	movq	%r15, %rcx
	callq	gen_rtx
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r13, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	movq	$6, %rdi
	xorq	%rdx, %rdx
	movq	%r12, %rsi
	callq	gen_rtx_SUBREG
	movq	%rax, %r15
	movq	%r12, %rdi
	callq	copy_rtx
	movq	$6, %rdi
	xorq	%rdx, %rdx
	movq	%rax, %rsi
	callq	gen_rtx_SUBREG
	movq	%rax, %rbx
	movq	%r13, %rdi
	callq	copy_rtx
	movq	$6, %rdi
	xorq	%rdx, %rdx
	movq	%rax, %rsi
	callq	gen_rtx_SUBREG
	movq	$83, %rdi
	movq	$6, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r15, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	movq	%r13, %rdi
	callq	copy_rtx
	movq	$6, %rdi
	xorq	%rdx, %rdx
	movq	%rax, %rsi
	callq	gen_rtx_SUBREG
	movq	%rax, %r15
	movq	%r13, %rdi
	callq	copy_rtx
	movq	$6, %rdi
	xorq	%rdx, %rdx
	movq	%rax, %rsi
	callq	gen_rtx_SUBREG
	movq	$86, %rdi
	movq	$6, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rbx
	movq	$6, %rdi
	xorq	%rdx, %rdx
	movq	(%rsp), %rsi            # 8-byte Reload
	callq	gen_rtx_SUBREG
	movq	$83, %rdi
	movq	$6, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r15, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	movq	$6, %rdi
	xorq	%rdx, %rdx
	movq	8(%rsp), %rsi           # 8-byte Reload
	callq	gen_rtx_SUBREG
	movq	%rax, %r14
	movq	$6, %rdi
	xorq	%rdx, %rdx
	movq	16(%rsp), %rsi          # 8-byte Reload
	callq	gen_rtx_SUBREG
	movq	%rax, %rbx
	movq	$6, %rdi
	xorq	%rdx, %rdx
	movq	24(%rsp), %rsi          # 8-byte Reload
	callq	gen_rtx_SUBREG
	movq	$84, %rdi
	movq	$6, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB687_5:                              # %if.then
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.gen_split_1231, %rcx
	movq	$16727, %rsi            # imm = 0x4157
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp6451:
	.size	gen_split_1231, .Ltmp6451-gen_split_1231
	.cfi_endproc

	.globl	gen_split_1232
	.align	16, 0x90
	.type	gen_split_1232,@function
gen_split_1232:                         # @gen_split_1232
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp6458:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp6459:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp6460:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp6461:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp6462:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp6463:
	.cfi_def_cfa_offset 64
.Ltmp6464:
	.cfi_offset %rbx, -48
.Ltmp6465:
	.cfi_offset %r12, -40
.Ltmp6466:
	.cfi_offset %r13, -32
.Ltmp6467:
	.cfi_offset %r14, -24
.Ltmp6468:
	.cfi_offset %r15, -16
	movq	%rdi, %r13
	callq	start_sequence
	movq	(%r13), %rax
	movl	(%rax), %eax
	movq	8(%r13), %rdx
	movq	$16, %rcx
	shrq	%cl, %rax
	movb	%al, 2(%rdx)
	movq	8(%r13), %rdi
	xorq	%rsi, %rsi
	callq	sse_comparison_operator
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	testq	%rbx, %rax
	jne	.LBB688_2
# BB#1:                                 # %if.then
	movq	32(%r13), %rax
	movq	40(%r13), %rcx
	movq	%rax, 40(%r13)
	movq	%rcx, 32(%r13)
	movq	8(%r13), %rax
	movzbl	(%rax), %edi
	callq	swap_condition
	movq	8(%r13), %rcx
	movw	%ax, (%rcx)
.LBB688_2:                              # %if.end
	movq	(%r13), %rax
	movq	16(%r13), %rdi
	movzbl	2(%rax), %esi
	callq	const0_operand
	testq	%rbx, %rax
	je	.LBB688_4
# BB#3:                                 # %if.then25
	movq	24(%r13), %rax
	movq	%rax, 56(%r13)
	movq	(%r13), %rsi
	movq	$6, %rdi
	xorq	%rdx, %rdx
	callq	gen_rtx_SUBREG
	movq	$86, %rdi
	movq	$6, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	jmp	.LBB688_5
.LBB688_4:                              # %if.else
	movq	16(%r13), %rax
	movq	%rax, 56(%r13)
	movq	(%r13), %rsi
	movq	$6, %rdi
	xorq	%rdx, %rdx
	callq	gen_rtx_SUBREG
.LBB688_5:                              # %if.end37
	movq	%rax, %r14
	movq	%r14, 48(%r13)
	movq	8(%r13), %rax
	movq	(%r13), %r15
	movq	40(%r13), %r12
	movl	(%rax), %ebx
	movq	56(%r13), %rax
	movq	%rax, 8(%rsp)           # 8-byte Spill
	movq	%rbx, %r13
	andq	$65535, %r13            # imm = 0xFFFF
	movq	$16, %rcx
	shrq	%cl, %rbx
	andq	$255, %rbx
	movq	%r15, %rdi
	callq	copy_rtx
	movq	%rax, %rcx
	xorq	%rax, %rax
	movq	%r13, %rdi
	movq	%rbx, %rsi
	movq	%rcx, %rdx
	movq	%r12, %rcx
	callq	gen_rtx
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r15, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	movq	%r15, %rdi
	callq	copy_rtx
	movq	$6, %rdi
	xorq	%rdx, %rdx
	movq	%rax, %rsi
	callq	gen_rtx_SUBREG
	movq	%rax, %rbx
	movq	$6, %rdi
	xorq	%rdx, %rdx
	movq	8(%rsp), %rsi           # 8-byte Reload
	callq	gen_rtx_SUBREG
	movq	$83, %rdi
	movq	$6, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp6469:
	.size	gen_split_1232, .Ltmp6469-gen_split_1232
	.cfi_endproc

	.globl	gen_allocate_stack_worker
	.align	16, 0x90
	.type	gen_allocate_stack_worker,@function
gen_allocate_stack_worker:              # @gen_allocate_stack_worker
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp6472:
	.cfi_def_cfa_offset 16
.Ltmp6473:
	.cfi_offset %rbx, -16
	movq	%rdi, %rbx
	callq	start_sequence
	movzbl	target_flags+3(%rip), %eax
	movq	%rbx, %rdi
	testq	$2, %rax
	je	.LBB689_2
# BB#1:                                 # %if.then
	callq	gen_allocate_stack_worker_rex64
	jmp	.LBB689_3
.LBB689_2:                              # %if.else
	callq	gen_allocate_stack_worker_1
.LBB689_3:                              # %if.end
	movq	%rax, %rdi
	callq	emit_insn
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	popq	%rbx
	retq
.Ltmp6474:
	.size	gen_allocate_stack_worker, .Ltmp6474-gen_allocate_stack_worker
	.cfi_endproc

	.globl	gen_allocate_stack
	.align	16, 0x90
	.type	gen_allocate_stack,@function
gen_allocate_stack:                     # @gen_allocate_stack
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp6478:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp6479:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp6480:
	.cfi_def_cfa_offset 32
.Ltmp6481:
	.cfi_offset %rbx, -24
.Ltmp6482:
	.cfi_offset %r14, -16
	movq	%rsi, %rbx
	movq	%rdi, %r14
	callq	start_sequence
	movq	$4, %rdi
	movq	%rbx, %rsi
	callq	copy_to_mode_reg
	movq	%rax, %rbx
	callq	start_sequence
	movzbl	target_flags+3(%rip), %eax
	movq	%rbx, %rdi
	testq	$2, %rax
	je	.LBB690_2
# BB#1:                                 # %if.then.i
	callq	gen_allocate_stack_worker_rex64
	jmp	.LBB690_3
.LBB690_2:                              # %if.else.i
	callq	gen_allocate_stack_worker_1
.LBB690_3:                              # %gen_allocate_stack_worker.exit
	movq	%rax, %rdi
	callq	emit_insn
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rdi
	callq	emit_insn
	movq	global_rtl+64(%rip), %rsi
	movq	%r14, %rdi
	callq	emit_move_insn
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp6483:
	.size	gen_allocate_stack, .Ltmp6483-gen_allocate_stack
	.cfi_endproc

	.globl	gen_builtin_setjmp_receiver
	.align	16, 0x90
	.type	gen_builtin_setjmp_receiver,@function
gen_builtin_setjmp_receiver:            # @gen_builtin_setjmp_receiver
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp6486:
	.cfi_def_cfa_offset 16
.Ltmp6487:
	.cfi_offset %rbx, -16
	callq	start_sequence
	callq	load_pic_register
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	popq	%rbx
	retq
.Ltmp6488:
	.size	gen_builtin_setjmp_receiver, .Ltmp6488-gen_builtin_setjmp_receiver
	.cfi_endproc

	.globl	gen_split_1236
	.align	16, 0x90
	.type	gen_split_1236,@function
gen_split_1236:                         # @gen_split_1236
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp6492:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp6493:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp6494:
	.cfi_def_cfa_offset 32
.Ltmp6495:
	.cfi_offset %rbx, -24
.Ltmp6496:
	.cfi_offset %r14, -16
	movq	%rdi, %rbx
	callq	start_sequence
	movq	(%rbx), %rsi
	movq	$4, %rdi
	callq	gen_lowpart
	movq	%rax, (%rbx)
	movq	8(%rbx), %rsi
	movq	$4, %rdi
	callq	gen_lowpart
	movq	%rax, 8(%rbx)
	movq	24(%rbx), %rax
	movl	(%rax), %ecx
	movq	%rcx, %rdx
	andq	$65535, %rdx            # imm = 0xFFFF
	cmpq	$87, %rdx
	je	.LBB692_2
# BB#1:                                 # %if.then
	movq	16(%rbx), %rsi
	movq	$4, %rdi
	callq	gen_lowpart
	movq	%rax, 16(%rbx)
	movq	24(%rbx), %rax
	movl	(%rax), %ecx
.LBB692_2:                              # %if.end
	movabsq	$4278255615, %rdx       # imm = 0xFF00FFFF
	andq	%rcx, %rdx
	orq	$262144, %rdx           # imm = 0x40000
	movl	%edx, (%rax)
	movq	24(%rbx), %rax
	movq	(%rbx), %r14
	movq	8(%rbx), %rdx
	movl	(%rax), %esi
	movq	16(%rbx), %rbx
	movq	%rsi, %rdi
	andq	$65535, %rdi            # imm = 0xFFFF
	movq	$16, %rcx
	shrq	%cl, %rsi
	andq	$255, %rsi
	xorq	%rax, %rax
	movq	%rbx, %rcx
	callq	gen_rtx
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rbx
	movq	$52, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rcx
	movq	$2, %rdi
	xorq	%rax, %rax
	movq	%rbx, %rsi
	movq	%rcx, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_E
	movq	%rax, %rdi
	callq	emit
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp6497:
	.size	gen_split_1236, .Ltmp6497-gen_split_1236
	.cfi_endproc

	.globl	gen_split_1237
	.align	16, 0x90
	.type	gen_split_1237,@function
gen_split_1237:                         # @gen_split_1237
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp6504:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp6505:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp6506:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp6507:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp6508:
	.cfi_def_cfa_offset 48
.Ltmp6509:
	.cfi_offset %rbx, -48
.Ltmp6510:
	.cfi_offset %r12, -40
.Ltmp6511:
	.cfi_offset %r13, -32
.Ltmp6512:
	.cfi_offset %r14, -24
.Ltmp6513:
	.cfi_offset %r15, -16
	movq	%rdi, %rbx
	callq	start_sequence
	movq	(%rbx), %rax
	movq	16(%rbx), %rcx
	movzbl	2(%rax), %eax
	movq	8(%rcx), %rcx
	movq	mode_mask_array(,%rax,8), %rdi
	andq	%rcx, %rdi
	movq	$4, %rsi
	callq	trunc_int_for_mode
	xorq	%rdi, %rdi
	movq	%rax, %rsi
	callq	gen_rtx_CONST_INT
	movq	%rax, 16(%rbx)
	movq	(%rbx), %rsi
	movq	$4, %rdi
	callq	gen_lowpart
	movq	%rax, (%rbx)
	movq	8(%rbx), %rsi
	movq	$4, %rdi
	callq	gen_lowpart
	movq	%rax, %r15
	movq	%r15, 8(%rbx)
	movq	(%rbx), %r14
	movq	16(%rbx), %r13
	movq	$55, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	%rax, %r12
	movq	$83, %rdi
	movq	$4, %rsi
	movq	%r15, %rdx
	movq	%r13, %rcx
	callq	gen_rtx_fmt_ee
	movq	const_int_rtx+512(%rip), %rcx
	movq	$74, %rdi
	movq	$55, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r12, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %r12
	movq	%r15, %rdi
	callq	copy_rtx
	movq	%rax, %rbx
	movq	%r13, %rdi
	callq	copy_rtx
	movq	$83, %rdi
	movq	$4, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rcx
	movq	$2, %rdi
	xorq	%rax, %rax
	movq	%r12, %rsi
	movq	%rcx, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_E
	movq	%rax, %rdi
	callq	emit
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp6514:
	.size	gen_split_1237, .Ltmp6514-gen_split_1237
	.cfi_endproc

	.globl	gen_split_1238
	.align	16, 0x90
	.type	gen_split_1238,@function
gen_split_1238:                         # @gen_split_1238
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp6519:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp6520:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp6521:
	.cfi_def_cfa_offset 32
.Ltmp6522:
	.cfi_offset %rbx, -32
.Ltmp6523:
	.cfi_offset %r14, -24
.Ltmp6524:
	.cfi_offset %r15, -16
	movq	%rdi, %rbx
	callq	start_sequence
	movq	(%rbx), %rax
	movq	8(%rbx), %rcx
	movzbl	2(%rax), %eax
	movq	8(%rcx), %rcx
	movq	mode_mask_array(,%rax,8), %rdi
	andq	%rcx, %rdi
	movq	$4, %rsi
	callq	trunc_int_for_mode
	xorq	%rdi, %rdi
	movq	%rax, %rsi
	callq	gen_rtx_CONST_INT
	movq	%rax, 8(%rbx)
	movq	(%rbx), %rsi
	movq	$4, %rdi
	callq	gen_lowpart
	movq	%rax, %r14
	movq	%r14, (%rbx)
	movq	8(%rbx), %rbx
	movq	$55, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	%rax, %r15
	movq	$83, %rdi
	movq	$4, %rsi
	movq	%r14, %rdx
	movq	%rbx, %rcx
	callq	gen_rtx_fmt_ee
	movq	const_int_rtx+512(%rip), %rcx
	movq	$74, %rdi
	movq	$55, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r15, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp6525:
	.size	gen_split_1238, .Ltmp6525-gen_split_1238
	.cfi_endproc

	.globl	gen_split_1239
	.align	16, 0x90
	.type	gen_split_1239,@function
gen_split_1239:                         # @gen_split_1239
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp6528:
	.cfi_def_cfa_offset 16
.Ltmp6529:
	.cfi_offset %rbx, -16
	movq	%rdi, %rbx
	callq	start_sequence
	movq	(%rbx), %rsi
	movq	$4, %rdi
	callq	gen_lowpart
	movq	%rax, (%rbx)
	movq	8(%rbx), %rsi
	movq	$4, %rdi
	callq	gen_lowpart
	movq	%rax, 8(%rbx)
	movq	(%rbx), %rbx
	movq	$77, %rdi
	movq	$4, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rbx
	movq	$52, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rcx
	movq	$2, %rdi
	xorq	%rax, %rax
	movq	%rbx, %rsi
	movq	%rcx, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_E
	movq	%rax, %rdi
	callq	emit
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	popq	%rbx
	retq
.Ltmp6530:
	.size	gen_split_1239, .Ltmp6530-gen_split_1239
	.cfi_endproc

	.globl	gen_split_1240
	.align	16, 0x90
	.type	gen_split_1240,@function
gen_split_1240:                         # @gen_split_1240
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp6533:
	.cfi_def_cfa_offset 16
.Ltmp6534:
	.cfi_offset %rbx, -16
	movq	%rdi, %rbx
	callq	start_sequence
	movq	(%rbx), %rsi
	movq	$4, %rdi
	callq	gen_lowpart
	movq	%rax, (%rbx)
	movq	8(%rbx), %rsi
	movq	$4, %rdi
	callq	gen_lowpart
	movq	%rax, 8(%rbx)
	movq	(%rbx), %rbx
	movq	$86, %rdi
	movq	$4, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	popq	%rbx
	retq
.Ltmp6535:
	.size	gen_split_1240, .Ltmp6535-gen_split_1240
	.cfi_endproc

	.globl	gen_split_1241
	.align	16, 0x90
	.type	gen_split_1241,@function
gen_split_1241:                         # @gen_split_1241
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp6539:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp6540:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp6541:
	.cfi_def_cfa_offset 32
.Ltmp6542:
	.cfi_offset %rbx, -24
.Ltmp6543:
	.cfi_offset %r14, -16
	movq	%rdi, %rbx
	callq	start_sequence
	movq	(%rbx), %rsi
	movq	$4, %rdi
	callq	gen_lowpart
	movq	%rax, (%rbx)
	movq	16(%rbx), %rsi
	movq	$4, %rdi
	callq	gen_lowpart
	movq	%rax, 16(%rbx)
	movq	24(%rbx), %rsi
	movq	$4, %rdi
	callq	gen_lowpart
	movq	%rax, 24(%rbx)
	movq	(%rbx), %r14
	movq	8(%rbx), %rdx
	movq	16(%rbx), %rcx
	movq	$72, %rdi
	movq	$4, %rsi
	movq	%rax, %r8
	callq	gen_rtx_fmt_eee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp6544:
	.size	gen_split_1241, .Ltmp6544-gen_split_1241
	.cfi_endproc

	.globl	gen_peephole2_1242
	.align	16, 0x90
	.type	gen_peephole2_1242,@function
gen_peephole2_1242:                     # @gen_peephole2_1242
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp6548:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp6549:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp6550:
	.cfi_def_cfa_offset 32
.Ltmp6551:
	.cfi_offset %rbx, -24
.Ltmp6552:
	.cfi_offset %r14, -16
	movq	%rsi, %rbx
	movq	$0, (%rsp)
	movabsq	$.L.str1, %rdx
	leaq	(%rsp), %r8
	movq	$1, %rdi
	movq	$1, %rsi
	movq	$4, %rcx
	callq	peep2_find_free_register
	xorq	%r14, %r14
	testq	%rax, %rax
	movq	%rax, 16(%rbx)
	je	.LBB698_2
# BB#1:                                 # %if.end
	callq	start_sequence
	movq	(%rbx), %r14
	movq	8(%rbx), %rcx
	movq	16(%rbx), %rbx
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	movq	%rbx, %rdi
	callq	copy_rtx
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	callq	gen_sequence
	movq	%rax, %r14
	callq	end_sequence
.LBB698_2:                              # %return
	movq	%r14, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp6553:
	.size	gen_peephole2_1242, .Ltmp6553-gen_peephole2_1242
	.cfi_endproc

	.globl	gen_peephole2_1243
	.align	16, 0x90
	.type	gen_peephole2_1243,@function
gen_peephole2_1243:                     # @gen_peephole2_1243
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp6557:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp6558:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp6559:
	.cfi_def_cfa_offset 32
.Ltmp6560:
	.cfi_offset %rbx, -24
.Ltmp6561:
	.cfi_offset %r14, -16
	movq	%rsi, %rbx
	movq	$0, (%rsp)
	movabsq	$.L.str1, %rdx
	leaq	(%rsp), %r8
	movq	$1, %rdi
	movq	$1, %rsi
	movq	$5, %rcx
	callq	peep2_find_free_register
	xorq	%r14, %r14
	testq	%rax, %rax
	movq	%rax, 16(%rbx)
	je	.LBB699_2
# BB#1:                                 # %if.end
	callq	start_sequence
	movq	(%rbx), %r14
	movq	8(%rbx), %rcx
	movq	16(%rbx), %rbx
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	movq	%rbx, %rdi
	callq	copy_rtx
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	callq	gen_sequence
	movq	%rax, %r14
	callq	end_sequence
.LBB699_2:                              # %return
	movq	%r14, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp6562:
	.size	gen_peephole2_1243, .Ltmp6562-gen_peephole2_1243
	.cfi_endproc

	.globl	gen_peephole2_1244
	.align	16, 0x90
	.type	gen_peephole2_1244,@function
gen_peephole2_1244:                     # @gen_peephole2_1244
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp6566:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp6567:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp6568:
	.cfi_def_cfa_offset 32
.Ltmp6569:
	.cfi_offset %rbx, -24
.Ltmp6570:
	.cfi_offset %r14, -16
	movq	%rsi, %rbx
	movq	$0, (%rsp)
	movabsq	$.L.str1, %rdx
	leaq	(%rsp), %r8
	movq	$1, %rdi
	movq	$1, %rsi
	movq	$15, %rcx
	callq	peep2_find_free_register
	xorq	%r14, %r14
	testq	%rax, %rax
	movq	%rax, 16(%rbx)
	je	.LBB700_2
# BB#1:                                 # %if.end
	callq	start_sequence
	movq	(%rbx), %r14
	movq	8(%rbx), %rcx
	movq	16(%rbx), %rbx
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	movq	%rbx, %rdi
	callq	copy_rtx
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	callq	gen_sequence
	movq	%rax, %r14
	callq	end_sequence
.LBB700_2:                              # %return
	movq	%r14, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp6571:
	.size	gen_peephole2_1244, .Ltmp6571-gen_peephole2_1244
	.cfi_endproc

	.globl	gen_peephole2_1245
	.align	16, 0x90
	.type	gen_peephole2_1245,@function
gen_peephole2_1245:                     # @gen_peephole2_1245
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp6575:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp6576:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp6577:
	.cfi_def_cfa_offset 32
.Ltmp6578:
	.cfi_offset %rbx, -24
.Ltmp6579:
	.cfi_offset %r14, -16
	movq	%rsi, %rbx
	movq	$0, (%rsp)
	movabsq	$.L.str1, %rdx
	leaq	(%rsp), %r8
	movq	$1, %rdi
	movq	$1, %rsi
	movq	$3, %rcx
	callq	peep2_find_free_register
	xorq	%r14, %r14
	testq	%rax, %rax
	movq	%rax, 16(%rbx)
	je	.LBB701_2
# BB#1:                                 # %if.end
	callq	start_sequence
	movq	(%rbx), %r14
	movq	8(%rbx), %rcx
	movq	16(%rbx), %rbx
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	movq	%rbx, %rdi
	callq	copy_rtx
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	callq	gen_sequence
	movq	%rax, %r14
	callq	end_sequence
.LBB701_2:                              # %return
	movq	%r14, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp6580:
	.size	gen_peephole2_1245, .Ltmp6580-gen_peephole2_1245
	.cfi_endproc

	.globl	gen_peephole2_1246
	.align	16, 0x90
	.type	gen_peephole2_1246,@function
gen_peephole2_1246:                     # @gen_peephole2_1246
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp6584:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp6585:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp6586:
	.cfi_def_cfa_offset 32
.Ltmp6587:
	.cfi_offset %rbx, -24
.Ltmp6588:
	.cfi_offset %r14, -16
	movq	%rsi, %rbx
	movq	$0, (%rsp)
	movabsq	$.L.str2, %rdx
	leaq	(%rsp), %r8
	movq	$1, %rdi
	movq	$1, %rsi
	movq	$2, %rcx
	callq	peep2_find_free_register
	xorq	%r14, %r14
	testq	%rax, %rax
	movq	%rax, 16(%rbx)
	je	.LBB702_2
# BB#1:                                 # %if.end
	callq	start_sequence
	movq	(%rbx), %r14
	movq	8(%rbx), %rcx
	movq	16(%rbx), %rbx
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	movq	%rbx, %rdi
	callq	copy_rtx
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	callq	gen_sequence
	movq	%rax, %r14
	callq	end_sequence
.LBB702_2:                              # %return
	movq	%r14, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp6589:
	.size	gen_peephole2_1246, .Ltmp6589-gen_peephole2_1246
	.cfi_endproc

	.globl	gen_peephole2_1247
	.align	16, 0x90
	.type	gen_peephole2_1247,@function
gen_peephole2_1247:                     # @gen_peephole2_1247
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp6594:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp6595:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp6596:
	.cfi_def_cfa_offset 32
	subq	$16, %rsp
.Ltmp6597:
	.cfi_def_cfa_offset 48
.Ltmp6598:
	.cfi_offset %rbx, -32
.Ltmp6599:
	.cfi_offset %r14, -24
.Ltmp6600:
	.cfi_offset %r15, -16
	movq	%rsi, %rbx
	movq	$0, 8(%rsp)
	movabsq	$.L.str1, %rdx
	leaq	8(%rsp), %r8
	xorq	%rdi, %rdi
	xorq	%rsi, %rsi
	movq	$4, %rcx
	xorq	%r14, %r14
	callq	peep2_find_free_register
	testq	%rax, %rax
	movq	%rax, 8(%rbx)
	je	.LBB703_2
# BB#1:                                 # %if.end
	callq	start_sequence
	movq	(%rbx), %r14
	movq	8(%rbx), %r15
	movq	const_int_rtx+512(%rip), %rcx
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r15, %rdx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rbx
	movq	$52, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rcx
	movq	$2, %rdi
	xorq	%rax, %rax
	movq	%rbx, %rsi
	movq	%rcx, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_E
	movq	%rax, %rdi
	callq	emit
	movq	%r15, %rdi
	callq	copy_rtx
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	callq	gen_sequence
	movq	%rax, %r14
	callq	end_sequence
.LBB703_2:                              # %return
	movq	%r14, %rax
	addq	$16, %rsp
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp6601:
	.size	gen_peephole2_1247, .Ltmp6601-gen_peephole2_1247
	.cfi_endproc

	.globl	gen_peephole2_1248
	.align	16, 0x90
	.type	gen_peephole2_1248,@function
gen_peephole2_1248:                     # @gen_peephole2_1248
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp6606:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp6607:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp6608:
	.cfi_def_cfa_offset 32
	subq	$16, %rsp
.Ltmp6609:
	.cfi_def_cfa_offset 48
.Ltmp6610:
	.cfi_offset %rbx, -32
.Ltmp6611:
	.cfi_offset %r14, -24
.Ltmp6612:
	.cfi_offset %r15, -16
	movq	%rsi, %rbx
	movq	$0, 8(%rsp)
	movabsq	$.L.str1, %rdx
	leaq	8(%rsp), %r8
	xorq	%rdi, %rdi
	xorq	%rsi, %rsi
	movq	$3, %rcx
	xorq	%r14, %r14
	callq	peep2_find_free_register
	testq	%rax, %rax
	movq	%rax, 8(%rbx)
	je	.LBB704_2
# BB#1:                                 # %if.end
	callq	start_sequence
	movq	8(%rbx), %rdi
	callq	true_regnum
	movq	$4, %rdi
	movq	%rax, %rsi
	callq	gen_rtx_REG
	movq	%rax, 16(%rbx)
	movq	(%rbx), %r15
	movq	8(%rbx), %r14
	movq	const_int_rtx+512(%rip), %rcx
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rbx
	movq	$52, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rcx
	movq	$2, %rdi
	xorq	%rax, %rax
	movq	%rbx, %rsi
	movq	%rcx, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_E
	movq	%rax, %rdi
	callq	emit
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r15, %rdx
	movq	%r14, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	callq	gen_sequence
	movq	%rax, %r14
	callq	end_sequence
.LBB704_2:                              # %return
	movq	%r14, %rax
	addq	$16, %rsp
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp6613:
	.size	gen_peephole2_1248, .Ltmp6613-gen_peephole2_1248
	.cfi_endproc

	.globl	gen_peephole2_1249
	.align	16, 0x90
	.type	gen_peephole2_1249,@function
gen_peephole2_1249:                     # @gen_peephole2_1249
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp6618:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp6619:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp6620:
	.cfi_def_cfa_offset 32
	subq	$16, %rsp
.Ltmp6621:
	.cfi_def_cfa_offset 48
.Ltmp6622:
	.cfi_offset %rbx, -32
.Ltmp6623:
	.cfi_offset %r14, -24
.Ltmp6624:
	.cfi_offset %r15, -16
	movq	%rsi, %rbx
	movq	$0, 8(%rsp)
	movabsq	$.L.str2, %rdx
	leaq	8(%rsp), %r8
	xorq	%rdi, %rdi
	xorq	%rsi, %rsi
	movq	$2, %rcx
	xorq	%r14, %r14
	callq	peep2_find_free_register
	testq	%rax, %rax
	movq	%rax, 8(%rbx)
	je	.LBB705_2
# BB#1:                                 # %if.end
	callq	start_sequence
	movq	8(%rbx), %rdi
	callq	true_regnum
	movq	$4, %rdi
	movq	%rax, %rsi
	callq	gen_rtx_REG
	movq	%rax, 16(%rbx)
	movq	(%rbx), %r15
	movq	8(%rbx), %r14
	movq	const_int_rtx+512(%rip), %rcx
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rbx
	movq	$52, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rcx
	movq	$2, %rdi
	xorq	%rax, %rax
	movq	%rbx, %rsi
	movq	%rcx, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_E
	movq	%rax, %rdi
	callq	emit
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r15, %rdx
	movq	%r14, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	callq	gen_sequence
	movq	%rax, %r14
	callq	end_sequence
.LBB705_2:                              # %return
	movq	%r14, %rax
	addq	$16, %rsp
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp6625:
	.size	gen_peephole2_1249, .Ltmp6625-gen_peephole2_1249
	.cfi_endproc

	.globl	gen_peephole2_1250
	.align	16, 0x90
	.type	gen_peephole2_1250,@function
gen_peephole2_1250:                     # @gen_peephole2_1250
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp6629:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp6630:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp6631:
	.cfi_def_cfa_offset 32
.Ltmp6632:
	.cfi_offset %rbx, -24
.Ltmp6633:
	.cfi_offset %r14, -16
	movq	%rsi, %rbx
	movq	$0, (%rsp)
	movabsq	$.L.str1, %rdx
	leaq	(%rsp), %r8
	xorq	%rdi, %rdi
	xorq	%rsi, %rsi
	movq	$4, %rcx
	xorq	%r14, %r14
	callq	peep2_find_free_register
	testq	%rax, %rax
	movq	%rax, 16(%rbx)
	je	.LBB706_2
# BB#1:                                 # %if.end
	callq	start_sequence
	movq	(%rbx), %r14
	movq	8(%rbx), %rcx
	movq	16(%rbx), %rbx
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	movq	%rbx, %rdi
	callq	copy_rtx
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	callq	gen_sequence
	movq	%rax, %r14
	callq	end_sequence
.LBB706_2:                              # %return
	movq	%r14, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp6634:
	.size	gen_peephole2_1250, .Ltmp6634-gen_peephole2_1250
	.cfi_endproc

	.globl	gen_peephole2_1251
	.align	16, 0x90
	.type	gen_peephole2_1251,@function
gen_peephole2_1251:                     # @gen_peephole2_1251
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp6638:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp6639:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp6640:
	.cfi_def_cfa_offset 32
.Ltmp6641:
	.cfi_offset %rbx, -24
.Ltmp6642:
	.cfi_offset %r14, -16
	movq	%rsi, %rbx
	movq	$0, (%rsp)
	movabsq	$.L.str1, %rdx
	leaq	(%rsp), %r8
	xorq	%rdi, %rdi
	xorq	%rsi, %rsi
	movq	$3, %rcx
	xorq	%r14, %r14
	callq	peep2_find_free_register
	testq	%rax, %rax
	movq	%rax, 16(%rbx)
	je	.LBB707_2
# BB#1:                                 # %if.end
	callq	start_sequence
	movq	(%rbx), %r14
	movq	8(%rbx), %rcx
	movq	16(%rbx), %rbx
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	movq	%rbx, %rdi
	callq	copy_rtx
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	callq	gen_sequence
	movq	%rax, %r14
	callq	end_sequence
.LBB707_2:                              # %return
	movq	%r14, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp6643:
	.size	gen_peephole2_1251, .Ltmp6643-gen_peephole2_1251
	.cfi_endproc

	.globl	gen_peephole2_1252
	.align	16, 0x90
	.type	gen_peephole2_1252,@function
gen_peephole2_1252:                     # @gen_peephole2_1252
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp6647:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp6648:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp6649:
	.cfi_def_cfa_offset 32
.Ltmp6650:
	.cfi_offset %rbx, -24
.Ltmp6651:
	.cfi_offset %r14, -16
	movq	%rsi, %rbx
	movq	$0, (%rsp)
	movabsq	$.L.str2, %rdx
	leaq	(%rsp), %r8
	xorq	%rdi, %rdi
	xorq	%rsi, %rsi
	movq	$2, %rcx
	xorq	%r14, %r14
	callq	peep2_find_free_register
	testq	%rax, %rax
	movq	%rax, 16(%rbx)
	je	.LBB708_2
# BB#1:                                 # %if.end
	callq	start_sequence
	movq	(%rbx), %r14
	movq	8(%rbx), %rcx
	movq	16(%rbx), %rbx
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	movq	%rbx, %rdi
	callq	copy_rtx
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	callq	gen_sequence
	movq	%rax, %r14
	callq	end_sequence
.LBB708_2:                              # %return
	movq	%r14, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp6652:
	.size	gen_peephole2_1252, .Ltmp6652-gen_peephole2_1252
	.cfi_endproc

	.globl	gen_peephole2_1253
	.align	16, 0x90
	.type	gen_peephole2_1253,@function
gen_peephole2_1253:                     # @gen_peephole2_1253
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp6656:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp6657:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp6658:
	.cfi_def_cfa_offset 32
.Ltmp6659:
	.cfi_offset %rbx, -24
.Ltmp6660:
	.cfi_offset %r14, -16
	movq	%rsi, %r14
	movq	$0, (%rsp)
	movabsq	$.L.str1, %rdx
	leaq	(%rsp), %r8
	movq	$1, %rdi
	movq	$1, %rsi
	movq	$4, %rcx
	callq	peep2_find_free_register
	xorq	%rbx, %rbx
	testq	%rax, %rax
	movq	%rax, 24(%r14)
	je	.LBB709_2
# BB#1:                                 # %if.end
	callq	start_sequence
	movq	(%r14), %rcx
	movq	24(%r14), %rbx
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	movq	$55, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	%rax, %r14
	movq	%rbx, %rdi
	callq	copy_rtx
	movq	const_int_rtx+512(%rip), %rcx
	movq	$74, %rdi
	movq	$55, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
.LBB709_2:                              # %return
	movq	%rbx, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp6661:
	.size	gen_peephole2_1253, .Ltmp6661-gen_peephole2_1253
	.cfi_endproc

	.globl	gen_peephole2_1254
	.align	16, 0x90
	.type	gen_peephole2_1254,@function
gen_peephole2_1254:                     # @gen_peephole2_1254
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp6665:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp6666:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp6667:
	.cfi_def_cfa_offset 32
.Ltmp6668:
	.cfi_offset %rbx, -24
.Ltmp6669:
	.cfi_offset %r14, -16
	movq	%rsi, %rbx
	callq	start_sequence
	movq	(%rbx), %r14
	movq	8(%rbx), %rdx
	movq	const_int_rtx+504(%rip), %rcx
	movq	$85, %rdi
	movq	$4, %rsi
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rbx
	movq	$52, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rcx
	movq	$2, %rdi
	xorq	%rax, %rax
	movq	%rbx, %rsi
	movq	%rcx, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_E
	movq	%rax, %rdi
	callq	emit
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp6670:
	.size	gen_peephole2_1254, .Ltmp6670-gen_peephole2_1254
	.cfi_endproc

	.globl	gen_peephole2_1255
	.align	16, 0x90
	.type	gen_peephole2_1255,@function
gen_peephole2_1255:                     # @gen_peephole2_1255
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp6674:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp6675:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp6676:
	.cfi_def_cfa_offset 32
.Ltmp6677:
	.cfi_offset %rbx, -24
.Ltmp6678:
	.cfi_offset %r14, -16
	movq	%rsi, %rbx
	callq	start_sequence
	movq	(%rbx), %r14
	movq	8(%rbx), %rdx
	movq	const_int_rtx+504(%rip), %rcx
	movq	$85, %rdi
	movq	$3, %rsi
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rbx
	movq	$52, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rcx
	movq	$2, %rdi
	xorq	%rax, %rax
	movq	%rbx, %rsi
	movq	%rcx, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_E
	movq	%rax, %rdi
	callq	emit
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp6679:
	.size	gen_peephole2_1255, .Ltmp6679-gen_peephole2_1255
	.cfi_endproc

	.globl	gen_peephole2_1256
	.align	16, 0x90
	.type	gen_peephole2_1256,@function
gen_peephole2_1256:                     # @gen_peephole2_1256
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp6683:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp6684:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp6685:
	.cfi_def_cfa_offset 32
.Ltmp6686:
	.cfi_offset %rbx, -24
.Ltmp6687:
	.cfi_offset %r14, -16
	movq	%rsi, %rbx
	callq	start_sequence
	movq	(%rbx), %r14
	movq	8(%rbx), %rdx
	movq	const_int_rtx+504(%rip), %rcx
	movq	$85, %rdi
	movq	$2, %rsi
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rbx
	movq	$52, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rcx
	movq	$2, %rdi
	xorq	%rax, %rax
	movq	%rbx, %rsi
	movq	%rcx, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_E
	movq	%rax, %rdi
	callq	emit
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp6688:
	.size	gen_peephole2_1256, .Ltmp6688-gen_peephole2_1256
	.cfi_endproc

	.globl	gen_peephole2_1257
	.align	16, 0x90
	.type	gen_peephole2_1257,@function
gen_peephole2_1257:                     # @gen_peephole2_1257
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp6695:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp6696:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp6697:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp6698:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp6699:
	.cfi_def_cfa_offset 48
.Ltmp6700:
	.cfi_offset %rbx, -48
.Ltmp6701:
	.cfi_offset %r12, -40
.Ltmp6702:
	.cfi_offset %r13, -32
.Ltmp6703:
	.cfi_offset %r14, -24
.Ltmp6704:
	.cfi_offset %r15, -16
	movq	%rsi, %rbx
	callq	start_sequence
	movq	(%rbx), %r12
	movq	8(%rbx), %r14
	movq	$55, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	%rax, %rbx
	movq	$83, %rdi
	movq	$4, %rsi
	movq	%r12, %rdx
	movq	%r14, %rcx
	callq	gen_rtx_fmt_ee
	movq	const_int_rtx+512(%rip), %rcx
	movq	$74, %rdi
	movq	$55, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %r15
	movq	%r12, %rdi
	callq	copy_rtx
	movq	%rax, %r13
	movq	%r12, %rdi
	callq	copy_rtx
	movq	%rax, %rbx
	movq	%r14, %rdi
	callq	copy_rtx
	movq	$83, %rdi
	movq	$4, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r13, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rcx
	movq	$2, %rdi
	xorq	%rax, %rax
	movq	%r15, %rsi
	movq	%rcx, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_E
	movq	%rax, %rdi
	callq	emit
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp6705:
	.size	gen_peephole2_1257, .Ltmp6705-gen_peephole2_1257
	.cfi_endproc

	.globl	gen_peephole2_1258
	.align	16, 0x90
	.type	gen_peephole2_1258,@function
gen_peephole2_1258:                     # @gen_peephole2_1258
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp6712:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp6713:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp6714:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp6715:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp6716:
	.cfi_def_cfa_offset 48
.Ltmp6717:
	.cfi_offset %rbx, -48
.Ltmp6718:
	.cfi_offset %r12, -40
.Ltmp6719:
	.cfi_offset %r13, -32
.Ltmp6720:
	.cfi_offset %r14, -24
.Ltmp6721:
	.cfi_offset %r15, -16
	movq	%rsi, %rbx
	callq	start_sequence
	movq	(%rbx), %r12
	movq	8(%rbx), %r14
	movq	$55, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	%rax, %rbx
	movq	$83, %rdi
	movq	$2, %rsi
	movq	%r12, %rdx
	movq	%r14, %rcx
	callq	gen_rtx_fmt_ee
	movq	const_int_rtx+512(%rip), %rcx
	movq	$74, %rdi
	movq	$55, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %r15
	movq	%r12, %rdi
	callq	copy_rtx
	movq	%rax, %r13
	movq	%r12, %rdi
	callq	copy_rtx
	movq	%rax, %rbx
	movq	%r14, %rdi
	callq	copy_rtx
	movq	$83, %rdi
	movq	$2, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r13, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rcx
	movq	$2, %rdi
	xorq	%rax, %rax
	movq	%r15, %rsi
	movq	%rcx, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_E
	movq	%rax, %rdi
	callq	emit
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp6722:
	.size	gen_peephole2_1258, .Ltmp6722-gen_peephole2_1258
	.cfi_endproc

	.globl	gen_peephole2_1259
	.align	16, 0x90
	.type	gen_peephole2_1259,@function
gen_peephole2_1259:                     # @gen_peephole2_1259
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp6729:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp6730:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp6731:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp6732:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp6733:
	.cfi_def_cfa_offset 48
.Ltmp6734:
	.cfi_offset %rbx, -48
.Ltmp6735:
	.cfi_offset %r12, -40
.Ltmp6736:
	.cfi_offset %r13, -32
.Ltmp6737:
	.cfi_offset %r14, -24
.Ltmp6738:
	.cfi_offset %r15, -16
	movq	%rsi, %rbx
	callq	start_sequence
	movq	(%rbx), %r13
	movq	8(%rbx), %r14
	movq	$55, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	%rax, %r15
	xorq	%rdi, %rdi
	movq	$8, %rsi
	callq	gen_rtx_CONST_INT
	movq	%rax, %rbx
	xorq	%rdi, %rdi
	movq	$8, %rsi
	callq	gen_rtx_CONST_INT
	movq	$133, %rdi
	movq	$4, %rsi
	movq	%r13, %rdx
	movq	%rbx, %rcx
	movq	%rax, %r8
	callq	gen_rtx_fmt_eee
	movq	$83, %rdi
	movq	$4, %rsi
	movq	%rax, %rdx
	movq	%r14, %rcx
	callq	gen_rtx_fmt_ee
	movq	const_int_rtx+512(%rip), %rcx
	movq	$74, %rdi
	movq	$55, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r15, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %r15
	movq	%r13, %rdi
	callq	copy_rtx
	movq	%rax, %r12
	xorq	%rdi, %rdi
	movq	$8, %rsi
	callq	gen_rtx_CONST_INT
	movq	%rax, %rbx
	xorq	%rdi, %rdi
	movq	$8, %rsi
	callq	gen_rtx_CONST_INT
	movq	$133, %rdi
	movq	$4, %rsi
	movq	%r12, %rdx
	movq	%rbx, %rcx
	movq	%rax, %r8
	callq	gen_rtx_fmt_eee
	movq	%rax, %r12
	movq	%r13, %rdi
	callq	copy_rtx
	movq	%rax, %r13
	xorq	%rdi, %rdi
	movq	$8, %rsi
	callq	gen_rtx_CONST_INT
	movq	%rax, %rbx
	xorq	%rdi, %rdi
	movq	$8, %rsi
	callq	gen_rtx_CONST_INT
	movq	$133, %rdi
	movq	$4, %rsi
	movq	%r13, %rdx
	movq	%rbx, %rcx
	movq	%rax, %r8
	callq	gen_rtx_fmt_eee
	movq	%rax, %rbx
	movq	%r14, %rdi
	callq	copy_rtx
	movq	$83, %rdi
	movq	$4, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r12, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rcx
	movq	$2, %rdi
	xorq	%rax, %rax
	movq	%r15, %rsi
	movq	%rcx, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_E
	movq	%rax, %rdi
	callq	emit
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp6739:
	.size	gen_peephole2_1259, .Ltmp6739-gen_peephole2_1259
	.cfi_endproc

	.globl	gen_peephole2_1260
	.align	16, 0x90
	.type	gen_peephole2_1260,@function
gen_peephole2_1260:                     # @gen_peephole2_1260
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp6746:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp6747:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp6748:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp6749:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp6750:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp6751:
	.cfi_def_cfa_offset 64
.Ltmp6752:
	.cfi_offset %rbx, -48
.Ltmp6753:
	.cfi_offset %r12, -40
.Ltmp6754:
	.cfi_offset %r13, -32
.Ltmp6755:
	.cfi_offset %r14, -24
.Ltmp6756:
	.cfi_offset %r15, -16
	movq	%rsi, %rbx
	movq	$0, 8(%rsp)
	movabsq	$.L.str1, %rdx
	leaq	8(%rsp), %r8
	xorq	%rdi, %rdi
	xorq	%rsi, %rsi
	movq	$4, %rcx
	xorq	%r14, %r14
	callq	peep2_find_free_register
	testq	%rax, %rax
	movq	%rax, 16(%rbx)
	je	.LBB716_2
# BB#1:                                 # %if.end
	callq	start_sequence
	movq	(%rbx), %r14
	movq	8(%rbx), %rcx
	movq	16(%rbx), %r15
	movq	24(%rbx), %rbx
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r15, %rdx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	movl	(%rbx), %ebx
	movq	%rbx, %r12
	andq	$65535, %r12            # imm = 0xFFFF
	movq	$16, %rcx
	shrq	%cl, %rbx
	andq	$255, %rbx
	movq	%r14, %rdi
	callq	copy_rtx
	movq	%rax, %r13
	movq	%r15, %rdi
	callq	copy_rtx
	movq	%rax, %rcx
	xorq	%rax, %rax
	movq	%r12, %rdi
	movq	%rbx, %rsi
	movq	%r13, %rdx
	callq	gen_rtx
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rbx
	movq	$52, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rcx
	movq	$2, %rdi
	xorq	%rax, %rax
	movq	%rbx, %rsi
	movq	%rcx, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_E
	movq	%rax, %rdi
	callq	emit
	callq	gen_sequence
	movq	%rax, %r14
	callq	end_sequence
.LBB716_2:                              # %return
	movq	%r14, %rax
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp6757:
	.size	gen_peephole2_1260, .Ltmp6757-gen_peephole2_1260
	.cfi_endproc

	.globl	gen_peephole2_1261
	.align	16, 0x90
	.type	gen_peephole2_1261,@function
gen_peephole2_1261:                     # @gen_peephole2_1261
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp6763:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp6764:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp6765:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp6766:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp6767:
	.cfi_def_cfa_offset 48
.Ltmp6768:
	.cfi_offset %rbx, -40
.Ltmp6769:
	.cfi_offset %r12, -32
.Ltmp6770:
	.cfi_offset %r14, -24
.Ltmp6771:
	.cfi_offset %r15, -16
	movq	%rsi, %rbx
	movq	$0, (%rsp)
	movabsq	$.L.str1, %rdx
	leaq	(%rsp), %r8
	xorq	%rdi, %rdi
	xorq	%rsi, %rsi
	movq	$4, %rcx
	xorq	%r14, %r14
	callq	peep2_find_free_register
	testq	%rax, %rax
	movq	%rax, 16(%rbx)
	je	.LBB717_2
# BB#1:                                 # %if.end
	callq	start_sequence
	movq	(%rbx), %r14
	movq	8(%rbx), %rcx
	movq	16(%rbx), %r15
	movq	24(%rbx), %rbx
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r15, %rdx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	movl	(%rbx), %ebx
	movq	%rbx, %r12
	andq	$65535, %r12            # imm = 0xFFFF
	movq	$16, %rcx
	shrq	%cl, %rbx
	andq	$255, %rbx
	movq	%r15, %rdi
	callq	copy_rtx
	movq	%rax, %r15
	movq	%r14, %rdi
	callq	copy_rtx
	movq	%rax, %rcx
	xorq	%rax, %rax
	movq	%r12, %rdi
	movq	%rbx, %rsi
	movq	%r15, %rdx
	callq	gen_rtx
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rbx
	movq	$52, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rcx
	movq	$2, %rdi
	xorq	%rax, %rax
	movq	%rbx, %rsi
	movq	%rcx, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_E
	movq	%rax, %rdi
	callq	emit
	callq	gen_sequence
	movq	%rax, %r14
	callq	end_sequence
.LBB717_2:                              # %return
	movq	%r14, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.Ltmp6772:
	.size	gen_peephole2_1261, .Ltmp6772-gen_peephole2_1261
	.cfi_endproc

	.globl	gen_peephole2_1262
	.align	16, 0x90
	.type	gen_peephole2_1262,@function
gen_peephole2_1262:                     # @gen_peephole2_1262
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp6779:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp6780:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp6781:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp6782:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp6783:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp6784:
	.cfi_def_cfa_offset 64
.Ltmp6785:
	.cfi_offset %rbx, -48
.Ltmp6786:
	.cfi_offset %r12, -40
.Ltmp6787:
	.cfi_offset %r13, -32
.Ltmp6788:
	.cfi_offset %r14, -24
.Ltmp6789:
	.cfi_offset %r15, -16
	movq	%rsi, %rbx
	movq	$0, 8(%rsp)
	movabsq	$.L.str1, %rdx
	leaq	8(%rsp), %r8
	xorq	%rdi, %rdi
	xorq	%rsi, %rsi
	movq	$4, %rcx
	xorq	%r14, %r14
	callq	peep2_find_free_register
	testq	%rax, %rax
	movq	%rax, 16(%rbx)
	je	.LBB718_2
# BB#1:                                 # %if.end
	callq	start_sequence
	movq	(%rbx), %rcx
	movq	%rcx, (%rsp)            # 8-byte Spill
	movq	8(%rbx), %r15
	movq	16(%rbx), %r12
	movq	24(%rbx), %rbx
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r12, %rdx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	movq	%r12, %rdi
	callq	copy_rtx
	movl	(%rbx), %ebx
	movq	%rax, %r13
	movq	%rbx, %r14
	andq	$65535, %r14            # imm = 0xFFFF
	movq	$16, %rcx
	shrq	%cl, %rbx
	andq	$255, %rbx
	movq	%r12, %rdi
	callq	copy_rtx
	movq	%rax, %rcx
	xorq	%rax, %rax
	movq	%r14, %rdi
	movq	%rbx, %rsi
	movq	%rcx, %rdx
	movq	%r15, %rcx
	callq	gen_rtx
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r13, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rbx
	movq	$52, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rcx
	movq	$2, %rdi
	xorq	%rax, %rax
	movq	%rbx, %rsi
	movq	%rcx, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_E
	movq	%rax, %rdi
	callq	emit
	movq	(%rsp), %rdi            # 8-byte Reload
	callq	copy_rtx
	movq	%rax, %rbx
	movq	%r12, %rdi
	callq	copy_rtx
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	callq	gen_sequence
	movq	%rax, %r14
	callq	end_sequence
.LBB718_2:                              # %return
	movq	%r14, %rax
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp6790:
	.size	gen_peephole2_1262, .Ltmp6790-gen_peephole2_1262
	.cfi_endproc

	.globl	gen_peephole2_1263
	.align	16, 0x90
	.type	gen_peephole2_1263,@function
gen_peephole2_1263:                     # @gen_peephole2_1263
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp6797:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp6798:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp6799:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp6800:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp6801:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp6802:
	.cfi_def_cfa_offset 64
.Ltmp6803:
	.cfi_offset %rbx, -48
.Ltmp6804:
	.cfi_offset %r12, -40
.Ltmp6805:
	.cfi_offset %r13, -32
.Ltmp6806:
	.cfi_offset %r14, -24
.Ltmp6807:
	.cfi_offset %r15, -16
	movq	%rsi, %rbx
	movq	$0, 8(%rsp)
	movabsq	$.L.str1, %rdx
	leaq	8(%rsp), %r8
	xorq	%rdi, %rdi
	xorq	%rsi, %rsi
	movq	$4, %rcx
	xorq	%r14, %r14
	callq	peep2_find_free_register
	testq	%rax, %rax
	movq	%rax, 16(%rbx)
	je	.LBB719_2
# BB#1:                                 # %if.end
	callq	start_sequence
	movq	(%rbx), %rcx
	movq	%rcx, (%rsp)            # 8-byte Spill
	movq	8(%rbx), %r15
	movq	16(%rbx), %r12
	movq	24(%rbx), %rbx
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r12, %rdx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	movq	%r12, %rdi
	callq	copy_rtx
	movl	(%rbx), %ebx
	movq	%rax, %r13
	movq	%rbx, %r14
	andq	$65535, %r14            # imm = 0xFFFF
	movq	$16, %rcx
	shrq	%cl, %rbx
	andq	$255, %rbx
	movq	%r12, %rdi
	callq	copy_rtx
	movq	%rax, %rcx
	xorq	%rax, %rax
	movq	%r14, %rdi
	movq	%rbx, %rsi
	movq	%r15, %rdx
	callq	gen_rtx
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r13, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rbx
	movq	$52, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rcx
	movq	$2, %rdi
	xorq	%rax, %rax
	movq	%rbx, %rsi
	movq	%rcx, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_E
	movq	%rax, %rdi
	callq	emit
	movq	(%rsp), %rdi            # 8-byte Reload
	callq	copy_rtx
	movq	%rax, %rbx
	movq	%r12, %rdi
	callq	copy_rtx
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	callq	gen_sequence
	movq	%rax, %r14
	callq	end_sequence
.LBB719_2:                              # %return
	movq	%r14, %rax
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp6808:
	.size	gen_peephole2_1263, .Ltmp6808-gen_peephole2_1263
	.cfi_endproc

	.globl	gen_peephole2_1264
	.align	16, 0x90
	.type	gen_peephole2_1264,@function
gen_peephole2_1264:                     # @gen_peephole2_1264
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp6812:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp6813:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp6814:
	.cfi_def_cfa_offset 32
.Ltmp6815:
	.cfi_offset %rbx, -24
.Ltmp6816:
	.cfi_offset %r14, -16
	movq	%rsi, %r14
	callq	start_sequence
	movq	(%r14), %rdi
	movl	(%rdi), %eax
	andq	$16711680, %rax         # imm = 0xFF0000
	cmpq	$327680, %rax           # imm = 0x50000
	jne	.LBB720_1
# BB#2:                                 # %entry
	movabsq	$1, %rbx
	jmp	.LBB720_3
.LBB720_1:
	movabsq	$0, %rbx
.LBB720_3:                              # %entry
	orq	$4, %rbx
	callq	true_regnum
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	gen_rtx_REG
	movq	%rax, (%r14)
	movq	const_int_rtx+512(%rip), %rcx
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rbx
	movq	$52, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rcx
	movq	$2, %rdi
	xorq	%rax, %rax
	movq	%rbx, %rsi
	movq	%rcx, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_E
	movq	%rax, %rdi
	callq	emit
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp6817:
	.size	gen_peephole2_1264, .Ltmp6817-gen_peephole2_1264
	.cfi_endproc

	.globl	gen_peephole2_1265
	.align	16, 0x90
	.type	gen_peephole2_1265,@function
gen_peephole2_1265:                     # @gen_peephole2_1265
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp6820:
	.cfi_def_cfa_offset 16
.Ltmp6821:
	.cfi_offset %rbx, -16
	movq	%rsi, %rbx
	callq	start_sequence
	movq	(%rbx), %rdx
	movq	$64, %rdi
	xorq	%rsi, %rsi
	callq	gen_rtx_fmt_e
	movq	const_int_rtx+512(%rip), %rcx
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rbx
	movq	$52, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rcx
	movq	$2, %rdi
	xorq	%rax, %rax
	movq	%rbx, %rsi
	movq	%rcx, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_E
	movq	%rax, %rdi
	callq	emit
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	popq	%rbx
	retq
.Ltmp6822:
	.size	gen_peephole2_1265, .Ltmp6822-gen_peephole2_1265
	.cfi_endproc

	.globl	gen_peephole2_1266
	.align	16, 0x90
	.type	gen_peephole2_1266,@function
gen_peephole2_1266:                     # @gen_peephole2_1266
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp6826:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp6827:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp6828:
	.cfi_def_cfa_offset 32
.Ltmp6829:
	.cfi_offset %rbx, -24
.Ltmp6830:
	.cfi_offset %r14, -16
	movq	%rsi, %r14
	callq	start_sequence
	movq	(%r14), %rdi
	movl	(%rdi), %eax
	andq	$16711680, %rax         # imm = 0xFF0000
	cmpq	$327680, %rax           # imm = 0x50000
	jne	.LBB722_1
# BB#2:                                 # %entry
	movabsq	$1, %rbx
	jmp	.LBB722_3
.LBB722_1:
	movabsq	$0, %rbx
.LBB722_3:                              # %entry
	orq	$4, %rbx
	callq	true_regnum
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	gen_rtx_REG
	movq	%rax, (%r14)
	movq	const_int_rtx+504(%rip), %rcx
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rbx
	movq	$52, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rcx
	movq	$2, %rdi
	xorq	%rax, %rax
	movq	%rbx, %rsi
	movq	%rcx, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_E
	movq	%rax, %rdi
	callq	emit
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp6831:
	.size	gen_peephole2_1266, .Ltmp6831-gen_peephole2_1266
	.cfi_endproc

	.globl	gen_peephole2_1267
	.align	16, 0x90
	.type	gen_peephole2_1267,@function
gen_peephole2_1267:                     # @gen_peephole2_1267
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp6835:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp6836:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp6837:
	.cfi_def_cfa_offset 32
.Ltmp6838:
	.cfi_offset %rbx, -24
.Ltmp6839:
	.cfi_offset %r14, -16
	movq	%rsi, %rbx
	callq	start_sequence
	movq	(%rbx), %r14
	movq	8(%rbx), %rbx
	movq	%r14, %rdi
	callq	copy_rtx
	movq	$75, %rdi
	movq	$4, %rsi
	movq	%rax, %rdx
	movq	%rbx, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rbx
	movq	$52, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rcx
	movq	$2, %rdi
	xorq	%rax, %rax
	movq	%rbx, %rsi
	movq	%rcx, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_E
	movq	%rax, %rdi
	callq	emit
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp6840:
	.size	gen_peephole2_1267, .Ltmp6840-gen_peephole2_1267
	.cfi_endproc

	.globl	gen_peephole2_1268
	.align	16, 0x90
	.type	gen_peephole2_1268,@function
gen_peephole2_1268:                     # @gen_peephole2_1268
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp6844:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp6845:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp6846:
	.cfi_def_cfa_offset 32
.Ltmp6847:
	.cfi_offset %rbx, -24
.Ltmp6848:
	.cfi_offset %r14, -16
	movq	%rsi, %rbx
	callq	start_sequence
	movq	16(%rbx), %rsi
	movq	$4, %rdi
	callq	gen_lowpart
	movq	%rax, %r14
	movq	%r14, 16(%rbx)
	movq	(%rbx), %rbx
	movq	%rbx, %rdi
	callq	copy_rtx
	movq	$75, %rdi
	movq	$4, %rsi
	movq	%rax, %rdx
	movq	%r14, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rbx
	movq	$52, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rcx
	movq	$2, %rdi
	xorq	%rax, %rax
	movq	%rbx, %rsi
	movq	%rcx, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_E
	movq	%rax, %rdi
	callq	emit
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp6849:
	.size	gen_peephole2_1268, .Ltmp6849-gen_peephole2_1268
	.cfi_endproc

	.globl	gen_peephole2_1269
	.align	16, 0x90
	.type	gen_peephole2_1269,@function
gen_peephole2_1269:                     # @gen_peephole2_1269
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp6853:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp6854:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp6855:
	.cfi_def_cfa_offset 32
.Ltmp6856:
	.cfi_offset %rbx, -24
.Ltmp6857:
	.cfi_offset %r14, -16
	movq	%rsi, %rbx
	callq	start_sequence
	movq	(%rbx), %r14
	movq	8(%rbx), %rbx
	movq	%r14, %rdi
	callq	copy_rtx
	movq	$75, %rdi
	movq	$5, %rsi
	movq	%rax, %rdx
	movq	%rbx, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rbx
	movq	$52, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rcx
	movq	$2, %rdi
	xorq	%rax, %rax
	movq	%rbx, %rsi
	movq	%rcx, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_E
	movq	%rax, %rdi
	callq	emit
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp6858:
	.size	gen_peephole2_1269, .Ltmp6858-gen_peephole2_1269
	.cfi_endproc

	.globl	gen_peephole2_1270
	.align	16, 0x90
	.type	gen_peephole2_1270,@function
gen_peephole2_1270:                     # @gen_peephole2_1270
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp6862:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp6863:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp6864:
	.cfi_def_cfa_offset 32
.Ltmp6865:
	.cfi_offset %rbx, -24
.Ltmp6866:
	.cfi_offset %r14, -16
	movq	%rsi, %rbx
	callq	start_sequence
	movq	8(%rbx), %rax
	movq	8(%rax), %rdi
	callq	exact_log2_wide
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	xorq	%rdi, %rdi
	movq	%rax, %rsi
	callq	gen_rtx_CONST_INT
	movq	%rax, %r14
	movq	%r14, 16(%rbx)
	movq	(%rbx), %rbx
	movq	%rbx, %rdi
	callq	copy_rtx
	movq	$87, %rdi
	movq	$4, %rsi
	movq	%rax, %rdx
	movq	%r14, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rbx
	movq	$52, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rcx
	movq	$2, %rdi
	xorq	%rax, %rax
	movq	%rbx, %rsi
	movq	%rcx, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_E
	movq	%rax, %rdi
	callq	emit
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp6867:
	.size	gen_peephole2_1270, .Ltmp6867-gen_peephole2_1270
	.cfi_endproc

	.globl	gen_peephole2_1271
	.align	16, 0x90
	.type	gen_peephole2_1271,@function
gen_peephole2_1271:                     # @gen_peephole2_1271
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp6871:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp6872:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp6873:
	.cfi_def_cfa_offset 32
.Ltmp6874:
	.cfi_offset %rbx, -24
.Ltmp6875:
	.cfi_offset %r14, -16
	movq	%rsi, %rbx
	callq	start_sequence
	movq	8(%rbx), %rax
	movq	8(%rax), %rdi
	callq	exact_log2_wide
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	xorq	%rdi, %rdi
	movq	%rax, %rsi
	callq	gen_rtx_CONST_INT
	movq	%rax, %r14
	movq	%r14, 16(%rbx)
	movq	(%rbx), %rbx
	movq	%rbx, %rdi
	callq	copy_rtx
	movq	$87, %rdi
	movq	$5, %rsi
	movq	%rax, %rdx
	movq	%r14, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rbx
	movq	$52, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rcx
	movq	$2, %rdi
	xorq	%rax, %rax
	movq	%rbx, %rsi
	movq	%rcx, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_E
	movq	%rax, %rdi
	callq	emit
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp6876:
	.size	gen_peephole2_1271, .Ltmp6876-gen_peephole2_1271
	.cfi_endproc

	.globl	gen_peephole2_1272
	.align	16, 0x90
	.type	gen_peephole2_1272,@function
gen_peephole2_1272:                     # @gen_peephole2_1272
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp6880:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp6881:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp6882:
	.cfi_def_cfa_offset 32
.Ltmp6883:
	.cfi_offset %rbx, -24
.Ltmp6884:
	.cfi_offset %r14, -16
	movq	%rsi, %rbx
	callq	start_sequence
	movq	16(%rbx), %rax
	movq	8(%rax), %rdi
	callq	exact_log2_wide
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	xorq	%rdi, %rdi
	movq	%rax, %rsi
	callq	gen_rtx_CONST_INT
	movq	%rax, %r14
	movq	%r14, 16(%rbx)
	movq	(%rbx), %rbx
	movq	%rbx, %rdi
	callq	copy_rtx
	movq	$87, %rdi
	movq	$4, %rsi
	movq	%rax, %rdx
	movq	%r14, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rbx
	movq	$52, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rcx
	movq	$2, %rdi
	xorq	%rax, %rax
	movq	%rbx, %rsi
	movq	%rcx, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_E
	movq	%rax, %rdi
	callq	emit
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp6885:
	.size	gen_peephole2_1272, .Ltmp6885-gen_peephole2_1272
	.cfi_endproc

	.globl	gen_peephole2_1273
	.align	16, 0x90
	.type	gen_peephole2_1273,@function
gen_peephole2_1273:                     # @gen_peephole2_1273
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp6889:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp6890:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp6891:
	.cfi_def_cfa_offset 32
.Ltmp6892:
	.cfi_offset %rbx, -24
.Ltmp6893:
	.cfi_offset %r14, -16
	movq	%rsi, %r14
	movq	$0, (%rsp)
	movabsq	$.L.str1, %rdx
	leaq	(%rsp), %r8
	xorq	%rdi, %rdi
	xorq	%rsi, %rsi
	movq	$4, %rcx
	xorq	%rbx, %rbx
	callq	peep2_find_free_register
	testq	%rax, %rax
	movq	%rax, (%r14)
	je	.LBB729_2
# BB#1:                                 # %if.end
	callq	start_sequence
	movq	(%r14), %r14
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rdi
	callq	emit_insn
	movq	$4, %rdi
	movq	$7, %rsi
	callq	gen_rtx_REG
	movq	$96, %rdi
	movq	$4, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	$4, %rdi
	movq	%rax, %rsi
	callq	gen_rtx_MEM
	movq	%rax, %rbx
	movq	%r14, %rdi
	callq	copy_rtx
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rbx
	movq	$62, %rdi
	xorq	%rsi, %rsi
	callq	gen_rtx_fmt_0
	movq	$51, %rdi
	movq	%rax, %rsi
	callq	gen_rtx_MEM
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rcx
	movq	$2, %rdi
	xorq	%rax, %rax
	movq	%rbx, %rsi
	movq	%rcx, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_E
	movq	%rax, %rdi
	callq	emit
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
.LBB729_2:                              # %return
	movq	%rbx, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp6894:
	.size	gen_peephole2_1273, .Ltmp6894-gen_peephole2_1273
	.cfi_endproc

	.globl	gen_peephole2_1274
	.align	16, 0x90
	.type	gen_peephole2_1274,@function
gen_peephole2_1274:                     # @gen_peephole2_1274
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp6898:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp6899:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp6900:
	.cfi_def_cfa_offset 32
.Ltmp6901:
	.cfi_offset %rbx, -24
.Ltmp6902:
	.cfi_offset %r14, -16
	movq	%rsi, %r14
	movq	$0, (%rsp)
	movabsq	$.L.str1, %rdx
	leaq	(%rsp), %r8
	xorq	%rdi, %rdi
	xorq	%rsi, %rsi
	movq	$4, %rcx
	xorq	%rbx, %rbx
	callq	peep2_find_free_register
	testq	%rax, %rax
	movq	%rax, (%r14)
	je	.LBB730_2
# BB#1:                                 # %if.end
	callq	start_sequence
	movq	(%r14), %r14
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rdi
	callq	emit_insn
	movq	$4, %rdi
	movq	$7, %rsi
	callq	gen_rtx_REG
	movq	$96, %rdi
	movq	$4, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	$4, %rdi
	movq	%rax, %rsi
	callq	gen_rtx_MEM
	movq	%rax, %rbx
	movq	%r14, %rdi
	callq	copy_rtx
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	movq	$4, %rdi
	movq	$7, %rsi
	callq	gen_rtx_REG
	movq	$96, %rdi
	movq	$4, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	$4, %rdi
	movq	%rax, %rsi
	callq	gen_rtx_MEM
	movq	%rax, %rbx
	movq	%r14, %rdi
	callq	copy_rtx
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rbx
	movq	$62, %rdi
	xorq	%rsi, %rsi
	callq	gen_rtx_fmt_0
	movq	$51, %rdi
	movq	%rax, %rsi
	callq	gen_rtx_MEM
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rcx
	movq	$2, %rdi
	xorq	%rax, %rax
	movq	%rbx, %rsi
	movq	%rcx, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_E
	movq	%rax, %rdi
	callq	emit
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
.LBB730_2:                              # %return
	movq	%rbx, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp6903:
	.size	gen_peephole2_1274, .Ltmp6903-gen_peephole2_1274
	.cfi_endproc

	.globl	gen_peephole2_1275
	.align	16, 0x90
	.type	gen_peephole2_1275,@function
gen_peephole2_1275:                     # @gen_peephole2_1275
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp6907:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp6908:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp6909:
	.cfi_def_cfa_offset 32
.Ltmp6910:
	.cfi_offset %rbx, -24
.Ltmp6911:
	.cfi_offset %r14, -16
	movq	%rsi, %r14
	movq	$0, (%rsp)
	movabsq	$.L.str1, %rdx
	leaq	(%rsp), %r8
	xorq	%rdi, %rdi
	xorq	%rsi, %rsi
	movq	$4, %rcx
	xorq	%rbx, %rbx
	callq	peep2_find_free_register
	testq	%rax, %rax
	movq	%rax, (%r14)
	je	.LBB731_2
# BB#1:                                 # %if.end
	callq	start_sequence
	movq	(%r14), %r14
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rdi
	callq	emit_insn
	movq	$4, %rdi
	movq	$7, %rsi
	callq	gen_rtx_REG
	movq	$96, %rdi
	movq	$4, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	$4, %rdi
	movq	%rax, %rsi
	callq	gen_rtx_MEM
	movq	%rax, %rbx
	movq	%r14, %rdi
	callq	copy_rtx
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
.LBB731_2:                              # %return
	movq	%rbx, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp6912:
	.size	gen_peephole2_1275, .Ltmp6912-gen_peephole2_1275
	.cfi_endproc

	.globl	gen_peephole2_1276
	.align	16, 0x90
	.type	gen_peephole2_1276,@function
gen_peephole2_1276:                     # @gen_peephole2_1276
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp6916:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp6917:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp6918:
	.cfi_def_cfa_offset 32
.Ltmp6919:
	.cfi_offset %rbx, -24
.Ltmp6920:
	.cfi_offset %r14, -16
	movq	%rsi, %r14
	movq	$0, (%rsp)
	movabsq	$.L.str1, %rdx
	leaq	(%rsp), %r8
	xorq	%rdi, %rdi
	xorq	%rsi, %rsi
	movq	$4, %rcx
	xorq	%rbx, %rbx
	callq	peep2_find_free_register
	testq	%rax, %rax
	movq	%rax, (%r14)
	je	.LBB732_2
# BB#1:                                 # %if.end
	callq	start_sequence
	movq	(%r14), %r14
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rdi
	callq	emit_insn
	movq	$4, %rdi
	movq	$7, %rsi
	callq	gen_rtx_REG
	movq	$96, %rdi
	movq	$4, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	$4, %rdi
	movq	%rax, %rsi
	callq	gen_rtx_MEM
	movq	%rax, %rbx
	movq	%r14, %rdi
	callq	copy_rtx
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	movq	$4, %rdi
	movq	$7, %rsi
	callq	gen_rtx_REG
	movq	$96, %rdi
	movq	$4, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	$4, %rdi
	movq	%rax, %rsi
	callq	gen_rtx_MEM
	movq	%rax, %rbx
	movq	%r14, %rdi
	callq	copy_rtx
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
.LBB732_2:                              # %return
	movq	%rbx, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp6921:
	.size	gen_peephole2_1276, .Ltmp6921-gen_peephole2_1276
	.cfi_endproc

	.globl	gen_peephole2_1277
	.align	16, 0x90
	.type	gen_peephole2_1277,@function
gen_peephole2_1277:                     # @gen_peephole2_1277
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp6926:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp6927:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp6928:
	.cfi_def_cfa_offset 32
	subq	$16, %rsp
.Ltmp6929:
	.cfi_def_cfa_offset 48
.Ltmp6930:
	.cfi_offset %rbx, -32
.Ltmp6931:
	.cfi_offset %r14, -24
.Ltmp6932:
	.cfi_offset %r15, -16
	movq	%rsi, %r14
	movq	$0, 8(%rsp)
	movabsq	$.L.str1, %rdx
	leaq	8(%rsp), %r8
	xorq	%rdi, %rdi
	xorq	%rsi, %rsi
	movq	$4, %rcx
	xorq	%rbx, %rbx
	callq	peep2_find_free_register
	testq	%rax, %rax
	movq	%rax, (%r14)
	je	.LBB733_2
# BB#1:                                 # %if.end
	callq	start_sequence
	movq	(%r14), %rbx
	movq	$4, %rdi
	movq	$7, %rsi
	callq	gen_rtx_REG
	movq	$4, %rdi
	movq	%rax, %rsi
	callq	gen_rtx_MEM
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %r14
	movq	$4, %rdi
	movq	$7, %rsi
	callq	gen_rtx_REG
	movq	%rax, %r15
	movq	$4, %rdi
	movq	$7, %rsi
	callq	gen_rtx_REG
	movq	%rax, %rbx
	xorq	%rdi, %rdi
	movq	$4, %rsi
	callq	gen_rtx_CONST_INT
	movq	$75, %rdi
	movq	$4, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r15, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rbx
	movq	$62, %rdi
	xorq	%rsi, %rsi
	callq	gen_rtx_fmt_0
	movq	$51, %rdi
	movq	%rax, %rsi
	callq	gen_rtx_MEM
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rcx
	movq	$3, %rdi
	xorq	%rax, %rax
	movq	%r14, %rsi
	movq	%rbx, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_E
	movq	%rax, %rdi
	callq	emit
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
.LBB733_2:                              # %return
	movq	%rbx, %rax
	addq	$16, %rsp
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp6933:
	.size	gen_peephole2_1277, .Ltmp6933-gen_peephole2_1277
	.cfi_endproc

	.globl	gen_peephole2_1278
	.align	16, 0x90
	.type	gen_peephole2_1278,@function
gen_peephole2_1278:                     # @gen_peephole2_1278
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp6939:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp6940:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp6941:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp6942:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp6943:
	.cfi_def_cfa_offset 48
.Ltmp6944:
	.cfi_offset %rbx, -40
.Ltmp6945:
	.cfi_offset %r12, -32
.Ltmp6946:
	.cfi_offset %r14, -24
.Ltmp6947:
	.cfi_offset %r15, -16
	movq	%rsi, %r14
	movq	$0, (%rsp)
	movabsq	$.L.str1, %rdx
	leaq	(%rsp), %r8
	xorq	%rdi, %rdi
	xorq	%rsi, %rsi
	movq	$4, %rcx
	xorq	%rbx, %rbx
	callq	peep2_find_free_register
	testq	%rax, %rax
	movq	%rax, (%r14)
	je	.LBB734_3
# BB#1:                                 # %if.end
	movabsq	$.L.str1, %rdx
	leaq	(%rsp), %r8
	xorq	%rdi, %rdi
	xorq	%rsi, %rsi
	movq	$4, %rcx
	xorq	%rbx, %rbx
	callq	peep2_find_free_register
	testq	%rax, %rax
	movq	%rax, 8(%r14)
	je	.LBB734_3
# BB#2:                                 # %if.end5
	callq	start_sequence
	movq	(%r14), %rbx
	movq	8(%r14), %r14
	movq	$4, %rdi
	movq	$7, %rsi
	callq	gen_rtx_REG
	movq	$4, %rdi
	movq	%rax, %rsi
	callq	gen_rtx_MEM
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %r15
	movq	$4, %rdi
	movq	$7, %rsi
	callq	gen_rtx_REG
	movq	%rax, %r12
	movq	$4, %rdi
	movq	$7, %rsi
	callq	gen_rtx_REG
	movq	%rax, %rbx
	xorq	%rdi, %rdi
	movq	$4, %rsi
	callq	gen_rtx_CONST_INT
	movq	$75, %rdi
	movq	$4, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r12, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rbx
	movq	$62, %rdi
	xorq	%rsi, %rsi
	callq	gen_rtx_fmt_0
	movq	$51, %rdi
	movq	%rax, %rsi
	callq	gen_rtx_MEM
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rcx
	movq	$3, %rdi
	xorq	%rax, %rax
	movq	%r15, %rsi
	movq	%rbx, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_E
	movq	%rax, %rdi
	callq	emit
	movq	$4, %rdi
	movq	$7, %rsi
	callq	gen_rtx_REG
	movq	$4, %rdi
	movq	%rax, %rsi
	callq	gen_rtx_MEM
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %r14
	movq	$4, %rdi
	movq	$7, %rsi
	callq	gen_rtx_REG
	movq	%rax, %r15
	movq	$4, %rdi
	movq	$7, %rsi
	callq	gen_rtx_REG
	movq	%rax, %rbx
	xorq	%rdi, %rdi
	movq	$4, %rsi
	callq	gen_rtx_CONST_INT
	movq	$75, %rdi
	movq	$4, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r15, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rcx
	movq	$2, %rdi
	xorq	%rax, %rax
	movq	%r14, %rsi
	movq	%rcx, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_E
	movq	%rax, %rdi
	callq	emit
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
.LBB734_3:                              # %return
	movq	%rbx, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.Ltmp6948:
	.size	gen_peephole2_1278, .Ltmp6948-gen_peephole2_1278
	.cfi_endproc

	.globl	gen_peephole2_1279
	.align	16, 0x90
	.type	gen_peephole2_1279,@function
gen_peephole2_1279:                     # @gen_peephole2_1279
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp6954:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp6955:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp6956:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp6957:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp6958:
	.cfi_def_cfa_offset 48
.Ltmp6959:
	.cfi_offset %rbx, -40
.Ltmp6960:
	.cfi_offset %r12, -32
.Ltmp6961:
	.cfi_offset %r14, -24
.Ltmp6962:
	.cfi_offset %r15, -16
	movq	%rsi, %r14
	movq	$0, (%rsp)
	movabsq	$.L.str1, %rdx
	leaq	(%rsp), %r8
	xorq	%rdi, %rdi
	xorq	%rsi, %rsi
	movq	$4, %rcx
	xorq	%rbx, %rbx
	callq	peep2_find_free_register
	testq	%rax, %rax
	movq	%rax, (%r14)
	je	.LBB735_2
# BB#1:                                 # %if.end
	callq	start_sequence
	movq	(%r14), %r15
	movq	$4, %rdi
	movq	$7, %rsi
	callq	gen_rtx_REG
	movq	$4, %rdi
	movq	%rax, %rsi
	callq	gen_rtx_MEM
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r15, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %r14
	movq	$4, %rdi
	movq	$7, %rsi
	callq	gen_rtx_REG
	movq	%rax, %r12
	movq	$4, %rdi
	movq	$7, %rsi
	callq	gen_rtx_REG
	movq	%rax, %rbx
	xorq	%rdi, %rdi
	movq	$4, %rsi
	callq	gen_rtx_CONST_INT
	movq	$75, %rdi
	movq	$4, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r12, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rbx
	movq	$62, %rdi
	xorq	%rsi, %rsi
	callq	gen_rtx_fmt_0
	movq	$51, %rdi
	movq	%rax, %rsi
	callq	gen_rtx_MEM
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rcx
	movq	$3, %rdi
	xorq	%rax, %rax
	movq	%r14, %rsi
	movq	%rbx, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_E
	movq	%rax, %rdi
	callq	emit
	movq	%r15, %rdi
	callq	copy_rtx
	movq	%rax, %rbx
	movq	$4, %rdi
	movq	$7, %rsi
	callq	gen_rtx_REG
	movq	$4, %rdi
	movq	%rax, %rsi
	callq	gen_rtx_MEM
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %r14
	movq	$4, %rdi
	movq	$7, %rsi
	callq	gen_rtx_REG
	movq	%rax, %r15
	movq	$4, %rdi
	movq	$7, %rsi
	callq	gen_rtx_REG
	movq	%rax, %rbx
	xorq	%rdi, %rdi
	movq	$4, %rsi
	callq	gen_rtx_CONST_INT
	movq	$75, %rdi
	movq	$4, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r15, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rcx
	movq	$2, %rdi
	xorq	%rax, %rax
	movq	%r14, %rsi
	movq	%rcx, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_E
	movq	%rax, %rdi
	callq	emit
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
.LBB735_2:                              # %return
	movq	%rbx, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.Ltmp6963:
	.size	gen_peephole2_1279, .Ltmp6963-gen_peephole2_1279
	.cfi_endproc

	.globl	gen_peephole2_1280
	.align	16, 0x90
	.type	gen_peephole2_1280,@function
gen_peephole2_1280:                     # @gen_peephole2_1280
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp6968:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp6969:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp6970:
	.cfi_def_cfa_offset 32
	subq	$16, %rsp
.Ltmp6971:
	.cfi_def_cfa_offset 48
.Ltmp6972:
	.cfi_offset %rbx, -32
.Ltmp6973:
	.cfi_offset %r14, -24
.Ltmp6974:
	.cfi_offset %r15, -16
	movq	%rsi, %r14
	movq	$0, 8(%rsp)
	movabsq	$.L.str1, %rdx
	leaq	8(%rsp), %r8
	xorq	%rdi, %rdi
	xorq	%rsi, %rsi
	movq	$4, %rcx
	xorq	%rbx, %rbx
	callq	peep2_find_free_register
	testq	%rax, %rax
	movq	%rax, (%r14)
	je	.LBB736_2
# BB#1:                                 # %if.end
	callq	start_sequence
	movq	(%r14), %rbx
	movq	$4, %rdi
	movq	$7, %rsi
	callq	gen_rtx_REG
	movq	$4, %rdi
	movq	%rax, %rsi
	callq	gen_rtx_MEM
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %r14
	movq	$4, %rdi
	movq	$7, %rsi
	callq	gen_rtx_REG
	movq	%rax, %r15
	movq	$4, %rdi
	movq	$7, %rsi
	callq	gen_rtx_REG
	movq	%rax, %rbx
	xorq	%rdi, %rdi
	movq	$4, %rsi
	callq	gen_rtx_CONST_INT
	movq	$75, %rdi
	movq	$4, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r15, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rcx
	movq	$2, %rdi
	xorq	%rax, %rax
	movq	%r14, %rsi
	movq	%rcx, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_E
	movq	%rax, %rdi
	callq	emit
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
.LBB736_2:                              # %return
	movq	%rbx, %rax
	addq	$16, %rsp
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp6975:
	.size	gen_peephole2_1280, .Ltmp6975-gen_peephole2_1280
	.cfi_endproc

	.globl	gen_peephole2_1281
	.align	16, 0x90
	.type	gen_peephole2_1281,@function
gen_peephole2_1281:                     # @gen_peephole2_1281
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp6981:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp6982:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp6983:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp6984:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp6985:
	.cfi_def_cfa_offset 48
.Ltmp6986:
	.cfi_offset %rbx, -40
.Ltmp6987:
	.cfi_offset %r12, -32
.Ltmp6988:
	.cfi_offset %r14, -24
.Ltmp6989:
	.cfi_offset %r15, -16
	movq	%rsi, %r14
	movq	$0, (%rsp)
	movabsq	$.L.str1, %rdx
	leaq	(%rsp), %r8
	xorq	%rdi, %rdi
	xorq	%rsi, %rsi
	movq	$4, %rcx
	xorq	%rbx, %rbx
	callq	peep2_find_free_register
	testq	%rax, %rax
	movq	%rax, (%r14)
	je	.LBB737_3
# BB#1:                                 # %if.end
	movabsq	$.L.str1, %rdx
	leaq	(%rsp), %r8
	xorq	%rdi, %rdi
	xorq	%rsi, %rsi
	movq	$4, %rcx
	xorq	%rbx, %rbx
	callq	peep2_find_free_register
	testq	%rax, %rax
	movq	%rax, 8(%r14)
	je	.LBB737_3
# BB#2:                                 # %if.end5
	callq	start_sequence
	movq	(%r14), %rbx
	movq	8(%r14), %r14
	movq	$4, %rdi
	movq	$7, %rsi
	callq	gen_rtx_REG
	movq	$4, %rdi
	movq	%rax, %rsi
	callq	gen_rtx_MEM
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %r15
	movq	$4, %rdi
	movq	$7, %rsi
	callq	gen_rtx_REG
	movq	%rax, %r12
	movq	$4, %rdi
	movq	$7, %rsi
	callq	gen_rtx_REG
	movq	%rax, %rbx
	xorq	%rdi, %rdi
	movq	$4, %rsi
	callq	gen_rtx_CONST_INT
	movq	$75, %rdi
	movq	$4, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r12, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rcx
	movq	$2, %rdi
	xorq	%rax, %rax
	movq	%r15, %rsi
	movq	%rcx, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_E
	movq	%rax, %rdi
	callq	emit
	movq	$4, %rdi
	movq	$7, %rsi
	callq	gen_rtx_REG
	movq	$4, %rdi
	movq	%rax, %rsi
	callq	gen_rtx_MEM
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %r14
	movq	$4, %rdi
	movq	$7, %rsi
	callq	gen_rtx_REG
	movq	%rax, %r15
	movq	$4, %rdi
	movq	$7, %rsi
	callq	gen_rtx_REG
	movq	%rax, %rbx
	xorq	%rdi, %rdi
	movq	$4, %rsi
	callq	gen_rtx_CONST_INT
	movq	$75, %rdi
	movq	$4, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r15, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rcx
	movq	$2, %rdi
	xorq	%rax, %rax
	movq	%r14, %rsi
	movq	%rcx, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_E
	movq	%rax, %rdi
	callq	emit
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
.LBB737_3:                              # %return
	movq	%rbx, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.Ltmp6990:
	.size	gen_peephole2_1281, .Ltmp6990-gen_peephole2_1281
	.cfi_endproc

	.globl	gen_peephole2_1282
	.align	16, 0x90
	.type	gen_peephole2_1282,@function
gen_peephole2_1282:                     # @gen_peephole2_1282
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp6996:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp6997:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp6998:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp6999:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp7000:
	.cfi_def_cfa_offset 48
.Ltmp7001:
	.cfi_offset %rbx, -40
.Ltmp7002:
	.cfi_offset %r12, -32
.Ltmp7003:
	.cfi_offset %r14, -24
.Ltmp7004:
	.cfi_offset %r15, -16
	movq	%rsi, %r14
	movq	$0, (%rsp)
	movabsq	$.L.str1, %rdx
	leaq	(%rsp), %r8
	xorq	%rdi, %rdi
	xorq	%rsi, %rsi
	movq	$4, %rcx
	xorq	%rbx, %rbx
	callq	peep2_find_free_register
	testq	%rax, %rax
	movq	%rax, (%r14)
	je	.LBB738_2
# BB#1:                                 # %if.end
	callq	start_sequence
	movq	(%r14), %r14
	movq	$4, %rdi
	movq	$7, %rsi
	callq	gen_rtx_REG
	movq	$4, %rdi
	movq	%rax, %rsi
	callq	gen_rtx_MEM
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %r15
	movq	$4, %rdi
	movq	$7, %rsi
	callq	gen_rtx_REG
	movq	%rax, %r12
	movq	$4, %rdi
	movq	$7, %rsi
	callq	gen_rtx_REG
	movq	%rax, %rbx
	xorq	%rdi, %rdi
	movq	$4, %rsi
	callq	gen_rtx_CONST_INT
	movq	$75, %rdi
	movq	$4, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r12, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rcx
	movq	$2, %rdi
	xorq	%rax, %rax
	movq	%r15, %rsi
	movq	%rcx, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_E
	movq	%rax, %rdi
	callq	emit
	movq	%r14, %rdi
	callq	copy_rtx
	movq	%rax, %rbx
	movq	$4, %rdi
	movq	$7, %rsi
	callq	gen_rtx_REG
	movq	$4, %rdi
	movq	%rax, %rsi
	callq	gen_rtx_MEM
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %r14
	movq	$4, %rdi
	movq	$7, %rsi
	callq	gen_rtx_REG
	movq	%rax, %r15
	movq	$4, %rdi
	movq	$7, %rsi
	callq	gen_rtx_REG
	movq	%rax, %rbx
	xorq	%rdi, %rdi
	movq	$4, %rsi
	callq	gen_rtx_CONST_INT
	movq	$75, %rdi
	movq	$4, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r15, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rcx
	movq	$2, %rdi
	xorq	%rax, %rax
	movq	%r14, %rsi
	movq	%rcx, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_E
	movq	%rax, %rdi
	callq	emit
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
.LBB738_2:                              # %return
	movq	%rbx, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.Ltmp7005:
	.size	gen_peephole2_1282, .Ltmp7005-gen_peephole2_1282
	.cfi_endproc

	.globl	gen_peephole2_1283
	.align	16, 0x90
	.type	gen_peephole2_1283,@function
gen_peephole2_1283:                     # @gen_peephole2_1283
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp7010:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp7011:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp7012:
	.cfi_def_cfa_offset 32
.Ltmp7013:
	.cfi_offset %rbx, -32
.Ltmp7014:
	.cfi_offset %r14, -24
.Ltmp7015:
	.cfi_offset %r15, -16
	movq	%rsi, %rbx
	callq	start_sequence
	movq	(%rbx), %r14
	movq	8(%rbx), %rbx
	movq	$53, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	%rax, %r15
	movq	$74, %rdi
	movq	$53, %rsi
	movq	%r14, %rdx
	movq	%rbx, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r15, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rbx
	movq	%r14, %rdi
	callq	copy_rtx
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rcx
	movq	$2, %rdi
	xorq	%rax, %rax
	movq	%rbx, %rsi
	movq	%rcx, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_E
	movq	%rax, %rdi
	callq	emit
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp7016:
	.size	gen_peephole2_1283, .Ltmp7016-gen_peephole2_1283
	.cfi_endproc

	.globl	gen_peephole2_1284
	.align	16, 0x90
	.type	gen_peephole2_1284,@function
gen_peephole2_1284:                     # @gen_peephole2_1284
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp7021:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp7022:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp7023:
	.cfi_def_cfa_offset 32
.Ltmp7024:
	.cfi_offset %rbx, -32
.Ltmp7025:
	.cfi_offset %r14, -24
.Ltmp7026:
	.cfi_offset %r15, -16
	movq	%rsi, %rbx
	callq	start_sequence
	movq	(%rbx), %r14
	movq	8(%rbx), %rbx
	movq	$53, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	%rax, %r15
	movq	$74, %rdi
	movq	$53, %rsi
	movq	%r14, %rdx
	movq	%rbx, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r15, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rbx
	movq	%r14, %rdi
	callq	copy_rtx
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rcx
	movq	$2, %rdi
	xorq	%rax, %rax
	movq	%rbx, %rsi
	movq	%rcx, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_E
	movq	%rax, %rdi
	callq	emit
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp7027:
	.size	gen_peephole2_1284, .Ltmp7027-gen_peephole2_1284
	.cfi_endproc

	.globl	gen_peephole2_1285
	.align	16, 0x90
	.type	gen_peephole2_1285,@function
gen_peephole2_1285:                     # @gen_peephole2_1285
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp7032:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp7033:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp7034:
	.cfi_def_cfa_offset 32
.Ltmp7035:
	.cfi_offset %rbx, -32
.Ltmp7036:
	.cfi_offset %r14, -24
.Ltmp7037:
	.cfi_offset %r15, -16
	movq	%rsi, %rbx
	callq	start_sequence
	movq	(%rbx), %r14
	movq	8(%rbx), %rbx
	movq	$53, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	%rax, %r15
	movq	$74, %rdi
	movq	$53, %rsi
	movq	%r14, %rdx
	movq	%rbx, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r15, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rbx
	movq	%r14, %rdi
	callq	copy_rtx
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rcx
	movq	$2, %rdi
	xorq	%rax, %rax
	movq	%rbx, %rsi
	movq	%rcx, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_E
	movq	%rax, %rdi
	callq	emit
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp7038:
	.size	gen_peephole2_1285, .Ltmp7038-gen_peephole2_1285
	.cfi_endproc

	.globl	gen_peephole2_1286
	.align	16, 0x90
	.type	gen_peephole2_1286,@function
gen_peephole2_1286:                     # @gen_peephole2_1286
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp7042:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp7043:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp7044:
	.cfi_def_cfa_offset 32
.Ltmp7045:
	.cfi_offset %rbx, -24
.Ltmp7046:
	.cfi_offset %r14, -16
	movq	%rsi, %rbx
	callq	start_sequence
	movq	(%rbx), %rbx
	movq	$53, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	%rax, %r14
	xorq	%rdi, %rdi
	movq	$128, %rsi
	callq	gen_rtx_CONST_INT
	movq	$74, %rdi
	movq	$53, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %r14
	movq	%rbx, %rdi
	callq	copy_rtx
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rcx
	movq	$2, %rdi
	xorq	%rax, %rax
	movq	%r14, %rsi
	movq	%rcx, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_E
	movq	%rax, %rdi
	callq	emit
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp7047:
	.size	gen_peephole2_1286, .Ltmp7047-gen_peephole2_1286
	.cfi_endproc

	.globl	gen_peephole2_1287
	.align	16, 0x90
	.type	gen_peephole2_1287,@function
gen_peephole2_1287:                     # @gen_peephole2_1287
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp7051:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp7052:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp7053:
	.cfi_def_cfa_offset 32
.Ltmp7054:
	.cfi_offset %rbx, -24
.Ltmp7055:
	.cfi_offset %r14, -16
	movq	%rsi, %rbx
	callq	start_sequence
	movq	(%rbx), %rbx
	movq	$53, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	%rax, %r14
	xorq	%rdi, %rdi
	movq	$128, %rsi
	callq	gen_rtx_CONST_INT
	movq	$74, %rdi
	movq	$53, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %r14
	movq	%rbx, %rdi
	callq	copy_rtx
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rcx
	movq	$2, %rdi
	xorq	%rax, %rax
	movq	%r14, %rsi
	movq	%rcx, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_E
	movq	%rax, %rdi
	callq	emit
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp7056:
	.size	gen_peephole2_1287, .Ltmp7056-gen_peephole2_1287
	.cfi_endproc

	.globl	gen_peephole2_1288
	.align	16, 0x90
	.type	gen_peephole2_1288,@function
gen_peephole2_1288:                     # @gen_peephole2_1288
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp7060:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp7061:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp7062:
	.cfi_def_cfa_offset 32
.Ltmp7063:
	.cfi_offset %rbx, -24
.Ltmp7064:
	.cfi_offset %r14, -16
	movq	%rsi, %r14
	movq	$0, (%rsp)
	movabsq	$.L.str1, %rdx
	leaq	(%rsp), %r8
	xorq	%rdi, %rdi
	xorq	%rsi, %rsi
	movq	$5, %rcx
	xorq	%rbx, %rbx
	callq	peep2_find_free_register
	testq	%rax, %rax
	movq	%rax, (%r14)
	je	.LBB744_2
# BB#1:                                 # %if.end
	callq	start_sequence
	movq	(%r14), %r14
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rdi
	callq	emit_insn
	movq	$5, %rdi
	movq	$7, %rsi
	callq	gen_rtx_REG
	movq	$96, %rdi
	movq	$5, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	$5, %rdi
	movq	%rax, %rsi
	callq	gen_rtx_MEM
	movq	%rax, %rbx
	movq	%r14, %rdi
	callq	copy_rtx
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rbx
	movq	$62, %rdi
	xorq	%rsi, %rsi
	callq	gen_rtx_fmt_0
	movq	$51, %rdi
	movq	%rax, %rsi
	callq	gen_rtx_MEM
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rcx
	movq	$2, %rdi
	xorq	%rax, %rax
	movq	%rbx, %rsi
	movq	%rcx, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_E
	movq	%rax, %rdi
	callq	emit
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
.LBB744_2:                              # %return
	movq	%rbx, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp7065:
	.size	gen_peephole2_1288, .Ltmp7065-gen_peephole2_1288
	.cfi_endproc

	.globl	gen_peephole2_1289
	.align	16, 0x90
	.type	gen_peephole2_1289,@function
gen_peephole2_1289:                     # @gen_peephole2_1289
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp7069:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp7070:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp7071:
	.cfi_def_cfa_offset 32
.Ltmp7072:
	.cfi_offset %rbx, -24
.Ltmp7073:
	.cfi_offset %r14, -16
	movq	%rsi, %r14
	movq	$0, (%rsp)
	movabsq	$.L.str1, %rdx
	leaq	(%rsp), %r8
	xorq	%rdi, %rdi
	xorq	%rsi, %rsi
	movq	$5, %rcx
	xorq	%rbx, %rbx
	callq	peep2_find_free_register
	testq	%rax, %rax
	movq	%rax, (%r14)
	je	.LBB745_2
# BB#1:                                 # %if.end
	callq	start_sequence
	movq	(%r14), %r14
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rdi
	callq	emit_insn
	movq	$5, %rdi
	movq	$7, %rsi
	callq	gen_rtx_REG
	movq	$96, %rdi
	movq	$5, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	$5, %rdi
	movq	%rax, %rsi
	callq	gen_rtx_MEM
	movq	%rax, %rbx
	movq	%r14, %rdi
	callq	copy_rtx
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	movq	$5, %rdi
	movq	$7, %rsi
	callq	gen_rtx_REG
	movq	$96, %rdi
	movq	$5, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	$5, %rdi
	movq	%rax, %rsi
	callq	gen_rtx_MEM
	movq	%rax, %rbx
	movq	%r14, %rdi
	callq	copy_rtx
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rbx
	movq	$62, %rdi
	xorq	%rsi, %rsi
	callq	gen_rtx_fmt_0
	movq	$51, %rdi
	movq	%rax, %rsi
	callq	gen_rtx_MEM
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rcx
	movq	$2, %rdi
	xorq	%rax, %rax
	movq	%rbx, %rsi
	movq	%rcx, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_E
	movq	%rax, %rdi
	callq	emit
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
.LBB745_2:                              # %return
	movq	%rbx, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp7074:
	.size	gen_peephole2_1289, .Ltmp7074-gen_peephole2_1289
	.cfi_endproc

	.globl	gen_peephole2_1290
	.align	16, 0x90
	.type	gen_peephole2_1290,@function
gen_peephole2_1290:                     # @gen_peephole2_1290
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp7078:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp7079:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp7080:
	.cfi_def_cfa_offset 32
.Ltmp7081:
	.cfi_offset %rbx, -24
.Ltmp7082:
	.cfi_offset %r14, -16
	movq	%rsi, %r14
	movq	$0, (%rsp)
	movabsq	$.L.str1, %rdx
	leaq	(%rsp), %r8
	xorq	%rdi, %rdi
	xorq	%rsi, %rsi
	movq	$5, %rcx
	xorq	%rbx, %rbx
	callq	peep2_find_free_register
	testq	%rax, %rax
	movq	%rax, (%r14)
	je	.LBB746_2
# BB#1:                                 # %if.end
	callq	start_sequence
	movq	(%r14), %r14
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rdi
	callq	emit_insn
	movq	$5, %rdi
	movq	$7, %rsi
	callq	gen_rtx_REG
	movq	$96, %rdi
	movq	$5, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	$5, %rdi
	movq	%rax, %rsi
	callq	gen_rtx_MEM
	movq	%rax, %rbx
	movq	%r14, %rdi
	callq	copy_rtx
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
.LBB746_2:                              # %return
	movq	%rbx, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp7083:
	.size	gen_peephole2_1290, .Ltmp7083-gen_peephole2_1290
	.cfi_endproc

	.globl	gen_peephole2_1291
	.align	16, 0x90
	.type	gen_peephole2_1291,@function
gen_peephole2_1291:                     # @gen_peephole2_1291
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp7087:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp7088:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp7089:
	.cfi_def_cfa_offset 32
.Ltmp7090:
	.cfi_offset %rbx, -24
.Ltmp7091:
	.cfi_offset %r14, -16
	movq	%rsi, %r14
	movq	$0, (%rsp)
	movabsq	$.L.str1, %rdx
	leaq	(%rsp), %r8
	xorq	%rdi, %rdi
	xorq	%rsi, %rsi
	movq	$5, %rcx
	xorq	%rbx, %rbx
	callq	peep2_find_free_register
	testq	%rax, %rax
	movq	%rax, (%r14)
	je	.LBB747_2
# BB#1:                                 # %if.end
	callq	start_sequence
	movq	(%r14), %r14
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rdi
	callq	emit_insn
	movq	$5, %rdi
	movq	$7, %rsi
	callq	gen_rtx_REG
	movq	$96, %rdi
	movq	$5, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	$5, %rdi
	movq	%rax, %rsi
	callq	gen_rtx_MEM
	movq	%rax, %rbx
	movq	%r14, %rdi
	callq	copy_rtx
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	movq	$5, %rdi
	movq	$7, %rsi
	callq	gen_rtx_REG
	movq	$96, %rdi
	movq	$5, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	$5, %rdi
	movq	%rax, %rsi
	callq	gen_rtx_MEM
	movq	%rax, %rbx
	movq	%r14, %rdi
	callq	copy_rtx
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
.LBB747_2:                              # %return
	movq	%rbx, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp7092:
	.size	gen_peephole2_1291, .Ltmp7092-gen_peephole2_1291
	.cfi_endproc

	.globl	gen_peephole2_1292
	.align	16, 0x90
	.type	gen_peephole2_1292,@function
gen_peephole2_1292:                     # @gen_peephole2_1292
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp7097:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp7098:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp7099:
	.cfi_def_cfa_offset 32
	subq	$16, %rsp
.Ltmp7100:
	.cfi_def_cfa_offset 48
.Ltmp7101:
	.cfi_offset %rbx, -32
.Ltmp7102:
	.cfi_offset %r14, -24
.Ltmp7103:
	.cfi_offset %r15, -16
	movq	%rsi, %r14
	movq	$0, 8(%rsp)
	movabsq	$.L.str1, %rdx
	leaq	8(%rsp), %r8
	xorq	%rdi, %rdi
	xorq	%rsi, %rsi
	movq	$5, %rcx
	xorq	%rbx, %rbx
	callq	peep2_find_free_register
	testq	%rax, %rax
	movq	%rax, (%r14)
	je	.LBB748_2
# BB#1:                                 # %if.end
	callq	start_sequence
	movq	(%r14), %rbx
	movq	$5, %rdi
	movq	$7, %rsi
	callq	gen_rtx_REG
	movq	$5, %rdi
	movq	%rax, %rsi
	callq	gen_rtx_MEM
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %r14
	movq	$5, %rdi
	movq	$7, %rsi
	callq	gen_rtx_REG
	movq	%rax, %r15
	movq	$5, %rdi
	movq	$7, %rsi
	callq	gen_rtx_REG
	movq	%rax, %rbx
	xorq	%rdi, %rdi
	movq	$8, %rsi
	callq	gen_rtx_CONST_INT
	movq	$75, %rdi
	movq	$5, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r15, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rbx
	movq	$62, %rdi
	xorq	%rsi, %rsi
	callq	gen_rtx_fmt_0
	movq	$51, %rdi
	movq	%rax, %rsi
	callq	gen_rtx_MEM
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rcx
	movq	$3, %rdi
	xorq	%rax, %rax
	movq	%r14, %rsi
	movq	%rbx, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_E
	movq	%rax, %rdi
	callq	emit
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
.LBB748_2:                              # %return
	movq	%rbx, %rax
	addq	$16, %rsp
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp7104:
	.size	gen_peephole2_1292, .Ltmp7104-gen_peephole2_1292
	.cfi_endproc

	.globl	gen_peephole2_1293
	.align	16, 0x90
	.type	gen_peephole2_1293,@function
gen_peephole2_1293:                     # @gen_peephole2_1293
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp7110:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp7111:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp7112:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp7113:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp7114:
	.cfi_def_cfa_offset 48
.Ltmp7115:
	.cfi_offset %rbx, -40
.Ltmp7116:
	.cfi_offset %r12, -32
.Ltmp7117:
	.cfi_offset %r14, -24
.Ltmp7118:
	.cfi_offset %r15, -16
	movq	%rsi, %r14
	movq	$0, (%rsp)
	movabsq	$.L.str1, %rdx
	leaq	(%rsp), %r8
	xorq	%rdi, %rdi
	xorq	%rsi, %rsi
	movq	$5, %rcx
	xorq	%rbx, %rbx
	callq	peep2_find_free_register
	testq	%rax, %rax
	movq	%rax, (%r14)
	je	.LBB749_3
# BB#1:                                 # %if.end
	movabsq	$.L.str1, %rdx
	leaq	(%rsp), %r8
	xorq	%rdi, %rdi
	xorq	%rsi, %rsi
	movq	$5, %rcx
	xorq	%rbx, %rbx
	callq	peep2_find_free_register
	testq	%rax, %rax
	movq	%rax, 8(%r14)
	je	.LBB749_3
# BB#2:                                 # %if.end5
	callq	start_sequence
	movq	(%r14), %rbx
	movq	8(%r14), %r14
	movq	$5, %rdi
	movq	$7, %rsi
	callq	gen_rtx_REG
	movq	$5, %rdi
	movq	%rax, %rsi
	callq	gen_rtx_MEM
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %r15
	movq	$5, %rdi
	movq	$7, %rsi
	callq	gen_rtx_REG
	movq	%rax, %r12
	movq	$5, %rdi
	movq	$7, %rsi
	callq	gen_rtx_REG
	movq	%rax, %rbx
	xorq	%rdi, %rdi
	movq	$8, %rsi
	callq	gen_rtx_CONST_INT
	movq	$75, %rdi
	movq	$5, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r12, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rbx
	movq	$62, %rdi
	xorq	%rsi, %rsi
	callq	gen_rtx_fmt_0
	movq	$51, %rdi
	movq	%rax, %rsi
	callq	gen_rtx_MEM
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rcx
	movq	$3, %rdi
	xorq	%rax, %rax
	movq	%r15, %rsi
	movq	%rbx, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_E
	movq	%rax, %rdi
	callq	emit
	movq	$5, %rdi
	movq	$7, %rsi
	callq	gen_rtx_REG
	movq	$5, %rdi
	movq	%rax, %rsi
	callq	gen_rtx_MEM
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %r14
	movq	$5, %rdi
	movq	$7, %rsi
	callq	gen_rtx_REG
	movq	%rax, %r15
	movq	$5, %rdi
	movq	$7, %rsi
	callq	gen_rtx_REG
	movq	%rax, %rbx
	xorq	%rdi, %rdi
	movq	$8, %rsi
	callq	gen_rtx_CONST_INT
	movq	$75, %rdi
	movq	$5, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r15, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rcx
	movq	$2, %rdi
	xorq	%rax, %rax
	movq	%r14, %rsi
	movq	%rcx, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_E
	movq	%rax, %rdi
	callq	emit
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
.LBB749_3:                              # %return
	movq	%rbx, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.Ltmp7119:
	.size	gen_peephole2_1293, .Ltmp7119-gen_peephole2_1293
	.cfi_endproc

	.globl	gen_peephole2_1294
	.align	16, 0x90
	.type	gen_peephole2_1294,@function
gen_peephole2_1294:                     # @gen_peephole2_1294
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp7125:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp7126:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp7127:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp7128:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp7129:
	.cfi_def_cfa_offset 48
.Ltmp7130:
	.cfi_offset %rbx, -40
.Ltmp7131:
	.cfi_offset %r12, -32
.Ltmp7132:
	.cfi_offset %r14, -24
.Ltmp7133:
	.cfi_offset %r15, -16
	movq	%rsi, %r14
	movq	$0, (%rsp)
	movabsq	$.L.str1, %rdx
	leaq	(%rsp), %r8
	xorq	%rdi, %rdi
	xorq	%rsi, %rsi
	movq	$5, %rcx
	xorq	%rbx, %rbx
	callq	peep2_find_free_register
	testq	%rax, %rax
	movq	%rax, (%r14)
	je	.LBB750_2
# BB#1:                                 # %if.end
	callq	start_sequence
	movq	(%r14), %r15
	movq	$5, %rdi
	movq	$7, %rsi
	callq	gen_rtx_REG
	movq	$5, %rdi
	movq	%rax, %rsi
	callq	gen_rtx_MEM
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r15, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %r14
	movq	$5, %rdi
	movq	$7, %rsi
	callq	gen_rtx_REG
	movq	%rax, %r12
	movq	$5, %rdi
	movq	$7, %rsi
	callq	gen_rtx_REG
	movq	%rax, %rbx
	xorq	%rdi, %rdi
	movq	$8, %rsi
	callq	gen_rtx_CONST_INT
	movq	$75, %rdi
	movq	$5, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r12, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rbx
	movq	$62, %rdi
	xorq	%rsi, %rsi
	callq	gen_rtx_fmt_0
	movq	$51, %rdi
	movq	%rax, %rsi
	callq	gen_rtx_MEM
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rcx
	movq	$3, %rdi
	xorq	%rax, %rax
	movq	%r14, %rsi
	movq	%rbx, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_E
	movq	%rax, %rdi
	callq	emit
	movq	%r15, %rdi
	callq	copy_rtx
	movq	%rax, %rbx
	movq	$5, %rdi
	movq	$7, %rsi
	callq	gen_rtx_REG
	movq	$5, %rdi
	movq	%rax, %rsi
	callq	gen_rtx_MEM
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %r14
	movq	$5, %rdi
	movq	$7, %rsi
	callq	gen_rtx_REG
	movq	%rax, %r15
	movq	$5, %rdi
	movq	$7, %rsi
	callq	gen_rtx_REG
	movq	%rax, %rbx
	xorq	%rdi, %rdi
	movq	$8, %rsi
	callq	gen_rtx_CONST_INT
	movq	$75, %rdi
	movq	$5, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r15, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rcx
	movq	$2, %rdi
	xorq	%rax, %rax
	movq	%r14, %rsi
	movq	%rcx, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_E
	movq	%rax, %rdi
	callq	emit
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
.LBB750_2:                              # %return
	movq	%rbx, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.Ltmp7134:
	.size	gen_peephole2_1294, .Ltmp7134-gen_peephole2_1294
	.cfi_endproc

	.globl	gen_peephole2_1295
	.align	16, 0x90
	.type	gen_peephole2_1295,@function
gen_peephole2_1295:                     # @gen_peephole2_1295
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp7139:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp7140:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp7141:
	.cfi_def_cfa_offset 32
	subq	$16, %rsp
.Ltmp7142:
	.cfi_def_cfa_offset 48
.Ltmp7143:
	.cfi_offset %rbx, -32
.Ltmp7144:
	.cfi_offset %r14, -24
.Ltmp7145:
	.cfi_offset %r15, -16
	movq	%rsi, %r14
	movq	$0, 8(%rsp)
	movabsq	$.L.str1, %rdx
	leaq	8(%rsp), %r8
	xorq	%rdi, %rdi
	xorq	%rsi, %rsi
	movq	$5, %rcx
	xorq	%rbx, %rbx
	callq	peep2_find_free_register
	testq	%rax, %rax
	movq	%rax, (%r14)
	je	.LBB751_2
# BB#1:                                 # %if.end
	callq	start_sequence
	movq	(%r14), %rbx
	movq	$5, %rdi
	movq	$7, %rsi
	callq	gen_rtx_REG
	movq	$5, %rdi
	movq	%rax, %rsi
	callq	gen_rtx_MEM
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %r14
	movq	$5, %rdi
	movq	$7, %rsi
	callq	gen_rtx_REG
	movq	%rax, %r15
	movq	$5, %rdi
	movq	$7, %rsi
	callq	gen_rtx_REG
	movq	%rax, %rbx
	xorq	%rdi, %rdi
	movq	$8, %rsi
	callq	gen_rtx_CONST_INT
	movq	$75, %rdi
	movq	$5, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r15, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rcx
	movq	$2, %rdi
	xorq	%rax, %rax
	movq	%r14, %rsi
	movq	%rcx, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_E
	movq	%rax, %rdi
	callq	emit
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
.LBB751_2:                              # %return
	movq	%rbx, %rax
	addq	$16, %rsp
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp7146:
	.size	gen_peephole2_1295, .Ltmp7146-gen_peephole2_1295
	.cfi_endproc

	.globl	gen_peephole2_1296
	.align	16, 0x90
	.type	gen_peephole2_1296,@function
gen_peephole2_1296:                     # @gen_peephole2_1296
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp7152:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp7153:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp7154:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp7155:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp7156:
	.cfi_def_cfa_offset 48
.Ltmp7157:
	.cfi_offset %rbx, -40
.Ltmp7158:
	.cfi_offset %r12, -32
.Ltmp7159:
	.cfi_offset %r14, -24
.Ltmp7160:
	.cfi_offset %r15, -16
	movq	%rsi, %r14
	movq	$0, (%rsp)
	movabsq	$.L.str1, %rdx
	leaq	(%rsp), %r8
	xorq	%rdi, %rdi
	xorq	%rsi, %rsi
	movq	$5, %rcx
	xorq	%rbx, %rbx
	callq	peep2_find_free_register
	testq	%rax, %rax
	movq	%rax, (%r14)
	je	.LBB752_3
# BB#1:                                 # %if.end
	movabsq	$.L.str1, %rdx
	leaq	(%rsp), %r8
	xorq	%rdi, %rdi
	xorq	%rsi, %rsi
	movq	$5, %rcx
	xorq	%rbx, %rbx
	callq	peep2_find_free_register
	testq	%rax, %rax
	movq	%rax, 8(%r14)
	je	.LBB752_3
# BB#2:                                 # %if.end5
	callq	start_sequence
	movq	(%r14), %rbx
	movq	8(%r14), %r14
	movq	$5, %rdi
	movq	$7, %rsi
	callq	gen_rtx_REG
	movq	$5, %rdi
	movq	%rax, %rsi
	callq	gen_rtx_MEM
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %r15
	movq	$5, %rdi
	movq	$7, %rsi
	callq	gen_rtx_REG
	movq	%rax, %r12
	movq	$5, %rdi
	movq	$7, %rsi
	callq	gen_rtx_REG
	movq	%rax, %rbx
	xorq	%rdi, %rdi
	movq	$8, %rsi
	callq	gen_rtx_CONST_INT
	movq	$75, %rdi
	movq	$5, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r12, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rcx
	movq	$2, %rdi
	xorq	%rax, %rax
	movq	%r15, %rsi
	movq	%rcx, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_E
	movq	%rax, %rdi
	callq	emit
	movq	$5, %rdi
	movq	$7, %rsi
	callq	gen_rtx_REG
	movq	$5, %rdi
	movq	%rax, %rsi
	callq	gen_rtx_MEM
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %r14
	movq	$5, %rdi
	movq	$7, %rsi
	callq	gen_rtx_REG
	movq	%rax, %r15
	movq	$5, %rdi
	movq	$7, %rsi
	callq	gen_rtx_REG
	movq	%rax, %rbx
	xorq	%rdi, %rdi
	movq	$8, %rsi
	callq	gen_rtx_CONST_INT
	movq	$75, %rdi
	movq	$5, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r15, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rcx
	movq	$2, %rdi
	xorq	%rax, %rax
	movq	%r14, %rsi
	movq	%rcx, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_E
	movq	%rax, %rdi
	callq	emit
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
.LBB752_3:                              # %return
	movq	%rbx, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.Ltmp7161:
	.size	gen_peephole2_1296, .Ltmp7161-gen_peephole2_1296
	.cfi_endproc

	.globl	gen_peephole2_1297
	.align	16, 0x90
	.type	gen_peephole2_1297,@function
gen_peephole2_1297:                     # @gen_peephole2_1297
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp7167:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp7168:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp7169:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp7170:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp7171:
	.cfi_def_cfa_offset 48
.Ltmp7172:
	.cfi_offset %rbx, -40
.Ltmp7173:
	.cfi_offset %r12, -32
.Ltmp7174:
	.cfi_offset %r14, -24
.Ltmp7175:
	.cfi_offset %r15, -16
	movq	%rsi, %r14
	movq	$0, (%rsp)
	movabsq	$.L.str1, %rdx
	leaq	(%rsp), %r8
	xorq	%rdi, %rdi
	xorq	%rsi, %rsi
	movq	$5, %rcx
	xorq	%rbx, %rbx
	callq	peep2_find_free_register
	testq	%rax, %rax
	movq	%rax, (%r14)
	je	.LBB753_2
# BB#1:                                 # %if.end
	callq	start_sequence
	movq	(%r14), %r14
	movq	$5, %rdi
	movq	$7, %rsi
	callq	gen_rtx_REG
	movq	$5, %rdi
	movq	%rax, %rsi
	callq	gen_rtx_MEM
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %r15
	movq	$5, %rdi
	movq	$7, %rsi
	callq	gen_rtx_REG
	movq	%rax, %r12
	movq	$5, %rdi
	movq	$7, %rsi
	callq	gen_rtx_REG
	movq	%rax, %rbx
	xorq	%rdi, %rdi
	movq	$8, %rsi
	callq	gen_rtx_CONST_INT
	movq	$75, %rdi
	movq	$5, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r12, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rcx
	movq	$2, %rdi
	xorq	%rax, %rax
	movq	%r15, %rsi
	movq	%rcx, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_E
	movq	%rax, %rdi
	callq	emit
	movq	%r14, %rdi
	callq	copy_rtx
	movq	%rax, %rbx
	movq	$5, %rdi
	movq	$7, %rsi
	callq	gen_rtx_REG
	movq	$5, %rdi
	movq	%rax, %rsi
	callq	gen_rtx_MEM
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %r14
	movq	$5, %rdi
	movq	$7, %rsi
	callq	gen_rtx_REG
	movq	%rax, %r15
	movq	$5, %rdi
	movq	$7, %rsi
	callq	gen_rtx_REG
	movq	%rax, %rbx
	xorq	%rdi, %rdi
	movq	$8, %rsi
	callq	gen_rtx_CONST_INT
	movq	$75, %rdi
	movq	$5, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r15, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rcx
	movq	$2, %rdi
	xorq	%rax, %rax
	movq	%r14, %rsi
	movq	%rcx, %rdx
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_E
	movq	%rax, %rdi
	callq	emit
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
.LBB753_2:                              # %return
	movq	%rbx, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.Ltmp7176:
	.size	gen_peephole2_1297, .Ltmp7176-gen_peephole2_1297
	.cfi_endproc

	.globl	gen_conditional_trap
	.align	16, 0x90
	.type	gen_conditional_trap,@function
gen_conditional_trap:                   # @gen_conditional_trap
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp7180:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp7181:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp7182:
	.cfi_def_cfa_offset 32
.Ltmp7183:
	.cfi_offset %rbx, -24
.Ltmp7184:
	.cfi_offset %r14, -16
	movq	%rsi, %r14
	movq	%rdi, %rbx
	callq	start_sequence
	movzbl	(%rbx), %edi
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	callq	ix86_expand_compare
	movq	$52, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	movq	%r14, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp7185:
	.size	gen_conditional_trap, .Ltmp7185-gen_conditional_trap
	.cfi_endproc

	.globl	gen_movti
	.align	16, 0x90
	.type	gen_movti,@function
gen_movti:                              # @gen_movti
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp7189:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp7190:
	.cfi_def_cfa_offset 24
	subq	$24, %rsp
.Ltmp7191:
	.cfi_def_cfa_offset 48
.Ltmp7192:
	.cfi_offset %rbx, -24
.Ltmp7193:
	.cfi_offset %r14, -16
	movq	%rsi, %r14
	movq	%rdi, %rbx
	callq	start_sequence
	movzbl	target_flags+3(%rip), %eax
	movq	%rbx, (%rsp)
	movq	%r14, 8(%rsp)
	leaq	(%rsp), %rsi
	movq	$6, %rdi
	testq	$2, %rax
	je	.LBB755_2
# BB#1:                                 # %if.then
	callq	ix86_expand_move
	jmp	.LBB755_3
.LBB755_2:                              # %if.else
	callq	ix86_expand_vector_move
.LBB755_3:                              # %if.end
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	addq	$24, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp7194:
	.size	gen_movti, .Ltmp7194-gen_movti
	.cfi_endproc

	.globl	gen_movv4sf
	.align	16, 0x90
	.type	gen_movv4sf,@function
gen_movv4sf:                            # @gen_movv4sf
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp7198:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp7199:
	.cfi_def_cfa_offset 24
	subq	$24, %rsp
.Ltmp7200:
	.cfi_def_cfa_offset 48
.Ltmp7201:
	.cfi_offset %rbx, -24
.Ltmp7202:
	.cfi_offset %r14, -16
	movq	%rsi, %r14
	movq	%rdi, %rbx
	callq	start_sequence
	movq	%rbx, (%rsp)
	movq	%r14, 8(%rsp)
	leaq	(%rsp), %rsi
	movq	$46, %rdi
	callq	ix86_expand_vector_move
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	addq	$24, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp7203:
	.size	gen_movv4sf, .Ltmp7203-gen_movv4sf
	.cfi_endproc

	.globl	gen_movv4si
	.align	16, 0x90
	.type	gen_movv4si,@function
gen_movv4si:                            # @gen_movv4si
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp7207:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp7208:
	.cfi_def_cfa_offset 24
	subq	$24, %rsp
.Ltmp7209:
	.cfi_def_cfa_offset 48
.Ltmp7210:
	.cfi_offset %rbx, -24
.Ltmp7211:
	.cfi_offset %r14, -16
	movq	%rsi, %r14
	movq	%rdi, %rbx
	callq	start_sequence
	movq	%rbx, (%rsp)
	movq	%r14, 8(%rsp)
	leaq	(%rsp), %rsi
	movq	$37, %rdi
	callq	ix86_expand_vector_move
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	addq	$24, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp7212:
	.size	gen_movv4si, .Ltmp7212-gen_movv4si
	.cfi_endproc

	.globl	gen_movv2si
	.align	16, 0x90
	.type	gen_movv2si,@function
gen_movv2si:                            # @gen_movv2si
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp7216:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp7217:
	.cfi_def_cfa_offset 24
	subq	$24, %rsp
.Ltmp7218:
	.cfi_def_cfa_offset 48
.Ltmp7219:
	.cfi_offset %rbx, -24
.Ltmp7220:
	.cfi_offset %r14, -16
	movq	%rsi, %r14
	movq	%rdi, %rbx
	callq	start_sequence
	movq	%rbx, (%rsp)
	movq	%r14, 8(%rsp)
	leaq	(%rsp), %rsi
	movq	$33, %rdi
	callq	ix86_expand_vector_move
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	addq	$24, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp7221:
	.size	gen_movv2si, .Ltmp7221-gen_movv2si
	.cfi_endproc

	.globl	gen_movv4hi
	.align	16, 0x90
	.type	gen_movv4hi,@function
gen_movv4hi:                            # @gen_movv4hi
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp7225:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp7226:
	.cfi_def_cfa_offset 24
	subq	$24, %rsp
.Ltmp7227:
	.cfi_def_cfa_offset 48
.Ltmp7228:
	.cfi_offset %rbx, -24
.Ltmp7229:
	.cfi_offset %r14, -16
	movq	%rsi, %r14
	movq	%rdi, %rbx
	callq	start_sequence
	movq	%rbx, (%rsp)
	movq	%r14, 8(%rsp)
	leaq	(%rsp), %rsi
	movq	$36, %rdi
	callq	ix86_expand_vector_move
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	addq	$24, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp7230:
	.size	gen_movv4hi, .Ltmp7230-gen_movv4hi
	.cfi_endproc

	.globl	gen_movv8qi
	.align	16, 0x90
	.type	gen_movv8qi,@function
gen_movv8qi:                            # @gen_movv8qi
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp7234:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp7235:
	.cfi_def_cfa_offset 24
	subq	$24, %rsp
.Ltmp7236:
	.cfi_def_cfa_offset 48
.Ltmp7237:
	.cfi_offset %rbx, -24
.Ltmp7238:
	.cfi_offset %r14, -16
	movq	%rsi, %r14
	movq	%rdi, %rbx
	callq	start_sequence
	movq	%rbx, (%rsp)
	movq	%r14, 8(%rsp)
	leaq	(%rsp), %rsi
	movq	$39, %rdi
	callq	ix86_expand_vector_move
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	addq	$24, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp7239:
	.size	gen_movv8qi, .Ltmp7239-gen_movv8qi
	.cfi_endproc

	.globl	gen_movv2sf
	.align	16, 0x90
	.type	gen_movv2sf,@function
gen_movv2sf:                            # @gen_movv2sf
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp7243:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp7244:
	.cfi_def_cfa_offset 24
	subq	$24, %rsp
.Ltmp7245:
	.cfi_def_cfa_offset 48
.Ltmp7246:
	.cfi_offset %rbx, -24
.Ltmp7247:
	.cfi_offset %r14, -16
	movq	%rsi, %r14
	movq	%rdi, %rbx
	callq	start_sequence
	movq	%rbx, (%rsp)
	movq	%r14, 8(%rsp)
	leaq	(%rsp), %rsi
	movq	$44, %rdi
	callq	ix86_expand_vector_move
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	addq	$24, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp7248:
	.size	gen_movv2sf, .Ltmp7248-gen_movv2sf
	.cfi_endproc

	.globl	gen_split_1306
	.align	16, 0x90
	.type	gen_split_1306,@function
gen_split_1306:                         # @gen_split_1306
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp7253:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp7254:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp7255:
	.cfi_def_cfa_offset 32
.Ltmp7256:
	.cfi_offset %rbx, -32
.Ltmp7257:
	.cfi_offset %r14, -24
.Ltmp7258:
	.cfi_offset %r15, -16
	movq	%rdi, %rbx
	callq	start_sequence
	movq	8(%rbx), %r14
	movq	$4, %rdi
	movq	$7, %rsi
	callq	gen_rtx_REG
	movq	%rax, %r15
	movq	$4, %rdi
	movq	$7, %rsi
	callq	gen_rtx_REG
	movq	%rax, %rbx
	xorq	%rdi, %rdi
	movq	$-16, %rsi
	callq	gen_rtx_CONST_INT
	movq	$75, %rdi
	movq	$4, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r15, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	movq	$4, %rdi
	movq	$7, %rsi
	callq	gen_rtx_REG
	movq	$6, %rdi
	movq	%rax, %rsi
	callq	gen_rtx_MEM
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	movq	%r14, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp7259:
	.size	gen_split_1306, .Ltmp7259-gen_split_1306
	.cfi_endproc

	.globl	gen_split_1307
	.align	16, 0x90
	.type	gen_split_1307,@function
gen_split_1307:                         # @gen_split_1307
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp7264:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp7265:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp7266:
	.cfi_def_cfa_offset 32
.Ltmp7267:
	.cfi_offset %rbx, -32
.Ltmp7268:
	.cfi_offset %r14, -24
.Ltmp7269:
	.cfi_offset %r15, -16
	movq	%rdi, %rbx
	callq	start_sequence
	movq	8(%rbx), %r14
	movq	$4, %rdi
	movq	$7, %rsi
	callq	gen_rtx_REG
	movq	%rax, %r15
	movq	$4, %rdi
	movq	$7, %rsi
	callq	gen_rtx_REG
	movq	%rax, %rbx
	xorq	%rdi, %rdi
	movq	$-16, %rsi
	callq	gen_rtx_CONST_INT
	movq	$75, %rdi
	movq	$4, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r15, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	movq	$4, %rdi
	movq	$7, %rsi
	callq	gen_rtx_REG
	movq	$46, %rdi
	movq	%rax, %rsi
	callq	gen_rtx_MEM
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	movq	%r14, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp7270:
	.size	gen_split_1307, .Ltmp7270-gen_split_1307
	.cfi_endproc

	.globl	gen_split_1308
	.align	16, 0x90
	.type	gen_split_1308,@function
gen_split_1308:                         # @gen_split_1308
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp7275:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp7276:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp7277:
	.cfi_def_cfa_offset 32
.Ltmp7278:
	.cfi_offset %rbx, -32
.Ltmp7279:
	.cfi_offset %r14, -24
.Ltmp7280:
	.cfi_offset %r15, -16
	movq	%rdi, %rbx
	callq	start_sequence
	movq	8(%rbx), %r14
	movq	$4, %rdi
	movq	$7, %rsi
	callq	gen_rtx_REG
	movq	%rax, %r15
	movq	$4, %rdi
	movq	$7, %rsi
	callq	gen_rtx_REG
	movq	%rax, %rbx
	xorq	%rdi, %rdi
	movq	$-16, %rsi
	callq	gen_rtx_CONST_INT
	movq	$75, %rdi
	movq	$4, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r15, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	movq	$4, %rdi
	movq	$7, %rsi
	callq	gen_rtx_REG
	movq	$37, %rdi
	movq	%rax, %rsi
	callq	gen_rtx_MEM
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	movq	%r14, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp7281:
	.size	gen_split_1308, .Ltmp7281-gen_split_1308
	.cfi_endproc

	.globl	gen_split_1309
	.align	16, 0x90
	.type	gen_split_1309,@function
gen_split_1309:                         # @gen_split_1309
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp7286:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp7287:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp7288:
	.cfi_def_cfa_offset 32
.Ltmp7289:
	.cfi_offset %rbx, -32
.Ltmp7290:
	.cfi_offset %r14, -24
.Ltmp7291:
	.cfi_offset %r15, -16
	movq	%rdi, %rbx
	callq	start_sequence
	movq	8(%rbx), %r14
	movq	$4, %rdi
	movq	$7, %rsi
	callq	gen_rtx_REG
	movq	%rax, %r15
	movq	$4, %rdi
	movq	$7, %rsi
	callq	gen_rtx_REG
	movq	%rax, %rbx
	xorq	%rdi, %rdi
	movq	$-8, %rsi
	callq	gen_rtx_CONST_INT
	movq	$75, %rdi
	movq	$4, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r15, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	movq	$4, %rdi
	movq	$7, %rsi
	callq	gen_rtx_REG
	movq	$33, %rdi
	movq	%rax, %rsi
	callq	gen_rtx_MEM
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	movq	%r14, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp7292:
	.size	gen_split_1309, .Ltmp7292-gen_split_1309
	.cfi_endproc

	.globl	gen_split_1310
	.align	16, 0x90
	.type	gen_split_1310,@function
gen_split_1310:                         # @gen_split_1310
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp7297:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp7298:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp7299:
	.cfi_def_cfa_offset 32
.Ltmp7300:
	.cfi_offset %rbx, -32
.Ltmp7301:
	.cfi_offset %r14, -24
.Ltmp7302:
	.cfi_offset %r15, -16
	movq	%rdi, %rbx
	callq	start_sequence
	movq	8(%rbx), %r14
	movq	$4, %rdi
	movq	$7, %rsi
	callq	gen_rtx_REG
	movq	%rax, %r15
	movq	$4, %rdi
	movq	$7, %rsi
	callq	gen_rtx_REG
	movq	%rax, %rbx
	xorq	%rdi, %rdi
	movq	$-8, %rsi
	callq	gen_rtx_CONST_INT
	movq	$75, %rdi
	movq	$4, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r15, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	movq	$4, %rdi
	movq	$7, %rsi
	callq	gen_rtx_REG
	movq	$36, %rdi
	movq	%rax, %rsi
	callq	gen_rtx_MEM
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	movq	%r14, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp7303:
	.size	gen_split_1310, .Ltmp7303-gen_split_1310
	.cfi_endproc

	.globl	gen_split_1311
	.align	16, 0x90
	.type	gen_split_1311,@function
gen_split_1311:                         # @gen_split_1311
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp7308:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp7309:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp7310:
	.cfi_def_cfa_offset 32
.Ltmp7311:
	.cfi_offset %rbx, -32
.Ltmp7312:
	.cfi_offset %r14, -24
.Ltmp7313:
	.cfi_offset %r15, -16
	movq	%rdi, %rbx
	callq	start_sequence
	movq	8(%rbx), %r14
	movq	$4, %rdi
	movq	$7, %rsi
	callq	gen_rtx_REG
	movq	%rax, %r15
	movq	$4, %rdi
	movq	$7, %rsi
	callq	gen_rtx_REG
	movq	%rax, %rbx
	xorq	%rdi, %rdi
	movq	$-8, %rsi
	callq	gen_rtx_CONST_INT
	movq	$75, %rdi
	movq	$4, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r15, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	movq	$4, %rdi
	movq	$7, %rsi
	callq	gen_rtx_REG
	movq	$39, %rdi
	movq	%rax, %rsi
	callq	gen_rtx_MEM
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	movq	%r14, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp7314:
	.size	gen_split_1311, .Ltmp7314-gen_split_1311
	.cfi_endproc

	.globl	gen_split_1312
	.align	16, 0x90
	.type	gen_split_1312,@function
gen_split_1312:                         # @gen_split_1312
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp7319:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp7320:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp7321:
	.cfi_def_cfa_offset 32
.Ltmp7322:
	.cfi_offset %rbx, -32
.Ltmp7323:
	.cfi_offset %r14, -24
.Ltmp7324:
	.cfi_offset %r15, -16
	movq	%rdi, %rbx
	callq	start_sequence
	movq	8(%rbx), %r14
	movq	$4, %rdi
	movq	$7, %rsi
	callq	gen_rtx_REG
	movq	%rax, %r15
	movq	$4, %rdi
	movq	$7, %rsi
	callq	gen_rtx_REG
	movq	%rax, %rbx
	xorq	%rdi, %rdi
	movq	$-8, %rsi
	callq	gen_rtx_CONST_INT
	movq	$75, %rdi
	movq	$4, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r15, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	movq	$4, %rdi
	movq	$7, %rsi
	callq	gen_rtx_REG
	movq	$44, %rdi
	movq	%rax, %rsi
	callq	gen_rtx_MEM
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	movq	%r14, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp7325:
	.size	gen_split_1312, .Ltmp7325-gen_split_1312
	.cfi_endproc

	.globl	gen_split_1313
	.align	16, 0x90
	.type	gen_split_1313,@function
gen_split_1313:                         # @gen_split_1313
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp7328:
	.cfi_def_cfa_offset 16
.Ltmp7329:
	.cfi_offset %rbx, -16
	movq	%rdi, %rbx
	callq	start_sequence
	movq	%rbx, %rdi
	callq	ix86_split_long_move
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	popq	%rbx
	retq
.Ltmp7330:
	.size	gen_split_1313, .Ltmp7330-gen_split_1313
	.cfi_endproc

	.globl	gen_sfence
	.align	16, 0x90
	.type	gen_sfence,@function
gen_sfence:                             # @gen_sfence
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp7333:
	.cfi_def_cfa_offset 16
.Ltmp7334:
	.cfi_offset %rbx, -16
	callq	start_sequence
	movl	target_flags(%rip), %esi
	movq	$25, %rcx
	shrq	%cl, %rsi
	andq	$1, %rsi
	orq	$4, %rsi
	movq	$62, %rdi
	callq	gen_rtx_fmt_0
	movq	$51, %rdi
	movq	%rax, %rsi
	callq	gen_rtx_MEM
	movq	%rax, %rbx
	movl	(%rbx), %eax
	orq	$134217728, %rax        # imm = 0x8000000
	movl	%eax, (%rbx)
	movq	$1, %rdi
	xorq	%rax, %rax
	movq	%rbx, %rsi
	callq	gen_rtvec
	movq	$42, %rdi
	movq	$51, %rsi
	movq	$44, %rcx
	movq	%rax, %rdx
	callq	gen_rtx_fmt_Ei
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	popq	%rbx
	retq
.Ltmp7335:
	.size	gen_sfence, .Ltmp7335-gen_sfence
	.cfi_endproc

	.globl	gen_sse_prologue_save
	.align	16, 0x90
	.type	gen_sse_prologue_save,@function
gen_sse_prologue_save:                  # @gen_sse_prologue_save
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp7342:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp7343:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp7344:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp7345:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp7346:
	.cfi_def_cfa_offset 48
	subq	$80, %rsp
.Ltmp7347:
	.cfi_def_cfa_offset 128
.Ltmp7348:
	.cfi_offset %rbx, -48
.Ltmp7349:
	.cfi_offset %r12, -40
.Ltmp7350:
	.cfi_offset %r13, -32
.Ltmp7351:
	.cfi_offset %r14, -24
.Ltmp7352:
	.cfi_offset %r15, -16
	movq	%rcx, 72(%rsp)          # 8-byte Spill
	movq	%rdx, 64(%rsp)          # 8-byte Spill
	movq	%rsi, 56(%rsp)          # 8-byte Spill
	movq	%rdi, 48(%rsp)          # 8-byte Spill
	movq	$5, %rdi
	movq	$21, %rsi
	callq	gen_rtx_REG
	movq	%rax, 40(%rsp)          # 8-byte Spill
	movq	$5, %rdi
	movq	$22, %rsi
	callq	gen_rtx_REG
	movq	%rax, 32(%rsp)          # 8-byte Spill
	movq	$5, %rdi
	movq	$23, %rsi
	callq	gen_rtx_REG
	movq	%rax, %r15
	movq	$5, %rdi
	movq	$24, %rsi
	callq	gen_rtx_REG
	movq	%rax, %r12
	movq	$5, %rdi
	movq	$25, %rsi
	callq	gen_rtx_REG
	movq	%rax, %r13
	movq	$5, %rdi
	movq	$26, %rsi
	callq	gen_rtx_REG
	movq	%rax, %rbx
	movq	$5, %rdi
	movq	$27, %rsi
	callq	gen_rtx_REG
	movq	%rax, %r14
	movq	$5, %rdi
	movq	$28, %rsi
	callq	gen_rtx_REG
	movq	%rax, 16(%rsp)
	movq	%r14, 8(%rsp)
	movq	$8, %rdi
	xorq	%rax, %rax
	movq	%rbx, (%rsp)
	movq	40(%rsp), %rsi          # 8-byte Reload
	movq	32(%rsp), %rdx          # 8-byte Reload
	movq	%r15, %rcx
	movq	%r12, %r8
	movq	%r13, %r9
	callq	gen_rtvec
	movq	$42, %rdi
	movq	$51, %rsi
	movq	$13, %rcx
	movq	%rax, %rdx
	callq	gen_rtx_fmt_Ei
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	48(%rsp), %rdx          # 8-byte Reload
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %r14
	movq	$48, %rdi
	xorq	%rsi, %rsi
	movq	56(%rsp), %rdx          # 8-byte Reload
	callq	gen_rtx_fmt_e
	movq	%rax, %r15
	movq	$48, %rdi
	xorq	%rsi, %rsi
	movq	64(%rsp), %rdx          # 8-byte Reload
	callq	gen_rtx_fmt_e
	movq	%rax, %r12
	movq	$67, %rdi
	movq	$5, %rsi
	movq	72(%rsp), %rdx          # 8-byte Reload
	callq	gen_rtx_fmt_u00
	movq	$48, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rbx
	movq	$4, %rdi
	xorq	%rax, %rax
	movq	%r14, %rsi
	movq	%r15, %rdx
	movq	%r12, %rcx
	movq	%rbx, %r8
	callq	gen_rtvec
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	addq	$80, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	gen_rtx_fmt_E  # TAILCALL
.Ltmp7353:
	.size	gen_sse_prologue_save, .Ltmp7353-gen_sse_prologue_save
	.cfi_endproc

	.globl	gen_prefetch
	.align	16, 0x90
	.type	gen_prefetch,@function
gen_prefetch:                           # @gen_prefetch
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp7358:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp7359:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp7360:
	.cfi_def_cfa_offset 32
.Ltmp7361:
	.cfi_offset %rbx, -32
.Ltmp7362:
	.cfi_offset %r14, -24
.Ltmp7363:
	.cfi_offset %r15, -16
	movq	%rdx, %r15
	movq	%rsi, %rbx
	movq	%rdi, %r14
	callq	start_sequence
	movq	8(%rbx), %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	movq	%rax, %rdx
	andq	%rcx, %rdx
	cmpq	$2, %rdx
	jae	.LBB772_1
# BB#3:                                 # %if.end
	movl	8(%r15), %edx
	cmpq	$4, %rdx
	jae	.LBB772_4
# BB#5:                                 # %if.end18
	movzbl	target_flags+2(%rip), %edx
	testq	$16, %rdx
	je	.LBB772_9
# BB#6:                                 # %land.lhs.true21
	movl	x86_prefetch_sse(%rip), %edx
	testq	%rdx, %rdx
	je	.LBB772_8
# BB#7:                                 # %land.lhs.true21
	testq	%rcx, %rax
	jne	.LBB772_8
.LBB772_9:                              # %if.else
	movq	const_int_rtx+512(%rip), %rbx
	jmp	.LBB772_10
.LBB772_8:                              # %if.then24
	xorq	%rdi, %rdi
	movq	$3, %rsi
	callq	gen_rtx_CONST_INT
	movq	%rax, %r15
.LBB772_10:                             # %if.end27
	movq	$46, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	movq	%rbx, %rcx
	movq	%r15, %r8
	callq	gen_rtx_fmt_eee
	movq	%rax, %rdi
	callq	emit_insn
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.LBB772_1:                              # %if.then
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.gen_prefetch, %rcx
	movq	$19621, %rsi            # imm = 0x4CA5
	jmp	.LBB772_2
.LBB772_4:                              # %if.then17
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.gen_prefetch, %rcx
	movq	$19623, %rsi            # imm = 0x4CA7
.LBB772_2:                              # %if.then
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp7364:
	.size	gen_prefetch, .Ltmp7364-gen_prefetch
	.cfi_endproc

	.globl	add_clobbers
	.align	16, 0x90
	.type	add_clobbers,@function
add_clobbers:                           # @add_clobbers
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp7367:
	.cfi_def_cfa_offset 16
.Ltmp7368:
	.cfi_offset %rbx, -16
	movq	%rdi, %rbx
	movq	$32, %rcx
	movq	%rsi, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$814, %rax              # imm = 0x32E
	jg	.LBB773_7
# BB#1:                                 # %entry
	movq	$32, %rcx
	movq	%rsi, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$800, %rax              # imm = 0x320
	jg	.LBB773_32
# BB#2:                                 # %entry
	movq	$32, %rcx
	movq	%rsi, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$610, %rax              # imm = 0x262
	jg	.LBB773_5
# BB#3:                                 # %entry
	addq	$-43, %rsi
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rsi, %rax
	cmpq	$475, %rax              # imm = 0x1DB
	ja	.LBB773_33
# BB#4:                                 # %entry
	jmpq	*.LJTI773_1(,%rax,8)
.LBB773_19:                             # %sw.bb206
	movq	$62, %rdi
	movq	$4, %rsi
	jmp	.LBB773_10
.LBB773_7:                              # %entry
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %rsi
	cmpq	$815, %rsi              # imm = 0x32F
	je	.LBB773_8
	jmp	.LBB773_33
.LBB773_32:                             # %entry
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %rsi
	cmpq	$801, %rsi              # imm = 0x321
	jne	.LBB773_33
.LBB773_8:                              # %sw.bb
	movq	$17, %rdi
	movq	$8, %rsi
	callq	gen_rtx_REG
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	8(%rbx), %rcx
	movq	%rax, 16(%rcx)
	movq	$17, %rdi
	movq	$9, %rsi
	callq	gen_rtx_REG
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	8(%rbx), %rcx
	movq	%rax, 24(%rcx)
	movq	$17, %rdi
	movq	$10, %rsi
	callq	gen_rtx_REG
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	8(%rbx), %rcx
	movq	%rax, 32(%rcx)
	movq	$17, %rdi
	movq	$11, %rsi
	callq	gen_rtx_REG
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	8(%rbx), %rcx
	movq	%rax, 40(%rcx)
	movq	$17, %rdi
	movq	$12, %rsi
	callq	gen_rtx_REG
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	8(%rbx), %rcx
	movq	%rax, 48(%rcx)
	movq	$17, %rdi
	movq	$13, %rsi
	callq	gen_rtx_REG
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	8(%rbx), %rcx
	movq	%rax, 56(%rcx)
	movq	$17, %rdi
	movq	$14, %rsi
	callq	gen_rtx_REG
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	8(%rbx), %rcx
	movq	%rax, 64(%rcx)
	movq	$17, %rdi
	movq	$15, %rsi
	callq	gen_rtx_REG
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	8(%rbx), %rcx
	movq	%rax, 72(%rcx)
	movq	$5, %rdi
	movq	$29, %rsi
	callq	gen_rtx_REG
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	8(%rbx), %rcx
	movq	%rax, 80(%rcx)
	movq	$5, %rdi
	movq	$30, %rsi
	callq	gen_rtx_REG
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	8(%rbx), %rcx
	movq	%rax, 88(%rcx)
	movq	$5, %rdi
	movq	$31, %rsi
	callq	gen_rtx_REG
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	8(%rbx), %rcx
	movq	%rax, 96(%rcx)
	movq	$5, %rdi
	movq	$32, %rsi
	callq	gen_rtx_REG
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	8(%rbx), %rcx
	movq	%rax, 104(%rcx)
	movq	$5, %rdi
	movq	$33, %rsi
	callq	gen_rtx_REG
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	8(%rbx), %rcx
	movq	%rax, 112(%rcx)
	movq	$5, %rdi
	movq	$34, %rsi
	callq	gen_rtx_REG
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	8(%rbx), %rcx
	movq	%rax, 120(%rcx)
	movq	$5, %rdi
	movq	$35, %rsi
	callq	gen_rtx_REG
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	8(%rbx), %rcx
	movq	%rax, 128(%rcx)
	movq	$5, %rdi
	movq	$36, %rsi
	callq	gen_rtx_REG
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	8(%rbx), %rcx
	movq	%rax, 136(%rcx)
	popq	%rbx
	retq
.LBB773_5:                              # %entry
	addq	$-611, %rsi             # imm = 0xFFFFFFFFFFFFFD9D
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rsi, %rax
	cmpq	$39, %rax
	ja	.LBB773_33
# BB#6:                                 # %entry
	jmpq	*.LJTI773_0(,%rax,8)
.LBB773_31:                             # %sw.bb299
	movq	$52, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
.LBB773_23:                             # %sw.epilog
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	8(%rbx), %rcx
	movq	%rax, 16(%rcx)
	popq	%rbx
	retq
.LBB773_33:                             # %sw.default
	movabsq	$.L.str, %rdi
	movabsq	$.L__FUNCTION__.add_clobbers, %rdx
	movq	$20035, %rsi            # imm = 0x4E43
	callq	fancy_abort
.LBB773_14:                             # %sw.bb123
	movq	$62, %rdi
	movq	$15, %rsi
	jmp	.LBB773_10
.LBB773_9:                              # %sw.bb108
	movq	$62, %rdi
	movq	$16, %rsi
.LBB773_10:                             # %sw.epilog
	callq	gen_rtx_fmt_0
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	8(%rbx), %rcx
	movq	%rax, 16(%rcx)
.LBB773_11:                             # %sw.bb198
	movq	$52, %rdi
.LBB773_12:                             # %sw.epilog
	movq	$17, %rsi
	callq	gen_rtx_REG
.LBB773_13:                             # %sw.epilog
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	8(%rbx), %rcx
	movq	%rax, 24(%rcx)
	popq	%rbx
	retq
.LBB773_16:                             # %sw.bb175
	movq	$57, %rdi
	movq	$18, %rsi
	callq	gen_rtx_REG
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	8(%rbx), %rcx
	movq	%rax, 16(%rcx)
	movq	$57, %rdi
	jmp	.LBB773_12
.LBB773_26:                             # %sw.bb260
	movq	$62, %rdi
	movq	$5, %rsi
	jmp	.LBB773_22
.LBB773_25:                             # %sw.bb252
	movq	$62, %rdi
	movq	$4, %rsi
	jmp	.LBB773_22
.LBB773_24:                             # %sw.bb244
	movq	$62, %rdi
	movq	$3, %rsi
	jmp	.LBB773_22
.LBB773_21:                             # %sw.bb236
	movq	$62, %rdi
	movq	$2, %rsi
.LBB773_22:                             # %sw.epilog
	callq	gen_rtx_fmt_0
	jmp	.LBB773_23
.LBB773_15:                             # %sw.bb153
	movq	$57, %rdi
	movq	$18, %rsi
	callq	gen_rtx_REG
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	8(%rbx), %rcx
	movq	%rax, 16(%rcx)
	movq	$57, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	8(%rbx), %rcx
	movq	%rax, 24(%rcx)
	movq	$62, %rdi
	movq	$3, %rsi
	jmp	.LBB773_28
.LBB773_20:                             # %sw.bb221
	movq	$62, %rdi
	movq	$5, %rsi
	jmp	.LBB773_10
.LBB773_30:                             # %sw.bb284
	movq	$52, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	8(%rbx), %rcx
	movq	%rax, 16(%rcx)
	movq	$62, %rdi
	movq	$4, %rsi
	callq	gen_rtx_fmt_0
	jmp	.LBB773_13
.LBB773_34:                             # %sw.bb276
	movq	$62, %rdi
	movq	$16, %rsi
	callq	gen_rtx_fmt_0
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	8(%rbx), %rcx
	movq	%rax, 40(%rcx)
	popq	%rbx
	retq
.LBB773_27:                             # %sw.bb268
	movq	$62, %rdi
	movq	$16, %rsi
.LBB773_28:                             # %sw.epilog
	callq	gen_rtx_fmt_0
	jmp	.LBB773_29
.LBB773_17:                             # %sw.bb138
	movq	$62, %rdi
	movq	$4, %rsi
	callq	gen_rtx_fmt_0
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	8(%rbx), %rcx
	movq	%rax, 24(%rcx)
.LBB773_18:                             # %sw.bb190
	movq	$52, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
.LBB773_29:                             # %sw.epilog
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	8(%rbx), %rcx
	movq	%rax, 32(%rcx)
	popq	%rbx
	retq
.Ltmp7369:
	.size	add_clobbers, .Ltmp7369-add_clobbers
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI773_0:
	.quad	.LBB773_18
	.quad	.LBB773_18
	.quad	.LBB773_31
	.quad	.LBB773_33
	.quad	.LBB773_31
	.quad	.LBB773_33
	.quad	.LBB773_33
	.quad	.LBB773_33
	.quad	.LBB773_33
	.quad	.LBB773_33
	.quad	.LBB773_33
	.quad	.LBB773_33
	.quad	.LBB773_31
	.quad	.LBB773_31
	.quad	.LBB773_33
	.quad	.LBB773_31
	.quad	.LBB773_31
	.quad	.LBB773_33
	.quad	.LBB773_31
	.quad	.LBB773_31
	.quad	.LBB773_33
	.quad	.LBB773_31
	.quad	.LBB773_31
	.quad	.LBB773_33
	.quad	.LBB773_33
	.quad	.LBB773_33
	.quad	.LBB773_14
	.quad	.LBB773_14
	.quad	.LBB773_9
	.quad	.LBB773_9
	.quad	.LBB773_33
	.quad	.LBB773_33
	.quad	.LBB773_33
	.quad	.LBB773_33
	.quad	.LBB773_33
	.quad	.LBB773_33
	.quad	.LBB773_33
	.quad	.LBB773_33
	.quad	.LBB773_18
	.quad	.LBB773_18
.LJTI773_1:
	.quad	.LBB773_31
	.quad	.LBB773_31
	.quad	.LBB773_33
	.quad	.LBB773_33
	.quad	.LBB773_33
	.quad	.LBB773_33
	.quad	.LBB773_33
	.quad	.LBB773_33
	.quad	.LBB773_33
	.quad	.LBB773_33
	.quad	.LBB773_33
	.quad	.LBB773_33
	.quad	.LBB773_33
	.quad	.LBB773_33
	.quad	.LBB773_31
	.quad	.LBB773_33
	.quad	.LBB773_33
	.quad	.LBB773_33
	.quad	.LBB773_33
	.quad	.LBB773_33
	.quad	.LBB773_31
	.quad	.LBB773_33
	.quad	.LBB773_33
	.quad	.LBB773_33
	.quad	.LBB773_33
	.quad	.LBB773_33
	.quad	.LBB773_33
	.quad	.LBB773_33
	.quad	.LBB773_33
	.quad	.LBB773_33
	.quad	.LBB773_33
	.quad	.LBB773_33
	.quad	.LBB773_33
	.quad	.LBB773_33
	.quad	.LBB773_33
	.quad	.LBB773_33
	.quad	.LBB773_33
	.quad	.LBB773_33
	.quad	.LBB773_31
	.quad	.LBB773_31
	.quad	.LBB773_33
	.quad	.LBB773_33
	.quad	.LBB773_33
	.quad	.LBB773_33
	.quad	.LBB773_33
	.quad	.LBB773_33
	.quad	.LBB773_33
	.quad	.LBB773_33
	.quad	.LBB773_33
	.quad	.LBB773_33
	.quad	.LBB773_33
	.quad	.LBB773_33
	.quad	.LBB773_33
	.quad	.LBB773_33
	.quad	.LBB773_33
	.quad	.LBB773_33
	.quad	.LBB773_33
	.quad	.LBB773_33
	.quad	.LBB773_33
	.quad	.LBB773_33
	.quad	.LBB773_33
	.quad	.LBB773_33
	.quad	.LBB773_33
	.quad	.LBB773_33
	.quad	.LBB773_31
	.quad	.LBB773_33
	.quad	.LBB773_31
	.quad	.LBB773_31
	.quad	.LBB773_33
	.quad	.LBB773_31
	.quad	.LBB773_31
	.quad	.LBB773_33
	.quad	.LBB773_31
	.quad	.LBB773_33
	.quad	.LBB773_33
	.quad	.LBB773_33
	.quad	.LBB773_30
	.quad	.LBB773_33
	.quad	.LBB773_33
	.quad	.LBB773_33
	.quad	.LBB773_33
	.quad	.LBB773_33
	.quad	.LBB773_33
	.quad	.LBB773_33
	.quad	.LBB773_33
	.quad	.LBB773_33
	.quad	.LBB773_33
	.quad	.LBB773_33
	.quad	.LBB773_33
	.quad	.LBB773_33
	.quad	.LBB773_33
	.quad	.LBB773_33
	.quad	.LBB773_33
	.quad	.LBB773_33
	.quad	.LBB773_33
	.quad	.LBB773_33
	.quad	.LBB773_33
	.quad	.LBB773_33
	.quad	.LBB773_33
	.quad	.LBB773_33
	.quad	.LBB773_33
	.quad	.LBB773_33
	.quad	.LBB773_33
	.quad	.LBB773_33
	.quad	.LBB773_33
	.quad	.LBB773_33
	.quad	.LBB773_33
	.quad	.LBB773_33
	.quad	.LBB773_33
	.quad	.LBB773_33
	.quad	.LBB773_34
	.quad	.LBB773_27
	.quad	.LBB773_33
	.quad	.LBB773_33
	.quad	.LBB773_33
	.quad	.LBB773_33
	.quad	.LBB773_33
	.quad	.LBB773_33
	.quad	.LBB773_33
	.quad	.LBB773_33
	.quad	.LBB773_33
	.quad	.LBB773_33
	.quad	.LBB773_33
	.quad	.LBB773_33
	.quad	.LBB773_33
	.quad	.LBB773_33
	.quad	.LBB773_33
	.quad	.LBB773_33
	.quad	.LBB773_33
	.quad	.LBB773_33
	.quad	.LBB773_33
	.quad	.LBB773_33
	.quad	.LBB773_33
	.quad	.LBB773_33
	.quad	.LBB773_33
	.quad	.LBB773_33
	.quad	.LBB773_33
	.quad	.LBB773_33
	.quad	.LBB773_33
	.quad	.LBB773_33
	.quad	.LBB773_33
	.quad	.LBB773_33
	.quad	.LBB773_31
	.quad	.LBB773_31
	.quad	.LBB773_33
	.quad	.LBB773_31
	.quad	.LBB773_31
	.quad	.LBB773_33
	.quad	.LBB773_33
	.quad	.LBB773_33
	.quad	.LBB773_33
	.quad	.LBB773_33
	.quad	.LBB773_33
	.quad	.LBB773_33
	.quad	.LBB773_33
	.quad	.LBB773_33
	.quad	.LBB773_33
	.quad	.LBB773_33
	.quad	.LBB773_33
	.quad	.LBB773_31
	.quad	.LBB773_33
	.quad	.LBB773_26
	.quad	.LBB773_26
	.quad	.LBB773_26
	.quad	.LBB773_31
	.quad	.LBB773_31
	.quad	.LBB773_33
	.quad	.LBB773_33
	.quad	.LBB773_25
	.quad	.LBB773_33
	.quad	.LBB773_25
	.quad	.LBB773_25
	.quad	.LBB773_31
	.quad	.LBB773_31
	.quad	.LBB773_33
	.quad	.LBB773_24
	.quad	.LBB773_24
	.quad	.LBB773_24
	.quad	.LBB773_31
	.quad	.LBB773_31
	.quad	.LBB773_33
	.quad	.LBB773_21
	.quad	.LBB773_21
	.quad	.LBB773_21
	.quad	.LBB773_31
	.quad	.LBB773_31
	.quad	.LBB773_31
	.quad	.LBB773_31
	.quad	.LBB773_31
	.quad	.LBB773_31
	.quad	.LBB773_33
	.quad	.LBB773_33
	.quad	.LBB773_31
	.quad	.LBB773_31
	.quad	.LBB773_31
	.quad	.LBB773_31
	.quad	.LBB773_33
	.quad	.LBB773_33
	.quad	.LBB773_33
	.quad	.LBB773_33
	.quad	.LBB773_31
	.quad	.LBB773_33
	.quad	.LBB773_33
	.quad	.LBB773_31
	.quad	.LBB773_33
	.quad	.LBB773_33
	.quad	.LBB773_31
	.quad	.LBB773_31
	.quad	.LBB773_31
	.quad	.LBB773_31
	.quad	.LBB773_31
	.quad	.LBB773_31
	.quad	.LBB773_31
	.quad	.LBB773_31
	.quad	.LBB773_31
	.quad	.LBB773_31
	.quad	.LBB773_31
	.quad	.LBB773_20
	.quad	.LBB773_19
	.quad	.LBB773_19
	.quad	.LBB773_20
	.quad	.LBB773_19
	.quad	.LBB773_19
	.quad	.LBB773_31
	.quad	.LBB773_31
	.quad	.LBB773_11
	.quad	.LBB773_11
	.quad	.LBB773_18
	.quad	.LBB773_11
	.quad	.LBB773_11
	.quad	.LBB773_18
	.quad	.LBB773_11
	.quad	.LBB773_11
	.quad	.LBB773_18
	.quad	.LBB773_11
	.quad	.LBB773_18
	.quad	.LBB773_18
	.quad	.LBB773_33
	.quad	.LBB773_33
	.quad	.LBB773_33
	.quad	.LBB773_33
	.quad	.LBB773_33
	.quad	.LBB773_33
	.quad	.LBB773_33
	.quad	.LBB773_33
	.quad	.LBB773_33
	.quad	.LBB773_33
	.quad	.LBB773_31
	.quad	.LBB773_33
	.quad	.LBB773_31
	.quad	.LBB773_31
	.quad	.LBB773_33
	.quad	.LBB773_33
	.quad	.LBB773_31
	.quad	.LBB773_33
	.quad	.LBB773_31
	.quad	.LBB773_31
	.quad	.LBB773_33
	.quad	.LBB773_33
	.quad	.LBB773_31
	.quad	.LBB773_33
	.quad	.LBB773_31
	.quad	.LBB773_31
	.quad	.LBB773_31
	.quad	.LBB773_31
	.quad	.LBB773_33
	.quad	.LBB773_26
	.quad	.LBB773_31
	.quad	.LBB773_31
	.quad	.LBB773_31
	.quad	.LBB773_33
	.quad	.LBB773_33
	.quad	.LBB773_33
	.quad	.LBB773_25
	.quad	.LBB773_31
	.quad	.LBB773_33
	.quad	.LBB773_24
	.quad	.LBB773_31
	.quad	.LBB773_31
	.quad	.LBB773_33
	.quad	.LBB773_33
	.quad	.LBB773_21
	.quad	.LBB773_31
	.quad	.LBB773_33
	.quad	.LBB773_26
	.quad	.LBB773_31
	.quad	.LBB773_31
	.quad	.LBB773_31
	.quad	.LBB773_33
	.quad	.LBB773_33
	.quad	.LBB773_33
	.quad	.LBB773_25
	.quad	.LBB773_31
	.quad	.LBB773_33
	.quad	.LBB773_24
	.quad	.LBB773_31
	.quad	.LBB773_31
	.quad	.LBB773_33
	.quad	.LBB773_21
	.quad	.LBB773_33
	.quad	.LBB773_33
	.quad	.LBB773_31
	.quad	.LBB773_31
	.quad	.LBB773_33
	.quad	.LBB773_31
	.quad	.LBB773_31
	.quad	.LBB773_33
	.quad	.LBB773_33
	.quad	.LBB773_31
	.quad	.LBB773_33
	.quad	.LBB773_31
	.quad	.LBB773_33
	.quad	.LBB773_31
	.quad	.LBB773_11
	.quad	.LBB773_31
	.quad	.LBB773_31
	.quad	.LBB773_11
	.quad	.LBB773_11
	.quad	.LBB773_31
	.quad	.LBB773_31
	.quad	.LBB773_31
	.quad	.LBB773_31
	.quad	.LBB773_33
	.quad	.LBB773_33
	.quad	.LBB773_33
	.quad	.LBB773_33
	.quad	.LBB773_33
	.quad	.LBB773_33
	.quad	.LBB773_33
	.quad	.LBB773_33
	.quad	.LBB773_33
	.quad	.LBB773_31
	.quad	.LBB773_11
	.quad	.LBB773_31
	.quad	.LBB773_31
	.quad	.LBB773_11
	.quad	.LBB773_11
	.quad	.LBB773_31
	.quad	.LBB773_31
	.quad	.LBB773_31
	.quad	.LBB773_31
	.quad	.LBB773_33
	.quad	.LBB773_33
	.quad	.LBB773_33
	.quad	.LBB773_33
	.quad	.LBB773_33
	.quad	.LBB773_33
	.quad	.LBB773_33
	.quad	.LBB773_33
	.quad	.LBB773_33
	.quad	.LBB773_33
	.quad	.LBB773_33
	.quad	.LBB773_33
	.quad	.LBB773_33
	.quad	.LBB773_33
	.quad	.LBB773_33
	.quad	.LBB773_33
	.quad	.LBB773_33
	.quad	.LBB773_33
	.quad	.LBB773_33
	.quad	.LBB773_31
	.quad	.LBB773_33
	.quad	.LBB773_19
	.quad	.LBB773_31
	.quad	.LBB773_31
	.quad	.LBB773_31
	.quad	.LBB773_31
	.quad	.LBB773_33
	.quad	.LBB773_33
	.quad	.LBB773_31
	.quad	.LBB773_31
	.quad	.LBB773_33
	.quad	.LBB773_31
	.quad	.LBB773_31
	.quad	.LBB773_33
	.quad	.LBB773_31
	.quad	.LBB773_31
	.quad	.LBB773_31
	.quad	.LBB773_33
	.quad	.LBB773_33
	.quad	.LBB773_19
	.quad	.LBB773_31
	.quad	.LBB773_31
	.quad	.LBB773_31
	.quad	.LBB773_31
	.quad	.LBB773_31
	.quad	.LBB773_31
	.quad	.LBB773_31
	.quad	.LBB773_31
	.quad	.LBB773_33
	.quad	.LBB773_33
	.quad	.LBB773_33
	.quad	.LBB773_33
	.quad	.LBB773_31
	.quad	.LBB773_31
	.quad	.LBB773_33
	.quad	.LBB773_33
	.quad	.LBB773_31
	.quad	.LBB773_31
	.quad	.LBB773_33
	.quad	.LBB773_33
	.quad	.LBB773_31
	.quad	.LBB773_31
	.quad	.LBB773_33
	.quad	.LBB773_33
	.quad	.LBB773_19
	.quad	.LBB773_31
	.quad	.LBB773_31
	.quad	.LBB773_31
	.quad	.LBB773_31
	.quad	.LBB773_31
	.quad	.LBB773_33
	.quad	.LBB773_33
	.quad	.LBB773_33
	.quad	.LBB773_33
	.quad	.LBB773_31
	.quad	.LBB773_31
	.quad	.LBB773_33
	.quad	.LBB773_33
	.quad	.LBB773_31
	.quad	.LBB773_31
	.quad	.LBB773_33
	.quad	.LBB773_33
	.quad	.LBB773_31
	.quad	.LBB773_31
	.quad	.LBB773_31
	.quad	.LBB773_31
	.quad	.LBB773_31
	.quad	.LBB773_31
	.quad	.LBB773_31
	.quad	.LBB773_31
	.quad	.LBB773_31
	.quad	.LBB773_31
	.quad	.LBB773_31
	.quad	.LBB773_31
	.quad	.LBB773_31
	.quad	.LBB773_31
	.quad	.LBB773_31
	.quad	.LBB773_31
	.quad	.LBB773_31
	.quad	.LBB773_31
	.quad	.LBB773_31
	.quad	.LBB773_31
	.quad	.LBB773_33
	.quad	.LBB773_33
	.quad	.LBB773_33
	.quad	.LBB773_33
	.quad	.LBB773_33
	.quad	.LBB773_33
	.quad	.LBB773_16
	.quad	.LBB773_16
	.quad	.LBB773_16
	.quad	.LBB773_16
	.quad	.LBB773_16
	.quad	.LBB773_16
	.quad	.LBB773_15
	.quad	.LBB773_15
	.quad	.LBB773_15
	.quad	.LBB773_15
	.quad	.LBB773_33
	.quad	.LBB773_33
	.quad	.LBB773_33
	.quad	.LBB773_33
	.quad	.LBB773_33
	.quad	.LBB773_17
	.quad	.LBB773_33
	.quad	.LBB773_33
	.quad	.LBB773_33
	.quad	.LBB773_33
	.quad	.LBB773_33
	.quad	.LBB773_33
	.quad	.LBB773_33
	.quad	.LBB773_33
	.quad	.LBB773_33
	.quad	.LBB773_33
	.quad	.LBB773_31

	.text
	.globl	added_clobbers_hard_reg_p
	.align	16, 0x90
	.type	added_clobbers_hard_reg_p,@function
added_clobbers_hard_reg_p:              # @added_clobbers_hard_reg_p
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rax
.Ltmp7371:
	.cfi_def_cfa_offset 16
	movq	$32, %rcx
	movq	%rdi, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$814, %rax              # imm = 0x32E
	jg	.LBB774_7
# BB#1:                                 # %entry
	movq	$32, %rcx
	movq	%rdi, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$800, %rax              # imm = 0x320
	jg	.LBB774_10
# BB#2:                                 # %entry
	movq	$32, %rcx
	movq	%rdi, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$610, %rax              # imm = 0x262
	jg	.LBB774_5
# BB#3:                                 # %entry
	addq	$-43, %rdi
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rdi, %rcx
	cmpq	$475, %rcx              # imm = 0x1DB
	ja	.LBB774_11
# BB#4:                                 # %entry
	xorq	%rax, %rax
	jmpq	*.LJTI774_0(,%rcx,8)
.LBB774_7:                              # %entry
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %rdi
	cmpq	$815, %rdi              # imm = 0x32F
	je	.LBB774_8
	jmp	.LBB774_11
.LBB774_10:                             # %entry
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %rdi
	cmpq	$801, %rdi              # imm = 0x321
	je	.LBB774_8
	jmp	.LBB774_11
.LBB774_5:                              # %entry
	addq	$-611, %rdi             # imm = 0xFFFFFFFFFFFFFD9D
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rdi, %rcx
	cmpq	$39, %rcx
	ja	.LBB774_11
# BB#6:                                 # %entry
	movq	$1, %rax
	shlq	%cl, %rax
	movabsq	$825647542295, %rcx     # imm = 0xC03C6DB017
	testq	%rcx, %rax
	je	.LBB774_11
.LBB774_8:                              # %sw.bb1
	movq	$1, %rax
.LBB774_9:                              # %return
	popq	%rdx
	retq
.LBB774_11:                             # %sw.default
	movabsq	$.L.str, %rdi
	movabsq	$.L__FUNCTION__.added_clobbers_hard_reg_p, %rdx
	movq	$20280, %rsi            # imm = 0x4F38
	callq	fancy_abort
.Ltmp7372:
	.size	added_clobbers_hard_reg_p, .Ltmp7372-added_clobbers_hard_reg_p
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI774_0:
	.quad	.LBB774_8
	.quad	.LBB774_8
	.quad	.LBB774_11
	.quad	.LBB774_11
	.quad	.LBB774_11
	.quad	.LBB774_11
	.quad	.LBB774_11
	.quad	.LBB774_11
	.quad	.LBB774_11
	.quad	.LBB774_11
	.quad	.LBB774_11
	.quad	.LBB774_11
	.quad	.LBB774_11
	.quad	.LBB774_11
	.quad	.LBB774_8
	.quad	.LBB774_11
	.quad	.LBB774_11
	.quad	.LBB774_11
	.quad	.LBB774_11
	.quad	.LBB774_11
	.quad	.LBB774_8
	.quad	.LBB774_11
	.quad	.LBB774_11
	.quad	.LBB774_11
	.quad	.LBB774_11
	.quad	.LBB774_11
	.quad	.LBB774_11
	.quad	.LBB774_11
	.quad	.LBB774_11
	.quad	.LBB774_11
	.quad	.LBB774_11
	.quad	.LBB774_11
	.quad	.LBB774_11
	.quad	.LBB774_11
	.quad	.LBB774_11
	.quad	.LBB774_11
	.quad	.LBB774_11
	.quad	.LBB774_11
	.quad	.LBB774_8
	.quad	.LBB774_8
	.quad	.LBB774_11
	.quad	.LBB774_11
	.quad	.LBB774_11
	.quad	.LBB774_11
	.quad	.LBB774_11
	.quad	.LBB774_11
	.quad	.LBB774_11
	.quad	.LBB774_11
	.quad	.LBB774_11
	.quad	.LBB774_11
	.quad	.LBB774_11
	.quad	.LBB774_11
	.quad	.LBB774_11
	.quad	.LBB774_11
	.quad	.LBB774_11
	.quad	.LBB774_11
	.quad	.LBB774_11
	.quad	.LBB774_11
	.quad	.LBB774_11
	.quad	.LBB774_11
	.quad	.LBB774_11
	.quad	.LBB774_11
	.quad	.LBB774_11
	.quad	.LBB774_11
	.quad	.LBB774_8
	.quad	.LBB774_11
	.quad	.LBB774_8
	.quad	.LBB774_8
	.quad	.LBB774_11
	.quad	.LBB774_8
	.quad	.LBB774_8
	.quad	.LBB774_11
	.quad	.LBB774_8
	.quad	.LBB774_11
	.quad	.LBB774_11
	.quad	.LBB774_11
	.quad	.LBB774_8
	.quad	.LBB774_11
	.quad	.LBB774_11
	.quad	.LBB774_11
	.quad	.LBB774_11
	.quad	.LBB774_11
	.quad	.LBB774_11
	.quad	.LBB774_11
	.quad	.LBB774_11
	.quad	.LBB774_11
	.quad	.LBB774_11
	.quad	.LBB774_11
	.quad	.LBB774_11
	.quad	.LBB774_11
	.quad	.LBB774_11
	.quad	.LBB774_11
	.quad	.LBB774_11
	.quad	.LBB774_11
	.quad	.LBB774_11
	.quad	.LBB774_11
	.quad	.LBB774_11
	.quad	.LBB774_11
	.quad	.LBB774_11
	.quad	.LBB774_11
	.quad	.LBB774_11
	.quad	.LBB774_11
	.quad	.LBB774_11
	.quad	.LBB774_11
	.quad	.LBB774_11
	.quad	.LBB774_11
	.quad	.LBB774_11
	.quad	.LBB774_11
	.quad	.LBB774_11
	.quad	.LBB774_11
	.quad	.LBB774_9
	.quad	.LBB774_9
	.quad	.LBB774_11
	.quad	.LBB774_11
	.quad	.LBB774_11
	.quad	.LBB774_11
	.quad	.LBB774_11
	.quad	.LBB774_11
	.quad	.LBB774_11
	.quad	.LBB774_11
	.quad	.LBB774_11
	.quad	.LBB774_11
	.quad	.LBB774_11
	.quad	.LBB774_11
	.quad	.LBB774_11
	.quad	.LBB774_11
	.quad	.LBB774_11
	.quad	.LBB774_11
	.quad	.LBB774_11
	.quad	.LBB774_11
	.quad	.LBB774_11
	.quad	.LBB774_11
	.quad	.LBB774_11
	.quad	.LBB774_11
	.quad	.LBB774_11
	.quad	.LBB774_11
	.quad	.LBB774_11
	.quad	.LBB774_11
	.quad	.LBB774_11
	.quad	.LBB774_11
	.quad	.LBB774_11
	.quad	.LBB774_11
	.quad	.LBB774_8
	.quad	.LBB774_8
	.quad	.LBB774_11
	.quad	.LBB774_8
	.quad	.LBB774_8
	.quad	.LBB774_11
	.quad	.LBB774_11
	.quad	.LBB774_11
	.quad	.LBB774_11
	.quad	.LBB774_11
	.quad	.LBB774_11
	.quad	.LBB774_11
	.quad	.LBB774_11
	.quad	.LBB774_11
	.quad	.LBB774_11
	.quad	.LBB774_11
	.quad	.LBB774_11
	.quad	.LBB774_8
	.quad	.LBB774_11
	.quad	.LBB774_9
	.quad	.LBB774_9
	.quad	.LBB774_9
	.quad	.LBB774_8
	.quad	.LBB774_8
	.quad	.LBB774_11
	.quad	.LBB774_11
	.quad	.LBB774_9
	.quad	.LBB774_11
	.quad	.LBB774_9
	.quad	.LBB774_9
	.quad	.LBB774_8
	.quad	.LBB774_8
	.quad	.LBB774_11
	.quad	.LBB774_9
	.quad	.LBB774_9
	.quad	.LBB774_9
	.quad	.LBB774_8
	.quad	.LBB774_8
	.quad	.LBB774_11
	.quad	.LBB774_9
	.quad	.LBB774_9
	.quad	.LBB774_9
	.quad	.LBB774_8
	.quad	.LBB774_8
	.quad	.LBB774_8
	.quad	.LBB774_8
	.quad	.LBB774_8
	.quad	.LBB774_8
	.quad	.LBB774_11
	.quad	.LBB774_11
	.quad	.LBB774_8
	.quad	.LBB774_8
	.quad	.LBB774_8
	.quad	.LBB774_8
	.quad	.LBB774_11
	.quad	.LBB774_11
	.quad	.LBB774_11
	.quad	.LBB774_11
	.quad	.LBB774_8
	.quad	.LBB774_11
	.quad	.LBB774_11
	.quad	.LBB774_8
	.quad	.LBB774_11
	.quad	.LBB774_11
	.quad	.LBB774_8
	.quad	.LBB774_8
	.quad	.LBB774_8
	.quad	.LBB774_8
	.quad	.LBB774_8
	.quad	.LBB774_8
	.quad	.LBB774_8
	.quad	.LBB774_8
	.quad	.LBB774_8
	.quad	.LBB774_8
	.quad	.LBB774_8
	.quad	.LBB774_8
	.quad	.LBB774_8
	.quad	.LBB774_8
	.quad	.LBB774_8
	.quad	.LBB774_8
	.quad	.LBB774_8
	.quad	.LBB774_8
	.quad	.LBB774_8
	.quad	.LBB774_8
	.quad	.LBB774_8
	.quad	.LBB774_8
	.quad	.LBB774_8
	.quad	.LBB774_8
	.quad	.LBB774_8
	.quad	.LBB774_8
	.quad	.LBB774_8
	.quad	.LBB774_8
	.quad	.LBB774_8
	.quad	.LBB774_8
	.quad	.LBB774_8
	.quad	.LBB774_11
	.quad	.LBB774_11
	.quad	.LBB774_11
	.quad	.LBB774_11
	.quad	.LBB774_11
	.quad	.LBB774_11
	.quad	.LBB774_11
	.quad	.LBB774_11
	.quad	.LBB774_11
	.quad	.LBB774_11
	.quad	.LBB774_8
	.quad	.LBB774_11
	.quad	.LBB774_8
	.quad	.LBB774_8
	.quad	.LBB774_11
	.quad	.LBB774_11
	.quad	.LBB774_8
	.quad	.LBB774_11
	.quad	.LBB774_8
	.quad	.LBB774_8
	.quad	.LBB774_11
	.quad	.LBB774_11
	.quad	.LBB774_8
	.quad	.LBB774_11
	.quad	.LBB774_8
	.quad	.LBB774_8
	.quad	.LBB774_8
	.quad	.LBB774_8
	.quad	.LBB774_11
	.quad	.LBB774_9
	.quad	.LBB774_8
	.quad	.LBB774_8
	.quad	.LBB774_8
	.quad	.LBB774_11
	.quad	.LBB774_11
	.quad	.LBB774_11
	.quad	.LBB774_9
	.quad	.LBB774_8
	.quad	.LBB774_11
	.quad	.LBB774_9
	.quad	.LBB774_8
	.quad	.LBB774_8
	.quad	.LBB774_11
	.quad	.LBB774_11
	.quad	.LBB774_9
	.quad	.LBB774_8
	.quad	.LBB774_11
	.quad	.LBB774_9
	.quad	.LBB774_8
	.quad	.LBB774_8
	.quad	.LBB774_8
	.quad	.LBB774_11
	.quad	.LBB774_11
	.quad	.LBB774_11
	.quad	.LBB774_9
	.quad	.LBB774_8
	.quad	.LBB774_11
	.quad	.LBB774_9
	.quad	.LBB774_8
	.quad	.LBB774_8
	.quad	.LBB774_11
	.quad	.LBB774_9
	.quad	.LBB774_11
	.quad	.LBB774_11
	.quad	.LBB774_8
	.quad	.LBB774_8
	.quad	.LBB774_11
	.quad	.LBB774_8
	.quad	.LBB774_8
	.quad	.LBB774_11
	.quad	.LBB774_11
	.quad	.LBB774_8
	.quad	.LBB774_11
	.quad	.LBB774_8
	.quad	.LBB774_11
	.quad	.LBB774_8
	.quad	.LBB774_8
	.quad	.LBB774_8
	.quad	.LBB774_8
	.quad	.LBB774_8
	.quad	.LBB774_8
	.quad	.LBB774_8
	.quad	.LBB774_8
	.quad	.LBB774_8
	.quad	.LBB774_8
	.quad	.LBB774_11
	.quad	.LBB774_11
	.quad	.LBB774_11
	.quad	.LBB774_11
	.quad	.LBB774_11
	.quad	.LBB774_11
	.quad	.LBB774_11
	.quad	.LBB774_11
	.quad	.LBB774_11
	.quad	.LBB774_8
	.quad	.LBB774_8
	.quad	.LBB774_8
	.quad	.LBB774_8
	.quad	.LBB774_8
	.quad	.LBB774_8
	.quad	.LBB774_8
	.quad	.LBB774_8
	.quad	.LBB774_8
	.quad	.LBB774_8
	.quad	.LBB774_11
	.quad	.LBB774_11
	.quad	.LBB774_11
	.quad	.LBB774_11
	.quad	.LBB774_11
	.quad	.LBB774_11
	.quad	.LBB774_11
	.quad	.LBB774_11
	.quad	.LBB774_11
	.quad	.LBB774_11
	.quad	.LBB774_11
	.quad	.LBB774_11
	.quad	.LBB774_11
	.quad	.LBB774_11
	.quad	.LBB774_11
	.quad	.LBB774_11
	.quad	.LBB774_11
	.quad	.LBB774_11
	.quad	.LBB774_11
	.quad	.LBB774_8
	.quad	.LBB774_11
	.quad	.LBB774_8
	.quad	.LBB774_8
	.quad	.LBB774_8
	.quad	.LBB774_8
	.quad	.LBB774_8
	.quad	.LBB774_11
	.quad	.LBB774_11
	.quad	.LBB774_8
	.quad	.LBB774_8
	.quad	.LBB774_11
	.quad	.LBB774_8
	.quad	.LBB774_8
	.quad	.LBB774_11
	.quad	.LBB774_8
	.quad	.LBB774_8
	.quad	.LBB774_8
	.quad	.LBB774_11
	.quad	.LBB774_11
	.quad	.LBB774_8
	.quad	.LBB774_8
	.quad	.LBB774_8
	.quad	.LBB774_8
	.quad	.LBB774_8
	.quad	.LBB774_8
	.quad	.LBB774_8
	.quad	.LBB774_8
	.quad	.LBB774_8
	.quad	.LBB774_11
	.quad	.LBB774_11
	.quad	.LBB774_11
	.quad	.LBB774_11
	.quad	.LBB774_8
	.quad	.LBB774_8
	.quad	.LBB774_11
	.quad	.LBB774_11
	.quad	.LBB774_8
	.quad	.LBB774_8
	.quad	.LBB774_11
	.quad	.LBB774_11
	.quad	.LBB774_8
	.quad	.LBB774_8
	.quad	.LBB774_11
	.quad	.LBB774_11
	.quad	.LBB774_8
	.quad	.LBB774_8
	.quad	.LBB774_8
	.quad	.LBB774_8
	.quad	.LBB774_8
	.quad	.LBB774_8
	.quad	.LBB774_11
	.quad	.LBB774_11
	.quad	.LBB774_11
	.quad	.LBB774_11
	.quad	.LBB774_8
	.quad	.LBB774_8
	.quad	.LBB774_11
	.quad	.LBB774_11
	.quad	.LBB774_8
	.quad	.LBB774_8
	.quad	.LBB774_11
	.quad	.LBB774_11
	.quad	.LBB774_8
	.quad	.LBB774_8
	.quad	.LBB774_8
	.quad	.LBB774_8
	.quad	.LBB774_8
	.quad	.LBB774_8
	.quad	.LBB774_8
	.quad	.LBB774_8
	.quad	.LBB774_8
	.quad	.LBB774_8
	.quad	.LBB774_8
	.quad	.LBB774_8
	.quad	.LBB774_8
	.quad	.LBB774_8
	.quad	.LBB774_8
	.quad	.LBB774_8
	.quad	.LBB774_8
	.quad	.LBB774_8
	.quad	.LBB774_8
	.quad	.LBB774_8
	.quad	.LBB774_11
	.quad	.LBB774_11
	.quad	.LBB774_11
	.quad	.LBB774_11
	.quad	.LBB774_11
	.quad	.LBB774_11
	.quad	.LBB774_8
	.quad	.LBB774_8
	.quad	.LBB774_8
	.quad	.LBB774_8
	.quad	.LBB774_8
	.quad	.LBB774_8
	.quad	.LBB774_8
	.quad	.LBB774_8
	.quad	.LBB774_8
	.quad	.LBB774_8
	.quad	.LBB774_11
	.quad	.LBB774_11
	.quad	.LBB774_11
	.quad	.LBB774_11
	.quad	.LBB774_11
	.quad	.LBB774_8
	.quad	.LBB774_11
	.quad	.LBB774_11
	.quad	.LBB774_11
	.quad	.LBB774_11
	.quad	.LBB774_11
	.quad	.LBB774_11
	.quad	.LBB774_11
	.quad	.LBB774_11
	.quad	.LBB774_11
	.quad	.LBB774_11
	.quad	.LBB774_8

	.type	.L.str,@object          # @.str
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.asciz	"insn-emit.c"
	.size	.L.str, 12

	.type	.L__FUNCTION__.gen_reload_outqi,@object # @__FUNCTION__.gen_reload_outqi
.L__FUNCTION__.gen_reload_outqi:
	.asciz	"gen_reload_outqi"
	.size	.L__FUNCTION__.gen_reload_outqi, 17

	.type	.L.str1,@object         # @.str1
.L.str1:
	.asciz	"r"
	.size	.L.str1, 2

	.type	.L__FUNCTION__.gen_split_1001,@object # @__FUNCTION__.gen_split_1001
.L__FUNCTION__.gen_split_1001:
	.asciz	"gen_split_1001"
	.size	.L__FUNCTION__.gen_split_1001, 15

	.type	.L__FUNCTION__.gen_split_1003,@object # @__FUNCTION__.gen_split_1003
.L__FUNCTION__.gen_split_1003:
	.asciz	"gen_split_1003"
	.size	.L__FUNCTION__.gen_split_1003, 15

	.type	.L__FUNCTION__.gen_call_pop,@object # @__FUNCTION__.gen_call_pop
.L__FUNCTION__.gen_call_pop:
	.asciz	"gen_call_pop"
	.size	.L__FUNCTION__.gen_call_pop, 13

	.type	.L__FUNCTION__.gen_split_1231,@object # @__FUNCTION__.gen_split_1231
.L__FUNCTION__.gen_split_1231:
	.asciz	"gen_split_1231"
	.size	.L__FUNCTION__.gen_split_1231, 15

	.type	.L.str2,@object         # @.str2
.L.str2:
	.asciz	"q"
	.size	.L.str2, 2

	.type	.L__FUNCTION__.gen_prefetch,@object # @__FUNCTION__.gen_prefetch
.L__FUNCTION__.gen_prefetch:
	.asciz	"gen_prefetch"
	.size	.L__FUNCTION__.gen_prefetch, 13

	.type	.L__FUNCTION__.add_clobbers,@object # @__FUNCTION__.add_clobbers
.L__FUNCTION__.add_clobbers:
	.asciz	"add_clobbers"
	.size	.L__FUNCTION__.add_clobbers, 13

	.type	.L__FUNCTION__.added_clobbers_hard_reg_p,@object # @__FUNCTION__.added_clobbers_hard_reg_p
.L__FUNCTION__.added_clobbers_hard_reg_p:
	.asciz	"added_clobbers_hard_reg_p"
	.size	.L__FUNCTION__.added_clobbers_hard_reg_p, 26


	.ident	"clang version 3.5 (trunk)"
	.section	".note.GNU-stack","",@progbits
