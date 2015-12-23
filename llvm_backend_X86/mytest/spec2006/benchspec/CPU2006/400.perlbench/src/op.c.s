	.file	"op.c.bc"
	.text
	.globl	Perl_allocmy
	.align	16, 0x90
	.type	Perl_allocmy,@function
Perl_allocmy:                           # @Perl_allocmy
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
	movq	PL_in_my(%rip), %rbx
	movq	%rdi, %r15
	cmpq	$137, %rbx
	movq	$137, %rax
	je	.LBB0_27
# BB#1:                                 # %lor.lhs.false
	movzbl	1(%r15), %r14d
	leaq	-65(%r14), %rax
	andq	$255, %rax
	cmpq	$26, %rax
	jb	.LBB0_2
# BB#3:                                 # %lor.lhs.false
	leaq	-97(%r14), %rax
	andq	$255, %rax
	cmpq	$26, %rax
	jb	.LBB0_4
# BB#5:                                 # %lor.lhs.false17
	testq	$8388608, PL_hints(%rip) # imm = 0x800000
	je	.LBB0_8
# BB#6:                                 # %lor.lhs.false17
	leaq	64(%r14), %rax
	andq	$255, %rax
	cmpq	$62, %rax
	jae	.LBB0_8
# BB#7:
	movq	%rbx, %rax
	jmp	.LBB0_27
.LBB0_2:
	movq	%rbx, %rax
	jmp	.LBB0_27
.LBB0_4:
	movq	%rbx, %rax
	jmp	.LBB0_27
.LBB0_8:                                # %lor.lhs.false28
	movq	%r14, %r12
	andq	$255, %r12
	cmpq	$95, %r12
	jne	.LBB0_10
# BB#9:                                 # %land.lhs.true33
	movq	%r15, %rdi
	callq	strlen
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$2, %rax
	movq	%rbx, %rax
	jg	.LBB0_27
.LBB0_10:                               # %if.then
	movq	$56, %rcx
	movq	%r14, %rax
	shlq	%cl, %rax
	movq	$56, %rcx
	sarq	%cl, %rax
	cmpq	$32, %rax
	movabsq	$0, %rax
	movabsq	$1, %rdx
	movq	%rax, %rcx
	jle	.LBB0_12
# BB#11:                                # %if.then
	movq	%rdx, %rcx
.LBB0_12:                               # %if.then
	cmpq	$127, %r12
	je	.LBB0_14
# BB#13:                                # %if.then
	movq	%rdx, %rax
.LBB0_14:                               # %if.then
	testq	%rcx, %rax
	jne	.LBB0_16
# BB#15:                                # %if.then
	cmpq	$32, %r12
	jne	.LBB0_17
.LBB0_16:                               # %lor.lhs.false51
	movq	$56, %rcx
	shlq	%cl, %r14
	movq	$56, %rcx
	sarq	%cl, %r14
	movabsq	$.L.str, %rdi
	movq	$5, %rdx
	movq	%r14, %rsi
	callq	memchr
	testq	%rax, %rax
	je	.LBB0_26
.LBB0_17:                               # %if.then56
	movq	%r15, %rdi
	callq	strlen
	cmpq	$201, %rax
	jl	.LBB0_19
# BB#18:                                # %if.then60
	movq	$3026478, %rax          # imm = 0x2E2E2E
	movl	%eax, 200(%r15)
	leaq	199(%r15), %rcx
	jmp	.LBB0_20
.LBB0_19:                               # %if.else
	leaq	(%r15,%rax), %rcx
	xorq	%rdx, %rdx
	movb	%dl, 1(%r15,%rax)
.LBB0_20:                               # %for.cond.preheader
	movq	%rcx, %rax
	subq	%r15, %rax
	cmpq	$3, %rax
	jl	.LBB0_23
# BB#21:                                # %for.body.preheader
	decq	%rcx
	.align	16, 0x90
.LBB0_22:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movzbl	(%rcx), %eax
	movb	%al, 1(%rcx)
	movq	%rcx, %rax
	subq	%r15, %rax
	decq	%rcx
	cmpq	$2, %rax
	jg	.LBB0_22
.LBB0_23:                               # %for.end
	movzbl	1(%r15), %eax
	leaq	-97(%rax), %rcx
	andq	$255, %rcx
	cmpq	$26, %rcx
	jae	.LBB0_25
# BB#24:
	addq	$-32, %rax
.LBB0_25:                               # %for.end
	xorq	$64, %rax
	movb	%al, 2(%r15)
	movq	$94, %rax
	movb	%al, 1(%r15)
.LBB0_26:                               # %if.end86
	movabsq	$.L.str2, %rdi
	xorq	%rax, %rax
	movq	%r15, %rsi
	callq	Perl_form
	movq	%rax, %rcx
	xorq	%rax, %rax
	movq	%rcx, %rdi
	callq	yyerror
	movq	PL_in_my(%rip), %rax
.LBB0_27:                               # %if.end89
	cmpq	$137, %rax
	jne	.LBB0_28
# BB#29:                                # %if.end89
	movabsq	$1, %rsi
	jmp	.LBB0_30
.LBB0_28:
	movabsq	$0, %rsi
.LBB0_30:                               # %if.end89
	movq	PL_curstash(%rip), %rdx
	testq	%rdx, %rdx
	jne	.LBB0_32
# BB#31:                                # %if.end89
	movq	PL_defstash(%rip), %rdx
.LBB0_32:                               # %if.end89
	xorq	%rax, %rax
	xorq	%rbx, %rbx
	movq	%r15, %rdi
	callq	pad_check_dup
	movq	PL_in_my_stash(%rip), %rax
	testq	%rax, %rax
	movabsq	$0, %rsi
	je	.LBB0_38
# BB#33:                                # %land.lhs.true101
	movzbl	(%r15), %ecx
	cmpq	$36, %rcx
	movq	%rax, %rsi
	je	.LBB0_38
# BB#34:                                # %if.then105
	movq	PL_in_my(%rip), %rax
	cmpq	$137, %rax
	jne	.LBB0_36
# BB#35:
	movabsq	$.L.str4, %rdx
	jmp	.LBB0_37
.LBB0_36:                               # %select.mid
	movabsq	$.L.str5, %rdx
.LBB0_37:                               # %select.end
	movabsq	$.L.str3, %rdi
	xorq	%rax, %rax
	movq	%r15, %rsi
	callq	Perl_form
	movq	%rax, %rcx
	xorq	%rax, %rax
	movq	%rcx, %rdi
	callq	yyerror
	movq	PL_in_my_stash(%rip), %rsi
.LBB0_38:                               # %if.end111
	movq	PL_in_my(%rip), %rax
	cmpq	$137, %rax
	jne	.LBB0_41
# BB#39:                               