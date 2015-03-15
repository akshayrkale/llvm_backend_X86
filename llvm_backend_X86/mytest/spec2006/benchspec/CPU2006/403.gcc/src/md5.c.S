	.file	"md5.c.bc"
	.text
	.globl	md5_init_ctx
	.align	16, 0x90
	.type	md5_init_ctx,@function
md5_init_ctx:                           # @md5_init_ctx
	.cfi_startproc
# BB#0:                                 # %entry
	movq	$1732584193, %rax       # imm = 0x67452301
	movl	%eax, (%rdi)
	movabsq	$4023233417, %rax       # imm = 0xEFCDAB89
	movl	%eax, 4(%rdi)
	movabsq	$2562383102, %rax       # imm = 0x98BADCFE
	movl	%eax, 8(%rdi)
	movq	$271733878, %rax        # imm = 0x10325476
	movl	%eax, 12(%rdi)
	xorq	%rax, %rax
	movl	%eax, 20(%rdi)
	movl	%eax, 16(%rdi)
	movl	%eax, 24(%rdi)
	retq
.Ltmp0:
	.size	md5_init_ctx, .Ltmp0-md5_init_ctx
	.cfi_endproc

	.globl	md5_read_ctx
	.align	16, 0x90
	.type	md5_read_ctx,@function
md5_read_ctx:                           # @md5_read_ctx
	.cfi_startproc
# BB#0:                                 # %entry
	movl	(%rdi), %eax
	movl	%eax, (%rsi)
	movl	4(%rdi), %eax
	movl	%eax, 4(%rsi)
	movl	8(%rdi), %eax
	movl	%eax, 8(%rsi)
	movl	12(%rdi), %eax
	movl	%eax, 12(%rsi)
	movq	%rsi, %rax
	retq
.Ltmp1:
	.size	md5_read_ctx, .Ltmp1-md5_read_ctx
	.cfi_endproc

	.globl	md5_finish_ctx
	.align	16, 0x90
	.type	md5_finish_ctx,@function
md5_finish_ctx:                         # @md5_finish_ctx
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp8:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp9:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp10:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp11:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp12:
	.cfi_def_cfa_offset 48
.Ltmp13:
	.cfi_offset %rbx, -48
.Ltmp14:
	.cfi_offset %r12, -40
.Ltmp15:
	.cfi_offset %r13, -32
.Ltmp16:
	.cfi_offset %r14, -24
.Ltmp17:
	.cfi_offset %r15, -16
	movq	%rdi, %rbx
	movl	24(%rbx), %r13d
	movl	16(%rbx), %ecx
	movq	%rsi, %r14
	addq	%r13, %rcx
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	movq	%rcx, %rdx
	andq	%rax, %rdx
	cmpq	%rcx, %rdx
	movl	%ecx, 16(%rbx)
	je	.LBB2_2
# BB#1:                                 # %if.then
	movl	20(%rbx), %ecx
	incq	%rcx
	movl	%ecx, 20(%rbx)
.LBB2_2:                                # %if.end
	cmpq	$55, %r13
	movq	$120, %r15
	ja	.LBB2_4
# BB#3:                                 # %if.end
	movq	$56, %r15
.LBB2_4:                                # %if.end
	subq	%r13, %r15
	andq	%rax, %r15
	leaq	28(%rbx,%r13), %rdi
	leaq	28(%rbx), %r12
	movabsq	$fillbuf, %rsi
	movq	%r15, %rdx
	callq	memcpy
	movl	16(%rbx), %eax
	movq	$3, %rcx
	shlq	%cl, %rax
	leaq	(%r15,%r13), %rdx
	movl	%eax, 28(%rbx,%rdx)
	movl	20(%rbx), %eax
	movq	$3, %rcx
	movl	16(%rbx), %esi
	shlq	%cl, %rax
	movq	$29, %rcx
	shrq	%cl, %rsi
	orq	%rax, %rsi
	movl	%esi, 32(%rbx,%rdx)
	leaq	8(%r15,%r13), %rsi
	movq	%r12, %rdi
	movq	%rbx, %rdx
	callq	md5_process_block
	movl	(%rbx), %eax
	movl	%eax, (%r14)
	movl	4(%rbx), %eax
	movl	%eax, 4(%r14)
	movl	8(%rbx), %eax
	movl	%eax, 8(%r14)
	movl	12(%rbx), %eax
	movl	%eax, 12(%r14)
	movq	%r14, %rax
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp18:
	.size	md5_finish_ctx, .Ltmp18-md5_finish_ctx
	.cfi_endproc

	.globl	md5_process_block
	.align	16, 0x90
	.type	md5_process_block,@function
md5_process_block:                      # @md5_process_block
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp25:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp26:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp27:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp28:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp29:
	.cfi_def_cfa_offset 48
	subq	$136, %rsp
.Ltmp30:
	.cfi_def_cfa_offset 184
.Ltmp31:
	.cfi_offset %rbx, -48
.Ltmp32:
	.cfi_offset %r12, -40
.Ltmp33:
	.cfi_offset %r13, -32
.Ltmp34:
	.cfi_offset %r14, -24
.Ltmp35:
	.cfi_offset %r15, -16
	movq	%rsi, %r8
	andq	$-4, %r8
	addq	%rdi, %r8
	movl	(%rdx), %r11d
	movl	4(%rdx), %r10d
	movl	8(%rdx), %r9d
	movl	16(%rdx), %eax
	movl	12(%rdx), %ebx
	addq	%rsi, %rax
	movl	%eax, 16(%rdx)
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rax, %rcx
	cmpq	%rsi, %rcx
	jae	.LBB3_2
# BB#1:                                 # %if.then
	movl	20(%rdx), %eax
	incq	%rax
	movl	%eax, 20(%rdx)
.LBB3_2:                                # %while.cond.preheader
	movq	%rdx, -32(%rsp)         # 8-byte Spill
	cmpq	%r8, %rdi
	jae	.LBB3_5
# BB#3:
	movq	%r8, -24(%rsp)          # 8-byte Spill
	.align	16, 0x90
.LBB3_4:                                # %while.body
                                        # =>This Inner Loop Header: Depth=1
	movq	%rdi, 128(%rsp)         # 8-byte Spill
	movq	%r10, -16(%rsp)         # 8-byte Spill
	movq	%r11, -8(%rsp)          # 8-byte Spill
	movq	%r9, (%rsp)             # 8-byte Spill
	movq	%rbx, %rsi
	movq	%rsi, 8(%rsp)           # 8-byte Spill
	movq	%r9, %rax
	xorq	%rsi, %rax
	movq	%r10, %rdx
	xorq	%r9, %rdx
	andq	%r10, %rax
	xorq	%rsi, %rax
	movl	(%rdi), %ecx
	movq	%rcx, 48(%rsp)          # 8-byte Spill
	addq	%r11, %rax
	leaq	-680876936(%rcx,%rax), %rax
	movq	%rax, %rbx
	movabsq	$4261412864, %r14       # imm = 0xFE000000
	andq	%r14, %rbx
	movq	$7, %rcx
	shlq	%cl, %rax
	movq	$25, %rcx
	shrq	%cl, %rbx
	orq	%rax, %rbx
	addq	%r10, %rbx
	andq	%rbx, %rdx
	movl	4(%rdi), %eax
	xorq	%r9, %rdx
	leaq	(%rsi,%rax), %rax
	leaq	-389564586(%rdx,%rax), %rax
	movq	%rax, %rsi
	movabsq	$4293918720, %r8        # imm = 0xFFF00000
	andq	%r8, %rsi
	movq	$12, %rcx
	shlq	%cl, %rax
	movq	$20, %rcx
	shrq	%cl, %rsi
	orq	%rax, %rsi
	addq	%rbx, %rsi
	movq	%rbx, %rax
	xorq	%r10, %rax
	andq	%rsi, %rax
	movl	8(%rdi), %ecx
	movq	%rcx, 96(%rsp)          # 8-byte Spill
	xorq	%r10, %rax
	leaq	(%r9,%rcx), %rcx
	leaq	606105819(%rax,%rcx), %rdx
	movq	%rdx, %rax
	movabsq	$4294934528, %r9        # imm = 0xFFFF8000
	andq	%r9, %rax
	movq	$17, %rcx
	shlq	%cl, %rdx
	movq	$15, %rcx
	shrq	%cl, %rax
	orq	%rdx, %rax
	addq	%rsi, %rax
	movq	%rsi, %rcx
	xorq	%rbx, %rcx
	andq	%rax, %rcx
	movl	12(%rdi), %edx
	movq	%rdx, 88(%rsp)          # 8-byte Spill
	xorq	%rbx, %rcx
	leaq	(%r10,%rdx), %rdx
	leaq	-1044525330(%rcx,%rdx), %r11
	movq	%r11, %rdx
	movabsq	$4294966272, %r15       # imm = 0xFFFFFC00
	andq	%r15, %rdx
	movq	$22, %rcx
	shlq	%cl, %r11
	movq	$10, %rcx
	shrq	%cl, %rdx
	orq	%r11, %rdx
	addq	%rax, %rdx
	movq	%rax, %rcx
	xorq	%rsi, %rcx
	andq	%rdx, %rcx
	movl	16(%rdi), %r10d
	movq	%r10, 80(%rsp)          # 8-byte Spill
	xorq	%rsi, %rcx
	addq	%r10, %rbx
	leaq	-176418897(%rcx,%rbx), %r11
	movq	%r11, %rbx
	andq	%r14, %rbx
	movq	$7, %rcx
	shlq	%cl, %r11
	movq	$25, %rcx
	shrq	%cl, %rbx
	orq	%r11, %rbx
	addq	%rdx, %rbx
	movq	%rdx, %rcx
	xorq	%rax, %rcx
	andq	%rbx, %rcx
	movl	20(%rdi), %r10d
	movq	%r10, 64(%rsp)          # 8-byte Spill
	xorq	%rax, %rcx
	addq	%r10, %rsi
	leaq	1200080426(%rcx,%rsi), %r11
	movq	%r11, %rsi
	andq	%r8, %rsi
	movq	$12, %rcx
	shlq	%cl, %r11
	movq	$20, %rcx
	shrq	%cl, %rsi
	orq	%r11, %rsi
	addq	%rbx, %rsi
	movq	%rbx, %rcx
	xorq	%rdx, %rcx
	andq	%rsi, %rcx
	movl	24(%rdi), %r10d
	xorq	%rdx, %rcx
	addq	%r10, %rax
	leaq	-1473231341(%rcx,%rax), %r11
	movq	%r11, %rax
	andq	%r9, %rax
	movq	$17, %rcx
	shlq	%cl, %r11
	movq	$15, %rcx
	shrq	%cl, %rax
	orq	%r11, %rax
	addq	%rsi, %rax
	movq	%rsi, %rcx
	xorq	%rbx, %rcx
	andq	%rax, %rcx
	movl	28(%rdi), %r10d
	movq	%r10, 32(%rsp)          # 8-byte Spill
	xorq	%rbx, %rcx
	addq	%r10, %rdx
	leaq	-45705983(%rcx,%rdx), %r11
	movq	%r11, %rdx
	andq	%r15, %rdx
	movq	$22, %rcx
	shlq	%cl, %r11
	movq	$10, %rcx
	shrq	%cl, %rdx
	orq	%r11, %rdx
	addq	%rax, %rdx
	movq	%rax, %rcx
	xorq	%rsi, %rcx
	movl	32(%rdi), %r10d
	movq	%r10, 24(%rsp)          # 8-byte Spill
	andq	%rdx, %rcx
	xorq	%rsi, %rcx
	addq	%r10, %rbx
	leaq	1770035416(%rcx,%rbx), %r11
	movq	%r11, %rbx
	movq	$7, %rcx
	shlq	%cl, %r11
	movq	$25, %rcx
	andq	%r14, %rbx
	shrq	%cl, %rbx
	orq	%r11, %rbx
	movq	%rdx, %rcx
	movl	36(%rdi), %r10d
	movq	%r10, 112(%rsp)         # 8-byte Spill
	addq	%rdx, %rbx
	xorq	%rax, %rcx
	andq	%rbx, %rcx
	xorq	%rax, %rcx
	addq	%r10, %rsi
	leaq	-1958414417(%rcx,%rsi), %r11
	movq	%r11, %rsi
	movq	$12, %rcx
	shlq	%cl, %r11
	movq	$20, %rcx
	andq	%r8, %rsi
	shrq	%cl, %rsi
	orq	%r11, %rsi
	movq	%rbx, %rcx
	movl	40(%rdi), %r10d
	movq	%r10, 120(%rsp)         # 8-byte Spill
	addq	%rbx, %rsi
	xorq	%rdx, %rcx
	andq	%rsi, %rcx
	xorq	%rdx, %rcx
	addq	%r10, %rax
	leaq	-42063(%rcx,%rax), %r11
	movq	%r11, %rax
	movq	$17, %rcx
	shlq	%cl, %r11
	movq	$15, %rcx
	andq	%r9, %rax
	shrq	%cl, %rax
	orq	%r11, %rax
	movq	%rsi, %rcx
	movl	44(%rdi), %r11d
	movq	%r11, 56(%rsp)          # 8-byte Spill
	addq	%rsi, %rax
	xorq	%rbx, %rcx
	andq	%rax, %rcx
	xorq	%rbx, %rcx
	addq	%r11, %rdx
	movq	%r11, %rdi
	leaq	-1990404162(%rcx,%rdx), %rdx
	movq	%rdx, %r12
	movq	$22, %rcx
	shlq	%cl, %rdx
	movq	$10, %rcx
	andq	%r15, %r12
	shrq	%cl, %r12
	orq	%rdx, %r12
	movq	%rax, %rcx
	movq	128(%rsp), %rdx         # 8-byte Reload
	movl	48(%rdx), %edx
	movq	%rdx, 104(%rsp)         # 8-byte Spill
	addq	%rax, %r12
	xorq	%rsi, %rcx
	andq	%r12, %rcx
	xorq	%rsi, %rcx
	addq	%rdx, %rbx
	leaq	1804603682(%rcx,%rbx), %rdx
	movq	%rdx, %r13
	movq	$7, %rcx
	shlq	%cl, %rdx
	movq	$25, %rcx
	andq	%r14, %r13
	shrq	%cl, %r13
	orq	%rdx, %r13
	movq	%r12, %rcx
	movq	128(%rsp), %rdx         # 8-byte Reload
	movl	52(%rdx), %edx
	movq	%rdx, 72(%rsp)          # 8-byte Spill
	addq	%r12, %r13
	xorq	%rax, %rcx
	andq	%r13, %rcx
	xorq	%rax, %rcx
	addq	%rdx, %rsi
	leaq	-40341101(%rcx,%rsi), %rdx
	movq	%rdx, %r11
	movq	$12, %rcx
	shlq	%cl, %rdx
	movq	$20, %rcx
	andq	%r8, %r11
	shrq	%cl, %r11
	orq	%rdx, %r11
	movq	%r13, %rcx
	movq	128(%rsp), %rdx         # 8-byte Reload
	movl	56(%rdx), %edx
	movq	%rdx, 16(%rsp)          # 8-byte Spill
	addq	%r13, %r11
	xorq	%r12, %rcx
	andq	%r11, %rcx
	xorq	%r12, %rcx
	addq	%rdx, %rax
	leaq	-1502002290(%rcx,%rax), %rdx
	movq	%rdx, %rax
	movq	$17, %rcx
	shlq	%cl, %rdx
	movq	$15, %rcx
	andq	%r9, %rax
	shrq	%cl, %rax
	orq	%rdx, %rax
	movq	%r11, %rcx
	movq	128(%rsp), %rdx         # 8-byte Reload
	movl	60(%rdx), %edx
	movq	%rdx, 40(%rsp)          # 8-byte Spill
	addq	%r11, %rax
	xorq	%r13, %rcx
	andq	%rax, %rcx
	xorq	%r13, %rcx
	addq	%rdx, %r12
	movq	%rdx, %r14
	leaq	1236535329(%rcx,%r12), %rdx
	movq	%rdx, %rbx
	movq	$22, %rcx
	shlq	%cl, %rdx
	movq	$10, %rcx
	andq	%r15, %rbx
	shrq	%cl, %rbx
	orq	%rdx, %rbx
	addq	%rax, %rbx
	movq	%rbx, %rcx
	xorq	%rax, %rcx
	andq	%r11, %rcx
	xorq	%rax, %rcx
	movq	128(%rsp), %rdx         # 8-byte Reload
	movl	4(%rdx), %edx
	addq	%rdx, %r13
	leaq	-165796510(%rcx,%r13), %r12
	movq	%r12, %rdx
	movq	$5, %rcx
	shlq	%cl, %r12
	movq	$27, %rcx
	movabsq	$4160749568, %r15       # imm = 0xF8000000
	andq	%r15, %rdx
	shrq	%cl, %rdx
	orq	%r12, %rdx
	addq	%rbx, %rdx
	movq	%rdx, %rcx
	xorq	%rbx, %rcx
	andq	%rax, %rcx
	xorq	%rbx, %rcx
	movq	128(%rsp), %rsi         # 8-byte Reload
	movl	24(%rsi), %esi
	addq	%rsi, %r11
	leaq	-1069501632(%rcx,%r11), %r12
	movq	%r12, %rsi
	movq	$9, %rcx
	shlq	%cl, %r12
	movq	$23, %rcx
	movabsq	$4286578688, %r13       # imm = 0xFF800000
	andq	%r13, %rsi
	shrq	%cl, %rsi
	orq	%r12, %rsi
	addq	%rdx, %rsi
	movq	%rsi, %rcx
	xorq	%rdx, %rcx
	andq	%rbx, %rcx
	xorq	%rdx, %rcx
	addq	%rdi, %rax
	leaq	643717713(%rcx,%rax), %r12
	movq	%r12, %rax
	movq	$14, %rcx
	shlq	%cl, %r12
	movq	$18, %rcx
	movabsq	$4294705152, %r8        # imm = 0xFFFC0000
	andq	%r8, %rax
	shrq	%cl, %rax
	orq	%r12, %rax
	addq	%rsi, %rax
	movq	%rax, %rcx
	xorq	%rsi, %rcx
	andq	%rdx, %rcx
	xorq	%rsi, %rcx
	movq	48(%rsp), %rdi          # 8-byte Reload
	addq	%rdi, %rbx
	leaq	-373897302(%rcx,%rbx), %r12
	movq	%r12, %rbx
	movq	$20, %rcx
	shlq	%cl, %r12
	movq	$12, %rcx
	movabsq	$4294963200, %r9        # imm = 0xFFFFF000
	movq	%r9, %r10
	andq	%r10, %rbx
	shrq	%cl, %rbx
	orq	%r12, %rbx
	addq	%rax, %rbx
	movq	%rbx, %rcx
	xorq	%rax, %rcx
	andq	%rsi, %rcx
	xorq	%rax, %rcx
	movq	64(%rsp), %rdi          # 8-byte Reload
	addq	%rdi, %rdx
	leaq	-701558691(%rcx,%rdx), %r12
	movq	%r12, %rdx
	movq	$5, %rcx
	shlq	%cl, %r12
	movq	$27, %rcx
	andq	%r15, %rdx
	shrq	%cl, %rdx
	orq	%r12, %rdx
	addq	%rbx, %rdx
	movq	%rdx, %rcx
	xorq	%rbx, %rcx
	andq	%rax, %rcx
	xorq	%rbx, %rcx
	movq	120(%rsp), %rdi         # 8-byte Reload
	addq	%rdi, %rsi
	leaq	38016083(%rcx,%rsi), %r12
	movq	%r12, %rsi
	movq	$9, %rcx
	shlq	%cl, %r12
	movq	$23, %rcx
	andq	%r13, %rsi
	shrq	%cl, %rsi
	orq	%r12, %rsi
	addq	%rdx, %rsi
	movq	%rsi, %rcx
	xorq	%rdx, %rcx
	andq	%rbx, %rcx
	xorq	%rdx, %rcx
	addq	%r14, %rax
	leaq	-660478335(%rcx,%rax), %r12
	movq	%r12, %rax
	movq	$14, %rcx
	shlq	%cl, %r12
	movq	$18, %rcx
	andq	%r8, %rax
	shrq	%cl, %rax
	orq	%r12, %rax
	addq	%rsi, %rax
	movq	%rax, %rcx
	xorq	%rsi, %rcx
	andq	%rdx, %rcx
	xorq	%rsi, %rcx
	movq	80(%rsp), %rdi          # 8-byte Reload
	addq	%rdi, %rbx
	leaq	-405537848(%rcx,%rbx), %r12
	movq	%r12, %rbx
	movq	$20, %rcx
	shlq	%cl, %r12
	movq	$12, %rcx
	andq	%r10, %rbx
	shrq	%cl, %rbx
	orq	%r12, %rbx
	addq	%rax, %rbx
	movq	%rbx, %rcx
	xorq	%rax, %rcx
	andq	%rsi, %rcx
	xorq	%rax, %rcx
	movq	112(%rsp), %rdi         # 8-byte Reload
	addq	%rdi, %rdx
	leaq	568446438(%rcx,%rdx), %r12
	movq	%r12, %rdx
	movq	$5, %rcx
	shlq	%cl, %r12
	movq	$27, %rcx
	andq	%r15, %rdx
	shrq	%cl, %rdx
	orq	%r12, %rdx
	addq	%rbx, %rdx
	movq	%rdx, %rcx
	xorq	%rbx, %rcx
	andq	%rax, %rcx
	xorq	%rbx, %rcx
	movq	16(%rsp), %r14          # 8-byte Reload
	addq	%r14, %rsi
	leaq	-1019803690(%rcx,%rsi), %r12
	movq	%r12, %rsi
	movq	$9, %rcx
	shlq	%cl, %r12
	movq	$23, %rcx
	andq	%r13, %rsi
	shrq	%cl, %rsi
	orq	%r12, %rsi
	addq	%rdx, %rsi
	movq	%rsi, %rcx
	xorq	%rdx, %rcx
	andq	%rbx, %rcx
	xorq	%rdx, %rcx
	movq	88(%rsp), %rdi          # 8-byte Reload
	addq	%rdi, %rax
	leaq	-187363961(%rcx,%rax), %r12
	movq	%r12, %rax
	movq	$14, %rcx
	shlq	%cl, %r12
	movq	$18, %rcx
	andq	%r8, %rax
	shrq	%cl, %rax
	orq	%r12, %rax
	addq	%rsi, %rax
	movq	%rax, %rcx
	xorq	%rsi, %rcx
	andq	%rdx, %rcx
	xorq	%rsi, %rcx
	movq	24(%rsp), %r9           # 8-byte Reload
	addq	%r9, %rbx
	leaq	1163531501(%rcx,%rbx), %r12
	movq	%r12, %rbx
	movq	$20, %rcx
	shlq	%cl, %r12
	movq	$12, %rcx
	andq	%r10, %rbx
	shrq	%cl, %rbx
	orq	%r12, %rbx
	addq	%rax, %rbx
	movq	%rbx, %rcx
	xorq	%rax, %rcx
	andq	%rsi, %rcx
	xorq	%rax, %rcx
	movq	72(%rsp), %rdi          # 8-byte Reload
	addq	%rdi, %rdx
	leaq	-1444681467(%rcx,%rdx), %r12
	movq	%r12, %rdx
	movq	$5, %rcx
	shlq	%cl, %r12
	movq	$27, %rcx
	andq	%r15, %rdx
	shrq	%cl, %rdx
	orq	%r12, %rdx
	addq	%rbx, %rdx
	movq	%rdx, %rcx
	xorq	%rbx, %rcx
	andq	%rax, %rcx
	xorq	%rbx, %rcx
	movq	96(%rsp), %rdi          # 8-byte Reload
	addq	%rdi, %rsi
	leaq	-51403784(%rcx,%rsi), %r12
	movq	%r12, %rsi
	movq	$9, %rcx
	shlq	%cl, %r12
	movq	$23, %rcx
	andq	%r13, %rsi
	shrq	%cl, %rsi
	orq	%r12, %rsi
	addq	%rdx, %rsi
	movq	%rsi, %rcx
	xorq	%rdx, %rcx
	andq	%rbx, %rcx
	xorq	%rdx, %rcx
	movq	32(%rsp), %r11          # 8-byte Reload
	addq	%r11, %rax
	leaq	1735328473(%rcx,%rax), %r12
	movq	%r12, %rax
	movq	$14, %rcx
	shlq	%cl, %r12
	movq	$18, %rcx
	andq	%r8, %rax
	shrq	%cl, %rax
	orq	%r12, %rax
	addq	%rsi, %rax
	movq	%rax, %r12
	xorq	%rsi, %r12
	movq	%r12, %rcx
	andq	%rdx, %rcx
	xorq	%rsi, %rcx
	movq	104(%rsp), %rdi         # 8-byte Reload
	addq	%rdi, %rbx
	leaq	-1926607734(%rcx,%rbx), %r13
	movq	%r13, %rbx
	movq	$20, %rcx
	shlq	%cl, %r13
	movq	$12, %rcx
	andq	%r10, %rbx
	shrq	%cl, %rbx
	orq	%r13, %rbx
	addq	%rax, %rbx
	xorq	%rbx, %r12
	movq	64(%rsp), %rcx          # 8-byte Reload
	addq	%rcx, %rdx
	leaq	-378558(%r12,%rdx), %r12
	movq	%r12, %rdx
	movq	$4, %rcx
	shlq	%cl, %r12
	movq	$28, %rcx
	movabsq	$4026531840, %r8        # imm = 0xF0000000
	andq	%r8, %rdx
	shrq	%cl, %rdx
	orq	%r12, %rdx
	movq	%rbx, %rcx
	addq	%rbx, %rdx
	xorq	%rax, %rcx
	xorq	%rdx, %rcx
	addq	%r9, %rsi
	leaq	-2022574463(%rcx,%rsi), %r12
	movq	%r12, %rsi
	movq	$11, %rcx
	shlq	%cl, %r12
	movq	$21, %rcx
	movabsq	$4292870144, %r9        # imm = 0xFFE00000
	andq	%r9, %rsi
	shrq	%cl, %rsi
	orq	%r12, %rsi
	movq	%rdx, %rcx
	addq	%rdx, %rsi
	xorq	%rbx, %rcx
	xorq	%rsi, %rcx
	movq	56(%rsp), %rdi          # 8-byte Reload
	addq	%rdi, %rax
	leaq	1839030562(%rcx,%rax), %r12
	movq	%r12, %r13
	movq	$16, %rcx
	shlq	%cl, %r12
	movq	$16, %rcx
	movabsq	$4294901760, %r15       # imm = 0xFFFF0000
	andq	%r15, %r13
	shrq	%cl, %r13
	orq	%r12, %r13
	movq	%rsi, %rcx
	addq	%rsi, %r13
	xorq	%rdx, %rcx
	xorq	%r13, %rcx
	addq	%r14, %rbx
	leaq	-35309556(%rcx,%rbx), %r12
	movq	%r12, %rbx
	movq	$23, %rcx
	shlq	%cl, %r12
	movq	$9, %rcx
	movabsq	$4294966784, %r10       # imm = 0xFFFFFE00
	andq	%r10, %rbx
	shrq	%cl, %rbx
	orq	%r12, %rbx
	movq	%r13, %rcx
	addq	%r13, %rbx
	xorq	%rsi, %rcx
	xorq	%rbx, %rcx
	movq	128(%rsp), %rax         # 8-byte Reload
	movl	4(%rax), %eax
	addq	%rax, %rdx
	leaq	-1530992060(%rcx,%rdx), %r12
	movq	%r12, %rdx
	movq	$4, %rcx
	shlq	%cl, %r12
	movq	$28, %rcx
	andq	%r8, %rdx
	shrq	%cl, %rdx
	orq	%r12, %rdx
	movq	%rbx, %rcx
	addq	%rbx, %rdx
	xorq	%r13, %rcx
	xorq	%rdx, %rcx
	movq	80(%rsp), %rax          # 8-byte Reload
	addq	%rax, %rsi
	leaq	1272893353(%rcx,%rsi), %r12
	movq	%r12, %rsi
	movq	$11, %rcx
	shlq	%cl, %r12
	movq	$21, %rcx
	andq	%r9, %rsi
	shrq	%cl, %rsi
	orq	%r12, %rsi
	movq	%rdx, %rcx
	addq	%rdx, %rsi
	xorq	%rbx, %rcx
	xorq	%rsi, %rcx
	addq	%r11, %r13
	leaq	-155497632(%rcx,%r13), %r12
	movq	%r12, %rax
	movq	$16, %rcx
	shlq	%cl, %r12
	movq	$16, %rcx
	andq	%r15, %rax
	shrq	%cl, %rax
	orq	%r12, %rax
	movq	%rsi, %rcx
	addq	%rsi, %rax
	xorq	%rdx, %rcx
	xorq	%rax, %rcx
	movq	120(%rsp), %rdi         # 8-byte Reload
	addq	%rdi, %rbx
	leaq	-1094730640(%rcx,%rbx), %r12
	movq	%r12, %rbx
	movq	$23, %rcx
	shlq	%cl, %r12
	movq	$9, %rcx
	andq	%r10, %rbx
	shrq	%cl, %rbx
	orq	%r12, %rbx
	movq	%rax, %rcx
	addq	%rax, %rbx
	xorq	%rsi, %rcx
	xorq	%rbx, %rcx
	movq	72(%rsp), %rdi          # 8-byte Reload
	addq	%rdi, %rdx
	leaq	681279174(%rcx,%rdx), %r12
	movq	%r12, %rdx
	movq	$4, %rcx
	shlq	%cl, %r12
	movq	$28, %rcx
	andq	%r8, %rdx
	shrq	%cl, %rdx
	orq	%r12, %rdx
	movq	%rbx, %rcx
	addq	%rbx, %rdx
	xorq	%rax, %rcx
	xorq	%rdx, %rcx
	movq	48(%rsp), %r14          # 8-byte Reload
	addq	%r14, %rsi
	leaq	-358537222(%rcx,%rsi), %r12
	movq	%r12, %rsi
	movq	$11, %rcx
	shlq	%cl, %r12
	movq	$21, %rcx
	andq	%r9, %rsi
	shrq	%cl, %rsi
	orq	%r12, %rsi
	movq	%rdx, %rcx
	addq	%rdx, %rsi
	xorq	%rbx, %rcx
	xorq	%rsi, %rcx
	movq	88(%rsp), %rdi          # 8-byte Reload
	addq	%rdi, %rax
	leaq	-722521979(%rcx,%rax), %r12
	movq	%r12, %r11
	movq	$16, %rcx
	shlq	%cl, %r12
	movq	$16, %rcx
	andq	%r15, %r11
	shrq	%cl, %r11
	orq	%r12, %r11
	movq	%rsi, %rcx
	addq	%rsi, %r11
	xorq	%rdx, %rcx
	xorq	%r11, %rcx
	movq	128(%rsp), %rax         # 8-byte Reload
	movl	24(%rax), %eax
	addq	%rax, %rbx
	leaq	76029189(%rcx,%rbx), %r12
	movq	%r12, %rbx
	movq	$23, %rcx
	shlq	%cl, %r12
	movq	$9, %rcx
	andq	%r10, %rbx
	shrq	%cl, %rbx
	orq	%r12, %rbx
	movq	%r11, %rcx
	addq	%r11, %rbx
	xorq	%rsi, %rcx
	xorq	%rbx, %rcx
	movq	112(%rsp), %rax         # 8-byte Reload
	addq	%rax, %rdx
	leaq	-640364487(%rcx,%rdx), %r12
	movq	%r12, %rdx
	movq	$4, %rcx
	shlq	%cl, %r12
	movq	$28, %rcx
	andq	%r8, %rdx
	shrq	%cl, %rdx
	orq	%r12, %rdx
	movq	%rbx, %rcx
	addq	%rbx, %rdx
	xorq	%r11, %rcx
	xorq	%rdx, %rcx
	movq	104(%rsp), %rax         # 8-byte Reload
	addq	%rax, %rsi
	leaq	-421815835(%rcx,%rsi), %r12
	movq	%r12, %rsi
	movq	$11, %rcx
	shlq	%cl, %r12
	movq	$21, %rcx
	andq	%r9, %rsi
	shrq	%cl, %rsi
	orq	%r12, %rsi
	movq	%rdx, %rcx
	addq	%rdx, %rsi
	xorq	%rbx, %rcx
	xorq	%rsi, %rcx
	movq	40(%rsp), %rax          # 8-byte Reload
	addq	%rax, %r11
	leaq	530742520(%rcx,%r11), %r12
	movq	%r12, %rax
	movq	$16, %rcx
	shlq	%cl, %r12
	movq	$16, %rcx
	andq	%r15, %rax
	shrq	%cl, %rax
	orq	%r12, %rax
	movq	%rsi, %rcx
	addq	%rsi, %rax
	xorq	%rdx, %rcx
	xorq	%rax, %rcx
	movq	96(%rsp), %rdi          # 8-byte Reload
	addq	%rdi, %rbx
	leaq	-995338651(%rcx,%rbx), %rbx
	movq	%rbx, %r13
	movq	$23, %rcx
	shlq	%cl, %rbx
	movq	$9, %rcx
	andq	%r10, %r13
	movq	-16(%rsp), %r10         # 8-byte Reload
	shrq	%cl, %r13
	orq	%rbx, %r13
	addq	%r14, %rdx
	movq	%rsi, %rcx
	addq	%rax, %r13
	notq	%rcx
	orq	%r13, %rcx
	xorq	%rax, %rcx
	leaq	-198630844(%rcx,%rdx), %rbx
	movq	%rbx, %rdx
	movq	$6, %rcx
	shlq	%cl, %rbx
	movq	$26, %rcx
	movabsq	$4227858432, %r8        # imm = 0xFC000000
	andq	%r8, %rdx
	shrq	%cl, %rdx
	orq	%rbx, %rdx
	movq	32(%rsp), %rcx          # 8-byte Reload
	addq	%rcx, %rsi
	movq	%rax, %rcx
	addq	%r13, %rdx
	notq	%rcx
	orq	%rdx, %rcx
	xorq	%r13, %rcx
	leaq	1126891415(%rcx,%rsi), %rsi
	movq	%rsi, %r12
	movq	$10, %rcx
	shlq	%cl, %rsi
	movq	$22, %rcx
	movabsq	$4290772992, %r9        # imm = 0xFFC00000
	andq	%r9, %r12
	shrq	%cl, %r12
	orq	%rsi, %r12
	movq	16(%rsp), %rcx          # 8-byte Reload
	addq	%rcx, %rax
	movq	%r13, %rcx
	addq	%rdx, %r12
	notq	%rcx
	orq	%r12, %rcx
	xorq	%rdx, %rcx
	leaq	-1416354905(%rcx,%rax), %rax
	movq	%rax, %r14
	movq	$15, %rcx
	shlq	%cl, %rax
	movq	$17, %rcx
	movabsq	$4294836224, %r15       # imm = 0xFFFE0000
	andq	%r15, %r14
	shrq	%cl, %r14
	orq	%rax, %r14
	movq	64(%rsp), %rax          # 8-byte Reload
	addq	%rax, %r13
	movq	%rdx, %rax
	addq	%r12, %r14
	notq	%rax
	orq	%r14, %rax
	xorq	%r12, %rax
	leaq	-57434055(%rax,%r13), %rsi
	movq	%rsi, %rax
	movq	$21, %rcx
	shlq	%cl, %rsi
	movq	$11, %rcx
	movabsq	$4294965248, %r13       # imm = 0xFFFFF800
	andq	%r13, %rax
	shrq	%cl, %rax
	orq	%rsi, %rax
	movq	104(%rsp), %rcx         # 8-byte Reload
	addq	%rcx, %rdx
	movq	%r12, %rcx
	addq	%r14, %rax
	notq	%rcx
	orq	%rax, %rcx
	xorq	%r14, %rcx
	leaq	1700485571(%rcx,%rdx), %rdx
	movq	%rdx, %rsi
	movq	$6, %rcx
	shlq	%cl, %rdx
	movq	$26, %rcx
	andq	%r8, %rsi
	shrq	%cl, %rsi
	orq	%rdx, %rsi
	movq	88(%rsp), %rcx          # 8-byte Reload
	addq	%rcx, %r12
	movq	%r14, %rcx
	addq	%rax, %rsi
	notq	%rcx
	orq	%rsi, %rcx
	xorq	%rax, %rcx
	leaq	-1894986606(%rcx,%r12), %rdx
	movq	%rdx, %rbx
	movq	$10, %rcx
	shlq	%cl, %rdx
	movq	$22, %rcx
	andq	%r9, %rbx
	shrq	%cl, %rbx
	orq	%rdx, %rbx
	addq	%rsi, %rbx
	movq	120(%rsp), %rcx         # 8-byte Reload
	addq	%rcx, %r14
	movq	%rax, %rcx
	notq	%rcx
	orq	%rbx, %rcx
	xorq	%rsi, %rcx
	leaq	-1051523(%rcx,%r14), %r11
	movq	%r11, %r14
	movq	$15, %rcx
	shlq	%cl, %r11
	movq	$17, %rcx
	andq	%r15, %r14
	shrq	%cl, %r14
	orq	%r11, %r14
	addq	%rbx, %r14
	movq	%rsi, %rcx
	notq	%rcx
	orq	%r14, %rcx
	xorq	%rbx, %rcx
	movq	128(%rsp), %rdx         # 8-byte Reload
	movl	4(%rdx), %edx
	addq	%rdx, %rax
	leaq	-2054922799(%rcx,%rax), %r11
	movq	%r11, %rax
	andq	%r13, %rax
	movq	$21, %rcx
	shlq	%cl, %r11
	movq	$11, %rcx
	shrq	%cl, %rax
	orq	%r11, %rax
	addq	%r14, %rax
	movq	%rbx, %rcx
	notq	%rcx
	orq	%rax, %rcx
	xorq	%r14, %rcx
	movq	24(%rsp), %rdx          # 8-byte Reload
	addq	%rdx, %rsi
	leaq	1873313359(%rcx,%rsi), %rsi
	movq	%rsi, %r11
	andq	%r8, %r11
	movq	$6, %rcx
	shlq	%cl, %rsi
	movq	$26, %rcx
	shrq	%cl, %r11
	orq	%rsi, %r11
	addq	%rax, %r11
	movq	%r14, %rcx
	notq	%rcx
	orq	%r11, %rcx
	xorq	%rax, %rcx
	movq	40(%rsp), %rdx          # 8-byte Reload
	addq	%rdx, %rbx
	leaq	-30611744(%rcx,%rbx), %rbx
	movq	%rbx, %rsi
	andq	%r9, %rsi
	movq	$10, %rcx
	shlq	%cl, %rbx
	movq	$22, %rcx
	shrq	%cl, %rsi
	orq	%rbx, %rsi
	addq	%r11, %rsi
	movq	%rax, %rcx
	notq	%rcx
	orq	%rsi, %rcx
	xorq	%r11, %rcx
	movq	128(%rsp), %rdx         # 8-byte Reload
	movl	24(%rdx), %edx
	addq	%rdx, %r14
	leaq	-1560198380(%rcx,%r14), %rdx
	movq	%rdx, %rbx
	andq	%r15, %rbx
	movq	$15, %rcx
	shlq	%cl, %rdx
	movq	$17, %rcx
	shrq	%cl, %rbx
	orq	%rdx, %rbx
	addq	%rsi, %rbx
	movq	%r11, %rcx
	notq	%rcx
	orq	%rbx, %rcx
	xorq	%rsi, %rcx
	movq	72(%rsp), %rdx          # 8-byte Reload
	addq	%rdx, %rax
	leaq	1309151649(%rcx,%rax), %rdx
	movq	%rdx, %rax
	andq	%r13, %rax
	movq	$21, %rcx
	shlq	%cl, %rdx
	movq	$11, %rcx
	shrq	%cl, %rax
	orq	%rdx, %rax
	addq	%rbx, %rax
	movq	%rsi, %rcx
	notq	%rcx
	orq	%rax, %rcx
	xorq	%rbx, %rcx
	movq	80(%rsp), %rdx          # 8-byte Reload
	addq	%rdx, %r11
	leaq	-145523070(%rcx,%r11), %rdx
	movq	%rdx, %r11
	andq	%r8, %r11
	movq	$6, %rcx
	shlq	%cl, %rdx
	movq	$26, %rcx
	shrq	%cl, %r11
	orq	%rdx, %r11
	addq	%rax, %r11
	movq	%rbx, %rcx
	notq	%rcx
	orq	%r11, %rcx
	xorq	%rax, %rcx
	movq	56(%rsp), %rdx          # 8-byte Reload
	addq	%rdx, %rsi
	movq	128(%rsp), %rdi         # 8-byte Reload
	leaq	-1120210379(%rcx,%rsi), %rdx
	movq	%rdx, %rsi
	andq	%r9, %rsi
	movq	$10, %rcx
	shlq	%cl, %rdx
	movq	$22, %rcx
	shrq	%cl, %rsi
	orq	%rdx, %rsi
	addq	%r11, %rsi
	movq	%rax, %rcx
	notq	%rcx
	orq	%rsi, %rcx
	xorq	%r11, %rcx
	movq	96(%rsp), %rdx          # 8-byte Reload
	addq	%rdx, %rbx
	leaq	718787259(%rcx,%rbx), %rdx
	movq	%rdx, %rbx
	andq	%r15, %rbx
	movq	$15, %rcx
	shlq	%cl, %rdx
	movq	$17, %rcx
	shrq	%cl, %rbx
	orq	%rdx, %rbx
	addq	%rsi, %rbx
	movq	%r11, %rcx
	notq	%rcx
	orq	%rbx, %rcx
	xorq	%rsi, %rcx
	movq	112(%rsp), %rdx         # 8-byte Reload
	addq	%rdx, %rax
	leaq	-343485551(%rcx,%rax), %rax
	movq	%rax, %rdx
	andq	%r13, %rdx
	movq	$21, %rcx
	shlq	%cl, %rax
	movq	$11, %rcx
	shrq	%cl, %rdx
	orq	%rax, %rdx
	movq	-8(%rsp), %rax          # 8-byte Reload
	addq	%r11, %rax
	movq	%rax, %r11
	addq	%rbx, %r10
	addq	%rdx, %r10
	movq	(%rsp), %rax            # 8-byte Reload
	addq	%rbx, %rax
	movq	%rax, %r9
	movq	8(%rsp), %rax           # 8-byte Reload
	addq	%rsi, %rax
	movq	%rax, %rbx
	addq	$64, %rdi
	movq	-24(%rsp), %rax         # 8-byte Reload
	cmpq	%rax, %rdi
	jb	.LBB3_4
.LBB3_5:                                # %while.end
	movq	-32(%rsp), %rax         # 8-byte Reload
	movl	%r11d, (%rax)
	movl	%r10d, 4(%rax)
	movl	%r9d, 8(%rax)
	movl	%ebx, 12(%rax)
	addq	$136, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp36:
	.size	md5_process_block, .Ltmp36-md5_process_block
	.cfi_endproc

	.globl	md5_stream
	.align	16, 0x90
	.type	md5_stream,@function
md5_stream:                             # @md5_stream
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp43:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp44:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp45:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp46:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp47:
	.cfi_def_cfa_offset 48
	subq	$4352, %rsp             # imm = 0x1100
.Ltmp48:
	.cfi_def_cfa_offset 4400
.Ltmp49:
	.cfi_offset %rbx, -48
.Ltmp50:
	.cfi_offset %r12, -40
.Ltmp51:
	.cfi_offset %r13, -32
.Ltmp52:
	.cfi_offset %r14, -24
.Ltmp53:
	.cfi_offset %r15, -16
	movq	%rsi, 8(%rsp)           # 8-byte Spill
	movq	%rdi, %r12
	movq	$1732584193, %rax       # imm = 0x67452301
	movl	%eax, 4192(%rsp)
	movabsq	$4023233417, %rax       # imm = 0xEFCDAB89
	movl	%eax, 4196(%rsp)
	movabsq	$2562383102, %rax       # imm = 0x98BADCFE
	movl	%eax, 4200(%rsp)
	movq	$271733878, %rax        # imm = 0x10325476
	movl	%eax, 4204(%rsp)
	xorq	%r14, %r14
	movl	%r14d, 4212(%rsp)
	movl	%r14d, 4208(%rsp)
	movl	%r14d, 4216(%rsp)
	leaq	16(%rsp), %r15
	leaq	4192(%rsp), %r13
	jmp	.LBB4_1
	.align	16, 0x90
.LBB4_22:                               # %if.end6
                                        #   in Loop: Header=BB4_1 Depth=1
	movq	$4096, %rsi             # imm = 0x1000
	movq	%r15, %rdi
	movq	%r13, %rdx
	callq	md5_process_block
	xorq	%r14, %r14
.LBB4_1:                                # %do.body
                                        # =>This Inner Loop Header: Depth=1
	movq	%r14, %rbx
	leaq	16(%rsp,%rbx), %rdi
	movq	$4096, %rdx             # imm = 0x1000
	subq	%rbx, %rdx
	movq	$1, %rsi
	movq	%r12, %rcx
	callq	fread
	movq	%rax, %r14
	addq	%rbx, %r14
	cmpq	$4095, %r14             # imm = 0xFFF
	ja	.LBB4_3
# BB#2:                                 # %do.body
                                        #   in Loop: Header=BB4_1 Depth=1
	testq	%rax, %rax
	jne	.LBB4_1
.LBB4_3:                                # %do.end
                                        #   in Loop: Header=BB4_1 Depth=1
	testq	%rax, %rax
	jne	.LBB4_22
# BB#4:                                 # %land.lhs.true
	movq	%r12, %rdi
	callq	ferror
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	testq	%r13, %rax
	movq	$1, %rax
	jne	.LBB4_21
# BB#5:                                 # %while.end
	testq	%r14, %r14
	je	.LBB4_16
# BB#6:                                 # %if.then9
	movl	4216(%rsp), %r12d
	movq	%r13, %rbx
	testq	%r12, %r12
	je	.LBB4_12
# BB#7:                                 # %if.then.i29
	movq	$128, %rax
	subq	%r12, %rax
	cmpq	%r14, %rax
	movq	%r14, %r13
	ja	.LBB4_9
# BB#8:                                 # %if.then.i29
	movq	%rax, %r13
.LBB4_9:                                # %if.then.i29
	leaq	4220(%rsp,%r12), %rdi
	leaq	16(%rsp), %rsi
	movq	%r13, %rdx
	callq	memcpy
	movl	4216(%rsp), %eax
	addq	%r13, %rax
	movl	%eax, 4216(%rsp)
	addq	%r13, %r12
	cmpq	$65, %r12
	jb	.LBB4_11
# BB#10:                                # %if.then13.i
	leaq	4220(%rsp), %rdi
	movq	%r12, %r15
	andq	$-64, %r15
	leaq	4192(%rsp), %rdx
	movq	%r15, %rsi
	callq	md5_process_block
	leaq	4220(%rsp,%r15), %rsi
	andq	$63, %r12
	leaq	4220(%rsp), %rdi
	movq	%r12, %rdx
	callq	memcpy
	movl	%r12d, 4216(%rsp)
.LBB4_11:                               # %if.end.i
	leaq	16(%rsp,%r13), %r15
	subq	%r13, %r14
.LBB4_12:                               # %if.end28.i
	cmpq	$65, %r14
	movq	%rbx, %r13
	jb	.LBB4_14
# BB#13:                                # %if.then31.i
	movq	%r14, %r12
	andq	$-64, %r12
	leaq	4192(%rsp), %rdx
	movq	%r15, %rdi
	movq	%r12, %rsi
	callq	md5_process_block
	addq	%r12, %r15
	andq	$63, %r14
.LBB4_14:                               # %if.end36.i
	testq	%r14, %r14
	je	.LBB4_16
# BB#15:                                # %if.then39.i
	leaq	4220(%rsp), %rdi
	movq	%r15, %rsi
	movq	%r14, %rdx
	callq	memcpy
	movl	%r14d, 4216(%rsp)
.LBB4_16:                               # %if.end11
	movl	4216(%rsp), %r12d
	movl	4208(%rsp), %eax
	addq	%r12, %rax
	movq	%rax, %rcx
	andq	%r13, %rcx
	cmpq	%rax, %rcx
	movl	%eax, 4208(%rsp)
	je	.LBB4_18
# BB#17:                                # %if.then.i
	movl	4212(%rsp), %eax
	incq	%rax
	movl	%eax, 4212(%rsp)
.LBB4_18:                               # %md5_finish_ctx.exit
	cmpq	$55, %r12
	movq	$120, %rbx
	ja	.LBB4_20
# BB#19:                                # %md5_finish_ctx.exit
	movq	$56, %rbx
.LBB4_20:                               # %md5_finish_ctx.exit
	subq	%r12, %rbx
	andq	%r13, %rbx
	leaq	4220(%rsp,%r12), %rdi
	leaq	4220(%rsp), %r15
	movabsq	$fillbuf, %rsi
	movq	%rbx, %rdx
	callq	memcpy
	movl	4208(%rsp), %eax
	movq	$3, %rcx
	shlq	%cl, %rax
	leaq	(%rbx,%r12), %rdx
	movl	%eax, 4220(%rsp,%rdx)
	movl	4212(%rsp), %eax
	movq	$3, %rcx
	movl	4208(%rsp), %esi
	shlq	%cl, %rax
	movq	$29, %rcx
	shrq	%cl, %rsi
	orq	%rax, %rsi
	movl	%esi, 4224(%rsp,%rdx)
	leaq	8(%rbx,%r12), %rsi
	leaq	4192(%rsp), %rdx
	movq	%r15, %rdi
	callq	md5_process_block
	movl	4192(%rsp), %eax
	movl	4200(%rsp), %ecx
	movl	4196(%rsp), %edx
	movl	4204(%rsp), %esi
	movq	8(%rsp), %rdi           # 8-byte Reload
	movl	%esi, 12(%rdi)
	movl	%ecx, 8(%rdi)
	movl	%edx, 4(%rdi)
	movl	%eax, (%rdi)
	xorq	%rax, %rax
.LBB4_21:                               # %cleanup
	addq	$4352, %rsp             # imm = 0x1100
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp54:
	.size	md5_stream, .Ltmp54-md5_stream
	.cfi_endproc

	.globl	md5_process_bytes
	.align	16, 0x90
	.type	md5_process_bytes,@function
md5_process_bytes:                      # @md5_process_bytes
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp61:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp62:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp63:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp64:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp65:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp66:
	.cfi_def_cfa_offset 64
.Ltmp67:
	.cfi_offset %rbx, -48
.Ltmp68:
	.cfi_offset %r12, -40
.Ltmp69:
	.cfi_offset %r13, -32
.Ltmp70:
	.cfi_offset %r14, -24
.Ltmp71:
	.cfi_offset %r15, -16
	movq	%rdx, %r14
	movl	24(%r14), %ebx
	movq	%rsi, %r12
	movq	%rdi, %r15
	testq	%rbx, %rbx
	je	.LBB5_6
# BB#1:                                 # %if.then
	movq	$128, %rax
	subq	%rbx, %rax
	cmpq	%r12, %rax
	movq	%r12, %r13
	ja	.LBB5_3
# BB#2:                                 # %if.then
	movq	%rax, %r13
.LBB5_3:                                # %if.then
	leaq	28(%r14,%rbx), %rdi
	movq	%r15, %rsi
	movq	%r13, %rdx
	callq	memcpy
	movl	24(%r14), %eax
	addq	%r13, %rax
	movl	%eax, 24(%r14)
	addq	%r13, %rbx
	cmpq	$65, %rbx
	jb	.LBB5_5
# BB#4:                                 # %if.then13
	leaq	28(%r14), %rdi
	movq	%rdi, (%rsp)            # 8-byte Spill
	movq	%r15, 8(%rsp)           # 8-byte Spill
	movq	%rbx, %r15
	andq	$-64, %r15
	movq	%r15, %rsi
	movq	%r14, %rdx
	callq	md5_process_block
	leaq	28(%r14,%r15), %rsi
	movq	8(%rsp), %r15           # 8-byte Reload
	andq	$63, %rbx
	movq	(%rsp), %rdi            # 8-byte Reload
	movq	%rbx, %rdx
	callq	memcpy
	movl	%ebx, 24(%r14)
.LBB5_5:                                # %if.end
	addq	%r13, %r15
	subq	%r13, %r12
.LBB5_6:                                # %if.end28
	cmpq	$65, %r12
	jb	.LBB5_8
# BB#7:                                 # %if.then31
	movq	%r12, %rbx
	andq	$-64, %rbx
	movq	%r15, %rdi
	movq	%rbx, %rsi
	movq	%r14, %rdx
	callq	md5_process_block
	addq	%rbx, %r15
	andq	$63, %r12
.LBB5_8:                                # %if.end36
	testq	%r12, %r12
	je	.LBB5_10
# BB#9:                                 # %if.then39
	leaq	28(%r14), %rdi
	movq	%r15, %rsi
	movq	%r12, %rdx
	callq	memcpy
	movl	%r12d, 24(%r14)
.LBB5_10:                               # %if.end43
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp72:
	.size	md5_process_bytes, .Ltmp72-md5_process_bytes
	.cfi_endproc

	.globl	md5_buffer
	.align	16, 0x90
	.type	md5_buffer,@function
md5_buffer:                             # @md5_buffer
	.cfi_startproc
# BB#0:                                 # %if.end28.i
	pushq	%r15
.Ltmp79:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp80:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp81:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp82:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp83:
	.cfi_def_cfa_offset 48
	subq	$160, %rsp
.Ltmp84:
	.cfi_def_cfa_offset 208
.Ltmp85:
	.cfi_offset %rbx, -48
.Ltmp86:
	.cfi_offset %r12, -40
.Ltmp87:
	.cfi_offset %r13, -32
.Ltmp88:
	.cfi_offset %r14, -24
.Ltmp89:
	.cfi_offset %r15, -16
	movq	%rdx, %r14
	movq	%rsi, %r12
	movq	%rdi, %r15
	xorq	%rax, %rax
	cmpq	$65, %r12
	movq	$1732584193, %rcx       # imm = 0x67452301
	movl	%ecx, (%rsp)
	movabsq	$4023233417, %rcx       # imm = 0xEFCDAB89
	movl	%ecx, 4(%rsp)
	movabsq	$2562383102, %rcx       # imm = 0x98BADCFE
	movl	%ecx, 8(%rsp)
	movq	$271733878, %rcx        # imm = 0x10325476
	movl	%ecx, 12(%rsp)
	movl	%eax, 20(%rsp)
	movl	%eax, 16(%rsp)
	movl	%eax, 24(%rsp)
	jb	.LBB6_2
# BB#1:                                 # %if.then31.i
	movq	%r12, %rbx
	andq	$-64, %rbx
	leaq	(%rsp), %rdx
	movq	%r15, %rdi
	movq	%rbx, %rsi
	callq	md5_process_block
	addq	%rbx, %r15
	andq	$63, %r12
.LBB6_2:                                # %if.end36.i
	testq	%r12, %r12
	je	.LBB6_3
# BB#4:                                 # %if.then39.i
	leaq	28(%rsp), %rdi
	movq	%r15, %rsi
	movq	%r12, %rdx
	callq	memcpy
	movl	%r12d, 24(%rsp)
	jmp	.LBB6_5
.LBB6_3:                                # %if.end36.i.md5_process_bytes.exit_crit_edge
	movl	24(%rsp), %r12d
.LBB6_5:                                # %md5_process_bytes.exit
	movl	16(%rsp), %ecx
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	movq	%r12, %r13
	andq	%rax, %r13
	addq	%r13, %rcx
	movq	%rcx, %rdx
	andq	%rax, %rdx
	cmpq	%rcx, %rdx
	movl	%ecx, 16(%rsp)
	je	.LBB6_7
# BB#6:                                 # %if.then.i
	movl	20(%rsp), %ecx
	incq	%rcx
	movl	%ecx, 20(%rsp)
.LBB6_7:                                # %md5_finish_ctx.exit
	cmpq	$55, %r13
	movq	$120, %rbx
	ja	.LBB6_9
# BB#8:                                 # %md5_finish_ctx.exit
	movq	$56, %rbx
.LBB6_9:                                # %md5_finish_ctx.exit
	subq	%r12, %rbx
	andq	%rax, %rbx
	leaq	28(%rsp,%r13), %rdi
	leaq	28(%rsp), %r15
	movabsq	$fillbuf, %rsi
	movq	%rbx, %rdx
	callq	memcpy
	movl	16(%rsp), %eax
	movq	$3, %rcx
	shlq	%cl, %rax
	leaq	(%rbx,%r13), %rdx
	movl	%eax, 28(%rsp,%rdx)
	movl	20(%rsp), %eax
	movq	$3, %rcx
	movl	16(%rsp), %esi
	shlq	%cl, %rax
	movq	$29, %rcx
	shrq	%cl, %rsi
	orq	%rax, %rsi
	movl	%esi, 32(%rsp,%rdx)
	leaq	8(%rbx,%r13), %rsi
	leaq	(%rsp), %rdx
	movq	%r15, %rdi
	callq	md5_process_block
	movl	(%rsp), %eax
	movl	8(%rsp), %ecx
	movl	4(%rsp), %edx
	movl	12(%rsp), %esi
	movl	%esi, 12(%r14)
	movl	%ecx, 8(%r14)
	movl	%edx, 4(%r14)
	movl	%eax, (%r14)
	movq	%r14, %rax
	addq	$160, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp90:
	.size	md5_buffer, .Ltmp90-md5_buffer
	.cfi_endproc

	.type	fillbuf,@object         # @fillbuf
	.section	.rodata,"a",@progbits
	.align	16
fillbuf:
	.asciz	"\200\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"
	.size	fillbuf, 64


	.ident	"clang version 3.5 (trunk)"
	.section	".note.GNU-stack","",@progbits
