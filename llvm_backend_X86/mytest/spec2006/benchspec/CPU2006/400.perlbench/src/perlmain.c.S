	.file	"perlmain.c.bc"
	.text
	.globl	main
	.align	16, 0x90
	.type	main,@function
main:                                   # @main
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
	movzbl	PL_do_undump(%rip), %eax
	movq	%rsi, %r14
	movq	%rdi, %r15
	xorq	%rbx, %rbx
	movl	%ebx, PL_use_safe_putenv(%rip)
	testq	%rax, %rax
	jne	.LBB0_3
# BB#1:                                 # %if.then
	callq	perl_alloc
	movq	%rax, %rbx
	testq	%rbx, %rbx
	jne	.LBB0_2
# BB#6:                                 # %if.then2
	movq	$1, %rdi
	callq	exit
.LBB0_2:                                # %if.end
	movq	%rbx, %rdi
	callq	perl_construct
	xorq	%rax, %rax
	movl	%eax, PL_perl_destruct_level(%rip)
.LBB0_3:                                # %if.end3
	movzbl	PL_exit_flags(%rip), %eax
	orq	$2, %rax
	movb	%al, PL_exit_flags(%rip)
	movabsq	$xs_init, %rsi
	xorq	%r8, %r8
	movq	%rbx, %rdi
	movq	%r15, %rdx
	movq	%r14, %rcx
	callq	perl_parse
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB0_5
# BB#4:                                 # %if.then7
	movq	%rbx, %rdi
	callq	perl_run
.LBB0_5:                                # %if.end9
	movq	%rbx, %rdi
	callq	perl_destruct
	movq	%rax, %r14
	movq	%rbx, %rdi
	callq	perl_free
	movq	%r14, %rdi
	callq	exit
.Ltmp10:
	.size	main, .Ltmp10-main
	.cfi_endproc

	.align	16, 0x90
	.type	xs_init,@function
xs_init:                                # @xs_init
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
	movabsq	$.L.str1, %rdi
	movabsq	$boot_DynaLoader, %rsi
	movabsq	$.L.str, %rbx
	movq	%rbx, %rdx
	callq	Perl_newXS
	movabsq	$.L.str2, %rdi
	movabsq	$boot_Cwd, %rsi
	movq	%rbx, %rdx
	callq	Perl_newXS
	movabsq	$.L.str3, %rdi
	movabsq	$boot_Data__Dumper, %rsi
	movq	%rbx, %rdx
	callq	Perl_newXS
	movabsq	$.L.str4, %rdi
	movabsq	$boot_Devel__Peek, %rsi
	movq	%rbx, %rdx
	callq	Perl_newXS
	movabsq	$.L.str5, %rdi
	movabsq	$boot_Digest__MD5, %rsi
	movq	%rbx, %rdx
	callq	Perl_newXS
	movabsq	$.L.str6, %rdi
	movabsq	$boot_IO, %rsi
	movq	%rbx, %rdx
	callq	Perl_newXS
	movabsq	$.L.str7, %r14
	movabsq	$boot_MIME__Base64, %r15
	movq	%r14, %rdi
	movq	%r15, %rsi
	movq	%rbx, %rdx
	callq	Perl_newXS
	movabsq	$.L.str8, %rdi
	movabsq	$boot_Sys__Hostname, %rsi
	movq	%rbx, %rdx
	callq	Perl_newXS
	movabsq	$.L.str9, %rdi
	movabsq	$boot_Time__HiRes, %rsi
	movq	%rbx, %rdx
	callq	Perl_newXS
	movabsq	$.L.str10, %rdi
	movabsq	$boot_attrs, %rsi
	movq	%rbx, %rdx
	callq	Perl_newXS
	movabsq	$.L.str11, %rdi
	movabsq	$boot_Storable, %rsi
	movq	%rbx, %rdx
	callq	Perl_newXS
	movabsq	$.L.str12, %rdi
	movabsq	$boot_HTML__Parser, %rsi
	movq	%rbx, %rdx
	callq	Perl_newXS
	movq	%r14, %rdi
	movq	%r15, %rsi
	movq	%rbx, %rdx
	callq	Perl_newXS
	movabsq	$.L.str13, %rdi
	movabsq	$boot_Opcode, %rsi
	movq	%rbx, %rdx
	popq	%rbx
	popq	%r14
	popq	%r15
	jmp	Perl_newXS  # TAILCALL
.Ltmp21:
	.size	xs_init, .Ltmp21-xs_init
	.cfi_endproc

	.type	.L.str,@object          # @.str
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.asciz	"perlmain.c"
	.size	.L.str, 11

	.type	.L.str1,@object         # @.str1
.L.str1:
	.asciz	"DynaLoader::boot_DynaLoader"
	.size	.L.str1, 28

	.type	.L.str2,@object         # @.str2
.L.str2:
	.asciz	"Cwd::bootstrap"
	.size	.L.str2, 15

	.type	.L.str3,@object         # @.str3
.L.str3:
	.asciz	"Data::Dumper::bootstrap"
	.size	.L.str3, 24

	.type	.L.str4,@object         # @.str4
.L.str4:
	.asciz	"Devel::Peek::bootstrap"
	.size	.L.str4, 23

	.type	.L.str5,@object         # @.str5
.L.str5:
	.asciz	"Digest::MD5::bootstrap"
	.size	.L.str5, 23

	.type	.L.str6,@object         # @.str6
.L.str6:
	.asciz	"IO::bootstrap"
	.size	.L.str6, 14

	.type	.L.str7,@object         # @.str7
.L.str7:
	.asciz	"MIME::Base64::bootstrap"
	.size	.L.str7, 24

	.type	.L.str8,@object         # @.str8
.L.str8:
	.asciz	"Sys::Hostname::bootstrap"
	.size	.L.str8, 25

	.type	.L.str9,@object         # @.str9
.L.str9:
	.asciz	"Time::HiRes::bootstrap"
	.size	.L.str9, 23

	.type	.L.str10,@object        # @.str10
.L.str10:
	.asciz	"attrs::bootstrap"
	.size	.L.str10, 17

	.type	.L.str11,@object        # @.str11
.L.str11:
	.asciz	"Storable::bootstrap"
	.size	.L.str11, 20

	.type	.L.str12,@object        # @.str12
.L.str12:
	.asciz	"HTML::Parser::bootstrap"
	.size	.L.str12, 24

	.type	.L.str13,@object        # @.str13
.L.str13:
	.asciz	"Opcode::bootstrap"
	.size	.L.str13, 18


	.ident	"clang version 3.5 (trunk)"
	.section	".note.GNU-stack","",@progbits
