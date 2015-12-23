	.file	"globals.c.bc"
	.text
	.globl	Perl_fprintf_nocontext
	.align	16, 0x90
	.type	Perl_fprintf_nocontext,@function
Perl_fprintf_nocontext:                 # @Perl_fprintf_nocontext
	.cfi_startproc
# BB#0:                                 # %entry
	subq	$72, %rsp
.Ltmp1:
	.cfi_def_cfa_offset 80
	movq	%r9, 40(%rsp)
	movq	%r8, 32(%rsp)
	movq	%rcx, 24(%rsp)
	movq	%rdx, 16(%rsp)
	leaq	(%rsp), %rax
	movq	%rax, 64(%rsp)
	leaq	80(%rsp), %rax
	movq	%rax, 56(%rsp)
	movq	$48, 52(%rsp)
	movq	$16, 48(%rsp)
	leaq	48(%rsp), %rdx
	callq	PerlIO_vprintf
	addq	$72, %rsp
	retq
.Ltmp2:
	.size	Perl_fprintf_nocontext, .Ltmp2-Perl_fprintf_nocontext
	.cfi_endproc

	.globl	Perl_printf_nocontext
	.align	16, 0x90
	.type	Perl_printf_nocontext,@function
Perl_printf_nocontext:                  # @Perl_printf_nocontext
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp5:
	.cfi_def_cfa_offset 16
	subq	$80, %rsp
.Ltmp6:
	.cfi_def_cfa_offset 96
.Ltmp7:
	.cfi_offset %rbx, -16
	movq	%rdi, %rbx
	movq	%r9, 40(%rsp)
	movq	%r8, 32(%rsp)
	movq	%rcx, 24(%rsp)
	movq	%rdx, 16(%rsp)
	movq	%rsi, 8(%rsp)
	leaq	(%rsp), %rax
	movq	%rax, 64(%rsp)
	leaq	96(%rsp), %rax
	movq	%rax, 56(%rsp)
	movq	$48, 52(%rsp)
	movq	$8, 48(%rsp)
	callq	Perl_PerlIO_stdout
	leaq	48(%rsp), %rdx
	movq	%rax, %rdi
	movq	%rbx, %rsi
	callq	PerlIO_vprintf
	addq	$80, %rsp
	popq	%rbx
	retq
.Ltmp8:
	.size	Perl_printf_nocontext, .Ltmp8-Perl_printf_nocontext
	.cfi_endproc

	.type	PL_utf8skip,@object     # @PL_utf8skip
	.section	.rodata,"a",@progbits
	.globl	PL_utf8skip
	.align	16
PL_utf8skip:
	.ascii	"\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\002\002\002\002\002\002\002\002\002\002\002\002\002\002\002\002\002\002\002\002\002\002\002\002\002\002\002\002\002\002\002\002\003\003\003\003\003\003\003\003\003\003\003\003\003\003\003\003\004\004\004\004\004\004\004\004\005\005\005\005\006\006\007\r"
	.size	PL_utf8skip, 256

	.type	PL_warn_uninit,@object  # @PL_warn_uninit
	.globl	PL_warn_uninit
	.align	16
PL_warn_uninit:
	.asciz	"Use of uninitialized value%s%s"
	.size	PL_warn_uninit, 31

	.type	PL_warn_nosemi,@object  # @PL_warn_nosemi
	.globl	PL_warn_nosemi
	.align	16
PL_warn_nosemi:
	.asciz	"Semicolon seems to be missing"
	.size	PL_warn_nosemi, 30

	.type	PL_warn_reserved,@object # @PL_warn_reserved
	.globl	PL_warn_reserved
	.align	16
PL_warn_reserved:
	.asciz	"Unquoted string \"%s\" may clash with future reserved word"
	.size	PL_warn_reserved, 57

	.type	PL_warn_nl,@object      # @PL_warn_nl
	.globl	PL_warn_nl
	.align	16
PL_warn_nl:
	.asciz	"Unsuccessful %s on filename containing newline"
	.size	PL_warn_nl, 47

	.type	PL_no_wrongref,@object  # @PL_no_wrongref
	.globl	PL_no_wrongref
	.align	16
PL_no_wrongref:
	.asciz	"Can't use %s ref as %s ref"
	.size	PL_no_wrongref, 27

	.type	PL_no_symref,@object    # @PL_no_symref
	.globl	PL_no_symref
	.align	16
PL_no_symref:
	.asciz	"Can't use string (\"%.32s\") as %s ref while \"strict refs\" in use"
	.size	PL_no_symref, 64

	.type	PL_no_usym,@object      # @PL_no_usym
	.globl	PL_no_usym
	.align	16
PL_no_usym:
	.asciz	"Can't use an undefined value as %s reference"
	.size	PL_no_usym, 45

	.type	PL_no_aelem,@object     # @PL_no_aelem
	.globl	PL_no_aelem
	.align	16
PL_no_aelem:
	.asciz	"Modification of non-creatable array value attempted, subscript %d"
	.size	PL_no_aelem, 66

	.type	PL_no_helem,@object     # @PL_no_helem
	.globl	PL_no_helem
	.align	16
PL_no_helem:
	.asciz	"Modification of non-creatable hash value attempted, subscript \"%s\""
	.size	PL_no_helem, 67

	.type	PL_no_modify,@object    # @PL_no_modify
	.globl	PL_no_modify
	.align	16
PL_no_modify:
	.asciz	"Modification of a read-only value attempted"
	.size	PL_no_modify, 44

	.type	PL_no_mem,@object       # @PL_no_mem
	.globl	PL_no_mem
	.align	16
PL_no_mem:
	.asciz	"Out of memory!\n"
	.size	PL_no_mem, 16

	.type	PL_no_security,@object  # @PL_no_security
	.globl	PL_no_security
	.align	16
PL_no_security:
	.asciz	"Insecure dependency in %s%s"
	.size	PL_no_security, 28

	.type	PL_no_sock_func,@object # @PL_no_sock_func
	.globl	PL_no_sock_func
	.align	16
PL_no_sock_func:
	.asciz	"Unsupported socket function \"%s\" called"
	.size	PL_no_sock_func, 40

	.type	PL_no_dir_func,@object  # @PL_no_dir_func
	.globl	PL_no_dir_func
	.align	16
PL_no_dir_func:
	.asciz	"Unsupported directory function \"%s\" called"
	.size	PL_no_dir_func, 43

	.type	PL_no_func,@object      # @PL_no_func
	.globl	PL_no_func
	.align	16
PL_no_func:
	.asciz	"The %s function is unimplemented"
	.size	PL_no_func, 33

	.type	PL_no_myglob,@object    # @PL_no_myglob
	.globl	PL_no_myglob
	.align	16
PL_no_myglob:
	.asciz	"\"my\" variable %s can't be in a package"
	.size	PL_no_myglob, 39

	.type	PL_no_localize_ref,@object # @PL_no_localize_ref
	.globl	PL_no_localize_ref
	.align	16
PL_no_localize_ref:
	.asciz	"Can't localize through a reference"
	.size	PL_no_localize_ref, 35

	.type	PL_uuemap,@object       # @PL_uuemap
	.globl	PL_uuemap
	.align	16
PL_uuemap:
	.asciz	"`!\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_"
	.size	PL_uuemap, 65

	.type	.L.str,@object          # @.str
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.asciz	"ZERO"
	.size	.L.str, 5

	.type	.L.str1,@object         # @.str1
.L.str1:
	.asciz	"HUP"
	.size	.L.str1, 4

	.type	.L.str2,@object         # @.str2
.L.str2:
	.asciz	"INT"
	.size	.L.str2, 4

	.type	.L.str3,@object         # @.str3
.L.str3:
	.asciz	"QUIT"
	.size	.L.str3, 5

	.type	.L.str4,@object         # @.str4
.L.str4:
	.asciz	"ILL"
	.size	.L.str4, 4

	.type	.L.str5,@object         # @.str5
.L.str5:
	.asciz	"TRAP"
	.size	.L.str5, 5

	.type	.L.str6,@object         # @.str6
.L.str6:
	.asciz	"ABRT"
	.size	.L.str6, 5

	.type	.L.str7,@object         # @.str7
.L.str7:
	.asciz	"BUS"
	.size	.L.str7, 4

	.type	.L.str8,@object         # @.str8
.L.str8:
	.asciz	"FPE"
	.size	.L.str8, 4

	.type	.L.str9,@object         # @.str9
.L.str9:
	.asciz	"KILL"
	.size	.L.str9, 5

	.type	.L.str10,@object        # @.str10
.L.str10:
	.asciz	"USR1"
	.size	.L.str10, 5

	.type	.L.str11,@object        # @.str11
.L.str11:
	.asciz	"SEGV"
	.size	.L.str11, 5

	.type	.L.str12,@object        # @.str12
.L.str12:
	.asciz	"USR2"
	.size	.L.str12, 5

	.type	.L.str13,@object        # @.str13
.L.str13:
	.asciz	"PIPE"
	.size	.L.str13, 5

	.type	.L.str14,@object        # @.str14
.L.str14:
	.asciz	"ALRM"
	.size	.L.str14, 5

	.type	.L.str15,@object        # @.str15
.L.str15:
	.asciz	"TERM"
	.size	.L.str15, 5

	.type	PL_sig_name,@object     # @PL_sig_name
	.data
	.globl	PL_sig_name
	.align	16
PL_sig_name:
	.quad	.L.str
	.quad	.L.str1
	.quad	.L.str2
	.quad	.L.str3
	.quad	.L.str4
	.quad	.L.str5
	.quad	.L.str6
	.quad	.L.str7
	.quad	.L.str8
	.quad	.L.str9
	.quad	.L.str10
	.quad	.L.str11
	.quad	.L.str12
	.quad	.L.str13
	.quad	.L.str14
	.quad	.L.str15
	.quad	0
	.size	PL_sig_name, 136

	.type	PL_sig_num,@object      # @PL_sig_num
	.globl	PL_sig_num
	.align	16
PL_sig_num:
	.long	0                       # 0x0
	.long	1                       # 0x1
	.long	2                       # 0x2
	.long	3                       # 0x3
	.long	4                       # 0x4
	.long	5                       # 0x5
	.long	6                       # 0x6
	.long	7                       # 0x7
	.long	8                       # 0x8
	.long	9                       # 0x9
	.long	10                      # 0xa
	.long	11                      # 0xb
	.long	12                      # 0xc
	.long	13                      # 0xd
	.long	14                      # 0xe
	.long	15                      # 0xf
	.long	0                       # 0x0
	.size	PL_sig_num, 68

	.type	PL_fold,@object         # @PL_fold
	.section	.rodata,"a",@progbits
	.globl	PL_fold
	.align	16
PL_fold:
	.ascii	"\000\001\002\003\004\005\006\007\b\t\n\013\f\r\016\017\020\021\022\023\024\025\026\027\030\031\032\033\034\035\036\037 !\"#$%&'()*+,-./0123456789:;<=>?@abcdefghijklmnopqrstuvwxyz[\\]^_`ABCDEFGHIJKLMNOPQRSTUVWXYZ{|}~\177\200\201\202\203\204\205\206\207\210\211\212\213\214\215\216\217\220\221\222\223\224\225\226\227\230\231\232\233\234\235\236\237\240\241\242\243\244\245\246\247\250\251\252\253\254\255\256\257\260\261\262\263\264\265\266\267\270\271\272\273\274\275\276\277\300\301\302\303\304\305\306\307\310\311\312\313\314\315\316\317\320\321\322\323\324\325\326\327\330\331\332\333\334\335\336\337\340\341\342\343\344\345\346\347\350\351\352\353\354\355\356\357\360\361\362\363\364\365\366\367\370\371\372\373\374\375\376\377"
	.size	PL_fold, 256

	.type	PL_fold_locale,@object  # @PL_fold_locale
	.data
	.globl	PL_fold_locale
	.align	16
PL_fold_locale:
	.ascii	"\000\001\002\003\004\005\006\007\b\t\n\013\f\r\016\017\020\021\022\023\024\025\026\027\030\031\032\033\034\035\036\037 !\"#$%&'()*+,-./0123456789:;<=>?@abcdefghijklmnopqrstuvwxyz[\\]^_`ABCDEFGHIJKLMNOPQRSTUVWXYZ{|}~\177\200\201\202\203\204\205\206\207\210\211\212\213\214\215\216\217\220\221\222\223\224\225\226\227\230\231\232\233\234\235\236\237\240\241\242\243\244\245\246\247\250\251\252\253\254\255\256\257\260\261\262\263\264\265\266\267\270\271\272\273\274\275\276\277\300\301\302\303\304\305\306\307\310\311\312\313\314\315\316\317\320\321\322\323\324\325\326\327\330\331\332\333\334\335\336\337\340\341\342\343\344\345\346\347\350\351\352\353\354\355\356\357\360\361\362\363\364\365\366\367\370\371\372\373\374\375\376\377"
	.size	PL_fold_locale, 256

	.type	PL_freq,@object         # @PL_freq
	.section	.rodata,"a",@progbits
	.globl	PL_freq
	.align	16
PL_freq:
	.ascii	"\001\002T\227\232\233\234\235\245\366\372\003\236\007\022\035(3>IU`kv\201\214\223\224\225\226\230\231\377\266\340\315\256\260\264\331\351\350\354\273\353\344\352\342\336\333\323\303\274\301\271\270\277\267\311\345\265\334\302\242\243\320\272\312\310\332\306\263\262\326\246\252\317\307\321\316\314\240\324\330\327\300\257\255\363\254\241\276\313\275\244\346\247\370\343\364\362\377\361\347\360\375\251\322\365\355\371\367\357\250\374\373\376\356\337\335\325\341\261\305\253\304\237\004\005\006\b\t\n\013\f\r\016\017\020\021\023\024\025\026\027\030\031\032\033\034\036\037 !\"#$%&')*+,-./012456789:;<=?@ABCDEFGHJKLMNOPQRSVWXYZ[\\]^_abcdefghijlmnopqrstuwxyz{|}~\177\200\202\203\204\205\206\207\210\211\212\213\215\216\217\220\221\222"
	.size	PL_freq, 256

	.type	.L.str16,@object        # @.str16
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str16:
	.asciz	"null"
	.size	.L.str16, 5

	.type	.L.str17,@object        # @.str17
.L.str17:
	.asciz	"stub"
	.size	.L.str17, 5

	.type	.L.str18,@object        # @.str18
.L.str18:
	.asciz	"scalar"
	.size	.L.str18, 7

	.type	.L.str19,@object        # @.str19
.L.str19:
	.asciz	"pushmark"
	.size	.L.str19, 9

	.type	.L.str20,@object        # @.str20
.L.str20:
	.asciz	"wantarray"
	.size	.L.str20, 10

	.type	.L.str21,@object        # @.str21
.L.str21:
	.asciz	"const"
	.size	.L.str21, 6

	.type	.L.str22,@object        # @.str22
.L.str22:
	.asciz	"gvsv"
	.size	.L.str22, 5

	.type	.L.str23,@object        # @.str23
.L.str23:
	.asciz	"gv"
	.size	.L.str23, 3

	.type	.L.str24,@object        # @.str24
.L.str24:
	.asciz	"gelem"
	.size	.L.str24, 6

	.type	.L.str25,@object        # @.str25
.L.str25:
	.asciz	"padsv"
	.size	.L.str25, 6

	.type	.L.str26,@object        # @.str26
.L.str26:
	.asciz	"padav"
	.size	.L.str26, 6

	.type	.L.str27,@object        # @.str27
.L.str27:
	.asciz	"padhv"
	.size	.L.str27, 6

	.type	.L.str28,@object        # @.str28
.L.str28:
	.asciz	"padany"
	.size	.L.str28, 7

	.type	.L.str29,@object        # @.str29
.L.str29:
	.asciz	"pushre"
	.size	.L.str29, 7

	.type	.L.str30,@object        # @.str30
.L.str30:
	.asciz	"rv2gv"
	.size	.L.str30, 6

	.type	.L.str31,@object        # @.str31
.L.str31:
	.asciz	"rv2sv"
	.size	.L.str31, 6

	.type	.L.str32,@object        # @.str32
.L.str32:
	.asciz	"av2arylen"
	.size	.L.str32, 10

	.type	.L.str33,@object        # @.str33
.L.str33:
	.asciz	"rv2cv"
	.size	.L.str33, 6

	.type	.L.str34,@object        # @.str34
.L.str34:
	.asciz	"anoncode"
	.size	.L.str34, 9

	.type	.L.str35,@object        # @.str35
.L.str35:
	.asciz	"prototype"
	.size	.L.str35, 10

	.type	.L.str36,@object        # @.str36
.L.str36:
	.asciz	"refgen"
	.size	.L.str36, 7

	.type	.L.str37,@object        # @.str37
.L.str37:
	.asciz	"srefgen"
	.size	.L.str37, 8

	.type	.L.str38,@object        # @.str38
.L.str38:
	.asciz	"ref"
	.size	.L.str38, 4

	.type	.L.str39,@object        # @.str39
.L.str39:
	.asciz	"bless"
	.size	.L.str39, 6

	.type	.L.str40,@object        # @.str40
.L.str40:
	.asciz	"backtick"
	.size	.L.str40, 9

	.type	.L.str41,@object        # @.str41
.L.str41:
	.asciz	"glob"
	.size	.L.str41, 5

	.type	.L.str42,@object        # @.str42
.L.str42:
	.asciz	"readline"
	.size	.L.str42, 9

	.type	.L.str43,@object        # @.str43
.L.str43:
	.asciz	"rcatline"
	.size	.L.str43, 9

	.type	.L.str44,@object        # @.str44
.L.str44:
	.asciz	"regcmaybe"
	.size	.L.str44, 10

	.type	.L.str45,@object        # @.str45
.L.str45:
	.asciz	"regcreset"
	.size	.L.str45, 10

	.type	.L.str46,@object        # @.str46
.L.str46:
	.asciz	"regcomp"
	.size	.L.str46, 8

	.type	.L.str47,@object        # @.str47
.L.str47:
	.asciz	"match"
	.size	.L.str47, 6

	.type	.L.str48,@object        # @.str48
.L.str48:
	.asciz	"qr"
	.size	.L.str48, 3

	.type	.L.str49,@object        # @.str49
.L.str49:
	.asciz	"subst"
	.size	.L.str49, 6

	.type	.L.str50,@object        # @.str50
.L.str50:
	.asciz	"substcont"
	.size	.L.str50, 10

	.type	.L.str51,@object        # @.str51
.L.str51:
	.asciz	"trans"
	.size	.L.str51, 6

	.type	.L.str52,@object        # @.str52
.L.str52:
	.asciz	"sassign"
	.size	.L.str52, 8

	.type	.L.str53,@object        # @.str53
.L.str53:
	.asciz	"aassign"
	.size	.L.str53, 8

	.type	.L.str54,@object        # @.str54
.L.str54:
	.asciz	"chop"
	.size	.L.str54, 5

	.type	.L.str55,@object        # @.str55
.L.str55:
	.asciz	"schop"
	.size	.L.str55, 6

	.type	.L.str56,@object        # @.str56
.L.str56:
	.asciz	"chomp"
	.size	.L.str56, 6

	.type	.L.str57,@object        # @.str57
.L.str57:
	.asciz	"schomp"
	.size	.L.str57, 7

	.type	.L.str58,@object        # @.str58
.L.str58:
	.asciz	"defined"
	.size	.L.str58, 8

	.type	.L.str59,@object        # @.str59
.L.str59:
	.asciz	"undef"
	.size	.L.str59, 6

	.type	.L.str60,@object        # @.str60
.L.str60:
	.asciz	"study"
	.size	.L.str60, 6

	.type	.L.str61,@object        # @.str61
.L.str61:
	.asciz	"pos"
	.size	.L.str61, 4

	.type	.L.str62,@object        # @.str62
.L.str62:
	.asciz	"preinc"
	.size	.L.str62, 7

	.type	.L.str63,@object        # @.str63
.L.str63:
	.asciz	"i_preinc"
	.size	.L.str63, 9

	.type	.L.str64,@object        # @.str64
.L.str64:
	.asciz	"predec"
	.size	.L.str64, 7

	.type	.L.str65,@object        # @.str65
.L.str65:
	.asciz	"i_predec"
	.size	.L.str65, 9

	.type	.L.str66,@object        # @.str66
.L.str66:
	.asciz	"postinc"
	.size	.L.str66, 8

	.type	.L.str67,@object        # @.str67
.L.str67:
	.asciz	"i_postinc"
	.size	.L.str67, 10

	.type	.L.str68,@object        # @.str68
.L.str68:
	.asciz	"postdec"
	.size	.L.str68, 8

	.type	.L.str69,@object        # @.str69
.L.str69:
	.asciz	"i_postdec"
	.size	.L.str69, 10

	.type	.L.str70,@object        # @.str70
.L.str70:
	.asciz	"pow"
	.size	.L.str70, 4

	.type	.L.str71,@object        # @.str71
.L.str71:
	.asciz	"multiply"
	.size	.L.str71, 9

	.type	.L.str72,@object        # @.str72
.L.str72:
	.asciz	"i_multiply"
	.size	.L.str72, 11

	.type	.L.str73,@object        # @.str73
.L.str73:
	.asciz	"divide"
	.size	.L.str73, 7

	.type	.L.str74,@object        # @.str74
.L.str74:
	.asciz	"i_divide"
	.size	.L.str74, 9

	.type	.L.str75,@object        # @.str75
.L.str75:
	.asciz	"modulo"
	.size	.L.str75, 7

	.type	.L.str76,@object        # @.str76
.L.str76:
	.asciz	"i_modulo"
	.size	.L.str76, 9

	.type	.L.str77,@object        # @.str77
.L.str77:
	.asciz	"repeat"
	.size	.L.str77, 7

	.type	.L.str78,@object        # @.str78
.L.str78:
	.asciz	"add"
	.size	.L.str78, 4

	.type	.L.str79,@object        # @.str79
.L.str79:
	.asciz	"i_add"
	.size	.L.str79, 6

	.type	.L.str80,@object        # @.str80
.L.str80:
	.asciz	"subtract"
	.size	.L.str80, 9

	.type	.L.str81,@object        # @.str81
.L.str81:
	.asciz	"i_subtract"
	.size	.L.str81, 11

	.type	.L.str82,@object        # @.str82
.L.str82:
	.asciz	"concat"
	.size	.L.str82, 7

	.type	.L.str83,@object        # @.str83
.L.str83:
	.asciz	"stringify"
	.size	.L.str83, 10

	.type	.L.str84,@object        # @.str84
.L.str84:
	.asciz	"left_shift"
	.size	.L.str84, 11

	.type	.L.str85,@object        # @.str85
.L.str85:
	.asciz	"right_shift"
	.size	.L.str85, 12

	.type	.L.str86,@object        # @.str86
.L.str86:
	.asciz	"lt"
	.size	.L.str86, 3

	.type	.L.str87,@object        # @.str87
.L.str87:
	.asciz	"i_lt"
	.size	.L.str87, 5

	.type	.L.str88,@object        # @.str88
.L.str88:
	.asciz	"gt"
	.size	.L.str88, 3

	.type	.L.str89,@object        # @.str89
.L.str89:
	.asciz	"i_gt"
	.size	.L.str89, 5

	.type	.L.str90,@object        # @.str90
.L.str90:
	.asciz	"le"
	.size	.L.str90, 3

	.type	.L.str91,@object        # @.str91
.L.str91:
	.asciz	"i_le"
	.size	.L.str91, 5

	.type	.L.str92,@object        # @.str92
.L.str92:
	.asciz	"ge"
	.size	.L.str92, 3

	.type	.L.str93,@object        # @.str93
.L.str93:
	.asciz	"i_ge"
	.size	.L.str93, 5

	.type	.L.str94,@object        # @.str94
.L.str94:
	.asciz	"eq"
	.size	.L.str94, 3

	.type	.L.str95,@object        # @.str95
.L.str95:
	.asciz	"i_eq"
	.size	.L.str95, 5

	.type	.L.str96,@object        # @.str96
.L.str96:
	.asciz	"ne"
	.size	.L.str96, 3

	.type	.L.str97,@object        # @.str97
.L.str97:
	.asciz	"i_ne"
	.size	.L.str97, 5

	.type	.L.str98,@object        # @.str98
.L.str98:
	.asciz	"ncmp"
	.size	.L.str98, 5

	.type	.L.str99,@object        # @.str99
.L.str99:
	.asciz	"i_ncmp"
	.size	.L.str99, 7

	.type	.L.str100,@object       # @.str100
.L.str100:
	.asciz	"slt"
	.size	.L.str100, 4

	.type	.L.str101,@object       # @.str101
.L.str101:
	.asciz	"sgt"
	.size	.L.str101, 4

	.type	.L.str102,@object       # @.str102
.L.str102:
	.asciz	"sle"
	.size	.L.str102, 4

	.type	.L.str103,@object       # @.str103
.L.str103:
	.asciz	"sge"
	.size	.L.str103, 4

	.type	.L.str104,@object       # @.str104
.L.str104:
	.asciz	"seq"
	.size	.L.str104, 4

	.type	.L.str105,@object       # @.str105
.L.str105:
	.asciz	"sne"
	.size	.L.str105, 4

	.type	.L.str106,@object       # @.str106
.L.str106:
	.asciz	"scmp"
	.size	.L.str106, 5

	.type	.L.str107,@object       # @.str107
.L.str107:
	.asciz	"bit_and"
	.size	.L.str107, 8

	.type	.L.str108,@object       # @.str108
.L.str108:
	.asciz	"bit_xor"
	.size	.L.str108, 8

	.type	.L.str109,@object       # @.str109
.L.str109:
	.asciz	"bit_or"
	.size	.L.str109, 7

	.type	.L.str110,@object       # @.str110
.L.str110:
	.asciz	"negate"
	.size	.L.str110, 7

	.type	.L.str111,@object       # @.str111
.L.str111:
	.asciz	"i_negate"
	.size	.L.str111, 9

	.type	.L.str112,@object       # @.str112
.L.str112:
	.asciz	"not"
	.size	.L.str112, 4

	.type	.L.str113,@object       # @.str113
.L.str113:
	.asciz	"complement"
	.size	.L.str113, 11

	.type	.L.str114,@object       # @.str114
.L.str114:
	.asciz	"atan2"
	.size	.L.str114, 6

	.type	.L.str115,@object       # @.str115
.L.str115:
	.asciz	"sin"
	.size	.L.str115, 4

	.type	.L.str116,@object       # @.str116
.L.str116:
	.asciz	"cos"
	.size	.L.str116, 4

	.type	.L.str117,@object       # @.str117
.L.str117:
	.asciz	"rand"
	.size	.L.str117, 5

	.type	.L.str118,@object       # @.str118
.L.str118:
	.asciz	"srand"
	.size	.L.str118, 6

	.type	.L.str119,@object       # @.str119
.L.str119:
	.asciz	"exp"
	.size	.L.str119, 4

	.type	.L.str120,@object       # @.str120
.L.str120:
	.asciz	"log"
	.size	.L.str120, 4

	.type	.L.str121,@object       # @.str121
.L.str121:
	.asciz	"sqrt"
	.size	.L.str121, 5

	.type	.L.str122,@object       # @.str122
.L.str122:
	.asciz	"int"
	.size	.L.str122, 4

	.type	.L.str123,@object       # @.str123
.L.str123:
	.asciz	"hex"
	.size	.L.str123, 4

	.type	.L.str124,@object       # @.str124
.L.str124:
	.asciz	"oct"
	.size	.L.str124, 4

	.type	.L.str125,@object       # @.str125
.L.str125:
	.asciz	"abs"
	.size	.L.str125, 4

	.type	.L.str126,@object       # @.str126
.L.str126:
	.asciz	"length"
	.size	.L.str126, 7

	.type	.L.str127,@object       # @.str127
.L.str127:
	.asciz	"substr"
	.size	.L.str127, 7

	.type	.L.str128,@object       # @.str128
.L.str128:
	.asciz	"vec"
	.size	.L.str128, 4

	.type	.L.str129,@object       # @.str129
.L.str129:
	.asciz	"index"
	.size	.L.str129, 6

	.type	.L.str130,@object       # @.str130
.L.str130:
	.asciz	"rindex"
	.size	.L.str130, 7

	.type	.L.str131,@object       # @.str131
.L.str131:
	.asciz	"sprintf"
	.size	.L.str131, 8

	.type	.L.str132,@object       # @.str132
.L.str132:
	.asciz	"formline"
	.size	.L.str132, 9

	.type	.L.str133,@object       # @.str133
.L.str133:
	.asciz	"ord"
	.size	.L.str133, 4

	.type	.L.str134,@object       # @.str134
.L.str134:
	.asciz	"chr"
	.size	.L.str134, 4

	.type	.L.str135,@object       # @.str135
.L.str135:
	.asciz	"crypt"
	.size	.L.str135, 6

	.type	.L.str136,@object       # @.str136
.L.str136:
	.asciz	"ucfirst"
	.size	.L.str136, 8

	.type	.L.str137,@object       # @.str137
.L.str137:
	.asciz	"lcfirst"
	.size	.L.str137, 8

	.type	.L.str138,@object       # @.str138
.L.str138:
	.asciz	"uc"
	.size	.L.str138, 3

	.type	.L.str139,@object       # @.str139
.L.str139:
	.asciz	"lc"
	.size	.L.str139, 3

	.type	.L.str140,@object       # @.str140
.L.str140:
	.asciz	"quotemeta"
	.size	.L.str140, 10

	.type	.L.str141,@object       # @.str141
.L.str141:
	.asciz	"rv2av"
	.size	.L.str141, 6

	.type	.L.str142,@object       # @.str142
.L.str142:
	.asciz	"aelemfast"
	.size	.L.str142, 10

	.type	.L.str143,@object       # @.str143
.L.str143:
	.asciz	"aelem"
	.size	.L.str143, 6

	.type	.L.str144,@object       # @.str144
.L.str144:
	.asciz	"aslice"
	.size	.L.str144, 7

	.type	.L.str145,@object       # @.str145
.L.str145:
	.asciz	"each"
	.size	.L.str145, 5

	.type	.L.str146,@object       # @.str146
.L.str146:
	.asciz	"values"
	.size	.L.str146, 7

	.type	.L.str147,@object       # @.str147
.L.str147:
	.asciz	"keys"
	.size	.L.str147, 5

	.type	.L.str148,@object       # @.str148
.L.str148:
	.asciz	"delete"
	.size	.L.str148, 7

	.type	.L.str149,@object       # @.str149
.L.str149:
	.asciz	"exists"
	.size	.L.str149, 7

	.type	.L.str150,@object       # @.str150
.L.str150:
	.asciz	"rv2hv"
	.size	.L.str150, 6

	.type	.L.str151,@object       # @.str151
.L.str151:
	.asciz	"helem"
	.size	.L.str151, 6

	.type	.L.str152,@object       # @.str152
.L.str152:
	.asciz	"hslice"
	.size	.L.str152, 7

	.type	.L.str153,@object       # @.str153
.L.str153:
	.asciz	"unpack"
	.size	.L.str153, 7

	.type	.L.str154,@object       # @.str154
.L.str154:
	.asciz	"pack"
	.size	.L.str154, 5

	.type	.L.str155,@object       # @.str155
.L.str155:
	.asciz	"split"
	.size	.L.str155, 6

	.type	.L.str156,@object       # @.str156
.L.str156:
	.asciz	"join"
	.size	.L.str156, 5

	.type	.L.str157,@object       # @.str157
.L.str157:
	.asciz	"list"
	.size	.L.str157, 5

	.type	.L.str158,@object       # @.str158
.L.str158:
	.asciz	"lslice"
	.size	.L.str158, 7

	.type	.L.str159,@object       # @.str159
.L.str159:
	.asciz	"anonlist"
	.size	.L.str159, 9

	.type	.L.str160,@object       # @.str160
.L.str160:
	.asciz	"anonhash"
	.size	.L.str160, 9

	.type	.L.str161,@object       # @.str161
.L.str161:
	.asciz	"splice"
	.size	.L.str161, 7

	.type	.L.str162,@object       # @.str162
.L.str162:
	.asciz	"push"
	.size	.L.str162, 5

	.type	.L.str163,@object       # @.str163
.L.str163:
	.asciz	"pop"
	.size	.L.str163, 4

	.type	.L.str164,@object       # @.str164
.L.str164:
	.asciz	"shift"
	.size	.L.str164, 6

	.type	.L.str165,@object       # @.str165
.L.str165:
	.asciz	"unshift"
	.size	.L.str165, 8

	.type	.L.str166,@object       # @.str166
.L.str166:
	.asciz	"sort"
	.size	.L.str166, 5

	.type	.L.str167,@object       # @.str167
.L.str167:
	.asciz	"reverse"
	.size	.L.str167, 8

	.type	.L.str168,@object       # @.str168
.L.str168:
	.asciz	"grepstart"
	.size	.L.str168, 10

	.type	.L.str169,@object       # @.str169
.L.str169:
	.asciz	"grepwhile"
	.size	.L.str169, 10

	.type	.L.str170,@object       # @.str170
.L.str170:
	.asciz	"mapstart"
	.size	.L.str170, 9

	.type	.L.str171,@object       # @.str171
.L.str171:
	.asciz	"mapwhile"
	.size	.L.str171, 9

	.type	.L.str172,@object       # @.str172
.L.str172:
	.asciz	"range"
	.size	.L.str172, 6

	.type	.L.str173,@object       # @.str173
.L.str173:
	.asciz	"flip"
	.size	.L.str173, 5

	.type	.L.str174,@object       # @.str174
.L.str174:
	.asciz	"flop"
	.size	.L.str174, 5

	.type	.L.str175,@object       # @.str175
.L.str175:
	.asciz	"and"
	.size	.L.str175, 4

	.type	.L.str176,@object       # @.str176
.L.str176:
	.asciz	"or"
	.size	.L.str176, 3

	.type	.L.str177,@object       # @.str177
.L.str177:
	.asciz	"xor"
	.size	.L.str177, 4

	.type	.L.str178,@object       # @.str178
.L.str178:
	.asciz	"cond_expr"
	.size	.L.str178, 10

	.type	.L.str179,@object       # @.str179
.L.str179:
	.asciz	"andassign"
	.size	.L.str179, 10

	.type	.L.str180,@object       # @.str180
.L.str180:
	.asciz	"orassign"
	.size	.L.str180, 9

	.type	.L.str181,@object       # @.str181
.L.str181:
	.asciz	"method"
	.size	.L.str181, 7

	.type	.L.str182,@object       # @.str182
.L.str182:
	.asciz	"entersub"
	.size	.L.str182, 9

	.type	.L.str183,@object       # @.str183
.L.str183:
	.asciz	"leavesub"
	.size	.L.str183, 9

	.type	.L.str184,@object       # @.str184
.L.str184:
	.asciz	"leavesublv"
	.size	.L.str184, 11

	.type	.L.str185,@object       # @.str185
.L.str185:
	.asciz	"caller"
	.size	.L.str185, 7

	.type	.L.str186,@object       # @.str186
.L.str186:
	.asciz	"warn"
	.size	.L.str186, 5

	.type	.L.str187,@object       # @.str187
.L.str187:
	.asciz	"die"
	.size	.L.str187, 4

	.type	.L.str188,@object       # @.str188
.L.str188:
	.asciz	"reset"
	.size	.L.str188, 6

	.type	.L.str189,@object       # @.str189
.L.str189:
	.asciz	"lineseq"
	.size	.L.str189, 8

	.type	.L.str190,@object       # @.str190
.L.str190:
	.asciz	"nextstate"
	.size	.L.str190, 10

	.type	.L.str191,@object       # @.str191
.L.str191:
	.asciz	"dbstate"
	.size	.L.str191, 8

	.type	.L.str192,@object       # @.str192
.L.str192:
	.asciz	"unstack"
	.size	.L.str192, 8

	.type	.L.str193,@object       # @.str193
.L.str193:
	.asciz	"enter"
	.size	.L.str193, 6

	.type	.L.str194,@object       # @.str194
.L.str194:
	.asciz	"leave"
	.size	.L.str194, 6

	.type	.L.str195,@object       # @.str195
.L.str195:
	.asciz	"scope"
	.size	.L.str195, 6

	.type	.L.str196,@object       # @.str196
.L.str196:
	.asciz	"enteriter"
	.size	.L.str196, 10

	.type	.L.str197,@object       # @.str197
.L.str197:
	.asciz	"iter"
	.size	.L.str197, 5

	.type	.L.str198,@object       # @.str198
.L.str198:
	.asciz	"enterloop"
	.size	.L.str198, 10

	.type	.L.str199,@object       # @.str199
.L.str199:
	.asciz	"leaveloop"
	.size	.L.str199, 10

	.type	.L.str200,@object       # @.str200
.L.str200:
	.asciz	"return"
	.size	.L.str200, 7

	.type	.L.str201,@object       # @.str201
.L.str201:
	.asciz	"last"
	.size	.L.str201, 5

	.type	.L.str202,@object       # @.str202
.L.str202:
	.asciz	"next"
	.size	.L.str202, 5

	.type	.L.str203,@object       # @.str203
.L.str203:
	.asciz	"redo"
	.size	.L.str203, 5

	.type	.L.str204,@object       # @.str204
.L.str204:
	.asciz	"dump"
	.size	.L.str204, 5

	.type	.L.str205,@object       # @.str205
.L.str205:
	.asciz	"goto"
	.size	.L.str205, 5

	.type	.L.str206,@object       # @.str206
.L.str206:
	.asciz	"exit"
	.size	.L.str206, 5

	.type	.L.str207,@object       # @.str207
.L.str207:
	.asciz	"open"
	.size	.L.str207, 5

	.type	.L.str208,@object       # @.str208
.L.str208:
	.asciz	"close"
	.size	.L.str208, 6

	.type	.L.str209,@object       # @.str209
.L.str209:
	.asciz	"pipe_op"
	.size	.L.str209, 8

	.type	.L.str210,@object       # @.str210
.L.str210:
	.asciz	"fileno"
	.size	.L.str210, 7

	.type	.L.str211,@object       # @.str211
.L.str211:
	.asciz	"umask"
	.size	.L.str211, 6

	.type	.L.str212,@object       # @.str212
.L.str212:
	.asciz	"binmode"
	.size	.L.str212, 8

	.type	.L.str213,@object       # @.str213
.L.str213:
	.asciz	"tie"
	.size	.L.str213, 4

	.type	.L.str214,@object       # @.str214
.L.str214:
	.asciz	"untie"
	.size	.L.str214, 6

	.type	.L.str215,@object       # @.str215
.L.str215:
	.asciz	"tied"
	.size	.L.str215, 5

	.type	.L.str216,@object       # @.str216
.L.str216:
	.asciz	"dbmopen"
	.size	.L.str216, 8

	.type	.L.str217,@object       # @.str217
.L.str217:
	.asciz	"dbmclose"
	.size	.L.str217, 9

	.type	.L.str218,@object       # @.str218
.L.str218:
	.asciz	"sselect"
	.size	.L.str218, 8

	.type	.L.str219,@object       # @.str219
.L.str219:
	.asciz	"select"
	.size	.L.str219, 7

	.type	.L.str220,@object       # @.str220
.L.str220:
	.asciz	"getc"
	.size	.L.str220, 5

	.type	.L.str221,@object       # @.str221
.L.str221:
	.asciz	"read"
	.size	.L.str221, 5

	.type	.L.str222,@object       # @.str222
.L.str222:
	.asciz	"enterwrite"
	.size	.L.str222, 11

	.type	.L.str223,@object       # @.str223
.L.str223:
	.asciz	"leavewrite"
	.size	.L.str223, 11

	.type	.L.str224,@object       # @.str224
.L.str224:
	.asciz	"prtf"
	.size	.L.str224, 5

	.type	.L.str225,@object       # @.str225
.L.str225:
	.asciz	"print"
	.size	.L.str225, 6

	.type	.L.str226,@object       # @.str226
.L.str226:
	.asciz	"sysopen"
	.size	.L.str226, 8

	.type	.L.str227,@object       # @.str227
.L.str227:
	.asciz	"sysseek"
	.size	.L.str227, 8

	.type	.L.str228,@object       # @.str228
.L.str228:
	.asciz	"sysread"
	.size	.L.str228, 8

	.type	.L.str229,@object       # @.str229
.L.str229:
	.asciz	"syswrite"
	.size	.L.str229, 9

	.type	.L.str230,@object       # @.str230
.L.str230:
	.asciz	"send"
	.size	.L.str230, 5

	.type	.L.str231,@object       # @.str231
.L.str231:
	.asciz	"recv"
	.size	.L.str231, 5

	.type	.L.str232,@object       # @.str232
.L.str232:
	.asciz	"eof"
	.size	.L.str232, 4

	.type	.L.str233,@object       # @.str233
.L.str233:
	.asciz	"tell"
	.size	.L.str233, 5

	.type	.L.str234,@object       # @.str234
.L.str234:
	.asciz	"seek"
	.size	.L.str234, 5

	.type	.L.str235,@object       # @.str235
.L.str235:
	.asciz	"truncate"
	.size	.L.str235, 9

	.type	.L.str236,@object       # @.str236
.L.str236:
	.asciz	"fcntl"
	.size	.L.str236, 6

	.type	.L.str237,@object       # @.str237
.L.str237:
	.asciz	"ioctl"
	.size	.L.str237, 6

	.type	.L.str238,@object       # @.str238
.L.str238:
	.asciz	"flock"
	.size	.L.str238, 6

	.type	.L.str239,@object       # @.str239
.L.str239:
	.asciz	"socket"
	.size	.L.str239, 7

	.type	.L.str240,@object       # @.str240
.L.str240:
	.asciz	"sockpair"
	.size	.L.str240, 9

	.type	.L.str241,@object       # @.str241
.L.str241:
	.asciz	"bind"
	.size	.L.str241, 5

	.type	.L.str242,@object       # @.str242
.L.str242:
	.asciz	"connect"
	.size	.L.str242, 8

	.type	.L.str243,@object       # @.str243
.L.str243:
	.asciz	"listen"
	.size	.L.str243, 7

	.type	.L.str244,@object       # @.str244
.L.str244:
	.asciz	"accept"
	.size	.L.str244, 7

	.type	.L.str245,@object       # @.str245
.L.str245:
	.asciz	"shutdown"
	.size	.L.str245, 9

	.type	.L.str246,@object       # @.str246
.L.str246:
	.asciz	"gsockopt"
	.size	.L.str246, 9

	.type	.L.str247,@object       # @.str247
.L.str247:
	.asciz	"ssockopt"
	.size	.L.str247, 9

	.type	.L.str248,@object       # @.str248
.L.str248:
	.asciz	"getsockname"
	.size	.L.str248, 12

	.type	.L.str249,@object       # @.str249
.L.str249:
	.asciz	"getpeername"
	.size	.L.str249, 12

	.type	.L.str250,@object       # @.str250
.L.str250:
	.asciz	"lstat"
	.size	.L.str250, 6

	.type	.L.str251,@object       # @.str251
.L.str251:
	.asciz	"stat"
	.size	.L.str251, 5

	.type	.L.str252,@object       # @.str252
.L.str252:
	.asciz	"ftrread"
	.size	.L.str252, 8

	.type	.L.str253,@object       # @.str253
.L.str253:
	.asciz	"ftrwrite"
	.size	.L.str253, 9

	.type	.L.str254,@object       # @.str254
.L.str254:
	.asciz	"ftrexec"
	.size	.L.str254, 8

	.type	.L.str255,@object       # @.str255
.L.str255:
	.asciz	"fteread"
	.size	.L.str255, 8

	.type	.L.str256,@object       # @.str256
.L.str256:
	.asciz	"ftewrite"
	.size	.L.str256, 9

	.type	.L.str257,@object       # @.str257
.L.str257:
	.asciz	"fteexec"
	.size	.L.str257, 8

	.type	.L.str258,@object       # @.str258
.L.str258:
	.asciz	"ftis"
	.size	.L.str258, 5

	.type	.L.str259,@object       # @.str259
.L.str259:
	.asciz	"fteowned"
	.size	.L.str259, 9

	.type	.L.str260,@object       # @.str260
.L.str260:
	.asciz	"ftrowned"
	.size	.L.str260, 9

	.type	.L.str261,@object       # @.str261
.L.str261:
	.asciz	"ftzero"
	.size	.L.str261, 7

	.type	.L.str262,@object       # @.str262
.L.str262:
	.asciz	"ftsize"
	.size	.L.str262, 7

	.type	.L.str263,@object       # @.str263
.L.str263:
	.asciz	"ftmtime"
	.size	.L.str263, 8

	.type	.L.str264,@object       # @.str264
.L.str264:
	.asciz	"ftatime"
	.size	.L.str264, 8

	.type	.L.str265,@object       # @.str265
.L.str265:
	.asciz	"ftctime"
	.size	.L.str265, 8

	.type	.L.str266,@object       # @.str266
.L.str266:
	.asciz	"ftsock"
	.size	.L.str266, 7

	.type	.L.str267,@object       # @.str267
.L.str267:
	.asciz	"ftchr"
	.size	.L.str267, 6

	.type	.L.str268,@object       # @.str268
.L.str268:
	.asciz	"ftblk"
	.size	.L.str268, 6

	.type	.L.str269,@object       # @.str269
.L.str269:
	.asciz	"ftfile"
	.size	.L.str269, 7

	.type	.L.str270,@object       # @.str270
.L.str270:
	.asciz	"ftdir"
	.size	.L.str270, 6

	.type	.L.str271,@object       # @.str271
.L.str271:
	.asciz	"ftpipe"
	.size	.L.str271, 7

	.type	.L.str272,@object       # @.str272
.L.str272:
	.asciz	"ftlink"
	.size	.L.str272, 7

	.type	.L.str273,@object       # @.str273
.L.str273:
	.asciz	"ftsuid"
	.size	.L.str273, 7

	.type	.L.str274,@object       # @.str274
.L.str274:
	.asciz	"ftsgid"
	.size	.L.str274, 7

	.type	.L.str275,@object       # @.str275
.L.str275:
	.asciz	"ftsvtx"
	.size	.L.str275, 7

	.type	.L.str276,@object       # @.str276
.L.str276:
	.asciz	"fttty"
	.size	.L.str276, 6

	.type	.L.str277,@object       # @.str277
.L.str277:
	.asciz	"fttext"
	.size	.L.str277, 7

	.type	.L.str278,@object       # @.str278
.L.str278:
	.asciz	"ftbinary"
	.size	.L.str278, 9

	.type	.L.str279,@object       # @.str279
.L.str279:
	.asciz	"chdir"
	.size	.L.str279, 6

	.type	.L.str280,@object       # @.str280
.L.str280:
	.asciz	"chown"
	.size	.L.str280, 6

	.type	.L.str281,@object       # @.str281
.L.str281:
	.asciz	"chroot"
	.size	.L.str281, 7

	.type	.L.str282,@object       # @.str282
.L.str282:
	.asciz	"unlink"
	.size	.L.str282, 7

	.type	.L.str283,@object       # @.str283
.L.str283:
	.asciz	"chmod"
	.size	.L.str283, 6

	.type	.L.str284,@object       # @.str284
.L.str284:
	.asciz	"utime"
	.size	.L.str284, 6

	.type	.L.str285,@object       # @.str285
.L.str285:
	.asciz	"rename"
	.size	.L.str285, 7

	.type	.L.str286,@object       # @.str286
.L.str286:
	.asciz	"link"
	.size	.L.str286, 5

	.type	.L.str287,@object       # @.str287
.L.str287:
	.asciz	"symlink"
	.size	.L.str287, 8

	.type	.L.str288,@object       # @.str288
.L.str288:
	.asciz	"readlink"
	.size	.L.str288, 9

	.type	.L.str289,@object       # @.str289
.L.str289:
	.asciz	"mkdir"
	.size	.L.str289, 6

	.type	.L.str290,@object       # @.str290
.L.str290:
	.asciz	"rmdir"
	.size	.L.str290, 6

	.type	.L.str291,@object       # @.str291
.L.str291:
	.asciz	"open_dir"
	.size	.L.str291, 9

	.type	.L.str292,@object       # @.str292
.L.str292:
	.asciz	"readdir"
	.size	.L.str292, 8

	.type	.L.str293,@object       # @.str293
.L.str293:
	.asciz	"telldir"
	.size	.L.str293, 8

	.type	.L.str294,@object       # @.str294
.L.str294:
	.asciz	"seekdir"
	.size	.L.str294, 8

	.type	.L.str295,@object       # @.str295
.L.str295:
	.asciz	"rewinddir"
	.size	.L.str295, 10

	.type	.L.str296,@object       # @.str296
.L.str296:
	.asciz	"closedir"
	.size	.L.str296, 9

	.type	.L.str297,@object       # @.str297
.L.str297:
	.asciz	"fork"
	.size	.L.str297, 5

	.type	.L.str298,@object       # @.str298
.L.str298:
	.asciz	"wait"
	.size	.L.str298, 5

	.type	.L.str299,@object       # @.str299
.L.str299:
	.asciz	"waitpid"
	.size	.L.str299, 8

	.type	.L.str300,@object       # @.str300
.L.str300:
	.asciz	"system"
	.size	.L.str300, 7

	.type	.L.str301,@object       # @.str301
.L.str301:
	.asciz	"exec"
	.size	.L.str301, 5

	.type	.L.str302,@object       # @.str302
.L.str302:
	.asciz	"kill"
	.size	.L.str302, 5

	.type	.L.str303,@object       # @.str303
.L.str303:
	.asciz	"getppid"
	.size	.L.str303, 8

	.type	.L.str304,@object       # @.str304
.L.str304:
	.asciz	"getpgrp"
	.size	.L.str304, 8

	.type	.L.str305,@object       # @.str305
.L.str305:
	.asciz	"setpgrp"
	.size	.L.str305, 8

	.type	.L.str306,@object       # @.str306
.L.str306:
	.asciz	"getpriority"
	.size	.L.str306, 12

	.type	.L.str307,@object       # @.str307
.L.str307:
	.asciz	"setpriority"
	.size	.L.str307, 12

	.type	.L.str308,@object       # @.str308
.L.str308:
	.asciz	"time"
	.size	.L.str308, 5

	.type	.L.str309,@object       # @.str309
.L.str309:
	.asciz	"tms"
	.size	.L.str309, 4

	.type	.L.str310,@object       # @.str310
.L.str310:
	.asciz	"localtime"
	.size	.L.str310, 10

	.type	.L.str311,@object       # @.str311
.L.str311:
	.asciz	"gmtime"
	.size	.L.str311, 7

	.type	.L.str312,@object       # @.str312
.L.str312:
	.asciz	"alarm"
	.size	.L.str312, 6

	.type	.L.str313,@object       # @.str313
.L.str313:
	.asciz	"sleep"
	.size	.L.str313, 6

	.type	.L.str314,@object       # @.str314
.L.str314:
	.asciz	"shmget"
	.size	.L.str314, 7

	.type	.L.str315,@object       # @.str315
.L.str315:
	.asciz	"shmctl"
	.size	.L.str315, 7

	.type	.L.str316,@object       # @.str316
.L.str316:
	.asciz	"shmread"
	.size	.L.str316, 8

	.type	.L.str317,@object       # @.str317
.L.str317:
	.asciz	"shmwrite"
	.size	.L.str317, 9

	.type	.L.str318,@object       # @.str318
.L.str318:
	.asciz	"msgget"
	.size	.L.str318, 7

	.type	.L.str319,@object       # @.str319
.L.str319:
	.asciz	"msgctl"
	.size	.L.str319, 7

	.type	.L.str320,@object       # @.str320
.L.str320:
	.asciz	"msgsnd"
	.size	.L.str320, 7

	.type	.L.str321,@object       # @.str321
.L.str321:
	.asciz	"msgrcv"
	.size	.L.str321, 7

	.type	.L.str322,@object       # @.str322
.L.str322:
	.asciz	"semget"
	.size	.L.str322, 7

	.type	.L.str323,@object       # @.str323
.L.str323:
	.asciz	"semctl"
	.size	.L.str323, 7

	.type	.L.str324,@object       # @.str324
.L.str324:
	.asciz	"semop"
	.size	.L.str324, 6

	.type	.L.str325,@object       # @.str325
.L.str325:
	.asciz	"require"
	.size	.L.str325, 8

	.type	.L.str326,@object       # @.str326
.L.str326:
	.asciz	"dofile"
	.size	.L.str326, 7

	.type	.L.str327,@object       # @.str327
.L.str327:
	.asciz	"entereval"
	.size	.L.str327, 10

	.type	.L.str328,@object       # @.str328
.L.str328:
	.asciz	"leaveeval"
	.size	.L.str328, 10

	.type	.L.str329,@object       # @.str329
.L.str329:
	.asciz	"entertry"
	.size	.L.str329, 9

	.type	.L.str330,@object       # @.str330
.L.str330:
	.asciz	"leavetry"
	.size	.L.str330, 9

	.type	.L.str331,@object       # @.str331
.L.str331:
	.asciz	"ghbyname"
	.size	.L.str331, 9

	.type	.L.str332,@object       # @.str332
.L.str332:
	.asciz	"ghbyaddr"
	.size	.L.str332, 9

	.type	.L.str333,@object       # @.str333
.L.str333:
	.asciz	"ghostent"
	.size	.L.str333, 9

	.type	.L.str334,@object       # @.str334
.L.str334:
	.asciz	"gnbyname"
	.size	.L.str334, 9

	.type	.L.str335,@object       # @.str335
.L.str335:
	.asciz	"gnbyaddr"
	.size	.L.str335, 9

	.type	.L.str336,@object       # @.str336
.L.str336:
	.asciz	"gnetent"
	.size	.L.str336, 8

	.type	.L.str337,@object       # @.str337
.L.str337:
	.asciz	"gpbyname"
	.size	.L.str337, 9

	.type	.L.str338,@object       # @.str338
.L.str338:
	.asciz	"gpbynumber"
	.size	.L.str338, 11

	.type	.L.str339,@object       # @.str339
.L.str339:
	.asciz	"gprotoent"
	.size	.L.str339, 10

	.type	.L.str340,@object       # @.str340
.L.str340:
	.asciz	"gsbyname"
	.size	.L.str340, 9

	.type	.L.str341,@object       # @.str341
.L.str341:
	.asciz	"gsbyport"
	.size	.L.str341, 9

	.type	.L.str342,@object       # @.str342
.L.str342:
	.asciz	"gservent"
	.size	.L.str342, 9

	.type	.L.str343,@object       # @.str343
.L.str343:
	.asciz	"shostent"
	.size	.L.str343, 9

	.type	.L.str344,@object       # @.str344
.L.str344:
	.asciz	"snetent"
	.size	.L.str344, 8

	.type	.L.str345,@object       # @.str345
.L.str345:
	.asciz	"sprotoent"
	.size	.L.str345, 10

	.type	.L.str346,@object       # @.str346
.L.str346:
	.asciz	"sservent"
	.size	.L.str346, 9

	.type	.L.str347,@object       # @.str347
.L.str347:
	.asciz	"ehostent"
	.size	.L.str347, 9

	.type	.L.str348,@object       # @.str348
.L.str348:
	.asciz	"enetent"
	.size	.L.str348, 8

	.type	.L.str349,@object       # @.str349
.L.str349:
	.asciz	"eprotoent"
	.size	.L.str349, 10

	.type	.L.str350,@object       # @.str350
.L.str350:
	.asciz	"eservent"
	.size	.L.str350, 9

	.type	.L.str351,@object       # @.str351
.L.str351:
	.asciz	"gpwnam"
	.size	.L.str351, 7

	.type	.L.str352,@object       # @.str352
.L.str352:
	.asciz	"gpwuid"
	.size	.L.str352, 7

	.type	.L.str353,@object       # @.str353
.L.str353:
	.asciz	"gpwent"
	.size	.L.str353, 7

	.type	.L.str354,@object       # @.str354
.L.str354:
	.asciz	"spwent"
	.size	.L.str354, 7

	.type	.L.str355,@object       # @.str355
.L.str355:
	.asciz	"epwent"
	.size	.L.str355, 7

	.type	.L.str356,@object       # @.str356
.L.str356:
	.asciz	"ggrnam"
	.size	.L.str356, 7

	.type	.L.str357,@object       # @.str357
.L.str357:
	.asciz	"ggrgid"
	.size	.L.str357, 7

	.type	.L.str358,@object       # @.str358
.L.str358:
	.asciz	"ggrent"
	.size	.L.str358, 7

	.type	.L.str359,@object       # @.str359
.L.str359:
	.asciz	"sgrent"
	.size	.L.str359, 7

	.type	.L.str360,@object       # @.str360
.L.str360:
	.asciz	"egrent"
	.size	.L.str360, 7

	.type	.L.str361,@object       # @.str361
.L.str361:
	.asciz	"getlogin"
	.size	.L.str361, 9

	.type	.L.str362,@object       # @.str362
.L.str362:
	.asciz	"syscall"
	.size	.L.str362, 8

	.type	.L.str363,@object       # @.str363
.L.str363:
	.asciz	"lock"
	.size	.L.str363, 5

	.type	.L.str364,@object       # @.str364
.L.str364:
	.asciz	"threadsv"
	.size	.L.str364, 9

	.type	.L.str365,@object       # @.str365
.L.str365:
	.asciz	"setstate"
	.size	.L.str365, 9

	.type	.L.str366,@object       # @.str366
.L.str366:
	.asciz	"method_named"
	.size	.L.str366, 13

	.type	.L.str367,@object       # @.str367
.L.str367:
	.asciz	"custom"
	.size	.L.str367, 7

	.type	PL_op_name,@object      # @PL_op_name
	.data
	.globl	PL_op_name
	.align	16
PL_op_name:
	.quad	.L.str16
	.quad	.L.str17
	.quad	.L.str18
	.quad	.L.str19
	.quad	.L.str20
	.quad	.L.str21
	.quad	.L.str22
	.quad	.L.str23
	.quad	.L.str24
	.quad	.L.str25
	.quad	.L.str26
	.quad	.L.str27
	.quad	.L.str28
	.quad	.L.str29
	.quad	.L.str30
	.quad	.L.str31
	.quad	.L.str32
	.quad	.L.str33
	.quad	.L.str34
	.quad	.L.str35
	.quad	.L.str36
	.quad	.L.str37
	.quad	.L.str38
	.quad	.L.str39
	.quad	.L.str40
	.quad	.L.str41
	.quad	.L.str42
	.quad	.L.str43
	.quad	.L.str44
	.quad	.L.str45
	.quad	.L.str46
	.quad	.L.str47
	.quad	.L.str48
	.quad	.L.str49
	.quad	.L.str50
	.quad	.L.str51
	.quad	.L.str52
	.quad	.L.str53
	.quad	.L.str54
	.quad	.L.str55
	.quad	.L.str56
	.quad	.L.str57
	.quad	.L.str58
	.quad	.L.str59
	.quad	.L.str60
	.quad	.L.str61
	.quad	.L.str62
	.quad	.L.str63
	.quad	.L.str64
	.quad	.L.str65
	.quad	.L.str66
	.quad	.L.str67
	.quad	.L.str68
	.quad	.L.str69
	.quad	.L.str70
	.quad	.L.str71
	.quad	.L.str72
	.quad	.L.str73
	.quad	.L.str74
	.quad	.L.str75
	.quad	.L.str76
	.quad	.L.str77
	.quad	.L.str78
	.quad	.L.str79
	.quad	.L.str80
	.quad	.L.str81
	.quad	.L.str82
	.quad	.L.str83
	.quad	.L.str84
	.quad	.L.str85
	.quad	.L.str86
	.quad	.L.str87
	.quad	.L.str88
	.quad	.L.str89
	.quad	.L.str90
	.quad	.L.str91
	.quad	.L.str92
	.quad	.L.str93
	.quad	.L.str94
	.quad	.L.str95
	.quad	.L.str96
	.quad	.L.str97
	.quad	.L.str98
	.quad	.L.str99
	.quad	.L.str100
	.quad	.L.str101
	.quad	.L.str102
	.quad	.L.str103
	.quad	.L.str104
	.quad	.L.str105
	.quad	.L.str106
	.quad	.L.str107
	.quad	.L.str108
	.quad	.L.str109
	.quad	.L.str110
	.quad	.L.str111
	.quad	.L.str112
	.quad	.L.str113
	.quad	.L.str114
	.quad	.L.str115
	.quad	.L.str116
	.quad	.L.str117
	.quad	.L.str118
	.quad	.L.str119
	.quad	.L.str120
	.quad	.L.str121
	.quad	.L.str122
	.quad	.L.str123
	.quad	.L.str124
	.quad	.L.str125
	.quad	.L.str126
	.quad	.L.str127
	.quad	.L.str128
	.quad	.L.str129
	.quad	.L.str130
	.quad	.L.str131
	.quad	.L.str132
	.quad	.L.str133
	.quad	.L.str134
	.quad	.L.str135
	.quad	.L.str136
	.quad	.L.str137
	.quad	.L.str138
	.quad	.L.str139
	.quad	.L.str140
	.quad	.L.str141
	.quad	.L.str142
	.quad	.L.str143
	.quad	.L.str144
	.quad	.L.str145
	.quad	.L.str146
	.quad	.L.str147
	.quad	.L.str148
	.quad	.L.str149
	.quad	.L.str150
	.quad	.L.str151
	.quad	.L.str152
	.quad	.L.str153
	.quad	.L.str154
	.quad	.L.str155
	.quad	.L.str156
	.quad	.L.str157
	.quad	.L.str158
	.quad	.L.str159
	.quad	.L.str160
	.quad	.L.str161
	.quad	.L.str162
	.quad	.L.str163
	.quad	.L.str164
	.quad	.L.str165
	.quad	.L.str166
	.quad	.L.str167
	.quad	.L.str168
	.quad	.L.str169
	.quad	.L.str170
	.quad	.L.str171
	.quad	.L.str172
	.quad	.L.str173
	.quad	.L.str174
	.quad	.L.str175
	.quad	.L.str176
	.quad	.L.str177
	.quad	.L.str178
	.quad	.L.str179
	.quad	.L.str180
	.quad	.L.str181
	.quad	.L.str182
	.quad	.L.str183
	.quad	.L.str184
	.quad	.L.str185
	.quad	.L.str186
	.quad	.L.str187
	.quad	.L.str188
	.quad	.L.str189
	.quad	.L.str190
	.quad	.L.str191
	.quad	.L.str192
	.quad	.L.str193
	.quad	.L.str194
	.quad	.L.str195
	.quad	.L.str196
	.quad	.L.str197
	.quad	.L.str198
	.quad	.L.str199
	.quad	.L.str200
	.quad	.L.str201
	.quad	.L.str202
	.quad	.L.str203
	.quad	.L.str204
	.quad	.L.str205
	.quad	.L.str206
	.quad	.L.str207
	.quad	.L.str208
	.quad	.L.str209
	.quad	.L.str210
	.quad	.L.str211
	.quad	.L.str212
	.quad	.L.str213
	.quad	.L.str214
	.quad	.L.str215
	.quad	.L.str216
	.quad	.L.str217
	.quad	.L.str218
	.quad	.L.str219
	.quad	.L.str220
	.quad	.L.str221
	.quad	.L.str222
	.quad	.L.str223
	.quad	.L.str224
	.quad	.L.str225
	.quad	.L.str226
	.quad	.L.str227
	.quad	.L.str228
	.quad	.L.str229
	.quad	.L.str230
	.quad	.L.str231
	.quad	.L.str232
	.quad	.L.str233
	.quad	.L.str234
	.quad	.L.str235
	.quad	.L.str236
	.quad	.L.str237
	.quad	.L.str238
	.quad	.L.str239
	.quad	.L.str240
	.quad	.L.str241
	.quad	.L.str242
	.quad	.L.str243
	.quad	.L.str244
	.quad	.L.str245
	.quad	.L.str246
	.quad	.L.str247
	.quad	.L.str248
	.quad	.L.str249
	.quad	.L.str250
	.quad	.L.str251
	.quad	.L.str252
	.quad	.L.str253
	.quad	.L.str254
	.quad	.L.str255
	.quad	.L.str256
	.quad	.L.str257
	.quad	.L.str258
	.quad	.L.str259
	.quad	.L.str260
	.quad	.L.str261
	.quad	.L.str262
	.quad	.L.str263
	.quad	.L.str264
	.quad	.L.str265
	.quad	.L.str266
	.quad	.L.str267
	.quad	.L.str268
	.quad	.L.str269
	.quad	.L.str270
	.quad	.L.str271
	.quad	.L.str272
	.quad	.L.str273
	.quad	.L.str274
	.quad	.L.str275
	.quad	.L.str276
	.quad	.L.str277
	.quad	.L.str278
	.quad	.L.str279
	.quad	.L.str280
	.quad	.L.str281
	.quad	.L.str282
	.quad	.L.str283
	.quad	.L.str284
	.quad	.L.str285
	.quad	.L.str286
	.quad	.L.str287
	.quad	.L.str288
	.quad	.L.str289
	.quad	.L.str290
	.quad	.L.str291
	.quad	.L.str292
	.quad	.L.str293
	.quad	.L.str294
	.quad	.L.str295
	.quad	.L.str296
	.quad	.L.str297
	.quad	.L.str298
	.quad	.L.str299
	.quad	.L.str300
	.quad	.L.str301
	.quad	.L.str302
	.quad	.L.str303
	.quad	.L.str304
	.quad	.L.str305
	.quad	.L.str306
	.quad	.L.str307
	.quad	.L.str308
	.quad	.L.str309
	.quad	.L.str310
	.quad	.L.str311
	.quad	.L.str312
	.quad	.L.str313
	.quad	.L.str314
	.quad	.L.str315
	.quad	.L.str316
	.quad	.L.str317
	.quad	.L.str318
	.quad	.L.str319
	.quad	.L.str320
	.quad	.L.str321
	.quad	.L.str322
	.quad	.L.str323
	.quad	.L.str324
	.quad	.L.str325
	.quad	.L.str326
	.quad	.L.str327
	.quad	.L.str328
	.quad	.L.str329
	.quad	.L.str330
	.quad	.L.str331
	.quad	.L.str332
	.quad	.L.str333
	.quad	.L.str334
	.quad	.L.str335
	.quad	.L.str336
	.quad	.L.str337
	.quad	.L.str338
	.quad	.L.str339
	.quad	.L.str340
	.quad	.L.str341
	.quad	.L.str342
	.quad	.L.str343
	.quad	.L.str344
	.quad	.L.str345
	.quad	.L.str346
	.quad	.L.str347
	.quad	.L.str348
	.quad	.L.str349
	.quad	.L.str350
	.quad	.L.str351
	.quad	.L.str352
	.quad	.L.str353
	.quad	.L.str354
	.quad	.L.str355
	.quad	.L.str356
	.quad	.L.str357
	.quad	.L.str358
	.quad	.L.str359
	.quad	.L.str360
	.quad	.L.str361
	.quad	.L.str362
	.quad	.L.str363
	.quad	.L.str364
	.quad	.L.str365
	.quad	.L.str366
	.quad	.L.str367
	.size	PL_op_name, 2816

	.type	.L.str368,@object       # @.str368
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str368:
	.asciz	"null operation"
	.size	.L.str368, 15

	.type	.L.str369,@object       # @.str369
.L.str369:
	.asciz	"constant item"
	.size	.L.str369, 14

	.type	.L.str370,@object       # @.str370
.L.str370:
	.asciz	"scalar variable"
	.size	.L.str370, 16

	.type	.L.str371,@object       # @.str371
.L.str371:
	.asciz	"glob value"
	.size	.L.str371, 11

	.type	.L.str372,@object       # @.str372
.L.str372:
	.asciz	"glob elem"
	.size	.L.str372, 10

	.type	.L.str373,@object       # @.str373
.L.str373:
	.asciz	"private variable"
	.size	.L.str373, 17

	.type	.L.str374,@object       # @.str374
.L.str374:
	.asciz	"private array"
	.size	.L.str374, 14

	.type	.L.str375,@object       # @.str375
.L.str375:
	.asciz	"private hash"
	.size	.L.str375, 13

	.type	.L.str376,@object       # @.str376
.L.str376:
	.asciz	"private value"
	.size	.L.str376, 14

	.type	.L.str377,@object       # @.str377
.L.str377:
	.asciz	"push regexp"
	.size	.L.str377, 12

	.type	.L.str378,@object       # @.str378
.L.str378:
	.asciz	"ref-to-glob cast"
	.size	.L.str378, 17

	.type	.L.str379,@object       # @.str379
.L.str379:
	.asciz	"scalar dereference"
	.size	.L.str379, 19

	.type	.L.str380,@object       # @.str380
.L.str380:
	.asciz	"array length"
	.size	.L.str380, 13

	.type	.L.str381,@object       # @.str381
.L.str381:
	.asciz	"subroutine dereference"
	.size	.L.str381, 23

	.type	.L.str382,@object       # @.str382
.L.str382:
	.asciz	"anonymous subroutine"
	.size	.L.str382, 21

	.type	.L.str383,@object       # @.str383
.L.str383:
	.asciz	"subroutine prototype"
	.size	.L.str383, 21

	.type	.L.str384,@object       # @.str384
.L.str384:
	.asciz	"reference constructor"
	.size	.L.str384, 22

	.type	.L.str385,@object       # @.str385
.L.str385:
	.asciz	"single ref constructor"
	.size	.L.str385, 23

	.type	.L.str386,@object       # @.str386
.L.str386:
	.asciz	"reference-type operator"
	.size	.L.str386, 24

	.type	.L.str387,@object       # @.str387
.L.str387:
	.asciz	"quoted execution (``, qx)"
	.size	.L.str387, 26

	.type	.L.str388,@object       # @.str388
.L.str388:
	.asciz	"<HANDLE>"
	.size	.L.str388, 9

	.type	.L.str389,@object       # @.str389
.L.str389:
	.asciz	"append I/O operator"
	.size	.L.str389, 20

	.type	.L.str390,@object       # @.str390
.L.str390:
	.asciz	"regexp internal guard"
	.size	.L.str390, 22

	.type	.L.str391,@object       # @.str391
.L.str391:
	.asciz	"regexp internal reset"
	.size	.L.str391, 22

	.type	.L.str392,@object       # @.str392
.L.str392:
	.asciz	"regexp compilation"
	.size	.L.str392, 19

	.type	.L.str393,@object       # @.str393
.L.str393:
	.asciz	"pattern match (m//)"
	.size	.L.str393, 20

	.type	.L.str394,@object       # @.str394
.L.str394:
	.asciz	"pattern quote (qr//)"
	.size	.L.str394, 21

	.type	.L.str395,@object       # @.str395
.L.str395:
	.asciz	"substitution (s///)"
	.size	.L.str395, 20

	.type	.L.str396,@object       # @.str396
.L.str396:
	.asciz	"substitution iterator"
	.size	.L.str396, 22

	.type	.L.str397,@object       # @.str397
.L.str397:
	.asciz	"transliteration (tr///)"
	.size	.L.str397, 24

	.type	.L.str398,@object       # @.str398
.L.str398:
	.asciz	"scalar assignment"
	.size	.L.str398, 18

	.type	.L.str399,@object       # @.str399
.L.str399:
	.asciz	"list assignment"
	.size	.L.str399, 16

	.type	.L.str400,@object       # @.str400
.L.str400:
	.asciz	"scalar chop"
	.size	.L.str400, 12

	.type	.L.str401,@object       # @.str401
.L.str401:
	.asciz	"scalar chomp"
	.size	.L.str401, 13

	.type	.L.str402,@object       # @.str402
.L.str402:
	.asciz	"defined operator"
	.size	.L.str402, 17

	.type	.L.str403,@object       # @.str403
.L.str403:
	.asciz	"undef operator"
	.size	.L.str403, 15

	.type	.L.str404,@object       # @.str404
.L.str404:
	.asciz	"match position"
	.size	.L.str404, 15

	.type	.L.str405,@object       # @.str405
.L.str405:
	.asciz	"preincrement (++)"
	.size	.L.str405, 18

	.type	.L.str406,@object       # @.str406
.L.str406:
	.asciz	"integer preincrement (++)"
	.size	.L.str406, 26

	.type	.L.str407,@object       # @.str407
.L.str407:
	.asciz	"predecrement (--)"
	.size	.L.str407, 18

	.type	.L.str408,@object       # @.str408
.L.str408:
	.asciz	"integer predecrement (--)"
	.size	.L.str408, 26

	.type	.L.str409,@object       # @.str409
.L.str409:
	.asciz	"postincrement (++)"
	.size	.L.str409, 19

	.type	.L.str410,@object       # @.str410
.L.str410:
	.asciz	"integer postincrement (++)"
	.size	.L.str410, 27

	.type	.L.str411,@object       # @.str411
.L.str411:
	.asciz	"postdecrement (--)"
	.size	.L.str411, 19

	.type	.L.str412,@object       # @.str412
.L.str412:
	.asciz	"integer postdecrement (--)"
	.size	.L.str412, 27

	.type	.L.str413,@object       # @.str413
.L.str413:
	.asciz	"exponentiation (**)"
	.size	.L.str413, 20

	.type	.L.str414,@object       # @.str414
.L.str414:
	.asciz	"multiplication (*)"
	.size	.L.str414, 19

	.type	.L.str415,@object       # @.str415
.L.str415:
	.asciz	"integer multiplication (*)"
	.size	.L.str415, 27

	.type	.L.str416,@object       # @.str416
.L.str416:
	.asciz	"division (/)"
	.size	.L.str416, 13

	.type	.L.str417,@object       # @.str417
.L.str417:
	.asciz	"integer division (/)"
	.size	.L.str417, 21

	.type	.L.str418,@object       # @.str418
.L.str418:
	.asciz	"modulus (%)"
	.size	.L.str418, 12

	.type	.L.str419,@object       # @.str419
.L.str419:
	.asciz	"integer modulus (%)"
	.size	.L.str419, 20

	.type	.L.str420,@object       # @.str420
.L.str420:
	.asciz	"repeat (x)"
	.size	.L.str420, 11

	.type	.L.str421,@object       # @.str421
.L.str421:
	.asciz	"addition (+)"
	.size	.L.str421, 13

	.type	.L.str422,@object       # @.str422
.L.str422:
	.asciz	"integer addition (+)"
	.size	.L.str422, 21

	.type	.L.str423,@object       # @.str423
.L.str423:
	.asciz	"subtraction (-)"
	.size	.L.str423, 16

	.type	.L.str424,@object       # @.str424
.L.str424:
	.asciz	"integer subtraction (-)"
	.size	.L.str424, 24

	.type	.L.str425,@object       # @.str425
.L.str425:
	.asciz	"concatenation (.) or string"
	.size	.L.str425, 28

	.type	.L.str426,@object       # @.str426
.L.str426:
	.asciz	"string"
	.size	.L.str426, 7

	.type	.L.str427,@object       # @.str427
.L.str427:
	.asciz	"left bitshift (<<)"
	.size	.L.str427, 19

	.type	.L.str428,@object       # @.str428
.L.str428:
	.asciz	"right bitshift (>>)"
	.size	.L.str428, 20

	.type	.L.str429,@object       # @.str429
.L.str429:
	.asciz	"numeric lt (<)"
	.size	.L.str429, 15

	.type	.L.str430,@object       # @.str430
.L.str430:
	.asciz	"integer lt (<)"
	.size	.L.str430, 15

	.type	.L.str431,@object       # @.str431
.L.str431:
	.asciz	"numeric gt (>)"
	.size	.L.str431, 15

	.type	.L.str432,@object       # @.str432
.L.str432:
	.asciz	"integer gt (>)"
	.size	.L.str432, 15

	.type	.L.str433,@object       # @.str433
.L.str433:
	.asciz	"numeric le (<=)"
	.size	.L.str433, 16

	.type	.L.str434,@object       # @.str434
.L.str434:
	.asciz	"integer le (<=)"
	.size	.L.str434, 16

	.type	.L.str435,@object       # @.str435
.L.str435:
	.asciz	"numeric ge (>=)"
	.size	.L.str435, 16

	.type	.L.str436,@object       # @.str436
.L.str436:
	.asciz	"integer ge (>=)"
	.size	.L.str436, 16

	.type	.L.str437,@object       # @.str437
.L.str437:
	.asciz	"numeric eq (==)"
	.size	.L.str437, 16

	.type	.L.str438,@object       # @.str438
.L.str438:
	.asciz	"integer eq (==)"
	.size	.L.str438, 16

	.type	.L.str439,@object       # @.str439
.L.str439:
	.asciz	"numeric ne (!=)"
	.size	.L.str439, 16

	.type	.L.str440,@object       # @.str440
.L.str440:
	.asciz	"integer ne (!=)"
	.size	.L.str440, 16

	.type	.L.str441,@object       # @.str441
.L.str441:
	.asciz	"numeric comparison (<=>)"
	.size	.L.str441, 25

	.type	.L.str442,@object       # @.str442
.L.str442:
	.asciz	"integer comparison (<=>)"
	.size	.L.str442, 25

	.type	.L.str443,@object       # @.str443
.L.str443:
	.asciz	"string lt"
	.size	.L.str443, 10

	.type	.L.str444,@object       # @.str444
.L.str444:
	.asciz	"string gt"
	.size	.L.str444, 10

	.type	.L.str445,@object       # @.str445
.L.str445:
	.asciz	"string le"
	.size	.L.str445, 10

	.type	.L.str446,@object       # @.str446
.L.str446:
	.asciz	"string ge"
	.size	.L.str446, 10

	.type	.L.str447,@object       # @.str447
.L.str447:
	.asciz	"string eq"
	.size	.L.str447, 10

	.type	.L.str448,@object       # @.str448
.L.str448:
	.asciz	"string ne"
	.size	.L.str448, 10

	.type	.L.str449,@object       # @.str449
.L.str449:
	.asciz	"string comparison (cmp)"
	.size	.L.str449, 24

	.type	.L.str450,@object       # @.str450
.L.str450:
	.asciz	"bitwise and (&)"
	.size	.L.str450, 16

	.type	.L.str451,@object       # @.str451
.L.str451:
	.asciz	"bitwise xor (^)"
	.size	.L.str451, 16

	.type	.L.str452,@object       # @.str452
.L.str452:
	.asciz	"bitwise or (|)"
	.size	.L.str452, 15

	.type	.L.str453,@object       # @.str453
.L.str453:
	.asciz	"negation (-)"
	.size	.L.str453, 13

	.type	.L.str454,@object       # @.str454
.L.str454:
	.asciz	"integer negation (-)"
	.size	.L.str454, 21

	.type	.L.str455,@object       # @.str455
.L.str455:
	.asciz	"1's complement (~)"
	.size	.L.str455, 19

	.type	.L.str456,@object       # @.str456
.L.str456:
	.asciz	"array dereference"
	.size	.L.str456, 18

	.type	.L.str457,@object       # @.str457
.L.str457:
	.asciz	"constant array element"
	.size	.L.str457, 23

	.type	.L.str458,@object       # @.str458
.L.str458:
	.asciz	"array element"
	.size	.L.str458, 14

	.type	.L.str459,@object       # @.str459
.L.str459:
	.asciz	"array slice"
	.size	.L.str459, 12

	.type	.L.str460,@object       # @.str460
.L.str460:
	.asciz	"hash dereference"
	.size	.L.str460, 17

	.type	.L.str461,@object       # @.str461
.L.str461:
	.asciz	"hash element"
	.size	.L.str461, 13

	.type	.L.str462,@object       # @.str462
.L.str462:
	.asciz	"hash slice"
	.size	.L.str462, 11

	.type	.L.str463,@object       # @.str463
.L.str463:
	.asciz	"join or string"
	.size	.L.str463, 15

	.type	.L.str464,@object       # @.str464
.L.str464:
	.asciz	"list slice"
	.size	.L.str464, 11

	.type	.L.str465,@object       # @.str465
.L.str465:
	.asciz	"anonymous list ([])"
	.size	.L.str465, 20

	.type	.L.str466,@object       # @.str466
.L.str466:
	.asciz	"anonymous hash ({})"
	.size	.L.str466, 20

	.type	.L.str467,@object       # @.str467
.L.str467:
	.asciz	"grep"
	.size	.L.str467, 5

	.type	.L.str468,@object       # @.str468
.L.str468:
	.asciz	"grep iterator"
	.size	.L.str468, 14

	.type	.L.str469,@object       # @.str469
.L.str469:
	.asciz	"map"
	.size	.L.str469, 4

	.type	.L.str470,@object       # @.str470
.L.str470:
	.asciz	"map iterator"
	.size	.L.str470, 13

	.type	.L.str471,@object       # @.str471
.L.str471:
	.asciz	"flipflop"
	.size	.L.str471, 9

	.type	.L.str472,@object       # @.str472
.L.str472:
	.asciz	"range (or flip)"
	.size	.L.str472, 16

	.type	.L.str473,@object       # @.str473
.L.str473:
	.asciz	"range (or flop)"
	.size	.L.str473, 16

	.type	.L.str474,@object       # @.str474
.L.str474:
	.asciz	"logical and (&&)"
	.size	.L.str474, 17

	.type	.L.str475,@object       # @.str475
.L.str475:
	.asciz	"logical or (||)"
	.size	.L.str475, 16

	.type	.L.str476,@object       # @.str476
.L.str476:
	.asciz	"logical xor"
	.size	.L.str476, 12

	.type	.L.str477,@object       # @.str477
.L.str477:
	.asciz	"conditional expression"
	.size	.L.str477, 23

	.type	.L.str478,@object       # @.str478
.L.str478:
	.asciz	"logical and assignment (&&=)"
	.size	.L.str478, 29

	.type	.L.str479,@object       # @.str479
.L.str479:
	.asciz	"logical or assignment (||=)"
	.size	.L.str479, 28

	.type	.L.str480,@object       # @.str480
.L.str480:
	.asciz	"method lookup"
	.size	.L.str480, 14

	.type	.L.str481,@object       # @.str481
.L.str481:
	.asciz	"subroutine entry"
	.size	.L.str481, 17

	.type	.L.str482,@object       # @.str482
.L.str482:
	.asciz	"subroutine exit"
	.size	.L.str482, 16

	.type	.L.str483,@object       # @.str483
.L.str483:
	.asciz	"lvalue subroutine return"
	.size	.L.str483, 25

	.type	.L.str484,@object       # @.str484
.L.str484:
	.asciz	"symbol reset"
	.size	.L.str484, 13

	.type	.L.str485,@object       # @.str485
.L.str485:
	.asciz	"line sequence"
	.size	.L.str485, 14

	.type	.L.str486,@object       # @.str486
.L.str486:
	.asciz	"next statement"
	.size	.L.str486, 15

	.type	.L.str487,@object       # @.str487
.L.str487:
	.asciz	"debug next statement"
	.size	.L.str487, 21

	.type	.L.str488,@object       # @.str488
.L.str488:
	.asciz	"iteration finalizer"
	.size	.L.str488, 20

	.type	.L.str489,@object       # @.str489
.L.str489:
	.asciz	"block entry"
	.size	.L.str489, 12

	.type	.L.str490,@object       # @.str490
.L.str490:
	.asciz	"block exit"
	.size	.L.str490, 11

	.type	.L.str491,@object       # @.str491
.L.str491:
	.asciz	"block"
	.size	.L.str491, 6

	.type	.L.str492,@object       # @.str492
.L.str492:
	.asciz	"foreach loop entry"
	.size	.L.str492, 19

	.type	.L.str493,@object       # @.str493
.L.str493:
	.asciz	"foreach loop iterator"
	.size	.L.str493, 22

	.type	.L.str494,@object       # @.str494
.L.str494:
	.asciz	"loop entry"
	.size	.L.str494, 11

	.type	.L.str495,@object       # @.str495
.L.str495:
	.asciz	"loop exit"
	.size	.L.str495, 10

	.type	.L.str496,@object       # @.str496
.L.str496:
	.asciz	"pipe"
	.size	.L.str496, 5

	.type	.L.str497,@object       # @.str497
.L.str497:
	.asciz	"select system call"
	.size	.L.str497, 19

	.type	.L.str498,@object       # @.str498
.L.str498:
	.asciz	"write"
	.size	.L.str498, 6

	.type	.L.str499,@object       # @.str499
.L.str499:
	.asciz	"write exit"
	.size	.L.str499, 11

	.type	.L.str500,@object       # @.str500
.L.str500:
	.asciz	"printf"
	.size	.L.str500, 7

	.type	.L.str501,@object       # @.str501
.L.str501:
	.asciz	"socketpair"
	.size	.L.str501, 11

	.type	.L.str502,@object       # @.str502
.L.str502:
	.asciz	"getsockopt"
	.size	.L.str502, 11

	.type	.L.str503,@object       # @.str503
.L.str503:
	.asciz	"setsockopt"
	.size	.L.str503, 11

	.type	.L.str504,@object       # @.str504
.L.str504:
	.asciz	"-R"
	.size	.L.str504, 3

	.type	.L.str505,@object       # @.str505
.L.str505:
	.asciz	"-W"
	.size	.L.str505, 3

	.type	.L.str506,@object       # @.str506
.L.str506:
	.asciz	"-X"
	.size	.L.str506, 3

	.type	.L.str507,@object       # @.str507
.L.str507:
	.asciz	"-r"
	.size	.L.str507, 3

	.type	.L.str508,@object       # @.str508
.L.str508:
	.asciz	"-w"
	.size	.L.str508, 3

	.type	.L.str509,@object       # @.str509
.L.str509:
	.asciz	"-x"
	.size	.L.str509, 3

	.type	.L.str510,@object       # @.str510
.L.str510:
	.asciz	"-e"
	.size	.L.str510, 3

	.type	.L.str511,@object       # @.str511
.L.str511:
	.asciz	"-O"
	.size	.L.str511, 3

	.type	.L.str512,@object       # @.str512
.L.str512:
	.asciz	"-o"
	.size	.L.str512, 3

	.type	.L.str513,@object       # @.str513
.L.str513:
	.asciz	"-z"
	.size	.L.str513, 3

	.type	.L.str514,@object       # @.str514
.L.str514:
	.asciz	"-s"
	.size	.L.str514, 3

	.type	.L.str515,@object       # @.str515
.L.str515:
	.asciz	"-M"
	.size	.L.str515, 3

	.type	.L.str516,@object       # @.str516
.L.str516:
	.asciz	"-A"
	.size	.L.str516, 3

	.type	.L.str517,@object       # @.str517
.L.str517:
	.asciz	"-C"
	.size	.L.str517, 3

	.type	.L.str518,@object       # @.str518
.L.str518:
	.asciz	"-S"
	.size	.L.str518, 3

	.type	.L.str519,@object       # @.str519
.L.str519:
	.asciz	"-c"
	.size	.L.str519, 3

	.type	.L.str520,@object       # @.str520
.L.str520:
	.asciz	"-b"
	.size	.L.str520, 3

	.type	.L.str521,@object       # @.str521
.L.str521:
	.asciz	"-f"
	.size	.L.str521, 3

	.type	.L.str522,@object       # @.str522
.L.str522:
	.asciz	"-d"
	.size	.L.str522, 3

	.type	.L.str523,@object       # @.str523
.L.str523:
	.asciz	"-p"
	.size	.L.str523, 3

	.type	.L.str524,@object       # @.str524
.L.str524:
	.asciz	"-l"
	.size	.L.str524, 3

	.type	.L.str525,@object       # @.str525
.L.str525:
	.asciz	"-u"
	.size	.L.str525, 3

	.type	.L.str526,@object       # @.str526
.L.str526:
	.asciz	"-g"
	.size	.L.str526, 3

	.type	.L.str527,@object       # @.str527
.L.str527:
	.asciz	"-k"
	.size	.L.str527, 3

	.type	.L.str528,@object       # @.str528
.L.str528:
	.asciz	"-t"
	.size	.L.str528, 3

	.type	.L.str529,@object       # @.str529
.L.str529:
	.asciz	"-T"
	.size	.L.str529, 3

	.type	.L.str530,@object       # @.str530
.L.str530:
	.asciz	"-B"
	.size	.L.str530, 3

	.type	.L.str531,@object       # @.str531
.L.str531:
	.asciz	"opendir"
	.size	.L.str531, 8

	.type	.L.str532,@object       # @.str532
.L.str532:
	.asciz	"times"
	.size	.L.str532, 6

	.type	.L.str533,@object       # @.str533
.L.str533:
	.asciz	"do \"file\""
	.size	.L.str533, 10

	.type	.L.str534,@object       # @.str534
.L.str534:
	.asciz	"eval \"string\""
	.size	.L.str534, 14

	.type	.L.str535,@object       # @.str535
.L.str535:
	.asciz	"eval \"string\" exit"
	.size	.L.str535, 19

	.type	.L.str536,@object       # @.str536
.L.str536:
	.asciz	"eval {block}"
	.size	.L.str536, 13

	.type	.L.str537,@object       # @.str537
.L.str537:
	.asciz	"eval {block} exit"
	.size	.L.str537, 18

	.type	.L.str538,@object       # @.str538
.L.str538:
	.asciz	"gethostbyname"
	.size	.L.str538, 14

	.type	.L.str539,@object       # @.str539
.L.str539:
	.asciz	"gethostbyaddr"
	.size	.L.str539, 14

	.type	.L.str540,@object       # @.str540
.L.str540:
	.asciz	"gethostent"
	.size	.L.str540, 11

	.type	.L.str541,@object       # @.str541
.L.str541:
	.asciz	"getnetbyname"
	.size	.L.str541, 13

	.type	.L.str542,@object       # @.str542
.L.str542:
	.asciz	"getnetbyaddr"
	.size	.L.str542, 13

	.type	.L.str543,@object       # @.str543
.L.str543:
	.asciz	"getnetent"
	.size	.L.str543, 10

	.type	.L.str544,@object       # @.str544
.L.str544:
	.asciz	"getprotobyname"
	.size	.L.str544, 15

	.type	.L.str545,@object       # @.str545
.L.str545:
	.asciz	"getprotobynumber"
	.size	.L.str545, 17

	.type	.L.str546,@object       # @.str546
.L.str546:
	.asciz	"getprotoent"
	.size	.L.str546, 12

	.type	.L.str547,@object       # @.str547
.L.str547:
	.asciz	"getservbyname"
	.size	.L.str547, 14

	.type	.L.str548,@object       # @.str548
.L.str548:
	.asciz	"getservbyport"
	.size	.L.str548, 14

	.type	.L.str549,@object       # @.str549
.L.str549:
	.asciz	"getservent"
	.size	.L.str549, 11

	.type	.L.str550,@object       # @.str550
.L.str550:
	.asciz	"sethostent"
	.size	.L.str550, 11

	.type	.L.str551,@object       # @.str551
.L.str551:
	.asciz	"setnetent"
	.size	.L.str551, 10

	.type	.L.str552,@object       # @.str552
.L.str552:
	.asciz	"setprotoent"
	.size	.L.str552, 12

	.type	.L.str553,@object       # @.str553
.L.str553:
	.asciz	"setservent"
	.size	.L.str553, 11

	.type	.L.str554,@object       # @.str554
.L.str554:
	.asciz	"endhostent"
	.size	.L.str554, 11

	.type	.L.str555,@object       # @.str555
.L.str555:
	.asciz	"endnetent"
	.size	.L.str555, 10

	.type	.L.str556,@object       # @.str556
.L.str556:
	.asciz	"endprotoent"
	.size	.L.str556, 12

	.type	.L.str557,@object       # @.str557
.L.str557:
	.asciz	"endservent"
	.size	.L.str557, 11

	.type	.L.str558,@object       # @.str558
.L.str558:
	.asciz	"getpwnam"
	.size	.L.str558, 9

	.type	.L.str559,@object       # @.str559
.L.str559:
	.asciz	"getpwuid"
	.size	.L.str559, 9

	.type	.L.str560,@object       # @.str560
.L.str560:
	.asciz	"getpwent"
	.size	.L.str560, 9

	.type	.L.str561,@object       # @.str561
.L.str561:
	.asciz	"setpwent"
	.size	.L.str561, 9

	.type	.L.str562,@object       # @.str562
.L.str562:
	.asciz	"endpwent"
	.size	.L.str562, 9

	.type	.L.str563,@object       # @.str563
.L.str563:
	.asciz	"getgrnam"
	.size	.L.str563, 9

	.type	.L.str564,@object       # @.str564
.L.str564:
	.asciz	"getgrgid"
	.size	.L.str564, 9

	.type	.L.str565,@object       # @.str565
.L.str565:
	.asciz	"getgrent"
	.size	.L.str565, 9

	.type	.L.str566,@object       # @.str566
.L.str566:
	.asciz	"setgrent"
	.size	.L.str566, 9

	.type	.L.str567,@object       # @.str567
.L.str567:
	.asciz	"endgrent"
	.size	.L.str567, 9

	.type	.L.str568,@object       # @.str568
.L.str568:
	.asciz	"per-thread value"
	.size	.L.str568, 17

	.type	.L.str569,@object       # @.str569
.L.str569:
	.asciz	"set statement info"
	.size	.L.str569, 19

	.type	.L.str570,@object       # @.str570
.L.str570:
	.asciz	"method with known name"
	.size	.L.str570, 23

	.type	.L.str571,@object       # @.str571
.L.str571:
	.asciz	"unknown custom operator"
	.size	.L.str571, 24

	.type	PL_op_desc,@object      # @PL_op_desc
	.data
	.globl	PL_op_desc
	.align	16
PL_op_desc:
	.quad	.L.str368
	.quad	.L.str17
	.quad	.L.str18
	.quad	.L.str19
	.quad	.L.str20
	.quad	.L.str369
	.quad	.L.str370
	.quad	.L.str371
	.quad	.L.str372
	.quad	.L.str373
	.quad	.L.str374
	.quad	.L.str375
	.quad	.L.str376
	.quad	.L.str377
	.quad	.L.str378
	.quad	.L.str379
	.quad	.L.str380
	.quad	.L.str381
	.quad	.L.str382
	.quad	.L.str383
	.quad	.L.str384
	.quad	.L.str385
	.quad	.L.str386
	.quad	.L.str39
	.quad	.L.str387
	.quad	.L.str41
	.quad	.L.str388
	.quad	.L.str389
	.quad	.L.str390
	.quad	.L.str391
	.quad	.L.str392
	.quad	.L.str393
	.quad	.L.str394
	.quad	.L.str395
	.quad	.L.str396
	.quad	.L.str397
	.quad	.L.str398
	.quad	.L.str399
	.quad	.L.str54
	.quad	.L.str400
	.quad	.L.str56
	.quad	.L.str401
	.quad	.L.str402
	.quad	.L.str403
	.quad	.L.str60
	.quad	.L.str404
	.quad	.L.str405
	.quad	.L.str406
	.quad	.L.str407
	.quad	.L.str408
	.quad	.L.str409
	.quad	.L.str410
	.quad	.L.str411
	.quad	.L.str412
	.quad	.L.str413
	.quad	.L.str414
	.quad	.L.str415
	.quad	.L.str416
	.quad	.L.str417
	.quad	.L.str418
	.quad	.L.str419
	.quad	.L.str420
	.quad	.L.str421
	.quad	.L.str422
	.quad	.L.str423
	.quad	.L.str424
	.quad	.L.str425
	.quad	.L.str426
	.quad	.L.str427
	.quad	.L.str428
	.quad	.L.str429
	.quad	.L.str430
	.quad	.L.str431
	.quad	.L.str432
	.quad	.L.str433
	.quad	.L.str434
	.quad	.L.str435
	.quad	.L.str436
	.quad	.L.str437
	.quad	.L.str438
	.quad	.L.str439
	.quad	.L.str440
	.quad	.L.str441
	.quad	.L.str442
	.quad	.L.str443
	.quad	.L.str444
	.quad	.L.str445
	.quad	.L.str446
	.quad	.L.str447
	.quad	.L.str448
	.quad	.L.str449
	.quad	.L.str450
	.quad	.L.str451
	.quad	.L.str452
	.quad	.L.str453
	.quad	.L.str454
	.quad	.L.str112
	.quad	.L.str455
	.quad	.L.str114
	.quad	.L.str115
	.quad	.L.str116
	.quad	.L.str117
	.quad	.L.str118
	.quad	.L.str119
	.quad	.L.str120
	.quad	.L.str121
	.quad	.L.str122
	.quad	.L.str123
	.quad	.L.str124
	.quad	.L.str125
	.quad	.L.str126
	.quad	.L.str127
	.quad	.L.str128
	.quad	.L.str129
	.quad	.L.str130
	.quad	.L.str131
	.quad	.L.str132
	.quad	.L.str133
	.quad	.L.str134
	.quad	.L.str135
	.quad	.L.str136
	.quad	.L.str137
	.quad	.L.str138
	.quad	.L.str139
	.quad	.L.str140
	.quad	.L.str456
	.quad	.L.str457
	.quad	.L.str458
	.quad	.L.str459
	.quad	.L.str145
	.quad	.L.str146
	.quad	.L.str147
	.quad	.L.str148
	.quad	.L.str149
	.quad	.L.str460
	.quad	.L.str461
	.quad	.L.str462
	.quad	.L.str153
	.quad	.L.str154
	.quad	.L.str155
	.quad	.L.str463
	.quad	.L.str157
	.quad	.L.str464
	.quad	.L.str465
	.quad	.L.str466
	.quad	.L.str161
	.quad	.L.str162
	.quad	.L.str163
	.quad	.L.str164
	.quad	.L.str165
	.quad	.L.str166
	.quad	.L.str167
	.quad	.L.str467
	.quad	.L.str468
	.quad	.L.str469
	.quad	.L.str470
	.quad	.L.str471
	.quad	.L.str472
	.quad	.L.str473
	.quad	.L.str474
	.quad	.L.str475
	.quad	.L.str476
	.quad	.L.str477
	.quad	.L.str478
	.quad	.L.str479
	.quad	.L.str480
	.quad	.L.str481
	.quad	.L.str482
	.quad	.L.str483
	.quad	.L.str185
	.quad	.L.str186
	.quad	.L.str187
	.quad	.L.str484
	.quad	.L.str485
	.quad	.L.str486
	.quad	.L.str487
	.quad	.L.str488
	.quad	.L.str489
	.quad	.L.str490
	.quad	.L.str491
	.quad	.L.str492
	.quad	.L.str493
	.quad	.L.str494
	.quad	.L.str495
	.quad	.L.str200
	.quad	.L.str201
	.quad	.L.str202
	.quad	.L.str203
	.quad	.L.str204
	.quad	.L.str205
	.quad	.L.str206
	.quad	.L.str207
	.quad	.L.str208
	.quad	.L.str496
	.quad	.L.str210
	.quad	.L.str211
	.quad	.L.str212
	.quad	.L.str213
	.quad	.L.str214
	.quad	.L.str215
	.quad	.L.str216
	.quad	.L.str217
	.quad	.L.str497
	.quad	.L.str219
	.quad	.L.str220
	.quad	.L.str221
	.quad	.L.str498
	.quad	.L.str499
	.quad	.L.str500
	.quad	.L.str225
	.quad	.L.str226
	.quad	.L.str227
	.quad	.L.str228
	.quad	.L.str229
	.quad	.L.str230
	.quad	.L.str231
	.quad	.L.str232
	.quad	.L.str233
	.quad	.L.str234
	.quad	.L.str235
	.quad	.L.str236
	.quad	.L.str237
	.quad	.L.str238
	.quad	.L.str239
	.quad	.L.str501
	.quad	.L.str241
	.quad	.L.str242
	.quad	.L.str243
	.quad	.L.str244
	.quad	.L.str245
	.quad	.L.str502
	.quad	.L.str503
	.quad	.L.str248
	.quad	.L.str249
	.quad	.L.str250
	.quad	.L.str251
	.quad	.L.str504
	.quad	.L.str505
	.quad	.L.str506
	.quad	.L.str507
	.quad	.L.str508
	.quad	.L.str509
	.quad	.L.str510
	.quad	.L.str511
	.quad	.L.str512
	.quad	.L.str513
	.quad	.L.str514
	.quad	.L.str515
	.quad	.L.str516
	.quad	.L.str517
	.quad	.L.str518
	.quad	.L.str519
	.quad	.L.str520
	.quad	.L.str521
	.quad	.L.str522
	.quad	.L.str523
	.quad	.L.str524
	.quad	.L.str525
	.quad	.L.str526
	.quad	.L.str527
	.quad	.L.str528
	.quad	.L.str529
	.quad	.L.str530
	.quad	.L.str279
	.quad	.L.str280
	.quad	.L.str281
	.quad	.L.str282
	.quad	.L.str283
	.quad	.L.str284
	.quad	.L.str285
	.quad	.L.str286
	.quad	.L.str287
	.quad	.L.str288
	.quad	.L.str289
	.quad	.L.str290
	.quad	.L.str531
	.quad	.L.str292
	.quad	.L.str293
	.quad	.L.str294
	.quad	.L.str295
	.quad	.L.str296
	.quad	.L.str297
	.quad	.L.str298
	.quad	.L.str299
	.quad	.L.str300
	.quad	.L.str301
	.quad	.L.str302
	.quad	.L.str303
	.quad	.L.str304
	.quad	.L.str305
	.quad	.L.str306
	.quad	.L.str307
	.quad	.L.str308
	.quad	.L.str532
	.quad	.L.str310
	.quad	.L.str311
	.quad	.L.str312
	.quad	.L.str313
	.quad	.L.str314
	.quad	.L.str315
	.quad	.L.str316
	.quad	.L.str317
	.quad	.L.str318
	.quad	.L.str319
	.quad	.L.str320
	.quad	.L.str321
	.quad	.L.str322
	.quad	.L.str323
	.quad	.L.str324
	.quad	.L.str325
	.quad	.L.str533
	.quad	.L.str534
	.quad	.L.str535
	.quad	.L.str536
	.quad	.L.str537
	.quad	.L.str538
	.quad	.L.str539
	.quad	.L.str540
	.quad	.L.str541
	.quad	.L.str542
	.quad	.L.str543
	.quad	.L.str544
	.quad	.L.str545
	.quad	.L.str546
	.quad	.L.str547
	.quad	.L.str548
	.quad	.L.str549
	.quad	.L.str550
	.quad	.L.str551
	.quad	.L.str552
	.quad	.L.str553
	.quad	.L.str554
	.quad	.L.str555
	.quad	.L.str556
	.quad	.L.str557
	.quad	.L.str558
	.quad	.L.str559
	.quad	.L.str560
	.quad	.L.str561
	.quad	.L.str562
	.quad	.L.str563
	.quad	.L.str564
	.quad	.L.str565
	.quad	.L.str566
	.quad	.L.str567
	.quad	.L.str361
	.quad	.L.str362
	.quad	.L.str363
	.quad	.L.str568
	.quad	.L.str569
	.quad	.L.str570
	.quad	.L.str571
	.size	PL_op_desc, 2816

	.type	PL_ppaddr,@object       # @PL_ppaddr
	.globl	PL_ppaddr
	.align	16
PL_ppaddr:
	.quad	Perl_pp_null
	.quad	Perl_pp_stub
	.quad	Perl_pp_scalar
	.quad	Perl_pp_pushmark
	.quad	Perl_pp_wantarray
	.quad	Perl_pp_const
	.quad	Perl_pp_gvsv
	.quad	Perl_pp_gv
	.quad	Perl_pp_gelem
	.quad	Perl_pp_padsv
	.quad	Perl_pp_padav
	.quad	Perl_pp_padhv
	.quad	Perl_pp_padany
	.quad	Perl_pp_pushre
	.quad	Perl_pp_rv2gv
	.quad	Perl_pp_rv2sv
	.quad	Perl_pp_av2arylen
	.quad	Perl_pp_rv2cv
	.quad	Perl_pp_anoncode
	.quad	Perl_pp_prototype
	.quad	Perl_pp_refgen
	.quad	Perl_pp_srefgen
	.quad	Perl_pp_ref
	.quad	Perl_pp_bless
	.quad	Perl_pp_backtick
	.quad	Perl_pp_glob
	.quad	Perl_pp_readline
	.quad	Perl_pp_rcatline
	.quad	Perl_pp_regcmaybe
	.quad	Perl_pp_regcreset
	.quad	Perl_pp_regcomp
	.quad	Perl_pp_match
	.quad	Perl_pp_qr
	.quad	Perl_pp_subst
	.quad	Perl_pp_substcont
	.quad	Perl_pp_trans
	.quad	Perl_pp_sassign
	.quad	Perl_pp_aassign
	.quad	Perl_pp_chop
	.quad	Perl_pp_schop
	.quad	Perl_pp_chomp
	.quad	Perl_pp_schomp
	.quad	Perl_pp_defined
	.quad	Perl_pp_undef
	.quad	Perl_pp_study
	.quad	Perl_pp_pos
	.quad	Perl_pp_preinc
	.quad	Perl_pp_preinc
	.quad	Perl_pp_predec
	.quad	Perl_pp_predec
	.quad	Perl_pp_postinc
	.quad	Perl_pp_postinc
	.quad	Perl_pp_postdec
	.quad	Perl_pp_postdec
	.quad	Perl_pp_pow
	.quad	Perl_pp_multiply
	.quad	Perl_pp_i_multiply
	.quad	Perl_pp_divide
	.quad	Perl_pp_i_divide
	.quad	Perl_pp_modulo
	.quad	Perl_pp_i_modulo
	.quad	Perl_pp_repeat
	.quad	Perl_pp_add
	.quad	Perl_pp_i_add
	.quad	Perl_pp_subtract
	.quad	Perl_pp_i_subtract
	.quad	Perl_pp_concat
	.quad	Perl_pp_stringify
	.quad	Perl_pp_left_shift
	.quad	Perl_pp_right_shift
	.quad	Perl_pp_lt
	.quad	Perl_pp_i_lt
	.quad	Perl_pp_gt
	.quad	Perl_pp_i_gt
	.quad	Perl_pp_le
	.quad	Perl_pp_i_le
	.quad	Perl_pp_ge
	.quad	Perl_pp_i_ge
	.quad	Perl_pp_eq
	.quad	Perl_pp_i_eq
	.quad	Perl_pp_ne
	.quad	Perl_pp_i_ne
	.quad	Perl_pp_ncmp
	.quad	Perl_pp_i_ncmp
	.quad	Perl_pp_slt
	.quad	Perl_pp_sgt
	.quad	Perl_pp_sle
	.quad	Perl_pp_sge
	.quad	Perl_pp_seq
	.quad	Perl_pp_sne
	.quad	Perl_pp_scmp
	.quad	Perl_pp_bit_and
	.quad	Perl_pp_bit_xor
	.quad	Perl_pp_bit_or
	.quad	Perl_pp_negate
	.quad	Perl_pp_i_negate
	.quad	Perl_pp_not
	.quad	Perl_pp_complement
	.quad	Perl_pp_atan2
	.quad	Perl_pp_sin
	.quad	Perl_pp_cos
	.quad	Perl_pp_rand
	.quad	Perl_pp_srand
	.quad	Perl_pp_exp
	.quad	Perl_pp_log
	.quad	Perl_pp_sqrt
	.quad	Perl_pp_int
	.quad	Perl_pp_hex
	.quad	Perl_pp_oct
	.quad	Perl_pp_abs
	.quad	Perl_pp_length
	.quad	Perl_pp_substr
	.quad	Perl_pp_vec
	.quad	Perl_pp_index
	.quad	Perl_pp_rindex
	.quad	Perl_pp_sprintf
	.quad	Perl_pp_formline
	.quad	Perl_pp_ord
	.quad	Perl_pp_chr
	.quad	Perl_pp_crypt
	.quad	Perl_pp_ucfirst
	.quad	Perl_pp_lcfirst
	.quad	Perl_pp_uc
	.quad	Perl_pp_lc
	.quad	Perl_pp_quotemeta
	.quad	Perl_pp_rv2av
	.quad	Perl_pp_aelemfast
	.quad	Perl_pp_aelem
	.quad	Perl_pp_aslice
	.quad	Perl_pp_each
	.quad	Perl_pp_values
	.quad	Perl_pp_keys
	.quad	Perl_pp_delete
	.quad	Perl_pp_exists
	.quad	Perl_pp_rv2hv
	.quad	Perl_pp_helem
	.quad	Perl_pp_hslice
	.quad	Perl_pp_unpack
	.quad	Perl_pp_pack
	.quad	Perl_pp_split
	.quad	Perl_pp_join
	.quad	Perl_pp_list
	.quad	Perl_pp_lslice
	.quad	Perl_pp_anonlist
	.quad	Perl_pp_anonhash
	.quad	Perl_pp_splice
	.quad	Perl_pp_push
	.quad	Perl_pp_pop
	.quad	Perl_pp_shift
	.quad	Perl_pp_unshift
	.quad	Perl_pp_sort
	.quad	Perl_pp_reverse
	.quad	Perl_pp_grepstart
	.quad	Perl_pp_grepwhile
	.quad	Perl_pp_mapstart
	.quad	Perl_pp_mapwhile
	.quad	Perl_pp_range
	.quad	Perl_pp_flip
	.quad	Perl_pp_flop
	.quad	Perl_pp_and
	.quad	Perl_pp_or
	.quad	Perl_pp_xor
	.quad	Perl_pp_cond_expr
	.quad	Perl_pp_andassign
	.quad	Perl_pp_orassign
	.quad	Perl_pp_method
	.quad	Perl_pp_entersub
	.quad	Perl_pp_leavesub
	.quad	Perl_pp_leavesublv
	.quad	Perl_pp_caller
	.quad	Perl_pp_warn
	.quad	Perl_pp_die
	.quad	Perl_pp_reset
	.quad	Perl_pp_lineseq
	.quad	Perl_pp_nextstate
	.quad	Perl_pp_dbstate
	.quad	Perl_pp_unstack
	.quad	Perl_pp_enter
	.quad	Perl_pp_leave
	.quad	Perl_pp_scope
	.quad	Perl_pp_enteriter
	.quad	Perl_pp_iter
	.quad	Perl_pp_enterloop
	.quad	Perl_pp_leaveloop
	.quad	Perl_pp_return
	.quad	Perl_pp_last
	.quad	Perl_pp_next
	.quad	Perl_pp_redo
	.quad	Perl_pp_dump
	.quad	Perl_pp_goto
	.quad	Perl_pp_exit
	.quad	Perl_pp_open
	.quad	Perl_pp_close
	.quad	Perl_pp_pipe_op
	.quad	Perl_pp_fileno
	.quad	Perl_pp_umask
	.quad	Perl_pp_binmode
	.quad	Perl_pp_tie
	.quad	Perl_pp_untie
	.quad	Perl_pp_tied
	.quad	Perl_pp_dbmopen
	.quad	Perl_pp_dbmclose
	.quad	Perl_pp_sselect
	.quad	Perl_pp_select
	.quad	Perl_pp_getc
	.quad	Perl_pp_read
	.quad	Perl_pp_enterwrite
	.quad	Perl_pp_leavewrite
	.quad	Perl_pp_prtf
	.quad	Perl_pp_print
	.quad	Perl_pp_sysopen
	.quad	Perl_pp_sysseek
	.quad	Perl_pp_sysread
	.quad	Perl_pp_syswrite
	.quad	Perl_pp_send
	.quad	Perl_pp_recv
	.quad	Perl_pp_eof
	.quad	Perl_pp_tell
	.quad	Perl_pp_seek
	.quad	Perl_pp_truncate
	.quad	Perl_pp_fcntl
	.quad	Perl_pp_ioctl
	.quad	Perl_pp_flock
	.quad	Perl_pp_socket
	.quad	Perl_pp_sockpair
	.quad	Perl_pp_bind
	.quad	Perl_pp_connect
	.quad	Perl_pp_listen
	.quad	Perl_pp_accept
	.quad	Perl_pp_shutdown
	.quad	Perl_pp_gsockopt
	.quad	Perl_pp_ssockopt
	.quad	Perl_pp_getsockname
	.quad	Perl_pp_getpeername
	.quad	Perl_pp_lstat
	.quad	Perl_pp_stat
	.quad	Perl_pp_ftrread
	.quad	Perl_pp_ftrwrite
	.quad	Perl_pp_ftrexec
	.quad	Perl_pp_fteread
	.quad	Perl_pp_ftewrite
	.quad	Perl_pp_fteexec
	.quad	Perl_pp_ftis
	.quad	Perl_pp_fteowned
	.quad	Perl_pp_ftrowned
	.quad	Perl_pp_ftzero
	.quad	Perl_pp_ftsize
	.quad	Perl_pp_ftmtime
	.quad	Perl_pp_ftatime
	.quad	Perl_pp_ftctime
	.quad	Perl_pp_ftsock
	.quad	Perl_pp_ftchr
	.quad	Perl_pp_ftblk
	.quad	Perl_pp_ftfile
	.quad	Perl_pp_ftdir
	.quad	Perl_pp_ftpipe
	.quad	Perl_pp_ftlink
	.quad	Perl_pp_ftsuid
	.quad	Perl_pp_ftsgid
	.quad	Perl_pp_ftsvtx
	.quad	Perl_pp_fttty
	.quad	Perl_pp_fttext
	.quad	Perl_pp_ftbinary
	.quad	Perl_pp_chdir
	.quad	Perl_pp_chown
	.quad	Perl_pp_chroot
	.quad	Perl_pp_unlink
	.quad	Perl_pp_chmod
	.quad	Perl_pp_utime
	.quad	Perl_pp_rename
	.quad	Perl_pp_link
	.quad	Perl_pp_symlink
	.quad	Perl_pp_readlink
	.quad	Perl_pp_mkdir
	.quad	Perl_pp_rmdir
	.quad	Perl_pp_open_dir
	.quad	Perl_pp_readdir
	.quad	Perl_pp_telldir
	.quad	Perl_pp_seekdir
	.quad	Perl_pp_rewinddir
	.quad	Perl_pp_closedir
	.quad	Perl_pp_fork
	.quad	Perl_pp_wait
	.quad	Perl_pp_waitpid
	.quad	Perl_pp_system
	.quad	Perl_pp_exec
	.quad	Perl_pp_kill
	.quad	Perl_pp_getppid
	.quad	Perl_pp_getpgrp
	.quad	Perl_pp_setpgrp
	.quad	Perl_pp_getpriority
	.quad	Perl_pp_setpriority
	.quad	Perl_pp_time
	.quad	Perl_pp_tms
	.quad	Perl_pp_localtime
	.quad	Perl_pp_gmtime
	.quad	Perl_pp_alarm
	.quad	Perl_pp_sleep
	.quad	Perl_pp_shmget
	.quad	Perl_pp_shmctl
	.quad	Perl_pp_shmread
	.quad	Perl_pp_shmwrite
	.quad	Perl_pp_msgget
	.quad	Perl_pp_msgctl
	.quad	Perl_pp_msgsnd
	.quad	Perl_pp_msgrcv
	.quad	Perl_pp_semget
	.quad	Perl_pp_semctl
	.quad	Perl_pp_semop
	.quad	Perl_pp_require
	.quad	Perl_pp_dofile
	.quad	Perl_pp_entereval
	.quad	Perl_pp_leaveeval
	.quad	Perl_pp_entertry
	.quad	Perl_pp_leavetry
	.quad	Perl_pp_ghbyname
	.quad	Perl_pp_ghbyaddr
	.quad	Perl_pp_ghostent
	.quad	Perl_pp_gnbyname
	.quad	Perl_pp_gnbyaddr
	.quad	Perl_pp_gnetent
	.quad	Perl_pp_gpbyname
	.quad	Perl_pp_gpbynumber
	.quad	Perl_pp_gprotoent
	.quad	Perl_pp_gsbyname
	.quad	Perl_pp_gsbyport
	.quad	Perl_pp_gservent
	.quad	Perl_pp_shostent
	.quad	Perl_pp_snetent
	.quad	Perl_pp_sprotoent
	.quad	Perl_pp_sservent
	.quad	Perl_pp_ehostent
	.quad	Perl_pp_enetent
	.quad	Perl_pp_eprotoent
	.quad	Perl_pp_eservent
	.quad	Perl_pp_gpwnam
	.quad	Perl_pp_gpwuid
	.quad	Perl_pp_gpwent
	.quad	Perl_pp_spwent
	.quad	Perl_pp_epwent
	.quad	Perl_pp_ggrnam
	.quad	Perl_pp_ggrgid
	.quad	Perl_pp_ggrent
	.quad	Perl_pp_sgrent
	.quad	Perl_pp_egrent
	.quad	Perl_pp_getlogin
	.quad	Perl_pp_syscall
	.quad	Perl_pp_lock
	.quad	Perl_pp_threadsv
	.quad	Perl_pp_setstate
	.quad	Perl_pp_method_named
	.size	PL_ppaddr, 2808

	.type	PL_check,@object        # @PL_check
	.globl	PL_check
	.align	16
PL_check:
	.quad	Perl_ck_null
	.quad	Perl_ck_null
	.quad	Perl_ck_fun
	.quad	Perl_ck_null
	.quad	Perl_ck_null
	.quad	Perl_ck_svconst
	.quad	Perl_ck_null
	.quad	Perl_ck_null
	.quad	Perl_ck_null
	.quad	Perl_ck_null
	.quad	Perl_ck_null
	.quad	Perl_ck_null
	.quad	Perl_ck_null
	.quad	Perl_ck_null
	.quad	Perl_ck_rvconst
	.quad	Perl_ck_rvconst
	.quad	Perl_ck_null
	.quad	Perl_ck_rvconst
	.quad	Perl_ck_anoncode
	.quad	Perl_ck_null
	.quad	Perl_ck_spair
	.quad	Perl_ck_null
	.quad	Perl_ck_fun
	.quad	Perl_ck_fun
	.quad	Perl_ck_open
	.quad	Perl_ck_glob
	.quad	Perl_ck_null
	.quad	Perl_ck_null
	.quad	Perl_ck_fun
	.quad	Perl_ck_fun
	.quad	Perl_ck_null
	.quad	Perl_ck_match
	.quad	Perl_ck_match
	.quad	Perl_ck_null
	.quad	Perl_ck_null
	.quad	Perl_ck_null
	.quad	Perl_ck_sassign
	.quad	Perl_ck_null
	.quad	Perl_ck_spair
	.quad	Perl_ck_null
	.quad	Perl_ck_spair
	.quad	Perl_ck_null
	.quad	Perl_ck_defined
	.quad	Perl_ck_lfun
	.quad	Perl_ck_fun
	.quad	Perl_ck_lfun
	.quad	Perl_ck_lfun
	.quad	Perl_ck_lfun
	.quad	Perl_ck_lfun
	.quad	Perl_ck_lfun
	.quad	Perl_ck_lfun
	.quad	Perl_ck_lfun
	.quad	Perl_ck_lfun
	.quad	Perl_ck_lfun
	.quad	Perl_ck_null
	.quad	Perl_ck_null
	.quad	Perl_ck_null
	.quad	Perl_ck_null
	.quad	Perl_ck_null
	.quad	Perl_ck_null
	.quad	Perl_ck_null
	.quad	Perl_ck_repeat
	.quad	Perl_ck_null
	.quad	Perl_ck_null
	.quad	Perl_ck_null
	.quad	Perl_ck_null
	.quad	Perl_ck_concat
	.quad	Perl_ck_fun
	.quad	Perl_ck_bitop
	.quad	Perl_ck_bitop
	.quad	Perl_ck_null
	.quad	Perl_ck_null
	.quad	Perl_ck_null
	.quad	Perl_ck_null
	.quad	Perl_ck_null
	.quad	Perl_ck_null
	.quad	Perl_ck_null
	.quad	Perl_ck_null
	.quad	Perl_ck_null
	.quad	Perl_ck_null
	.quad	Perl_ck_null
	.quad	Perl_ck_null
	.quad	Perl_ck_null
	.quad	Perl_ck_null
	.quad	Perl_ck_null
	.quad	Perl_ck_null
	.quad	Perl_ck_null
	.quad	Perl_ck_null
	.quad	Perl_ck_null
	.quad	Perl_ck_null
	.quad	Perl_ck_null
	.quad	Perl_ck_bitop
	.quad	Perl_ck_bitop
	.quad	Perl_ck_bitop
	.quad	Perl_ck_null
	.quad	Perl_ck_null
	.quad	Perl_ck_null
	.quad	Perl_ck_bitop
	.quad	Perl_ck_fun
	.quad	Perl_ck_fun
	.quad	Perl_ck_fun
	.quad	Perl_ck_fun
	.quad	Perl_ck_fun
	.quad	Perl_ck_fun
	.quad	Perl_ck_fun
	.quad	Perl_ck_fun
	.quad	Perl_ck_fun
	.quad	Perl_ck_fun
	.quad	Perl_ck_fun
	.quad	Perl_ck_fun
	.quad	Perl_ck_lengthconst
	.quad	Perl_ck_substr
	.quad	Perl_ck_fun
	.quad	Perl_ck_index
	.quad	Perl_ck_index
	.quad	Perl_ck_fun
	.quad	Perl_ck_fun
	.quad	Perl_ck_fun
	.quad	Perl_ck_fun
	.quad	Perl_ck_fun
	.quad	Perl_ck_fun
	.quad	Perl_ck_fun
	.quad	Perl_ck_fun
	.quad	Perl_ck_fun
	.quad	Perl_ck_fun
	.quad	Perl_ck_rvconst
	.quad	Perl_ck_null
	.quad	Perl_ck_null
	.quad	Perl_ck_null
	.quad	Perl_ck_fun
	.quad	Perl_ck_fun
	.quad	Perl_ck_fun
	.quad	Perl_ck_delete
	.quad	Perl_ck_exists
	.quad	Perl_ck_rvconst
	.quad	Perl_ck_null
	.quad	Perl_ck_null
	.quad	Perl_ck_fun
	.quad	Perl_ck_fun
	.quad	Perl_ck_split
	.quad	Perl_ck_join
	.quad	Perl_ck_null
	.quad	Perl_ck_null
	.quad	Perl_ck_fun
	.quad	Perl_ck_fun
	.quad	Perl_ck_fun
	.quad	Perl_ck_fun
	.quad	Perl_ck_shift
	.quad	Perl_ck_shift
	.quad	Perl_ck_fun
	.quad	Perl_ck_sort
	.quad	Perl_ck_fun
	.quad	Perl_ck_grep
	.quad	Perl_ck_null
	.quad	Perl_ck_grep
	.quad	Perl_ck_null
	.quad	Perl_ck_null
	.quad	Perl_ck_null
	.quad	Perl_ck_null
	.quad	Perl_ck_null
	.quad	Perl_ck_null
	.quad	Perl_ck_null
	.quad	Perl_ck_null
	.quad	Perl_ck_null
	.quad	Perl_ck_null
	.quad	Perl_ck_method
	.quad	Perl_ck_subr
	.quad	Perl_ck_null
	.quad	Perl_ck_null
	.quad	Perl_ck_fun
	.quad	Perl_ck_fun
	.quad	Perl_ck_die
	.quad	Perl_ck_fun
	.quad	Perl_ck_null
	.quad	Perl_ck_null
	.quad	Perl_ck_null
	.quad	Perl_ck_null
	.quad	Perl_ck_null
	.quad	Perl_ck_null
	.quad	Perl_ck_null
	.quad	Perl_ck_null
	.quad	Perl_ck_null
	.quad	Perl_ck_null
	.quad	Perl_ck_null
	.quad	Perl_ck_return
	.quad	Perl_ck_null
	.quad	Perl_ck_null
	.quad	Perl_ck_null
	.quad	Perl_ck_null
	.quad	Perl_ck_null
	.quad	Perl_ck_exit
	.quad	Perl_ck_open
	.quad	Perl_ck_fun
	.quad	Perl_ck_fun
	.quad	Perl_ck_fun
	.quad	Perl_ck_fun
	.quad	Perl_ck_fun
	.quad	Perl_ck_fun
	.quad	Perl_ck_fun
	.quad	Perl_ck_fun
	.quad	Perl_ck_fun
	.quad	Perl_ck_fun
	.quad	Perl_ck_select
	.quad	Perl_ck_select
	.quad	Perl_ck_eof
	.quad	Perl_ck_fun
	.quad	Perl_ck_fun
	.quad	Perl_ck_null
	.quad	Perl_ck_listiob
	.quad	Perl_ck_listiob
	.quad	Perl_ck_fun
	.quad	Perl_ck_fun
	.quad	Perl_ck_fun
	.quad	Perl_ck_fun
	.quad	Perl_ck_fun
	.quad	Perl_ck_fun
	.quad	Perl_ck_eof
	.quad	Perl_ck_fun
	.quad	Perl_ck_fun
	.quad	Perl_ck_trunc
	.quad	Perl_ck_fun
	.quad	Perl_ck_fun
	.quad	Perl_ck_fun
	.quad	Perl_ck_fun
	.quad	Perl_ck_fun
	.quad	Perl_ck_fun
	.quad	Perl_ck_fun
	.quad	Perl_ck_fun
	.quad	Perl_ck_fun
	.quad	Perl_ck_fun
	.quad	Perl_ck_fun
	.quad	Perl_ck_fun
	.quad	Perl_ck_fun
	.quad	Perl_ck_fun
	.quad	Perl_ck_ftst
	.quad	Perl_ck_ftst
	.quad	Perl_ck_ftst
	.quad	Perl_ck_ftst
	.quad	Perl_ck_ftst
	.quad	Perl_ck_ftst
	.quad	Perl_ck_ftst
	.quad	Perl_ck_ftst
	.quad	Perl_ck_ftst
	.quad	Perl_ck_ftst
	.quad	Perl_ck_ftst
	.quad	Perl_ck_ftst
	.quad	Perl_ck_ftst
	.quad	Perl_ck_ftst
	.quad	Perl_ck_ftst
	.quad	Perl_ck_ftst
	.quad	Perl_ck_ftst
	.quad	Perl_ck_ftst
	.quad	Perl_ck_ftst
	.quad	Perl_ck_ftst
	.quad	Perl_ck_ftst
	.quad	Perl_ck_ftst
	.quad	Perl_ck_ftst
	.quad	Perl_ck_ftst
	.quad	Perl_ck_ftst
	.quad	Perl_ck_ftst
	.quad	Perl_ck_ftst
	.quad	Perl_ck_ftst
	.quad	Perl_ck_ftst
	.quad	Perl_ck_fun
	.quad	Perl_ck_fun
	.quad	Perl_ck_fun
	.quad	Perl_ck_fun
	.quad	Perl_ck_fun
	.quad	Perl_ck_fun
	.quad	Perl_ck_fun
	.quad	Perl_ck_fun
	.quad	Perl_ck_fun
	.quad	Perl_ck_fun
	.quad	Perl_ck_fun
	.quad	Perl_ck_fun
	.quad	Perl_ck_fun
	.quad	Perl_ck_fun
	.quad	Perl_ck_fun
	.quad	Perl_ck_fun
	.quad	Perl_ck_fun
	.quad	Perl_ck_fun
	.quad	Perl_ck_null
	.quad	Perl_ck_null
	.quad	Perl_ck_fun
	.quad	Perl_ck_exec
	.quad	Perl_ck_exec
	.quad	Perl_ck_fun
	.quad	Perl_ck_null
	.quad	Perl_ck_fun
	.quad	Perl_ck_fun
	.quad	Perl_ck_fun
	.quad	Perl_ck_fun
	.quad	Perl_ck_null
	.quad	Perl_ck_null
	.quad	Perl_ck_fun
	.quad	Perl_ck_fun
	.quad	Perl_ck_fun
	.quad	Perl_ck_fun
	.quad	Perl_ck_fun
	.quad	Perl_ck_fun
	.quad	Perl_ck_fun
	.quad	Perl_ck_fun
	.quad	Perl_ck_fun
	.quad	Perl_ck_fun
	.quad	Perl_ck_fun
	.quad	Perl_ck_fun
	.quad	Perl_ck_fun
	.quad	Perl_ck_fun
	.quad	Perl_ck_fun
	.quad	Perl_ck_require
	.quad	Perl_ck_fun
	.quad	Perl_ck_eval
	.quad	Perl_ck_null
	.quad	Perl_ck_null
	.quad	Perl_ck_null
	.quad	Perl_ck_fun
	.quad	Perl_ck_fun
	.quad	Perl_ck_null
	.quad	Perl_ck_fun
	.quad	Perl_ck_fun
	.quad	Perl_ck_null
	.quad	Perl_ck_fun
	.quad	Perl_ck_fun
	.quad	Perl_ck_null
	.quad	Perl_ck_fun
	.quad	Perl_ck_fun
	.quad	Perl_ck_null
	.quad	Perl_ck_fun
	.quad	Perl_ck_fun
	.quad	Perl_ck_fun
	.quad	Perl_ck_fun
	.quad	Perl_ck_null
	.quad	Perl_ck_null
	.quad	Perl_ck_null
	.quad	Perl_ck_null
	.quad	Perl_ck_fun
	.quad	Perl_ck_fun
	.quad	Perl_ck_null
	.quad	Perl_ck_null
	.quad	Perl_ck_null
	.quad	Perl_ck_fun
	.quad	Perl_ck_fun
	.quad	Perl_ck_null
	.quad	Perl_ck_null
	.quad	Perl_ck_null
	.quad	Perl_ck_null
	.quad	Perl_ck_fun
	.quad	Perl_ck_rfun
	.quad	Perl_ck_null
	.quad	Perl_ck_null
	.quad	Perl_ck_null
	.quad	Perl_ck_null
	.size	PL_check, 2816

	.type	PL_opargs,@object       # @PL_opargs
	.globl	PL_opargs
	.align	16
PL_opargs:
	.quad	0                       # 0x0
	.quad	0                       # 0x0
	.quad	13828                   # 0x3604
	.quad	4                       # 0x4
	.quad	20                      # 0x14
	.quad	3076                    # 0xc04
	.quad	3140                    # 0xc44
	.quad	3140                    # 0xc44
	.quad	140352                  # 0x22440
	.quad	68                      # 0x44
	.quad	64                      # 0x40
	.quad	64                      # 0x40
	.quad	64                      # 0x40
	.quad	2624                    # 0xa40
	.quad	580                     # 0x244
	.quad	580                     # 0x244
	.quad	532                     # 0x214
	.quad	576                     # 0x240
	.quad	3072                    # 0xc00
	.quad	13828                   # 0x3604
	.quad	16897                   # 0x4201
	.quad	8710                    # 0x2206
	.quad	79500                   # 0x1368c
	.quad	1189892                 # 0x122804
	.quad	5640                    # 0x1608
	.quad	75784                   # 0x12808
	.quad	120328                  # 0x1d608
	.quad	3080                    # 0xc08
	.quad	8708                    # 0x2204
	.quad	8708                    # 0x2204
	.quad	9732                    # 0x2604
	.quad	2624                    # 0xa40
	.quad	2564                    # 0xa04
	.quad	10836                   # 0x2a54
	.quad	1620                    # 0x654
	.quad	12308                   # 0x3014
	.quad	4                       # 0x4
	.quad	279560                  # 0x44408
	.quad	22029                   # 0x560d
	.quad	79500                   # 0x1368c
	.quad	22285                   # 0x570d
	.quad	79756                   # 0x1378c
	.quad	79508                   # 0x13694
	.quad	79364                   # 0x13604
	.quad	79492                   # 0x13684
	.quad	79500                   # 0x1368c
	.quad	8804                    # 0x2264
	.quad	8788                    # 0x2254
	.quad	8804                    # 0x2264
	.quad	8788                    # 0x2254
	.quad	8812                    # 0x226c
	.quad	9052                    # 0x235c
	.quad	8812                    # 0x226c
	.quad	9052                    # 0x235c
	.quad	140558                  # 0x2250e
	.quad	140590                  # 0x2252e
	.quad	140574                  # 0x2251e
	.quad	140590                  # 0x2252e
	.quad	140574                  # 0x2251e
	.quad	140606                  # 0x2253e
	.quad	140574                  # 0x2251e
	.quad	148489                  # 0x24409
	.quad	140590                  # 0x2252e
	.quad	140574                  # 0x2251e
	.quad	140590                  # 0x2252e
	.quad	140574                  # 0x2251e
	.quad	140558                  # 0x2250e
	.quad	10510                   # 0x290e
	.quad	140558                  # 0x2250e
	.quad	140558                  # 0x2250e
	.quad	140342                  # 0x22436
	.quad	140310                  # 0x22416
	.quad	140342                  # 0x22436
	.quad	140310                  # 0x22416
	.quad	140342                  # 0x22436
	.quad	140310                  # 0x22416
	.quad	140342                  # 0x22436
	.quad	140310                  # 0x22416
	.quad	140342                  # 0x22436
	.quad	140310                  # 0x22416
	.quad	140342                  # 0x22436
	.quad	140310                  # 0x22416
	.quad	140350                  # 0x2243e
	.quad	140318                  # 0x2241e
	.quad	140310                  # 0x22416
	.quad	140310                  # 0x22416
	.quad	140310                  # 0x22416
	.quad	140310                  # 0x22416
	.quad	140310                  # 0x22416
	.quad	140310                  # 0x22416
	.quad	140318                  # 0x2241e
	.quad	140302                  # 0x2240e
	.quad	140302                  # 0x2240e
	.quad	140302                  # 0x2240e
	.quad	8750                    # 0x222e
	.quad	8990                    # 0x231e
	.quad	8726                    # 0x2216
	.quad	8718                    # 0x220e
	.quad	141582                  # 0x2290e
	.quad	79758                   # 0x1378e
	.quad	79758                   # 0x1378e
	.quad	79628                   # 0x1370c
	.quad	79364                   # 0x13604
	.quad	79758                   # 0x1378e
	.quad	79758                   # 0x1378e
	.quad	79758                   # 0x1378e
	.quad	79758                   # 0x1378e
	.quad	79758                   # 0x1378e
	.quad	79758                   # 0x1378e
	.quad	79758                   # 0x1378e
	.quad	79772                   # 0x1379c
	.quad	321005580               # 0x1322280c
	.quad	2238492                 # 0x22281c
	.quad	19015964                # 0x122291c
	.quad	19015964                # 0x122291c
	.quad	272399                  # 0x4280f
	.quad	272389                  # 0x42805
	.quad	79774                   # 0x1379e
	.quad	79758                   # 0x1378e
	.quad	141582                  # 0x2290e
	.quad	79502                   # 0x1368e
	.quad	79502                   # 0x1368e
	.quad	79502                   # 0x1368e
	.quad	79502                   # 0x1368e
	.quad	79502                   # 0x1368e
	.quad	584                     # 0x248
	.quad	158724                  # 0x26c04
	.quad	156676                  # 0x26404
	.quad	288769                  # 0x46801
	.quad	38400                   # 0x9600
	.quad	38408                   # 0x9608
	.quad	38408                   # 0x9608
	.quad	13824                   # 0x3600
	.quad	13844                   # 0x3614
	.quad	584                     # 0x248
	.quad	164868                  # 0x28404
	.quad	296961                  # 0x48801
	.quad	141312                  # 0x22800
	.quad	272397                  # 0x4280d
	.quad	2238472                 # 0x222808
	.quad	272397                  # 0x4280d
	.quad	18433                   # 0x4801
	.quad	4490240                 # 0x448400
	.quad	18437                   # 0x4805
	.quad	18437                   # 0x4805
	.quad	87189505                # 0x5326801
	.quad	289053                  # 0x4691d
	.quad	95748                   # 0x17604
	.quad	95748                   # 0x17604
	.quad	289053                  # 0x4691d
	.quad	370689                  # 0x5a801
	.quad	18441                   # 0x4809
	.quad	305217                  # 0x4a841
	.quad	1608                    # 0x648
	.quad	305217                  # 0x4a841
	.quad	1608                    # 0x648
	.quad	140800                  # 0x22600
	.quad	139776                  # 0x22200
	.quad	512                     # 0x200
	.quad	1536                    # 0x600
	.quad	1536                    # 0x600
	.quad	140294                  # 0x22406
	.quad	1600                    # 0x640
	.quad	1540                    # 0x604
	.quad	1540                    # 0x604
	.quad	576                     # 0x240
	.quad	16969                   # 0x4249
	.quad	512                     # 0x200
	.quad	512                     # 0x200
	.quad	79368                   # 0x13608
	.quad	18461                   # 0x481d
	.quad	18525                   # 0x485d
	.quad	79380                   # 0x13614
	.quad	2048                    # 0x800
	.quad	5124                    # 0x1404
	.quad	5124                    # 0x1404
	.quad	4                       # 0x4
	.quad	0                       # 0x0
	.quad	2048                    # 0x800
	.quad	2048                    # 0x800
	.quad	4672                    # 0x1240
	.quad	0                       # 0x0
	.quad	4672                    # 0x1240
	.quad	1024                    # 0x400
	.quad	18497                   # 0x4841
	.quad	6724                    # 0x1a44
	.quad	6724                    # 0x1a44
	.quad	6724                    # 0x1a44
	.quad	6724                    # 0x1a44
	.quad	6724                    # 0x1a44
	.quad	79428                   # 0x13644
	.quad	5425181                 # 0x52c81d
	.quad	120340                  # 0x1d614
	.quad	837652                  # 0xcc814
	.quad	54812                   # 0xd61c
	.quad	79388                   # 0x1361c
	.quad	1230852                 # 0x12c804
	.quad	4384853                 # 0x42e855
	.quad	62996                   # 0xf614
	.quad	62980                   # 0xf604
	.quad	2263060                 # 0x228814
	.quad	38420                   # 0x9614
	.quad	35792904                # 0x2222808
	.quad	116748                  # 0x1c80c
	.quad	120332                  # 0x1d60c
	.quad	305055773               # 0x122ec81d
	.quad	120404                  # 0x1d654
	.quad	512                     # 0x200
	.quad	378901                  # 0x5c815
	.quad	378901                  # 0x5c815
	.quad	304269316               # 0x1222c804
	.quad	2279428                 # 0x22c804
	.quad	305055773               # 0x122ec81d
	.quad	321046557               # 0x1322c81d
	.quad	304269341               # 0x1222c81d
	.quad	36620317                # 0x22ec81d
	.quad	120340                  # 0x1d614
	.quad	120332                  # 0x1d60c
	.quad	2279428                 # 0x22c804
	.quad	141332                  # 0x22814
	.quad	2279436                 # 0x22c80c
	.quad	2279436                 # 0x22c80c
	.quad	182556                  # 0x2c91c
	.quad	35833876                # 0x222c814
	.quad	573360148               # 0x222cc814
	.quad	182292                  # 0x2c814
	.quad	182292                  # 0x2c814
	.quad	182292                  # 0x2c814
	.quad	837660                  # 0xcc81c
	.quad	182300                  # 0x2c81c
	.quad	2279444                 # 0x22c814
	.quad	35833876                # 0x222c814
	.quad	54804                   # 0xd614
	.quad	54804                   # 0xd614
	.quad	55424                   # 0xd880
	.quad	55424                   # 0xd880
	.quad	55444                   # 0xd894
	.quad	55444                   # 0xd894
	.quad	55444                   # 0xd894
	.quad	55444                   # 0xd894
	.quad	55444                   # 0xd894
	.quad	55444                   # 0xd894
	.quad	55444                   # 0xd894
	.quad	55444                   # 0xd894
	.quad	55444                   # 0xd894
	.quad	55444                   # 0xd894
	.quad	55452                   # 0xd89c
	.quad	55436                   # 0xd88c
	.quad	55436                   # 0xd88c
	.quad	55436                   # 0xd88c
	.quad	55444                   # 0xd894
	.quad	55444                   # 0xd894
	.quad	55444                   # 0xd894
	.quad	55444                   # 0xd894
	.quad	55444                   # 0xd894
	.quad	55444                   # 0xd894
	.quad	55444                   # 0xd894
	.quad	55444                   # 0xd894
	.quad	55444                   # 0xd894
	.quad	55444                   # 0xd894
	.quad	55316                   # 0xd814
	.quad	55444                   # 0xd894
	.quad	55444                   # 0xd894
	.quad	79644                   # 0x1371c
	.quad	18717                   # 0x491d
	.quad	79772                   # 0x1379c
	.quad	18845                   # 0x499d
	.quad	18717                   # 0x491d
	.quad	18717                   # 0x491d
	.quad	141596                  # 0x2291c
	.quad	141596                  # 0x2291c
	.quad	141596                  # 0x2291c
	.quad	79500                   # 0x1368c
	.quad	1190172                 # 0x12291c
	.quad	79772                   # 0x1379c
	.quad	182292                  # 0x2c814
	.quad	54784                   # 0xd600
	.quad	54796                   # 0xd60c
	.quad	182276                  # 0x2c804
	.quad	54788                   # 0xd604
	.quad	54804                   # 0xd614
	.quad	28                      # 0x1c
	.quad	284                     # 0x11c
	.quad	141596                  # 0x2291c
	.quad	338205                  # 0x5291d
	.quad	338269                  # 0x5295d
	.quad	18781                   # 0x495d
	.quad	284                     # 0x11c
	.quad	79644                   # 0x1371c
	.quad	1255708                 # 0x13291c
	.quad	141596                  # 0x2291c
	.quad	2238748                 # 0x22291c
	.quad	284                     # 0x11c
	.quad	0                       # 0x0
	.quad	79368                   # 0x13608
	.quad	79368                   # 0x13608
	.quad	79516                   # 0x1369c
	.quad	79644                   # 0x1371c
	.quad	2238493                 # 0x22281d
	.quad	2238493                 # 0x22281d
	.quad	35792925                # 0x222281d
	.quad	35792925                # 0x222281d
	.quad	141341                  # 0x2281d
	.quad	2238493                 # 0x22281d
	.quad	2238493                 # 0x22281d
	.quad	572663837               # 0x2222281d
	.quad	2238493                 # 0x22281d
	.quad	35792925                # 0x222281d
	.quad	141341                  # 0x2281d
	.quad	79552                   # 0x136c0
	.quad	8768                    # 0x2240
	.quad	13888                   # 0x3640
	.quad	8704                    # 0x2200
	.quad	1536                    # 0x600
	.quad	2048                    # 0x800
	.quad	13824                   # 0x3600
	.quad	141312                  # 0x22800
	.quad	0                       # 0x0
	.quad	13824                   # 0x3600
	.quad	141312                  # 0x22800
	.quad	0                       # 0x0
	.quad	13824                   # 0x3600
	.quad	10240                   # 0x2800
	.quad	0                       # 0x0
	.quad	141312                  # 0x22800
	.quad	141312                  # 0x22800
	.quad	0                       # 0x0
	.quad	13844                   # 0x3614
	.quad	13844                   # 0x3614
	.quad	13844                   # 0x3614
	.quad	13844                   # 0x3614
	.quad	20                      # 0x14
	.quad	20                      # 0x14
	.quad	20                      # 0x14
	.quad	20                      # 0x14
	.quad	13824                   # 0x3600
	.quad	13824                   # 0x3600
	.quad	0                       # 0x0
	.quad	20                      # 0x14
	.quad	20                      # 0x14
	.quad	13824                   # 0x3600
	.quad	13824                   # 0x3600
	.quad	0                       # 0x0
	.quad	20                      # 0x14
	.quad	20                      # 0x14
	.quad	12                      # 0xc
	.quad	272413                  # 0x4281d
	.quad	62980                   # 0xf604
	.quad	68                      # 0x44
	.quad	5124                    # 0x1404
	.quad	3136                    # 0xc40
	.quad	0                       # 0x0
	.size	PL_opargs, 2816

	.type	.L.str572,@object       # @.str572
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str572:
	.asciz	" "
	.size	.L.str572, 2

	.type	PL_splitstr,@object     # @PL_splitstr
	.data
	.globl	PL_splitstr
	.align	8
PL_splitstr:
	.quad	.L.str572
	.size	PL_splitstr, 8

	.type	PL_perl_destruct_level,@object # @PL_perl_destruct_level
	.bss
	.globl	PL_perl_destruct_level
	.align	4
PL_perl_destruct_level:
	.long	0                       # 0x0
	.size	PL_perl_destruct_level, 4

	.type	PL_maxsysfd,@object     # @PL_maxsysfd
	.data
	.globl	PL_maxsysfd
	.align	8
PL_maxsysfd:
	.quad	2                       # 0x2
	.size	PL_maxsysfd, 8

	.type	PL_sub_generation,@object # @PL_sub_generation
	.globl	PL_sub_generation
	.align	8
PL_sub_generation:
	.quad	1                       # 0x1
	.size	PL_sub_generation, 8

	.type	PL_op_mask,@object      # @PL_op_mask
	.bss
	.globl	PL_op_mask
	.align	8
PL_op_mask:
	.quad	0
	.size	PL_op_mask, 8

	.type	PL_curcopdb,@object     # @PL_curcopdb
	.globl	PL_curcopdb
	.align	8
PL_curcopdb:
	.quad	0
	.size	PL_curcopdb, 8

	.type	PL_copline,@object      # @PL_copline
	.data
	.globl	PL_copline
	.align	8
PL_copline:
	.quad	4294967295              # 0xffffffff
	.size	PL_copline, 8

	.type	PL_gensym,@object       # @PL_gensym
	.bss
	.globl	PL_gensym
	.align	8
PL_gensym:
	.quad	0                       # 0x0
	.size	PL_gensym, 8

	.type	PL_laststatval,@object  # @PL_laststatval
	.data
	.globl	PL_laststatval
	.align	4
PL_laststatval:
	.long	4294967295              # 0xffffffff
	.size	PL_laststatval, 4

	.type	PL_laststype,@object    # @PL_laststype
	.globl	PL_laststype
	.align	8
PL_laststype:
	.quad	235                     # 0xeb
	.size	PL_laststype, 8

	.type	PL_exitlist,@object     # @PL_exitlist
	.bss
	.globl	PL_exitlist
	.align	8
PL_exitlist:
	.quad	0
	.size	PL_exitlist, 8

	.type	PL_exitlistlen,@object  # @PL_exitlistlen
	.globl	PL_exitlistlen
	.align	8
PL_exitlistlen:
	.quad	0                       # 0x0
	.size	PL_exitlistlen, 8

	.type	PL_profiledata,@object  # @PL_profiledata
	.globl	PL_profiledata
	.align	8
PL_profiledata:
	.quad	0
	.size	PL_profiledata, 8

	.type	PL_rsfp,@object         # @PL_rsfp
	.globl	PL_rsfp
	.align	8
PL_rsfp:
	.quad	0
	.size	PL_rsfp, 8

	.type	PL_rsfp_filters,@object # @PL_rsfp_filters
	.globl	PL_rsfp_filters
	.align	8
PL_rsfp_filters:
	.quad	0
	.size	PL_rsfp_filters, 8

	.type	PL_generation,@object   # @PL_generation
	.data
	.globl	PL_generation
	.align	4
PL_generation:
	.long	100                     # 0x64
	.size	PL_generation, 4

	.type	PL_in_clean_objs,@object # @PL_in_clean_objs
	.bss
	.globl	PL_in_clean_objs
PL_in_clean_objs:
	.byte	0                       # 0x0
	.size	PL_in_clean_objs, 1

	.type	PL_in_clean_all,@object # @PL_in_clean_all
	.globl	PL_in_clean_all
PL_in_clean_all:
	.byte	0                       # 0x0
	.size	PL_in_clean_all, 1

	.type	PL_an,@object           # @PL_an
	.globl	PL_an
	.align	8
PL_an:
	.quad	0                       # 0x0
	.size	PL_an, 8

	.type	PL_cop_seqmax,@object   # @PL_cop_seqmax
	.globl	PL_cop_seqmax
	.align	8
PL_cop_seqmax:
	.quad	0                       # 0x0
	.size	PL_cop_seqmax, 8

	.type	PL_op_seqmax,@object    # @PL_op_seqmax
	.globl	PL_op_seqmax
	.align	2
PL_op_seqmax:
	.short	0                       # 0x0
	.size	PL_op_seqmax, 2

	.type	PL_evalseq,@object      # @PL_evalseq
	.globl	PL_evalseq
	.align	8
PL_evalseq:
	.quad	0                       # 0x0
	.size	PL_evalseq, 8

	.type	PL_maxo,@object         # @PL_maxo
	.data
	.globl	PL_maxo
	.align	4
PL_maxo:
	.long	352                     # 0x160
	.size	PL_maxo, 4

	.type	.L.str573,@object       # @.str573
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str573:
	.asciz	"/bin/sh"
	.size	.L.str573, 8

	.type	PL_sh_path_compat,@object # @PL_sh_path_compat
	.data
	.globl	PL_sh_path_compat
	.align	8
PL_sh_path_compat:
	.quad	.L.str573
	.size	PL_sh_path_compat, 8

	.type	PL_runops,@object       # @PL_runops
	.globl	PL_runops
	.align	8
PL_runops:
	.quad	Perl_runops_standard
	.size	PL_runops, 8

	.type	.L.str574,@object       # @.str574
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str574:
	.zero	1
	.size	.L.str574, 1

	.type	PL_cshname,@object      # @PL_cshname
	.data
	.globl	PL_cshname
	.align	8
PL_cshname:
	.quad	.L.str574
	.size	PL_cshname, 8

	.type	PL_cshlen,@object       # @PL_cshlen
	.bss
	.globl	PL_cshlen
	.align	8
PL_cshlen:
	.quad	0                       # 0x0
	.size	PL_cshlen, 8

	.type	PL_expect,@object       # @PL_expect
	.data
	.globl	PL_expect
	.align	4
PL_expect:
	.long	3                       # 0x3
	.size	PL_expect, 4

	.type	PL_amagic_generation,@object # @PL_amagic_generation
	.bss
	.globl	PL_amagic_generation
	.align	8
PL_amagic_generation:
	.quad	0                       # 0x0
	.size	PL_amagic_generation, 8

	.type	PL_glob_index,@object   # @PL_glob_index
	.globl	PL_glob_index
	.align	4
PL_glob_index:
	.long	0                       # 0x0
	.size	PL_glob_index, 4

	.type	PL_beginav_save,@object # @PL_beginav_save
	.globl	PL_beginav_save
	.align	8
PL_beginav_save:
	.quad	0
	.size	PL_beginav_save, 8

	.type	PL_sig_pending,@object  # @PL_sig_pending
	.globl	PL_sig_pending
	.align	4
PL_sig_pending:
	.long	0                       # 0x0
	.size	PL_sig_pending, 4

	.type	PL_savebegin,@object    # @PL_savebegin
	.globl	PL_savebegin
PL_savebegin:
	.byte	0                       # 0x0
	.size	PL_savebegin, 1

	.type	PL_perlio,@object       # @PL_perlio
	.globl	PL_perlio
	.align	8
PL_perlio:
	.quad	0
	.size	PL_perlio, 8

	.type	PL_known_layers,@object # @PL_known_layers
	.globl	PL_known_layers
	.align	8
PL_known_layers:
	.quad	0
	.size	PL_known_layers, 8

	.type	PL_def_layerlist,@object # @PL_def_layerlist
	.globl	PL_def_layerlist
	.align	8
PL_def_layerlist:
	.quad	0
	.size	PL_def_layerlist, 8

	.type	PL_encoding,@object     # @PL_encoding
	.globl	PL_encoding
	.align	8
PL_encoding:
	.quad	0
	.size	PL_encoding, 8

	.type	PL_checkav_save,@object # @PL_checkav_save
	.globl	PL_checkav_save
	.align	8
PL_checkav_save:
	.quad	0
	.size	PL_checkav_save, 8

	.type	PL_clocktick,@object    # @PL_clocktick
	.globl	PL_clocktick
	.align	8
PL_clocktick:
	.quad	0                       # 0x0
	.size	PL_clocktick, 8

	.type	PL_in_load_module,@object # @PL_in_load_module
	.globl	PL_in_load_module
	.align	4
PL_in_load_module:
	.long	0                       # 0x0
	.size	PL_in_load_module, 4

	.type	PL_sharehook,@object    # @PL_sharehook
	.data
	.globl	PL_sharehook
	.align	8
PL_sharehook:
	.quad	Perl_sv_nosharing
	.size	PL_sharehook, 8

	.type	PL_lockhook,@object     # @PL_lockhook
	.globl	PL_lockhook
	.align	8
PL_lockhook:
	.quad	Perl_sv_nolocking
	.size	PL_lockhook, 8

	.type	PL_unlockhook,@object   # @PL_unlockhook
	.globl	PL_unlockhook
	.align	8
PL_unlockhook:
	.quad	Perl_sv_nounlocking
	.size	PL_unlockhook, 8

	.type	PL_threadhook,@object   # @PL_threadhook
	.globl	PL_threadhook
	.align	8
PL_threadhook:
	.quad	Perl_nothreadhook
	.size	PL_threadhook, 8

	.type	PL_runops_std,@object   # @PL_runops_std
	.globl	PL_runops_std
	.align	8
PL_runops_std:
	.quad	Perl_runops_standard
	.size	PL_runops_std, 8

	.type	PL_runops_dbg,@object   # @PL_runops_dbg
	.globl	PL_runops_dbg
	.align	8
PL_runops_dbg:
	.quad	Perl_runops_debug
	.size	PL_runops_dbg, 8

	.type	PL_hash_seed,@object    # @PL_hash_seed
	.bss
	.globl	PL_hash_seed
	.align	8
PL_hash_seed:
	.quad	0                       # 0x0
	.size	PL_hash_seed, 8

	.type	PL_hash_seed_set,@object # @PL_hash_seed_set
	.globl	PL_hash_seed_set
PL_hash_seed_set:
	.byte	0                       # 0x0
	.size	PL_hash_seed_set, 1

	.type	PL_rehash_seed,@object  # @PL_rehash_seed
	.globl	PL_rehash_seed
	.align	8
PL_rehash_seed:
	.quad	0                       # 0x0
	.size	PL_rehash_seed, 8

	.type	PL_rehash_seed_set,@object # @PL_rehash_seed_set
	.globl	PL_rehash_seed_set
PL_rehash_seed_set:
	.byte	0                       # 0x0
	.size	PL_rehash_seed_set, 1

	.type	PL_fdscript,@object     # @PL_fdscript
	.data
	.globl	PL_fdscript
	.align	4
PL_fdscript:
	.long	4294967295              # 0xffffffff
	.size	PL_fdscript, 4

	.type	PL_suidscript,@object   # @PL_suidscript
	.globl	PL_suidscript
	.align	4
PL_suidscript:
	.long	4294967295              # 0xffffffff
	.size	PL_suidscript, 4

	.type	PL_tmps_ix,@object      # @PL_tmps_ix
	.globl	PL_tmps_ix
	.align	8
PL_tmps_ix:
	.quad	-1                      # 0xffffffffffffffff
	.size	PL_tmps_ix, 8

	.type	PL_tmps_floor,@object   # @PL_tmps_floor
	.globl	PL_tmps_floor
	.align	8
PL_tmps_floor:
	.quad	-1                      # 0xffffffffffffffff
	.size	PL_tmps_floor, 8

	.type	PL_statname,@object     # @PL_statname
	.bss
	.globl	PL_statname
	.align	8
PL_statname:
	.quad	0
	.size	PL_statname, 8

	.type	.L.str575,@object       # @.str575
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str575:
	.asciz	" \n-"
	.size	.L.str575, 4

	.type	PL_chopset,@object      # @PL_chopset
	.data
	.globl	PL_chopset
	.align	8
PL_chopset:
	.quad	.L.str575
	.size	PL_chopset, 8

	.type	PL_compiling,@object    # @PL_compiling
	.comm	PL_compiling,104,8
	.type	PL_curcop,@object       # @PL_curcop
	.globl	PL_curcop
	.align	8
PL_curcop:
	.quad	PL_compiling
	.size	PL_curcop, 8

	.type	PL_dirty,@object        # @PL_dirty
	.bss
	.globl	PL_dirty
PL_dirty:
	.byte	0                       # 0x0
	.size	PL_dirty, 1

	.type	PL_errors,@object       # @PL_errors
	.globl	PL_errors
	.align	8
PL_errors:
	.quad	0
	.size	PL_errors, 8

	.type	PL_dumpindent,@object   # @PL_dumpindent
	.data
	.globl	PL_dumpindent
	.align	8
PL_dumpindent:
	.quad	4                       # 0x4
	.size	PL_dumpindent, 8

	.type	PL_maxscream,@object    # @PL_maxscream
	.globl	PL_maxscream
	.align	8
PL_maxscream:
	.quad	-1                      # 0xffffffffffffffff
	.size	PL_maxscream, 8

	.type	PL_regindent,@object    # @PL_regindent
	.bss
	.globl	PL_regindent
	.align	4
PL_regindent:
	.long	0                       # 0x0
	.size	PL_regindent, 4

	.type	PL_reg_oldcurpm,@object # @PL_reg_oldcurpm
	.globl	PL_reg_oldcurpm
	.align	8
PL_reg_oldcurpm:
	.quad	0
	.size	PL_reg_oldcurpm, 8

	.type	PL_reg_curpm,@object    # @PL_reg_curpm
	.globl	PL_reg_curpm
	.align	8
PL_reg_curpm:
	.quad	0
	.size	PL_reg_curpm, 8

	.type	PL_reg_poscache,@object # @PL_reg_poscache
	.globl	PL_reg_poscache
	.align	8
PL_reg_poscache:
	.quad	0
	.size	PL_reg_poscache, 8

	.type	PL_peepp,@object        # @PL_peepp
	.data
	.globl	PL_peepp
	.align	8
PL_peepp:
	.quad	Perl_peep
	.size	PL_peepp, 8

	.type	PL_regcompp,@object     # @PL_regcompp
	.globl	PL_regcompp
	.align	8
PL_regcompp:
	.quad	Perl_pregcomp
	.size	PL_regcompp, 8

	.type	PL_regexecp,@object     # @PL_regexecp
	.globl	PL_regexecp
	.align	8
PL_regexecp:
	.quad	Perl_regexec_flags
	.size	PL_regexecp, 8

	.type	PL_regint_start,@object # @PL_regint_start
	.globl	PL_regint_start
	.align	8
PL_regint_start:
	.quad	Perl_re_intuit_start
	.size	PL_regint_start, 8

	.type	PL_regint_string,@object # @PL_regint_string
	.globl	PL_regint_string
	.align	8
PL_regint_string:
	.quad	Perl_re_intuit_string
	.size	PL_regint_string, 8

	.type	PL_regfree,@object      # @PL_regfree
	.globl	PL_regfree
	.align	8
PL_regfree:
	.quad	Perl_pregfree
	.size	PL_regfree, 8

	.type	PL_reginterp_cnt,@object # @PL_reginterp_cnt
	.bss
	.globl	PL_reginterp_cnt
	.align	4
PL_reginterp_cnt:
	.long	0                       # 0x0
	.size	PL_reginterp_cnt, 4

	.type	PL_reg_starttry,@object # @PL_reg_starttry
	.globl	PL_reg_starttry
	.align	8
PL_reg_starttry:
	.quad	0
	.size	PL_reg_starttry, 8

	.type	PL_watchaddr,@object    # @PL_watchaddr
	.globl	PL_watchaddr
	.align	8
PL_watchaddr:
	.quad	0
	.size	PL_watchaddr, 8

	.type	.L.str576,@object       # @.str576
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str576:
	.asciz	"1"
	.size	.L.str576, 2

	.type	PL_Yes,@object          # @PL_Yes
	.data
	.globl	PL_Yes
	.align	8
PL_Yes:
	.quad	.L.str576
	.size	PL_Yes, 8

	.type	PL_No,@object           # @PL_No
	.globl	PL_No
	.align	8
PL_No:
	.quad	.L.str574
	.size	PL_No, 8

	.type	.L.str577,@object       # @.str577
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str577:
	.asciz	"0123456789abcdef0123456789ABCDEF"
	.size	.L.str577, 33

	.type	PL_hexdigit,@object     # @PL_hexdigit
	.data
	.globl	PL_hexdigit
	.align	8
PL_hexdigit:
	.quad	.L.str577
	.size	PL_hexdigit, 8

	.type	.L.str578,@object       # @.str578
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str578:
	.asciz	"\\.^$@dDwWsSbB+*?|()-nrtfeaxc0123456789[{]}"
	.size	.L.str578, 43

	.type	PL_patleave,@object     # @PL_patleave
	.data
	.globl	PL_patleave
	.align	8
PL_patleave:
	.quad	.L.str578
	.size	PL_patleave, 8

	.type	PL_do_undump,@object    # @PL_do_undump
	.bss
	.globl	PL_do_undump
PL_do_undump:
	.byte	0                       # 0x0
	.size	PL_do_undump, 1

	.type	PL_sh_path,@object      # @PL_sh_path
	.data
	.globl	PL_sh_path
	.align	8
PL_sh_path:
	.quad	.L.str573
	.size	PL_sh_path, 8

	.type	PL_csighandlerp,@object # @PL_csighandlerp
	.globl	PL_csighandlerp
	.align	8
PL_csighandlerp:
	.quad	Perl_csighandler
	.size	PL_csighandlerp, 8

	.type	PL_use_safe_putenv,@object # @PL_use_safe_putenv
	.globl	PL_use_safe_putenv
	.align	4
PL_use_safe_putenv:
	.long	1                       # 0x1
	.size	PL_use_safe_putenv, 4

	.type	PL_vtbl_sv,@object      # @PL_vtbl_sv
	.globl	PL_vtbl_sv
	.align	8
PL_vtbl_sv:
	.quad	Perl_magic_get
	.quad	Perl_magic_set
	.quad	Perl_magic_len
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.size	PL_vtbl_sv, 56

	.type	PL_vtbl_env,@object     # @PL_vtbl_env
	.globl	PL_vtbl_env
	.align	8
PL_vtbl_env:
	.quad	0
	.quad	Perl_magic_set_all_env
	.quad	0
	.quad	Perl_magic_clear_all_env
	.quad	0
	.quad	0
	.quad	0
	.size	PL_vtbl_env, 56

	.type	PL_vtbl_envelem,@object # @PL_vtbl_envelem
	.globl	PL_vtbl_envelem
	.align	8
PL_vtbl_envelem:
	.quad	0
	.quad	Perl_magic_setenv
	.quad	0
	.quad	Perl_magic_clearenv
	.quad	0
	.quad	0
	.quad	0
	.size	PL_vtbl_envelem, 56

	.type	PL_vtbl_sig,@object     # @PL_vtbl_sig
	.bss
	.globl	PL_vtbl_sig
	.align	8
PL_vtbl_sig:
	.zero	56
	.size	PL_vtbl_sig, 56

	.type	PL_vtbl_sigelem,@object # @PL_vtbl_sigelem
	.data
	.globl	PL_vtbl_sigelem
	.align	8
PL_vtbl_sigelem:
	.quad	Perl_magic_getsig
	.quad	Perl_magic_setsig
	.quad	0
	.quad	Perl_magic_clearsig
	.quad	0
	.quad	0
	.quad	0
	.size	PL_vtbl_sigelem, 56

	.type	PL_vtbl_pack,@object    # @PL_vtbl_pack
	.globl	PL_vtbl_pack
	.align	8
PL_vtbl_pack:
	.quad	0
	.quad	0
	.quad	Perl_magic_sizepack
	.quad	Perl_magic_wipepack
	.quad	0
	.quad	0
	.quad	0
	.size	PL_vtbl_pack, 56

	.type	PL_vtbl_packelem,@object # @PL_vtbl_packelem
	.globl	PL_vtbl_packelem
	.align	8
PL_vtbl_packelem:
	.quad	Perl_magic_getpack
	.quad	Perl_magic_setpack
	.quad	0
	.quad	Perl_magic_clearpack
	.quad	0
	.quad	0
	.quad	0
	.size	PL_vtbl_packelem, 56

	.type	PL_vtbl_dbline,@object  # @PL_vtbl_dbline
	.globl	PL_vtbl_dbline
	.align	8
PL_vtbl_dbline:
	.quad	0
	.quad	Perl_magic_setdbline
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.size	PL_vtbl_dbline, 56

	.type	PL_vtbl_isa,@object     # @PL_vtbl_isa
	.globl	PL_vtbl_isa
	.align	8
PL_vtbl_isa:
	.quad	0
	.quad	Perl_magic_setisa
	.quad	0
	.quad	Perl_magic_setisa
	.quad	0
	.quad	0
	.quad	0
	.size	PL_vtbl_isa, 56

	.type	PL_vtbl_isaelem,@object # @PL_vtbl_isaelem
	.globl	PL_vtbl_isaelem
	.align	8
PL_vtbl_isaelem:
	.quad	0
	.quad	Perl_magic_setisa
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.size	PL_vtbl_isaelem, 56

	.type	PL_vtbl_arylen,@object  # @PL_vtbl_arylen
	.globl	PL_vtbl_arylen
	.align	8
PL_vtbl_arylen:
	.quad	Perl_magic_getarylen
	.quad	Perl_magic_setarylen
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.size	PL_vtbl_arylen, 56

	.type	PL_vtbl_glob,@object    # @PL_vtbl_glob
	.globl	PL_vtbl_glob
	.align	8
PL_vtbl_glob:
	.quad	Perl_magic_getglob
	.quad	Perl_magic_setglob
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.size	PL_vtbl_glob, 56

	.type	PL_vtbl_mglob,@object   # @PL_vtbl_mglob
	.globl	PL_vtbl_mglob
	.align	8
PL_vtbl_mglob:
	.quad	0
	.quad	Perl_magic_setmglob
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.size	PL_vtbl_mglob, 56

	.type	PL_vtbl_nkeys,@object   # @PL_vtbl_nkeys
	.globl	PL_vtbl_nkeys
	.align	8
PL_vtbl_nkeys:
	.quad	Perl_magic_getnkeys
	.quad	Perl_magic_setnkeys
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.size	PL_vtbl_nkeys, 56

	.type	PL_vtbl_taint,@object   # @PL_vtbl_taint
	.globl	PL_vtbl_taint
	.align	8
PL_vtbl_taint:
	.quad	Perl_magic_gettaint
	.quad	Perl_magic_settaint
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.size	PL_vtbl_taint, 56

	.type	PL_vtbl_substr,@object  # @PL_vtbl_substr
	.globl	PL_vtbl_substr
	.align	8
PL_vtbl_substr:
	.quad	Perl_magic_getsubstr
	.quad	Perl_magic_setsubstr
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.size	PL_vtbl_substr, 56

	.type	PL_vtbl_vec,@object     # @PL_vtbl_vec
	.globl	PL_vtbl_vec
	.align	8
PL_vtbl_vec:
	.quad	Perl_magic_getvec
	.quad	Perl_magic_setvec
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.size	PL_vtbl_vec, 56

	.type	PL_vtbl_pos,@object     # @PL_vtbl_pos
	.globl	PL_vtbl_pos
	.align	8
PL_vtbl_pos:
	.quad	Perl_magic_getpos
	.quad	Perl_magic_setpos
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.size	PL_vtbl_pos, 56

	.type	PL_vtbl_bm,@object      # @PL_vtbl_bm
	.globl	PL_vtbl_bm
	.align	8
PL_vtbl_bm:
	.quad	0
	.quad	Perl_magic_setbm
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.size	PL_vtbl_bm, 56

	.type	PL_vtbl_fm,@object      # @PL_vtbl_fm
	.globl	PL_vtbl_fm
	.align	8
PL_vtbl_fm:
	.quad	0
	.quad	Perl_magic_setfm
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.size	PL_vtbl_fm, 56

	.type	PL_vtbl_uvar,@object    # @PL_vtbl_uvar
	.globl	PL_vtbl_uvar
	.align	8
PL_vtbl_uvar:
	.quad	Perl_magic_getuvar
	.quad	Perl_magic_setuvar
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.size	PL_vtbl_uvar, 56

	.type	PL_vtbl_defelem,@object # @PL_vtbl_defelem
	.globl	PL_vtbl_defelem
	.align	8
PL_vtbl_defelem:
	.quad	Perl_magic_getdefelem
	.quad	Perl_magic_setdefelem
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.size	PL_vtbl_defelem, 56

	.type	PL_vtbl_regexp,@object  # @PL_vtbl_regexp
	.globl	PL_vtbl_regexp
	.align	8
PL_vtbl_regexp:
	.quad	0
	.quad	Perl_magic_setregexp
	.quad	0
	.quad	0
	.quad	Perl_magic_freeregexp
	.quad	0
	.quad	0
	.size	PL_vtbl_regexp, 56

	.type	PL_vtbl_regdata,@object # @PL_vtbl_regdata
	.globl	PL_vtbl_regdata
	.align	8
PL_vtbl_regdata:
	.quad	0
	.quad	0
	.quad	Perl_magic_regdata_cnt
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.size	PL_vtbl_regdata, 56

	.type	PL_vtbl_regdatum,@object # @PL_vtbl_regdatum
	.globl	PL_vtbl_regdatum
	.align	8
PL_vtbl_regdatum:
	.quad	Perl_magic_regdatum_get
	.quad	Perl_magic_regdatum_set
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.size	PL_vtbl_regdatum, 56

	.type	PL_vtbl_amagic,@object  # @PL_vtbl_amagic
	.globl	PL_vtbl_amagic
	.align	8
PL_vtbl_amagic:
	.quad	0
	.quad	Perl_magic_setamagic
	.quad	0
	.quad	0
	.quad	Perl_magic_setamagic
	.quad	0
	.quad	0
	.size	PL_vtbl_amagic, 56

	.type	PL_vtbl_amagicelem,@object # @PL_vtbl_amagicelem
	.globl	PL_vtbl_amagicelem
	.align	8
PL_vtbl_amagicelem:
	.quad	0
	.quad	Perl_magic_setamagic
	.quad	0
	.quad	0
	.quad	Perl_magic_setamagic
	.quad	0
	.quad	0
	.size	PL_vtbl_amagicelem, 56

	.type	PL_vtbl_backref,@object # @PL_vtbl_backref
	.globl	PL_vtbl_backref
	.align	8
PL_vtbl_backref:
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	Perl_magic_killbackrefs
	.quad	0
	.quad	0
	.size	PL_vtbl_backref, 56

	.type	PL_vtbl_ovrld,@object   # @PL_vtbl_ovrld
	.globl	PL_vtbl_ovrld
	.align	8
PL_vtbl_ovrld:
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	Perl_magic_freeovrld
	.quad	0
	.quad	0
	.size	PL_vtbl_ovrld, 56

	.type	PL_vtbl_utf8,@object    # @PL_vtbl_utf8
	.globl	PL_vtbl_utf8
	.align	8
PL_vtbl_utf8:
	.quad	0
	.quad	Perl_magic_setutf8
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.size	PL_vtbl_utf8, 56

	.type	.L.str579,@object       # @.str579
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str579:
	.asciz	"()"
	.size	.L.str579, 3

	.type	.L.str580,@object       # @.str580
.L.str580:
	.asciz	"(abs"
	.size	.L.str580, 5

	.type	.L.str581,@object       # @.str581
.L.str581:
	.asciz	"(bool"
	.size	.L.str581, 6

	.type	.L.str582,@object       # @.str582
.L.str582:
	.asciz	"(nomethod"
	.size	.L.str582, 10

	.type	.L.str583,@object       # @.str583
.L.str583:
	.asciz	"(\"\""
	.size	.L.str583, 4

	.type	.L.str584,@object       # @.str584
.L.str584:
	.asciz	"(0+"
	.size	.L.str584, 4

	.type	.L.str585,@object       # @.str585
.L.str585:
	.asciz	"(+"
	.size	.L.str585, 3

	.type	.L.str586,@object       # @.str586
.L.str586:
	.asciz	"(+="
	.size	.L.str586, 4

	.type	.L.str587,@object       # @.str587
.L.str587:
	.asciz	"(-"
	.size	.L.str587, 3

	.type	.L.str588,@object       # @.str588
.L.str588:
	.asciz	"(-="
	.size	.L.str588, 4

	.type	.L.str589,@object       # @.str589
.L.str589:
	.asciz	"(*"
	.size	.L.str589, 3

	.type	.L.str590,@object       # @.str590
.L.str590:
	.asciz	"(*="
	.size	.L.str590, 4

	.type	.L.str591,@object       # @.str591
.L.str591:
	.asciz	"(/"
	.size	.L.str591, 3

	.type	.L.str592,@object       # @.str592
.L.str592:
	.asciz	"(/="
	.size	.L.str592, 4

	.type	.L.str593,@object       # @.str593
.L.str593:
	.asciz	"(%"
	.size	.L.str593, 3

	.type	.L.str594,@object       # @.str594
.L.str594:
	.asciz	"(%="
	.size	.L.str594, 4

	.type	.L.str595,@object       # @.str595
.L.str595:
	.asciz	"(**"
	.size	.L.str595, 4

	.type	.L.str596,@object       # @.str596
.L.str596:
	.asciz	"(**="
	.size	.L.str596, 5

	.type	.L.str597,@object       # @.str597
.L.str597:
	.asciz	"(<<"
	.size	.L.str597, 4

	.type	.L.str598,@object       # @.str598
.L.str598:
	.asciz	"(<<="
	.size	.L.str598, 5

	.type	.L.str599,@object       # @.str599
.L.str599:
	.asciz	"(>>"
	.size	.L.str599, 4

	.type	.L.str600,@object       # @.str600
.L.str600:
	.asciz	"(>>="
	.size	.L.str600, 5

	.type	.L.str601,@object       # @.str601
.L.str601:
	.asciz	"(&"
	.size	.L.str601, 3

	.type	.L.str602,@object       # @.str602
.L.str602:
	.asciz	"(&="
	.size	.L.str602, 4

	.type	.L.str603,@object       # @.str603
.L.str603:
	.asciz	"(|"
	.size	.L.str603, 3

	.type	.L.str604,@object       # @.str604
.L.str604:
	.asciz	"(|="
	.size	.L.str604, 4

	.type	.L.str605,@object       # @.str605
.L.str605:
	.asciz	"(^"
	.size	.L.str605, 3

	.type	.L.str606,@object       # @.str606
.L.str606:
	.asciz	"(^="
	.size	.L.str606, 4

	.type	.L.str607,@object       # @.str607
.L.str607:
	.asciz	"(<"
	.size	.L.str607, 3

	.type	.L.str608,@object       # @.str608
.L.str608:
	.asciz	"(<="
	.size	.L.str608, 4

	.type	.L.str609,@object       # @.str609
.L.str609:
	.asciz	"(>"
	.size	.L.str609, 3

	.type	.L.str610,@object       # @.str610
.L.str610:
	.asciz	"(>="
	.size	.L.str610, 4

	.type	.L.str611,@object       # @.str611
.L.str611:
	.asciz	"(=="
	.size	.L.str611, 4

	.type	.L.str612,@object       # @.str612
.L.str612:
	.asciz	"(!="
	.size	.L.str612, 4

	.type	.L.str613,@object       # @.str613
.L.str613:
	.asciz	"(<=>"
	.size	.L.str613, 5

	.type	.L.str614,@object       # @.str614
.L.str614:
	.asciz	"(cmp"
	.size	.L.str614, 5

	.type	.L.str615,@object       # @.str615
.L.str615:
	.asciz	"(lt"
	.size	.L.str615, 4

	.type	.L.str616,@object       # @.str616
.L.str616:
	.asciz	"(le"
	.size	.L.str616, 4

	.type	.L.str617,@object       # @.str617
.L.str617:
	.asciz	"(gt"
	.size	.L.str617, 4

	.type	.L.str618,@object       # @.str618
.L.str618:
	.asciz	"(ge"
	.size	.L.str618, 4

	.type	.L.str619,@object       # @.str619
.L.str619:
	.asciz	"(eq"
	.size	.L.str619, 4

	.type	.L.str620,@object       # @.str620
.L.str620:
	.asciz	"(ne"
	.size	.L.str620, 4

	.type	.L.str621,@object       # @.str621
.L.str621:
	.asciz	"(!"
	.size	.L.str621, 3

	.type	.L.str622,@object       # @.str622
.L.str622:
	.asciz	"(~"
	.size	.L.str622, 3

	.type	.L.str623,@object       # @.str623
.L.str623:
	.asciz	"(++"
	.size	.L.str623, 4

	.type	.L.str624,@object       # @.str624
.L.str624:
	.asciz	"(--"
	.size	.L.str624, 4

	.type	.L.str625,@object       # @.str625
.L.str625:
	.asciz	"(atan2"
	.size	.L.str625, 7

	.type	.L.str626,@object       # @.str626
.L.str626:
	.asciz	"(cos"
	.size	.L.str626, 5

	.type	.L.str627,@object       # @.str627
.L.str627:
	.asciz	"(sin"
	.size	.L.str627, 5

	.type	.L.str628,@object       # @.str628
.L.str628:
	.asciz	"(exp"
	.size	.L.str628, 5

	.type	.L.str629,@object       # @.str629
.L.str629:
	.asciz	"(log"
	.size	.L.str629, 5

	.type	.L.str630,@object       # @.str630
.L.str630:
	.asciz	"(sqrt"
	.size	.L.str630, 6

	.type	.L.str631,@object       # @.str631
.L.str631:
	.asciz	"(x"
	.size	.L.str631, 3

	.type	.L.str632,@object       # @.str632
.L.str632:
	.asciz	"(x="
	.size	.L.str632, 4

	.type	.L.str633,@object       # @.str633
.L.str633:
	.asciz	"(."
	.size	.L.str633, 3

	.type	.L.str634,@object       # @.str634
.L.str634:
	.asciz	"(.="
	.size	.L.str634, 4

	.type	.L.str635,@object       # @.str635
.L.str635:
	.asciz	"(="
	.size	.L.str635, 3

	.type	.L.str636,@object       # @.str636
.L.str636:
	.asciz	"(neg"
	.size	.L.str636, 5

	.type	.L.str637,@object       # @.str637
.L.str637:
	.asciz	"(${}"
	.size	.L.str637, 5

	.type	.L.str638,@object       # @.str638
.L.str638:
	.asciz	"(@{}"
	.size	.L.str638, 5

	.type	.L.str639,@object       # @.str639
.L.str639:
	.asciz	"(%{}"
	.size	.L.str639, 5

	.type	.L.str640,@object       # @.str640
.L.str640:
	.asciz	"(*{}"
	.size	.L.str640, 5

	.type	.L.str641,@object       # @.str641
.L.str641:
	.asciz	"(&{}"
	.size	.L.str641, 5

	.type	.L.str642,@object       # @.str642
.L.str642:
	.asciz	"(<>"
	.size	.L.str642, 4

	.type	.L.str643,@object       # @.str643
.L.str643:
	.asciz	"(int"
	.size	.L.str643, 5

	.type	.L.str644,@object       # @.str644
.L.str644:
	.asciz	"DESTROY"
	.size	.L.str644, 8

	.type	PL_AMG_names,@object    # @PL_AMG_names
	.data
	.globl	PL_AMG_names
	.align	16
PL_AMG_names:
	.quad	.L.str579
	.quad	.L.str580
	.quad	.L.str581
	.quad	.L.str582
	.quad	.L.str583
	.quad	.L.str584
	.quad	.L.str585
	.quad	.L.str586
	.quad	.L.str587
	.quad	.L.str588
	.quad	.L.str589
	.quad	.L.str590
	.quad	.L.str591
	.quad	.L.str592
	.quad	.L.str593
	.quad	.L.str594
	.quad	.L.str595
	.quad	.L.str596
	.quad	.L.str597
	.quad	.L.str598
	.quad	.L.str599
	.quad	.L.str600
	.quad	.L.str601
	.quad	.L.str602
	.quad	.L.str603
	.quad	.L.str604
	.quad	.L.str605
	.quad	.L.str606
	.quad	.L.str607
	.quad	.L.str608
	.quad	.L.str609
	.quad	.L.str610
	.quad	.L.str611
	.quad	.L.str612
	.quad	.L.str613
	.quad	.L.str614
	.quad	.L.str615
	.quad	.L.str616
	.quad	.L.str617
	.quad	.L.str618
	.quad	.L.str619
	.quad	.L.str620
	.quad	.L.str621
	.quad	.L.str622
	.quad	.L.str623
	.quad	.L.str624
	.quad	.L.str625
	.quad	.L.str626
	.quad	.L.str627
	.quad	.L.str628
	.quad	.L.str629
	.quad	.L.str630
	.quad	.L.str631
	.quad	.L.str632
	.quad	.L.str633
	.quad	.L.str634
	.quad	.L.str635
	.quad	.L.str636
	.quad	.L.str637
	.quad	.L.str638
	.quad	.L.str639
	.quad	.L.str640
	.quad	.L.str641
	.quad	.L.str642
	.quad	.L.str643
	.quad	.L.str644
	.size	PL_AMG_names, 528

	.type	PL_origargc,@object     # @PL_origargc
	.comm	PL_origargc,4,4
	.type	PL_origargv,@object     # @PL_origargv
	.comm	PL_origargv,8,8
	.type	PL_envgv,@object        # @PL_envgv
	.comm	PL_envgv,8,8
	.type	PL_incgv,@object        # @PL_incgv
	.comm	PL_incgv,8,8
	.type	PL_hintgv,@object       # @PL_hintgv
	.comm	PL_hintgv,8,8
	.type	PL_origfilename,@object # @PL_origfilename
	.comm	PL_origfilename,8,8
	.type	PL_diehook,@object      # @PL_diehook
	.comm	PL_diehook,8,8
	.type	PL_warnhook,@object     # @PL_warnhook
	.comm	PL_warnhook,8,8
	.type	PL_minus_c,@object      # @PL_minus_c
	.comm	PL_minus_c,1,1
	.type	PL_patchlevel,@object   # @PL_patchlevel
	.comm	PL_patchlevel,8,8
	.type	PL_localpatches,@object # @PL_localpatches
	.comm	PL_localpatches,8,8
	.type	PL_preprocess,@object   # @PL_preprocess
	.comm	PL_preprocess,1,1
	.type	PL_minus_n,@object      # @PL_minus_n
	.comm	PL_minus_n,1,1
	.type	PL_minus_p,@object      # @PL_minus_p
	.comm	PL_minus_p,1,1
	.type	PL_minus_l,@object      # @PL_minus_l
	.comm	PL_minus_l,1,1
	.type	PL_minus_a,@object      # @PL_minus_a
	.comm	PL_minus_a,1,1
	.type	PL_minus_F,@object      # @PL_minus_F
	.comm	PL_minus_F,1,1
	.type	PL_doswitches,@object   # @PL_doswitches
	.comm	PL_doswitches,1,1
	.type	PL_dowarn,@object       # @PL_dowarn
	.comm	PL_dowarn,1,1
	.type	PL_widesyscalls,@object # @PL_widesyscalls
	.comm	PL_widesyscalls,1,1
	.type	PL_doextract,@object    # @PL_doextract
	.comm	PL_doextract,1,1
	.type	PL_sawampersand,@object # @PL_sawampersand
	.comm	PL_sawampersand,1,1
	.type	PL_unsafe,@object       # @PL_unsafe
	.comm	PL_unsafe,1,1
	.type	PL_inplace,@object      # @PL_inplace
	.comm	PL_inplace,8,8
	.type	PL_e_script,@object     # @PL_e_script
	.comm	PL_e_script,8,8
	.type	PL_perldb,@object       # @PL_perldb
	.comm	PL_perldb,8,8
	.type	PL_basetime,@object     # @PL_basetime
	.comm	PL_basetime,8,8
	.type	PL_formfeed,@object     # @PL_formfeed
	.comm	PL_formfeed,8,8
	.type	PL_multiline,@object    # @PL_multiline
	.comm	PL_multiline,4,4
	.type	PL_statusvalue,@object  # @PL_statusvalue
	.comm	PL_statusvalue,8,8
	.type	PL_exit_flags,@object   # @PL_exit_flags
	.comm	PL_exit_flags,1,1
	.type	PL_stdingv,@object      # @PL_stdingv
	.comm	PL_stdingv,8,8
	.type	PL_stderrgv,@object     # @PL_stderrgv
	.comm	PL_stderrgv,8,8
	.type	PL_defgv,@object        # @PL_defgv
	.comm	PL_defgv,8,8
	.type	PL_argvgv,@object       # @PL_argvgv
	.comm	PL_argvgv,8,8
	.type	PL_argvoutgv,@object    # @PL_argvoutgv
	.comm	PL_argvoutgv,8,8
	.type	PL_argvout_stack,@object # @PL_argvout_stack
	.comm	PL_argvout_stack,8,8
	.type	PL_replgv,@object       # @PL_replgv
	.comm	PL_replgv,8,8
	.type	PL_errgv,@object        # @PL_errgv
	.comm	PL_errgv,8,8
	.type	PL_DBgv,@object         # @PL_DBgv
	.comm	PL_DBgv,8,8
	.type	PL_DBline,@object       # @PL_DBline
	.comm	PL_DBline,8,8
	.type	PL_DBsub,@object        # @PL_DBsub
	.comm	PL_DBsub,8,8
	.type	PL_DBsingle,@object     # @PL_DBsingle
	.comm	PL_DBsingle,8,8
	.type	PL_DBtrace,@object      # @PL_DBtrace
	.comm	PL_DBtrace,8,8
	.type	PL_DBsignal,@object     # @PL_DBsignal
	.comm	PL_DBsignal,8,8
	.type	PL_lineary,@object      # @PL_lineary
	.comm	PL_lineary,8,8
	.type	PL_dbargs,@object       # @PL_dbargs
	.comm	PL_dbargs,8,8
	.type	PL_debstash,@object     # @PL_debstash
	.comm	PL_debstash,8,8
	.type	PL_globalstash,@object  # @PL_globalstash
	.comm	PL_globalstash,8,8
	.type	PL_curstname,@object    # @PL_curstname
	.comm	PL_curstname,8,8
	.type	PL_beginav,@object      # @PL_beginav
	.comm	PL_beginav,8,8
	.type	PL_endav,@object        # @PL_endav
	.comm	PL_endav,8,8
	.type	PL_checkav,@object      # @PL_checkav
	.comm	PL_checkav,8,8
	.type	PL_initav,@object       # @PL_initav
	.comm	PL_initav,8,8
	.type	PL_strtab,@object       # @PL_strtab
	.comm	PL_strtab,8,8
	.type	PL_sv_count,@object     # @PL_sv_count
	.comm	PL_sv_count,8,8
	.type	PL_sv_objcount,@object  # @PL_sv_objcount
	.comm	PL_sv_objcount,8,8
	.type	PL_sv_root,@object      # @PL_sv_root
	.comm	PL_sv_root,8,8
	.type	PL_sv_arenaroot,@object # @PL_sv_arenaroot
	.comm	PL_sv_arenaroot,8,8
	.type	PL_forkprocess,@object  # @PL_forkprocess
	.comm	PL_forkprocess,4,4
	.type	PL_fdpid,@object        # @PL_fdpid
	.comm	PL_fdpid,8,8
	.type	PL_tainting,@object     # @PL_tainting
	.comm	PL_tainting,1,1
	.type	PL_main_cv,@object      # @PL_main_cv
	.comm	PL_main_cv,8,8
	.type	PL_main_root,@object    # @PL_main_root
	.comm	PL_main_root,8,8
	.type	PL_main_start,@object   # @PL_main_start
	.comm	PL_main_start,8,8
	.type	PL_eval_root,@object    # @PL_eval_root
	.comm	PL_eval_root,8,8
	.type	PL_eval_start,@object   # @PL_eval_start
	.comm	PL_eval_start,8,8
	.type	PL_filemode,@object     # @PL_filemode
	.comm	PL_filemode,4,4
	.type	PL_lastfd,@object       # @PL_lastfd
	.comm	PL_lastfd,4,4
	.type	PL_oldname,@object      # @PL_oldname
	.comm	PL_oldname,8,8
	.type	PL_Argv,@object         # @PL_Argv
	.comm	PL_Argv,8,8
	.type	PL_Cmd,@object          # @PL_Cmd
	.comm	PL_Cmd,8,8
	.type	PL_preambled,@object    # @PL_preambled
	.comm	PL_preambled,1,1
	.type	PL_preambleav,@object   # @PL_preambleav
	.comm	PL_preambleav,8,8
	.type	PL_mess_sv,@object      # @PL_mess_sv
	.comm	PL_mess_sv,8,8
	.type	PL_ors_sv,@object       # @PL_ors_sv
	.comm	PL_ors_sv,8,8
	.type	PL_ofmt,@object         # @PL_ofmt
	.comm	PL_ofmt,8,8
	.type	PL_modglobal,@object    # @PL_modglobal
	.comm	PL_modglobal,8,8
	.type	PL_compcv,@object       # @PL_compcv
	.comm	PL_compcv,8,8
	.type	PL_BINCOMPAT0,@object   # @PL_BINCOMPAT0
	.comm	PL_BINCOMPAT0,8,8
	.type	PL_comppad_name,@object # @PL_comppad_name
	.comm	PL_comppad_name,8,8
	.type	PL_comppad_name_fill,@object # @PL_comppad_name_fill
	.comm	PL_comppad_name_fill,8,8
	.type	PL_comppad_name_floor,@object # @PL_comppad_name_floor
	.comm	PL_comppad_name_floor,8,8
	.type	PL_DBcv,@object         # @PL_DBcv
	.comm	PL_DBcv,8,8
	.type	PL_linestart,@object    # @PL_linestart
	.comm	PL_linestart,8,8
	.type	PL_pending_ident,@object # @PL_pending_ident
	.comm	PL_pending_ident,1,1
	.type	PL_sublex_info,@object  # @PL_sublex_info
	.comm	PL_sublex_info,40,8
	.type	PL_uid,@object          # @PL_uid
	.comm	PL_uid,4,4
	.type	PL_euid,@object         # @PL_euid
	.comm	PL_euid,4,4
	.type	PL_gid,@object          # @PL_gid
	.comm	PL_gid,4,4
	.type	PL_egid,@object         # @PL_egid
	.comm	PL_egid,4,4
	.type	PL_nomemok,@object      # @PL_nomemok
	.comm	PL_nomemok,1,1
	.type	PL_origenviron,@object  # @PL_origenviron
	.comm	PL_origenviron,8,8
	.type	PL_origalen,@object     # @PL_origalen
	.comm	PL_origalen,8,8
	.type	PL_pidstatus,@object    # @PL_pidstatus
	.comm	PL_pidstatus,8,8
	.type	PL_osname,@object       # @PL_osname
	.comm	PL_osname,8,8
	.type	PL_sighandlerp,@object  # @PL_sighandlerp
	.comm	PL_sighandlerp,8,8
	.type	PL_xiv_arenaroot,@object # @PL_xiv_arenaroot
	.comm	PL_xiv_arenaroot,8,8
	.type	PL_xiv_root,@object     # @PL_xiv_root
	.comm	PL_xiv_root,8,8
	.type	PL_xnv_root,@object     # @PL_xnv_root
	.comm	PL_xnv_root,8,8
	.type	PL_xrv_root,@object     # @PL_xrv_root
	.comm	PL_xrv_root,8,8
	.type	PL_xpv_root,@object     # @PL_xpv_root
	.comm	PL_xpv_root,8,8
	.type	PL_xpviv_root,@object   # @PL_xpviv_root
	.comm	PL_xpviv_root,8,8
	.type	PL_xpvnv_root,@object   # @PL_xpvnv_root
	.comm	PL_xpvnv_root,8,8
	.type	PL_xpvcv_root,@object   # @PL_xpvcv_root
	.comm	PL_xpvcv_root,8,8
	.type	PL_xpvav_root,@object   # @PL_xpvav_root
	.comm	PL_xpvav_root,8,8
	.type	PL_xpvhv_root,@object   # @PL_xpvhv_root
	.comm	PL_xpvhv_root,8,8
	.type	PL_xpvmg_root,@object   # @PL_xpvmg_root
	.comm	PL_xpvmg_root,8,8
	.type	PL_xpvlv_root,@object   # @PL_xpvlv_root
	.comm	PL_xpvlv_root,8,8
	.type	PL_xpvbm_root,@object   # @PL_xpvbm_root
	.comm	PL_xpvbm_root,8,8
	.type	PL_he_root,@object      # @PL_he_root
	.comm	PL_he_root,8,8
	.type	PL_nice_chunk,@object   # @PL_nice_chunk
	.comm	PL_nice_chunk,8,8
	.type	PL_nice_chunk_size,@object # @PL_nice_chunk_size
	.comm	PL_nice_chunk_size,8,8
	.type	PL_tokenbuf,@object     # @PL_tokenbuf
	.comm	PL_tokenbuf,256,16
	.type	PL_sv_undef,@object     # @PL_sv_undef
	.comm	PL_sv_undef,24,8
	.type	PL_sv_no,@object        # @PL_sv_no
	.comm	PL_sv_no,24,8
	.type	PL_sv_yes,@object       # @PL_sv_yes
	.comm	PL_sv_yes,24,8
	.type	PL_lex_state,@object    # @PL_lex_state
	.comm	PL_lex_state,8,8
	.type	PL_lex_defer,@object    # @PL_lex_defer
	.comm	PL_lex_defer,8,8
	.type	PL_lex_expect,@object   # @PL_lex_expect
	.comm	PL_lex_expect,4,4
	.type	PL_lex_brackets,@object # @PL_lex_brackets
	.comm	PL_lex_brackets,8,8
	.type	PL_lex_formbrack,@object # @PL_lex_formbrack
	.comm	PL_lex_formbrack,8,8
	.type	PL_lex_casemods,@object # @PL_lex_casemods
	.comm	PL_lex_casemods,8,8
	.type	PL_lex_dojoin,@object   # @PL_lex_dojoin
	.comm	PL_lex_dojoin,8,8
	.type	PL_lex_starts,@object   # @PL_lex_starts
	.comm	PL_lex_starts,8,8
	.type	PL_lex_stuff,@object    # @PL_lex_stuff
	.comm	PL_lex_stuff,8,8
	.type	PL_lex_repl,@object     # @PL_lex_repl
	.comm	PL_lex_repl,8,8
	.type	PL_lex_op,@object       # @PL_lex_op
	.comm	PL_lex_op,8,8
	.type	PL_lex_inpat,@object    # @PL_lex_inpat
	.comm	PL_lex_inpat,8,8
	.type	PL_lex_inwhat,@object   # @PL_lex_inwhat
	.comm	PL_lex_inwhat,8,8
	.type	PL_lex_brackstack,@object # @PL_lex_brackstack
	.comm	PL_lex_brackstack,8,8
	.type	PL_lex_casestack,@object # @PL_lex_casestack
	.comm	PL_lex_casestack,8,8
	.type	PL_nextval,@object      # @PL_nextval
	.comm	PL_nextval,40,16
	.type	PL_nexttype,@object     # @PL_nexttype
	.comm	PL_nexttype,40,16
	.type	PL_nexttoke,@object     # @PL_nexttoke
	.comm	PL_nexttoke,8,8
	.type	PL_linestr,@object      # @PL_linestr
	.comm	PL_linestr,8,8
	.type	PL_bufptr,@object       # @PL_bufptr
	.comm	PL_bufptr,8,8
	.type	PL_oldbufptr,@object    # @PL_oldbufptr
	.comm	PL_oldbufptr,8,8
	.type	PL_oldoldbufptr,@object # @PL_oldoldbufptr
	.comm	PL_oldoldbufptr,8,8
	.type	PL_bufend,@object       # @PL_bufend
	.comm	PL_bufend,8,8
	.type	PL_multi_start,@object  # @PL_multi_start
	.comm	PL_multi_start,8,8
	.type	PL_multi_end,@object    # @PL_multi_end
	.comm	PL_multi_end,8,8
	.type	PL_multi_open,@object   # @PL_multi_open
	.comm	PL_multi_open,8,8
	.type	PL_multi_close,@object  # @PL_multi_close
	.comm	PL_multi_close,8,8
	.type	PL_error_count,@object  # @PL_error_count
	.comm	PL_error_count,8,8
	.type	PL_subline,@object      # @PL_subline
	.comm	PL_subline,8,8
	.type	PL_subname,@object      # @PL_subname
	.comm	PL_subname,8,8
	.type	PL_min_intro_pending,@object # @PL_min_intro_pending
	.comm	PL_min_intro_pending,8,8
	.type	PL_max_intro_pending,@object # @PL_max_intro_pending
	.comm	PL_max_intro_pending,8,8
	.type	PL_padix,@object        # @PL_padix
	.comm	PL_padix,8,8
	.type	PL_padix_floor,@object  # @PL_padix_floor
	.comm	PL_padix_floor,8,8
	.type	PL_pad_reset_pending,@object # @PL_pad_reset_pending
	.comm	PL_pad_reset_pending,8,8
	.type	PL_last_uni,@object     # @PL_last_uni
	.comm	PL_last_uni,8,8
	.type	PL_last_lop,@object     # @PL_last_lop
	.comm	PL_last_lop,8,8
	.type	PL_last_lop_op,@object  # @PL_last_lop_op
	.comm	PL_last_lop_op,2,2
	.type	PL_in_my,@object        # @PL_in_my
	.comm	PL_in_my,8,8
	.type	PL_in_my_stash,@object  # @PL_in_my_stash
	.comm	PL_in_my_stash,8,8
	.type	PL_hints,@object        # @PL_hints
	.comm	PL_hints,8,8
	.type	PL_debug,@object        # @PL_debug
	.comm	PL_debug,8,8
	.type	PL_utf8_alnum,@object   # @PL_utf8_alnum
	.comm	PL_utf8_alnum,8,8
	.type	PL_utf8_alnumc,@object  # @PL_utf8_alnumc
	.comm	PL_utf8_alnumc,8,8
	.type	PL_utf8_ascii,@object   # @PL_utf8_ascii
	.comm	PL_utf8_ascii,8,8
	.type	PL_utf8_alpha,@object   # @PL_utf8_alpha
	.comm	PL_utf8_alpha,8,8
	.type	PL_utf8_space,@object   # @PL_utf8_space
	.comm	PL_utf8_space,8,8
	.type	PL_utf8_cntrl,@object   # @PL_utf8_cntrl
	.comm	PL_utf8_cntrl,8,8
	.type	PL_utf8_graph,@object   # @PL_utf8_graph
	.comm	PL_utf8_graph,8,8
	.type	PL_utf8_digit,@object   # @PL_utf8_digit
	.comm	PL_utf8_digit,8,8
	.type	PL_utf8_upper,@object   # @PL_utf8_upper
	.comm	PL_utf8_upper,8,8
	.type	PL_utf8_lower,@object   # @PL_utf8_lower
	.comm	PL_utf8_lower,8,8
	.type	PL_utf8_print,@object   # @PL_utf8_print
	.comm	PL_utf8_print,8,8
	.type	PL_utf8_punct,@object   # @PL_utf8_punct
	.comm	PL_utf8_punct,8,8
	.type	PL_utf8_xdigit,@object  # @PL_utf8_xdigit
	.comm	PL_utf8_xdigit,8,8
	.type	PL_utf8_mark,@object    # @PL_utf8_mark
	.comm	PL_utf8_mark,8,8
	.type	PL_utf8_toupper,@object # @PL_utf8_toupper
	.comm	PL_utf8_toupper,8,8
	.type	PL_utf8_totitle,@object # @PL_utf8_totitle
	.comm	PL_utf8_totitle,8,8
	.type	PL_utf8_tolower,@object # @PL_utf8_tolower
	.comm	PL_utf8_tolower,8,8
	.type	PL_utf8_tofold,@object  # @PL_utf8_tofold
	.comm	PL_utf8_tofold,8,8
	.type	PL_last_swash_hv,@object # @PL_last_swash_hv
	.comm	PL_last_swash_hv,8,8
	.type	PL_last_swash_klen,@object # @PL_last_swash_klen
	.comm	PL_last_swash_klen,8,8
	.type	PL_last_swash_key,@object # @PL_last_swash_key
	.comm	PL_last_swash_key,10,1
	.type	PL_last_swash_tmps,@object # @PL_last_swash_tmps
	.comm	PL_last_swash_tmps,8,8
	.type	PL_last_swash_slen,@object # @PL_last_swash_slen
	.comm	PL_last_swash_slen,8,8
	.type	PL_yydebug,@object      # @PL_yydebug
	.comm	PL_yydebug,4,4
	.type	PL_yynerrs,@object      # @PL_yynerrs
	.comm	PL_yynerrs,4,4
	.type	PL_yyerrflag,@object    # @PL_yyerrflag
	.comm	PL_yyerrflag,4,4
	.type	PL_yychar,@object       # @PL_yychar
	.comm	PL_yychar,4,4
	.type	PL_yyval,@object        # @PL_yyval
	.comm	PL_yyval,8,8
	.type	PL_yylval,@object       # @PL_yylval
	.comm	PL_yylval,8,8
	.type	PL_srand_called,@object # @PL_srand_called
	.comm	PL_srand_called,1,1
	.type	PL_uudmap,@object       # @PL_uudmap
	.comm	PL_uudmap,256,16
	.type	PL_bitcount,@object     # @PL_bitcount
	.comm	PL_bitcount,8,8
	.type	PL_psig_ptr,@object     # @PL_psig_ptr
	.comm	PL_psig_ptr,8,8
	.type	PL_psig_name,@object    # @PL_psig_name
	.comm	PL_psig_name,8,8
	.type	PL_nullstash,@object    # @PL_nullstash
	.comm	PL_nullstash,8,8
	.type	PL_xnv_arenaroot,@object # @PL_xnv_arenaroot
	.comm	PL_xnv_arenaroot,8,8
	.type	PL_xrv_arenaroot,@object # @PL_xrv_arenaroot
	.comm	PL_xrv_arenaroot,8,8
	.type	PL_xpv_arenaroot,@object # @PL_xpv_arenaroot
	.comm	PL_xpv_arenaroot,8,8
	.type	PL_xpviv_arenaroot,@object # @PL_xpviv_arenaroot
	.comm	PL_xpviv_arenaroot,8,8
	.type	PL_xpvnv_arenaroot,@object # @PL_xpvnv_arenaroot
	.comm	PL_xpvnv_arenaroot,8,8
	.type	PL_xpvcv_arenaroot,@object # @PL_xpvcv_arenaroot
	.comm	PL_xpvcv_arenaroot,8,8
	.type	PL_xpvav_arenaroot,@object # @PL_xpvav_arenaroot
	.comm	PL_xpvav_arenaroot,8,8
	.type	PL_xpvhv_arenaroot,@object # @PL_xpvhv_arenaroot
	.comm	PL_xpvhv_arenaroot,8,8
	.type	PL_xpvmg_arenaroot,@object # @PL_xpvmg_arenaroot
	.comm	PL_xpvmg_arenaroot,8,8
	.type	PL_xpvlv_arenaroot,@object # @PL_xpvlv_arenaroot
	.comm	PL_xpvlv_arenaroot,8,8
	.type	PL_xpvbm_arenaroot,@object # @PL_xpvbm_arenaroot
	.comm	PL_xpvbm_arenaroot,8,8
	.type	PL_he_arenaroot,@object # @PL_he_arenaroot
	.comm	PL_he_arenaroot,8,8
	.type	PL_psig_pend,@object    # @PL_psig_pend
	.comm	PL_psig_pend,8,8
	.type	PL_custom_op_names,@object # @PL_custom_op_names
	.comm	PL_custom_op_names,8,8
	.type	PL_custom_op_descs,@object # @PL_custom_op_descs
	.comm	PL_custom_op_descs,8,8
	.type	PL_debug_pad,@object    # @PL_debug_pad
	.comm	PL_debug_pad,72,8
	.type	PL_taint_warn,@object   # @PL_taint_warn
	.comm	PL_taint_warn,1,1
	.type	PL_utf8locale,@object   # @PL_utf8locale
	.comm	PL_utf8locale,1,1
	.type	PL_utf8_idstart,@object # @PL_utf8_idstart
	.comm	PL_utf8_idstart,8,8
	.type	PL_utf8_idcont,@object  # @PL_utf8_idcont
	.comm	PL_utf8_idcont,8,8
	.type	PL_sort_RealCmp,@object # @PL_sort_RealCmp
	.comm	PL_sort_RealCmp,8,8
	.type	PL_unicode,@object      # @PL_unicode
	.comm	PL_unicode,8,8
	.type	PL_signals,@object      # @PL_signals
	.comm	PL_signals,8,8
	.type	PL_stashcache,@object   # @PL_stashcache
	.comm	PL_stashcache,8,8
	.type	PL_reentrant_retint,@object # @PL_reentrant_retint
	.comm	PL_reentrant_retint,4,4
	.type	PL_stack_sp,@object     # @PL_stack_sp
	.comm	PL_stack_sp,8,8
	.type	PL_op,@object           # @PL_op
	.comm	PL_op,8,8
	.type	PL_curpad,@object       # @PL_curpad
	.comm	PL_curpad,8,8
	.type	PL_stack_base,@object   # @PL_stack_base
	.comm	PL_stack_base,8,8
	.type	PL_stack_max,@object    # @PL_stack_max
	.comm	PL_stack_max,8,8
	.type	PL_scopestack,@object   # @PL_scopestack
	.comm	PL_scopestack,8,8
	.type	PL_scopestack_ix,@object # @PL_scopestack_ix
	.comm	PL_scopestack_ix,8,8
	.type	PL_scopestack_max,@object # @PL_scopestack_max
	.comm	PL_scopestack_max,8,8
	.type	PL_savestack,@object    # @PL_savestack
	.comm	PL_savestack,8,8
	.type	PL_savestack_ix,@object # @PL_savestack_ix
	.comm	PL_savestack_ix,8,8
	.type	PL_savestack_max,@object # @PL_savestack_max
	.comm	PL_savestack_max,8,8
	.type	PL_tmps_stack,@object   # @PL_tmps_stack
	.comm	PL_tmps_stack,8,8
	.type	PL_tmps_max,@object     # @PL_tmps_max
	.comm	PL_tmps_max,8,8
	.type	PL_markstack,@object    # @PL_markstack
	.comm	PL_markstack,8,8
	.type	PL_markstack_ptr,@object # @PL_markstack_ptr
	.comm	PL_markstack_ptr,8,8
	.type	PL_markstack_max,@object # @PL_markstack_max
	.comm	PL_markstack_max,8,8
	.type	PL_retstack,@object     # @PL_retstack
	.comm	PL_retstack,8,8
	.type	PL_retstack_ix,@object  # @PL_retstack_ix
	.comm	PL_retstack_ix,8,8
	.type	PL_retstack_max,@object # @PL_retstack_max
	.comm	PL_retstack_max,8,8
	.type	PL_Sv,@object           # @PL_Sv
	.comm	PL_Sv,8,8
	.type	PL_Xpv,@object          # @PL_Xpv
	.comm	PL_Xpv,8,8
	.type	PL_na,@object           # @PL_na
	.comm	PL_na,8,8
	.type	PL_statbuf,@object      # @PL_statbuf
	.comm	PL_statbuf,144,8
	.type	PL_statcache,@object    # @PL_statcache
	.comm	PL_statcache,144,8
	.type	PL_statgv,@object       # @PL_statgv
	.comm	PL_statgv,8,8
	.type	PL_tainted,@object      # @PL_tainted
	.comm	PL_tainted,1,1
	.type	PL_curpm,@object        # @PL_curpm
	.comm	PL_curpm,8,8
	.type	PL_nrs,@object          # @PL_nrs
	.comm	PL_nrs,8,8
	.type	PL_rs,@object           # @PL_rs
	.comm	PL_rs,8,8
	.type	PL_last_in_gv,@object   # @PL_last_in_gv
	.comm	PL_last_in_gv,8,8
	.type	PL_ofs_sv,@object       # @PL_ofs_sv
	.comm	PL_ofs_sv,8,8
	.type	PL_defoutgv,@object     # @PL_defoutgv
	.comm	PL_defoutgv,8,8
	.type	PL_formtarget,@object   # @PL_formtarget
	.comm	PL_formtarget,8,8
	.type	PL_bodytarget,@object   # @PL_bodytarget
	.comm	PL_bodytarget,8,8
	.type	PL_toptarget,@object    # @PL_toptarget
	.comm	PL_toptarget,8,8
	.type	PL_defstash,@object     # @PL_defstash
	.comm	PL_defstash,8,8
	.type	PL_curstash,@object     # @PL_curstash
	.comm	PL_curstash,8,8
	.type	PL_restartop,@object    # @PL_restartop
	.comm	PL_restartop,8,8
	.type	PL_in_eval,@object      # @PL_in_eval
	.comm	PL_in_eval,4,4
	.type	PL_delaymagic,@object   # @PL_delaymagic
	.comm	PL_delaymagic,4,4
	.type	PL_localizing,@object   # @PL_localizing
	.comm	PL_localizing,4,4
	.type	PL_curstack,@object     # @PL_curstack
	.comm	PL_curstack,8,8
	.type	PL_curstackinfo,@object # @PL_curstackinfo
	.comm	PL_curstackinfo,8,8
	.type	PL_mainstack,@object    # @PL_mainstack
	.comm	PL_mainstack,8,8
	.type	PL_top_env,@object      # @PL_top_env
	.comm	PL_top_env,8,8
	.type	PL_start_env,@object    # @PL_start_env
	.comm	PL_start_env,216,8
	.type	PL_av_fetch_sv,@object  # @PL_av_fetch_sv
	.comm	PL_av_fetch_sv,8,8
	.type	PL_hv_fetch_sv,@object  # @PL_hv_fetch_sv
	.comm	PL_hv_fetch_sv,8,8
	.type	PL_hv_fetch_ent_mh,@object # @PL_hv_fetch_ent_mh
	.comm	PL_hv_fetch_ent_mh,8,8
	.type	PL_modcount,@object     # @PL_modcount
	.comm	PL_modcount,8,8
	.type	PL_lastgotoprobe,@object # @PL_lastgotoprobe
	.comm	PL_lastgotoprobe,8,8
	.type	PL_sortcop,@object      # @PL_sortcop
	.comm	PL_sortcop,8,8
	.type	PL_sortstash,@object    # @PL_sortstash
	.comm	PL_sortstash,8,8
	.type	PL_firstgv,@object      # @PL_firstgv
	.comm	PL_firstgv,8,8
	.type	PL_secondgv,@object     # @PL_secondgv
	.comm	PL_secondgv,8,8
	.type	PL_sortcxix,@object     # @PL_sortcxix
	.comm	PL_sortcxix,8,8
	.type	PL_efloatbuf,@object    # @PL_efloatbuf
	.comm	PL_efloatbuf,8,8
	.type	PL_efloatsize,@object   # @PL_efloatsize
	.comm	PL_efloatsize,8,8
	.type	PL_screamfirst,@object  # @PL_screamfirst
	.comm	PL_screamfirst,8,8
	.type	PL_screamnext,@object   # @PL_screamnext
	.comm	PL_screamnext,8,8
	.type	PL_lastscream,@object   # @PL_lastscream
	.comm	PL_lastscream,8,8
	.type	PL_regdummy,@object     # @PL_regdummy
	.comm	PL_regdummy,4,2
	.type	PL_regcomp_parse,@object # @PL_regcomp_parse
	.comm	PL_regcomp_parse,8,8
	.type	PL_regxend,@object      # @PL_regxend
	.comm	PL_regxend,8,8
	.type	PL_regcode,@object      # @PL_regcode
	.comm	PL_regcode,8,8
	.type	PL_regnaughty,@object   # @PL_regnaughty
	.comm	PL_regnaughty,8,8
	.type	PL_regsawback,@object   # @PL_regsawback
	.comm	PL_regsawback,8,8
	.type	PL_regprecomp,@object   # @PL_regprecomp
	.comm	PL_regprecomp,8,8
	.type	PL_regnpar,@object      # @PL_regnpar
	.comm	PL_regnpar,8,8
	.type	PL_regsize,@object      # @PL_regsize
	.comm	PL_regsize,8,8
	.type	PL_regflags,@object     # @PL_regflags
	.comm	PL_regflags,8,8
	.type	PL_regseen,@object      # @PL_regseen
	.comm	PL_regseen,8,8
	.type	PL_seen_zerolen,@object # @PL_seen_zerolen
	.comm	PL_seen_zerolen,8,8
	.type	PL_seen_evals,@object   # @PL_seen_evals
	.comm	PL_seen_evals,8,8
	.type	PL_regcomp_rx,@object   # @PL_regcomp_rx
	.comm	PL_regcomp_rx,8,8
	.type	PL_extralen,@object     # @PL_extralen
	.comm	PL_extralen,8,8
	.type	PL_colorset,@object     # @PL_colorset
	.comm	PL_colorset,4,4
	.type	PL_colors,@object       # @PL_colors
	.comm	PL_colors,48,16
	.type	PL_reg_whilem_seen,@object # @PL_reg_whilem_seen
	.comm	PL_reg_whilem_seen,8,8
	.type	PL_reginput,@object     # @PL_reginput
	.comm	PL_reginput,8,8
	.type	PL_regbol,@object       # @PL_regbol
	.comm	PL_regbol,8,8
	.type	PL_regeol,@object       # @PL_regeol
	.comm	PL_regeol,8,8
	.type	PL_regstartp,@object    # @PL_regstartp
	.comm	PL_regstartp,8,8
	.type	PL_regendp,@object      # @PL_regendp
	.comm	PL_regendp,8,8
	.type	PL_reglastparen,@object # @PL_reglastparen
	.comm	PL_reglastparen,8,8
	.type	PL_reglastcloseparen,@object # @PL_reglastcloseparen
	.comm	PL_reglastcloseparen,8,8
	.type	PL_regtill,@object      # @PL_regtill
	.comm	PL_regtill,8,8
	.type	PL_regcompat1,@object   # @PL_regcompat1
	.comm	PL_regcompat1,1,1
	.type	PL_reg_start_tmp,@object # @PL_reg_start_tmp
	.comm	PL_reg_start_tmp,8,8
	.type	PL_reg_start_tmpl,@object # @PL_reg_start_tmpl
	.comm	PL_reg_start_tmpl,8,8
	.type	PL_regdata,@object      # @PL_regdata
	.comm	PL_regdata,8,8
	.type	PL_bostr,@object        # @PL_bostr
	.comm	PL_bostr,8,8
	.type	PL_reg_flags,@object    # @PL_reg_flags
	.comm	PL_reg_flags,8,8
	.type	PL_reg_eval_set,@object # @PL_reg_eval_set
	.comm	PL_reg_eval_set,8,8
	.type	PL_regnarrate,@object   # @PL_regnarrate
	.comm	PL_regnarrate,8,8
	.type	PL_regprogram,@object   # @PL_regprogram
	.comm	PL_regprogram,8,8
	.type	PL_regcc,@object        # @PL_regcc
	.comm	PL_regcc,8,8
	.type	PL_reg_call_cc,@object  # @PL_reg_call_cc
	.comm	PL_reg_call_cc,8,8
	.type	PL_reg_re,@object       # @PL_reg_re
	.comm	PL_reg_re,8,8
	.type	PL_reg_ganch,@object    # @PL_reg_ganch
	.comm	PL_reg_ganch,8,8
	.type	PL_reg_sv,@object       # @PL_reg_sv
	.comm	PL_reg_sv,8,8
	.type	PL_reg_magic,@object    # @PL_reg_magic
	.comm	PL_reg_magic,8,8
	.type	PL_reg_oldpos,@object   # @PL_reg_oldpos
	.comm	PL_reg_oldpos,8,8
	.type	PL_reg_oldsaved,@object # @PL_reg_oldsaved
	.comm	PL_reg_oldsaved,8,8
	.type	PL_reg_oldsavedlen,@object # @PL_reg_oldsavedlen
	.comm	PL_reg_oldsavedlen,8,8
	.type	PL_reg_maxiter,@object  # @PL_reg_maxiter
	.comm	PL_reg_maxiter,8,8
	.type	PL_reg_leftiter,@object # @PL_reg_leftiter
	.comm	PL_reg_leftiter,8,8
	.type	PL_reg_poscache_size,@object # @PL_reg_poscache_size
	.comm	PL_reg_poscache_size,8,8
	.type	PL_watchok,@object      # @PL_watchok
	.comm	PL_watchok,8,8
	.type	PL_reg_match_utf8,@object # @PL_reg_match_utf8
	.comm	PL_reg_match_utf8,1,1
	.type	PL_comppad,@object      # @PL_comppad
	.comm	PL_comppad,8,8
	.type	PL_curinterp,@object    # @PL_curinterp
	.comm	PL_curinterp,8,8
	.type	PL_sigfpe_saved,@object # @PL_sigfpe_saved
	.comm	PL_sigfpe_saved,8,8
	.type	PL_sv_placeholder,@object # @PL_sv_placeholder
	.comm	PL_sv_placeholder,24,8

	.ident	"clang version 3.5 (trunk)"
	.section	".note.GNU-stack","",@progbits
