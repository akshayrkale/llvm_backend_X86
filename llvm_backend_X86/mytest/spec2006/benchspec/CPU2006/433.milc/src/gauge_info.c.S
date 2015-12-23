	.file	"gauge_info.c.bc"
	.text
	.globl	write_appl_gauge_info
	.align	16, 0x90
	.type	write_appl_gauge_info,@function
write_appl_gauge_info:                  # @write_appl_gauge_info
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
	movq	%rdi, %rbx
	movabsq	$.L.str, %rsi
	movabsq	$.L.str1, %r14
	movabsq	$.L.str2, %rcx
	xorq	%r8, %r8
	xorq	%r9, %r9
                                        # kill: RDI<def> RBX<kill>
	movq	%r14, %rdx
	callq	write_gauge_info_item
	movabsq	$.L.str3, %rsi
	movabsq	$gauge_action_description, %rcx
	xorq	%r8, %r8
	xorq	%r9, %r9
	movq	%rbx, %rdi
	movq	%r14, %rdx
	callq	write_gauge_info_item
	movabsq	$.L.str4, %rsi
	movabsq	$.L.str5, %r15
	movabsq	$gauge_action_nloops, %rcx
	xorq	%r8, %r8
	xorq	%r9, %r9
	movq	%rbx, %rdi
	movq	%r15, %rdx
	callq	write_gauge_info_item
	movabsq	$.L.str6, %rsi
	movabsq	$gauge_action_nreps, %rcx
	xorq	%r8, %r8
	xorq	%r9, %r9
	movq	%rbx, %rdi
	movq	%r15, %rdx
	callq	write_gauge_info_item
	movabsq	$.L.str7, %rsi
	movabsq	$.L.str8, %r12
	movabsq	$beta, %rcx
	xorq	%r8, %r8
	xorq	%r9, %r9
	movq	%rbx, %rdi
	movq	%r12, %rdx
	callq	write_gauge_info_item
	movabsq	$.L.str9, %rsi
	movabsq	$u0, %rcx
	xorq	%r8, %r8
	xorq	%r9, %r9
	movq	%rbx, %rdi
	movq	%r12, %rdx
	callq	write_gauge_info_item
	movabsq	$.L.str10, %rsi
	movabsq	$quark_action_description, %rcx
	xorq	%r8, %r8
	xorq	%r9, %r9
	movq	%rbx, %rdi
	movq	%r14, %rdx
	callq	write_gauge_info_item
	movabsq	$.L.str11, %rsi
	movabsq	$nflavors, %rcx
	xorq	%r8, %r8
	xorq	%r9, %r9
	movq	%rbx, %rdi
	movq	%r15, %rdx
	callq	write_gauge_info_item
	movabsq	$.L.str12, %rsi
	movabsq	$mass, %rcx
	xorq	%r8, %r8
	xorq	%r9, %r9
	movq	%rbx, %rdi
	movq	%r12, %rdx
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	jmp	write_gauge_info_item  # TAILCALL
.Ltmp14:
	.size	write_appl_gauge_info, .Ltmp14-write_appl_gauge_info
	.cfi_endproc

	.type	.L.str,@object          # @.str
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.asciz	"action.description"
	.size	.L.str, 19

	.type	.L.str1,@object         # @.str1
.L.str1:
	.asciz	"%s"
	.size	.L.str1, 3

	.type	.L.str2,@object         # @.str2
.L.str2:
	.asciz	"\"Gauge plus fermion (improved)\""
	.size	.L.str2, 32

	.type	.L.str3,@object         # @.str3
.L.str3:
	.asciz	"gauge.description"
	.size	.L.str3, 18

	.type	.L.str4,@object         # @.str4
.L.str4:
	.asciz	"gauge.nloops"
	.size	.L.str4, 13

	.type	.L.str5,@object         # @.str5
.L.str5:
	.asciz	"%d"
	.size	.L.str5, 3

	.type	.L.str6,@object         # @.str6
.L.str6:
	.asciz	"gauge.nreps"
	.size	.L.str6, 12

	.type	.L.str7,@object         # @.str7
.L.str7:
	.asciz	"gauge.beta11"
	.size	.L.str7, 13

	.type	.L.str8,@object         # @.str8
.L.str8:
	.asciz	"%f"
	.size	.L.str8, 3

	.type	.L.str9,@object         # @.str9
.L.str9:
	.asciz	"gauge.tadpole.u0"
	.size	.L.str9, 17

	.type	.L.str10,@object        # @.str10
.L.str10:
	.asciz	"quark.description"
	.size	.L.str10, 18

	.type	quark_action_description,@object # @quark_action_description
	.data
	.align	16
quark_action_description:
	.asciz	"\"O(a^2): couplings(pi)=0, Naik term, No O(a^2) errors, tadpole weights\""
	.size	quark_action_description, 72

	.type	.L.str11,@object        # @.str11
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str11:
	.asciz	"quark.flavors"
	.size	.L.str11, 14

	.type	.L.str12,@object        # @.str12
.L.str12:
	.asciz	"quark.mass"
	.size	.L.str12, 11


	.ident	"clang version 3.5 (trunk)"
	.section	".note.GNU-stack","",@progbits
