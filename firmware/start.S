// This is free and unencumbered software released into the public domain.
//
// Anyone is free to copy, modify, publish, use, compile, sell, or
// distribute this software, either in source code form or as a compiled
// binary, for any purpose, commercial or non-commercial, and by any
// means.

#define ENABLE_RVTST
#define ENABLE_SIEVE
#define ENABLE_MULTST
#define ENABLE_STATS

#ifndef XLEN
#define XLEN 64
#endif

#include "custom_ops.S"

	.section .text
	.global sieve
	.global multest
	.global hard_mul
	.global hard_mulh
	.global hard_mulhsu
	.global hard_mulhu
	.global stats

reset_vec:

/* Main program
 **********************************/

start:
	/* zero-initialize all registers */

	addi x1, zero, 0
	addi x2, zero, 0
	addi x3, zero, 0
	addi x4, zero, 0
	addi x5, zero, 0
	addi x6, zero, 0
	addi x7, zero, 0
	addi x8, zero, 0
	addi x9, zero, 0
	addi x10, zero, 0
	addi x11, zero, 0
	addi x12, zero, 0
	addi x13, zero, 0
	addi x14, zero, 0
	addi x15, zero, 0
	addi x16, zero, 0
	addi x17, zero, 0
	addi x18, zero, 0
	addi x19, zero, 0
	addi x20, zero, 0
	addi x21, zero, 0
	addi x22, zero, 0
	addi x23, zero, 0
	addi x24, zero, 0
	addi x25, zero, 0
	addi x26, zero, 0
	addi x27, zero, 0
	addi x28, zero, 0
	addi x29, zero, 0
	addi x30, zero, 0
	addi x31, zero, 0

	/* running tests from riscv-tests */

#ifdef ENABLE_RVTST
#  define TEST(n) \
	.global n; \
	addi x1, zero, 1000; \
	jal zero,n; \
	.global n ## _ret; \
	n ## _ret:
#else
#  define TEST(n) \
	.global n ## _ret; \
	n ## _ret:
#endif

	TEST(lui)
	TEST(auipc)
	TEST(jal)
	TEST(jalr)

	TEST(beq)
	TEST(bne)
	TEST(blt)
	TEST(bge)
	TEST(bltu)
	TEST(bgeu)

	TEST(addi)
	TEST(slti)
	TEST(sltiu)
	TEST(xori)
	TEST(ori)
	TEST(andi)
	TEST(slli)
	TEST(srli)
	TEST(srai)
#if defined XLEN && XLEN == 64
	TEST(addiw)
  TEST(slliw)
  TEST(srliw)
  TEST(sraiw)
#endif

	TEST(add)
	TEST(sub)
	TEST(sll)
	TEST(slt)
	TEST(sltu)
	TEST(xor)
	TEST(srl)
	TEST(sra)
	TEST(or)
	TEST(and)
#if defined XLEN && XLEN == 64
	TEST(addw)
	TEST(subw)
  TEST(sllw)
  TEST(srlw)
  TEST(sraw)
#endif

	TEST(lb)
	TEST(lh)
	TEST(lw)
#if defined XLEN && XLEN == 64
	TEST(ld)
#endif
	TEST(lbu)
	TEST(lhu)
#if defined XLEN && XLEN == 64
	TEST(lwu)
#endif

	TEST(sb)
	TEST(sh)
	TEST(sw)
#if defined XLEN && XLEN == 64
	TEST(sd)
#endif

	TEST(mul)
	TEST(mulhu)
	TEST(mulh)
	TEST(mulhsu)
#if defined XLEN && XLEN == 64
	TEST(mulw)
#endif
 
	
  TEST(div)
  TEST(divu)
  TEST(rem)
  TEST(remu)
#if defined XLEN && XLEN == 64
	TEST(divw)
  TEST(divuw)
  TEST(remw)
  TEST(remuw)
#endif

	TEST(simple)

	/* set stack pointer */
	lui sp,(16*1024)>>12

	/* set gp and tp */
	lui gp, %hi(0xdeadbeef)
	addi gp, gp, %lo(0xdeadbeef)
	addi tp, gp, 0
	
	/* Call aditional C functions here!*/
#ifdef ENABLE_SIEVE
	/* call sieve C code */
	jal ra,sieve
#endif

#if defined ENABLE_MULTST && defined XLEN && XLEN == 32
	/* call multest C code */
	/* Note: This only works in 32-bit mode*/
	jal ra,multest
#endif

#ifdef ENABLE_STATS
	/* call stats C code */
	jal ra,stats
#endif

	/* print "DONE\n" */
	lui a0,0x10000000>>12
	addi a1,zero,'D'
	addi a2,zero,'O'
	addi a3,zero,'N'
	addi a4,zero,'E'
	addi a5,zero,'\n'
	sw a1,0(a0)
	sw a2,0(a0)
	sw a3,0(a0)
	sw a4,0(a0)
	sw a5,0(a0)
	
	/*Tell simulator that everything is finished*/
	li a0, 0x20000000
	li a1, 123456789
#if defined XLEN && XLEN == 64
	sd a1,0(a0)
#else
  sw a1,0(a0)
#endif
	/* break */
	sbreak

/* Hard mul functions for multest.c
 **********************************/

hard_mul:
	mul a0, a0, a1
	ret

hard_mulh:
	mulh a0, a0, a1
	ret

hard_mulhsu:
	mulhsu a0, a0, a1
	ret

hard_mulhu:
	mulhu a0, a0, a1
	ret
