  .data
  .globl g
g:
  .word 100

  .text
  .globl foo
foo:
  addi sp, sp, -64
  sw ra, 60(sp)
  sw fp, 56(sp)
  addi fp, sp, 64
  sw s0, -12(fp)
  sw s1, -16(fp)
  sw s2, -20(fp)
  sw s3, -24(fp)
  sw s4, -28(fp)
  sw s5, -32(fp)
  sw s6, -36(fp)
  sw s7, -40(fp)
  sw s8, -44(fp)
  sw s9, -48(fp)
  sw s10, -52(fp)
  sw s11, -56(fp)
  li s1, 200
  sw s1, -60(fp)
  lw s1, -60(fp)
  mv a0, s1
  lw s0, -12(fp)
  lw s1, -16(fp)
  lw s2, -20(fp)
  lw s3, -24(fp)
  lw s4, -28(fp)
  lw s5, -32(fp)
  lw s6, -36(fp)
  lw s7, -40(fp)
  lw s8, -44(fp)
  lw s9, -48(fp)
  lw s10, -52(fp)
  lw s11, -56(fp)
  lw ra, 60(sp)
  lw fp, 56(sp)
  addi sp, sp, 64
  ret

  .globl main
main:
  addi sp, sp, -64
  sw ra, 60(sp)
  sw fp, 56(sp)
  addi fp, sp, 64
  sw s0, -12(fp)
  sw s1, -16(fp)
  sw s2, -20(fp)
  sw s3, -24(fp)
  sw s4, -28(fp)
  sw s5, -32(fp)
  sw s6, -36(fp)
  sw s7, -40(fp)
  sw s8, -44(fp)
  sw s9, -48(fp)
  sw s10, -52(fp)
  sw s11, -56(fp)
  call foo
  mv s1, a0
  mv a0, s1
  lw s0, -12(fp)
  lw s1, -16(fp)
  lw s2, -20(fp)
  lw s3, -24(fp)
  lw s4, -28(fp)
  lw s5, -32(fp)
  lw s6, -36(fp)
  lw s7, -40(fp)
  lw s8, -44(fp)
  lw s9, -48(fp)
  lw s10, -52(fp)
  lw s11, -56(fp)
  lw ra, 60(sp)
  lw fp, 56(sp)
  addi sp, sp, 64
  ret
