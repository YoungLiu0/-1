  .text
  .globl main
main:
  addi sp, sp, -32
  sw ra, 28(sp)
  sw fp, 24(sp)
  addi fp, sp, 0
main..L0:
  li t0, 1
  sw t0, -4(fp)
  li t1, 2
  sw t1, -8(fp)
  li t2, 3
  sw t2, -12(fp)
  li t3, 4
  sw t3, -16(fp)
  li t4, 5
  sw t4, -4(fp)
  li t5, 6
  sw t5, -8(fp)
  li t6, 7
  sw t6, -12(fp)
  li a0, 42
  lw ra, 28(sp)
  lw fp, 24(sp)
  addi sp, sp, 32
  ret
