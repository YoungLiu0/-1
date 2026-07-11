  .text
  .globl main
main:
  addi sp, sp, -32
  sw ra, 28(sp)
  sw fp, 24(sp)
  addi fp, sp, 0
main..L0:
  li t0, 10
  sw t0, -4(fp)
  li t1, 20
  sw t1, -8(fp)
  li t2, 30
  sw t2, -12(fp)
  lw t3, -8(fp)
  mv a0, t3
  lw ra, 28(sp)
  lw fp, 24(sp)
  addi sp, sp, 32
  ret
