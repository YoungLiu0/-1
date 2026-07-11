  .text
  .globl main
main:
  addi sp, sp, -16
  sw ra, 12(sp)
  sw fp, 8(sp)
  addi fp, sp, 0
  li t0, 1
  sw t0, -4(fp)
  li t1, 2
  sw t1, -8(fp)
  lw t2, -8(fp)
  mv a0, t2
  lw ra, 12(sp)
  lw fp, 8(sp)
  addi sp, sp, 16
  ret
