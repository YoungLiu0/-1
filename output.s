  .text
  .globl main
main:
  addi sp, sp, -16
  sw ra, 12(sp)
  sw fp, 8(sp)
  addi fp, sp, 0
  li a0, 42
  lw ra, 12(sp)
  lw fp, 8(sp)
  addi sp, sp, 16
  ret
