  .text
  .globl main
main:
  addi sp, sp, -16
  sw ra, 12(sp)
  sw fp, 8(sp)
  addi fp, sp, 0
  li t0, 0
  sw t0, -4(fp)
while_start_0:
  lw t1, -4(fp)
  li t2, 5
  slt t3, t1, t2
  beqz t3, while_end_2
while_body_1:
  lw t4, -4(fp)
  li t5, 1
  add t6, t4, t5
  sw t6, -4(fp)
  j while_start_0
while_end_2:
  lw s1, -4(fp)
  mv a0, s1
  lw ra, 12(sp)
  lw fp, 8(sp)
  addi sp, sp, 16
  ret
