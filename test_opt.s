  .text
  .globl fact
fact:
  addi sp, sp, -16
  sw ra, 12(sp)
  sw fp, 8(sp)
  addi fp, sp, 0
  sw a0, -4(fp)
fact..L0:
  beqz t0, fact.if_end_2
fact.then_0:
  li a0, 1
  lw ra, 12(sp)
  lw fp, 8(sp)
  addi sp, sp, 16
  ret
fact.if_end_2:
  lw t1, -4(fp)
  lw t2, -4(fp)
  li t3, 1
  sub t4, t2, t3
  mv a0, t4
  call fact
  mv t5, a0
  mul t6, t1, t5
  mv a0, t6
  lw ra, 12(sp)
  lw fp, 8(sp)
  addi sp, sp, 16
  ret

  .globl main
main:
  addi sp, sp, -16
  sw ra, 12(sp)
  sw fp, 8(sp)
  addi fp, sp, 0
main..L1:
  li t0, 7
  mv a0, t0
  call fact
  mv t1, a0
  mv a0, t1
  lw ra, 12(sp)
  lw fp, 8(sp)
  addi sp, sp, 16
  ret
