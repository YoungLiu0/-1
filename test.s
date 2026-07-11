  .text
  .globl sum
sum:
  addi sp, sp, -48
  sw ra, 44(sp)
  sw fp, 40(sp)
  addi fp, sp, 0
  sw a0, -4(fp)
  sw a1, -8(fp)
  sw a2, -12(fp)
  sw a3, -16(fp)
  sw a4, -20(fp)
  sw a5, -24(fp)
  sw a6, -28(fp)
  sw a7, -32(fp)
  lw t0, 48(fp)
  sw t0, -36(fp)
  lw t1, 52(fp)
  sw t1, -40(fp)
  lw t2, -4(fp)
  lw t3, -8(fp)
  add t4, t2, t3
  lw t5, -12(fp)
  add t6, t4, t5
  lw s1, -16(fp)
  add s2, t6, s1
  lw s3, -20(fp)
  add s4, s2, s3
  lw s5, -24(fp)
  add s6, s4, s5
  lw s7, -28(fp)
  add s8, s6, s7
  lw s9, -32(fp)
  add s10, s8, s9
  lw s11, -36(fp)
  add s10, s10, s11
  lw s11, -40(fp)
  add t0, s10, s11
  mv a0, t0
  lw ra, 44(sp)
  lw fp, 40(sp)
  addi sp, sp, 48
  ret

  .globl main
main:
  addi sp, sp, -16
  sw ra, 12(sp)
  sw fp, 8(sp)
  addi fp, sp, 0
  li t0, 1
  mv a0, t0
  li t1, 2
  mv a1, t1
  li t2, 3
  mv a2, t2
  li t3, 4
  mv a3, t3
  li t4, 5
  mv a4, t4
  li t5, 6
  mv a5, t5
  li t6, 7
  mv a6, t6
  li s1, 8
  mv a7, s1
  li s2, 9
  sw s2, 0(sp)
  li s3, 10
  sw s3, 4(sp)
  call sum
  mv s4, a0
  mv a0, s4
  lw ra, 12(sp)
  lw fp, 8(sp)
  addi sp, sp, 16
  ret
