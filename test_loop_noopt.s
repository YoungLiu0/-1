  .text
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
  li s1, 0
  sw s1, -60(fp)
  li s1, 0
  sw s1, -64(fp)
main.while_start_0:
  lw s1, -64(fp)
  li s2, 1000000
  slt s3, s1, s2
  beqz s3, main.while_end_2
main.while_body_1:
  lw s1, -60(fp)
  lw s2, -64(fp)
  add s3, s1, s2
  sw s3, -60(fp)
  lw s1, -64(fp)
  li s2, 1
  add s3, s1, s2
  sw s3, -64(fp)
  j main.while_start_0
main.while_end_2:
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
