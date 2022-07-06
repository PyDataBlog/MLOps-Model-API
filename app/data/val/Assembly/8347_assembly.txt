start:add $zero, $zero, $zero	
addi $s1, $s1, 1
add $s2, $s1, $s1
addi $s3, $s2, -1
sub $s4, $s3, $s1
ori $t1, $zero, 15 
andi $t2, $t1, 10
or $t3, $t2, $s1
and $t4, $t3, $s2
beq $s4, $zero, Lab1
add $zero, $zero, $zero 
Lab1:bne $s1, $s2, Lab2
add $zero, $zero, $zero
Lab2:lui $t4, 1
sw $t4, 1($s1)
lw $t5, 0($s2)
addi $t5, $t5, 0	
xor $t6, $s1, $t1 
xori $t7, $t6, 1
nor $t4, $t7, $t6
slt $s6, $t7, $t6 
slti $s6, $t7, 20 
j Lab3
add $zero, $zero, $zero 
add $zero, $zero, $zero 
Lab3:jal Lab4
j start
Lab4:jr $ra
end
