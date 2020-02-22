# CalcSimulator
A special calculator written in assembly with special functions

The following operations are:  
'q’ – quit  
‘+’ – unsigned addition  
pop two operands from operand stack, and push one result, their sum  
‘p’ – pop-and-print  
pop one operand from the operand stack, and print its value to stdout  
‘d’ – duplicate  
push a copy of the top of the operand stack onto the top of the operand stack  
‘^’ - X*2^Y, with X being the top of operand stack and Y the element next to x in the operand stack. If Y>200 this is considered an error, in which case you should print out an error message and leave the operand stack unaffected.
pop two operands from the operand stack, and push one result  
‘v’ – X*2^(-Y), with X and Y as above. This number may be not an integer. You are required to truncate the fraction part of it and keep only the integer part.  
pop two operands from the operand stack, and push one result  
‘n’ – number of '1' bits  
pop one operand from the operand stack, and push one result  
‘sr’ – square root (bonus item*)  
pop one operand from the operand stack, and push one result (only the integer part)  
