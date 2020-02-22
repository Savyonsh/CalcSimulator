%macro write 3
section	.text
	mov eax, 4
	mov ebx, %3
	mov ecx, %1
	mov edx, %2
	int 0x80
%endmacro

%macro read 2
section .text
	mov eax, 3
	mov ebx, STDIN
	mov ecx, %1
	mov edx, %2
	int 0x80
%endmacro

%macro getNextByte 2
section .text
	cmp %1, 0
	je %%.PUSH_Z
	inc %1
	cmp dword [%1], 0
	je %%.PUSH_Z
	xor %2, %2
	mov %1, dword [%1]
	mov al, byte [%1]
	push %2
	jmp %%.FINISH

	%%.PUSH_Z:
	mov %1, 0
	push dword 0 
%%.FINISH:	
%endmacro

%macro menu 2
section .text
	cmp byte [ecx], %1
	jne %%.END
	call %2
	inc esi
	jmp %%.END

%%.END:
%endmacro

%macro print_debug 4
section .text
	cmp byte [%1], 1		
	jne %%.END
		
	mov edx, dword [%2]
	push dword [%3 +4*edx]
	call %4
	add esp, 4
%%.END:
%endmacro

STDOUT equ 1
STDIN equ 0
STDERR equ 2
BUF_SIZE equ 81					
LINK_SIZE equ 5
MAX_STACK_SIZE equ 5

section .rodata
	calc: db "calc: "
	calc_len: equ $-calc
	hex_format: db "%X" ,0
	hex_format_mid: db "%02X" , 0
	new_line: db 10, 0
	overflow: db "Error: Operand Stack Overflow", 10
	overflow_len: equ $ - overflow
	underflow: db "Error: Insufficient Number of Arguments on Stack", 10
	underflow_len: equ $ - underflow
    	ybigger: db "wrong Y value", 10
	ybigger_len: equ $ - ybigger

section .data	
	stackPointer: dd -1			; the stack pointer/ size inidicator
	stack: dd 0, 0, 0, 0, 0			; initialize all cells of stack to be zero
	debug: db 0				; debug option on
    	shift_size: db 0			; variable for '^' op
	concat: db 0				; variable for '^','v' op
	carry: db 0				; variable for '+' op
	
	
section .bss
	buf: resb BUF_SIZE			; input buffer

section .text
	align 16
	global main 
	extern fgets
	extern printf
	extern calloc 
	extern free
	extern stdin
	extern fgets

main: 
	push ebp
	mov ebp, esp

	; ------- CHECK ARGUMENTS --------- ; 
	mov eax, dword [ebp + 8]		; check argc
	cmp al, 2
	jb .CON
	mov eax, dword [ebp + 12]		; go to arguments array
	mov eax, dword [eax + 4]		; go to second string (after ./calc)
	inc eax
	cmp byte [eax], 'd'			; check if DEBUG option is on
	jne .CON
	inc byte [debug]			; turn on debug flag

	.CON:	
	call myCalc

	; ------ PRINT NUMBER OF OPS ---- ; 
	push eax				; push number of ops returned from my calc
	push hex_format				
	call printf				
	add esp, 8				; clean stack from params

	push new_line				; print \n 
	call printf
	add esp, 4

	; ------- FREE OPERAND STACK FROM MEMORY -------- ;
	clearStack:
		mov edx, dword [stackPointer]
		cmp edx, -1			; check if pointer got to all cells (including 0)
		je end_main
		mov eax, dword [stack + 4*edx]	; mov address of first link to eax
		cmp eax, 0			; check to see if the cell is not empty
		je con				; if it is continue loop
		push dword [stack + 4*edx]	; push address as param for freeLink
		call freeLink
		add esp, 4			; clean stack from param
		con:
		dec dword [stackPointer]	; decrease pointer
		jmp clearStack

	end_main:
	mov esp, ebp
	pop ebp 

	xor ebx, ebx
	mov eax, 1
	int 0x80

; -------------------------------------------------------------------------------------- ;

myCalc:
	push esi
	xor esi, esi 					; op counter

	.WHILE:
		; ------- READING INPUT ----------;	
		write calc, calc_len, STDOUT			
		push dword [stdin]
		push dword BUF_SIZE + 1			; including \n and null terminator
		push buf
		call fgets
		add esp,12				; cleaning stack from params		
		mov ecx, buf				; ecx now points to input buffer 

		
		; ------ ANALYZING INPUT -------- ;
		cmp byte [ecx], 'q'			; compare first input byte with quit char
		je .END

		push esi				; backup op counter

		menu 'p', POP_PRINT

		menu 'd', DUP

		menu '^', POWER_MUL

		menu '+', SUM_LINKS

		menu 'n', N_ONES_COUNT

		menu 'v', DIV_LINKS

		pop eax					; restore previous op counter
		cmp esi, eax				; check if an operation has been executed
		ja .WHILE				; if it has, countinue on

		call INSERT_NUM				; if not, we need to insert a number
		print_debug debug, stackPointer, stack, printLink 
		jmp .WHILE

.END:
mov eax, esi
pop esi	
ret
	

; ------------------------------ OPERATION ------ FUNCTIONS --------------------------------------- ; 

INSERT_NUM:
	pushad
	cmp dword [stackPointer], MAX_STACK_SIZE - 1
	jl .INSERT
	write overflow, overflow_len, STDERR
	jmp .END		
		
	.INSERT:
	inc dword [stackPointer]		; increase stack pointer before insertion
	xor eax, eax
	xor esi, esi
	
	; ----- IGNORING LEADING ZEROS ------ ;	
	.LOOP:
		cmp byte [ecx], '0'
		jne .COUNT
		inc ecx
		jmp .LOOP
	
	; ----- COUNT LENGTH OF INPUT ------ ; 
	.COUNT:
	mov esi, ecx				; save length without leading zeros
	xor ebx, ebx	

	; --- CHECK IF NUM IS 0 ---- ; 
	cmp byte [ecx], 10
	jne .LEN
	dec ecx
	jmp .MSB

	.LEN:
		cmp byte [ecx], 10
		je .ODD
		cmp al, 80
		je .ODD
		inc al
		inc ecx
		jmp .LEN

	.ODD:
	mov ecx, esi
	mov bl, 2
	div bl
	cmp ah, 0
	je .NEW_LINK_LOOP

	; ---- CREATE MSB FIRST LINK -------- ; 
	.MSB:	
	push LINK_SIZE			; send LINK_SIZE as a variable to 'allocateLink' function
	call allocateLink 		; allocate new link on heap - eax holds the heap pointer
	pop eax				; eax now hold the heap allocated address of new link
	xor ebx, ebx
	call checkBounds 		
	mov byte [eax], bl		; put input value in hex representation in first byte
	inc ecx

	; ---- CONNECT LINK TO OLD HEAD ----- ;

	mov edx, dword [stackPointer]	; get current index of stack pointer
	mov ebx, dword [stack + 4*edx]	; save address of old head in eax	
	inc eax				; ebx now points to 4 bytes memory block of link's next
	mov dword [eax], ebx		; save the address of old head in link's next memory block 
	dec eax		 		; eax now points to new link after connection with old head
	mov dword [stack + 4*edx], eax	; new link is now the head


	.NEW_LINK_LOOP: 
		cmp byte [ecx], 10		; compare to end of line char
		je .END
		; ----- CREATE NEW LINK ------------- ;
		
		xor ebx, ebx
		call checkBounds
		inc ecx				; move ecx to 2nd digit
		cmp byte [ecx], 10		; if next byte is '\n' we are done
		je .END
		shl ebx, 4			; ebx * 16
		call checkBounds

		push LINK_SIZE			; send LINK_SIZE as a variable to 'allocateLink' function
		call allocateLink 		; allocate new link on heap - eax holds the heap pointer
		pop eax				; eax now hold the heap allocated address of new link
		mov byte [eax], bl		; put input value in hex representation in first byte

		; ---- CONNECT LINK TO OLD HEAD ----- ;

		mov edx, dword [stackPointer]	; get current index of stack pointer
		mov ebx, dword [stack + 4*edx]	; save address of old head in eax	
		inc eax				; ebx now points to 4 bytes memory block of link's next
		mov dword [eax], ebx		; save the address of old head in link's next memory block 
		dec eax		 		; eax now points to new link after connection with old head
		mov dword [stack + 4*edx], eax	; new link is now the head

		; ------ CONTINUE TO NEXT BYTE ------ ; 
		inc ecx		
		jmp .NEW_LINK_LOOP

	.END:
	popad
	ret

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

POP_PRINT:
	cmp dword [stackPointer], 0		; check underflow
	jge .CON				; print if greater than 0 or equal				
	write underflow, underflow_len, STDERR	; write Error message
	ret
	
	.CON:
	mov edx, dword [stackPointer]
	push dword [stack + 4*edx]		; push top link to stack
	call printLink				; print it 
	call freeLink				; free that link 
	add esp, 4				; clean stack from param
	mov edx, dword [stackPointer]
	mov dword [stack + 4*edx], 0		; put null in the free cell
	dec dword [stackPointer]		; decrement the operand stack pointer
	ret

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ; 

DUP:
	cmp dword [stackPointer], 0		; check underflow
	jge .CHECK_OVERFLOW			; dup if greater than 0 or equal				
	write underflow, underflow_len, STDERR	; write Error message
	ret

	.CHECK_OVERFLOW:
	cmp dword [stackPointer], MAX_STACK_SIZE-1
	jl .CON
	write overflow, overflow_len, STDERR
	ret

	.CON:
	mov edx, dword [stackPointer]
	push dword [stack + 4*edx]		; push top link to stack
	call duplicateLink			; duplicate it
	pop eax					; save new link's address in eax
	inc dword [stackPointer]		; increament stack pointer
	mov edx, dword [stackPointer]
	mov dword [stack + 4*edx], eax		; save new link in stack
	print_debug debug, stackPointer, stack, printLink

	ret
			
; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ; 

N_ONES_COUNT:
	pushad 
	xor esi, esi
	xor edx, edx

	mov ebx, dword [stackPointer]		; ebx now holds stackPointer
	cmp ebx, 0				; check if there is at least 1 number in operand stack
	jge .CONTINUE								
	write underflow, underflow_len, STDERR	; write Error message
	popad
	ret 

	.CONTINUE:
	mov eax, dword [stack + 4*ebx]		; get Y
	push eax

	cmp ebx, MAX_STACK_SIZE - 2		; check if there is room for two links in op stack
	jle .ADD_ZERO_LINK
	
	dec ebx
	dec dword [stackPointer]
	mov edx, dword [stack +4*ebx]		; save an extra number to make room for two numbers is operand stack
	
	.ADD_ZERO_LINK:
	; ---- INSERT A ZERO TO OPERAND STACK --- ;
	push LINK_SIZE
	call allocateLink
	pop esi
	mov dword [stack + 4*ebx], esi
	inc dword [stackPointer]	

	.LOOP:
		xor ebx, ebx
		xor ecx, ecx
		mov bl, byte [eax]
		.COUNT_BITS:
		cmp bl, 0
		je .BREAK
		shl bl, 1
		jnc .COUNT_BITS
		inc cl
		jmp .COUNT_BITS
	.BREAK:
	push LINK_SIZE
	call allocateLink
	pop esi
	mov byte [esi], cl
	mov ebx, dword [stackPointer]
	mov dword [stack + 4*ebx], esi
	call SUM_LINKS
	inc dword [stackPointer]
	inc eax
	mov eax, dword [eax]
	cmp eax, 0
	jne .LOOP	
		
	dec dword [stackPointer]
	cmp edx, 0				; check to see if we made room earlier
	je .END
	
	mov ebx, dword [stackPointer]
	mov eax, dword [stack + 4*ebx]		; save 'n' result
	mov dword [stack + 4*ebx], edx
	inc ebx
	inc dword [stackPointer]
	mov dword [stack + 4*ebx], eax
	
	.END:
	call freeLink
	add esp, 4

	popad
	ret

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ; 
POWER_MUL:

	pushad
	xor eax, eax
	xor ebx, ebx
	xor ecx, ecx
	xor edx, edx
	mov byte [concat], 0
	mov byte [shift_size], 0

	cmp dword [stackPointer], 1		; check if there is at least 2 numbers in operand stack
	jge .CONTINUE								
	write underflow, underflow_len, STDERR	; write Error message
	jmp .END 

	.CONTINUE:
	
    ; ------- CHECK IF X = 0 -------- ;
    mov eax, dword [stackPointer]
    mov ebx, dword [stack + 4*eax]
    inc ebx
    cmp dword [ebx], 0
    jne .GETY
    dec ebx
    mov dl, byte [ebx]
    cmp dl, 0
    je .XZERO
	
	; ------- GET Y -------- ;
	.GETY:
	mov eax, dword [stackPointer]
	dec eax
	mov ecx, dword [stack + 4*eax]		; move Y's first (and hopefully only) link to ecx
            
	; ------- CHECK IF Y <= 200 -------- ;
	inc ecx                                 ; ecx is now pointing on next
	cmp dword [ecx], 0			; check if next is null
	jne .BIGGERY                            ; if next isn't NULL then Y > C8=200
	dec ecx					; return to data
	mov dl, byte [ecx]	
	cmp dl, 200				; compre Y's only node with 200
	ja .BIGGERY				; jump if Y>200
	mov byte [shift_size], dl		; if Y<=200 then Y is at most 1 byte (C8)

	; --- CREATING THE ANSWER HEAD ------ ; 
	push LINK_SIZE				; send LINK_SIZE as a variable to 'allocateLink' function
	call allocateLink			; allocate new link on heap - eax holds the heap pointer - ALREADY BEEN INITIALIZED TO 0
	.AFTERH:
	pop ecx					; eax now hold the heap allocated address of new link            
	push ecx				; save it on stack for later
	xor esi, esi               		; clean ecx

	; ------- CREATE 0 NODES -------- ;
	.YLOOP:
        xor edx, edx
        mov dl, byte [shift_size]
		cmp edx, 8
		jl .SHIFTING			; if shift_size is smaller than 8 we need to start shifting
		sub byte [shift_size], 8	; decrease shift_size by 8
		sub edx, 8
		cmp esi, 0
		je .AFTER_ALL
		.NOT_FS:                	  ; not first node
		push LINK_SIZE			; send LINK_SIZE as a variable to 'allocateLink' function
		call allocateLink		; allocate new link on heap - eax holds the heap pointer - ALREADY BEEN INITIALIZED TO 0
		pop eax				; eax now hold the heap allocated address of new link
		inc ecx				; ecx now pointing on next node
		mov dword [ecx], eax		; put the address of new node in the tail of the list
		mov ecx, eax			; ecx is now the tail of the list
        .AFTER_ALL:   			        ; after allocate
	mov esi, 1
	jmp .YLOOP
            
	; ------- SHIFT X BY NUM OF LEFT 0's -------- ;
	.SHIFTING:
	xor eax, eax
	mov al, byte [shift_size]		; move shift_size to eax
	;mov byte [concat], al			; eax < 8 so we move only 1 byte && shift_size is what we need to concatenate
	mov ebx, dword [stackPointer]           
	mov edx, dword [stack + 4*ebx]          ; edx pointing on X's first link, ecx pointing on the tail of the result list
	xor ebx, ebx				; clean register
            
	.SHIFT_LOOP:
		mov bl, byte [edx]		; put the number of current X node in ebx
		push ecx			; save it to use it later
		xor ecx, ecx
		mov cl, [shift_size]		; move shift_size to cl
		shl ebx, cl			; shift left ebx by the number of left 0's (multiply by 2^[shift_size])
		pop ecx
		add ebx, [concat]		; concatenate concat to ebx
		cmp esi, 0
		jne .NOT_FS_S
		.FS_S:
		mov eax, ecx
		jmp .AFTER_ALL_S
		.NOT_FS_S:
		push LINK_SIZE			; send LINK_SIZE as a variable to 'allocateLink' function
		call allocateLink		; allocate new link on heap - eax holds the heap pointer
		pop eax				; eax now hold the heap allocated address of new link
		.AFTER_ALL_S:
		mov byte [eax], bl		; put bl in first byte of eax
		shr ebx, 8			; remove LS byte of ebx
		mov byte [concat], bl		; put what's left of ebx in concat
		cmp eax, ecx
		je .NOT_SAME
		inc ecx				; ecx now points on the next of the tail of list
		mov dword [ecx], eax		; put the address of new node in the tail of the list
		mov ecx, eax			; ecx is now the tail of the list
		.NOT_SAME:
		inc edx				; inc edx to see the next of the current X node
		mov eax, dword [edx]		; put next X node in eax
		cmp eax, 0			; check if next node is null
		je .LASTNODE
		mov edx, eax			; if it's not null, move to the next node
		mov esi, 1
		jmp .SHIFT_LOOP
	
	.LASTNODE:	
	cmp byte [concat], 0        ; put the last node
	je .CON                     ; if there's nothing to concat
	push LINK_SIZE              ; if there is we need to make a new link
	call allocateLink
	pop eax
	mov byte [eax], bl          ; concat is in ebx
	inc ecx                     ; ecx now points on the next of the tail of list
	mov dword [ecx], eax        ; put the address of new node in the tail of the list       
	mov ecx, eax                ; ecx is now the tail of the list
	jmp .CON
	
    ; ------- X = 0 -------- ;
    .XZERO:
    push LINK_SIZE				; send LINK_SIZE as a variable to 'allocateLink' function
	call allocateLink			; allocate new link on heap - eax holds the heap pointer - ALREADY BEEN INITIALIZED TO 0
	pop ecx
	push ecx				; save it on stack for later

	.CON:  
	; ---- REMOVE X & Y AND ADD RESULT ---- ;
	call putResult				; first link of answer is already on stack
	add esp, 4				; clean param
	jmp .END
            
	; ------- Y>200 -------- ;
	.BIGGERY:
	write ybigger, ybigger_len, STDERR		; print Error
            
	.END:
	popad
	ret

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ; 

DIV_LINKS:
	pushad 
	xor esi, esi

	mov eax, dword [stackPointer]
	cmp eax, 1				; check if there is at least 2 numbers in operand stack
	jge .CONTINUE								
	write underflow, underflow_len, STDERR	; write Error message
	jmp .END 

	.CONTINUE:
	dec eax
	mov ecx, dword [stack + 4*eax]		; move Y's first (and hopefully only) link to ecx
            
	; ------- CHECK IF Y <= 200 -------- ;
	xor eax, eax
	inc ecx                                 ; ecx is now pointing on next
	cmp dword [ecx], 0			; check if next is null
	jne .BIGGERY                            ; if next isn't NULL then Y > C8=200
	dec ecx					; return to data
	mov al, byte [ecx]	
	cmp al, 200				; compre Y's only node with 200
	ja .BIGGERY				; jump if Y>200
	mov dl, 8				; calculate y mod 8 to know how much bits we loose after shifiting
	div dl					; and need to take from next byte
	mov byte [concat], ah							

	; ------- GET X -------- ;
	mov ebx, dword [stackPointer]           
	mov edx, dword [stack + 4*ebx]          ; edx pointing on X's first link, ecx pointing on the tail of the result list
	xor ebx, ebx				; clean register

	.IGNORE_BYTES:
		cmp al, 0			; saved earlier in calculation of y mod 8
		je .SHIFT_LOOP			; this loop goes to the first byte that won't be "deleted" when shifting left
		inc edx				; if y = 20, 2 complete bytes will be lost add from the third byte only 4 bits will be lost
		mov edx, dword [edx]		; so this loop goes to the third byte in this example
		cmp edx, 0			; if we reached the end link that means that the result is 0 
		je .ADD_LINK			; so we will jump to add a 0 link and finish the function. 
		dec al				
		jmp .IGNORE_BYTES

	; ----- CALCULATE ANSWER -------- ; 
	.SHIFT_LOOP:
		cmp edx, 0			; check if next node is null
		je .CON
		mov bl, byte [edx]		; put the number of current X node in ebx
		push ecx			; save it to use it later
		xor ecx, ecx			; clean register
		mov cl, byte [concat]		; move shift_size to cl
		shr bl, cl			; shift right the number of y mod 8
		pop ecx				; restore ecx
		
		;---- GET NEXT BYTE ----- ;		
		inc edx				; go to next node
		mov edx, dword [edx]		
		cmp edx, 0			; check if next node is null
		je .CHECK_BL			; check if bl is zero and we don't have to add it to answer
		
		; ----- CALCULATE CONCAT ---- ; 
		xor eax, eax
		mov al, byte [edx]		; get next byte
		push ecx			; save it to use it later
		xor ecx, ecx			; we want to concat the next bits to this byte
		mov cl, 8			; so we will shift left the next byte (8-concat) times to get 
		sub cl, byte [concat]		; the bits on the left with concat amount of zeros on the right
		shl al, cl			
		pop ecx				; restore ecx
		add bl, al			; add to answer	
		
		; --- CREATE LINK FOR ANSWER --- ; 
		.ADD_LINK:
		push LINK_SIZE			; send LINK_SIZE as a variable to 'allocateLink' function
		call allocateLink		; allocate new link on heap - eax holds the heap pointer
		pop eax				; eax now hold the heap allocated address of new link
		mov byte [eax], bl		; put bl in first byte of eax
		cmp esi, 0
		je .INIT_HEAD
		inc esi				; ecx now points on the next of the tail of list
		mov dword [esi], eax		; put the address of new node in the tail of the list
		mov esi, eax			; ecx is now the tail of the list
		jmp .SHIFT_LOOP
	
		.INIT_HEAD:
		mov esi, eax
		push esi			; save in stack to return at the end
		jmp .SHIFT_LOOP

		.CHECK_BL:
		cmp bl, 0
		jne .ADD_LINK
		cmp esi, 0			; check if answer is a single zero link
		jne .CON			; if not, answer is in stack. if it is, we will create a link 
		push LINK_SIZE			; that is initialized with zero due to calloc
		call allocateLink		; and is in stack for "putResult"
		

	.CON:  
	; ---- REMOVE X & Y AND ADD RESULT ---- ;
	call putResult				; first link of answer is already on stack
	add esp, 4				; clean param
	print_debug debug, stackPointer, stack, printLink 
	jmp .END

	; ------- Y>200 -------- ;
	.BIGGERY:
	write ybigger, ybigger_len, STDERR	; print Error
            
	.END:
	popad
	ret


; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ; 

SUM_LINKS:
	pushad
	xor esi, esi				; clean register
	mov edx, dword [stackPointer]
	cmp edx, 1				; check if there is at least 2 numbers in operand stack
	jge .CONTINUE								
	write underflow, underflow_len, STDERR	; write Error message
	jmp .END 

	.CONTINUE:
	mov ebx, dword [stack + 4*edx]		; ebx now holds the first operator
	dec edx					; go to second operand
	mov edx, dword [stack + 4*edx]		; edx now hold the second operator
	
	xor ecx, ecx				; clean register
	xor eax, eax				; clean register
	mov al, byte [ebx]			; mov first byte value to eax
	mov cl, byte [edx]			; mov second byte value to ecx
	push ecx				; push values to stack
	push eax
	mov byte [carry], 0

	.WHILE:
		; --------- CALCULATE BYTE ----------- ;
		pop eax				; get first byte value
		pop ecx				; get second byte value
		add ax, cx			; calculate result
		xor ecx, ecx			; clean register
		mov cl, byte [carry]		; get previous carry value (if exists)
		mov byte [carry], 0		; clean previous carry
		add ax, cx			; add the carry to the result			
		cmp ah, 0			; check to see if there is carry in this calculation
		je .ADD_NUM
		mov byte [carry], ah		; add one to carry indicator

		.ADD_NUM:
		push LINK_SIZE			; create new link for the next calculation
		call allocateLink		
		pop ecx				; ecx now holds the address to new heap-allocated link
		mov byte [ecx], al		; add result to link
		cmp esi, 0			; check if head is null
		je .INIT_HEAD
		inc esi				; go to prev's next
		mov dword [esi], ecx		; save new link
		mov esi, ecx			; save new link as new prev
		jmp .NEXT_BYTES
		
		.INIT_HEAD:
		mov esi, ecx			; save new head
		push esi			; push head to stack to return later

		; ----- GET NEXT BYTES FROM POINTERS ----- ;
		
		.NEXT_BYTES:
		getNextByte edx, eax		; macro that checks the next link of a pointer
						; if it exists, the macro pushes the next byte value
		getNextByte ebx, eax		; to the stack. if not, it pushes zero and changes the pointer to zero.
		
		mov eax, ebx			; if both values of ebx and edx are zeros
		or eax, edx			; that means that both links reached the end
		jnz .WHILE			; and we can finish the loop

	add esp, 8				; clean stack from two zeros
	cmp byte [carry], 0
	je .RESULT

	; ----- ADD CARRY IF EXISTS -------- ;
	push LINK_SIZE			; create new link for the next calculation
	call allocateLink		
	pop ecx				; ecx now holds the address to new heap-allocated link
	mov al, byte [carry]
	mov byte [ecx], al		; add result to link
	inc esi				; go to prev's next
	mov dword [esi], ecx		; save new link

	.RESULT:
	call putResult
	add esp, 4
	print_debug debug, stackPointer, stack, printLink 

	.END:
	popad
	ret
	
; ---------------------- LINK ------ FUNCTIONS -------------------------------------- ;

allocateLink:
	push ebp
	mov ebp, esp	
	pushad

	push dword [ebp + 8]		; push size of link 	
	push dword 1			; push number of items 
	call calloc
	mov dword [ebp + 8], eax	; "return" heap allocated address
	add esp, 8			; clean stack
	
	popad	
	mov esp, ebp	
	pop ebp
	ret

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ; 

freeLink:
	push ebp
	mov ebp, esp
	pushad

	.LOOP:
		mov eax, [ebp + 8]		; move current link address to eax
		push eax			; push current link address for free function
		inc eax				; go to next
		mov ebx, dword [eax]		; save the address of next link in ebx		
		mov [ebp + 8], ebx		; save the address of next link in stack
		call free
		add esp, 4			; clean stack after call
		cmp dword [ebp + 8], 0		; check if next is null
		jne .LOOP		
	popad	
	mov esp, ebp	
	pop ebp
	ret

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ; 
printLink:
	push ebp
	mov ebp, esp
	pushad

	xor esi, esi				; first byte indicator
	xor ecx, ecx				; counter of push ops
	push dword [ebp + 8]			; backup link

	; ----- PUSH BYTES TO STACK ------- ;
	.LOOP:
		mov eax, dword [ebp + 8]	; mov address in index to eax
		xor edx, edx
		mov dl, byte [eax]		; mov it's first byte (value) to line for a print
		push edx
		inc esi
		inc ecx				; count how many push ops
		inc eax				; go to next
		mov ebx, dword [eax]		; save the address of next link in ebx
		mov dword [ebp + 8], ebx	; save the address of next link in temp
		cmp ebx, 0			; check if next is null
		jne .LOOP

	; ----- PRINT IN REVERSE --------- ; 
	.PRINT:	
		pop edx				; get value of byte
		pushad				; backup regs before printf
		push edx			; push value for printf
		cmp esi, ecx			; check if this is the first byte
		je .front
		push hex_format_mid		; push format of hex to printf
		jmp .printf
		.front:
		push hex_format			; if it is, print without leading zeros
		.printf:
		call printf			
		add esp, 8			; clean stack 
		popad				; restore regs
		dec ecx				; decrement counter
		cmp ecx, 0 			; see if there is no need for more pop ops
		jne .PRINT
	
	push new_line				; print \n
	call printf
	add esp, 4
	
	pop eax
	mov dword [ebp + 8], eax		; restore link
	popad
	mov esp, ebp	
	pop ebp
	ret

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ; 
checkBounds:
	xor edx, edx				; clean edx
	
	; --- NOT LEGAL --- ; 
	cmp byte [ecx], '0'			
	jl .end					; smaller than '0'
		
	; --- ABOVE NINE --- ;
	cmp byte [ecx], '9'			
	jg .lowerA
						
	mov dl, byte [ecx]			; number between 0 - 9 
	sub dl, 48				; turn it to real number
	add ebx, edx
	jmp .end
		
	.lowerA:
	cmp byte [ecx], 'A'			; check lower bound of 'A'
	jl .end					; between '9' to 'A'
		
	;upperF:
	cmp byte [ecx], 'F'			; check upper bound of 'F'
	jg .end					; bigger than 'F'

	mov dl, byte [ecx]
	sub dl, 55				; turn it to real number
	add ebx, edx

	.end:
ret

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ; 

duplicateLink:
	push ebp
	mov ebp, esp
	pushad
	
	xor esi, esi
	
	.LOOP:
		push LINK_SIZE
		call allocateLink		; allocate new link
		.con:
		pop eax				; eax now hold the heap allocated address
		mov ecx, dword [ebp + 8]	; get byte value of duplicated link
		xor edx, edx			; clean register		
		mov dl, byte [ecx]		; put the data of the link in dl
		mov byte [eax], dl		; add it to the new link
		cmp esi, 0
		je .initHead			; init first link
		inc esi				; get old link's next
		mov dword [esi], eax		; connect with new link
		mov esi, eax			; save it for new link
	
		.CONTINUE:
		inc ecx				; go to duplicated link's next
		mov ebx, dword [ecx]		; save the address of next link in ebx		
		mov [ebp + 8], ebx		; save the address of next link in stack
		cmp ebx, 0			; check if next is null
		jne .LOOP
		je .BREAK

		.initHead:
		mov esi, eax			; save new link as prev
		push esi			; save head to return later
		jmp .CONTINUE		

	.BREAK:
	pop esi
	mov dword [ebp + 8], esi		; "return" head of duplicated link
	popad
	pop ebp
	ret

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

putResult:
	push ebp
	mov ebp, esp
	pushad

	; ------ FREE X & Y FROM MEMORY AND OPERAND STACK ---------- ;
	mov eax, 2				; initialize counter
	.LOOP:
		mov edx, dword [stackPointer]			
		mov ebx, dword [stack + 4*edx]	; ebx now hold the address for the head link pointed by stackPointer	
		push ebx			
		call freeLink			; free link frees all links in the link
		add esp, 4			; clean stack from param 
		mov edx, dword [stackPointer]
		mov dword [stack + 4*edx], 0 	; put null in cell after freeing data
		dec dword [stackPointer]	; decrement operand stack pointer
		dec eax				; decrement counter
		cmp eax, 0
	jne .LOOP

	; ----- ADD RESULT TO OPERAND STACK ------------- ; 
	
	mov ecx, dword [ebp + 8]		; get head of result from stack 
	inc dword [stackPointer]		
	mov edx, dword [stackPointer]	
	mov dword [stack + 4*edx], ecx		; save answer in stack
	
	popad
	mov esp, ebp
	pop ebp
ret
		
