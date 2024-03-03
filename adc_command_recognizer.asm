LOCALS @@                           ; Enables the use of @@ as a local label prefix in procedures

.MODEL small
.STACK 100h

IntNo = 1h                          ; Defines interrupt number 1 for step-by-step debugging

; ///////////////////////////////////////////////////////////////////
; MACROS
; ///////////////////////////////////////////////////////////////////

PUTCH MACRO CHAR1                   ; Macro to print a character to the console
    mov ah,2                        ; Set function number for 'write character' in DOS interrupt
    mov dl,CHAR1                    ; Load the character into DL register
    int 21h
ENDM

NEW_LINE MACRO                      ; Macro to print a new line
    mov ah,2                        ; Set function number for 'write character'
    mov dl,13
    int 21h
    mov dl,10
    int 21h                         ; Call DOS interrupt to print LF
ENDM

WRITE macro _MESSAGE_               ; Macro to write a string to the console
    mov ah,09h                      ; Set function number for 'write string' in DOS interrupt
    mov dx,offset _MESSAGE_         ; Load the offset of the string into DX
    int 21h
ENDM

PAUSE MACRO                         ; Macro to wait for a key press
    mov ah, 8                       ; Set function number for 'wait for character' in DOS interrupt
    int 21h
ENDM


; ///////////////////////////////////////////////////////////////////
; FIRST BYTE OF CODE OPERATION OF ADC COMMAND
; ///////////////////////////////////////////////////////////////////

COP_ADC_AL_I8		EQU	14H		; ADC Al, i8
COP_ADC_AX_I16		EQU	15H		; ADC AX, i16
COP_ADC_R8_R8		EQU	12H		; ADC R8, R8
COP_ADC_R8_MEM01	EQU	12H		; ADC R8, MEM01
COP_ADC_R8_I8		EQU	80H		; ADC R8, i8
COP_ADC_R16_R16		EQU	13H		; ADC R16, R16
COP_ADC_R16_MEM01	EQU	13H		; ADC R16, MEM01
COP_ADC_R16_I16		EQU	81H		; ADC R16, i16
COP_ADC_R16_I8		EQU	83H		; ADC R16, i8
COP_ADC_MEM01_R8	EQU	10H		; ADC MEM01, R8
COP_ADC_MEM01_R16	EQU 11H		; ADC MEM01, R16


; ///////////////////////////////////////////////////////////////////
.DATA                                 ; Beginning of the data segment
; ///////////////////////////////////////////////////////////////////

result      db '0000','$'             ; String to store a 16-bit value in ASCII, initialized with '0000'
result1Byte db '00','$'               ; String to store an 8-bit value in ASCII, initialized with '00'
cs_reg      dw 0                      ; Word to store the CS register value
ip_reg      dw 0                      ; Word to store the IP register value

tblhex      db '0123456789ABCDEF'     ; Table of hexadecimal characters for conversion
tblReg8     db 'AL$ CL$ DL$ BL$ AH$ CH$ DH$ BH$' ; Table of 8-bit register names
tblReg16    db 'AX$ CX$ DX$ BX$ SP$ BP$ SI$ DI$' ; Table of 16-bit register names

operandsAL_I8  db " ADC AL,", "$"     ; Operand string template for ADC AL, immediate 8-bit
operandsAX_I16 db " ADC AX,", "$"     ; Operand string template for ADC AX, immediate 16-bit
operandsRR     db " ADC ", "$"        ; Operand string template for ADC register, register

tblMem01    db "[BX+SI]", "$"         ; Memory operand templates for ADC instruction
            db "[BX+DI]", "$"
            db "[BP+SI]", "$"
            db "[BP+DI]", "$"
            db "[SI]", "$   "
            db "[DI]", "$   "
            db "[OFFx]", "$ "
            db "[BX]", "$   "
pran        db 'Zingsninio rezimo pertraukimo (int 1) apdorojimo procedura, atpazistanti komanda ADC reg+r/m.',10,13
            db '----------------------------------------------------',10,13
            db 'Si procedura turi patikrinti, ar pertraukimas ivyko pries vykdant komandos ADC pirmaji varianta, jei taip, į ekrana isvesti perspejima, ir visa informacija apie komanda: adresa, koda, mnemonika, operandus.',10,13
            db 'Pvz.: I ekrana isvedama informacija galetu atrodyti taip: Zingsninio rezimo pertraukimas! 0000:0128  12C6  adc al, dh ; al= 00, dh= 11',10,13
            db '----------------------------------------------------',10,13
            db 'Atliko: Gustas Grieze, 1 grupe',10,13
pranIlgis   dw ($-pran)

.CODE

OldISRSeg   DW 0                       ; Word to store the old ISR segment address
OldISROfs   DW 0                       ; Word to store the old ISR offset address


Strt:
    mov ax,@data                       ; Load the data segment address into AX
    mov ds,ax                          ; Set DS to the data segment

    MOV ah, 40h                        ; DOS function to write to file or device
    MOV bx, 1                          ; Standard output handle
    MOV cx, pranIlgis                  ; Number of bytes to write
    LEA dx, pran                       ; Load effective address of the string 'pran' into DX
    int 21h                            ; Call DOS interrupt 21h to output the string

    NEW_LINE                           ; Call macro to print a new line

    mov ax, 0                          ; Clear AX
    mov es, ax                         ; Set ES (extra segment) to 0

                                        ; Saving the current ISR address for interrupt 1
    mov ax,es:[IntNo*4]                ; Load the offset of the current ISR into AX
    mov cs:[OldISROfs],ax              ; Store ISR offset in OldISROfs
    mov ax,es:[IntNo*4 + 2]            ; Load the segment of the current ISR into AX
    mov cs:[OldISRSeg],ax              ; Store ISR segment in OldISRSeg
                                        ; Setting up custom interrupt handler for int 1
    pushf                               ; Save flags on stack
    cli                                 ; Clear interrupt flag to disable interrupts
    mov word ptr es:[IntNo*4], offset Proc_Int01 ; Set offset of custom ISR in interrupt vector table
    mov word ptr es:[IntNo*4 + 2], seg Proc_Int01 ; Set segment of custom ISR in interrupt vector table
    popf                                ; Restore flags from stack

    pushf                               ; Save flags on stack again
    pop ax                              ; Pop flags into AX
    mov bx,0100h                        ; Load trap flag mask into BX
    or ax,bx                            ; Set trap flag in AX (to enable step-by-step mode)
    push ax                             ; Push modified flags back onto stack
    popf                                ; Restore modified flags (with trap flag set)

    NOP                                 ; No operation (used here as a breakpoint for the interrupt)

	mov al,1
	ADC     AL,01
;   ADC     BL,[0001]
;	mov bl,3
	adc ah,bl
	adc ah,cl
;	adc ah,ah
;	ADC     AX, AX
	ADC     AX, BX
	ADC     AX, CX
;	ADC     AX, DX
;	ADC	AX, [SI]
;	ADC AX, 01234h
	ADC CH, 7
	ADC BX, 01234h
	ADC CX, 05678h
	ADC DX, 05678h
	ADC BX, 01h
	ADC DI, SI
;	ADC SP, BP
;	ADC     [BX+SI],AL
	ADC     [BP+SI],AX
;	ADC     [BX],CX
;	ADC     AL,[BX+SI]
;	mov al,2
	mov ah,2
	mov bl,2
                                                    ;Atstatome sena pertraukimu apdorojimo procedura

    pushf                               ; Save flags on stack again
    pop ax                              ; Pop flags into AX
    mov bx,0FEFFh                       ; Load inverse of trap flag mask into BX
    and ax,bx                           ; Clear trap flag in AX (to disable step-by-step mode)
    push ax                             ; Push modified flags back onto stack
    popf                                ; Restore modified flags (with trap flag cleared)
                                        ; Restoring the original interrupt handler
    pushf                               ; Save flags on stack
    cli                                 ; Clear interrupt flag to disable interrupts
    mov ax,cs:[OldISROfs]               ; Load the old ISR offset into AX
    mov word ptr es:[IntNo*4],ax        ; Set the old ISR offset in the interrupt vector table
    mov ax,cs:[OldISRSeg]               ; Load the old ISR segment into AX
    mov word ptr es:[IntNo*4 + 2],ax    ; Set the old ISR segment in the interrupt vector table
    popf                                ; Restore flags from stack

    PAUSE                               ; Wait for a key press (calls PAUSE macro)

                                        ; Exiting the program
    mov ax,04C00h                       ; Load exit program function number into AX
    int 21h                             ; Call DOS interrupt to terminate the program

; ///////////////////////////////////////////////////////////////////
; END OF PROGRAM
; ///////////////////////////////////////////////////////////////////

;-------------------------------------------------------------------
Proc_Int01 PROC                        ; Start of the custom interrupt handler procedure
;-------------------------------------------------------------------
    ; Pushing registers and flags onto stack to preserve the current state
    push sp                             ; Save SP (stack pointer) on stack
    push ax                             ; Save AX on stack
    push bx                             ; Save BX on stack
    push cx                             ; Save CX on stack
    push dx                             ; Save DX on stack
    push si                             ; Save SI on stack
    push di                             ; Save DI on stack
    push bp                             ; Save BP (base pointer) on stack
    push ds                             ; Save DS (data segment) on stack
    push es                             ; Save ES (extra segment) on stack
    push ss                             ; Save SS (stack segment) on stack
    mov BP,SP                           ; Move current stack pointer to BP for stack frame

    mov ax,@data                        ; Load the data segment address into AX
    mov ds,ax                           ; Set DS to the data segment

    mov ax, [bp]+24                     ; Load the CS register value from the stack into AX
    mov [cs_reg], ax                    ; Store the CS register value in cs_reg
    mov si, [bp]+22                     ; Load the IP register value from the stack into SI
    mov [ip_reg], si                    ; Store the IP register value in ip_reg
    push ds                             ; Save DS on stack
    mov ax, [cs_reg]                    ; Load the CS register value into AX
    push ax                             ; Save AX (CS value) on stack
    pop ds                              ; Set DS to the CS value for fetching next instruction
    mov ax, [si]                        ; Load the next instruction opcode into AX
    mov cx, ax                          ; Store the opcode in CX
    pop ds                              ; Restore DS
                                        ; Checking if the next operation is an ADC operation
    cmp al, COP_ADC_R8_R8               ; Compare opcode with ADC R8 to R8
    jne L_NEXT                          ; Jump to L_NEXT if not ADC R8 to R8

    push cx                             ; Save CX on stack
    mov bl,0C0h                         ; Load mod-reg-r/m byte mask into BL
    and ch,bl                           ; Extract mod bits from the opcode
    shr ch,6                            ; Shift right to get the mod value
    mov bl,ch                           ; Move mod value to BL
    pop cx                              ; Restore CX from stack
    cmp bl,3                            ; Compare mod value with 3 (indicating register addressing)
    je L_WRITE1                         ; Jump to L_WRITE1 if mod value is 3 (register addressing)

    L_NEXT:
        cmp al, COP_ADC_R16_R16             ; Compare opcode with ADC R16 to R16
        jne L_NEXT2                         ; Jump to L_NEXT2 if not ADC R16 to R16

        push cx                             ; Save CX on stack
        mov bl,0C0h                         ; Load mod-reg-r/m byte mask into BL
        and ch,bl                           ; Extract mod bits from the opcode
        shr ch,6                            ; Shift right to get the mod value
        mov bl,ch                           ; Move mod value to BL
        pop cx                              ; Restore CX from stack
        cmp bl,3                            ; Compare mod value with 3 (indicating register addressing)
        je L_WRITE2                         ; Jump to L_WRITE2 if mod value is 3 (register addressing)

    L_NEXT2:
        cmp al, COP_ADC_AL_I8               ; Compare opcode with ADC AL, immediate 8
        je L_WRITE3                         ; Jump to L_WRITE3 if ADC AL, immediate 8
        cmp al, COP_ADC_AX_I16              ; Compare opcode with ADC AX, immediate 16
        je L_WRITE4                         ; Jump to L_WRITE4 if ADC AX, immediate 16
        cmp al, COP_ADC_R8_I8               ; Compare opcode with ADC R8, immediate 8
        je L_WRITE5                         ; Jump to L_WRITE5 if ADC R8, immediate 8
        cmp al, COP_ADC_R16_I16             ; Compare opcode with ADC R16, immediate 16
        je L_WRITE6                         ; Jump to L_WRITE6 if ADC R16, immediate 16
        cmp al, COP_ADC_R16_I8              ; Compare opcode with ADC R16, immediate 8
        je L_WRITE7                         ; Jump to L_WRITE7 if ADC R16, immediate 8
        cmp al, COP_ADC_MEM01_R8            ; Compare opcode with ADC memory to R8
        je L_WRITE8                         ; Jump to L_WRITE8 if ADC memory to R8
        cmp al, COP_ADC_MEM01_R16           ; Compare opcode with ADC memory to R16
        je L_WRITE9                         ; Jump to L_WRITE9 if ADC memory to R16
        cmp al, COP_ADC_R8_MEM01            ; Compare opcode with ADC R8 to memory
        je L_WRITE10                        ; Jump to L_WRITE10 if ADC R8 to memory
        cmp al, COP_ADC_R16_MEM01           ; Compare opcode with ADC R16 to memory
        je L_WRITE11                        ; Jump to L_WRITE11 if ADC R16 to memory

        JMP @@skip                          ; Jump to @@skip label if none of the above conditions are met
    L_WRITE1:
        call WriteSegOff                      ; Call procedure to write the segment and offset of the instruction
        call WriteOperandsADC_R8_R8           ; Call procedure to write operands for ADC R8 to R8 operation
        JMP @@END_WRITE                       ; Jump to end of write procedure

    L_WRITE2:
        call WriteSegOff                      ; Call procedure to write the segment and offset of the instruction
        call WriteOperandsADC_R16_R16         ; Call procedure to write operands for ADC R16 to R16 operation
        JMP @@END_WRITE                       ; Jump to end of write procedure

    L_WRITE3:
        call WriteSegOff
        call WriteOperandsADC_AL_I8
        JMP @@END_WRITE

    L_WRITE4:
        call WriteSegOff
        call WriteOperandsADC_AX_I16
        JMP @@END_WRITE

    L_WRITE5:
        call WriteSegOff
        call WriteOperandsADC_R8_I8
        JMP @@END_WRITE

    L_WRITE6:
        call WriteSegOff
        call WriteOperandsADC_R16_I16
        JMP @@END_WRITE

    L_WRITE7:
        call WriteSegOff
        call WriteOperandsADC_R16_I8
        JMP @@END_WRITE

    L_WRITE8:
        call WriteSegOff
        call WriteOperandsADC_MEM01_R8
        JMP @@END_WRITE

    L_WRITE9:
        call WriteSegOff
        call WriteOperandsADC_MEM01_R16
        JMP @@END_WRITE

    L_WRITE10:
        call WriteSegOff
        call WriteOperandsADCR8_MEM01
        JMP @@END_WRITE

    L_WRITE11:
        call WriteSegOff
        call WriteOperandsADCR16_MEM01
        JMP @@END_WRITE

    @@END_WRITE:
        NEW_LINE                              ; Call macro to print a new line

    @@skip:
        ; Restore stack and registers to original state before returning from interrupt
        pop ss                                ; Restore stack segment (SS) from stack
        pop es                                ; Restore extra segment (ES) from stack
        pop ds                                ; Restore data segment (DS) from stack
        pop bp                                ; Restore base pointer (BP) from stack
        pop di                                ; Restore destination index (DI) from stack
        pop si                                ; Restore source index (SI) from stack
        pop dx                                ; Restore data register (DX) from stack
        pop cx                                ; Restore count register (CX) from stack
        pop bx                                ; Restore base register (BX) from stack
        pop ax                                ; Restore accumulator (AX) from stack
        pop sp                                ; Restore stack pointer (SP) from stack

        ; Return control to the calling program
        push cs:[OldISRSeg]                   ; Push old ISR segment onto stack
        push cs:[OldISROfs]                   ; Push old ISR offset onto stack
        retf                                  ; Return far from procedure (restores CS and IP from stack)
Proc_Int01 ENDP                          ; End of the custom interrupt handler procedure

;========================================================
WriteSegOff PROC                        ; Procedure to write the segment and offset of the instruction
;========================================================
    push si                             ; Save SI register on stack
    push ax                             ; Save AX register on stack
    push bx                             ; Save BX register on stack
    push dx                             ; Save DX register on stack

    mov si, offset result               ; Load the address of the result string into SI
    mov ax, [cs_reg]                    ; Load the value of CS register into AX
    call binasc                         ; Call procedure to convert binary AX to ASCII
    WRITE result                        ; Write the result string (CS value in ASCII)

    PUTCH ':'                           ; Print colon character to separate CS and IP
    mov ax, [ip_reg]                    ; Load the value of IP register into AX
    mov si, offset result               ; Load the address of the result string into SI
    call binasc                         ; Call procedure to convert binary AX to ASCII
    WRITE result                        ; Write the result string (IP value in ASCII)

    PUTCH ' '                           ; Print space character

    xchg cl,ch                          ; Exchange high and low bytes of CX
    mov ax, cx                          ; Move the opcode value into AX
    mov si, offset result               ; Load the address of the result string into SI
    call binasc                         ; Call procedure to convert binary AX to ASCII
    WRITE result                        ; Write the opcode in ASCII

    pop dx                              ; Restore DX register from stack
    pop bx                              ; Restore BX register from stack
    pop ax                              ; Restore AX register from stack
    pop si                              ; Restore SI register from stack
    ret                                 ; Return from procedure
ENDP

;========================================================
WriteOperandsADC_R8_R8 PROC           ; Procedure to write operands for ADC R8 to R8 operation
;========================================================
    push ax                           ; Save AX register on stack
    push cx                           ; Save CX register on stack
    push dx                           ; Save DX register on stack

    WRITE operandsRR                  ; Write the operand template string " ADC "

    push cx                           ; Save CX register on stack
    mov al,38h                        ; Load mask for extracting register from opcode
    and cl,al                         ; Apply mask to isolate register bits
    shr cl,3                          ; Right-shift to get register index
    shl cl,2                          ; Multiply index by 4 (each register name is 4 characters including '$')
    mov si,offset tblReg8             ; Load address of 8-bit register name table into SI
    mov ch,0                          ; Clear CH
    add si,cx                         ; Add index to SI to point to the correct register name
    WRITE si                          ; Write the register name

    PUTCH ','                         ; Write a comma to separate operands
    pop cx                            ; Restore CX register from stack

    mov al,07h                        ; Load mask for extracting second register from opcode
    and cl,al                         ; Apply mask to isolate second register bits
    shl cl,2                          ; Multiply index by 4
    mov si,offset tblReg8             ; Load address of 8-bit register name table into SI
    mov ch,0                          ; Clear CH
    add si,cx                         ; Add index to SI
    WRITE si                          ; Write the second register name

    pop dx                            ; Restore DX register from stack
    pop cx                            ; Restore CX register from stack
    pop ax                            ; Restore AX register from stack
    ret                               ; Return from procedure
WriteOperandsADC_R8_R8 ENDP

;========================================================
WriteOperandsADC_R16_R16 PROC         ; Procedure to write operands for ADC R16 to R16 operation
;========================================================
    push ax                           ; Save AX register on stack
    push cx                           ; Save CX register on stack
    push dx                           ; Save DX register on stack

    WRITE operandsRR                  ; Write the operand template string " ADC "

    push cx                           ; Save CX register on stack
    mov al,38h                        ; Load mask for extracting register from opcode
    and cl,al                         ; Apply mask to isolate register bits
    shr cl,3                          ; Right-shift to get register index
    shl cl,2                          ; Multiply index by 4 (each register name is 4 characters including '$')
    mov si,offset tblReg16            ; Load address of 16-bit register name table into SI
    mov ch,0                          ; Clear CH
    add si,cx                         ; Add index to SI to point to the correct register name
    WRITE si                          ; Write the register name

    PUTCH ','                         ; Write a comma to separate operands
    pop cx                            ; Restore CX register from stack

    mov al,07h                        ; Load mask for extracting second register from opcode
    and cl,al                         ; Apply mask to isolate second register bits
    shl cl,2                          ; Multiply index by 4
    mov si,offset tblReg16            ; Load address of 16-bit register name table into SI
    mov ch,0                          ; Clear CH
    add si,cx                         ; Add index to SI
    WRITE si                          ; Write the second register name

    pop dx                            ; Restore DX register from stack
    pop cx                            ; Restore CX register from stack
    pop ax                            ; Restore AX register from stack
    ret                               ; Return from procedure
WriteOperandsADC_R16_R16 ENDP

;========================================================
WriteOperandsADC_AL_I8 PROC           ; Procedure to write operands for ADC AL, immediate 8 operation
;========================================================
    push ax                           ; Save AX register on stack
    push cx                           ; Save CX register on stack
    push dx                           ; Save DX register on stack

    WRITE operandsAL_I8               ; Write the operand template string " ADC AL,"

    MOV AL, CL                        ; Move the immediate value (second byte of instruction) into AL
    mov si, offset result1Byte        ; Load the address of the result string for one byte into SI
    CALL binasc1Byte                  ; Convert the immediate value to ASCII hex
    WRITE result1Byte                 ; Write the immediate value in ASCII hex

    pop dx                            ; Restore DX register from stack
    pop cx                            ; Restore CX register from stack
    pop ax                            ; Restore AX register from stack
    ret                               ; Return from procedure
WriteOperandsADC_AL_I8 ENDP

;========================================================
WriteOperandsADC_AX_I16 PROC          ; Procedure to write operands for ADC AX, immediate 16 operation
;========================================================
    push ax                           ; Save AX register on stack
    push cx                           ; Save CX register on stack
    push dx                           ; Save DX register on stack

    mov ax, [cs_reg]                  ; Load CS register value into AX
    mov si, [ip_reg]                  ; Load IP register value into SI
    push ds                           ; Save DS register on stack
    push ax                           ; Save AX register on stack (CS value)
    pop ds                            ; Set DS to the CS value to fetch next instruction byte
    mov ax, [si]+1                    ; Load the next byte (immediate value) of the instruction into AX
    pop ds                            ; Restore DS register from stack

    push ax                           ; Save the immediate value on stack
    mov si, offset result1Byte        ; Load the address of the result string for one byte into SI
    xchg al,ah                        ; Exchange high and low bytes of AX
    CALL binasc1Byte                  ; Convert the high byte of the immediate value to ASCII hex
    WRITE result1Byte                 ; Write the high byte of the immediate value in ASCII hex

    WRITE operandsAX_I16              ; Write the operand template string " ADC AX,"

    pop ax                            ; Restore the immediate value from stack
    mov si, offset result             ; Load the address of the result string for two bytes into SI
    CALL binasc                       ; Convert the low byte of the immediate value to ASCII hex
    WRITE result                      ; Write the low byte of the immediate value in ASCII hex

    pop dx                            ; Restore DX register from stack
    pop cx                            ; Restore CX register from stack
    pop ax                            ; Restore AX register from stack
    ret                               ; Return from procedure
WriteOperandsADC_AX_I16 ENDP
;========================================================
;========================================================
WriteOperandsADC_R8_I8 PROC
;========================================================
	push ax
 	push cx
 	push dx

	mov ax, [cs_reg]			                                ;Paimam kita baita
	mov si, [ip_reg]
	push ds
	push ax
	pop ds
	mov ax, [si]+1
	pop ds

	push ax
	mov si, offset result1Byte	                                ;one byte to code
	xchg al,ah
	CALL binasc1Byte
	WRITE result1Byte

	WRITE operandsRR
								                                ;Randam registro varda1101_[0reg]
                                                                ;in CL kitas operacijos kodo baitas
	mov al,07h					                                ;in CL [0reg]
	and cl,al					                                ;CL, indexas tblReg8
	shl cl,2					                                ;cl*4, tblReg8 struktura
	mov si,offset tblReg8
	mov ch,0
	add si,cx
	WRITE si

	pop ax						                               ;Parasom paskutini inm operanda
	xchg al,ah
	mov si, offset result1Byte
	CALL binasc1Byte
	PUTCH ','
	WRITE result1Byte

	pop dx
	pop cx
	pop ax
	ret

WriteOperandsADC_R8_I8  ENDP

;========================================================
WriteOperandsADC_MEM01_R8 PROC
;========================================================
	push ax
 	push cx
 	push dx

	WRITE operandsRR

	PUSH CX
								                                ;Randam registro varda [mod][reg][r/m] [xx][xxx][xxx]
	mov al,07h					                                ;in CL [rM]
	and cl,al					                                ;CL, indexas tblMem01
	shl cl,3					                                ;cl*8, tblMem01 struktura
	mov si,offset tblMem01
	mov ch,0
	add si,cx
	WRITE si
	PUTCH ','

	POP CX
                                                                ;Randam registro varda [mod][reg][r/m] [xx][xxx][xxx]
                            							         ;in CL, kitas kodo operacijos baitas
	mov al,38h					                                 ;in CL [reg]
	and cl,al					                                 ;CL, indexas tblReg8
	shr cl,3
	shl cl,2					                                  ;cl*4, tblReg8 struktura
	mov si,offset tblReg8
	mov ch,0
	add si,cx
	WRITE si

	pop dx
	pop cx
	pop ax
	ret

WriteOperandsADC_MEM01_R8  ENDP

;========================================================
WriteOperandsADCR8_MEM01 PROC
;========================================================
	push ax
 	push cx
 	push dx

	WRITE operandsRR

	PUSH CX
                                                        ;Randam registro varda [mod][reg][r/m] [xx][xxx][xxx]
                                                        ;in CL, kitas kodo operacijos baitas
	mov al,38h					                        ;in CL [reg]
	and cl,al					                        ;CL, indexas tblReg8
	shr cl,3
   	shl cl,2					                        ;cl*4, tblReg8 struktura
	mov si,offset tblReg8
	mov ch,0
	add si,cx
	WRITE si
	PUTCH ','

	POP CX
								                        ;Randam registro varda [mod][reg][r/m] [xx][xxx][xxx]
	mov al,07h					                        ;in CL [rM]
	and cl,al					                        ;CL, indexas tblMem01
	shl cl,3					                        ;cl*8, tblMem01 struktura
	mov si,offset tblMem01
	mov ch,0
	add si,cx
	WRITE si

	pop dx
	pop cx
	pop ax
	ret

WriteOperandsADCR8_MEM01 ENDP

;========================================================
WriteOperandsADCR16_MEM01 PROC
;========================================================
	push ax
 	push cx
 	push dx

	WRITE operandsRR

	PUSH CX

							                              ; Randam registro varda  [mod][reg][r/m] [xx][xxx][xxx]
							                              ;in CL, kitas kodo operacijos baitas
	mov al,38h				                              ;in CL [reg]
	and cl,al				                              ;CL, indexa tblReg8
	shr cl,3
	shl cl,2				                              ;cl*4, tblReg8 struktura
	mov si,offset tblReg16
	mov ch,0
	add si,cx
	WRITE si
	PUTCH ','

	POP CX
							                               ;Randam registro varda  [mod][reg][r/m] [xx][xxx][xxx]
	mov al,07h				                               ;in CL [rM]
	and cl,al				                               ;CL, indexas tblMem01
	shl cl,3				                               ;cl*8, tblMem01 struktura
	mov si,offset tblMem01
	mov ch,0
	add si,cx
	WRITE si


	pop dx
	pop cx
	pop ax
	ret

WriteOperandsADCR16_MEM01  ENDP


;========================================================
WriteOperandsADC_MEM01_R16 PROC
;========================================================
	push ax
 	push cx
 	push dx

	WRITE operandsRR

	PUSH CX
								                ;Randam registro varda  [mod][reg][r/m] [xx][xxx][xxx]
	mov al,07h					                ;in CL [rM]
	and cl,al					                ;CL, indexas tblMem01
	shl cl,3				                   	;cl*8, tblMem01 struktura
	mov si,offset tblMem01
	mov ch,0
	add si,cx
	WRITE si
	PUTCH ','

	POP CX

								                ;Randam registro varda  [mod][reg][r/m] [xx][xxx][xxx]
								                ;in CL, kitas kodo operacijos baitas
	mov al,38h					                ;in CL [reg]
	and cl,al					                ;CL,  indexas tblReg8
	shr cl,3
	shl cl,2					                ;cl*4, tblReg8 struktura
	mov si,offset tblReg16
	mov ch,0
	add si,cx
	WRITE si

	pop dx
	pop cx
	pop ax
	ret

WriteOperandsADC_MEM01_R16  ENDP



;========================================================
WriteOperandsADC_R16_I16 PROC
;========================================================
	push ax
 	push cx
 	push dx

	mov ax, [cs_reg]			                        ;Paimam kitus 2 baitus
	mov si, [ip_reg]
	push ds
	push ax
	pop ds
	mov ax, [si]+2
	pop ds

	push ax
	mov si, offset result		                        ;2 baitus to code
	CALL binasc
	WRITE result

	WRITE operandsRR
                                                        ;Randam registro varda  1101_[0reg]
								                        ;in CL, kitas kodo operacijos baitas
	mov al,07h					                        ;in CL [0reg]
	and cl,al					                        ;CL, indexas tblReg8
	shl cl,2					                        ;cl*4, tblReg8 struktura
	mov si,offset tblReg16
	mov ch,0
	add si,cx
	WRITE si

	pop ax						                       ;Parasom paskutinius inm operandus
	mov si, offset result
	CALL binasc
	PUTCH ','
	WRITE result

	pop dx
	pop cx
	pop ax
	ret

WriteOperandsADC_R16_I16 ENDP

;========================================================
WriteOperandsADC_R16_I8 PROC
;========================================================
	push ax
 	push cx
 	push dx

	mov ax, [cs_reg]			                        ;Paimam kita baita
	mov si, [ip_reg]
	push ds
	push ax
	pop ds
	mov ax, [si]+1
	pop ds

	push ax
	mov si, offset result1Byte	                        ;viena baita to code
	xchg al,ah
	CALL binasc1Byte
	WRITE result1Byte

	WRITE operandsRR
								                    ;Randam registro varda  1101_[0reg]
								                    ;in CL, kitas kodo operacijos baitas
	mov al,07h					                    ;in CL [0reg]
	and cl,al					                    ;CL, indexas tblReg8
	shl cl,2					                    ;cl*4, tblReg8 struktura
	mov si,offset tblReg16
	mov ch,0
	add si,cx
	WRITE si

	pop ax						                   ;Parasom paskutinius inm operandus
	xchg al,ah
	mov si, offset result1Byte
	CALL binasc1Byte
	PUTCH ','
	WRITE result1Byte

	pop dx
	pop cx
	pop ax
	ret

WriteOperandsADC_R16_I8  ENDP

;========================================================
binasc1Byte proc
;========================================================
;AL                                  ; Indicates that the input value is in the AL register
;DS:SI eilutes su rezultatu adresas   ; Indicates that DS:SI points to where the result will be stored

    push cx                          ; Save the CX register on the stack (to preserve its value for later restoration)
    push ax                          ; Save the AX register on the stack (to preserve the AL part which contains the byte to convert)

    and al,0f0h                      ; Isolate the high nibble (4 bits) of the byte in AL
    mov cl,4                         ; Set CL to 4, preparing to right shift AL by 4 bits
    shr al,cl                        ; Right shift AL by 4 bits, moving the high nibble to the low nibble position
    call hexasc                      ; Call the 'hexasc' procedure to convert this low nibble (originally high nibble) to its ASCII character

    pop ax                           ; Restore AX (and thus AL) from the stack to its original state
    push ax                          ; Save AX again for further conversion of the next nibble
    and al,0fh                       ; Isolate the low nibble (4 bits) of the original byte in AL
    inc si                           ; Increment SI to point to the next position in the result string

    call hexasc                      ; Call the 'hexasc' procedure to convert this low nibble to its ASCII character
    pop ax                           ; Restore AX (and thus AL) from the stack to its original state
    pop cx                           ; Restore the original value of the CX register from the stack
    ret                              ; Return from the procedure
binasc1Byte endp


;========================================================
binasc proc                            ; Procedure to convert binary value to ASCII hex
;========================================================
    ;dvejetainius i siesioliktainius
    ;isvedam i ekrana (ax)
    ;DS:IS eilutes su rezultatu adresas

    push cx                             ; Save CX register on stack
    push ax                             ; Save AX register on stack

    and ax,0f000h                       ; Isolate the high nibble of the high byte of AX
    mov cl,12                           ; Set CL to 12 (for right-shifting 12 bits)
    shr ax,cl                           ; Right-shift AX by 12 bits
    call hexasc                         ; Convert the high nibble to ASCII hex

    pop ax                              ; Restore AX register from stack
    push ax                             ; Save AX register on stack again
    and ax,0f00h                        ; Isolate the low nibble of the high byte of AX
    mov cl,8                            ; Set CL to 8 (for right-shifting 8 bits)
    shr ax,cl                           ; Right-shift AX by 8 bits
    inc si                              ; Increment SI to move to next character position
    call hexasc                         ; Convert the nibble to ASCII hex

    pop ax                              ; Restore AX register from stack
    push ax                             ; Save AX register on stack again
    and ax,0f0h                         ; Isolate the high nibble of the low byte of AX
    mov cl,4                            ; Set CL to 4 (for right-shifting 4 bits)
    shr ax,cl                           ; Right-shift AX by 4 bits
    inc si                              ; Increment SI to move to next character position
    call hexasc                         ; Convert the nibble to ASCII hex

    pop ax                              ; Restore AX register from stack
    push ax                             ; Save AX register on stack again
    and ax,0fh                          ; Isolate the low nibble of the low byte of AX
    inc si                              ; Increment SI to move to next character position

    call hexasc                         ; Convert the nibble to ASCII hex
    pop ax                              ; Restore AX register from stack
    pop cx                              ; Restore CX register from stack
    ret                                 ; Return from procedure
binasc endp

;========================================================
hexasc proc
;========================================================
    push bx                           ; Save BX register on stack
    mov bx, offset tblhex             ; Load the address of hexadecimal character table into BX
    xlat                              ; Translate AL using table at DS:BX (converts nibble in AL to ASCII hex)
    mov [si], al                      ; Store the ASCII character in the string at DS:SI
    pop bx                            ; Restore BX register from stack
    ret                               ; Return from procedure
hexasc endp

END Strt
