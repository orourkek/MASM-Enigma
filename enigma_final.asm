TITLE The Enigma Machine						(ENIGMA FINAL.asm)

; Description: The Enigma Machine - A simulation of the World War I German encryption and decryption machine.
; Auhors: Jacqueline Manzi, Devin Pleuler, Kevin O'Rourke
; Revision date: 12/1/2009

INCLUDE Irvine32.inc
.data

	inputPrompt		 db  "Enter a character <Enter 0 to exit>: ",0
	inputPromptTwo	 db  "Enter RII Inital Condition: ", 0
	inputPromptThree db  "Enter RIII Inital Condition: ", 0
	inputPromptOne   db  "Enter RI Inital Condition: ", 0
	inputFirstPlug   db  "Enter the first character to swap <Enter 0 to end swapping.>:  ", 0
	inputSecondPlug  db	 "Enter the second character to swap <Enter 0 to end swapping.>: ", 0
	currentPlugBoard db  "This is the current plugboard position.",0
	swapError		 db	 "This character has already been swapped.",0
	capitalError     db  "You must enter a capital character, please re-enter a character: ",0
	
	
	rotor1     dd 'E','K','M','F','L','G','D','Q','V','Z','N','T','O','W','Y','H','X','U','S','P','A','I','B','R','C','J',0    
	rotor2     dd 'A','J','D','K','S','I','R','U','X','B','L','H','W','T','M','C','Q','G','Z','N','P','Y','F','V','O','E',0    
	rotor3     dd 'B','D','F','H','J','L','C','P','R','T','X','V','Z','N','Y','E','I','W','G','A','K','M','U','S','Q','O',0
	reflector  dd 'Y','R','U','H','Q','S','L','D','P','X','N','G','O','K','M','I','E','B','F','Z','C','W','V','J','A','T',0
	rotor1back dd 'U','W','Y','G','A','D','F','P','V','Z','B','E','C','K','M','T','H','X','S','L','R','I','N','Q','O','J',0
	rotor2back dd 'A','J','P','C','Z','W','R','L','F','B','D','K','O','T','Y','U','Q','G','E','N','H','X','M','I','V','S',0
	rotor3back dd 'T','A','G','B','P','C','S','D','Q','E','U','F','V','N','Z','H','Y','I','X','J','W','L','R','K','O','M',0
	plugBoard  dd 'A','B','C','D','E','F','G','H','I','J','K','L','M','N','O','P','Q','R','S','T','U','V','W','X','Y','Z',0
   
    rotor1Pos  dd ?
    rotor2Pos  dd ?
    rotor3Pos  dd ?
    doublestep dd 0
    
    

.code
main PROC
    
    ; Read in the initial position for rotor one
    
    mov edx, OFFSET inputPromptOne							; Load prompt into register
	call WriteString										; Display prompt
	call ReadCapital										; Read capital letter with error checking
    mov rotor1Pos, eax										; Save the initial position
    
    ; Read in the initial position for rotor two
    
    mov edx, OFFSET inputPromptTwo
	call WriteString
	call ReadCapital  
    mov rotor2Pos, eax
    
    ; Read in the initial position for rotor three
    
    mov edx, OFFSET inputPromptThree
	call WriteString
	call ReadCapital                                     
    mov rotor3Pos, eax
    
    call SetupPlugboard										; Display the plugboard setup interface
    

	EncryptionLoop:
		
		mov edx, OFFSET inputPrompt							; Load prompt into register
		call WriteString								    ; Display prompt
										
		call ReadCapital										 
		cmp  eax,30h										; Exit if zero
		je exitL


		call crlf

    
		; Plugboard Transformations
    
		mov ecx, 65											; Move the Ascii decimal value of A into ecx to ensure no rotation on the plugboard
		mov ebx, OFFSET plugBoard							; Pass in plugboard as Transformation array
		call Transform
	
    
		; Rotor 3 Transformations
    
		call RotateRotors
    
		mov ecx, rotor3Pos									; Pass in the rotation of rotor 3
		call RotateForward									; Rotates eax forward from the user input
		mov ebx, OFFSET rotor3								; Pass in rotor three as Transformation array
		call Transform                                      ; Applies Transform to eax
		call RotateBackward									; Rotates eax backward from the user input (uses previous rotor three position from eax)
	
		; Rotor 2 Transformations
	
		mov ecx, rotor2Pos									; Pass in the rotation of rotor 2
		call RotateForward                                  ; Rotates eax forward from the user input
		mov ebx, OFFSET rotor2                              ; Pass in rotor two as Transformation array
		call Transform                                      ; Applies Transform to eax
		call RotateBackward                                 ; Rotates eax backward from the user input (uses previous rotor two position from eax)
	                                                  
		; Rotor 1 Transformations
	
		mov ecx, rotor1Pos									; Pass in the rotation of rotor 1
		call RotateForward                                  ; Rotates eax forward from the user input
		mov ebx, OFFSET rotor1                              ; Pass in rotor one as Transformation array
		call Transform                                      ; Applies Transform to eax
		call RotateBackward                                 ; Rotates eax backward from the user input (uses previous rotor one position from eax)
                                                      
		mov ebx, OFFSET reflector							; Pass in reflector as the Transformation array
		call Transform										; Applies Transform to eax
	
		; Rotor 1 Reverse Transformations
	
		mov ecx, rotor1Pos									; Pass in the rotation of rotor 1
		call RotateForward                                  ; Rotates eax forward from the user input
		mov ebx, OFFSET rotor1back                          ; Pass in the inverse of rotor one as Transformation array
		call Transform                                      ; Applies Transform to eax
		call RotateBackward                                 ; Rotates eax backward from the user input (uses previous rotor one position from eax)
	                                                  
		; Rotor 2 Reverse Transformations
		
		mov ecx, rotor2Pos									; Pass in the rotation of rotor 2
		call RotateForward                                  ; Rotates eax forward from the user input
		mov ebx, OFFSET rotor2back                          ; Pass in inverse of rotor two as Transformation array
		call Transform                                      ; Applies Transform to eax
		call RotateBackward                                 ; Rotates eax backward from the user input (uses previous rotor two position from eax)
		                                               
		; Rotor 3 Reverse Transformations
	
		mov ecx, rotor3Pos									; Pass in the rotation of rotor 3
		call RotateForward                                  ; Rotates eax forward from the user input
		mov ebx, OFFSET rotor3back                          ; Pass in the inverse of rotor three as Transformation array
		call Transform                                      ; Applies Transform to eax
		call RotateBackward                                 ; Rotates eax backward from the user input (uses previous rotor three position from eax)
		                                                 
		mov ecx, 65						                    ; Move the Ascii decimal value of A into ecx to ensure no rotation on the plugboard
		mov ebx, OFFSET plugBoard							; Pass in plugboard as Transformation array
		call Transform
	
		call WriteChar

		call crlf
		call crlf
		dec ecx												; Decrement ecx and jump to EncryptionLoop if dec does not raise the 0 flag
	jnz EncryptionLoop										; This prevents over stepping the loop bounds of -128 to 127

	exitL:

	exit
main ENDP

;--------------------------------------------------------
;          TRANSFORM PROCEDURE 				            |
;--------------------------------------------------------
;	Given a rotation array and its initial position, 	|
;	transforms eax.   									|
;-------------------------------------------------------|
;   EAX: Specifies the character to transform and output|
;--------------------------------------------------------
;   EBX: Specifies the initial position of the rotor ,  |
;   set to 65 if it is not a rotor                      |
;--------------------------------------------------------

Transform PROC USES ebx edx                             
	
	mov edx, 4												; Define multiplicant
	mul edx                                                 ; Multiply by four to index into a double word
	add ebx, eax											; Add the index to the address
	sub ebx, 65 * 4									    	; Subtract the value of A(65) from the address of ebx (multipied by 4 since it is a double word) to zero index it

	mov eax, [ebx]											; Get the transformed value

ret
Transform endp

;--------------------------------------------------------
;          ROTOR ROTATION PROCEDURE (Forward)           |
;--------------------------------------------------------
;  Index the character to the rotor's current position. |
;-------------------------------------------------------|
;  EAX: Specifies the character to rotate and output    |
;-------------------------------------------------------|
;  ECX: Specifies the amount to rotate					|
;--------------------------------------------------------

RotateForward PROC USES ecx

	
	add eax, ecx                                        	; Add the amount to rotate to the specified character
	sub eax, 65                                         	; Subtract 65 because the rotation amount is indexed from Ascii A
	cmp eax, 90                                         	; Check if we have rotated past Ascii Z
	jbe SKIPSUB                                         	; If not rotated past Ascii Z, return

	sub eax, 26                                         	; If past Z, rotate back to A

SKIPSUB:	

ret
RotateForward endp

;---------------------------------------------------------
;          ROTOR ROTATION PROCEDURE (Backward)            |
;---------------------------------------------------------|
;  Index the character from thw rotor's current position. |
;---------------------------------------------------------|
;  EAX: Specifies the character to rotate and output      |
;---------------------------------------------------------|
;  ECX: Specifies the amount to rotate					  |
;---------------------------------------------------------

RotateBackward PROC USES ecx

	
	sub eax, ecx										; Subtract the amount to rotate to the specified character
	add eax, 65											; Add 65 because the rotation amount is indexed from Ascii A
	cmp eax, 65										    ; Check if we have rotated past Ascii A
	jae SKIPADD										    ; If not rotated past Ascii A, return
	add eax,26								            ; If past A, rotate back to Z

SKIPADD:   

ret
RotateBackward endp

;--------------------------------------------------------
;          ROTOR ROTATION PROCEDURE                     |
;--------------------------------------------------------
;  Steps the rotors forward.                            |
;--------------------------------------------------------


RotateRotors PROC USES esi

	inc rotor3Pos										; Advance rotor 3
	mov esi, rotor3Pos									; Pass the rotor position to check the rotor
	call CheckRotor										; Sets the rotor position to A if the rotors passes Z
	mov rotor3Pos, esi									; Saves that position
	
	
	cmp rotor3Pos, 'W'									; Checks if rotated from V in rotor 3
	jne EndR											; If not equal to W, do not rotate anything else
	
	cmp rotor2Pos, 'E'									; Check if rotor 2 is in rotate position
	jne R3												; If not equal to E, advance rotor 2 and not rotor 1
		
	inc rotor1Pos										; Advance rotor 1
	mov esi, rotor1Pos									; Pass the rotor position to check the rotor 
	call CheckRotor                                     ; Sets the rotor position to A if the rotors passes Z
	mov rotor1Pos, esi                                  ; Saves that position

	R3:

	inc rotor2Pos                                       ; Advance rotor 2	                                                
	mov esi, rotor2Pos                                  ; Pass the rotor position to check the rotor 
	call CheckRotor                                     ; Sets the rotor position to A if the rotors passes Z
	mov rotor2Pos, esi 									; Saves that position
	
	cmp rotor2Pos, 'E'									; Check if rotor 2 is in rotate position
	jne EndR											; If not equal to E, do not double step
	mov doublestep, 1									; Set double step flag
	
	EndR:

	cmp doublestep, 0									; Test if double step
	jne EndR2											; Return if not double step
	
	cmp rotor2Pos, 'E'									; Check if rotor 2 is in rotate position
	jne EndR2											; If not equal to E, reset double step flag
		
	inc rotor2Pos										; Perform double step for rotor 2
	inc rotor1Pos										; Perform double step for rotor 1
	mov esi, rotor1Pos									; Pass the rotor position to check the rotor 
	call CheckRotor                                     ; Sets the rotor position to A if the rotors passes Z
	mov rotor1Pos, esi                               	; Save that position
	
	EndR2:
	mov doublestep, 0									; Clear double step flag
ret
RotateRotors endp

;--------------------------------------------------------
;         CHECK ROTOR PROCEDURE                         |
;--------------------------------------------------------
;  Rotates rotor back to A if past Z.                   |
;--------------------------------------------------------

CheckRotor PROC 
	cmp esi, 'Z'										; Check if past Z
	jbe finished										; Return if not
	mov esi, 'A'										; Restore to A otherwise
	finished:
	ret
CheckRotor endp

;--------------------------------------------------------
;          PLUGBOARD SETUP PROCEDURE                     |
;--------------------------------------------------------
;  Interface to setup the plugboard array                |
;--------------------------------------------------------


SetupPlugboard PROC USES edx ecx eax

begin:
	
	call DisplayPlugboard								; Display the current plugboard state

	firstInput:
	
	mov edx, OFFSET inputFirstPlug						; Move prompt to register
	call WriteString									; Display prompt
	call ReadCapital									; Perform capital character read and check
	cmp eax, 30h										; Exit if 0
	je exitP											; Return if 0
	mov edx, eax
	sub edx, 65										    ; Subtract A to get the index in the plugboard array
	mov ecx, edx										; Save the value
	cmp eax, plugboard[edx * 4]							; Compare input character to the associated position in the plugboard array
	je secondInput										; Allow the swap if equal
	call crlf
	mov edx, OFFSET swapError							; If not equal, error and re-prompt
	call WriteString
	call crlf
	jmp firstInput
	
	secondInput:
	
	mov edx, OFFSET inputSecondPlug						; Move prompt to register
	call WriteString                                    ; Display prompt
	call ReadCapital                                    ; Perform capital character read and check
	cmp eax, 0                                          ; Exit if 0
	je  exitP                                           ; Return if 0
	mov edx, eax                                        
	sub edx, 65                                         ; Subtract A to get the index in the plugboard array
	mov ebx, edx                                        ; Save the value
	cmp eax, plugboard[edx * 4]                         ; Compare input character to the associated position in the plugboard array
	je Swap                                             ; Allow the swap if equal
	call crlf
	mov edx, OFFSET swapError							; If not equal, error and re-prompt
	call WriteString
    call crlf
	jmp secondInput										; Ask prompt again
	

	Swap:
	
	mov edx, ecx										; Move the first input character into the second input index
	add edx, 65
	mov plugboard[ebx * 4], edx
	
	mov edx, ebx										; Move the second input character into the first input index
	add edx, 65
	mov plugboard[ecx * 4], edx
	jmp begin


exitP:		 

ret
SetupPlugboard endp

;--------------------------------------------------------
;          DISPLAY PLUGBOARD PROCEDURE                   |
;--------------------------------------------------------
;  Procedure to display the plugboard array              |
;--------------------------------------------------------

DisplayPlugboard  PROC USES eax ecx edx

	mov ecx, 26											 ; Number of times to loop    
	call crlf
	mov edx, OFFSET currentPlugboard					 
	call WriteString
	call crlf
    mov edx, 0
	DisplayLoop:
	
		mov eax, plugBoard[edx * 4]						 ; Display all indexs seperated by a space 
		inc edx
		call WriteChar
		mov eax, 20h									 
		call WriteChar
		
loop DisplayLoop

call crlf
call crlf

ret
DisplayPlugboard endp


;--------------------------------------------------------
;          READ CAPITAL PROCEDURE                        |
;--------------------------------------------------------
;  A ReadChar that will re-prompt if a non-capital,      |
;  non-zero character is entered .                       |
;--------------------------------------------------------

ReadCapital PROC USES edx

Check:
	call ReadChar
	call WriteChar
	call crlf
	and  eax, 000000ffh									 ; Removes the garbage in order to get a pure integer value
	cmp  eax, 30h										 ; Allow zero and return
	je   exitC											 
	cmp  eax, 65										 ; Error and re-prompt if less than A
	jb   Error
	
	cmp  eax, 90										 ; Error and re-prompt if greater than Z
	ja   Error
	
	jmp exitC											 ; Allow and return
	
Error:
	mov edx, OFFSET capitalError						 ; Display error prompt
	call WriteString
	jmp Check

	
exitC:

ret
ReadCapital endp

END main

