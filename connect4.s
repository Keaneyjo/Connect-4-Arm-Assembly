;
; CS1022 Introduction to Computing II 2018/2019
; Mid-Term Assignment - Connect 4 - SOLUTION
;
; get, put and puts subroutines provided by jones@scss.tcd.ie
;


PINSEL0	EQU	0xE002C000
U0RBR	EQU	0xE000C000
U0THR	EQU	0xE000C000
U0LCR	EQU	0xE000C00C
U0LSR	EQU	0xE000C014


	AREA	globals, DATA, READWRITE
		

	
	AREA	RESET, CODE, READONLY
	ENTRY

	; initialise SP to top of RAM
 	LDR	R13, =0x40010000	; initialse SP

	; initialise the console
	BL	inithw

	;
	; your program goes here
	;
	
restartGame
	LDR R0, =str_go			;"Let's play Connect4!!"		
	BL puts		
	LDR R0, =str_top_row	; Loads top set of numbers to be used as columns
	BL puts
	LDR R0, =str_newl		; Skips to next line
	BL puts
	

	BL 	initialiseboard		; Routine to initialise array of "0" characters
	LDR R1, =0x40000000		; Loading where board array is stored / R1 is where the board is stored
	BL	printboard			; Routine to print out the board
	MOV R2, #0				; R2 used to show whose turn it is / whether to restart or not
	
continueGame
	BL makeMove				; Routine to make move/ place counter
	CMP R2, #90
	BEQ restartGame			; Branch that resets the game if player choses to do so
	MOV R2, #0
	LDR	R0, =str_newl		; Skip line
	BL	puts
	LDR	R0, =str_newl		; Skip line
	BL	puts
	LDR R0, =str_top_row	; Load top row
	BL 	puts
	LDR	R0, =str_newl		; Skip line
	BL	puts
	BL	printboard			
	BL	checkhorizontal		; Check horizontal sequences for potential win
	BL	checkvertical		; Check vertical sequences for potential win
	BL	checkrightdiagonal	; Check right facing diagonal sequences for potential win
	BL	checkleftdiagonal	; Check left facing diagonal sequences for potential win
	MOV R2, #0				; Resets turn
	B	continueGame		; Continues game
finish
stop	B	stop


;
; your subroutines go here
;

; Interface:
; initialiseboard Subroutine
; 	Initialises the board full of array of "0" ascii codes
; 	No parameters
; 	No return parameters

initialiseboard		
	
	PUSH {R4, R5, R6, R7, lr}
	LDR	R4, =0x40000000
	LDR	R5, =BOARD
	LDR	R6, =0
	
whInit	CMP	R6, #42				; Number of spaces in a connect four 
	BHS	eWhInit					; Stop initialising once reached 42
	LDR 	R7, =0x30			; Ascii code for zero stored
	STRB	R7, [R4, R6]		; Store the value back to RAM so that it can be used
	ADD	R6, R6, #1				; Move to next index
	
	B	whInit					; Repeat
eWhInit
	POP {R4, R5, R6, R7, pc}
	


; Interface:
; printboard Subroutine
; 	Prints the values stored in the board
; 	R1 - Address - Uses LSR and a count to reach the values of the board in memory
	
printboard
	PUSH{R4, R5, R6, R7, R8, R9, R10, lr}
	LDR R4, =0				; Row Shifter
	LDR R6, =0				; Storage for Row and Column Shifter
	LDR R8, =0x40000000		; Starting Address
	LDR R9, =1				; Determines which row value should be outputted
	LDR R10, =7				; Size of Row
forc	CMP R4, #6			
	BEQ endforc

;Row Numbering - Block of code which determines which row numbering to use
	CMP R9, #1
	BNE col2 
	LDR R0, =str_r_1
col2	CMP R9, #2
	BNE col3 
	LDR R0, =str_r_2
col3	CMP R9, #3
	BNE col4 
	LDR R0, =str_r_3
col4	CMP R9, #4
	BNE col5
	LDR R0, =str_r_4
col5	CMP R9, #5
	BNE col6 
	LDR R0, =str_r_5
col6	CMP R9, #6
	BNE colfin 
	LDR R0, =str_r_6
colfin	
	BL  puts
	LDR R0, =str_space
	BL puts
	ADD R9, R9, #1
	
	LDR R5, =0				; Restart Column Shifter


forr		
	CMP R5, #7					; If finished row (i.e Column shifter at max, restart)
	BEQ endforr
	
	MUL R6, R4, R10				; R4 = Row decider		
	ADD R6, R5, R6				; R5 = Column decider
	LDRB R0, [R8, R6] 			; Finds place on board / Stores above found place in R0


	BL put						; Puts place in UART #1
	LDR R0, = str_space 		; Put space
	BL puts
	ADD R5, R5, #1				; Move to next column
	B forr
endforr
	LDR R0, =str_newl			; Skip line
	BL puts
	ADD R4, R4, #1				; Move to next row
	LDR R6, =0					; Empty position
	B forc						; Restart
endforc
	POP {R4, R5, R6, R7, R8, R9, R10, pc}
	
	
	
	
; Interface:
; makemove Subroutine
; 	Users input a value which they input
; 	R10 - Address - Uses LSR and a count to reach the values of the board in memory
; 	R2 - Player Turn Count - Remembers who's turn it is. I.e. if "0", Red moves; if "1" Yellow moves;

makeMove 
	PUSH{R4, R5, R6, R7, R8, R9, R10, R11, lr}
	LDR R10, =0x40000000						
	LDR R11, =7					; Size of row
	CMP R2, #0					; Red's turn first
	BNE yellowturn
	LDR R4, =0x52				; Ascii code for "R"
	MOV R2, #1					; Yellow's turn next
	B redturn
yellowturn	
	LDR R4, =0x59				; Ascii code for "Y"
	MOV R2, #0					; Red's turn next
	B aiturn					; Move to ai code
redturn
	LDR R0, = red_move
	BL puts
	

	BL get			; Find which column is selected
	BL put
	MOV R6, R0		; input stored in R6
	CMP R6, #0x71
	BEQ	quittingTime	; if user enters "q" restart program
	MOV R7, #0
	

	SUB R6, R6, #0x30	; Find which column is selected by removing #0x30
	MUL R8, R7, R11		; Find position [i, _]
	ADD R8, R8, R6		; Find position [i, j]
	LDRB R9, [R10, R8]  ; Load position value
	
	
	; Loop through the whole column
	; if empty space overwrite register with address
	CMP R9, #0x30			;checks if the top row at that certain column is not an empty space, if true cannot make the move
	BNE cannotmakemove
	
	;block of code to check if empty space below in board
continuefinding
	CMP R7, #6					; Temporary number of places to drop
	BEQ finishedplacement		; If finished checking all places, finished loop
	ADD R7, R7, #1				; Move to check next row
	MUL R8, R7, R11				; Getting next row
	ADD R8, R8, R6				; Getting next row
	LDRB R9, [R10, R8]			; Loading row
	CMP	R9, #0x30				; Is that place empty?
	BNE finishedplacement		; If not empty finished placing
	B continuefinding			; If empty continue checking
finishedplacement
	SUB	R7, R7, #1				; Remove last added value
	MUL R8, R7, R11				; Find position [i, _]				
	ADD R8, R8, R6				; Find position [i, j]
	STRB R4, [R10, R8]			; Store user inputted value


aiturn				
	; Block of code which runs through AI's movements
	
	BL airightdiagonal		; Checks if AI can win by right diagonal, if not moves on
	BL aileftdiagonal		; Checks if AI can win by left diagonal, if not moves on
	BL aivertical			; Checks if AI can win by vertical, if not moves on
	BL aihorizontal			; Checks if AI can win by horizontal, if not places random/ or beside user ....
														;... inputted (depends on how many previous moves
	
cannotmakemove
	B skipquit				; Unless user has quit, skip
	
quittingTime
	MOV R2, #90				; Perpare for quit

skipquit

	POP {R4, R5, R6, R7, R8, R9, R10, R11, pc}
	
	
	
	

; Interface:
; checkhorizontal Subroutine
; 	Routine checks if any of the horizontal rows have won
; 	R1 - Address - Uses LSR and a count to reach the values of the board in memory
; 	R2 - Player Turn Count - Remembers who's turn it is. I.e. if "0", Red moves; if "1" Yellow moves;
checkhorizontal
	PUSH{R4-R12, lr}
	LDR R4, =0x40000000

	MOV R5, #0x52			; Use this to compare reds
yellowCheck
	MOV R6, #0				; Counts consecutive similar counters (i.e. if three "R"'s in a row then will be =3) 
	MOV R7, #0				; Row finder
	MOV R8, #7				; Size of row
	MOV R11, R6				; Column Counter 2
	MOV R12, #0
	
checksEachRow
	CMP R6, #7				; Column counter
	BEQ nextrow				; Moves to next column
	
	MUL R9, R7, R8			; Find position [i, _]
	ADD R9, R9, R6			; Find position [i, j]
	LDRB R10, [R4, R9]		; Loads said position
	CMP R10, R5				; Checks if has counter
	BNE nextletter			; If no counter move to next letter
	ADD R12, R12, #1		; If has counter, increase win count
	ADD R6, R6, #1			; and move to next letter
	CMP R12, #4				; If winCount reaches =4 them game won
	BEQ	confirmWinner
	B	checksEachRow

nextletter
	ADD R6, R6, #1			; Moves to next letter
	MOV R12, #0				; Win count restarts
	B	checksEachRow
nextrow
	MOV R6, #0				; Column counter restarted
	ADD R7, R7, #1			; Moves to next row
	CMP R7, #6				; If past max row finish
	BEQ endOfCheckEach
	B	checksEachRow

	; End of      "Routine which checks"
endOfCheckEach
	CMP R5, #0x59			; If player 2 turn over end routine
	BEQ finishedhori
	MOV R5, #0x59			; Else player 2's turn
	B yellowCheck
	
confirmWinner
	CMP R5, #0x52			; If "R" won print red win
	BEQ redwin
	LDR R0, =yellow_winner  ; If "Y" won print yellow win
	BL	puts
	B	stop
	
redwin
	LDR R0, =red_winner
	BL	puts
	B	stop
	
finishedhori				; Close routine
	POP {R4-R12, pc}
	
	
	
	
	
	
	
	
	
	
	
; Interface:
; checkvertical Subroutine
; 	Routine checks if any of the vertical rows have won
; 	R1 - Address - Uses LSR and a count to reach the values of the board in memory
; 	R2 - Player Turn Count - Remembers who's turn it is. I.e. if "0", Red moves; if "1" Yellow moves;
	
checkvertical
	PUSH{R4-R12, lr}
	LDR R4, =0x40000000

	MOV R5, #0x52			; Use this to compare reds
yellowCheck2
	MOV R6, #0				; Counts consecutive similar counters (i.e. if three "R"'s in a row then will be =3) 
	MOV R7, #0				; R7 Row Getter/Skipper
	MOV R8, #7				; R8 Size of Row
	MOV R12, #0
	
checksEachRow2
	CMP R7, #6				; Column counter
	BEQ nextletter2			; Moves to next column

	
	MUL R9, R7, R8		; Find position [i, _]
	ADD R9, R9, R6		; Find position [i, j]
	LDRB R10, [R4, R9]	; Loads said position
	CMP R10, R5			; Checks if has counter
	BNE nextrow2		; If no counter move to next row
	ADD R12, R12, #1	; If has counter, increase win count
	ADD R7, R7, #1		; and move to next row
	CMP R12, #4			; If winCount reaches =4 them game won
	BEQ	confirmWinner2
	B	checksEachRow2

nextletter2
	MOV R7, #0			; Restart rows
	ADD R6, R6, #1		; Move to nex column
	CMP R6, #7			; If column over max then finish routine
	BEQ endOfCheckEach2
	B	checksEachRow2
	
nextrow2
	MOV R12, #0			; Restart counter
	ADD R7, R7, #1		; Move to next row
	B	checksEachRow2

	
endOfCheckEach2
	CMP R5, #0x59		; If player 2 turn over end routine
	BEQ finishedvert
	MOV R5, #0x59		; Else player 2's turn
	B yellowCheck2
	
	
	
confirmWinner2
	CMP R5, #0x52		; If "R" won print red win
	BEQ redwin2
	LDR R0, =yellow_winner	; If "Y" won print yellow win
	BL	puts
	B	stop
redwin2
	LDR R0, =red_winner
	BL	puts
	B	stop
	
finishedvert
	POP {R4-R12, pc}	
	
	
	
; Interface:
; checkrightdiagonal Subroutine
; 	Routine checks if any of the right diagonal rows have won
; 	R1 - Address - Uses LSR and a count to reach the values of the board in memory
; 	R2 - Player Turn Count - Remembers who's turn it is. I.e. if "0", Red moves; if "1" Yellow moves;
checkrightdiagonal
	PUSH{R4-R12, lr}
	LDR R4, =0x40000000
	
	MOV R5, #0x52			; Use this to compare reds
yellowCheck3
	MOV R3, #0
	MOV R6, #0				; Counts consecutive similar counters (i.e. if three "R"'s in a row then will be =3) 
	MOV R7, #0				; Row finder
	MOV R8, #7				; Size of row
	MOV R11, #0
	MOV R12, #0

checksEachRow3
	CMP R6, #7				; Column counter
	BEQ nextrowdown			; Moves to next column
	B 	skipnextrowdown
nextrowdown
	ADD R3, R3, #1
	MOV R7, R3
skipnextrowdown
	CMP R7, #6				; Column counter
	BEQ nextletter3			

	
	MUL R9, R7, R8		; Find position [i, _]
	ADD R9, R9, R6		; Find position [i, j]
	LDRB R10, [R4, R9]	; Loads said position
	CMP R10, R5			; Checks if has counter
	BNE nextrow3		; If no counter move to next row
	ADD R12, R12, #1	; If has counter, increase win count
	ADD R6, R6, #1		; and move to next space
	ADD R7, R7, #1		; ...... and move to next space
	CMP R12, #4			; If winCount reaches =4 them game won
	BEQ	confirmWinner3
	B	checksEachRow3

nextletter3
	MOV R7, R3			; Restart rows
	ADD R11, R11, #1	; Add to max possible diagonals
	MOV R6, R11			; Uses count to move over letter
	CMP R11, #4			; If past max possible diagonals restarts
	BEQ endOfCheckEach3
	B	checksEachRow3
	
nextrow3
	MOV R12, #0			; Restarts count
	ADD R6, R6, #1		; Move to next space
	ADD R7, R7, #1		; ...... and move to next space
	B	checksEachRow3

endOfCheckEach3
	CMP R5, #0x59			; If player 2 turn over end routine
	BEQ finishedrightdia
	MOV R5, #0x59			; Else player 2's turn
	B yellowCheck3
	
confirmWinner3
	CMP R5, #0x52			; If "R" won print red win
	BEQ redwin3
	LDR R0, =yellow_winner	; If "Y" won print red win
	BL	puts
	B	stop
redwin3
	LDR R0, =red_winner
	BL	puts
	B	stop
	
finishedrightdia
	POP {R4-R12, pc}
	
	
	

; Interface:
; checkleftdiagonal Subroutine
; 	Routine checks if any of the left diagonal rows have won
; 	R1 - Address - Uses LSR and a count to reach the values of the board in memory
; 	R2 - Player Turn Count - Remembers who's turn it is. I.e. if "0", Red moves; if "1" Yellow moves;

checkleftdiagonal
	PUSH{R4-R12, lr}
	LDR R4, =0x40000000
	
	MOV R5, #0x52			; Use this to compare reds
yellowCheck4
	MOV R3, #0
	MOV R6, #6				; Counts consecutive similar counters (i.e. if three "R"'s in a row then will be =3) 
	MOV R7, #0				; Row finder
	MOV R8, #7				; Size of row
	MOV R11, #6				; Max possible diagonals
	MOV R12, #0				; WinCount
	
checksEachRow4
	CMP R6, #0xFFFFFFFF		; If top left of diagonal reaches restart
	BEQ nextrowdown4
	B 	skipnextrowdown4
nextrowdown4
	ADD R3, R3, #1
	MOV R7, R3
skipnextrowdown4
	CMP R7, #6				; Column counter
	BEQ nextletter4			

	MUL R9, R7, R8		; Find position [i, _]
	ADD R9, R9, R6		; Find position [i, j]
	LDRB R10, [R4, R9]	; Loads said position
	CMP R10, R5			; Checks if has counter
	BNE nextrow4		; If no counter move to next row
	ADD R12, R12, #1	; If has counter, increase win count
	SUB R6, R6, #1		; and move to next space
	ADD R7, R7, #1		; ...... and move to next space
	CMP R12, #4			; If winCount reaches =4 them game won
	BEQ	confirmWinner4
	B	checksEachRow4

nextletter4	
	MOV R7, R3			; Restart rows
	SUB R11, R11, #1	; Add to max possible diagonals
	MOV R6, R11			; Uses count to move over letter
	CMP R11, #4			; If past max possible diagonals 
	BEQ endOfCheckEach4
	B	checksEachRow4
	
nextrow4
	MOV R12, #0			; Restarts count
	SUB R6, R6, #1		; Move to next space
	ADD R7, R7, #1		; ...... and move to next space
	B	checksEachRow4
	
endOfCheckEach4
	CMP R5, #0x59		; If player 2 turn over end routine
	BEQ finishedleftdia
	MOV R5, #0x59		; Else player 2's turn
	B yellowCheck4
	
	
	
confirmWinner4
	CMP R5, #0x52		; If "R" won print red win
	BEQ redwin4
	LDR R0, =yellow_winner	; If "Y" won print red win
	BL	puts
	B	stop
redwin4
	LDR R0, =red_winner
	BL	puts
	B	stop
	
finishedleftdia
	POP {R4-R12, pc}
	
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;; AI SUBROUTINES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Interface:
; aihorizontal Subroutine
; 	Routine checks if any of the horizontal rows can win for the AI and if so places there
; 	R1 - Address - Uses LSR and a count to reach the values of the board in memory
; 	R2 - Player Turn Count - Remembers who's turn it is. I.e. if "0", Red moves; if "1" Yellow moves;
	
	
aihorizontal
	PUSH{R4-R12, lr}
	LDR R4, =0x40000000

	MOV R5, #0x59			; Use this to compare reds
	MOV R6, #0				; Counts consecutive similar counters (i.e. if three "R"'s in a row then will be =3) 
	MOV R7, #0				; Row finder
	MOV R8, #7				; Size of row
	MOV R11, R6				; Column Counter 2
	MOV R12, #0
	;"Routine which checks"
checksEachRowAI
	CMP R6, #7				; Column counter
	BEQ nextrowAI			; Moves to next column

	
	MUL R9, R7, R8			; Find position [i, _]
	ADD R9, R9, R6			; Find position [i, j]
	LDRB R10, [R4, R9]		; Loads said position
	CMP R10, R5				; Checks if has counter
	BNE nextletterAI		; If no counter move to next letter
	ADD R12, R12, #1		; If has counter, increase win count
	ADD R6, R6, #1			; and move to next letter
	CMP R12, #3				; If winCount reaches =4 them 
	BEQ	confirmWinnerAI
	B	checksEachRowAI

nextletterAI
	ADD R6, R6, #1			; Moves to next letter
	MOV R12, #0				; Win count restarts
	B	checksEachRowAI
nextrowAI
	MOV R6, #0				; Column counter restarted
	ADD R7, R7, #1			; Moves to next row
	CMP R7, #6				; If past max row finish
	BEQ endOfCheckEachAI
	B	checksEachRowAI

endOfCheckEachAI
	LDR R10, =0x40000000
	MOV R6, #36				; Choses AI's first move
	LDRB R9, [R10, R6]		; If empty place
	CMP R9, #0x36			; Choses AI's second move
	BNE nextmoveright		; If empty place
	MOV R9, #1
	MOV R6, R9
	B confirmWinnerAI
	
nextmoveright
	MOV R9, #2				; Prepares placing
	MOV R6, R9				
	LDR R10, =0x40000000
	MOV R6, #30				
	LDRB R9, [R10, R6]
	CMP R9, #0x30			; If not empty move on
	BNE nextmoveright2
	MOV R9, #2
	MOV R6, R9
	B confirmWinnerAI
	
nextmoveright2
	ADD R6, R6, #1
	LDR R10, =0x40000000
	MOV R6, #31
	LDRB R9, [R10, R6]
	CMP R9, #0x30		 	; If not empty move on
	BNE nextmoveright3
	MOV R9, #3
	MOV R6, R9
	B confirmWinnerAI
	
nextmoveright3
	ADD R6, R6, #1
	
	B confirmWinnerAI
	
confirmWinnerAI
	LDR R11, =7				; Size of row
	LDR R4, =0x59			; Prepare to place
	MOV R7, #0				; Place Count = 0

	MUL R8, R7, R11			
	ADD R8, R8, R6			
	LDRB R9, [R10, R8] 		
	
	; Loop through the whole column
	; if empty space overwrite register with address

	LDR R10, =0x40000000
	;block of code to check if empty space below in board
continuefindingAI
	CMP R7, #6					; Temporary number of places to drop
	BEQ finishedplacementAI		; If finished checking all places, finished loop
	ADD R7, R7, #1				; Move to check next row
	MUL R8, R7, R11				; Getting next row
	ADD R8, R8, R6				; Getting next row
	LDRB R9, [R10, R8]			; Loading row
	CMP	R9, #0x30				; Is that place empty?
	BNE finishedplacementAI		; If not empty finished placing
	B continuefindingAI			; If empty continue checking
finishedplacementAI
	SUB	R7, R7, #1				; Remove last add
	MUL R8, R7, R11				; Find position [i, _]
	ADD R8, R8, R6				; Find position [i, j]
	STRB R4, [R10, R8]			; Place counter in position
	
cannotmakemoveAI
finishedhoriAI
	POP {R4-R12, pc}
	
	
	
	
	
;;;;;;;;;;;;;;;;;; AI Vertical ;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Interface:
; aivertical Subroutine
; 	Routine checks if any of the vertical rows can win for the AI and if so places there
; 	R1 - Address - Uses LSR and a count to reach the values of the board in memory
; 	R2 - Player Turn Count - Remembers who's turn it is. I.e. if "0", Red moves; if "1" Yellow moves;
aivertical
	PUSH{R4-R12, lr}
	LDR R4, =0x40000000

	MOV R5, #0x59			; Use this to compare reds
	MOV R6, #0				; Counts consecutive similar counters (i.e. if three "R"'s in a row then will be =3) 
	MOV R7, #0				; Row finder
	MOV R8, #7				; Size of row
	MOV R12, #0				; WinCount
	
checksEachRow2AI
	CMP R7, #6				; Column counter
	BEQ nextletter2AI			

	
	MUL R9, R7, R8		; Find position [i, _]
	ADD R9, R9, R6		; Find position [i, j]
	LDRB R10, [R4, R9]	; Loads said position
	CMP R10, R5			; Checks if has counter
	BNE nextrow2AI		; If no counter move to next letter
	ADD R12, R12, #1	; If has counter, increase win count
	ADD R7, R7, #1		; and move to next letter
	CMP R12, #3			; If winCount reaches =4 them 
	BEQ	confirmWinner2AI
	B	checksEachRow2AI

nextletter2AI			
	MOV R7, #0			; Row counter restarts
	ADD R6, R6, #1		; Moves to next row
	CMP R6, #7
	BEQ endOfCheckEach2AI
	B	checksEachRow2AI
	
nextrow2AI
	MOV R12, #0			; Win count restarts
	ADD R7, R7, #1		; Moves to next row
	B	checksEachRow2AI	; If past max row finish
	
endOfCheckEach2AI
	B finishedvertAI2
	
confirmWinner2AI
	SUB R9, R9, #2				; Adjust placement
	
	LDR R10, =0x40000000
	LDR R11, =7					; Size of row
	LDR R4, =0x59				; Prepare to place
	MOV R7, #0					; Place Count = 0
	
	MUL R8, R7, R11
	ADD R8, R8, R6
	LDRB R9, [R10, R8] 
	
	
	; Loop through the whole column
	; if empty space overwrite register with address
	
	;block of code to check if empty space below in board
	LDR R10, =0x40000000
continuefindingAI2
	CMP R7, #6					; Temporary number of places to drop
	BEQ finishedplacementAI2	; If finished checking all places, finished loop
	ADD R7, R7, #1				; Move to check next row
	MUL R8, R7, R11				; Getting next row
	ADD R8, R8, R6				; Getting next row
	LDRB R9, [R10, R8]			; Loading row
	CMP	R9, #0x30				; Is that place empty?
	BNE finishedplacementAI2		; If not empty finished placing
	B continuefindingAI2			; If empty continue checking
finishedplacementAI2
	SUB	R7, R7, #1				; Remove last add
	MUL R8, R7, R11				; Find position [i, _]
	ADD R8, R8, R6				; Find position [i, j]
	STRB R4, [R10, R8]			; Place counter in position
	
cannotmakemoveAI2
	
finishedvertAI2
	POP {R4-R12, pc}	
	
	
	
;;;;;;;;;;;;;;  AI Right Diagonal   ;;;;;;;;;;;;;;;;;;;;;;;;
; Interface:
; airightdiagonal Subroutine
; 	Routine checks if any of the right diagonal rows can win for the AI and if so places there
; 	R1 - Address - Uses LSR and a count to reach the values of the board in memory
; 	R2 - Player Turn Count - Remembers who's turn it is. I.e. if "0", Red moves; if "1" Yellow moves;
airightdiagonal
	PUSH{R4-R12, lr}
	LDR R4, =0x40000000
	
	MOV R5, #0x59			; Use this to compare reds
yellowCheck3AI
	MOV R3, #0
	MOV R6, #0				; Counts consecutive similar counters (i.e. if three "R"'s in a row then will be =3) 
	MOV R7, #0				; Row finder
	MOV R8, #7				; Size of row
	MOV R11, #0
	MOV R12, #0
	
checksEachRow3AI
	CMP R6, #7
	BEQ nextrowdownAI
	B 	skipnextrowdownAI
nextrowdownAI
	ADD R3, R3, #1
	MOV R7, R3
skipnextrowdownAI
	CMP R7, #6				; Column counter
	BEQ nextletter3AI		

	
	MUL R9, R7, R8		; Find position [i, _]
	ADD R9, R9, R6		; Find position [i, j]
	LDRB R10, [R4, R9]	; Loads said position
	CMP R10, R5			; Checks if has counter
	BNE nextrow3AI		; If no counter move to next letter
	ADD R12, R12, #1	; If has counter, increase win count
	ADD R6, R6, #1		; and move to next letter
	ADD R7, R7, #1		; If winCount reaches =4 them 
	CMP R12, #4
	BEQ	confirmWinner3AI
	B	checksEachRow3AI

nextletter3AI
	MOV R7, R3
	ADD R11, R11, #1	; Move to next letter
	MOV R6, R11			
	CMP R11, #2			; If finished diagonal move to next row
	BEQ endOfCheckEach3AI
	B	checksEachRow3AI
	
nextrow3AI
	MOV R12, #0			; Win counter restarts
	ADD R6, R6, #1		; Prepare the next diagonal
	ADD R7, R7, #1		; Move to the next row
	B	checksEachRow3AI

endOfCheckEach3AI
	B finishedrightdiaAI
	
	
	
confirmWinner3AI
	ADD R6, R6, #1		; Adjust placement
	
	LDR R10, =0x40000000
	LDR R11, =7			; Size of row
	LDR R4, =0x59		; Prepare to place
	MOV R7, #0			; Place Count = 0
	

	MUL R8, R7, R11
	ADD R8, R8, R6
	LDRB R9, [R10, R8] 
	
	
	; Loop through the whole column
	; if empty space overwrite register with address
	
	;block of code to check if empty space below in board
	LDR R10, =0x40000000
continuefindingAI3
	CMP R7, #6					; Temporary number of places to drop
	BEQ finishedplacementAI3	; If finished checking all places, finished loop
	ADD R7, R7, #1				; Move to check next row
	MUL R8, R7, R11				; Getting next row
	ADD R8, R8, R6				; Getting next row
	LDRB R9, [R10, R8]			; Loading row
	CMP	R9, #0x30				; Is that place empty?
	BNE finishedplacementAI3		; If not empty finished placing
	B continuefindingAI3			; If empty continue checking
finishedplacementAI3
	SUB	R7, R7, #1				; Remove last add
	MUL R8, R7, R11				; Find position [i, _]
	ADD R8, R8, R6				; Find position [i, j]
	STRB R4, [R10, R8]			; Place counter in position
	
cannotmakemoveAI3
	
finishedrightdiaAI
	POP {R4-R12, pc}
	
	
	
	
	

	
; Interface:
; aileftdiagonal Subroutine
; 	Routine checks if any of the left diagonal rows can win for the AI and if so places there
; 	R1 - Address - Uses LSR and a count to reach the values of the board in memory
; 	R2 - Player Turn Count - Remembers who's turn it is. I.e. if "0", Red moves; if "1" Yellow moves;	
aileftdiagonal
	PUSH{R4-R12, lr}
	LDR R4, =0x40000000
	
	MOV R5, #0x59			; Use this to compare reds
	MOV R3, #0
	MOV R6, #6				; Counts consecutive similar counters (i.e. if three "R"'s in a row then will be =3) 
	MOV R7, #0				; Row finder
	MOV R8, #7				; Size of row
	MOV R11, #6
	MOV R12, #0
	;"Routine which checks"
	; R6 Adder/ Column getter
	; R7 Row Getter/Skipper
	; R8 Size of Row
	
checksEachRow4AI
	CMP R6, #0xFFFFFFFF
	BEQ nextrowdown4AI
	B 	skipnextrowdown4AI
nextrowdown4AI
	ADD R3, R3, #1
	MOV R7, R3
skipnextrowdown4AI
	CMP R7, #6				; Column counter
	BEQ nextletter4AI		

	
	MUL R9, R7, R8		; Find position [i, _]
	ADD R9, R9, R6		; Find position [i, j]
	LDRB R10, [R4, R9]	; Loads said position
	CMP R10, R5			; Checks if has counter
	BNE nextrow4		; If no counter move to next letter
	ADD R12, R12, #1	; If has counter, increase win count
	SUB R6, R6, #1		; and move to next letter
	ADD R7, R7, #1		; .... and move to next letter
	CMP R12, #2			; If winCount reaches =2 then place
	BEQ	confirmWinner4AI
	B	checksEachRow4AI

nextletter4AI
	MOV R7, R3
	SUB R11, R11, #1	; Move to next letter
	MOV R6, R11
	CMP R11, #2			; If finished diagonal move to next row
	BEQ endOfCheckEach4AI
	B	checksEachRow4AI
	
nextrow4AI
	MOV R12, #0			; Win counter restarts
	SUB R6, R6, #1		; Prepare the next diagonal
	ADD R7, R7, #1		; Move to the next row
	B	checksEachRow4AI

endOfCheckEach4AI
	B	finishedleftdiaAI
	
	
	
confirmWinner4AI
	SUB R6, R6, #2		; Adjust placement
	
	LDR R10, =0x40000000
	LDR R11, =7			; Size of row
	LDR R4, =0x59		; Prepare to place
	MOV R7, #0			; Place Count = 0
	

	MUL R8, R7, R11
	ADD R8, R8, R6
	LDRB R9, [R10, R8] 
	
	
	; Loop through the whole column
	; if empty space overwrite register with address

	
	;block of code to check if empty space below in board
	LDR R10, =0x40000000
continuefindingAI4
	CMP R7, #6					; Temporary number of places to drop
	BEQ finishedplacementAI4	; If finished checking all places, finished loop
	ADD R7, R7, #1				; Move to check next row
	MUL R8, R7, R11				; Getting next row
	ADD R8, R8, R6				; Getting next row
	LDRB R9, [R10, R8]			; Loading row
	CMP	R9, #0x30				; Is that place empty?
	BNE finishedplacementAI4		; If not empty finished placing
	B continuefindingAI4			; If empty continue checking
finishedplacementAI4
	SUB	R7, R7, #1				; Remove last add
	MUL R8, R7, R11				; Find position [i, _]
	ADD R8, R8, R6				; Find position [i, j]
	STRB R4, [R10, R8]			; Place counter in position
	
cannotmakemoveAI4
	
finishedleftdiaAI
	POP {R4-R12, pc}
	
	
	
	
	
	
	
;
; inithw subroutines
; performs hardware initialisation, including console
; parameters:
;	none
; return value:
;	none
;
inithw
	LDR	R0, =PINSEL0		; enable UART0 TxD and RxD signals
	MOV	R1, #0x50
	STRB	R1, [R0]
	LDR	R0, =U0LCR		; 7 data bits + parity
	LDR	R1, =0x02
	STRB	R1, [R0]
	BX	LR

;
; get subroutine
; returns the ASCII code of the next character read on the console
; parameters:
;	none
; return value:
;	R0 - ASCII code of the character read on teh console (byte)
;
get	LDR	R1, =U0LSR		; R1 -> U0LSR (Line Status Register)
get0	LDR	R0, [R1]		; wait until
	ANDS	R0, #0x01		; receiver data
	BEQ	get0			; ready
	LDR	R1, =U0RBR		; R1 -> U0RBR (Receiver Buffer Register)
	LDRB	R0, [R1]		; get received data
	BX	LR			; return

;
; put subroutine
; writes a character to the console
; parameters:
;	R0 - ASCII code of the character to write
; return value:
;	none
;
put	LDR	R1, =U0LSR		; R1 -> U0LSR (Line Status Register)
	LDRB	R1, [R1]		; wait until transmit
	ANDS	R1, R1, #0x20		; holding register
	BEQ	put			; empty
	LDR	R1, =U0THR		; R1 -> U0THR
	STRB	R0, [R1]		; output charcter
put0	LDR	R1, =U0LSR		; R1 -> U0LSR
	LDRB	R1, [R1]		; wait until
	ANDS	R1, R1, #0x40		; transmitter
	BEQ	put0			; empty (data flushed)
	BX	LR			; return

;
; puts subroutine
; writes the sequence of characters in a NULL-terminated string to the console
; parameters:
;	R0 - address of NULL-terminated ASCII string
; return value:
;	R0 - ASCII code of the character read on teh console (byte)
;
puts	STMFD	SP!, {R4, LR} 		; push R4 and LR
	MOV	R4, R0			; copy R0
puts0	LDRB	R0, [R4], #1		; get character + increment R4
	CMP	R0, #0			; 0?
	BEQ	puts1			; return
	BL	put			; put character
	B	puts0			; next character
puts1	LDMFD	SP!, {R4, PC} 		; pop R4 and PC


;
; hint! put the strings used by your program here ...
;

str_go
	DCB	"Let's play Connect4!!",0xA, 0xD, 0xA, 0xD, 0

str_newl
	DCB	0xA, 0xD, 0x0, 0
	
str_top_row
	DCB	"   0 1 2 3 4 5 6", 0
	
str_space
	DCB 	0x20, 0
	
str_r_1	
	DCB	"0 ", 0
	
str_r_2
	DCB	"1 ", 0
	
str_r_3
	DCB	"2 ", 0
	
str_r_4
	DCB	"3 ", 0
	
str_r_5
	DCB	"4 ", 0
	
str_r_6
	DCB	"5 ", 0
	
red_move
	DCB	"RED: choose a column for your next move (0-6, q to restart): ", 0
	
yellow_move
	DCB	"YELLOW: choose a column for your next move (0-6, q to restart): ", 0	
	
yellow_winner
	DCB 0x0a, 0x27, 0x20, 0x20, 0x5f, 0x5f, 0x5f, 0x20, 0x20, 0x5f, 0x20, 0x5f, 0x5f, 0x5f, 0x5f, 0x5f, 0x20, 0x5f, 0x20, 0x20, 0x20, 0x20, 0x20, 0x5f, 0x20, 0x20, 0x20, 0x20, 0x20, 0x5f, 0x5f, 0x5f, 0x5f, 0x20, 0x20, 0x5f, 0x20, 0x20, 0x20, 0x20, 0x20, 0x20, 0x20, 0x20, 0x5f, 0x20, 0x20, 0x20, 0x20, 0x20, 0x20, 0x5f, 0x20, 0x20, 0x5f, 0x20, 0x20, 0x20, 0x20, 0x20, 0x20, 0x5f, 0x5f, 0x5f, 0x5f, 0x20, 0x20, 0x20, 0x20, 0x5f, 0x20, 0x0a, 0x27, 0x20, 0x20, 0x5c, 0x20, 0x20, 0x5c, 0x2f, 0x2f, 0x2f, 0x20, 0x20, 0x5f, 0x5f, 0x2f, 0x2f, 0x20, 0x5c, 0x20, 0x20, 0x20, 0x2f, 0x20, 0x5c, 0x20, 0x20, 0x20, 0x2f, 0x20, 0x20, 0x5f, 0x20, 0x5c, 0x2f, 0x20, 0x5c, 0x20, 0x20, 0x2f, 0x7c, 0x20, 0x20, 0x2f, 0x20, 0x5c, 0x20, 0x20, 0x2f, 0x7c, 0x2f, 0x20, 0x5c, 0x2f, 0x20, 0x5c, 0x20, 0x20, 0x2f, 0x7c, 0x2f, 0x20, 0x5f, 0x5f, 0x5f, 0x5c, 0x20, 0x20, 0x2f, 0x20, 0x5c, 0x0a, 0x27, 0x20, 0x20, 0x20, 0x5c, 0x20, 0x20, 0x2f, 0x20, 0x7c, 0x20, 0x20, 0x5c, 0x20, 0x20, 0x7c, 0x20, 0x7c, 0x20, 0x20, 0x20, 0x7c, 0x20, 0x7c, 0x20, 0x20, 0x20, 0x7c, 0x20, 0x2f, 0x20, 0x5c, 0x7c, 0x7c, 0x20, 0x7c, 0x20, 0x20, 0x7c, 0x7c, 0x20, 0x20, 0x7c, 0x20, 0x7c, 0x20, 0x20, 0x7c, 0x7c, 0x7c, 0x20, 0x7c, 0x7c, 0x20, 0x7c, 0x5c, 0x20, 0x7c, 0x7c, 0x7c, 0x20, 0x20, 0x20, 0x20, 0x5c, 0x20, 0x20, 0x7c, 0x20, 0x7c, 0x0a, 0x27, 0x20, 0x20, 0x20, 0x2f, 0x20, 0x2f, 0x20, 0x20, 0x7c, 0x20, 0x20, 0x2f, 0x5f, 0x20, 0x7c, 0x20, 0x7c, 0x5f, 0x2f, 0x5c, 0x7c, 0x20, 0x7c, 0x5f, 0x2f, 0x5c, 0x7c, 0x20, 0x5c, 0x5f, 0x2f, 0x7c, 0x7c, 0x20, 0x7c, 0x2f, 0x5c, 0x7c, 0x7c, 0x20, 0x20, 0x7c, 0x20, 0x7c, 0x2f, 0x5c, 0x7c, 0x7c, 0x7c, 0x20, 0x7c, 0x7c, 0x20, 0x7c, 0x20, 0x5c, 0x7c, 0x7c, 0x5c, 0x5f, 0x5f, 0x5f, 0x20, 0x7c, 0x20, 0x20, 0x5c, 0x5f, 0x2f, 0x0a, 0x27, 0x20, 0x20, 0x2f, 0x5f, 0x2f, 0x20, 0x20, 0x20, 0x5c, 0x5f, 0x5f, 0x5f, 0x5f, 0x5c, 0x5c, 0x5f, 0x5f, 0x5f, 0x5f, 0x2f, 0x5c, 0x5f, 0x5f, 0x5f, 0x5f, 0x2f, 0x5c, 0x5f, 0x5f, 0x5f, 0x5f, 0x2f, 0x5c, 0x5f, 0x2f, 0x20, 0x20, 0x5c, 0x7c, 0x20, 0x20, 0x5c, 0x5f, 0x2f, 0x20, 0x20, 0x5c, 0x7c, 0x5c, 0x5f, 0x2f, 0x5c, 0x5f, 0x2f, 0x20, 0x20, 0x5c, 0x7c, 0x5c, 0x5f, 0x5f, 0x5f, 0x5f, 0x2f, 0x20, 0x20, 0x28, 0x5f, 0x29, 0x0a, 0x27, 0x20, 0x20, 0x20, 0x20, 0x20, 0x20, 0x20, 0x20, 0x20, 0x20, 0x20, 0x20, 0x20, 0x20, 0x20, 0x20, 0x20, 0x20, 0x20, 0x20, 0x20, 0x20, 0x20, 0x20, 0x20, 0x20, 0x20, 0x20, 0x20, 0x20, 0x20, 0x20, 0x20, 0x20, 0x20, 0x20, 0x20, 0x20, 0x20, 0x20, 0x20, 0x20, 0x20, 0x20, 0x20, 0x20, 0x20, 0x20, 0x20, 0x20, 0x20, 0x20, 0x20, 0x20, 0x20, 0x20, 0x20, 0x20, 0x20, 0x20, 0x20, 0x20, 0x20, 0x20, 0x20, 0x20, 0x20, 0x20, 0x20, 0x0a, 0
	
red_winner
	DCB 0x0a, 0x27, 0x20, 0x20, 0x20, 0x5f, 0x5f, 0x5f, 0x5f, 0x20, 0x20, 0x5f, 0x5f, 0x5f, 0x5f, 0x5f, 0x20, 0x5f, 0x5f, 0x5f, 0x5f, 0x20, 0x20, 0x20, 0x20, 0x5f, 0x20, 0x20, 0x20, 0x20, 0x20, 0x20, 0x5f, 0x20, 0x20, 0x5f, 0x20, 0x20, 0x20, 0x20, 0x20, 0x20, 0x5f, 0x5f, 0x5f, 0x5f, 0x20, 0x20, 0x20, 0x20, 0x5f, 0x20, 0x0a, 0x27, 0x20, 0x20, 0x2f, 0x20, 0x20, 0x5f, 0x5f, 0x5c, 0x2f, 0x20, 0x20, 0x5f, 0x5f, 0x2f, 0x2f, 0x20, 0x20, 0x5f, 0x20, 0x5c, 0x20, 0x20, 0x2f, 0x20, 0x5c, 0x20, 0x20, 0x2f, 0x7c, 0x2f, 0x20, 0x5c, 0x2f, 0x20, 0x5c, 0x20, 0x20, 0x2f, 0x7c, 0x2f, 0x20, 0x5f, 0x5f, 0x5f, 0x5c, 0x20, 0x20, 0x2f, 0x20, 0x5c, 0x0a, 0x27, 0x20, 0x20, 0x7c, 0x20, 0x20, 0x5c, 0x2f, 0x7c, 0x7c, 0x20, 0x20, 0x5c, 0x20, 0x20, 0x7c, 0x20, 0x7c, 0x20, 0x5c, 0x7c, 0x20, 0x20, 0x7c, 0x20, 0x7c, 0x20, 0x20, 0x7c, 0x7c, 0x7c, 0x20, 0x7c, 0x7c, 0x20, 0x7c, 0x5c, 0x20, 0x7c, 0x7c, 0x7c, 0x20, 0x20, 0x20, 0x20, 0x5c, 0x20, 0x20, 0x7c, 0x20, 0x7c, 0x0a, 0x27, 0x20, 0x20, 0x7c, 0x20, 0x20, 0x20, 0x20, 0x2f, 0x7c, 0x20, 0x20, 0x2f, 0x5f, 0x20, 0x7c, 0x20, 0x7c, 0x5f, 0x2f, 0x7c, 0x20, 0x20, 0x7c, 0x20, 0x7c, 0x2f, 0x5c, 0x7c, 0x7c, 0x7c, 0x20, 0x7c, 0x7c, 0x20, 0x7c, 0x20, 0x5c, 0x7c, 0x7c, 0x5c, 0x5f, 0x5f, 0x5f, 0x20, 0x7c, 0x20, 0x20, 0x5c, 0x5f, 0x2f, 0x0a, 0x27, 0x20, 0x20, 0x5c, 0x5f, 0x2f, 0x5c, 0x5f, 0x5c, 0x5c, 0x5f, 0x5f, 0x5f, 0x5f, 0x5c, 0x5c, 0x5f, 0x5f, 0x5f, 0x5f, 0x2f, 0x20, 0x20, 0x5c, 0x5f, 0x2f, 0x20, 0x20, 0x5c, 0x7c, 0x5c, 0x5f, 0x2f, 0x5c, 0x5f, 0x2f, 0x20, 0x20, 0x5c, 0x7c, 0x5c, 0x5f, 0x5f, 0x5f, 0x5f, 0x2f, 0x20, 0x20, 0x28, 0x5f, 0x29, 0x0a, 0x27, 0x20, 0x20, 0x20, 0x20, 0x20, 0x20, 0x20, 0x20, 0x20, 0x20, 0x20, 0x20, 0x20, 0x20, 0x20, 0x20, 0x20, 0x20, 0x20, 0x20, 0x20, 0x20, 0x20, 0x20, 0x20, 0x20, 0x20, 0x20, 0x20, 0x20, 0x20, 0x20, 0x20, 0x20, 0x20, 0x20, 0x20, 0x20, 0x20, 0x20, 0x20, 0x20, 0x20, 0x20, 0x20, 0x20, 0x20, 0x20, 0x20, 0x20, 0x0a, 0


BOARD	DCB	0,0,0,0,0,0,0
	DCB	0,0,0,0,0,0,0
	DCB	0,0,0,0,0,0,0
	DCB	0,0,0,0,0,0,0
	DCB	0,0,0,0,0,0,0
	DCB	0,0,0,0,0,0,0

	END