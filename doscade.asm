; game.asm
; Michael Kim
; Spring 2021
; mkim@hamilton.edu

INCLUDE CS240.inc ;this line may be commented out => I don't believe any of the code requires PROCs from this file

DOS = 21h
TERMINATE=4C00h

.8086

;&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
;PROCEDURE TABLE OF CONTENTS:
;&&&&&&&&&&&&&&&

    ;1.)    PrintHexDigit           O
    ;2.)    PrintWord               O
    ;3.)    SetInterruptVector      O
    ;4.)    InstallHandler          UNUSED
    ;5.)    GetInterruptVector      O
    ;6.)    SaveVector              O
    ;7.)    RestoreVector           O
    ;8.)    WriteInterruptVector    O
    ;9.)    WriteSavedVector        O
    ;10.)   PlaceChar               O
    ;11.)   RowMultiply             O
    ;12.)   SetES                   O
    ;13.)   Stall                   O
;** ;14.)   Tick                    O       ;Handler to replace BIOS tick
    ;15.)   GetCurrentPage          O
    ;16.)   SetDisplayPage          O
    ;17.)   PrintStringPage         O
    ;18.)   FindCharLoc             O
    ;19.)   EraseChar               O
    ;20.)   SetVelocity             O
    ;21.)   DrawMario               O
    ;22.)   ChangeInterrupt         O
    ;23.)   ReinstateBIOS           O
    ;24.)   TestPrint               O     ;(Used for testing to see if code ran)
    ;25.)   CheckMove               UNFINISHED - Need to implement game ending ; ALSO function that is bugged
        ;Deals with GameWin as well
    ;26.)   CheckBelow              O
    ;27.)   BlankPages              O
    ;28.)   JumpHelper              O
    ;29.)   CheckAbove              O
    ;30.)   DrawMap                 O
    ;31.)   GameMainLoop            O
    ;32.)   CheckFallDeath          O
    ;33.)   DrawPipe                O
    ;34.)   PipeHelper              O
    ;35.)   PipeHelper_2            O
    ;36.)   DrawGround              O
    ;37.)   DrawCloud               O
    ;38.)   DrawBlockRow            O
    ;39.)   DrawGap                 O
    ;40.)   HideCursor              UNFINISHED
    ;41.)   DrawBigCloud            O
    ;42.)   DrawStaircase_1         O
    ;43.)   DrawStaircase_2         O
    ;44.)   DrawBigStaircase        O
    ;45.)   CheckEnemyCollision     O
    ;46.)   CheckMoveHelper         O

;ENEMY DATA IS LOCATED BETWEEN 46 AND 47

    ;47.)   DrawEnemy               O
    ;48.)   CheckEnemyMoveHelper    O
    ;49.)   ReverseEnemyVel         O
    ;50.)   UpdateEnemyPos          O

;EnemyTicks VARIABLE LOCATED BETWEEN 50 AND 51

    ;51.)   DrawFrame               O
    ;52.)   DrawStartEnemies        O
    ;53.)   CheckMarioHit           O
    ;54.)   ReverseNextEnemyVel     UNUSED
    ;55.)   ClearKeyBuffer          O
    ;56.)   ChangeTypematicDelay    CAN'T WRITE: NOT SUPPORTED (I don't think)
    ;57.)   ResetTypematicDelay     CAN'T WRITE: NOT SUPPORTED (I don't think)
    ;58.)   DrawFlagpole            O
    ;59.)   PrintString             O
    ;60.)   UpdateScore             O
    ;61.)   WriteScore              O
    ;62.)   WriteScoreWord          O
    ;63.)   ChangeScoreBufferString O
    ;64.)   CheckScoreTimer         O
    ;65.)   WarpKey                 O
    ;66.)   Delay                   O (Didn't end up using)
    ;67.)   SpeakerOn               O
    ;68.)   SpeakerOff              O
    ;69.)   PlayFrequency           O

;&&&&&&&&&&&&&&&
;PROCEDURE TABLE OF CONTENTS:
;&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

;Screen Pages
    ;DOS in normal mode is page 0
    ;can change to page 1 to draw something new or make really quick transformations
    ;change pages, then write everything to a new page


;OR find out where in memory the current page/screen display is located
    ;then I can just access that array


;load B800 (OR B000 for monochrome) into ES, then use ES to reference the
;location on the page
    ;have to save the different ES locations (int vectors, pages, etc.)


;A handler is the function/procedure that an interrupt vector points to


;**** JUMPING MECHANIC ******
    ;Dependent on the variable called "ticks"
        ;If ticks = 0, Mario should be on the ground, not jumping
        ;If 0 < ticks < JUMP_TICKS (9), he is still jumping upwards
        ;If ticks > JUMP_TICKS, he should be falling

        ;need to set/cmp the variable accordingly to the above rules



;TO DO
    ;Make EZ key teleports
    ;***BUG: on page1, jumping and moving left can sometimes move you to page 0
        ;fix this last, PROC #25.) CheckMove
    ;Remove the blinking cursor
    ;Change Repeat Rate for keypresses:
        ;int 16h, function 03h

;EXTRA TO DO (If you have extra time):
    ;Underground through pipe
    ;extra enemy type: spider that sticks to blocks and moves around them



PAGE0 = 0B800h
BIOS_CLK_INTERRUPT = 1Ch

MARIO_START_POS_X = 1d
MARIO_START_POS_Y = 21d

GROUND_BLOCK = 0DBh
GROUND_BLOCK_COLOR = 00001110b

PIPE_CHAR = 0DBh
PIPE_STALK_COLOR = 00000010b
PIPE_TOP_COLOR = 00001010b

CLOUD_CHAR = 3FB0h
CLOUD_ATTRIBUTE = 00111111b

BLANK_CHAR = 0h
BLANK_CHAR_BACKGROUND = 00110000b

FLAGPOLE_COLOR = 00110111b
FLAGPOLE_CHAR = 0DEh

ENEMY_CHAR = 0EAh
ENEMY_COLOR = 00110101b

MARIO_CHAR = 3402h     ;a dark red smiley face   ;3402h gives a light blue background
MARIO_COLOR = 00000100b (04h)   ;00010100b

BRICK_BLOCK = 08h
BRICK_COLOR = 00001100b  ;00001100b

JUMP_TICKS = 6

START_PAGE = 1

START_TIME = 900


.code

    MusicScore LABEL WORD
        X1    WORD 3044, 4560, 3630, 3044, 4560, 3630, 3044, 4832, 4301   ;1 -9
        X2    WORD 2416, 2712, 3044, 4832, 4063, 3044, 4832, 4063, 3044   ;2 -9
        X3    WORD 4063, 3620, 2416, 2712, 2416, 2280, 2416, 2280, 2712   ;3 -9
        X4    WORD 2280, 2416, 2712, 3044, 3225, 3044, 3620, 4301, 4063   ;4 -9
        X5    WORD 3620, 3417, 3620, 3417, 4832, 3620, 4063, 4560, 10    ;REST     ;5 -8
        X6    WORD 3620, 3044, 2712, 2280, 2712, 3044, 3620, 4560         ;6 -8
        X7    WORD 4063, 3620, 4063, 3620, 4063, 5424, 4063, 10    ;REST    ;7 -7 w/o rest
        X8    WORD 4063, 3620, 4063, 3620, 4063, 3620, 2712, 3044         ;8 -8
        X9    WORD 2712, 3044, 3620, 4063, 4560, 3620              ;9 -6 w/o rest
        X10    WORD 3044, 3835, 4832, 5424, 6088, 10      ;REST              ;10 -5 w/o rest
        X11    WORD 3620, 3044, 2712, 2280, 2712, 3044, 3620, 4560         ;11 -8
        X12    WORD 4063, 3620, 4063, 3620, 4063, 5424, 4063, 10   ;REST     ;12 -7 w/o rest
        X13    WORD 4063, 3620, 4063, 3620, 4063, 3620, 2712, 3044         ;13 -8
        X14    WORD 2712, 3044, 2712, 3620, 4560, 4560, 10   ;REST           ;14 -6 w/o rest
        X15    WORD 6450, 6088, 5746, 5424, 4560, 10, 4560, 10   ;REST        ;15 -6 w/o rest
        X16    WORD 4063, 3835, 3620, 3835, 3620, 3835, 3620, 2416, 2873    ;16 -9
        X17    WORD 3417, 4560                                             ;17 -2
        X18    WORD 4063, 3620, 4063, 4560, 4832, 4063, 4560, 4832, 4560    ;18 -9 ;All played very fast
        X19    WORD 10, 4560, 4832, 4560, 4063, 4301, 4063, 2712, 3225   ;REST      ;19 -8
        X20    WORD 3044, 2712, 2416, 2560, 2416, 2416      ;REST               ;20 -6
        X21    WORD 2712, 2873, 2712, 2712, 2873, 3044, 2873, 2873  ;2 ;REST       ;21 -8
        X22    WORD 3044, 3835, 4832, 6088                           ;22 -4 w/o rest

;RESTS: MEASURE - NOTE#
    ;5 - 9
    ;7 - 8
    ;10 - 6
    ;12- 8
    ;14 - 8
    ;15 - 6, 8
    ;19 - 1 (btw notes)

    ;
    LengthScore LABEL WORD
        Y1    WORD 3, 1, 3, 4, 1, 3, 1, 2, 2   ;1 -9
        Y2    WORD 3, 12, 3, 1, 3, 4, 1, 3, 1   ;2 -9
        Y3    WORD 2, 2, 3, 12, 1, 3, 1, 3, 4   ;3 -9
        Y4    WORD 1, 3, 1, 3, 1, 3, 4, 1, 3   ;4 -9
        Y5    WORD 1, 3, 1, 3, 4, 1, 3, 12, 7    ;REST     ;5 -8
        Y6    WORD 6, 4, 3, 9, 3, 1, 3, 1         ;6 -8
        Y7    WORD 3, 1, 3, 1, 4, 3, 12, 0   ;REST     ;7 -7 w/o rest
        Y8    WORD 3, 1, 3, 1, 3, 4, 4, 1         ;8 -8
        Y9    WORD 3, 1, 3, 1, 3, 14    ;9 -6
        Y10    WORD 4, 4, 4, 4, 4, 4  ;REST                 ;10 -5 w/o rest
        Y11    WORD 6, 4, 3, 9, 3, 1, 3, 1         ;11 -8
        Y12    WORD 3, 1, 3, 1, 4, 3, 12, 0   ;REST     ;12 -7 w/o rest
        Y13    WORD 3, 1, 3, 1, 3, 4, 4, 1         ;13 -8
        Y14    WORD 3, 1, 3, 1, 2, 8, 2   ;REST  0         ;14 -6 w/o rest
        Y15    WORD 1, 3, 1, 3, 4, 1, 8, 4   ;2 REST           ;15 -6 w/o rest
        Y16    WORD 4, 1, 3, 1, 3, 1, 3, 5, 4    ;16 -9
        Y17    WORD 4, 4                                            ;17 -2
        Y18    WORD 1, 1, 1, 1, 1, 3, 1, 3, 8    ;18 -9 ;All played very fast
        Y19    WORD 0, 1, 4, 7, 5, 4, 3, 5, 4    ;REST     ;19 -8
        Y20    WORD 4, 4, 3, 1, 3, 1          ;REST         ;20 -6
        Y21    WORD 3, 1, 3, 1, 3, 1, 3, 1    ;2 REST    ;21 -8
        Y22    WORD 3, 1, 3, 1                           ;22 -4 w/o rest
    ;
    ;LengthScore LABEL WORD
        ;Y1    WORD 3, 1, 3, 4, 1, 3, 1, 2, 2   ;1 -9
        ;Y2    WORD 3, 9, 3, 1, 3, 4, 1, 3, 1   ;2 -9
        ;Y3    WORD 2, 2, 3, 8, 1, 3, 1, 3, 4   ;3 -9
        ;Y4    WORD 1, 3, 1, 3, 1, 3, 4, 1, 3   ;4 -9
        ;Y5    WORD 1, 3, 1, 3, 4, 1, 3, 12, 7    ;REST     ;5 -8
        ;Y6    WORD 6, 4, 3, 9, 3, 1, 3, 1         ;6 -8
        ;Y7    WORD 3, 1, 3, 1, 4, 3, 12, 2   ;REST     ;7 -7 w/o rest
        ;Y8    WORD 3, 1, 3, 1, 3, 4, 4, 1         ;8 -8
        ;Y9    WORD 3, 1, 3, 1, 3, 14    ;9 -6
        ;Y10    WORD 4, 4, 4, 4, 4, 4  ;REST                 ;10 -5 w/o rest
        ;Y11    WORD 6, 4, 3, 9, 3, 1, 3, 1         ;11 -8
        ;Y12    WORD 3, 1, 3, 1, 4, 3, 12, 2   ;REST     ;12 -7 w/o rest
        ;Y13    WORD 3, 1, 3, 1, 3, 4, 4, 1         ;13 -8
        ;Y14    WORD 3, 1, 3, 3, 1, 8, 2   ;REST           ;14 -6 w/o rest
        ;Y15    WORD 1, 3, 1, 3, 4, 2, 8, 4   ;2 REST           ;15 -6 w/o rest
        ;Y16    WORD 4, 1, 3, 1, 3, 1, 3, 5, 4    ;16 -9
        ;Y17    WORD 4, 4                                            ;17 -2
        ;Y18    WORD 1, 1, 1, 1, 1, 3, 1, 3, 8    ;18 -9 ;All played very fast
        ;Y19    WORD 1, 1, 4, 7, 5, 4, 3, 5, 4    ;REST     ;19 -8
        ;Y20    WORD 4, 4, 3, 1, 3, 1                   ;20 -6
        ;Y21    WORD 3, 1, 3, 1, 3, 1, 3, 1        ;21 -8
        ;Y22    WORD 3, 1, 3, 1                           ;22 -4 w/o rest

    ticks BYTE 0        ;variable to keep track of the ticks, which are used for
                        ;jumping

    note_ticks WORD 0               ;variable that keeps track of which note is
                                    ;being played

    note_length_ticks WORD 0        ;variable to keep track of the length of
                                    ;each note being played (incremented in Tick
                                    ;PROC)

    note_timer WORD 0               ;variable that gets set with each new note,
                                    ;and has note_length_ticks compared against
                                    ;it to determin of the note should end

    Current_Page WORD 0     ;variable to keep track of the current page

    BIOS_ClockVector LABEL DWORD      ;storage for the original BIOS_ClockVector
    ClockOffset WORD 0
    ClockSegment WORD 0

    Mario_Pos_X BYTE 0     ;column that mario is in
    Mario_Pos_Y BYTE 0     ;row that mario is in

    Mario_Vel_X BYTE 0     ;Mario's x-dir movement for one clock tick
    Mario_Vel_Y BYTE 0     ;Mario's y-dir movement for one clock tick

    GameOver BYTE 0         ;Bool that determines if the game is over
                            ;is changed in the Tick PROC, but is checked
                            ;in a loop in the main PROC

    GameWin BYTE 0          ;Bool that determines if you won the game
                            ;is checked in a loop in the main PROC

    ScoreTimer WORD 0       ;Game timer that ticks as the game goes on
                            ;to keep track of play time
                                ;Every 3 ticks, it decrements by 10 points

;1.)
;*********************************************************************
PrintHexDigit PROC

    ;prints a single hex digit to standard output

    ;DL: value to be written is in the lower 4 bits
        ;DL has 8 bits total, but need to only access the lower 4
    ;Returns nothing, thus should leave everything unchanged

        pushf           ;save register values
        push ax
        push bx
        push cx
        push dx


        mov ax, 0       ;zero out AX register for DL->AL data movement
        mov al, dl      ;moves dl to al for division
        mov bl, 10h      ;moves 16d = 10h as divisor

        and al, 0Fh     ;keeps only the lower 4 bits of AL


;*******
;now, need to cmp to see what type of digit we have
;*******


        mov bl, 0Ah     ;mov Ah into BL for comparison to AL

        cmp al, bl
        ;cmp ah, bl      ;CF=1 if AL < BL, ZF=1 if AL = BL (which we don't care abt)

        jc number       ;if CF is set, hex digit will be a #, so jump to number:
        jmp letter      ;if CF isnt set, hex digit will be a letter

    number:
        ;instructions for if the digit is a number

        add al, 30h

        jmp complete

    letter:
        ;instructions for if the digit is a letter

        add al, 55d     ;A in ASCII is 65, but 10d in hex. Add 55 to get ASCII

    complete:

        mov dl, al          ;moves the ASCII value to be written into DL
        mov ah, 02h               ;sets AH to DOS code for write-character
        int 21h                   ;tells DOS to takeover with given command

        pop dx
        pop cx              ;returns register values
        pop bx
        pop ax
        popf

        ret

PrintHexDigit ENDP
;*************************************************************************
;2.)
;WORKS
;*************************************************************************
PrintWord PROC
    ;prints out the word in the DX register

    ;RECEIVES:
        ;DX = hex value to be printed

        pushf
        push cx
        push dx
        push si

        mov cl, 4       ;sets the rotation number
        rol dx, cl      ;rotates DX so 1st 4 bits are the last 4 bits

;loop for printing out whatever is in dx

        mov si, 0
        jmp off_4
    off_not_4:
        call PrintHexDigit      ;prints low 4 bits of DX
        rol dx, cl              ;rotates DX so that highest 4 bits are now low
        inc si                  ;increments the counter
    off_4:
        cmp si, 4
        jl off_not_4

        pop si
        pop dx
        pop cx          ;return register vals
        popf
        ret

PrintWord ENDP
;*************************************************************************
;3.)
;TEST NEEDED
;*************************************************************************
SetInterruptVector PROC

;assumes DS

    ;RECEIVES:
    ;AL=interrupt number
    ;DS:DX = new interrupt handler

    ;Uses int 21h, function 25h to replace the interrupt at AL with a new
    ;procedure

        pushf
        push ax
        push ds
        push bx

        ;mov bx, cs
        ;mov ds, bx

        mov ah, 25h     ;sets the interrupt vector at AL to the new PROC
        int 21h         ;pointed to by the offset in DX

        pop bx
        pop ds
        pop ax
        popf
        ret

SetInterruptVector ENDP
;*************************************************************************
;4.)
;TEST NEEDED
;*************************************************************************
InstallHandler PROC
    ;installs a particular function in the code segment
    ;RECEIVES:
        ;AL = handler number
        ;CS:DX = new handler

        pushf

        call SetInterruptVector

        popf
        ret

InstallHandler ENDP
;*************************************************************************
;5.)
;TEST NEEDED
;*************************************************************************
GetInterruptVector PROC

    ;given an interrupt in AL, returns the OFFSET of the interrupt in BX
    ;by using int 21h, function 35h

    ;RECEIVES:
        ;AL = interrupt number
    ;RETURNS:
        ;ES:BX = interrupt (location)

        pushf
        push ax

        mov ah, 35h         ;places the offset for the AL interrupt number
        int 21h             ;in BX

        pop ax
        popf
        ret

GetInterruptVector ENDP
;*************************************************************************
;6.)
;TEST NEEDED
;*************************************************************************
SaveVector PROC
    ;saves the memory address (16-bit segment and offset) of the interrupt
    ; handler given in AL to the DWORD location in memory given at DX

    ;RECEIVES:
        ;AL = interrupt handler
        ;DX = offset of DWORD to store the memory address
            ;offset (declared first)
            ;segment (declared second)

    pushf


    call GetInterruptVector     ;BX holds offset of interrupt, ES holds segment

    push dx
    push bx
    pop dx          ;swaps bx and dx
    pop bx


    mov [bx], dx        ;moved offset into memory location received in DX
    mov [bx + 2], es    ;moved segment address into mem loc +2 received in DX

    push dx
    push bx
    pop dx          ;re-swaps bx and dx
    pop bx


    popf
    ret

SaveVector ENDP
;*************************************************************************
;7.)
;WORKS
;*************************************************************************
RestoreVector PROC

;takes the segment and some register, calls

    ;given an interrupt handler number in AL, replaces the handler with
    ;the one whose location is given in DX as a DWORD (offset, segment)

    ;ALSO changes ES

    ;RECEIVES:
        ;AL = interrupt handler number
        ;DX = offset of DWORD containing the memory location of the handler
        ;to be restored
            ;offset
            ;segment

        pushf
        push ax
        push bx
        push ds
        push es

        mov bx, dx              ;DX = OFFSET BIOS_ClockVector
        mov dx, [bx + 2]        ;moves segment for vector into DX
        call SetES              ;sets ES = vector segment
        mov dx, [bx]            ;sets DX to the vector offset into the segment

        mov bx, es          ;int 21h, func 25h needs DS:DX, so we need to set
        mov ds, bx          ;DS to ES temporarily

        mov ah, 25h         ;calls the SetInterruptVector
        int 21h

        pop es
        pop ds
        pop bx
        pop ax
        popf
        ret


RestoreVector ENDP
;*************************************************************************
;8.)
;WORKS
;*************************************************************************
WriteInterruptVector PROC

    ;given the offset of an interrupt vector in BX, writes it's 32-bit memory
    ;location (segment and offset) to the screen
        ;Writes it as: SEGMENT-OFFSET

    ;RECEIVES:
	   ;ES:BX - interrupt vector to write


        pushf
        push ax
        push dx

    ;write the segment
        mov dx, es
        call PrintWord      ;prints out DX, which is the segment (ES)

    ;prints out a dash
        mov ah, 02h
        mov dl, 3Ah
        int 21h

    ;write out the offset
        mov dx, bx
        call PrintWord      ;prints out DX, which is the offset

        pop dx
        pop ax
        popf
        ret

WriteInterruptVector ENDP
;*************************************************************************
;9.)
;TEST NEEDED
;*************************************************************************
WriteSavedVector PROC
    ;given the memory address of a vector in DX, writes it's 32-bit memory
    ;location (segment and offset) to the screen
        ;Writes it as: SEGMENT-OFFSET
            ;offset (declared first)
            ;segment (declared second)

    ;RECEIVES:
        ;DX = offset of DWORD containing the vector memory address

        pushf
        push ax
        push bx
        push dx

    ;prints out the segment
        mov bx, dx
        mov dx, WORD PTR [bx + 2]
        call PrintWord

    ;prints out a dash
        mov ah, 02h
        mov dl, 3Ah
        int 21h

    ;prints out the offset
        mov dx, WORD PTR [bx]
        call PrintWord

        pop dx
        pop bx
        pop ax
        popf
        ret


WriteSavedVector ENDP
;*************************************************************************
;10.)
;WORKS
;*************************************************************************
PlaceChar PROC
;places a character at a particular location on a particular display page

    ;RECEIVES:
        ;AL=char to write
        ;AH=attributes
        ;CH=row
        ;CL=column
        ;DX=display page number

        pushf
        push dx
        push di
        push es         ;save ES since FindCharLoc changes it

        call FindCharLoc    ;ES:DX = loc to write into memory

        mov di, dx          ;DI = offset for where to write the character on page


        mov BYTE PTR es:[di], al    ;places the character on the screen
        mov BYTE PTR es:[di + 1], ah    ;adds the color attributes to the char

        pop es
        pop di
        pop dx
        popf
        ret


PlaceChar ENDP
;*************************************************************************
;11.)
;WORKS
;*************************************************************************
RowMultiply PROC
;receives a row number and calculates the number bytes that come before the
;start of that row
    ;does so by multiplying by 64, then by 16 using bit shifting

    ;RECEIVES:
        ;CX = row
    ;RETURNS:
        ;CX = number of bytes

        pushf
        push ax
        push bx

        mov bx, cx  ;moves the row into CX and AX for manipulation
        mov ax, cx  ;AX = BX = row

    ;first, multiply by 64

        mov cx, 0
        mov cl, 6   ;shift left by 6 to multiply by 64
        shl ax, cl  ;multiply row by 64


    ;next, multiply by 16

        mov cl, 4   ;shl by 4 to multiply by 16
        shl bx, cl  ;multiply row by 16

    ;finally, mov AX into CX, then add BX to CX

        mov cx, ax
        add cx, bx      ;CX = row * 80


        pop bx
        pop ax
        popf
        ret


RowMultiply ENDP
;*************************************************************************
;12.)
;WORKS
;*************************************************************************
SetES PROC
;sets the extra segment register to whatever is in DX

    ;RECEIVES:
        ;DX = memory segment location to put into ES

        pushf

        mov es, dx      ;sets ES to whatever is in DX

        popf
        ret


SetES ENDP
;*************************************************************************
;13.)

;*************************************************************************
Stall PROC
;stalls for a fair amount of time

        pushf
        push cx

        mov cx, 0FFFFh

    waiting1:
        push cx
        mov cx, 60h
    waiting2:                       ;nested loop for time killing
        loop waiting2
        pop cx
        loop waiting1

        pop cx
        popf
        ret

Stall ENDP
;*************************************************************************
;14.)
;TEST
;*************************************************************************
Tick PROC
;Interrupt handler to replace the BIOS clock interrupt

        inc note_length_ticks
        sti

        call DrawFrame      ;PROC that contains all the checking and drawing
                            ;for movement


        iret

Tick ENDP
;*************************************************************************
;15.)
;WORKS
;*************************************************************************
GetCurrentPage PROC
;Gets the current DOS display page, returns it in BH

    ;RECEIVES:
        ;Nothing (clear BH)
    ;RETURNS:
        ;BH = Current Display Page Number

        pushf
        push ax

        mov ah, 0Fh
        int 10h         ;AL = Display mode
                        ;AH = # character columns
                        ;BH = active page

        pop ax          ;but we simply get rid of the AH,AL values since
        popf            ;we don't care about them
        ret

GetCurrentPage ENDP
;*************************************************************************
;16.)
;WORKS
    ;Max of 8 pages: 0-7
    ;Each Begins at B800:n000, where n is the page number
;*************************************************************************
SetDisplayPage PROC
;changes the current DOS display page
    ;Also updates the variable Current_Page

    ;RECEIVES:
        ;AL= # of display page to change to
    ;RETURNS:
        ;Current_Page = current DOS display page

        pushf
        push ax
        push dx

        mov dx, 0
        mov dl, al               ;DX = page to move to
        mov Current_Page, dx     ;Current_Page = page to move to

        mov ah, 05h
        int 10h         ;sets the display page to whatever number AL has

        ;call HideCursor     ;and hides the cursor from sight

        pop dx
        pop ax
        popf
        ret

SetDisplayPage ENDP
;*************************************************************************
;17.)
;WORKS
;*************************************************************************
PrintStringPage PROC
;Prints a string of repeated characters to a particular display screen

    ;RECEIVES:
        ;AL = character to print
        ;AH = attributes of character
        ;BX = # of repeats
        ;CH = starting row
        ;CL = starting col
        ;DX = display page number

        pushf
        push dx
        push es
        push di
        push si

        call FindCharLoc        ;ES:DX = starting point

;printing loop

        mov si, 0               ;zeroes si
        mov di, dx              ;sets DI to the initial offset
        jmp check_print
    keep_print:
        mov BYTE PTR es:[di], al        ;places the character on the screen
        mov BYTE PTR es:[di + 1], ah    ;adds the color attributes to the char
        add di, 2                       ;moves DI to next char loc
        inc si                          ;increments loop counter
    check_print:
        cmp si, bx
        jl keep_print                   ;continues loop if it did not print out
                                        ;enough repeats yet

        pop si
        pop di
        pop es
        pop dx
        popf
        ret

PrintStringPage ENDP
;*************************************************************************
;18.)
;WORKS
    ;* CHANGES ES
;*************************************************************************
FindCharLoc PROC

;Given a row, col, and page number, returns the address ES:DX of where to
;print a character

    ;RECEIVES:
        ;CH=row
        ;CL=col
        ;DX=display page number
    ;RETURNS:
        ;ES:DX = Offset of where to write a character

        pushf
        push ax
        push bx
        push cx

        mov bx, PAGE0
        mov es, bx          ;ES = B800h

        mov bx, cx          ;BH = row, BL = col
        mov cx, 0           ;zeroes cx...
        mov cl, bh          ;so that now CX just has the row
        call RowMultiply    ;CX = row * 80

        mov ax, 0           ;zeroes AX...
        mov al, bl          ;so that AX has the column

        add cx, ax          ;CX = (row * 80) + col

        shl cx, 1           ;CX = [(row * 80) + col] * 2
                            ;so CX has the offset to where to write the char

        mov ax, cx          ;now AX has offset
        mov cx, 0
        mov cl, 12
        shl dx, cl          ;DX = n000

        add dx, ax          ;DX = page offset + page location offset

        pop cx
        pop bx
        pop ax
        popf
        ret

FindCharLoc ENDP
;*************************************************************************
;19.)
;WORKS
;*************************************************************************
EraseChar PROC
;erases the character and attribute at a specific location and page

    ;RECEIVES:
        ;CH=row
        ;CL=col
        ;DX=display page number

        pushf
        push ax


        mov ah, BLANK_CHAR_BACKGROUND
        mov al, BLANK_CHAR
        call PlaceChar      ;puts a nothing at the specified location

        pop ax
        popf
        ret

EraseChar ENDP
;*************************************************************************
;20.)
;WORKS
;*************************************************************************
SetVelocity PROC
;gets a keystroke from user keyboard input for Mario movement, then sets
;his velocity accordingly

    ;RETURNS:
        ;AH= BIOS Scan code
        ;AL= ASCII character
        ;ZF = 1 if no keystroke
        ;ZF = 0 if there was a keystoke

        pushf
        push ax

;check for key press here

        mov ah, 01h
        int 16h         ;checks keypress, but doesn't remove from buffer
                        ;ZF=1 if no keypress, ZF=0 if keypress

        jz no_key       ;jump to end if there was no key press

;Now, check to see which key was pressed


        mov ah, 00h
        int 16h         ;since there was a keypress, we will take it out of
                        ;the buffer
                        ;AH= BIOS Scan code, AL= ASCII character

        call ClearKeyBuffer         ;removes keys from the buffer



    ;esc for quitting the game

        cmp ah, 01h
        je quit_game

;
;************
;PRESENTATION KEYS FOR WARPING AROUND THE MAP
;************

        cmp ah, 08h
        jle presentation_warp


    ;left arrow first:

        cmp ah, 4Bh
        je left_velocity

    ;then right arrow

        cmp ah, 4Dh
        je right_velocity

    ;then up arrow (special case for jump)

        cmp ah, 48h
        je up_velocity

        jmp no_key          ;if keypress was none of these, assume no key was
                            ;pressed


;This section contains velocity changes

    ;left
    left_velocity:
        mov Mario_Vel_X, 2      ;left is 2
        jmp finish_key_press

    ;right
    right_velocity:
        mov Mario_Vel_X, 1     ;right is 1
        jmp finish_key_press

    ;up (pressed while not yet jumping) ******************* IMPORTANT FOR JUMPING
    up_velocity:

        cmp ticks, 0            ;if ticks > 0, Mario is jumping
        jg currently_jumping    ;if jumping, don't mess with the velocity

                                ;otherwise, set his velocity to go up (up = 2)
        mov ticks, 1            ;and set ticks to 1 (which means he is jumping)

        mov Mario_Vel_Y, 2      ;up is 2, down is 1
        jmp finish_key_press


    no_key:             ;jump here if there was no key press

        cmp ticks, 0
        jg currently_jumping    ;if jumping, don't touch y-vel, let CheckBelow
                                ;handle it

        mov Mario_Vel_X, 0          ;sets his velocity to zero
        mov Mario_Vel_Y, 0
        jmp finish_key_press

    currently_jumping:
                                                                ;********************************
        mov Mario_Vel_X, 0          ;sets his X-velocity to zero, *********************************
                                    ;since PRGM only jumps to this section if
                                    ;up-arrow was pressed
                                            ;either change the velocity during jumps in here
                                            ;or in a different PROC

    ;****   ;if there is no key press, set the direction to 0 or down to have
            ;him fall


    presentation_warp:

        call WarpKey

        pop ax
        popf
        ret

    finish_key_press:

        pop ax
        popf
        ret

    quit_game:              ;if esc was pressed, quits the game

        mov GameOver, 1

        pop ax
        popf
        ret


SetVelocity ENDP
;*************************************************************************
;21.)
;RUDIMENTARY works - TEST NEEDED for switching pages
    ;Also, will need to be able to switch screens at the boundaries
;*************************************************************************
DrawMario PROC
;Procedure that removes and re-draws Mario on the screen based on his movement

        pushf
        push ax
        push bx
        push cx
        push dx


;Puts the Mario character into AX

        mov ax, MARIO_CHAR      ;AH=attributes, AL=char

;first, need to erase current Mario

        mov cl, Mario_Pos_X     ;sets row and col as Mario's current position
        mov ch, Mario_Pos_Y
        mov dx, Current_Page    ;and gives the current page as well
        call EraseChar          ;then erases the current Mario

;next, need to update his position (using BL as intermediary)

        mov bl, Mario_Pos_X     ;BL = Mario col number

        cmp Mario_Vel_X, 2     ;if vel = 2, then direction is left
        je left_movement

        ;cmp Mario_Vel_X, 0
        ;je vertical_movement    ;if not moving L/R, don't want to accidentally
                                ;change screens


    right_movement:

        cmp bl, 79d         ;IF THIS SETS ZF=1, WILL NEED TO MOVE TO THE RIGHT PAGE
        jne not_right_edge      ;if not at edge, move normally

        cmp Mario_Vel_X, 0
        je vertical_movement    ;if not moving L/R, don't want to accidentally
                                ;change screens

        mov dx, Current_Page
        inc dx
        push ax
        mov ax, dx
        call SetDisplayPage     ;Current_Page = new page, Display changed as well
        pop ax
        mov bl, 0     ;and sets Mario's position to the left side
                                ;of the screen
        jmp done_left_movement      ;jump to update the variable with his new position


    not_right_edge:

        add bl, Mario_Vel_X    ;if not left, then either right or 0, so just add
        jmp done_left_movement      ;jump to update the variable with his new position

    left_movement:
        cmp bl, 0            ;;IF THIS SETS ZF=1, WILL NEED TO MOVE TO THE LEFT PAGE
        jne not_left_edge


        mov dx, Current_Page
        dec dx
        push ax
        mov ax, dx
        call SetDisplayPage     ;Current_Page = new page, Display changed as well
        pop ax
        mov bl, 79d             ;and sets Mario's position to the right side
                                ;of the screen
        jmp done_left_movement          ;jump to update the variable with his new position

    not_left_edge:

        dec bl                  ;decrements his X position by 1

    done_left_movement:

        mov Mario_Pos_X, bl     ;updates Mario's col position

    ;vertical movement
    vertical_movement:

        mov bl, Mario_Pos_Y     ;BL = Mario row number

        cmp Mario_Vel_Y, 2     ;if vel = 2, then direction is up
        je up_movement

        add bl, Mario_Vel_Y    ;if not up, then either down or 0, so just add
        jmp done_up_movement

    up_movement:

        cmp bl, 0         ;LATER, WILL CHANGE THIS TO A PROC, CheckMove
        je done_up_movement

        dec bl                  ;decrements his Y position by 1

    done_up_movement:
        mov Mario_Pos_Y, bl     ;updates Mario's row position

        ;push cx
        ;push dx
        ;mov cx, 1
        ;mov dx, OFFSET Mario_Pos_Y         ;based on this test, for some
        ;call DumpMem                       ;reason sets cl from 15 to 2
        ;pop dx
        ;pop cx

    ;print out Mario_Pos_Y here

;finally, need to draw Mario to the screen

        mov cl, Mario_Pos_X     ;sets row and col as Mario's new position
        mov ch, Mario_Pos_Y
        mov dx, Current_Page    ;and gives the current page as well
        call PlaceChar          ;Then, places him at his new spot on the screen

    no_movement:


        call CheckFallDeath


        pop dx
        pop cx
        pop bx
        pop ax
        popf
        ret


DrawMario ENDP
;*************************************************************************
;22.)
    ;WORKS
;*************************************************************************
ChangeInterrupt PROC
;changes the BIOS Clock Interrupt to Tick handler


        pushf
        push ax
        push dx

;save the BIOS_ClockVector memory location
        mov al, BIOS_CLK_INTERRUPT
        mov dx, OFFSET BIOS_ClockVector
        call SaveVector


;sets the new interrupt handler for the BIOS clock
        mov al, BIOS_CLK_INTERRUPT
        mov dx, Tick
        call SetInterruptVector

        pop dx
        pop ax
        popf
        ret

ChangeInterrupt ENDP
;*************************************************************************
;23.)
    ;WORKS
;*************************************************************************
ReinstateBIOS PROC
;reinstates the BIOS_ClockVector

        pushf
        push ax
        push dx

        mov al, BIOS_CLK_INTERRUPT
        ;mov dx, ClockSegment
        call SetES
        mov dx, OFFSET BIOS_ClockVector
        call RestoreVector              ;should this change ES? NO

        pop dx
        pop ax
        popf
        ret

ReinstateBIOS ENDP
;*************************************************************************
;24.)
    ;WORKS
;*************************************************************************
TestPrint PROC
;Procedure to test if code is being run by printing an A

        pushf
        push dx

        mov dx, 0Ah
        call PrintHexDigit

        pop dx
        popf
        ret

TestPrint ENDP
;*************************************************************************
;25.)
    ;BUGS
;*************************************************************************
CheckMove PROC
;based on the velocity of Mario, checks to see if the move is possible

        pushf
        push cx
        push dx
        push es

        mov ch, Mario_Pos_Y     ;CH = Mairo's current row
        mov cl, Mario_Pos_X     ;CL = Mario's current col

        call CheckMoveHelper    ;CH = char at the intended move spot
                                ;changes ES

        cmp ch, FLAGPOLE_CHAR       ;if he moved into the flagpole, make
        je game_win                 ;him win the game

        cmp ch, BLANK_CHAR       ;if not empty
        jne not_empty
        jmp CheckMove_end           ;if empty, make sure it isn't the 1st
                                        ;page
    not_empty:

;first check for Right Boundary
        cmp Mario_Vel_X, 1
        jne left_boundary_check   ;if not moving right, check the left boundary
        cmp Mario_Pos_X, 79d      ;if moving right, if he is at the boundary,
        jne no_move                ;keep his X_vel to the right
        mov Mario_Vel_X, 1
        jmp CheckMove_end

    left_boundary_check:        ;BUG IS HERE FOR MOVING TO PAGE 0

        cmp Mario_Vel_X, 2
        jne CheckMove_end           ;if not moving left eiher, end PROC
        cmp Mario_Pos_X, 0d         ;if moving right, if he is at the boundary,
        jne no_move                    ;keep his X_vel to the left
        cmp Current_Page, 1
        je no_move                  ;if he is at the first page, don't move him
        mov Mario_Vel_X, 2          ;to the left of that page
        jmp CheckMove_end

    no_move:
        mov Mario_Vel_X, 0
        mov Mario_Vel_Y, 0
        jmp CheckMove_end

    game_win:
        mov GameWin, 1

    CheckMove_end:

        pop es
        pop dx
        pop cx
        popf
        ret


CheckMove ENDP
;*************************************************************************
;26.)

;Deals with jumping mechanic
;*************************************************************************
CheckBelow PROC
;checks to see if there is ground below Mario's current position. If there is,
;sets his Y_velocity equal to zero
    ;All blank chars should be 00,00 thanks to BlankPages PROC

        pushf
        push bx
        push cx
        push dx
        push es
        push di

        mov bh, Mario_Pos_Y     ;BH = mario row
        mov bl, Mario_Pos_X     ;BL = mario col

        mov ch, bh              ;
        inc ch                  ;CH = block below Mario
        mov cl, bl              ;CL = Mario's Col
        mov dx, Current_Page    ;DX = current display page #

        call FindCharLoc        ;ES:DX = Offset of block below Mario
        mov di, dx              ;DI = Offset of block
        mov bh, es:[di]         ;BH = char below Mario
        cmp bh, BLANK_CHAR
        je empty_space_or_jumping

;keeps moving if there is something beneath

        cmp ticks, 0                ;if jumping, ticks won't be zero
        jg empty_space_or_jumping   ;if jumping, but there is a block below
                                    ;(i.e. start of jump), we still want to
                                    ;increment ticks

        jmp CheckBelow_end          ;if not jumping, exit the PROC

    empty_space_or_jumping:         ;JUMP_TICKS = 9

        cmp ticks, JUMP_TICKS        ;if jumping up, ticks < JUMP_TICKS
        jle zero_ticks_check         ;in which case, next check to see if 0
        jmp falling                  ; if greater, than Mario is falling
    zero_ticks_check:
        cmp ticks, 0        ;if ticks is zero with no block beneath, he walked
        je falling          ;off the edge, so have him fall
                            ;otherwise, if ticks was less than JUMP_TICKS but
                            ;not zero, he is jumping up
    jmp_up:
        inc ticks
        call JumpHelper     ;if jumping up, increment ticks and call Helper
        jmp not_falling
    falling:                    ;otherwise, he should be falling
        cmp bh, BLANK_CHAR       ;if falling there is not a block below
        jne block_below_y        ;jump...
        mov Mario_Vel_Y, 1  ;otherwise, set his Y_vel to have him move down

        mov ticks, JUMP_TICKS   ;this is for if he moved off the edge of a
        inc ticks               ;platform -> he moves down and can't jump again

        jmp CheckBelow_end
    block_below_y:          ;NOT AIR BLOCK BELOW MARIO
        ;call TestPrint
        mov ticks, 0
        mov Mario_Vel_Y, 0      ;set ticks to zero if there is something below
                                ;and set his velocity to zero as well
    not_falling:

    CheckBelow_end:

        pop di
        pop es
        pop dx
        pop cx
        pop bx
        popf
        ret

CheckBelow ENDP
;*************************************************************************
;27.)
;WORKS
;*************************************************************************
BlankPages PROC
;blanks all bytes for pages 1-7

;PrintStringPage PROC
;Prints a string of repeated characters to a particular display screen

    ;RECEIVES:
        ;AL = character to print
        ;AH = attributes of character
        ;BX = # of repeats
        ;CH = starting row
        ;CL = starting col
        ;DX = display page number


        pushf
        push ax
        push bx
        push cx
        push dx

        mov ax, 0       ;Char = 00 with no attributes
        mov al, 0
        mov ah, 00110111b
        mov cx, 0       ;starts in the upper left
        mov bx, 2000d   ;80*25 = 2000, so 2000 blanks to print
        mov dx, 0       ;zeroes DX

    BlankPage_L0:

        inc dx                  ;starts on page 1
        call PrintStringPage    ;blanks the page in DX

        cmp dx, 7               ;if dx is not 7, repeats loop
        jne BlankPage_L0



        pop dx
        pop cx
        pop bx
        pop ax
        popf
        ret

BlankPages ENDP
;*************************************************************************
;28.)
    ;WORKS
;*************************************************************************
JumpHelper PROC
;PROC to deal with setting his Y_velocity during a jump

    ;RECEIVES:
        ;BH = block below Mario

        pushf
        push bx

        ;push dx
        ;mov dl, ticks          ;test code to print out the ticks
        ;call WriteHexByte
        ;pop dx


        cmp ticks, JUMP_TICKS        ;if ticks = 9, don't touch his Y-vel
        jl moving_upwards

        cmp bh, BLANK_CHAR      ;if not moving up and there is a block below
        jne block_below         ;jump...
        mov Mario_Vel_Y, 1  ;once ticks reaches JUMP_TICKS, have him moving down
        jmp moving_upwards
    block_below:
        mov Mario_Vel_Y, 0      ;if not moving up and there is a block below
        mov ticks, 0           ;set ticks to 0, as well as his Y-vel
        jmp JumpHelper_end
    moving_upwards:             ;if moving up still, keep his Y_vel at 2
        mov Mario_Vel_Y, 2
    JumpHelper_end:

        pop bx
        popf
        ret


JumpHelper ENDP
;*************************************************************************
;29.)
    ;WORKS
;*************************************************************************
CheckAbove PROC
;checks to see if up movement is possible. If not, sets Jumping_Up = 0

        pushf
        push bx
        push cx
        push dx
        push es
        push di


        mov bh, Mario_Pos_Y     ;BH = mario row
        mov bl, Mario_Pos_X     ;BL = mario col

        mov ch, bh              ;
        dec ch                  ;CH = block above Mario
        mov cl, bl              ;CL = Mario's Col
        mov dx, Current_Page    ;DX = current display page #

        call FindCharLoc        ;ES:DX = Offset of block above Mario
        mov di, dx              ;DI = Offset of block
        mov bh, es:[di]         ;BH = char below Mario
        cmp bh, BLANK_CHAR
        je CheckAbove_end       ;if space above, leave alone

        mov ticks, JUMP_TICKS
        inc ticks               ;otherwise, have him fall by setting ticks to 10

    CheckAbove_end:

        pop di
        pop es
        pop dx
        pop cx
        pop bx
        popf
        ret

CheckAbove ENDP
;*************************************************************************
;30.)
    ;UNFINISHED
;*************************************************************************
DrawMap PROC
;Draws the entire map for the game on each page

        pushf


;drawing the ground blocks for all the pages

        mov dx, 0
    drawing_ground_loop:
        inc dx
        call DrawGround
        cmp dx, 7d
        jne drawing_ground_loop



;drawing the start page (page1 - simple stuff)

        mov dx, 1

    ;brick blocks

        mov bx, 20d
        mov ch, 18d
        mov cl, 3d
        call DrawBlockRow

        ;mov al, 0EAh
        ;mov ah, 00110101b
        ;mov ch, 13
        ;mov cl, 40
        ;mov bx, 1
        ;call PrintStringPage

        mov bx, 5d
        mov ch, 16d
        mov cl, 25d
        call DrawBlockRow

        mov bx, 5d
        mov ch, 17d
        mov cl, 35d
        call DrawBlockRow

    ;pipes

        mov bx, 4d
        mov ch, 21d
        mov cl, 60d
        call DrawPipe

        mov bx, 7d
        mov cl, 30d
        call DrawPipe

    ;clouds

        mov ch, 3
        mov cl, 10
        call DrawCloud

        mov ch, 5
        mov cl, 65
        call DrawBigCloud

;second page (pipes and enemies)

        mov dx, 2

    ;pipes

        ;mov ch, 21d
        ;mov cl, 42d
        ;mov bx, 1
        ;call DrawBlockRow

        mov bx, 3d
        mov ch, 21d
        mov cl, 15d
        call DrawPipe

        mov cl, 30d
        mov bx, 5d
        call DrawPipe

        mov cl, 65d
        mov bx, 4d
        call DrawPipe

    ;clouds

        mov cl, 35
        mov ch, 4
        call DrawBigCloud

;third page (bricks and holes)

        mov dx, 3

    ;gaps

        mov bx, 2       ;Any gap more than 2 is hard to get over
        mov cl, 13d
        call DrawGap

        mov bx, 5
        mov cl, 55d
        call DrawGap

    ;clouds

        mov ch, 7
        mov cl, 8
        call DrawCloud

        mov ch, 3
        mov cl, 70
        call DrawCloud

        mov ch, 2
        mov cl, 34
        call DrawCloud

    ;bricks

        mov bx, 8
        mov cl, 46
        mov ch, 14
        call DrawBlockRow

        mov cl, 56
        call DrawBlockRow

        mov bx, 15
        mov cl, 33
        mov ch, 18
        call DrawBlockRow

    ;pipes

        mov cl, 35
        mov ch, 21
        mov bx, 5
        call DrawPipe

        mov cl, 18
        mov bx, 2
        call DrawPipe

;4th page (lots of enemies)

        mov dx, 4

    ;clouds

        mov ch, 3
        mov cl, 4
        call DrawBigCloud

        mov ch, 6
        mov cl, 65
        call DrawBigCloud

    ;pipes

        mov cl, 8
        mov ch, 21
        mov bx, 3
        call DrawPipe

        mov cl, 71
        mov bx, 8
        call DrawPipe

    ;bricks

        mov cl, 66
        mov ch, 16
        mov bx, 10
        call DrawBlockRow

        mov bx, 2
        mov cl, 64
        mov ch, 18
        call DrawBlockRow

        mov cl, 77
        mov bx, 1
        call DrawBlockRow

        mov ch, 17
        mov cl, 40
        call DrawBlockRow

        mov cl, 33
        call DrawBlockRow

        mov cl, 47
        call DrawBlockRow

        mov ch, 14
        mov cl, 40
        call DrawBlockRow

;Page 5 (stair looking blocks)

        mov dx, 5

    ;clouds

        mov ch, 3
        mov cl, 4
        call DrawCloud

        mov ch, 5
        mov cl, 39
        call DrawBigCloud

        mov ch, 2
        mov cl, 67
        call DrawCloud

    ;bricks

        mov cl, 10
        call DrawStaircase_2

        mov cl, 50
        call DrawStaircase_1

    ;gap

        mov bx, 2
        mov cl, 64
        call DrawGap

;Page 6 (last page)

        mov dx, 6

    ;clouds

        mov ch, 4
        mov cl, 5
        call DrawBigCloud

        mov ch, 4
        mov cl, 35
        call DrawCloud

        mov ch, 2
        mov cl, 63
        call DrawBigCloud

    ;pipe

        mov bx, 4
        mov cl, 9
        mov ch, 21
        call DrawPipe

    ;big staircase

        mov cl, 15
        call DrawBigStaircase

    ;flagpole

        mov bx, 16d
        mov cl, 38
        mov ch, 21
        call DrawFlagpole


;draws all enemies

        call DrawStartEnemies

        popf
        ret

DrawMap ENDP
;*************************************************************************

;have one variable that increments (or adds by two) every time a note is
;played (this 1st one is used for indexing), and then another one that keep
;track of the timing for note lengths
    ;2nd one is incremented with every BIOS clock tick, and is set to zero
    ;after a note finishes playing

    ;1st is note_ticks
    ;2nd is note_length_ticks

;3rd variable that is a note timer, used for comparison to see how long the
;note should be played for
    ;gets set to whatever the corresponding timer is for the LengthScore array

    ;this is called note_timer



;31.)
    ;WORKS
;*********************************************************************
GameMainLoop PROC
;Endless loop that runs in the main program to check if the game is over

        pushf

    music_loop_1:
        call SpeakerOn
        call PlayFrequency      ;sets the speaker freq based on note_ticks
                                ;as well as the note_timer
        mov note_length_ticks, 0
    music_loop_2:

        cmp GameOver, 1         ;code for gameover check is placed here, as
        je ending               ;this loop should run at least once (though
        cmp GameWin, 1          ;probably more) every BIOS clock tick
        je ending

        push cx
        mov cx, note_timer
        cmp note_length_ticks, cx
        pop cx
        jl music_loop_2         ;only is able to exit once the note is done

        mov note_length_ticks, 0
        call SpeakerOff
        inc note_ticks          ;moves the "sheet music" along to the next note
        cmp note_ticks, 167
        jne music_loop_1        ;plays the next note if not at the end of the
                                ;music

        mov note_ticks, 0       ;resets note_ticks
        jmp music_loop_1        ;then jumps back into the loop

    ending:

        call SpeakerOff

        popf
        ret

GameMainLoop ENDP
;*********************************************************************
;32.)
    ;WORKS
;*************************************************************************
CheckFallDeath PROC
;procedure that sets the GameOver bool if Mario has fallen to the bottom of the
;screen

        pushf

        cmp ch, 24d
        jne not_fallen
        mov GameOver, 1       ;If he is at the bottom of the screen, he died
    not_fallen:

        popf
        ret

CheckFallDeath ENDP
;*************************************************************************
;33.)
    ;WORKS
;*************************************************************************
DrawPipe PROC
;procedure that draws a pipe to the current screen
    ;pipe is 4 blocks tall (prehaps have it vary?)

    ;RECEIVES:
        ;BX = height of pipe (not including the top)
        ;CH = Row for bottom left of the pipe
        ;CL = Col for bottom left of the pipe
        ;DX = Page to draw pipe to


        pushf
        push ax
        push dx
        push es



        mov al, PIPE_CHAR

        call FindCharLoc        ;ES:DX = bottom left of pipe

;draws the stalk of the pipe

        mov ah, PIPE_STALK_COLOR

        call PipeHelper
        add dx, 2
        call PipeHelper     ;called 3 times to draw the 3 stalks of the pipe
        add dx, 2
        call PipeHelper


;draw the top of the pipe

        mov ah, PIPE_TOP_COLOR

        sub dx, 4           ;DX = offset for bottom left of pipe
        call PipeHelper_2


        pop es
        pop dx
        pop ax
        popf
        ret

DrawPipe ENDP
;*************************************************************************
;34.)
    ;WORKS
;*************************************************************************
PipeHelper PROC
;Helper function for DrawPipe

    ;RECEIVES:
        ;AX = pipe character
        ;BX = height of pipe (not including the top)
        ;ES:DX = bottom right of pipe segment to draw


        pushf
        push di
        push si

        mov di, dx              ;DI = offset of bottom left of pipe
        mov si, 0               ;zeroes si


;loop for drawing the left side of the pipe

        jmp PipeHelper_L0
    pipe_stalk:
        mov BYTE PTR es:[di], al        ;places the character on the screen
        mov BYTE PTR es:[di + 1], ah    ;adds the color attributes to the char
        inc si
        sub di, 160d                    ;moves offset up by one row
    PipeHelper_L0:
        cmp si, bx
        jne pipe_stalk              ;jump back in if the pipe is not tall enough


        pop si
        pop di
        popf
        ret


PipeHelper ENDP
;*************************************************************************
;35.)
    ;WORKS
;*************************************************************************
PipeHelper_2 PROC
;
    ;RECEIVES:
        ;AX = pipe character
        ;BX = height of pipe (not including the top)
        ;ES:DX = bottom of pipe segment to draw

        pushf
        push si
        push di

        mov di, dx      ;DI = offset of bottom left of pipe
        mov si, 0       ;zeroes SI

        jmp top_comparison
    PipeHelper_2_L0:
        inc si
        sub di, 160d            ;moves DI up one row
    top_comparison:
        cmp si, bx
        jne PipeHelper_2_L0     ;if not at the right height yet, jump back in

        sub di, 2               ;moves offset to upper right

        mov si, 0

        jmp top_draw_comp
    PipeHelper_2_L1:
        inc si
        mov BYTE PTR es:[di], al        ;places the character on the screen
        mov BYTE PTR es:[di + 1], ah    ;gives the character attributes
        add di, 2
    top_draw_comp:
        cmp si, 5
        jne PipeHelper_2_L1


        pop di
        pop si
        popf
        ret


PipeHelper_2 ENDP
;*************************************************************************
;36.)
    ;WORKS
;*************************************************************************
DrawGround PROC
;draws the ground for a page

    ;RECEIVES:
        ;DX = page number to draw to


        pushf
        push ax
        push bx
        push cx

        mov al, GROUND_BLOCK
        mov ah, GROUND_BLOCK_COLOR
        mov bx, 80d
        mov ch, 23d
        mov cl, 0
        call PrintStringPage
        mov ch, 24d
        call PrintStringPage
        mov ch, 22d
        call PrintStringPage

        pop cx
        pop bx
        pop ax
        popf
        ret

DrawGround ENDP
;*************************************************************************
;37.)
    ;WORKS
;*************************************************************************
DrawCloud PROC
;draws the clouds for a page

    ;RECEIVES:
        ;CL = Column to draw left of the cloud in
        ;CH = Row to draw middle of the cloud in
        ;DX = page number to draw to

        pushf
        push ax
        push bx
        push cx

        mov ax, CLOUD_CHAR

        mov bx, 7
        call PrintStringPage

        mov bx, 5
        inc cl
        dec ch
        call PrintStringPage

        pop cx
        pop bx
        pop ax
        popf
        ret

DrawCloud ENDP
;*************************************************************************
;38.)
    ;WORKS
;*************************************************************************
DrawBlockRow PROC
;draws a row of blocks

    ;RECEIVES:
        ;BX = # of blocks to draw
        ;CH = row the blocks are in
        ;CL = column that the leftmost block is in
        ;DX = page to draw to

        pushf
        push ax

        mov ah, BRICK_COLOR
        mov al, BRICK_BLOCK     ;move block character into AX

        call PrintStringPage    ;prints out the blocks

        pop ax
        popf
        ret

DrawBlockRow ENDP
;*************************************************************************
;39.)
    ;WORKS
;*************************************************************************
DrawGap PROC
;draws a gap in the ground at a particular point and page with a given length

    ;RECEIVES:
        ;BX = width of gap
        ;CL = start column of gap
        ;DX = Page to draw to

        pushf
        push ax

        mov ah, BLANK_CHAR_BACKGROUND
        mov al, BLANK_CHAR
        mov ch, 22
        call PrintStringPage
        mov ch, 23
        call PrintStringPage
        mov ch, 24
        call PrintStringPage            ;prints a gap with the specified width
                                        ;starting at the specified column

        pop ax
        popf
        ret


DrawGap ENDP
;*************************************************************************
;40.)
    ;DOESN'T WORK
;*************************************************************************
HideCursor PROC
;hides the cursor from view

        pushf
        push ax
        push bx
        push dx

        mov ah, 2
        mov bh, 1
        mov dx, 0
        int 10h

        pop dx
        pop bx
        pop ax
        popf
        ret

HideCursor ENDP
;*************************************************************************
;41.)
    ;WORKS
;*************************************************************************
DrawBigCloud PROC
;draws a big cloud to the page

    ;RECEIVES:
        ;CL = Column to draw left of the cloud in
        ;CH = Row to draw bottom of the cloud in
        ;DX = page number to draw to

        pushf
        push ax
        push bx
        push cx

        mov ax, CLOUD_CHAR

        mov bx, 13
        call PrintStringPage

        mov bx, 5
        inc cl
        dec ch
        call PrintStringPage

        mov bx, 6
        add cl, 5
        call PrintStringPage

        dec ch
        sub cl, 4
        mov bx, 9
        call PrintStringPage

        pop cx
        pop bx
        pop ax
        popf
        ret

DrawBigCloud ENDP
;*************************************************************************
;42.)
    ;WORKS
;*************************************************************************
DrawStaircase_1 PROC
;draws a staircase on the page

    ;RECEIVES:
        ;DX = page
        ;CL = start of staircase

        pushf
        push bx
        push cx

        mov ch, 21
        mov bx, 14
        call DrawBlockRow

        mov ch, 20
        add cl, 2
        mov bx, 12
        call DrawBlockRow

        mov ch, 19
        add cl, 2
        mov bx, 10
        call DrawBlockRow

        mov ch, 18
        add cl, 2
        mov bx, 8
        call DrawBlockRow

;now for top

        mov ch, 18
        add cl, 10
        mov bx, 6
        call DrawBlockRow

        mov ch, 19
        mov bx, 8
        call DrawBlockRow

        mov ch, 20
        mov bx, 10
        call DrawBlockRow

        mov ch, 21
        mov bx, 12
        call DrawBlockRow

        pop cx
        pop bx
        popf
        ret

DrawStaircase_1 ENDP
;*************************************************************************
;43.)
    ;WORKS
;*************************************************************************
DrawStaircase_2 PROC
;draws a staircase on the page

    ;RECEIVES:
        ;DX = page
        ;CL = start of staircase

        pushf
        push bx
        push cx

        mov ch, 21
        mov bx, 12
        call DrawBlockRow

        mov ch, 20
        add cl, 2
        mov bx, 10
        call DrawBlockRow

        mov ch, 19
        add cl, 2
        mov bx, 8
        call DrawBlockRow

        mov ch, 18
        add cl, 2
        mov bx, 6
        call DrawBlockRow

;now for top

        mov ch, 18
        add cl, 8
        mov bx, 6
        call DrawBlockRow

        mov ch, 19
        mov bx, 8
        call DrawBlockRow

        mov ch, 20
        mov bx, 10
        call DrawBlockRow

        mov ch, 21
        mov bx, 12
        call DrawBlockRow

        pop cx
        pop bx
        popf
        ret

DrawStaircase_2 ENDP
;*************************************************************************
;44.)
    ;WORKS
;*************************************************************************
DrawBigStaircase PROC
;draws a staircase on the page

    ;RECEIVES:
        ;DX = page
        ;CL = start of staircase

        pushf
        push bx
        push cx
        push si

        mov ch, 22
        mov bx, 20
        mov si, 0
        jmp BigStairs_L1
    BigStairs_L0:
        dec ch
        add cl, 2
        sub bx, 2
        call DrawBlockRow
        inc si
    BigStairs_L1:
        cmp si, 8
        jne BigStairs_L0


        pop si
        pop cx
        pop bx
        popf
        ret

DrawBigStaircase ENDP
;*************************************************************************
;45.)
    ;WORKS
;*************************************************************************
CheckEnemyCollision PROC
;based on Mario's velocity and position, if he moved into an enemy, end the game

        pushf
        push cx
        push es

        mov ch, Mario_Pos_Y
        mov cl, Mario_Pos_X

        call CheckMoveHelper    ;CH = char at intended move spot
                                ;ES = Segment of intended move

        cmp ch, ENEMY_CHAR       ;if intended move spot doesn't have enemy...
        jne not_enemy               ;exit the proc
        mov GameOver, 1            ;otherwise, end the game

    not_enemy:

        pop es
        pop cx
        popf
        ret

CheckEnemyCollision ENDP
;*************************************************************************
;46.)
    ;WORKS
;*************************************************************************
CheckMoveHelper PROC
;helper for CheckMove and CheckEnemyCollision procedures for Mario

    ;RECEIVES:
        ;CH = Mario's row
        ;CL = Mario's col
    ;RETURNS:
        ;CH = char at intended move spot
        ;ES= Segment of intended move

        pushf
        push dx
        push di

        cmp Mario_Vel_X, 2      ;checks to see if his X-vel is left
        jl not_left                 ;jump away if not
        dec cl                  ;CL = Mario's intended Col move
        jmp X_done
    not_left:
        add cl, Mario_Vel_X     ;CL = Mario's intended Col move
    X_done:

        cmp Mario_Vel_Y, 2      ;checks to see if his Y-vel is up
        jl not_up                   ;jump away if not
        dec ch                   ;CH = Mario's intended Row move
        jmp Y_done
    not_up:
        add ch, Mario_Vel_Y      ;CH = Mario's intended Row move
    Y_done:

        mov dx, Current_Page
        call FindCharLoc        ;ES:DX = Offset of intended move
        mov di, dx              ;DI = Offset

        mov ch, es:[di]         ;CH = char at the intended move spot

        pop di
        pop dx
        popf
        ret


CheckMoveHelper ENDP
;*************************************************************************

;*************************************************************************

NUM_ENEMIES = 9

        ;EnemyData LABEL BYTE
        EnemyPage WORD 2h, 2h, 2h, 3h, 4h, 4h, 4h, 5h, 6h
        Enemy_X_Pos BYTE 25, 40, 46, 25, 15, 30, 50, 40, 15
        Enemy_Y_Pos BYTE 21, 21, 21, 21, 21, 21, 21, 21, 21
        Enemy_X_Vel BYTE 1, 1, 2, 1, 1, 1, 1, 1, 1


;47.)
    ;WORKS
;*************************************************************************
DrawEnemy PROC
;erases and draws an enemy that is moving based on its current position
;and velocity

;likely will need a CheckEnemyMove PROC that checks to see if the enemy ran into
;Mario or needs to change velocity if it hits a wall

;Probably have each enemy in an ordered array with the corresponding page,
;position, and velocity

    ;RECEIVES:
        ;Enemy number (in which register???)
        ;Or, perhaps this PROC will loop through all the present enemies, in
        ;which case this PROC may not need to receive anything
            ;9 enemies total

        pushf
        push ax
        push bx
        push cx
        push dx
        push si
        push di
        push es

        mov si, 0                ;SI will be a counter for the number of enemies
        ;mov di, OFFSET EnemyData ;DI = offset of start of enemy data
                                    ;enemy data = page, X_pos, Y_pos, then X_vel
        mov bx, PAGE0
        mov es, bx

    enemy_loop:
        mov bx, 0
        inc si                      ;SI = current enemy number
        mov bx, si                  ;BX = current enemy number
        dec bx                      ;BX = current enemy number-1, so it can be
                                    ;used to index

        mov di, OFFSET EnemyPage

        shl bx, 1
        mov dx, WORD PTR ds:[bx + di]   ;DX = page of enemy
        shr bx, 1

        mov di, OFFSET Enemy_Y_Pos
        mov ch, BYTE PTR ds:[bx + di]   ;CH = row of enemy

        mov di, OFFSET Enemy_X_Pos
        mov cl, BYTE PTR ds:[bx + di]   ;CL = col of enemy

;first, need to erase the enemy:

        call EraseChar

;then, need to update its position:     ;CL = col of enemy

        mov di, OFFSET Enemy_X_Vel

        mov ax, 0                       ;zeroes AX
        mov ah, BYTE PTR ds:[bx + di]   ;AH = Enemy_X_Vel

        ;push dx
        ;mov dx, ax
        ;call PrintWord
        ;pop dx

        call CheckEnemyMoveHelper       ;AL = char at intended move spot
                                        ;ES = segment of intended move


        call CheckMarioHit      ;CHECK HERE FOR IF IT RUNS INTO MARIO
                                ;if so, makes GameOver = 1

        cmp al, 02h
        je enemy_move_ok        ;if the move if over Mario, you can erase him
        cmp al, BLANK_CHAR
        je enemy_move_ok       ;if the blank char, we can move there
        mov di, OFFSET Enemy_X_Vel
        call ReverseEnemyVel    ;otherwise, reverse its velocity
                                    ;AH = new enemy vel

        mov di, OFFSET Enemy_X_Vel
        mov BYTE PTR ds:[bx + di], ah       ;then update the velocity as well

        jmp enemy_pos_updated      ;<< ONLY WAY TO STOP THEM FROM WEIRD MOVEMENT

        mov di, OFFSET Enemy_X_Pos
        call UpdateEnemyPos           ;CL = new enemy position
        mov BYTE PTR ds:[bx + di], cl   ;then update the position

        jmp enemy_pos_updated

    enemy_move_ok:
        mov di, OFFSET Enemy_X_Pos
        call UpdateEnemyPos             ;CL = new enemy position
        mov BYTE PTR ds:[bx + di], cl   ;then update the position

    enemy_pos_updated:


;and finally, draw the enemy to the screen:

        mov ah, ENEMY_COLOR
        mov al, ENEMY_CHAR
        call PlaceChar

    enemy_loop_cmp:
        cmp si, NUM_ENEMIES
        jne enemy_loop          ;if not drawn all the enemies yet, return
                                ;to top of loop

        pop es
        pop di
        pop si
        pop dx
        pop cx
        pop bx
        pop ax
        popf
        ret


DrawEnemy ENDP
;*************************************************************************
;48.)
    ;WORKS
;*************************************************************************
CheckEnemyMoveHelper PROC
;helper for CheckMove and CheckEnemyCollision procedures for Mario
;returns the char at the intended move spot

    ;RECEIVES:
        ;AH = Enemy X-vel
        ;CH = Enemy row
        ;CL = Enemy col
        ;DX = Enemy Page
    ;RETURNS:
        ;AL = char at intended move spot
        ;ES = Segment of intended move

        pushf
        push cx
        push dx
        push di

        cmp ah, 2      ;checks to see if X-vel is left
        jl enemy_not_left                 ;jump away if not
        dec cl                 ;CL = Enemy intended Col move
        jmp enemy_X_done
    enemy_not_left:
        add cl, ah           ;CL = Enemy intended Col move
    enemy_X_done:

        call FindCharLoc        ;ES:DX = Offset of intended move
        mov di, dx              ;DI = Offset

        mov al, es:[di]         ;AL = char at the intended move spot

        pop di
        pop dx
        pop cx
        popf
        ret


CheckEnemyMoveHelper ENDP
;*************************************************************************
;49.)
    ;WORKS
;*************************************************************************
ReverseEnemyVel PROC
;reverses the enemy velocity

    ;RECEIVES:
        ;AH = enemy velocity
        ;BX = index for the enemy into datasets
        ;DI = offset for the enemy velocity
    ;RETURNS
        ;AH = new enemy velocity

        pushf
        push bx
        push di

        ;call ReverseNextEnemyVel

        cmp ah, 2
        je ReverseEnemyVel_L    ;if it is left, jump accordingly
        mov ah, 2               ;otherwise, change it to left
        jmp ReverseEnemyVel_end
    ReverseEnemyVel_L:
        mov ah, 1               ;it was left, but now is right
    ReverseEnemyVel_end:



        pop di
        pop bx
        popf
        ret

ReverseEnemyVel ENDP
;*************************************************************************
;50.)
    ;BUGGED (Sort of, need to change code so it will do as intended)
;*************************************************************************
UpdateEnemyPos PROC
;updates the enemy's col position based on the velocity

    ;RECEIVES:
        ;AH = enemy vel
        ;CL = col of enemy
    ;RETURN
        ;CL = new enemy col


        pushf
        push ax

;THERE WILL BE A BUG HERE
        call CheckEnemyMoveHelper   ;AL = char at intended mov pos
        cmp al, BLANK_CHAR          ;if the spot is not empty
        jne UpdateEnemyPos_end      ;don't move at all
        cmp ah, 2               ;now, cmp velocity to 2
        je UpdateEnemyPos_L    ;if it is left, jump accordingly
        inc cl                    ;otherwise, add to get the new col
        jmp UpdateEnemyPos_end
    UpdateEnemyPos_L:
        dec cl                  ;move the enemy position left
    UpdateEnemyPos_end:

        pop ax
        popf
        ret

UpdateEnemyPos ENDP
;*************************************************************************

        EnemyTicks WORD 0

;51.)
    ;WORKS
;*************************************************************************
DrawFrame PROC
;Proc called by Tick handler that is resposible for calling all the PROCs
;that check for movement and draw frames

        pushf

        call SetVelocity    ;velocities for Mario are set here based ONLY on
                            ;key presses
                                ;if up pressed, resets "ticks" as well

        call CheckEnemyCollision    ;checks to see if mario will move into an enemy

        call CheckAbove     ;if there is a block above him, set Jumping_Up = 0
                            ;so that he falls back down

        call CheckBelow     ;resets Y_vel to zero if there is a block directly below
                            ;also deals with incrementing ticks while jumping
                            ;as well as resetting the Jumping Bool

        call CheckMove      ;checks to see if move is possible. If not, does
                            ;not move there

        call DrawMario      ;moves Mario on the screen based on his velocity
                            ;and position

        ;***
        ;FUNCTION HERE TO MOVE ENEMIES, AND CHECK TO SEE IF THEY MOVED INTO
        ;MARIO
        ;***

        inc EnemyTicks
        cmp EnemyTicks, 5
        jne no_enemy_move_yet

        call DrawEnemy
        dec ScoreTimer


        mov EnemyTicks, 0

    no_enemy_move_yet:

        call UpdateScore        ;Updates the score
        call WriteScore         ;Writes Score to the screen

        call CheckScoreTimer    ;checks to see if the time ran out



        popf
        ret

DrawFrame ENDP
;*************************************************************************
;52.)
    ;WORKS
;*************************************************************************
DrawStartEnemies PROC
;Initializes all enemies to the map

        pushf
        push ax
        push bx
        push cx
        push dx
        push si
        push di

        mov ah, ENEMY_COLOR
        mov al, ENEMY_CHAR

        mov si, 0       ;zeroes SI

    DrawStartEnemies_top:
        mov bx, 0
        inc si
        mov bx, si
        dec bx              ;bx can now be used to index

        mov di, OFFSET Enemy_X_Pos
        mov cl, BYTE PTR ds:[bx + di]   ;CL = enemy start col

        mov di, OFFSET Enemy_Y_Pos
        mov ch, BYTE PTR ds:[bx + di]   ;CH = enemy start row

        shl bx, 1
        mov di, OFFSET EnemyPage
        mov dx, WORD PTR ds:[bx + di]            ;DX = EnemyPage
        shr bx, 1

        call PlaceChar                  ;draws enemy to screen

    DrawStartEnemies_cmp:
        cmp si, NUM_ENEMIES
        jne DrawStartEnemies_top


        pop di
        pop si
        pop dx
        pop cx
        pop bx
        pop ax
        popf
        ret

DrawStartEnemies ENDP
;*************************************************************************
;53.)
    ;WORKS
;*************************************************************************
CheckMarioHit PROC

    ;RECEIVES:
        ;AL = char at intended move spot
    ;RETURNS:
        ;Sets GameOver =1 if enemy is going to move into Mario

        pushf

        cmp al, 02h     ;02h = Mario character
        jne mario_safe
        mov GameOver, 1
    mario_safe:

        popf
        ret


CheckMarioHit ENDP
;*************************************************************************
;54.)
    ;Will not use
;*************************************************************************
ReverseNextEnemyVel PROC



        pushf
        push ax

        cmp bx, 8
        je ReverseNextEnemyVel_end
        call CheckEnemyMoveHelper   ;AL = intended move char
        cmp al, ENEMY_CHAR
        jne ReverseNextEnemyVel_end
        mov ah, BYTE PTR ds:[bx + di + 1]   ;AH = next enemy vel
        cmp ah, 2
        je rev_L
        mov ah, 2
        mov BYTE PTR ds:[bx + di + 1], ah

        ;call DumpRegs

        jmp ReverseNextEnemyVel_end
    rev_L:
        mov ah, 1
        mov BYTE PTR ds:[bx + di + 1], ah
    ReverseNextEnemyVel_end:

        pop ax
        popf
        ret


ReverseNextEnemyVel ENDP
;*************************************************************************
;55.)
    ;WORKS
;*************************************************************************
ClearKeyBuffer PROC
;removes key presses from the buffer until there are none left

        pushf
        push ax

        jmp check_for_key
    remove_key:
        mov ah, 00h
        int 16h         ;removes key from buffer
    check_for_key:
        mov ah, 01h
        int 16h         ;ZF=1 if no key available
        jnz remove_key

        pop ax
        popf
        ret


ClearKeyBuffer ENDP
;*************************************************************************
;56.)
    ;UNWRITTEN
;*************************************************************************
ChangeTypematicDelay PROC
;changes the typematic delay
    ;May not be able to write this because it isn't supported

        pushf

        popf
        ret


ChangeTypematicDelay ENDP
;*************************************************************************
;57.)
    ;UNWRITTEN
;*************************************************************************
ResetTypematicDelay PROC
;resets the typematic delay to original value
    ;May not be able to write this because it isn't supported

        pushf

        popf
        ret


ResetTypematicDelay ENDP
;*************************************************************************
;58.)
    ;WORKS
;*************************************************************************
DrawFlagpole PROC
;draws the flagpole

    ;RECEIVES:
        ;BX = height of flagpole
        ;CL = col
        ;CH = row
        ;DX = page number

        pushf
        push ax
        push cx
        push dx
        push si


        mov ah, FLAGPOLE_COLOR
        mov al, FLAGPOLE_CHAR

        mov si, 0

        jmp DrawFlagpole_cond
    DrawFlagpole_loop:
        inc si              ;increments loop counter
        call PlaceChar      ;places character
        dec ch              ;decrements CH (moves up a row)
    DrawFlagpole_cond:
        cmp si, bx
        jl DrawFlagpole_loop

        inc ch
        inc cl              ;moves col to the left

        mov ah, 00111111b
        mov al, 10h

        call PlaceChar


        pop si
        pop dx
        pop cx
        pop ax
        popf
        ret


DrawFlagpole ENDP
;*************************************************************************

        GameWinMessage BYTE "YOU WIN!!!", 0Dh, 0Ah, 0
        GameLoseMessage BYTE "You died...", 0Dh, 0Ah, 0

;59.)
    ;WORKS
;*************************************************************************
PrintString PROC

    ;DX = OFFSET for str to be written to standard output
    ;should leave everything unchanged


        ;loop that goes through and prints out each char
            ;string literals are stored in memory as sequences of integer
            ;byte values
        ;null terminated strings, so stops when it hits the 00h

        pushf
        push ax
        push bx             ;saves values of the registers being used
        push si

        mov ax, 0       ;initialize to zero for NULL comparison
        mov bx, 0       ;initialize BX to 0 for moving through str OFFSET

        mov si, dx     ;moves offset of string into SI register

    writing:

        cmp al, [si + bx]        ;checks to see if next Byte is NULL
                                    ;al instead of ax so that it checks the BYTE
                                    ;starting at [si+bx] and not the word
                                        ;register (8bit vs 16bit) determines the
                                        ;size of the operation when using the
                                        ;dereference to a memory location

        je writingEnd           ;jumps to end if it is the NULL terminating char

        push ax                  ;saves AX and DX before output
        push dx

        mov dl,[si + bx]          ;moves the ASCII value to be written into DL
        mov ah, 02h               ;sets AH to DOS code for write-character
        int 21h                   ;tells DOS to takeover with given command

        pop dx               ;return AX and DX values to the registers
        pop ax

        inc bx             ;moves bx along in order to point to next char in str
        jmp writing         ;returns to start of writing loop

    writingEnd:

        pop si
        pop bx              ;returns the original register values
        pop ax
        popf

        ret

PrintString ENDP
;*********************************************************************

        ScoreBuffer BYTE 0, 0, 0    ;leftmost is ones, middle is tens, 3rd
                                    ;is hundreds

        ScoreBufferString BYTE 0, 0, 0, 0Dh, 0Ah, 0   ;buffer for printing out the score
                                                        ;if you win

;60.)
    ;WORKS
;*************************************************************************
UpdateScore PROC
;updates the current score buffer

    ;RECEIVES:
        ;ScoreTimer global variable

        pushf
        push ax
        push bx
        push cx
        push si

        mov si, 0               ;sets SI to index into ones spot in ScoreBuffer
        mov ch, 10d             ;CH will be used to get the remainder for DIV
        mov bx, OFFSET ScoreBuffer      ;BX = location of ScoreBuffer

        mov ax, 0               ;zeroes AX to be the dividend
        mov ax, ScoreTimer      ;AX = ScoreTimer

        div ch                  ;AH = remainder, AL = quotient

        mov BYTE PTR ds:[bx + si], ah   ;sets the ones score buffer
        inc si                          ;moves SI to index into tens spot

        mov ah, 0               ;removes remainder
        div ch                  ;AH = remainder, AL = quotient

        mov BYTE PTR ds:[bx + si], ah   ;sets the tens score buffer
        inc si                          ;moves SI to index into ones spot

        mov ah, 0               ;removes remainder
        div ch                  ;AH = remainder, AL = quotient

        mov BYTE PTR ds:[bx + si], ah   ;sets the ones score buffer
        dec si


        pop si
        pop cx
        pop bx
        pop ax
        popf
        ret


UpdateScore ENDP
;*************************************************************************

        ScoreWord BYTE "SCORE: ", 0

SCORE_COL_LOC = 33


;61.)
    ;WORKS
;*************************************************************************
WriteScore PROC
;Writes the score to the current screen

        pushf
        push ax
        push bx
        push dx
        push si

        mov dx, Current_Page
        call WriteScoreWord     ;"SCORE " is written to the page
                                ;CH = top row (0)
                                ;CL = start column to write the score

        mov bx, OFFSET ScoreBuffer   ;BX = loc of ScoreBuffer array
        mov ah, 00110000b            ;sky background plus black letters

        mov si, 3

        jmp WriteScore_cmp
    WriteScore_loop:
        dec si
        mov al, BYTE PTR ds:[bx + si]
        add al, 30h                 ;AL = ASCII for the number
        call PlaceChar
        inc cl
    WriteScore_cmp:
        cmp si, 0
        jne WriteScore_loop

        pop si
        pop dx
        pop bx
        pop ax
        popf
        ret

WriteScore ENDP
;*************************************************************************
;62.)
    ;WORKS
;*************************************************************************
WriteScoreWord PROC
;Writes "SCORE " to the current page

    ;RETURNS:
        ;CH = top row (0)
        ;CL = start column to write the score


        pushf
        push ax
        push bx
        push dx
        push si


        mov bx, OFFSET ScoreWord
        mov ch, 0
        mov cl, SCORE_COL_LOC
        mov dx, Current_Page
        mov si, 0

        mov ah, 00110000b       ;sky background plus black letters

        jmp WriteScoreWord_cmp
    WriteScoreWord_loop:
        mov al, BYTE PTR ds:[bx + si]
        call PlaceChar
        inc si
        inc cl
    WriteScoreWord_cmp:
        cmp si, 7
        jne WriteScoreWord_loop


        pop si
        pop dx
        pop bx
        pop ax
        popf
        ret


WriteScoreWord ENDP
;*************************************************************************
;63.)
    ;WORKS
;*************************************************************************
ChangeScoreBufferString PROC
;updates the ScoreBufferString array based on the ScoreBuffer array

        pushf
        push ax
        push bx
        push bp
        push si
        push di

        mov al, 0
        mov si, 0
        mov di, 2
        mov bx, OFFSET ScoreBuffer
        mov bp, OFFSET ScoreBufferString

        jmp ChangeScoreBufferString_cmp
    ChangeScoreBufferString_loop:
        mov al, BYTE PTR ds:[bx + si]   ;AL = corresponding pos in ScoreBuffer
        mov BYTE PTR ds:[bp + di], al   ;updates the corresponding pos in buffer str
        add BYTE PTR ds:[bp + di], 30h  ;turns it into ASCII
        inc si
        dec di
    ChangeScoreBufferString_cmp:
        cmp si, 3
        jne ChangeScoreBufferString_loop

        pop di
        pop si
        pop bp
        pop bx
        pop ax
        popf
        ret


ChangeScoreBufferString ENDP
;*************************************************************************
;64.)
    ;WORKS
;*************************************************************************
CheckScoreTimer PROC
;checks to see if the score timer hit zero, and causes a gameover if it does

        pushf

        cmp ScoreTimer, 0
        je timer_game_over
        jmp CheckScoreTimer_end
    timer_game_over:
        mov GameOver, 1
    CheckScoreTimer_end:

        popf
        ret

CheckScoreTimer ENDP
;*************************************************************************
;65.)
    ;TEST NEEDED
;*************************************************************************
WarpKey PROC
;function that erases the current Mario and then warps him to a new location
    ;based on the number (1-6) that was pressed
    ;7 sets the timer to 10s

        pushf
        push ax
        push cx
        push dx

        mov cl, Mario_Pos_X
        mov ch, Mario_Pos_Y
        mov dx, Current_Page
        call EraseChar          ;erases the current Mario

    ;1 keypress
        cmp ah, 02h
        je keypress_1

    ;2 keypress
        cmp ah, 03h
        je keypress_2

    ;3 keypress
        cmp ah, 04h
        je keypress_3

    ;4 keypress
        cmp ah, 05h
        je keypress_4

    ;5 keypress
        cmp ah, 06h
        je keypress_5

    ;6 keypress
        cmp ah, 07h
        je keypress_6

    ;7 keypress             ;7 sets the timer to 10s
        cmp ah, 08h
        je keypress_7
        jmp WarpKey_end

    keypress_1:
        mov al, 1
        call SetDisplayPage
        jmp WarpKey_end

    keypress_2:
        mov al, 2
        call SetDisplayPage
        jmp WarpKey_end

    keypress_3:
        mov al, 3
        call SetDisplayPage
        jmp WarpKey_end

    keypress_4:
        mov al, 4
        call SetDisplayPage
        jmp WarpKey_end

    keypress_5:
        mov al, 5
        call SetDisplayPage
        jmp WarpKey_end

    keypress_6:
        mov al, 6
        call SetDisplayPage
        jmp WarpKey_end

    keypress_7:
        mov ScoreTimer, 10
        jmp WarpKey_end


    WarpKey_end:

        ;mov Mario_Pos_X, MARIO_START_POS_X
        ;mov Mario_Pos_Y, MARIO_START_POS_Y



        pop dx
        pop cx
        pop ax
        popf
        ret


WarpKey ENDP
;*************************************************************************
;66.)

;*************************************************************************
Delay PROC
;Delay function used for music

        pushf
        push bx
        push cx

        mov ticks, 0

        mov bx, OFFSET LengthScore
        mov cx, WORD PTR ds:[bx + di]

    ;
    ;check_loop:
        ;cmp GameOver, 1
        ;je ending
        ;cmp GameWin, 1
        ;je ending
        ;jmp check_loop

        jmp Delay_cmp
    Delay_loop:
    Delay_cmp:
        cmp note_ticks, cx
        jng Delay_loop

        pop cx
        pop bx
        popf
        ret

Delay ENDP
;*************************************************************************
;67.)

;*************************************************************************
SpeakerOn PROC
;turns on the speaker

        pushf
        push ax

        in al, 61h      ;reads in a byte from the speaker
        or al, 03h      ;sets low 3 bits
        out 61h, al    ;sends the new byte back to the speaker so that it is in
                        ;ON status

        pop ax
        popf
        ret

SpeakerOn ENDP
;*************************************************************************
;68.)

;*************************************************************************
SpeakerOff PROC
;turns off the speaker


        pushf
        push ax

        in al, 61h      ;reads in a byte from the speaker
        and al, 0FCh     ;clears low 3 bits
        out 61h, al    ;sends the new byte back to the speaker so that it is in
                        ;OFF status

        pop ax
        popf
        ret

SpeakerOff ENDP
;*************************************************************************
;69.)

;*************************************************************************
PlayFrequency PROC
;PROC that sets the speaker frequency to that of the next note in the music
;score

        pushf
        push ax
        push bx
        push dx
        push di

        ;RECEIVES:
            ;note_ticks = note to be played in the music score

        mov bx, OFFSET MusicScore       ;BX = offset of the music score
        mov di, note_ticks              ;DI = # of note to be played
        shl di, 1                       ;DI = index of note to be played

        mov dx, WORD PTR ds:[bx + di]    ;DX = note ratio
        ;call PrintWord                  ;test code to see what the freq is

        mov al, 0B6h
        out 43h, al
        mov al, dl
        out 42h, al
        mov al, dh
        out 42h, al

        mov bx, OFFSET LengthScore
        mov ax, WORD PTR ds:[bx + di]
        mov note_timer, ax              ;sets the note_timer for the
                                        ;note as well
        inc note_timer


        pop di
        pop dx
        pop bx
        pop ax
        popf
        ret

PlayFrequency ENDP
;*************************************************************************


;*************************************************************************
main PROC
        mov ax, @data   ; load data segment register...
        mov ax, cs      ;include only the code segment
        mov ds, ax      ; ...with location of our data
                        ;NOW, CS = DS

        call BlankPages     ;sets all MemoryMapped bytes to 00 (char
                            ;and then attribute as teal/cyan)

        mov Mario_Pos_X, MARIO_START_POS_X;1d
        mov Mario_Pos_Y, MARIO_START_POS_Y;21d
        mov Mario_Vel_X, 0
        mov Mario_Vel_Y, 0

        mov ticks, 0
        mov note_ticks, 0
        mov note_timer, 0
        mov note_length_ticks, 0

        mov ScoreTimer, START_TIME

        call DrawMap            ;draws the entire game map

        mov Current_Page, START_PAGE


        mov al, START_PAGE
        call SetDisplayPage

        call SpeakerOn
        call ChangeInterrupt        ;switches BIOS interrupt for Tick PROC

        call GameMainLoop           ;endless loop that runs until game ends

        call ReinstateBIOS
        call SpeakerOff

;resets the display page back to zero
        mov al, 0
        call SetDisplayPage

        cmp GameOver, 1
        je lose
        cmp GameWin, 1
        je win

    lose:
        mov dx, OFFSET GameLoseMessage
        call PrintString
        jmp program_end
    win:
        mov dx, OFFSET GameWINMessage
        call PrintString
        mov dx, OFFSET ScoreWord
        call PrintString
        call ChangeScoreBufferString
        mov dx, OFFSET ScoreBufferString
        call PrintString
    program_end:







        ;mov ch, 5d
        ;mov cl, 20d
        ;mov dx, 2
        ;mov al, 26h
        ;mov ah, 11001111b
        ;call DumpRegs
        ;call PlaceChar
        ;call DumpRegs

        ;mov bx, 0
        ;call GetCurrentPage
        ;push bx
        ;mov al, 2
        ;call SetDisplayPage
        ;call GetCurrentPage
        ;push bx

        ;call Stall
        ;call Stall
        ;pop cx
        ;pop bx
        ;mov al, bh
        ;call SetDisplayPage

        ;mov bx, 0
        ;call GetCurrentPage
        ;push bx
        ;call DumpRegs
        ;mov al, 7
        ;call SetDisplayPage
        ;call GetCurrentPage
        ;push bx

        ;call Stall
        ;pop cx
        ;pop bx
        ;mov al, bh
        ;call SetDisplayPage
        ;call DumpRegs

;write the BIOS clock interrupt vector
        ;mov al, BIOS_CLK_INTERRUPT
        ;call GetInterruptVector
        ;call WriteInterruptVector

;save the BIOS_ClockVector memory location
        ;mov al, BIOS_CLK_INTERRUPT
        ;mov dx, OFFSET BIOS_ClockVector
        ;call SaveVector

;check to see if it was saved
        ;mov dx, OFFSET BIOS_ClockVector
        ;call WriteSavedVector
        ;call DumpRegs


;sets the new interrupt handler for the BIOS clock
        ;mov al, BIOS_CLK_INTERRUPT
        ;mov dx, Tick
        ;call SetInterruptVector

;checks to make sure that the vector was set correctly
        ;mov al, BIOS_CLK_INTERRUPT
        ;call GetInterruptVector         ;CHANGES ES
        ;call WriteInterruptVector        ;currently writes DS:011E


        ;call Stall

;restores the BIOS_ClockVector
        ;mov al, BIOS_CLK_INTERRUPT
        ;mov dx, ClockSegment
        ;call SetES
        ;mov dx, OFFSET BIOS_ClockVector
        ;call RestoreVector              ;should this change ES? NO
        ;call DumpRegs

;checks to see that it was restored
        ;mov al, BIOS_CLK_INTERRUPT
        ;call GetInterruptVector
        ;call WriteInterruptVector


        ;mov dx, PAGE0       ;moves page0 offset (B800) into DX
        ;call SetES          ;sets ES to that value
        ;mov al, 26h
        ;mov ah, 11001111b
        ;mov ch, 24d      ;row
        ;mov cl, 79d      ;col
        ;call PlaceChar



        ;mov al, 1Ch    ;BIOS clock tick interrupt
        ;call GetInterruptVector     ;how do I even tell if this worked?
        ;call WriteInterruptVector
        ;call DumpRegs


        mov ax, TERMINATE   ; DOS function to exit with termination code
        int DOS             ; ...exit
main ENDP
END main
