.386
.model flat, stdcall
.stack 4096
ExitProcess PROTO, dwExitCode: DWORD

INCLUDE Irvine32.inc
TITLE TYPING TUTOR GAME PROJECT (23K-0505 AND 23K-0513)

.data 
    menuMessage1 BYTE "1. Typing Tutor Game", 0   
    menuMessage2 BYTE "2. Word Dropping Game", 0   
    menuMessage3 BYTE "Enter your Choice: ", 0   
    invalidMsg BYTE "Invalid Choice!", 0  
    selectedOption dword ?

    ;for typing tutor game (1)
    strOne BYTE "Assembly language programming with Irvine32 provides convenient functions for input and output operations.", 0
    strTwo BYTE "The Irvine32 library simplifies assembly programming by offering built-in procedures like WriteChar for displaying characters and ReadChar for receiving user input, making it easier to handle basic tasks.", 0
    strThree BYTE "By using the Irvine32 library, assembly language programmers can focus on core logic without worrying about low-level details of handling input and output, as the library provides functions which are essential for creating interactive programs.", 0
   
    counter BYTE 0
    cord_Y BYTE 0

    ;for now - length will be max 7
    strArray BYTE 500 DUP(?), 0

    xCords BYTE 30 DUP(?)
    yCords BYTE 30 DUP(?)

    finishLn BYTE "--------------------------------------------------------------------------------------",0
    lnHeight BYTE 23
    touchLine BYTE 0
    startTime DWORD ?
    currTime DWORD ?
    miliSec DWORD ?
    Lost BYTE 0
    lostMsg BYTE "You lost!", 0
    wonMsg BYTE "You won! ", 0

    count DWORD 0
    typedWords DWORD 0
    typedChars DWORD 0
    wordCount DWORD 0

    arrInput BYTE 12 DUP(?)
    input_correct BYTE 1
    errCount BYTE 0
    gameWon BYTE 0

    printedWords DWORD 1
    wordLen DWORD ?

    ;file reading business
    dict4 BYTE "fourletterwords.txt", 0
    dict8 BYTE "eightletterwords.txt", 0
    dict12 BYTE "twelveletterwords.txt", 0

    bufSize = 5000 ;5000 bytes
    buf BYTE bufSize DUP(?)
    Readbytes DWORD ?
    fileErr BYTE "Could not read file", 0
    file_Handle DWORD ?

    ;menu stuff
    msgWlcm BYTE "Welcome to the Typing Tutor Game!", 0
    navMsg BYTE "Please select one of the following game modes using arrow keys", 0
    easyMsg BYTE "EASY MODE", 0
    mediumMsg BYTE "MEDIUM MODE", 0
    hardMsg BYTE "HARD MODE", 0

    ;hover difficulty
    hoverLvl BYTE 1

    ;loadingscreen stuff
    startGame BYTE "GAME STARTING IN", 0
    dot BYTE ".", 0

    three BYTE "3", 0
    two BYTE "2", 0
    one BYTE "1", 0
    go BYTE "Ready Set Go!", 0

    ;accuracy & stuff
    mistakeMsg BYTE "Mistakes: ", 0
    accuracyMsg BYTE "Accuracy: ", 0

    totalmistakes DWORD 0 ; don't change
    accuracy DWORD 0 
    totalInpCount DWORD 0
    playerScore DWORD 0
    wordsCompleteMsg BYTE "Words Completed: ", 0

    mistakes dword 0 
    totalchars dword 0 
    correctchars dword 0 


.code

screenLoader PROC uses edx eax ecx
    
    ;gamestart message
    mov dl, 30 ;column
    mov dh, 12
    call gotoxy
    mov edx, OFFSET startGame
    call WriteString

    ;add dots
    mov ecx, 3

    dots:
        mov eax, 500
        call Delay
        mov edx, OFFSET dot
        call WriteString
    loop dots

    call clrscr

    ;3
    mov dl, 37 ;column
    mov dh, 12
    call gotoxy
    mov edx, OFFSET three
    call WriteString

    mov eax, 500
    call Delay
    call clrscr

    ;2
    mov dl, 37 ;column
    mov dh, 12
    call gotoxy
    mov edx, OFFSET two
    call WriteString

    mov eax, 500
    call Delay
    call clrscr

    ;1
    mov dl, 37 ;column
    mov dh, 12
    call gotoxy
    mov edx, OFFSET one
    call WriteString

    mov eax, 500
    call Delay
    call clrscr

    ;go
    mov dl, 32 ;column
    mov dh, 12
    call gotoxy
    mov edx, OFFSET go
    call WriteString

    mov eax, 500
    call Delay
    call clrscr

ret
screenLoader ENDP


displayMenu PROC uses edx eax
    
    ;welcome message
    mov dl, 20 ;column
    mov dh, 8
    call gotoxy
    mov edx, OFFSET msgWlcm
    call WriteString

    ;navigate message
    mov dl, 10 ;column
    mov dh, 10
    call gotoxy
    mov edx, OFFSET navMsg
    call WriteString

    .IF hoverLvl == 1
        call set_Green
    .ENDIF
    ;easy message - 1
    mov dl, 34 ;column
    mov dh, 13
    call gotoxy
    mov edx, OFFSET easyMsg
    call WriteString
    call restoreColors

    .IF hoverLvl == 2
        call set_Green
    .ENDIF
    ;medium message - 2
    mov dl, 34 ;column
    mov dh, 15
    call gotoxy
    mov edx, OFFSET mediumMsg
    call WriteString
    call restoreColors

    .IF hoverLvl == 3
        call set_Green
    .ENDIF
    ;hard message - 3
    mov dl, 34 ;column
    mov dh, 17
    call gotoxy
    mov edx, OFFSET hardMsg
    call WriteString
    call restoreColors

    call restoreColors 
ret
displayMenu ENDP

open_menu PROC uses edx eax
    
    call displayMenu

    ;get initial time before program starts
    call GetMseconds

    ;time interval for dropping words by one line
    mov miliSec, eax

    searchForKey:

        mov eax, 100
        call Delay

        call ReadKey         ; look for keyboard input
        call GetMseconds

        .IF dx == VK_DOWN
            inc hoverLvl

            .IF hoverLvl > 3
                mov hoverLvl, 1
            .ENDIF

        .ELSEIF dx == VK_UP
            dec hoverLvl

            .IF hoverLvl < 1
                mov hoverLvl, 3
            .ENDIF

        .ELSEIF dx == VK_RETURN
            jmp goToGame

        .ENDIF

        ;only jump line if 1 sec has passed
        .IF eax >= miliSec
           call displayMenu
           add miliSec, 1000

            ;hide cursor
            call getMaxXy
            mov dl, 119 ;column
            mov dh, 29
            call gotoxy
       .ENDIF


    loop searchForKey

    goToGame:

ret
open_menu ENDP

fetchWordsFromFile PROC uses eax esi edi ecx edx

    .IF hoverLvl == 1
    mov wordLen, 4
    mov edx, OFFSET dict4

    .ELSEIF hoverLvl == 2
    mov wordLen, 8
    mov edx, OFFSET dict8
    
    .ELSEIF hoverLvl == 3
    mov wordLen, 12
    mov edx, OFFSET dict12

    

.ENDIF
    call OpenInputFile
    ;file handle is returned in eax
    mov file_Handle, eax

    mov edx, OFFSET buf
    mov ecx, bufSize
    call ReadFromFile
    jc fileReadError
    mov Readbytes, eax

    ; Tokenize the content and store words in the array
    mov esi, offset buf
    mov edi, offset strArray

tokenizeLoop:
    ; Read a character from the buffer
    mov al, [esi]
    cmp al, 0
    je  tokenizeDone

    ; Check for space or newline character
    cmp al, ' '
    je  continue
    ;cmp al, 0Ah ; nextline character
    ;je continue

    mov [edi], al
    jmp done
    

continue:
    ;adjusting edi
    dec edi
    inc wordCount

done:
    inc edi
    inc esi
    jmp tokenizeLoop

tokenizeDone:

    inc wordCount
    jmp closeFile1

    fileReadError:
        mov edx, OFFSET fileErr
        call WriteString

closeFile1:
        ;call WriteString
        mov eax, file_Handle
        call CloseFile

ret
fetchWordsFromFile ENDP

generateRandX_Y PROC uses ecx esi edx eax
    
    ;esi = x coordinate
    mov esi, OFFSET xCords
    ;edx = y coordinate
    mov edx, OFFSET yCords
    mov ecx, wordCount
    call randomize

    generate:
        ;for y coordinates
        mov al, 0
        mov [edx], al
        inc edx

        ;for x coordinates
        push edx
            call getMaxXY
            sub edx, wordLen
            movzx eax, dl ;eax is the range
            call randomRange
            ;eax now contains a random value
        pop edx
        mov [esi], al
        inc esi

    loop generate
    

ret
generateRandX_Y ENDP

displayPrompt PROC USES edx
;requires: the prompt needs to be in esi
    mov edx, esi
    call WriteString

ret
displayPrompt ENDP

createStrings PROC uses ecx ebx esi
	mov ecx, wordCount					;loop counter
	mov esi, OFFSET strArray
	call Randomize
	L1:
		call randomizedString		;create random string in str_array
		add esi, wordLen			;move next index of str_array
		loop L1
	ret
createStrings ENDP

;esi should hold the offset of str_array
randomizedString PROC uses ecx eax esi		;return random string and stored in str_array

	mov ecx, wordLen
	inc ecx
	L5:
		mov eax, 26
		call RandomRange	;generate within 26 alphabets
		add eax, 'a'		;move eax to first Alpha
		mov [esi],al		;change the string
		inc esi				;increase to next index
		loop L5
		;inc count
	ret
randomizedString ENDP




returnToStart PROC USES edx
;go to start of the 0 x 0 on the screen

    mov dl, 0; column
    mov dh, 0; row
    call Gotoxy
ret
returnToStart ENDP

set_Green PROC USES eax
  mov eax, green + (black * 16)    ; Set text color to green
    call SetTextColor
ret
set_Green ENDP

set_Red PROC USES eax
  mov eax, RED+(black * 16)    ; Set text color to green
    call SetTextColor
ret
set_Red ENDP

restoreColors PROC USES eax
    mov eax, white + (black * 16)    ; Reset to default white text on black background
    call SetTextColor
ret
restoreColors ENDP

;don't change (can change procedure name)
dealBSpace PROC USES edx
    ; Edge case: If x = 0, do nothing
    cmp counter, 0
    jne NOT_EDGE

    ; Edge case: If y = 0, do nothing
    cmp cord_Y, 0
    je NO_ACTION

    ; If x = 0 but y != 0, set counter to the largest column - 1 (move cursor to the last column)
    call getMaxXY       ; dl = highest column, dh = highest row
    mov counter, dl
    dec cord_Y           ; Move cursor up one row

NOT_EDGE:
    ; Check if the current character was correct
    mov al, BYTE PTR [esi]   ; Load the current character
    cmp al, BYTE PTR [esi + 1] ; Compare with the expected character
    je REMOVE_CORRECT

    ; Otherwise, it was a mistake
    jmp REMOVE_MISTAKE

REMOVE_CORRECT:
    ; Decrement `correctChars` if the character was correct
    dec correctChars
    jmp CONTINUE_BACKSPACE

REMOVE_MISTAKE:
    ; Decrement mistake counters
    dec mistakes
    dec totalMistakes

CONTINUE_BACKSPACE:
    ; Move one step back
    dec counter
    dec esi             ; Move to the previous character in the string

    ; Reset the text color to default (black on white)
    call restoreColors
    call positionCursor

NO_ACTION:
    ret
dealBSpace ENDP


positionCursor PROC USES edx
    mov dh, cord_Y       ; Set Y-coordinate (row)
    mov dl, counter     ; Set X-coordinate (column)
    call gotoxy         ; Move cursor to the (x, y) position
ret
positionCursor ENDP

; don't change (can change procedure name)
runTest PROC USES eax esi
    ; run typing test with given prompt, correct words will be green, wrong ones red
    ; requires: prompt is in the esi

    ; Initialize variables for wordsCompleted, typedWords, mistakes, totalMistakes, correctChars, and accuracy
    mov typedWords, 0        ; Initialize typedWords counter
    mov mistakes, 0          ; Initialize mistakes counter for the current word
    mov totalMistakes, 0     ; Initialize totalMistakes counter
    mov correctChars, 0      ; Initialize correctChars counter
    mov totalchars, LENGTHOF strOne

L_INPUT: 

    ; Read the char
    call ReadChar

    ; IF IT IS A BACKSPACE
    cmp al, 8                ; 8 is ASCII for backspace
    jne NOT_BACKSPACE
    call dealBSpace
  jmp done

NOT_BACKSPACE:
    ; Compare the characters
    cmp BYTE PTR [esi], al
    jne MISMATCH_CHAR

    ; Correct character typed, set green
    call set_Green
    inc correctChars          ; Increment correctChars counter
    jmp MATCH_CHAR

MISMATCH_CHAR: 
    ; Incorrect character typed, set red
    call set_Red
    inc mistakes              ; Increment mistakes counter for the current word
    inc totalMistakes         ; Increment totalMistakes counter for all mistakes
    jmp MATCH_CHAR

MATCH_CHAR:
    ; Output the typed character
    mov al, BYTE PTR [esi]
    call WriteChar

    ; Move to next character in prompt
    inc esi
    inc counter
    
    ; Check if we are at the end of the string (null terminator)
    cmp BYTE PTR [esi], 0     ; Check if current character is the null terminator
    je CHECK_LAST_WORD        ; If null terminator, check the last word and end the test

    ; Check if the current character is a space or punctuation (word boundary)
    cmp al, " "               ; 20h is ASCII for space
    je INCREMENT_WORDS

    ; Check if it goes over the bound (screen width), then move to next line
    call getMaxXY             ; dl = highest column, dh = highest row
    dec dl
    cmp dl, counter
    jae IN_BOUND

    ; Out of bound, move to the next line
    inc cord_Y
    mov counter, 0

IN_BOUND:
    ; Do nothing if inbound
    jmp L_INPUT               ; Continue reading next character

INCREMENT_WORDS:
    ; Check if the entire word was correct (i.e., no mistakes in the word)
    mov eax, mistakes         ; Copy current mistakes counter
    cmp eax, 0                ; If no mistakes, this word is correct
    je WORD_IS_CORRECT        ; Jump to increment typedWords

    ; Reset mistakes for the next word and continue
    mov mistakes, 0
    jmp L_INPUT

WORD_IS_CORRECT:
    ; Word is correct (all characters green)
    inc typedWords

    ; Reset mistakes counter for the next word
    mov mistakes, 0

    ; Continue to the next character
    jmp L_INPUT

DONE: 
    call positionCursor
    jmp L_INPUT



CHECK_LAST_WORD:
    ; Handle the last word for correctness
    mov eax, mistakes         ; Check if there are any mistakes in the last word
    cmp eax, 0                ; If no mistakes, increment typedWords
    je WORD_IS_CORRECT_FINAL

    ; No need to reset `mistakes` for last word since we're exiting
    jmp FINISHED_TYPING

WORD_IS_CORRECT_FINAL:
    ; Final word is correct
    inc typedWords

FINISHED_TYPING:     

 mov eax, correctchars ; EAX = correctchars
    mov correctchars, eax

    mov ebx, totalchars   ;EBX = totalinputcount
    dec ebx
    mov totalchars, ebx

    mov ecx, 100                       ; ECX = 100 (for percentage calculation)

    mul ecx                            ; EAX = correctchars * 100, result fits in EDX:EAX
    xor edx, edx                       ; Clear EDX before division
    div ebx                            ; EAX = (correctchars * 100) / totalinputcount

    mov accuracy, eax                  ; Store accuracy percentage in accuracy variable

     .IF totalmistakes > 0
     mov lost, 1
     .ENDIF 




    ; Reset or display counters as needed
    mov counter, 0

ret
runTest ENDP




typingTutorGame PROC USES esi ecx

     .IF hoverLvl == 1
     mov esi, OFFSET strOne
     mov ecx, LENGTHOF strOne

    .ELSEIF hoverLvl == 2
    mov esi, OFFSET strTwo
    mov ecx, LENGTHOF strTwo
    
    .ELSEIF hoverLvl == 3
    mov esi, OFFSET strThree
    mov ecx, LENGTHOF strThree

    .ENDIF

    
    ;make sure to take one space off of null char
    dec ecx

    call displayPrompt
    call returnToStart ;go to 0x0 to type    


    ;call the typing test
    call runTest 
   

    call restoreColors

ret
typingTutorGame ENDP


drawFinLine PROC USES edx eax ecx

    ;get max_X and maxY
    call getMaxXY

    ;choose vertical position
    mov dh, lnHeight
    ;fill the screen horizontally
    movzx ecx, dl 

    mov dl, 0
    mov al, '-'

loopFLn:
 call gotoxy
    call WriteChar
     inc dl
   

    loop loopFLn

    
        ;print accuracy & mistake count
        call set_Red

        call crlf
        mov edx, OFFSET mistakeMsg
        call WriteString

        mov eax, totalmistakes
        call WriteDec
        call crlf

COMMENT $
        mov edx, OFFSET accuracyMsg
        call WriteString

        call CalcAccu
        mov eax, accuracy
        call WriteDec $

        call restoreColors
    
ret
drawFinLine ENDP

drawBaseLn PROC uses edx

    mov dl, 1
    mov dh, lnHeight
    call gotoxy

    mov edx, OFFSET finishLn
    call WriteString

ret
drawBaseLn ENDP

verifyTouchLine PROC 

    ;check if any words have touched the base line
    cmp touchLine, 1
    jne CONTINUE_DROP
    mov ecx, 1
    mov Lost, 1

CONTINUE_DROP:

ret
verifyTouchLine ENDP

displayWords PROC USES edx ecx esi eax ebx

    ;adjust loop count for completed words
    mov ecx, printedWords
    sub ecx, typedWords

loop_Words:

        mov ebx, count
        ;don't print if a word is completed
        add ebx, typedWords

        .IF count == 0
            ;adjust the esi pointer to next word
            push ecx
            .IF typedWords > 0
                mov ecx, typedWords
                movESI:
                    add esi, wordLen
                loop movESI
            .ENDIF 
            pop ecx

        .ENDIF

        mov dl, xCords[ebx]
        mov dh, yCords[ebx]
        call gotoxy

        ;check if it has touched the base line

        cmp dh, lnHeight
        jne LW_CONTINUE
        mov touchLine, 1
LW_CONTINUE:
       
        push ecx
        mov ecx, wordLen
    CHAR_LOOP:
            mov al, BYTE PTR [esi]
            call WriteChar
            inc esi
    loop CHAR_LOOP
        pop ecx
        

        ;increase business
        inc count

        loop loop_Words

         ;reset count
         mov count, 0
ret
displayWords ENDP

showInputArray PROC USES edx ecx esi eax ebx

    mov ebx, typedWords
    mov dl, xCords[ebx]
    mov dh, yCords[ebx]
    call gotoxy

    ;no need to check touchLine
    mov ecx, typedChars

    ;esi points to InputArr
    mov esi, OFFSET arrInput

    .IF input_correct == 0
        call set_Red
    .ELSEIF input_correct == 1
        call set_Green
    .ENDIF

    .IF typedChars > 0
        CHAR_LOOP:
                mov al, BYTE PTR [esi]
                call WriteChar
                inc esi
        loop CHAR_LOOP
    .ENDIF    

    ;show the incorrect character
;-----------------------------------------
.IF (input_correct == 0) && (totalmistakes >= 1)
    mov esi, OFFSET strArray
    add esi, typedChars
    .IF typedWords > 0
        mov ecx, typedWords
        movESI:
            add esi, wordLen
        loop movESI
    .ENDIF

    mov al, BYTE PTR [esi]
    call WriteChar

    ;relocate cursor if wrong
    mov ebx, typedWords
    mov dl, xCords[ebx]
    mov dh, yCords[ebx]
    add edx, typedChars
    call gotoxy
.ENDIF
;-------------------------------------


    call restoreColors
    ;increase business
    inc count
        ;reset count
    mov count, 0
ret
showInputArray ENDP

validateKey PROC USES edx esi eax ecx

    add esi, typedChars

    .IF typedWords > 0
        mov ecx, typedWords
        movESI:
            add esi, wordLen
        loop movESI
    .ENDIF  
    
    mov eax, wordLen
    .IF typedChars < eax
        mov eax, typedChars
        ;add dl, al <--- I dont know why I put this, gave me errors!!
    .ENDIF

    mov eax, wordCount
    .IF typedWords < eax
        call gotoxy
    .ENDIF

    ;THIS SAVED MY LIFE OMFFGGGGGGGGGGG!!!
    mov eax, 10
    call Delay
    call ReadKey
    ;nothing is read into
    jz DONE
        
    ;keep track of how many have been typed
    inc totalInpCount

    cmp BYTE PTR [esi], al
    jne MISMATCH_CHAR
    call set_Green
    jmp MATCH_CHAR

MISMATCH_CHAR: 
    mov input_correct, 0
    inc totalmistakes
    jmp DONE

MATCH_CHAR:
    inc typedChars

    ;update input array
    mov edx, OFFSET arrInput
    add edx, typedChars
    dec edx
    mov [edx], al
    inc edx

    ;set input_correct
    mov input_correct, 1

DONE:  
     call showInputArray
     mov eax, wordLen
        .IF typedChars >= eax
        inc typedWords
        mov typedChars, 0
    .ENDIF

    mov eax, wordCount
    .IF typedWords == eax
        mov gameWon, 1
    .ENDIF


ret
validateKey ENDP

wordDroppingGame PROC USES edx ebx eax esi

    call fetchWordsFromFile

;fill up cords
    call generateRandX_Y

;for now - loop infinitely until the finishline
    mov ecx, -1

;get initial time before program starts
    call GetMseconds
    mov startTime, eax

    ;time interval for dropping words by one line
    mov miliSec, eax


loop_WD:
    ;-----------------
    ;hardcode ecx / the number of arrays for now
    mov ecx, wordCount
    mov esi, OFFSET strArray
    
    ;call drawBaseLn

    call validateKey

    call GetMseconds ;time right now -> eax

    ;only jump line if 1 sec has passed
    .IF eax >= miliSec
        
        call clrscr

        ;increase each y coords

        push ecx
        mov ecx, printedWords
        increase_y:
            inc yCords[ecx - 1]
        loop increase_y
        pop ecx

        ;draw FinishLine
        call drawFinLine

        ;print all the words in the array
        call displayWords

        ;print typed characters
        ;call showInputArray
        
        mov eax, wordCount
        .IF printedWords < eax
            inc printedWords
        .ENDIF
        add miliSec, 1000
    .ENDIF  
   

    call verifyTouchLine
    .IF gameWon == 1
        jmp DONE
    .ENDIF

NOTNOW:
    mov count, 0

loop loop_WD

DONE:
     call computeAccu
   
ret
wordDroppingGame ENDP


msgFinish PROC
;edx: message to print
    
    call clrscr

    mov dl, 35
    mov dh, 10
    call gotoxy
    .IF Lost == 1
        mov edx, OFFSET lostMsg
    .ELSE
        mov edx, OFFSET wonMsg
    .ENDIF

    call WriteString

ret
msgFinish ENDP

main proc

 call restoreColors

    mov dl, 27           ; X-coordinate for the menu
    mov dh, 9            ; Y-coordinate for the menu
    call gotoxy
    mov edx, OFFSET menuMessage1
    call WriteString      ; Write "1. Typing Tutor Game"
    
    mov dl, 27            ; X-coordinate for the menu
    mov dh, 10            ; Y-coordinate for the menu
    call gotoxy
    mov edx, OFFSET menuMessage2
    call WriteString      ; Write "2. Word Dropping Game"

    mov dl, 27            ; X-coordinate for the menu
    mov dh, 11            ; Y-coordinate for the menu
    call gotoxy
    mov edx, OFFSET menuMessage3
    call WriteString      ; Write "Enter your choice:"

  

    mov eax, 0            ; Prepare to store user input
    call ReadInt          ; Read user input into `eax`


    ; Branch based on user choice
    cmp eax, 1
    je MODE1             ; Jump to Typing Tutor Game if input is 1

    cmp eax, 2
    je MODE2             ; Jump to Word Dropping Game if input is 2

    ; Handle invalid input
    mov dl, 30
    mov dh, 14
    call gotoxy
    mov edx, OFFSET invalidMsg
    call WriteString
    jmp END_PROGRAM        ; Exit if invalid choice is entered
     

MODE1: 
call clrscr
       call getMaxXy
  mov dl, 119 ;column
      mov dh, 29
      call gotoxy

call open_menu
call clrscr

   call typingTutorGame  ; Call Typing Tutor Game
 jmp DONE              ; Jump to the end after the game

MODE2:
call clrscr
       call getMaxXy
  mov dl, 119 ;column
      mov dh, 29
      call gotoxy
call open_menu
call clrscr
     .IF hoverLvl >= 1 && hoverLvl <= 3
        call screenLoader
        call wordDroppingGame
    .ENDIF

DONE:
    call msgFinish        ; Display the result message
    call crlf             ; Move to a new line
    call finalGameReport       ; Generate and display the game report

    mov eax, 1000
    call delay            ; Delay before exiting

END_PROGRAM:  
    INVOKE ExitProcess, 0 ; Exit the program

main endp


finalGameReport PROC uses EDX EAX

    ;print accuracy & mistake count
 
        mov dl, 29
        mov dh, 12
        call gotoxy
        mov edx, OFFSET mistakeMsg
        call WriteString

        mov eax, totalmistakes
        call WriteDec
        call crlf

        mov dl, 29
        mov dh, 13
        call gotoxy
        mov edx, OFFSET accuracyMsg
        call WriteString

   
        mov eax, accuracy
        call WriteDec 

        ;words completed
        mov dl, 29
        mov dh, 14
        call gotoxy
        mov edx, OFFSET wordsCompleteMsg
        call WriteString

        mov eax, typedWords
        call WriteDec 

        call crlf
        call crlf

ret
finalGameReport ENDP

computeAccu proc uses EAX EBX EDX
 
.IF totalInpCount > 0
     mov edx, 0
     mov eax, totalInpCount
     mov ebx, totalmistakes
     sub eax, ebx
     mov playerScore, eax ;now eax has number of right char
     mov ebx, 100
     mul ebx; 
     mov ebx, totalInpCount
     div ebx
     mov ACCURACY, eax

.ELSE
    mov ACCURACY, 0
 .ENDIF

ret
computeAccu ENDP

end main
