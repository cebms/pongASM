org 0x7e00
jmp main

playerPositionY dw 75
playerPositionX dw 10
secPlayerPositionY dw 75
secPlayerPositionX dw 303
movCount dw 0
movCountSec dw 0
racketSpeed dw 7

FirstPlayerScore db 48, 0
SecondPlayerScore db 48, 0

ballPositionX dw 153
ballPositionY dw 60

ballDirectionX dw 0; 0 esquerda, 1 parado, 2 direita
ballDirectionY dw 1; 0 baixo, 1 parado, 2 cima

ballSpeed dw 15
ballCount dw 0

play db 'P','L','A','Y', 0
instructions db 'I','N','S','T','R','U','C','O','E','S', 0
credits db 'C','R','E','D','I','T','O','S', 0

thiago db 'T','h','i','a','g','o',' ','J','o','s','e',' ','A','l','v','e','s',' ','d','e',' ','S','o','u','z','a',' ','<','T','J','A','S','>', 0
carlos db 'C','a','r','l','o','s',' ','E','d','u','a','r','d','o',' ','B','e','z','e','r','r','a',' ','<','C','E','B','M','S','>',0
clebson db 'J','o','s','e',' ','C','l','e','b','s','o','n',' ','d','e',' ','S','o','u','z','a',' ','O','l','i','v','e','i','r','a',' ','<','J','C','S','O','2','>',0

blueWins db 'B','L','U','E',' ','w','i','n','s',0
redWins db 'R','E','D',' ','w','i','n','s',0

instructionLabel db 'M','o','v','e','r',' ' ,'r','a','q','u','e','t','e','s',' ',':', 0
firstPlayerInstructions db 'P','1',':',' ',' ','W',' ',':',' ','S', 0
secPlayerInstructions db 'P','2',':',' ',' ','S','E','T','A','S', 0
extraInstructionLabel db 'O','b','j','e','t','i','v','o',' ',':', 0
goalInstruction db '9',' ','P','O','N','T','O','S',0

frictionInstruction db 'M','e','c','a','n','i','c','a',' ',':',0
frictionUpInstruction db 'B','o','l','a',' ','s','o','b','e',' ','c','a','s','o',' ',' ',0
upKeyInstruction db '-',' ','W',' ','|',' ','U','P',0
frictionDownInstruction db 'B','o','l','a',' ','d','e','s','c','e',' ','c','a','s','o',' ',' ',0
downKeyInstruction db '-',' ','S',' ','|',' ','D','O','W','N',0
ballInstruction db 'B','o','l','a',' ','s','e','g','u','e',' ','r','e','t','a',' ','p','/',' ','o','u','t','r','a','s',' ','t','e','c','l','a','s',0

menuState dw 0

main:
    xor ax, ax
    mov ds, ax
    mov cx, ax
    mov dx, ax

    ;Setup ISR for the scancode
    call init
   
    call clearScreen
    call setVideoMode
    
    menuBegin:
        call clearScreen
        call doMenu

    startLoop:
        call clearScreen
        call setVideoMode

    mainLoop:

        call drawRacket
        
        call drawRacketSec
        
        call drawBall
        
        call movBall

        call printFirstPlayerScore
        call printSecondPlayerScore
        
        mov al, 1fh           ;s
        call is_scancode_pressed
        ; jz mainLoops
        jnz delayFirstPlayerDown

        checkUp:
        mov al, 11h           ;w 
        call is_scancode_pressed
        jnz delayFirstPlayerUp

        checkDownSec:
        mov al, 50h         ; arrow down
        call is_scancode_pressed
        jnz delaySecPlayerDown

        checkUpSec:
        mov al, 48h         ; arrow up
        call is_scancode_pressed
        jnz delaySecPlayerUp

        jmp mainLoop   

jmp done

;----- GAME FUNCTIONS -----

creditScreen:
    call clearScreen
    call setVideoMode

    ; carlos
    mov ah,02h
    mov dh,9    ;row
    mov dl,6   ;column
    int 10h
    mov si, carlos
    mov bh, 0
    mov bl, 0fh  ;makes it white
    call printf
    ; clebson
    mov ah,02h
    mov dh,11    ;row
    mov dl,1   ;column
    int 10h
    mov si, clebson
    mov bh, 0
    mov bl, 0fh  ;makes it white
    call printf
    ; thiago
    mov ah,02h
    mov dh,13    ;row
    mov dl,5   ;column
    int 10h
    mov si, thiago
    mov bh, 0
    mov bl, 0fh  ;makes it white
    call printf
    mov ah, 00h
    int 16h
    call clearScreen
    call setVideoMode

    jmp menuBegin

    instructionScreen:
    call clearScreen
    call setVideoMode

    ; carlos
    mov ah,02h
    mov dh,1    ;row
    mov dl, 1   ;column
    int 10h
    mov si, instructionLabel
    mov bh, 0
    mov bl, 0fh  ;makes it white
    call printf
    ; clebson
    mov ah,02h
    mov dh,3   ;row
    mov dl,2   ;column
    int 10h
    mov si, firstPlayerInstructions
    mov bh, 0
    mov bl, 4  ;makes it white
    call printf
    ; thiago
    mov ah,02h
    mov dh,5    ;row
    mov dl,2   ;column
    int 10h
    mov si, secPlayerInstructions
    mov bh, 0
    mov bl, 1  ;makes it white
    call printf

    mov ah,02h
    mov dh,8    ;row
    mov dl,1   ;column
    int 10h
    mov si, extraInstructionLabel
    mov bh, 0
    mov bl, 15  ;makes it white
    call printf


    mov ah,02h
    mov dh,10    ;row
    mov dl,2   ;column
    int 10h
    mov si, goalInstruction
    mov bh, 0
    mov bl, 2  ;makes it white
    call printf

    mov ah,02h
    mov dh,13    ;row
    mov dl,1   ;column
    int 10h
    mov si, frictionInstruction
    mov bh, 0
    mov bl, 15  ;makes it white
    call printf

    mov ah,02h
    mov dh,15    ;row
    mov dl,1   ;column
    int 10h
    mov si, frictionUpInstruction
    mov bh, 0
    mov bl, 15  ;makes it white
    call printf

    mov ah,02h
    mov dh,15    ;row
    mov dl,18   ;column
    int 10h
    mov si, upKeyInstruction
    mov bh, 0
    mov bl, 4  ;makes it white
    call printf

    mov ah,02h
    mov dh,16    ;row
    mov dl,1   ;column
    int 10h
    mov si, frictionDownInstruction
    mov bh, 0
    mov bl, 15  ;makes it white
    call printf

    mov ah,02h
    mov dh,16    ;row
    mov dl,18   ;column
    int 10h
    mov si, downKeyInstruction
    mov bh, 0
    mov bl, 1  ;makes it white
    call printf

    mov ah,02h
    mov dh,17    ;row
    mov dl,1   ;column
    int 10h
    mov si, ballInstruction
    mov bh, 0
    mov bl, 7  ;makes it white
    call printf

    mov ah, 00h
    int 16h
    call clearScreen
    call setVideoMode

    jmp menuBegin


doMenu:
    cmp word[menuState], 0
    je printplay
    cmp word[menuState], 1
    je printInstructions
    cmp word[menuState], 2
    je printCredits

    printplay:
        ;play
        mov ah,02h
        mov dh,9    ;row
        mov dl,18    ;column
        int 10h
        mov si, play
        mov bh, 0
        mov bl, 2  ;makes it green
        call printf
        ;intrucoes
        mov ah,02h
        mov dh,11   ;row
        mov dl,15    ;column
        int 10h
        mov si, instructions
        mov bh, 0
        mov bl, 0fh  ;makes it white
        call printf
        ; creditos
        mov ah,02h
        mov dh,13    ;row
        mov dl,16   ;column
        int 10h
        mov si, credits
        mov bh, 0
        mov bl, 0fh  ;makes it white
        call printf
        ; aqui espero o usuario digitar
        inputPlay:
            mov ah, 00
            int 16h
            cmp al, 's'
            je incMenu
            cmp al, 13  ;enter comeca o jogo
            je startLoop
            cmp al, ' ' ;espaco comeca o jogo
            je startLoop
        jmp printplay
    
    printInstructions:
        ;play
        mov ah,02h
        mov dh,9    ;row
        mov dl,18    ;column
        int 10h
        mov si, play
        mov bh, 0
        mov bl, 0fh  ;makes it green
        call printf
        ;intrucoes
        mov ah,02h
        mov dh,11   ;row
        mov dl,15    ;column
        int 10h
        mov si, instructions
        mov bh, 0
        mov bl, 02h  ;makes it white
        call printf
        ; creditos
        mov ah,02h
        mov dh,13    ;row
        mov dl,16   ;column
        int 10h
        mov si, credits
        mov bh, 0
        mov bl, 0fh  ;makes it white
        call printf
        ; aqui espero o usuario digitar
        inputInst:
            mov ah, 00
            int 16h
            cmp al, 's'
            je incMenu
            cmp al, 'w'
            je decMenu
            cmp al, 13  ;enter comeca o jogo
            je instructionScreen
            cmp al, ' ' ;espaco comeca o jogo
            je instructionScreen
        jmp printInstructions

    printCredits:
       ;play
        mov ah,02h
        mov dh,9    ;row
        mov dl,18    ;column
        int 10h
        mov si, play
        mov bh, 0
        mov bl, 0fh  ;makes it white
        call printf
        ;intrucoes
        mov ah,02h
        mov dh,11   ;row
        mov dl,15    ;column
        int 10h
        mov si, instructions
        mov bh, 0
        mov bl, 0fh  ;makes it white
        call printf
        ; creditos
        mov ah,02h
        mov dh,13    ;row
        mov dl,16   ;column
        int 10h
        mov si, credits
        mov bh, 0
        mov bl, 02h  ;makes it green
        call printf
        ; aqui espero o usuario digitar
        inputCredit:
            mov ah, 00
            int 16h
            cmp al, 'w'
            je decMenu
            cmp al, 13  ;enter comeca o jogo
            je creditScreen
            cmp al, ' ' ;espaco comeca o jogo
            je creditScreen
        jmp printCredits
ret

incMenu:
    inc word[menuState]
jmp menuBegin

decMenu:
    dec word[menuState]
jmp menuBegin

printSecondPlayerScore:
    mov ah,02h
    mov dh,2    ;row
    mov dl,22    ;column
    int 10h

    mov si, SecondPlayerScore
    mov bl, 15 ; white color for scoreboard text
    call printf
ret

printFirstPlayerScore:
    mov ah,02h
    mov dh,2    ;row
    mov dl,16    ;column
    int 10h

    mov si, FirstPlayerScore
    mov bl, 15 ; white color for scoreboard text
    call printf
ret

printf:
    lodsb
    cmp al, 0
    je finish
    mov ah, 0eh
    int 10h
    jmp printf

finish:
    ret

incFirstPlayerScore:
    mov ax, word[FirstPlayerScore]
    inc ax
    mov word[FirstPlayerScore], ax
    call printFirstPlayerScore
ret

incSecondPlayerScore:
    mov ax, word[SecondPlayerScore]
    inc ax
    mov word[SecondPlayerScore], ax
    call printSecondPlayerScore
ret

firstPlayerWins:
    mov ah,02h
    mov dh, 12    ;row
    mov dl, 15   ;column
    int 10h

    mov si, redWins
    mov bl, 0x04
    call printf

    call checkForSpace

ret

secondPlayerWins:
    mov ah,02h
    mov dh, 12    ;row
    mov dl, 15   ;column
    int 10h

    mov si, blueWins
    mov bl, 0x01
    call printf

    call printFirstPlayerScore
    call printSecondPlayerScore
    call drawRacket
    call drawRacketSec
    

    call checkForSpace

ret

checkForSpace:
    mov al, 11h           ;w 
    call is_scancode_pressed
    ;reseta variaveis do jogo
    mov word[FirstPlayerScore], 48
    mov word[SecondPlayerScore], 48
    mov word[ballPositionX], 153
    mov word[ballPositionY], 60
    mov word[ballDirectionX], 0
    mov word[ballDirectionY], 1
    mov word[ballCount], 0
    mov word[menuState], 0
    jnz menuBegin
    jmp checkForSpace

clearScreen:
    mov ah, 0       ; primeiro parametro para chamar modo de video
    mov al, 13h     ; segundo parametro para chamar modo de video
    int 10h

    ; printa o placar
    mov ah, 0xe ;escolhe cor da letra
    mov bh, 0   ;numero da pagina
    mov bl, 0xf ;cor branca da letra
ret

movBall:

    mov ax, word[ballCount]
    cmp ax, word[ballSpeed] 
    je controlY
    inc word[ballCount]
    ret

    controlY: ;colisao com teto e chao
        mov word[ballCount], 0
        cmp word[ballPositionY], 0
        je goDown
        cmp word[ballPositionY], 192
        je goUp

    controlX:
        ;se estiver perto de bater na raquete da esquerda
        cmp word[ballPositionX], 18
        je checkCol
        ;se estiver perto de bater na raquete da direita
        cmp word[ballPositionX], 295
        je checkColSec
        ; se bater na parede da esquerda
        cmp word[ballPositionX], 0
        je p2Point
        ; se bater na parede da direita
        cmp word[ballPositionX], 312
        je p1Point
    
    movX:
        cmp word[ballDirectionX], 1
        ja incBallX
        jb decBallX

    movY:
        cmp word[ballDirectionY], 1
        jb incBallY
        ja decBallY

    endMov:
ret

p1Point:
    call incFirstPlayerScore
    call printSecondPlayerScore
    cmp word[FirstPlayerScore], 56 ;compara se a pontuacao passou de 9
    jg firstPlayerWins
    mov word[ballPositionX], 153
    mov word[playerPositionY], 75
    mov word[playerPositionX], 10
    mov word[secPlayerPositionY], 75
    mov word[secPlayerPositionX], 303
    mov word[ballDirectionX],  0; 0 esquerda, 1 parado, 2 direita
    mov word[ballDirectionY], 0; 0 baixo, 1 parado, 2 cima
    mov word[ballSpeed], 15
    call clearScreen
jmp mainLoop

p2Point:
    call incSecondPlayerScore
    call printFirstPlayerScore
    cmp word[SecondPlayerScore], 56 ;compara se a pontuacao passou de 9
    jg secondPlayerWins
    mov word[ballPositionX], 153
    mov word[playerPositionY], 75
    mov word[playerPositionX], 10
    mov word[secPlayerPositionY], 75
    mov word[secPlayerPositionX], 303
    mov word[ballDirectionX],  2; 0 esquerda, 1 parado, 2 direita
    mov word[ballDirectionY], 0; 0 baixo, 1 parado, 2 cima
    call clearScreen
    
jmp mainLoop

checkCol:
    ;preciso checar se ele esta na mesma altura
    checkBaixo:
        mov ax, word[ballPositionY]
        mov bx, word[playerPositionY]
        add bx, 32
        cmp ax, bx
        ja movX  ; se ax > bx+32 nao colidiu
    checaCima:
        mov ax, word[ballPositionY]
        mov bx, word[playerPositionY]
        add ax, 8
        cmp ax, bx
        jb movX

    mov al, 11h           ;w 
    call is_scancode_pressed
    jnz frictionUp

    mov al, 1fh           ;s 
    call is_scancode_pressed
    jnz frictionDown

    mov word[ballDirectionY], 1; 0 baixo, 1 parado, 2 cima

    endRacketCheck:
    cmp word[ballSpeed], 5
    ja fasterBall
    endBallSpeedCheck:
jmp goRight

checkColSec:
    ;preciso checar se ele esta na mesma altura
    ; checkBaixo1
        mov ax, word[ballPositionY]
        mov bx, word[secPlayerPositionY]
        add bx, 32
        cmp ax, bx
        ja movX  ; se bx+32 < ax nao colidiu
    ; checaCima:
        mov ax, word[ballPositionY]
        add ax, 8
        mov bx, word[secPlayerPositionY]
        cmp ax, bx
        jb movX

    mov al, 48h           ;arrow up
    call is_scancode_pressed
    jnz frictionUpSec

    mov al, 50h           ;arrow down
    call is_scancode_pressed
    jnz frictionDownSec

    mov word[ballDirectionY], 1; 0 baixo, 1 parado, 2 cima

    endRacketCheckSec:
        cmp word[ballSpeed], 5
        ja fasterBallSec
    endBallSpeedCheckSec:
jmp goLeft

fasterBall:
    dec word[ballSpeed]
jmp endBallSpeedCheck

fasterBallSec:
    dec word[ballSpeed]
jmp endBallSpeedCheckSec

frictionUp:
    mov word[ballDirectionY], 2 ;go up
jmp endRacketCheck

frictionDown:
    mov word[ballDirectionY], 0 ;do down
jmp endRacketCheck

frictionUpSec:
    mov word[ballDirectionY], 2 ;go up
jmp endRacketCheckSec

frictionDownSec:
    mov word[ballDirectionY], 0 ;do down
jmp endRacketCheckSec

goRight:
    mov word[ballDirectionX], 2
jmp movX
goLeft:
    mov word[ballDirectionX], 0
jmp movX
goDown:
    mov word[ballDirectionY], 0
jmp controlX
goUp:
    mov word[ballDirectionY], 2
jmp controlX


incBallX:
    mov dx, word[ballPositionY]
    mov cx, word[ballPositionX]
    mov bx, 0x00
    clearBallTraceRight:
        mov al, 0x00 ; black pixel
        mov ah, 0ch
        int 10h
        inc dx
        inc bx
        cmp bx, 9
    jne clearBallTraceRight
    inc word[ballPositionX]
jmp movY

decBallX:
    mov dx, word[ballPositionY]
    mov cx, word[ballPositionX]
    mov bx, 0x00
    add cx, 8
    clearBallTraceLeft:
        mov al, 0x00 ; black pixel
        mov ah, 0ch
        int 10h
        inc dx
        inc bx
        cmp bx, 9
    jne clearBallTraceLeft
    dec word[ballPositionX]
jmp movY

incBallY:
    mov dx, word[ballPositionY]
    mov cx, word[ballPositionX]
    mov bx, 0x00
    clearBallTraceDown:
        mov al, 0x00 ; black pixel
        mov ah, 0ch
        int 10h
        inc cx
        inc bx
        cmp bx, 9
    jne clearBallTraceDown
    inc word[ballPositionY]
jmp endMov
decBallY:
    mov dx, word[ballPositionY]
    mov cx, word[ballPositionX]
    mov bx, 0x00
    add dx, 8
    clearBallTraceUp:
        mov al, 0x00 ; black pixel
        mov ah, 0ch
        int 10h
        inc cx
        inc bx
        cmp bx, 9
    jne clearBallTraceUp
    dec word[ballPositionY]
jmp endMov

drawBall:
    mov ah, 0ch ; coloca no modo de pintar pixels
    mov dx, word[ballPositionY]

    for5:
        mov bx, 8 ;altura da bola 
        add bx, word[ballPositionY]
        cmp dx, bx  ; fim da bola (linhas)
        je .fim5
        mov cx, word[ballPositionX]  ; inicio da bola (coluna)
        jmp .for6
        jmp for5

        .fim5:
        ret

        ; laco for para varrer as 8 colunas da imagem
        .for6:
        mov bx, 8 ;largura da bola
        add bx, word[ballPositionX]
        cmp cx, bx   ; direita da bola
        je .fim6
        mov al, 0x0f
        mov ah, 0ch
        int 10h
        inc cx
        jmp .for6

        .fim6:
        inc dx
        jmp for5

ret

delayFirstPlayerDown:
inc word[movCount]
mov ax, word[racketSpeed]
cmp word[movCount], ax
je moveDown
jmp checkUp

delaySecPlayerDown:
inc word[movCountSec]
mov ax, word[racketSpeed]
cmp word[movCountSec], ax
je moveDownSec
jmp checkUpSec

delayFirstPlayerUp:
inc word[movCount]
mov ax, word[racketSpeed]
cmp word[movCount], ax
je moveUp
jmp checkDownSec

delaySecPlayerUp:
inc word[movCountSec]
mov ax, word[racketSpeed]
cmp word[movCountSec], ax
je moveUpSec
jmp mainLoop

setVideoMode: 
    mov ah, 0       ; primeiro parametro para chamar modo de video
    mov al, 13h     ; segundo parametro para chamar modo de video
    int 10h
ret

drawRacket:
    mov ah, 0ch ; coloca no modo de pintar pixels
    mov dx, word[playerPositionY]

    for1:
        mov bx, 32 ;altura da raquete 
        add bx, word[playerPositionY]
        cmp dx, bx  ; fim da raquete (linhas)
        je .fim1
        mov cx, word[playerPositionX]  ; inicio da raquete (coluna)
        jmp .for2
        jmp for1

        .fim1:
        ret

        ; laco for para varrer as 8 colunas da imagem
        .for2:
        mov bx, 8 ;largura da raquete
        add bx, word[playerPositionX]
        cmp cx, bx   ; direita da raquete
        je .fim2
        mov al, 0x4
        mov ah, 0ch
        int 10h
        inc cx
        jmp .for2

        .fim2:
        inc dx
        jmp for1
ret

drawRacketSec:
    mov ah, 0ch ; coloca no modo de pintar pixels
    mov dx, word[secPlayerPositionY]

    lop1:
        mov bx, 32 ;altura da raquete 
        add bx, word[secPlayerPositionY]
        cmp dx, bx  ; fim da raquete (linhas)
        je .end1
        mov cx, word[secPlayerPositionX]  ; inicio da raquete (coluna)
        jmp .lop2
        jmp lop1

        .end1:
        ret

        ; laco for para varrer as 8 colunas da imagem
        .lop2:
        mov bx, 8 ;largura da raquete
        add bx, word[secPlayerPositionX]
        cmp cx, bx   ; direita da raquete
        je .end2
        mov al, 0x1
        mov ah, 0ch
        int 10h
        inc cx
        jmp .lop2

        .end2:
        inc dx
        jmp lop1
ret

moveDown:
    mov word[movCount], 0
    cmp word[playerPositionY], 168
    je checkUp
    mov dx, word[playerPositionY]
    mov cx, word[playerPositionX]
    mov bx, 0x00
    clearTraceDown:
        mov al, 0x00 ; black pixel
        mov ah, 0ch
        int 10h
        inc cx
        inc bx
        cmp bx, 8
    jne clearTraceDown

    mov ax, word[playerPositionY]
    inc ax 
    mov [playerPositionY], ax
    mov ah, 0x04
    int 16h
jmp checkUp

done:
    jmp $

moveUp:
    mov word[movCount], 0
    cmp word[playerPositionY], 0
    je checkDownSec
    mov dx, word[playerPositionY]
    mov cx, word[playerPositionX]
    mov bx, 0x00
    add dx, 31
    clearTraceUp:
        mov al, 0x00 ; black pixel
        mov ah, 0ch
        int 10h
        inc cx
        inc bx
        cmp bx, 8
    jne clearTraceUp

    mov ax, word[playerPositionY]

    dec ax
    mov [playerPositionY], ax
    mov ah, 0x04
    int 16h
jmp checkDownSec

moveUpSec:
    mov word[movCountSec], 0
    cmp word[secPlayerPositionY], 0
    je mainLoop
    mov dx, word[secPlayerPositionY]
    mov cx, word[secPlayerPositionX]
    mov bx, 0x00
    add dx, 31
    clearTraceUpSec:
        mov al, 0x00 ; black pixel
        mov ah, 0ch
        int 10h
        inc cx
        inc bx
        cmp bx, 8
    jne clearTraceUpSec

    mov ax, word[secPlayerPositionY]

    dec ax
    mov [secPlayerPositionY], ax
    mov ah, 0x04
    int 16h
jmp mainLoop

moveDownSec:
    mov word[movCountSec], 0
    cmp word[secPlayerPositionY], 168
    je checkUpSec
    mov dx, word[secPlayerPositionY]
    mov cx, word[secPlayerPositionX]
    mov bx, 0x00
    clearTraceDownSec:
        mov al, 0x00 ; black pixel
        mov ah, 0ch
        int 10h
        inc cx
        inc bx
        cmp bx, 8
    jne clearTraceDownSec

    mov ax, word[secPlayerPositionY]

    inc ax
    mov [secPlayerPositionY], ax
    mov ah, 0x04
    int 16h
jmp checkUpSec


 ;----- SCANCODE FUNCTIONS -----

 ;Set the ISR 
init:
  push ax  

  mov ax, cs
  mov WORD [old_isr_15 + 02h], ax          
  ;old_isr_15 is now a far pointer to new_isr_15

  call swap_isr_15          ;Swap the current isr15 with the one in old_isr_15

  pop ax 
  ret

  ;Restore the original ISR 
dispose:
  call swap_isr_15          ;Swap the current isr15 with the one in old_isr_15                  

  ret  

  ;Swap the pointer in the IVT for int 15h with the pointer in old_isr_15
swap_isr_15:
  push eax
  push es 

  xor ax, ax 
  mov es, ax 

  cli

  mov eax, DWORD [es: 15h*4]
  xchg eax, DWORD [old_isr_15]
  mov DWORD [es: 15h*4], eax 

  sti

  pop es 
  pop eax

  ret  

 ;Tell if a scancode is pressed
 ;
 ;al = scancode  
 ;ZF clear is pressed 
is_scancode_pressed:
  push bx

  movzx bx, al 
  cmp BYTE [scancode_status + bx], 0 

  pop bx 
  ret 


prints:             ; mov si, string
  .loop:
    lodsb           ; bota character em al 
    cmp al, 0
    je .endloop
    call putchar
    jmp .loop
  .endloop:
ret

putchar:
  mov ah, 0x0e
  int 10h
ret

 ;AL = scancode 
new_isr_15:
 ;Check for right function
 cmp ah, 4fh
 jne _ni15_legacy

 ;Save used regs
 push bx
 push ax


 movzx bx, al            ;BX = scancode 
 and bl, 7fh             ;BX = scancode value

 sar al, 07h             ;AL = 0ffh if scancode has bit7 set (release), 00h otherwise
 not al                  ;AL = 00h if scancode has bit7 set (release), 0ffh otherwise

 ;Save the scancode status
 mov BYTE [cs:bx + scancode_status], al 
 ;Increment the count
 inc WORD [cs:new_scancode]

 pop ax
 pop bx 

_ni15_legacy:   
 ;This is a far jump, in NASM is simply jmp FAR [cs:old_isr_15]
 ;Ended up this way for debug
 push WORD [cs: old_isr_15 + 02h] 
 push WORD [cs: old_isr_15] 
 retf

;Original ISR
old_isr_15                      dw new_isr_15, 0  

;Scan code status table
scancode_status     TIMES 128   db 0
;Scan code count 
new_scancode                    dw 0